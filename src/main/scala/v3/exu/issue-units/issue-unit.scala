//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.v3.exu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.{Str, CoreFuzzingConstants}

import boom.v3.common._
import boom.v3.exu.FUConstants._
import boom.v3.util.{BoolToChar}

/**
 * Class used for configurations
 *
 * @param issueWidth amount of things that can be issued
 * @param numEntries size of issue queue
 * @param iqType type of issue queue
 */
case class IssueParams(
  dispatchWidth: Int = 1,
  issueWidth: Int = 1,
  numEntries: Int = 8,
  iqType: BigInt
)

// corefuzzing: bundle carrying per-cycle winner info to a losing issue slot
class IssueContentionInfo(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val winner_op_count = UInt(inflOpCountWidthCF.W)   // [NARROW] see micro-op.scala
  val winner_is_atk   = Bool()
  val winner_is_sec   = Bool()
}

// corefuzzing: bundle sent from issue unit → ROB when a slot issues with accumulated contention
class IssueContentionUpdate(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  // O1 (corefuzzing): which influence this update reports.  The ROB used to hardcode
  // INFL_ISSUE_CONTENTION, so a non-pipelined FU blocking issue had nowhere to be
  // reported.  Carrying the type reuses the whole existing bus, pending table and
  // drain -- no second path.
  val infl_type       = UInt(inflTypeWidthCF.W)
  val rob_idx         = UInt(robAddrSz.W)
  val winner_op_count = UInt(inflOpCountWidthCF.W)   // [NARROW] see micro-op.scala
  val winner_is_atk   = Bool()
  val winner_is_sec   = Bool()
  val deny_count      = UInt(6.W)   // linear cycles; ROB compresses to a 3-bit log bucket
  // [FIFO10] step 10: the slot keeps the NEWEST winner; `dropped` counts the distinct winners it
  // replaced (and same-cycle extra denials), `any_atk` ORs every winner's is_atk (R-STICKY)
  val dropped         = UInt(2.W)
  val any_atk         = Bool()
  val any_sec         = if (ENABLE_IFT) Some(Bool()) else None   // [R117SWEEP] patch 33: OR of every denying winner's is_secret (r117); None IFT off (r124)
}

// corefuzzing: bundle driven into each issue slot to notify it of a cross-domain winner
class IssueContendInput(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  // O1: which influence this denial represents -- issue-PORT contention, or a
  // non-pipelined FU still occupied.  Carried so the ROB can type the entry.
  val infl_type       = UInt(inflTypeWidthCF.W)
  val winner_op_count = UInt(inflOpCountWidthCF.W)   // [NARROW] see micro-op.scala
  val winner_is_atk   = Bool()
  val winner_is_sec   = Bool()
  // [FIFO10] step 10: more than one denying winner this cycle (the extra ones count as dropped)
  // and the OR of all their is_atk
  val multi           = Bool()
  val any_atk         = Bool()
  val any_sec         = if (ENABLE_IFT) Some(Bool()) else None   // [R117SWEEP] patch 33: the secret twin of any_atk
}

/**
 * Constants for knowing about the status of a MicroOp
 */
trait IssueUnitConstants
{
  // invalid  : slot holds no valid uop.
  // s_valid_1: slot holds a valid uop.
  // s_valid_2: slot holds a store-like uop that may be broken into two micro-ops.
  val s_invalid :: s_valid_1 :: s_valid_2 :: Nil = Enum(3)
}

/**
 * What physical register is broadcasting its wakeup?
 * Is the physical register poisoned (aka, was it woken up by a speculative issue)?
 *
 * @param pregSz size of physical destination register
 */
class IqWakeup(val pregSz: Int, val withAtk: Boolean = false) extends Bundle
{
  val pdst = UInt(width=pregSz.W)
  val poisoned = Bool()
  // C2 (corefuzzing): the producer's taint, carried on the SAME broadcast that says
  // its destination register is ready.  DIFT tags at producer-dest-ready, and an
  // issue slot never sees the operand VALUE (unlike a load's AGU), so the wakeup is
  // its ONLY channel -- and it is exactly the moment the slot learns.  This matters
  // because INFL_ISSUE_CONTENTION samples winner_is_sec AT ISSUE, one stage later.
  // The source is already an ExeUnitResp carrying .secret; only this narrowing
  // dropped it, so the cost is 1 bit per wakeup port.
  val secret = Bool()
  // [IQHEAD] patch 49 (r174/r175): on a writeback wakeup, the producer's attacker VALUE taint
  // (taint_atk OR an attacker-domain producer: exactly the regfile write's taint_atk seed), the twin
  // of `secret`; constant 0 on a fast (issue-time) wakeup (core.scala).  Read only by the slot's
  // cf_src_atk (the IQ_FULL head attribution).  None with IFT off.
  val atk = if (withAtk) Some(Bool()) else None
}

/**
 * [IQHEAD] patch 49 (owner r175): the issue queue's stalling occupant = its OLDEST valid slot,
 * registered in the issue unit.  oc = cf_op_count_id; atk = attacker domain OR attacker-influenced
 * (slot cf_src_atk); sec = s_acc || s_prop (incl. cf_src_secret) || cf_pend_sec.  No valid slot: 0.
 */
class IftIqHead(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants
{
  val oc  = UInt(uopIDCounterWidthCF.W)
  val atk = Bool()
  val sec = Bool()
}

/**
 * [WAKESEC] Load-hit speculative wakeup: the destination preg plus the load's secret as
 * known at TLB time (secret-range address or a secret-propagating uop).  Without the secret
 * term a load-hit consumer issues before the secret-bearing memory wakeup arrives (Phase 0
 * finding (b)).  Residual: a secret that arrives by memory dataflow is unknown at TLB time.
 *
 * @param pregSz size of physical destination register
 */
class SpecLdWakeup(val pregSz: Int) extends Bundle
{
  val pdst   = UInt(width=pregSz.W)
  val secret = Bool()
}

/**
 * IO bundle to interact with the issue unit
 *
 * @param issueWidth amount of operations that can be issued at once
 * @param numWakeupPorts number of wakeup ports for issue unit
 */
class IssueUnitIO(
  val issueWidth: Int,
  val numWakeupPorts: Int,
  val dispatchWidth: Int)
  (implicit p: Parameters) extends BoomBundle
  with CoreFuzzingConstants
{
  val dis_uops         = Vec(dispatchWidth, Flipped(Decoupled(new MicroOp)))

  val iss_valids       = Output(Vec(issueWidth, Bool()))
  val iss_uops         = Output(Vec(issueWidth, new MicroOp()))
  val wakeup_ports     = Flipped(Vec(numWakeupPorts, Valid(new IqWakeup(maxPregSz, ENABLE_IFT))))
  val pred_wakeup_port = Flipped(Valid(UInt(log2Ceil(ftqSz).W)))

  val spec_ld_wakeup   = Flipped(Vec(memWidth, Valid(new SpecLdWakeup(maxPregSz))))

  // tell the issue unit what each execution pipeline has in terms of functional units
  val fu_types         = Input(Vec(issueWidth, Bits(width=FUC_SZ.W)))
  // O1: aggregated occupancy taint of the non-pipelined FUs behind these ports.
  val cf_fu_busy       = Input(Valid(new Bundle {
    val op_count = UInt(uopIDCounterWidthCF.W)
    val is_atk   = Bool()
    val is_sec   = Bool()
    // [I2BUSYFU] (ruling 100, I-2) FU class(es) of the busy unit(s) (FU_DIV / FU_FDV), so a
    // denial is charged to the busy unit only when the slot needs that unit.  Constant per
    // unit, OR-ed over the busy ones by the aggregators (core.scala, fp-pipeline.scala).
    val fu_code  = UInt(FUC_SZ.W)
  }))

  val brupdate         = Input(new BrUpdateInfo())
  val flush_pipeline   = Input(Bool())
  val ld_miss          = Input(Bool())

  // corefuzzing: gate to enable speculative prints in issue unit/slots
  val cf_debug_issue_enable = Input(Bool())

  val event_empty      = Output(Bool()) // used by HPM events; is the issue unit empty?

  val tsc_reg          = Input(UInt(width=xLen.W))

  // corefuzzing: per-port contention updates — fires when a slot issues and had accumulated cross-domain denial cycles
  val cf_contention_upd = Output(Vec(issueWidth, Valid(new IssueContentionUpdate)))

  // 2-bit index into issueQueueEntryOptions for runtime issue queue size selection
  val cf_iq_idx = Input(UInt(2.W))

  // Task 3.4 (D18): kill-time contention records, one drain per cycle per IQ (the core
  // arbitrates the IQs onto the squash lane), and the per-cycle overflow flag (a slot killed
  // again while its previous record still waits: counted in 0xbce [15:0], never stalled, D6).
  val cf_kill_rec = Decoupled(new IftKillRecord)
  val cf_kill_ovf = Output(Bool())

  // [IQHEAD] patch 49 (owner r175, replaces the [IQFULL] step-16 occupancy OR summaries): the
  // INFL_IQ_FULL candidate names the queue's stalling occupant, its oldest slot, registered here
  // (one cycle old, like the ROB/LSU [HEADREG] heads).  The two summary ports are kept ONLY with
  // IFT off (constant 0 there, as before), so IFT-off Verilog is unchanged (r124); IFT on they are
  // gone (r91: no reader left).
  val cf_iq_sec_any = if (!ENABLE_IFT) Some(Output(Bool())) else None
  val cf_iq_atk_any = if (!ENABLE_IFT) Some(Output(Bool())) else None
  val cf_iq_head    = if (ENABLE_IFT) Some(Output(new IftIqHead)) else None
}

/**
 * Abstract top level issue unit
 *
 * @param numIssueSlots depth of issue queue
 * @param issueWidth amoutn of operations that can be issued at once
 * @param numWakeupPorts number of wakeup ports for issue unit
 * @param iqType type of issue queue (mem, int, fp)
 */
abstract class IssueUnit(
  val numIssueSlots: Int,
  val issueWidth: Int,
  val numWakeupPorts: Int,
  val iqType: BigInt,
  val dispatchWidth: Int)
  (implicit p: Parameters)
  extends BoomModule
  with IssueUnitConstants
  with freechips.rocketchip.util.CoreFuzzingConstants
{
  val io = IO(new IssueUnitIO(issueWidth, numWakeupPorts, dispatchWidth))

  // Runtime issue queue size selection.  Authoritative list lives in rocket-chip
  // util/CoreFuzzing.scala: issueQueueEntryOptions = Seq(32, 24, 16, 8).
  // Every option MUST be <= the queue's built numIssueSlots (32 here).  active_offset in
  // issue-unit-age-ordered.scala is `numIssueSlots.U - cf_iq_active`, an UNSIGNED subtract:
  // an option larger than the built size underflows and gates OFF every slot, freezing the
  // queue.  Verified in gen-collateral/IssueUnitCollapsing*.sv:
  //   _GEN = '{6'h8, 6'h10, 6'h18, 6'h20}   (packed literal: index 3 FIRST)
  //   => idx0..3 = 32, 24, 16, 8, and _active_offset_T = 6'h20 - _GEN[idx]  -- never underflows.
  // Do not restate the option values here without checking CoreFuzzing.scala.
  val iqOptionsVec = VecInit(issueQueueEntryOptions.map(_.U))
  val cf_iq_active = iqOptionsVec(io.cf_iq_idx)

  //-------------------------------------------------------------
  // Set up the dispatch uops
  // special case "storing" 2 uops within one issue slot.

  val dis_uops = Array.fill(dispatchWidth) {Wire(new MicroOp())}
  for (w <- 0 until dispatchWidth) {
    dis_uops(w) := io.dis_uops(w).bits
    dis_uops(w).iw_p1_poisoned := false.B
    dis_uops(w).iw_p2_poisoned := false.B
    dis_uops(w).iw_state := s_valid_1

    if (iqType == IQT_MEM.litValue || iqType == IQT_INT.litValue) {
      // For StoreAddrGen for Int, or AMOAddrGen, we go to addr gen state
      when ((io.dis_uops(w).bits.uopc === uopSTA && io.dis_uops(w).bits.lrs2_rtype === RT_FIX) ||
             io.dis_uops(w).bits.uopc === uopAMO_AG) {
        dis_uops(w).iw_state := s_valid_2
        // For store addr gen for FP, rs2 is the FP register, and we don't wait for that here
      } .elsewhen (io.dis_uops(w).bits.uopc === uopSTA && io.dis_uops(w).bits.lrs2_rtype =/= RT_FIX) {
        dis_uops(w).lrs2_rtype := RT_X
        dis_uops(w).prs2_busy  := false.B
      }
      dis_uops(w).prs3_busy := false.B
    } else if (iqType == IQT_FP.litValue) {
      // FP "StoreAddrGen" is really storeDataGen, and rs1 is the integer address register
      when (io.dis_uops(w).bits.uopc === uopSTA) {
        dis_uops(w).lrs1_rtype := RT_X
        dis_uops(w).prs1_busy  := false.B
      }
    }

    if (iqType != IQT_INT.litValue) {
      assert(!(io.dis_uops(w).bits.ppred_busy && io.dis_uops(w).valid))
      dis_uops(w).ppred_busy := false.B
    }
  }

  //-------------------------------------------------------------
  // Issue Table

  // [IQPEND] patch 41: one pending-secret broadcast port per issue port, compared as RT_FLT in the FP IQ
  val pendFlt = iqType == IQT_FP.litValue
  val slots = for (i <- 0 until numIssueSlots) yield { val slot = Module(new IssueSlot(numWakeupPorts, issueWidth, pendFlt)); slot }
  val issue_slots = VecInit(slots.map(_.io))

  for (i <- 0 until numIssueSlots) {
    issue_slots(i).wakeup_ports     := io.wakeup_ports
    issue_slots(i).pred_wakeup_port := io.pred_wakeup_port
    issue_slots(i).spec_ld_wakeup   := io.spec_ld_wakeup
    issue_slots(i).ldspec_miss      := io.ld_miss
    issue_slots(i).brupdate         := io.brupdate
    issue_slots(i).kill             := io.flush_pipeline
    // propagate cf_debug gate
    issue_slots(i).cf_debug_issue_enable := io.cf_debug_issue_enable
    // corefuzzing: default — no contention this cycle; subclass overrides in contention detection
    issue_slots(i).cf_contend_in.valid := false.B
    issue_slots(i).cf_contend_in.bits  := DontCare
    // Task 3.4: a dispatch-entry position carries no contention state; the collapsing
    // subclass overrides this beside in_uop.bits for the shifted positions.
    issue_slots(i).in_cntd := 0.U.asTypeOf(new IssueCntdCarry)
  }

  // Task 3.4 (D18): per-slot hold register for the kill-time contention record, written only
  // by its own slot (no compaction network), drained one per cycle by priority.  A slot
  // killed while its previous record still waits overwrites it and counts one overflow; the
  // depth is numIssueSlots per IQ by construction.  Nothing here touches dis_uops.ready or
  // the slot grant.  The valid bits reset; the payload registers have no reset.
  // [IQHEAD] patch 49: IFT off the kept summary ports stay constant 0; IFT on the head is driven by
  // the collapsing subclass (issue-unit-age-ordered.scala), default "no head" (0) here.
  io.cf_iq_sec_any.foreach(_ := false.B)
  io.cf_iq_atk_any.foreach(_ := false.B)
  io.cf_iq_head.foreach(_ := 0.U.asTypeOf(new IftIqHead))
  if (ENABLE_IFT) {
    // [IQPEND] patch 41 (owner r167, option A of p4-38-author.md; closes the IQ_FULL pending-taint
    // under-taint, p4-r3-t60t91t94.md A3).  Each issue port registers {valid, pdst} of the uop it
    // issued last cycle when that uop is secret at issue (s_prop incl. the slot's cf_src_secret, or
    // s_acc: the same estimate the fast wakeup carries, core.scala [WAKESEC]), writes a register of
    // this IQ's kind (int/mem: RT_FIX, FP: RT_FLT) and is NOT bypassable.  A bypassable producer's
    // consumers already get the secret from the fast wakeup in its issue cycle; a non-bypassable
    // one (div, mul, load, FP) wakes them only at writeback, and until then they were clean here.
    // Every slot of THIS IQ compares the broadcast with its sources (issue-slot.scala cf_pend_sec).
    //   levels: the D inputs hang off io.iss_uops (the select mux) beside the core's fast-wakeup
    //     compare, which goes much deeper from the same mux; the slot compare is register-sourced.
    //   cost: issueWidth x (pdst + 1) FF; slots x issueWidth x 2-3 source 8-bit compares.
    val pend_bc_v = RegInit(VecInit(Seq.fill(issueWidth)(false.B)))
    val pend_bc_p = Reg(Vec(issueWidth, UInt(maxPregSz.W)))
    for (w <- 0 until issueWidth) {
      val u = io.iss_uops(w)
      pend_bc_v(w) := io.iss_valids(w) && !u.bypassable && u.ldst_val &&
                      u.dst_rtype === (if (pendFlt) RT_FLT else RT_FIX) &&
                      (u.cf_secret_propagation.getOrElse(false.B) || u.cf_secret_access.getOrElse(false.B))
      pend_bc_p(w) := u.pdst
    }
    for (i <- 0 until numIssueSlots; w <- 0 until issueWidth) {
      issue_slots(i).cf_pend_bcast.get(w).valid := pend_bc_v(w)
      issue_slots(i).cf_pend_bcast.get(w).bits  := pend_bc_p(w)
    }
    // [IQHEAD] patch 49: the [IQFULL] OR summaries (and their pend_bc_v term, read only by them) are
    // removed; the slots' cf_pend_sec is now read by the head attribution (issue-unit-age-ordered).
  }

  io.cf_kill_rec.valid := false.B
  io.cf_kill_rec.bits  := DontCare
  io.cf_kill_ovf       := false.B
  if (ENABLE_IFT) {
    val kill_hold_val  = RegInit(VecInit(Seq.fill(numIssueSlots)(false.B)))
    val kill_hold_bits = Reg(Vec(numIssueSlots, new IftKillRecord))
    val kill_sel       = PriorityEncoderOH(kill_hold_val.asUInt)      // one drain per cycle per IQ
    io.cf_kill_rec.valid := kill_hold_val.asUInt.orR
    io.cf_kill_rec.bits  := Mux1H(kill_sel, kill_hold_bits)
    val kill_ovf = Wire(Vec(numIssueSlots, Bool()))
    for (i <- 0 until numIssueSlots) {
      val draining = io.cf_kill_rec.fire && kill_sel(i)
      val k = issue_slots(i).cf_kill_out
      kill_ovf(i) := k.valid && kill_hold_val(i) && !draining        // D6: counted, never stalled
      when (k.valid)         { kill_hold_val(i) := true.B; kill_hold_bits(i) := k.bits }
        .elsewhen (draining) { kill_hold_val(i) := false.B }
      if (ENABLE_CF_DEBUG_PRINTF) {
        when (kill_ovf(i)) { printf("[KILLOVF] iq=%d slot=%d\n", iqType.U, i.U) }
      }
    }
    // Task 3.9 (owner decision 2): one flop here, so the IQ grant (through cf_kill_rec.fire ->
    // draining) is not on the core's 0xbce counter path; the count lags the event by a cycle.
    io.cf_kill_ovf := RegNext(kill_ovf.asUInt.orR, false.B)
  }

  io.event_empty := !(issue_slots.map(s => s.valid).reduce(_|_))
  // corefuzzing: defaults — subclasses override with actual contention data
  for (w <- 0 until issueWidth) {
    io.cf_contention_upd(w).valid := false.B
    io.cf_contention_upd(w).bits  := DontCare
  }

  val count = PopCount(slots.map(_.io.valid))
  dontTouch(count)

  //-------------------------------------------------------------

  assert (PopCount(issue_slots.map(s => s.grant)) <= issueWidth.U, "[issue] window giving out too many grants.")


  //-------------------------------------------------------------


  def getType: String =
    if (iqType == IQT_INT.litValue) "int"
    else if (iqType == IQT_MEM.litValue) "mem"
    else if (iqType == IQT_FP.litValue) " fp"
    else "unknown"
}
