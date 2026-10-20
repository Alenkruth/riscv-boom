//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Issue Slot Logic
//--------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Note: stores (and AMOs) are "broken down" into 2 uops, but stored within a single issue-slot.
// TODO XXX make a separate issueSlot for MemoryIssueSlots, and only they break apart stores.
// TODO Disable ldspec for FP queue.

package boom.v3.exu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.CoreFuzzingConstants

import boom.v3.common._
import boom.v3.util._
import FUConstants._

/**
 * IO bundle to interact with Issue slot
 *
 * @param numWakeupPorts number of wakeup ports for the slot
 */
class IssueSlotIO(val numWakeupPorts: Int, val numPendPorts: Int = 0)(implicit p: Parameters) extends BoomBundle
  with CoreFuzzingConstants
{
  val valid         = Output(Bool())
  val will_be_valid = Output(Bool()) // TODO code review, do we need this signal so explicitely?
  val request       = Output(Bool())
  val request_hp    = Output(Bool())
  val grant         = Input(Bool())

  val brupdate        = Input(new BrUpdateInfo())
  val kill          = Input(Bool()) // pipeline flush
  val clear         = Input(Bool()) // entry being moved elsewhere (not mutually exclusive with grant)
  val ldspec_miss   = Input(Bool()) // Previous cycle's speculative load wakeup was mispredicted.
  // corefuzzing: gate for speculative issue-slot prints
  val cf_debug_issue_enable = Input(Bool())
  // corefuzzing: per-slot issue contention — input from issue unit, output when granted
  val cf_contend_in  = Flipped(Valid(new IssueContendInput))
  val cf_contend_out = Output(Valid(new IssueContentionUpdate))
  // Task 3.4 (thin uop): the slot-local contention state travels on the collapsing shift
  // BESIDE the uop (in_cntd/out_cntd), not inside it; cf_kill_out is the kill-time record
  // (D18) of a denied uop that leaves the queue without issuing.
  val in_cntd        = Input(new IssueCntdCarry)
  val out_cntd       = Output(new IssueCntdCarry)
  val cf_kill_out    = Output(Valid(new IftKillRecord))
  // [IQPEND] patch 41 (owner r167, option A of p4-38-author.md): the issue unit's REGISTERED
  // issue-time secret broadcast (pdst of each port's last-cycle secret, non-bypassable issue) and
  // this slot's pending-secret bit for the IQ_FULL summary.  IFT off both are None.
  val cf_pend_bcast  = if (ENABLE_IFT && numPendPorts > 0) Some(Flipped(Vec(numPendPorts, Valid(UInt(maxPregSz.W))))) else None
  val cf_pend_sec    = if (ENABLE_IFT) Some(Output(Bool())) else None
  // [IQHEAD] patch 49 (r174/r175): this slot's attacker-influence bit, read only by the IQ_FULL head
  // attribution (issue-unit-age-ordered.scala).  IFT off None.
  val cf_src_atk     = if (ENABLE_IFT) Some(Output(Bool())) else None

  val wakeup_ports  = Flipped(Vec(numWakeupPorts, Valid(new IqWakeup(maxPregSz, ENABLE_IFT))))
  val pred_wakeup_port = Flipped(Valid(UInt(log2Ceil(ftqSz).W)))
  val spec_ld_wakeup = Flipped(Vec(memWidth, Valid(new SpecLdWakeup(maxPregSz))))
  val in_uop        = Flipped(Valid(new MicroOp())) // if valid, this WILL overwrite an entry!
  val out_uop   = Output(new MicroOp()) // the updated slot uop; will be shifted upwards in a collasping queue.
  val uop           = Output(new MicroOp()) // the current Slot's uop. Sent down the pipeline when issued.

  val debug = {
    val result = new Bundle {
      val p1 = Bool()
      val p2 = Bool()
      val p3 = Bool()
      val ppred = Bool()
      val state = UInt(width=2.W)
    }
    Output(result)
  }
}

/**
 * Single issue slot. Holds a uop within the issue queue
 *
 * @param numWakeupPorts number of wakeup ports
 */
class IssueSlot(val numWakeupPorts: Int, val numPendPorts: Int = 0, val pendFlt: Boolean = false)(implicit p: Parameters)
  extends BoomModule
  with IssueUnitConstants
  with CoreFuzzingConstants
{
  val io = IO(new IssueSlotIO(numWakeupPorts, numPendPorts))

  // slot invalid?
  // slot is valid, holding 1 uop
  // slot is valid, holds 2 uops (like a store)
  def is_invalid = state === s_invalid
  def is_valid = state =/= s_invalid

  val next_state      = Wire(UInt()) // the next state of this slot (which might then get moved to a new slot)
  val next_uopc       = Wire(UInt()) // the next uopc of this slot (which might then get moved to a new slot)
  val next_lrs1_rtype = Wire(UInt()) // the next reg type of this slot (which might then get moved to a new slot)
  val next_lrs2_rtype = Wire(UInt()) // the next reg type of this slot (which might then get moved to a new slot)

  val state = RegInit(s_invalid)
  val p1    = RegInit(false.B)
  val p2    = RegInit(false.B)
  val p3    = RegInit(false.B)
  val ppred = RegInit(false.B)

  // Poison if woken up by speculative load.
  // Poison lasts 1 cycle (as ldMiss will come on the next cycle).
  // SO if poisoned is true, set it to false!
  val p1_poisoned = RegInit(false.B)
  val p2_poisoned = RegInit(false.B)
  p1_poisoned := false.B
  p2_poisoned := false.B
  val next_p1_poisoned = Mux(io.in_uop.valid, io.in_uop.bits.iw_p1_poisoned, p1_poisoned)
  val next_p2_poisoned = Mux(io.in_uop.valid, io.in_uop.bits.iw_p2_poisoned, p2_poisoned)

  val slot_uop = RegInit(NullMicroOp())
  val next_uop = Mux(io.in_uop.valid, io.in_uop.bits, slot_uop)

  // corefuzzing: per-slot ISSUE_CONTENTION accumulation registers.
  // Declared here (before out_uop assignments) so they can be referenced in out_uop.
  val cf_cntd_valid      = RegInit(false.B)
  val cf_cntd_winner_op  = Reg(UInt(inflOpCountWidthCF.W))   // [NARROW] see micro-op.scala
  val cf_cntd_winner_atk = RegInit(false.B)
  val cf_cntd_winner_sec = RegInit(false.B)
  // O1: the influence type latched with the denial (ISSUE_CONTENTION or FU_BUSY)
  val cf_cntd_infl_type  = RegInit(INFL_ISSUE_CONTENTION.U(inflTypeWidthCF.W))
  // 6 bits linear here so the shared log bucket has real range above 32; the ROB
  // compresses to 3 bits once at capture rather than needing an encoder per slot.
  val cf_cntd_deny_count = RegInit(0.U(6.W))
  // [FIFO10] step 10: distinct winners replaced by a newer one (+ same-cycle extra denials),
  // saturating, and the OR of every denying winner's is_atk (R-FIFO / R-STICKY)
  val cf_cntd_dropped    = RegInit(0.U(2.W))
  val cf_cntd_any_atk    = RegInit(false.B)
  val cf_cntd_any_sec    = RegInit(false.B)   // [R117SWEEP] patch 33: OR of every denying winner's is_secret
  // Next-cycle combinational shadows — default to current register values.
  // A when-block below adds the cf_contend_in override.
  // out_uop uses nc_cntd_* so same-cycle denials are inherited across collapsing shifts.
  val nc_cntd_valid      = WireInit(cf_cntd_valid)
  val nc_cntd_winner_op  = WireInit(cf_cntd_winner_op)
  val nc_cntd_winner_atk = WireInit(cf_cntd_winner_atk)
  val nc_cntd_winner_sec = WireInit(cf_cntd_winner_sec)
  val nc_cntd_deny_count = WireInit(cf_cntd_deny_count)
  val nc_cntd_infl_type  = WireInit(cf_cntd_infl_type)   // [CNTDTYPE] finding (c)
  val nc_cntd_dropped    = WireInit(cf_cntd_dropped)     // [FIFO10]
  val nc_cntd_any_atk    = WireInit(cf_cntd_any_atk)     // [FIFO10]
  val nc_cntd_any_sec    = WireInit(cf_cntd_any_sec)     // [R117SWEEP]
  // C2 (corefuzzing): this slot's operand taint, learned from the WAKEUP -- the moment
  // the producer's destination register becomes ready, which is when DIFT defines the
  // tag.  Must be a slot REGISTER: next_uop is Mux(in_uop.valid, in_uop.bits, slot_uop),
  // a read-only OpResult.  Held so io.uop.cf_secret_propagation is correct AT ISSUE,
  // which is when the issue unit samples winner_is_sec for INFL_ISSUE_CONTENTION.
  val cf_src_secret = RegInit(false.B)
  // [SLOTSHIFT] Task 0.11: every wakeup / spec_ld_wakeup secret match of THIS cycle, taken
  // against next_uop (the incoming uop on a shift-in), kept apart from the held register so
  // a shift-in can OR it into the destination's load instead of losing it.
  val wk_src_secret = WireInit(false.B)
  val nc_src_secret = cf_src_secret || wk_src_secret
  // [IQPEND] patch 41 (owner r167): PENDING secret.  Set when a source operand matches a register
  // that a secret producer of this IQ issued for last cycle (io.cf_pend_bcast, registered in the
  // issue unit), so a chain uop waiting on a secret divide / multiply / load counts as secret in
  // the IQ_FULL summary while its producer executes.  Read ONLY by that summary (issue-unit.scala
  // cf_iq_sec_any; since patch 49 [IQHEAD] by the IQ_FULL head attribution instead): it never reaches io.uop, the wakeups, the contention winner or any record,
  // so on the immediate path the records' own s_prop is unchanged; only IQ_FULL edges (and their
  // r117 sticky s_prop) can be added.  (Second order, by design: that sticky s_prop on an uop that
  // later heads the ROB/LDQ/STQ in a full-queue stall feeds rob_head_is_secret -> ROB_FULL / C5;
  // review p4-41 M3.)  Sticky until the next occupant loads the slot (dispatch or shift-in, carried
  // on the shift beside the uop like cf_src_secret).  Compared against next_uop as the wakeups are.
  val cf_pend_sec = if (ENABLE_IFT) Some(RegInit(false.B)) else None
  val wk_pend_sec = if (ENABLE_IFT) Some(WireInit(false.B)) else None   // 41b: no IFT-off wire
  // [IQHEAD] patch 49 (owner r174 "mirror the secret side", r175 head attribution): ATTACKER
  // influence of the occupant, the twin of cf_src_secret.  Loaded with the incoming uop's
  // cf_attacker_influence (dispatch: the core's dispatch-time seed = fetch-time attacker influence ||
  // attacker-window membership, WITHOUT the dispatch-edge promotion, core.scala [IQHEAD]; shift: the
  // source slot's cf_src_atk carried on out_uop.cf_attacker_influence, [SLOTSHIFT] as for
  // cf_src_secret), then ORed with every wakeup attacker bit matched this cycle (late-arriving
  // attacker-tainted source).  Read ONLY by the IQ_FULL head (issue-unit-age-ordered.scala): never
  // io.uop (the slot_uop copy stays zeroed below), never a wakeup, the contention winner or a value
  // taint.  No kill clear (as cf_pend_sec, 41b): the head reads it under s.valid and the next
  // occupant reloads it.
  val cf_src_atk = if (ENABLE_IFT) Some(RegInit(false.B)) else None
  val wk_src_atk = if (ENABLE_IFT) Some(WireInit(false.B)) else None

  //-----------------------------------------------------------------------------
  // next slot state computation
  // compute the next state for THIS entry slot (in a collasping queue, the
  // current uop may get moved elsewhere, and a new uop can enter

  when (io.kill) {
    state := s_invalid
  } .elsewhen (io.in_uop.valid) {
    state := io.in_uop.bits.iw_state
  } .elsewhen (io.clear) {
    state := s_invalid
  } .otherwise {
    state := next_state
  }

  //-----------------------------------------------------------------------------
  // "update" state
  // compute the next state for the micro-op in this slot. This micro-op may
  // be moved elsewhere, so the "next_state" travels with it.

  // defaults
  next_state := state
  next_uopc := slot_uop.uopc
  next_lrs1_rtype := slot_uop.lrs1_rtype
  next_lrs2_rtype := slot_uop.lrs2_rtype

  when (io.kill) {
    next_state := s_invalid
  } .elsewhen ((io.grant && (state === s_valid_1)) ||
    (io.grant && (state === s_valid_2) && p1 && p2 && ppred)) {
    // try to issue this uop.
    when (!(io.ldspec_miss && (p1_poisoned || p2_poisoned))) {
      next_state := s_invalid
    }
  } .elsewhen (io.grant && (state === s_valid_2)) {
    when (!(io.ldspec_miss && (p1_poisoned || p2_poisoned))) {
      next_state := s_valid_1
      when (p1) {
        slot_uop.uopc := uopSTD
        next_uopc := uopSTD
        slot_uop.lrs1_rtype := RT_X
        next_lrs1_rtype := RT_X
      } .otherwise {
        slot_uop.lrs2_rtype := RT_X
        next_lrs2_rtype := RT_X
      }
    }
  }

  when (io.in_uop.valid) {
    slot_uop := io.in_uop.bits
    // IFT LUT optimization: zero pass-through cf_* fields not used inside the
    // issue slot or issue unit. The slot has standalone cf_cntd_* registers
    // (lines 114-118) for contention accumulation. The issue UNIT (issue-unit-
    // age-ordered.scala lines 188-201) reads cf_domain_id, cf_op_count_id,
    // cf_secret_access, cf_secret_propagation from slot_uop for cross-domain
    // contention attribution — these MUST be preserved. Everything else listed
    // here is pure pass-through to wb_resps, where the merge is additive
    // (zero wb_uop fields are no-ops; rob_uop holds dispatch-time IFT state).
    slot_uop.cf_attacker_influence.foreach(_ := false.B)
    slot_uop.cf_secret_transmission.foreach(_ := false.B)
    slot_uop.cf_fu_bitmap.foreach(_ := 0.U)
    slot_uop.cf_spec_branch_is_secret.foreach(_ := false.B)
    slot_uop.cf_mem_dataflow_atk.foreach(_ := false.B)   // [R124GATE2] Option field
    slot_uop.cf_mem_sec_dataflow.foreach(_ := false.B)
    assert (is_invalid || io.clear || io.kill, "trying to overwrite a valid issue slot.")
  }

  // Wakeup Compare Logic

  // these signals are the "next_p*" for the current slot's micro-op.
  // they are important for shifting the current slot_uop up to an other entry.
  val next_p1 = WireInit(p1)
  val next_p2 = WireInit(p2)
  val next_p3 = WireInit(p3)
  val next_ppred = WireInit(ppred)

  when (io.in_uop.valid) {
    p1 := !(io.in_uop.bits.prs1_busy)
    p2 := !(io.in_uop.bits.prs2_busy)
    p3 := !(io.in_uop.bits.prs3_busy)
    ppred := !(io.in_uop.bits.ppred_busy)
  }

  when (io.ldspec_miss && next_p1_poisoned) {
    assert(next_uop.prs1 =/= 0.U, "Poison bit can't be set for prs1=x0!")
    p1 := false.B
  }
  when (io.ldspec_miss && next_p2_poisoned) {
    assert(next_uop.prs2 =/= 0.U, "Poison bit can't be set for prs2=x0!")
    p2 := false.B
  }

  for (i <- 0 until numWakeupPorts) {
    when (io.wakeup_ports(i).valid &&
         (io.wakeup_ports(i).bits.pdst === next_uop.prs1)) {
      p1 := true.B
      if (ENABLE_IFT) {
        when (io.wakeup_ports(i).bits.secret &&
              (next_uop.lrs1_rtype === RT_FIX || next_uop.lrs1_rtype === RT_FLT)) {
          wk_src_secret := true.B
        }
        when (io.wakeup_ports(i).bits.atk.get &&   // [IQHEAD] patch 49
              (next_uop.lrs1_rtype === RT_FIX || next_uop.lrs1_rtype === RT_FLT)) {
          wk_src_atk.get := true.B
        }
      }
    }
    when (io.wakeup_ports(i).valid &&
         (io.wakeup_ports(i).bits.pdst === next_uop.prs2)) {
      p2 := true.B
      if (ENABLE_IFT) {
        when (io.wakeup_ports(i).bits.secret &&
              (next_uop.lrs2_rtype === RT_FIX || next_uop.lrs2_rtype === RT_FLT)) {
          wk_src_secret := true.B
        }
        when (io.wakeup_ports(i).bits.atk.get &&   // [IQHEAD] patch 49
              (next_uop.lrs2_rtype === RT_FIX || next_uop.lrs2_rtype === RT_FLT)) {
          wk_src_atk.get := true.B
        }
      }
    }
    when (io.wakeup_ports(i).valid &&
         (io.wakeup_ports(i).bits.pdst === next_uop.prs3)) {
      p3 := true.B
      if (ENABLE_IFT) {
        when (io.wakeup_ports(i).bits.secret && next_uop.frs3_en) {
          wk_src_secret := true.B
        }
        when (io.wakeup_ports(i).bits.atk.get && next_uop.frs3_en) {   // [IQHEAD] patch 49
          wk_src_atk.get := true.B
        }
      }
    }
  }
  when (io.pred_wakeup_port.valid && io.pred_wakeup_port.bits === next_uop.ppred) {
    ppred := true.B
  }

  for (w <- 0 until memWidth) {
    assert (!(io.spec_ld_wakeup(w).valid && io.spec_ld_wakeup(w).bits.pdst === 0.U),
      "Loads to x0 should never speculatively wakeup other instructions")
  }

  // TODO disable if FP IQ.
  for (w <- 0 until memWidth) {
    when (io.spec_ld_wakeup(w).valid &&
      io.spec_ld_wakeup(w).bits.pdst === next_uop.prs1 &&
      next_uop.lrs1_rtype === RT_FIX) {
      p1 := true.B
      p1_poisoned := true.B
      assert (!next_p1_poisoned)
      // [WAKESEC] a load-hit consumer can issue on this wakeup before the secret-bearing
      // memory wakeup arrives, so take the load's secret here (Phase 0 finding (b)).
      if (ENABLE_IFT) {
        when (io.spec_ld_wakeup(w).bits.secret) {
          wk_src_secret := true.B
        }
      }
    }
    when (io.spec_ld_wakeup(w).valid &&
      io.spec_ld_wakeup(w).bits.pdst === next_uop.prs2 &&
      next_uop.lrs2_rtype === RT_FIX) {
      p2 := true.B
      p2_poisoned := true.B
      assert (!next_p2_poisoned)
      if (ENABLE_IFT) {
        when (io.spec_ld_wakeup(w).bits.secret) {
          wk_src_secret := true.B   // [WAKESEC]
        }
      }
    }
  }


  // [IQPEND] patch 41: pending-secret match.  Sources: the broadcast pdst (a register) and
  // next_uop's prs/rtype (the same select the wakeup compares above use), so this D path is no
  // deeper than the existing wk_src_secret / p1 D paths (their fast-wakeup pdst comes through
  // the issue select in the same cycle; this one is registered).  It feeds only cf_pend_sec's D.
  if (ENABLE_IFT && numPendPorts > 0) {
    val rt  = if (pendFlt) RT_FLT else RT_FIX
    val hit = io.cf_pend_bcast.get.map { b =>
      b.valid && ((b.bits === next_uop.prs1 && next_uop.lrs1_rtype === rt) ||
                  (b.bits === next_uop.prs2 && next_uop.lrs2_rtype === rt) ||
                  (if (pendFlt) b.bits === next_uop.prs3 && next_uop.frs3_en else false.B))
    }
    wk_pend_sec.get := hit.reduce(_ || _)
  }

  // Handle branch misspeculations
  val next_br_mask = GetNewBrMask(io.brupdate, slot_uop)

  // was this micro-op killed by a branch? if yes, we can't let it be valid if
  // we compact it into an other entry
  // Non-destructive speculative logging: print any slot that will be killed by a branch
  // corefuzzing
  when (IsKilledByBranch(io.brupdate, slot_uop)) {
    // Compile-time gated (ENABLE_CF_DEBUG_PRINTF). The " x%d/f%d" printfs are part of
    // this log and are gated with it. `next_state := s_invalid` is functional, not gated.
    if (ENABLE_CF_DEBUG_PRINTF) {
      // Print only if the slot currently holds a valid uop
      when (is_valid) {
        // Match commit log format from exu/core.scala but tag as speculative
        // Modified: use new overload to include MicroOp so cf_* fields are printed
        // Old call (kept for reference):
        // SpeculativePrintf.dump("ISSUE", Sext.apply(slot_uop.debug_pc(vaddrBits-1,0), xLen), slot_uop.debug_inst, slot_uop.is_rvc, io.cf_debug_issue_enable)
        SpeculativePrintf.dump("ISSUE", Sext.apply(slot_uop.debug_pc(vaddrBits-1,0), xLen), slot_uop.debug_inst, slot_uop.is_rvc, io.cf_debug_issue_enable, slot_uop)
        when (slot_uop.dst_rtype === RT_FIX && slot_uop.ldst =/= 0.U) {
          // No writeback data available at issue-slot; print a placeholder 0
          printf(" x%d 0x%x\n", slot_uop.ldst, 0.U)
        } .elsewhen (slot_uop.dst_rtype === RT_FLT) {
          printf(" f%d 0x%x\n", slot_uop.ldst, 0.U)
        }
      }
    }
    next_state := s_invalid
  }

  when (!io.in_uop.valid) {
    slot_uop.br_mask := next_br_mask
  }

  //-------------------------------------------------------------
  // Request Logic
  io.request := is_valid && p1 && p2 && p3 && ppred && !io.kill
  val high_priority = slot_uop.is_br || slot_uop.is_jal || slot_uop.is_jalr
  io.request_hp := io.request && high_priority

  when (state === s_valid_1) {
    io.request := p1 && p2 && p3 && ppred && !io.kill
  } .elsewhen (state === s_valid_2) {
    io.request := (p1 || p2) && ppred && !io.kill
  } .otherwise {
    io.request := false.B
  }

  //assign outputs
  io.valid := is_valid
  io.uop := slot_uop
  if (ENABLE_IFT) {
    io.uop.cf_secret_propagation.foreach(_ := slot_uop.cf_secret_propagation.getOrElse(false.B) || cf_src_secret)
  }
  io.uop.iw_p1_poisoned := p1_poisoned
  io.uop.iw_p2_poisoned := p2_poisoned

  // micro-op will vacate due to grant.
  val may_vacate = io.grant && ((state === s_valid_1) || (state === s_valid_2) && p1 && p2 && ppred)
  val squash_grant = io.ldspec_miss && (p1_poisoned || p2_poisoned)
  io.will_be_valid := is_valid && !(may_vacate && !squash_grant)

  io.out_uop            := slot_uop
  io.out_uop.iw_state   := next_state
  io.out_uop.uopc       := next_uopc
  io.out_uop.lrs1_rtype := next_lrs1_rtype
  io.out_uop.lrs2_rtype := next_lrs2_rtype
  io.out_uop.br_mask    := next_br_mask
  io.out_uop.prs1_busy  := !p1
  io.out_uop.prs2_busy  := !p2
  io.out_uop.prs3_busy  := !p3
  io.out_uop.ppred_busy := !ppred
  io.out_uop.iw_p1_poisoned := p1_poisoned
  io.out_uop.iw_p2_poisoned := p2_poisoned
  // Carry contention state through collapsing-queue shifts on the side carry (Task 3.4).
  // nc_cntd_* includes same-cycle denials so the destination slot inherits them on shift.
  io.out_cntd.valid      := nc_cntd_valid
  io.out_cntd.winner_op  := nc_cntd_winner_op
  io.out_cntd.winner_atk := nc_cntd_winner_atk
  io.out_cntd.winner_sec := nc_cntd_winner_sec
  io.out_cntd.deny_count := nc_cntd_deny_count
  io.out_cntd.infl_type  := nc_cntd_infl_type    // [CNTDTYPE] carry the type on the shift
  io.out_cntd.dropped    := nc_cntd_dropped      // [FIFO10]
  io.out_cntd.any_atk    := nc_cntd_any_atk      // [FIFO10]
  io.out_cntd.any_sec.foreach(_ := nc_cntd_any_sec)   // [R117SWEEP]
  // [IQPEND] the leaving uop's registered bit; a match in the shift cycle is taken by the
  // destination against its in_uop (next_uop), as for cf_src_secret ([SLOTSHIFT]).
  io.out_cntd.pend_sec.foreach(_ := cf_pend_sec.get)
  io.cf_pend_sec.foreach(_ := cf_pend_sec.get)
  io.cf_src_atk.foreach(_ := cf_src_atk.get)   // [IQHEAD] patch 49
  if (ENABLE_IFT) {
    // [SLOTSHIFT] the leaving uop is slot_uop: its own registered secret, not next_uop's
    // (next_uop is the INCOMING uop when this slot is refilled in the same cycle); a wakeup
    // arriving this cycle is caught by the destination slot against its in_uop.
    io.out_uop.cf_secret_propagation.foreach(_ := slot_uop.cf_secret_propagation.getOrElse(false.B) || cf_src_secret)
    // [IQHEAD] patch 49: the leaving uop's registered attacker bit rides the shift the same way (the
    // slot_uop copy of cf_attacker_influence is zeroed on load, so this field is otherwise 0 and
    // out_uop is read only by the destination slot's in_uop).
    io.out_uop.cf_attacker_influence.foreach(_ := cf_src_atk.get)
  }

  when (state === s_valid_2) {
    when (p1 && p2 && ppred) {
      ; // send out the entire instruction as one uop
    } .elsewhen (p1 && ppred) {
      io.uop.uopc := slot_uop.uopc
      io.uop.lrs2_rtype := RT_X
    } .elsewhen (p2 && ppred) {
      io.uop.uopc := uopSTD
      io.uop.lrs1_rtype := RT_X
    }
  }

  // corefuzzing: per-slot ISSUE_CONTENTION accumulation
  // nc_cntd_* Wires declared above (near slot_uop) default to current register values.
  // Drive them conditionally from cf_contend_in so same-cycle denials are captured.
  //
  // IFT compile-time gate: when ENABLE_IFT=false the denial-accumulation path is
  // elided.  cf_cntd_* registers have no write driver from cf_contend_in; their
  // only remaining writes come from io.in_uop propagation (always-false inputs)
  // and the kill/otherwise branches, so FIRRTL constant-propagates all cf_cntd_*
  // values to false/0 and DCE removes the registers entirely.
  if (ENABLE_IFT) {
    when (io.cf_contend_in.valid) {
      // [FIFO10] step 10 (owner ruling 93, R-FIFO): a one-entry list of the NEWEST denying
      // winner.  Was `when (!cf_cntd_valid)`: the first winner was kept and every later one
      // refused uncounted.  A replaced DISTINCT winner (op_count or type differs; the same
      // winner denying again is the same influence) counts as dropped, and so does every
      // extra same-cycle denial (`multi`).  R-STICKY: any_atk ORs every winner's is_atk.
      val cin      = io.cf_contend_in.bits
      val replaces = cf_cntd_valid && (cf_cntd_winner_op =/= cin.winner_op_count || cf_cntd_infl_type =/= cin.infl_type)
      nc_cntd_valid      := true.B
      nc_cntd_winner_op  := cin.winner_op_count
      nc_cntd_winner_atk := cin.winner_is_atk
      nc_cntd_winner_sec := cin.winner_is_sec
      nc_cntd_infl_type  := cin.infl_type
      nc_cntd_dropped    := SatDropped(cf_cntd_dropped, replaces.asUInt +& cin.multi.asUInt)
      nc_cntd_any_atk    := cf_cntd_any_atk || cin.any_atk || cin.winner_is_atk
      // [R117SWEEP] patch 33 (r117, DF-29b-ic): a replaced secret winner keeps its s_prop summary
      nc_cntd_any_sec    := cf_cntd_any_sec || cin.any_sec.getOrElse(false.B) || cin.winner_is_sec
      nc_cntd_deny_count := Mux(cf_cntd_deny_count === 63.U, 63.U, cf_cntd_deny_count + 1.U)
    }
  }

  // Register update priority (last-connect wins in Chisel):
  //   1. io.in_uop.valid — load inherited state from the shifting/dispatching uop
  //   2. io.kill         — reset (branch misprediction / pipeline flush)
  //   3. otherwise       — advance to next-cycle values (accumulate denial)
  when (io.in_uop.valid) {
    cf_cntd_valid      := io.in_cntd.valid
    cf_cntd_winner_op  := io.in_cntd.winner_op
    cf_cntd_winner_atk := io.in_cntd.winner_atk
    cf_cntd_winner_sec := io.in_cntd.winner_sec
    // [SLOTSHIFT] on a shift in_uop.bits is the source slot's out_uop, whose
    // cf_secret_propagation already ORs in that slot's registered cf_src_secret (above), so
    // the source's secret arrives here without a carry bit (Task 3.9 removed the redundant
    // IssueCntdCarry.src_secret); a dispatching uop brings its dispatch-time taint.
    cf_src_secret      := io.in_uop.bits.cf_secret_propagation.getOrElse(false.B) || wk_src_secret
    cf_cntd_deny_count := io.in_cntd.deny_count
    cf_cntd_infl_type  := io.in_cntd.infl_type   // [CNTDTYPE]
    cf_cntd_dropped    := io.in_cntd.dropped     // [FIFO10]
    cf_cntd_any_atk    := io.in_cntd.any_atk     // [FIFO10]
    cf_cntd_any_sec    := io.in_cntd.any_sec.getOrElse(false.B)   // [R117SWEEP]
  } .elsewhen (io.kill) {
    cf_cntd_valid      := false.B
    cf_cntd_deny_count := 0.U
    cf_cntd_dropped    := 0.U                    // [FIFO10]
    cf_cntd_any_atk    := false.B                // [FIFO10]
    cf_cntd_any_sec    := false.B                // [R117SWEEP]
    cf_cntd_winner_atk := false.B
    cf_cntd_winner_sec := false.B
    cf_src_secret      := false.B
  } .otherwise {
    cf_cntd_valid      := nc_cntd_valid
    cf_cntd_winner_op  := nc_cntd_winner_op
    cf_cntd_winner_atk := nc_cntd_winner_atk
    cf_cntd_winner_sec := nc_cntd_winner_sec
    cf_src_secret      := nc_src_secret
    cf_cntd_deny_count := nc_cntd_deny_count
    cf_cntd_infl_type  := nc_cntd_infl_type                  // [CNTDTYPE]
    cf_cntd_dropped    := nc_cntd_dropped                    // [FIFO10]
    cf_cntd_any_atk    := nc_cntd_any_atk                    // [FIFO10]
    cf_cntd_any_sec    := nc_cntd_any_sec                    // [R117SWEEP]
  }
  // [IQPEND] patch 41 (41b, review p4-41 M6): load on a shift-in / dispatch, else accumulate.  No
  // kill clear: after io.kill the slot is invalid, the IQ_FULL summary gates every slot with
  // s.valid, and the next occupant overwrites the bit through the in_uop load, so a clear on kill
  // was dead logic (one fewer D input per slot).
  cf_pend_sec.foreach { r =>
    r := Mux(io.in_uop.valid, io.in_cntd.pend_sec.get, r) || wk_pend_sec.get }
  // [IQHEAD] patch 49: load on a shift-in / dispatch, else accumulate (as cf_pend_sec).
  cf_src_atk.foreach { r =>
    r := Mux(io.in_uop.valid, io.in_uop.bits.cf_attacker_influence.getOrElse(false.B), r) || wk_src_atk.get }

  // Output: fire when granted AND contention was recorded.
  // Uses registered values — grant and denial are mutually exclusive so registers are final.
  val is_granted = io.grant && ((state === s_valid_1) ||
    ((state === s_valid_2) && p1 && p2 && ppred))
  io.cf_contend_out.valid                  := is_granted && cf_cntd_valid
  io.cf_contend_out.bits.rob_idx           := slot_uop.rob_idx
  io.cf_contend_out.bits.winner_op_count   := cf_cntd_winner_op
  io.cf_contend_out.bits.winner_is_atk     := cf_cntd_winner_atk
  io.cf_contend_out.bits.winner_is_sec     := cf_cntd_winner_sec
  // O1: default influence is issue-PORT contention.  A non-pipelined FU blocking
  // issue is reported as INFL_FU_BUSY by the issue unit instead (see below).
  io.cf_contend_out.bits.infl_type         := cf_cntd_infl_type
  io.cf_contend_out.bits.deny_count        := cf_cntd_deny_count
  io.cf_contend_out.bits.dropped           := cf_cntd_dropped    // [FIFO10]
  io.cf_contend_out.bits.any_atk           := cf_cntd_any_atk    // [FIFO10]
  io.cf_contend_out.bits.any_sec.foreach(_ := cf_cntd_any_sec)   // [R117SWEEP]

  // Task 3.4 (D18): a denied uop killed in the slot (flush or branch) never reaches
  // cf_contend_out, so its contention state would be lost with it.  Emit it here, once, from
  // the slot that holds it in the kill cycle; the issue unit holds it per slot and drains one
  // per cycle (no stall, counted overflow).  killed_now is two levels from the brupdate flops
  // and the slot registers and only drives the hold register's enable.
  io.cf_kill_out.valid           := false.B
  io.cf_kill_out.bits            := DontCare
  if (ENABLE_IFT) {
    // Owner ruling 91 (Task 3.4 note 1): a uop granted in its kill cycle already sends its
    // contention on cf_contend_out, so the kill record is gated off then (one record, not two).
    val killed_now = (io.kill || IsKilledByBranch(io.brupdate, slot_uop)) && is_valid && cf_cntd_valid && !is_granted
    io.cf_kill_out.valid           := killed_now
    io.cf_kill_out.bits.op_count   := slot_uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
    io.cf_kill_out.bits.pc         := slot_uop.debug_pc(31, 0)
    io.cf_kill_out.bits.domain     := slot_uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W))
    io.cf_kill_out.bits.winner_op  := cf_cntd_winner_op
    io.cf_kill_out.bits.infl_type  := cf_cntd_infl_type
    io.cf_kill_out.bits.winner_atk := cf_cntd_winner_atk
    io.cf_kill_out.bits.winner_sec := cf_cntd_winner_sec
    io.cf_kill_out.bits.deny_count := cf_cntd_deny_count
    io.cf_kill_out.bits.dropped    := cf_cntd_dropped    // [FIFO10]
    io.cf_kill_out.bits.any_atk    := cf_cntd_any_atk    // [FIFO10]
    io.cf_kill_out.bits.any_sec.foreach(_ := cf_cntd_any_sec)   // [R117SWEEP]
  }

  // debug outputs
  io.debug.p1 := p1
  io.debug.p2 := p2
  io.debug.p3 := p3
  io.debug.ppred := ppred
  io.debug.state := state
}
