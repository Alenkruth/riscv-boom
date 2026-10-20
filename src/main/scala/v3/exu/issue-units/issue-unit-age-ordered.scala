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
import chisel3.util.{log2Ceil, PopCount, UIntToOH, Mux1H}

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.Str

import FUConstants._
import boom.v3.common._
import boom.v3.util.{appendModuleTag}
import freechips.rocketchip.util._

/**
 * Specific type of issue unit
 *
 * @param params issue queue params
 * @param numWakeupPorts number of wakeup ports for the issue queue
 */
class IssueUnitCollapsing(
  params: IssueParams,
  numWakeupPorts: Int)
  (implicit p: Parameters)
  extends IssueUnit(params.numEntries, params.issueWidth, numWakeupPorts, params.iqType, params.dispatchWidth)
  with CoreFuzzingConstants
{
  // corefuzzing
  // values obtained from IQType trait in consts.scala
  // val moduleTagCF = params.iqType match {
  //   case 4.U => fpissqTagCF
  //   case 2.U => memissqTagCF
  //   case 1.U => intissqTagCF
  // }
  val moduleTagCF = if (params.iqType == BigInt(4)) {
    // FP issue queue tag
    fpissqTagCF
  } else if (params.iqType == BigInt(2)) {
    // MEM issue queue tag
    memissqTagCF
  } else {
    // INT issue queue tag
    intissqTagCF
  }

  //-------------------------------------------------------------
  // Figure out how much to shift entries by
  val maxShift = dispatchWidth

  // corefuzzing: active slots are at the TOP of the queue (high indices), so they are adjacent
  // to the dispatch positions. Inactive slots at low indices are forced to appear non-vacant
  // so that shamts_oh does not get inflated and cause active slots to shift into inactive territory.
  // active_offset = first active slot index = numIssueSlots - cf_iq_active
  val active_offset = numIssueSlots.U - cf_iq_active

  // Inactive slots (i < active_offset) appear non-vacant to shamts_oh.
  val vacants = issue_slots.zipWithIndex.map { case (s, i) =>
    !(s.valid) && (i.U >= active_offset)
  } ++ io.dis_uops.map(_.valid).map(!_.asBool)
  val shamts_oh = Array.fill(numIssueSlots+dispatchWidth) {Wire(UInt(width=maxShift.W))}
  // track how many to shift up this entry by by counting previous vacant spots
  def SaturatingCounterOH(count_oh:UInt, inc: Bool, max: Int): UInt = {
     val next = Wire(UInt(width=max.W))
     next := count_oh
     when (count_oh === 0.U && inc) {
       next := 1.U
     } .elsewhen (!count_oh(max-1) && inc) {
       next := (count_oh << 1.U)
     }
     next
  }
  shamts_oh(0) := 0.U
  for (i <- 1 until numIssueSlots + dispatchWidth) {
    shamts_oh(i) := SaturatingCounterOH(shamts_oh(i-1), vacants(i-1), maxShift)
  }

  //-------------------------------------------------------------

  // which entries' uops will still be next cycle? (not being issued and vacated)
  // Gate inactive slots: their will_be_valid is always false (they never hold uops)
  val will_be_valid = (0 until numIssueSlots).map(i =>
                        issue_slots(i).will_be_valid && (i.U >= active_offset)) ++
                      (0 until dispatchWidth).map(i => io.dis_uops(i).valid &&
                                                        !dis_uops(i).exception &&
                                                        !dis_uops(i).is_fence &&
                                                        !dis_uops(i).is_fencei)

  val uops = issue_slots.map(s=>s.out_uop) ++ dis_uops.map(s=>s)
  // Task 3.4: the slot-local contention carry shifts beside the uop; dispatch positions
  // carry a zeroed one (the base-class default is the same zero).
  val cntds = issue_slots.map(s=>s.out_cntd) ++ Seq.fill(dispatchWidth)(0.U.asTypeOf(new IssueCntdCarry))
  for (i <- 0 until numIssueSlots) {
  issue_slots(i).in_uop.valid := false.B
  issue_slots(i).in_uop.bits  := uops(i+1)
  issue_slots(i).in_cntd      := cntds(i+1)
    // Gate: only active slots (i >= active_offset) receive new uops.
    // Active slots are at TOP indices so they are adjacent to dispatch positions.
    when (i.U >= active_offset) {
      for (j <- 1 to maxShift by 1) {
        when (shamts_oh(i+j) === (1 << (j-1)).U) {
          issue_slots(i).in_uop.valid := will_be_valid(i+j)
          issue_slots(i).in_uop.bits  := uops(i+j)
          issue_slots(i).in_cntd      := cntds(i+j)
          // corefuzzing: stamp issue-queue bitmap bit when uop enters slot
          issue_slots(i).in_uop.bits.cf_fu_bitmap.foreach(_ := uops(i+j).cf_fu_bitmap.getOrElse(0.U(numModules.W)) | (1.U << moduleTagCF.U))
        }
      }
    }
    issue_slots(i).clear        := shamts_oh(i) =/= 0.U
  }

  //-------------------------------------------------------------
  // Dispatch/Entry Logic
  // did we find a spot to slide the new dispatched uops into?

  // Gate slot availability to active range: only top slots (i >= active_offset) are available.
  val will_be_available = (0 until numIssueSlots).map(i =>
                            (!issue_slots(i).will_be_valid || issue_slots(i).clear) &&
                            !(issue_slots(i).in_uop.valid) &&
                            (i.U >= active_offset))
  val num_available = PopCount(will_be_available)
  for (w <- 0 until dispatchWidth) {
    io.dis_uops(w).ready := RegNext(num_available > w.U)
  }

  //-------------------------------------------------------------
  // Issue Select Logic

  // set default
  for (w <- 0 until issueWidth) {
    io.iss_valids(w) := false.B
    io.iss_uops(w)   := NullMicroOp
    // unsure if this is overkill
    io.iss_uops(w).prs1 := 0.U
    io.iss_uops(w).prs2 := 0.U
    io.iss_uops(w).prs3 := 0.U
    io.iss_uops(w).lrs1_rtype := RT_X
    io.iss_uops(w).lrs2_rtype := RT_X
  }

  val requests = issue_slots.map(s => s.request)
  val port_issued = Array.fill(issueWidth){Bool()}
  for (w <- 0 until issueWidth) {
    port_issued(w) = false.B
  }

  // Track which slot index won each port; used below to route cf_contend_out without a re-scan.
  val port_winner_idx = Array.fill(issueWidth)(WireInit(numIssueSlots.U(log2Ceil(numIssueSlots + 1).W)))

  // Pre-compute fu_code compatibility once; reused in grant pass and contention pass.
  val slot_can_use_port = Array.tabulate(numIssueSlots, issueWidth) { (i, w) =>
    (issue_slots(i).uop.fu_code & io.fu_types(w)) =/= 0.U
  }

  for (i <- 0 until numIssueSlots) {
    issue_slots(i).grant := false.B
    var uop_issued = false.B

    for (w <- 0 until issueWidth) {
      val can_allocate = slot_can_use_port(i)(w)

      when (requests(i) && !uop_issued && can_allocate && !port_issued(w)) {
        issue_slots(i).grant := true.B
        io.iss_valids(w) := true.B
        io.iss_uops(w) := issue_slots(i).uop
        port_winner_idx(w) := i.U
        // probably not necessary. Hence commented.
        // Tag the micro-op as it leaves the issue queue and is issued to an
        // execution unit. Keeping the append combinational avoids extra
        // cycles; it simply records passage through the issue queue.
        // io.iss_uops(w).appendModuleTag(intissqTagCF)
      }
      val was_port_issued_yet = port_issued(w)
      port_issued(w) = (requests(i) && !uop_issued && can_allocate) | port_issued(w)
      uop_issued = (requests(i) && can_allocate && !was_port_issued_yet) | uop_issued
    }
  }

  // corefuzzing: per-slot cross-domain issue contention detection
  // Default: no contention this cycle. Overridden below for losing slots.
  for (i <- 0 until numIssueSlots) {
    issue_slots(i).cf_contend_in.valid := false.B
    issue_slots(i).cf_contend_in.bits  := DontCare
  }
  // Pass 1: for each port, identify losing slots from a different domain and fire cf_contend_in.
  // All Bool computations are in the outer scope (not inside a when-block) so no Chisel scope
  // escaping occurs when the Scala vars are updated.
  // Scala-var priority ensures each slot fires cf_contend_in at most once per cycle (lowest port).
  val already_denied = Array.fill(numIssueSlots)(WireInit(false.B))
  // [FIFO10] step 10: every port's denial of each slot this cycle, and that port's winner is_atk,
  // so the slot counts the extra same-cycle denials as dropped and ORs all their is_atk.
  val cross_loser = Array.fill(numIssueSlots, issueWidth)(false.B)
  val port_atk    = Array.fill(issueWidth)(false.B)
  val port_sec    = Array.fill(issueWidth)(false.B)   // [R117SWEEP] patch 33
  for (w <- 0 until issueWidth) {
    // Compute winner info unconditionally; gate with io.iss_valids(w) via AND.
    val winner_domain = io.iss_uops(w).cf_domain_id.getOrElse(0.U(iftTagWidth.W))
    // [NARROW 2026-09-10] truncate explicitly to the contention field's width; the
      // ty=2 edge discarded these bits anyway (rob.scala:715), so cutting here keeps
      // it visible instead of relying on an implicit Chisel width truncation.
      val winner_op     = io.iss_uops(w).cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))(inflOpCountWidthCF-1, 0)
    // [WINATK] patch 66 (V10, owner 2026-10-19 16:3x; P6 full occupant bit): the winner is attacker when it is
    // attacker-owned OR attacker-influenced (io.iss_uops(w).cf_attacker_influence = the slot's dispatch seed
    // (window, packet tag, dispatch edges) || its wakeup operand taint cf_src_atk), as FU_BUSY ([I3BUSYATK])
    // already uses.  It also makes a same-domain (victim vs victim) loss a contention event, like a secret
    // winner.  The field comes through the same issue select as cf_domain_id (parallel): the domain-test LUT
    // goes from 4 to 5 inputs, 0 levels; is_cross_loser ends at the slot's cf_contend_in register.
    // More ISSUE_CONTENTION edges (slot pressure).  IFT off: both fields are None (constant 0).
    val winner_is_atk = io.iss_uops(w).cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 1.U ||
                        io.iss_uops(w).cf_attacker_influence.getOrElse(false.B)
    val winner_is_sec = io.iss_uops(w).cf_secret_access.getOrElse(false.B) || io.iss_uops(w).cf_secret_propagation.getOrElse(false.B)
    for (i <- 0 until numIssueSlots) {
      val can_use_port   = slot_can_use_port(i)(w)
      // Gate with io.iss_valids(w) here — all signals are in the outer scope.
      // Also fires when winner is same-domain but secret (same-domain secret contention).
      val is_cross_loser = io.iss_valids(w) && requests(i) && !issue_slots(i).grant &&
                           can_use_port && (issue_slots(i).uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= winner_domain || winner_is_sec ||
                                            winner_is_atk)   // [WINATK] patch 66
      when (is_cross_loser && !already_denied(i)) {
        issue_slots(i).cf_contend_in.valid                    := true.B
        issue_slots(i).cf_contend_in.bits.winner_op_count     := winner_op
        issue_slots(i).cf_contend_in.bits.winner_is_atk       := winner_is_atk
        issue_slots(i).cf_contend_in.bits.winner_is_sec       := winner_is_sec
        issue_slots(i).cf_contend_in.bits.infl_type           := INFL_ISSUE_CONTENTION.U
      }
      already_denied(i) = already_denied(i) | is_cross_loser
      cross_loser(i)(w) = is_cross_loser
    }
    port_atk(w) = winner_is_atk
    port_sec(w) = winner_is_sec
  }
  if (ENABLE_IFT) {
    for (i <- 0 until numIssueSlots) {
      issue_slots(i).cf_contend_in.bits.multi   := PopCount(cross_loser(i).toSeq) > 1.U
      issue_slots(i).cf_contend_in.bits.any_atk := (0 until issueWidth).map(w => cross_loser(i)(w) && port_atk(w)).reduce(_ || _)
      // [R117SWEEP] patch 33 (r117): the secret twin, same AND-OR shape beside any_atk (registered
      // in the slot; 0 levels on the select/grant path, which it does not feed)
      issue_slots(i).cf_contend_in.bits.any_sec.foreach(_ := (0 until issueWidth).map(w => cross_loser(i)(w) && port_sec(w)).reduce(_ || _))
    }
  }

  // O1: FU-occupancy denial.  A non-pipelined unit (div/fdivsqrt) holds its slot for
  // ~20-30 cycles; a slot needing that unit finds no usable port at all, so there is
  // no winner/loser pair and INFL_ISSUE_CONTENTION can never fire for it.
  if (ENABLE_IFT) {
    for (i <- 0 until numIssueSlots) {
      val no_port_available = !(0 until issueWidth).map(w => slot_can_use_port(i)(w)).reduce(_||_)
      // [I2BUSYFU] (ruling 100, I-2) only a slot that needs the busy unit is charged to it.  A
      // slot starved for another reason (I2F queue full, F2I busy, ...) also sees no port and
      // was named after the divider.  fu_code is a slot register, the mask a busy-unit OR:
      // one more input on the existing AND, no new level.
      val needs_busy_fu  = (issue_slots(i).uop.fu_code & io.cf_fu_busy.bits.fu_code) =/= 0.U
      val fu_busy_denial = issue_slots(i).request && !issue_slots(i).grant &&
                           no_port_available && io.cf_fu_busy.valid && needs_busy_fu
      when (fu_busy_denial && !already_denied(i)) {
        issue_slots(i).cf_contend_in.valid                := true.B
        issue_slots(i).cf_contend_in.bits.winner_op_count := io.cf_fu_busy.bits.op_count(inflOpCountWidthCF-1, 0)
        issue_slots(i).cf_contend_in.bits.winner_is_atk   := io.cf_fu_busy.bits.is_atk
        issue_slots(i).cf_contend_in.bits.winner_is_sec   := io.cf_fu_busy.bits.is_sec
        issue_slots(i).cf_contend_in.bits.infl_type       := INFL_FU_BUSY.U
        issue_slots(i).cf_contend_in.bits.multi           := false.B                  // [FIFO10] one occupant
        issue_slots(i).cf_contend_in.bits.any_atk         := io.cf_fu_busy.bits.is_atk // [FIFO10]
        issue_slots(i).cf_contend_in.bits.any_sec.foreach(_ := io.cf_fu_busy.bits.is_sec) // [R117SWEEP]
      }
    }
  }

  // [IQHEAD] patch 49 (owner r175: "IQ full should follow the same"): the IQ_FULL edge names the
  // queue's STALLING occupant, as ROB_FULL / LDQ_FULL / STQ_FULL name their head.  This queue
  // collapses toward slot 0 and fills from the top, so its oldest occupant sits at the lowest ACTIVE
  // slot, active_offset = numIssueSlots - issueQueueEntryOptions(cf_iq_idx): a FIXED slot per
  // option (4 options -> slots 0/8/16/24 at 32 entries), no search, no scan.  The one-cycle hole
  // after the head issues (the slot above shifts down next cycle) is covered by the slot above.
  // Registered here: io.cf_iq_head at cycle t = the head of t-1, the occupancy the dispatch ready
  // (RegNext(num_available > w)) of t was computed from.  Fields: oc = cf_op_count_id; atk =
  // attacker domain || cf_src_atk (dispatch seed + wakeup taint_atk); sec = s_acc || s_prop (io.uop,
  // incl. cf_src_secret) || cf_pend_sec (41).  No valid head: 0 (oc 0, clean).
  // Levels: slot registers -> per-slot field LUT (sec 3-in OR, atk 2-3-in) -> 2:1 hole mux -> 4:1
  // option mux (2-bit CSR index) -> FF: ~3 LUT, register to register, off every existing path.
  // Cost: (uopIDCounterWidthCF + 2) FF; ~8 x 18 LUT inputs of muxing (~40-55 LUT) per IQ.
  if (ENABLE_IFT) {
    def headOf(i: Int): IftIqHead = {
      val s = issue_slots(i)
      val h = Wire(new IftIqHead)
      h.oc  := s.uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
      h.atk := s.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 1.U || s.cf_src_atk.get
      h.sec := s.uop.cf_secret_access.getOrElse(false.B) || s.uop.cf_secret_propagation.getOrElse(false.B) ||
               s.cf_pend_sec.get
      h
    }
    val noHead = 0.U.asTypeOf(new IftIqHead)
    val optHeads = issueQueueEntryOptions.map { opt =>
      val o = (numIssueSlots - opt) max 0          // the active_offset of this option (never underflows)
      val n = (o + 1) min (numIssueSlots - 1)
      Mux(issue_slots(o).valid, headOf(o), Mux(issue_slots(n).valid, headOf(n), noHead))
    }
    io.cf_iq_head.get := RegNext(VecInit(optHeads)(io.cf_iq_idx), noHead)
  }

  // Route cf_contend_out from each port's winning slot.
  // UIntToOH safe: iss_valids(w)=true implies port_winner_idx(w) is a valid slot index.
  for (w <- 0 until issueWidth) {
    when (io.iss_valids(w)) {
      val sel = UIntToOH(port_winner_idx(w), numIssueSlots)
      val winner_out = Mux1H(sel, issue_slots.map(_.cf_contend_out))
      when (winner_out.valid) {
        io.cf_contention_upd(w).valid := true.B
        io.cf_contention_upd(w).bits  := winner_out.bits
      }
    }
  }
}
