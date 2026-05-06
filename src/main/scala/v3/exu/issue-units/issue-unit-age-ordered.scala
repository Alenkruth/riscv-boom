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
  for (i <- 0 until numIssueSlots) {
  issue_slots(i).in_uop.valid := false.B
  issue_slots(i).in_uop.bits  := uops(i+1)
    // Gate: only active slots (i >= active_offset) receive new uops.
    // Active slots are at TOP indices so they are adjacent to dispatch positions.
    when (i.U >= active_offset) {
      for (j <- 1 to maxShift by 1) {
        when (shamts_oh(i+j) === (1 << (j-1)).U) {
          issue_slots(i).in_uop.valid := will_be_valid(i+j)
          issue_slots(i).in_uop.bits  := uops(i+j)
          // corefuzzing: stamp issue-queue bitmap bit when uop enters slot
          issue_slots(i).in_uop.bits.cf_fu_bitmap := uops(i+j).cf_fu_bitmap | (1.U << moduleTagCF.U)
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
  for (w <- 0 until issueWidth) {
    // Compute winner info unconditionally; gate with io.iss_valids(w) via AND.
    val winner_domain = io.iss_uops(w).cf_domain_id
    val winner_op     = io.iss_uops(w).cf_op_count_id
    val winner_is_atk = io.iss_uops(w).cf_domain_id === 1.U
    val winner_is_sec = io.iss_uops(w).cf_secret_access || io.iss_uops(w).cf_secret_propagation
    for (i <- 0 until numIssueSlots) {
      val can_use_port   = slot_can_use_port(i)(w)
      // Gate with io.iss_valids(w) here — all signals are in the outer scope.
      // Also fires when winner is same-domain but secret (same-domain secret contention).
      val is_cross_loser = io.iss_valids(w) && requests(i) && !issue_slots(i).grant &&
                           can_use_port && (issue_slots(i).uop.cf_domain_id =/= winner_domain || winner_is_sec)
      when (is_cross_loser && !already_denied(i)) {
        issue_slots(i).cf_contend_in.valid                    := true.B
        issue_slots(i).cf_contend_in.bits.winner_op_count     := winner_op
        issue_slots(i).cf_contend_in.bits.winner_is_atk       := winner_is_atk
        issue_slots(i).cf_contend_in.bits.winner_is_sec       := winner_is_sec
      }
      already_denied(i) = already_denied(i) | is_cross_loser
    }
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
