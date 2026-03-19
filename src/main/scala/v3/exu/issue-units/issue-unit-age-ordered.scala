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
import chisel3.util.{log2Ceil, PopCount}

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
  val vacants = issue_slots.map(s => !(s.valid)) ++ io.dis_uops.map(_.valid).map(!_.asBool)
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
  val will_be_valid = (0 until numIssueSlots).map(i => issue_slots(i).will_be_valid) ++
                      (0 until dispatchWidth).map(i => io.dis_uops(i).valid &&
                                                        !dis_uops(i).exception &&
                                                        !dis_uops(i).is_fence &&
                                                        !dis_uops(i).is_fencei)

  val uops = issue_slots.map(s=>s.out_uop) ++ dis_uops.map(s=>s)
  for (i <- 0 until numIssueSlots) {
  issue_slots(i).in_uop.valid := false.B
  issue_slots(i).in_uop.bits  := uops(i+1)
    for (j <- 1 to maxShift by 1) {
      when (shamts_oh(i+j) === (1 << (j-1)).U) {
        issue_slots(i).in_uop.valid := will_be_valid(i+j)
        issue_slots(i).in_uop.bits  := uops(i+j)
        // corefuzzing: stamp issue-queue bitmap bit when uop enters slot
        issue_slots(i).in_uop.bits.cf_fu_bitmap := uops(i+j).cf_fu_bitmap | (1.U << moduleTagCF.U)
      }
    }
    issue_slots(i).clear        := shamts_oh(i) =/= 0.U
  }

  //-------------------------------------------------------------
  // Dispatch/Entry Logic
  // did we find a spot to slide the new dispatched uops into?

  val will_be_available = (0 until numIssueSlots).map(i =>
                            (!issue_slots(i).will_be_valid || issue_slots(i).clear) && !(issue_slots(i).in_uop.valid))
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

  for (i <- 0 until numIssueSlots) {
    issue_slots(i).grant := false.B
    var uop_issued = false.B

    for (w <- 0 until issueWidth) {
      val can_allocate = (issue_slots(i).uop.fu_code & io.fu_types(w)) =/= 0.U

      when (requests(i) && !uop_issued && can_allocate && !port_issued(w)) {
        issue_slots(i).grant := true.B
        io.iss_valids(w) := true.B
        io.iss_uops(w) := issue_slots(i).uop
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
      val can_use_port   = (issue_slots(i).uop.fu_code & io.fu_types(w)) =/= 0.U
      // Gate with io.iss_valids(w) here — all signals are in the outer scope
      val is_cross_loser = io.iss_valids(w) && requests(i) && !issue_slots(i).grant &&
                           can_use_port && (issue_slots(i).uop.cf_domain_id =/= winner_domain)
      when (is_cross_loser && !already_denied(i)) {
        issue_slots(i).cf_contend_in.valid                    := true.B
        issue_slots(i).cf_contend_in.bits.winner_op_count     := winner_op
        issue_slots(i).cf_contend_in.bits.winner_is_atk       := winner_is_atk
        issue_slots(i).cf_contend_in.bits.winner_is_sec       := winner_is_sec
      }
      already_denied(i) = already_denied(i) | is_cross_loser
    }
  }

  // Pass 2: collect cf_contend_out from granted slots → output as per-port contention updates.
  // Re-scan to find which slot was granted on which port (mirrors the grant loop above).
  // Scala vars ensure priority (first matching slot per port wins).
  val port_winner_idx = Array.fill(issueWidth)(WireInit(numIssueSlots.U(log2Ceil(numIssueSlots + 1).W)))
  val port_assigned2  = Array.fill(issueWidth)(WireInit(false.B))
  for (i <- 0 until numIssueSlots) {
    var uop_seen2 = false.B
    for (w <- 0 until issueWidth) {
      val can_allocate2 = (issue_slots(i).uop.fu_code & io.fu_types(w)) =/= 0.U
      when (requests(i) && !uop_seen2 && can_allocate2 && !port_assigned2(w)) {
        port_winner_idx(w) := i.U
      }
      val was_port_assigned2 = port_assigned2(w)
      port_assigned2(w) = port_assigned2(w) | (requests(i) && !uop_seen2 && can_allocate2)
      uop_seen2 = uop_seen2 | (requests(i) && can_allocate2 && !was_port_assigned2)
    }
  }
  for (w <- 0 until issueWidth) {
    when (io.iss_valids(w)) {
      for (i <- 0 until numIssueSlots) {
        when (port_winner_idx(w) === i.U && issue_slots(i).cf_contend_out.valid) {
          io.cf_contention_upd(w).valid := true.B
          io.cf_contention_upd(w).bits  := issue_slots(i).cf_contend_out.bits
        }
      }
    }
  }
}
