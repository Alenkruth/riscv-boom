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

  // [WCAP] width CSR 0xbcc issue-grant cap (owner 2026-10-28, RULES C "Width CSR design"): this queue grants at most
  // cap = min(N, issueWidth) uops per cycle (INT 4 ports: N; MEM 2: 1 at N=1, else 2; FP 1: never capped, no input).
  // A stop-after-cap-grants term on the existing select; no FU/port remapping; the contention pass is unchanged.
  //  - Mode flops: one per binding cap k (k in coreWidthOptions, k < issueWidth: INT k = 2, 1; MEM k = 1), wcap_m(k) =
  //    RegNext(idx == index of k).  The CSR changes only with the queues empty (csrw is unique + flush_on_commit; DMI
  //    writes in debug halt), so the one-cycle lag is invisible.  INT 2 FF, MEM 1 FF.
  //  - Count: every grant takes one distinct port, so the grants made by the slots below slot i are exactly the set
  //    port_issued carries entering slot i (chain nodes that already exist; never io.iss_valids, which would close a
  //    combinational loop).  wcap_ok(i) = AND_k !(wcap_m(k) && atLeast_k(carries)): a 6-input LUT on INT
  //    (4 carries + 2 mode flops), a 3-input term on MEM.
  //  - Parallel form: the chain itself is NOT changed; wcap_ok(i) only qualifies slot i's grant and iss_valids
  //    outputs.  That equals gating the request at the front of the slot (the serial form): up to the cap-th grant the
  //    two chains are identical (fewer than cap grants below), and from there on the count stays >= cap, so the
  //    serial form blocks every later request and this form drops every later grant: same grants, same ports.  The
  //    ports a dropped slot would have taken are visible only to slots that are dropped too.  The oldest eligible
  //    slot always gets the first grant (all ports free), so the cap never starves it.
  //  - io.iss_uops(w) and port_winner_idx(w) keep the unqualified select (0 logic on the uop mux, the deepest
  //    select-to-wakeup path): with io.iss_valids(w) low no consumer reads them (fast/pred wakeup, regread, idiv
  //    back-to-back, jmp_pc_req, contention routing are all gated by the valid).  At N=4 (idx 0) every wcap_m is 0:
  //    cycle-identical to the stock select.  Reconf off / FP: wcap_ok is the literal true.B, the Verilog is unchanged.
  val wcap_m: Seq[(Int, Bool)] = io.cf_width_idx.map { idx =>
    coreWidthOptions.filter(_ < issueWidth).map(k => k -> RegNext(idx === coreWidthOptions.indexOf(k).U, false.B))
  }.getOrElse(Nil)
  def wcapAtLeast(c: Seq[Bool], k: Int): Bool = c.combinations(k).map(_.reduce(_ && _)).reduce(_ || _)
  def wcapOk(c: Seq[Bool]): Bool =
    if (wcap_m.isEmpty) true.B else wcap_m.map { case (k, m) => !(m && wcapAtLeast(c, k)) }.reduce(_ && _)
  val wcap_dbg   = wcap_m.nonEmpty && ENABLE_CF_DEBUG_PRINTF
  val wcap_g_unc = Array.fill(numIssueSlots)(false.B)   // debug only: the unqualified per-slot grant

  for (i <- 0 until numIssueSlots) {
    issue_slots(i).grant := false.B
    var uop_issued = false.B
    val wcap_ok = wcapOk(port_issued.toSeq)             // the carries entering slot i; true.B without the cap

    for (w <- 0 until issueWidth) {
      val can_allocate = slot_can_use_port(i)(w)
      if (wcap_dbg) wcap_g_unc(i) = wcap_g_unc(i) || (requests(i) && !uop_issued && can_allocate && !port_issued(w))

      when (requests(i) && !uop_issued && can_allocate && !port_issued(w)) {
        issue_slots(i).grant := wcap_ok
        io.iss_valids(w) := wcap_ok
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

  // [WCAP] debug-print builds only (Verilator; never FireSim / oracle synthesis): the cap is a hard invariant, and
  // +cf_wcap=1 prints one line per cycle in which the cap dropped a grant or more than one uop issued, plus a line
  // whenever the mode flops change (cap = the live cap of this queue; q = iqType: 1 INT, 2 MEM).
  if (wcap_dbg) {
    val wcapIss    = PopCount(io.iss_valids)
    val wcapCap    = wcap_m.foldLeft(issueWidth.U(3.W)) { case (acc, (k, m)) => Mux(m, k.U(3.W), acc) }
    val wcapCapped = PopCount((0 until numIssueSlots).map(i => wcap_g_unc(i) && !issue_slots(i).grant))
    for ((k, m) <- wcap_m) {
      assert(!(m && wcapIss > k.U), "[WCAP] issue queue granted more uops than the width cap")
    }
    assert(PopCount(wcap_m.map(_._2)) <= 1.U, "[WCAP] more than one width-cap mode flop set")
    val wcapPa = freechips.rocketchip.util.PlusArg("cf_wcap", 0, "[WCAP] width-cap debug prints (1 = on)", 1)(0)
    when (wcapPa && (wcapCapped =/= 0.U || wcapIss > 1.U)) {
      printf("[WCAP] c=%d q=%d cap=%d iss=%d capped=%d\n", io.tsc_reg, iqType.U, wcapCap, wcapIss, wcapCapped)
    }
    when (wcapPa && (wcapCap =/= RegNext(wcapCap, issueWidth.U(3.W)))) {
      printf("[WCAP-MODE] c=%d q=%d cap=%d\n", io.tsc_reg, iqType.U, wcapCap)
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
      val winner_op     = io.iss_uops(w).cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))(inflKeyWidthCF-1, 0)
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
      // [ATKVIC] stall-edge audit D5 (owner 2026-10-26: the F1 [VICGATE] rule extends to ISSUE_CONTENTION): the
      // winner's attacker label only on a VICTIM loser, and therefore no same-domain attacker-vs-attacker contention
      // event without a secret winner.  The loser's domain bit is already a leaf of this LUT: 0 FF, 0 LUT, 0 levels
      // (is_cross_loser ends at the slot's cf_contend_in register).  IFT off: domain 0 (unchanged).
      val winner_is_atk_i = winner_is_atk && issue_slots(i).uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U
      // Gate with io.iss_valids(w) here — all signals are in the outer scope.
      // Also fires when winner is same-domain but secret (same-domain secret contention).
      val is_cross_loser = io.iss_valids(w) && requests(i) && !issue_slots(i).grant &&
                           can_use_port && (issue_slots(i).uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= winner_domain || winner_is_sec ||
                                            winner_is_atk_i)   // [WINATK] patch 66, [ATKVIC]
      when (is_cross_loser && !already_denied(i)) {
        issue_slots(i).cf_contend_in.valid                    := true.B
        issue_slots(i).cf_contend_in.bits.winner_op_count     := winner_op
        issue_slots(i).cf_contend_in.bits.winner_is_atk       := winner_is_atk_i   // [ATKVIC]
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
      issue_slots(i).cf_contend_in.bits.any_atk := (0 until issueWidth).map(w => cross_loser(i)(w) && port_atk(w)).reduce(_ || _) &&
                                                   issue_slots(i).uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U   // [ATKVIC] D5
      // [R117SWEEP] patch 33 (r117): the secret twin, same AND-OR shape beside any_atk (registered
      // in the slot; 0 levels on the select/grant path, which it does not feed)
      issue_slots(i).cf_contend_in.bits.any_sec.foreach(_ := (0 until issueWidth).map(w => cross_loser(i)(w) && port_sec(w)).reduce(_ || _))
    }
  }

  // [V3PAUSE] SP-20 (record v3 design 6.2, 6.2.2 LOAD_PAUSE): in the pause cycle the core zeroes this (mem) IQ's
  // fu_types, so no ready slot can use a port; each such slot records {LOAD_PAUSE, key 0, atk 0, sec 0} through the
  // existing deny latch (newest denier + count; its dcy bucket counts the cycle).  No culprit register (V3-D14).
  // Placed before the FU_BUSY block, which wins on the (impossible for mem slots) overlap by last connect.  Mem IQ
  // only (Scala gate).  Levels: pause_mem is a 1-LUT function of two core flops; it joins the existing
  // request / no-port AND (the FU_BUSY shape), 0 added levels.
  if (ENABLE_IFT && iqType == IQT_MEM.litValue) {
    val v3pPause = io.cf_pause_mem.get
    for (i <- 0 until numIssueSlots) {
      val v3pNoPort = !(0 until issueWidth).map(w => slot_can_use_port(i)(w)).reduce(_ || _)
      when (v3pPause && issue_slots(i).request && v3pNoPort && !already_denied(i)) {
        issue_slots(i).cf_contend_in.valid                := true.B
        issue_slots(i).cf_contend_in.bits.winner_op_count := 0.U
        issue_slots(i).cf_contend_in.bits.winner_is_atk   := false.B
        issue_slots(i).cf_contend_in.bits.winner_is_sec   := false.B
        issue_slots(i).cf_contend_in.bits.infl_type       := INFL_LOAD_PAUSE.U
        issue_slots(i).cf_contend_in.bits.multi           := false.B
        issue_slots(i).cf_contend_in.bits.any_atk         := false.B
        issue_slots(i).cf_contend_in.bits.any_sec.foreach(_ := false.B)
      }
    }
    if (ENABLE_CF_DEBUG_PRINTF) {   // [V3PRINT] SP-22 (FT-7): the ready mem slots LOAD_PAUSE denied this cycle
      val pa = boom.v3.util.CfStatePlusArg(true)
      val dn = (0 until numIssueSlots).map(i => issue_slots(i).request &&
                 !(0 until issueWidth).map(w => slot_can_use_port(i)(w)).reduce(_ || _))
      when (pa && v3pPause) { printf("[EDGE-PAUSE] c=%d denied=%d\n", io.tsc_reg, PopCount(dn)) }
    }
    // [V3PAUSE-END] SP-22 [EDGE-PAUSE] print is inserted above this line
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
        issue_slots(i).cf_contend_in.bits.winner_op_count := io.cf_fu_busy.bits.op_count(inflKeyWidthCF-1, 0)
        // [ATKVIC] stall-edge audit D5: the occupant's attacker label only on a VICTIM denied slot (0 FF/LUT/levels)
        val fu_atk_i = io.cf_fu_busy.bits.is_atk && issue_slots(i).uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U
        issue_slots(i).cf_contend_in.bits.winner_is_atk   := fu_atk_i
        issue_slots(i).cf_contend_in.bits.winner_is_sec   := io.cf_fu_busy.bits.is_sec
        issue_slots(i).cf_contend_in.bits.infl_type       := INFL_FU_BUSY.U
        issue_slots(i).cf_contend_in.bits.multi           := false.B                  // [FIFO10] one occupant
        issue_slots(i).cf_contend_in.bits.any_atk         := fu_atk_i                 // [FIFO10] [ATKVIC]
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
      // [CNTDSUM] stall-edge audit D3(a): io.uop's attacker bit = cf_src_atk || the slot's latched contention / FU_BUSY
      // attacker winners (issue-slot.scala); same flop-sourced LUT, 0 levels.
      h.atk := s.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 1.U || s.uop.cf_attacker_influence.getOrElse(false.B)
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
