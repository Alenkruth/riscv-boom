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
class IssueSlotIO(val numWakeupPorts: Int)(implicit p: Parameters) extends BoomBundle
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

  val wakeup_ports  = Flipped(Vec(numWakeupPorts, Valid(new IqWakeup(maxPregSz))))
  val pred_wakeup_port = Flipped(Valid(UInt(log2Ceil(ftqSz).W)))
  val spec_ld_wakeup = Flipped(Vec(memWidth, Valid(UInt(width=maxPregSz.W))))
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
class IssueSlot(val numWakeupPorts: Int)(implicit p: Parameters)
  extends BoomModule
  with IssueUnitConstants
  with CoreFuzzingConstants
{
  val io = IO(new IssueSlotIO(numWakeupPorts))

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
  val cf_cntd_winner_op  = Reg(UInt(uopIDCounterWidthCF.W))
  val cf_cntd_winner_atk = RegInit(false.B)
  val cf_cntd_winner_sec = RegInit(false.B)
  val cf_cntd_deny_count = RegInit(0.U(4.W))
  // Next-cycle combinational shadows — default to current register values.
  // A when-block below adds the cf_contend_in override.
  // out_uop uses nc_cntd_* so same-cycle denials are inherited across collapsing shifts.
  val nc_cntd_valid      = WireInit(cf_cntd_valid)
  val nc_cntd_winner_op  = WireInit(cf_cntd_winner_op)
  val nc_cntd_winner_atk = WireInit(cf_cntd_winner_atk)
  val nc_cntd_winner_sec = WireInit(cf_cntd_winner_sec)
  val nc_cntd_deny_count = WireInit(cf_cntd_deny_count)

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
    slot_uop.cf_speculated               := false.B
    slot_uop.cf_attacker_influence       := false.B
    slot_uop.cf_secret_transmission      := false.B
    slot_uop.cf_single_step              := false.B
    slot_uop.cf_fu_bitmap                := 0.U
    slot_uop.cf_src_tainted              := false.B
    slot_uop.cf_taint_producer_op        := 0.U
    slot_uop.cf_taint_producer_is_atk    := false.B
    slot_uop.cf_taint_producer_is_secret := false.B
    slot_uop.cf_spec_branch_is_atk       := false.B
    slot_uop.cf_spec_branch_op_id        := 0.U
    slot_uop.cf_spec_branch_is_secret    := false.B
    slot_uop.cf_infl_overflow            := false.B
    slot_uop.cf_influencer_list.foreach { e =>
      e.valid      := false.B
      e.op_count   := 0.U
      e.infl_type  := 0.U
      e.is_atk     := false.B
      e.is_secret  := false.B
      e.deny_count := 0.U
    }
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
    }
    when (io.wakeup_ports(i).valid &&
         (io.wakeup_ports(i).bits.pdst === next_uop.prs2)) {
      p2 := true.B
    }
    when (io.wakeup_ports(i).valid &&
         (io.wakeup_ports(i).bits.pdst === next_uop.prs3)) {
      p3 := true.B
    }
  }
  when (io.pred_wakeup_port.valid && io.pred_wakeup_port.bits === next_uop.ppred) {
    ppred := true.B
  }

  for (w <- 0 until memWidth) {
    assert (!(io.spec_ld_wakeup(w).valid && io.spec_ld_wakeup(w).bits === 0.U),
      "Loads to x0 should never speculatively wakeup other instructions")
  }

  // TODO disable if FP IQ.
  for (w <- 0 until memWidth) {
    when (io.spec_ld_wakeup(w).valid &&
      io.spec_ld_wakeup(w).bits === next_uop.prs1 &&
      next_uop.lrs1_rtype === RT_FIX) {
      p1 := true.B
      p1_poisoned := true.B
      assert (!next_p1_poisoned)
    }
    when (io.spec_ld_wakeup(w).valid &&
      io.spec_ld_wakeup(w).bits === next_uop.prs2 &&
      next_uop.lrs2_rtype === RT_FIX) {
      p2 := true.B
      p2_poisoned := true.B
      assert (!next_p2_poisoned)
    }
  }


  // Handle branch misspeculations
  val next_br_mask = GetNewBrMask(io.brupdate, slot_uop)

  // was this micro-op killed by a branch? if yes, we can't let it be valid if
  // we compact it into an other entry
  // Non-destructive speculative logging: print any slot that will be killed by a branch
  // corefuzzing
  when (IsKilledByBranch(io.brupdate, slot_uop)) {
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
  // Carry contention state through collapsing-queue shifts.
  // nc_cntd_* includes same-cycle denials so the destination slot inherits them on shift.
  io.out_uop.cf_cntd_valid      := nc_cntd_valid
  io.out_uop.cf_cntd_winner_op  := nc_cntd_winner_op
  io.out_uop.cf_cntd_winner_atk := nc_cntd_winner_atk
  io.out_uop.cf_cntd_winner_sec := nc_cntd_winner_sec
  io.out_uop.cf_cntd_deny_count := nc_cntd_deny_count

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

  when (io.cf_contend_in.valid) {
    when (!cf_cntd_valid) {
      nc_cntd_valid      := true.B
      nc_cntd_winner_op  := io.cf_contend_in.bits.winner_op_count
      nc_cntd_winner_atk := io.cf_contend_in.bits.winner_is_atk
      nc_cntd_winner_sec := io.cf_contend_in.bits.winner_is_sec
    }
    nc_cntd_deny_count := Mux(cf_cntd_deny_count === 15.U, 15.U, cf_cntd_deny_count + 1.U)
  }

  // Register update priority (last-connect wins in Chisel):
  //   1. io.in_uop.valid — load inherited state from the shifting/dispatching uop
  //   2. io.kill         — reset (branch misprediction / pipeline flush)
  //   3. otherwise       — advance to next-cycle values (accumulate denial)
  when (io.in_uop.valid) {
    cf_cntd_valid      := io.in_uop.bits.cf_cntd_valid
    cf_cntd_winner_op  := io.in_uop.bits.cf_cntd_winner_op
    cf_cntd_winner_atk := io.in_uop.bits.cf_cntd_winner_atk
    cf_cntd_winner_sec := io.in_uop.bits.cf_cntd_winner_sec
    cf_cntd_deny_count := io.in_uop.bits.cf_cntd_deny_count
  } .elsewhen (io.kill) {
    cf_cntd_valid      := false.B
    cf_cntd_deny_count := 0.U
    cf_cntd_winner_atk := false.B
    cf_cntd_winner_sec := false.B
  } .otherwise {
    cf_cntd_valid      := nc_cntd_valid
    cf_cntd_winner_op  := nc_cntd_winner_op
    cf_cntd_winner_atk := nc_cntd_winner_atk
    cf_cntd_winner_sec := nc_cntd_winner_sec
    cf_cntd_deny_count := nc_cntd_deny_count
  }

  // Output: fire when granted AND contention was recorded.
  // Uses registered values — grant and denial are mutually exclusive so registers are final.
  val is_granted = io.grant && ((state === s_valid_1) ||
    ((state === s_valid_2) && p1 && p2 && ppred))
  io.cf_contend_out.valid                  := is_granted && cf_cntd_valid
  io.cf_contend_out.bits.rob_idx           := slot_uop.rob_idx
  io.cf_contend_out.bits.winner_op_count   := cf_cntd_winner_op
  io.cf_contend_out.bits.winner_is_atk     := cf_cntd_winner_atk
  io.cf_contend_out.bits.winner_is_sec     := cf_cntd_winner_sec
  io.cf_contend_out.bits.deny_count        := cf_cntd_deny_count

  // debug outputs
  io.debug.p1 := p1
  io.debug.p2 := p2
  io.debug.p3 := p3
  io.debug.ppred := ppred
  io.debug.state := state
}
