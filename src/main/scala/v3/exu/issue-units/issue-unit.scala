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
  val winner_op_count = UInt(uopIDCounterWidthCF.W)
  val winner_is_atk   = Bool()
  val winner_is_sec   = Bool()
}

// corefuzzing: bundle sent from issue unit → ROB when a slot issues with accumulated contention
class IssueContentionUpdate(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val rob_idx         = UInt(robAddrSz.W)
  val winner_op_count = UInt(uopIDCounterWidthCF.W)
  val winner_is_atk   = Bool()
  val winner_is_sec   = Bool()
  val deny_count      = UInt(4.W)
}

// corefuzzing: bundle driven into each issue slot to notify it of a cross-domain winner
class IssueContendInput(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val winner_op_count = UInt(uopIDCounterWidthCF.W)
  val winner_is_atk   = Bool()
  val winner_is_sec   = Bool()
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
class IqWakeup(val pregSz: Int) extends Bundle
{
  val pdst = UInt(width=pregSz.W)
  val poisoned = Bool()
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
  val wakeup_ports     = Flipped(Vec(numWakeupPorts, Valid(new IqWakeup(maxPregSz))))
  val pred_wakeup_port = Flipped(Valid(UInt(log2Ceil(ftqSz).W)))

  val spec_ld_wakeup   = Flipped(Vec(memWidth, Valid(UInt(width=maxPregSz.W))))

  // tell the issue unit what each execution pipeline has in terms of functional units
  val fu_types         = Input(Vec(issueWidth, Bits(width=FUC_SZ.W)))

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

  // Runtime issue queue size selection: issueQueueEntryOptions = Seq(64, 32, 16, 8)
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

  val slots = for (i <- 0 until numIssueSlots) yield { val slot = Module(new IssueSlot(numWakeupPorts)); slot }
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
