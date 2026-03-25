//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Re-order Buffer
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Bank the ROB, such that each "dispatch" group gets its own row of the ROB,
// and each instruction in the dispatch group goes to a different bank.
// We can compress out the PC by only saving the high-order bits!
//
// ASSUMPTIONS:
//    - dispatch groups are aligned to the PC.
//
// NOTES:
//    - Currently we do not compress out bubbles in the ROB.
//    - Exceptions are only taken when at the head of the commit bundle --
//      this helps deal with loads, stores, and refetch instructions.

package boom.v3.exu

import scala.math.ceil

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._

import boom.v3.common._
import boom.v3.util._

/**
 * IO bundle to interact with the ROB
 *
 * @param numWakeupPorts number of wakeup ports to the rob
 * @param numFpuPorts number of fpu ports that will write back fflags
 */
class RobIo(
  val numWakeupPorts: Int,
  val numFpuPorts: Int
  )(implicit p: Parameters)  extends BoomBundle
  with CoreFuzzingConstants
{
  // Decode Stage
  // (Allocate, write instruction to ROB).
  val enq_valids       = Input(Vec(coreWidth, Bool()))
  val enq_uops         = Input(Vec(coreWidth, new MicroOp()))
  val enq_partial_stall= Input(Bool()) // we're dispatching only a partial packet,
                                       // and stalling on the rest of it (don't
                                       // advance the tail ptr)

  val xcpt_fetch_pc = Input(UInt(vaddrBitsExtended.W))

  val rob_tail_idx       = Output(UInt(robAddrSz.W))
  val rob_pnr_idx        = Output(UInt(robAddrSz.W))
  val rob_head_idx       = Output(UInt(robAddrSz.W))
  val rob_head_op_count  = Output(UInt(uopIDCounterWidthCF.W))
  val rob_head_domain    = Output(UInt(1.W))   // cf_domain_id of ROB head entry
  val rob_head_is_secret = Output(Bool())      // ROB head had s_acc=1 or s_prop=1
  // corefuzzing: cycle-N mispredicting branch UOP (same cycle as b1.mispredict_mask)
  val cf_mispredict_uop  = Input(Valid(new MicroOp))

  // Handle Branch Misspeculations
  val brupdate = Input(new BrUpdateInfo())

  // Write-back Stage
  // (Update of ROB)
  // Instruction is no longer busy and can be committed
  val wb_resps = Flipped(Vec(numWakeupPorts, Valid(new ExeUnitResp(xLen max fLen+1))))

  // Unbusying ports for stores.
  // +1 for fpstdata
  val lsu_clr_bsy           = Input(Vec(memWidth + 1, Valid(UInt(robAddrSz.W))))
  // corefuzzing: accumulated cf_fu_bitmap from LSU (dtlb+dcache+stq bits) for store commit
  val lsu_clr_bsy_cf_bitmap = Input(Vec(memWidth + 1, UInt(numModules.W)))
  // corefuzzing: cf_secret_transmission for stores, from LSU
  val lsu_clr_bsy_cf_stx    = Input(Vec(memWidth + 1, Bool()))

  // Port for unmarking loads/stores as speculation hazards..
  val lsu_clr_unsafe   = Input(Vec(memWidth, Valid(UInt(robAddrSz.W))))


  // Track side-effects for debug purposes.
  // Also need to know when loads write back, whereas we don't need loads to unbusy.
  val debug_wb_valids = Input(Vec(numWakeupPorts, Bool()))
  val debug_wb_wdata  = Input(Vec(numWakeupPorts, Bits(xLen.W)))

  val fflags = Flipped(Vec(numFpuPorts, new ValidIO(new FFlagsResp())))
  val lxcpt = Input(Valid(new Exception())) // LSU
  val csr_replay = Input(Valid(new Exception()))

  // Commit stage (free resources; also used for rollback).
  val commit = Output(new CommitSignals())

  // tell the LSU that the head of the ROB is a load
  // (some loads can only execute once they are at the head of the ROB).
  val com_load_is_at_rob_head = Output(Bool())

  // Communicate exceptions to the CSRFile
  val com_xcpt = Valid(new CommitExceptionSignals())

  // Let the CSRFile stall us (e.g., wfi).
  val csr_stall = Input(Bool())

  // Flush signals (including exceptions, pipeline replays, and memory ordering failures)
  // to send to the frontend for redirection.
  val flush = Valid(new CommitExceptionSignals)

  // Stall Decode as appropriate
  val empty = Output(Bool())
  val ready = Output(Bool()) // ROB is busy unrolling rename state...

  // Stall the frontend if we know we will redirect the PC
  val flush_frontend = Output(Bool())


  val debug_tsc = Input(UInt(xLen.W))

  // corefuzzing: issue contention update — fires from issue units when an instruction issues
  // with accumulated cross-domain denial cycles; ROB applies addInfluencer to the entry.
  // FP issue unit lives inside fp_pipeline (not in issue_units in core.scala), so only
  // sum non-FP IQ widths. IQT_FP.litValue == BigInt(4).
  val cf_issue_contention_upd = Input(Vec(issueParams.filter(_.iqType != BigInt(4)).map(_.issueWidth).sum, Valid(new IssueContentionUpdate)))

  // corefuzzing: direct ROB update to set cf_secret_access at address-compute (TLB) time.
  // Fires when LSU TLB detects the effective address falls in the secret range, before
  // the dcache response — ensures s_acc is visible in [FLUSH] logs for speculative loads
  // killed before the dcache responds.
  val cf_lsu_s_acc_upd = Input(Vec(memWidth, Valid(new CF_SAccUpdate)))

  // corefuzzing: direct ROB update to set cf_secret_propagation for in-flight instructions
  // whose physical source registers are secret-tainted, discovered at writeback time.
  // One port per integer writeback port (numWakeupPorts); unused ports stay valid=false.
  val cf_s_prop_rob_upd = Input(Vec(numWakeupPorts, Valid(new CF_SAccUpdate)))

  // corefuzzing changes
  val cf_debug_rob_enable = Input(Bool())
  val cf_rob_entries = Input(UInt(log2Ceil(robEntryOptions.length).W))
  // Pulse to reset ROB head/tail to 0 on quiesce drain so WrapInc wraps correctly
  // after a cf_rob_entries CSR change (ROB is empty when this fires)
  val cf_rob_quiesce_reset = Input(Bool())
}

/**
 * Bundle to send commit signals across processor
 */
class CommitSignals(implicit p: Parameters) extends BoomBundle
{
  val valids      = Vec(retireWidth, Bool()) // These instructions may not correspond to an architecturally executed insn
  val arch_valids = Vec(retireWidth, Bool())
  val uops        = Vec(retireWidth, new MicroOp())
  val fflags      = Valid(UInt(5.W))

  // These come a cycle later
  val debug_insts = Vec(retireWidth, UInt(32.W))

  // Perform rollback of rename state (in conjuction with commit.uops).
  val rbk_valids = Vec(retireWidth, Bool())
  val rollback   = Bool()

  val debug_wdata = Vec(retireWidth, UInt(xLen.W))
}

/**
 * Bundle to communicate exceptions to CSRFile
 *
 * TODO combine FlushSignals and ExceptionSignals (currently timed to different cycles).
 */
class CommitExceptionSignals(implicit p: Parameters) extends BoomBundle
{
  val ftq_idx    = UInt(log2Ceil(ftqSz).W)
  val edge_inst  = Bool()
  val is_rvc     = Bool()
  val pc_lob     = UInt(log2Ceil(icBlockBytes).W)
  val cause      = UInt(xLen.W)
  val badvaddr   = UInt(xLen.W)
// The ROB needs to tell the FTQ if there's a pipeline flush (and what type)
// so the FTQ can drive the frontend with the correct redirected PC.
  val flush_typ  = FlushTypes()
}

/**
 * Tell the frontend the type of flush so it can set up the next PC properly.
 */
object FlushTypes
{
  def SZ = 3
  def apply() = UInt(SZ.W)
  def none = 0.U
  def xcpt = 1.U // An exception occurred.
  def eret = (2+1).U // Execute an environment return instruction.
  def refetch = 2.U // Flush and refetch the head instruction.
  def next = 4.U // Flush and fetch the next instruction.

  def useCsrEvec(typ: UInt): Bool = typ(0) // typ === xcpt.U || typ === eret.U
  def useSamePC(typ: UInt): Bool = typ === refetch
  def usePCplus4(typ: UInt): Bool = typ === next

  def getType(valid: Bool, i_xcpt: Bool, i_eret: Bool, i_refetch: Bool): UInt = {
    val ret =
      Mux(!valid, none,
      Mux(i_eret, eret,
      Mux(i_xcpt, xcpt,
      Mux(i_refetch, refetch,
        next))))
    ret
  }
}

/**
 * Bundle of signals indicating that an exception occurred
 */
class Exception(implicit p: Parameters) extends BoomBundle
{
  val uop = new MicroOp()
  val cause = Bits(log2Ceil(freechips.rocketchip.rocket.Causes.all.max+2).W)
  val badvaddr = UInt(coreMaxAddrBits.W)
}

/**
 * Bundle for debug ROB signals
 * These should not be synthesized!
 */
class DebugRobSignals(implicit p: Parameters) extends BoomBundle
{
  val state = UInt()
  val rob_head = UInt(robAddrSz.W)
  val rob_pnr = UInt(robAddrSz.W)
  val xcpt_val = Bool()
  val xcpt_uop = new MicroOp()
  val xcpt_badvaddr = UInt(xLen.W)
}

/**
 * Reorder Buffer to keep track of dependencies and inflight instructions
 *
 * @param numWakeupPorts number of wakeup ports to the ROB
 * @param numFpuPorts number of FPU units that will write back fflags
 */
class Rob(
  val numWakeupPorts: Int,
  val numFpuPorts: Int
  )(implicit p: Parameters) extends BoomModule
  with CoreFuzzingConstants
{
  val io = IO(new RobIo(numWakeupPorts, numFpuPorts))

  // ROB Finite State Machine
  val s_reset :: s_normal :: s_rollback :: s_wait_till_empty :: Nil = Enum(4)
  val rob_state = RegInit(s_reset)

  // corefuzzing values
  val optionsVec = VecInit(robEntryOptions.map(_.U))
  def entryCount = optionsVec(io.cf_rob_entries)

  // Make sure entryCount is a multiple of coreWidth ALWAYS
  def cf_rob_rows = entryCount/coreWidth.U

  // TODO - Add this assertion in fuzzer to remove the extra logic generation
  assert(entryCount % coreWidth.U === 0.U, "ROB entryCount must be a multiple of coreWidth")
  assert(cf_rob_rows <= numRobRows.U, "cf_rob_rows exceeds loop bound")
  assert(cf_rob_rows =/= 0.U, "cf_rob_rows is zero")

  //commit entries at the head, and unwind exceptions from the tail
  val rob_head     = RegInit(0.U(log2Ceil(numRobRows).W))
  val rob_head_lsb = RegInit(0.U((1 max log2Ceil(coreWidth)).W)) // TODO: Accurately track head LSB (currently always 0)
  val rob_head_idx = if (coreWidth == 1) rob_head else Cat(rob_head, rob_head_lsb)

  val rob_tail     = RegInit(0.U(log2Ceil(numRobRows).W))
  val rob_tail_lsb = RegInit(0.U((1 max log2Ceil(coreWidth)).W))
  val rob_tail_idx = if (coreWidth == 1) rob_tail else Cat(rob_tail, rob_tail_lsb)

  val rob_pnr      = RegInit(0.U(log2Ceil(numRobRows).W))
  val rob_pnr_lsb  = RegInit(0.U((1 max log2Ceil(coreWidth)).W))
  val rob_pnr_idx  = if (coreWidth == 1) rob_pnr  else Cat(rob_pnr , rob_pnr_lsb)

  val com_idx = Mux(rob_state === s_rollback, rob_tail, rob_head)


  val maybe_full   = RegInit(false.B)
  val full         = Wire(Bool())
  val empty        = Wire(Bool())

  val will_commit         = Wire(Vec(coreWidth, Bool()))
  val can_commit          = Wire(Vec(coreWidth, Bool()))
  val can_throw_exception = Wire(Vec(coreWidth, Bool()))

  val rob_pnr_unsafe      = Wire(Vec(coreWidth, Bool())) // are the instructions at the pnr unsafe?
  val rob_head_vals       = Wire(Vec(coreWidth, Bool())) // are the instructions at the head valid?
  val rob_tail_vals       = Wire(Vec(coreWidth, Bool())) // are the instructions at the tail valid? (to track partial row dispatches)
  val rob_head_uses_stq   = Wire(Vec(coreWidth, Bool()))
  val rob_head_uses_ldq   = Wire(Vec(coreWidth, Bool()))
  val rob_head_fflags     = Wire(Vec(coreWidth, UInt(freechips.rocketchip.tile.FPConstants.FLAGS_SZ.W)))

  // for core fuzzing
  val cf_rob_head_uop     = Wire(Vec(coreWidth, new MicroOp()))

  val exception_thrown = Wire(Bool())

  // exception info
  // TODO compress xcpt cause size. Most bits in the middle are zero.
  val r_xcpt_val       = RegInit(false.B)
  val r_xcpt_uop       = Reg(new MicroOp())
  val r_xcpt_badvaddr  = Reg(UInt(coreMaxAddrBits.W))
  io.flush_frontend := r_xcpt_val

  //--------------------------------------------------
  // Utility

  def GetRowIdx(rob_idx: UInt): UInt = {
    if (coreWidth == 1) return rob_idx
    else return rob_idx >> log2Ceil(coreWidth).U
  }
  def GetBankIdx(rob_idx: UInt): UInt = {
    if(coreWidth == 1) { return 0.U }
    else           { return rob_idx(log2Ceil(coreWidth)-1, 0).asUInt }
  }

  // **************************************************************************
  // Debug

  class DebugRobBundle extends BoomBundle
  {
    val valid      = Bool()
    val busy       = Bool()
    val unsafe     = Bool()
    val uop        = new MicroOp()
    val exception  = Bool()
  }
  val debug_entry = Wire(Vec(numRobEntries, new DebugRobBundle))
  debug_entry := DontCare // override in statements below

  // **************************************************************************
  // --------------------------------------------------------------------------
  // **************************************************************************

  // Contains all information the PNR needs to find the oldest instruction which can't be safely speculated past.
  val rob_unsafe_masked = WireInit(VecInit(Seq.fill(numRobRows << log2Ceil(coreWidth)){false.B}))

  // Used for trace port, for debug purposes only
  val rob_debug_inst_mem   = SyncReadMem(numRobRows, Vec(coreWidth, UInt(32.W)))
  val rob_debug_inst_wmask = WireInit(VecInit(0.U(coreWidth.W).asBools))
  val rob_debug_inst_wdata = Wire(Vec(coreWidth, UInt(32.W)))
  rob_debug_inst_mem.write(rob_tail, rob_debug_inst_wdata, rob_debug_inst_wmask)
  val rob_debug_inst_rdata = rob_debug_inst_mem.read(rob_head, will_commit.reduce(_||_))

  val rob_fflags    = Seq.fill(coreWidth)(Reg(Vec(numRobRows, UInt(freechips.rocketchip.tile.FPConstants.FLAGS_SZ.W))))

  for (w <- 0 until coreWidth) {
    def MatchBank(bank_idx: UInt): Bool = (bank_idx === w.U)

    // one bank
    val rob_val       = RegInit(VecInit(Seq.fill(numRobRows){false.B}))
    val rob_bsy       = Reg(Vec(numRobRows, Bool()))
    val rob_unsafe    = Reg(Vec(numRobRows, Bool()))
    val rob_uop       = Reg(Vec(numRobRows, new MicroOp()))
    val rob_exception = Reg(Vec(numRobRows, Bool()))
    val rob_predicated = Reg(Vec(numRobRows, Bool())) // Was this instruction predicated out?

    val rob_debug_wdata = Mem(numRobRows, UInt(xLen.W))

    // corefuzzing: pending tables for serialized influencer list writes.
    // Replaces direct N-port writes to rob_uop influencer lists with a
    // 1-drain-per-cycle pattern, reducing ROB register file MUX tree complexity.
    //   sprob: one pending bit per row (fixed content: INFL_REG_DATAFLOW, is_secret=true)
    //   ic:    pending bit + variable winner data per row (INFL_ISSUE_CONTENTION)
    val sprob_infl_pending    = RegInit(VecInit(Seq.fill(numRobRows)(false.B)))
    val ic_pending_valid      = RegInit(VecInit(Seq.fill(numRobRows)(false.B)))
    val ic_pending_winner_op  = Reg(Vec(numRobRows, UInt(uopIDCounterWidthCF.W)))
    val ic_pending_winner_atk = Reg(Vec(numRobRows, Bool()))
    val ic_pending_winner_sec = Reg(Vec(numRobRows, Bool()))
    val ic_pending_deny_cnt   = Reg(Vec(numRobRows, UInt(4.W)))

    //-----------------------------------------------
    // Dispatch: Add Entry to ROB

    rob_debug_inst_wmask(w) := io.enq_valids(w)
    rob_debug_inst_wdata(w) := io.enq_uops(w).debug_inst

    when (io.enq_valids(w)) {
      rob_val(rob_tail)       := true.B
      rob_bsy(rob_tail)       := !(io.enq_uops(w).is_fence ||
                                   io.enq_uops(w).is_fencei)
      rob_unsafe(rob_tail)    := io.enq_uops(w).unsafe
      // corefuzzing: stamp robTagCF bit into cf_fu_bitmap at enqueue
      rob_uop(rob_tail)       := io.enq_uops(w)
      rob_uop(rob_tail).cf_fu_bitmap := io.enq_uops(w).cf_fu_bitmap | (1.U << robTagCF.U)
      rob_exception(rob_tail) := io.enq_uops(w).exception
      rob_predicated(rob_tail)   := false.B
      rob_fflags(w)(rob_tail)    := 0.U

      assert (rob_val(rob_tail) === false.B, "[rob] overwriting a valid entry.")
      assert ((io.enq_uops(w).rob_idx >> log2Ceil(coreWidth)) === rob_tail)
    } .elsewhen (io.enq_valids.reduce(_|_) && !rob_val(rob_tail)) {
      rob_uop(rob_tail).debug_inst := BUBBLE // just for debug purposes
    }

    //-----------------------------------------------
    // Writeback

    for (i <- 0 until numWakeupPorts) {
      val wb_resp = io.wb_resps(i)
      val wb_uop = wb_resp.bits.uop
      val row_idx = GetRowIdx(wb_uop.rob_idx)
      // corefuzzing: guard with rob_val AND rob_bsy to suppress stale FU wakeups
      // from branch-killed instructions.  After a branch misprediction, rob_val
      // is cleared for killed entries but rob_bsy is NOT cleared.  The FU can
      // complete in-flight and send a late writeback.  By that time the ROB slot
      // may have been reused for a new instruction Y that:
      //   (a) is still in-flight  (rob_val=1, rob_bsy=1) — pdst check below catches this
      //   (b) has already written back (rob_val=1, rob_bsy=0) — guard drops it here
      //   (c) is invalid           (rob_val=0)              — guard drops it here
      // Only update when the slot is both live (rob_val=1) and still busy (rob_bsy=1).
      when (wb_resp.valid && MatchBank(GetBankIdx(wb_uop.rob_idx)) && rob_val(row_idx) && rob_bsy(row_idx)) {
        rob_bsy(row_idx)         := false.B
        rob_unsafe(row_idx)      := false.B
        rob_predicated(row_idx)  := wb_resp.bits.predicated
      }
      // TODO check that fflags aren't overwritten
      // TODO check that the wb is to a valid ROB entry, give it a time stamp
//        assert (!(wb_resp.valid && MatchBank(GetBankIdx(wb_uop.rob_idx)) &&
//                  wb_uop.fp_val && !(wb_uop.is_load || wb_uop.is_store) &&
//                  rob_exc_cause(row_idx) =/= 0.U),
//                  "FP instruction writing back exc bits is overriding an existing exception.")
    }

    // Stores have a separate method to clear busy bits
    for (((clr_rob_idx, cf_bmap), cf_stx) <- io.lsu_clr_bsy.zip(io.lsu_clr_bsy_cf_bitmap).zip(io.lsu_clr_bsy_cf_stx)) {
      when (clr_rob_idx.valid && MatchBank(GetBankIdx(clr_rob_idx.bits))) {
        val cidx = GetRowIdx(clr_rob_idx.bits)
        rob_bsy(cidx)    := false.B
        rob_unsafe(cidx) := false.B
        // corefuzzing: merge accumulated store cf_fu_bitmap (stq+dtlb+dcache bits) into rob_uop
        rob_uop(cidx).cf_fu_bitmap := rob_uop(cidx).cf_fu_bitmap | cf_bmap
        // corefuzzing: set cf_secret_transmission if store writes secret-derived data to non-secret addr
        when (cf_stx) { rob_uop(cidx).cf_secret_transmission := true.B }
        assert (rob_val(cidx) === true.B, "[rob] store writing back to invalid entry.")
        assert (rob_bsy(cidx) === true.B, "[rob] store writing back to a not-busy entry.")
      }
    }
    for (clr <- io.lsu_clr_unsafe) {
      when (clr.valid && MatchBank(GetBankIdx(clr.bits))) {
        val cidx = GetRowIdx(clr.bits)
        rob_unsafe(cidx) := false.B
      }
    }

    // corefuzzing: issue contention update — instead of writing directly to the ROB
    // influencer list (6 ports × numRobRows × 140 bits = large MUX tree), record in a
    // compact per-row pending table and drain 1 entry per cycle in the background.
    for (upd <- io.cf_issue_contention_upd) {
      when (upd.valid && MatchBank(GetBankIdx(upd.bits.rob_idx))) {
        val cidx = GetRowIdx(upd.bits.rob_idx)
        when (rob_val(cidx)) {
          ic_pending_valid(cidx)      := true.B
          ic_pending_winner_op(cidx)  := upd.bits.winner_op_count
          ic_pending_winner_atk(cidx) := upd.bits.winner_is_atk
          ic_pending_winner_sec(cidx) := upd.bits.winner_is_sec
          ic_pending_deny_cnt(cidx)   := upd.bits.deny_count
        }
      }
    }

    // corefuzzing: direct s_acc update from LSU TLB stage — fires at address-compute time,
    // before dcache response, so speculative secret loads show s_acc=1 in [FLUSH] logs.
    for (upd <- io.cf_lsu_s_acc_upd) {
      when (upd.valid && MatchBank(GetBankIdx(upd.bits.rob_idx))) {
        val cidx = GetRowIdx(upd.bits.rob_idx)
        // Guard against ROB slot reuse: only apply if the slot still holds the
        // same instruction (same op_count_id) that generated this TLB-stage update.
        // A squashed instruction can have its slot reallocated before the update
        // arrives; without this guard the new instruction inherits a stale s_acc=1.
        when (rob_val(cidx) && rob_uop(cidx).cf_op_count_id === upd.bits.op_count_id) {
          rob_uop(cidx).cf_secret_access := true.B
        }
      }
    }

    // corefuzzing: s_prop direct ROB update — set cf_secret_propagation on in-flight
    // instructions whose physical sources were secret-tainted (discovered at writeback time).
    // The 1-bit cf_secret_propagation write is kept as N-port (cheap); the influencer list
    // write (was 8 ports × numRobRows × 140 bits) is replaced by a pending bit and drained
    // 1 entry per cycle in the background.  op_count_id guard prevents stale updates.
    for (upd <- io.cf_s_prop_rob_upd) {
      when (upd.valid && MatchBank(GetBankIdx(upd.bits.rob_idx))) {
        val cidx = GetRowIdx(upd.bits.rob_idx)
        when (rob_val(cidx) && rob_uop(cidx).cf_op_count_id === upd.bits.op_count_id) {
          rob_uop(cidx).cf_secret_propagation := true.B  // 1-bit: cheap N-port write
          sprob_infl_pending(cidx) := true.B              // defer influencer write to drain
        }
      }
    }

    // -----------------------------------------------
    // Background drain: one influencer list write per pending table per cycle.
    // Uses PriorityEncoderOH to pick one entry; clears pending bit after write.
    // Timing correctness: since collect writes set pending bits as registers, the
    // drain reads old (pre-collect) values and cannot fire for an entry in the same
    // cycle its pending bit is set — guaranteed 1-cycle gap before first drain.
    val sprob_drain_oh  = PriorityEncoderOH(sprob_infl_pending.asUInt)
    val sprob_drain_idx = OHToUInt(sprob_drain_oh)
    val sprob_drain_any = sprob_infl_pending.asUInt.orR

    val ic_drain_oh   = PriorityEncoderOH(ic_pending_valid.asUInt)
    val ic_drain_idx  = OHToUInt(ic_drain_oh)
    val ic_drain_any  = ic_pending_valid.asUInt.orR
    // Suppress ic drain when it would target the same ROB row as the sprob drain.
    // Both use PopCount(valid) as the base slot; same row → same base → same slot → conflict.
    val ic_drain_fires = ic_drain_any && !(sprob_drain_any && ic_drain_idx === sprob_drain_idx)

    when (sprob_drain_any) {
      val cidx = sprob_drain_idx
      when (rob_val(cidx)) {
        val infl_base = PopCount(VecInit(rob_uop(cidx).cf_influencer_list.map(_.valid)))
        when (infl_base < numInfluencerSlotsCF.U) {
          for (k <- 0 until numInfluencerSlotsCF) {
            when (infl_base === k.U) {
              rob_uop(cidx).cf_influencer_list(k).valid     := true.B
              rob_uop(cidx).cf_influencer_list(k).op_count  := 0.U
              rob_uop(cidx).cf_influencer_list(k).infl_type := INFL_REG_DATAFLOW.U
              rob_uop(cidx).cf_influencer_list(k).is_atk    := false.B
              rob_uop(cidx).cf_influencer_list(k).is_secret := true.B
            }
          }
        } .otherwise {
          rob_uop(cidx).cf_infl_overflow := true.B
        }
      }
      sprob_infl_pending(cidx) := false.B
    }

    when (ic_drain_fires) {
      val cidx = ic_drain_idx
      when (rob_val(cidx)) {
        val infl_base = PopCount(VecInit(rob_uop(cidx).cf_influencer_list.map(_.valid)))
        when (infl_base < numInfluencerSlotsCF.U) {
          for (k <- 0 until numInfluencerSlotsCF) {
            when (infl_base === k.U) {
              rob_uop(cidx).cf_influencer_list(k).valid      := true.B
              rob_uop(cidx).cf_influencer_list(k).op_count   := ic_pending_winner_op(cidx)
              rob_uop(cidx).cf_influencer_list(k).infl_type  := INFL_ISSUE_CONTENTION.U
              rob_uop(cidx).cf_influencer_list(k).is_atk     := ic_pending_winner_atk(cidx)
              rob_uop(cidx).cf_influencer_list(k).is_secret  := ic_pending_winner_sec(cidx)
              rob_uop(cidx).cf_influencer_list(k).deny_count := ic_pending_deny_cnt(cidx)
            }
          }
        } .otherwise {
          rob_uop(cidx).cf_infl_overflow := true.B
        }
        when (ic_pending_winner_atk(cidx)) { rob_uop(cidx).cf_attacker_influence := true.B }
      }
      ic_pending_valid(cidx) := false.B
    }

    //-----------------------------------------------
    // Accruing fflags
    for (i <- 0 until numFpuPorts) {
      val fflag_uop = io.fflags(i).bits.uop
      when (io.fflags(i).valid && MatchBank(GetBankIdx(fflag_uop.rob_idx))) {
        rob_fflags(w)(GetRowIdx(fflag_uop.rob_idx)) := io.fflags(i).bits.flags
      }
    }

    //-----------------------------------------------------
    // Exceptions
    // (the cause bits are compressed and stored elsewhere)

    when (io.lxcpt.valid && MatchBank(GetBankIdx(io.lxcpt.bits.uop.rob_idx))) {
      rob_exception(GetRowIdx(io.lxcpt.bits.uop.rob_idx)) := true.B
      when (io.lxcpt.bits.cause =/= MINI_EXCEPTION_MEM_ORDERING) {
        // In the case of a mem-ordering failure, the failing load will have been marked safe already.
        assert(rob_unsafe(GetRowIdx(io.lxcpt.bits.uop.rob_idx)),
          "An instruction marked as safe is causing an exception")
      }
    }

    when (io.csr_replay.valid && MatchBank(GetBankIdx(io.csr_replay.bits.uop.rob_idx))) {
      rob_exception(GetRowIdx(io.csr_replay.bits.uop.rob_idx)) := true.B
    }
    can_throw_exception(w) := rob_val(rob_head) && rob_exception(rob_head)

    //-----------------------------------------------
    // Commit or Rollback

    // Can this instruction commit? (the check for exceptions/rob_state happens later).
    can_commit(w) := rob_val(rob_head) && !(rob_bsy(rob_head)) && !io.csr_stall


    // use the same "com_uop" for both rollback AND commit
    // Perform Commit
    io.commit.valids(w) := will_commit(w)
    io.commit.arch_valids(w) := will_commit(w) && !rob_predicated(com_idx)
    io.commit.uops(w)   := rob_uop(com_idx)
    io.commit.debug_insts(w) := rob_debug_inst_rdata(w)

    // We unbusy branches in b1, but its easier to mark the taken/provider src in b2,
    // when the branch might be committing
    when (io.brupdate.b2.mispredict &&
      MatchBank(GetBankIdx(io.brupdate.b2.uop.rob_idx)) &&
      GetRowIdx(io.brupdate.b2.uop.rob_idx) === com_idx) {
      io.commit.uops(w).debug_fsrc := BSRC_C
      io.commit.uops(w).taken      := io.brupdate.b2.taken
    }

    // corefuzzing: commit-time combinational override for pending influencer entries.
    // If the drain hasn't serviced com_idx yet by the time it commits, apply the
    // influencer(s) directly to io.commit.uops(w) (a Wire) so the commit log is correct.
    // Purely combinational: reads registers, writes only to the commit output Wire.
    // Also clears the pending bits so the drain skips this row after commit.
    when (will_commit(w)) {
      val pend_base0 = PopCount(VecInit(rob_uop(com_idx).cf_influencer_list.map(_.valid)))
      val has_sprob  = sprob_infl_pending(com_idx)
      val has_ic     = ic_pending_valid(com_idx)
      // sprob slot = pend_base0; ic slot = pend_base0 + (1 if sprob also pending)
      val ic_slot    = pend_base0 + has_sprob.asUInt

      when (has_sprob) {
        when (pend_base0 < numInfluencerSlotsCF.U) {
          for (k <- 0 until numInfluencerSlotsCF) {
            when (pend_base0 === k.U) {
              io.commit.uops(w).cf_influencer_list(k).valid     := true.B
              io.commit.uops(w).cf_influencer_list(k).op_count  := 0.U
              io.commit.uops(w).cf_influencer_list(k).infl_type := INFL_REG_DATAFLOW.U
              io.commit.uops(w).cf_influencer_list(k).is_atk    := false.B
              io.commit.uops(w).cf_influencer_list(k).is_secret := true.B
            }
          }
        } .otherwise {
          io.commit.uops(w).cf_infl_overflow := true.B
        }
        sprob_infl_pending(com_idx) := false.B
      }

      when (has_ic) {
        when (ic_slot < numInfluencerSlotsCF.U) {
          for (k <- 0 until numInfluencerSlotsCF) {
            when (ic_slot === k.U) {
              io.commit.uops(w).cf_influencer_list(k).valid      := true.B
              io.commit.uops(w).cf_influencer_list(k).op_count   := ic_pending_winner_op(com_idx)
              io.commit.uops(w).cf_influencer_list(k).infl_type  := INFL_ISSUE_CONTENTION.U
              io.commit.uops(w).cf_influencer_list(k).is_atk     := ic_pending_winner_atk(com_idx)
              io.commit.uops(w).cf_influencer_list(k).is_secret  := ic_pending_winner_sec(com_idx)
              io.commit.uops(w).cf_influencer_list(k).deny_count := ic_pending_deny_cnt(com_idx)
            }
          }
        } .otherwise {
          io.commit.uops(w).cf_infl_overflow := true.B
        }
        when (ic_pending_winner_atk(com_idx)) { io.commit.uops(w).cf_attacker_influence := true.B }
        ic_pending_valid(com_idx) := false.B
      }
    }


    // Don't attempt to rollback the tail's row when the rob is full.
    val rbk_row = rob_state === s_rollback && !full

    io.commit.rbk_valids(w) := rbk_row && rob_val(com_idx) && !(enableCommitMapTable.B)
    io.commit.rollback := (rob_state === s_rollback)

    assert (!(io.commit.valids.reduce(_||_) && io.commit.rbk_valids.reduce(_||_)),
      "com_valids and rbk_valids are mutually exclusive")

    when (rbk_row) {
      rob_val(com_idx)       := false.B
      rob_exception(com_idx) := false.B
    }

    if (enableCommitMapTable) {
      when (RegNext(exception_thrown)) {
        for (i <- 0 until numRobRows) {
          when(i.U < cf_rob_rows) {
            rob_val(i) := false.B
            rob_bsy(i) := false.B
            rob_uop(i).debug_inst := BUBBLE
          }
        }
      }
    }

    // -----------------------------------------------
    // Kill speculated entries on branch mispredict
    for (i <- 0 until numRobRows) {
      when(i.U < cf_rob_rows) {
        val br_mask = rob_uop(i).br_mask
        // corefuzzing
        // [SPECULATIVE][ROB] speculative flush logging (non-destructive)
        // We print any valid ROB entries that will be killed by the branch update
        when (rob_val(i) && IsKilledByBranch(io.brupdate, br_mask) && io.cf_debug_rob_enable) {
          // [FLUSH] log: full commit-log format for squashed entries
          // corefuzzing: add INFL_PIPELINE_FLUSH when flushing branch is from different domain
          // corefuzzing: single printf per entry — atomic in Verilator multi-threaded mode,
          // preventing interleaving of header/influencer-loop/footer across threads.
          // corefuzzing: no WireInit copy needed — read ROB state directly.
          // Pipeline-flush info is printed as separate fl/floc fields (like spec_atk/spec_oc),
          // completely outside the influencer list.  This eliminates the addInfluencer call
          // and WireInit(full MicroOp) that previously generated ~100 mux expressions per entry
          // across 128 entries, causing Rob.sv to balloon to 168 MB.
          val fu = rob_uop(i)
          val fl_cross_domain = io.cf_mispredict_uop.valid &&
            (io.cf_mispredict_uop.bits.cf_domain_id =/= fu.cf_domain_id)
          val fl_is_secret = io.cf_mispredict_uop.valid &&
            (io.cf_mispredict_uop.bits.cf_secret_propagation || io.cf_mispredict_uop.bits.cf_secret_access)
          val fl_op_count = Mux(io.cf_mispredict_uop.valid,
            io.cf_mispredict_uop.bits.cf_op_count_id, 0.U)
          val robFlushInflFmt = (0 until numInfluencerSlotsCF).zipWithIndex.map{case(_,k) => s"I$k={v=%d,oc=%d,ty=%d,atk=%d,sec=%d,dc=%d}"}.mkString(" ")
          val robFlushFmt = s"[FLUSH] 0x%x (0x%x) CF(domain=%d spec=%d atk=%d s_acc=%d s_prop=%d s_tx=%d opcount=%d spec_atk=%d spec_oc=%d fl=%d fl_sec=%d floc=%d) FU=0x%x SRC=%d INFL_FU=0x%x OVF=%d $robFlushInflFmt\n"
          val robInflArgs = (0 until numInfluencerSlotsCF).flatMap(k => Seq[Bits](
            fu.cf_influencer_list(k).valid,
            fu.cf_influencer_list(k).op_count,
            fu.cf_influencer_list(k).infl_type,
            fu.cf_influencer_list(k).is_atk,
            fu.cf_influencer_list(k).is_secret,
            fu.cf_influencer_list(k).deny_count
          ))
          printf(robFlushFmt, (Seq[Bits](
            Sext(fu.debug_pc(vaddrBits-1,0), xLen), fu.inst,
            fu.cf_domain_id, fu.cf_speculated, fu.cf_attacker_influence,
            fu.cf_secret_access, fu.cf_secret_propagation, fu.cf_secret_transmission,
            fu.cf_op_count_id, fu.cf_spec_branch_is_atk, fu.cf_spec_branch_op_id,
            fl_cross_domain, fl_is_secret, fl_op_count,
            fu.cf_fu_bitmap, 3.U, inflBitmapFromList(fu.cf_influencer_list), fu.cf_infl_overflow
          ) ++ robInflArgs): _*)
        }

        //kill instruction if mispredict & br mask match
        when (IsKilledByBranch(io.brupdate, br_mask))
        {
          rob_val(i) := false.B
          rob_uop(i.U).debug_inst := BUBBLE
          // corefuzzing: clear pending influencer bits so the drain doesn't
          // write to a slot that has been freed and potentially reallocated.
          sprob_infl_pending(i) := false.B
          ic_pending_valid(i)   := false.B
        } .elsewhen (rob_val(i)) {
          // clear speculation bit even on correct speculation
          rob_uop(i).br_mask := GetNewBrMask(io.brupdate, br_mask)
        }
      }
    }


    // Debug signal to figure out which prediction structure
    // or core resolved a branch correctly
    when (io.brupdate.b2.mispredict &&
      MatchBank(GetBankIdx(io.brupdate.b2.uop.rob_idx))) {
      rob_uop(GetRowIdx(io.brupdate.b2.uop.rob_idx)).debug_fsrc := BSRC_C
      rob_uop(GetRowIdx(io.brupdate.b2.uop.rob_idx)).taken      := io.brupdate.b2.taken
    }

    // -----------------------------------------------
    // Commit
    when (will_commit(w)) {
      rob_val(rob_head) := false.B
    }

    // -----------------------------------------------
    // Outputs
    rob_head_vals(w)     := rob_val(rob_head)
    rob_tail_vals(w)     := rob_val(rob_tail)
    rob_head_fflags(w)   := rob_fflags(w)(rob_head)
    rob_head_uses_stq(w) := rob_uop(rob_head).uses_stq
    rob_head_uses_ldq(w) := rob_uop(rob_head).uses_ldq

    // -------------------------------------------------
    // CoreFuzzing outputs
    cf_rob_head_uop(w)   := rob_uop(rob_head)
    
    //------------------------------------------------
    // Invalid entries are safe; thrown exceptions are unsafe.
    for (i <- 0 until numRobRows) {
      when(i.U < cf_rob_rows) {
        rob_unsafe_masked((i << log2Ceil(coreWidth)) + w) := rob_val(i) && (rob_unsafe(i) || rob_exception(i))
      }
    }

    // Read unsafe status of PNR row.
    rob_pnr_unsafe(w) := rob_val(rob_pnr) && (rob_unsafe(rob_pnr) || rob_exception(rob_pnr))

    // -----------------------------------------------
    // debugging write ports that should not be synthesized
    when (will_commit(w)) {
      rob_uop(rob_head).debug_inst := BUBBLE
    } .elsewhen (rbk_row)
    {
      rob_uop(rob_tail).debug_inst := BUBBLE
    }

    //--------------------------------------------------
    // Debug: for debug purposes, track side-effects to all register destinations

    for (i <- 0 until numWakeupPorts) {
      val rob_idx = io.wb_resps(i).bits.uop.rob_idx
      when (io.debug_wb_valids(i) && MatchBank(GetBankIdx(rob_idx))) {
        rob_debug_wdata(GetRowIdx(rob_idx)) := io.debug_wb_wdata(i)
      }
      // corefuzzing: merge cf_fu_bitmap bits and IFT flags from the functional unit (Phase 2)
      // Also guarded by rob_val && rob_bsy to suppress stale wakeups (same reason as above).
      when (io.wb_resps(i).valid && MatchBank(GetBankIdx(rob_idx)) && rob_val(GetRowIdx(rob_idx)) && rob_bsy(GetRowIdx(rob_idx))) {
        val rob_row  = GetRowIdx(rob_idx)
        val wb_uop_i = io.wb_resps(i).bits.uop
        rob_uop(rob_row).cf_fu_bitmap :=
          rob_uop(rob_row).cf_fu_bitmap | wb_uop_i.cf_fu_bitmap
        when (wb_uop_i.cf_attacker_influence)  { rob_uop(rob_row).cf_attacker_influence   := true.B }
        when (wb_uop_i.cf_secret_access)       { rob_uop(rob_row).cf_secret_access        := true.B }
        when (wb_uop_i.cf_secret_transmission) { rob_uop(rob_row).cf_secret_transmission  := true.B }
        when (wb_uop_i.cf_secret_propagation)  { rob_uop(rob_row).cf_secret_propagation   := true.B }

        // Merge influencer list: parallel prefix compact-and-append.
        // base_cnt = number of already-occupied slots in rob_uop.
        // prefix(j) = number of valid wb slots before j → direct destination index.
        val rob_base_cnt = PopCount(VecInit(rob_uop(rob_row).cf_influencer_list.map(_.valid)))
        val wb_valid_vec = VecInit(wb_uop_i.cf_influencer_list.map(_.valid))
        // Prefix sums: wb_prefix(j) = number of valid wb slots before j.
        val wb_prefix    = (0 until numInfluencerSlotsCF).map { j =>
          if (j == 0) 0.U(4.W) else PopCount(VecInit(wb_valid_vec.take(j)))
        }
        val wb_total = PopCount(wb_valid_vec)
        when (!rob_uop(rob_row).cf_infl_overflow && rob_base_cnt +& wb_total > numInfluencerSlotsCF.U) {
          rob_uop(rob_row).cf_infl_overflow := true.B
        }
        // Precompute destination slot for each wb entry once (avoids recomputing inside d-loop).
        // writers are one-hot per slot by prefix-sum construction → Mux1H is valid.
        val dst_slot = (0 until numInfluencerSlotsCF).map { j => rob_base_cnt + wb_prefix(j) }
        when (!rob_uop(rob_row).cf_infl_overflow) {
          for (d <- 0 until numInfluencerSlotsCF) {
            val writers: Seq[Bool] = (0 until numInfluencerSlotsCF).map { j =>
              wb_valid_vec(j) && (dst_slot(j) === d.U)
            }
            val any_write = writers.reduce(_ || _)
            when (any_write) {
              rob_uop(rob_row).cf_influencer_list(d).valid      := true.B
              rob_uop(rob_row).cf_influencer_list(d).op_count   := Mux1H(writers, wb_uop_i.cf_influencer_list.map(_.op_count))
              rob_uop(rob_row).cf_influencer_list(d).infl_type  := Mux1H(writers, wb_uop_i.cf_influencer_list.map(_.infl_type))
              rob_uop(rob_row).cf_influencer_list(d).is_atk     := Mux1H(writers, wb_uop_i.cf_influencer_list.map(_.is_atk))
              rob_uop(rob_row).cf_influencer_list(d).is_secret  := Mux1H(writers, wb_uop_i.cf_influencer_list.map(_.is_secret))
              rob_uop(rob_row).cf_influencer_list(d).deny_count := Mux1H(writers, wb_uop_i.cf_influencer_list.map(_.deny_count))
            }
          }
        }
        when (wb_uop_i.cf_infl_overflow) { rob_uop(rob_row).cf_infl_overflow := true.B }
      }
      val temp_uop = rob_uop(GetRowIdx(rob_idx))

      // corefuzzing: stale FU wakeups (rob_val=0 or rob_bsy=0) are silently
      // dropped by the guards above.  The "not-busy" assertion is removed
      // because Case C (rob_val=1, rob_bsy=0) is a benign stale wakeup.
      // The pdst check is preserved for Case B (valid, busy) to catch any
      // real writeback to the wrong instruction.
      assert (!(io.wb_resps(i).valid && MatchBank(GetBankIdx(rob_idx)) &&
               rob_val(GetRowIdx(rob_idx)) && rob_bsy(GetRowIdx(rob_idx)) &&
               temp_uop.ldst_val && temp_uop.pdst =/= io.wb_resps(i).bits.uop.pdst),
               "[rob] writeback (" + i + ") occurred to the wrong pdst.")
    }
    io.commit.debug_wdata(w) := rob_debug_wdata(rob_head)

  } //for (w <- 0 until coreWidth)

  // **************************************************************************
  // --------------------------------------------------------------------------
  // **************************************************************************

  // -----------------------------------------------
  // Commit Logic
  // need to take a "can_commit" array, and let the first can_commits commit
  // previous instructions may block the commit of younger instructions in the commit bundle
  // e.g., exception, or (valid && busy).
  // Finally, don't throw an exception if there are instructions in front of
  // it that want to commit (only throw exception when head of the bundle).

  var block_commit = (rob_state =/= s_normal) && (rob_state =/= s_wait_till_empty) || RegNext(exception_thrown) || RegNext(RegNext(exception_thrown))
  var will_throw_exception = false.B
  var block_xcpt   = false.B

  for (w <- 0 until coreWidth) {
    will_throw_exception = (can_throw_exception(w) && !block_commit && !block_xcpt) || will_throw_exception

    will_commit(w)       := can_commit(w) && !can_throw_exception(w) && !block_commit
    block_commit         = (rob_head_vals(w) &&
                           (!can_commit(w) || can_throw_exception(w))) || block_commit
    block_xcpt           = will_commit(w)
  }

  // Note: exception must be in the commit bundle.
  // Note: exception must be the first valid instruction in the commit bundle.
  exception_thrown := will_throw_exception
  val is_mini_exception = io.com_xcpt.bits.cause.isOneOf(MINI_EXCEPTION_MEM_ORDERING, MINI_EXCEPTION_CSR_REPLAY)
  io.com_xcpt.valid := exception_thrown && !is_mini_exception
  io.com_xcpt.bits := DontCare
  io.com_xcpt.bits.cause := r_xcpt_uop.exc_cause

  io.com_xcpt.bits.badvaddr := Sext(r_xcpt_badvaddr, xLen)
  val insn_sys_pc2epc =
    rob_head_vals.reduce(_|_) && PriorityMux(rob_head_vals, io.commit.uops.map{u => u.is_sys_pc2epc})

  val refetch_inst = exception_thrown || insn_sys_pc2epc
  val com_xcpt_uop = PriorityMux(rob_head_vals, io.commit.uops)
  io.com_xcpt.bits.ftq_idx   := com_xcpt_uop.ftq_idx
  io.com_xcpt.bits.edge_inst := com_xcpt_uop.edge_inst
  io.com_xcpt.bits.is_rvc    := com_xcpt_uop.is_rvc
  io.com_xcpt.bits.pc_lob    := com_xcpt_uop.pc_lob

  val flush_commit_mask = Range(0,coreWidth).map{i => io.commit.valids(i) && io.commit.uops(i).flush_on_commit}
  val flush_commit = flush_commit_mask.reduce(_|_)
  val flush_val = exception_thrown || flush_commit

  assert(!(PopCount(flush_commit_mask) > 1.U),
    "[rob] Can't commit multiple flush_on_commit instructions on one cycle")

  val flush_uop = Mux(exception_thrown, com_xcpt_uop, Mux1H(flush_commit_mask, io.commit.uops))

  // delay a cycle for critical path considerations
  io.flush.valid          := flush_val
  io.flush.bits           := DontCare
  io.flush.bits.ftq_idx   := flush_uop.ftq_idx
  io.flush.bits.pc_lob    := flush_uop.pc_lob
  io.flush.bits.edge_inst := flush_uop.edge_inst
  io.flush.bits.is_rvc    := flush_uop.is_rvc
  io.flush.bits.flush_typ := FlushTypes.getType(flush_val,
                                                exception_thrown && !is_mini_exception,
                                                flush_commit && flush_uop.uopc === uopERET,
                                                refetch_inst)


  // -----------------------------------------------
  // FP Exceptions
  // send fflags bits to the CSRFile to accrue

  val fflags_val = Wire(Vec(coreWidth, Bool()))
  val fflags     = Wire(Vec(coreWidth, UInt(freechips.rocketchip.tile.FPConstants.FLAGS_SZ.W)))

  for (w <- 0 until coreWidth) {
    fflags_val(w) :=
      io.commit.valids(w) &&
      io.commit.uops(w).fp_val &&
      !io.commit.uops(w).uses_stq

    fflags(w) := Mux(fflags_val(w), rob_head_fflags(w), 0.U)

    assert (!(io.commit.valids(w) &&
             !io.commit.uops(w).fp_val &&
             rob_head_fflags(w) =/= 0.U),
             "Committed non-FP instruction has non-zero fflag bits.")
    assert (!(io.commit.valids(w) &&
             io.commit.uops(w).fp_val &&
             (io.commit.uops(w).uses_ldq || io.commit.uops(w).uses_stq) &&
             rob_head_fflags(w) =/= 0.U),
             "Committed FP load or store has non-zero fflag bits.")
  }
  io.commit.fflags.valid := fflags_val.reduce(_|_)
  io.commit.fflags.bits  := fflags.reduce(_|_)

  // -----------------------------------------------
  // Exception Tracking Logic
  // only store the oldest exception, since only one can happen!

  val next_xcpt_uop = Wire(new MicroOp())
  next_xcpt_uop := r_xcpt_uop
  val enq_xcpts = Wire(Vec(coreWidth, Bool()))
  for (i <- 0 until coreWidth) {
    enq_xcpts(i) := io.enq_valids(i) && io.enq_uops(i).exception
  }

  when (!(io.flush.valid || exception_thrown) && rob_state =/= s_rollback) {

    val new_xcpt_valid = io.lxcpt.valid || io.csr_replay.valid
    val lxcpt_older = !io.csr_replay.valid || (IsOlder(io.lxcpt.bits.uop.rob_idx, io.csr_replay.bits.uop.rob_idx, rob_head_idx) && io.lxcpt.valid)
    val new_xcpt = Mux(lxcpt_older, io.lxcpt.bits, io.csr_replay.bits)

    when (new_xcpt_valid) {
      when (!r_xcpt_val || IsOlder(new_xcpt.uop.rob_idx, r_xcpt_uop.rob_idx, rob_head_idx)) {
        r_xcpt_val              := true.B
        next_xcpt_uop           := new_xcpt.uop
        next_xcpt_uop.exc_cause := new_xcpt.cause
        r_xcpt_badvaddr         := new_xcpt.badvaddr
      }
    } .elsewhen (!r_xcpt_val && enq_xcpts.reduce(_|_)) {
      val idx = enq_xcpts.indexWhere{i: Bool => i}

      // if no exception yet, dispatch exception wins
      r_xcpt_val      := true.B
      next_xcpt_uop   := io.enq_uops(idx)
      r_xcpt_badvaddr := AlignPCToBoundary(io.xcpt_fetch_pc, icBlockBytes) | io.enq_uops(idx).pc_lob

    }
  }

  r_xcpt_uop         := next_xcpt_uop
  r_xcpt_uop.br_mask := GetNewBrMask(io.brupdate, next_xcpt_uop)
  when (io.flush.valid || IsKilledByBranch(io.brupdate, next_xcpt_uop)) {
    r_xcpt_val := false.B
  }

  assert (!(exception_thrown && !r_xcpt_val),
    "ROB trying to throw an exception, but it doesn't have a valid xcpt_cause")

  assert (!(empty && r_xcpt_val),
    "ROB is empty, but believes it has an outstanding exception.")

  assert (!(will_throw_exception && (GetRowIdx(r_xcpt_uop.rob_idx) =/= rob_head)),
    "ROB is throwing an exception, but the stored exception information's " +
    "rob_idx does not match the rob_head")

  // -----------------------------------------------
  // ROB Head Logic

  // remember if we're still waiting on the rest of the dispatch packet, and prevent
  // the rob_head from advancing if it commits a partial parket before we
  // dispatch the rest of it.
  // update when committed ALL valid instructions in commit_bundle

  val rob_deq = WireInit(false.B)
  val r_partial_row = RegInit(false.B)

  when (io.enq_valids.reduce(_|_)) {
    r_partial_row := io.enq_partial_stall
  }

  val finished_committing_row =
    (io.commit.valids.asUInt =/= 0.U) &&
    ((will_commit.asUInt ^ rob_head_vals.asUInt) === 0.U) &&
    !(r_partial_row && rob_head === rob_tail && !maybe_full)

  when (finished_committing_row) {
    // when (io.cf_debug_rob_enable)
    // {
    //   for (i <- 0 until coreWidth)
    //   {
    //     val cf_uop_info = cf_rob_head_uop(i)
    //     printf(cf"[ROB] Finshed committing instruction Entry $i" + 
    //           cf" in Row $rob_head " + 
    //           cf" Info -  $cf_uop_info \n"
    //           )
    //   }
    // }
    // rob_head     := WrapInc(rob_head, numRobRows)
    rob_head := WrapInc(rob_head, cf_rob_rows)
    rob_head_lsb := 0.U
    rob_deq      := true.B
  } .otherwise {
    rob_head_lsb := OHToUInt(PriorityEncoderOH(rob_head_vals.asUInt))
  }

  // -----------------------------------------------
  // ROB Point-of-No-Return (PNR) Logic
  // Acts as a second head, but only waits on busy instructions which might cause misspeculation.
  // TODO is it worth it to add an extra 'parity' bit to all rob pointer logic?
  // Makes 'older than' comparisons ~3x cheaper, in case we're going to use the PNR to do a large number of those.
  // Also doesn't require the rob tail (or head) to be exported to whatever we want to compare with the PNR.

  if (enableFastPNR) {
    val unsafe_entry_in_rob = rob_unsafe_masked.reduce(_||_)
    val next_rob_pnr_idx = Mux(unsafe_entry_in_rob,
                               AgePriorityEncoder(rob_unsafe_masked, rob_head_idx),
                               rob_tail << log2Ceil(coreWidth) | PriorityEncoder(~rob_tail_vals.asUInt))
    rob_pnr := next_rob_pnr_idx >> log2Ceil(coreWidth)
    if (coreWidth > 1)
      rob_pnr_lsb := next_rob_pnr_idx(log2Ceil(coreWidth)-1, 0)
  } else {
    // Distinguish between PNR being at head/tail when ROB is full.
    // Works the same as maybe_full tracking for the ROB tail.
    val pnr_maybe_at_tail = RegInit(false.B)

    val safe_to_inc = rob_state === s_normal || rob_state === s_wait_till_empty
    val do_inc_row  = !rob_pnr_unsafe.reduce(_||_) && (rob_pnr =/= rob_tail || (full && !pnr_maybe_at_tail))
    when (empty && io.enq_valids.asUInt =/= 0.U) {
      // Unforunately for us, the ROB does not use its entries in monotonically
      //  increasing order, even in the case of no exceptions. The edge case
      //  arises when partial rows are enqueued and committed, leaving an empty
      //  ROB.
      rob_pnr     := rob_head
      rob_pnr_lsb := PriorityEncoder(io.enq_valids)
    } .elsewhen (safe_to_inc && do_inc_row) {
      // rob_pnr     := WrapInc(rob_pnr, io.rob_size_rows)
      rob_pnr := WrapInc(rob_pnr, cf_rob_rows)
      rob_pnr_lsb := 0.U
    } .elsewhen (safe_to_inc && (rob_pnr =/= rob_tail || (full && !pnr_maybe_at_tail))) {
      rob_pnr_lsb := PriorityEncoder(rob_pnr_unsafe)
    } .elsewhen (safe_to_inc && !full && !empty) {
      rob_pnr_lsb := PriorityEncoder(rob_pnr_unsafe.asUInt | ~MaskLower(rob_tail_vals.asUInt))
    } .elsewhen (full && pnr_maybe_at_tail) {
      rob_pnr_lsb := 0.U
    }

    pnr_maybe_at_tail := !rob_deq && (do_inc_row || pnr_maybe_at_tail)
  }

  // Head overrunning PNR likely means an entry hasn't been marked as safe when it should have been.
  assert(!IsOlder(rob_pnr_idx, rob_head_idx, rob_tail_idx) || rob_pnr_idx === rob_tail_idx)

  // PNR overrunning tail likely means an entry has been marked as safe when it shouldn't have been.
  assert(!IsOlder(rob_tail_idx, rob_pnr_idx, rob_head_idx) || full)

  // -----------------------------------------------
  // ROB Tail Logic

  val rob_enq = WireInit(false.B)

  when (rob_state === s_rollback && (rob_tail =/= rob_head || maybe_full)) {
    // Rollback a row
    // rob_tail     := WrapDec(rob_tail, numRobRows)
    rob_tail    := WrapDec(rob_tail, cf_rob_rows)
    rob_tail_lsb := (coreWidth-1).U
    rob_deq := true.B
  } .elsewhen (rob_state === s_rollback && (rob_tail === rob_head) && !maybe_full) {
    // Rollback an entry
    rob_tail_lsb := rob_head_lsb
  } .elsewhen (io.brupdate.b2.mispredict) {
    // rob_tail     := WrapInc(GetRowIdx(io.brupdate.b2.uop.rob_idx), numRobRows)
    rob_tail     := WrapInc(GetRowIdx(io.brupdate.b2.uop.rob_idx), cf_rob_rows)
    rob_tail_lsb := 0.U
  } .elsewhen (io.enq_valids.asUInt =/= 0.U && !io.enq_partial_stall) {
    // rob_tail     := WrapInc(rob_tail, numRobRows)
    rob_tail     := WrapInc(rob_tail, cf_rob_rows)
    rob_tail_lsb := 0.U
    rob_enq      := true.B
  } .elsewhen (io.enq_valids.asUInt =/= 0.U && io.enq_partial_stall) {
    rob_tail_lsb := PriorityEncoder(~MaskLower(io.enq_valids.asUInt))
  }


  if (enableCommitMapTable) {
    when (RegNext(exception_thrown)) {
      rob_tail     := 0.U
      rob_tail_lsb := 0.U
      rob_head     := 0.U
      rob_pnr      := 0.U
      rob_pnr_lsb  := 0.U
    }
  }

  // -----------------------------------------------
  // Full/Empty Logic
  // The ROB can be completely full, but only if it did not dispatch a row in the prior cycle.
  // I.E. at least one entry will be empty when in a steady state of dispatching and committing a row each cycle.
  // TODO should we add an extra 'parity bit' onto the ROB pointers to simplify this logic?

  maybe_full := !rob_deq && (rob_enq || maybe_full) || io.brupdate.b1.mispredict_mask =/= 0.U

  // corefuzzing: reset ROB head/tail to 0 on quiesce drain (last-write-wins over normal updates above).
  // When this fires, the ROB is empty (rob.io.empty = true in pipeline_drained_strict).
  // Resetting ensures WrapInc(ptr, cf_rob_rows) wraps correctly after a cf_rob_entries CSR change.
  when (io.cf_rob_quiesce_reset) {
    rob_head     := 0.U
    rob_head_lsb := 0.U
    rob_tail     := 0.U
    rob_tail_lsb := 0.U
    rob_pnr      := 0.U
    rob_pnr_lsb  := 0.U
    maybe_full   := false.B
  }
  full       := rob_tail === rob_head && maybe_full
  empty      := (rob_head === rob_tail) && (rob_head_vals.asUInt === 0.U)

  io.rob_head_idx      := rob_head_idx
  io.rob_tail_idx      := rob_tail_idx
  io.rob_pnr_idx       := rob_pnr_idx
  io.rob_head_op_count  := cf_rob_head_uop(0).cf_op_count_id
  io.rob_head_domain    := cf_rob_head_uop(0).cf_domain_id
  io.rob_head_is_secret := cf_rob_head_uop(0).cf_secret_access || cf_rob_head_uop(0).cf_secret_propagation
  io.empty        := empty
  io.ready        := (rob_state === s_normal) && !full && !r_xcpt_val

  //-----------------------------------------------
  //-----------------------------------------------
  //-----------------------------------------------

  // ROB FSM
  if (!enableCommitMapTable) {
    switch (rob_state) {
      is (s_reset) {
        rob_state := s_normal
      }
      is (s_normal) {
        // Delay rollback 2 cycles so branch mispredictions can drain
        when (RegNext(RegNext(exception_thrown))) {
          rob_state := s_rollback
        } .otherwise {
          for (w <- 0 until coreWidth) {
            when (io.enq_valids(w) && io.enq_uops(w).is_unique) {
              rob_state := s_wait_till_empty
            }
          }
        }
      }
      is (s_rollback) {
        when (empty) {
          rob_state := s_normal
        }
      }
      is (s_wait_till_empty) {
        when (RegNext(exception_thrown)) {
          rob_state := s_rollback
        } .elsewhen (empty) {
          rob_state := s_normal
        }
      }
    }
  } else {
    switch (rob_state) {
      is (s_reset) {
        rob_state := s_normal
      }
      is (s_normal) {
        when (exception_thrown) {
          ; //rob_state := s_rollback
        } .otherwise {
          for (w <- 0 until coreWidth) {
            when (io.enq_valids(w) && io.enq_uops(w).is_unique) {
              rob_state := s_wait_till_empty
            }
          }
        }
      }
      is (s_rollback) {
        when (rob_tail_idx  === rob_head_idx) {
          rob_state := s_normal
        }
      }
      is (s_wait_till_empty) {
        when (exception_thrown) {
          ; //rob_state := s_rollback
        } .elsewhen (rob_tail === rob_head) {
          rob_state := s_normal
        }
      }
    }
  }

  // -----------------------------------------------
  // Outputs

  io.com_load_is_at_rob_head := RegNext(rob_head_uses_ldq(PriorityEncoder(rob_head_vals.asUInt)) &&
                                        !will_commit.reduce(_||_))



  override def toString: String = BoomCoreStringPrefix(
    "==ROB==",
    "Machine Width      : " + coreWidth,
    "Rob Entries        : " + numRobEntries,
    "Rob Rows           : " + numRobRows,
    "Rob Row size       : " + log2Ceil(numRobRows),
    "log2Ceil(coreWidth): " + log2Ceil(coreWidth),
    "FPU FFlag Ports    : " + numFpuPorts)
}
