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
/**
 * [WINSHADOW] step 20: one implicit-flow window shadow range (core.scala winShadow).  Entries
 * younger than the arming branch B (all of them once B has retired: bdone) and older than the
 * ROB tail T at resolution (all of them when the ROB was full at capture: tall).  `head` is the
 * ROB head the ages are taken against.
 */
class WinShadowInfo(implicit p: Parameters) extends BoomBundle
{
  val v     = Bool()
  val b     = UInt(robAddrSz.W)
  val bdone = Bool()
  val t     = UInt(robAddrSz.W)
  val tall  = Bool()
  val head  = UInt(robAddrSz.W)
  // [WINEXP] step 21 (owner r139, F3): the kind's current window epoch (core.scala winCtr).
  val ep    = UInt(2.W)
  def in(idx: UInt): Bool = v && (bdone || IsOlder(b, idx, head)) && (tall || IsOlder(idx, t, head))
  // [WINAGE] Phase 4 Task 4.3c (gate review C3): the same test on head-relative ages,
  // ai = (idx - head) mod 2^robAddrSz.  Exact for every robAddrSz-bit value: with N = 2^k and
  // a, c, h in [0, N), IsOlder(a, c, h) = (a<c) ^ (a<h) ^ (c<h) equals (a-h mod N) < (c-h mod N)
  // (both >= h: no wrap, a<c; both < h: both +N, a<c; a < h <= c: a-h+N > c-h, false, and the
  // XOR is 1^1^0 = 0; c < h <= a: true, 0^0^1 = 1; a = c: both false).  Used where the age is
  // cheaper than the index (the ROB commit slots, whose age is a function of rob_head_lsb only).
  def age(x: UInt): UInt = (x - head)(robAddrSz - 1, 0)
  def inAge(ai: UInt): Bool = v && (bdone || age(b) < ai) && (tall || ai < age(t))
  // [WINEXP] the LIVE window taint of a result: the uop's own window taint (membership or a
  // window operand) counts only if it was stamped in the current epoch (uep = uop.cf_wep_*);
  // a shadow member (in) is marked in the current epoch.  The 2-bit compare (one LUT) runs in
  // parallel with in(); the final OR is the one the step-20 `win || in` already had.
  def res(win: Bool, uep: UInt, idx: UInt): Bool = (win && uep === ep) || in(idx)
}

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
  // IFT overhaul Phase 3, Task 3.3: the dispatch column's payload (base list, stall buckets,
  // branch context), built at dispatch beside the uop.
  val enq_ift          = Input(Vec(coreWidth, new IftDispatchInfo))
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
  // [ROBHEADATK] patch 48 (r173 (6)): the head row's own result carries attacker VALUE taint
  // (its C_sprob is_atk column, written at writeback by the existing port; registered like the
  // other head fields, [HEADREG]).  IFT only.
  val rob_head_valatk    = if (ENABLE_IFT) Some(Output(Bool())) else None
  // corefuzzing: cycle-N mispredicting branch UOP (same cycle as b1.mispredict_mask)
  val cf_mispredict_uop  = Input(Valid(new MicroOp))

  // Handle Branch Misspeculations
  val brupdate = Input(new BrUpdateInfo())

  // [STEP1 2026-09-06] Retroactive attacker-shadow broadcast.
  // One-hot union of the br_tags of branches that resolved this cycle with an
  // ATTACKER-INFLUENCED CONDITION (BrResolutionInfo.cond_atk).  Every in-flight uop
  // still holding one of those tags in its br_mask was fetched in that branch's
  // speculative shadow, so it is attacker-influenced.
  // WHY A BROADCAST, NOT A CONTEXT REGISTER: cond_atk is known only at RESOLVE, after
  // the shadow has already dispatched.  The secret side bridges that with the sticky
  // cf_ctrl_secret_ctx, which doc 16 sec.B forbids here -- a sticky "the attacker
  // branched at some point" bit never decays and tagged ~97% of a function on the
  // secret side.  This broadcast is exact and self-limiting: the tag is freed at
  // resolution, so the marked set is precisely the speculative window.
  // COST: reuses the per-entry br_mask compare that already runs for brupdate.
  val cf_br_atk_mask = Input(UInt(maxBrCount.W))

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
  val lsu_clr_bsy_cf_sprop  = Input(Vec(memWidth + 1, Bool()))

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
  // Includes all IQs: mem, int, and FP (fp_pipeline exposes cf_contention_upd as module IO).
  val cf_issue_contention_upd = Input(Vec(issueParams.map(_.issueWidth).sum, Valid(new IssueContentionUpdate)))

  // corefuzzing: direct ROB update to set cf_secret_access at address-compute (TLB) time.
  // Fires when LSU TLB detects the effective address falls in the secret range, before
  // the dcache response — ensures s_acc is visible in [FLUSH] logs for speculative loads
  // killed before the dcache responds.
  val cf_lsu_s_acc_upd = Input(Vec(memWidth, Valid(new CF_TlbFlagsUpdate)))   // [FIFO10] + atk / s_prop

  // corefuzzing: direct ROB s_tx update at TLB stage — fires when live preg_secret check
  // detects a secret-tainted source address register at issue time (Gap 1 fix, DOC:23).
  // Mirrors cf_lsu_s_acc_upd; op_count_id guard prevents stale ROB slot updates.
  val cf_lsu_s_tx_upd  = Input(Vec(memWidth, Valid(new CF_SAccUpdate)))

  // [BRSELF 2026-09-07] The attacker-STEERING branch's own entry.  Step 1 marks a
  // branch's SHADOW via br_mask, but a uop's br_mask holds only the OLDER branches it
  // speculates under -- never its own tag -- so the steering branch excluded itself.
  // It also cannot be marked at writeback: branches have rf_wen=false, so the ROB's
  // writeback path skips them.  MEASURED: bounds branch 0x80001730 had cond_atk=1 on
  // 650/650 resolves yet its own record read atk=0 on 477/477 commits.
  // Direct indexed write (mirrors cf_lsu_s_acc_upd): one bank-match per port, NOT
  // another 256-entry broadcast.
  val cf_br_atk_self = Input(Vec(coreWidth, Valid(new CF_SAccUpdate)))

  // [MEMORD 2026-09-08] ty=8 MEM_ORDER edge from the LSU's order_fail sites.  See
  // CF_MemOrdUpdate in micro-op.scala for why the LDQ-resident edge cannot survive.
  val cf_lsu_memord_upd = Input(Vec(memWidth, Valid(new CF_MemOrdUpdate)))
  // ([FIFO10] step 10, owner ruling 94: the Task 3.2 cf_lsu_dtlb_upd INFL_DTLB_STATE port is
  //  removed with the DTLB domain/secret shadows; paging is off, it could never fire.)

  // corefuzzing: direct ROB update to set cf_secret_propagation for in-flight instructions
  // whose physical source registers are secret-tainted, discovered at writeback time.
  // One port per integer writeback port (numWakeupPorts); unused ports stay valid=false.
  val cf_s_prop_rob_upd = Input(Vec(numWakeupPorts, Valid(new CF_SPropUpdate)))

  // [WINSHADOW] step 20: the window shadow ranges (core.scala winShadow): entries younger than
  // B (all of them once B has committed, bdone) are in the window when they commit.
  val cf_wsh_sec       = Input(new WinShadowInfo)
  val cf_wsh_atk       = Input(new WinShadowInfo)
  val cf_com_win_sec   = Output(Vec(coreWidth, Bool()))
  val cf_com_win_atk   = Output(Vec(coreWidth, Bool()))

  // corefuzzing changes
  val cf_debug_rob_enable = Input(Bool())
  val cf_rob_entries = Input(UInt(log2Ceil(robEntryOptions.length).W))
  // Fires in the cycle the CSRRW to robSizeCSRCF commits (flush_on_commit=Y, is_unique=Y).
  // Used to reset head/tail/pnr to 0 so WrapInc wraps correctly under the new cf_rob_rows.
  val cf_rob_entries_wen = Input(Bool())
  // Pulse to reset ROB head/tail to 0 on quiesce drain so WrapInc wraps correctly
  // after a cf_rob_entries CSR change (ROB is empty when this fires)
  val cf_rob_quiesce_reset = Input(Bool())

  // IFT bridge outputs: 256-bit records for committed and squashed IFT-active instructions.
  // Driven unconditionally (zero when ENABLE_IFT_BRIDGE=false in BoomCoreParams).
  // core.scala gates them behind ENABLE_IFT_BRIDGE before forwarding to tile IO.
  val ift_priv            = Input(UInt(3.W))               // current privilege mode (from csr.io.status.prv)
  val ift_commit_valids   = Output(Vec(retireWidth, Bool())) // arch_valids
  val ift_commit_records  = Output(Vec(retireWidth, UInt(256.W)))
  // [THINUOP] Task 3.5: the IFT record fields that left the MicroOp, per retire slot, for the
  // commit printf in core.scala: the sidecar readout (all zero with IFT off, except the two
  // retained uop fields, which are taken from the uop as before).  The row's sticky flags
  // stay on io.commit.uops (IftApply); Task 3.9 removed the unread commit_sticky copy.
  val commit_ift          = Output(Vec(retireWidth, new IftCommitInfo))
  val ift_squash          = Decoupled(UInt(256.W))          // one squash record per cycle; Task 3.4: ready from
                                                            // the core's lane arbiter, absorbed by IftSquashMerge's
                                                            // output queue (never reaches the banks, D6)
  val ift_squash_ovf      = Output(Bool())                  // sticky: a squash record has been lost since reset (Task 2.7, D6)
  val ift_squash_ovf_cnt  = Output(UInt(16.W))   // D6: squash records lost (bank FIFO full + row reuse), saturating
  val ift_squash_reuse_cnt = Output(UInt(16.W))  // D6: the row-reuse share of ift_squash_ovf_cnt (Task 2.7)
  val ift_third_drop_cnt  = Output(UInt(16.W))   // D19: readouts that saw a third same-type update
  // [FFTAINT] step 18 (ruling 118, G-2): this cycle a retiring FP op RAISED a flag (its
  // committed fflags are non-zero) and that op was secret (sticky s_acc || s_prop) / attacker
  // tainted (sticky atk || domain != 0).  Combinational on the commit readout, beside
  // io.commit.fflags; core.scala ORs them into its two sticky fflags taint bits.  Outputs
  // only: no new message path into the ROB.  Constant 0 without ENABLE_IFT.
  val cf_ff_set_sec       = Output(Bool())
  val cf_ff_set_atk       = Output(Bool())
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
  // [FFTAINT] step 18: the head row's secret (s_acc || s_prop) and attacker (atk || domain)
  // status per bank, read beside rob_head_fflags; used only by the FP-exceptions block.
  val rob_head_ffsec      = Wire(Vec(coreWidth, Bool()))
  val rob_head_ffatk      = Wire(Vec(coreWidth, Bool()))

  // for core fuzzing: the head row's op_count and domain (bank 0 = retire slot 0), driven inside
  // the bank loop for w == 0.  Task 2.10: two fields instead of a Vec of whole MicroOps of which
  // only bank 0's two fields were read.
  val rob_head_oc_bank0  = Wire(UInt(uopIDCounterWidthCF.W))
  val rob_head_dom_bank0 = Wire(UInt(1.W))
  // [SIDECAR] Task 2.4: the head row's secret status (s_acc || s_prop) comes from bank 0's
  // sticky flags (spec 5.1.1), not from rob_uop; driven inside the bank loop for w == 0.
  val rob_head_is_secret_bank0 = Wire(Bool())
  rob_head_is_secret_bank0 := false.B
  val rob_head_valatk_bank0 = WireInit(false.B)   // [ROBHEADATK] patch 48
  // [SIDECAR] Task 2.7 (spec 5.1.7, deferred D-9): the ROB pointer-reset pulse (quiesce drain,
  // or the ROB-size CSR change at its flush_commit).  Driven once after the bank loop (its
  // terms are computed there) and read inside the loop: it clears the squash-pending bits and
  // the sticky flags and re-syncs the squash drain pointer to the new tail (row 0).
  val ift_rob_ptr_reset = Wire(Bool())

  // LSU-fed writeback ports (the ones that carry influencer lists): 0 = ll_wbarb
  // (mem_resps(0) + fp->int), 1 = mem_resps(1); with the FPU the fp_pipeline wakeups
  // are the LAST three ports and their first two are the memory ones (fp ll_wbarb =
  // ll_wports(0) + int->fp, then ll_wports(1)); the FPU itself is last
  // (core.scala:2071-2128, fp-pipeline.scala:299-330).  Task 2.10d: hoisted out of the bank
  // loop because the raw squash entry's shape (IftSqEntry) is needed before it.
  val lsuWbPorts  = Seq(0, 1) ++ (if (usingFPU) Seq(numWakeupPorts - 3, numWakeupPorts - 2) else Nil)
  val iftNSprob   = numWakeupPorts                               // the sidecar's sprobPorts
  val iftNIc      = io.cf_issue_contention_upd.length
  val iftNMo      = memWidth
  // Task 2.10d (review F7, ruling 63 Q2): the squash path's printf copies (IftSqDbg memories,
  // [FLUSH], [SQREC], [SQDROP], [SQRAW]) and the Verilator-only bit-exact shadow check of the
  // 4b record exist only in printf builds without the IFT bridge.  No FireSim bridge build
  // elaborates them; in the oracle build they feed only printf/assert and Vivado removes them.
  val iftSqDbgOn  = ENABLE_IFT && ENABLE_CF_DEBUG_PRINTF && !ENABLE_IFT_BRIDGE
  val iftSqShadow = iftSqDbgOn

  // IFT bridge: per-bank squash captures (tail-order drain + rollback walk, Task 2.6).  Task
  // 2.10d (design 4b): each bank writes the RAW row of the captured entry into its own FIFO
  // in IftSquashMerge (after the loop), whose one shared merge drains the four FIFOs round-
  // robin onto the single squash lane.  No back-pressure into the bank (D6).
  val ift_sq_cap = Wire(Vec(coreWidth, Valid(new IftSqEntry(iftNSprob, iftNIc, lsuWbPorts.length, iftNMo))))
  val ift_sq_dbg = Wire(Vec(coreWidth, new IftSqDbg(iftSqShadow)))
  // D19 / D6 event pulses, counted after the loop: a readout that dropped a third same-type
  // update (commit readout and squash capture separately: both can fire in one cycle), a row
  // re-allocated while its squash record was still pending, and (Task 2.10d, D-22) the
  // pending rows a ROB pointer reset clears without a record.
  val ift_third_drop_bank    = Wire(Vec(coreWidth, Bool()))
  val ift_third_drop_sq_bank = Wire(Vec(coreWidth, Bool()))
  val ift_sq_reuse_bank      = Wire(Vec(coreWidth, Bool()))
  val ift_sq_ptrrst_bank     = Wire(Vec(coreWidth, UInt((log2Ceil(numRobRows) + 1).W)))
  // Initialised to safe defaults (no DontCare: Phase 0 Bug 3); each bank drives its own slot.
  ift_sq_cap             := 0.U.asTypeOf(ift_sq_cap)
  ift_sq_dbg             := 0.U.asTypeOf(ift_sq_dbg)
  ift_third_drop_bank    := VecInit(Seq.fill(coreWidth)(false.B))
  ift_third_drop_sq_bank := VecInit(Seq.fill(coreWidth)(false.B))
  ift_sq_reuse_bank      := VecInit(Seq.fill(coreWidth)(false.B))
  ift_sq_ptrrst_bank     := VecInit(Seq.fill(coreWidth)(0.U((log2Ceil(numRobRows) + 1).W)))
  // [FLUSH] printf (Task 2.6): the flushing branch's fields (the brief's
  // `RegEnable(io.cf_mispredict_uop.bits, io.cf_mispredict_uop.valid)`, reduced to the three
  // fields the printf reads and hoisted out of the bank loop), captured in the b1 cycle and
  // held for the drain.  Task 2.10d: the drain copies them into the entry's IftSqDbg at capture
  // (review F4 of the 4c design), so a later mispredict cannot change a printed field.
  // Debug-printf only; constants unless iftSqDbgOn (ruling 5, review F7).
  val flush_mp_gate = iftSqDbgOn
  val flush_mp_dom  = if (flush_mp_gate) RegEnable(io.cf_mispredict_uop.bits.cf_domain_id.getOrElse(0.U(iftTagWidth.W)), io.cf_mispredict_uop.valid)
                      else 0.U.asTypeOf(io.cf_mispredict_uop.bits.cf_domain_id.getOrElse(0.U(iftTagWidth.W)))
  val flush_mp_sec  = if (flush_mp_gate) RegEnable(io.cf_mispredict_uop.bits.cf_secret_propagation.getOrElse(false.B) ||
                                                   io.cf_mispredict_uop.bits.cf_secret_access.getOrElse(false.B), io.cf_mispredict_uop.valid)
                      else false.B
  val flush_mp_oc   = if (flush_mp_gate) RegEnable(io.cf_mispredict_uop.bits.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)), io.cf_mispredict_uop.valid)
                      else 0.U(uopIDCounterWidthCF.W)

  // [SIDECAR] arrival-stamp counter shared by the four sidecar banks (spec 5.1.2, D15).  Ruling 38:
  // a register only under ENABLE_IFT (rule 6.1/5), constant otherwise.  Task 2.10: one counter
  // instead of one identical copy per bank (48 declared register bits fewer; Vivado already
  // merged the copies, so the FPGA count does not move).
  val ift_stamp = if (ENABLE_IFT) { val r = RegInit(0.U(IftStamp.W.W)); r := r + 1.U; r } else 0.U(IftStamp.W.W)

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

    // IFT bridge: per-row squash-pending bits.  [SQUASHALL] Task 2.5 (D5, spec 5.1.6): set
    // in the kill loop for EVERY branch-killed valid row (the old IFT-activity gate and the
    // old sprob/ic/wb pending tables are gone, ruling 38 item 2); cleared by the tail-order
    // drain (Task 2.6, after the commit section) and on dispatch (new enqueue to the row).
    val ift_squash_pending = RegInit(VecInit(Seq.fill(numRobRows)(false.B)))
    // corefuzzing [SIDECAR] (IFT overhaul Phase 2, Task 2.2): IFT state for this bank moves
    // into a per-bank LUTRAM sidecar (RobIftBank, spec 5.1; D3, D15, D17, D19).  Once
    // Tasks 2.3/2.4 route the update ports and the readout, rob_uop keeps only op_count,
    // domain and ras_pred_xdom; everything else lives in the sidecar columns or in the four
    // sticky bits below (same-cycle readers: shadow broadcast, head status, squash gate).
    // Task 2.3 routed every update port into the columns / sticky bits and deleted the
    // drains; Task 2.4 made the commit path read the sidecar (io.commit.uops(w).cf_* is
    // reconstructed from the readout + sticky flags, the commit-time override is gone).
    // Task 2.5 rewrote the kill loop (D5: every branch-killed valid row is marked for a
    // squash record, no activity gate; the STEP1 shadow promotion writes only sticky_atk)
    // and deleted the old pending tables (sprob/ic/wb) with their last readers.  Task 2.6
    // moved the squash record and the [FLUSH] printf to the sidecar squash readout (tail-order
    // drain + rollback walk) and zeroes the rob_uop.cf_* copy at enqueue: rob_uop.cf_* is read
    // only for op_count, domain and ras_pred_xdom (spec 5.1.1) when ENABLE_IFT.
    // LSU-fed writeback ports: `lsuWbPorts` above the bank loop (Task 2.10d).
    // One op_count guard read port per guarded update port, in guardOk call order
    // (s_acc, br_atk_self, memord, s_tx, sprob); the require after the last call checks it.
    // ([FIFO10] step 10: the 2 DTLB guard ports are gone, 21 -> 19.)
    val sidecarGuardPorts = io.cf_lsu_s_acc_upd.length + io.cf_br_atk_self.length +
                            io.cf_lsu_memord_upd.length +
                            io.cf_lsu_s_tx_upd.length + io.cf_s_prop_rob_upd.length
    // Rule 6.1/5: every new element is under ENABLE_IFT.  With IFT off the sidecar is not
    // elaborated and the readout fields below read as zero, so BaselineBoomConfig's Verilog
    // is unchanged (checked in Task 2.11).  Written as a Module inside `if (ENABLE_IFT)`
    // with its IO exposed through a wire so the rest of this file reads one name:
    val sidecar_io = Wire(new RobIftBankIO(numRobRows, numWakeupPorts, memWidth + 1, numWakeupPorts,
                                           io.cf_issue_contention_upd.length, memWidth,
                                           guardPorts = sidecarGuardPorts, lsuWbCount = lsuWbPorts.length))
    // Ruling 29 (Task 2.1 review; Phase 0 Bug 3): no DontCare on this wire.  Every field
    // gets an explicit zero/false default here; the real drivers follow (last connect wins).
    sidecar_io.stamp        := 0.U
    sidecar_io.clear_all    := false.B
    sidecar_io.enq.valid    := false.B
    sidecar_io.enq.bits     := 0.U.asTypeOf(sidecar_io.enq.bits)
    sidecar_io.wb.foreach     { x => x.valid := false.B; x.bits := 0.U.asTypeOf(x.bits) }
    sidecar_io.clr.foreach    { x => x.valid := false.B; x.bits := 0.U.asTypeOf(x.bits) }
    sidecar_io.sprob.foreach  { x => x.valid := false.B; x.bits := 0.U.asTypeOf(x.bits) }
    sidecar_io.ic.foreach     { x => x.valid := false.B; x.bits := 0.U.asTypeOf(x.bits) }
    sidecar_io.memord.foreach { x => x.valid := false.B; x.bits := 0.U.asTypeOf(x.bits) }
    sidecar_io.ras.valid    := false.B
    sidecar_io.ras.bits     := 0.U
    sidecar_io.guard_row.foreach(_ := 0.U)
    sidecar_io.guard_oc.foreach(_ := 0.U)
    sidecar_io.commit       := 0.U.asTypeOf(sidecar_io.commit)
    sidecar_io.commit_out   := 0.U.asTypeOf(sidecar_io.commit_out)
    sidecar_io.commit_third := false.B
    sidecar_io.commit_valatk := false.B   // [ROBHEADATK] patch 48
    sidecar_io.commit_age   := 0.U
    sidecar_io.squash       := 0.U.asTypeOf(sidecar_io.squash)
    sidecar_io.sq_raw       := 0.U.asTypeOf(sidecar_io.sq_raw)
    sidecar_io.sq_third     := false.B
    if (ENABLE_IFT) {
      val m = Module(new RobIftBank(numRobRows, numWakeupPorts, lsuWbPorts, memWidth + 1,
                                    numWakeupPorts, io.cf_issue_contention_upd.length, memWidth,
                                    guardPorts = sidecarGuardPorts))
      m.io <> sidecar_io
    }
    // Keeps the `sidecar.io.*` spelling used by Tasks 2.3/2.4.  A local object rather than
    // the plan's `new { val io = ... }` so member access is not reflective (Task 2.1 review).
    object sidecar { val io = sidecar_io }
    sidecar.io.stamp     := ift_stamp
    sidecar.io.clear_all := io.cf_rob_quiesce_reset
    // Ruling 38: the four sticky vectors are registers only under ENABLE_IFT (rule 6.1/5);
    // constant-false wires otherwise, and every write to them sits under if (ENABLE_IFT).
    def stickyVec(): Vec[Bool] =
      if (ENABLE_IFT) RegInit(VecInit(Seq.fill(numRobRows)(false.B)))
      else            WireDefault(VecInit(Seq.fill(numRobRows)(false.B)))
    val sticky_s_acc  = stickyVec()
    val sticky_s_prop = stickyVec()
    val sticky_s_tx   = stickyVec()
    val sticky_atk    = stickyVec()
    // [SIDECAR] Task 2.3: op_count reuse guard through the sidecar's replicated op_count
    // column (spec 5.1.4).  One asynchronous read port per guarded update port, compared
    // once here: rob_val(row) && C_oc(row) === op_count.  Callers connect the row address
    // unconditionally (outside their `when`) so no mux sits in front of the LUTRAM read.
    var guard_k = 0
    def guardOk(row: UInt, oc: UInt): Bool = {
      val k = guard_k; guard_k += 1
      sidecar.io.guard_row(k) := row
      rob_val(row) && sidecar.io.guard_oc(k) === oc
    }

    //-----------------------------------------------
    // Dispatch: Add Entry to ROB

    rob_debug_inst_wmask(w) := io.enq_valids(w)
    rob_debug_inst_wdata(w) := io.enq_uops(w).debug_inst

    when (io.enq_valids(w)) {
      rob_val(rob_tail)       := true.B
      rob_bsy(rob_tail)       := !(io.enq_uops(w).is_fence ||
                                   io.enq_uops(w).is_fencei)
      rob_unsafe(rob_tail)    := io.enq_uops(w).unsafe
      rob_uop(rob_tail)       := io.enq_uops(w)
      // corefuzzing: stamp robTagCF bit into cf_fu_bitmap at enqueue.  IFT off only: with IFT
      // the sidecar's dispatch column stamps it (rob-ift-sidecar.scala, enqueue) and the copy
      // here is zeroed below (Task 2.10).
      if (!ENABLE_IFT) {
        rob_uop(rob_tail).cf_fu_bitmap.foreach(_ := io.enq_uops(w).cf_fu_bitmap.getOrElse(0.U(numModules.W)) | (1.U << robTagCF.U))
      }
      // IFT LUT optimization (Change 3): zero dead fields at ROB enqueue.
      //   cf_cntd_*: consumed at issue time via cf_contend_out → the C_ic sidecar
      //              columns (io.cf_issue_contention_upd); never read from rob_uop after enqueue.
      // [SIDECAR] Task 2.2: dispatch writes the sidecar's dispatch column and the four
      // sticky bits.  Task 2.6 (ruling 38 amendment 1, deferred from Task 2.2): with the
      // [FLUSH] printf and the squash record now read from the sidecar squash readout, no
      // rob_uop.cf_* reader is left except cf_op_count_id, cf_domain_id and cf_ras_pred_xdom
      // (spec 5.1.1), so the row's copy of every other cf_* field is zeroed here and the
      // flops fold away.  Under ENABLE_IFT only (ruling 5): with IFT off the sidecar does not
      // exist and the commit uop still carries the rob_uop copy, as before Phase 2.
      if (ENABLE_IFT) {
        rob_uop(rob_tail).cf_attacker_influence.foreach(_ := false.B)
        rob_uop(rob_tail).cf_secret_access.foreach(_ := false.B)
        rob_uop(rob_tail).cf_secret_propagation.foreach(_ := false.B)
        rob_uop(rob_tail).cf_secret_transmission.foreach(_ := false.B)
        rob_uop(rob_tail).cf_fu_bitmap.foreach(_ := 0.U)
        rob_uop(rob_tail).cf_mem_dataflow_atk.foreach(_ := false.B)
        rob_uop(rob_tail).cf_mem_sec_dataflow.foreach(_ := false.B)
        rob_uop(rob_tail).cf_spec_branch_is_secret.foreach(_ := false.B)
      }
      sidecar.io.enq.valid    := true.B
      sidecar.io.enq.bits.row  := rob_tail
      sidecar.io.enq.bits.oc   := io.enq_uops(w).cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
      sidecar.io.enq.bits.fu_bitmap := io.enq_uops(w).cf_fu_bitmap.getOrElse(0.U(numModules.W))
      sidecar.io.enq.bits.spec_branch_is_secret := io.enq_uops(w).cf_spec_branch_is_secret.getOrElse(false.B)
      sidecar.io.enq.bits.info := io.enq_ift(w)
      if (ENABLE_IFT) {
        sticky_s_acc(rob_tail)  := io.enq_uops(w).cf_secret_access.getOrElse(false.B)
        // [RSTICKY] step 17 (audit P18, R-STICKY): a dispatch-time edge with is_secret=1 sets the
        // row's s_prop summary, as dis_promoted_atk already does for is_atk (core.scala).  Read
        // from the base list the row is enqueued with, so the uop's own s_prop (which feeds the
        // issue-time wakeup secret) is untouched: summary only, no value taint.  Covers the
        // step-16 IQ_FULL and BTB_STATE (bit 7: secret hit / displacement) edges, and the other
        // dispatch edges with sec=1 that had no s_prop (REG_PRESSURE head, MEM_HOL).  An edge
        // dropped by the 4-slot list is not seen (the atk twin sees all candidates).
        // [R117SWEEP] patch 33 (r117 + r93): the enqueue uop's cf_secret_propagation now carries
        // the dispatch summary itself: core.scala ORs in is_secret over EVERY firing dispatch
        // candidate (stored or dropped by the 4-slot list) on rob.io.enq_uops only.  That is a
        // superset of the surviving base entries (zero base: every stored entry is a firing
        // candidate), so the old base-list OR is redundant and is removed (r91): 0 LUT here.
        sticky_s_prop(rob_tail) := io.enq_uops(w).cf_secret_propagation.getOrElse(false.B)
        sticky_s_tx(rob_tail)   := io.enq_uops(w).cf_secret_transmission.getOrElse(false.B)
        sticky_atk(rob_tail)    := io.enq_uops(w).cf_attacker_influence.getOrElse(false.B)
      }
      rob_exception(rob_tail) := io.enq_uops(w).exception
      rob_predicated(rob_tail)   := false.B
      rob_fflags(w)(rob_tail)    := 0.U
      // IFT bridge: clear stale squash-pending bit so an old killed entry doesn't
      // pollute a freshly dispatched instruction at the same ROB row.  With the tail-order
      // drain (Task 2.6) the bit is normally clear here; a set bit means the record was lost
      // to reuse, which the drain section asserts and counts (sq_reuse, D6).
      ift_squash_pending(rob_tail) := false.B

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
    for (((((clr_rob_idx, cf_bmap), cf_stx), cf_sprop), ci) <- io.lsu_clr_bsy.zip(io.lsu_clr_bsy_cf_bitmap).zip(io.lsu_clr_bsy_cf_stx).zip(io.lsu_clr_bsy_cf_sprop).zipWithIndex) {
      when (clr_rob_idx.valid && MatchBank(GetBankIdx(clr_rob_idx.bits))) {
        val cidx = GetRowIdx(clr_rob_idx.bits)
        rob_bsy(cidx)    := false.B
        rob_unsafe(cidx) := false.B
        // corefuzzing [SIDECAR] Task 2.3: the store's accumulated cf_fu_bitmap
        // (stq+dtlb+dcache bits) goes to this port's C_clr column; its data-taint flags go
        // to the sticky bits: s_tx = the store writes secret-derived data to a non-secret
        // address; s_prop = a store's DATA taint reaches the ROB only here (stores have
        // rf_wen=false, so they never appear as writeback resps).
        if (ENABLE_IFT) {
          sidecar.io.clr(ci).valid          := true.B
          sidecar.io.clr(ci).bits.row       := cidx
          sidecar.io.clr(ci).bits.fu_bitmap := cf_bmap
          when (cf_stx)   { sticky_s_tx(cidx)   := true.B }
          when (cf_sprop) { sticky_s_prop(cidx) := true.B }
        }
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

    // corefuzzing [SIDECAR] Task 2.3: issue contention update -> this port's C_ic column
    // (the entry, Log2Bucket(deny_count) and the stamp are formed in the column write; the
    // deny bucket is read back at readout) and the sticky attacker bit.  rob_val is the
    // only guard, as before (the port carries no op_count).  The old per-row contention
    // pending tables are gone (Task 2.5).
    if (ENABLE_IFT) {
      for ((upd, ui) <- io.cf_issue_contention_upd.zipWithIndex) {
        when (upd.valid && MatchBank(GetBankIdx(upd.bits.rob_idx))) {
          val cidx = GetRowIdx(upd.bits.rob_idx)
          when (rob_val(cidx)) {
            sidecar.io.ic(ui).valid    := true.B
            sidecar.io.ic(ui).bits.row := cidx
            sidecar.io.ic(ui).bits.upd := upd.bits
            // [FIFO10] any_atk: every winner that denied the uop (the slot keeps only the newest)
            when (upd.bits.winner_is_atk || upd.bits.any_atk) { sticky_atk(cidx) := true.B }
            // [R117SWEEP] patch 33 (DF-29b-ic, r117): the secret twin.  A sec=1 ISSUE_CONTENTION /
            // FU_BUSY winner (the newest, or any the slot's one-entry list replaced: any_sec) sets
            // the row's s_prop summary.  Same enable as the sticky_atk write, +0 levels on it.
            when (upd.bits.winner_is_sec || upd.bits.any_sec.getOrElse(false.B)) { sticky_s_prop(cidx) := true.B }
          }
        }
      }
    }

    // [SIDECAR] Task 2.3: the op_count-guarded update ports (spec 5.1.4).  Each guard's
    // row address is connected outside the `when` (guardOk) so the LUTRAM read sees the
    // port's row bits directly, and the valid/bank/guard conjunction gates only the one-bit
    // sticky writes and the column write enables.  None of these sites touches rob_uop
    // any more; the entries they used to append are reconstructed at readout (Task 2.4).
    if (ENABLE_IFT) {
      // corefuzzing: direct s_acc update from LSU TLB stage -- fires at address-compute
      // time, before the dcache response, so speculative secret loads show s_acc=1 in
      // [FLUSH] logs.  Guard against ROB slot reuse: only apply if the slot still holds
      // the same instruction (same op_count_id) that generated this TLB-stage update.
      for (upd <- io.cf_lsu_s_acc_upd) {
        val cidx = GetRowIdx(upd.bits.rob_idx)
        val ok   = guardOk(cidx, upd.bits.op_count_id)
        when (upd.valid && MatchBank(GetBankIdx(upd.bits.rob_idx)) && ok) {
          when (upd.bits.s_acc)  { sticky_s_acc(cidx)  := true.B }
          // [FIFO10] step 10: a load's issue-path flags at the TLB stage (squash-record parity)
          when (upd.bits.atk)    { sticky_atk(cidx)    := true.B }
          when (upd.bits.s_prop) { sticky_s_prop(cidx) := true.B }
        }
      }
      // [BRSELF] mark the attacker-steered branch itself.  Same op_count_id guard: a
      // squashed branch's slot may be reallocated before this lands.
      for (upd <- io.cf_br_atk_self) {
        val bidx = GetRowIdx(upd.bits.rob_idx)
        val ok   = guardOk(bidx, upd.bits.op_count_id)
        when (upd.valid && MatchBank(GetBankIdx(upd.bits.rob_idx)) && ok) { sticky_atk(bidx) := true.B }
      }
      // [MEMORD] the INFL_MEM_ORDER edge of the squashed load goes to this port's C_memord
      // column so it lands on the squash record.  Same bank-match + op_count guard.
      for ((upd, mi) <- io.cf_lsu_memord_upd.zipWithIndex) {
        val midx = GetRowIdx(upd.bits.rob_idx)
        val ok   = guardOk(midx, upd.bits.op_count_id)
        when (upd.valid && MatchBank(GetBankIdx(upd.bits.rob_idx)) && ok) {
          sidecar.io.memord(mi).valid    := true.B
          sidecar.io.memord(mi).bits.row := midx
          sidecar.io.memord(mi).bits.upd := upd.bits
          // [FIFO10] step 10 (R-STICKY): the attacker MEM_ORDER edge sets the row's attacker
          // flag whether or not its entry survives the merge or a later write to the column.
          when (upd.bits.is_atk) { sticky_atk(midx) := true.B }
          // [MEMORDSP] patch 29b (r117; t83 finding F-t83-1): a secret MEM_ORDER edge (the
          // conflicting store / searcher had s_acc or s_prop, lsu.scala order_fail sites) sets the
          // squashed load's sticky s_prop.  Step 10 added only the attacker half (R-STICKY, r93);
          // the r117 sweep (step 17) covered residency / IQ_FULL / BTB / nack / REG_PRESSURE /
          // MEM_HOL and missed this port, and the record's s_prop is the sticky bit alone (the
          // merge ORs is_atk over every edge, edge_atk, but has no is_secret twin).  Same enable
          // as the sticky_atk write beside it: +0 levels; +memWidth s_prop write ports per bank.
          when (upd.bits.is_secret) { sticky_s_prop(midx) := true.B }
        }
      }
      // [RASGATE doc 67 Part B] RAS cross-domain edge, MISPREDICT-GATED: raise
      // INFL_RAS_STATE only when a ret whose RAS-predicted top was cross-domain
      // (cf_ras_pred_xdom, set in fetch-buffer) actually mispredicts -- a real ret2spec.
      // The entry (op_count 0, is_atk = the ret was fetched in the victim domain) is
      // built at readout from the row's domain; only the arrival stamp is stored (C_ras).
      when (io.brupdate.b2.mispredict && MatchBank(GetBankIdx(io.brupdate.b2.uop.rob_idx))) {
        val ras_ridx = GetRowIdx(io.brupdate.b2.uop.rob_idx)
        when (rob_val(ras_ridx) && rob_uop(ras_ridx).cf_ras_pred_xdom.getOrElse(false.B)) {
          sidecar.io.ras.valid := true.B
          sidecar.io.ras.bits  := ras_ridx
        }
      }
      // Gap 1 fix (DOC:23): direct s_tx update from LSU TLB stage -- a live preg_secret
      // check found a secret-tainted source address.  Same op_count_id guard.
      for (upd <- io.cf_lsu_s_tx_upd) {
        val cidx = GetRowIdx(upd.bits.rob_idx)
        val ok   = guardOk(cidx, upd.bits.op_count_id)
        when (upd.valid && MatchBank(GetBankIdx(upd.bits.rob_idx)) && ok) { sticky_s_tx(cidx) := true.B }
      }
      // corefuzzing: s_prop direct ROB update -- an in-flight instruction whose physical
      // sources were secret-tainted, discovered at writeback (core.scala, the
      // `for (i <- 0 until rob.numWakeupPorts)` block under ENABLE_IFT).  The 1-bit
      // s_prop goes to the sticky flag; the INFL_REG_DATAFLOW mark goes to this port's
      // C_sprob column.  (The Gap 2 retroactive s_tx clause was removed 2026-09-03.)
      //
      // Spec 5.1.2 amendment (2026-10-08, owner-approved; Task 0.9): the column carries
      // is_atk / is_secret from the WRITEBACK SOURCE so the readout can emit the attacker
      // register-dataflow edge lost at the 2026-09-02 taint-follows-data cutover instead
      // of a hard-coded is_atk = false.  cf_s_prop_rob_upd(si) is driven from
      // wb_resps(si) in the same cycle, index-aligned, so the result taint on wb_resps(si)
      // is this update's source.  The guard reads row/op_count from that uop (equal to
      // upd.bits whenever upd.valid; asserted) so the attacker twin shares the guard port:
      //   is_secret = the core's s_prop condition (upd.valid);
      //   is_atk    = the old dispatch-time rule, victim consumer of an attacker-tainted
      //               operand: taint_atk && domain == 0, on a real int/FP register write
      //               (the ownership seed is applied at the regfile write, so for a
      //               domain-0 uop taint_atk is exactly the operand taint).
      // The twin fires the column on is_atk alone (is_secret = 0) and writes no sticky
      // bit: sticky_atk is already set by the R0 site in the writeback merge below.
      for ((upd, si) <- io.cf_s_prop_rob_upd.zipWithIndex) {
        val sp_wb    = io.wb_resps(si)
        val sp_uop   = sp_wb.bits.uop
        val sp_row   = GetRowIdx(sp_uop.rob_idx)
        val sp_bank  = MatchBank(GetBankIdx(sp_uop.rob_idx))
        val sp_ok    = guardOk(sp_row, sp_uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)))
        val sp_regwr = sp_wb.valid && sp_uop.rf_wen && sp_uop.ldst_val &&
                       (sp_uop.dst_rtype === RT_FIX || sp_uop.dst_rtype === RT_FLT)
        // [MEMATKGATE] Task 0.14 item 6 (owner, 2026-10-09): a load whose attacker taint came
        // through the D$ line (cf_mem_dataflow_atk, set at the dcache response) already has its
        // type-17 INFL_MEM_DATAFLOW edge; no duplicate type-0 edge.  The store-forward half
        // has no per-uop flag at the ROB and still raises the type-0 twin (spec 6.2).
        val sp_atk   = sp_regwr && (sp_wb.bits.taint_atk || sp_uop.cf_opnd_watk.getOrElse(false.B)) &&   // [IMPLWIN]
                       (sp_uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U) &&
                       !sp_uop.cf_mem_dataflow_atk.getOrElse(false.B)
        val sp_sec   = upd.valid
        assert (!upd.valid || (upd.bits.rob_idx === sp_uop.rob_idx &&
                               upd.bits.op_count_id === sp_uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))),
                "[rob] cf_s_prop_rob_upd(" + si + ") does not name the wb_resps(" + si + ") uop")
        when ((sp_sec || sp_atk) && sp_bank && sp_ok) {
          when (sp_sec) { sticky_s_prop(sp_row) := true.B }
          sidecar.io.sprob(si).valid          := true.B
          sidecar.io.sprob(si).bits.row       := sp_row
          sidecar.io.sprob(si).bits.is_atk    := sp_atk
          sidecar.io.sprob(si).bits.is_secret := sp_sec
        }
      }
      require(guard_k == sidecar.io.guard_row.length,
        s"[rob] sidecar guard ports: ${sidecar.io.guard_row.length} declared, ${guard_k} used")
    }

    // (The squash drain moved below the commit section in Task 2.6: it shares applyIft and
    // reads rbk_row; see [SQUASHALL] tail-order drain.)

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

    // corefuzzing [SIDECAR] Task 2.4: the commit record's IFT fields are reconstructed from
    // the sidecar readout (dispatch column + late columns merged in arrival order, the four
    // newest surviving: spec 5.1.5, D3, D19) and the four sticky flags.  rob_uop supplies
    // op_count, domain and the fields the sidecar does not hold.  This replaces the
    // commit-time combinational override of the old pending tables (sprob/ic/wb), which is
    // deleted; nothing at commit reads those tables any more.  The readout is combinational
    // here (core.scala's commit printf reads io.commit.* in the commit cycle); the register
    // stage on io.ift_commit_records is Phase 4's [RECREG] (below, at the record pack).
    // applyIft is used by the commit readout and, in printf builds, by the capture-time shadow
    // record of the squash drain below (Task 2.10d); it is only called under ENABLE_IFT.
    // Task 2.10d: the body moved to IftApply (rob-ift-sidecar.scala), which IftSquashMerge
    // shares; the commit readout passes this row's sticky flags.
    def applyIft(u: MicroOp, row: UInt, i: IftCommitInfo): Unit =
      IftApply(u, sticky_s_acc(row), sticky_s_prop(row), sticky_s_tx(row), sticky_atk(row), i)
    // [THINUOP] Task 3.5: the commit readout and sticky flags of this retire slot, for the
    // bridge record (uopToIFTBits) and the commit printf (RobIo.commit_ift).
    val com_ift    = WireInit(0.U.asTypeOf(new IftCommitInfo))
    val com_sticky = WireInit(0.U(4.W))
    if (!ENABLE_IFT) {
      // IFT off: no sidecar; the record's retained fields come from the uop, as before.
      com_ift.fu_bitmap             := io.commit.uops(w).cf_fu_bitmap.getOrElse(0.U(numModules.W))
      com_ift.spec_branch_is_secret := io.commit.uops(w).cf_spec_branch_is_secret.getOrElse(false.B)
      com_sticky := Cat(io.commit.uops(w).cf_attacker_influence.getOrElse(false.B), io.commit.uops(w).cf_secret_transmission.getOrElse(false.B),
                        io.commit.uops(w).cf_secret_propagation.getOrElse(false.B), io.commit.uops(w).cf_secret_access.getOrElse(false.B))
    }
    io.commit_ift(w)    := com_ift
    io.cf_com_win_sec(w) := false.B   // [WINSHADOW] driven below under ENABLE_IFT
    io.cf_com_win_atk(w) := false.B
    if (ENABLE_IFT) {
      // [SIDECAR] reconstruct the record fields from the sidecar + sticky flags
      sidecar.io.commit.row    := com_idx
      sidecar.io.commit.domain := rob_uop(com_idx).cf_domain_id.getOrElse(0.U(iftTagWidth.W))
      com_ift    := sidecar.io.commit_out
      // [WINSHADOW] step 20: a committing entry in the window shadow range is in the window
      // [WINAGE] Phase 4 Task 4.3c (gate review C3, the ROB commit instance): the test no longer
      // reads the committing uop's rob_idx out of the rob_uop readout (com_idx mux -> 64:1 row
      // read -> 2 x IsOlder -> com_sticky / cf_com_win_* -> regfile set ports, cross-module).
      // Proof the index is the same where it is observed: every consumer of win_sec / win_atk
      // is gated by this lane's commit valid (cf_com_win_* by cwr = commit.valids in
      // core.scala; the record by arch_valids; the commit uop's cf_secret_propagation /
      // cf_attacker_influence / cf_win_* by commit.valids in the LSU, the commit printf and
      // the record; rollback readers of io.commit.uops use only ldst/pdst/stale_pdst).
      // commit.valids(w) = will_commit(w) implies !block_commit, so rob_state is s_normal or
      // s_wait_till_empty and com_idx = rob_head; and it implies rob_val(rob_head) in bank w,
      // whose rob_uop was written at enqueue with io.enq_uops(w) = dis_uops(w), rob_idx =
      // Cat(rob_tail_idx >> log2(coreWidth), w) with rob_tail = the written row (core.scala
      // dispatch; nothing rewrites rob_uop(*).rob_idx).  So cidx = Cat(rob_head, w) on every
      // observed cycle, and its age against io.cf_wsh_*.head (= rob.io.rob_head_idx =
      // Cat(rob_head, rob_head_lsb)) is (w - rob_head_lsb) mod 2^robAddrSz: a function of one
      // 2-bit register.  inAge on that age is exact (identity at WinShadowInfo.inAge).
      // Levels: the window bit is now register-sourced (b, t, head, rob_head_lsb, v/bdone/tall:
      // one subtract, one compare, one LUT, about 5) and runs in parallel with the readout;
      // the readout -> com_sticky / cf_com_win_* leg keeps only its final OR (was readout +
      // about 4).  LUT: the per-slot compare is against a 2-bit-dependent constant (ab and at
      // are shared by the coreWidth slots), about half of the two IsOlder it replaces.
      val cage    = if (coreWidth == 1) 0.U(robAddrSz.W)
                    else (w.U(robAddrSz.W) - rob_head_lsb.pad(robAddrSz))(robAddrSz - 1, 0)
      val win_sec = io.cf_wsh_sec.inAge(cage)
      val win_atk = io.cf_wsh_atk.inAge(cage) && rob_uop(com_idx).cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U
      io.cf_com_win_sec(w) := win_sec
      io.cf_com_win_atk(w) := win_atk
      com_sticky := Cat(sticky_atk(com_idx) || win_atk, sticky_s_tx(com_idx),
                        sticky_s_prop(com_idx) || win_sec, sticky_s_acc(com_idx))
      applyIft(io.commit.uops(w), com_idx, sidecar.io.commit_out)
      when (win_sec) { io.commit.uops(w).cf_secret_propagation.foreach(_ := true.B) }
      when (win_atk) { io.commit.uops(w).cf_attacker_influence.foreach(_ := true.B) }
      // [WINMEM] step 21 (owner r139, F2): a committing STORE in the shadow range is a window
      // member; the LSU ORs this into its STQ entry so the D$ write marks the doubleword with
      // window taint (the `if (secret) x = 1` store dispatched before the branch resolved).
      // Nothing else reads the commit uop's cf_win_* (the regfile set ports use cf_com_win_*).
      // [AREA35] patch 35 (B): drive cf_win_* from the commit-time test alone.  Its only reader,
      // the LSU store commit (lsu.scala stq(idx).cf_win_* := stq.cf_win_* || commit.cf_win_*),
      // already holds the dispatch-time value: rob.io.enq_uops := dis_uops and the STQ entry
      // takes io.lsu.dis_uops(w).bits = dis_uops(w) in the same cycle, and nothing writes the
      // STQ uop's cf_win_* between enqueue and that commit OR.  So stq || (rob || win) =
      // stq || win: record-identical, and the 256 x 2 rob_uop cf_win_* flops plus their commit
      // readout die (-512 FF).  Levels: unchanged (win_sec / win_atk already feed this output).
      io.commit.uops(w).cf_win_sec.foreach(_ := win_sec)
      io.commit.uops(w).cf_win_atk.foreach(_ := win_atk)
      // D15: residency bound that makes the 16-bit stamps monotone
      assert(!(will_commit(w) && ENABLE_IFT.B) || sidecar.io.commit_age < 65535.U,
        "[rob] ROB row resident for >= 65535 cycles; IFT stamps would wrap (D15)")
      // D19 (Task 2.6): the commit readout dropped a third same-type update on a real commit
      ift_third_drop_bank(w) := sidecar.io.commit_third && will_commit(w)
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

    // -----------------------------------------------
    // [SQUASHALL] tail-order drain (section 5.1.6; D5, D6, D2b; Task 2.6).  One squash capture
    // per bank per cycle.  Task 2.10d (design 4b): the capture reads the row RAW (every merge
    // input, sidecar.io.sq_raw) plus its side fields and writes them into this bank's raw FIFO
    // in IftSquashMerge (after the loop); the shared merge runs when the entry leaves the FIFO.
    // Two sources share the capture:
    //  - rollback walk (cause = 1): during s_rollback the walk reads the walked row
    //    (com_idx = rob_tail) in the cycle before its rob_val clear lands;
    //  - branch-kill drain (cause = 0): the pointer restarts at the new tail on every
    //    mispredict (b2), i.e. at the first killed row, and walks forward one row per cycle
    //    (emit or skip) while any row of the bank is pending -- the order re-allocation
    //    reuses rows, so the drain stays ahead of the tail.
    // Every update that lands in the kill cycle is in the record (the row is read one or more
    // cycles later and nothing writes a killed row afterwards).  Under ENABLE_IFT only
    // (ruling 5); with IFT off ift_squash_pending is never set and the outputs are constant.
    if (ENABLE_IFT) {
      val sq_ptr     = RegInit(0.U(log2Ceil(numRobRows).W))
      val sq_any     = ift_squash_pending.asUInt.orR
      val rbk_emit   = rbk_row && rob_val(com_idx)
      // The readout address selects on rbk_row (registers only), not on rbk_emit, so no
      // rob_val read sits in front of the LUTRAM address: the squash readout keeps the commit
      // readout's depth.  The drain therefore pauses for the whole rollback walk (no enqueue
      // happens during s_rollback, so it cannot be overtaken meanwhile).
      val drain_emit = !rbk_row && ift_squash_pending(sq_ptr)
      val sq_row     = Mux(rbk_row, com_idx, sq_ptr)
      sidecar.io.squash.row    := sq_row
      sidecar.io.squash.domain := rob_uop(sq_row).cf_domain_id.getOrElse(0.U(iftTagWidth.W))   // raw.domain (4c review F2)
      val sq_cause = rbk_row                                    // 0 branch kill, 1 exception rollback (D2b)
      val su       = rob_uop(sq_row)
      val cap      = ift_sq_cap(w)
      cap.valid                := rbk_emit || drain_emit
      cap.bits.raw             := sidecar.io.sq_raw
      cap.bits.side.pc         := su.debug_pc(31, 0)
      cap.bits.side.inst       := su.debug_inst                 // the record packs debug_inst (4c review F3)
      cap.bits.side.op_count   := su.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
      cap.bits.side.pdst       := su.pdst
      cap.bits.side.prs1       := su.prs1
      cap.bits.side.prs2       := su.prs2
      cap.bits.side.lrs1_rtype := su.lrs1_rtype
      cap.bits.side.lrs2_rtype := su.lrs2_rtype
      cap.bits.side.s_acc      := sticky_s_acc(sq_row)
      cap.bits.side.s_prop     := sticky_s_prop(sq_row)
      cap.bits.side.s_tx       := sticky_s_tx(sq_row)
      cap.bits.side.atk        := sticky_atk(sq_row)
      cap.bits.side.priv       := io.ift_priv
      cap.bits.side.cause      := sq_cause
      when (rbk_emit)   { ift_squash_pending(com_idx) := false.B }
      when (drain_emit) { ift_squash_pending(sq_ptr)  := false.B }
      when (!rbk_row && sq_any) { sq_ptr := WrapInc(sq_ptr, cf_rob_rows) }   // emit or skip
      // The rollback walk rewinds rob_tail to rob_head, and re-allocation restarts there; the
      // pointer follows the tail through the walk so it resumes at the new tail too.  Without
      // this a still-pending row BEHIND the pointer (the younger, killed entries in the
      // mispredicting branch's own row, which the b2 restart below skips over: they are reached
      // only after a wrap) could be re-allocated first once an older instruction's exception
      // rewinds the tail below it.
      when (rob_state === s_rollback) { sq_ptr := rob_tail }
      // Restart LAST so it wins over the walk's increment in the b2 cycle (the pending bits
      // of this kill are already set then, so the walk is active in that cycle).
      when (io.brupdate.b2.mispredict) { sq_ptr := WrapInc(GetRowIdx(io.brupdate.b2.uop.rob_idx), cf_rob_rows) }
      // Task 2.7 (D-9): the pointer reset rewinds rob_tail to row 0, so the drain pointer
      // follows it there (the pending bits are cleared on the same pulse, end of this loop).
      when (ift_rob_ptr_reset) { sq_ptr := 0.U }
      // a row re-used while still pending would lose its record: count it (D6) and assert in
      // sim.  A row drained in the reuse cycle itself is read before the write lands: no loss.
      val sq_reuse = io.enq_valids(w) && ift_squash_pending(rob_tail) && !(drain_emit && sq_ptr === rob_tail)
      assert(!sq_reuse, "[rob] squash record lost to row reuse (tail-order drain overtaken)")
      ift_sq_reuse_bank(w)      := sq_reuse
      // D19: the same function of the same valid bits, in the same cycle, as the old squash
      // readout's third-drop pulse (a capture dropped on a full FIFO still counts, as before)
      ift_third_drop_sq_bank(w) := sidecar.io.sq_third && (rbk_emit || drain_emit)
      // D-22 (Task 2.10d, ruling 64): the pointer reset below clears every pending bit of the
      // bank.  Count the rows it clears without a record.  The row the drain captures in the
      // same cycle is not lost (drain_emit implies its pending bit is set, so no underflow).  A
      // rollback capture never names a pending row: enqueue clears the bit and a kill sets it
      // only while clearing rob_val, so rbk_emit (rob_val set) excludes it.  The pulse fires on
      // an empty ROB (quiesce drain, or the ROB-size CSRRW's own flush_commit), so no kill can
      // set a pending bit in the same cycle.
      ift_sq_ptrrst_bank(w) := Mux(ift_rob_ptr_reset, PopCount(ift_squash_pending) - drain_emit.asUInt, 0.U)
      // Review Focus 3: a MEM_ORDER edge must land before the rollback walk reads its row
      val memord_hits_rbk_row = io.cf_lsu_memord_upd.map(u => u.valid && MatchBank(GetBankIdx(u.bits.rob_idx)) &&
                                                               GetRowIdx(u.bits.rob_idx) === com_idx).reduce(_ || _)
      assert(!(rbk_emit && memord_hits_rbk_row), "[rob] MEM_ORDER edge arrived in the rollback cycle of its row")
      // [FLUSH] printf (Task 2.10d, ruling 63 Q3): printed by IftSquashMerge from the shipped
      // record, at the merge, in lane order.  The printf-only fields are copied here at capture
      // (4c review F3, F4): the expanded inst, the full pc, br_mask, br_tag, the enable and the
      // flushing branch's fl/fl_sec/floc (flush_mp_*, which a later mispredict reloads).
      // fl/fl_sec/floc name the flushing branch, captured at its b1; a rollback record (SRC=4)
      // has no flushing branch and prints 0 for all three, as before.
      if (iftSqDbgOn) {
        val d = ift_sq_dbg(w)
        d.en      := io.cf_debug_rob_enable
        d.pc      := su.debug_pc(vaddrBits - 1, 0)
        d.inst    := su.inst
        d.br_mask := su.br_mask
        d.br_tag  := su.br_tag
        d.fl_dom  := flush_mp_dom
        d.fl_sec  := flush_mp_sec
        d.fl_oc   := flush_mp_oc
        if (iftSqShadow) {
          // the pre-4b record, packed at capture exactly as the old drain did (Verilator only;
          // IftSquashMerge asserts the shipped record equals it, design 7.9 B5)
          val old_ift = IftMerge(sidecar.io.sq_raw)._1
          d.old_rec.get := uopToIFTBits(rob_uop(sq_row), old_ift,
            (sticky_s_acc(sq_row), sticky_s_prop(sq_row), sticky_s_tx(sq_row), sticky_atk(sq_row)),
            2.U, io.ift_priv) | (sq_cause.asUInt << 253)
        }
        when (ift_sq_ptrrst_bank(w) =/= 0.U) {
          printf("[SQPTRRST] bank=%d lost=%d\n", w.U, ift_sq_ptrrst_bank(w))
        }
      }
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
        // [FLUSHFIX 2026-09-07] The attacker-shadow term, computed ONCE per entry and
        // reused at all three consumers below.  PROVEN NECESSARY: rob_val(i) := false.B
        // (the IsKilledByBranch kill) and the shadow register write land on the SAME clock
        // edge, so a squashed uop's register never becomes observable -- the [FLUSH] printf
        // and cf_has_ift_activity both read the PRE-write register and saw atk=0.
        // Measured before this fix: FLUSH 16/8469 = 0% tainted vs COMMIT 1802/6188 = 29%.
        // Sharing the val keeps this to ONE 20-bit AND-reduce per entry, not three.
        val cf_in_atk_shadow = (br_mask & io.cf_br_atk_mask).orR
        // corefuzzing
        // [SPECULATIVE][ROB] speculative flush logging (non-destructive)
        // We print any valid ROB entries that will be killed by the branch update
        // Gated by ENABLE_CF_DEBUG_PRINTF (compile-time): eliding this removes the
        // large influencer-dump mux cone that inflated Rob.sv.
        // [RBKLOG 2026-09-08] Exception-squashed entries were logged NOWHERE.
        //
        // This printf fired only on IsKilledByBranch.  A uop squashed by an EXCEPTION --
        // including MINI_EXCEPTION_MEM_ORDERING, which flushes via exception_thrown ->
        // io.flush.valid -> s_rollback -- produced no record at all: its influencer list,
        // its taint bits, everything, discarded silently.  PROVEN (t35, 2026-09-08): the
        // [MEMORD] probe fired once, so a real ordering violation occurred and
        // cf_lsu_memord_upd routed the ty=8 edge into rob_uop -- yet ty=8 appeared ZERO
        // times in the whole log and the victim load had no [FLUSH] record.  The edge
        // existed in hardware and was never printed.
        //
        // Not MEM_ORDER-specific: ANY transient flow ending in an exception was invisible.
        //
        // The rollback walk clears one row per bank per cycle at `com_idx`, which during
        // s_rollback is rob_tail (see `val rbk_row` above).  Reusing THIS printf rather
        // than adding a second one is deliberate -- the comment above notes the
        // influencer-dump mux cone is what inflated Rob.sv, so a duplicate would pay that
        // area twice.  SRC distinguishes the two: 3 = branch kill, 4 = exception rollback.
        //
        // [SQUASHALL] Task 2.6: with ENABLE_IFT the [FLUSH] printf lives in the squash path
        // (Task 2.10d: IftSquashMerge prints it from the shipped record, after the kill, so the
        // record carries the late edges) and this per-row printf is not elaborated.  It stays
        // for ENABLE_IFT = false, where there is no sidecar and rob_uop.cf_* is not zeroed,
        // so an IFT-off build (BaselineBoomConfig) keeps its pre-Phase-2 [FLUSH] output.
        // [R124GATE3] patch 28 (B51): this IFT-off [FLUSH] record is IFT output (CF(...) fields,
        // all 0 IFT off) and was the only reader that kept rob_uop.inst/prs1/prs2/lrs*_rtype/br_tag
        // in every ROB row of an IFT-off build.  It is no longer elaborated (robIftOffFlushLog);
        // set it true to restore the pre-patch-28 IFT-off [FLUSH] lines.  IFT on: never elaborated
        // (unchanged).
        val robIftOffFlushLog = false
        if (ENABLE_CF_DEBUG_PRINTF && !ENABLE_IFT && robIftOffFlushLog) {
          val rbk_kill_i = (rob_state === s_rollback) && !full && (i.U === rob_tail)
          when (rob_val(i) && (IsKilledByBranch(io.brupdate, br_mask) || rbk_kill_i) && io.cf_debug_rob_enable) {
            // [FLUSH] log: full commit-log format for squashed entries
            // corefuzzing: add INFL_PIPELINE_FLUSH when flushing branch is from different domain
            // corefuzzing: single printf per entry — atomic in Verilator multi-threaded mode,
            // preventing interleaving of header/influencer-loop/footer across threads.
            // corefuzzing: no WireInit copy needed — read ROB state directly.
            // [SQUASHALL] Task 2.5: atk reads the sticky flag (spec 5.1.1), the only place the
            // STEP1 shadow promotion lands now; the same-cycle shadow is ORed in as before.
            // Pipeline-flush info is printed as separate fl/floc fields (like spec_atk/spec_oc),
            // completely outside the influencer list.  This eliminates the addInfluencer call
            // and WireInit(full MicroOp) that previously generated ~100 mux expressions per entry
            // across 128 entries, causing Rob.sv to balloon to 168 MB.
            val fu = rob_uop(i)
            val fl_cross_domain = io.cf_mispredict_uop.valid &&
              (io.cf_mispredict_uop.bits.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= fu.cf_domain_id.getOrElse(0.U(iftTagWidth.W)))
            val fl_is_secret = io.cf_mispredict_uop.valid &&
              (io.cf_mispredict_uop.bits.cf_secret_propagation.getOrElse(false.B) || io.cf_mispredict_uop.bits.cf_secret_access.getOrElse(false.B))
            val fl_op_count = Mux(io.cf_mispredict_uop.valid,
              io.cf_mispredict_uop.bits.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)), 0.U)
            val robFlushInflFmt = (0 until numInfluencerSlotsCF).zipWithIndex.map{case(_,k) => s"I$k={v=%d,oc=%d,ty=%d,atk=%d,sec=%d}"}.mkString(" ")
            val robFlushFmt = s"[FLUSH] 0x%x (0x%x) CF(domain=%d spec=%d atk=%d s_acc=%d s_prop=%d s_tx=%d opcount=%d spec_atk=%d spec_oc=%d fl=%d fl_sec=%d floc=%d FU=0x%x SRC=%d INFL_FU=0x%x OVF=%d dcy=%d srob=%d sstq=%d alias=%d pdst=%d prs1=%d prs2=%d rt1=%d rt2=%d brmask=0x%x brtag=%d abr=%d sbr=%d) $robFlushInflFmt\n"
            // [THINUOP] Task 3.5: this IFT-off printf used to read the removed MicroOp IFT fields,
            // which are constant zero in an IFT-off build (only the fetch buffer ever wrote them,
            // with zeros), so it prints the same zeros from a zero IftCommitInfo.
            val fu_ift = 0.U.asTypeOf(new IftCommitInfo)
            val robInflArgs = (0 until numInfluencerSlotsCF).flatMap(k => Seq[Bits](
              fu_ift.influencer_list(k).valid,
              fu_ift.influencer_list(k).op_count,
              fu_ift.influencer_list(k).infl_type,
              fu_ift.influencer_list(k).is_atk,
              fu_ift.influencer_list(k).is_secret
            ))
            printf(robFlushFmt, (Seq[Bits](
              Sext(fu.debug_pc(vaddrBits-1,0), xLen), fu.inst,
              fu.cf_domain_id.getOrElse(0.U(iftTagWidth.W)), fu_ift.speculated, (sticky_atk(i) || cf_in_atk_shadow),
              fu.cf_secret_access.getOrElse(false.B), fu.cf_secret_propagation.getOrElse(false.B), fu.cf_secret_transmission.getOrElse(false.B),
              fu.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)), fu_ift.spec_branch_is_atk, fu_ift.spec_branch_op_id,
              fl_cross_domain, fl_is_secret, fl_op_count,
              fu.cf_fu_bitmap.getOrElse(0.U(numModules.W)), Mux(rbk_kill_i, 4.U, 3.U), inflBitmapFromList(fu_ift.influencer_list), fu_ift.infl_dropped, fu_ift.cntd_deny_count, fu_ift.stall_cycles_rob, fu_ift.stall_cycles_stq, fu_ift.infl_oc_aliased, fu.pdst, fu.prs1, fu.prs2, fu.lrs1_rtype, fu.lrs2_rtype,
              // [BRMASK 2026-09-08] full speculation shadow + own tag; see core.scala
              br_mask, rob_uop(i).br_tag,
              fu_ift.atk_branch_ctr, fu_ift.sec_branch_ctr   // [GHISTFIELD]
            ) ++ robInflArgs): _*)
          }
        }

        //kill instruction if mispredict & br mask match
        // [SQUASHALL] Task 2.5 (D5, spec 5.1.6): every branch-killed valid row is marked for
        // a squash record.  The old IFT-activity gate (cf_has_ift_activity), the clears of
        // the old sprob/ic/wb pending tables and the [A6PROBE] measurement are gone with the
        // tables (ruling 38 item 2); the drain (Task 2.6: tail order, sidecar squash readout)
        // reads the row one or more cycles later, so nothing here touches rob_uop(i).
        when (IsKilledByBranch(io.brupdate, br_mask)) {
          rob_val(i) := false.B
          // NOTE: no `debug_inst := BUBBLE` here any more (Phase 0 bug 7): the squash
          // record is read from this row one or more cycles later.
          if (ENABLE_IFT) {
            when (rob_val(i)) { ift_squash_pending(i) := true.B }      // D5: every killed row
          }
        } .elsewhen (rob_val(i)) {
          // clear speculation bit even on correct speculation
          rob_uop(i).br_mask := GetNewBrMask(io.brupdate, br_mask)
        }
        // [STEP1] attacker shadow: this uop is in the window of a branch whose CONDITION
        // was attacker-influenced.  Tested against the CURRENT br_mask, so the resolving
        // branch's own tag still matches before GetNewBrMask retires it.  The promotion
        // lands in the sticky flag only (spec 5.1.1): the commit readout (Task 2.4) and the
        // squash readout with its [FLUSH] printf (Task 2.6 drain) read sticky_atk, so the
        // rob_uop.cf_attacker_influence write is gone.
        if (ENABLE_IFT) {
          when (rob_val(i) && cf_in_atk_shadow) { sticky_atk(i) := true.B }
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
    // [FFTAINT] step 18: same head-row read as the flags; sticky_* are constant-false
    // wires without ENABLE_IFT, and the domain term is gated here.
    rob_head_ffsec(w)    := sticky_s_acc(rob_head) || sticky_s_prop(rob_head)
    rob_head_ffatk(w)    := sticky_atk(rob_head) ||
                            (ENABLE_IFT.B && rob_uop(rob_head).cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= 0.U)
    rob_head_uses_stq(w) := rob_uop(rob_head).uses_stq
    rob_head_uses_ldq(w) := rob_uop(rob_head).uses_ldq

    // -------------------------------------------------
    // CoreFuzzing outputs
    if (w == 0) {
      rob_head_oc_bank0  := rob_uop(rob_head).cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
      rob_head_dom_bank0 := rob_uop(rob_head).cf_domain_id.getOrElse(0.U(iftTagWidth.W))
    }
    // [SIDECAR] Task 2.4: head secret status from the sticky flags (bank 0 = retire slot 0)
    if (w == 0) rob_head_is_secret_bank0 := sticky_s_acc(rob_head) || sticky_s_prop(rob_head)
    // [ROBHEADATK] patch 48 (r173 (6), task-reports/p4-owner-gap-check.md C-rob): the same head row,
    // bank 0, through the sidecar's EXISTING commit readout (its row is com_idx = rob_head outside
    // rollback; rollback reads the tail, so it is masked).  An output only: no new ROB state, no new
    // write port or path into the ROB (r98).  sp_atk is value taint (taint_atk / window operand
    // taint of a domain-0 register write, not a line-borne load value), never a sticky summary, so
    // the stalled consumers' sticky atk can never feed it back (no chain, the r168a principle).
    if (ENABLE_IFT && w == 0) rob_head_valatk_bank0 := sidecar.io.commit_valatk && rob_state =/= s_rollback

    // IFT bridge commit records: one per retire slot, valid on arch_valids.  Packed from
    // io.commit.uops (the Wire carrying the sidecar readout, Task 2.4), not from rob_uop.
    // D2a (Task 2.4): an invalid retire lane packs event_type 0 instead of a constant 1, so
    // the bridge can compact the valid records (D12).  2-bit literals: uopToIFTBits slices
    // event_type(1, 0), which Chisel rejects on a 1-bit Mux result.  Under ENABLE_IFT only.
    val com_evt = if (ENABLE_IFT) Mux(io.commit.arch_valids(w), 1.U(2.W), 0.U(2.W)) else 1.U(2.W)
    // [RECREG] (Phase 4 Task 4.3, spec 5.3, ruling 44) the bridge record and its valid are
    // registered together under ENABLE_IFT, so the ROB readout -> uopToIFTBits -> core -> tile
    // -> ChipTop -> bridge leg starts at a flop beside the ROB.  The commit printf and every
    // commit-time consumer in core.scala still read io.commit.* in the commit cycle (unchanged
    // logs).  Record-identical: each record is the same bits one cycle later, with its valid.
    // The squash lane gets the matching extra cycle in core.scala ([RECREG] there), so the
    // commit/squash order on the bridge token stream is unchanged.  Cost: retireWidth x 257 FF.
    val com_rec = uopToIFTBits(io.commit.uops(w), com_ift,
      (com_sticky(0), com_sticky(1), com_sticky(2), com_sticky(3)), com_evt, io.ift_priv)
    if (ENABLE_IFT) {
      io.ift_commit_valids(w)  := RegNext(io.commit.arch_valids(w), false.B)
      io.ift_commit_records(w) := RegNext(com_rec, 0.U)
    } else {
      io.ift_commit_valids(w)  := io.commit.arch_valids(w)
      io.ift_commit_records(w) := com_rec
    }
    
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
    // [SQUASHALL] Task 2.6: the rollback-walk BUBBLE write is gone with ENABLE_IFT (the
    // rollback record must carry the instruction); kept for ENABLE_IFT = false (ruling 5).
    when (will_commit(w)) {
      rob_uop(rob_head).debug_inst := BUBBLE
    } .elsewhen (rbk_row && (!ENABLE_IFT).B)
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
      // corefuzzing [SIDECAR] Task 2.3: the writeback's IFT contribution goes to this
      // port's sidecar column (edges + fu_bitmap on the LSU-fed ports, fu_bitmap only on
      // the others; the sidecar picks the shape by port index) and to the sticky bits.
      // Guarded by rob_val && rob_bsy like the busy-clear above, to drop stale wakeups of
      // branch-killed instructions.  The old read-modify-write of rob_uop(row).cf_* and the
      // writeback pending-list capture are gone; the commit readout (Task 2.4) merges the column.
      // The branch-counter max merge is dropped: every post-dispatch copy of the uop zeroes
      // cf_atk/sec_branch_ctr (issue slots, FUs, LSU), so it never changed the row
      // (spec 5.1.5 step 6 amendment, ruling 29).
      if (ENABLE_IFT) {
        when (io.wb_resps(i).valid && MatchBank(GetBankIdx(rob_idx)) && rob_val(GetRowIdx(rob_idx)) && rob_bsy(GetRowIdx(rob_idx))) {
          val rob_row  = GetRowIdx(rob_idx)
          val wb_uop_i = io.wb_resps(i).bits.uop
          // sticky flags (same semantics as the old per-row ORs)
          when (wb_uop_i.cf_attacker_influence.getOrElse(false.B)) { sticky_atk(rob_row) := true.B }
          // [R0 2026-09-04] the RESULT's attacker taint AS WRITTEN TO THE REGISTER FILE
          // (core.scala: taint_atk || domain seed), on a real int/FP register write
          // ([FPRTYPE 2026-09-08]: RT_FLT included; rf_wen && ldst_val exclude stores and
          // branches).  Not widened to cf_attacker_influence: it is the value's own taint.
          val wb_res_atk = io.wb_resps(i).bits.taint_atk || (wb_uop_i.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= 0.U)
          when (wb_res_atk && wb_uop_i.rf_wen && wb_uop_i.ldst_val &&
                (wb_uop_i.dst_rtype === RT_FIX || wb_uop_i.dst_rtype === RT_FLT)) { sticky_atk(rob_row) := true.B }
          when (wb_uop_i.cf_secret_access.getOrElse(false.B))       { sticky_s_acc(rob_row)  := true.B }
          when (wb_uop_i.cf_secret_transmission.getOrElse(false.B)) { sticky_s_tx(rob_row)   := true.B }
          when (wb_uop_i.cf_secret_propagation.getOrElse(false.B))  { sticky_s_prop(rob_row) := true.B }
          // Fix 3c: dcache memsec_fire marks INFL_MEM_DATAFLOW on cf_mem_sec_dataflow (the
          // dcache does not write cf_secret_propagation); it is s_prop on the ROB entry.
          when (wb_uop_i.cf_mem_sec_dataflow.getOrElse(false.B))    { sticky_s_prop(rob_row) := true.B }
          // column write: edges (LSU-fed ports), fu_bitmap (all ports), stamp.  Task 3.2 (D16):
          // the edges are the response's own list; non-LSU ports tie it to zero (unread).
          sidecar.io.wb(i).valid          := true.B
          sidecar.io.wb(i).bits.row       := rob_row
          sidecar.io.wb(i).bits.edges     := io.wb_resps(i).bits.edges
          sidecar.io.wb(i).bits.fu_bitmap := wb_uop_i.cf_fu_bitmap.getOrElse(0.U(numModules.W))
        }
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

    // [SCFWD] patch 40b (D17 sweep, phase4/patches/p4-40b-review.md): the commit readout is an
    // asynchronous read of columns written at the clock edge, so a column or sticky write in its
    // row's commit cycle would be lost.  Only C_ras can do that (b2 vs a head-ready ret) and it
    // is forwarded into the commit merge ([RASBYP], rob-ift-sidecar.scala).  For every other
    // port the write precedes the commit by at least one edge (proof per port in the report);
    // these checks make a later change that breaks it a hard stop.  Verilator printf builds
    // only (iftSqShadow: ENABLE_CF_DEBUG_PRINTF and no bridge), like the squash shadow check:
    // no FireSim or NoLog Verilog changes.  The guarded ports use rob_uop's op_count (the value
    // C_oc holds) so no sidecar guard port is added.
    if (ENABLE_IFT && iftSqShadow) {
      def scChk(v: Bool, row: UInt, what: String): Unit =
        assert(!(will_commit(w) && v && row === com_idx),
          "[rob] [SCFWD] " + what + " lands in its row's commit cycle (lost by the commit readout)")
      def scOk(row: UInt, oc: UInt): Bool =
        rob_val(row) && rob_uop(row).cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)) === oc
      sidecar.io.wb.zipWithIndex.foreach     { case (p, k) => scChk(p.valid, p.bits.row, s"sidecar wb($k)") }
      sidecar.io.clr.zipWithIndex.foreach    { case (p, k) => scChk(p.valid, p.bits.row, s"sidecar clr($k)") }
      sidecar.io.sprob.zipWithIndex.foreach  { case (p, k) => scChk(p.valid, p.bits.row, s"sidecar sprob($k)") }
      sidecar.io.ic.zipWithIndex.foreach     { case (p, k) => scChk(p.valid, p.bits.row, s"sidecar ic($k)") }
      sidecar.io.memord.zipWithIndex.foreach { case (p, k) => scChk(p.valid, p.bits.row, s"sidecar memord($k)") }
      // sticky-only ports (same enables as their writes above)
      def scUpd(v: Bool, idx: UInt, oc: UInt, what: String): Unit = {
        val r = GetRowIdx(idx)
        scChk(v && MatchBank(GetBankIdx(idx)) && scOk(r, oc), r, what)
      }
      io.cf_lsu_s_acc_upd.zipWithIndex.foreach { case (u, k) => scUpd(u.valid, u.bits.rob_idx, u.bits.op_count_id, s"cf_lsu_s_acc_upd($k)") }
      io.cf_lsu_s_tx_upd.zipWithIndex.foreach  { case (u, k) => scUpd(u.valid, u.bits.rob_idx, u.bits.op_count_id, s"cf_lsu_s_tx_upd($k)") }
      io.cf_br_atk_self.zipWithIndex.foreach   { case (u, k) => scUpd(u.valid, u.bits.rob_idx, u.bits.op_count_id, s"cf_br_atk_self($k)") }
    }

    // [SIDECAR] Task 2.7 (spec 5.1.7): on the ROB pointer reset (quiesce drain / ROB-size
    // change; the ROB is empty) the squash-pending bits and the four sticky flags are cleared.
    // Last connect in the bank, so it wins over a same-cycle kill, writeback or enqueue write.
    // The sidecar columns are cleared by sidecar.io.clear_all (cf_rob_quiesce_reset, Task 2.2);
    // records already in the squash FIFO drain normally.
    if (ENABLE_IFT) {
      when (ift_rob_ptr_reset) {
        ift_squash_pending.foreach(_ := false.B)
        Seq(sticky_s_acc, sticky_s_prop, sticky_s_tx, sticky_atk).foreach(_.foreach(_ := false.B))
      }
    }

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

  // [FFTAINT] step 18 (ruling 118, G-2): a retire slot SETS the flags taint only when its
  // committed flags are non-zero (fflags(w) is already gated by fflags_val(w): commit.valids,
  // fp_val, !uses_stq; FP loads carry 0 flags, asserted above).  A tainted op whose flags stay
  // 0 is knowingly untagged (owner).  Per slot: a 5-bit OR and an AND, then a coreWidth-wide
  // OR, after the existing head-row readout.  The bits go to core.scala's sticky registers.
  val ff_raised = fflags.map(_.orR)
  if (ENABLE_IFT) {
    io.cf_ff_set_sec := (ff_raised zip rob_head_ffsec).map { case (r, t) => r && t }.reduce(_||_)
    io.cf_ff_set_atk := (ff_raised zip rob_head_ffatk).map { case (r, t) => r && t }.reduce(_||_)
  } else {
    io.cf_ff_set_sec := false.B
    io.cf_ff_set_atk := false.B
  }

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

  // corefuzzing: reset ROB head/tail to 0 when cf_rob_rows changes (last-write-wins over WrapInc above).
  // Two triggers:
  //   cf_rob_quiesce_reset — quiesce drain path (software-initiated, ROB already empty)
  //   cf_rob_entries_wen_pending && flush_commit — DMI CSRRW to robSizeCSRCF
  //     wen fires at CSR writeback time (1-2 cycles before ROB commit); the sticky flag
  //     arms on wen and the reset fires on the subsequent flush_commit from that same CSRRW.
  //     CSRRW is is_unique so no other flush_on_commit can intervene between wen and commit.
  val cf_rob_entries_wen_pending = RegInit(false.B)
  when (io.cf_rob_entries_wen)                              { cf_rob_entries_wen_pending := true.B  }
  .elsewhen (flush_commit && !exception_thrown)             { cf_rob_entries_wen_pending := false.B }
  ift_rob_ptr_reset := io.cf_rob_quiesce_reset || (cf_rob_entries_wen_pending && flush_commit && !exception_thrown)
  when (ift_rob_ptr_reset) {
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
  // [HEADREG] (Phase 4 Task 4.2) the head status is registered under ENABLE_IFT (IFT off:
  // plain wires).  Before: rob_head -> 64:1 row mux (rob_uop / sticky columns) -> ROB->core
  // route -> the dispatch stall captures.  After: the route starts at a flop.  core.scala's
  // consumers read it only one cycle after a stall cycle (see [HEADREG] there), so every
  // record is unchanged.  Cost: 16 + 1 + 1 = 18 FF.
  def headReg[T <: Data](x: T): T = if (ENABLE_IFT) RegNext(x, 0.U.asTypeOf(x)) else x
  io.rob_head_op_count  := headReg(rob_head_oc_bank0)
  io.rob_head_domain    := headReg(rob_head_dom_bank0)
  io.rob_head_is_secret := headReg(rob_head_is_secret_bank0)
  io.rob_head_valatk.foreach(_ := headReg(rob_head_valatk_bank0))   // [ROBHEADATK] patch 48: 1 FF
  io.empty        := empty
  io.ready        := (rob_state === s_normal) && !full && !r_xcpt_val

  // IFT bridge: per-bank raw squash FIFOs and the shared merge onto the single squash lane
  // (spec 5.4.3, Task 2.6; Task 2.10d design 4b).  The banks never wait (D6): a full raw FIFO
  // drops the capture and counts it; a row re-allocated before its capture is counted too
  // (sq_reuse), and so are the pending rows a ROB pointer reset clears (D-22, Task 2.10d).
  // Under ENABLE_IFT only (ruling 5), constant outputs otherwise.
  // Saturating 16-bit accumulate (the +& sum is one bit wider; its carry clamps).
  def satAdd16(a: UInt, b: UInt): UInt = { val s = a +& b; Mux(s(16), "hFFFF".U(16.W), s(15, 0)) }
  if (ENABLE_IFT) {
    val sqm = Module(new IftSquashMerge(coreWidth, iftNSprob, iftNIc, lsuWbPorts.length, iftNMo,
                                        dbg = iftSqDbgOn, shadow = iftSqShadow))
    sqm.io.cap := ift_sq_cap
    sqm.io.dbg := ift_sq_dbg
    io.ift_squash <> sqm.io.deq
    // Task 2.10c (ruling 48): keep the squash readout alive in builds without the IFT
    // bridge (the DMI config), so its logic is simulated and counted by the oracle.  No
    // effect in FireSim, where the bridge consumes both ports.
    dontTouch(io.ift_squash)
    // 0xbce [47:32]: records lost before the merge path, i.e. rows re-allocated while still
    // pending (sq_reuse) and, since Task 2.10d, pending rows cleared by a pointer reset (D-22).
    // [QSREG R3] (Phase 4 timing, r142) the per-cycle increment is registered before the
    // accumulate.  ift_sq_ptrrst_bank hangs off ift_rob_ptr_reset (io.cf_rob_quiesce_reset,
    // the quiesce FSM), and PopCount + 4-way sum + saturating add was the 14-level tail of the
    // routed worst path.  Equivalence: the same increments are summed in the same order, so
    // reuse_cnt (and ift_squash_ovf_cnt / ovf_flag, which read it) holds exactly the old value
    // one cycle later.  These feed only 0xbce and the bridge squash_ovf input, which the bridge
    // ignores (IFTBridgeModule.scala); no record field carries them.  Software cannot see the
    // lag: a reuse event is a row allocation at dispatch and a pointer reset is a quiesce or
    // 0xbc2-commit flush, while a csrr of 0xbce is unique and executes only after the ROB has
    // drained, many cycles later.  Cost: +10 FF (3 + 4x7 summed to 10 b), -~12 levels.
    val reuse_inc = RegNext(PopCount(ift_sq_reuse_bank) +& ift_sq_ptrrst_bank.reduce(_ +& _), 0.U)
    val reuse_cnt = RegInit(0.U(16.W))
    reuse_cnt := satAdd16(reuse_cnt, reuse_inc)
    val third_cnt = RegInit(0.U(16.W))
    third_cnt := satAdd16(third_cnt, PopCount(ift_third_drop_bank ++ ift_third_drop_sq_bank))
    io.ift_squash_ovf_cnt   := satAdd16(sqm.io.ovf_cnt, reuse_cnt)
    io.ift_squash_reuse_cnt := reuse_cnt
    io.ift_third_drop_cnt   := third_cnt
    // Task 2.7 (D6): sticky loss flag, set once either loss counter is non-zero (both only
    // count up and saturate).  Reads two registers, one cycle after the counter moves.
    val ovf_flag = RegInit(false.B)
    ovf_flag := ovf_flag || sqm.io.ovf_cnt.orR || reuse_cnt.orR
    io.ift_squash_ovf := ovf_flag
  } else {
    io.ift_squash.valid     := false.B
    io.ift_squash.bits      := 0.U
    io.ift_squash_ovf       := false.B
    io.ift_squash_ovf_cnt   := 0.U
    io.ift_squash_reuse_cnt := 0.U
    io.ift_third_drop_cnt   := 0.U
  }

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
