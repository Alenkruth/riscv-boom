//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// MicroOp
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.v3.common

import chisel3._
import chisel3.util._

// for Corefuzzing
import freechips.rocketchip.util._

import org.chipsalliance.cde.config.Parameters

import boom.v3.exu.FUConstants

/**
 * Extension to BoomBundle to add a MicroOp
 */
abstract trait HasBoomUOP extends BoomBundle
{
  val uop = new MicroOp()
}

/**
 * One entry in the per-uop influencer list (IFT Phase 2).
 * Records the op_count and influence type of a single cross-domain influence event.
 */
class InfluencerEntry(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val valid     = Bool()
  // LOW inflOpCountWidthCF bits of the influencing uop's op_count_id; software
  // reconstructs the full value against the consuming record's own op_count.
  // See inflOpCountWidthCF in CoreFuzzing.scala for the range argument.
  val op_count  = UInt(inflOpCountWidthCF.W)
  val infl_type = UInt(inflTypeWidthCF.W)       // 5 bits: influence type (INFL_*)
  val is_atk    = Bool()   // influencer instruction was from attacker domain (domain=1)
  val is_secret = Bool()   // influencer instruction had s_acc=1 or s_prop=1
  // NOTE: deny_count used to live here, replicated into every slot.  It is now the
  // per-uop cf_cntd_deny_count: a uop can hold at most one INFL_ISSUE_CONTENTION
  // entry (the issue slot latches the FIRST denier and aggregates the count over all
  // of them), so the per-slot association was already fictional past the first
  // denial.  Measured: deny_count != 0 on exactly 95,815 entries, identical to the
  // INFL_ISSUE_CONTENTION count.
}

/**
 * Payload of the LSU→ROB direct s_acc update bus.
 * Includes op_count_id so the ROB can validate the slot has not been reused
 * (ROB slot reuse hazard: a squashed instruction's late TLB-stage update must
 *  not corrupt a new instruction that was allocated the same rob_idx).
 */
class CF_SAccUpdate(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val rob_idx    = UInt(robAddrSz.W)
  val op_count_id = UInt(uopIDCounterWidthCF.W)
}

/** [FIFO10] step 10 (owner ruling 93, R-STICKY): the TLB-stage s_acc message also carries a
  * LOAD's issue-path attacker / s_prop flags (operand taint from register-read and the slot's
  * wakeup-time secret), so a load killed between the TLB stage and writeback reaches its
  * squash record with the same summary bits a committed load gets at writeback. */
class CF_TlbFlagsUpdate(implicit p: Parameters) extends CF_SAccUpdate {
  val s_acc  = Bool()
  val atk    = Bool()
  val s_prop = Bool()
}

// [MEMORD 2026-09-08] Routes an INFL_MEM_ORDER edge from the LSU into the ROB entry.
//
// WHY THIS PORT EXISTS.  lsu.scala writes the ty=8 edge into `ldq(i).bits.uop` when a
// store's late-resolving address finds a younger load that already read stale data.  That
// edge is then DESTROYED, three ways over:
//   1. order_fail raises MINI_EXCEPTION_MEM_ORDERING -> rob flush -> io.lsu.exception ->
//      lsu.scala:2394 `ldq_head := 0; ldq_tail := 0` wipes the whole queue;
//   2. an LDQ uop's only ride to the ROB is a load writeback, and the order_fail condition
//      REQUIRES the load to have already executed -- that ride has departed;
//   3. the replay does not re-violate: by then the store address is resolved and the load
//      forwards correctly, so the edge is never regenerated.
// Result: ty=8 has never once appeared in any log.
//
// The violating load is squashed BY DEFINITION, so the edge belongs on the [FLUSH] record.
// Writing it into rob_uop here puts it exactly there (rob.scala already gates flush
// logging on influencer presence), and order_fail is detected well before the exception is
// taken at the head, so there is ample slack.
class CF_MemOrdUpdate(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val rob_idx       = UInt(robAddrSz.W)
  val op_count_id   = UInt(uopIDCounterWidthCF.W)   // guard against ROB slot reuse
  val prod_op_count = UInt(inflOpCountWidthCF.W)    // the conflicting store/load's op_count
  val is_atk        = Bool()
  val is_secret     = Bool()
}

/** ROB update for cf_secret_propagation, carrying WHY it fired.
  *
  * core.scala emits an s_prop update on two very different conditions:
  *   (a) a real source taint  -- preg_secret[prs1|prs2], or a secret mem dataflow;
  *   (b) the instruction is itself the secret access (wb_uop.cf_secret_access),
  *       fired purely as bookkeeping so downstream consumers see the taint chain.
  *
  * rob.scala's Gap-2 setter must only turn (a) into cf_secret_transmission: for a
  * load, prs1 IS the address register, so (a) literally means "this load's address
  * is secret-derived" -- a genuine transmission.  Case (b) transmits nothing.
  *
  * The two arrive as identical valid pulses on the same port, so the ROB cannot
  * tell them apart.  Its `!rob_uop(cidx).cf_secret_access` guard was intended to,
  * but cannot: that bit is written by the SAME writeback (rob.scala:1012) in the
  * SAME cycle, so the guard reads the stale register value and never fires.
  * Measured on run_00016: zero records anywhere with s_acc=1, uses_ldq, s_tx=0 --
  * the guard had no observable effect in 190,653 load-with-s_prop events, while
  * all 10 secret-range loads were tagged s_tx despite transmitting nothing.
  *
  * Carrying the reason on the update makes the distinction explicit and immune to
  * the cycle timing.  One bit per wakeup port.
  */
class CF_SPropUpdate(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val rob_idx     = UInt(robAddrSz.W)
  val op_count_id = UInt(uopIDCounterWidthCF.W)
  val src_tainted = Bool()
  // corefuzzing: this op's OWN effective address was in the secret range (its
  // cf_secret_access, set by the LSU at TLB).  The ROB's retroactive s_tx rule needs
  // it to reproduce the LSU's `!in_secret` guard: a load whose pointer lands IN
  // secret memory stays inside the secure domain and is an ACCESS, not a
  // TRANSMISSION.  Without this the secret load tags itself as its own transmitter.
  val self_secret_acc = Bool()
}

/**
 * Payload for the TLB-stage preg_secret early-update bus.
 * is_fp=true  → pdst indexes the FP physical register file  (fp_preg_secret)
 * is_fp=false → pdst indexes the INT physical register file (preg_secret)
 */
class CF_PregSecretUpd(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val pdst  = UInt(ipregSz.W)
  val is_fp = Bool()
}

/**
 * MicroOp passing through the pipeline
 */
class MicroOp(implicit p: Parameters) extends BoomBundle
  with freechips.rocketchip.rocket.constants.MemoryOpConstants
  with freechips.rocketchip.rocket.constants.ScalarOpConstants
  with CoreFuzzingConstants // for CoreFuzzing
{
  val uopc             = UInt(UOPC_SZ.W)       // micro-op code
  val inst             = UInt(32.W)
  val debug_inst       = UInt(32.W)
  val is_rvc           = Bool()
  val debug_pc         = UInt(coreMaxAddrBits.W)
  val iq_type          = UInt(IQT_SZ.W)        // which issue unit do we use?
  val fu_code          = UInt(FUConstants.FUC_SZ.W) // which functional unit do we use?
  val ctrl             = new CtrlSignals

  // What is the next state of this uop in the issue window? useful
  // for the compacting queue.
  val iw_state         = UInt(2.W)
  // Has operand 1 or 2 been waken speculatively by a load?
  // Only integer operands are speculaively woken up,
  // so we can ignore p3.
  val iw_p1_poisoned   = Bool()
  val iw_p2_poisoned   = Bool()

  val is_br            = Bool()                      // is this micro-op a (branch) vs a regular PC+4 inst?
  val is_jalr          = Bool()                      // is this a jump? (jal or jalr)
  val is_jal           = Bool()                      // is this a JAL (doesn't include JR)? used for branch unit
  val is_sfb           = Bool()                      // is this a sfb or in the shadow of a sfb

  val br_mask          = UInt(maxBrCount.W)  // which branches are we being speculated under?
  val br_tag           = UInt(brTagSz.W)

  // Index into FTQ to figure out our fetch PC.
  val ftq_idx          = UInt(log2Ceil(ftqSz).W)
  // This inst straddles two fetch packets
  val edge_inst        = Bool()
  // Low-order bits of our own PC. Combine with ftq[ftq_idx] to get PC.
  // Aligned to a cache-line size, as that is the greater fetch granularity.
  // TODO: Shouldn't this be aligned to fetch-width size?
  val pc_lob           = UInt(log2Ceil(icBlockBytes).W)

  // Was this a branch that was predicted taken?
  val taken            = Bool()

  val imm_packed       = UInt(LONGEST_IMM_SZ.W) // densely pack the imm in decode...
                                              // then translate and sign-extend in execute
  val csr_addr         = UInt(CSR_ADDR_SZ.W)    // only used for critical path reasons in Exe
  val rob_idx          = UInt(robAddrSz.W)
  val ldq_idx          = UInt(ldqAddrSz.W)
  val stq_idx          = UInt(stqAddrSz.W)
  val rxq_idx          = UInt(log2Ceil(numRxqEntries).W)
  val pdst             = UInt(maxPregSz.W)
  val prs1             = UInt(maxPregSz.W)
  val prs2             = UInt(maxPregSz.W)
  val prs3             = UInt(maxPregSz.W)
  val ppred            = UInt(log2Ceil(ftqSz).W) // PP ready?

  val prs1_busy        = Bool()
  val prs2_busy        = Bool()
  val prs3_busy        = Bool()
  val ppred_busy       = Bool()
  val stale_pdst       = UInt(maxPregSz.W)
  val exception        = Bool()
  val exc_cause        = UInt(xLen.W)          // TODO compress this down, xlen is insanity
  val bypassable       = Bool()                      // can we bypass ALU results? (doesn't include loads, csr, etc...)
  val mem_cmd          = UInt(M_SZ.W)          // sync primitives/cache flushes
  val mem_size         = UInt(2.W)
  val mem_signed       = Bool()
  val is_fence         = Bool()
  val is_fencei        = Bool()
  val is_amo           = Bool()
  val uses_ldq         = Bool()
  val uses_stq         = Bool()
  val is_sys_pc2epc    = Bool()                      // Is a ECall or Breakpoint -- both set EPC to PC.
  val is_unique        = Bool()                      // only allow this instruction in the pipeline, wait for STQ to
                                                     // drain, clear fetcha fter it (tell ROB to un-ready until empty)
  val flush_on_commit  = Bool()                      // some instructions need to flush the pipeline behind them

  // Preditation
  def is_sfb_br        = is_br && is_sfb && enableSFBOpt.B // Does this write a predicate
  def is_sfb_shadow    = !is_br && is_sfb && enableSFBOpt.B // Is this predicated
  val ldst_is_rs1      = Bool() // If this is set and we are predicated off, copy rs1 to dst,
                                // else copy rs2 to dst

  // logical specifiers (only used in Decode->Rename), except rollback (ldst)
  val ldst             = UInt(lregSz.W)
  val lrs1             = UInt(lregSz.W)
  val lrs2             = UInt(lregSz.W)
  val lrs3             = UInt(lregSz.W)

  val ldst_val         = Bool()              // is there a destination? invalid for stores, rd==x0, etc.
  val dst_rtype        = UInt(2.W)
  val lrs1_rtype       = UInt(2.W)
  val lrs2_rtype       = UInt(2.W)
  val frs3_en          = Bool()

  // floating point information
  val fp_val           = Bool()             // is a floating-point instruction (F- or D-extension)?
                                            // If it's non-ld/st it will write back exception bits to the fcsr.
  val fp_single        = Bool()             // single-precision floating point instruction (F-extension)

  // frontend exception information
  val xcpt_pf_if       = Bool()             // I-TLB page fault.
  val xcpt_ae_if       = Bool()             // I$ access exception.
  val xcpt_ma_if       = Bool()             // Misaligned fetch (jal/brjumping to misaligned addr).
  val bp_debug_if      = Bool()             // Breakpoint
  val bp_xcpt_if       = Bool()             // Breakpoint


  // What prediction structure provides the prediction FROM this op
  val debug_fsrc       = UInt(BSRC_SZ.W)
  // What prediction structure provides the prediction TO this op
  val debug_tsrc       = UInt(BSRC_SZ.W)

  // Corefuzzing - IFT tags for every micro-op
  // adding the tag to the Microp to let it propagate through the pipeline once created
  // [THINUOP] Phase 3 Task 3.5: the uop carries only the closed-loop IFT fields (D13) plus
  // cf_spec_branch_is_secret (ruling 74).  Everything else lives in the sidecars/side
  // bundles (FetchIftInfo, IftDispatchInfo, IssueCntdCarry, IftCommitInfo, IftEdgeList).
  // [R124GATE3] patch 28 (B51): the pre-overhaul uop IFT fields below are Option fields, Some
  // only with ENABLE_IFT (the patch-27 method, see cf_mem_*).  IFT off they do not exist, so no
  // uop copy (ROB, LDQ/STQ, IQ slots, BKQs, fetch buffer, FU pipes, D$ s1/s2, MSHRs) holds them.
  // IFT on: the same type at the same position, so the IFT-on bundle and Verilog are unchanged.
  val cf_domain_id            = if (ENABLE_IFT) Some(UInt(iftTagWidth.W)) else None
  val cf_attacker_influence   = if (ENABLE_IFT) Some(Bool()) else None
  val cf_secret_access        = if (ENABLE_IFT) Some(Bool()) else None      // set when the micro-op accesses a secret
  val cf_secret_propagation   = if (ENABLE_IFT) Some(Bool()) else None      // set when the micro-op is in the dependence chain of originating in a secret
  val cf_secret_transmission  = if (ENABLE_IFT) Some(Bool()) else None      // set when a secret dependent micro-op makes a update to a stateful unit

  val cf_op_count_id = if (ENABLE_IFT) Some(UInt(uopIDCounterWidthCF.W)) else None // 16-bit sequential uop counter, assigned in fetch-buffer.
                               // Wraps at 65536. Provides comfortable headroom for all current workloads.

  val cf_ras_pred_xdom        = if (ENABLE_IFT) Some(Bool()) else None      // ret whose RAS-predicted top was cross-domain (doc 67 Part B); RAS edge raised at ret-mispredict in ROB

  // Bitmap of all pipeline modules this uop has visited (one bit per module, see CoreFuzzingConstants)
  val cf_fu_bitmap            = if (ENABLE_IFT) Some(UInt(numModules.W)) else None

  // Summary flags set by dcache to avoid scanning an influencer list on writeback paths.
  // [R124GATE2] patch 27 (r124/B51): this field and the step-20/21 window fields below are
  // Option fields, Some only with ENABLE_IFT.  firtool does not fold a constant through the
  // uop register copies (issue-slot collapse, FU pipelines, LSU queues, BKQs, D$ s1/s2, MSHRs),
  // so a constant-0 source still left one flop per copy IFT off.  IFT off the field does not
  // exist; writers use `.foreach(_ := rhs)`, readers `.getOrElse(<zero>)`.  IFT on it is the
  // same Bool/UInt at the same position, so the IFT-on bundle and Verilog are unchanged.
  val cf_mem_dataflow_atk  = if (ENABLE_IFT) Some(Bool()) else None  // INFL_MEM_DATAFLOW(is_atk=true)  injected at dcache
  val cf_mem_sec_dataflow  = if (ENABLE_IFT) Some(Bool()) else None  // INFL_MEM_DATAFLOW(is_atk=false, is_secret=true) injected

  // Ruling 74: stays in MicroOp (read by the dispatch s_prop set; the dispatch column takes it from the uop).
  val cf_spec_branch_is_secret = if (ENABLE_IFT) Some(Bool()) else None  // any outstanding branch in br_mask was from secret domain

  // [IMPLWIN] step 20 (rulings 132/134/135): WINDOW taint, a second register-taint kind beside
  // the DATA taint (regfile `secret` / `taint_atk`).  Window-secret means "produced under
  // secret-dependent control"; it never upgrades to data-secret and never re-arms the window.
  //   cf_win_sec/atk   : this uop is IN the open window (fetched or dispatched while it is
  //                      open; fetch-buffer.scala, core.scala dispatch).  Seeds its result.
  //                      [WINEXP] step 21: core.scala dispatch rewrites it to "dispatched while
  //                      open" (stamped cf_wep_*); the fetch-time bit still gives s_prop.
  //   cf_opnd_wsec/watk: an operand this uop read carried window taint (register-read.scala).
  // Result window taint = cf_win_* || cf_opnd_* (regfile write ports, bypass).  1 bit each.
  val cf_win_sec   = if (ENABLE_IFT) Some(Bool()) else None   // [R124GATE2] Option, see cf_mem_*
  val cf_win_atk   = if (ENABLE_IFT) Some(Bool()) else None
  val cf_opnd_wsec = if (ENABLE_IFT) Some(Bool()) else None
  val cf_opnd_watk = if (ENABLE_IFT) Some(Bool()) else None
  // [WINEXP] step 21 (owner r139, F3): the window EPOCH of each kind at this uop's DISPATCH.
  // Window taint that this uop produces (membership or window operands) is stamped with it and
  // counts only while it equals the core's current epoch, which advances when that window
  // CLOSES (counter reaches 0, or [WINDISARM]).  Every uop in flight at a close is younger than
  // the branch that closed it, i.e. outside the window, so its result and its memory marks die.
  val cf_wep_sec   = if (ENABLE_IFT) Some(UInt(2.W)) else None   // [R124GATE2]
  val cf_wep_atk   = if (ENABLE_IFT) Some(UInt(2.W)) else None
  // [WINMEM] step 21 (owner r139, F2): STORES only (uses_stq).  Register-read gives a store's
  // ADDRESS (rs1) window taint in cf_opnd_* and its DATA (rs2) window taint here, so the STQ
  // keeps them apart even for a fused STA+STD issue.  0 for every other uop.
  val cf_dwin_sec  = if (ENABLE_IFT) Some(Bool()) else None   // [R124GATE2]
  val cf_dwin_atk  = if (ENABLE_IFT) Some(Bool()) else None

  // Do we allocate a branch tag for this?
  // SFB branches don't get a mask, they get a predicate bit
  def allocate_brtag   = (is_br && !is_sfb) || is_jalr

  // Does this register write-back
  def rf_wen           = dst_rtype =/= RT_X

  // Is it possible for this uop to misspeculate, preventing the commit of subsequent uops?
  def unsafe           = uses_ldq || (uses_stq && !is_fence) || is_br || is_jalr

  def fu_code_is(_fu: UInt) = (fu_code & _fu) =/= 0.U

  override def toPrintable: Printable = {
    val cf_info = Cat(cf_domain_id.getOrElse(0.U(iftTagWidth.W)), cf_attacker_influence.getOrElse(false.B),
                      cf_secret_access.getOrElse(false.B), cf_secret_propagation.getOrElse(false.B),
                      cf_secret_transmission.getOrElse(false.B))   // [R124GATE3]
    val cf_oc   = cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
    // val cf_taint = Cat(cf_taint_module_id_1, cf_taint_type_1, cf_taint_op_count_1, cf_taint_module_id_2, cf_taint_type_2, cf_taint_op_count_2, cf_taint_module_id_3, cf_taint_type_3, cf_taint_op_count_3)
    
    cf"UOP Code is $uopc " +
    cf"UOP running count is $cf_oc " +
    cf"PC is $debug_pc " +
    cf"IQ type $iq_type FU type $fu_code " +
    cf"is branch $is_br, is taken $taken " +
    cf"cf info is $cf_info " +
    cf"cf taint info cf_taint \n"
  }
}


/**
 * Control signals within a MicroOp
 *
 * TODO REFACTOR this, as this should no longer be true, as bypass occurs in stage before branch resolution
 */
class CtrlSignals extends Bundle()
{
  val br_type     = UInt(BR_N.getWidth.W)
  val op1_sel     = UInt(OP1_X.getWidth.W)
  val op2_sel     = UInt(OP2_X.getWidth.W)
  val imm_sel     = UInt(IS_X.getWidth.W)
  val op_fcn      = UInt(freechips.rocketchip.rocket.ALU.SZ_ALU_FN.W)
  val fcn_dw      = Bool()
  val csr_cmd     = UInt(freechips.rocketchip.rocket.CSR.SZ.W)
  val is_load     = Bool()   // will invoke TLB address lookup
  val is_sta      = Bool()   // will invoke TLB address lookup
  val is_std      = Bool()
}



