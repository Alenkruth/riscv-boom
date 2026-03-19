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
  val op_count  = UInt(uopIDCounterWidthCF.W)  // 8 bits: op_count_id of the influencing uop
  val infl_type = UInt(inflTypeWidthCF.W)       // 5 bits: influence type (INFL_*)
  val is_atk    = Bool()   // influencer instruction was from attacker domain (domain=1)
  val is_secret = Bool()   // influencer instruction had s_acc=1 or s_prop=1
  // deny_count: for INFL_ISSUE_CONTENTION only — number of cycles this instruction was denied
  // an issue port by a cross-domain instruction. 0 for all other influence types.
  // Saturates at 15 (4-bit field). Tracked in issue slot, injected into ROB at issue time.
  val deny_count = UInt(4.W)
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
  val cf_domain_id            = UInt(iftTagWidth.W)
  val cf_speculated           = Bool()      // set when the micro-op is speculatively issued
  val cf_attacker_influence   = Bool()      // set when the micro-op is either fetched/dispatched based on an attacker/secret dependent thread/micro-op
  val cf_secret_access        = Bool()      // set when the micro-op accesses a secret
  val cf_secret_propagation   = Bool()      // set when the micro-op is in the dependence chain of originating in a secret
  val cf_secret_transmission  = Bool()      // set when a secret dependent micro-op makes a update to a stateful unit

  // Corefuzzing - IFT tags for every micro-op shadow
  val cf_op_count_id = UInt(uopIDCounterWidthCF.W) // counts the number of active micro-ops. Count is incremented when it the micro-op is created with a counter. 
                               // counter wraps around at 255. It is fine because the maximum number of uops currently supported in 130. Even if we 
                               // increase this number, we can increase the width of the counter.

  val cf_single_step          = Bool()      // set when the micro-op is single-stepped (quiesce mode)

  // Bitmap of all pipeline modules this uop has visited (one bit per module, see CoreFuzzingConstants)
  val cf_fu_bitmap            = UInt(numModules.W)

  // IFT Phase 2: multi-slot influencer list (replaces cf_influencer_uop_count)
  // Records up to numInfluencerSlotsCF cross-domain influence events
  val cf_influencer_list      = Vec(numInfluencerSlotsCF, new InfluencerEntry)
  val cf_infl_overflow        = Bool()   // set when >numInfluencerSlotsCF influencers occurred

  // IFT Phase 2: register taint (set in rename; used at dispatch in core.scala)
  val cf_src_tainted              = Bool()                       // any source preg tainted by attacker
  val cf_taint_producer_op        = UInt(uopIDCounterWidthCF.W)  // op_count_id of the taint producer
  val cf_taint_producer_is_atk    = Bool()   // taint producer was from attacker domain (domain=1)
  val cf_taint_producer_is_secret = Bool()   // taint producer had s_acc=1 or s_prop=1

  // IFT Phase 2: speculative-branch domain tracking (set at dispatch in core.scala)
  // cf_spec_branch_is_atk: true if any outstanding branch in br_mask was from attacker domain.
  //   Enables detecting victim ops fetched/executed under an attacker speculative branch.
  // cf_spec_branch_op_id: cf_op_count_id of the first (lowest br_tag index) attacker-domain
  //   branch under which this uop is speculated.  Zero when cf_spec_branch_is_atk=false.
  val cf_spec_branch_is_atk   = Bool()
  val cf_spec_branch_op_id    = UInt(uopIDCounterWidthCF.W)

  // Do we allocate a branch tag for this?
  // SFB branches don't get a mask, they get a predicate bit
  def allocate_brtag   = (is_br && !is_sfb) || is_jalr

  // Does this register write-back
  def rf_wen           = dst_rtype =/= RT_X

  // Is it possible for this uop to misspeculate, preventing the commit of subsequent uops?
  def unsafe           = uses_ldq || (uses_stq && !is_fence) || is_br || is_jalr

  def fu_code_is(_fu: UInt) = (fu_code & _fu) =/= 0.U

  override def toPrintable: Printable = {
    val cf_info = Cat(cf_domain_id, cf_speculated, cf_attacker_influence, cf_secret_access, cf_secret_propagation, cf_secret_transmission)
    // val cf_taint = Cat(cf_taint_module_id_1, cf_taint_type_1, cf_taint_op_count_1, cf_taint_module_id_2, cf_taint_type_2, cf_taint_op_count_2, cf_taint_module_id_3, cf_taint_type_3, cf_taint_op_count_3)
    
    cf"UOP Code is $uopc " +
    cf"UOP running count is $cf_op_count_id " +
    cf"PC is $debug_pc " +
    cf"IQ type $iq_type FU type $fu_code " +
    cf"is branch $is_br, is taken $taken " +
    cf"cf info is $cf_info " +
    cf"cf taint info cf_taint \n"
  }
}


/**
 * Extension to BoomBundle to add the IFT shadow MicroOp
 */
abstract trait HasShadowUOP extends BoomBundle
{
  val uopshadow = new Shadow()
}


class Shadow(implicit p: Parameters) extends BoomBundle
  with CoreFuzzingConstants
{
  // the taint module acts like a FIFO style queue. The oldest to modify the op woiuld be in module_id_1 and the latest in module_id_5

  val cf_taint_module_id_1 = UInt(moduleCountCF.W) // keeps track of modules where transmission happened
  val cf_taint_type_1 = UInt(taintTypeCf.W) // keeps track of which taint was set first
  val cf_taint_op_count_1 = UInt(uopIDCounterWidthCF.W) // keeps track of the uop count when the first taint was set

  val cf_taint_module_id_2 = UInt(moduleCountCF.W) // keeps track of modules where transmission happened
  val cf_taint_type_2 = UInt(taintTypeCf.W) // keeps track of which taint was set first
  val cf_taint_op_count_2 = UInt(uopIDCounterWidthCF.W) // keeps track of the uop count when the first taint was set

  val cf_taint_module_id_3 = UInt(moduleCountCF.W) // keeps track of modules where transmission happened
  val cf_taint_type_3 = UInt(taintTypeCf.W) // keeps track of which taint was set first
  val cf_taint_op_count_3 = UInt(uopIDCounterWidthCF.W) // keeps track of the uop count when the first taint was set

  val cf_taint_module_id_4 = UInt(moduleCountCF.W) // keeps track of modules where transmission happened
  val cf_taint_type_4 = UInt(taintTypeCf.W) // keeps track of which taint was set first
  val cf_taint_op_count_4 = UInt(uopIDCounterWidthCF.W) // keeps track of the uop count when the first taint was set

  val cf_taint_module_id_5 = UInt(moduleCountCF.W) // keeps track of modules where transmission happened
  val cf_taint_type_5 = UInt(taintTypeCf.W) // keeps track of which taint was set first
  val cf_taint_op_count_5 = UInt(uopIDCounterWidthCF.W) // keeps track of the uop count when the first taint was set

  // the predispatch taint module acts like a FIFO style queuefor all the modules that 
  // the uop resided in before being dispatched.
  // This is because, once the uop is dispatched, a copy of the uop would be in the rob and another in the Functional units/queues
  // where they get operated upon. The uops eventually converge back during writeback.
  // The oldest to modify the op woiuld be in module_id_1 and the latest in module_id_5

  val cf_predis_taint_module_id_1 = UInt(moduleCountCF.W) // keeps track of modules where transmission happened
  val cf_predis_taint_type_1 = UInt(taintTypeCf.W) // keeps track of which taint was set first
  val cf_predis_taint_op_count_1 = UInt(uopIDCounterWidthCF.W) // keeps track of the uop count when the first taint was set

  val cf_predis_taint_module_id_2 = UInt(moduleCountCF.W) // keeps track of modules where transmission happened
  val cf_predis_taint_type_2 = UInt(taintTypeCf.W) // keeps track of which taint was set first
  val cf_predis_taint_op_count_2 = UInt(uopIDCounterWidthCF.W) // keeps track of the uop count when the first taint was set

  val cf_predis_taint_module_id_3 = UInt(moduleCountCF.W) // keeps track of modules where transmission happened
  val cf_predis_taint_type_3 = UInt(taintTypeCf.W) // keeps track of which taint was set first
  val cf_predis_taint_op_count_3 = UInt(uopIDCounterWidthCF.W) // keeps track of the uop count when the first taint was set

  val cf_predis_taint_module_id_4 = UInt(moduleCountCF.W) // keeps track of modules where transmission happened
  val cf_predis_taint_type_4 = UInt(taintTypeCf.W) // keeps track of which taint was set first
  val cf_predis_taint_op_count_4 = UInt(uopIDCounterWidthCF.W) // keeps track of the uop count when the first taint was set

  val cf_predis_taint_module_id_5 = UInt(moduleCountCF.W) // keeps track of modules where transmission happened
  val cf_predis_taint_type_5 = UInt(taintTypeCf.W) // keeps track of which taint was set first
  val cf_predis_taint_op_count_5 = UInt(uopIDCounterWidthCF.W) // keeps track of the uop count when the first taint was set
  
  // UInts to store the number of load and store wakeups and retries
  val cf_count_ldq_wakeups      = UInt(3.W)
  val cf_count_ldq_stq_retries  = UInt(3.W)
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



