//******************************************************************************
// Copyright (c) 2017 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Frontend
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.v3.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._

import boom.v3.common._
import boom.v3.exu.{CommitExceptionSignals, BranchDecode, BrUpdateInfo, BranchDecodeSignals}
import boom.v3.util._


class FrontendResp(implicit p: Parameters) extends BoomBundle()(p) {
  val pc = UInt(vaddrBitsExtended.W)  // ID stage PC
  val data = UInt((fetchWidth * coreInstBits).W)
  val mask = UInt(fetchWidth.W)
  val xcpt = new FrontendExceptions
  val ghist = new GlobalHistory

  // fsrc provides the prediction FROM a branch in this packet
  // tsrc provides the prediction TO this packet
  val fsrc = UInt(BSRC_SZ.W)
  val tsrc = UInt(BSRC_SZ.W)
  // corefuzzing: ICache line was last refilled by a different domain
  val icache_domain_mismatch = if (ENABLE_IFT) Some(Bool()) else None
  // corefuzzing: ICache line was last refilled during a secret-range fetch (same-domain case)
  val icache_secret_mismatch = if (ENABLE_IFT) Some(Bool()) else None
}

class GlobalHistory(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  // For the dual banked case, each bank ignores the contribution of the
  // last bank to the history. Thus we have to track the most recent update to the
  // history in that case
  val old_history = UInt(globalHistoryLength.W)

  val current_saw_branch_not_taken = Bool()

  val new_saw_branch_not_taken = Bool()
  val new_saw_branch_taken     = Bool()

  val ras_idx = UInt(log2Ceil(nRasEntries).W)

  // C3 (corefuzzing): GHR taint.  A single bit, like the PHT shadow, meaning "this
  // history contains a bit contributed by a secret-dependent branch".
  //
  // These live INSIDE GlobalHistory deliberately: the bundle is snapshotted per FTQ
  // entry and restored on redirect, so a squashed secret-dependent branch rolls its
  // tag back along with the history bit it contributed.  A tag held outside would
  // survive a rollback the history itself discarded.
  //
  // Untaint is COUNTER-GATED rather than ad-hoc.  Unlike BTB/PHT entries the GHR has
  // no per-entry ownership, so nothing overwrites the tag; instead the contributed bit
  // simply shifts out.  The horizon is therefore globalHistoryLength (64 here), which
  // is also the longest TAGE table history (tables: 2,4,8,16,16,32,64) -- past that the
  // bit cannot influence any prediction.  A new secret-dependent branch resets the
  // counter, re-extending the window.
  val secret_tag = if (ENABLE_IFT) Some(Bool()) else None
  val secret_ctr = if (ENABLE_IFT) Some(UInt(log2Ceil(globalHistoryLength + 1).W)) else None

  def histories(bank: Int) = {
    if (nBanks == 1) {
      old_history
    } else {
      require(nBanks == 2)
      if (bank == 0) {
        old_history
      } else {
        Mux(new_saw_branch_taken                            , old_history << 1 | 1.U,
        Mux(new_saw_branch_not_taken                        , old_history << 1,
                                                              old_history))
      }
    }
  }

  // NOTE: secret_tag/secret_ctr are deliberately EXCLUDED from ===.  They are IFT
  // metadata, not architectural history; including them would make two identical
  // histories compare unequal and perturb the predictor's own repair logic.
  def ===(other: GlobalHistory): Bool = {
    ((old_history === other.old_history) &&
     (new_saw_branch_not_taken === other.new_saw_branch_not_taken) &&
     (new_saw_branch_taken === other.new_saw_branch_taken)
    )
  }
  def =/=(other: GlobalHistory): Bool = !(this === other)

  def update(branches: UInt, cfi_taken: Bool, cfi_is_br: Bool, cfi_idx: UInt,
    cfi_valid: Bool, addr: UInt,
    cfi_is_call: Bool, cfi_is_ret: Bool,
    rasActive: UInt = nRasEntries.U,
    cfi_is_secret: Bool = false.B): GlobalHistory = {
    val cfi_idx_fixed = cfi_idx(log2Ceil(fetchWidth)-1,0)
    val cfi_idx_oh = UIntToOH(cfi_idx_fixed)
    val new_history = Wire(new GlobalHistory)

    val not_taken_branches = branches & Mux(cfi_valid,
                                            MaskLower(cfi_idx_oh) & ~Mux(cfi_is_br && cfi_taken, cfi_idx_oh, 0.U(fetchWidth.W)),
                                            ~(0.U(fetchWidth.W)))

    if (nBanks == 1) {
      // In the single bank case every bank sees the history including the previous bank
      new_history := DontCare
      new_history.current_saw_branch_not_taken := false.B
      val saw_not_taken_branch = not_taken_branches =/= 0.U || current_saw_branch_not_taken
      new_history.old_history := Mux(cfi_is_br && cfi_taken && cfi_valid   , histories(0) << 1 | 1.U,
                                 Mux(saw_not_taken_branch                  , histories(0) << 1,
                                                                             histories(0)))
    } else {
      // In the two bank case every bank ignore the history added by the previous bank
      val base = histories(1)
      val cfi_in_bank_0 = cfi_valid && cfi_taken && cfi_idx_fixed < bankWidth.U
      val ignore_second_bank = cfi_in_bank_0 || mayNotBeDualBanked(addr)

      val first_bank_saw_not_taken = not_taken_branches(bankWidth-1,0) =/= 0.U || current_saw_branch_not_taken
      new_history.current_saw_branch_not_taken := false.B
      when (ignore_second_bank) {
        new_history.old_history := histories(1)
        new_history.new_saw_branch_not_taken := first_bank_saw_not_taken
        new_history.new_saw_branch_taken     := cfi_is_br && cfi_in_bank_0
      } .otherwise {
        new_history.old_history := Mux(cfi_is_br && cfi_in_bank_0                             , histories(1) << 1 | 1.U,
                                   Mux(first_bank_saw_not_taken                               , histories(1) << 1,
                                                                                                histories(1)))

        new_history.new_saw_branch_not_taken := not_taken_branches(fetchWidth-1,bankWidth) =/= 0.U
        new_history.new_saw_branch_taken     := cfi_valid && cfi_taken && cfi_is_br && !cfi_in_bank_0

      }
    }
    // C3: a branch that shifts the history advances the counter; a secret-dependent
    // branch sets the tag and restarts the window; the tag clears once the contributed
    // bit has aged past globalHistoryLength.
    val shifted = cfi_valid && cfi_is_br
    // [R124GATE3] patch 28: secret_tag/secret_ctr exist only with ENABLE_IFT (IFT off the nodes
    // below have no reader and are dropped; IFT on the same expressions in the same order).
    val aged    = secret_ctr.getOrElse(0.U(7.W)) >= globalHistoryLength.U
    new_history.secret_ctr.foreach(_ := Mux(cfi_is_secret, 0.U,
                              Mux(shifted && !aged, secret_ctr.get + 1.U, secret_ctr.get)))
    new_history.secret_tag.foreach(_ := Mux(cfi_is_secret, true.B,
                              Mux(aged, false.B, secret_tag.get)))

    new_history.ras_idx := Mux(cfi_valid && cfi_is_call, WrapInc(ras_idx, rasActive),
                           Mux(cfi_valid && cfi_is_ret , WrapDec(ras_idx, rasActive), ras_idx))
    new_history
  }

}

/**
 * Parameters to manage a L1 Banked ICache
 */
trait HasBoomFrontendParameters extends HasL1ICacheParameters
{
  // How many banks does the ICache use?
  val nBanks = if (cacheParams.fetchBytes <= 8) 1 else 2
  // How many bytes wide is a bank?
  val bankBytes = fetchBytes/nBanks

  val bankWidth = fetchWidth/nBanks
  // [SPECBPU-REP] (owner 2026-11-02, RULES C 'Speculative BPU repair': "a 16-record undo log"): undo-log depth and the
  // record id width, shared by the FTQ log and the predictors' local undo words
  val cfSpecLogN = 16
  val cfSpecLogW = log2Ceil(cfSpecLogN)

  require(nBanks == 1 || nBanks == 2)



  // How many "chunks"/interleavings make up a cache line?
  val numChunks = cacheParams.blockBytes / bankBytes

  // Which bank is the address pointing to?
  def bank(addr: UInt) = if (nBanks == 2) addr(log2Ceil(bankBytes)) else 0.U
  def isLastBankInBlock(addr: UInt) = {
    (nBanks == 2).B && addr(blockOffBits-1, log2Ceil(bankBytes)) === (numChunks-1).U
  }
  def mayNotBeDualBanked(addr: UInt) = {
    require(nBanks == 2)
    isLastBankInBlock(addr)
  }

  def blockAlign(addr: UInt) = ~(~addr | (cacheParams.blockBytes-1).U)
  def bankAlign(addr: UInt) = ~(~addr | (bankBytes-1).U)

  def fetchIdx(addr: UInt) = addr >> log2Ceil(fetchBytes)

  def nextBank(addr: UInt) = bankAlign(addr) + bankBytes.U
  def nextFetch(addr: UInt) = {
    if (nBanks == 1) {
      bankAlign(addr) + bankBytes.U
    } else {
      require(nBanks == 2)
      bankAlign(addr) + Mux(mayNotBeDualBanked(addr), bankBytes.U, fetchBytes.U)
    }
  }

  def fetchMask(addr: UInt) = {
    val idx = addr.extract(log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))
    if (nBanks == 1) {
      ((1 << fetchWidth)-1).U << idx
    } else {
      val shamt = idx.extract(log2Ceil(fetchWidth)-2, 0)
      val end_mask = Mux(mayNotBeDualBanked(addr), Fill(fetchWidth/2, 1.U), Fill(fetchWidth, 1.U))
      ((1 << fetchWidth)-1).U << shamt & end_mask
    }
  }

  def bankMask(addr: UInt) = {
    val idx = addr.extract(log2Ceil(fetchWidth)+log2Ceil(coreInstBytes)-1, log2Ceil(coreInstBytes))
    if (nBanks == 1) {
      1.U(1.W)
    } else {
      Mux(mayNotBeDualBanked(addr), 1.U(2.W), 3.U(2.W))
    }
  }
}



/**
 * Bundle passed into the FetchBuffer and used to combine multiple
 * relevant signals together.
 */
class FetchBundle(implicit p: Parameters) extends BoomBundle
  with HasBoomFrontendParameters
{
  val pc            = UInt(vaddrBitsExtended.W)
  val next_pc       = UInt(vaddrBitsExtended.W)
  val edge_inst     = Vec(nBanks, Bool()) // True if 1st instruction in this bundle is pc - 2
  val insts         = Vec(fetchWidth, Bits(32.W))
  val exp_insts     = Vec(fetchWidth, Bits(32.W))

  // Information for sfb folding
  // NOTE: This IS NOT equivalent to uop.pc_lob, that gets calculated in the FB
  val sfbs                 = Vec(fetchWidth, Bool())
  val sfb_masks            = Vec(fetchWidth, UInt((2*fetchWidth).W))
  val sfb_dests            = Vec(fetchWidth, UInt((1+log2Ceil(fetchBytes)).W))
  val shadowable_mask      = Vec(fetchWidth, Bool())
  val shadowed_mask        = Vec(fetchWidth, Bool())

  val cfi_idx       = Valid(UInt(log2Ceil(fetchWidth).W))
  val cfi_type      = UInt(CFI_SZ.W)
  val cfi_is_call   = Bool()
  val cfi_is_ret    = Bool()
  val cfi_npc_plus4 = Bool()

  val ras_top       = UInt(vaddrBitsExtended.W)

  val ftq_idx       = UInt(log2Ceil(ftqSz).W)
  val mask          = UInt(fetchWidth.W) // mark which words are valid instructions

  val br_mask       = UInt(fetchWidth.W)

  val ghist         = new GlobalHistory
  val lhist         = Vec(nBanks, UInt(localHistoryLength.W))

  val xcpt_pf_if    = Bool() // I-TLB miss (instruction fetch fault).
  val xcpt_ae_if    = Bool() // Access exception.

  val bp_debug_if_oh= Vec(fetchWidth, Bool())
  val bp_xcpt_if_oh = Vec(fetchWidth, Bool())

  val end_half      = Valid(UInt(16.W))


  val bpd_meta      = Vec(nBanks, UInt())

  // Source of the prediction from this bundle
  val fsrc    = UInt(BSRC_SZ.W)
  // Source of the prediction to this bundle
  val tsrc    = UInt(BSRC_SZ.W)

  // corefuzzing: domain mismatch flags for IFT influencer injection
  val icache_domain_mismatch = if (ENABLE_IFT) Some(Bool()) else None  // ICache line last filled by different domain
  val icache_secret_mismatch = if (ENABLE_IFT) Some(Bool()) else None  // ICache line last filled during secret-range fetch
  val ras_domain_mismatch    = if (ENABLE_IFT) Some(Bool()) else None  // RAS return addr pushed by different domain
  val bpd_domain_mismatch    = if (ENABLE_IFT) Some(Bool()) else None  // TAGE entry last updated by different domain
  val btb_domain_mismatch    = if (ENABLE_IFT) Some(Bool()) else None  // BTB entry last written by different domain
  // [TY27RM 2026-10-22] btb_target_xdom removed (ty27 BTB_TARGET_INJECT retired; the BTB edge is btb_domain_mismatch, ty10)
  val cf_fetch_domain        = if (ENABLE_IFT) Some(UInt(1.W)) else None // domain of this fetch packet (for FTQ/bpdupdate)
  // corefuzzing: secret shadow mismatch — predictor entry was trained by secret instruction
  val bpd_secret_mismatch    = if (ENABLE_IFT) Some(Bool()) else None
  val btb_secret_mismatch    = if (ENABLE_IFT) Some(Bool()) else None
  // [BTBDISP] step 16: fetch of a tag a secret BTB/uBTB allocation displaced, and PC[13:2] of
  // the secret allocator behind btb_secret_mismatch || btb_sec_disp
  // [R124GATE] patch 26: None without ENABLE_IFT (as BranchPredictionBundle, predictor.scala)
  val btb_sec_disp           = if (ENABLE_IFT) Some(Bool()) else None
  val btb_sec_pc             = if (ENABLE_IFT) Some(UInt(12.W)) else None
  // [BTBATK] DF-btbatk: fetch of a tag an ATTACKER-domain BTB/uBTB allocation displaced (its PC
  // rides btb_sec_pc when no secret BTB edge fires)
  val btb_atk_disp           = if (ENABLE_IFT) Some(Bool()) else None
  // C3: this packet was predicted using a GHR that still holds a secret branch outcome
  val ghist_secret           = if (ENABLE_IFT) Some(Bool()) else None
  // [GHISTATK 2026-09-08] attacker twin: the GHR holds an attacker-determined outcome
  val ghist_atk              = if (ENABLE_IFT) Some(Bool()) else None
  // [GHISTOC] distance-to-arming-branch, carried as the influencer's op_count
  val ghist_secret_ctr       = if (ENABLE_IFT) Some(UInt(7.W)) else None
  val ghist_atk_ctr          = if (ENABLE_IFT) Some(UInt(7.W)) else None
  // corefuzzing: RAS pop was from a slot pushed by a secret call (retroactively tagged)
  val ras_pop_secret         = if (ENABLE_IFT) Some(Bool()) else None
  // [RASLBL] DF-ras-repair-label: domain and secret labels of the slot ras_top was read from (the same
  // registered RAS read, ras.io.read_domain / read_secret), saved in the FTQ for the top repair
  val ras_top_dom            = if (ENABLE_IFT) Some(Bool()) else None
  val ras_top_sec            = if (ENABLE_IFT) Some(Bool()) else None
  // [B60RAS] bundle60 (C): the slot's attacker-window label (ras.io.read_watk), saved for the top repair
  val ras_top_watk           = if (ENABLE_IFT) Some(Bool()) else None
}



/**
 * IO for the BOOM Frontend to/from the CPU
 */
class BoomFrontendIO(implicit p: Parameters) extends BoomBundle
{
  // Give the backend a packet of instructions.
  val fetchpacket       = Flipped(new DecoupledIO(new FetchBufferResp))

  // 1 for xcpt/jalr/auipc/flush
  val get_pc            = Flipped(Vec(2, new GetPCFromFtqIO()))
  val debug_ftq_idx     = Output(Vec(coreWidth, UInt(log2Ceil(ftqSz).W)))
  val debug_fetch_pc    = Input(Vec(coreWidth, UInt(vaddrBitsExtended.W)))

  // Breakpoint info
  val status            = Output(new MStatus)
  val bp                = Output(Vec(nBreakpoints, new BP))
  val mcontext          = Output(UInt(coreParams.mcontextWidth.W))
  val scontext          = Output(UInt(coreParams.scontextWidth.W))

  val sfence = Valid(new SFenceReq)

  val brupdate          = Output(new BrUpdateInfo)
  // [SPECBPU] (owner 2026-11-02): one resolved CFI per cycle for the resolution-time predictor training (core.scala)
  val bpu_res           = Output(Valid(new BpuResolveInfo))
  // [SPECBPU-REP] (owner 2026-11-02 'Speculative BPU repair'): the slot rule of the redirect (core.scala), for the FTQ
  // undo log's squash test; read by the FTQ only together with redirect_val
  val bpu_sq            = Output(new BpuSquashInfo)

  // Redirects change the PC
  val redirect_flush   = Output(Bool()) // Flush and hang the frontend?
  val redirect_val     = Output(Bool()) // Redirect the frontend?
  val redirect_pc      = Output(UInt()) // Where do we redirect to?
  val redirect_ftq_idx = Output(UInt()) // Which ftq entry should we reset to?
  val redirect_ghist   = Output(new GlobalHistory) // What are we setting as the global history?
  // corefuzzing: true when redirect_flush was caused by a branch misprediction (not flush_on_commit).
  // Used to suppress s0_vpc update during quiesce (allow_fetch=false) so the Packet-B/C oscillation
  // is preserved. ROB flush-on-commit redirects (e.g., Packet A → Packet B) always update s0_vpc.
  val redirect_flush_is_mispredict = Output(Bool())

  val commit = Valid(UInt(ftqSz.W))

  val flush_icache = Output(Bool())

  val perf = Input(new FrontendPerfEvents)

  // wire with the flag to enable debug log printing 
  // val cf_debug_log = Output(Bool())
  // for the fuzzycore. cf_tage_to_gshare is an output because the bundle gets flipped for 
  // some reason in the BoomFrontendBundle class.

  val cf_bpd_tage_to_gshare = Output(Bool())
  // 3-bit index into fetchBufferEntryOptions for fetch-buffer reconfiguration
  val cf_fb_idx = Output(UInt(3.W))
  // 3-bit effective decode width for core-width throttling (1, 2, or 4)
  val cf_active_width = Output(UInt(3.W))
  // 2-bit index into ftQueueEntryOptions for FTQ reconfiguration
  val cf_ftq_idx = Output(UInt(2.W))
  // 2-bit index into rasEntryCountOptions for RAS reconfiguration
  val cf_ras_idx = Output(UInt(2.W))
  // BTB reconfiguration: bits[1:0] = set index (btbSetOptions), bit[2] = way index (btbWayOptions)
  val cf_btb_set_idx = Output(UInt(2.W))
  val cf_btb_way_idx = Output(UInt(1.W))
  // 3-bit index into tagetableCountOptions for TAGE active table count
  val cf_tage_count_idx = Output(UInt(3.W))

  // corefuzzing: gate to enable speculative prints in frontend modules
  val cf_debug_frontend_enable = Output(Bool())
  // corefuzzing: enable [FLUSH] logging in fetch buffer (same bit as ROB flush logging)
  val cf_debug_rob_enable      = Output(Bool())

  // for corefuzzing
  // Control signal from core to allow or block new fetches (quiesce)
  val allow_fetch       = Output(Bool())
  // Pulse on QS_DRAINING→QS_FETCH transition: reset FTQ pointers to canonical state
  val cf_ftq_quiesce_reset = Output(Bool())
  // Suppress BTB/BPD redirects during quiesce to prevent stale predictions from
  // killing single-step fetch packets mid-bundle (driven = cf_quiesce_core)
  val cf_btb_quiesce = Output(Bool())
  // Pulse to clear all RAS domain/secret shadow bits on quiesce flush
  val cf_ras_quiesce_flush = Output(Bool())

  // ICache set/way reconfiguration indices (from icacheCSRCF at 0xbcb)
  // bits[1:0]: set index into icacheSetOptions = Seq(64, 32, 16, 8)
  // bits[3:2]: way index into cacheWayOptions  = Seq(8, 4, 2, 1)
  val cf_icache_set_conf = Output(UInt(2.W))
  val cf_icache_way_conf = Output(UInt(2.W))
  val cf_ceviche_en     = if (ENABLE_CEVICHE) Some(Output(Bool()))     else None
  val cf_ceviche_pol    = if (ENABLE_CEVICHE) Some(Output(UInt(8.W)))  else None
  val cf_ceviche_hard_i = if (ENABLE_CEVICHE) Some(Output(UInt(10.W))) else None
  val cf_ceviche_soft_i = if (ENABLE_CEVICHE) Some(Output(UInt(10.W))) else None
  val cf_cev_inval_dom  = if (ENABLE_CEVICHE) Some(Output(Bool()))     else None   // domain of the committing fence.i (K2)
  val cf_cev_dbg_flush = if (ENABLE_CEVICHE) Some(Output(Bool())) else None

  // corefuzzing: attacker address range for fetch-domain computation in frontend
  val cf_attacker_start_addr = Output(UInt(vaddrBitsExtended.W))
  val cf_attacker_end_addr   = Output(UInt(vaddrBitsExtended.W))
  // corefuzzing: secret address range for ITLB secret shadow computation in frontend
  val cf_secret_start_addr   = Output(UInt(vaddrBitsExtended.W))
  val cf_secret_end_addr     = Output(UInt(vaddrBitsExtended.W))

  // corefuzzing: multi-port dispatch/execute-time FTQ secret marking (replaces commit-time single port)
  // Driven from core.scala dispatch and lsu.scala TLB stage; forwarded to FTQ.cf_secret_ftq_updates
  val cf_secret_ftq_updates = Output(Vec(coreWidth + memWidth, Valid(UInt(log2Ceil(ftqSz).W))))

  // [ARMRES] step 20 (rulings 134/137): the secret / attacker window counters now live in the
  // core (armed at branch resolution, aged by retiring branches younger than the arming one).
  // The FTQ's retire-time counters are no longer read.
  val cf_win_sec_ctr = Output(UInt(7.W))
  val cf_win_atk_ctr = Output(UInt(7.W))
  // [DBGFREEZE] patch 85 (b): the hart is in debug mode (core.scala cf_dbg, a flop): the fetch buffer gives the
  // packets it enqueues no fetch-time label (edges, s_prop / atk, domain).  IFT only.
  val cf_dbg = if (ENABLE_IFT) Some(Output(Bool())) else None
  // [WINFTQ] patch 29b (owner r160): the FTQ index of the oldest secret-window arming branch
  // that resolved (registered); the FTQ tags every entry younger than it (fetch-target-queue.scala).
  val cf_win_ftq_rs = if (ENABLE_IFT) Some(Output(Valid(UInt(log2Ceil(ftqSz).W)))) else None
  // [WINFTQATK] (DF-ftq-atk-window-range, owner 2026-10-28): the attacker twin, the FTQ index of the oldest
  // attacker-window arming branch that resolved (registered); the FTQ sets cf_fetch_atk on every younger entry.
  val cf_win_ftq_ra = if (ENABLE_IFT) Some(Output(Valid(UInt(log2Ceil(ftqSz).W)))) else None
  // [FTQATK] patch 47 (r173 (5)): per brinfos lane, a br/jalr resolved with an attacker-tainted
  // condition / target (cond_atk) at this ftq_idx; the FTQ ORs it into the predictor update domain.
  val cf_atk_ftq_updates = if (ENABLE_IFT) Some(Output(Vec(coreWidth, Valid(UInt(log2Ceil(ftqSz).W))))) else None
  // [FTQFULL] patch 59 (was 57; owner 2026-10-19, item D3): a fetch packet was held at f4 because the FTQ was
  // full (ftq.io.enq.ready = 0), registered here so the route to the core starts at a flop.  Read
  // only by the core's FTQ-starvation edge (core.scala [FTQFULL]).  IFT only.
  val cf_ftq_full = if (ENABLE_IFT) Some(Input(Bool())) else None
}

/**
 * Top level Frontend class
 *
 * @param icacheParams parameters for the icache
 * @param hartid id for the hardware thread of the core
 */
class BoomFrontend(val icacheParams: ICacheParams, staticIdForMetadataUseOnly: Int)(implicit p: Parameters) extends LazyModule
{
  lazy val module = new BoomFrontendModule(this)
  val icache = LazyModule(new boom.v3.ifu.ICache(icacheParams, staticIdForMetadataUseOnly))
  val masterNode = icache.masterNode
  val resetVectorSinkNode = BundleBridgeSink[UInt](Some(() =>
    UInt(masterNode.edges.out.head.bundle.addressBits.W)))
}

/**
 * Bundle wrapping the IO for the Frontend as a whole
 *
 * @param outer top level Frontend class
 */
class BoomFrontendBundle(val outer: BoomFrontend) extends CoreBundle()(outer.p)
{
  val cpu = Flipped(new BoomFrontendIO())
  val ptw = new TLBPTWIO()
  // [V3STATE] SP-14: the frontend STATE-record taps (IC, RAS, PRED), driven by the frontend cut-out register
  // (SP-18/SP-19).  CoreBundle has no ENABLE_* vals: the gate reads the BoomCoreParams directly.
  val cf_state = if (tileParams.core.asInstanceOf[BoomCoreParams].enableIFTBridge ||
                     tileParams.core.asInstanceOf[BoomCoreParams].iftOracle) Some(Output(new IftFeState)) else None
}

/**
 * Main Frontend module that connects the icache, TLB, fetch controller,
 * and branch prediction pipeline together.
 *
 * @param outer top level Frontend class
 */
class BoomFrontendModule(outer: BoomFrontend) extends LazyModuleImp(outer)
  with HasBoomCoreParameters
  with HasBoomFrontendParameters
  with freechips.rocketchip.util.CoreFuzzingConstants
{
  val io = IO(new BoomFrontendBundle(outer))
  val io_reset_vector = outer.resetVectorSinkNode.bundle
  implicit val edge = outer.masterNode.edges.out(0)
  require(fetchWidth*coreInstBytes == outer.icacheParams.fetchBytes)

  val bpd = Module(new BranchPredictor)
  bpd.io.f3_fire := false.B
  val ras = Module(new BoomRAS)

  // RAS runtime size selection: rasEntryCountOptions = Seq(32, 16, 8, 4)
  // [STATIC-CLAMP] each option is capped at the built size: identical constants when the module is built at
  // the max option (the fuzzing build); a static smaller build with reconf off (index 0) gets its own size.
  val rasOptionsVec = VecInit(rasEntryCountOptions.map(o => (o min nRasEntries).U))
  val cf_ras_active = rasOptionsVec(io.cpu.cf_ras_idx)

  val icache = outer.icache.module
  icache.io.invalidate := io.cpu.flush_icache
  val tlb = Module(new TLB(true, log2Ceil(fetchBytes), TLBConfig(nTLBSets, nTLBWays)))
  io.ptw <> tlb.io.ptw
  io.cpu.perf.tlbMiss := io.ptw.req.fire
  io.cpu.perf.acquire := icache.io.perf.acquire

  // --------------------------------------------------------
  // **** NextPC Select (F0) ****
  //      Send request to ICache
  // --------------------------------------------------------

  val s0_vpc       = WireInit(0.U(vaddrBitsExtended.W))
  val s0_ghist     = WireInit((0.U).asTypeOf(new GlobalHistory))
  val s0_tsrc      = WireInit(0.U(BSRC_SZ.W))
  val s0_valid     = WireInit(false.B)
  val s0_is_replay = WireInit(false.B)
  val s0_is_sfence = WireInit(false.B)
  val s0_replay_resp = Wire(new TLBResp(log2Ceil(fetchBytes)))
  val s0_replay_bpd_resp = Wire(new BranchPredictionBundle)
  val s0_replay_ppc  = Wire(UInt())
  val s0_s1_use_f3_bpd_resp = WireInit(false.B)




  when (RegNext(reset.asBool) && !reset.asBool) {
    s0_valid   := true.B
    s0_vpc     := io_reset_vector
    s0_ghist   := (0.U).asTypeOf(new GlobalHistory)
    s0_tsrc    := BSRC_C
  }

  // corefuzzing: fetch-throttle gate — driven by FetchBuffer cooldown timer after fb is instantiated.
  // Declared here (before fb) to allow use at the icache/bpd req sites; single driver assigned below.
  val cf_fetch_throttle_gate = Wire(Bool())

  // for corefuzzing
  // icache.io.req.valid := s0_valid
  icache.io.req.valid     := s0_valid && io.cpu.allow_fetch && cf_fetch_throttle_gate
  icache.io.req.bits.addr := s0_vpc

  // corefuzzing: gate BPD requests when fetch is blocked (quiesce/QS_DRAINING).
  // Previously un-gated, BPD continued predicting during QS_DRAINING, queuing FTQ entries
  // that the IFU processed immediately on allow_fetch re-enable, causing over-fetch.
  bpd.io.f0_req.valid      := s0_valid && io.cpu.allow_fetch && cf_fetch_throttle_gate
  bpd.io.f0_req.bits.pc    := s0_vpc
  bpd.io.f0_req.bits.ghist := s0_ghist
  // corefuzzing: compute s0 domain for BPD domain shadow tracking
  // [F4S1RNG] lean58: IFT on, the BPD gets the s1 domain (bpd.io.f1_req_domain_id, below); this port is tied 0.
  bpd.io.f0_req_domain_id := (if (ENABLE_IFT) 0.U else Mux(
    (io.cpu.cf_attacker_start_addr =/= 0.U) &&
    (s0_vpc >= io.cpu.cf_attacker_start_addr) &&
    (s0_vpc < io.cpu.cf_attacker_end_addr), 1.U, 0.U))

  // Value from reconfigure CSR - AK
  // for the fuzzycore.
  bpd.io.cf_bpd_tage_to_gshare := io.cpu.cf_bpd_tage_to_gshare
  // corefuzzing: runtime BTB/TAGE reconfiguration
  bpd.io.cf_btb_set_idx    := io.cpu.cf_btb_set_idx
  bpd.io.cf_btb_way_idx    := io.cpu.cf_btb_way_idx
  bpd.io.cf_tage_count_idx := io.cpu.cf_tage_count_idx
  bpd.io.cf_btb_quiesce    := io.cpu.cf_btb_quiesce

  // --------------------------------------------------------
  // **** ICache Access (F1) ****
  //      Translate VPC
  // --------------------------------------------------------
  val s1_vpc       = RegNext(s0_vpc)
  val s1_valid     = RegNext(s0_valid, false.B)
  val s1_ghist     = RegNext(s0_ghist)
  val s1_is_replay = RegNext(s0_is_replay)
  val s1_is_sfence = RegNext(s0_is_sfence)
  val f1_clear     = WireInit(false.B)
  val s1_tsrc      = RegNext(s0_tsrc)
  tlb.io.req.valid      := (s1_valid && !s1_is_replay && !f1_clear) || s1_is_sfence
  tlb.io.req.bits.cmd   := DontCare
  tlb.io.req.bits.vaddr := s1_vpc
  tlb.io.req.bits.passthrough := false.B
  tlb.io.req.bits.size  := log2Ceil(coreInstBytes * fetchWidth).U
  tlb.io.req.bits.v     := io.ptw.status.v
  tlb.io.req.bits.prv   := io.ptw.status.prv
  tlb.io.sfence         := RegNext(io.cpu.sfence)
  tlb.io.kill           := false.B

  val s1_tlb_miss = !s1_is_replay && tlb.io.resp.miss
  val s1_tlb_resp = Mux(s1_is_replay, RegNext(s0_replay_resp), tlb.io.resp)
  val s1_ppc  = Mux(s1_is_replay, RegNext(s0_replay_ppc), tlb.io.resp.paddr)
  val s1_bpd_resp = bpd.io.resp.f1

  icache.io.s1_paddr := s1_ppc
  icache.io.s1_kill  := tlb.io.resp.miss || f1_clear

  // corefuzzing: compute fetch domain (attacker=1, victim=0) based on s0_vpc vs CSR range
  // [F4S1RNG] lean58 timing F4 (timing-audit-20261023 section 2.5): IFT on, the three fetch-PC range tests (attacker
  // range for the BPD and for the I$, secret range for the I$) no longer hang on s0_vpc, the frontend's next-PC select
  // (the stock critical net; lean54's worst family).  They are computed in s1 from s1_vpc = RegNext(s0_vpc) and from
  // a registered copy of the range CSRs and of their enable terms (cf_rng_q).  Exact: every consumer took the s0
  // result through an unconditional RegNext (s1_fetch_domain, s1_fetch_is_secret, and each predictor bank's s1_domain
  // = RegNext(io.f0_domain_id)), and for any combinational f, RegNext(f(s0_vpc, csr)) = f(RegNext(s0_vpc),
  // RegNext(csr)) in every cycle after the first: no reliance on the CSR-write flush (a fetch in flight across a
  // range-CSR write computes exactly what it did before).  The f3 range compare (f3_imemresp.pc) and the fetch
  // buffer keep the live CSRs.  Cost: 4 x vaddrBitsExtended + 2 FF (the CSR copies sit on the frontend side of the
  // SLR2-SLR3 crossing); the compares move from s0_vpc's fan-out to s1 cones that end at the f2 flops / I$ s1 logic
  // (parallel with the SRAM read and tag compare).  IFT off: the s0 form below is unchanged.
  val cf_rng_q = if (ENABLE_IFT) Some(RegNext(VecInit(io.cpu.cf_attacker_start_addr, io.cpu.cf_attacker_end_addr,
                                                      io.cpu.cf_secret_start_addr,   io.cpu.cf_secret_end_addr))) else None
  val cf_rng_en_q = if (ENABLE_IFT) Some(RegNext(VecInit(io.cpu.cf_attacker_start_addr =/= 0.U,
                                                         io.cpu.cf_secret_start_addr =/= io.cpu.cf_secret_end_addr))) else None
  val s1_vpc_atk = cf_rng_q.map(q => cf_rng_en_q.get(0) && (s1_vpc >= q(0)) && (s1_vpc < q(1))).getOrElse(false.B)
  val s1_vpc_sec = cf_rng_q.map(q => cf_rng_en_q.get(1) && (s1_vpc >= q(2)) && (s1_vpc < q(3))).getOrElse(false.B)
  bpd.io.f1_req_domain_id.foreach(_ := s1_vpc_atk)
  // The s0 forms are kept for IFT off (unchanged Verilog); IFT on they have no reader and are removed.
  val s0_fetch_is_attacker = (io.cpu.cf_attacker_start_addr =/= 0.U) &&
    (s0_vpc >= io.cpu.cf_attacker_start_addr) && (s0_vpc < io.cpu.cf_attacker_end_addr)
  // [R124GATE3] patch 28 (B51): IFT-only (feeds only the ICache domain-mismatch compare).
  val s1_fetch_domain = if (ENABLE_IFT) s1_vpc_atk else false.B   // [F4S1RNG] (was RegNext(s0_fetch_is_attacker))
  // corefuzzing: compute fetch secret flag based on s0_vpc vs secret CSR range
  val s0_fetch_is_secret = (io.cpu.cf_secret_start_addr =/= io.cpu.cf_secret_end_addr) &&
    (s0_vpc >= io.cpu.cf_secret_start_addr) && (s0_vpc < io.cpu.cf_secret_end_addr)
  val s1_fetch_is_secret = if (ENABLE_IFT) s1_vpc_sec else RegNext(s0_fetch_is_secret)   // [F4S1RNG]
  // [ICREDIR] F3 (owner 2026-10-22, FINAL: "a fetch redirected by an attacker-domain CFI is attacker-influenced"):
  // for the I$ domain compare and the fill domain, a fetch packet that is the TARGET of a control transfer made by
  // an attacker-domain CFI counts as attacker even when its PC is victim/neutral code (r17: the attacker's scan jalr
  // probing victim-range stubs never mismatched, icache.scala s1_domain_mismatch).  s0_redir_atk follows s0_vpc's
  // own select (same when-blocks below): core mispredict redirect -> the redirecting uop's domain (brupdate.b2, a
  // register, the same uop the core redirects for); f1/f2/f3 predicted-taken redirect -> the PC-range domain of the
  // predicting packet; f2 replay -> the replayed packet's bit; sequential fetch, sfence and ROB flushes -> 0.  Only
  // the target packet is marked (the next sequential packet is back to its PC domain); the fetch buffer's packet
  // domain (cf_domain_id, the ty16 is_atk = any_is_victim gate) is unchanged.
  // Cost: +3 FF (s1/s2 bit, s2 PC domain), ~4 LUT on the s0 select (parallel to s0_vpc, flop or BPD-select data,
  // +0 levels on s0_vpc), +1 OR input on the I$ s1 domain compare (flop-sourced, absorbed: +0 levels expected,
  // <= +1 bound).  IFT off: constant 0, no flops.
  val s0_redir_atk    = WireInit(false.B)
  val s1_redir_atk    = if (ENABLE_IFT) RegNext(s0_redir_atk, false.B) else false.B
  val s2_redir_atk    = if (ENABLE_IFT) RegNext(s1_redir_atk, false.B) else false.B
  val s2_fetch_domain = if (ENABLE_IFT) RegNext(s1_fetch_domain, false.B) else false.B
  icache.io.s1_domain_id := s1_fetch_domain || s1_redir_atk
  icache.io.cf_cev_s1_dom.foreach(_ := s1_fetch_domain)   // Ceviche: the running domain by PC range, without the IFT redirect bit
  // [M3ATKWIN] lean58 (Fable audit M3; RULES A U8 "symmetric", reading 1): a line FILLED while the attacker window is open takes the attacker fill
  // domain (the twin of the secret window term on s1_is_secret, registered the same way).  Fill side only: the
  // fetch's own domain for the I$ mismatch compare is unchanged (a domain compare, unlike the secret flag, would
  // flag every window fetch of a victim line).  RECORD CHANGE.
  icache.io.cf_s1_fill_atk.foreach(_ := RegNext(io.cpu.cf_win_atk_ctr =/= 0.U, false.B))
  // [ICSEC] step 15: icache.io.s1_is_secret is driven below, after `ftq` is in Scala scope.

  // [FIFO10] step 10 (owner ruling 94: machine mode, paging off): the ITLB domain/secret shadow
  // (INFL_ITLB_STATE) is removed.  It was filled only on an ITLB miss -> hit transition, which
  // needs translation, so it could never fire.  Type code 15 stays reserved.

  val f1_mask = fetchMask(s1_vpc)
  val f1_redirects = (0 until fetchWidth) map { i =>
    s1_valid && f1_mask(i) && s1_bpd_resp.preds(i).predicted_pc.valid &&
    (s1_bpd_resp.preds(i).is_jal ||
      (s1_bpd_resp.preds(i).is_br && s1_bpd_resp.preds(i).taken))
  }
  val f1_redirect_idx = PriorityEncoder(f1_redirects)
  val f1_do_redirect = f1_redirects.reduce(_||_) && useBPD.B && !io.cpu.cf_btb_quiesce
  val f1_targs = s1_bpd_resp.preds.map(_.predicted_pc.bits)
  val f1_predicted_target = Mux(f1_do_redirect,
                                f1_targs(f1_redirect_idx),
                                nextFetch(s1_vpc))

  val f1_predicted_ghist = s1_ghist.update(
    s1_bpd_resp.preds.map(p => p.is_br && p.predicted_pc.valid).asUInt & f1_mask,
    s1_bpd_resp.preds(f1_redirect_idx).taken && f1_do_redirect,
    s1_bpd_resp.preds(f1_redirect_idx).is_br,
    f1_redirect_idx,
    f1_do_redirect,
    s1_vpc,
    false.B,
    false.B,
    cf_ras_active)

  when (s1_valid && !s1_tlb_miss) {
    // Stop fetching on fault
    s0_valid     := !(s1_tlb_resp.ae.inst || s1_tlb_resp.pf.inst)
    s0_tsrc      := BSRC_1
    s0_vpc       := f1_predicted_target
    s0_ghist     := f1_predicted_ghist
    s0_is_replay := false.B
    s0_redir_atk := f1_do_redirect && s1_fetch_domain   // [ICREDIR]
  }

  // --------------------------------------------------------
  // **** ICache Response (F2) ****
  // --------------------------------------------------------

  val s2_valid = RegNext(s1_valid && !f1_clear, false.B)
  val s2_vpc   = RegNext(s1_vpc)
  val s2_ghist = Reg(new GlobalHistory)
  s2_ghist := s1_ghist
  val s2_ppc  = RegNext(s1_ppc)
  val s2_tsrc = RegNext(s1_tsrc) // tsrc provides the predictor component which provided the prediction TO this instruction
  val s2_fsrc = WireInit(BSRC_1) // fsrc provides the predictor component which provided the prediction FROM this instruction
  val f2_clear = WireInit(false.B)
  val s2_tlb_resp = RegNext(s1_tlb_resp)
  val s2_tlb_miss = RegNext(s1_tlb_miss)
  val s2_is_replay = RegNext(s1_is_replay) && s2_valid
  val s2_xcpt = s2_valid && (s2_tlb_resp.ae.inst || s2_tlb_resp.pf.inst) && !s2_is_replay
  val f3_ready = Wire(Bool())

  icache.io.s2_kill := s2_xcpt

  val f2_bpd_resp = bpd.io.resp.f2
  val f2_mask = fetchMask(s2_vpc)
  val f2_redirects = (0 until fetchWidth) map { i =>
    s2_valid && f2_mask(i) && f2_bpd_resp.preds(i).predicted_pc.valid &&
    (f2_bpd_resp.preds(i).is_jal ||
      (f2_bpd_resp.preds(i).is_br && f2_bpd_resp.preds(i).taken))
  }
  val f2_redirect_idx = PriorityEncoder(f2_redirects)
  val f2_targs = f2_bpd_resp.preds.map(_.predicted_pc.bits)
  val f2_do_redirect = f2_redirects.reduce(_||_) && useBPD.B && !io.cpu.cf_btb_quiesce
  val f2_predicted_target = Mux(f2_do_redirect,
                                f2_targs(f2_redirect_idx),
                                nextFetch(s2_vpc))
  val f2_predicted_ghist = s2_ghist.update(
    f2_bpd_resp.preds.map(p => p.is_br && p.predicted_pc.valid).asUInt & f2_mask,
    f2_bpd_resp.preds(f2_redirect_idx).taken && f2_do_redirect,
    f2_bpd_resp.preds(f2_redirect_idx).is_br,
    f2_redirect_idx,
    f2_do_redirect,
    s2_vpc,
    false.B,
    false.B,
    cf_ras_active)

  val f2_correct_f1_ghist = s1_ghist =/= f2_predicted_ghist && enableGHistStallRepair.B

  // [D4FIX] fix A: the f2 replay condition, named so the f3 hand-off below can test the SAME
  // node.  Same expression as before (pure rename, identical netlist).
  val f2_replay = (s2_valid && !icache.io.resp.valid) ||
                  (s2_valid && icache.io.resp.valid && !f3_ready)
  when (f2_replay) {
    s0_valid := (!s2_tlb_resp.ae.inst && !s2_tlb_resp.pf.inst) || s2_is_replay || s2_tlb_miss
    s0_vpc   := s2_vpc
    s0_is_replay := s2_valid && icache.io.resp.valid
    s0_redir_atk := s2_redir_atk   // [ICREDIR] a replay keeps its packet's bit
    // When this is not a replay (it queried the BPDs, we should use f3 resp in the replaying s1)
    s0_s1_use_f3_bpd_resp := !s2_is_replay
    s0_ghist := s2_ghist
    s0_tsrc  := s2_tsrc
    f1_clear := true.B
  } .elsewhen (s2_valid && f3_ready) {
    when (s1_valid && s1_vpc === f2_predicted_target && !f2_correct_f1_ghist) {
      // We trust our prediction of what the global history for the next branch should be
      s2_ghist := f2_predicted_ghist
    }
    when ((s1_valid && (s1_vpc =/= f2_predicted_target || f2_correct_f1_ghist)) || !s1_valid) {
      f1_clear := true.B

      s0_valid     := !((s2_tlb_resp.ae.inst || s2_tlb_resp.pf.inst) && !s2_is_replay)
      s0_vpc       := f2_predicted_target
      s0_is_replay := false.B
      s0_redir_atk := f2_do_redirect && s2_fetch_domain   // [ICREDIR]
      s0_ghist     := f2_predicted_ghist
      s2_fsrc      := BSRC_2
      s0_tsrc      := BSRC_2
    }
  }
  s0_replay_bpd_resp := f2_bpd_resp
  s0_replay_resp := s2_tlb_resp
  s0_replay_ppc  := s2_ppc

  // --------------------------------------------------------
  // **** F3 ****
  // --------------------------------------------------------
  val f3_clear = WireInit(false.B)
  val f3 = withReset(reset.asBool || f3_clear) {
    Module(new Queue(new FrontendResp, 1, pipe=true, flow=false)) }

  // Queue up the bpd resp as well, incase f4 backpressures f3
  // This is "flow" because the response (enq) arrives in f3, not f2
  val f3_bpd_resp = withReset(reset.asBool || f3_clear) {
    Module(new Queue(new BranchPredictionBundle, 1, pipe=true, flow=true)) }




  val f4_ready = Wire(Bool())
  f3_ready := f3.io.enq.ready
  f3.io.enq.valid   := (s2_valid && !f2_clear &&
    (icache.io.resp.valid || ((s2_tlb_resp.ae.inst || s2_tlb_resp.pf.inst) && !s2_tlb_miss))
  )
  f3.io.enq.bits.pc := s2_vpc
  f3.io.enq.bits.data  := Mux(s2_xcpt, 0.U, icache.io.resp.bits.data)
  f3.io.enq.bits.ghist := s2_ghist
  f3.io.enq.bits.mask := fetchMask(s2_vpc)
  f3.io.enq.bits.xcpt := s2_tlb_resp
  f3.io.enq.bits.fsrc := s2_fsrc
  f3.io.enq.bits.tsrc := s2_tsrc
  // corefuzzing: carry icache domain mismatch through the f3 queue;
  // gate on resp.valid to avoid propagating stale bits when we enqueue due to a TLB fault
  f3.io.enq.bits.icache_domain_mismatch.foreach(_ := icache.io.resp.bits.icache_domain_mismatch &&
                                            icache.io.resp.valid)
  f3.io.enq.bits.icache_secret_mismatch.foreach(_ := icache.io.resp.bits.icache_secret_mismatch &&
                                            icache.io.resp.valid)

  // RAS takes a cycle to read
  val ras_read_idx = RegInit(0.U(log2Ceil(nRasEntries).W))
  ras.io.read_idx := ras_read_idx
  when (f3.io.enq.fire) {
    ras_read_idx := f3.io.enq.bits.ghist.ras_idx
    ras.io.read_idx := f3.io.enq.bits.ghist.ras_idx
  }


  // The BPD resp comes in f3
  f3_bpd_resp.io.enq.valid := f3.io.deq.valid && RegNext(f3.io.enq.ready)
  f3_bpd_resp.io.enq.bits  := bpd.io.resp.f3
  when (f3_bpd_resp.io.enq.fire) {
    bpd.io.f3_fire := true.B
  }

  f3.io.deq.ready := f4_ready
  f3_bpd_resp.io.deq.ready := f4_ready


  val f3_imemresp     = f3.io.deq.bits
  val f3_bank_mask    = bankMask(f3_imemresp.pc)
  val f3_data         = f3_imemresp.data
  val f3_aligned_pc   = bankAlign(f3_imemresp.pc)
  val f3_is_last_bank_in_block = isLastBankInBlock(f3_aligned_pc)
  val f3_is_rvc       = Wire(Vec(fetchWidth, Bool()))
  val f3_redirects    = Wire(Vec(fetchWidth, Bool()))
  val f3_targs        = Wire(Vec(fetchWidth, UInt(vaddrBitsExtended.W)))
  val f3_cfi_types    = Wire(Vec(fetchWidth, UInt(CFI_SZ.W)))
  val f3_shadowed_mask = Wire(Vec(fetchWidth, Bool()))
  val f3_fetch_bundle = Wire(new FetchBundle)
  val f3_mask         = Wire(Vec(fetchWidth, Bool()))
  val f3_br_mask      = Wire(Vec(fetchWidth, Bool()))
  val f3_call_mask    = Wire(Vec(fetchWidth, Bool()))
  val f3_ret_mask     = Wire(Vec(fetchWidth, Bool()))
  val f3_npc_plus4_mask = Wire(Vec(fetchWidth, Bool()))
  val f3_btb_mispredicts = Wire(Vec(fetchWidth, Bool()))
  f3_fetch_bundle.mask := f3_mask.asUInt
  f3_fetch_bundle.br_mask := f3_br_mask.asUInt
  f3_fetch_bundle.pc := f3_imemresp.pc
  f3_fetch_bundle.ftq_idx := 0.U // This gets assigned later
  f3_fetch_bundle.xcpt_pf_if := f3_imemresp.xcpt.pf.inst
  f3_fetch_bundle.xcpt_ae_if := f3_imemresp.xcpt.ae.inst
  f3_fetch_bundle.fsrc := f3_imemresp.fsrc
  f3_fetch_bundle.tsrc := f3_imemresp.tsrc
  f3_fetch_bundle.shadowed_mask := f3_shadowed_mask

  // Tracks trailing 16b of previous fetch packet
  val f3_prev_half    = Reg(UInt(16.W))
  // Tracks if last fetchpacket contained a half-inst
  val f3_prev_is_half = RegInit(false.B)

  require(fetchWidth >= 4) // Logic gets kind of annoying with fetchWidth = 2
  def isRVC(inst: UInt) = (inst(1,0) =/= 3.U)
  var redirect_found = false.B
  var bank_prev_is_half = f3_prev_is_half
  var bank_prev_half    = f3_prev_half
  var last_inst = 0.U(16.W)
  for (b <- 0 until nBanks) {
    val bank_data  = f3_data((b+1)*bankWidth*16-1, b*bankWidth*16)
    val bank_mask  = Wire(Vec(bankWidth, Bool()))
    val bank_insts = Wire(Vec(bankWidth, UInt(32.W)))

    for (w <- 0 until bankWidth) {
      val i = (b * bankWidth) + w

      val valid = Wire(Bool())
      val bpu = Module(new BreakpointUnit(nBreakpoints))
      bpu.io.status   := io.cpu.status
      bpu.io.bp       := io.cpu.bp
      bpu.io.ea       := DontCare
      bpu.io.mcontext := io.cpu.mcontext
      bpu.io.scontext := io.cpu.scontext

      val brsigs = Wire(new BranchDecodeSignals)
      if (w == 0) {
        val inst0 = Cat(bank_data(15,0), f3_prev_half)
        val inst1 = bank_data(31,0)
        val exp_inst0 = ExpandRVC(inst0)
        val exp_inst1 = ExpandRVC(inst1)
        val pc0 = (f3_aligned_pc + (i << log2Ceil(coreInstBytes)).U - 2.U)
        val pc1 = (f3_aligned_pc + (i << log2Ceil(coreInstBytes)).U)

        val bpd_decoder0 = Module(new BranchDecode)
        bpd_decoder0.io.inst := exp_inst0
        bpd_decoder0.io.pc   := pc0
        val bpd_decoder1 = Module(new BranchDecode)
        bpd_decoder1.io.inst := exp_inst1
        bpd_decoder1.io.pc   := pc1

        when (bank_prev_is_half) {
          bank_insts(w)                := inst0
          f3_fetch_bundle.insts(i)     := inst0
          f3_fetch_bundle.exp_insts(i) := exp_inst0
          bpu.io.pc                    := pc0
          brsigs                       := bpd_decoder0.io.out
          f3_fetch_bundle.edge_inst(b) := true.B
          if (b > 0) {
            val inst0b     = Cat(bank_data(15,0), last_inst)
            val exp_inst0b = ExpandRVC(inst0b)
            val bpd_decoder0b = Module(new BranchDecode)
            bpd_decoder0b.io.inst := exp_inst0b
            bpd_decoder0b.io.pc   := pc0

            when (f3_bank_mask(b-1)) {
              bank_insts(w)                := inst0b
              f3_fetch_bundle.insts(i)     := inst0b
              f3_fetch_bundle.exp_insts(i) := exp_inst0b
              brsigs                       := bpd_decoder0b.io.out
            }
          }
        } .otherwise {
          bank_insts(w)                := inst1
          f3_fetch_bundle.insts(i)     := inst1
          f3_fetch_bundle.exp_insts(i) := exp_inst1
          bpu.io.pc                    := pc1
          brsigs                       := bpd_decoder1.io.out
          f3_fetch_bundle.edge_inst(b) := false.B
        }
        valid := true.B
      } else {
        val inst = Wire(UInt(32.W))
        val exp_inst = ExpandRVC(inst)
        val pc = f3_aligned_pc + (i << log2Ceil(coreInstBytes)).U
        val bpd_decoder = Module(new BranchDecode)
        bpd_decoder.io.inst := exp_inst
        bpd_decoder.io.pc   := pc

        bank_insts(w)                := inst
        f3_fetch_bundle.insts(i)     := inst
        f3_fetch_bundle.exp_insts(i) := exp_inst
        bpu.io.pc                    := pc
        brsigs                       := bpd_decoder.io.out
        if (w == 1) {
          // Need special case since 0th instruction may carry over the wrap around
          inst  := bank_data(47,16)
          valid := bank_prev_is_half || !(bank_mask(0) && !isRVC(bank_insts(0)))
        } else if (w == bankWidth - 1) {
          inst  := Cat(0.U(16.W), bank_data(bankWidth*16-1,(bankWidth-1)*16))
          valid := !((bank_mask(w-1) && !isRVC(bank_insts(w-1))) ||
            !isRVC(inst))
        } else {
          inst  := bank_data(w*16+32-1,w*16)
          valid := !(bank_mask(w-1) && !isRVC(bank_insts(w-1)))
        }
      }

      f3_is_rvc(i) := isRVC(bank_insts(w))


      bank_mask(w) := f3.io.deq.valid && f3_imemresp.mask(i) && valid && !redirect_found
      f3_mask  (i) := f3.io.deq.valid && f3_imemresp.mask(i) && valid && !redirect_found
      f3_targs (i) := Mux(brsigs.cfi_type === CFI_JALR,
        f3_bpd_resp.io.deq.bits.preds(i).predicted_pc.bits,
        brsigs.target)

      // Flush BTB entries for JALs if we mispredict the target
      f3_btb_mispredicts(i) := (brsigs.cfi_type === CFI_JAL && valid &&
        f3_bpd_resp.io.deq.bits.preds(i).predicted_pc.valid &&
        (f3_bpd_resp.io.deq.bits.preds(i).predicted_pc.bits =/= brsigs.target)
      )


      f3_npc_plus4_mask(i) := (if (w == 0) {
        !f3_is_rvc(i) && !bank_prev_is_half
      } else {
        !f3_is_rvc(i)
      })
      val offset_from_aligned_pc = (
        (i << 1).U((log2Ceil(icBlockBytes)+1).W) +
        brsigs.sfb_offset.bits -
        Mux(bank_prev_is_half && (w == 0).B, 2.U, 0.U)
      )
      val lower_mask = Wire(UInt((2*fetchWidth).W))
      val upper_mask = Wire(UInt((2*fetchWidth).W))
      lower_mask := UIntToOH(i.U)
      upper_mask := UIntToOH(offset_from_aligned_pc(log2Ceil(fetchBytes)+1,1)) << Mux(f3_is_last_bank_in_block, bankWidth.U, 0.U)

      f3_fetch_bundle.sfbs(i) := (
        f3_mask(i) &&
        brsigs.sfb_offset.valid &&
        (offset_from_aligned_pc <= Mux(f3_is_last_bank_in_block, (fetchBytes+bankBytes).U,(2*fetchBytes).U))
      )
      f3_fetch_bundle.sfb_masks(i)       := ~MaskLower(lower_mask) & ~MaskUpper(upper_mask)
      f3_fetch_bundle.shadowable_mask(i) := (!(f3_fetch_bundle.xcpt_pf_if || f3_fetch_bundle.xcpt_ae_if || bpu.io.debug_if || bpu.io.xcpt_if) &&
                                             f3_bank_mask(b) &&
                                             (brsigs.shadowable || !f3_mask(i)))
      f3_fetch_bundle.sfb_dests(i)       := offset_from_aligned_pc

      // Redirect if
      //  1) its a JAL/JALR (unconditional)
      //  2) the BPD believes this is a branch and says we should take it
      f3_redirects(i)    := f3_mask(i) && (
        brsigs.cfi_type === CFI_JAL || brsigs.cfi_type === CFI_JALR ||
        (brsigs.cfi_type === CFI_BR && f3_bpd_resp.io.deq.bits.preds(i).taken && useBPD.B)
      )

      f3_br_mask(i)   := f3_mask(i) && brsigs.cfi_type === CFI_BR
      f3_cfi_types(i) := brsigs.cfi_type
      f3_call_mask(i) := brsigs.is_call
      f3_ret_mask(i)  := brsigs.is_ret

      f3_fetch_bundle.bp_debug_if_oh(i) := bpu.io.debug_if
      f3_fetch_bundle.bp_xcpt_if_oh (i) := bpu.io.xcpt_if

      redirect_found = redirect_found || f3_redirects(i)
    }
    last_inst = bank_insts(bankWidth-1)(15,0)
    bank_prev_is_half = Mux(f3_bank_mask(b),
      (!(bank_mask(bankWidth-2) && !isRVC(bank_insts(bankWidth-2))) && !isRVC(last_inst)),
      bank_prev_is_half)
    bank_prev_half    = Mux(f3_bank_mask(b),
      last_inst(15,0),
      bank_prev_half)
  }

  f3_fetch_bundle.cfi_type      := f3_cfi_types(f3_fetch_bundle.cfi_idx.bits)
  f3_fetch_bundle.cfi_is_call   := f3_call_mask(f3_fetch_bundle.cfi_idx.bits)
  f3_fetch_bundle.cfi_is_ret    := f3_ret_mask (f3_fetch_bundle.cfi_idx.bits)
  f3_fetch_bundle.cfi_npc_plus4 := f3_npc_plus4_mask(f3_fetch_bundle.cfi_idx.bits)

  f3_fetch_bundle.ghist    := f3.io.deq.bits.ghist
  f3_fetch_bundle.lhist    := f3_bpd_resp.io.deq.bits.lhist
  f3_fetch_bundle.bpd_meta := f3_bpd_resp.io.deq.bits.meta

  f3_fetch_bundle.end_half.valid := bank_prev_is_half
  f3_fetch_bundle.end_half.bits  := bank_prev_half

  when (f3.io.deq.fire) {
    f3_prev_is_half := bank_prev_is_half
    f3_prev_half    := bank_prev_half
    assert(f3_bpd_resp.io.deq.bits.pc === f3_fetch_bundle.pc)
  }

  when (f3_clear) {
    f3_prev_is_half := false.B
  }

  // corefuzzing: suppress BTB/BPD redirects during quiesce to prevent stale predictions
  // from cancelling single-step fetch packets (cf_btb_quiesce = cf_quiesce_core in core.scala)
  f3_fetch_bundle.cfi_idx.valid := f3_redirects.reduce(_||_) && !io.cpu.cf_btb_quiesce
  f3_fetch_bundle.cfi_idx.bits  := PriorityEncoder(f3_redirects)

  f3_fetch_bundle.ras_top := ras.io.read_addr
  // [RASLBL] the labels read beside ras_top (RAS read flops, 0 logic); +2 FF in the f4 queue
  f3_fetch_bundle.ras_top_dom.foreach(_ := ras.io.read_domain)
  f3_fetch_bundle.ras_top_sec.foreach(_ := ras.io.read_secret)
  f3_fetch_bundle.ras_top_watk.foreach(_ := ras.io.read_watk.get)   // [B60RAS] RAS read flop, +1 FF per f4 entry

  // corefuzzing: compute fetch domain at f3 for RAS/ICache mismatch detection
  val f3_fetch_is_attacker = (io.cpu.cf_attacker_start_addr =/= 0.U) &&
    (f3_imemresp.pc >= io.cpu.cf_attacker_start_addr) &&
    (f3_imemresp.pc < io.cpu.cf_attacker_end_addr)
  f3_fetch_bundle.icache_domain_mismatch.foreach(_ := f3_imemresp.icache_domain_mismatch.getOrElse(false.B))
  f3_fetch_bundle.icache_secret_mismatch.foreach(_ := f3_imemresp.icache_secret_mismatch.getOrElse(false.B))
  // RAS domain mismatch: ret pops an addr pushed by a different domain
  // [RASPTR] patch 29 (F-RAS-1): or a VICTIM ret pops an entry written before the latest
  // attacker pointer move (ras.scala read_stale).  Same field, so the same ROB edge
  // (INFL_RAS_STATE, is_atk = the ret is domain 0).  One more input on the same LUT.
  // [RASEVERY] owner 2026-10-21: both halves raise the edge on EVERY such ret (no mispredict
  // gate); the ROB takes it from the dispatch column (rob-ift-sidecar.scala IftMerge).
  // [B60RAS] bundle60 (C), owner 2026-10-24 (FINDINGS-open-rebase59 item 4, "separate push-domain bit per RAS entry so
  // both rules hold"): read_domain is again the PUSHER's domain only (RULES.md C RAS-edge rule: a pusher-domain
  // mismatch edges both ways), and the 07g M3 attacker-window label rides its own bit read_watk: a VICTIM ret popping
  // a slot pushed while the attacker window was open gets the edge (as with the one-bit 07g M3 design).  Before, one
  // bit held "attacker push OR window", so an ATTACKER ret popping a victim slot pushed inside the window saw no
  // mismatch and lost RAS_STATE (t91 m92 2/8, m93 2/16).  Victim rets: identical to before (dom || watk).  Attacker
  // rets: edge iff the pusher was the victim (window label not involved: the attacker window labels attacker
  // influence, which an attacker ret does not receive from its own window).  Levels: one more flop input on the
  // existing mismatch LUT (ret, cfi valid, f3 domain, read_domain, read_stale, read_watk): 0 added levels.
  f3_fetch_bundle.ras_domain_mismatch.foreach(_ := (
    f3_fetch_bundle.cfi_is_ret && f3_fetch_bundle.cfi_idx.valid &&
    ((ras.io.read_domain =/= f3_fetch_is_attacker) ||
     (!f3_fetch_is_attacker && (ras.io.read_stale || ras.io.read_watk.getOrElse(false.B))))
  ))
  // corefuzzing: RAS pop secret — the popped slot was retroactively marked secret
  // (pushed by a call from a secret fetch packet). Gated to actual returns only.
  f3_fetch_bundle.ras_pop_secret.foreach(_ := (
    f3_fetch_bundle.cfi_is_ret && f3_fetch_bundle.cfi_idx.valid &&
    ras.io.read_secret
  ))
  // corefuzzing: BPD/BTB domain and secret mismatches from the prediction response
  f3_fetch_bundle.bpd_domain_mismatch.foreach(_ := f3_bpd_resp.io.deq.bits.bpd_domain_mismatch.getOrElse(false.B))
  f3_fetch_bundle.btb_domain_mismatch.foreach(_ := f3_bpd_resp.io.deq.bits.btb_domain_mismatch.getOrElse(false.B))
  // [TY27RM 2026-10-22, owner order] the BTB TARGET-domain edge (btb_target_xdom, ty27, doc 67 Part A) is
  // removed with its two f3 range compares on f3_btb_pred_target: the BTB edge is btb_domain_mismatch (ty10).
  f3_fetch_bundle.bpd_secret_mismatch.foreach(_ := f3_bpd_resp.io.deq.bits.bpd_secret_mismatch.getOrElse(false.B))
  f3_fetch_bundle.btb_secret_mismatch.foreach(_ := f3_bpd_resp.io.deq.bits.btb_secret_mismatch.getOrElse(false.B))
  if (ENABLE_IFT) {   // [R124GATE] the fields exist only with ENABLE_IFT
    f3_fetch_bundle.btb_sec_disp.get    := f3_bpd_resp.io.deq.bits.btb_sec_disp.get
    f3_fetch_bundle.btb_sec_pc.get      := f3_bpd_resp.io.deq.bits.btb_sec_pc.get
    f3_fetch_bundle.btb_atk_disp.get    := f3_bpd_resp.io.deq.bits.btb_atk_disp.get
  }
  // corefuzzing: record the fetch domain for the FTQ bpdupdate path
  f3_fetch_bundle.cf_fetch_domain.foreach(_ := f3_fetch_is_attacker.asUInt)
  // Redirect earlier stages only if the later stage
  // can consume this packet

  val f3_predicted_target = Mux(f3_redirects.reduce(_||_),
    Mux(f3_fetch_bundle.cfi_is_ret && useBPD.B && useRAS.B,
      ras.io.read_addr,
      f3_targs(PriorityEncoder(f3_redirects))
    ),
    nextFetch(f3_fetch_bundle.pc)
  )

  f3_fetch_bundle.next_pc       := f3_predicted_target
  val f3_predicted_ghist = f3_fetch_bundle.ghist.update(
    f3_fetch_bundle.br_mask,
    f3_fetch_bundle.cfi_idx.valid,
    f3_fetch_bundle.br_mask(f3_fetch_bundle.cfi_idx.bits),
    f3_fetch_bundle.cfi_idx.bits,
    f3_fetch_bundle.cfi_idx.valid,
    f3_fetch_bundle.pc,
    f3_fetch_bundle.cfi_is_call,
    f3_fetch_bundle.cfi_is_ret,
    cf_ras_active
  )


  ras.io.write_valid  := false.B
  ras.io.write_addr   := f3_aligned_pc + (f3_fetch_bundle.cfi_idx.bits << 1) + Mux(
    f3_fetch_bundle.cfi_npc_plus4, 4.U, 2.U)
  ras.io.write_idx    := WrapInc(f3_fetch_bundle.ghist.ras_idx, cf_ras_active)
  // corefuzzing: record domain of the call that pushes to RAS
  // [M3ATKWIN] lean58 (Fable audit M3; RULES A U8 "symmetric", reading 1): ...or a call pushed while the attacker window is open (the twin of
  // write_secret below): a victim ret that later pops it gets the cross-domain RAS edge.  RECORD CHANGE.
  // [B60RAS] bundle60 (C): the window half moved to its own bit (write_watk); write_domain is the pusher's domain only
  // (the IFT-off expression, unchanged).  Levels: write_domain data loses one input; write_watk = the shared 7-bit
  // zero detect of the window counter, the same term the one-bit design ORed in: 0 added levels.
  ras.io.write_domain := f3_fetch_is_attacker
  ras.io.write_watk.foreach(_ := io.cpu.cf_win_atk_ctr =/= 0.U)
  // corefuzzing: secret not yet known at push time; retroactively updated via late_write_secret
  ras.io.write_secret := false.B
  // [WINFTQPOST] patch 38: a call pushed while the secret window is open is a window member, so the
  // slot it pushes is window-secret at once (same f3 bit as ghist_secret; the late ports above only
  // cover tagged FTQ entries, which a jalr call with a clean target and no window operand never
  // was, and a JAL only through [RASJAL] a few cycles after dispatch).  The top repair below writes
  // the label saved in the FTQ ([RASLBL]).  Levels: core winCtr register -> 7-bit zero detect (shared) ->
  // ras_secret D data and the read-forward flop; the RAS write select is unchanged.  Cost: about
  // 1 LUT per RAS slot plus the forward flop (previously a constant).
  if (ENABLE_IFT) { ras.io.write_secret := io.cpu.cf_win_sec_ctr =/= 0.U }
  // [RASPTR] patch 29 (F-RAS-1): the f3 push (default) vs the top repair (below), and the
  // attacker pointer move: an attacker-domain packet whose first CFI is a call (push) or a ret
  // (pop), on the same f3 fire that writes the push / takes the ghist pop.
  ras.io.write_push := true.B
  ras.io.atk_move   := (if (ENABLE_IFT) f3.io.deq.valid && f4_ready && f3_fetch_bundle.cfi_idx.valid &&
                                        (f3_fetch_bundle.cfi_is_call || f3_fetch_bundle.cfi_is_ret) &&
                                        f3_fetch_is_attacker
                        else false.B)


  val f3_correct_f1_ghist = s1_ghist =/= f3_predicted_ghist && enableGHistStallRepair.B
  val f3_correct_f2_ghist = s2_ghist =/= f3_predicted_ghist && enableGHistStallRepair.B

  when (f3.io.deq.valid && f4_ready) {
    when (f3_fetch_bundle.cfi_is_call && f3_fetch_bundle.cfi_idx.valid) {
      ras.io.write_valid := true.B
    }
    when (f3_redirects.reduce(_||_)) {
      f3_prev_is_half := false.B
    }
    when (s2_valid && s2_vpc === f3_predicted_target && !f3_correct_f2_ghist) {
      f3.io.enq.bits.ghist := f3_predicted_ghist
      // [D4FIX] fix A (D4, stock-BOOM correctness, applies with ENABLE_IFT off too): the f3
      // ras_idx hand-off above reaches the successor only if f3.io.enq fires.  When f2 replays s2
      // instead (no I$ response this cycle -- the corefuzzing fetch throttle makes this common at
      // CoreWidth <= 2 -- since f3 is dequeuing here f3_ready is true, so f2_replay reduces to
      // s2_valid && !icache.io.resp.valid), the replay at the f2 block above sends s0_ghist :=
      // s2_ghist, whose ras_idx predates this call/ret (f1/f2 pass call/ret = false), and ===
      // ignores ras_idx, so nothing repairs it.  Carry the f3 pointer into the replay.
      // Only ras_idx: under !f3_correct_f2_ghist (enableGHistStallRepair) the === fields of
      // s2_ghist and f3_predicted_ghist are equal, and secret_tag/secret_ctr have no reader, so
      // the BPD-visible history bits keep their select unchanged.  A later redirect_flush/sfence
      // assignment still wins (last connect).  Levels: f3_predicted_ghist.ras_idx is already a
      // data input of this mux (f3 redirect leg); the leg's select gains one early term
      // (!icache.io.resp.valid).  Endpoint: the 5 s1_ghist.ras_idx flops only (the predictor
      // reads f0_req.bits.ghist only through histories(), never ras_idx).
      when (f2_replay) {
        s0_ghist.ras_idx := f3_predicted_ghist.ras_idx
      }
    } .elsewhen (!s2_valid && s1_valid && s1_vpc === f3_predicted_target && !f3_correct_f1_ghist) {
      s2_ghist := f3_predicted_ghist
    } .elsewhen (( s2_valid &&  (s2_vpc =/= f3_predicted_target || f3_correct_f2_ghist)) ||
          (!s2_valid &&  s1_valid && (s1_vpc =/= f3_predicted_target || f3_correct_f1_ghist)) ||
          (!s2_valid && !s1_valid)) {
      f2_clear := true.B
      f1_clear := true.B

      s0_valid     := !(f3_fetch_bundle.xcpt_pf_if || f3_fetch_bundle.xcpt_ae_if)
      s0_vpc       := f3_predicted_target
      s0_is_replay := false.B
      s0_redir_atk := f3_fetch_bundle.cfi_idx.valid && f3_fetch_is_attacker   // [ICREDIR] taken CFI at f3
      s0_ghist     := f3_predicted_ghist
      s0_tsrc      := BSRC_3

      f3_fetch_bundle.fsrc := BSRC_3
    }
  }

  // When f3 finds a btb mispredict, queue up a bpd correction update
  val f4_btb_corrections = Module(new Queue(new BranchPredictionUpdate, 2))
  f4_btb_corrections.io.enq.valid := f3.io.deq.fire && f3_btb_mispredicts.reduce(_||_) && enableBTBFastRepair.B
  f4_btb_corrections.io.enq.bits  := DontCare
  f4_btb_corrections.io.enq.bits.is_mispredict_update := false.B
  f4_btb_corrections.io.enq.bits.is_repair_update     := false.B
  f4_btb_corrections.io.enq.bits.btb_mispredicts      := f3_btb_mispredicts.asUInt
  f4_btb_corrections.io.enq.bits.pc                   := f3_fetch_bundle.pc
  f4_btb_corrections.io.enq.bits.ghist                := f3_fetch_bundle.ghist
  f4_btb_corrections.io.enq.bits.lhist                := f3_fetch_bundle.lhist
  f4_btb_corrections.io.enq.bits.meta                 := f3_fetch_bundle.bpd_meta
  // corefuzzing: carry fetch domain into BTB correction update
  f4_btb_corrections.io.enq.bits.cf_domain_id.foreach(_ := f3_fetch_bundle.cf_fetch_domain.getOrElse(0.U(1.W)))


  // -------------------------------------------------------
  // **** F4 ****
  // -------------------------------------------------------
  val f4_clear = WireInit(false.B)
  val f4 = withReset(reset.asBool || f4_clear) {
    Module(new Queue(new FetchBundle, 1, pipe=true, flow=false))}

  val fb  = Module(new FetchBuffer)
  // corefuzzing: drive the fetch-throttle gate Wire now that fb is instantiated
  cf_fetch_throttle_gate := fb.io.fetch_throttle_gate
  // [SPECBPU] the FTQ keeps the resolution-trained slice of the predictor meta for the resolution-time update
  val ftq = Module(new FetchTargetQueue(bpd.specMetaLo, bpd.specMetaW))
  // C3: assigned here (not beside the other f3_fetch_bundle connects at :1009)
  // only because `ftq` is not in Scala scope until this line.  f3_fetch_bundle is a
  // Wire, so the connect resolves regardless of textual position.
  // C3 TOGGLE: held false so the address-taint fix (D) can be measured alone.
  // Chisel constant-folds the whole ty=19 path away, so slot pressure and s_prop are
  // untouched.  Flip to ftq.io.cf_ghist_secret_active to enable C3.
  // [GHIST-ENABLE 2026-09-08] C3 re-enabled.  This was held false so the address-taint
  // fix (D) could be measured in isolation; that experiment is long finished, and while
  // it was parked Chisel constant-folded the ENTIRE ty=19 INFL_GHIST_STATE path away --
  // the channel could never fire, in any workload, which is why it showed 0 occurrences
  // across every log.  Enabling it restores secret-tainted global history as an
  // influencer source.
  f3_fetch_bundle.ghist_secret.foreach(_ := io.cpu.cf_win_sec_ctr =/= 0.U)   // [ARMRES] core window
  // [ICSEC] step 15 (residue audit row 18): the I$ line secret bit was "fetch PC inside the
  // secret DATA range", which code practically never is, so a line fetched only because of
  // secret-dependent control flow was recorded clean.  The fetch is now also secret while the
  // secret window (io.cpu.cf_win_sec_ctr, core.scala [ARMRES]) is open.
  // [COMMENTFIX 2026-10-22] (was: "the committed secret-branch window ... armed by a secret branch packet" and
  // "wrong-path fetches before the branch commits stay unmarked"; both outdated, comment only): the window ARMS
  // when a secret-conditioned branch, or a jalr whose target register is secret ([JALRWIN]), RESOLVES, wrong-path
  // ones included, and decays per committed GHR shift.  So a fetch made after the arming resolve (e.g. the
  // redirect of that jalr's own mispredict) is marked even if it is wrong-path; a fetch made before the resolve
  // is not (secretness is not known yet at fetch).
  // Registered (1 FF) to keep the core counter compare off the I$ s1 path.  The same bit gates
  // the reader-side mismatch: a fetch inside the window does not flag itself.
  // [R124GATE] patch 26: the window-term flop exists only with ENABLE_IFT.
  icache.io.s1_is_secret := (if (ENABLE_IFT) s1_fetch_is_secret || RegNext(io.cpu.cf_win_sec_ctr =/= 0.U, false.B)
                             else s1_fetch_is_secret)
  f3_fetch_bundle.ghist_atk.foreach(_ := io.cpu.cf_win_atk_ctr =/= 0.U)
  f3_fetch_bundle.ghist_secret_ctr.foreach(_ := io.cpu.cf_win_sec_ctr)
  f3_fetch_bundle.ghist_atk_ctr.foreach(_ := io.cpu.cf_win_atk_ctr)
  // corefuzzing: late secret update from FTQ — fires when a secret instruction commits from a
  // call-containing fetch packet, retroactively marking the pushed RAS slot as secret
  ras.io.late_write_secret_valid := ftq.io.cf_ras_secret_upd_valid
  // [RASIDX] step 19: the FTQ stores the PRE-push RAS pointer (ghist.ras_idx,
  // fetch-target-queue.scala new_entry.ras_idx); a call pushed its return address at WrapInc
  // of it (ras.io.write_idx above), so mark that slot, not the caller's slot below it.
  // Off the fetch path (FTQ register read -> RAS register write); 5-bit wrap-inc per lane.
  ras.io.late_write_secret_idx   := VecInit(ftq.io.cf_ras_secret_upd_idx.map(i => WrapInc(i, cf_ras_active)))
  // [WINFTQ] patch 29b: the window range's calls (FTQ queue, one per cycle), same slot rule
  ftq.io.cf_win_ftq_rs.foreach(_ := io.cpu.cf_win_ftq_rs.get)
  ftq.io.cf_win_ftq_ra.foreach(_ := io.cpu.cf_win_ftq_ra.get)   // [WINFTQATK]
  ftq.io.cf_win_sec_open.foreach(_ := io.cpu.cf_win_sec_ctr =/= 0.U)   // [WINFTQPOST] patch 38
  ftq.io.cf_win_atk_open.foreach(_ := io.cpu.cf_win_atk_ctr =/= 0.U)   // [M3ATKWIN] lean58
  ftq.io.cf_atk_ftq_updates.foreach(_ := io.cpu.cf_atk_ftq_updates.get)   // [FTQATK] patch 47
  ras.io.late_write_win.foreach { l =>
    l.valid := ftq.io.cf_ras_win_upd.get.valid
    l.bits  := WrapInc(ftq.io.cf_ras_win_upd.get.bits, cf_ras_active)
  }
  ras.io.late_write_win_kind.foreach(_ := ftq.io.cf_ras_win_kind.get)   // [WINFTQATK] {atk, sec} of that slot
  ras.io.quiesce_flush           := io.cpu.cf_ras_quiesce_flush
  // corefuzzing
  // Wire frontend cf_debug gate into fetch buffer and FTQ
  fb.io.cf_debug_fetchbuf_enable := io.cpu.cf_debug_frontend_enable
  fb.io.brupdate                 := io.cpu.brupdate
  fb.io.cf_debug_rob_enable      := io.cpu.cf_debug_rob_enable
  fb.io.cf_attacker_start_addr   := io.cpu.cf_attacker_start_addr
  fb.io.cf_attacker_end_addr     := io.cpu.cf_attacker_end_addr
  fb.io.cf_dbg.foreach(_ := io.cpu.cf_dbg.getOrElse(false.B))   // [DBGFREEZE] patch 85 (b)
  ftq.io.cf_debug_ftq_enable   := io.cpu.cf_debug_frontend_enable
  ftq.io.cf_ftq_idx            := io.cpu.cf_ftq_idx
  // [F-RAS-3b] same wire as the F1/F2/F3 ghist updates and the RAS write index: no skew.
  ftq.io.cf_ras_active.foreach(_ := cf_ras_active)
  ftq.io.cf_ftq_quiesce_reset  := io.cpu.cf_ftq_quiesce_reset
  icache.io.cf_icache_set_conf := io.cpu.cf_icache_set_conf
  icache.io.cf_dbg_print.foreach(_ := io.cpu.cf_debug_rob_enable)   // [ICFILL] debug print enable (0xbc0 bit 4)
  icache.io.cf_icache_way_conf := io.cpu.cf_icache_way_conf
  // Ceviche (C4f): local copies on the frontend side of the SLR2-SLR3 cut (quasi-static CSR values, exact).
  icache.io.cf_ceviche_en.foreach(_     := RegNext(io.cpu.cf_ceviche_en.get, false.B))
  icache.io.cf_ceviche_pol.foreach(_    := RegNext(io.cpu.cf_ceviche_pol.get))
  icache.io.cf_ceviche_hard_i.foreach(_ := RegNext(io.cpu.cf_ceviche_hard_i.get))
  icache.io.cf_ceviche_soft_i.foreach(_ := RegNext(io.cpu.cf_ceviche_soft_i.get))
  icache.io.cf_cev_inval_dom.foreach(_  := io.cpu.cf_cev_inval_dom.get)          // same cycle as flush_icache (:499)
  icache.io.cf_cev_dbg_flush.foreach(_ := io.cpu.cf_cev_dbg_flush.get)

  // When we mispredict, we need to repair

  // Deal with sfbs
  val f4_shadowable_masks = VecInit((0 until fetchWidth) map { i =>
     f4.io.deq.bits.shadowable_mask.asUInt |
    ~f4.io.deq.bits.sfb_masks(i)(fetchWidth-1,0)
  })
  val f3_shadowable_masks = VecInit((0 until fetchWidth) map { i =>
    Mux(f4.io.enq.valid, f4.io.enq.bits.shadowable_mask.asUInt, 0.U) |
    ~f4.io.deq.bits.sfb_masks(i)(2*fetchWidth-1,fetchWidth)
  })
  val f4_sfbs = VecInit((0 until fetchWidth) map { i =>
    enableSFBOpt.B &&
    ((~f4_shadowable_masks(i) === 0.U) &&
     (~f3_shadowable_masks(i) === 0.U) &&
     f4.io.deq.bits.sfbs(i) &&
     !(f4.io.deq.bits.cfi_idx.valid && f4.io.deq.bits.cfi_idx.bits === i.U) &&
      Mux(f4.io.deq.bits.sfb_dests(i) === 0.U,
        !bank_prev_is_half,
      Mux(f4.io.deq.bits.sfb_dests(i) === fetchBytes.U,
        !f4.io.deq.bits.end_half.valid,
        true.B)
      )

     )
  })
  val f4_sfb_valid = f4_sfbs.reduce(_||_) && f4.io.deq.valid
  val f4_sfb_idx   = PriorityEncoder(f4_sfbs)
  val f4_sfb_mask  = f4.io.deq.bits.sfb_masks(f4_sfb_idx)
  // If we have a SFB, wait for next fetch to be available in f3
  val f4_delay     = (
    f4.io.deq.bits.sfbs.reduce(_||_) &&
    !f4.io.deq.bits.cfi_idx.valid &&
    !f4.io.enq.valid &&
    !f4.io.deq.bits.xcpt_pf_if &&
    !f4.io.deq.bits.xcpt_ae_if
  )
  when (f4_sfb_valid) {
    f3_shadowed_mask := f4_sfb_mask(2*fetchWidth-1,fetchWidth).asBools
  } .otherwise {
    f3_shadowed_mask := VecInit(0.U(fetchWidth.W).asBools)
  }

  f4_ready := f4.io.enq.ready
  f4.io.enq.valid := f3.io.deq.valid && !f3_clear
  f4.io.enq.bits  := f3_fetch_bundle
  f4.io.deq.ready := fb.io.enq.ready && ftq.io.enq.ready && !f4_delay
  // [FTQFULL] patch 59: ftq.io.enq.ready and f4.io.deq.valid are registers (FTQ RegNext, f4 Queue),
  // so this is 1 LUT -> 1 FF, off every fetch path (a new leaf).  IFT off: no port, no flop.
  io.cpu.cf_ftq_full.foreach(_ := RegNext(f4.io.deq.valid && !ftq.io.enq.ready, false.B))

  fb.io.enq.valid := f4.io.deq.valid && ftq.io.enq.ready && !f4_delay
  fb.io.enq.bits  := f4.io.deq.bits
  fb.io.enq.bits.ftq_idx := ftq.io.enq_idx
  // [FTQHOLD] FTQ plan T1: f4 FTQ-hold counter.  Counts the cycles the packet now in f4 waits with !ftq.io.enq.ready
  // (only the FTQ term: a fetch-buffer-full or f4_delay hold neither counts nor clears), saturating at 63 like
  // dis_stall_rob_cycles (core.scala).  Cleared when f4 is empty or its packet leaves (f4.io.deq.fire): the plan's
  // ".elsewhen (fire)" plus the empty case, so a held packet dropped by f4_clear cannot hand its count to the next
  // packet (f4 is empty the cycle after a clear).  At fire ftq.io.enq.ready = 1, so the count is complete.  The bucket
  // is its own register (f4_hold_b3 = Log2Bucket of the count's next value) and goes to the fetch buffer as an input
  // port (fb.io.cf_ftq_hold3), so FetchBundle, the f4 queue and the FTQ are unchanged and the fb leg is a flop.
  // Levels: the hold term is 1 LUT on two flops (f4 full flag, the FTQ's RegNext ready); the clear is f4.io.deq.fire
  // + 1 LUT into the new flops, the depth of the f4 queue's own full-flag update (a new leaf; no existing path gains
  // a level).  No stall / ready / valid change.  9 FF, about 12 LUT.  IFT off: not elaborated.
  if (ENABLE_IFT) {
    val f4_hold_cnt = RegInit(0.U(6.W))
    val f4_hold_b3  = RegInit(0.U(3.W))
    val f4_hold_clr = !f4.io.deq.valid || f4.io.deq.fire
    val f4_hold_run = Mux(!ftq.io.enq.ready && f4_hold_cnt =/= 63.U, f4_hold_cnt + 1.U, f4_hold_cnt)   // flops only
    f4_hold_cnt := Mux(f4_hold_clr, 0.U, f4_hold_run)
    f4_hold_b3  := Mux(f4_hold_clr, 0.U, Log2Bucket(f4_hold_run))
    fb.io.cf_ftq_hold3.get := f4_hold_b3
  }
  fb.io.enq.bits.sfbs    := Mux(f4_sfb_valid, UIntToOH(f4_sfb_idx), 0.U(fetchWidth.W)).asBools
  fb.io.enq.bits.shadowed_mask := (
    Mux(f4_sfb_valid, f4_sfb_mask(fetchWidth-1,0), 0.U(fetchWidth.W)) |
    f4.io.deq.bits.shadowed_mask.asUInt
  ).asBools

  // value from associated CSR for corefuzzing - alex
  fb.io.cf_fb_idx          := io.cpu.cf_fb_idx
  fb.io.cf_active_width    := io.cpu.cf_active_width


  ftq.io.enq.valid          := f4.io.deq.valid && fb.io.enq.ready && !f4_delay
  ftq.io.enq.bits           := f4.io.deq.bits
  // corefuzzing: wire commit-time secret feedback from core to FTQ
  ftq.io.cf_secret_ftq_updates := io.cpu.cf_secret_ftq_updates

  val bpd_update_arbiter = Module(new Arbiter(new BranchPredictionUpdate, 2))
  bpd_update_arbiter.io.in(0).valid := ftq.io.bpdupdate.valid
  bpd_update_arbiter.io.in(0).bits  := ftq.io.bpdupdate.bits
  assert(bpd_update_arbiter.io.in(0).ready)
  bpd_update_arbiter.io.in(1) <> f4_btb_corrections.io.deq
  bpd.io.update := bpd_update_arbiter.io.out
  bpd_update_arbiter.io.out.ready := true.B
  // [SPECBPU] (owner 2026-11-02, RULES C 'Speculative TAGE/BTB updates'): the resolution-time path.  The core's
  // registered resolved CFI addresses the FTQ; the FTQ's update goes straight to the predictor's second bus, read
  // by the TAGE, BTB, BIM and uBTB.  The commit update above (arbiter, unchanged) now reaches only the loop
  // predictor, which writes only on the mispredict / repair updates of the disabled FTQ repair walk (inert, as
  // before); the f4 fast repair (input 1) reaches no predictor write (it wrote nothing before either: scope P3).
  ftq.io.bpu_res  := io.cpu.bpu_res
  ftq.io.bpu_sq   := io.cpu.bpu_sq   // [SPECBPU-REP] the redirect's slot rule (undo-log squash test)
  // [SPECBPU-REP] debug printf builds only: the config indices for the FTQ's [BPDC] DMI reconfiguration measurement
  ftq.io.dbg_cfg.foreach(_ := Cat(io.cpu.cf_btb_way_idx, io.cpu.cf_btb_set_idx, io.cpu.cf_bpd_tage_to_gshare,
                                  io.cpu.cf_tage_count_idx, io.cpu.cf_ftq_idx))
  bpd.io.spec_update := ftq.io.spec_bpdupdate
  // [V3TAP-RAS] SP-18 (design 5.1 RAS [134:114]): the RAS write port; both writers (the f3 call push and the FTQ top
  // repair below) drive ras.io.write_*, which end at the RAS register write.  Read here (final connected values)
  // and registered once (the RAS tap register, E -> E+1).  Record/STATE builds only.
  val cf_ras_tap_q = if (ENABLE_IFT_BRIDGE || IFT_ORACLE) Some {
    require(log2Ceil(nRasEntries) == 5 && (new IftRasTap).getWidth == 22, "STATE RAS group: 32 entries (design 5.1)")
    val t = Wire(new IftRasTap)
    t.v        := ras.io.write_valid
    t.g.push   := ras.io.write_push
    t.g.sec    := ras.io.write_secret
    t.g.watk   := ras.io.write_watk.get
    t.g.dom    := ras.io.write_domain
    t.g.idx    := ras.io.write_idx
    t.g.addr12 := ras.io.write_addr(13, 2)
    val cf_state_ras_tap = RegNext(t, 0.U.asTypeOf(t))   // the RAS tap register (named cf_state_*: SP-27, review M5)
    cf_state_ras_tap
  } else None
  // [V3TAP-PRED] SP-19 (design 5.1 PRED [253:135], 5.3): one predictor update at its write stage.  The update bus
  // ([SPECBPU]: the FTQ resolution update; cycle u; ends at every bank's s1_update register) is piped u -> u+1 -> u+2;
  // the banks' BTB / uBTB taps arrive padded to u+2 and the TAGE taps are the decode of the u+2 table-input
  // registers.  All are sampled at E = u+2 (the TAGE table write) by the PRED tap register (E -> E+1), then the
  // SP-18 cut-out register.  gv_PRED = an update or a BTB re-reset start.  Outputs only.
  val cf_pred_tap_q = if (ENABLE_IFT_BRIDGE || IFT_ORACLE) Some {
    require(nBanks == 2 && fetchWidth == 8 && (new IftPredTap).getWidth == 120, "STATE PRED: 2 banks, fetchWidth 8 (design 5.1)")
    // [SPECBPU-REC] (owner 2026-11-02, RULES C 'Speculative BPU follow-ups' O1 "(a) BIM+uBTB too"): ONE update stream.
    // Every predictor write (TAGE, BTB, BIM, uBTB) now comes from the resolution update (bus spec_update), so the
    // u fields describe that update alone, with the same field meanings as before: one update per record, u_kind = 0
    // (a training update), u_cfi_v = 1, u_brmask = the branch's slot (0 for jal / jalr), labels and ghist = the
    // update's own.  The commit arbiter (commit update + f4 fast repair) now reaches only the loop predictor, which
    // never writes on it (scope report Q1/P3), so it is no longer recorded (u_kind = 1 no longer occurs).  The
    // update enters the banks at the same stage as the arbiter output did, so the write-stage alignment (u -> u+2)
    // and the bank taps (btb / ubtb / tg_*) are unchanged.  What changes for the host is the ORDER (resolution order,
    // wrong path included, one CFI per record): design note rev1 section 4 lists the parser changes.
    // Levels: the FTQ update bus (as before for the arbiter output) -> u1 flops, no select (the 2:1 is gone).
    // Record builds only.
    val sp = ftq.io.spec_bpdupdate
    val u0 = Wire(new IftPredU)
    u0.v       := sp.valid && !sp.bits.is_mispredict_update   // [SPECBPU-REP] a Q2 (a) drop trains nothing: no record
    u0.brmask  := sp.bits.br_mask
    u0.cfi_v   := sp.bits.cfi_idx.valid
    u0.cfi_idx := sp.bits.cfi_idx.bits
    u0.taken   := sp.bits.cfi_taken
    u0.mispred := sp.bits.cfi_mispredicted
    u0.isbr    := sp.bits.cfi_is_br
    u0.isjal   := sp.bits.cfi_is_jal
    u0.sec     := sp.bits.cf_is_secret.get
    u0.dom     := sp.bits.cf_domain_id.get(0)
    // [SPECBPU-REP] u_kind = 1: a repair (u_isbr or u_isjal = 1, the undone training's own fields) or, [SPECBPU-RLB],
    // a relabel (u_isbr = u_isjal = 0; u_dom / u_sec = the new label of the training with the same pc12 / slot / history)
    u0.kind    := sp.bits.is_repair_update
    u0.pc12    := sp.bits.pc(13, 2)
    u0.nst     := sp.bits.ghist.new_saw_branch_taken
    u0.nsnt    := sp.bits.ghist.new_saw_branch_not_taken
    u0.hist16  := sp.bits.ghist.old_history(15, 0)
    val u1 = RegNext(u0, 0.U.asTypeOf(u0))     // u+1
    val u2 = RegNext(u1, 0.U.asTypeOf(u1))     // u+2 = E
    val bt = bpd.io.cf_st.get                  // per physical bank, already at E
    val rst = bt(0).btb_rst || bt(1).btb_rst
    val a0 = bt(0).tg_alloc_v
    val a1 = bt(1).tg_alloc_v
    val d0 = bt(0).tg_udec_mask.orR
    val d1 = bt(1).tg_udec_mask.orR
    val p  = Wire(new IftPredTap)
    p.v                := u2.v || rst
    p.g.u_brmask       := u2.brmask
    p.g.u_cfi_v        := u2.cfi_v
    p.g.u_cfi_idx      := u2.cfi_idx
    p.g.u_taken        := u2.taken
    p.g.u_mispred      := u2.mispred
    p.g.u_isbr         := u2.isbr
    p.g.u_isjal        := u2.isjal
    p.g.u_sec          := u2.sec
    p.g.u_dom          := u2.dom
    p.g.u_kind         := u2.kind
    p.g.btb_rst        := rst
    p.g.u_pc12         := u2.pc12
    p.g.btb0_v         := bt(0).btb_v
    p.g.btb0_set       := bt(0).btb_set
    p.g.btb0_way       := bt(0).btb_way
    p.g.btb1_v         := bt(1).btb_v
    p.g.btb1_set       := bt(1).btb_set
    p.g.btb1_way       := bt(1).btb_way
    p.g.ubtb0_v        := bt(0).ubtb_v
    p.g.ubtb0_way      := bt(0).ubtb_way
    p.g.ubtb1_v        := bt(1).ubtb_v
    p.g.ubtb1_way      := bt(1).ubtb_way
    p.g.tg_prov        := Cat(bt(1).tg_prov, bt(0).tg_prov)          // bank-slot s = 4 * bank + slot, s = 0 at [2:0]
    p.g.tg_alloc_v     := a0 || a1
    p.g.tg_alloc_table := Mux(a0, bt(0).tg_alloc_table, Mux(a1, bt(1).tg_alloc_table, 0.U))
    p.g.tg_alloc_slot  := Mux(a0, Cat(0.U(1.W), bt(0).tg_alloc_slot), Mux(a1, Cat(1.U(1.W), bt(1).tg_alloc_slot), 0.U))
    p.g.tg_udec_mask   := bt(0).tg_udec_mask | bt(1).tg_udec_mask
    p.g.tg_udec_slot   := Mux(d0, Cat(0.U(1.W), bt(0).tg_udec_slot), Mux(d1, Cat(1.U(1.W), bt(1).tg_udec_slot), 0.U))
    p.g.tg_nst         := u2.nst
    p.g.tg_nsnt        := u2.nsnt
    p.g.tg_hist16      := u2.hist16
    if (ENABLE_CF_DEBUG_PRINTF && ENABLE_IFT) {
      // [SPECBPU-REC] the commit arbiter is no longer recorded; a mispredict / repair update there (the disabled
      // walk) would write the loop predictor unseen by the record, so it stays a hard stop.
      assert(!(bpd_update_arbiter.io.out.valid && (bpd_update_arbiter.io.out.bits.is_mispredict_update ||
                                                   bpd_update_arbiter.io.out.bits.is_repair_update)),
             "[V3TAP-PRED] a mispredict/repair predictor update reached the arbiter output (the loop would write it unrecorded)")
      assert(!(a0 && a1), "[V3TAP-PRED] TAGE allocations in both banks in one update")
      assert(!(d0 && d1), "[V3TAP-PRED] TAGE u-only writes in both banks in one update")
    }
    val cf_state_pred_tap = RegNext(p, 0.U.asTypeOf(p))   // the PRED tap register: E -> E+1 (named cf_state_*: SP-27, M5)
    cf_state_pred_tap
  } else None
  // [V3CUT] SP-18 (design 5.4, 6.6; 08 section 8.4): the frontend cut-out register.  IC tap (I$, held E+1), RAS tap
  // (E+1), PRED tap (SP-19, E+1) -> held E+2 on io.cf_state, toward the tile's cut-in register (SP-16): 158 wires
  // SLR3 -> SLR2, a register on both sides.
  io.cf_state.foreach { o =>
    require((new IftFeState).getWidth == 158, "STATE frontend cut = 158 wires (08 section 8.4)")
    val fe = Wire(new IftFeState)
    fe.ic   := icache.io.cf_ic_tap.get
    fe.ras  := cf_ras_tap_q.get
    fe.pred := cf_pred_tap_q.get
    val cf_state_cut = RegNext(fe, 0.U.asTypeOf(fe))   // the cut-out register (named cf_state_*: SP-27, review M5)
    o := cf_state_cut
  }

  when (ftq.io.ras_update && enableRasTopRepair.B) {
    ras.io.write_valid := true.B
    ras.io.write_push  := false.B   // [RASPTR] a repair restores content, it does not refresh
    // [RASLBL] DF-ras-repair-label: restore the labels the FTQ saved with ras_top (the slot's domain
    // and secret bit at the packet's f3 read) instead of this cycle's f3 packet domain and false;
    // ras.scala ORs the secret bit into the slot's current one (a later mark is kept).  IFT off:
    // unchanged (write_secret false; write_domain unread).  Levels: the write_domain / write_secret
    // data gain a 2:1 select by the repair (an FTQ flop) beside the f3 domain compare / window zero
    // detect: at most +1 LUT on those data paths (absorbable into the per-slot D LUT), select
    // (write_valid / write_idx) unchanged.
    ras.io.write_secret := (if (ENABLE_IFT) ftq.io.ras_update_sec.get else false.B)
    if (ENABLE_IFT) { ras.io.write_domain := ftq.io.ras_update_dom.get }
    if (ENABLE_IFT) { ras.io.write_watk.get := ftq.io.ras_update_watk.get }   // [B60RAS] restored as saved, like the domain
    ras.io.write_idx   := ftq.io.ras_update_idx
    ras.io.write_addr  := ftq.io.ras_update_pc
  }

  // [D4RAS] D4 probe (2026-10-18, measurement only, no logic change): every f3 RAS push/pop with
  // its pre-push pointer, the top repair, every front-end redirect's ras_idx, and the hand-off
  // hole under test: a call/ret packet leaves f3 while its successor sits in s2 with the same
  // PC but no I$ response (f3.io.enq does not fire), so the f3_predicted_ghist ras_idx written
  // into f3.io.enq.bits.ghist (line 1082) is dropped and s2 replays with s2_ghist (line 670).
  // GlobalHistory === ignores ras_idx (line 98), so f3_correct_f2_ghist cannot catch it.
  // Compile-time ENABLE_CF_DEBUG_PRINTF; runtime 0xbc0 bit 4 (cf_debug_rob_enable, set by
  // ENABLE_DEBUG()=29; the frontend bit 6 is not).  Printf cones only; elided in non-debug builds.
  if (ENABLE_CF_DEBUG_PRINTF) {
    val d4_cyc = RegInit(0.U(40.W))
    d4_cyc := d4_cyc + 1.U
    val d4_on    = io.cpu.cf_debug_rob_enable
    val d4_fire  = f3.io.deq.valid && f4_ready && f3_fetch_bundle.cfi_idx.valid
    val d4_cfipc = f3_aligned_pc + (f3_fetch_bundle.cfi_idx.bits << 1)   // -2 when the cfi is an edge inst
    val d4_push_req = d4_fire && f3_fetch_bundle.cfi_is_call
    when (d4_on && d4_push_req) {
      printf("[D4RAS] c=%d PUSH pkt=0x%x cfi=0x%x pre=%d widx=%d waddr=0x%x rep=%d f3clr=%d act=%d thr=%d\n",
        d4_cyc, f3_imemresp.pc, d4_cfipc, f3_fetch_bundle.ghist.ras_idx,
        WrapInc(f3_fetch_bundle.ghist.ras_idx, cf_ras_active),
        f3_aligned_pc + (f3_fetch_bundle.cfi_idx.bits << 1) + Mux(f3_fetch_bundle.cfi_npc_plus4, 4.U, 2.U),
        ftq.io.ras_update && enableRasTopRepair.B, f3_clear, cf_ras_active, cf_fetch_throttle_gate)
    }
    when (d4_on && d4_fire && f3_fetch_bundle.cfi_is_ret) {
      printf("[D4RAS] c=%d POP pkt=0x%x cfi=0x%x pre=%d raddr=0x%x tgt=0x%x f3clr=%d act=%d thr=%d\n",
        d4_cyc, f3_imemresp.pc, d4_cfipc, f3_fetch_bundle.ghist.ras_idx, ras.io.read_addr,
        f3_predicted_target, f3_clear, cf_ras_active, cf_fetch_throttle_gate)
    }
    // [D4FIX] HANDOFF = the hole's trigger occurred (any build); LEAK = the replay actually carried a
    // pointer different from the f3 one (the drop itself; expected 0 with fix A).  rpidx is the
    // ras_idx the replay sends to s0.  Same-cycle redirect_flush/sfence squash the replay: not a drop.
    val d4_handoff = d4_fire && (f3_fetch_bundle.cfi_is_call || f3_fetch_bundle.cfi_is_ret) &&
          s2_valid && s2_vpc === f3_predicted_target && !f3_correct_f2_ghist && !f3.io.enq.fire
    when (d4_on && d4_handoff) {
      printf("[D4RAS] c=%d HANDOFF pkt=0x%x s2=0x%x s2idx=%d f3nidx=%d rpidx=%d icv=%d thr=%d\n",
        d4_cyc, f3_imemresp.pc, s2_vpc, s2_ghist.ras_idx, f3_predicted_ghist.ras_idx,
        s0_ghist.ras_idx, icache.io.resp.valid, cf_fetch_throttle_gate)
    }
    when (d4_on && d4_handoff && s0_ghist.ras_idx =/= f3_predicted_ghist.ras_idx &&
          !io.cpu.redirect_flush && !io.cpu.sfence.valid) {
      printf("[D4RAS] c=%d LEAK pkt=0x%x s2=0x%x s2idx=%d f3nidx=%d icv=%d thr=%d f3rdy=%d\n",
        d4_cyc, f3_imemresp.pc, s2_vpc, s2_ghist.ras_idx, f3_predicted_ghist.ras_idx,
        icache.io.resp.valid, cf_fetch_throttle_gate, f3_ready)
    }
    when (d4_on && ftq.io.ras_update) {
      printf("[D4RAS] c=%d REPAIR idx=%d addr=0x%x pushlost=%d\n",
        d4_cyc, ftq.io.ras_update_idx, ftq.io.ras_update_pc, d4_push_req && enableRasTopRepair.B)
    }
    when (d4_on && io.cpu.redirect_flush && io.cpu.redirect_val) {
      printf("[D4RAS] c=%d REDIR pc=0x%x idx=%d ftq=%d misp=%d\n",
        d4_cyc, io.cpu.redirect_pc, io.cpu.redirect_ghist.ras_idx, io.cpu.redirect_ftq_idx,
        io.cpu.redirect_flush_is_mispredict)
    }
  }


  // -------------------------------------------------------
  // **** To Core (F5) ****
  // -------------------------------------------------------

  io.cpu.fetchpacket <> fb.io.deq
  // the output from the fetch buffer is connected to the output of the frontend
  // these microops will be used down the line by the decoder, dispatcher and so on,

  io.cpu.get_pc <> ftq.io.get_ftq_pc

  // set the privilege bit for IF tracking
  // for (i <- 0 until coreWidth) {
  //   val address_checker = Module(new AddressChecker)
  //  address_checker.io.in := fb.io.deq.bits.uops(i).bits.debug_pc
  //  io.cpu.fetchpacket.bits.uops(i).bits.privilege_tag := address_checker.io.out

  // }

  // val test = Wire(UInt(1.W))
  // test := io.cpu.fetchpacket.bits.uops(0).bits.privilege_tag;
  //  Trying to print the PCs in the Current Fetch Packet
  // for (i <- 0 until coreWidth) {
      // printf("\nThe (coreWidth index - %d ) PC of current Microop is 0x%x \n", i.asUInt, fb.io.deq.bits.uops(i).bits.debug_pc)
      // printf("\nThe privilege bit is - %d\n", test)//io.cpu.fetchpacket.bits.uops(i).bits.privilege_tag)
      // we use .bits multiple times because the deq is a decoupledIO
      // and then once we get into that, MicroOp class is created as a ValidIO.
  // }

  ftq.io.deq := io.cpu.commit
  ftq.io.brupdate := io.cpu.brupdate

  ftq.io.redirect.valid   := io.cpu.redirect_val
  ftq.io.redirect.bits    := io.cpu.redirect_ftq_idx
  fb.io.clear := false.B

  when (io.cpu.sfence.valid) {
    fb.io.clear := true.B
    f4_clear    := true.B
    f3_clear    := true.B
    f2_clear    := true.B
    f1_clear    := true.B

    s0_valid     := false.B
    s0_vpc       := io.cpu.sfence.bits.addr
    s0_is_replay := false.B
    s0_is_sfence := true.B
    s0_redir_atk := false.B   // [ICREDIR]

  }.elsewhen (io.cpu.redirect_flush) {
    fb.io.clear := true.B
    f4_clear    := true.B
    f3_clear    := true.B
    f2_clear    := true.B
    f1_clear    := true.B

    f3_prev_is_half := false.B

    s0_valid     := io.cpu.redirect_val
    // corefuzzing: suppress branch-mispredict PC update during quiesce (allow_fetch=false).
    // The Packet-B/C oscillation (established by Packet A's flush_on_commit) must be
    // preserved so QS_FETCH resumes from Packet B, not from the branch target.
    // ROB flush-on-commit redirects (redirect_flush_is_mispredict=false) always update s0_vpc.
    s0_vpc       := Mux(io.cpu.redirect_flush_is_mispredict && !io.cpu.allow_fetch,
                        s1_vpc, io.cpu.redirect_pc)
    // [ICREDIR] a mispredict redirect is made by brupdate.b2's uop (core.scala); a ROB flush redirect is not a CFI: 0
    s0_redir_atk := Mux(io.cpu.redirect_flush_is_mispredict && !io.cpu.allow_fetch, s1_redir_atk,
                        io.cpu.redirect_flush_is_mispredict &&
                        io.cpu.brupdate.b2.uop.cf_domain_id.getOrElse(0.U) =/= 0.U)
    s0_ghist     := io.cpu.redirect_ghist
    s0_tsrc      := BSRC_C
    s0_is_replay := false.B

    ftq.io.redirect.valid := io.cpu.redirect_val
    ftq.io.redirect.bits  := io.cpu.redirect_ftq_idx
  }

  ftq.io.debug_ftq_idx := io.cpu.debug_ftq_idx
  io.cpu.debug_fetch_pc := ftq.io.debug_fetch_pc


  override def toString: String =
    (BoomCoreStringPrefix("====Overall Frontend Params====") + "\n"
    + icache.toString + bpd.toString)
}
