//******************************************************************************
// Copyright (c) 2012 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Out-of-Order Load/Store Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Load/Store Unit is made up of the Load Queue, the Store Queue (LDQ and STQ).
//
// Stores are sent to memory at (well, after) commit, loads are executed
// optimstically ASAP.  If a misspeculation was discovered, the pipeline is
// cleared. Loads put to sleep are retried.  If a LoadAddr and StoreAddr match,
// the Load can receive its data by forwarding data out of the Store Queue.
//
// Currently, loads are sent to memory immediately, and in parallel do an
// associative search of the STQ, on entering the LSU. If a hit on the STQ
// search, the memory request is killed on the next cycle, and if the STQ entry
// is valid, the store data is forwarded to the load (delayed to match the
// load-use delay to delay with the write-port structural hazard). If the store
// data is not present, or it's only a partial match (SB->LH), the load is put
// to sleep in the LDQ.
//
// Memory ordering violations are detected by stores at their addr-gen time by
// associatively searching the LDQ for newer loads that have been issued to
// memory.
//
// The store queue contains both speculated and committed stores.
//
// Only one port to memory... loads and stores have to fight for it, West Side
// Story style.
//
// TODO:
//    - Add predicting structure for ordering failures
//    - currently won't STD forward if DMEM is busy
//    - ability to turn off things if VM is disabled
//    - reconsider port count of the wakeup, retry stuff

package boom.v3.lsu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{Str, CoreFuzzingConstants}
import freechips.rocketchip.util.property.{cover => RCcover}
import midas.targetutils.AutoCounterCoverModuleAnnotation
import chisel3.experimental.annotate

import boom.v3.common._
import boom.v3.exu.{BrUpdateInfo, Exception, FuncUnitResp, CommitSignals, ExeUnitResp, SpecLdWakeup, WinShadowInfo}

// fore corefuzzing - SpeculativePRintf and Sext
import boom.v3.util.{BoolToChar, AgePriorityEncoder, IsKilledByBranch, GetNewBrMask, WrapInc, IsOlder, UpdateBrMask, SpeculativePrintf}
import boom.v3.util.{Sext, appendModuleTag, addInfluencerBatchList, InfluencerCandidate, SatDropped, CfStatePlusArg}

class LSUExeIO(implicit p: Parameters) extends BoomBundle()(p)
{
  // The "resp" of the maddrcalc is really a "req" to the LSU
  val req       = Flipped(new ValidIO(new FuncUnitResp(xLen)))
  // Send load data to regfiles
  val iresp    = new DecoupledIO(new boom.v3.exu.ExeUnitResp(xLen))
  val fresp    = new DecoupledIO(new boom.v3.exu.ExeUnitResp(xLen+1)) // TODO: Should this be fLen?
}

class BoomDCacheReq(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomUOP
  with CoreFuzzingConstants
{
  val addr  = UInt(coreMaxAddrBits.W)
  val data  = Bits(coreDataBits.W)
  val is_hella = Bool() // Is this the hellacache req? If so this is not tracked in LDQ or STQ
  // [RESID] step 15 (residue audit rows 2/3/13): the access ADDRESS is secret-derived (the
  // address operand's own taint, rs1 at the AGU / ldq|stq.addr_is_secret).  Marks the D$ line
  // it fills or touches (ift_res_meta) and the eviction it causes (ift_evf).  LSU-driven.
  // [R124GATE] patch 26 (ruling 124): the six cf_* request fields below (and the four cf_res_*
  // response fields of BoomDCacheResp) exist only with ENABLE_IFT (None otherwise), so the
  // IFT-off D$ s1/s2, MSHR req, rpq and respq copies lose them.  Same names and order when
  // present: the IFT-on Verilog is unchanged.  Writers use .foreach, readers .getOrElse(0).
  val cf_addr_sec = if (ENABLE_IFT) Some(Bool()) else None
  // [ATKRES] step 17 (owner ruling 119 (3)): the access ADDRESS is attacker-tainted (rs1
  // taint_atk at the AGU / ldq|stq.addr_is_atk).  Marks the line it fills or touches
  // (ift_res_meta aatk bit); later victim readers get attacker value taint + a CACHE_EVICTION
  // {atk=1} edge.  LSU-driven, like cf_addr_sec.
  val cf_addr_atk = if (ENABLE_IFT) Some(Bool()) else None
  // [RESID] D$-internal: a residency/eviction edge for this request's MISS response, carried
  // from the s2 miss into the MSHR and out on its replay.  The LSU drives 0.
  val cf_res_v    = if (ENABLE_IFT) Some(Bool()) else None
  val cf_res_pc   = if (ENABLE_IFT) Some(UInt(inflPcBitsCF.W)) else None   // PC[13:2] of the secret-address access behind it
  // [ATKEVICT] step 17 (ruling 116 C): the edge is an attacker-caused eviction {atk=1,
  // sec = cf_res_esec}; when 0 it is {atk=0, sec=1} as in step 15.  D$-internal; LSU drives 0.
  val cf_res_atk  = if (ENABLE_IFT) Some(Bool()) else None
  val cf_res_esec = if (ENABLE_IFT) Some(Bool()) else None
  // [CEREPL] lean59: the edge's PC (cf_res_pc, from the D$ eviction record) names the access that last advanced the
  // set's replacement state, not the evicting access (dcache.scala [CEREPL]).  D$-internal; LSU drives 0.
  val cf_res_repl = if (ENABLE_IFT) Some(Bool()) else None
  val cf_res_nack = if (ENABLE_IFT) Some(Bool()) else None   // [RECV3] SP-07 3.D: the cf_res_* PC is a nack edge's (DC_NACK)
}

class BoomDCacheResp(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomUOP
  with CoreFuzzingConstants
{
  val data = Bits(coreDataBits.W)
  val is_hella = Bool()
  // [RESID] step 15: an MSHR (miss-path) response's residency/eviction edge {v, PC[13:2]};
  // the D$ turns it into `edges` at uncache_resp.  Zero on every other response.
  // [R124GATE] patch 26: None without ENABLE_IFT (see BoomDCacheReq)
  val cf_res_v  = if (ENABLE_IFT) Some(Bool()) else None
  val cf_res_pc = if (ENABLE_IFT) Some(UInt(inflPcBitsCF.W)) else None
  val cf_res_atk  = if (ENABLE_IFT) Some(Bool()) else None   // [ATKEVICT] step 17, as in BoomDCacheReq
  val cf_res_esec = if (ENABLE_IFT) Some(Bool()) else None
  val cf_res_repl = if (ENABLE_IFT) Some(Bool()) else None   // [CEREPL] as in BoomDCacheReq
  val cf_res_nack = if (ENABLE_IFT) Some(Bool()) else None   // [RECV3] SP-07 3.D, as in BoomDCacheReq
  // IFT overhaul Task 3.2 (D16): the D$ edges of this response (CACHE_EVICTION,
  // MEM_DATAFLOW), built on a zero base.  Zero on every MSHR/uncached response.
  val edges = new IftEdgeList
}

class LSUDMemIO(implicit p: Parameters, edge: TLEdgeOut) extends BoomBundle()(p)
with CoreFuzzingConstants
{
  // In LSU's dmem stage, send the request
  val req         = new DecoupledIO(Vec(memWidth, Valid(new BoomDCacheReq)))
  // In LSU's LCAM search stage, kill if order fail (or forwarding possible)
  val s1_kill     = Output(Vec(memWidth, Bool()))
  // Get a request any cycle
  val resp        = Flipped(Vec(memWidth, new ValidIO(new BoomDCacheResp)))
  // In our response stage, if we get a nack, we need to reexecute
  val nack        = Flipped(Vec(memWidth, new ValidIO(new BoomDCacheReq)))
  // [RESPSEL] patch 25 row 2d (IFT builds only): the D$ store-only-miss acks, moved off `resp`
  // (lane w carries one here exactly when the old resp(w) carried it; then resp(w).valid = 0).
  val st_ack      = if (boomParams.enableIFT) Some(Flipped(Vec(memWidth, new ValidIO(new BoomDCacheResp)))) else None
  // [NACKRES] step 16 (owner ruling 111): registered "a valid MSHR holds a secret-address
  // miss" + its PC[13:2] (mshrs.scala cf_asec_busy); read on a load nack.
  val cf_asec_busy = Flipped(Valid(UInt(inflPcBitsCF.W)))
  // [NACKATK] patch 44 (r111 extended, owner r173 (3)): registered "a valid MSHR holds an
  // attacker-influenced request" (attacker domain OR attacker-tainted address; mshrs.scala
  // cf_aatk_busy).  cf_asec_busy.bits is then the PC of the blocking access, secret-first.
  // cf_nack_mshr(w): lane w's nack is MSHR/WB-caused (MSHR full or same-set, hit on a line an MSHR
  // is evicting, same-set writeback), i.e. neither a probe nor a data-bank nack (registered terms).
  // IFT only (None otherwise).
  val cf_aatk_busy = if (ENABLE_IFT) Some(Flipped(Bool())) else None
  val cf_nack_mshr = if (ENABLE_IFT) Some(Flipped(Vec(memWidth, Bool()))) else None
  // [NACKCONF] patch 68 (V12): lane w's CONFLICTING MSHR(s) hold an attacker-influenced request (mshrs.scala)
  val cf_nack_aatk = if (ENABLE_IFT) Some(Flipped(Vec(memWidth, Bool()))) else None
  // [NACKCONFSEC] lean58 (M8): lane w's CONFLICTING MSHR(s) hold a secret-address / secret-fill request (mshrs.scala)
  val cf_nack_asec = if (ENABLE_IFT) Some(Flipped(Vec(memWidth, Bool()))) else None
  // [RDEDGE] patch 80: lane w's nack was a data-array bank conflict with a TAGGED older lane (dcache.scala)
  val cf_bank_nack = if (ENABLE_IFT) Some(Flipped(Vec(memWidth, Valid(new CF_BankNack)))) else None
  // [V3EDGE] SP-20 (design 6.2 DC_PORT_BLOCK 0x25): the D$ refused the LSU request vector this cycle
  // (req.valid && !req.ready); bits = the winning read-port user's key and record-summary labels (0 when it has
  // no single culprit).  IFT only.
  val cf_port_blk = if (ENABLE_IFT) Some(Flipped(Valid(new CF_BankNack))) else None
  // [NACKPC] stall-edge audit D6 (owner 2026-10-26, RULES C "Stall-edge audit fixes"): lane w's nack PC[13:2] names
  // the CONFLICTING MSHR (mshrs.scala [NACKPC]), chosen by the same mask as cf_nack_asec / cf_nack_aatk.  IFT only.
  val cf_nack_pc = if (ENABLE_IFT) Some(Flipped(Vec(memWidth, UInt(inflPcBitsCF.W)))) else None

  val brupdate     = Output(new BrUpdateInfo)
  val exception    = Output(Bool())
  val rob_pnr_idx  = Output(UInt(robAddrSz.W))
  val rob_head_idx = Output(UInt(robAddrSz.W))

  val release = Flipped(new DecoupledIO(new TLBundleC(edge.bundle)))

  // Clears prefetching MSHRs
  val force_order  = Output(Bool())
  val ordered     = Input(Bool())

  // debug log fro corefuzzing
  val cf_debug_dcache_enable = Output(Bool())
  val cf_debug_repl_enable   = Output(Bool())   // [REPLUPD] patch 30: 0xbc0 bit 7 (D$ replacement-port log)

  // for corefuzzing - dcache configuration flags
  val cf_dcache_set_conf  = Output(UInt(dcacheParamsWidthCF.W))
  val cf_dcache_way_conf  = Output(UInt(dcacheParamsWidthCF.W))
  val cf_dcache_repl_conf = Output(UInt(dcacheParamsWidthCF.W))
  // [reconf-fix Phase C] L1 invalidate-all walker (CSR 0xbc4, RESTORE-ONLY):
  // write pulse + wdata (bit1 = discard walk), and busy back-channel to core
  // (published as 0xbcd[17]).  Constant-tied in the baseline flavor → pruned.
  val cf_cachectl_wen     = Output(Bool())
  val cf_cachectl_wdata   = Output(UInt(2.W))
  val cf_dcache_wipe_busy = Input(Bool())
  // [reconf-fix 2.10f] 0xbcd[16:12] from the D$: {pending, applied way idx, applied set idx}.
  // Option so the baseline flavor elaborates no port at all.
  val cf_dcache_reconf_status = if (boomParams.enableReconf) Some(Input(UInt(5.W))) else None

  val perf = Input(new Bundle {
    val acquire = Bool()
    val release = Bool()
  })

}

class LSUCoreIO(implicit p: Parameters) extends BoomBundle()(p)
  with CoreFuzzingConstants
{
  val exe = Vec(memWidth, new LSUExeIO)

  val dis_uops    = Flipped(Vec(coreWidth, Valid(new MicroOp)))
  val dis_ldq_idx = Output(Vec(coreWidth, UInt(ldqAddrSz.W)))
  val dis_stq_idx = Output(Vec(coreWidth, UInt(stqAddrSz.W)))

  // corefuzzing
  // Status signals for pipeline draining
  val queues_empty    = Output(Bool()) // Both LDQ and STQ empty
  val no_pending_mem  = Output(Bool()) // No outstanding memory requests
  // Pulse to reset LSQ pointers to 0 on quiesce drain (queues are empty at this point)
  val cf_lsq_quiesce_reset = Input(Bool())
  // [ONETAGATK] r205 U8: the patch-64 close pulse (cf_wclr_atk) is removed; the attacker window shadow range is the
  // twin of cf_wsh_sec below (attacker shadow loads mark the lines they fill).
  val cf_wsh_atk  = if (ENABLE_IFT) Some(Input(new WinShadowInfo)) else None
  // [FILLSEC] r205 (RULES.md A reading 3): the secret window shadow range (core.scala wsh_sec), so a load dispatched
  // before the arming branch resolved but issued after it is an s_prop load for its D$ fill.
  val cf_wsh_sec  = if (ENABLE_IFT) Some(Input(new WinShadowInfo)) else None
  // [F1WSH] lean58 timing F1 (timing-audit-20261023 section 6): per lane, the window-shadow membership of the uop on
  // io.core.exe(w).iresp (cf_wsh_sec / cf_wsh_atk .in(rob_idx)), computed per response candidate (D$ load, AMO,
  // store-to-load forward) and selected by the same when-branches as the iresp uop, so the core's memory write ports
  // no longer run in() (two 8-bit IsOlder carry chains) AFTER the late LSU response select.  Exact: see lsu.scala.
  val cf_resp_wsec = if (ENABLE_IFT) Some(Output(Vec(memWidth, Bool()))) else None
  val cf_resp_watk = if (ENABLE_IFT) Some(Output(Vec(memWidth, Bool()))) else None

  val ldq_full    = Output(Vec(coreWidth, Bool()))
  val stq_full    = Output(Vec(coreWidth, Bool()))
  // [DISPAR] patch 51 (A): pre-stall dispatch valids (rename ren2_mask, = core dis_valids).
  // ldq_full/stq_full/dis_*q_idx are computed from these, slot-parallel, instead of from the
  // serial dis_uops(w).valid (= dis_fire) chain.  Same dispatch decision (proof at the use).
  val dis_valids  = Input(Vec(coreWidth, Bool()))
  // [DISPAR] the OLD fire-based full flags, kept for the IFT LDQ_FULL/STQ_FULL observers only
  // (core dis_stall_was_ldq/stq and their captures): record-identical to before.  Not read by
  // any dispatch decision.
  val ldq_full_obs = Output(Vec(coreWidth, Bool()))
  val stq_full_obs = Output(Vec(coreWidth, Bool()))
  // corefuzzing: op_count, domain, and secret-status of head entry for stall attribution
  val ldq_head_op_count  = Output(UInt(uopIDCounterWidthCF.W))
  val stq_head_op_count  = Output(UInt(uopIDCounterWidthCF.W))
  val ldq_head_domain    = Output(UInt(1.W))   // cf_domain_id of ldq_head entry
  val stq_head_domain    = Output(UInt(1.W))   // cf_domain_id of stq_head entry
  val ldq_head_is_secret = Output(Bool())      // ldq_head had s_acc=1 or s_prop=1
  val stq_head_is_secret = Output(Bool())      // stq_head had s_acc=1 or s_prop=1
  // [MEMORDATK] patch 42 (r116/r117 attacker destination rule): the head's ADDRESS carries
  // attacker taint (data-flow or window kind), i.e. its latency at the head is attacker-chosen.
  // Value taint only, never the sticky/dispatch summary, so a structural edge cannot feed itself.
  val ldq_head_atk       = if (ENABLE_IFT) Some(Output(Bool())) else None
  val stq_head_atk       = if (ENABLE_IFT) Some(Output(Bool())) else None
  val ldq_head_valid     = Output(Bool())      // ldq_head entry is occupied
  val stq_head_valid     = Output(Bool())      // stq_head entry is occupied
  // [HOLFIX 2026-09-08] Occupied is NOT blocking.  INFL_MEM_HOL previously fired whenever
  // a cross-domain op merely SAT at the queue head while a victim dispatched -- and it
  // fired on dis_fire, i.e. exactly when the victim was NOT blocked.  Pure coexistence,
  // no mechanism.  MEASURED consequence: control PC 0x800016c4 was 637/644 "attacker
  // influenced" and every single edge was ty3 MEM_HOL.
  // These say the head has NOT completed, so entries behind it genuinely cannot retire --
  // which is what head-of-line blocking means.
  val ldq_head_blocked   = Output(Bool())      // head occupied AND not yet succeeded
  val stq_head_blocked   = Output(Bool())      // head occupied AND not yet succeeded
  // corefuzzing: how long the CURRENT stq_head entry has occupied the head, in cycles
  // (saturating).  The STQ drains in order from the head, and a load's st_dep_mask bits
  // clear only as stq_head advances (lsu.scala st_dep_mask update), so a store parked at
  // the head genuinely holds younger loads AND younger stores.  This is the head-of-line
  // duration that INFL_MEM_HOL never carried.  Residency only -- the consumer's domain is
  // not known here, so the core applies the cross-domain/secret test at capture.
  val stq_hol_cycles     = Output(UInt(6.W))
  // corefuzzing: TLB-stage FTQ secret updates — fires when s_acc determined at address resolution
  // One port per memWidth; fires even for speculatively-executed (later squashed) memory ops
  val cf_secret_ftq_updates = Output(Vec(memWidth, Valid(UInt(log2Ceil(ftqSz).W))))
  // [MEMORD 2026-09-08] ty=8 MEM_ORDER edge -> ROB, so it survives the order-fail flush.
  val cf_memord_upd = Output(Vec(memWidth, Valid(new CF_MemOrdUpdate)))
  // [RDEDGE] patch 80: reader-side edges (INFL_STL_PARTIAL, INFL_DC_BANKCONF) -> ROB, one registered update per cycle
  val cf_rdedge_upd = if (ENABLE_IFT) Some(Output(Valid(new CF_MemOrdUpdate))) else None
  // Task 3.2 (D14): INFL_DTLB_STATE edge from the TLB stage straight to the ROB row
  // corefuzzing: direct ROB s_acc update at TLB stage — sets cf_secret_access in the ROB entry
  // immediately when the effective address is found to be in the secret range, without waiting
  // for the dcache response.  Enables correct s_acc in [FLUSH] for speculative secret loads.
  val cf_s_acc_rob_upd = Output(Vec(memWidth, Valid(new CF_TlbFlagsUpdate)))   // [FIFO10] + atk / s_prop
  // corefuzzing: preg_secret early update — fires at TLB stage when load hits secret range.
  // Enables in-flight consumers to receive s_prop before the producer commits (avoids needing
  // a fence.i between the secret load and its consumers).  Bit index = pdst of the load.

  // Gap 1 fix (DOC:23): live preg_secret read port — core exposes integer preg_secret so the
  // TLB stage can check whether the load's source address register is already secret-tainted
  // at issue time.  By the time a load reaches AGU, its source has written back (wakeup),
  // so preg_secret[prs1] reflects the full writeback-transitive taint chain P1→Pn.
  // Gap 1 fix (DOC:23): direct ROB s_tx update at TLB stage (mirrors cf_s_acc_rob_upd).
  // Without this, squashed probe loads never show s_tx=1 in bridge records because the
  // pipeline uop's cf_secret_transmission wire is discarded when the instruction is killed.
  val cf_s_tx_rob_upd = Output(Vec(memWidth, Valid(new CF_SAccUpdate)))

  val fp_stdata   = Flipped(Decoupled(new ExeUnitResp(fLen)))

  val commit      = Input(new CommitSignals)
  val commit_load_at_rob_head = Input(Bool())

  // Stores clear busy bit when stdata is received
  // memWidth for int, 1 for fp (to avoid back-pressure fpstdat)
  val clr_bsy         = Output(Vec(memWidth + 1, Valid(UInt(robAddrSz.W))))
  // corefuzzing: accumulated cf_fu_bitmap for the store (dtlb+dcache+stq bits), sent with clr_bsy
  val clr_bsy_cf_bitmap = Output(Vec(memWidth + 1, UInt(numModules.W)))
  // corefuzzing: cf_secret_transmission for stores, sent with clr_bsy
  val clr_bsy_cf_stx    = Output(Vec(memWidth + 1, Bool()))
  // corefuzzing: a store consuming a tainted register must show s_prop in its COMMIT
  // record.  It cannot arrive by the usual route: the ROB's s_prop comes from writeback
  // resps gated `rf_wen && dst_rtype===RT_FIX && ldst_val`, and a store has rf_wen=false
  // -- the same exclusion that hides branch conditions.  MEASURED on both attacks: the
  // final store of each gadget chain (v1 sb x15,-17(x8); v3 sd x15,-24(x8)) showed
  // s_prop=0 while every other chain member was tagged.  clr_bsy is the path stores DO
  // use, so carry it there (mirrors clr_bsy_cf_stx).
  val clr_bsy_cf_sprop  = Output(Vec(memWidth + 1, Bool()))
  // [M7STATK] bundle61: the attacker twin of the store s_prop carried on clr_bsy (cfStAtk: data or address operand
  // attacker taint).  IFT only.
  val clr_bsy_cf_atk    = if (ENABLE_IFT) Some(Output(Vec(memWidth + 1, Bool()))) else None

  // Speculatively safe load (barring memory ordering failure)
  val clr_unsafe      = Output(Vec(memWidth, Valid(UInt(robAddrSz.W))))

  // Tell the DCache to clear prefetches/speculating misses
  val fence_dmem   = Input(Bool())

  // Speculatively tell the IQs that we'll get load data back next cycle
  // [WAKESEC] carries the load's TLB-time secret beside the pdst (Phase 0 finding (b))
  val spec_ld_wakeup = Output(Vec(memWidth, Valid(new SpecLdWakeup(maxPregSz))))
  // Tell the IQs that the load we speculated last cycle was misspeculated
  val ld_miss      = Output(Bool())

  val brupdate       = Input(new BrUpdateInfo)
  val rob_pnr_idx  = Input(UInt(robAddrSz.W))
  val rob_head_idx = Input(UInt(robAddrSz.W))
  val exception    = Input(Bool())

  val fencei_rdy  = Output(Bool())

  val lxcpt       = Output(Valid(new Exception))

  val tsc_reg     = Input(UInt())

  // Queue state for corefuzzing quiescing
  // val queues_empty = Output(Bool()) // Indicates both LDQ and STQ are empty
  // val no_pending_mem = Output(Bool()) // No outstanding D$ requests
  
  
  // flag to print debug log
  val cf_debug_lsu_enable = Input(Bool())

  // for corefuzzing - dcache configuration flags
  val cf_debug_dcache_enable = Input(Bool())
  val cf_debug_repl_enable   = Input(Bool())    // [REPLUPD] patch 30: 0xbc0 bit 7
  val cf_dcache_set_conf  = Input(UInt(dcacheParamsWidthCF.W))
  val cf_dcache_way_conf  = Input(UInt(dcacheParamsWidthCF.W))
  val cf_dcache_repl_conf = Input(UInt(dcacheParamsWidthCF.W))
  // [reconf-fix Phase C] 0xbc4 invalidate walker trigger + 0xbcd[17] busy
  val cf_cachectl_wen     = Input(Bool())
  val cf_cachectl_wdata   = Input(UInt(2.W))
  val cf_dcache_wipe_busy = Output(Bool())
  // [reconf-fix 2.10f] 0xbcd[16:12] pass-through (Option: no port in the baseline flavor).
  val cf_dcache_reconf_status = if (boomParams.enableReconf) Some(Output(UInt(5.W))) else None

  val perf        = Output(new Bundle {
    val acquire = Bool()
    val release = Bool()
    val tlbMiss = Bool()
  })

  // 3-bit indices into ldQueueEntryOptions / stQueueEntryOptions for runtime reconfiguration
  val cf_ldq_idx = Input(UInt(3.W))
  val cf_stq_idx = Input(UInt(3.W))

  // corefuzzing: secret address range from CSRs for cf_secret_access detection
  val cf_secret_start_addr = Input(UInt(coreMaxAddrBits.W))
  val cf_secret_end_addr   = Input(UInt(coreMaxAddrBits.W))
  // [DBGFREEZE] patch 85 (b): the hart is in debug mode (core.scala cf_dbg, a flop); no s_acc then.  IFT only.
  val cf_dbg               = if (ENABLE_IFT) Some(Input(Bool())) else None
}

class LSUIO(implicit p: Parameters, edge: TLEdgeOut) extends BoomBundle()(p)
{
  val ptw   = new rocket.TLBPTWIO
  val core  = new LSUCoreIO
  val dmem  = new LSUDMemIO

  val hellacache = Flipped(new freechips.rocketchip.rocket.HellaCacheIO)
}

class LDQEntry(implicit p: Parameters) extends BoomBundle()(p)
    with HasBoomUOP
    with CoreFuzzingConstants
{
  // [NACKRES 2026-10-13, series-r step 16, owner ruling 111] the D$ nacked this load while a
  // valid MSHR held a secret-address miss (io.dmem.cf_asec_busy); PC[13:2] of that access.
  // Sent with the load's re-execution request (cf_res_v/pc) so the D$ attaches CACHE_EVICTION
  // {atk=0, sec=1, PC} to its response (hit or miss); cleared by that load's response and on
  // LDQ allocation (covers squash and reuse).  48 x 13 FF.
  // [R124GATE] patch 26 (ruling 124): None without ENABLE_IFT (48 x 13 LDQ flops in IFT-off).
  val cf_nack_res_v       = if (ENABLE_IFT) Some(Bool()) else None
  val cf_nack_res_pc      = if (ENABLE_IFT) Some(UInt(inflPcBitsCF.W)) else None
  // [NACKATK] patch 44 (r111 extended, r173 (3)): the attacker twin.  A VICTIM load was nacked
  // (MSHR/WB-caused) while a valid MSHR held an attacker-influenced request.  cf_nack_res_v now
  // means "secret nack recorded"; the request carries {v = res_v || res_atk, atk = res_atk,
  // esec = res_v}, so the edge is {0,1}, {1,0} or {1,1}; one PC, secret-first.  Same clears.
  // 1 FF per LDQ entry.
  val cf_nack_res_atk     = if (ENABLE_IFT) Some(Bool()) else None
  val addr                = Valid(UInt(coreMaxAddrBits.W))
  val addr_is_virtual     = Bool() // Virtual address, we got a TLB miss
  val addr_is_uncacheable = Bool() // Uncacheable, wait until head of ROB to execute
  // corefuzzing: taint of the ADDRESS operand (rs1) only -- NOT the uop's aggregate
  // cf_secret_propagation, which also accumulates forwarding and observability taint.
  val addr_is_secret      = if (ENABLE_IFT) Some(Bool()) else None
  // corefuzzing: ATTACKER taint of the address operand.  Same rule, same source.
  val addr_is_atk         = if (ENABLE_IFT) Some(Bool()) else None

  val executed            = Bool() // load sent to memory, reset by NACKs
  val succeeded           = Bool()
  val order_fail          = Bool()
  val observed            = Bool()

  val st_dep_mask         = UInt(numStqEntries.W) // list of stores older than us
  val youngest_stq_idx    = UInt(stqAddrSz.W) // index of the oldest store younger than us

  val forward_std_val     = Bool()
  val forward_stq_idx     = UInt(stqAddrSz.W) // Which store did we get the store-load forward from?

  val debug_wb_data       = UInt(xLen.W)
}
class STQEntry(implicit p: Parameters) extends BoomBundle()(p)
   with HasBoomUOP
{
  val addr                = Valid(UInt(coreMaxAddrBits.W))
  val addr_is_virtual     = Bool() // Virtual address, we got a TLB miss
  // corefuzzing: taint of the ADDRESS operand (rs1) only.  See LDQEntry.
  val addr_is_secret      = if (ENABLE_IFT) Some(Bool()) else None
  val addr_is_atk         = if (ENABLE_IFT) Some(Bool()) else None
  // corefuzzing: taint of the store's DATA operand (rs2) only.  Kept separate from
  // the uop's aggregate cf_secret_propagation, which also carries fetch/observability
  // taint and says nothing about the VALUE being stored.
  val data_is_secret      = if (ENABLE_IFT) Some(Bool()) else None
  // corefuzzing: ATTACKER taint of the store's DATA operand (rs2).
  val data_is_atk         = if (ENABLE_IFT) Some(Bool()) else None
  // [WINMEM] step 21 (owner r139, F2): WINDOW taint of the address (rs1) and data (rs2)
  // operands, kept apart from the data-taint bits above (step 20 collapsed them into those).
  // With the uop's window membership (cf_win_*) they mark the stored doubleword WINDOW-kind in
  // the D$ store metadata and give a forwarded load window taint; a reload never re-arms.
  // Stamped with the store's dispatch epoch (uop.cf_wep_*).  4 FF per entry.
  // [R124GATE2] patch 27 (r124/B51): None without ENABLE_IFT (writers foreach, readers getOrElse).
  // [ONETAG] r205 (RULES.md A reading 2, one secret tag): the secret window kinds are removed (2 FF per STQ entry).
  // A window-derived operand is already secret DATA (the regfile's one tag), so addr_is_secret / data_is_secret
  // carry it; the store's membership / record goes to its D$ doubleword through the commit-time record bit below.
  val addr_is_wsec: Option[Bool] = None
  val addr_is_watk: Option[Bool] = None   // [ONETAGATK] r205 U8: no attacker window kind (-1 FF per STQ entry)
  val data_is_wsec: Option[Bool] = None
  val data_is_watk: Option[Bool] = None   // [ONETAGATK] (-1 FF per STQ entry)
  // [SACCU] owner 2026-10-21 (RULES.md A, one s_acc definition): addr_in_secret (1 FF per entry) is retired.
  // "Was this store's ADDRESS inside the secret range" is now the entry uop's cf_secret_access, which the
  // TLB stage sets from is_secret_lsq for every LSQ op (plain stores included); its readers moved to that bit.
  // [MEMORDATK] patch 42: the store's DISPATCH-time attacker summary (dis_uops.cf_attacker_influence:
  // attacker packet / fetch edge, dispatch edge, attacker-window membership).  The entry uop's own
  // cf_attacker_influence stays zeroed (Change 8: the commit request and the AMO response read it).
  // Read only by the MEM_ORDER edge (lsu [MEMORDATK] below).  1 FF per entry.
  val cf_dis_atk          = if (ENABLE_IFT) Some(Bool()) else None
  val data                = Valid(UInt(xLen.W))

  val committed           = Bool() // committed by ROB
  val succeeded           = Bool() // D$ has ack'd this, we don't need to maintain this anymore

  val debug_wb_data       = UInt(xLen.W)
}

class LSU(implicit p: Parameters, edge: TLEdgeOut) extends BoomModule()(p)
  with rocket.HasL1HellaCacheParameters
  with CoreFuzzingConstants
{
  val io = IO(new LSUIO)
  io.hellacache := DontCare
  // Gap 1 fix: default drivers for new IOs — overridden inside the TLB for-loop below.
  io.core.cf_s_tx_rob_upd.foreach { u => u.valid := false.B; u.bits := DontCare }


  // Runtime LDQ/STQ size selection via CSR index
  val ldqOptionsVec = VecInit(ldQueueEntryOptions.map(_.U))
  val stqOptionsVec = VecInit(stQueueEntryOptions.map(_.U))
  // [reconf-fix 2026-08-13] DEFERRED RESIZE.
  // Previously cf_ldq_active/cf_stq_active tracked the CSR combinationally, so a write to
  // 0x7c4 shrank the queue *underneath live entries*. Any pointer already >= the new size is
  // then outside the active window, and WrapInc's non-power-of-2 path (wrap only on the exact
  // equality value === n-1) can never bring it back -> the head never reaches the ROB's
  // expected slot -> permanent deadlock. Observed on FPGA: cfg09/cfg11 (LDQ/STQ 48->24) commit
  // ~11.5k instructions after the write and then stop forever. Power-of-2 sizes only escaped
  // because their masking path re-ranges a stray pointer by accident.
  // The CSR is now a REQUEST: the queue adopts the new geometry only while fully drained, so
  // no entry or pointer can be stranded. Registered output also takes the option mux off the
  // LSQ's critical comparator path (see the update in the "adopt pending geometry" block).
  val cf_ldq_idx_applied = RegInit(0.U(io.core.cf_ldq_idx.getWidth.W))
  val cf_stq_idx_applied = RegInit(0.U(io.core.cf_stq_idx.getWidth.W))
  val cf_ldq_active = ldqOptionsVec(cf_ldq_idx_applied)
  val cf_stq_active = stqOptionsVec(cf_stq_idx_applied)

  val ldq = Reg(Vec(numLdqEntries, Valid(new LDQEntry)))
  val stq = Reg(Vec(numStqEntries, Valid(new STQEntry)))



  val ldq_head         = Reg(UInt(ldqAddrSz.W))
  val ldq_tail         = Reg(UInt(ldqAddrSz.W))
  val stq_head         = Reg(UInt(stqAddrSz.W)) // point to next store to clear from STQ (i.e., send to memory)
  val stq_tail         = Reg(UInt(stqAddrSz.W))
  val stq_commit_head  = Reg(UInt(stqAddrSz.W)) // point to next store to commit
  val stq_execute_head = Reg(UInt(stqAddrSz.W)) // point to next store to execute


  // If we got a mispredict, the tail will be misaligned for 1 extra cycle
  assert (io.core.brupdate.b2.mispredict ||
          stq(stq_execute_head).valid ||
          stq_head === stq_execute_head ||
          stq_tail === stq_execute_head,
            "stq_execute_head got off track.")

  val h_ready :: h_s1 :: h_s2 :: h_s2_nack :: h_wait :: h_replay :: h_dead :: Nil = Enum(7)
  // s1 : do TLB, if success and not killed, fire request go to h_s2
  //      store s1_data to register
  //      if tlb miss, go to s2_nack
  //      if don't get TLB, go to s2_nack
  //      store tlb xcpt
  // s2 : If kill, go to dead
  //      If tlb xcpt, send tlb xcpt, go to dead
  // s2_nack : send nack, go to dead
  // wait : wait for response, if nack, go to replay
  // replay : refire request, use already translated address
  // dead : wait for response, ignore it
  val hella_state           = RegInit(h_ready)
  val hella_req             = Reg(new rocket.HellaCacheReq)
  val hella_data            = Reg(new rocket.HellaCacheWriteData)
  val hella_paddr           = Reg(UInt(paddrBits.W))
  val hella_xcpt            = Reg(new rocket.HellaCacheExceptions)


  val dtlb = Module(new NBDTLB(
    instruction = false, lgMaxSize = log2Ceil(coreDataBytes), rocket.TLBConfig(dcacheParams.nTLBSets, dcacheParams.nTLBWays)))

  io.ptw <> dtlb.io.ptw
  io.core.perf.tlbMiss := io.ptw.req.fire
  io.core.perf.acquire := io.dmem.perf.acquire
  io.core.perf.release := io.dmem.perf.release



  val clear_store     = WireInit(false.B)
  val live_store_mask = RegInit(0.U(numStqEntries.W))
  var next_live_store_mask = Mux(clear_store, live_store_mask & ~(1.U << stq_head),
                                              live_store_mask)


  def widthMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Enqueue new entries
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // This is a newer store than existing loads, so clear the bit in all the store dependency masks
  for (i <- 0 until numLdqEntries)
  {
    when (clear_store)
    {
      ldq(i).bits.st_dep_mask := ldq(i).bits.st_dep_mask & ~(1.U << stq_head)
    }
  }

  // Decode stage
  // [DISPAR] patch 51 (A, path47-path39-analysis.md section 4.A): slot-parallel LDQ/STQ
  // allocation.  Before: idx(w+1) = dis_fire(w) && uses(w) ? WrapInc(idx(w)) : idx(w), and
  // full(w) = WrapInc(idx(w)) === head with full(w) inside dis_hazards(w) -> dis_fire(w): a
  // 4-slot serial chain through the core's dispatch hazards (about 9 LUT levels per slot).
  // After: the k-step successors c_k of the tail (k = 0..coreWidth) are computed in parallel
  // from REGISTERS (tail, head, the applied size index), full_k = (c_{k+1} === head), and slot
  // w selects index k = PopCount of the PRE-STALL load/store valids of the older slots
  // (io.core.dis_valids = rename r_valid, uop bits from rename registers).
  // Equivalence (dispatch decision, entry indices, tails): dis_stalls is a prefix OR of
  // dis_hazards and an invalid slot has no hazard.  If slot w fires, no slot 0..w has a hazard,
  // so every valid older slot fires: the pre-stall count equals the old fire-based count, and
  // idx(w)/full(w) equal the old values.  If an older slot has a hazard, slot w and every
  // younger slot are stalled whatever full(w) is, and dis_ready (= !dis_stalls.last) is low
  // anyway.  Induction over w: dis_fire, dis_ready, every enqueued entry's index, ldq_tail,
  // stq_tail, st_dep_mask, youngest_stq_idx and the [lsu] asserts are unchanged.  Only
  // dis_*q_idx / *q_full of a NON-firing slot can differ; every consumer of those is gated by
  // that slot's fire (ROB/IQ/LSU enqueue), except the IFT observers, which read the separate
  // fire-based *_full_obs below (bit-identical to the old full).
  // c_k closed form: WrapInc(v, n) = (v+1 truncated) >= n ? 0 : v+1, which is (v < n-1) ? v+1 : 0
  // for every v (v = 2^W-1 truncates to 0 in both; v >= n self-heals to 0 in both, the
  // [reconf-fix 2026-08-13] property).  Iterated k >= 1 times: v >= n-1 -> (k-1) mod n, else
  // (v+k) mod n.  With n >= coreWidth (options 40..8 here) both are one conditional subtract,
  // evaluated per size option against constants and selected by the 3-bit applied index.
  // The option index is clamped (cfClampIdx) and resets to 0, so out-of-range indices are
  // unreachable, as for ldqOptionsVec(idx) before.
  def cfWrapSucc(v: UInt, opts: Seq[Int], sel: UInt, n_active: UInt): Seq[UInt] = {
    val wd = v.getWidth
    if (opts.min >= coreWidth) {
      v +: (1 to coreWidth).map { k =>
        VecInit(opts.map { o =>
          val vk = v +& k.U
          Mux(v >= (o - 1).U, ((k - 1) % o).U(wd.W),
            Mux(vk >= o.U, (vk - o.U)(wd-1, 0), vk(wd-1, 0)))
        })(sel)
      }
    } else {
      // generic fallback (small queue options): the old iterated WrapInc, from registers only
      (0 until coreWidth).scanLeft(v)((c, _) => WrapInc(c, n_active))
    }
  }
  val ldq_succ = cfWrapSucc(ldq_tail, ldQueueEntryOptions, cf_ldq_idx_applied, cf_ldq_active)
  val stq_succ = cfWrapSucc(stq_tail, stQueueEntryOptions, cf_stq_idx_applied, cf_stq_active)
  val ldq_full_k = (0 until coreWidth).map(k => ldq_succ(k + 1) === ldq_head)
  val stq_full_k = (0 until coreWidth).map(k => stq_succ(k + 1) === stq_head)
  val dis_pre_ld = (0 until coreWidth).map(w => io.core.dis_valids(w) && io.core.dis_uops(w).bits.uses_ldq && !io.core.dis_uops(w).bits.exception)
  val dis_pre_st = (0 until coreWidth).map(w => io.core.dis_valids(w) && io.core.dis_uops(w).bits.uses_stq && !io.core.dis_uops(w).bits.exception)
  val dis_ld_vals = (0 until coreWidth).map(w => io.core.dis_uops(w).valid && io.core.dis_uops(w).bits.uses_ldq && !io.core.dis_uops(w).bits.exception)
  val dis_st_vals = (0 until coreWidth).map(w => io.core.dis_uops(w).valid && io.core.dis_uops(w).bits.uses_stq && !io.core.dis_uops(w).bits.exception)
  // select element (count of set bits in older(0..w-1)) of xs(0..w)
  def cfSelByCount[T <: Data](xs: Seq[T], older: Seq[Bool]): T =
    if (older.isEmpty) xs.head else VecInit(xs.take(older.length + 1))(PopCount(older))

  var st_enq_idx = stq_tail   // per-slot STQ index (reassigned below); only the live-store-mask chain stays serial

  val stq_nonempty = (0 until numStqEntries).map{ i => stq(i).valid }.reduce(_||_) =/= 0.U

  for (w <- 0 until coreWidth)
  {
    val ld_enq_idx = cfSelByCount(ldq_succ, dis_pre_ld.take(w))
    st_enq_idx     = cfSelByCount(stq_succ, dis_pre_st.take(w))
    io.core.ldq_full(w)    := cfSelByCount(ldq_full_k, dis_pre_ld.take(w))
    io.core.dis_ldq_idx(w) := ld_enq_idx
    io.core.stq_full(w)    := cfSelByCount(stq_full_k, dis_pre_st.take(w))
    io.core.dis_stq_idx(w) := st_enq_idx
    // [DISPAR] old fire-based full (IFT observers only)
    io.core.ldq_full_obs(w) := cfSelByCount(ldq_full_k, dis_ld_vals.take(w))
    io.core.stq_full_obs(w) := cfSelByCount(stq_full_k, dis_st_vals.take(w))

    val dis_ld_val = dis_ld_vals(w)
    val dis_st_val = dis_st_vals(w)
    when (dis_ld_val)
    {
      ldq(ld_enq_idx).valid                := true.B
      ldq(ld_enq_idx).bits.uop             := io.core.dis_uops(w).bits
      // corefuzzing: stamp the LDQ unit bit into cf_fu_bitmap at LDQ enqueue ([RECV3] UNIT_LDQ, design 3.2 bit 2)
      ldq(ld_enq_idx).bits.uop.cf_fu_bitmap.foreach(_ := io.core.dis_uops(w).bits.cf_fu_bitmap.getOrElse(0.U(numModules.W)) | (1 << UNIT_LDQ).U(numModules.W))
      // IFT LUT optimization (Change 8): zero cf_* fields not read by LSU logic.
      // PRESERVED: cf_op_count_id, cf_domain_id (head reads), cf_secret_access,
      //   cf_secret_propagation (head + TLB update), cf_secret_transmission (TLB update),
      //   cf_fu_bitmap (set above, merged at TLB+commit).  Task 3.2: the influencer list,
      //   cf_infl_dropped and cf_infl_oc_aliased are zeroed too (below).
      // rob_uop holds the dispatch-time IFT state independently.
      // [ATKSEED] r205-10b (t206, 2026-10-20): cf_attacker_influence is now PRESERVED (the dispatch record value,
      // window membership and promotions included: the same io.core.dis_uops bit the STQ keeps as cf_dis_atk), the
      // twin of cf_secret_propagation.  Readers that assumed it: the load response uop (ldq_base_cf -> the r205-8
      // [ONETAGATK] writeback tag of the load's result), cfLdAtk on the wakeup / retry D$ request (fill marks) and
      // the r205-5 [RECEDGE] LDQ head attacker bit.  Before this they read constant 0 here.  Cost: 1 FF per LDQ entry
      // (numLdqEntries) and its bit on the existing entry read muxes.  Levels: the D input is the same dispatch bit
      // the STQ's cf_dis_atk flop takes: 0 added.
      ldq(ld_enq_idx).bits.uop.cf_spec_branch_is_secret.foreach(_ := false.B)
      // Task 3.2 (D16): nothing in the LSU reads a uop list any more (the D$ and STL edges
      // travel on the response, MEM_ORDER and DTLB go to the ROB as messages), so the LDQ
      // copy is zeroed and firtool prunes it.  Task 3.5 deletes these lines with the fields.
      ldq(ld_enq_idx).bits.youngest_stq_idx  := st_enq_idx
      ldq(ld_enq_idx).bits.st_dep_mask     := next_live_store_mask

      ldq(ld_enq_idx).bits.addr.valid      := false.B
      ldq(ld_enq_idx).bits.addr_is_secret.foreach(_ := false.B)
      ldq(ld_enq_idx).bits.addr_is_atk.foreach(_ := false.B)
      ldq(ld_enq_idx).bits.cf_nack_res_v.foreach(_ := false.B)   // [NACKRES]
      ldq(ld_enq_idx).bits.cf_nack_res_atk.foreach(_ := false.B)   // [NACKATK]
      ldq(ld_enq_idx).bits.executed        := false.B
      ldq(ld_enq_idx).bits.succeeded       := false.B
      ldq(ld_enq_idx).bits.order_fail      := false.B
      ldq(ld_enq_idx).bits.observed        := false.B
      ldq(ld_enq_idx).bits.forward_std_val := false.B

      assert (ld_enq_idx === io.core.dis_uops(w).bits.ldq_idx, "[lsu] mismatch enq load tag.")
      assert (!ldq(ld_enq_idx).valid, "[lsu] Enqueuing uop is overwriting ldq entries")
    }
      .elsewhen (dis_st_val)
    {
      stq(st_enq_idx).valid           := true.B
      stq(st_enq_idx).bits.uop        := io.core.dis_uops(w).bits
      // corefuzzing: stamp the STQ and D$ unit bits at STQ enqueue for non-fence stores ([RECV3] SP-09, design 3.3 item 5:
      // STQ | DC; the DTLB bit is out of scope).  Every committed non-fence store reaches the D$ at store_commit; the
      // pre-mark lets the ROB see it at commit time via clr_bsy_cf_bitmap (the store's own D$ event is a STATE R group).
      val store_bitmap_extra = (1 << UNIT_STQ).U(numModules.W) |
        Mux(!io.core.dis_uops(w).bits.is_fence, (1 << UNIT_DC).U(numModules.W), 0.U(numModules.W))
      stq(st_enq_idx).bits.uop.cf_fu_bitmap.foreach(_ := io.core.dis_uops(w).bits.cf_fu_bitmap.getOrElse(0.U(numModules.W)) | store_bitmap_extra)
      // IFT LUT optimization (Change 8): same zeroing as LDQ above.
      stq(st_enq_idx).bits.uop.cf_attacker_influence.foreach(_ := false.B)
      stq(st_enq_idx).bits.uop.cf_spec_branch_is_secret.foreach(_ := false.B)
      // Task 3.2 (D16): same as the LDQ above
      stq(st_enq_idx).bits.addr.valid := false.B
      stq(st_enq_idx).bits.addr_is_secret.foreach(_ := false.B)
      stq(st_enq_idx).bits.addr_is_atk.foreach(_ := false.B)
      stq(st_enq_idx).bits.data_is_secret.foreach(_ := false.B)
      stq(st_enq_idx).bits.data_is_atk.foreach(_ := false.B)
      stq(st_enq_idx).bits.addr_is_wsec.foreach(_ := false.B)   // [WINMEM]
      stq(st_enq_idx).bits.addr_is_watk.foreach(_ := false.B)
      stq(st_enq_idx).bits.data_is_wsec.foreach(_ := false.B)
      stq(st_enq_idx).bits.data_is_watk.foreach(_ := false.B)
      stq(st_enq_idx).bits.cf_dis_atk.foreach(_ := io.core.dis_uops(w).bits.cf_attacker_influence.getOrElse(false.B))   // [MEMORDATK]
      stq(st_enq_idx).bits.data.valid := false.B
      stq(st_enq_idx).bits.committed  := false.B
      stq(st_enq_idx).bits.succeeded  := false.B

      assert (st_enq_idx === io.core.dis_uops(w).bits.stq_idx, "[lsu] mismatch enq store tag.")
      assert (!stq(st_enq_idx).valid, "[lsu] Enqueuing uop is overwriting stq entries")
    }

    next_live_store_mask = Mux(dis_st_val, next_live_store_mask | (1.U << st_enq_idx),
                                           next_live_store_mask)

    assert(!(dis_ld_val && dis_st_val), "A UOP is trying to go into both the LDQ and the STQ")
  }

  // [DISPAR] tails: c_K with K = the number of FIRING loads/stores (= the old iterated value);
  // dis_fire arrives late into a 5:1 select that ends at the tail register.
  val ld_enq_idx_final = VecInit(ldq_succ)(PopCount(dis_ld_vals))
  st_enq_idx = VecInit(stq_succ)(PopCount(dis_st_vals))
  ldq_tail := ld_enq_idx_final
  stq_tail := st_enq_idx

  // corefuzzing: expose head entry op_count, domain, and secret-status for stall attribution
  // [HEADREG] (Phase 4 Task 4.1, spec 5.3) all ten head-status outputs are registered under
  // ENABLE_IFT (IFT off: plain wires, Verilog unchanged).  Before: ldq_head/stq_head ->
  // 40:1 entry mux -> valid/secret AND-OR -> LSU->core route -> the dispatch consumers
  // (stall-cycle RegEnable captures, combinational MEM_HOL at dis_fire, stq_head_hostile).
  // After: the consumers start at a flop beside the LSU port.  The core sees the head as it
  // was one cycle earlier (dependency map: one cycle of tolerance; a blocking head is still
  // the same entry while the stall persists).  stq_hol_cycles_r below is already one cycle
  // behind stq_head (it resets on the cycle after the head moves), so the registered
  // stq_head_* now line up with it.  Cost: 2x(16+1+1+1+1) = 40 FF, 0 LUT.
  def headReg[T <: Data](x: T): T = if (ENABLE_IFT) RegNext(x, 0.U.asTypeOf(x)) else x
  io.core.ldq_head_op_count  := headReg(Mux(ldq(ldq_head).valid, ldq(ldq_head).bits.uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)), 0.U))
  io.core.stq_head_op_count  := headReg(Mux(stq(stq_head).valid, stq(stq_head).bits.uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)), 0.U))
  io.core.ldq_head_domain    := headReg(Mux(ldq(ldq_head).valid, ldq(ldq_head).bits.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)), 0.U))
  io.core.stq_head_domain    := headReg(Mux(stq(stq_head).valid, stq(stq_head).bits.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)), 0.U))
  // [LDQRESP] stall-edge audit D1: + addr_is_secret, the [SECDEST] leaf of the load's record s_prop (its writeback
  // secret, RULES D1 l.16; the [L9LDSEC] MEM_ORDER searcher label already reads it).  One more leaf on the same
  // head-entry mux into the existing headReg flop: 0 FF, 0 levels.  IFT off: getOrElse(false) (unchanged).
  io.core.ldq_head_is_secret := headReg(Mux(ldq(ldq_head).valid,
    ldq(ldq_head).bits.uop.cf_secret_access.getOrElse(false.B) || ldq(ldq_head).bits.uop.cf_secret_propagation.getOrElse(false.B) ||
    ldq(ldq_head).bits.addr_is_secret.getOrElse(false.B), false.B))
  // [STSACC] patch 52 / [SACCU] 2026-10-21: the store's s_acc is its entry uop's cf_secret_access, now set at the
  // TLB stage for every in-range LSQ op (was the separate addr_in_secret; same value, same entry mux, 0 levels).
  io.core.stq_head_is_secret := headReg(Mux(stq(stq_head).valid,
    stq(stq_head).bits.uop.cf_secret_access.getOrElse(false.B) || stq(stq_head).bits.uop.cf_secret_propagation.getOrElse(false.B), false.B))
  // [MEMORDATK] patch 42: address attacker taint of the heads (2 FF; the same entry mux as the bits above)
  // [RECEDGE] r205 (RULES.md A reading 4; owner option "Record summary for both"): the attacker head label is the
  // head's attacker RECORD summary known here (the entry uop's cf_attacker_influence: dispatch seed, window,
  // promotions; the STQ's dispatch summary cf_dis_atk) as well as its address taint, exactly as the secret head
  // bit uses the record s_acc || s_prop: a uop stalled behind it gets atk, and so do the uops stalled behind that
  // one (chains, as on the secret side).  Two more leaves on the same head-entry mux, into the existing headReg
  // flop: 0 added levels, 0 FF.
  io.core.ldq_head_atk.foreach(_ := headReg(ldq(ldq_head).valid &&
    (ldq(ldq_head).bits.addr_is_atk.getOrElse(false.B) || ldq(ldq_head).bits.uop.cf_opnd_watk.getOrElse(false.B) ||
     ldq(ldq_head).bits.uop.cf_attacker_influence.getOrElse(false.B))))
  io.core.stq_head_atk.foreach(_ := headReg(stq(stq_head).valid &&
    (stq(stq_head).bits.addr_is_atk.getOrElse(false.B) || stq(stq_head).bits.addr_is_watk.getOrElse(false.B) ||
     stq(stq_head).bits.cf_dis_atk.getOrElse(false.B) || stq(stq_head).bits.uop.cf_attacker_influence.getOrElse(false.B) ||
     stq(stq_head).bits.data_is_atk.getOrElse(false.B))))   // [STQDATK] stall-edge audit D2: the store's record (cfStAtk) has it;
  // one more leaf on the per-entry OR (6 inputs: one LUT) ahead of the existing head mux -> headReg flop: 0 FF, 0 levels
  io.core.ldq_head_valid     := headReg(ldq(ldq_head).valid)
  io.core.stq_head_valid     := headReg(stq(stq_head).valid)
  // [HOLFIX] `succeeded` = the D$ has acked; until then the head holds the line.
  io.core.ldq_head_blocked   := headReg(ldq(ldq_head).valid && !ldq(ldq_head).bits.succeeded)
  io.core.stq_head_blocked   := headReg(stq(stq_head).valid && !stq(stq_head).bits.succeeded)
  // corefuzzing: head-of-line residency counter.  ONE counter for the whole LSU -- it
  // tracks the head slot, not per-entry -- so this is 6 flops plus a compare, not
  // numStqEntries counters.  Reset when the head advances; saturate rather than wrap so
  // a long block reads as ">32" after Log2Bucket instead of aliasing to a small value.
  val stq_hol_cycles_r = RegInit(0.U(6.W))
  val stq_head_prev    = RegNext(stq_head)
  when (stq_head =/= stq_head_prev) {
    stq_hol_cycles_r := 0.U
  } .elsewhen (stq(stq_head).valid) {
    stq_hol_cycles_r := Mux(stq_hol_cycles_r === 63.U, 63.U, stq_hol_cycles_r + 1.U)
  }
  io.core.stq_hol_cycles     := stq_hol_cycles_r

  io.dmem.force_order   := io.core.fence_dmem
  io.core.fencei_rdy    := !stq_nonempty && io.dmem.ordered


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Execute stage (access TLB, send requests to Memory)
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // We can only report 1 exception per cycle.
  // Just be sure to report the youngest one
  val mem_xcpt_valid  = Wire(Bool())
  val mem_xcpt_cause  = Wire(UInt())
  val mem_xcpt_uop    = Wire(new MicroOp)
  val mem_xcpt_vaddr  = Wire(UInt())


  //---------------------------------------
  // Can-fire logic and wakeup/retry select
  //
  // First we determine what operations are waiting to execute.
  // These are the "can_fire"/"will_fire" signals

  val will_fire_load_incoming  = Wire(Vec(memWidth, Bool()))
  val will_fire_stad_incoming  = Wire(Vec(memWidth, Bool()))
  val will_fire_sta_incoming   = Wire(Vec(memWidth, Bool()))
  val will_fire_std_incoming   = Wire(Vec(memWidth, Bool()))
  val will_fire_sfence         = Wire(Vec(memWidth, Bool()))
  val will_fire_hella_incoming = Wire(Vec(memWidth, Bool()))
  val will_fire_hella_wakeup   = Wire(Vec(memWidth, Bool()))
  val will_fire_release        = Wire(Vec(memWidth, Bool()))
  val will_fire_load_retry     = Wire(Vec(memWidth, Bool()))
  val will_fire_sta_retry      = Wire(Vec(memWidth, Bool()))
  val will_fire_store_commit   = Wire(Vec(memWidth, Bool()))
  val will_fire_load_wakeup    = Wire(Vec(memWidth, Bool()))

  val exe_req = WireInit(VecInit(io.core.exe.map(_.req)))
  // Sfence goes through all pipes
  for (i <- 0 until memWidth) {
    when (io.core.exe(i).req.bits.sfence.valid) {
      exe_req := VecInit(Seq.fill(memWidth) { io.core.exe(i).req })
    }
  }

  // -------------------------------
  // Assorted signals for scheduling

  // Don't wakeup a load if we just sent it last cycle or two cycles ago
  // The block_load_mask may be wrong, but the executing_load mask must be accurate
  val block_load_mask    = WireInit(VecInit((0 until numLdqEntries).map(x=>false.B)))
  val p1_block_load_mask = RegNext(block_load_mask)
  val p2_block_load_mask = RegNext(p1_block_load_mask)

 // Prioritize emptying the store queue when it is almost full
  val stq_almost_full = RegNext(WrapInc(WrapInc(st_enq_idx, cf_stq_active), cf_stq_active) === stq_head ||
                                WrapInc(st_enq_idx, cf_stq_active) === stq_head)

  // The store at the commit head needs the DCache to appear ordered
  // Delay firing load wakeups and retries now
  val store_needs_order = WireInit(false.B)

  val ldq_incoming_idx = widthMap(i => exe_req(i).bits.uop.ldq_idx)
  val ldq_incoming_e   = widthMap(i => ldq(ldq_incoming_idx(i)))

  val stq_incoming_idx = widthMap(i => exe_req(i).bits.uop.stq_idx)
  val stq_incoming_e   = widthMap(i => stq(stq_incoming_idx(i)))

  val ldq_retry_idx = RegNext(AgePriorityEncoder((0 until numLdqEntries).map(i => {
    val e = ldq(i).bits
    val block = block_load_mask(i) || p1_block_load_mask(i)
    e.addr.valid && e.addr_is_virtual && !block
  }), ldq_head))
  val ldq_retry_e            = ldq(ldq_retry_idx)

  val stq_retry_idx = RegNext(AgePriorityEncoder((0 until numStqEntries).map(i => {
    val e = stq(i).bits
    e.addr.valid && e.addr_is_virtual
  }), stq_commit_head))
  val stq_retry_e   = stq(stq_retry_idx)

  val stq_commit_e  = stq(stq_execute_head)

  val ldq_wakeup_idx = RegNext(AgePriorityEncoder((0 until numLdqEntries).map(i=> {
    val e = ldq(i).bits
    val block = block_load_mask(i) || p1_block_load_mask(i)
    e.addr.valid && !e.executed && !e.succeeded && !e.addr_is_virtual && !block
  }), ldq_head))
  val ldq_wakeup_e   = ldq(ldq_wakeup_idx)

  // -----------------------
  // Determine what can fire

  // Can we fire a incoming load
  val can_fire_load_incoming = widthMap(w => exe_req(w).valid && exe_req(w).bits.uop.ctrl.is_load)

  // Can we fire an incoming store addrgen + store datagen
  val can_fire_stad_incoming = widthMap(w => exe_req(w).valid && exe_req(w).bits.uop.ctrl.is_sta
                                                              && exe_req(w).bits.uop.ctrl.is_std)

  // Can we fire an incoming store addrgen
  val can_fire_sta_incoming  = widthMap(w => exe_req(w).valid && exe_req(w).bits.uop.ctrl.is_sta
                                                              && !exe_req(w).bits.uop.ctrl.is_std)

  // Can we fire an incoming store datagen
  val can_fire_std_incoming  = widthMap(w => exe_req(w).valid && exe_req(w).bits.uop.ctrl.is_std
                                                              && !exe_req(w).bits.uop.ctrl.is_sta)

  // Can we fire an incoming sfence
  val can_fire_sfence        = widthMap(w => exe_req(w).valid && exe_req(w).bits.sfence.valid)

  // Can we fire a request from dcache to release a line
  // This needs to go through LDQ search to mark loads as dangerous
  val can_fire_release       = widthMap(w => (w == memWidth-1).B && io.dmem.release.valid)
  io.dmem.release.ready     := will_fire_release.reduce(_||_)

  // Can we retry a load that missed in the TLB
  val can_fire_load_retry    = widthMap(w =>
                               ( ldq_retry_e.valid                            &&
                                 ldq_retry_e.bits.addr.valid                  &&
                                 ldq_retry_e.bits.addr_is_virtual             &&
                                !p1_block_load_mask(ldq_retry_idx)            &&
                                !p2_block_load_mask(ldq_retry_idx)            &&
                                RegNext(dtlb.io.miss_rdy)                     &&
                                !store_needs_order                            &&
                                (w == memWidth-1).B                           && // TODO: Is this best scheduling?
                                !ldq_retry_e.bits.order_fail))

  // Can we retry a store addrgen that missed in the TLB
  // - Weird edge case when sta_retry and std_incoming for same entry in same cycle. Delay this
  val can_fire_sta_retry     = widthMap(w =>
                               ( stq_retry_e.valid                            &&
                                 stq_retry_e.bits.addr.valid                  &&
                                 stq_retry_e.bits.addr_is_virtual             &&
                                 (w == memWidth-1).B                          &&
                                 RegNext(dtlb.io.miss_rdy)                    &&
                                 !(widthMap(i => (i != w).B               &&
                                                 can_fire_std_incoming(i) &&
                                                 stq_incoming_idx(i) === stq_retry_idx).reduce(_||_))
                               ))
  // Can we commit a store
  val can_fire_store_commit  = widthMap(w =>
                               ( stq_commit_e.valid                           &&
                                !stq_commit_e.bits.uop.is_fence               &&
                                !mem_xcpt_valid                               &&
                                !stq_commit_e.bits.uop.exception              &&
                                (w == 0).B                                    &&
                                (stq_commit_e.bits.committed || ( stq_commit_e.bits.uop.is_amo      &&
                                                                  stq_commit_e.bits.addr.valid      &&
                                                                 !stq_commit_e.bits.addr_is_virtual &&
                                                                  stq_commit_e.bits.data.valid))))

  // Can we wakeup a load that was nack'd
  val block_load_wakeup = WireInit(false.B)
  val can_fire_load_wakeup = widthMap(w =>
                             ( ldq_wakeup_e.valid                                      &&
                               ldq_wakeup_e.bits.addr.valid                            &&
                              !ldq_wakeup_e.bits.succeeded                             &&
                              !ldq_wakeup_e.bits.addr_is_virtual                       &&
                              !ldq_wakeup_e.bits.executed                              &&
                              !ldq_wakeup_e.bits.order_fail                            &&
                              !p1_block_load_mask(ldq_wakeup_idx)                      &&
                              !p2_block_load_mask(ldq_wakeup_idx)                      &&
                              !store_needs_order                                       &&
                              !block_load_wakeup                                       &&
                              (w == memWidth-1).B                                      &&
                              (!ldq_wakeup_e.bits.addr_is_uncacheable || (io.core.commit_load_at_rob_head &&
                                                                          ldq_head === ldq_wakeup_idx &&
                                                                          ldq_wakeup_e.bits.st_dep_mask.asUInt === 0.U))))

  // Can we fire an incoming hellacache request
  val can_fire_hella_incoming  = WireInit(widthMap(w => false.B)) // This is assigned to in the hellashim ocntroller

  // Can we fire a hellacache request that the dcache nack'd
  val can_fire_hella_wakeup    = WireInit(widthMap(w => false.B)) // This is assigned to in the hellashim controller

  //---------------------------------------------------------
  // Controller logic. Arbitrate which request actually fires

  val exe_tlb_valid = Wire(Vec(memWidth, Bool()))
  for (w <- 0 until memWidth) {
    var tlb_avail  = true.B
    var dc_avail   = true.B
    var lcam_avail = true.B
    var rob_avail  = true.B

    def lsu_sched(can_fire: Bool, uses_tlb:Boolean, uses_dc:Boolean, uses_lcam: Boolean, uses_rob:Boolean): Bool = {
      val will_fire = can_fire && !(uses_tlb.B && !tlb_avail) &&
                                  !(uses_lcam.B && !lcam_avail) &&
                                  !(uses_dc.B && !dc_avail) &&
                                  !(uses_rob.B && !rob_avail)
      tlb_avail  = tlb_avail  && !(will_fire && uses_tlb.B)
      lcam_avail = lcam_avail && !(will_fire && uses_lcam.B)
      dc_avail   = dc_avail   && !(will_fire && uses_dc.B)
      rob_avail  = rob_avail  && !(will_fire && uses_rob.B)
      dontTouch(will_fire) // dontTouch these so we can inspect the will_fire signals
      will_fire
    }

    // The order of these statements is the priority
    // Some restrictions
    //  - Incoming ops must get precedence, can't backpresure memaddrgen
    //  - Incoming hellacache ops must get precedence over retrying ops (PTW must get precedence over retrying translation)
    // Notes on performance
    //  - Prioritize releases, this speeds up cache line writebacks and refills
    //  - Store commits are lowest priority, since they don't "block" younger instructions unless stq fills up
    will_fire_load_incoming (w) := lsu_sched(can_fire_load_incoming (w) , true , true , true , false) // TLB , DC , LCAM
    will_fire_stad_incoming (w) := lsu_sched(can_fire_stad_incoming (w) , true , false, true , true)  // TLB ,    , LCAM , ROB
    will_fire_sta_incoming  (w) := lsu_sched(can_fire_sta_incoming  (w) , true , false, true , true)  // TLB ,    , LCAM , ROB
    will_fire_std_incoming  (w) := lsu_sched(can_fire_std_incoming  (w) , false, false, false, true)  //                 , ROB
    will_fire_sfence        (w) := lsu_sched(can_fire_sfence        (w) , true , false, false, true)  // TLB ,    ,      , ROB
    will_fire_release       (w) := lsu_sched(can_fire_release       (w) , false, false, true , false) //            LCAM
    will_fire_hella_incoming(w) := lsu_sched(can_fire_hella_incoming(w) , true , true , false, false) // TLB , DC
    will_fire_hella_wakeup  (w) := lsu_sched(can_fire_hella_wakeup  (w) , false, true , false, false) //     , DC
    will_fire_load_retry    (w) := lsu_sched(can_fire_load_retry    (w) , true , true , true , false) // TLB , DC , LCAM
    will_fire_sta_retry     (w) := lsu_sched(can_fire_sta_retry     (w) , true , false, true , true)  // TLB ,    , LCAM , ROB // TODO: This should be higher priority
    will_fire_load_wakeup   (w) := lsu_sched(can_fire_load_wakeup   (w) , false, true , true , false) //     , DC , LCAM1
    will_fire_store_commit  (w) := lsu_sched(can_fire_store_commit  (w) , false, true , false, false) //     , DC


    assert(!(exe_req(w).valid && !(will_fire_load_incoming(w) || will_fire_stad_incoming(w) || will_fire_sta_incoming(w) || will_fire_std_incoming(w) || will_fire_sfence(w))))

    when (will_fire_load_wakeup(w)) {
      block_load_mask(ldq_wakeup_idx)           := true.B
    } .elsewhen (will_fire_load_incoming(w)) {
      block_load_mask(exe_req(w).bits.uop.ldq_idx) := true.B
    } .elsewhen (will_fire_load_retry(w)) {
      block_load_mask(ldq_retry_idx)            := true.B
    }
    exe_tlb_valid(w) := !tlb_avail
  }
  assert((memWidth == 1).B ||
    (!(will_fire_sfence.reduce(_||_) && !will_fire_sfence.reduce(_&&_)) &&
     !will_fire_hella_incoming.reduce(_&&_) &&
     !will_fire_hella_wakeup.reduce(_&&_)   &&
     !will_fire_load_retry.reduce(_&&_)     &&
     !will_fire_sta_retry.reduce(_&&_)      &&
     !will_fire_store_commit.reduce(_&&_)   &&
     !will_fire_load_wakeup.reduce(_&&_)),
    "Some operations is proceeding down multiple pipes")

  require(memWidth <= 2)

  //--------------------------------------------
  // TLB Access

  assert(!(hella_state =/= h_ready && hella_req.cmd === rocket.M_SFENCE),
    "SFENCE through hella interface not supported")

  val exe_tlb_uop = widthMap(w =>
          Mux(will_fire_load_incoming (w) ||
            will_fire_stad_incoming (w) ||
            will_fire_sta_incoming  (w) ||
            will_fire_sfence        (w)  , exe_req(w).bits.uop,
          Mux(will_fire_load_retry    (w)  , ldq_retry_e.bits.uop,
          Mux(will_fire_sta_retry     (w)  , stq_retry_e.bits.uop,
          Mux(will_fire_hella_incoming(w)  , NullMicroOp(),
                             NullMicroOp())))))

  val exe_tlb_vaddr = widthMap(w =>
                    Mux(will_fire_load_incoming (w) ||
                        will_fire_stad_incoming (w) ||
                        will_fire_sta_incoming  (w)  , exe_req(w).bits.addr,
                    Mux(will_fire_sfence        (w)  , exe_req(w).bits.sfence.bits.addr,
                    Mux(will_fire_load_retry    (w)  , ldq_retry_e.bits.addr.bits,
                    Mux(will_fire_sta_retry     (w)  , stq_retry_e.bits.addr.bits,
                    Mux(will_fire_hella_incoming(w)  , hella_req.addr,
                                                       0.U))))))

  val exe_sfence = WireInit((0.U).asTypeOf(Valid(new rocket.SFenceReq)))
  for (w <- 0 until memWidth) {
    when (will_fire_sfence(w)) {
      exe_sfence := exe_req(w).bits.sfence
    }
  }

  val exe_size   = widthMap(w =>
                   Mux(will_fire_load_incoming (w) ||
                       will_fire_stad_incoming (w) ||
                       will_fire_sta_incoming  (w) ||
                       will_fire_sfence        (w) ||
                       will_fire_load_retry    (w) ||
                       will_fire_sta_retry     (w)  , exe_tlb_uop(w).mem_size,
                   Mux(will_fire_hella_incoming(w)  , hella_req.size,
                                                      0.U)))
  val exe_cmd    = widthMap(w =>
                   Mux(will_fire_load_incoming (w) ||
                       will_fire_stad_incoming (w) ||
                       will_fire_sta_incoming  (w) ||
                       will_fire_sfence        (w) ||
                       will_fire_load_retry    (w) ||
                       will_fire_sta_retry     (w)  , exe_tlb_uop(w).mem_cmd,
                   Mux(will_fire_hella_incoming(w)  , hella_req.cmd,
                                                      0.U)))

  val exe_passthr= widthMap(w =>
                   Mux(will_fire_hella_incoming(w)  , hella_req.phys,
                                                      false.B))
  val exe_kill   = widthMap(w =>
                   Mux(will_fire_hella_incoming(w)  , io.hellacache.s1_kill,
                                                      false.B))
  
  // val exe_tlb_uop_tagged = Wire(Vec(memWidth, new MicroOp()))
  for (w <- 0 until memWidth) {
    dtlb.io.req(w).valid            := exe_tlb_valid(w)
    // corefuzzing
    // Tag the micro-op as it enters the DTLB so later stages / writeback
    // will know the uop visited the DTLB. This is combinational and cheap.
    // when (dtlb.io.req(w).valid && exe_tlb_uop(w).cf_taint_module_id_1 =/= dtlbTagCF.U) {
    //   //exe_tlb_uop(w).appendModuleTag(dtlbTagCF)
    //   exe_tlb_uop_tagged(w) := appendModuleTag(dtlbTagCF.U, exe_tlb_uop(w))
    // }
    // .otherwise {
    //   exe_tlb_uop_tagged(w) := exe_tlb_uop(w)
    // }

    dtlb.io.req(w).bits.vaddr       := exe_tlb_vaddr(w)
    dtlb.io.req(w).bits.size        := exe_size(w)
    dtlb.io.req(w).bits.cmd         := exe_cmd(w)
    dtlb.io.req(w).bits.passthrough := exe_passthr(w)
    dtlb.io.req(w).bits.v           := io.ptw.status.v
    dtlb.io.req(w).bits.prv         := io.ptw.status.prv
    // corefuzzing: pass domain of requesting uop to DTLB
  }
  dtlb.io.kill                      := exe_kill.reduce(_||_)
  dtlb.io.sfence                    := exe_sfence

  // exceptions
  val ma_ld = widthMap(w => will_fire_load_incoming(w) && exe_req(w).bits.mxcpt.valid) // We get ma_ld in memaddrcalc
  val ma_st = widthMap(w => (will_fire_sta_incoming(w) || will_fire_stad_incoming(w)) && exe_req(w).bits.mxcpt.valid) // We get ma_ld in memaddrcalc
  val pf_ld = widthMap(w => dtlb.io.req(w).valid && dtlb.io.resp(w).pf.ld && exe_tlb_uop(w).uses_ldq)
  val pf_st = widthMap(w => dtlb.io.req(w).valid && dtlb.io.resp(w).pf.st && exe_tlb_uop(w).uses_stq)
  val ae_ld = widthMap(w => dtlb.io.req(w).valid && dtlb.io.resp(w).ae.ld && exe_tlb_uop(w).uses_ldq)
  val ae_st = widthMap(w => dtlb.io.req(w).valid && dtlb.io.resp(w).ae.st && exe_tlb_uop(w).uses_stq)

  // TODO check for xcpt_if and verify that never happens on non-speculative instructions.
  val mem_xcpt_valids = RegNext(widthMap(w =>
                     (pf_ld(w) || pf_st(w) || ae_ld(w) || ae_st(w) || ma_ld(w) || ma_st(w)) &&
                     !io.core.exception &&
                     !IsKilledByBranch(io.core.brupdate, exe_tlb_uop(w))))
  val mem_xcpt_uops   = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, exe_tlb_uop(w))))
  val mem_xcpt_causes = RegNext(widthMap(w =>
    Mux(ma_ld(w), rocket.Causes.misaligned_load.U,
    Mux(ma_st(w), rocket.Causes.misaligned_store.U,
    Mux(pf_ld(w), rocket.Causes.load_page_fault.U,
    Mux(pf_st(w), rocket.Causes.store_page_fault.U,
    Mux(ae_ld(w), rocket.Causes.load_access.U,
                  rocket.Causes.store_access.U)))))))
  val mem_xcpt_vaddrs = RegNext(exe_tlb_vaddr)

  for (w <- 0 until memWidth) {
    assert (!(dtlb.io.req(w).valid && exe_tlb_uop(w).is_fence), "Fence is pretending to talk to the TLB")
    assert (!((will_fire_load_incoming(w) || will_fire_sta_incoming(w) || will_fire_stad_incoming(w)) &&
      exe_req(w).bits.mxcpt.valid && dtlb.io.req(w).valid &&
    !(exe_tlb_uop(w).ctrl.is_load || exe_tlb_uop(w).ctrl.is_sta)),
      "A uop that's not a load or store-address is throwing a memory exception.")
  }

  mem_xcpt_valid := mem_xcpt_valids.reduce(_||_)
  mem_xcpt_cause := mem_xcpt_causes(0)
  mem_xcpt_uop   := mem_xcpt_uops(0)
  mem_xcpt_vaddr := mem_xcpt_vaddrs(0)
  var xcpt_found = mem_xcpt_valids(0)
  var oldest_xcpt_rob_idx = mem_xcpt_uops(0).rob_idx
  for (w <- 1 until memWidth) {
    val is_older = WireInit(false.B)
    when (mem_xcpt_valids(w) &&
      (IsOlder(mem_xcpt_uops(w).rob_idx, oldest_xcpt_rob_idx, io.core.rob_head_idx) || !xcpt_found)) {
      is_older := true.B
      mem_xcpt_cause := mem_xcpt_causes(w)
      mem_xcpt_uop   := mem_xcpt_uops(w)
      mem_xcpt_vaddr := mem_xcpt_vaddrs(w)
    }
    xcpt_found = xcpt_found || mem_xcpt_valids(w)
    oldest_xcpt_rob_idx = Mux(is_older, mem_xcpt_uops(w).rob_idx, oldest_xcpt_rob_idx)
  }

  val exe_tlb_miss  = widthMap(w => dtlb.io.req(w).valid && (dtlb.io.resp(w).miss || !dtlb.io.req(w).ready))
  val exe_tlb_paddr = widthMap(w => Cat(dtlb.io.resp(w).paddr(paddrBits-1,corePgIdxBits),
                                        exe_tlb_vaddr(w)(corePgIdxBits-1,0)))
  val exe_tlb_uncacheable = widthMap(w => !(dtlb.io.resp(w).cacheable))

  // corefuzzing: uop copies with dtlbTagCF bit set and cf_secret_access detected via physical address
  // Two-stage wire pattern avoids combinational cycle: Stage 1 = base mods, Stage 2 = influencer.
  val exe_tlb_uop_cf = Wire(Vec(memWidth, new MicroOp()))
  // [FILLSEC] r205 (RULES.md A reading 3: "a line filled by a secret or s_prop load becomes secret"): a LOAD's D$
  // request carries its s_prop as known at issue (dispatch-time window membership, fetch-state / stall edges,
  // operand taint) OR window-shadow membership (dispatched before the arming branch resolved, in the range now).
  // The D$ reads it only for loads, to mark the line its MSHR FILLS (dcache.scala [FILLSEC]); the D$ response
  // brings it back to the writeback, where the regfile tag already takes the same terms (regfile.scala [ONETAG]).
  // Levels: rob_idx flop -> 2 x 8-bit IsOlder -> OR -> the D$ s1 request flop (off the address / index path).
  // Residual (under-taint, OPEN O6): a shadow load that reached the D$ BEFORE its branch resolved fills unmarked.
  def cfLdSprop(u: MicroOp): Bool = u.cf_secret_propagation.getOrElse(false.B) ||
                                    io.core.cf_wsh_sec.map(_.in(u.rob_idx)).getOrElse(false.B)
  // [ONETAGATK] r205 U8: the attacker twin: a load's D$ request carries its attacker record known at issue OR attacker
  // shadow membership; the D$ marks the line its MSHR fills (dcache.scala [ONETAGATK]).
  def cfLdAtk(u: MicroOp): Bool = u.cf_attacker_influence.getOrElse(false.B) ||
                                  io.core.cf_wsh_atk.map(_.in(u.rob_idx)).getOrElse(false.B)
  // [SACCU] 2026-10-21: exe_tlb_in_secret (the per-way export for addr_in_secret) is retired; the STQ entry
  // takes the in-range fact through exe_tlb_uop_cf(w).cf_secret_access (is_secret_lsq) like every other LSQ op.
  // [WAKESEC] is_secret_load exported per way for the load-hit speculative wakeup.
  val exe_tlb_secret_load = Wire(Vec(memWidth, Bool()))
  // [RESID] step 15: the address operand's own taint per way, for the D$ request (cf_addr_sec).
  val exe_tlb_addr_sec = Wire(Vec(memWidth, Bool()))
  val exe_tlb_addr_atk = Wire(Vec(memWidth, Bool()))   // [ATKRES]
  for (w <- 0 until memWidth) {
    // Stage 1: bitmap + secret_access, based on exe_tlb_uop (register-backed, no feedback)
    val uop_tlb_base = WireInit(exe_tlb_uop(w))
    // [DBGFREEZE] patch 85 (b): a debug-mode access (fesvr reading or writing program memory through the program
    // buffer) is never a secret access: no s_acc, no FTQ secret mark, no secret D$ / MSHR / DTLB mark, no STQ
    // addr_in_secret.  One input on the range-valid LUT (CSR flops): 0 levels on the TLB-stage compare.
    val secret_range_valid = io.core.cf_secret_end_addr =/= io.core.cf_secret_start_addr && !io.core.cf_dbg.getOrElse(false.B)
    // Use virtual address for secret-range check: CSRs hold virtual addresses, and on a TLB miss
    // exe_tlb_paddr has invalid page-frame bits (0 from resp.paddr on miss), so physical comparison
    // would always be false for speculative loads that never committed a TLB fill.
    // In bare-metal simulation vaddr==paddr, so this is equivalent and correct in all cases.
    val in_secret = secret_range_valid &&
                    (exe_tlb_vaddr(w) >= io.core.cf_secret_start_addr) &&
                    (exe_tlb_vaddr(w) < io.core.cf_secret_end_addr)
    // s_acc = reading secret data FROM memory into a register (loads only).
    // Stores write TO the secret range but do not produce secret-tagged register values.
    val is_secret_load = in_secret && exe_tlb_uop(w).uses_ldq
    // [FIFO10] step 10 (section 2): an AMO also reads memory into a register but is an STQ op
    // (uses_ldq = 0), so an AMO on the secret range never got s_acc.  The s_acc summary, the uop
    // flag (the STQ copy feeds the AMO response's secret) and the FTQ mark take both; the
    // load-wakeup path keeps is_secret_load.
    val is_secret_rd   = in_secret && (exe_tlb_uop(w).uses_ldq || exe_tlb_uop(w).is_amo)
    // [STSACC] patch 52 (owner 10-17; RULES A "s_acc = an access whose address lies in the
    // secret range ... on every LSQ access"): the s_acc SUMMARY is every LSQ op whose address is
    // in the range, stores included (a store there is a secret access, never s_tx by itself).
    // (Superseded for the uop bit by [SACCU] below.)  Only the ROB message below takes it.  is_secret_rd stays on the uop bit (LDQ/STQ copies)
    // and on the FTQ mark: the STQ uop's cf_secret_access is also the D$ store-meta value mark
    // (dcache st_sec), the MSHR refill line mark (cf_req_secret) and the STL-forward value
    // secret, so setting it for a clean store would make that store mark lines and hand out
    // secret register values (r117/r119: only secret data or a secret-DERIVED address marks).
    // The STQ-side edge consumers (head bits, MEM_ORDER site 1) read the entry's existing
    // registered addr_in_secret instead.  uses_stq replaces is_amo (an AMO has uses_stq=1):
    // same AND/OR shape, same exe_tlb_uop mux arrival, 0 added levels.  sfence (uses_ldq =
    // uses_stq = 0) and hella (NullMicroOp) stay excluded, as before.
    // [STADDR] patch 73 (owner 2026-10-19): "never s_tx by itself" above is about an address IN the secret range.
    // A store whose address is secret-DERIVED (addr_is_secret) is now s_tx, decided at clr_bsy (cfStTx below).
    val is_secret_lsq  = in_secret && (exe_tlb_uop(w).uses_ldq || exe_tlb_uop(w).uses_stq)
    exe_tlb_secret_load(w) := is_secret_load
    // [SACCU] owner 2026-10-21 (RULES.md A "s_acc / cf_secret_access": the uop bit and the record s_acc are ONE
    // definition; every LSQ access (load, store, AMO) whose address is in the secret range sets both).  The uop
    // bit now takes is_secret_lsq, the same wire as the ROB message below (was is_secret_rd).  It differs only for
    // a plain store (uses_stq && !is_amo); loads and AMOs are unchanged.  What the patch-52 note above guarded:
    //   - D$ store-meta (dcache st_sec = s_prop || s_acc): no change, the commit request's s_prop already carries
    //     the store's record s_acc through cf_win_sec (r205 S-a, below); U1/O4 "a store tags its doubleword".
    //   - MSHR refill line mark / op_count adoption (cf_req_secret): gated with isRead in mshrs.scala [SACCU], so
    //     a plain in-range store that misses still marks only its own doubleword (at its rpq replay), never the
    //     whole filled line.
    //   - STL forward (stl_store_is_secret): the forwarding load overlaps the store, so it is in range itself and
    //     its value was already secret (fwd_sacc); the change is the STL_FORWARD edge sec label (U2: the store's
    //     record summary labels the edge).
    // The FTQ mark below stays on is_secret_rd (loads/AMOs).  Levels: one LUT input swapped (is_amo -> uses_stq,
    // the ROB-message term already built from the same exe_tlb_uop flops): 0 added levels, 0 FF.
    uop_tlb_base.cf_secret_access.foreach(_ := exe_tlb_uop(w).cf_secret_access.getOrElse(false.B) || is_secret_lsq)
    // corefuzzing: TLB-stage FTQ update — mark this fetch packet as secret as soon as s_acc is known.
    // Fires speculatively (even for squashed memory ops) to capture transient secret access flows.
    // [L12FTQLIVE] lean59 (Fable audit 2026-10-23 L12): the mark is sent only for a uop that is still LIVE this cycle
    // (not killed by this cycle's branch mispredict mask, no flush), the same kill terms as the mem_xcpt_valids
    // above.  Then every FTQ mark names the entry of a live, uncommitted uop, which the FTQ cannot have
    // re-enqueued (an entry is re-enqueued only after a redirect beyond it, which kills every uop in it, or after
    // it retired), so neither the FTQ bit nor the RAS late mark (cf_ras_secret_upd) can land on another packet,
    // independent of the frontend refetch latency.  "Speculatively" still holds for wrong-path ops that are not
    // yet known dead (unchanged); only the kill cycle's mark is dropped.  Levels: the kill reduce (brupdate
    // replica flops, 2-3 LUT) is shallower than the 40-bit in_secret compares it joins at the final AND: 0 added
    // levels on the exe_tlb -> FTQ ram path.  Cost: ~1 LUT per lane (the kill terms are already built above;
    // firtool shares them).  IFT off: the port is unread (frontend IFT-only), so nothing changes.  RECORD CHANGE.
    io.core.cf_secret_ftq_updates(w).valid := exe_tlb_valid(w) && is_secret_rd &&
                                              !io.core.exception && !IsKilledByBranch(io.core.brupdate, exe_tlb_uop(w))
    io.core.cf_secret_ftq_updates(w).bits  := exe_tlb_uop(w).ftq_idx
    // Direct ROB update: set s_acc immediately when address lands in secret range.
    // op_count_id is included to guard against ROB slot reuse: if this TLB-stage
    // update arrives after the original instruction was squashed and the slot reused,
    // the ROB validates op_count_id before applying cf_secret_access.
    // [FIFO10] step 10 (R-STICKY): a LOAD's issue-path attacker / s_prop flags ride the same
    // message, so a load squashed before writeback keeps them (stores excluded: a committed store
    // never takes its address taint into these bits either, clr_bsy carries only its data taint).
    // [M7STATK] bundle61 (parity with [M7STSP] below): the uses_ldq gate is dropped, so a STORE's issue-time attacker
    // summary rides this message too and a store killed before clr_bsy keeps it in its squash record.  (Comment above
    // "stores excluded" is superseded: the store's data / address attacker taint now also reaches the ROB at clr_bsy,
    // [M7STATK] cfStAtk.)  One LUT input less: 0 levels.  RECORD CHANGE.
    val tlb_ld_atk   = exe_tlb_uop(w).cf_attacker_influence.getOrElse(false.B)
    // [M7STSP] lean58: the s_prop flag rides this message for stores too (the uses_ldq gate is dropped), so a store
    // killed before clr_bsy keeps its issue-time s_prop in its squash record, as a load does.  RECORD CHANGE.
    val tlb_ld_sprop = exe_tlb_uop(w).cf_secret_propagation.getOrElse(false.B)
    // [STSACC] patch 52: is_secret_lsq (was is_secret_rd) on valid and s_acc; ROB side unchanged.
    io.core.cf_s_acc_rob_upd(w).valid               := exe_tlb_valid(w) && (is_secret_lsq || tlb_ld_atk || tlb_ld_sprop)
    io.core.cf_s_acc_rob_upd(w).bits.rob_idx        := exe_tlb_uop(w).rob_idx
    io.core.cf_s_acc_rob_upd(w).bits.op_count_id    := exe_tlb_uop(w).cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
    io.core.cf_s_acc_rob_upd(w).bits.s_acc          := is_secret_lsq
    io.core.cf_s_acc_rob_upd(w).bits.atk            := tlb_ld_atk
    io.core.cf_s_acc_rob_upd(w).bits.s_prop         := tlb_ld_sprop
    // preg_secret early update: mark the physical destination register as secret-tainted.
    // This fires 1+ cycles before writeback, allowing consumers that are issued immediately
    // after the load to see the taint at their issue-grant check cycle.
    // Live preg_secret check for both store data registers and load address registers.
    // The dispatch-time cf_secret_propagation snapshot misses tight-wave cases where the
    // source register's secret load dispatched just before this instruction (its TLB hadn't
    // fired at dispatch, so preg_secret wasn't set yet). By TLB time the load has written
    // back and preg_secret[prs1/prs2] is up to date.
    // taint-follows-data: cf_secret_propagation is now set at BYPASS (register-read),
    // when the tainted operand actually reaches this op.  The old prs1/prs2 table
    // lookups existed only to patch the dispatch-time snapshot missing tight-wave
    // cases; bypass-time tagging has no such gap, so the table is not needed.
    val prs1_live_secret = false.B
    val prs2_live_secret = false.B
    val data_reg_secret  = exe_tlb_uop(w).cf_secret_propagation.getOrElse(false.B) || prs1_live_secret || prs2_live_secret
    // s_tx Case A: store carrying secret-dependent data to a non-secret memory address.
    //   The secret escapes to attacker-accessible memory (anything outside the secret range).
    // [SACCU] (exe_tlb_in_secret retired: the STQ entry takes in_secret through its uop cf_secret_access)
    // s_tx Case B: load where the effective address is derived from secret-propagated data
    //   AND the address is outside the secret range (attacker-accessible memory).
    //   The secret controls which non-secret memory location is accessed, leaking it via address
    //   pattern (e.g., cache timing: load from mem[secret_value + base]).
    //   Exclude loads to secret-range addresses: if the pointer happens to land in secret memory,
    //   the access stays within the secure domain and is not observable by the attacker.
    // 2026-09-03 -- was `data_reg_secret`, i.e. exe_tlb_uop(w).cf_secret_propagation.
    // That is the uop's AGGREGATE taint: it accumulates store-to-load forwarding,
    // BPD/BTB observability edges and dispatch-time state, none of which say anything
    // about the ADDRESS.  Using it here asserted "this load's address is secret-derived"
    // for any load that merely RECEIVED a secret.  MEASURED on t18: the spill reload
    // `lbu a5,-17(s0)` -- address s0, a provably clean frame pointer -- carried s_tx on
    // 481 of its commits, and s_tx totalled 6559 against 256 real secret accesses.
    // The address operand's own taint is on exe_req.bits.secret (the AGU forwards
    // rs1_secret); retries do not read exe_req, so they take the bit stored with the
    // address in their queue entry.  Structure mirrors exe_tlb_vaddr above.
    val addr_reg_secret  = Mux(will_fire_load_incoming(w) || will_fire_stad_incoming(w) ||
                               will_fire_sta_incoming(w), exe_req(w).bits.secret,
                           Mux(will_fire_load_retry(w),   ldq_retry_e.bits.addr_is_secret.getOrElse(false.B),
                           Mux(will_fire_sta_retry(w),    stq_retry_e.bits.addr_is_secret.getOrElse(false.B),
                                                          false.B)))
    // [AMOSTX] step 14 (owner ruling 102): an AMO is a load plus a store.  Its LOAD half
    // takes the load rule: a secret-derived address outside the secret range is a
    // transmission, through the same uop bit and the same direct ROB message as a load
    // (the AMO is an STQ op, uses_ldq = 0, so it was missed).  Its STORE half keeps the
    // store rule (data_is_secret && !addr_in_secret), applied on the AMO's writeback because
    // AMOs never take clr_bsy (see [AMOSTX] at the AMO response below).
    // Same (uses_ldq || is_amo) term as is_secret_rd above: one OR, no new port.
    // [NORANGE] step 17 (owner answer Q3, 2026-10-13: no range gate anywhere): a load/AMO at a
    // secret-derived address is a transmission even when the address is inside the secret
    // range.  Was `&& !in_secret`.  (The store-data rule keeps its own !addr_in_secret: the
    // owner rule there is "secret data to a NON-secret address", Q5/Q8.)  One AND removed.
    val is_secret_addr_load = (exe_tlb_uop(w).uses_ldq || exe_tlb_uop(w).is_amo) && addr_reg_secret
    exe_tlb_addr_sec(w) := addr_reg_secret   // [RESID] not range-gated: residency is observable either way
    // [ATKRES] step 17: the attacker twin, same source mux as addr_reg_secret (same select terms)
    exe_tlb_addr_atk(w) := Mux(will_fire_load_incoming(w) || will_fire_stad_incoming(w) ||
                               will_fire_sta_incoming(w), exe_req(w).bits.taint_atk,
                           Mux(will_fire_load_retry(w),   ldq_retry_e.bits.addr_is_atk.getOrElse(false.B),
                           Mux(will_fire_sta_retry(w),    stq_retry_e.bits.addr_is_atk.getOrElse(false.B),
                                                          false.B)))
    // is_secret_bearing_store REMOVED here: it was computed from the aggregate and the
    // store's data is not knowable at TLB time.  The store's s_tx is now raised at
    // clr_bsy (Case A above).  Leaving it here would let the aggregate back in via
    // rob.scala:1037, which ORs the uop bit into the ROB entry.
    uop_tlb_base.cf_secret_transmission.foreach(_ := exe_tlb_uop(w).cf_secret_transmission.getOrElse(false.B) ||
                                           is_secret_addr_load)
    // Gap 1 fix: emit direct ROB write-back for s_tx at TLB time.
    // Without this, a squashed probe load's ROB entry never shows s_tx=1 because the
    // pipeline uop wire is discarded when the instruction is killed before commit.
    // op_count_id guard (enforced in rob.scala handler) prevents stale-slot updates.
    io.core.cf_s_tx_rob_upd(w).valid            := exe_tlb_valid(w) && is_secret_addr_load
    io.core.cf_s_tx_rob_upd(w).bits.rob_idx     := exe_tlb_uop(w).rob_idx
    io.core.cf_s_tx_rob_upd(w).bits.op_count_id := exe_tlb_uop(w).cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
    // [FIFO10] step 10 (owner ruling 94: machine mode, paging off): the INFL_DTLB_STATE edge
    // (DTLB domain/secret shadow -> message -> C_dtlb column) is removed; it needed a translated
    // TLB hit and could never fire.  The address-range secret access / transmission above is
    // unchanged.
    exe_tlb_uop_cf(w) := uop_tlb_base
  }

  for (w <- 0 until memWidth) {
    assert (exe_tlb_paddr(w) === dtlb.io.resp(w).paddr || exe_req(w).bits.sfence.valid, "[lsu] paddrs should match.")

    when (mem_xcpt_valids(w))
    {
      assert(RegNext(will_fire_load_incoming(w) || will_fire_stad_incoming(w) || will_fire_sta_incoming(w) ||
        will_fire_load_retry(w) || will_fire_sta_retry(w)))
      // Technically only faulting AMOs need this
      assert(mem_xcpt_uops(w).uses_ldq ^ mem_xcpt_uops(w).uses_stq)
      when (mem_xcpt_uops(w).uses_ldq)
      {
        ldq(mem_xcpt_uops(w).ldq_idx).bits.uop.exception := true.B
      }
        .otherwise
      {
        stq(mem_xcpt_uops(w).stq_idx).bits.uop.exception := true.B
      }
    }
  }



  //------------------------------
  // Issue Someting to Memory
  //
  // A memory op can come from many different places
  // The address either was freshly translated, or we are
  // reading a physical address from the LDQ,STQ, or the HellaCache adapter

  // pass corefuzzing configure values to dmem
  io.dmem.cf_dcache_set_conf  := io.core.cf_dcache_set_conf
  io.dmem.cf_dcache_way_conf  := io.core.cf_dcache_way_conf
  io.dmem.cf_dcache_repl_conf := io.core.cf_dcache_repl_conf
  io.dmem.cf_debug_dcache_enable := io.core.cf_debug_dcache_enable && io.core.cf_debug_lsu_enable
  io.dmem.cf_debug_repl_enable   := io.core.cf_debug_repl_enable
  // [reconf-fix Phase C] invalidate-walker trigger down, busy back up
  io.dmem.cf_cachectl_wen      := io.core.cf_cachectl_wen
  io.dmem.cf_cachectl_wdata    := io.core.cf_cachectl_wdata
  io.core.cf_dcache_wipe_busy  := io.dmem.cf_dcache_wipe_busy
  // [reconf-fix 2.10f] D$ reconfiguration status up to the core (0xbcd[16:12])
  io.core.cf_dcache_reconf_status.foreach(_ := io.dmem.cf_dcache_reconf_status.get)

  // defaults
  io.dmem.brupdate       := io.core.brupdate
  io.dmem.exception      := io.core.exception
  io.dmem.rob_head_idx   := io.core.rob_head_idx
  io.dmem.rob_pnr_idx    := io.core.rob_pnr_idx

  val dmem_req = Wire(Vec(memWidth, Valid(new BoomDCacheReq)))
  io.dmem.req.valid := dmem_req.map(_.valid).reduce(_||_)
  io.dmem.req.bits  := dmem_req

  for (w <- 0 until memWidth) {
    // corefuzzing [LSU] debug log
    // Compile-time gated: when ENABLE_CF_DEBUG_PRINTF is false this block is never
    // emitted, so neither the printf nor its argument cone reaches synthesis.
    if (ENABLE_CF_DEBUG_PRINTF) {
      when (io.dmem.req.valid && (io.dmem.req.bits(w).bits.addr === 0.U) && io.core.cf_debug_lsu_enable) {
          // [CFNOISE-COMMENTED] printf("[LSU] lsu out valid and dmem.req.bits(%d).bits.addr - 0x%x\n", w.U, io.dmem.req.bits(w).bits.addr)
      }
    }
  }
  val dmem_req_fire = widthMap(w => dmem_req(w).valid && io.dmem.req.fire)
  // [DREQPROBE 2026-09-09] which will_fire path produced this D$ request.
  // PROVEN 2026-09-09: the request that misses on cache_buf reaches the MSHR with
  // domain=0 AND op_count=0 while the SAME load commits with domain=1 -- i.e. the
  // request carries no op identity.  The address-derived cf_secret_access DOES survive
  // (t38: domain=0 secret=1 oc=0), which is the signature of exe_tlb_uop being a
  // NullMicroOp with the secret bit OR'd on afterwards.  This tag localises WHICH path
  // is responsible instead of inferring it: 1=load_incoming 2=load_retry
  // 3=store_commit 4=load_wakeup 5=hella_incoming 6=hella_wakeup 0=none.
  // [C2 2026-09-10] DREQ probe RETIRED.  No tool consumed it (grep: zero consumers in
  // ift-tests/*.py, fuzzer/*.py), and it cost ~4.4pp of parse recovery in ift-tests
  // (87.4% with vs 91.8% without) by interleaving into other records' lines.  Gated, so
  // this is a sim-speed and parse-fidelity change, not an area change.  Re-enable by
  // uncommenting the wire, its 4 assignments, and the printf block below.
  // val cf_dreq_src = WireInit(VecInit(Seq.fill(memWidth)(0.U(4.W))))

  val s0_executing_loads = WireInit(VecInit((0 until numLdqEntries).map(x=>false.B)))


  for (w <- 0 until memWidth) {
    dmem_req(w).valid := false.B
  dmem_req(w).bits.uop   := NullMicroOp()
    dmem_req(w).bits.addr  := 0.U
    dmem_req(w).bits.data  := 0.U
    dmem_req(w).bits.is_hella := false.B
    // [RESID] step 15 defaults; overridden by the four LSU-op sources below (hella stays 0).
    dmem_req(w).bits.cf_addr_sec.foreach(_ := false.B)
    dmem_req(w).bits.cf_addr_atk.foreach(_ := false.B)   // [ATKRES]
    dmem_req(w).bits.cf_res_v.foreach(_ := false.B)
    dmem_req(w).bits.cf_res_atk.foreach(_ := false.B)   // [ATKEVICT] D$-internal
    dmem_req(w).bits.cf_res_esec.foreach(_ := false.B)
    dmem_req(w).bits.cf_res_repl.foreach(_ := false.B)   // [CEREPL] D$-internal (a nack edge is never replacement-state)
    dmem_req(w).bits.cf_res_nack.foreach(_ := false.B)   // [RECV3] D$-internal (an LSU request's edge is known by s2_type)
    dmem_req(w).bits.cf_res_pc.foreach(_ := 0.U)

    io.dmem.s1_kill(w) := false.B

    when (will_fire_load_incoming(w)) {
      // if (ENABLE_CF_DEBUG_PRINTF) cf_dreq_src(w)         := 1.U
      dmem_req(w).valid      := !exe_tlb_miss(w) && !exe_tlb_uncacheable(w)
      dmem_req(w).bits.addr  := exe_tlb_paddr(w)
      dmem_req(w).bits.uop   := exe_tlb_uop_cf(w) // corefuzzing: use IFT-tagged uop
      dmem_req(w).bits.uop.cf_secret_propagation.foreach(_ := cfLdSprop(exe_tlb_uop_cf(w)))   // [FILLSEC]
      dmem_req(w).bits.uop.cf_attacker_influence.foreach(_ := cfLdAtk(exe_tlb_uop_cf(w)))      // [ONETAGATK]
      dmem_req(w).bits.cf_addr_sec.foreach(_ := exe_tlb_addr_sec(w))   // [RESID]
      dmem_req(w).bits.cf_addr_atk.foreach(_ := exe_tlb_addr_atk(w))   // [ATKRES]

      s0_executing_loads(ldq_incoming_idx(w)) := dmem_req_fire(w)
      assert(!ldq_incoming_e(w).bits.executed)
    } .elsewhen (will_fire_load_retry(w)) {
      // if (ENABLE_CF_DEBUG_PRINTF) cf_dreq_src(w)         := 2.U
      dmem_req(w).valid      := !exe_tlb_miss(w) && !exe_tlb_uncacheable(w)
      dmem_req(w).bits.addr  := exe_tlb_paddr(w)
      dmem_req(w).bits.uop   := exe_tlb_uop_cf(w) // corefuzzing: use IFT-tagged uop
      dmem_req(w).bits.uop.cf_secret_propagation.foreach(_ := cfLdSprop(exe_tlb_uop_cf(w)))   // [FILLSEC]
      dmem_req(w).bits.uop.cf_attacker_influence.foreach(_ := cfLdAtk(exe_tlb_uop_cf(w)))      // [ONETAGATK]
      dmem_req(w).bits.cf_addr_sec.foreach(_ := exe_tlb_addr_sec(w))   // [RESID]
      dmem_req(w).bits.cf_addr_atk.foreach(_ := exe_tlb_addr_atk(w))   // [ATKRES]
      dmem_req(w).bits.cf_res_v.foreach(_ := ldq_retry_e.bits.cf_nack_res_v.getOrElse(false.B) ||
                                             ldq_retry_e.bits.cf_nack_res_atk.getOrElse(false.B))   // [NACKRES] [NACKATK]
      dmem_req(w).bits.cf_res_pc.foreach(_ := ldq_retry_e.bits.cf_nack_res_pc.getOrElse(0.U))
      dmem_req(w).bits.cf_res_atk.foreach(_ := ldq_retry_e.bits.cf_nack_res_atk.getOrElse(false.B))    // [NACKATK]
      dmem_req(w).bits.cf_res_esec.foreach(_ := ldq_retry_e.bits.cf_nack_res_v.getOrElse(false.B))

      s0_executing_loads(ldq_retry_idx) := dmem_req_fire(w)
      assert(!ldq_retry_e.bits.executed)
    } .elsewhen (will_fire_store_commit(w)) {
      // if (ENABLE_CF_DEBUG_PRINTF) cf_dreq_src(w)            := 3.U
      dmem_req(w).valid         := true.B
      dmem_req(w).bits.addr     := stq_commit_e.bits.addr.bits
      dmem_req(w).bits.data     := (new freechips.rocketchip.rocket.StoreGen(
                                    stq_commit_e.bits.uop.mem_size, 0.U,
                                    stq_commit_e.bits.data.bits,
                                    coreDataBytes)).data
      dmem_req(w).bits.uop      := stq_commit_e.bits.uop
      // The line tag records "is this line's content attacker-influenced".  The storing
      // uop's DOMAIN answers that only for a direct attacker store; a victim storing
      // attacker-DERIVED data is equally influence.  data_is_atk is the value's own taint,
      // captured at STD arrival -- not an aggregate.
      // [R1a 2026-09-05] The line tag must carry the STORED VALUE's taint ONLY.
      // Was: `uop.cf_attacker_influence || data_is_atk`.  The comment above already
      // said "not an aggregate" -- and then OR'd the aggregate in anyway.  5th instance
      // of an aggregate standing in for a specific channel.
      // PROVEN 2026-09-05: after R0 widened cf_attacker_influence (16k -> 113k), the
      // victim's own store of array1_sz marked that line attacker-owned, so EVERY later
      // load of array1_sz fired memdf_fire -- control PC 0x800016c4 went 245/245 = 100%
      // attacker-influenced for a value the attacker never wrote.
      // data_is_atk is captured at STD arrival from the store DATA operand's own taint,
      // which is exactly "is this line's content attacker-derived".
      // [ONETAGATK] r205 U8 (the attacker twin of S-a, OPEN O4): the stored doubleword's attacker tag = data taint OR the
      // store's commit-time attacker RECORD (kept in the STQ uop's cf_win_atk at commit, below).
      dmem_req(w).bits.uop.cf_attacker_influence.foreach(_ := stq_commit_e.bits.data_is_atk.getOrElse(false.B) ||
                                                    stq_commit_e.bits.uop.cf_win_atk.getOrElse(false.B))
      // [STDSEC] step 15 (residue audit row 1c): the line's secret DW bit must carry the STORED
      // VALUE's secret taint only, exactly like the attacker bit above.  The uop's aggregate
      // s_prop includes the ADDRESS operand (register-read ORs rs1), so a clean store to a
      // secret-derived address marked its DW secret and every later reader got a false
      // MEM_DATAFLOW sec edge.  The address residue is now marked separately ([RESID]).
      // dcache st_sec = s_prop || s_acc; nothing else reads s_prop on a store request (the
      // MSHR fill secret is s_acc only; the LSU never takes s_prop from a D$ response uop).
      // [ONETAG] r205 (option S-a, OPEN O4 in p4-r205-author.md): the stored doubleword gets the ONE secret tag
      // when the stored VALUE is secret OR the store is a secret / s_prop instruction (its commit-time record,
      // kept in the STQ uop's cf_win_sec at commit, below): the memory twin of "a register written by an s_prop
      // instruction carries the secret tag".  This replaces the window-kind DW mark (address, data or
      // membership, epoch-gated).  One OR on an existing entry read, 0 levels.
      dmem_req(w).bits.uop.cf_secret_propagation.foreach(_ := stq_commit_e.bits.data_is_secret.getOrElse(false.B) ||
                                                    stq_commit_e.bits.uop.cf_win_sec.getOrElse(false.B))
      dmem_req(w).bits.cf_addr_sec.foreach(_ := stq_commit_e.bits.addr_is_secret.getOrElse(false.B))   // [RESID]
      dmem_req(w).bits.cf_addr_atk.foreach(_ := stq_commit_e.bits.addr_is_atk.getOrElse(false.B))   // [ATKRES]
      // [WLSEC] step 17 (owner answer Q5): a store of SECRET DATA to a NON-secret address (the
      // store s_tx rule, clr_bsy / [AMOSTX]) marks the WHOLE D$ line secret.  Carried to the D$
      // on the request uop's cf_secret_transmission, which no D$/MSHR logic read before and the
      // rpq keeps (so a store that misses applies it at its replay).  The D$ response uop of a
      // store/AMO is not read for s_tx (the STQ entry is), so the override is local.
      dmem_req(w).bits.uop.cf_secret_transmission.foreach(_ := stq_commit_e.bits.data_is_secret.getOrElse(false.B) &&
                                                     !stq_commit_e.bits.uop.cf_secret_access.getOrElse(false.B))   // [SACCU]
      // [WINMEM] step 21 (owner r139, F2): the store's WINDOW taint (address, data or
      // membership, incl. the commit-time shadow) rides the request uop's cf_opnd_* to the D$,
      // which marks the stored doubleword window-kind with the uop's epoch (cf_wep_*).  The rpq
      // keeps both for a store that misses.  One 3-input OR per kind.
      dmem_req(w).bits.uop.cf_opnd_wsec.foreach(_ := false.B)   // [ONETAG] r205: no secret window kind
      dmem_req(w).bits.uop.cf_opnd_watk.foreach(_ := false.B)   // [ONETAGATK] r205 U8: no attacker window kind

      stq_execute_head                     := Mux(dmem_req_fire(w),
                                                WrapInc(stq_execute_head, cf_stq_active),
                                                stq_execute_head)

      stq(stq_execute_head).bits.succeeded := false.B
    } .elsewhen (will_fire_load_wakeup(w)) {
      // if (ENABLE_CF_DEBUG_PRINTF) cf_dreq_src(w)         := 4.U
      dmem_req(w).valid      := true.B
      dmem_req(w).bits.addr  := ldq_wakeup_e.bits.addr.bits
      dmem_req(w).bits.uop   := ldq_wakeup_e.bits.uop
      dmem_req(w).bits.uop.cf_secret_propagation.foreach(_ := cfLdSprop(ldq_wakeup_e.bits.uop))   // [FILLSEC]
      dmem_req(w).bits.uop.cf_attacker_influence.foreach(_ := cfLdAtk(ldq_wakeup_e.bits.uop))      // [ONETAGATK]
      dmem_req(w).bits.cf_addr_sec.foreach(_ := ldq_wakeup_e.bits.addr_is_secret.getOrElse(false.B))   // [RESID]
      dmem_req(w).bits.cf_addr_atk.foreach(_ := ldq_wakeup_e.bits.addr_is_atk.getOrElse(false.B))   // [ATKRES]
      dmem_req(w).bits.cf_res_v.foreach(_ := ldq_wakeup_e.bits.cf_nack_res_v.getOrElse(false.B) ||
                                             ldq_wakeup_e.bits.cf_nack_res_atk.getOrElse(false.B))   // [NACKRES] [NACKATK]
      dmem_req(w).bits.cf_res_pc.foreach(_ := ldq_wakeup_e.bits.cf_nack_res_pc.getOrElse(0.U))
      dmem_req(w).bits.cf_res_atk.foreach(_ := ldq_wakeup_e.bits.cf_nack_res_atk.getOrElse(false.B))    // [NACKATK]
      dmem_req(w).bits.cf_res_esec.foreach(_ := ldq_wakeup_e.bits.cf_nack_res_v.getOrElse(false.B))

      s0_executing_loads(ldq_wakeup_idx) := dmem_req_fire(w)

      assert(!ldq_wakeup_e.bits.executed && !ldq_wakeup_e.bits.addr_is_virtual)
    } .elsewhen (will_fire_hella_incoming(w)) {
      assert(hella_state === h_s1)

      dmem_req(w).valid               := !io.hellacache.s1_kill && (!exe_tlb_miss(w) || hella_req.phys)
      dmem_req(w).bits.addr           := exe_tlb_paddr(w)
      dmem_req(w).bits.data           := (new freechips.rocketchip.rocket.StoreGen(
        hella_req.size, 0.U,
        io.hellacache.s1_data.data,
        coreDataBytes)).data
      dmem_req(w).bits.uop.mem_cmd    := hella_req.cmd
      dmem_req(w).bits.uop.mem_size   := hella_req.size
      dmem_req(w).bits.uop.mem_signed := hella_req.signed
      dmem_req(w).bits.is_hella       := true.B

      hella_paddr := exe_tlb_paddr(w)
    }
      .elsewhen (will_fire_hella_wakeup(w))
    {
      assert(hella_state === h_replay)
      dmem_req(w).valid               := true.B
      dmem_req(w).bits.addr           := hella_paddr
      dmem_req(w).bits.data           := (new freechips.rocketchip.rocket.StoreGen(
        hella_req.size, 0.U,
        hella_data.data,
        coreDataBytes)).data
      dmem_req(w).bits.uop.mem_cmd    := hella_req.cmd
      dmem_req(w).bits.uop.mem_size   := hella_req.size
      dmem_req(w).bits.uop.mem_signed := hella_req.signed
      dmem_req(w).bits.is_hella       := true.B
    }

    // [DREQPROBE] every D$ request that actually fires, with its op identity.
    // A line's IFT tag is written from THIS uop (dcache -> mshrs req.uop -> cf_req_*),
    // so a request with src=N and domain=0/oc=0 names the exact path that drops it.
    // if (ENABLE_CF_DEBUG_PRINTF) {
      // when (dmem_req_fire(w) && io.core.cf_debug_lsu_enable) {
        // printf("\n[DREQ] w=%d src=%d addr=0x%x cmd=%d domain=%d sec=%d oc=%d\n",
          // w.U, cf_dreq_src(w), dmem_req(w).bits.addr, dmem_req(w).bits.uop.mem_cmd,
          // dmem_req(w).bits.uop.cf_domain_id, dmem_req(w).bits.uop.cf_secret_access,
          // dmem_req(w).bits.uop.cf_op_count_id)
      // }
    // }

    //-------------------------------------------------------------
    // Write Addr into the LAQ/SAQ
    when (will_fire_load_incoming(w) || will_fire_load_retry(w))
    {
      val ldq_idx = Mux(will_fire_load_incoming(w), ldq_incoming_idx(w), ldq_retry_idx)
      ldq(ldq_idx).bits.addr.valid          := true.B
      ldq(ldq_idx).bits.addr.bits           := Mux(exe_tlb_miss(w), exe_tlb_vaddr(w), exe_tlb_paddr(w))
      ldq(ldq_idx).bits.uop.pdst            := exe_tlb_uop(w).pdst
      ldq(ldq_idx).bits.addr_is_virtual     := exe_tlb_miss(w)
      when (will_fire_load_incoming(w)) {
        ldq(ldq_idx).bits.addr_is_secret.foreach(_ := exe_req(w).bits.secret)   // rs1 taint at the AGU
        ldq(ldq_idx).bits.addr_is_atk.foreach(_ := exe_req(w).bits.taint_atk)
        // [IMPLWIN] step 20: a window-tainted ADDRESS selects a window-tainted value (the
        // window twin of [SECDEST]); the load's result window taint reads its LDQ uop.
        ldq(ldq_idx).bits.uop.cf_opnd_wsec.foreach(_ := false.B)   // [ONETAG] r205: no secret window kind
        ldq(ldq_idx).bits.uop.cf_opnd_watk.foreach(_ := false.B)   // [ONETAGATK] r205 U8
      }
      ldq(ldq_idx).bits.addr_is_uncacheable := exe_tlb_uncacheable(w) && !exe_tlb_miss(w)
      // corefuzzing: write IFT flags computed in TLB stage back to LDQ entry.
      // Required because will_fire_load_wakeup bypasses TLB and uses the LDQ entry UOP
      // directly as the dcache request UOP; without this write-back, IFT bits computed
      // in the TLB stage (cf_secret_access, dtlb FU bit, cf_secret_transmission) are lost
      // on dcache nack + wakeup replay paths.
      // corefuzzing: the entry uop is frozen at ENQUEUE (dispatch); under
      // taint-follows-data the taint is not known then.  cf_secret_access /
      // cf_secret_transmission were already refreshed here -- cf_secret_propagation was
      // simply missing, which left ldq_head_is_secret permanently false and
      // INFL_MEM_HOL / INFL_STL_FORWARD with no secret flag.  exe_tlb_uop is the ISSUED
      // uop, so this is the register-read-resolved taint, not a dispatch snapshot.
      ldq(ldq_idx).bits.uop.cf_secret_propagation.foreach(_ :=
        ldq(ldq_idx).bits.uop.cf_secret_propagation.getOrElse(false.B) || exe_tlb_uop_cf(w).cf_secret_propagation.getOrElse(false.B))
      // [CNTDLSQ] stall-edge audit D3(a): the ISSUED uop's attacker bit (the slot's cf_src_atk || its latched contention /
      // FU_BUSY winners, issue-slot.scala [CNTDSUM]) reaches the LDQ copy as its s_prop does on the line above, so the
      // LDQ head (LDQ_FULL / MEM_HOL), the wakeup / retry fill marks (cfLdAtk) and the response see it.  Same select and
      // enable as the s_prop bit: 0 FF, one more write leg per entry bit (~40 LUT), 0 levels.  Retry: the entry ORs itself.
      ldq(ldq_idx).bits.uop.cf_attacker_influence.foreach(_ :=
        ldq(ldq_idx).bits.uop.cf_attacker_influence.getOrElse(false.B) || exe_tlb_uop_cf(w).cf_attacker_influence.getOrElse(false.B))
      ldq(ldq_idx).bits.uop.cf_secret_access.foreach(_ :=
        ldq(ldq_idx).bits.uop.cf_secret_access.getOrElse(false.B) || exe_tlb_uop_cf(w).cf_secret_access.getOrElse(false.B))
      ldq(ldq_idx).bits.uop.cf_secret_transmission.foreach(_ :=
        ldq(ldq_idx).bits.uop.cf_secret_transmission.getOrElse(false.B) || exe_tlb_uop_cf(w).cf_secret_transmission.getOrElse(false.B))
      ldq(ldq_idx).bits.uop.cf_fu_bitmap.foreach(_ :=
        ldq(ldq_idx).bits.uop.cf_fu_bitmap.getOrElse(0.U(numModules.W)) | exe_tlb_uop_cf(w).cf_fu_bitmap.getOrElse(0.U(numModules.W)))

      assert(!(will_fire_load_incoming(w) && ldq_incoming_e(w).bits.addr.valid),
        "[lsu] Incoming load is overwriting a valid address")
    }

    when (will_fire_sta_incoming(w) || will_fire_stad_incoming(w) || will_fire_sta_retry(w))
    {
      val stq_idx = Mux(will_fire_sta_incoming(w) || will_fire_stad_incoming(w),
        stq_incoming_idx(w), stq_retry_idx)

      stq(stq_idx).bits.addr.valid := !pf_st(w) // Prevent AMOs from executing!
      stq(stq_idx).bits.addr.bits  := Mux(exe_tlb_miss(w), exe_tlb_vaddr(w), exe_tlb_paddr(w))
      stq(stq_idx).bits.uop.pdst   := exe_tlb_uop(w).pdst // Needed for AMOs
      stq(stq_idx).bits.addr_is_virtual := exe_tlb_miss(w)
      when (will_fire_sta_incoming(w) || will_fire_stad_incoming(w)) {
        stq(stq_idx).bits.addr_is_secret.foreach(_ := exe_req(w).bits.secret)   // rs1 taint at the AGU
        stq(stq_idx).bits.addr_is_atk.foreach(_ := exe_req(w).bits.taint_atk)
        // [WINMEM] the address operand's window taint (register-read: cf_opnd_* of a store)
        stq(stq_idx).bits.addr_is_wsec.foreach(_ := exe_req(w).bits.uop.cf_opnd_wsec.getOrElse(false.B))
        stq(stq_idx).bits.addr_is_watk.foreach(_ := exe_req(w).bits.uop.cf_opnd_watk.getOrElse(false.B))
      }
      // corefuzzing: write IFT flags computed in TLB stage into STQ so clr_bsy carries them to ROB
      // same omission on the store path (see the LDQ note above)
      stq(stq_idx).bits.uop.cf_secret_propagation.foreach(_ :=
        stq(stq_idx).bits.uop.cf_secret_propagation.getOrElse(false.B) || exe_tlb_uop_cf(w).cf_secret_propagation.getOrElse(false.B))
      stq(stq_idx).bits.uop.cf_secret_transmission.foreach(_ :=
        stq(stq_idx).bits.uop.cf_secret_transmission.getOrElse(false.B) || exe_tlb_uop_cf(w).cf_secret_transmission.getOrElse(false.B))
      stq(stq_idx).bits.uop.cf_secret_access.foreach(_ :=
        stq(stq_idx).bits.uop.cf_secret_access.getOrElse(false.B) || exe_tlb_uop_cf(w).cf_secret_access.getOrElse(false.B))
      // [CNTDLSQ] stall-edge audit D3(a): the issued store's attacker bit (incl. its latched contention / FU_BUSY winners)
      // joins cf_dis_atk, the STQ's attacker record summary (readers: STQ head, MEM_ORDER, STL_PARTIAL / STL_FORWARD
      // labels, LSU_LANE winner).  The uop copy stays zeroed (Change 8: the commit request / AMO response read it).
      // Existing flop, same select and enable as the s_prop bit above: 0 FF, ~40 LUT, 0 levels.  sta_retry: the entry's
      // zeroed uop bit (no-op).
      stq(stq_idx).bits.cf_dis_atk.foreach(_ :=
        stq(stq_idx).bits.cf_dis_atk.getOrElse(false.B) || exe_tlb_uop_cf(w).cf_attacker_influence.getOrElse(false.B))

      assert(!(will_fire_sta_incoming(w) && stq_incoming_e(w).bits.addr.valid),
        "[lsu] Incoming store is overwriting a valid address")

    }

    //-------------------------------------------------------------
    // Write data into the STQ
    if (w == 0)
      io.core.fp_stdata.ready := !will_fire_std_incoming(w) && !will_fire_stad_incoming(w)
    val fp_stdata_fire = io.core.fp_stdata.fire && (w == 0).B
    when (will_fire_std_incoming(w) || will_fire_stad_incoming(w) || fp_stdata_fire)
    {
      val sidx = Mux(will_fire_std_incoming(w) || will_fire_stad_incoming(w),
        stq_incoming_idx(w),
        io.core.fp_stdata.bits.uop.stq_idx)
      stq(sidx).bits.data.valid := true.B
      stq(sidx).bits.data.bits  := Mux(will_fire_std_incoming(w) || will_fire_stad_incoming(w),
        exe_req(w).bits.data,
        io.core.fp_stdata.bits.data)
      // STD TAINT (2026-09-03): a store is split STA (address, via AGU/TLB) and STD
      // (data, this port).  lsu.scala:1171 refreshes the STQ entry's taint from the
      // TLB path only -- i.e. from the ADDRESS operand -- so for `sb a5,-17(s0)` the
      // tainted operand (the DATA, a5) never reached the entry and it stayed clean.
      // That starved BOTH memory taint paths, which are otherwise correct:
      //   - STL forward reads stq_e.bits.uop.cf_secret_propagation  (lsu.scala:1883)
      //   - dcache line tag writes s2_req.uop.cf_secret_propagation (dcache.scala:1365)
      // Consequence: taint did not survive a spill/reload, so a -O0 secret reloaded
      // from the stack came back clean.  MEASURED: t18 had 0 tainted branches out of
      // 27,241 while the same run showed 256 tainted registers.
      // The data operand's taint rides in on .secret for both sources, so capture it
      // here, monotone within the entry's lifetime (enqueue reassigns the whole uop).
      stq(sidx).bits.uop.cf_secret_propagation.foreach(_ := stq(sidx).bits.uop.cf_secret_propagation.getOrElse(false.B) ||
        Mux(will_fire_std_incoming(w) || will_fire_stad_incoming(w),
            exe_req(w).bits.data_secret,   // rs2 taint, NOT .secret (that is the ADDRESS)
            io.core.fp_stdata.bits.secret))
      // and record it on its own, so consumers that mean "the VALUE is secret" do not
      // have to read the aggregate uop bit (see stl_store_is_secret below).
      stq(sidx).bits.data_is_secret.foreach(_ :=
        Mux(will_fire_std_incoming(w) || will_fire_stad_incoming(w),
            exe_req(w).bits.data_secret,
            io.core.fp_stdata.bits.secret))
      // [A1PROBE 2026-09-10] A1: stores never carry s_prop, yet the clr_bsy path that is
      // SUPPOSED to deliver it already exists and is complete
      // (lsu:1521 -> lsu:1588 -> core:2141 -> rob:527) -- and 0 of 270,725 store records
      // in spectre-v1 have s_prop=1.  So the taint dies BEFORE data_is_secret.  Print the
      // operand taint at STD arrival: data_sec=0 here => upstream (regfile/bypass/rtype
      // guard); data_sec=1 => downstream (clr_bsy never fires for a squashed store).
   // [PROBE-STRIPPED 2026-09-10] A1STD -- A1 is CLOSED (store sink 0x80001760 recovered by
      // preg_dataflow.py A1SINK, 6/6 chain).  Probe retired.
      // if (ENABLE_CF_DEBUG_PRINTF) {
        // when (io.core.cf_debug_lsu_enable) {
          // printf("\n[A1STD] stq=%d std=%d stad=%d data_sec=%d data_atk=%d oc=%d\n",
            // sidx, will_fire_std_incoming(w), will_fire_stad_incoming(w),
            // exe_req(w).bits.data_secret, exe_req(w).bits.data_taint_atk,
            // exe_req(w).bits.uop.cf_op_count_id)
        // }
      // }
      // ATTACKER taint of the stored VALUE, captured at the same instant from the same
      // operand.  rs2 is the data; the AGU forwards its attacker taint alongside rs1's.
      stq(sidx).bits.data_is_atk.foreach(_ :=
        Mux(will_fire_std_incoming(w) || will_fire_stad_incoming(w),
            exe_req(w).bits.data_taint_atk,
            io.core.fp_stdata.bits.taint_atk))
      // [WINMEM] the data operand's window taint (register-read cf_dwin_*; the FP store-data
      // uop is the FP register-read's, so it carries the same field)
      stq(sidx).bits.data_is_wsec.foreach(_ :=
        Mux(will_fire_std_incoming(w) || will_fire_stad_incoming(w),
            exe_req(w).bits.uop.cf_dwin_sec.getOrElse(false.B),
            io.core.fp_stdata.bits.uop.cf_dwin_sec.getOrElse(false.B)))
      stq(sidx).bits.data_is_watk.foreach(_ :=
        Mux(will_fire_std_incoming(w) || will_fire_stad_incoming(w),
            exe_req(w).bits.uop.cf_dwin_atk.getOrElse(false.B),
            io.core.fp_stdata.bits.uop.cf_dwin_atk.getOrElse(false.B)))
      assert(!(stq(sidx).bits.data.valid),
        "[lsu] Incoming store is overwriting a valid data entry")
    }
  }
  val will_fire_stdf_incoming = io.core.fp_stdata.fire
  require (xLen >= fLen) // for correct SDQ size

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Cache Access Cycle (Mem)
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Note the DCache may not have accepted our request

  val exe_req_killed = widthMap(w => IsKilledByBranch(io.core.brupdate, exe_req(w).bits.uop))
  val stdf_killed = IsKilledByBranch(io.core.brupdate, io.core.fp_stdata.bits.uop)

  val fired_load_incoming  = widthMap(w => RegNext(will_fire_load_incoming(w) && !exe_req_killed(w)))
  // [WAKESEC] the incoming load's secret as known at TLB time, aligned with
  // fired_load_incoming for spec_ld_wakeup (Phase 0 finding (b)).
  val fired_load_secret    = widthMap(w => RegNext(exe_tlb_secret_load(w) || exe_tlb_uop(w).cf_secret_propagation.getOrElse(false.B)))
  val fired_stad_incoming  = widthMap(w => RegNext(will_fire_stad_incoming(w) && !exe_req_killed(w)))
  val fired_sta_incoming   = widthMap(w => RegNext(will_fire_sta_incoming (w) && !exe_req_killed(w)))
  val fired_std_incoming   = widthMap(w => RegNext(will_fire_std_incoming (w) && !exe_req_killed(w)))
  val fired_stdf_incoming  = RegNext(will_fire_stdf_incoming && !stdf_killed)
  val fired_sfence         = RegNext(will_fire_sfence)
  val fired_release        = RegNext(will_fire_release)
  val fired_load_retry     = widthMap(w => RegNext(will_fire_load_retry   (w) && !IsKilledByBranch(io.core.brupdate, ldq_retry_e.bits.uop)))
  val fired_sta_retry      = widthMap(w => RegNext(will_fire_sta_retry    (w) && !IsKilledByBranch(io.core.brupdate, stq_retry_e.bits.uop)))
  val fired_store_commit   = RegNext(will_fire_store_commit)
  val fired_load_wakeup    = widthMap(w => RegNext(will_fire_load_wakeup  (w) && !IsKilledByBranch(io.core.brupdate, ldq_wakeup_e.bits.uop)))
  val fired_hella_incoming = RegNext(will_fire_hella_incoming)
  val fired_hella_wakeup   = RegNext(will_fire_hella_wakeup)

  // corefuzzing changes
  // Tag micro-ops when they enter the memory subsystem/queues.
  // We append a memory-issuing queue tag so the uop history records
  // passage through the LSU's dispatch/issue logic.
  // Build temporary maps, append tags explicitly, then register them with RegNext.
  // this might not be necessary. We can probalby get away with modifying the entries in place
  // the tempv might actually hurt usage
  // val mem_incoming_uop_w = widthMap(w => {
  //   val tmp = Wire(new MicroOp())
  //   // val tmp: MicroOp = UpdateBrMask(io.core.brupdate, exe_req(w).bits.uop)
  //   // Append the mem-issuing-queue tag only when this request is actually
  //   // scheduled to fire into the memory subsystem. This prevents repeated
  //   // tagging when the temporary uop value is constructed but not enqueued.
  //   when (will_fire_load_incoming(w) || will_fire_stad_incoming(w) || will_fire_sta_incoming(w) ||
  //         will_fire_sfence(w) || will_fire_load_retry(w) || will_fire_sta_retry(w) ||
  //         will_fire_hella_incoming(w) && exe_req(w).bits.uop.cf_taint_module_id_1 =/= memissqTagCF.U) {
  //     tmp := UpdateBrMask(io.core.brupdate, appendModuleTag(memissqTagCF.U, exe_req(w).bits.uop))
  //   }
  //   .otherwise{
  //     tmp := UpdateBrMask(io.core.brupdate, exe_req(w).bits.uop)
  //   }
  //   tmp
  // })
  // val mem_incoming_uop = RegNext(mem_incoming_uop_w)

  // val mem_ldq_incoming_e_w = widthMap(w => {
  //   // val tmp = new Valid(new LDQEntry)
  //   // when (ldq_incoming_e(w).valid && will_fire_load_incoming(w) && ldq_incoming_e(w).bits.uop.cf_taint_module_id_1 =/= ldqTagCF.U) {
  //   //   tmp.bits.uop := ldq_incoming_e(w).bits
  //   // }
  //   // val tmpv: Valid[LDQEntry] = UpdateBrMask(io.core.brupdate, ldq_incoming_e(w))
  //   val tmpv = Wire(new Valid(new LDQEntry))
  //   // Only tag when the LDQ entry is actually being inserted (will_fire)
  //   when (ldq_incoming_e(w).valid && will_fire_load_incoming(w) && ldq_incoming_e(w).bits.uop.cf_taint_module_id_1 =/= ldqTagCF.U) {
  //     tmpv := UpdateBrMask(io.core.brupdate, appendModuleTag(ldqTagCF.U, ldq_incoming_e(w)))
  //   }
  //   .otherwise {
  //     tmpv := UpdateBrMask(io.core.brupdate, ldq_incoming_e(w))
  //   }
  //   tmpv
  // })
  // val mem_ldq_incoming_e = RegNext(mem_ldq_incoming_e_w)

  // val mem_stq_incoming_e_w = widthMap(w => {
  //   val tmpv = Wire(new Valid(new STQEntry))
  //   // val tmpv: Valid[STQEntry] = UpdateBrMask(io.core.brupdate, stq_incoming_e(w))
  //   // Only tag when the STQ entry is actually being inserted (will_fire)
  //   when (stq_incoming_e(w).valid && (will_fire_stad_incoming(w) || will_fire_sta_incoming(w)) && stq_incoming_e(w).bits.uop.cf_taint_module_id_1 =/= stqTagCF.U) { 
  //     tmpv := UpdateBrMask(io.core.brupdate, appendModuleTag(stqTagCF.U, stq_incoming_e(w))) 
  //   }
  //   .otherwise {
  //     tmpv := UpdateBrMask(io.core.brupdate, stq_incoming_e(w))
  //   }
  //   tmpv
  // })
  // val mem_stq_incoming_e = RegNext(mem_stq_incoming_e_w)

  val mem_incoming_uop = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, exe_req(w).bits.uop)))
  val mem_ldq_incoming_e = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, ldq_incoming_e(w))))
  val mem_stq_incoming_e = RegNext(widthMap(w => UpdateBrMask(io.core.brupdate, stq_incoming_e(w))))
  val mem_ldq_wakeup_e     = RegNext(UpdateBrMask(io.core.brupdate, ldq_wakeup_e))
  val mem_ldq_retry_e      = RegNext(UpdateBrMask(io.core.brupdate, ldq_retry_e))
  val mem_stq_retry_e      = RegNext(UpdateBrMask(io.core.brupdate, stq_retry_e))
  
  // not tagging the uop on retries and wakeups but we count them
  // val mem_ldq_wakeup_e_w = {
  //   val tmpv = WireInit(ldq_wakeup_e)
  //   // Tag LDQ wakeups -- keep existing behavior: append when valid.
  //   when (ldq_wakeup_e.valid) { 
  //     tmpv.bits.uop.cf_count_ldq_wakeups := ldq_wakeup_e.bits.uop.cf_count_ldq_wakeups + 1.U }
  //   tmpv
  // }
  // val mem_ldq_wakeup_e = RegNext(mem_ldq_wakeup_e_w)

  // val mem_ldq_retry_e_w = {
  //   val tmpv = WireInit(ldq_retry_e)
  // // Tag LDQ retries -- keep existing behavior: append when valid.
  //   when (ldq_retry_e.valid) { 
  //     tmpv.bits.uop.cf_count_ldq_stq_retries := ldq_retry_e.bits.uop.cf_count_ldq_stq_retries + 1.U
  //     // appendModuleTag(ldqTagCF.U, tmpv.bits.uop) 
  //   }
  //   tmpv
  // }
  // val mem_ldq_retry_e = RegNext(mem_ldq_retry_e_w)

  // val mem_stq_retry_e_w = {
  //   // val tmpv: Valid[STQEntry] = UpdateBrMask(io.core.brupdate, stq_retry_e)
  //   val tmpv = WireInit(stq_retry_e)
  //   // Tag STQ retries -- keep existing behavior: append when valid.
  //   when (stq_retry_e.valid) { 
  //     tmpv.bits.uop.cf_count_ldq_stq_retries := stq_retry_e.bits.uop.cf_count_ldq_stq_retries + 1.U 
  //   }
  //   tmpv
  // }
  // val mem_stq_retry_e = RegNext(mem_stq_retry_e_w)
  val mem_ldq_e            = widthMap(w =>
                             Mux(fired_load_incoming(w), mem_ldq_incoming_e(w),
                             Mux(fired_load_retry   (w), mem_ldq_retry_e,
                             Mux(fired_load_wakeup  (w), mem_ldq_wakeup_e, (0.U).asTypeOf(Valid(new LDQEntry))))))
  val mem_stq_e            = widthMap(w =>
                             Mux(fired_stad_incoming(w) ||
                                 fired_sta_incoming (w), mem_stq_incoming_e(w),
                             Mux(fired_sta_retry    (w), mem_stq_retry_e, (0.U).asTypeOf(Valid(new STQEntry)))))
  val mem_stdf_uop         = RegNext(UpdateBrMask(io.core.brupdate, io.core.fp_stdata.bits.uop))


  val mem_tlb_miss             = RegNext(exe_tlb_miss)
  val mem_tlb_uncacheable      = RegNext(exe_tlb_uncacheable)
  val mem_paddr                = RegNext(widthMap(w => dmem_req(w).bits.addr))

  // [WINMEM] step 21 (owner r139, F2): the store's s_prop / s_tx RECORD counts window-tainted
  // DATA as before (step 20 reached it through the collapsed data_is_secret); only the memory
  // marks keep the kinds apart.  One OR on each existing entry read.
  // [ONETAG] r205: one tag; window-derived data is already in data_is_secret.  The store s_tx rule stays the DATA rule
  // (secret data to a non-secret address, or a secret-derived address): a store that is s_prop only does not
  // become s_tx (OPEN O4).  With one tag the window address now counts as a secret address ([STADDR] term).
  def cfStDataSec(e: STQEntry): Bool = e.data_is_secret.getOrElse(false.B)
  // [STADDR] patch 73 (owner approval 2026-10-19; SINKS.md S3, the dead-end sink "secret-indexed store"): a store
  // whose ADDRESS is secret-derived (addr_is_secret: the AGU rs1 DATA taint, written at sta/stad incoming) is a
  // transmission even with clean data: its D$ fill / replacement update / dirty bit / Acquire are chosen by the
  // secret and outlive the store whether or not any later instruction reads them (the load Case B twin).  No
  // range gate (r118 Q3, as [NORANGE]).  [ONETAG] r205: there is no separate window taint of the address any more;
  // a window-derived address register carries the one secret tag, so addr_is_secret covers it.
  // Decided at clr_bsy like Case A, so a squashed store that never reached clr_bsy is not marked (a store writes
  // the D$ only after commit).  Replaces r117's "the store itself is not s_tx" (RULES A l.18-19).
  // Levels: addr_is_secret is an STQ entry flop read through the same stq_idx select as the s_acc uop bit; the
  // final term gains one LUT input ((a || b) && !c || d, 4 inputs), ends at clr_bsy_cf_stx_r: 0 added levels.
  // IFT off: addr_is_secret is None (false), the term folds to the old one.
  def cfStAddrTx(e: STQEntry): Bool = e.addr_is_secret.getOrElse(false.B)
  def cfStTx(e: STQEntry): Bool = (cfStDataSec(e) && !e.uop.cf_secret_access.getOrElse(false.B)) || cfStAddrTx(e)   // [SACCU]
  // [M7STSP] lean58 (Fable audit M7; spec s_prop, load/store symmetry): a store's s_prop record takes its ADDRESS
  // operand too (addr_is_secret, the [STADDR] bit), as a load's does: a store whose address register is secret is
  // s_tx ([STADDR]) and now also s_prop.  Used by every clr_bsy s_prop site.  One OR input: 0 levels.  RECORD CHANGE.
  def cfStSprop(e: STQEntry): Bool = cfStDataSec(e) || cfStAddrTx(e)
  // [M7STATK] bundle61 (owner 2026-10-24 13:2x via the controller; RULES A reading 4 / U8 symmetric, C M7 line): the
  // attacker twin of [M7STSP].  A store's attacker record takes its DATA operand (data_is_atk, written at STD) and its
  // ADDRESS operand (addr_is_atk, written at the TLB stage) attacker taint, as a load's record does; before, a store
  // whose data / address register was attacker-tainted committed with atk=0 while the load forwarded from it got ty1
  // atk=1 from the same STQ entry (t75 0x80001594 sd a5).  Carried on clr_bsy like s_prop (clr_bsy_cf_atk) and set
  // into the row's sticky attacker bit at the ROB.  Levels: two STQ entry flops through the same stq_idx select as
  // the s_prop bits, into a new flop: 0 added levels on existing paths.
  def cfStAtk(e: STQEntry): Bool = e.data_is_atk.getOrElse(false.B) || e.addr_is_atk.getOrElse(false.B)

  // Task 1: Clr ROB busy bit
  val clr_bsy_valid    = RegInit(widthMap(w => false.B))
  val clr_bsy_rob_idx  = Reg(Vec(memWidth, UInt(robAddrSz.W)))
  val clr_bsy_brmask   = Reg(Vec(memWidth, UInt(maxBrCount.W)))
  // corefuzzing: carry accumulated STQ uop cf_fu_bitmap to ROB so stores have dtlb+dcache bits
  val clr_bsy_cf_bmap  = Reg(Vec(memWidth, UInt(numModules.W)))
  // corefuzzing: carry cf_secret_transmission from STQ uop to ROB
  val clr_bsy_cf_stx_r = RegInit(widthMap(w => false.B))
  val clr_bsy_cf_sprop_r = RegInit(widthMap(w => false.B))
  val clr_bsy_cf_atk_r   = if (ENABLE_IFT) Some(RegInit(widthMap(w => false.B))) else None   // [M7STATK]

  for (w <- 0 until memWidth) {
    clr_bsy_valid      (w) := false.B
    clr_bsy_rob_idx    (w) := 0.U
    clr_bsy_brmask     (w) := 0.U
    clr_bsy_cf_bmap    (w) := 0.U
    clr_bsy_cf_stx_r   (w) := false.B
    clr_bsy_cf_sprop_r (w) := false.B
    clr_bsy_cf_atk_r.foreach(_(w) := false.B)   // [M7STATK]


    when (fired_stad_incoming(w)) {
      clr_bsy_valid      (w) := mem_stq_incoming_e(w).valid           &&
                               !mem_tlb_miss(w)                       &&
                               !mem_stq_incoming_e(w).bits.uop.is_amo &&
                               !IsKilledByBranch(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      clr_bsy_rob_idx    (w) := mem_stq_incoming_e(w).bits.uop.rob_idx
      clr_bsy_brmask     (w) := GetNewBrMask(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      // corefuzzing: capture live stq cf_fu_bitmap (updated in EXE stage with dtlb/dcache bits)
      clr_bsy_cf_bmap    (w) := stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.uop.cf_fu_bitmap.getOrElse(0.U(numModules.W))
      // Case A (2026-09-03): decide the store's s_tx HERE, not at the TLB stage.
      // clr_bsy fires once BOTH halves of the store have landed, so data_is_secret is
      // valid; the TLB stage could not know it for a split STA/STD, which is why the
      // old code fell back to the uop's aggregate cf_secret_propagation and marked any
      // store inside a tainted region as a transmission (e.g. `sd s0,40(sp)`, x507,
      // spilling a provably CLEAN s0).  A transmission is secret DATA reaching a
      // non-secret address -- both facts now live in the entry.
      clr_bsy_cf_sprop_r (w) := cfStSprop(stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits)   // [WINMEM] [M7STSP]
      clr_bsy_cf_atk_r.foreach(_(w) := cfStAtk(stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits))   // [M7STATK]
      clr_bsy_cf_stx_r   (w) := cfStTx(stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits)   // Case A || [STADDR]
    } .elsewhen (fired_sta_incoming(w)) {
      clr_bsy_valid      (w) := mem_stq_incoming_e(w).valid            &&
                                mem_stq_incoming_e(w).bits.data.valid  &&
                               !mem_tlb_miss(w)                        &&
                               !mem_stq_incoming_e(w).bits.uop.is_amo  &&
                               !IsKilledByBranch(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      clr_bsy_rob_idx    (w) := mem_stq_incoming_e(w).bits.uop.rob_idx
      clr_bsy_brmask     (w) := GetNewBrMask(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      clr_bsy_cf_bmap    (w) := stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.uop.cf_fu_bitmap.getOrElse(0.U(numModules.W))
      // Case A (2026-09-03): decide the store's s_tx HERE, not at the TLB stage.
      // clr_bsy fires once BOTH halves of the store have landed, so data_is_secret is
      // valid; the TLB stage could not know it for a split STA/STD, which is why the
      // old code fell back to the uop's aggregate cf_secret_propagation and marked any
      // store inside a tainted region as a transmission (e.g. `sd s0,40(sp)`, x507,
      // spilling a provably CLEAN s0).  A transmission is secret DATA reaching a
      // non-secret address -- both facts now live in the entry.
      clr_bsy_cf_sprop_r (w) := cfStSprop(stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits)   // [WINMEM] [M7STSP]
      clr_bsy_cf_atk_r.foreach(_(w) := cfStAtk(stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits))   // [M7STATK]
      clr_bsy_cf_stx_r   (w) := cfStTx(stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits)   // Case A || [STADDR]
    } .elsewhen (fired_std_incoming(w)) {
      clr_bsy_valid      (w) := mem_stq_incoming_e(w).valid                 &&
                                mem_stq_incoming_e(w).bits.addr.valid       &&
                               !mem_stq_incoming_e(w).bits.addr_is_virtual  &&
                               !mem_stq_incoming_e(w).bits.uop.is_amo       &&
                               !IsKilledByBranch(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      clr_bsy_rob_idx    (w) := mem_stq_incoming_e(w).bits.uop.rob_idx
      clr_bsy_brmask     (w) := GetNewBrMask(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      clr_bsy_cf_bmap    (w) := stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.uop.cf_fu_bitmap.getOrElse(0.U(numModules.W))
      // Case A (2026-09-03): decide the store's s_tx HERE, not at the TLB stage.
      // clr_bsy fires once BOTH halves of the store have landed, so data_is_secret is
      // valid; the TLB stage could not know it for a split STA/STD, which is why the
      // old code fell back to the uop's aggregate cf_secret_propagation and marked any
      // store inside a tainted region as a transmission (e.g. `sd s0,40(sp)`, x507,
      // spilling a provably CLEAN s0).  A transmission is secret DATA reaching a
      // non-secret address -- both facts now live in the entry.
      clr_bsy_cf_sprop_r (w) := cfStSprop(stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits)   // [WINMEM] [M7STSP]
      clr_bsy_cf_atk_r.foreach(_(w) := cfStAtk(stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits))   // [M7STATK]
      clr_bsy_cf_stx_r   (w) := cfStTx(stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits)   // Case A || [STADDR]
    } .elsewhen (fired_sfence(w)) {
      clr_bsy_valid      (w) := (w == 0).B // SFence proceeds down all paths, only allow one to clr the rob
      clr_bsy_rob_idx    (w) := mem_incoming_uop(w).rob_idx
      clr_bsy_brmask     (w) := GetNewBrMask(io.core.brupdate, mem_incoming_uop(w))
      clr_bsy_cf_bmap    (w) := mem_incoming_uop(w).cf_fu_bitmap.getOrElse(0.U(numModules.W))
      clr_bsy_cf_stx_r   (w) := false.B
    clr_bsy_cf_sprop_r (w) := false.B
      clr_bsy_cf_atk_r.foreach(_(w) := false.B)   // [M7STATK]
    } .elsewhen (fired_sta_retry(w)) {
      clr_bsy_valid      (w) := mem_stq_retry_e.valid            &&
                                mem_stq_retry_e.bits.data.valid  &&
                               !mem_tlb_miss(w)                  &&
                               !mem_stq_retry_e.bits.uop.is_amo  &&
                               !IsKilledByBranch(io.core.brupdate, mem_stq_retry_e.bits.uop)
      clr_bsy_rob_idx    (w) := mem_stq_retry_e.bits.uop.rob_idx
      clr_bsy_brmask     (w) := GetNewBrMask(io.core.brupdate, mem_stq_retry_e.bits.uop)
      clr_bsy_cf_bmap    (w) := stq(mem_stq_retry_e.bits.uop.stq_idx).bits.uop.cf_fu_bitmap.getOrElse(0.U(numModules.W))
      // [FPSTX] patch 81 (p4-73-author.md flagged item 8; owner approval 2026-10-19): Case A as on the three incoming
      // paths (cfStTx: secret data to a non-secret address, || [STADDR]).  This path read the uop's s_tx bit, which
      // is set only for AMOs (and AMOs never take clr_bsy here), so a store whose address RETRIED (TLB miss) after
      // its data landed lost its secret-data s_tx.  The entry's data bits are written at STD, before this path can
      // fire (it requires data.valid).  The uop bit stays ORed (nothing dropped).  Same entry select, one more LUT
      // input: 0 added levels.
      clr_bsy_cf_stx_r   (w) := stq(mem_stq_retry_e.bits.uop.stq_idx).bits.uop.cf_secret_transmission.getOrElse(false.B) ||
                               cfStTx(stq(mem_stq_retry_e.bits.uop.stq_idx).bits)   // Case A || [STADDR] ([FPSTX])
      clr_bsy_cf_sprop_r (w) := cfStSprop(stq(mem_stq_retry_e.bits.uop.stq_idx).bits)   // [WINMEM] [M7STSP]
      clr_bsy_cf_atk_r.foreach(_(w) := cfStAtk(stq(mem_stq_retry_e.bits.uop.stq_idx).bits))   // [M7STATK]
    }

    io.core.clr_bsy(w).valid          := clr_bsy_valid(w) &&
                               !IsKilledByBranch(io.core.brupdate, clr_bsy_brmask(w)) &&
                               !io.core.exception && !RegNext(io.core.exception) && !RegNext(RegNext(io.core.exception))
    io.core.clr_bsy(w).bits           := clr_bsy_rob_idx(w)
    io.core.clr_bsy_cf_bitmap(w)      := clr_bsy_cf_bmap(w)
    io.core.clr_bsy_cf_stx(w)        := clr_bsy_cf_stx_r(w)
 // [PROBE-STRIPPED 2026-09-10] A1CLR -- A1 is CLOSED; probe retired.
    io.core.clr_bsy_cf_sprop(w)      := clr_bsy_cf_sprop_r(w)
    io.core.clr_bsy_cf_atk.foreach(_(w) := clr_bsy_cf_atk_r.get(w))   // [M7STATK]
    // // [A1PROBE] downstream half: does the store's secret flag ever leave the LSU?
    // if (ENABLE_CF_DEBUG_PRINTF) {
      // when (io.core.clr_bsy(w).valid && io.core.cf_debug_lsu_enable) {
        // printf("\n[A1CLR] w=%d sprop=%d stx=%d\n",
          // w.U, clr_bsy_cf_sprop_r(w), clr_bsy_cf_stx_r(w))
      // }
    // }
  }

  val stdf_clr_bsy_valid    = RegInit(false.B)
  val stdf_clr_bsy_rob_idx  = Reg(UInt(robAddrSz.W))
  val stdf_clr_bsy_brmask   = Reg(UInt(maxBrCount.W))
  val stdf_clr_bsy_cf_bmap  = Reg(UInt(numModules.W))
  val stdf_clr_bsy_cf_stx   = RegInit(false.B)
  val stdf_clr_bsy_cf_sprop = RegInit(false.B)
  val stdf_clr_bsy_cf_atk   = if (ENABLE_IFT) Some(RegInit(false.B)) else None   // [M7STATK]
  stdf_clr_bsy_valid    := false.B
  stdf_clr_bsy_rob_idx  := 0.U
  stdf_clr_bsy_brmask   := 0.U
  stdf_clr_bsy_cf_bmap  := 0.U
  stdf_clr_bsy_cf_stx   := false.B
  stdf_clr_bsy_cf_sprop := false.B
  stdf_clr_bsy_cf_atk.foreach(_ := false.B)
  when (fired_stdf_incoming) {
    val s_idx = mem_stdf_uop.stq_idx
    stdf_clr_bsy_valid   := stq(s_idx).valid                 &&
                            stq(s_idx).bits.addr.valid       &&
                            !stq(s_idx).bits.addr_is_virtual &&
                            !stq(s_idx).bits.uop.is_amo      &&
                            !IsKilledByBranch(io.core.brupdate, mem_stdf_uop)
    stdf_clr_bsy_rob_idx := mem_stdf_uop.rob_idx
    stdf_clr_bsy_brmask  := GetNewBrMask(io.core.brupdate, mem_stdf_uop)
    stdf_clr_bsy_cf_bmap := stq(s_idx).bits.uop.cf_fu_bitmap.getOrElse(0.U(numModules.W))
    // [FPSTX] patch 81: Case A here too.  An FP store whose DATA arrives after its address clears busy on this path
    // (the STA path needs data.valid), and it read only the uop's s_tx bit (AMOs only), so an FP store of SECRET data
    // to a non-secret address was s_tx only when its data came first (t108 m153 / m154 order dependence).  The FP
    // data bit (data_is_secret) is written at fp_stdata.fire, one cycle before fired_stdf_incoming
    // reads the entry.  0 added levels (as above).
    stdf_clr_bsy_cf_stx  := stq(s_idx).bits.uop.cf_secret_transmission.getOrElse(false.B) ||
                            cfStTx(stq(s_idx).bits)   // Case A || [STADDR] ([FPSTX])
    stdf_clr_bsy_cf_sprop := cfStSprop(stq(s_idx).bits)   // [WINMEM] [M7STSP]
    stdf_clr_bsy_cf_atk.foreach(_ := cfStAtk(stq(s_idx).bits))   // [M7STATK]
  }



  io.core.clr_bsy(memWidth).valid           := stdf_clr_bsy_valid &&
                                    !IsKilledByBranch(io.core.brupdate, stdf_clr_bsy_brmask) &&
                                    !io.core.exception && !RegNext(io.core.exception) && !RegNext(RegNext(io.core.exception))
  io.core.clr_bsy(memWidth).bits            := stdf_clr_bsy_rob_idx
  io.core.clr_bsy_cf_bitmap(memWidth)       := stdf_clr_bsy_cf_bmap
  io.core.clr_bsy_cf_stx(memWidth)         := stdf_clr_bsy_cf_stx
  io.core.clr_bsy_cf_sprop(memWidth)       := stdf_clr_bsy_cf_sprop
  io.core.clr_bsy_cf_atk.foreach(_(memWidth) := stdf_clr_bsy_cf_atk.get)   // [M7STATK]



  // Task 2: Do LD-LD. ST-LD searches for ordering failures
  //         Do LD-ST search for forwarding opportunities
  // We have the opportunity to kill a request we sent last cycle. Use it wisely!

  // We translated a store last cycle
  val do_st_search = widthMap(w => (fired_stad_incoming(w) || fired_sta_incoming(w) || fired_sta_retry(w)) && !mem_tlb_miss(w))
  // We translated a load last cycle
  val do_ld_search = widthMap(w => ((fired_load_incoming(w) || fired_load_retry(w)) && !mem_tlb_miss(w)) ||
                     fired_load_wakeup(w))
  // We are making a local line visible to other harts
  val do_release_search = widthMap(w => fired_release(w))

  // Store addrs don't go to memory yet, get it from the TLB response
  // Load wakeups don't go through TLB, get it through memory
  // Load incoming and load retries go through both

  val lcam_addr  = widthMap(w => Mux(fired_stad_incoming(w) || fired_sta_incoming(w) || fired_sta_retry(w),
                                     RegNext(exe_tlb_paddr(w)),
                                     Mux(fired_release(w), RegNext(io.dmem.release.bits.address),
                                         mem_paddr(w))))
  val lcam_uop   = widthMap(w => Mux(do_st_search(w), mem_stq_e(w).bits.uop,
                                 Mux(do_ld_search(w), mem_ldq_e(w).bits.uop, NullMicroOp())))

  val lcam_mask  = widthMap(w => GenByteMask(lcam_addr(w), lcam_uop(w).mem_size))
  val lcam_st_dep_mask = widthMap(w => mem_ldq_e(w).bits.st_dep_mask)
  val lcam_is_release = widthMap(w => fired_release(w))
  val lcam_ldq_idx  = widthMap(w =>
                      Mux(fired_load_incoming(w), mem_incoming_uop(w).ldq_idx,
                      Mux(fired_load_wakeup  (w), RegNext(ldq_wakeup_idx),
                      Mux(fired_load_retry   (w), RegNext(ldq_retry_idx), 0.U))))
  val lcam_stq_idx  = widthMap(w =>
                      Mux(fired_stad_incoming(w) ||
                          fired_sta_incoming (w), mem_incoming_uop(w).stq_idx,
                      Mux(fired_sta_retry    (w), RegNext(stq_retry_idx), 0.U)))

  val can_forward = WireInit(widthMap(w =>
    Mux(fired_load_incoming(w) || fired_load_retry(w), !mem_tlb_uncacheable(w),
      !ldq(lcam_ldq_idx(w)).bits.addr_is_uncacheable)))

  // Mask of stores which we conflict on address with
  val ldst_addr_matches    = WireInit(widthMap(w => VecInit((0 until numStqEntries).map(x=>false.B))))
  // Mask of stores which we can forward from
  val ldst_forward_matches = WireInit(widthMap(w => VecInit((0 until numStqEntries).map(x=>false.B))))

  // [MEMORD] default: no edge this cycle.  Overridden at the two order_fail sites below.
  for (w <- 0 until memWidth) {
    io.core.cf_memord_upd(w).valid := false.B
    io.core.cf_memord_upd(w).bits  := DontCare
    io.core.cf_memord_upd(w).bits.infl_type.foreach(_ := INFL_MEM_ORDER.U)   // [RDEDGE] patch 80 (the sites never set it)
  }
  // [MEMORDATK] patch 42 (DF-memord-atk, r116/r117 destination rule for attacker influence): the
  // order-fail producer is attacker-influenced when it is attacker-domain OR its address/data is
  // attacker-tainted (data-flow or window kind) OR (stores) its dispatch-time attacker summary is set.
  // The edge lands only on the squashed load (terminal: a flushed load feeds no producer test), so
  // reading the summary here cannot chain.  Per entry: one LUT on entry flops, ahead of the same
  // lcam_stq_idx / lcam_ldq_idx entry mux the domain and s_prop bits already take (0 added levels).
  // Boundary (42b, review p4-42 m4): data_is_atk is written when the store DATA arrives (STD, the
  // [A1STD] capture above).  A victim store whose attacker-tainted data arrives AFTER its own STA
  // search has fired raises no atk through data_is_atk: the squash is caused by the late ADDRESS,
  // whose taint (addr_is_atk, written at the TLB stage before the search) and the dispatch summary
  // cf_dis_atk are both in place.  A boundary, not a defect.
  val stq_atk_infl = VecInit(stq.map(e => e.bits.cf_dis_atk.getOrElse(false.B) ||
                                          e.bits.addr_is_atk.getOrElse(false.B)  || e.bits.data_is_atk.getOrElse(false.B) ||
                                          e.bits.addr_is_watk.getOrElse(false.B) || e.bits.data_is_watk.getOrElse(false.B)))
  // [U2MO] variant for RULES.md E/U2 = YES (record summary also for MEM_ORDER): the searcher's attacker record summary
  // (one leaf, same entry mux, 0 levels).  The store side already carries cf_dis_atk (its dispatch summary); the
  // 10-19 "drop cf_dis_atk" question in U2 is not touched here.
  val ldq_atk_infl = VecInit(ldq.map(e => e.bits.addr_is_atk.getOrElse(false.B) || e.bits.uop.cf_opnd_watk.getOrElse(false.B) ||
                                          e.bits.uop.cf_attacker_influence.getOrElse(false.B)))
  val st_atk_sel   = widthMap(w => stq_atk_infl(lcam_stq_idx(w)))   // one entry mux per way, shared by every LDQ entry
  val ld_atk_sel   = widthMap(w => ldq_atk_infl(lcam_ldq_idx(w)))
  // [L9LDSEC] lean58 (Fable audit L9; RULES A U2): the LD-LD MEM_ORDER searcher's SECRET label from its live LDQ
  // entry (written at the TLB stage, like ld_atk_sel), not from lcam_uop (the pre-write snapshot): s_acc || s_prop,
  // or a secret-derived address (such a load is s_prop by RULES D1 l.16).  One entry mux per way: 0 levels on the
  // order-fail path (it feeds only the memord message).  RECORD CHANGE.
  val ldq_sec_infl = VecInit(ldq.map(e => e.bits.uop.cf_secret_access.getOrElse(false.B) ||
                                          e.bits.uop.cf_secret_propagation.getOrElse(false.B) ||
                                          e.bits.addr_is_secret.getOrElse(false.B)))
  val ld_sec_sel   = widthMap(w => ldq_sec_infl(lcam_ldq_idx(w)))
  // [B60LDB] bundle60 (B): per way, the OLDER LDQ entries that blocked this load search (the LD-LD branch below: an
  // older load to the same DW that has not executed or was nacked kills the searcher and clears can_forward).  Read
  // only by the [RDEDGE] STL_PARTIAL edge (names that load as the producer).  A copy of the kill term into new
  // wires: 0 levels on the kill / can_forward path.  IFT off: None (not elaborated).
  val cf_ld_blk = if (ENABLE_IFT) Some(WireInit(widthMap(w => VecInit((0 until numLdqEntries).map(x => false.B))))) else None
  val failed_loads     = WireInit(VecInit((0 until numLdqEntries).map(x=>false.B))) // Loads which we will report as failures (throws a mini-exception)
  val nacking_loads    = WireInit(VecInit((0 until numLdqEntries).map(x=>false.B))) // Loads which are being nacked by dcache in the next stage

  val s1_executing_loads = RegNext(s0_executing_loads)
  val s1_set_execute     = WireInit(s1_executing_loads)

  val mem_forward_valid   = Wire(Vec(memWidth, Bool()))
  val mem_forward_ldq_idx = lcam_ldq_idx
  val mem_forward_ld_addr = lcam_addr
  val mem_forward_stq_idx = Wire(Vec(memWidth, UInt(log2Ceil(numStqEntries).W)))

  val wb_forward_valid    = RegNext(mem_forward_valid)
  val wb_forward_ldq_idx  = RegNext(mem_forward_ldq_idx)
  val wb_forward_ld_addr  = RegNext(mem_forward_ld_addr)
  val wb_forward_stq_idx  = RegNext(mem_forward_stq_idx)

  for (i <- 0 until numLdqEntries) {
    val l_valid = ldq(i).valid
    val l_bits  = ldq(i).bits
    val l_addr  = ldq(i).bits.addr.bits
    val l_mask  = GenByteMask(l_addr, l_bits.uop.mem_size)

    val l_forwarders      = widthMap(w => wb_forward_valid(w) && wb_forward_ldq_idx(w) === i.U)
    val l_is_forwarding   = l_forwarders.reduce(_||_)
    val l_forward_stq_idx = Mux(l_is_forwarding, Mux1H(l_forwarders, wb_forward_stq_idx), l_bits.forward_stq_idx)


    val block_addr_matches = widthMap(w => lcam_addr(w) >> blockOffBits === l_addr >> blockOffBits)
    val dword_addr_matches = widthMap(w => block_addr_matches(w) && lcam_addr(w)(blockOffBits-1,3) === l_addr(blockOffBits-1,3))
    val mask_match   = widthMap(w => (l_mask & lcam_mask(w)) === l_mask)
    val mask_overlap = widthMap(w => (l_mask & lcam_mask(w)).orR)

    // Searcher is a store
    for (w <- 0 until memWidth) {

      when (do_release_search(w) &&
            l_valid              &&
            l_bits.addr.valid    &&
            block_addr_matches(w)) {
        // This load has been observed, so if a younger load to the same address has not
        // executed yet, this load must be squashed
        ldq(i).bits.observed := true.B
      } .elsewhen (do_st_search(w)                                                                                                &&
                   l_valid                                                                                                        &&
                   l_bits.addr.valid                                                                                              &&
                   (l_bits.executed || l_bits.succeeded || l_is_forwarding)                                                       &&
                   !l_bits.addr_is_virtual                                                                                        &&
                   l_bits.st_dep_mask(lcam_stq_idx(w))                                                                            &&
                   dword_addr_matches(w)                                                                                          &&
                   mask_overlap(w)) {

        val forwarded_is_older = IsOlder(l_forward_stq_idx, lcam_stq_idx(w), l_bits.youngest_stq_idx)
        // We are older than this load, which overlapped us.
        when (!l_bits.forward_std_val || // If the load wasn't forwarded, it definitely failed
          ((l_forward_stq_idx =/= lcam_stq_idx(w)) && forwarded_is_older)) { // If the load forwarded from us, we might be ok
          ldq(i).bits.order_fail := true.B
          failed_loads(i)        := true.B
          // corefuzzing: memory ordering violation (cross-domain OR secret-tainted store)
          // [MEMORDATK] ...OR attacker-influenced store (ssbx: a victim store whose address the attacker delayed)
          val st_atk_infl = st_atk_sel(w)
          when (stq(lcam_stq_idx(w)).bits.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= l_bits.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) ||
                stq(lcam_stq_idx(w)).bits.uop.cf_secret_propagation.getOrElse(false.B) ||
                stq(lcam_stq_idx(w)).bits.uop.cf_secret_access.getOrElse(false.B) || st_atk_infl) {   // [STSACC] patch 52, [SACCU]
            val stq_infl_uop = stq(lcam_stq_idx(w)).bits.uop
            // [STSACC] patch 52 / [SACCU]: the store's s_acc = its uop cf_secret_access (written at its TLB stage, one
            // cycle before this search reads it); same entry mux as the bit it replaces, 0 levels.
            val stq_infl_sacc = stq(lcam_stq_idx(w)).bits.uop.cf_secret_access.getOrElse(false.B)
            // [MEMORD] The edge goes to the ROB entry, where it lands on the squashed load's
            // [FLUSH] record.  Task 3.2: the LDQ-copy addInfluencer is gone (its result was
            // destroyed by the order-fail flush anyway, doc 53 B1; spec 5.2.2).
            io.core.cf_memord_upd(w).valid             := true.B
            io.core.cf_memord_upd(w).bits.rob_idx      := l_bits.uop.rob_idx
            io.core.cf_memord_upd(w).bits.op_count_id  := l_bits.uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
            io.core.cf_memord_upd(w).bits.prod_op_count := stq_infl_uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
            // [ATKVIC] stall-edge audit D5 (the F1 [VICGATE] rule): atk only on a VICTIM squashed load (l_bits is ldq(i),
            // whose domain bit is already a leaf of this when's LUT): 0 FF, 0 LUT, 0 levels.  IFT off: domain 0 (unchanged).
            io.core.cf_memord_upd(w).bits.is_atk       := (stq_infl_uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 1.U || st_atk_infl) &&
                                                          l_bits.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U   // [MEMORDATK] [ATKVIC]
            io.core.cf_memord_upd(w).bits.is_secret    := stq_infl_sacc || stq_infl_uop.cf_secret_propagation.getOrElse(false.B)
          }
        }
      } .elsewhen (do_ld_search(w)            &&
                   l_valid                    &&
                   l_bits.addr.valid          &&
                   !l_bits.addr_is_virtual    &&
                   dword_addr_matches(w)      &&
                   mask_overlap(w)) {
        val searcher_is_older = IsOlder(lcam_ldq_idx(w), i.U, ldq_head)
        when (searcher_is_older) {
          when ((l_bits.executed || l_bits.succeeded || l_is_forwarding) &&
                !s1_executing_loads(i) && // If the load is proceeding in parallel we don't need to kill it
                l_bits.observed) {        // Its only a ordering failure if the cache line was observed between the younger load and us
            ldq(i).bits.order_fail := true.B
            failed_loads(i)        := true.B
            // corefuzzing: LD-LD ordering violation (cross-domain OR secret-tainted searcher)
            // [MEMORDATK] ...OR a searcher at an attacker-tainted address (its LDQ entry, written at the
            // TLB stage; the lcam_uop copy is the pre-write snapshot with the attacker bit zeroed)
            val ld_atk_infl = ld_atk_sel(w)
            when (lcam_uop(w).cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= l_bits.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) ||
                  ld_sec_sel(w) || ld_atk_infl) {   // [L9LDSEC] (was lcam_uop's s_prop || s_acc)
              // [MEMORD] same routing as site 1 (Task 3.2: LDQ-copy addInfluencer removed).  This site additionally
              // requires l_bits.observed, set by a release search (do_release_search: the D$ releases the line, e.g.
              // a dirty-victim writeback on an ordinary eviction), so it IS reachable in a single-core sim: t48 hits
              // it (deferred-findings D-17; 42b).
              io.core.cf_memord_upd(w).valid             := true.B
              io.core.cf_memord_upd(w).bits.rob_idx      := l_bits.uop.rob_idx
              io.core.cf_memord_upd(w).bits.op_count_id  := l_bits.uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
              io.core.cf_memord_upd(w).bits.prod_op_count := lcam_uop(w).cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
              io.core.cf_memord_upd(w).bits.is_atk       := (lcam_uop(w).cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 1.U || ld_atk_infl) &&
                                                            l_bits.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U   // [MEMORDATK] [ATKVIC] D5
              io.core.cf_memord_upd(w).bits.is_secret    := ld_sec_sel(w)   // [L9LDSEC]
            }
          }
        } .elsewhen (lcam_ldq_idx(w) =/= i.U) {
          // The load is older, and either it hasn't executed, it was nacked, or it is ignoring its response
          // we need to kill ourselves, and prevent forwarding
          val older_nacked = nacking_loads(i) || RegNext(nacking_loads(i))
          when (!(l_bits.executed || l_bits.succeeded) || older_nacked) {
            s1_set_execute(lcam_ldq_idx(w))    := false.B
            io.dmem.s1_kill(w)                 := RegNext(dmem_req_fire(w))
            can_forward(w)                     := false.B
            cf_ld_blk.foreach(_(w)(i)          := true.B)   // [B60LDB]
          }
        }
      }
    }
  }

  for (i <- 0 until numStqEntries) {
    val s_addr = stq(i).bits.addr.bits
    val s_uop  = stq(i).bits.uop
    val dword_addr_matches = widthMap(w =>
                             ( stq(i).bits.addr.valid      &&
                              !stq(i).bits.addr_is_virtual &&
                              (s_addr(corePAddrBits-1,3) === lcam_addr(w)(corePAddrBits-1,3))))
    val write_mask = GenByteMask(s_addr, s_uop.mem_size)
    for (w <- 0 until memWidth) {
      when (do_ld_search(w) && stq(i).valid && lcam_st_dep_mask(w)(i)) {
        when (((lcam_mask(w) & write_mask) === lcam_mask(w)) && !s_uop.is_fence && !s_uop.is_amo && dword_addr_matches(w) && can_forward(w))
        {
          ldst_addr_matches(w)(i)            := true.B
          ldst_forward_matches(w)(i)         := true.B
          io.dmem.s1_kill(w)                 := RegNext(dmem_req_fire(w))
          s1_set_execute(lcam_ldq_idx(w))    := false.B
        }
          .elsewhen (((lcam_mask(w) & write_mask) =/= 0.U) && dword_addr_matches(w))
        {
          ldst_addr_matches(w)(i)            := true.B
          io.dmem.s1_kill(w)                 := RegNext(dmem_req_fire(w))
          s1_set_execute(lcam_ldq_idx(w))    := false.B
        }
          .elsewhen (s_uop.is_fence || s_uop.is_amo)
        {
          ldst_addr_matches(w)(i)            := true.B
          io.dmem.s1_kill(w)                 := RegNext(dmem_req_fire(w))
          s1_set_execute(lcam_ldq_idx(w))    := false.B
        }
      }
    }
  }

  // Set execute bit in LDQ
  for (i <- 0 until numLdqEntries) {
    when (s1_set_execute(i)) { ldq(i).bits.executed := true.B }
  }

  // Find the youngest store which the load is dependent on
  val forwarding_age_logic = Seq.fill(memWidth) { Module(new ForwardingAgeLogic(numStqEntries)) }
  for (w <- 0 until memWidth) {
    forwarding_age_logic(w).io.addr_matches    := ldst_addr_matches(w).asUInt
    forwarding_age_logic(w).io.youngest_st_idx := lcam_uop(w).stq_idx
  }
  val forwarding_idx = widthMap(w => forwarding_age_logic(w).io.forwarding_idx)

  // Forward if st-ld forwarding is possible from the writemask and loadmask
  mem_forward_valid       := widthMap(w =>
                                  (ldst_forward_matches(w)(forwarding_idx(w))        &&
                                 !IsKilledByBranch(io.core.brupdate, lcam_uop(w))    &&
                                 !io.core.exception && !RegNext(io.core.exception)))
  mem_forward_stq_idx     := forwarding_idx

  // Avoid deadlock with a 1-w LSU prioritizing load wakeups > store commits
  // On a 2W machine, load wakeups and store commits occupy separate pipelines,
  // so only add this logic for 1-w LSU
  if (memWidth == 1) {
    // Wakeups may repeatedly find a st->ld addr conflict and fail to forward,
    // repeated wakeups may block the store from ever committing
    // Disallow load wakeups 1 cycle after this happens to allow the stores to drain
    when (RegNext(ldst_addr_matches(0).reduce(_||_) && !mem_forward_valid(0))) {
      block_load_wakeup := true.B
    }

    // If stores remain blocked for 15 cycles, block load wakeups to get a store through
    val store_blocked_counter = Reg(UInt(4.W))
    when (will_fire_store_commit(0) || !can_fire_store_commit(0)) {
      store_blocked_counter := 0.U
    } .elsewhen (can_fire_store_commit(0) && !will_fire_store_commit(0)) {
      store_blocked_counter := Mux(store_blocked_counter === 15.U, 15.U, store_blocked_counter + 1.U)
    }
    when (store_blocked_counter === 15.U) {
      block_load_wakeup := true.B
    }
  }


  // [RDEDGE] patch 80 (CLEAN.md section 6 "reader-side gaps"; SPEC-P1 classes 20 / 21; owner approval 2026-10-19):
  // two reader-side edges the record had no class for, both TIMING edges (no value taint), both keyed by the
  // producer's op_count and emitted only when the producer is tagged (its own label, CLEAN.md R2: not gated on the
  // reader's domain):
  //   STL_PARTIAL (v1 code 29): a load search found an older store that overlaps it but cannot forward (partial byte
  //     overlap, a fence / AMO, or an uncacheable full overlap: the three ldst_addr_matches cases that are not a
  //     forward), so the load is killed (s1_kill) and replayed until the store drains: its latency is a function of
  //     that store's ADDRESS.  Tag = OR over the matching stores of the producer RECORD summary ([F21REC] below;
  //     was the address label only); key = the youngest matching store (forwarding_idx).  It fires on every
  //     replaying search (one column entry per row, rewritten).  [B60LDB] a search that an OLDER pending load to
  //     the same DW blocked (can_forward cleared by the LD-LD branch) names that load instead (below).
  //   DC_BANKCONF (v1 code 30): the D$ nacked the load because an older lane read the same data-array bank at
  //     another row (dcache.scala cf_bank_nack, the label and op_count of that lane's access).  Loads only (a store
  //     nack is retried after commit, when its ROB row is gone).
  // Transport: a dedicated op_count-guarded ROB port (rob.scala cf_lsu_rdedge_upd, sidecar C_memord column
  // memWidth), NOT the MEM_ORDER ports, so no existing port gains a mux.  [B61LDQ] bundle61 (replaces bundle60's
  // per-source holding registers, [B60ARB]): each event is staged one cycle, then held PER LOAD in a pending entry
  // beside its LDQ entry; one edge per cycle goes to the port, oldest load first; the port is driven from flops.
  // Debug-only [RDELOST] / [RDELATE] count edges whose load left the LDQ before the edge reached the ROB.  Details
  // at [B61LDQ] below.  Levels: every term is a new endpoint (stage / pending / port registers); the label ORs run
  // beside the existing match / nack logic and feed only them.  IFT off: not elaborated.
  if (ENABLE_IFT) {
    def rdeUpd(v: Bool, u: MicroOp, oc: UInt, atk: Bool, sec: Bool, ty: Int): Valid[CF_MemOrdUpdate] = {
      val r = Wire(Valid(new CF_MemOrdUpdate))
      r.valid              := v
      r.bits.rob_idx       := u.rob_idx
      r.bits.op_count_id   := u.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
      r.bits.prod_op_count := oc.pad(inflKeyWidthCF)(inflKeyWidthCF - 1, 0)
      r.bits.is_atk        := atk
      r.bits.is_secret     := sec
      r.bits.infl_type.get := ty.U(inflTypeWidthCF.W)
      r
    }
    // [F21REC] Fable review 2026-10-23 F2-1, owner answer "Use the producer record" (RULES.md C): the STL_PARTIAL
    // labels are the matching STQ entry's RECORD summary as known at the search, exactly like STL_FORWARD after 07f
    // M6 ([M6STLATK]) and the STQ-head labels ([RECEDGE]); address taint alone is not enough.  Per entry:
    //   sec = s_acc (uop.cf_secret_access, [SACCU]) || s_prop (uop.cf_secret_propagation: dispatch summary, the
    //         TLB-stage issue s_prop and the STD data taint) || addr_is_secret (part of the store's s_prop record,
    //         [M7STSP] cfStSprop) || uop.cf_win_sec (the commit-time record s_acc || s_prop, written at commit:
    //         a committed store still waiting to drain carries its FINAL record, incl. retroactive s_prop)
    //   atk = cf_dis_atk (dispatch attacker summary: attacker packet / edges / window) || uop.cf_attacker_influence
    //         (as [M6STLATK]) || addr_is_atk (ownership-seeded for attacker-domain stores, N3) || data_is_atk (the
    //         data operand's attacker taint, as the MEM_ORDER stq_atk_infl) || uop.cf_win_atk (commit-time record)
    // A fence / AMO in the STQ takes its own record the same way.  The fire condition (sec || atk) follows the new
    // labels: a store whose record is tagged but whose address is clean now produces the edge.  The old address
    // leaves stay as members of the record (superset: no edge of the old design is lost).
    // Levels: each label is a 4/5-input OR of STQ entry flops (one LUT, off the ldst_addr_matches path); the
    // m(i) & lab(i) AND-OR tree keeps its shape, so the match -> holding-register path gains 0 levels.
    // Cost [inf]: ~2 x 40 LUT (one per entry per label), 0 FF.  IFT off: inside if (ENABLE_IFT), not elaborated.
    val stq_lab_sec = VecInit(stq.map(e => e.bits.uop.cf_secret_access.get || e.bits.uop.cf_secret_propagation.get ||
                                           e.bits.addr_is_secret.get || e.bits.uop.cf_win_sec.get)).asUInt
    val stq_lab_atk = VecInit(stq.map(e => e.bits.cf_dis_atk.get || e.bits.uop.cf_attacker_influence.get ||
                                           e.bits.addr_is_atk.get || e.bits.data_is_atk.get ||
                                           e.bits.uop.cf_win_atk.get)).asUInt
    // [B60LDB] bundle60 (B), owner 2026-10-24 (FINDINGS-open-rebase59 item 1b: "keep the edge but name the older
    // load as producer", its record summary for the labels as RULES.md C F2-1 "producer record"): an OLDER load to
    // the same DW that is still pending (not executed, or nacked) kills the searcher and clears can_forward, so a
    // fully overlapping store that could forward falls into the partial branch and set ldst_addr_matches: the edge
    // fired naming that store although the delay is the LD-LD order.  For a search with such a blocker
    // (cf_ld_blk) the edge names the older load:
    //   sec = OR over the blockers of ldq_sec_infl (s_acc || s_prop || secret-derived address, [L9LDSEC])
    //   atk = OR over the blockers of ldq_atk_infl (attacker address || operand window || attacker record, [U2MO];
    //         the same LDQ record labels as the [RECEDGE] LDQ head)
    //   key = the op_count of one TAGGED blocker (lowest LDQ index among the tagged blockers: each blocker alone
    //         keeps the searcher from completing, so any tagged one is a true producer).
    // The store record ([F21REC]) labels the edge only when no older load blocks.  A store that ALSO blocks (a real
    // partial overlap) is named by the next replay after the older load executes (that search has no blocker); if it
    // drained first it did not delay the load.  The fire condition keeps the store match (m.orR): no new trigger.
    // Levels: cf_ld_blk(i) is a copy of the LD-LD kill term, which also feeds can_forward and so arrives before the
    // store-side match / forwarding_idx terms; the three 2:1 selects add 1 LUT after the store-side AND-OR / oc mux,
    // into the [B61LDQ] stage register (a new endpoint, D input only).  Cost [inf] per way: 40-bit one-hot priority
    // over the tagged blockers (~45 LUT, the per-entry AND folds into it), 10-bit 40:1 AND-OR of the LDQ op_count
    // (~140 LUT), 2 x 40-bit label OR (~28 LUT), 12 select LUT; 0 FF.
    val ld_lab_sec = ldq_sec_infl.asUInt
    val ld_lab_atk = ldq_atk_infl.asUInt
    val stp = (0 until memWidth).map { w =>
      val m    = ldst_addr_matches(w).asUInt
      val lb   = cf_ld_blk.get(w).asUInt
      val lbs  = lb & ld_lab_sec
      val lba  = lb & ld_lab_atk
      val lsel = PriorityEncoderOH(lbs | lba)
      val l_oc = Mux1H(lsel, ldq.map(_.bits.uop.cf_op_count_id.get(inflKeyWidthCF - 1, 0)))
      val blk  = lb.orR
      val sec  = Mux(blk, lbs.orR, (m & stq_lab_sec).orR)
      val atk  = Mux(blk, lba.orR, (m & stq_lab_atk).orR)
      val oc   = Mux(blk, l_oc, stq(forwarding_idx(w)).bits.uop.cf_op_count_id.get(inflKeyWidthCF - 1, 0))
      rdeUpd(do_ld_search(w) && m.orR && !mem_forward_valid(w) && (sec || atk) &&
             !IsKilledByBranch(io.core.brupdate, lcam_uop(w)) && !io.core.exception && !RegNext(io.core.exception),
             lcam_uop(w), oc, atk, sec, INFL_STL_PARTIAL)
    }
    val dcb = (0 until memWidth).map { w =>
      val n = io.dmem.cf_bank_nack.get(w)
      rdeUpd(io.dmem.nack(w).valid && !io.dmem.nack(w).bits.is_hella && io.dmem.nack(w).bits.uop.uses_ldq && n.valid &&
             !IsKilledByBranch(io.core.brupdate, io.dmem.nack(w).bits.uop) && !io.core.exception,
             io.dmem.nack(w).bits.uop, n.bits.oc, n.bits.atk, n.bits.sec, INFL_DC_BANKCONF)
    }
    // [B60ARB] the DC_BANKCONF sources exist only with a banked D$: with numDCacheBanks = 1 the data array is
    // BoomDuplicatedDataArray, whose nacks are constant false (dcache.scala), so their stage registers / type bits would be
    // dead flops that firtool cannot fold (a valid register that only clears itself).  Same edges, no dead state.
    val dcbOn = boomParams.numDCacheBanks > 1
    val src  = (if (dcbOn) dcb else Nil) ++ stp
    // the LDQ index of each source's load (both edge kinds land on a load: dcb requires uses_ldq, stp is a load search)
    val srcLdq = (if (dcbOn) (0 until memWidth).map(w => io.dmem.nack(w).bits.uop.ldq_idx) else Nil) ++
                 (0 until memWidth).map(w => lcam_ldq_idx(w))
    // [B61LDQ] bundle61, owner 2026-10-24 12:0x ("Back to author, rebuild"; bundle60 left [RDELOST] nlive = 9 on t50,
    // 26 on ift_all, every one k = 1).  Root cause in bundle60's [B60ARB]: one holding register per SOURCE (lane).  A
    // loop with several loads to one DW (t50 0x80001c4c-0x80001c80: four lw -20(s0) and a sw -20(s0) per iteration,
    // more than one iteration in flight) has those loads replaying on both lanes, so a lane sees a DIFFERENT row on
    // consecutive searches: the per-source de-dup never matches, lane 0 is "urgent" every cycle and wins the port
    // every cycle (fixed priority), lane 1's waiting edge starves, and when lane 1 then gets its next event both are
    // urgent and k = 0 wins: lane 1's held edge is replaced.  t50 [RDELOST] 1: row oc 25188 (lw 0x80001c68), ty29
    // naming the older lw oc 25171; its commit record carries no ty29 at all.  Two sources sharing one port per cycle
    // cannot be lossless while one lane alone fills the port; the loss is structural, not a priority detail.
    // Fix: hold the pending edge PER LOAD (per LDQ entry) instead of per lane, and send the OLDEST pending load first:
    //   stage   : stg(k) := src(k) every cycle, as bundle60 (the source cone ends in this D input: 0 levels).  Only the
    //             LDQ index, producer op_count and labels are kept (the row and op_count are read from the LDQ entry).
    //   table   : per LDQ entry {pend, prod op_count, sec, atk (, bank-conflict type bit with a banked D$)}.  A staged
    //             event writes ITS load's entry: pend := 1, producer := the new one; labels := the new labels, ORed
    //             with the entry's if it still holds an unsent edge (sticky: no label is dropped; the column keeps
    //             one entry per row anyway, newest producer).  Two rows never share an entry, so no event of one load
    //             can replace another load's edge.
    //   grant   : the oldest pending entry whose load is valid, in LDQ age order from ldq_head (AgePriorityEncoder).
    //             The load at the LDQ head is the next load to commit, so the edge most at risk is always sent first; a
    //             younger load cannot commit while an older one is pending.  pend := 0 on grant (a new event in the
    //             same cycle re-sets it with the new content).  A load that leaves the LDQ (commit, branch kill,
    //             exception flush) cannot be granted (ldq.valid), and its pend is cleared the next cycle, before the
    //             entry can be reallocated (a freed entry is enqueued one cycle later at the earliest).
    //   port    : registered (out); rob_idx / op_count_id from the granted LDQ entry's uop (the same load).
    // Residual (counted, debug): an edge that reaches the ROB in or after its row's commit cycle (the commit record
    // reads the sidecar column asynchronously in that cycle).  Timing: event t, table t+2, port t+3, column visible
    // t+4; the load was killed by the event and must replay, return data and write back before it commits (>= t+5).
    // Levels: stage D input as bundle60 (0).  Table: flop (stg) -> LDQ-index decode (1) -> per-entry D mux (1).  Grant:
    // flop (pend, ldq.valid) -> age priority (~4) -> 40:1 read (3) -> out D; all flop-to-flop new endpoints, the ROB
    // port input starts at a flop.  No existing path changes.
    // Cost [inf]: table 40 x 13 = 520 FF (+40 with a banked D$); stage 2 x 19 = 38 FF; port 37 FF (bundle60: 140 FF of
    // stage / holding / port, removed); LUT: ~520 table D, ~100 age priority, ~500 port read (36 bits x 40:1), ~20 misc.
    // No stall, nack, replay or queue: one entry per existing LDQ entry (the per-LDQ precedent: [NACKRES] {v, pc}).
    val stg_v   = src.map(s => RegNext(s.valid, false.B))
    val stg     = src.map(s => RegNext(s.bits))
    val stg_ldq = srcLdq.map(i => RegNext(i))
    val isBank  = (0 until src.length).map(k => dcbOn && k < memWidth)   // src order: dcb lanes first when banked
    val pend    = RegInit(VecInit(Seq.fill(numLdqEntries)(false.B)))
    val p_oc    = Reg(Vec(numLdqEntries, UInt(inflKeyWidthCF.W)))
    val p_sec   = Reg(Vec(numLdqEntries, Bool()))
    val p_atk   = Reg(Vec(numLdqEntries, Bool()))
    val p_bank  = if (dcbOn) Some(Reg(Vec(numLdqEntries, Bool()))) else None
    val elig    = (0 until numLdqEntries).map(i => pend(i) && ldq(i).valid)
    val rdeTabSec = Wire(Vec(numLdqEntries, Bool()))   // [LDQRDE] D4: this cycle's staged table events, per entry
    val rdeTabAtk = Wire(Vec(numLdqEntries, Bool()))
    val gv      = elig.reduce(_ || _)
    val gidx    = AgePriorityEncoder(elig, ldq_head)
    for (i <- 0 until numLdqEntries) {
      val hits = (0 until src.length).map(k => stg_v(k) && stg_ldq(k) === i.U)
      val gnti = gv && gidx === i.U
      val keep = pend(i) && !gnti   // an unsent edge of this load: merge its labels
      when (hits.reduce(_ || _)) {
        pend(i)  := true.B
        p_oc(i)  := PriorityMux(hits.reverse, stg.reverse.map(_.prod_op_count))   // the later source wins
        p_sec(i) := hits.zip(stg).map { case (h, s) => h && s.is_secret }.reduce(_ || _) || (keep && p_sec(i))
        p_atk(i) := hits.zip(stg).map { case (h, s) => h && s.is_atk    }.reduce(_ || _) || (keep && p_atk(i))
        p_bank.foreach(b => b(i) := PriorityMux(hits.reverse, isBank.reverse.map(_.B)))
      } .elsewhen (gnti || !ldq(i).valid) {
        pend(i)  := false.B
      }
      rdeTabSec(i) := hits.zip(stg).map { case (h, s) => h && s.is_secret }.reduce(_ || _)   // [LDQRDE] (shares the
      rdeTabAtk(i) := hits.zip(stg).map { case (h, s) => h && s.is_atk    }.reduce(_ || _)   //  p_sec / p_atk terms)
    }
    // [V3EDGE] SP-20 (record v3 design 6.2 / 6.2.1, V3-D11, N-D3 a): LSU_LANE (0x24) and DC_PORT_BLOCK (0x25),
    // same cycle, on this port, drop-and-count.  Each edge lands on the DELAYED instruction's ROB row: {type, key =
    // the winner's op_count[11:0] or 0, atk / sec = the winner's record summary} (RULES A "record summary for
    // both").  No hold, stage or culprit register (V3-D14): the terms end in the port register `out`, which they
    // take only when the [B61LDQ] grant is idle (gv = 0), so that table's timing is unchanged; a candidate that
    // does not get the port is dropped (counted by the SP-22 debug prints).
    //   LSU_LANE: lane w, a retry / wakeup / commit candidate X with can_fire && !will_fire lost to the FIRST
    //   higher-priority candidate Y of the same lane (lsu_sched order) that fired and needs one of X's resources.
    //   Y's summary: atk = attacker domain || cf_attacker_influence, sec = s_acc || s_prop (the ISSUE_CONTENTION
    //   winner rule); Y without a uop (release, hellacache) -> key 0, labels 0.  A committed store-commit has no
    //   live row: no edge.
    //   DC_PORT_BLOCK: io.dmem.cf_port_blk.valid (the D$ refused the vector); every lane whose request has a live
    //   row (not hellacache, not a committed store) is a loser; key / labels from io.dmem.cf_port_blk.bits.
    def v3eWin(u: MicroOp): (UInt, Bool, Bool) =
      (u.cf_op_count_id.get, u.cf_domain_id.get === 1.U || u.cf_attacker_influence.get,
       u.cf_secret_access.get || u.cf_secret_propagation.get)
    def v3eLive(u: MicroOp): Bool = !IsKilledByBranch(io.core.brupdate, u) && !io.core.exception
    // [V3EWIN] stall-edge audit D7: a winner from an LSQ ENTRY takes the entry's record summary, not its uop copy alone:
    //   LDQ (load_retry / load_wakeup): atk = domain || uop atk (dispatch seed, [CNTDLSQ] issue bits) || addr_is_atk (the
    //        record has it through tlb_ld_atk, as the [RECEDGE] LDQ head); sec = s_acc || s_prop || addr_is_secret ([SECDEST]).
    //   STQ (sta_retry / store_commit): atk = domain || cf_dis_atk || addr_is_atk || data_is_atk (the store's record, rob
    //        [M7STATK] / [CNTDLSQ]; the uop copy is zeroed at enqueue); sec = s_acc || s_prop (incl. STD data, [M7STSP]).
    // Entry flops through the existing winner Mux1H into the port register: ~4 LUT inputs per winner, 0 FF, 0 levels.
    def v3eWinLd(e: LDQEntry): (UInt, Bool, Bool) =
      (e.uop.cf_op_count_id.get, e.uop.cf_domain_id.get === 1.U || e.uop.cf_attacker_influence.get || e.addr_is_atk.get,
       e.uop.cf_secret_access.get || e.uop.cf_secret_propagation.get || e.addr_is_secret.get)
    def v3eWinSt(e: STQEntry): (UInt, Bool, Bool) =
      (e.uop.cf_op_count_id.get, e.uop.cf_domain_id.get === 1.U || e.cf_dis_atk.get || e.addr_is_atk.get || e.data_is_atk.get,
       e.uop.cf_secret_access.get || e.uop.cf_secret_propagation.get)
    // [ATKVIC] stall-edge audit D5 (the F1 [VICGATE] rule): LSU_LANE / DC_PORT_BLOCK atk only on a VICTIM loser (its uop
    // domain bit is a flop leaf of the same label LUT): 0 FF, 0 LUT, 0 levels.
    def v3eVic(u: MicroOp): Bool = u.cf_domain_id.get === 0.U
    // (edge, kind: 1 DC_PORT_BLOCK / 0 LSU_LANE, lane, LSU_LANE candidate index 8..11 or 0)
    val v3eCands = scala.collection.mutable.ArrayBuffer[(Valid[CF_MemOrdUpdate], Int, Int, Int)]()
    val v3eBlk   = io.dmem.cf_port_blk.get
    for (w <- 0 until memWidth) {
      val u  = dmem_req(w).bits.uop
      val cm = will_fire_store_commit(w) && stq_commit_e.bits.committed
      v3eCands += ((rdeUpd(v3eBlk.valid && dmem_req(w).valid && !dmem_req(w).bits.is_hella && !cm && v3eLive(u),
                           u, v3eBlk.bits.oc, v3eBlk.bits.atk && v3eVic(u), v3eBlk.bits.sec, INFL_DC_PORT_BLOCK), 1, w, 0))   // [ATKVIC]
    }
    for (w <- 0 until memWidth) {
      // lane w in lsu_sched priority order: (can_fire, will_fire, resources T tlb / D dcache / L lcam / R rob, winner uop)
      // (can_fire, will_fire, resources, the candidate's uop (loser row), its winner labels ([V3EWIN] D7 for LSQ entries))
      val ord = Seq(
        (can_fire_load_incoming(w),  will_fire_load_incoming(w),  "TDL", Some(exe_req(w).bits.uop), Some(v3eWin(exe_req(w).bits.uop))),
        (can_fire_stad_incoming(w),  will_fire_stad_incoming(w),  "TLR", Some(exe_req(w).bits.uop), Some(v3eWin(exe_req(w).bits.uop))),
        (can_fire_sta_incoming(w),   will_fire_sta_incoming(w),   "TLR", Some(exe_req(w).bits.uop), Some(v3eWin(exe_req(w).bits.uop))),
        (can_fire_std_incoming(w),   will_fire_std_incoming(w),   "R",   Some(exe_req(w).bits.uop), Some(v3eWin(exe_req(w).bits.uop))),
        (can_fire_sfence(w),         will_fire_sfence(w),         "TR",  Some(exe_req(w).bits.uop), Some(v3eWin(exe_req(w).bits.uop))),
        (can_fire_release(w),        will_fire_release(w),        "L",   None, None),
        (can_fire_hella_incoming(w), will_fire_hella_incoming(w), "TD",  None, None),
        (can_fire_hella_wakeup(w),   will_fire_hella_wakeup(w),   "D",   None, None),
        (can_fire_load_retry(w),     will_fire_load_retry(w),     "TDL", Some(ldq_retry_e.bits.uop),  Some(v3eWinLd(ldq_retry_e.bits))),
        (can_fire_sta_retry(w),      will_fire_sta_retry(w),      "TLR", Some(stq_retry_e.bits.uop),  Some(v3eWinSt(stq_retry_e.bits))),
        (can_fire_load_wakeup(w),    will_fire_load_wakeup(w),    "DL",  Some(ldq_wakeup_e.bits.uop), Some(v3eWinLd(ldq_wakeup_e.bits))),
        (can_fire_store_commit(w),   will_fire_store_commit(w),   "D",   Some(stq_commit_e.bits.uop), Some(v3eWinSt(stq_commit_e.bits))))
      for (j <- 8 until 12) {   // load_retry, sta_retry, load_wakeup, store_commit (live only before commit: an AMO)
        val (can, will, res, lu, _) = ord(j)
        val ws   = ord.take(j).filter { case (_, _, r, _, _) => r.exists(c => res.contains(c)) }
        val wsel = PriorityEncoderOH(ws.map(_._2))
        val wv   = ws.map(_._5)
        val key  = Mux1H(wsel, wv.map(_.map(_._1).getOrElse(0.U(uopIDCounterWidthCF.W))))
        val atk  = Mux1H(wsel, wv.map(_.map(_._2).getOrElse(false.B)))
        val sec  = Mux1H(wsel, wv.map(_.map(_._3).getOrElse(false.B)))
        val live = if (j == 11) !stq_commit_e.bits.committed else true.B
        v3eCands += ((rdeUpd(can && !will && live && v3eLive(lu.get), lu.get, key, atk && v3eVic(lu.get), sec, INFL_LSU_LANE), 0, w, j))   // [ATKVIC]
        if (ENABLE_CF_DEBUG_PRINTF) {
          assert(!(can && !will) || ws.map(_._2).reduce(_ || _), "[V3EDGE] LSU_LANE loser without a firing winner")
        }
      }
    }
    // [LDQRDE] stall-edge audit D4 (owner 2026-10-26: "all of it"): the labels of the reader edges a pending LOAD takes
    // (STL_PARTIAL through the [B61LDQ] table; LSU_LANE as the load_retry / load_wakeup loser; DC_PORT_BLOCK on a
    // refused load request) also land on its LDQ entry, so the LDQ head (MEM_HOL / LDQ_FULL / C5), the replay request's
    // fill mark (cfLdSprop / cfLdAtk, RULES A reading 3) and the response see what the ROB record sees.  Set-only and
    // gated by ldq(i).valid: a freed entry (kill / flush in the event cycle) is never written, so the enqueue of a new
    // load into it cannot be overridden; every other writer of these two bits is earlier in source (TLB-stage OR) or
    // set-only too ([LDQRESP]), so no write is lost.  A V3EDGE candidate sets the label whether or not it wins the edge
    // port (drop-and-count drops the SLOT, not the label: R-STICKY).
    // V3EDGE kinds are STAGED one cycle (their valid ends deep in the will_fire / D$-ready cone): 4 stage entries
    // {v, ldq idx, sec, atk} (DC_PORT_BLOCK per lane; LSU_LANE retry / wakeup, lanes ORed: one LDQ entry each).
    // Cost: 4 x (1 + ldqAddrSz + 2) = 36 FF; per LDQ entry 6 idx compares (4 staged + the table's own) and a set-only
    // D leg on 2 bits: ~4 LUT per entry, ~160 LUT.  Levels: stage flops -> idx compare (1-2) -> entry D (1): flop to
    // flop, new endpoints only, 0 levels on the grant / nack / will_fire paths (the candidates end in a flop as before).
    val l4src = (0 until memWidth).map { w =>
                  val e = v3eCands.collect { case (c, 1, `w`, _) => c }.head
                  (e.valid && dmem_req(w).bits.uop.uses_ldq, dmem_req(w).bits.uop.ldq_idx, e.bits.is_secret, e.bits.is_atk) } ++
                Seq((8, ldq_retry_idx), (10, ldq_wakeup_idx)).map { case (j, ix) =>
                  val es = v3eCands.collect { case (c, 0, _, `j`) => c }.toSeq
                  (es.map(_.valid).reduce(_ || _), ix, es.map(c => c.valid && c.bits.is_secret).reduce(_ || _),
                   es.map(c => c.valid && c.bits.is_atk).reduce(_ || _)) }
    val l4v   = l4src.map { case (v, _, s, a) => RegNext(v && (s || a), false.B) }
    val l4idx = l4src.map { case (_, ix, _, _) => RegNext(ix) }
    val l4sec = l4src.map { case (_, _, s, _) => RegNext(s) }
    val l4atk = l4src.map { case (_, _, _, a) => RegNext(a) }
    for (i <- 0 until numLdqEntries) {
      val h  = l4v.zip(l4idx).map { case (v, ix) => v && ix === i.U }
      val ls = rdeTabSec(i) || h.zip(l4sec).map { case (x, s) => x && s }.reduce(_ || _)
      val la = rdeTabAtk(i) || h.zip(l4atk).map { case (x, a) => x && a }.reduce(_ || _)
      when (ldq(i).valid && ls) { ldq(i).bits.uop.cf_secret_propagation.foreach(_ := true.B) }
      when (ldq(i).valid && la) { ldq(i).bits.uop.cf_attacker_influence.foreach(_ := true.B) }
    }
    val v3eAll = v3eCands.map(_._1).toSeq
    val v3eSel = PriorityEncoderOH(v3eAll.map(_.valid))
    val v3e    = Mux1H(v3eSel, v3eAll)
    val v3eAny = v3eAll.map(_.valid).reduce(_ || _)
    val out = RegInit(0.U.asTypeOf(Valid(new CF_MemOrdUpdate)))
    out.valid              := gv || v3eAny   // [V3EDGE] SP-20
    out.bits.rob_idx       := ldq(gidx).bits.uop.rob_idx
    out.bits.op_count_id   := ldq(gidx).bits.uop.cf_op_count_id.get
    out.bits.prod_op_count := p_oc(gidx)
    out.bits.is_atk        := p_atk(gidx)
    out.bits.is_secret     := p_sec(gidx)
    out.bits.infl_type.get := (if (dcbOn) Mux(p_bank.get(gidx), INFL_DC_BANKCONF.U(inflTypeWidthCF.W), INFL_STL_PARTIAL.U(inflTypeWidthCF.W))
                               else INFL_STL_PARTIAL.U(inflTypeWidthCF.W))
    when (!gv && v3eAny) { out.bits := v3e.bits }   // [V3EDGE] SP-20: a new edge takes the idle port (last connect)
    // [V3PRINT] SP-22 (FT-7): one line per LSU_LANE / DC_PORT_BLOCK loser this cycle: emitted = it took the port,
    // dropped = it did not (drop-and-count, N-D3 a); a refused COMMITTED store-commit (no live row, no edge) prints
    // committed=1 emitted=0 dropped=0.  Debug-print builds only, +cf_state=1.
    if (ENABLE_CF_DEBUG_PRINTF) {
      val pa = boom.v3.util.CfStatePlusArg(true)
      for (((e, kind, lane, src), k) <- v3eCands.zipWithIndex) {
        val emitted = v3eSel(k) && e.valid && !gv
        when (pa && e.valid) {
          if (kind == 1) {
            printf("[EDGE-DCPORT] c=%d lane=%d loser_oc=%d winner_oc=%d committed=0 emitted=%d dropped=%d\n",
              io.core.tsc_reg, lane.U, e.bits.op_count_id, e.bits.prod_op_count, emitted, !emitted)
          } else {
            printf("[EDGE-LANE] c=%d lane=%d src=%d loser_oc=%d winner_oc=%d emitted=%d dropped=%d\n",
              io.core.tsc_reg, lane.U, src.U, e.bits.op_count_id, e.bits.prod_op_count, emitted, !emitted)
          }
        }
      }
      for (w <- 0 until memWidth) {
        when (pa && v3eBlk.valid && dmem_req(w).valid && will_fire_store_commit(w) && stq_commit_e.bits.committed) {
          printf("[EDGE-DCPORT] c=%d lane=%d loser_oc=%d winner_oc=%d committed=1 emitted=0 dropped=0\n",
            io.core.tsc_reg, w.U, stq_commit_e.bits.uop.cf_op_count_id.get, v3eBlk.bits.oc)
        }
      }
    }
    // [V3EDGE-END] SP-22 LSU prints are inserted above this line
    io.core.cf_rdedge_upd.get := out
    // Debug only (ENABLE_CF_DEBUG_PRINTF; no FPGA flops).  Observed one cycle late, from registered copies:
    //   [RDELOST] a load left the LDQ in cycle c while its entry was pending (incl. granted in c: the port register
    //   reaches the ROB after the commit).  live=1: it left by COMMIT (not killed / flushed) = a lost edge of a live
    //   row; live=0: dead row.  n = all, nlive = live.  Requirement: nlive = 0.
    //   [RDELATE] the port register carried an edge in the cycle its load committed (reaches the column too late for
    //   that commit record; counted in nlive as well).
    if (ENABLE_CF_DEBUG_PRINTF) {
      val killed = (0 until numLdqEntries).map(i => IsKilledByBranch(io.core.brupdate, ldq(i).bits.uop) || io.core.exception)
      val pv_r   = RegNext(VecInit(elig))
      val kil_r  = RegNext(VecInit(killed))
      val vld_r  = RegNext(VecInit(ldq.map(_.valid)))
      val left   = (0 until numLdqEntries).map(i => vld_r(i) && !ldq(i).valid)   // left the LDQ in the previous cycle
      val lost   = (0 until numLdqEntries).map(i => left(i) && pv_r(i))
      val lostLv = (0 until numLdqEntries).map(i => lost(i) && !kil_r(i))
      val oidx   = RegNext(gidx)                                                   // the entry the port carries now
      val o_v_r  = RegNext(out.valid, false.B)                                     // the port carried an edge last cycle,
      val o_i_r  = RegNext(oidx)                                                   // for this entry,
      val late   = o_v_r && VecInit(left)(o_i_r) && !kil_r(o_i_r)                  // and that load committed last cycle
      val cf_rde_lost  = RegInit(0.U(16.W))
      val cf_rde_llive = RegInit(0.U(16.W))
      val cf_rde_late  = RegInit(0.U(16.W))
      val nl  = cf_rde_lost  + PopCount(lost)
      val nll = cf_rde_llive + PopCount(lostLv) + late.asUInt
      cf_rde_lost  := nl
      cf_rde_llive := nll
      for (i <- 0 until numLdqEntries) {
        when (lost(i)) {
          printf("[RDELOST] n=%d nlive=%d ldq=%d rob=%d oc=%d prod=%d sec=%d atk=%d live=%d\n",
                 nl, nll, i.U, ldq(i).bits.uop.rob_idx, ldq(i).bits.uop.cf_op_count_id.get, p_oc(i), p_sec(i), p_atk(i),
                 lostLv(i))
        }
      }
      when (late) {
        cf_rde_late := cf_rde_late + 1.U
        printf("[RDELATE] n=%d nlive=%d rob=%d oc=%d prod=%d\n", cf_rde_late + 1.U, nll, RegNext(out.bits.rob_idx),
               RegNext(out.bits.op_count_id), RegNext(out.bits.prod_op_count))
      }
    }
  }

  // Task 3: Clr unsafe bit in ROB for succesful translations
  //         Delay this a cycle to avoid going ahead of the exception broadcast
  //         The unsafe bit is cleared on the first translation, so no need to fire for load wakeups
  for (w <- 0 until memWidth) {
    io.core.clr_unsafe(w).valid := RegNext((do_st_search(w) || do_ld_search(w)) && !fired_load_wakeup(w)) && false.B
    io.core.clr_unsafe(w).bits  := RegNext(lcam_uop(w).rob_idx)
  }

  // detect which loads get marked as failures, but broadcast to the ROB the oldest failing load
  // TODO encapsulate this in an age-based  priority-encoder
  //   val l_idx = AgePriorityEncoder((Vec(Vec.tabulate(numLdqEntries)(i => failed_loads(i) && i.U >= laq_head)
  //   ++ failed_loads)).asUInt)

  val temp_bits = (VecInit(VecInit.tabulate(numLdqEntries)(i =>
    failed_loads(i) && i.U >= ldq_head) ++ failed_loads)).asUInt
  val l_idx = PriorityEncoder(temp_bits)

  // one exception port, but multiple causes!
  // - 1) the incoming store-address finds a faulting load (it is by definition younger)
  // - 2) the incoming load or store address is excepting. It must be older and thus takes precedent.
  val r_xcpt_valid = RegInit(false.B)
  val r_xcpt       = Reg(new Exception)

  annotate(new AutoCounterCoverModuleAnnotation(chisel3.Module.currentModule.get.toTarget))
  val ld_xcpt_valid = failed_loads.reduce(_|_)
  RCcover(ld_xcpt_valid, "BOOM_v3_MemOrderViolation",
    "Memory ordering violation: a load observed a stale value and must be replayed")
  val ld_xcpt_uop   = ldq(Mux(l_idx >= numLdqEntries.U, l_idx - numLdqEntries.U, l_idx)).bits.uop
  // [MEMORD 2026-09-08] Until now there was NO simulation-visible signal that a memory
  // ordering violation occurred: RCcover/AutoCounter is a Golden Gate transform that
  // produces nothing under Verilator, and MINI_EXCEPTION_MEM_ORDERING is printed nowhere.
  // So "ty=8 never appears" could not be told apart from "order_fail never fires".
  // Gated by the usual printf plusarg, so it costs nothing in a normal run.
  // [MEMORD2 2026-09-08] The probe now carries the FULL EDGE, not just the fact.
  //
  // WHY: the ty=8 edge IS correctly routed into rob_uop by cf_lsu_memord_upd, but that ROB
  // entry is squashed through the EXCEPTION/rollback path, and rob.scala:993 emits its
  // [FLUSH] record only for `IsKilledByBranch`.  Exception-squashed uops are logged
  // NOWHERE.  So the edge existed in hardware and was never printed -- MEASURED: t35 fired
  // order_fail exactly once (probe line present) with ty=8 absent from the whole log.
  //
  // This line makes MEM_ORDER fully observable without touching the ROB's flush machinery.
  // The general hole -- no logging for ANY exception-squashed uop -- is a separate,
  // larger fix (emit a record on the s_rollback walk) and is written up as such.
  when (ld_xcpt_valid) {
    // [CFNOISE-COMMENTED] val mo = io.core.cf_memord_upd(0)
    // [CFNOISE-COMMENTED] printf("[MEMORD] order_fail rob_idx=%d ldq_head=%d oc=%d prod_oc=%d atk=%d sec=%d valid=%d\n",
      // [CFNOISE-COMMENTED] ld_xcpt_uop.rob_idx, ldq_head, ld_xcpt_uop.cf_op_count_id,
      // [CFNOISE-COMMENTED] mo.bits.prod_op_count, mo.bits.is_atk, mo.bits.is_secret, mo.valid)
  }


  val use_mem_xcpt = (mem_xcpt_valid && IsOlder(mem_xcpt_uop.rob_idx, ld_xcpt_uop.rob_idx, io.core.rob_head_idx)) || !ld_xcpt_valid

  val xcpt_uop = Mux(use_mem_xcpt, mem_xcpt_uop, ld_xcpt_uop)

  r_xcpt_valid := (ld_xcpt_valid || mem_xcpt_valid) &&
                   !io.core.exception &&
                   !IsKilledByBranch(io.core.brupdate, xcpt_uop)
  r_xcpt.uop         := xcpt_uop
  r_xcpt.uop.br_mask := GetNewBrMask(io.core.brupdate, xcpt_uop)
  r_xcpt.cause       := Mux(use_mem_xcpt, mem_xcpt_cause, MINI_EXCEPTION_MEM_ORDERING)
  r_xcpt.badvaddr    := mem_xcpt_vaddr // TODO is there another register we can use instead?

  io.core.lxcpt.valid := r_xcpt_valid && !io.core.exception && !IsKilledByBranch(io.core.brupdate, r_xcpt.uop)
  io.core.lxcpt.bits  := r_xcpt

  // Task 4: Speculatively wakeup loads 1 cycle before they come back
  for (w <- 0 until memWidth) {
    io.core.spec_ld_wakeup(w).valid := enableFastLoadUse.B          &&
                                       fired_load_incoming(w)       &&
                                       !mem_incoming_uop(w).fp_val  &&
                                       mem_incoming_uop(w).pdst =/= 0.U
    io.core.spec_ld_wakeup(w).bits.pdst   := mem_incoming_uop(w).pdst
    io.core.spec_ld_wakeup(w).bits.secret := fired_load_secret(w)   // [WAKESEC]
  }


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Writeback Cycle (St->Ld Forwarding Path)
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Handle Memory Responses and nacks
  // Task 3.2 (D16): cfMergeRespInfluencers is gone.  The D$ edges travel on the response
  // (io.dmem.resp.bits.edges) straight to iresp/fresp.edges and the ROB's C_wb column; the
  // LDQ/STQ uop lists they used to be merged into are zeroed at enqueue.  [AMOINFL] still
  // holds: the AMO path forwards the same response edges as the load path.
  // Attacker promotion keeps the merge's rule: any valid attacker edge on the response.
  def respEdgeAtk(e: IftEdgeList): Bool = e.edges.map(x => x.valid && x.is_atk).reduce(_ || _)
  // [RSTICKY] step 17 (audit P18, R-STICKY): the secret twin.  Any valid is_secret edge on the
  // response sets the uop's s_prop SUMMARY bit (ROB sticky_s_prop via the wb merge,
  // rob.scala wb_resps).  This is the summary only: the register value's secret bit is
  // iresp.secret, which this does not touch.  Covers the step-15 residency edge (hit and miss),
  // the step-16 NACKRES edge, and every other D$ edge with sec=1.  4 AND + OR, one LUT level.
  def respEdgeSec(e: IftEdgeList): Bool = e.edges.map(x => x.valid && x.is_secret).reduce(_ || _)

  //----------------------------------
  for (w <- 0 until memWidth) {
    io.core.exe(w).iresp.valid := false.B
    io.core.exe(w).iresp.bits  := DontCare
    io.core.exe(w).fresp.valid := false.B
    io.core.exe(w).fresp.bits  := DontCare
    // Task 3.2: zero edges by default (never DontCare, Phase 0 Bug 3)
    io.core.exe(w).iresp.bits.edges := 0.U.asTypeOf(new IftEdgeList)
    io.core.exe(w).fresp.bits.edges := 0.U.asTypeOf(new IftEdgeList)
    io.core.cf_resp_wsec.foreach(_(w) := false.B)   // [F1WSH] no response: the write port is not valid
    io.core.cf_resp_watk.foreach(_(w) := false.B)
  }
  // [F1WSH] lean58 timing F1: the window-shadow test of a response candidate's uop, assigned in the SAME when-branch
  // as io.core.exe(w).iresp.bits.uop (below), on the SAME uop.  Chisel last-connect gives both signals the same
  // branch selection, so in every cycle cf_resp_w*(w) === cf_wsh_*.in(io.core.exe(w).iresp.bits.uop.rob_idx) whenever
  // the iresp uop is a branch's uop (every cycle iresp.valid can be 1); outside the branches iresp.valid = 0 and the
  // write port is not valid.  cf_wsh_* is the core's wsh_* wire (core.scala), the same value WritePort used.
  // Levels: in() now runs on each candidate's own rob_idx (D$: the ldq_idx-muxed LDQ uop of an s2 register index;
  // AMO: the stq_idx-muxed STQ uop; forward: the wb_forward_ldq_idx-muxed LDQ uop), in parallel with the select
  // logic, and the late select (wb_forward_valid / dmem_resp_fired, fan-out 343/292 in lean57) now picks one bit.
  // Cost: 3 candidates x 2 kinds x one in() per lane (~10 LUT each) = ~120 LUT for memWidth 2; the core drops 4.
  def cfRespWin(w: Int, u: MicroOp): Unit = {
    io.core.cf_resp_wsec.foreach(_(w) := io.core.cf_wsh_sec.get.in(u.rob_idx))
    io.core.cf_resp_watk.foreach(_(w) := io.core.cf_wsh_atk.get.in(u.rob_idx))
  }

  val dmem_resp_fired = WireInit(widthMap(w => false.B))

  for (w <- 0 until memWidth) {
    // Handle nacks
    when (io.dmem.nack(w).valid)
    {
      // We have to re-execute this!
      when (io.dmem.nack(w).bits.is_hella)
      {
        assert(hella_state === h_wait || hella_state === h_dead)
      }
        .elsewhen (io.dmem.nack(w).bits.uop.uses_ldq)
      {
        assert(ldq(io.dmem.nack(w).bits.uop.ldq_idx).bits.executed)
        ldq(io.dmem.nack(w).bits.uop.ldq_idx).bits.executed  := false.B
        nacking_loads(io.dmem.nack(w).bits.uop.ldq_idx) := true.B
        // [NACKRES] step 16: nacked while a secret-address miss holds an MSHR -> tag the load
        // [NACKSECQ] lean58 A7 (DF-nack-secqual, ledger 752; RULES D1 "MSHR/WB nack"): only a nack whose CAUSE is
        // the MSHR/WB side (MSHR full / same set / no free way, victim line, same-set writeback) is that edge, as on
        // the attacker side since patch 44: a probe or data-bank-conflict nack taken while some secret MSHR is live
        // no longer raises a secret nack edge (secret over-taint removed).  cf_nack_mshr is a register-only term
        // (dcache.scala, 0 levels); one more input on the existing write-enable LUT.  A nack with an MSHR cause AND
        // a probe/bank cause in the same cycle records nothing (the attacker side's existing under-taint corner).
        // [NACKCONFSEC] lean58 (Fable audit M8, owner 2026-10-23): ...and only when the CONFLICTING MSHR holds the
        // secret request (patch 68's attacker rule, now both sides): an unrelated probe nacked on full MSHRs or
        // another set while the victim's secret miss is live no longer gets a secret register.  The PC is still
        // cf_asec_busy.bits (the lowest live secret MSHR's), which names the conflicting one whenever only one
        // secret MSHR is live (residual: several live secret MSHRs may name another one).  RECORD CHANGE.
        if (ENABLE_IFT) {
          val nk_sec = io.dmem.cf_nack_asec.get(w) && io.dmem.cf_nack_mshr.get(w)
          when (nk_sec) {
            ldq(io.dmem.nack(w).bits.uop.ldq_idx).bits.cf_nack_res_v.foreach(_ := true.B)
            ldq(io.dmem.nack(w).bits.uop.ldq_idx).bits.cf_nack_res_pc.foreach(_ := io.dmem.cf_nack_pc.get(w))   // [NACKPC] D6
          }
          // [NACKATK] patch 44 (r111 extended, r173 (3)): a VICTIM load nacked by an MSHR/WB conflict
          // while a valid MSHR holds an attacker-influenced request.  Edge only (no value taint).
          // The PC (cf_asec_busy.bits: the secret MSHR's when one is valid, else the attacker
          // MSHR's) is written only if no secret nack is recorded (secret-first, per entry: no
          // LDQ read mux).  The data input is the same register as the secret write's.
          // [NACKCONF] patch 68 (V12): the conflicting MSHR, not any valid MSHR (was cf_aatk_busy)
          val nk_atk = io.dmem.cf_nack_aatk.get(w) && io.dmem.cf_nack_mshr.get(w) &&
                       io.dmem.nack(w).bits.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U
          when (nk_atk) {
            ldq(io.dmem.nack(w).bits.uop.ldq_idx).bits.cf_nack_res_atk.foreach(_ := true.B)
          }
          for (i <- 0 until numLdqEntries) {
            when (nk_atk && !nk_sec && io.dmem.nack(w).bits.uop.ldq_idx === i.U &&   // [NACKCONFSEC] (was !cf_asec_busy.valid)
                  !ldq(i).bits.cf_nack_res_v.get) {
              ldq(i).bits.cf_nack_res_pc.foreach(_ := io.dmem.cf_nack_pc.get(w))   // [NACKPC] D6: the conflicting MSHR's PC
            }
          }
        }
      }
        .otherwise
      {
        assert(io.dmem.nack(w).bits.uop.uses_stq)
        when (IsOlder(io.dmem.nack(w).bits.uop.stq_idx, stq_execute_head, stq_head)) {
          stq_execute_head := io.dmem.nack(w).bits.uop.stq_idx
        }
      }
    }
    // Handle the response
    when (io.dmem.resp(w).valid)
    {
      when (io.dmem.resp(w).bits.uop.uses_ldq)
      {
        assert(!io.dmem.resp(w).bits.is_hella)
        val ldq_idx = io.dmem.resp(w).bits.uop.ldq_idx
        val send_iresp = ldq(ldq_idx).bits.uop.dst_rtype === RT_FIX
        ldq(ldq_idx).bits.cf_nack_res_v.foreach(_ := false.B)   // [NACKRES] consumed: the D$ built the edge
        ldq(ldq_idx).bits.cf_nack_res_atk.foreach(_ := false.B)   // [NACKATK]
        val send_fresp = ldq(ldq_idx).bits.uop.dst_rtype === RT_FLT

        // corefuzzing: merge CF fields from response UOP (which went through TLB+dcache pipeline
        // accumulating INFL_DTLB_STATE and INFL_CACHE_EVICTION) into the canonical LDQ base UOP.
        val ldq_base_cf = ldq(ldq_idx).bits.uop
        val resp_cf     = io.dmem.resp(w).bits.uop
        val merged_base_cf = WireInit(ldq_base_cf)
        // [F7FUBMAP] lean58 timing F7 (timing-audit-20261023 section 6): the LSU sets dcacheTagCF itself and no longer
        // reads the D$ response's 24-bit cf_fu_bitmap, so the copies the D$ carries for it (s1/s2 request registers,
        // the 8 x 16 rpq entries in LUTRAM, the MSHR req registers, the response bus) lose their only reader and are
        // trimmed by synthesis, in the MSHR block that is the lean57 local-congestion hotspot.
        // Bit-identical (records unchanged): the D$ response bitmap is (request uop bitmap) | dcacheTagCF (dcache.scala
        // uop_resp_base on a hit / replay, uncache_resp for the MSHR path; no other D$ or MSHR site touches the field),
        // and every LSU load request's uop bitmap is a subset of this LDQ entry's bitmap by the time it responds:
        //   load incoming / retry: the request uop is exe_tlb_uop_cf(w), and the same cycle ORs exe_tlb_uop_cf(w)'s
        //     bitmap into ldq(ldq_idx) (the will_fire_load_incoming || will_fire_load_retry block, same ldq_idx);
        //   load wakeup: the request uop IS the LDQ entry's uop;
        //   MSHR replay: the rpq holds a copy of one of the above;
        // and an LDQ entry's bitmap only grows (written at enqueue and by that OR, nowhere else).  Hence
        // ldq | (req | dcacheBit) == ldq | dcacheBit.  This relies on the D$ answering only the live load its ldq_idx
        // names (BOOM's own invariant: killed loads are dropped in s1/s2 and in the rpq), the same invariant every
        // other field of this merge already relies on.  IFT off: the field is None.
        // [RECV3] SP-09 (design 3.2 bits 5-6): DC = the D$ answered this load (every path); MSHR = it answered through the
        // MSHR file or an IOMSHR, i.e. the response footprint kind is 2 or 3 (SP-08 dc.kind bit 1).  One OR input: 0 levels.
        merged_base_cf.cf_fu_bitmap.foreach(_ := ldq_base_cf.cf_fu_bitmap.getOrElse(0.U(numModules.W)) | (1 << UNIT_DC).U(numModules.W) |
          Mux(io.dmem.resp(w).bits.edges.dc.get.kind(1), (1 << UNIT_MSHR).U(numModules.W), 0.U(numModules.W)))
        merged_base_cf.cf_secret_access.foreach(_ := ldq_base_cf.cf_secret_access.getOrElse(false.B) || resp_cf.cf_secret_access.getOrElse(false.B))
        merged_base_cf.cf_secret_transmission.foreach(_ := ldq_base_cf.cf_secret_transmission.getOrElse(false.B) || resp_cf.cf_secret_transmission.getOrElse(false.B))
        // [MERGEFIX 2026-09-07] VALIDATED. merged_base_cf = WireInit(ldq_base_cf), so any
        // field not merged from the dcache response uop keeps the LDQ's stale copy.
        // cf_mem_dataflow_atk is set by the dcache (dcache.scala:1398 from memdf_fire) but
        // was never merged, so it read as 0 in 69,848/69,848 responses and R1d's
        // `addr_is_atk || cf_mem_dataflow_atk` collapsed to addr_is_atk alone.
        // After this: bounds branch cond_atk 2/650 -> 650/650, committed shadow 26% -> 92%,
        // invariants exact (10/50/10, 4,495,125,000 cycles, exit 3).  See artifact 43.
        // DO NOT also merge cf_mem_sec_dataflow -- tried, caused a secret-taint explosion
        // (s_prop 50 -> 144,332).  That needs the D$ secret mask granularity fixed first.
        merged_base_cf.cf_mem_dataflow_atk.foreach(_ := ldq_base_cf.cf_mem_dataflow_atk.getOrElse(false.B) || resp_cf.cf_mem_dataflow_atk.getOrElse(false.B))
        // [SECFIX 2026-09-07] The secret twin, re-enabled. The earlier explosion
        // (s_prop 50 -> 144,332) was NOT line granularity -- the per-DW masks are honoured
        // on the store path.  It was the REFILL blanketing the requester's carried secret
        // across all 8 DWs (mshrs.scala cf_req_secret, now fixed to the address property).
        // Without this merge core.scala:2191 / rob.scala:1140 read a permanently-0 field, so
        // memory-borne secret never sets s_prop -- t19_dcache_secret_inherit measured 0
        // records with s_prop=1 AND ty17 sec=1, i.e. it never tested what it documents.
        merged_base_cf.cf_mem_sec_dataflow.foreach(_ := ldq_base_cf.cf_mem_sec_dataflow.getOrElse(false.B) || resp_cf.cf_mem_sec_dataflow.getOrElse(false.B))
        // [WINMEM] step 21 (owner r139, F2): a load of a WINDOW-marked doubleword (the D$ sets the
        // response uop's cf_opnd_* from the store metadata, epoch-matched to this load) returns
        // WINDOW taint: the core writes it to the register's wsec/watk (epoch-checked) and gives
        // the s_prop update; it never reaches `secret`, so it never re-arms the window.
        merged_base_cf.cf_opnd_wsec.foreach(_ := false.B)   // [ONETAG] r205: no secret window kind
        merged_base_cf.cf_opnd_watk.foreach(_ := false.B)   // [ONETAGATK] r205 U8
        // Task 3.2 (D16): the D$ edges ride on the response; promote attacker influence on
        // an attacker edge exactly as the old list merge did.
        when (respEdgeAtk(io.dmem.resp(w).bits.edges)) { merged_base_cf.cf_attacker_influence.foreach(_ := true.B) }
        when (respEdgeSec(io.dmem.resp(w).bits.edges)) { merged_base_cf.cf_secret_propagation.foreach(_ := true.B) }   // [RSTICKY]
        val dmem_resp_uop_cf = merged_base_cf
        io.core.exe(w).iresp.bits.edges := io.dmem.resp(w).bits.edges
        io.core.exe(w).fresp.bits.edges := io.dmem.resp(w).bits.edges

        io.core.exe(w).iresp.bits.uop  := dmem_resp_uop_cf
        cfRespWin(w, dmem_resp_uop_cf)   // [F1WSH]
        io.core.exe(w).fresp.bits.uop  := dmem_resp_uop_cf
        io.core.exe(w).iresp.valid     := send_iresp
        io.core.exe(w).iresp.bits.data := io.dmem.resp(w).bits.data
        // [SECDEST] step 17 (owner ruling 116 A(1)): a load at a secret-derived ADDRESS (rs1
        // taint at the AGU, ldq.addr_is_secret -- the s_tx condition before the range gate)
        // returns a secret-selected value: the destination register is tagged secret, and the
        // core's writeback s_prop update (wb.bits.secret) sets the uop's s_prop.  Mirrors the
        // attacker twin below (addr_is_atk).  One OR on an existing ldq_idx-muxed read.
        val cf_ld_asec = ldq(io.dmem.resp(w).bits.uop.ldq_idx).bits.addr_is_secret.getOrElse(false.B)
        io.core.exe(w).iresp.bits.secret := dmem_resp_uop_cf.cf_secret_access.getOrElse(false.B) ||
                                           dmem_resp_uop_cf.cf_mem_sec_dataflow.getOrElse(false.B) || cf_ld_asec
        // ATTACKER: the loaded value's attacker taint.  Sourced from the LDQ entry, which
        // recorded the address operand's attacker taint at the AGU.  A D$ LINE tag (a
        // value written by the attacker and later loaded by the victim) is a separate
        // surface and is NOT covered here -- see 16-attacker-taint-scope.md.
        // [R1d 2026-09-05] The loaded VALUE's attacker taint, not just the address's.
        // Mirrors the secret twin above (cf_secret_access || cf_mem_sec_dataflow).
        // cf_mem_dataflow_atk is set at dcache.scala:1357 from memdf_fire: a load reading a
        // line whose last writer was attacker-influenced.  The addr term is KEPT: a load
        // whose address the attacker chose is influenced in its own right (array1[x]).
        // ORDER MATTERS -- an identical change FAILED on 2026-09-05 when applied before
        // R1a/R1c, because the line tag was then poisoned by the storing uop's aggregate
        // and the control PC went 100% contaminated.  Re-applied only after R1a (line tag
        // = data_is_atk) and R1c (value-class promotion) were validated clean.
        io.core.exe(w).iresp.bits.taint_atk := ldq(io.dmem.resp(w).bits.uop.ldq_idx).bits.addr_is_atk.getOrElse(false.B) ||
                                              dmem_resp_uop_cf.cf_mem_dataflow_atk.getOrElse(false.B)
        io.core.exe(w).fresp.valid     := send_fresp
        io.core.exe(w).fresp.bits.data := io.dmem.resp(w).bits.data
        io.core.exe(w).fresp.bits.secret := dmem_resp_uop_cf.cf_secret_access.getOrElse(false.B) ||
                                           dmem_resp_uop_cf.cf_mem_sec_dataflow.getOrElse(false.B) || cf_ld_asec   // [SECDEST]
        io.core.exe(w).fresp.bits.taint_atk := ldq(io.dmem.resp(w).bits.uop.ldq_idx).bits.addr_is_atk.getOrElse(false.B) ||
                                              dmem_resp_uop_cf.cf_mem_dataflow_atk.getOrElse(false.B)   // [R1d] see iresp

        assert(send_iresp ^ send_fresp)
        dmem_resp_fired(w) := true.B

        ldq(ldq_idx).bits.succeeded      := io.core.exe(w).iresp.valid || io.core.exe(w).fresp.valid
        ldq(ldq_idx).bits.debug_wb_data  := io.dmem.resp(w).bits.data
        // [LDQRESP] stall-edge audit D1 (owner 2026-10-26): the response-time record labels (the D$ edges' sec / atk,
        // the secret-marked DW (cf_mem_sec_dataflow) and the attacker line value (cf_mem_dataflow_atk)) also land on
        // the LDQ entry, so the LDQ head (LDQ_FULL / C5 / MEM_HOL), the LD-LD MEM_ORDER searcher and the [B60LDB]
        // blocker labels see what the ROB record sees (merged_base_cf above already carries them to the writeback, so
        // the record and the register tag are unchanged).  Set-only (monotone, as merged_base_cf): no other writer of
        // these bits can be lost; the responded entry makes no later D$ request.  Conditions use only response-bus
        // terms (no LDQ read mux), into the same ldq_idx decode as `succeeded`: 0 FF, ~2 x 40 LUT, 0 levels.
        if (ENABLE_IFT) {
          when (respEdgeSec(io.dmem.resp(w).bits.edges) || resp_cf.cf_mem_sec_dataflow.getOrElse(false.B)) {
            ldq(ldq_idx).bits.uop.cf_secret_propagation.foreach(_ := true.B) }
          when (respEdgeAtk(io.dmem.resp(w).bits.edges) || resp_cf.cf_mem_dataflow_atk.getOrElse(false.B)) {
            ldq(ldq_idx).bits.uop.cf_attacker_influence.foreach(_ := true.B) }
        }
      }
        .elsewhen (io.dmem.resp(w).bits.uop.uses_stq)
      {
        assert(!io.dmem.resp(w).bits.is_hella)
        stq(io.dmem.resp(w).bits.uop.stq_idx).bits.succeeded := true.B
        when (io.dmem.resp(w).bits.uop.is_amo) {
          dmem_resp_fired(w) := true.B
          io.core.exe(w).iresp.valid     := true.B
          // [AMOINFL 2026-09-08] Was a wholesale `:= stq(...).bits.uop`, which DISCARDED
          // the dcache response uop's influencer list.  t32_amo_taint measured the result:
          // the AMO carried atk=1 but all four influencer slots were v=0 -- the tag with no
          // edge, so no parser could recover WHICH store produced the taint.  The load path
          // has always merged this list; the AMO path never did.  Same merge, same helper.
          // Task 3.2 (D16): the edges ride on the response, as on the load path.
          val amo_base_cf = WireInit(stq(io.dmem.resp(w).bits.uop.stq_idx).bits.uop)
          when (respEdgeAtk(io.dmem.resp(w).bits.edges)) { amo_base_cf.cf_attacker_influence.foreach(_ := true.B) }
          when (respEdgeSec(io.dmem.resp(w).bits.edges)) { amo_base_cf.cf_secret_propagation.foreach(_ := true.B) }   // [RSTICKY]
          // [AMOSTX] step 14 (owner ruling 102), STORE half: an AMO never takes clr_bsy (every
          // clr_bsy site has !is_amo), so the store rule -- secret DATA to an address outside
          // the secret range -- is applied here, on the AMO's own writeback, from the same two
          // entry bits clr_bsy reads.  Both are final by now (store_commit needs data.valid and
          // addr.valid).  The existing wb port already ORs cf_secret_transmission into the ROB
          // row's sticky s_tx (rob.scala wb_resps): no new port.  The LOAD half rode in on the
          // entry uop at the TLB stage.  Logic: two 1-bit reads at the index already muxed
          // for the uop, one AND, one OR.
          val amo_e = stq(io.dmem.resp(w).bits.uop.stq_idx).bits
          amo_base_cf.cf_secret_transmission.foreach(_ := amo_e.uop.cf_secret_transmission.getOrElse(false.B) ||
                                                (cfStDataSec(amo_e) && !amo_e.uop.cf_secret_access.getOrElse(false.B)))   // [WINMEM] [SACCU]
          // [WINMEM] step 21: the AMO's old value is window-tainted when its address operand was
          // (the load half; the data operand does not select the returned value) or the D$
          // word it read was window-marked (response cf_opnd_*); membership is the uop's cf_win_*.
          amo_base_cf.cf_opnd_wsec.foreach(_ := false.B)   // [ONETAG] r205
          amo_base_cf.cf_opnd_watk.foreach(_ := false.B)   // [ONETAGATK]
          io.core.exe(w).iresp.bits.uop   := amo_base_cf
          cfRespWin(w, amo_base_cf)   // [F1WSH]
          io.core.exe(w).iresp.bits.edges := io.dmem.resp(w).bits.edges
          io.core.exe(w).iresp.bits.data := io.dmem.resp(w).bits.data
            // AMO path: the response uop is the STQ entry, so the taint is that
            // AMO's own secret-range hit recorded at the TLB.
            // [AMOFIX 2026-09-08] The AMO response path took the STQ entry's OWN taint only
            // and dropped everything the memory access itself contributed.  Both flavours had
            // the identical hole, so both are fixed together:
            //   secret   : was cf_secret_access only -- an AMO reading a secret-marked line
            //              returned data with secret=0.
            //   taint_atk: was addr_is_atk only -- an AMO reading an attacker-written line
            //              returned data with taint_atk=0.
            // Source is the DCACHE RESPONSE uop, not `dmem_resp_uop_cf`: that val is local to
            // the uses_ldq branch above and is not in scope here (that is what failed to
            // elaborate at lsu.scala:2034).  The response uop is where dcache.scala sets these
            // bits, so it is the direct and correct source.
            // Requires the companion AMOLOAD fix in dcache.scala -- without it memdf_fire /
            // memsec_fire never fire for an AMO and both new terms are constant 0.
            io.core.exe(w).iresp.bits.secret := stq(io.dmem.resp(w).bits.uop.stq_idx).bits.uop.cf_secret_access.getOrElse(false.B) ||
                                                io.dmem.resp(w).bits.uop.cf_mem_sec_dataflow.getOrElse(false.B) ||
                                                stq(io.dmem.resp(w).bits.uop.stq_idx).bits.addr_is_secret.getOrElse(false.B)   // [SECDEST] A(1)
            // [AMOTAINT 2026-09-08] Was ADDRESS taint only: an AMO reading attacker-tainted
            // memory returned CLEAN data.  Normal loads got the memory-dataflow term from the
            // merge fix (:1985); the AMO path was never updated to match.
            io.core.exe(w).iresp.bits.taint_atk := stq(io.dmem.resp(w).bits.uop.stq_idx).bits.addr_is_atk.getOrElse(false.B) ||
                                                   io.dmem.resp(w).bits.uop.cf_mem_dataflow_atk.getOrElse(false.B)

          stq(io.dmem.resp(w).bits.uop.stq_idx).bits.debug_wb_data := io.dmem.resp(w).bits.data
        }
      }
    }
    // [RESPSEL] patch 25 row 2d: a D$ store-only-miss ack, at the place the old resp(w) handled it
    // (same lane iteration, right after this lane's resp block; the two are exclusive per lane, so
    // the last-connect order on stq(*).succeeded is unchanged).  The old resp path for such an ack
    // was exactly: !uses_ldq (D$ assert), uses_stq -> assert(!is_hella); succeeded := true; never
    // is_amo (an AMO is a read).  A hella store ack only reaches the hella FSM (hella_dresp below).
    io.dmem.st_ack.foreach { sa =>
      when (sa(w).valid && sa(w).bits.uop.uses_stq) {
        assert(!sa(w).bits.is_hella && !sa(w).bits.uop.is_amo)
        stq(sa(w).bits.uop.stq_idx).bits.succeeded := true.B
      }
    }


    when (dmem_resp_fired(w) && wb_forward_valid(w))
    {
      // Twiddle thumbs. Can't forward because dcache response takes precedence
    }
      .elsewhen (!dmem_resp_fired(w) && wb_forward_valid(w))
    {
      val f_idx       = wb_forward_ldq_idx(w)
      val forward_uop = ldq(f_idx).bits.uop
      val stq_e       = stq(wb_forward_stq_idx(w))
      val data_ready  = stq_e.bits.data.valid
      val live        = !IsKilledByBranch(io.core.brupdate, forward_uop)
      val storegen = new freechips.rocketchip.rocket.StoreGen(
                                stq_e.bits.uop.mem_size, stq_e.bits.addr.bits,
                                stq_e.bits.data.bits, coreDataBytes)
      val loadgen  = new freechips.rocketchip.rocket.LoadGen(
                                forward_uop.mem_size, forward_uop.mem_signed,
                                wb_forward_ld_addr(w),
                                storegen.data, false.B, coreDataBytes)

      // corefuzzing: STL forwarding — two explicit stages to avoid combinational feedback.
      // Stage 1: cross-domain influencer injection (input: forward_uop only — no cycle).
      val stl_infl_uop = stq_e.bits.uop
      // 2026-09-03 -- was `stl_infl_uop.cf_secret_propagation`, the store uop's AGGREGATE
      // taint.  Store-to-load forwarding hands the LOADED REGISTER the store's VALUE, so
      // the taint it confers must be the DATA taint; the aggregate also carries fetch-time
      // and observability taint that says nothing about the value.
      // MEASURED on t18: `sd s0,40(sp)` (0x8000150c) stores a CLEAN s0 but sits inside the
      // secret function, so its aggregate bit is 1.  `ld s0,40(sp)` (0x80001560) forwarded
      // that bit and wrote s0 TAINTED -- 515 times.  Every stack access derived from that
      // frame pointer then looked secret-derived (PROBE J: reqsec=1 on 1790 stack loads),
      // which is where the false s_tx came from.  PROBE N proved the register file itself
      // is correct (0% stale taint), so the defect was the value handed to it.
      // [STLASEC] patch 43 (r168c, review p4-42 section 7): a store at a SECRET-DERIVED address (rs1 taint at
      // the AGU, stq.addr_is_secret) hands the forwarded load a secret value even with clean data: which
      // load it forwards to depends on the secret address.  The in-STQ twin of the [RESID]/[SECDEST] line
      // mark (a D$ hit on a line marked by a secret-address store gets edge + value, dcache res_line /
      // cf_mem_sec_dataflow, 116 A(2)); the secret twin of [MEMORDATK]'s store-address term.  Through the
      // existing paths: the STL_FORWARD edge sec (either domain candidate), the sticky s_prop on the
      // response uop (fwd_uop_cf below, r117) and iresp/fresp.secret (VALUE).  The store itself and the
      // load's own s_tx are unchanged (TLB-stage address rule).  r139 (edge only) governs the store-DATA
      // whole-line mark, not this.  Cost: one 1-bit entry mux by wb_forward_stq_idx beside the
      // data_is_secret one + one OR input; 0 levels (the OR folds into the consumers' LUTs).
      // [ONETAG] r205: the window-kind forward (stl_wsec, same-epoch compare) is gone; a window-derived store data or
      // address is already data_is_secret / addr_is_secret (the one tag).  Whether the forwarded value / STL_FORWARD
      // label also takes the store's record s_prop is RULES.md E/U2 (+ OPEN O4): staged as r205-u2b-stl-record.diff.
      // [U2STL] variant for RULES.md E/U2 = YES for STL_FORWARD (and OPEN O4 S-a): the forwarded value and the
      // STL_FORWARD sec label also take the store's record s_prop known now (dispatch-time membership / edges,
      // operand taint), as its D$ doubleword does at commit.  One leaf on the same entry mux: 0 levels.
      // [SACCU] 2026-10-21: stl_infl_uop.cf_secret_access is now set for a plain in-range store too (one s_acc
      // definition), so an STL_FORWARD from such a store carries sec = 1 (U2: the record summary labels the edge).
      // The forwarded value was already secret through the load's own fwd_sacc below; no new logic.
      val stl_store_is_secret = stl_infl_uop.cf_secret_access.getOrElse(false.B) || stq_e.bits.data_is_secret.getOrElse(false.B) ||
                                stq_e.bits.addr_is_secret.getOrElse(false.B) || stl_infl_uop.cf_secret_propagation.getOrElse(false.B)
      // ATTACKER: forwarding hands the loaded register the store's VALUE, so it also
      // hands over that value's attacker taint.  data_is_atk, not the uop's domain --
      // an attacker-written value forwarded into victim code is influence, not ownership.
      val stl_store_is_atk    = stq_e.bits.data_is_atk.getOrElse(false.B)
      // [MEMORDATK] patch 42 (r119 on the forward path, as [ATKRES] ratk_fire on a D$ hit): a store at an
      // ATTACKER-tainted address hands a VICTIM load attacker value taint and an atk edge; and the load's
      // own attacker address taints its value, as on the D$ response path (iresp taint_atk above).
      val stl_fwd_victim      = forward_uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U
      // [M6STLATK] lean58 (Fable audit M6; RULES A U2 "all edges", U8 symmetric): the store's attacker RECORD (its
      // dispatch summary cf_dis_atk and the uop's attacker bit, as the STQ head and the store's D$ DW at commit
      // use it) joins the forwarded value and both STL_FORWARD labels, the twin of stl_store_is_secret's
      // cf_secret_propagation leaf.  One leaf on the same forward-entry mux: 0 levels.  RECORD CHANGE.
      val stl_store_atk_rec   = stq_e.bits.cf_dis_atk.getOrElse(false.B) || stl_infl_uop.cf_attacker_influence.getOrElse(false.B)
      val stl_atk_v           = (stl_store_is_atk || stq_e.bits.addr_is_atk.getOrElse(false.B) || stl_store_atk_rec) && stl_fwd_victim
      // [STLDOM] patch 67 (V11, owner 2026-10-19 16:3x): ...and the store's attacker DOMAIN, as the D$ store-meta
      // write marks atkMask with (domain || data_is_atk) (dcache.scala st_atk).  An attacker store of x0 or of a
      // victim-written register forwarded a clean value and then committed a tainted doubleword.  P1: ownership
      // seeds the value the attacker writes.  The domain comes through the same forward-entry select as
      // data_is_atk (parallel): +1 OR input, 0 levels.  IFT off: cf_domain_id is None (constant 0).
      val stl_val_atk         = stl_store_is_atk || stl_infl_uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 1.U ||
                                stl_store_atk_rec ||   // [M6STLATK]
                                (stq_e.bits.addr_is_atk.getOrElse(false.B) && stl_fwd_victim) ||
                                ldq(f_idx).bits.addr_is_atk.getOrElse(false.B)
      // Task 3.2 (D16): the two STL_FORWARD edges are built on a zero base and travel on the
      // response (iresp/fresp.edges), not in the uop.  Cross-domain: the store's domain as
      // is_atk, its secret flag as is_secret.  Same-domain secret: is_atk 0, is_secret 1.
      // Exactly one can fire (the domain tests are complements).
      val (stl_edges, stl_atk) = addInfluencerBatchList(0.U.asTypeOf(new IftEdgeList), forward_uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)), Seq(
        InfluencerCandidate(data_ready && live && (forward_uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= stl_infl_uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W))),
                            stl_infl_uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)), INFL_STL_FORWARD,
                            stl_infl_uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 1.U || stl_store_atk_rec, stl_store_is_secret),   // [M6STLATK]
        // [MEMORDATK] same-domain: also an attacker-influenced (data or address) store forwarding to a victim
        // load, is_atk = that term, is_secret = the store's secret (was the constants {0, 1}).
        InfluencerCandidate(data_ready && live && (stl_store_is_secret || stl_atk_v) && (forward_uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === stl_infl_uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W))),
                            stl_infl_uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)), INFL_STL_FORWARD, stl_atk_v, stl_store_is_secret)))
      // Secret propagation (both domain cases) stays on the uop.
      val fwd_uop_cf = WireInit(forward_uop)
      when (data_ready && live && stl_store_is_secret) { fwd_uop_cf.cf_secret_propagation.foreach(_ := true.B) }
      fwd_uop_cf.cf_attacker_influence.foreach(_ := forward_uop.cf_attacker_influence.getOrElse(false.B) || stl_atk)
      // [RECV3] SP-09 (design 3.2 bit 4, 3.3 item 6): the data came from the store-to-load forwarding path (no D$ data).
      fwd_uop_cf.cf_fu_bitmap.foreach(_ := forward_uop.cf_fu_bitmap.getOrElse(0.U(numModules.W)) | (1 << UNIT_STQ_FWD).U(numModules.W))
      // [WINMEM] step 21 (owner r139, F2): forwarding from a WINDOW-tainted store (address, data
      // or membership) hands the load WINDOW taint, live when both were dispatched in the same
      // window epoch (a store is older than its load, so a later epoch means the window closed
      // in between).  The core makes it register window taint + s_prop; never `secret`.
      // One 2-bit compare + 4-input OR beside the existing data-taint terms.
      fwd_uop_cf.cf_opnd_wsec.foreach(_ := false.B)   // [ONETAG] r205
      // [ONETAGATK] r205 U8: the attacker window-kind forward (stl_watk, same-epoch compare) is gone; a window-derived
      // store value is already data_is_atk / addr_is_atk.  The store's attacker RECORD on the forwarded value is the
      // attacker side of U2 (u2b covers the secret side only).
      fwd_uop_cf.cf_opnd_watk.foreach(_ := false.B)
      io.core.exe(w).iresp.bits.edges := stl_edges
      io.core.exe(w).fresp.bits.edges := stl_edges

      io.core.exe(w).iresp.valid := (fwd_uop_cf.dst_rtype === RT_FIX) && data_ready && live
      io.core.exe(w).fresp.valid := (fwd_uop_cf.dst_rtype === RT_FLT) && data_ready && live
      io.core.exe(w).iresp.bits.uop  := fwd_uop_cf
      cfRespWin(w, fwd_uop_cf)   // [F1WSH]
      io.core.exe(w).fresp.bits.uop  := fwd_uop_cf
      io.core.exe(w).iresp.bits.data := loadgen.data
      // [SECDEST] step 17 A(1): a forwarded load at a secret-derived address also returns a
      // secret-selected value (which store forwarded depended on the address).
      val fwd_asec = ldq(f_idx).bits.addr_is_secret.getOrElse(false.B)
      // [FWDSACC] patch 52b (owner 10-17 13:4x; RULES s_acc + r116 destination rule; FPGA ridl
      // analysis): a load whose OWN address is in the secret range (s_acc, the LDQ entry's
      // cf_secret_access, written at its TLB stage two cycles before this wb-stage forward)
      // reads a secret value even when a CLEAN in-flight store supplies it, exactly as the D$
      // response path tags it (dmem_resp_uop_cf.cf_secret_access in iresp/fresp.secret above).
      // Missing, the register was written clean and its consumers (ridl: the sb of the
      // attacker's ld of ift_secret) lost s_tx.  The LOAD's bit, not the store's: patch 52
      // left the STQ uop's cf_secret_access clear for a clean store ([SACCU] 2026-10-21 now sets it
      // for an in-range store, which stl_store_is_secret above already reads).  The s_prop of the load itself needs nothing here: the core's writeback s_prop
      // update already fires on wb_uop.cf_secret_access (core.scala is_secret_wb), as on the
      // D$ path, so the sticky s_prop below stays store-taint only.
      // Cost: one more leaf (the same ldq(f_idx) register mux as fwd_asec) on the existing
      // OR, 4 -> 5 inputs, one LUT6 either way: 0 added levels, 0 FF.
      val fwd_sacc = forward_uop.cf_secret_access.getOrElse(false.B)
      io.core.exe(w).iresp.bits.secret := stl_store_is_secret || fwd_asec || fwd_sacc
      io.core.exe(w).iresp.bits.taint_atk := stl_val_atk   // [MEMORDATK]
      io.core.exe(w).fresp.bits.data := loadgen.data
      io.core.exe(w).fresp.bits.secret := stl_store_is_secret || fwd_asec || fwd_sacc   // [FWDSACC]
      io.core.exe(w).fresp.bits.taint_atk := stl_val_atk   // [MEMORDATK]

      when (data_ready && live) {
        ldq(f_idx).bits.succeeded := data_ready
        ldq(f_idx).bits.forward_std_val := true.B
        ldq(f_idx).bits.forward_stq_idx := wb_forward_stq_idx(w)

        ldq(f_idx).bits.debug_wb_data   := loadgen.data
      }
    }
  }

  // Initially assume the speculative load wakeup failed
  io.core.ld_miss         := RegNext(io.core.spec_ld_wakeup.map(_.valid).reduce(_||_))
  val spec_ld_succeed = widthMap(w =>
    !RegNext(io.core.spec_ld_wakeup(w).valid) ||
    (io.core.exe(w).iresp.valid &&
      io.core.exe(w).iresp.bits.uop.ldq_idx === RegNext(mem_incoming_uop(w).ldq_idx)
    )
  ).reduce(_&&_)
  when (spec_ld_succeed) {
    io.core.ld_miss := false.B
  }


  //-------------------------------------------------------------
  // Kill speculated entries on branch mispredict
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Kill stores
  val st_brkilled_mask = Wire(Vec(numStqEntries, Bool()))
  for (i <- 0 until numStqEntries)
  {
    st_brkilled_mask(i) := false.B

    when (stq(i).valid)
    {
      stq(i).bits.uop.br_mask := GetNewBrMask(io.core.brupdate, stq(i).bits.uop.br_mask)

      // corefuzzing
      // [SPECULATIVE][LSU] speculative flush logging -- non-destructive, see below for actual state clear
      when (IsKilledByBranch(io.core.brupdate, stq(i).bits.uop))
      {
        // BEGIN speculative flush logging
        // Compile-time gated (ENABLE_CF_DEBUG_PRINTF). The " x%d/f%d" printfs are part of
        // this log and are gated with it. The stq state clears below are NOT gated.
        if (ENABLE_CF_DEBUG_PRINTF) {
          // Only log uncommitted, valid stores that will be invalidated
          when (!stq(i).bits.committed) {
            // Printf block matches commit log format in exu/core.scala, with [SPECULATIVE][LSU] prefix
            // Modified: use new overload to include MicroOp (`stq(i).bits.uop`) so cf_* fields are printed
            // Old call (kept for reference):
            // SpeculativePrintf.dump("LSU", Sext.apply(stq(i).bits.uop.debug_pc(vaddrBits-1,0), xLen), stq(i).bits.uop.debug_inst, stq(i).bits.uop.is_rvc, io.core.cf_debug_lsu_enable)
            SpeculativePrintf.dump("LSU", Sext.apply(stq(i).bits.uop.debug_pc(vaddrBits-1,0), xLen), stq(i).bits.uop.debug_inst, stq(i).bits.uop.is_rvc, io.core.cf_debug_lsu_enable, stq(i).bits.uop)
            when (stq(i).bits.uop.dst_rtype === RT_FIX && stq(i).bits.uop.ldst =/= 0.U) {
              printf(" x%d 0x%x\n", stq(i).bits.uop.ldst, stq(i).bits.debug_wb_data)
            } .elsewhen (stq(i).bits.uop.dst_rtype === RT_FLT) {
              printf(" f%d 0x%x\n", stq(i).bits.uop.ldst, stq(i).bits.debug_wb_data)
            }
          }
        }
        // END speculative flush logging
        // Non-destructive: state clear remains as before
        stq(i).valid           := false.B
        stq(i).bits.addr.valid := false.B
        stq(i).bits.data.valid := false.B
        st_brkilled_mask(i)    := true.B
      }
    }

    assert (!(IsKilledByBranch(io.core.brupdate, stq(i).bits.uop) && stq(i).valid && stq(i).bits.committed),
      "Branch is trying to clear a committed store.")
  }

  // Kill loads
  for (i <- 0 until numLdqEntries)
  {
    when (ldq(i).valid)
    {
      ldq(i).bits.uop.br_mask := GetNewBrMask(io.core.brupdate, ldq(i).bits.uop.br_mask)
      // [SPECULATIVE][LSU] speculative flush logging -- non-destructive, see below for actual state clear
      when (IsKilledByBranch(io.core.brupdate, ldq(i).bits.uop))
      {
        // BEGIN speculative flush logging
        // Compile-time gated (ENABLE_CF_DEBUG_PRINTF). The " x%d/f%d" printfs are part of
        // this log and are gated with it. The ldq state clears below are NOT gated.
        if (ENABLE_CF_DEBUG_PRINTF) {
          // Only log valid loads that will be invalidated
          // Printf block matches commit log format in exu/core.scala, with [SPECULATIVE][LSU] prefix
          // Modified: use new overload that accepts MicroOp to print cf_* fields
          // Old call (kept for traceability):
          // SpeculativePrintf.dump("LSU", Sext.apply(ldq(i).bits.uop.debug_pc(vaddrBits-1,0), xLen), ldq(i).bits.uop.debug_inst, ldq(i).bits.uop.is_rvc, io.core.cf_debug_lsu_enable)
          SpeculativePrintf.dump("LSU", Sext.apply(ldq(i).bits.uop.debug_pc(vaddrBits-1,0), xLen), ldq(i).bits.uop.debug_inst, ldq(i).bits.uop.is_rvc, io.core.cf_debug_lsu_enable, ldq(i).bits.uop)
          when (ldq(i).bits.uop.dst_rtype === RT_FIX && ldq(i).bits.uop.ldst =/= 0.U) {
            printf(" x%d 0x%x\n", ldq(i).bits.uop.ldst, ldq(i).bits.debug_wb_data)
          } .elsewhen (ldq(i).bits.uop.dst_rtype === RT_FLT) {
            printf(" f%d 0x%x\n", ldq(i).bits.uop.ldst, ldq(i).bits.debug_wb_data)
          }
        }
        // END speculative flush logging
        ldq(i).valid           := false.B
        ldq(i).bits.addr.valid := false.B
      }
    }
  }

  //-------------------------------------------------------------
  when (io.core.brupdate.b2.mispredict && !io.core.exception)
  {
    stq_tail := io.core.brupdate.b2.uop.stq_idx
    ldq_tail := io.core.brupdate.b2.uop.ldq_idx
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // dequeue old entries on commit
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  var temp_stq_commit_head = stq_commit_head
  var temp_ldq_head        = ldq_head
  for (w <- 0 until coreWidth)
  {
    val commit_store = io.core.commit.valids(w) && io.core.commit.uops(w).uses_stq
    val commit_load  = io.core.commit.valids(w) && io.core.commit.uops(w).uses_ldq
    val idx = Mux(commit_store, temp_stq_commit_head, temp_ldq_head)
    when (commit_store)
    {
      stq(idx).bits.committed := true.B
      // [WINMEM] step 21: a store committing in the window shadow (rob.scala sets the commit
      // uop's cf_win_* from the [WINSHADOW] range) is a window member for its D$ write
      // [ONETAG] r205: the STQ uop's cf_win_sec now holds the store's commit-time RECORD secret (s_acc || s_prop,
      // which includes the commit-time window shadow; debug-masked by rob.scala), read only by the D$ write request
      // above (S-a).  The store writes the D$ only after commit, so the bit is final when it is read.
      stq(idx).bits.uop.cf_win_sec.foreach(_ := io.core.commit.uops(w).cf_secret_propagation.getOrElse(false.B) ||
                                                io.core.commit.uops(w).cf_secret_access.getOrElse(false.B))
      // [ONETAGATK] r205 U8: the STQ uop's cf_win_atk now holds the store's commit-time attacker RECORD (sticky atk ||
      // attacker shadow, debug-masked), read only by the D$ write request above.
      stq(idx).bits.uop.cf_win_atk.foreach(_ := io.core.commit.uops(w).cf_attacker_influence.getOrElse(false.B))
    } .elsewhen (commit_load) {
      assert (ldq(idx).valid, "[lsu] trying to commit an un-allocated load entry.")
      assert ((ldq(idx).bits.executed || ldq(idx).bits.forward_std_val) && ldq(idx).bits.succeeded ,
        "[lsu] trying to commit an un-executed load entry.")

      ldq(idx).valid                 := false.B
      ldq(idx).bits.addr.valid       := false.B
      ldq(idx).bits.executed         := false.B
      ldq(idx).bits.succeeded        := false.B
      ldq(idx).bits.order_fail       := false.B
      ldq(idx).bits.forward_std_val  := false.B

    }

    if (MEMTRACE_PRINTF) {
      when (commit_store || commit_load) {
        val uop    = Mux(commit_store, stq(idx).bits.uop, ldq(idx).bits.uop)
        val addr   = Mux(commit_store, stq(idx).bits.addr.bits, ldq(idx).bits.addr.bits)
        val stdata = Mux(commit_store, stq(idx).bits.data.bits, 0.U)
        val wbdata = Mux(commit_store, stq(idx).bits.debug_wb_data, ldq(idx).bits.debug_wb_data)
        // [CFNOISE-COMMENTED] printf("MT %x %x %x %x %x %x %x\n",
          // [CFNOISE-COMMENTED] io.core.tsc_reg, uop.uopc, uop.mem_cmd, uop.mem_size, addr, stdata, wbdata)
      }
    }

    temp_stq_commit_head = Mux(commit_store,
                               WrapInc(temp_stq_commit_head, cf_stq_active),
                               temp_stq_commit_head)

    temp_ldq_head        = Mux(commit_load,
                               WrapInc(temp_ldq_head, cf_ldq_active),
                               temp_ldq_head)
  }
  stq_commit_head := temp_stq_commit_head
  ldq_head        := temp_ldq_head

  // store has been committed AND successfully sent data to memory
  when (stq(stq_head).valid && stq(stq_head).bits.committed)
  {
    when (stq(stq_head).bits.uop.is_fence && !io.dmem.ordered) {
      io.dmem.force_order := true.B
      store_needs_order   := true.B
    }
    clear_store := Mux(stq(stq_head).bits.uop.is_fence, io.dmem.ordered,
                                                        stq(stq_head).bits.succeeded)
  }

  when (clear_store)
  {
    stq(stq_head).valid           := false.B
    stq(stq_head).bits.addr.valid := false.B
    stq(stq_head).bits.data.valid := false.B
    stq(stq_head).bits.succeeded  := false.B
    stq(stq_head).bits.committed  := false.B

    stq_head := WrapInc(stq_head, cf_stq_active)
    when (stq(stq_head).bits.uop.is_fence)
    {
      stq_execute_head := WrapInc(stq_execute_head, cf_stq_active)
    }
  }


  // -----------------------
  // Hellacache interface
  // We need to time things like a HellaCache would
  io.hellacache.req.ready := false.B
  io.hellacache.s2_nack   := false.B
  io.hellacache.s2_xcpt   := (0.U).asTypeOf(new rocket.HellaCacheExceptions)
  io.hellacache.resp.valid := false.B
  // [RESPSEL] patch 25: the hella FSM sees the old per-lane D$ response: resp(w) and st_ack(w) are
  // exclusive per lane and together equal the old resp(w) (valid, and bits whenever valid).
  // (A def, so the IFT-off build elaborates exactly the old io.dmem.resp(w) reads.)
  def hella_dresp(w: Int): Valid[BoomDCacheResp] =
    io.dmem.st_ack.map(sa => Mux(sa(w).valid, sa(w), io.dmem.resp(w))).getOrElse(io.dmem.resp(w))
  io.hellacache.store_pending := stq.map(_.valid).reduce(_||_)
  when (hella_state === h_ready) {
    io.hellacache.req.ready := true.B
    when (io.hellacache.req.fire) {
      hella_req   := io.hellacache.req.bits
      hella_state := h_s1
    }
  } .elsewhen (hella_state === h_s1) {
    can_fire_hella_incoming(memWidth-1) := true.B

    hella_data := io.hellacache.s1_data
    hella_xcpt := dtlb.io.resp(memWidth-1)

    when (io.hellacache.s1_kill) {
      when (will_fire_hella_incoming(memWidth-1) && dmem_req_fire(memWidth-1)) {
        hella_state := h_dead
      } .otherwise {
        hella_state := h_ready
      }
    } .elsewhen (will_fire_hella_incoming(memWidth-1) && dmem_req_fire(memWidth-1)) {
      hella_state := h_s2
    } .otherwise {
      hella_state := h_s2_nack
    }
  } .elsewhen (hella_state === h_s2_nack) {
    io.hellacache.s2_nack := true.B
    hella_state := h_ready
  } .elsewhen (hella_state === h_s2) {
    io.hellacache.s2_xcpt := hella_xcpt
    when (io.hellacache.s2_kill || hella_xcpt.asUInt =/= 0.U) {
      hella_state := h_dead
    } .otherwise {
      hella_state := h_wait
    }
  } .elsewhen (hella_state === h_wait) {
    for (w <- 0 until memWidth) {
      when (hella_dresp(w).valid && hella_dresp(w).bits.is_hella) {
        hella_state := h_ready

        io.hellacache.resp.valid       := true.B
        io.hellacache.resp.bits.addr   := hella_req.addr
        io.hellacache.resp.bits.tag    := hella_req.tag
        io.hellacache.resp.bits.cmd    := hella_req.cmd
        io.hellacache.resp.bits.signed := hella_req.signed
        io.hellacache.resp.bits.size   := hella_req.size
        io.hellacache.resp.bits.data   := hella_dresp(w).bits.data
      } .elsewhen (io.dmem.nack(w).valid && io.dmem.nack(w).bits.is_hella) {
        hella_state := h_replay
      }
    }
  } .elsewhen (hella_state === h_replay) {
    can_fire_hella_wakeup(memWidth-1) := true.B

    when (will_fire_hella_wakeup(memWidth-1) && dmem_req_fire(memWidth-1)) {
      hella_state := h_wait
    }
  } .elsewhen (hella_state === h_dead) {
    for (w <- 0 until memWidth) {
      when (hella_dresp(w).valid && hella_dresp(w).bits.is_hella) {
        hella_state := h_ready
      }
    }
  }

  //-------------------------------------------------------------
  // Exception / Reset

  // for the live_store_mask, need to kill stores that haven't been committed
  val st_exc_killed_mask = WireInit(VecInit((0 until numStqEntries).map(x=>false.B)))

  when (reset.asBool || io.core.exception)
  {
    ldq_head := 0.U
    ldq_tail := 0.U

    when (reset.asBool)
    {
      stq_head := 0.U
      stq_tail := 0.U
      stq_commit_head  := 0.U
      stq_execute_head := 0.U

      for (i <- 0 until numStqEntries)
      {
        stq(i).valid           := false.B
        stq(i).bits.addr.valid := false.B
        stq(i).bits.data.valid := false.B
  stq(i).bits.uop        := NullMicroOp()
      }
    }
      .otherwise // exception
    {
      stq_tail := stq_commit_head

      for (i <- 0 until numStqEntries)
      {
        when (!stq(i).bits.committed && !stq(i).bits.succeeded)
        {
          stq(i).valid           := false.B
          stq(i).bits.addr.valid := false.B
          stq(i).bits.data.valid := false.B
          st_exc_killed_mask(i)  := true.B
        }
      }
    }

    for (i <- 0 until numLdqEntries)
    {
      ldq(i).valid           := false.B
      ldq(i).bits.addr.valid := false.B
      ldq(i).bits.executed   := false.B
    }
  }

  // corefuzzing: on quiesce drain, reset all LSQ pointers to 0.
  // When this fires, queues_empty is true (ldq_head==ldq_tail, stq_commit_head==stq_tail),
  // so all queue entries are already invalid.  Resetting to 0 ensures WrapInc(ptr, cf_*_active)
  // wraps correctly within the new (possibly smaller) active range after a CSR reconfiguration.
  when (io.core.cf_lsq_quiesce_reset) {
    ldq_head         := 0.U
    ldq_tail         := 0.U
    stq_head         := 0.U
    stq_tail         := 0.U
    stq_commit_head  := 0.U
    stq_execute_head := 0.U
  }

  //-------------------------------------------------------------
  // Live Store Mask
  // track a bit-array of stores that are alive
  // (could maybe be re-produced from the stq_head/stq_tail, but need to know include spec_killed entries)

  // TODO is this the most efficient way to compute the live store mask?
  live_store_mask := next_live_store_mask &
                    ~(st_brkilled_mask.asUInt) &
                    ~(st_exc_killed_mask.asUInt)

  // for corefuzzing
  //-------------------------------------------------------------
  // Queue Empty Signals (for pipeline drain)
  
  // LDQ empty when head pointer equals tail pointer
  val ldq_empty = ldq_head === ldq_tail

  // STQ empty when commit head pointer equals tail pointer 
  val stq_empty = stq_commit_head === stq_tail

  // Queues empty when both LDQ and STQ are empty
  io.core.queues_empty := ldq_empty && stq_empty

  //-------------------------------------------------------------
  // [reconf-fix 2026-08-13] Adopt a pending LSQ geometry change (CSR 0x7c4) ONLY while the
  // queue is fully drained. See the cf_*_idx_applied declaration for the failure this fixes.
  // Stricter than ldq_empty/stq_empty above: EVERY pointer must be coincident, not just the
  // commit head, because a resize must leave no pointer outside the new active window.
  // stq_execute_head/stq_commit_head lag stq_head while stores drain to memory, so they are
  // checked explicitly. Cost is one 3-bit register per queue; the compare reuses pointers that
  // are already live, and cf_*_active becomes a registered value (one mux level off the
  // WrapInc/full-compare path). If a queue never drains the resize simply waits -- LSQs empty
  // constantly in practice, and waiting is always safe where corrupting is not.
  val ldq_drained = (ldq_head === ldq_tail)
  val stq_drained = (stq_head === stq_tail) && (stq_execute_head === stq_tail) &&
                    (stq_commit_head === stq_tail)
  when (ldq_drained) { cf_ldq_idx_applied := io.core.cf_ldq_idx }
  when (stq_drained) { cf_stq_idx_applied := io.core.cf_stq_idx }

  // this could be in a better place, but okay
  // No pending memory when we have drained LSU queues and no in-flight requests
  // [QSREG R2'] (Phase 4 timing, r142) `!io.dmem.req.valid` replaced by register-sourced
  // hellacache terms.  io.dmem.req.valid ends the LSU fire -> DTLB miss -> dmem_req chain
  // (~24 levels, 4 SLR crossings on the routed path), and its only consumer is
  // pipeline_drained_strict (core.scala), which also requires rob.io.empty and fencei_rdy
  // (=> !stq_nonempty).  Equivalence under those conjuncts, per dmem_req source:
  //   load_incoming: exe_req is a dispatched load, so it holds an LDQ entry (ldq_head =/=
  //     ldq_tail) unless flushed.  After rob.io.flush at cycle F, register-read kills at F+1
  //     (RegNext), so the last such load reaches the LSU at F+1, while the LDQ pointers reset
  //     on io.core.exception (= RegNext flush) only become visible at F+2: queues_empty = 0
  //     on every cycle a load is presented (a branch-killed load: its branch is still in
  //     the ROB, rob.io.empty = 0);
  //   load_retry / load_wakeup: need a valid LDQ entry, so ldq_head =/= ldq_tail;
  //   store_commit: needs stq_commit_e.valid, so stq_nonempty, so fencei_rdy = 0;
  //   hella_incoming / hella_wakeup: only in h_s1 / h_replay, excluded by h_ready.
  // So the new term implies the old one: never less conservative.  It is stricter only while
  // a hellacache (PTW) request is pending or in flight without a dmem request this cycle;
  // under r94 (machine mode, paging off) the PTW never issues, hella_state stays h_ready and
  // io.hellacache.req.valid stays 0, so both forms are equal on every reachable cycle and the
  // quiesce FSM timing is unchanged.  Levels: pipeline_drained_strict goes from ~33 to ~6
  // (register-sourced); LUT +1-2.
  // Only core.scala's ENABLE_RECONF quiesce logic reads no_pending_mem; without it the old
  // form is kept so those configs' Verilog does not change.
  io.core.no_pending_mem := (if (ENABLE_RECONF) io.core.queues_empty && hella_state === h_ready && !io.hellacache.req.valid
                             else io.core.queues_empty && !io.dmem.req.valid)

  // [ONETAGATK] r205 U8 (owner 2026-10-20: the attacker side is symmetric, no flash clear at close): the patch-64
  // LSQ close clear of the attacker window bits is removed with those bits (-1 FF, -160 FF-bit clear terms).

  // [RECV3] SP-08 / FIX-04: [DCFOOT] one line per load response from the D$: the footprint that load's record carries
  // (FT-3 oracle).  oc = the LDQ entry's op_count, the identity the record uses (merged_base_cf above); the response
  // uop's own cf_op_count_id is 0 for an MSHR replay (kind 2: mshrs.scala zeroes it at rpq enqueue), which is why the
  // print moved here from dcache.scala.  Verilator debug builds with +cf_state=1 (CfStatePlusArg, SP-05) only; no
  // hardware in any other build.  c = cycles since reset (the core's ift_cyc16 is its low 16 bits: same reset).
  if (ENABLE_CF_DEBUG_PRINTF && ENABLE_IFT) {
    val cf_df_on  = CfStatePlusArg(true)
    val cf_df_cyc = RegInit(0.U(32.W))
    cf_df_cyc := cf_df_cyc + 1.U
    for (w <- 0 until memWidth) {
      val d   = io.dmem.resp(w).bits.edges.dc.get
      val dli = io.dmem.resp(w).bits.uop.ldq_idx
      when (cf_df_on && io.dmem.resp(w).valid && io.dmem.resp(w).bits.uop.uses_ldq && ldq(dli).valid) {
        printf("[DCFOOT] c=%d oc=%d set=%d way=%d kind=%d\n", cf_df_cyc,
               ldq(dli).bits.uop.cf_op_count_id.get, d.set, d.way, d.kind)
      }
    }
  }
}

/**
 * Object to take an address and generate an 8-bit mask of which bytes within a
 * double-word.
 */
object GenByteMask
{
   def apply(addr: UInt, size: UInt): UInt =
   {
      val mask = Wire(UInt(8.W))
  mask := MuxCase(255.U(8.W), List(
       (size === 0.U) -> (1.U(8.W) << addr(2,0)),
       (size === 1.U) -> (3.U(8.W) << (addr(2,1) << 1.U)),
       (size === 2.U) -> Mux(addr(2), 240.U(8.W), 15.U(8.W)),
       (size === 3.U) -> 255.U(8.W)))
      mask
   }
}

/**
 * ...
 */
class ForwardingAgeLogic(num_entries: Int)(implicit p: Parameters) extends BoomModule()(p)
{
   val io = IO(new Bundle
   {
      val addr_matches    = Input(UInt(num_entries.W)) // bit vector of addresses that match
                                                       // between the load and the SAQ
      val youngest_st_idx = Input(UInt(stqAddrSz.W)) // needed to get "age"

      val forwarding_val  = Output(Bool())
      val forwarding_idx  = Output(UInt(stqAddrSz.W))
   })

   // generating mask that zeroes out anything younger than tail
   val age_mask = Wire(Vec(num_entries, Bool()))
   for (i <- 0 until num_entries)
   {
      age_mask(i) := true.B
      when (i.U >= io.youngest_st_idx) // currently the tail points PAST last store, so use >=
      {
         age_mask(i) := false.B
      }
   }

   // Priority encoder with moving tail: double length
   val matches = Wire(UInt((2*num_entries).W))
   matches := Cat(io.addr_matches & age_mask.asUInt,
                  io.addr_matches)

   val found_match = Wire(Bool())
   found_match       := false.B
   io.forwarding_idx := 0.U

   // look for youngest, approach from the oldest side, let the last one found stick
   for (i <- 0 until (2*num_entries))
   {
      when (matches(i))
      {
         found_match := true.B
         io.forwarding_idx := (i % num_entries).U
      }
   }

   io.forwarding_val := found_match
}
