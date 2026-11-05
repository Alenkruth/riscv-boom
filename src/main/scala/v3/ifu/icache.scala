//******************************************************************************
// Copyright (c) 2017 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// ICache
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.v3.ifu

import chisel3._
import chisel3.util._
import chisel3.util.random._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._
import freechips.rocketchip.rocket.{HasL1ICacheParameters, ICacheParams, ICacheErrors, ICacheReq}




import boom.v3.common._
import boom.v3.util.{BoomCoreStringPrefix}

/**
 * ICache module
 *
 * @param icacheParams parameters for the icache
 * @param hartId the id of the hardware thread in the cache
 * @param enableBlackBox use a blackbox icache
 */
class ICache(
  val icacheParams: ICacheParams,
  val staticIdForMetadataUseOnly: Int)(implicit p: Parameters)
  extends LazyModule
{
  lazy val module = new ICacheModule(this)
  val masterNode = TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(
    sourceId = IdRange(0, 1 + icacheParams.prefetch.toInt), // 0=refill, 1=hint
    name = s"Core ${staticIdForMetadataUseOnly} ICache")))))

  val size = icacheParams.nSets * icacheParams.nWays * icacheParams.blockBytes
  private val wordBytes = icacheParams.fetchBytes
}

/**
 * IO Signals leaving the ICache
 *
 * @param outer top level ICache class
 */
class ICacheResp(val outer: ICache) extends Bundle
{
  val data = UInt((outer.icacheParams.fetchBytes*8).W)
  val replay = Bool()
  val ae = Bool()
  // corefuzzing: set when the cache line was last filled by a different domain fetch
  val icache_domain_mismatch = Bool()
  // corefuzzing: set when the cache line was last filled during a secret-range fetch (same-domain)
  val icache_secret_mismatch = Bool()
}

/**
 * IO Signals for interacting with the ICache
 *
 * @param outer top level ICache class
 */
class ICacheBundle(val outer: ICache) extends BoomBundle()(outer.p)
  with HasBoomFrontendParameters
{
  val req = Flipped(Decoupled(new ICacheReq))
  val s1_paddr = Input(UInt(paddrBits.W)) // delayed one cycle w.r.t. req

  val s1_kill = Input(Bool()) // delayed one cycle w.r.t. req
  val s2_kill = Input(Bool()) // delayed two cycles; prevents I$ miss emission

  val resp = Valid(new ICacheResp(outer))
  val invalidate = Input(Bool())

  // corefuzzing: domain of s1 fetch PC (1=attacker, 0=victim), for domain mismatch detection
  val s1_domain_id = Input(Bool())
  // [M3ATKWIN] lean58: the attacker window is open (registered in the frontend); ORed into the FILL domain only
  val cf_s1_fill_atk = if (ENABLE_IFT) Some(Input(Bool())) else None
  // corefuzzing: whether s1 fetch PC is in the secret range (for secret mismatch detection)
  val s1_is_secret  = Input(Bool())
  // [ICPRED] 2026-11-04: the secret label of the prediction that chose the s1 / s2 fetch's PC (frontend.scala
  // [ICPRED]; s2 = the full label, the s1 part registered + the predecessor's f3 label).  It joins the FILL secret
  // mark and gates the s2 secret mismatch of the fetch's own refill only ([ICPRED-R1]); the s1 compare
  // (s1_is_secret) is not touched.  IFT only.
  val cf_s1_pred_sec = if (ENABLE_IFT) Some(Input(Bool())) else None
  val cf_s2_pred_sec = if (ENABLE_IFT) Some(Input(Bool())) else None
  // [ICPREDATK] 2026-11-04: the attacker label of the prediction that chose the s1 / s2 fetch's PC (frontend.scala
  // [ICPREDATK]; a victim packet's BPD / BTB / uBTB / RAS state written by the attacker).  It joins the FILL domain
  // only (cf_refill_dom_pred) and gates the s2 domain mismatch of the fetch's own refill when it alone made the line
  // attacker; the s1 domain compare (s1_domain_id) is not touched.  IFT only.
  val cf_s1_pred_atk = if (ENABLE_IFT) Some(Input(Bool())) else None
  val cf_s2_pred_atk = if (ENABLE_IFT) Some(Input(Bool())) else None
  // [ICOWNHOLD] 2026-11-04 (REVIEW icfill-atk-20261104 R1): the frontend will not take this cycle's s2 response (f3 not
  // ready: f2 replays the fetch, frontend.scala f2_replay).  An s2 hit in such a cycle does not consume the own-fill
  // flops (cf_ownfill_q, cf_ownfill_atk_q).  IFT only.
  val cf_s2_hold     = if (ENABLE_IFT) Some(Input(Bool())) else None
  // [ICFILL] runtime enable of the debug fill print (0xbc0 bit 4, cf_debug_rob_enable, as [D4RAS]); the port exists
  // only in debug-printf IFT builds (enableCfDebugPrintf && enableIFT), so other configs keep their Verilog.
  val cf_dbg_print  = if (ENABLE_CF_DEBUG_PRINTF && ENABLE_IFT) Some(Input(Bool())) else None
  // [V3STATE] SP-18 (design 5.1 IC): the I$ refill / invalidate event, registered once (the IC tap register).
  // Record/STATE builds only.
  val cf_ic_tap = if (ENABLE_IFT_BRIDGE || IFT_ORACLE) Some(Output(new IftIcTap)) else None

  // corefuzzing: ICache set/way reconfiguration indices (from icacheCSRCF at 0xbcb)
  val cf_icache_set_conf = Input(UInt(2.W))  // index into icacheSetOptions = Seq(64, 32, 16, 8)
  val cf_icache_way_conf = Input(UInt(2.W))  // index into cacheWayOptions  = Seq(8, 4, 2, 1)
  private val cevOn = p(TileKey).core.asInstanceOf[BoomCoreParams].enableCeviche   // Ceviche (C4g)
  val cf_ceviche_en     = if (cevOn) Some(Input(Bool()))     else None
  val cf_ceviche_pol    = if (cevOn) Some(Input(UInt(8.W)))  else None
  val cf_ceviche_hard_i = if (cevOn) Some(Input(UInt(10.W))) else None
  val cf_ceviche_soft_i = if (cevOn) Some(Input(UInt(10.W))) else None
  val cf_cev_inval_dom  = if (cevOn) Some(Input(Bool()))     else None
  val cf_cev_s1_dom     = if (cevOn) Some(Input(Bool()))     else None   // pure PC-range domain of the s1 fetch (F1)
  val cf_cev_dbg_flush  = if (cevOn) Some(Input(Bool()))     else None

  val perf = Output(new Bundle {
    val acquire = Bool()
  })
}

/**
 * Get a tile-specific property without breaking deduplication
 */
object GetPropertyByHartId
{
  def apply[T <: Data](tiles: Seq[RocketTileParams], f: RocketTileParams => Option[T], hartId: UInt): T = {
    PriorityMux(tiles.collect { case t if f(t).isDefined => (t.tileId.U === hartId) -> f(t).get })
  }
}

/**
 * Main ICache module
 *
 * @param outer top level ICache class
 */
class ICacheModule(outer: ICache) extends LazyModuleImp(outer)
  with HasBoomFrontendParameters
  with freechips.rocketchip.util.CoreFuzzingConstants
{
  val enableICacheDelay = tileParams.core.asInstanceOf[BoomCoreParams].enableICacheDelay
  // IFT compile-time gate: when false, ift_tag_meta has no write driver and
  // FIRRTL DCE elides the BRAM; downstream s1_domain_mismatch constant-props to
  // false and the IFU's INFL_ICACHE_STATE injection path dies at the consumer.
  val ENABLE_IFT = tileParams.core.asInstanceOf[BoomCoreParams].enableIFT
  val ENABLE_CEVICHE = tileParams.core.asInstanceOf[BoomCoreParams].enableCeviche
  // ---- Ceviche I$ (ceviche-plan-v5.md I1-I7, T5c; CevCRF / CevVSU from T1c) ----
  require(!ENABLE_CEVICHE || (!enableICacheDelay && nWays > 1), "Ceviche I$: not supported with enableICacheDelay or a direct-mapped I$")
  val cevILines = nSets * nWays                                   // 512
  val cevILineW = log2Ceil(cevILines)
  val cev_i_on  = if (ENABLE_CEVICHE) RegInit(false.B) else false.B
  val cevICrf   = if (ENABLE_CEVICHE) Some(Module(new CevCRF(cevILines, paddrBits - blockOffBits, 1, isI = true,
                                                             withSec = ENABLE_IFT, expireCycles = 64,
                                                             withIftDom = ENABLE_IFT))) else None   // [T-IFT v5] I$ IFT domain copy
  val cevIVsu   = if (ENABLE_CEVICHE) Some(Module(new CevVSU(cevILines, hardDefault = cevILines * 3 / 4, softDefault = cevILines / 2))) else None
  def pol(d: Bool): UInt = Mux(d, io.cf_ceviche_pol.get(7, 4), io.cf_ceviche_pol.get(3, 0))   // {S, I, W, R}
  val io = IO(new ICacheBundle(outer))
  val (tl_out, edge_out) = outer.masterNode.out(0)

  require(isPow2(nSets) && isPow2(nWays))
  require(usingVM)
  require(pgIdxBits >= untagBits)

  // How many bits do we intend to fetch at most every cycle?
  val wordBits = outer.icacheParams.fetchBytes*8
  // Each of these cases require some special-case handling.
  require (tl_out.d.bits.data.getWidth == wordBits || (2*tl_out.d.bits.data.getWidth == wordBits && nBanks == 2))
  // If TL refill is half the wordBits size and we have two banks, then the
  // refill writes to only one bank per cycle (instead of across two banks every
  // cycle).
  val refillsToOneBank = (2*tl_out.d.bits.data.getWidth == wordBits)

  val s0_valid = io.req.fire
  val s0_vaddr = io.req.bits.addr

  val s1_valid = RegNext(s0_valid)
  // Ceviche I$ s1 CRF search (I2). Key = the VA block address (paging off: VA = PA; the paper's L1 key is the VA tag).
  val s1_vaddr    = if (ENABLE_CEVICHE) RegNext(s0_vaddr) else s0_vaddr
  val s1_cev_hit  = if (ENABLE_CEVICHE) Wire(Bool()) else false.B
  val s1_cev_line = if (ENABLE_CEVICHE) Wire(UInt(cevILineW.W)) else 0.U(cevILineW.W)
  cevICrf.foreach { c =>
    val d = io.cf_cev_s1_dom.get
    c.io.sKey(0) := s1_vaddr.pad(paddrBits)(paddrBits - 1, blockOffBits); c.io.sDom(0) := d; c.io.sShare(0) := pol(d)(3)
    s1_cev_hit := c.io.sHit(0); s1_cev_line := c.io.sLine(0)
    c.io.incVec := Mux(cev_i_on && RegNext(s1_valid && !io.s1_kill && c.io.sHit(0)), RegNext(c.io.sMatch(0)), 0.U)
  }
  val s1_tag_hit = Wire(Vec(nWays, Bool()))
  val s1_hit = s1_tag_hit.reduce(_||_)
  val s2_valid = RegNext(s1_valid && !io.s1_kill)
  val s2_hit = RegNext(s1_hit)

  val invalidated = Reg(Bool())
  val refill_valid = RegInit(false.B)
  val refill_fire = tl_out.a.fire
  val s2_miss = s2_valid && !s2_hit && !RegNext(refill_valid)

  // corefuzzing: runtime ICache set/way reconfiguration
  // icacheSetOptions = Seq(64, 32, 16, 8); cacheWayOptions = Seq(8, 4, 2, 1)
  // All options are powers of 2, so (active_sets - 1) is a valid AND-mask.
  // [STATIC-CLAMP] each option is capped at the built size: identical constants when the module is built at
  // the max option (the fuzzing build); a static smaller build with reconf off (index 0) gets its own size.
  val icacheSetOptionsVec = VecInit(icacheSetOptions.map(o => (o min nSets).U))
  val cacheWayOptionsVec  = VecInit(cacheWayOptions.map(o => (o min nWays).U))
  val cf_icache_active_sets = icacheSetOptionsVec(io.cf_icache_set_conf)
  val cf_icache_active_ways = cacheWayOptionsVec(io.cf_icache_way_conf)
  val icache_set_mask = cf_icache_active_sets - 1.U

  // Extended tag: use bits down to the minimum active set boundary (icacheSetOptions.min = 8).
  // With the standard tag = paddr[63:untagBits], two addresses that map to the SAME masked
  // set index can share the same tag even if their full (unmasked) set indices differ.
  // Example with 8 active sets (untagBits=12): paddr[63:12] is identical for 0x80001084 and
  // 0x80001684 (both = 0x80001), but they map to different full sets (2 vs 26).  After masking,
  // both land in physical row 2 and the narrow tag comparison produces a FALSE HIT.
  //
  // Fix: widen the stored tag to paddr[63:minUntagBits] where minUntagBits uses the MINIMUM
  // active set count.  This includes the "inactive" set bits as extra tag bits, ensuring that
  // addresses differing only in those bits always produce a tag mismatch.
  // Cost: tag SRAM grows by (untagBits - minUntagBits) bits per entry (3 bits for 64→8 range).
  val icacheMinActiveSets = icacheSetOptions.min                   // = 8
  val minUntagBits = log2Ceil(icacheMinActiveSets) + blockOffBits  // = 3 + 6 = 9
  val extTagBits   = tagBits + untagBits - minUntagBits            // pAddrBits - minUntagBits

  val refill_paddr = RegEnable(io.s1_paddr, s1_valid && !(refill_valid || s2_miss))
  val refill_tag = refill_paddr(tagBits+untagBits-1, minUntagBits)
  // Mask refill set index to active sets so refills stay within active region
  // [ICVBW] (DF-icache-vb-width, owner 2026-10-22 "stage with G1 V2"): truncate to idxBits like s0_set_idx_masked.
  // icache_set_mask is 7 bits wide (the option literal 64 needs 7 bits), so the untruncated `&` made refill_idx 7 bits
  // and Cat(repl_way, refill_idx) below indexed vb_array at way*128+set: ways 4-7 landed past bit 511 and never became
  // valid (8-way I$ ran as 4-way).  The value is unchanged (bit 6 of the mask is constant 0); only the width is fixed,
  // so tag_array / ift_tag_meta / data-array addresses and the [ICFILL] set= field are unchanged.  0 logic levels.
  val refill_idx = (refill_paddr(untagBits-1,blockOffBits) & icache_set_mask)(idxBits-1, 0)
  val refill_one_beat = tl_out.d.fire && edge_out.hasData(tl_out.d.bits)

  io.req.ready := !refill_one_beat

  val (_, _, d_done, refill_cnt) = edge_out.count(tl_out.d)
  val refill_done = refill_one_beat && d_done
  tl_out.d.ready := true.B
  require (edge_out.manager.minLatency > 0)
  // [icache way fix 2026-08-22] repl_way used to come from `icacheParams.replacement_LRU`, a
  // PseudoLRU whose `state_reg` is assigned ONLY inside `.access()` -- which this file never
  // called.  The register had no driver, firtool folded it to its reset value, and repl_way
  // became a constant 0: ways 1..N-1 were never written (netlist showed
  // dataArrayB0Way_1..7_rw_wmode = 1'h0) and the I$ ran 4 KB DIRECT-MAPPED in *every*
  // configuration, baseline included.  See claude-artifacts/11-icache-way-bug.md.
  //
  // Restore upstream rocket-chip's stateless random victim (rocket/ICache.scala:406) and MASK --
  // not clamp -- it into the active region.  cacheWayOptions = Seq(8,4,2,1) are all powers of
  // two, so `& (active-1)` is exact and keeps the victim uniform across the active ways; the old
  // `Mux(raw < active, raw, 0)` would have piled every out-of-range victim onto way 0.  This is
  // the same idiom the D$ already uses correctly (dcache.scala:1068-1070).
  //
  // The explicit bit-extract pins the width to log2Ceil(nWays): repl_way feeds
  // Cat(repl_way, refill_idx) into vb_array below, and cf_icache_active_ways is a 4-bit literal
  // vector, so an un-truncated `&` would widen repl_way and index outside the valid-bit vector.
  // `random.LFSR` is qualified because freechips.rocketchip.util._ is also in scope (cf. tage.scala).
  // Ceviche: the refill's line comes from the VSU (requested at the miss, granted before the first beat is accepted).
  val cev_refill_line   = if (ENABLE_CEVICHE) Reg(UInt(cevILineW.W)) else 0.U(cevILineW.W)
  val cev_refill_line_v = if (ENABLE_CEVICHE) RegInit(false.B) else false.B
  val cev_refill_dom    = if (ENABLE_CEVICHE) RegEnable(io.cf_cev_s1_dom.get, s1_valid && !(refill_valid || s2_miss)) else false.B
  val cev_refill_rebal  = if (ENABLE_CEVICHE && io.cf_dbg_print.isDefined) Reg(Bool()) else false.B   // [CEVI] only
  val cev_refill_row    = if (ENABLE_CEVICHE) cev_refill_line(idxBits - 1, 0) else 0.U(idxBits.W)
  def replWayStock(): UInt = if (isDM) 0.U else {   // [IDOFF] the stock expression, built inside `val repl_way` so the stock names stay
    val repl_way_raw = random.LFSR(16, refill_done)(log2Ceil(nWays) - 1, 0)
    (repl_way_raw & (cf_icache_active_ways - 1.U))(log2Ceil(nWays) - 1, 0)
  }
  val repl_way = if (!ENABLE_CEVICHE) replWayStock() else Mux(cev_i_on, cev_refill_line(cevILineW - 1, idxBits), replWayStock())
  // Row of the data array written by the refill beats (the line's row in Ceviche mode, the address set otherwise).
  val refill_row = if (!ENABLE_CEVICHE) refill_idx else Mux(cev_i_on, cev_refill_row, refill_idx)
  cevIVsu.foreach { u =>
    val c = cevICrf.get
    u.io.hardCsr := io.cf_ceviche_hard_i.get; u.io.softCsr := io.cf_ceviche_soft_i.get
    u.io.occ := c.io.occ; u.io.poolEmpty := c.io.poolEmpty; u.io.freeIdx := c.io.freeIdx
    c.io.cIdx := u.io.cIdx; u.io.cV := c.io.cV; u.io.cDom := c.io.cDom; u.io.cFreq := c.io.cFreq; u.io.cRsv := c.io.cRsv
    u.io.en := cev_i_on
    u.io.req.valid := cev_i_on && refill_valid && !cev_refill_line_v
    u.io.req.bits  := cev_refill_dom
    u.io.reqId := 0.U                                                   // [T1c v3 F4] one requester (the refill); lock/roundEnd unused
    when (u.io.req.fire) { cev_refill_line := u.io.grant; cev_refill_line_v := true.B
                           if (io.cf_dbg_print.isDefined) cev_refill_rebal := u.io.rebal }
    c.io.rsvSet.valid := u.io.req.fire; c.io.rsvSet.bits := u.io.grant
    c.io.rsvSetDom := u.io.req.bits                                     // [T1c v2] = cev_refill_dom
    c.io.rsvHold.valid := false.B; c.io.rsvHold.bits := 0.U            // [R1c B1, T1c v4] the I$ has no upgrade path: tied off
    c.io.rsvRel.valid  := false.B; c.io.rsvRel.bits  := 0.U
    when (refill_done) { cev_refill_line_v := false.B }
    // Hold the D-channel beats until the line is granted. Last connect wins over the stock `tl_out.d.ready := true.B` above
    // (the plan's in-place replacement would use cev_refill_line_v before its declaration).
    tl_out.d.ready := !cev_i_on || cev_refill_line_v
    assert(!(cev_i_on && refill_one_beat && !cev_refill_line_v), "Ceviche I$: refill beat before the line grant")   // D13
  }

  // Masked set index for tag/data array reads (s0 stage uses virtual addr)
  val s0_set_idx_masked = (s0_vaddr(untagBits-1, blockOffBits) & icache_set_mask)(idxBits-1, 0)
  // Rebuild s0_vaddr with masked set index for use in data-array row functions (row/b0Row/b1Row)
  val s0_vaddr_msked = Cat(s0_vaddr(s0_vaddr.getWidth-1, untagBits), s0_set_idx_masked,
                           s0_vaddr(blockOffBits-1, 0))

  val tag_array = SyncReadMem(nSets, Vec(nWays, UInt(extTagBits.W)))
  val tag_rdata = tag_array.read(s0_set_idx_masked, !refill_done && s0_valid)
  when (refill_done) {
    tag_array.write(refill_idx, VecInit(Seq.fill(nWays)(refill_tag)), Seq.tabulate(nWays)(repl_way === _.U))
  }

  // corefuzzing: per-way IFT domain BRAM — read in s0 alongside tag_array, result in s1.
  // Tracks which domain last FILLED each (set, way). On FPGA, infers as BRAM (not FFs).
  // vb_array invalidation makes all hits false → stale BRAM data is never consumed after flush.
  val ift_tag_meta        = SyncReadMem(nSets, Vec(nWays, Bool()))
  val ift_tag_meta_secret = SyncReadMem(nSets, Vec(nWays, Bool()))
  val ift_rdata        = ift_tag_meta.read(s0_set_idx_masked, !refill_done && s0_valid)
  val ift_rdata_secret = ift_tag_meta_secret.read(s0_set_idx_masked, !refill_done && s0_valid)

  val vb_array = RegInit(0.U((nSets*nWays).W))
  when (refill_one_beat) {
    vb_array := vb_array.bitSet(Cat(repl_way, refill_idx), refill_done && !invalidated)
  }

  // corefuzzing: write ift_tag_meta on refill_done (same cycle as tag_array write, one way mask)
  val refill_domain_reg = RegEnable(if (ENABLE_IFT) io.s1_domain_id || io.cf_s1_fill_atk.get else io.s1_domain_id,   // [M3ATKWIN]
                                    s1_valid && !(refill_valid || s2_miss))
  // [ICPRED] 2026-11-04 (attack-ext psmic-nosignal-rootcause Q4: a fill steered by a sec=1 BTB/BPD prediction was
  // written sec=0): IFT on, the fill label also takes the label of the prediction that chose the missing fetch's PC.
  // - s1 part (cf_s1_pred_sec): captured with the existing s1 label, same enable.
  // - s2 label (cf_s2_pred_sec): ORed in at the missing fetch's request cycle (s2_miss && !refill_valid, the cycle
  //   tl_out.a.valid rises unless s2_kill).  The enable is low then (s2_miss), so the register holds exactly that
  //   fetch's s1 label; a request that is not sent is overwritten by the next capture as before.
  // Levels: io.s1_is_secret (the s1 range compare) still meets only the final D LUT {en, s1_is_secret, s1 part, q,
  // req && s2 label}: 0 added levels on the I$ s1 path.
  // RegInit + := (no RegNext(next, init)); IFT off: the RegEnable below is unchanged.
  val refill_secret_reg = if (ENABLE_IFT) RegInit(false.B)
                          else RegEnable(io.s1_is_secret,  s1_valid && !(refill_valid || s2_miss))
  if (ENABLE_IFT) {
    when (s1_valid && !(refill_valid || s2_miss)) {
      refill_secret_reg := io.s1_is_secret || io.cf_s1_pred_sec.get
    } .elsewhen (s2_miss && !refill_valid && io.cf_s2_pred_sec.get) {
      refill_secret_reg := true.B
    }
  }
  // [ICPREDATK] 2026-11-04 (REVIEW f0c25b0b F3, RULES C "Attacker-domain twin of the I$ fill gap"): the fill's
  // attacker label from the steering prediction, in its own bit beside refill_domain_reg (same capture and request-cycle
  // OR as refill_secret_reg [ICPRED]).  The line's written domain is refill_dom_w = refill_domain_reg ||
  // cf_refill_dom_pred (ift_tag_meta, the Ceviche CRF idom, [ICFILL], the V3STATE IC tap).  Kept apart so that
  // (1) refill_domain_reg and its D cone (io.s1_domain_id, the s1 range compare) are untouched: 0 added levels on the
  // I$ s1 domain path; the new OR sits between two flops and the BRAM write data; (2) the own-fill gate below can
  // tell a line made attacker by the label ALONE from one the PC range / [ICREDIR] / [M3ATKWIN] already made attacker
  // (whose own refetch flags today and keeps flagging).  RegInit + := (no RegNext(next, init)).  IFT off: not built,
  // refill_dom_w = refill_domain_reg (unchanged Verilog).
  val cf_refill_dom_pred = if (ENABLE_IFT) RegInit(false.B) else false.B
  if (ENABLE_IFT) {
    when (s1_valid && !(refill_valid || s2_miss)) {
      cf_refill_dom_pred := io.cf_s1_pred_atk.get
    } .elsewhen (s2_miss && !refill_valid && io.cf_s2_pred_atk.get) {
      cf_refill_dom_pred := true.B
    }
  }
  val refill_dom_w = if (ENABLE_IFT) refill_domain_reg || cf_refill_dom_pred else refill_domain_reg
  if (ENABLE_IFT) {
    when (refill_done) {
      // [T-IFT] refill_row = refill_idx in stock mode, the granted CRF line's row (with repl_way = its way) in Ceviche mode.  The read
      // above stays address-indexed: with cev_i_on its result is not selected (s1_way_domain/secret come from the CRF, I2).
      ift_tag_meta.write(refill_row, VecInit(Seq.fill(nWays)(refill_dom_w)),   // [ICPREDATK] + the prediction label
        Seq.tabulate(nWays)(repl_way === _.U))
      ift_tag_meta_secret.write(refill_row, VecInit(Seq.fill(nWays)(refill_secret_reg)),
        Seq.tabulate(nWays)(repl_way === _.U))
    }
  }

  // Ceviche I$ CRF write (I4). The line is keyed at refill_done. [T5c] The first beat of a refill also invalidates the CRF
  // entry of the line being overwritten (v := 0, rsv kept), as the stock vb_array bit of the victim way is cleared on every
  // refill beat: a fetch of the old block can no longer hit a half-written row. A valid victim needs no writeback (read-only
  // cache): the overwrite is its revocation.
  cevICrf.foreach { c =>
    val pd = pol(cev_refill_dom)
    val cev_first_beat = refill_one_beat && refill_cnt === 0.U && !d_done
    c.io.wr.valid      := cev_i_on && (refill_done || cev_first_beat)
    c.io.wr.bits.line  := cev_refill_line
    c.io.wr.bits.key   := refill_paddr >> blockOffBits
    c.io.wr.bits.v     := refill_done && !invalidated
    c.io.wr.bits.coh   := 0.U
    c.io.wr.bits.alloc := refill_done
    c.io.wr.bits.dom   := cev_refill_dom
    c.io.wr.bits.s     := pd(3); c.io.wr.bits.inv := pd(2)          // burned from the owner's Fig. 2(b) bits
    c.io.wr.bits.sec   := refill_secret_reg
    c.io.wr.bits.idom.foreach(_ := refill_dom_w)                     // [T-IFT v5] the value ift_tag_meta gets for this line ([ICPREDATK])
    // I7 [CEVI] debug record (same gate as [ICFILL]): one line per Ceviche-mode refill, in the cycle the line is keyed.
    io.cf_dbg_print.foreach { en =>
      val cevi_cyc = RegInit(0.U(40.W))
      cevi_cyc := cevi_cyc + 1.U
      when (en && cev_i_on && refill_done) {
        printf("[CEVI] c=%d line=%d dom=%d s=%d i=%d occ0=%d occ1=%d rebal=%d pa=0x%x\n", cevi_cyc, cev_refill_line,
          cev_refill_dom, pd(3), pd(2), c.io.occ(0), c.io.occ(1), cev_refill_rebal,
          (refill_paddr >> blockOffBits) << blockOffBits)
      }
    }
  }

  // [ICFILL] debug print (owner 2026-10-22, "Stage [ICFILL] print"; r17 follow-up): one line per I$ refill, in the
  // cycle the line is written (refill_done): set, way, the fill's domain and secret mark exactly as written into
  // ift_tag_meta / ift_tag_meta_secret, the line address and whether the fill is dropped (invalidated: fence.i during
  // the refill, the valid bit is not set).  Measurement only: reads existing nodes, no logic change.  Compile-time
  // enableCfDebugPrintf && enableIFT (the port above), runtime 0xbc0 bit 4 like [D4RAS]; its own cycle counter
  // (debug builds only, as d4_cyc).  Format:
  //   [ICFILL] c=<cycle> set=<set> way=<way> dom=<0|1> sec=<0|1> pa=0x<line address> inv=<0|1>
  io.cf_dbg_print.foreach { en =>
    val icf_cyc = RegInit(0.U(40.W))
    icf_cyc := icf_cyc + 1.U
    when (en && refill_done) {
      printf("[ICFILL] c=%d set=%d way=%d dom=%d sec=%d pa=0x%x inv=%d\n", icf_cyc, refill_row, repl_way,   // [T-IFT] refill_row = refill_idx in stock mode
        refill_dom_w, refill_secret_reg, (refill_paddr >> blockOffBits) << blockOffBits, invalidated)   // [ICPREDATK] dom as written
    }
  }

  // [V3STATE] SP-18 (design 5.1 IC [113:99]): refill_done (the TL-D completion, ends at the tag / ift_tag_meta write
  // enables) or io.invalidate (fence.i or debug-mode jalr, ends at vb_array).  With ic_inval alone the other IC
  // fields are 0 (design 5.1).  Registered once: E -> E+1.  Outputs only.
  io.cf_ic_tap.foreach { t =>
    require(idxBits == 6 && log2Ceil(nWays) == 3 && (new IftIcTap).getWidth == 16, "STATE IC group: 64 sets x 8 ways (design 5.1)")
    val w = Wire(new IftIcTap)
    w.v       := refill_done || io.invalidate
    w.g.inval := io.invalidate
    w.g.drop  := refill_done && invalidated
    w.g.dom   := refill_done && refill_dom_w   // [ICPREDATK] the domain as written into ift_tag_meta
    w.g.sec   := refill_done && refill_secret_reg
    w.g.set   := Mux(refill_done, refill_row, 0.U)   // [T-IFT] the line's row in Ceviche mode (refill_row = refill_idx in stock mode)
    w.g.way   := Mux(refill_done, repl_way, 0.U)
    w.g.hi    := Mux(refill_done, refill_paddr(13, 12), 0.U)
    val cf_state_ic_tap = RegNext(w, 0.U.asTypeOf(w))   // the IC tap register (named cf_state_*: SP-27, review M5)
    t := cf_state_ic_tap
  }
  val cev_i_toggle = if (ENABLE_CEVICHE) io.cf_ceviche_en.get =/= cev_i_on else false.B
  val cev_i_flush  = if (ENABLE_CEVICHE) io.invalidate || cev_i_toggle else io.invalidate
  when (cev_i_flush) {
    vb_array := 0.U
    invalidated := true.B
    // ift_tag_meta not cleared: vb_array flush prevents any tag hits, so stale domain data is unreachable
  }
  if (ENABLE_CEVICHE) {
    when (cev_i_toggle) { cev_i_on := io.cf_ceviche_en.get }
    val c = cevICrf.get
    // fence.i (paper 3e/4: no cross-domain invalidation unless the owner's I bit): revoke the executing domain's
    // lines and lines whose I bit is set. A debug-mode flush and a mode switch revoke everything.
    c.io.clearAll       := cev_i_toggle || (io.invalidate && io.cf_cev_dbg_flush.get)
    c.io.clearDom.valid := cev_i_on && io.invalidate && !io.cf_cev_dbg_flush.get
    c.io.clearDom.bits  := io.cf_cev_inval_dom.get
    assert(!cev_i_on || (io.cf_icache_set_conf === 0.U && io.cf_icache_way_conf === 0.U), "Ceviche needs the full I$ geometry")
    // [TF-LUTRAM] sSec / sIftDom are index reads at sLine: equal to the old Mux1H only for a one-hot match (G3: one own and one
    // shared copy, own first).  D13 twin, simulation / debug-print builds only (the PopCount of 512 is not built for the FPGA).
    if (tileParams.core.asInstanceOf[BoomCoreParams].enableCfDebugPrintf)
      assert(!(c.io.sHit(0) && PopCount(c.io.sMatch(0)) =/= 1.U), "Ceviche I$: multiple CRF matches")
  }

  val s2_dout   = Wire(Vec(nWays, UInt(wordBits.W)))
  val s1_bankid = Wire(Bool())

  for (i <- 0 until nWays) {
    // [ICVBW] truncated to idxBits so Cat(i.U, s1_idx) is way*nSets+set, matching the vb_array write above.
    val s1_idx = (io.s1_paddr(untagBits-1,blockOffBits) & icache_set_mask)(idxBits-1, 0)
    val s1_tag = io.s1_paddr(tagBits+untagBits-1, minUntagBits)
    val s1_vb = vb_array(Cat(i.U, s1_idx))
    val tag = tag_rdata(i)
    // Gate hits for ways beyond the active way count
    // Ceviche mode: the hit is the CRF line's way. [T5c] A hit on the way written by a refill beat this cycle is dropped
    // (the data read of that way is disabled by !wen, see I3); a false miss is replayed by the frontend like any miss.
    s1_tag_hit(i) := (if (!ENABLE_CEVICHE) s1_vb && tag === s1_tag && (i.U < cf_icache_active_ways)
                      else Mux(cev_i_on, s1_cev_hit && s1_cev_line(cevILineW - 1, idxBits) === i.U &&
                                         !(refill_one_beat && !invalidated && repl_way === i.U),
                               s1_vb && tag === s1_tag && (i.U < cf_icache_active_ways)))
  }
  assert(PopCount(s1_tag_hit) <= 1.U || !s1_valid)

  // corefuzzing: mux per-way IFT domain by hit way in s1 (ift_rdata was read from s0 address)
  // [T-IFT v5] (amend-TIFT5 I1; owner rule "IFT is never switched off by a defense") Ceviche mode: the hit line's IFT domain is the
  // CRF copy of the stock ift_tag_meta value (sIftDom, written with refill_domain_reg), not the Ceviche owner domain (sDomOut, PC range
  // without the [M3ATKWIN] / redirect terms), so ICACHE_STATE compares the same IFT domain as in stock mode.
  val s1_way_domain      = if (!ENABLE_CEVICHE) Mux1H(s1_tag_hit, ift_rdata)
                           else Mux(cev_i_on, cevICrf.get.io.sIftDom.map(_(0)).getOrElse(cevICrf.get.io.sDomOut(0)), Mux1H(s1_tag_hit, ift_rdata))
  val s1_way_secret      = if (!ENABLE_CEVICHE) Mux1H(s1_tag_hit, ift_rdata_secret) else Mux(cev_i_on, cevICrf.get.io.sSec(0),    Mux1H(s1_tag_hit, ift_rdata_secret))
  val s1_domain_mismatch = s1_hit && (s1_way_domain =/= io.s1_domain_id)
  // Secret mismatch: hit way was filled during a secret-range fetch, but current fetch is not secret.
  // Covers the same-domain channel where secret instruction's cache fill affects a non-secret fetch.
  val s1_secret_mismatch = s1_hit && s1_way_secret && !io.s1_is_secret
  // [R124GATE3] patch 28 (B51): the two s2 flops only with ENABLE_IFT (IFT off the resp bits are 0).
  val s2_domain_mismatch = if (ENABLE_IFT) RegNext(s1_domain_mismatch && !io.s1_kill) else false.B
  val s2_secret_mismatch = if (ENABLE_IFT) RegNext(s1_secret_mismatch && !io.s1_kill) else false.B

  val ramDepth = if (refillsToOneBank && nBanks == 2) {
    nSets * refillCycles / 2
  } else {
    nSets * refillCycles
  }

  val dataArrays = if (nBanks == 1) {
    // Use unbanked icache for narrow accesses.
    (0 until nWays).map { x =>
      DescribedSRAM(
        name = s"dataArrayWay_${x}",
        desc = "ICache Data Array",
        size = ramDepth,
        data = UInt((wordBits).W)
      )
    }
  } else {
    // Use two banks, interleaved.
    (0 until nWays).map { x =>
      DescribedSRAM(
        name = s"dataArrayB0Way_${x}",
        desc = "ICache Data Array",
        size = ramDepth,
        data = UInt((wordBits/nBanks).W)
      )} ++
    (0 until nWays).map { x =>
      DescribedSRAM(
        name = s"dataArrayB1Way_${x}",
        desc = "ICache Data Array",
        size = ramDepth,
        data = UInt((wordBits/nBanks).W)
      )}
  }
  if (nBanks == 1) {
    // Use unbanked icache for narrow accesses.
    s1_bankid := 0.U
    for ((dataArray, i) <- dataArrays.zipWithIndex) {
      def row(addr: UInt) = addr(untagBits-1, blockOffBits-log2Ceil(refillCycles))
      val s0_ren = s0_valid

      val wen = (refill_one_beat && !invalidated) && repl_way === i.U

      val mem_idx = Mux(refill_one_beat, (refill_row << log2Ceil(refillCycles)) | refill_cnt,
                    row(s0_vaddr_msked))
      when (wen) {
        dataArray.write(mem_idx, tl_out.d.bits.data)
      }
      if (enableICacheDelay)
        s2_dout(i) := dataArray.read(RegNext(mem_idx), RegNext(!wen && s0_ren))
      else if (!ENABLE_CEVICHE)
        s2_dout(i) := RegNext(dataArray.read(mem_idx, !wen && s0_ren))
      else {
        // Ceviche mode: read in s1 at (line row, offset) of way i, raw SRAM output used in s2; stock mode: the stock s0 read + RegNext.
        val s1_line_addr = Cat(0.U(1.W), s1_cev_line(idxBits - 1, 0), s1_vaddr(blockOffBits - 1, 0))
        val cev_ren      = s1_valid && s1_cev_hit && s1_cev_line(cevILineW - 1, idxBits) === i.U && !wen
        val raw = dataArray.read(Mux(cev_i_on, row(s1_line_addr), mem_idx), Mux(cev_i_on, cev_ren, !wen && s0_ren))
        s2_dout(i) := Mux(RegNext(cev_i_on), raw, RegNext(raw))
      }
    }
  } else {
    // Use two banks, interleaved.
    val dataArraysB0 = dataArrays.take(nWays)
    val dataArraysB1 = dataArrays.drop(nWays)
    require (nBanks == 2)

    // Bank0 row's id wraps around if Bank1 is the starting bank.
    def b0Row(addr: UInt) =
      if (refillsToOneBank) {
        addr(untagBits-1, blockOffBits-log2Ceil(refillCycles)+1) + bank(addr)
      } else {
        addr(untagBits-1, blockOffBits-log2Ceil(refillCycles)) + bank(addr)
      }
    // Bank1 row's id stays the same regardless of which Bank has the fetch address.
    def b1Row(addr: UInt) =
      if (refillsToOneBank) {
        addr(untagBits-1, blockOffBits-log2Ceil(refillCycles)+1)
      } else {
        addr(untagBits-1, blockOffBits-log2Ceil(refillCycles))
      }

    s1_bankid := RegNext(bank(s0_vaddr))

    for (i <- 0 until nWays) {
      val s0_ren = s0_valid
      val wen = (refill_one_beat && !invalidated)&& repl_way === i.U

      var mem_idx0: UInt = null
      var mem_idx1: UInt = null

      if (refillsToOneBank) {
        // write a refill beat across only one beat.
        mem_idx0 =
          Mux(refill_one_beat, (refill_row << (log2Ceil(refillCycles)-1)) | (refill_cnt >> 1.U),
          b0Row(s0_vaddr_msked))
        mem_idx1 =
          Mux(refill_one_beat, (refill_row << (log2Ceil(refillCycles)-1)) | (refill_cnt >> 1.U),
          b1Row(s0_vaddr_msked))

        when (wen && refill_cnt(0) === 0.U) {
          dataArraysB0(i).write(mem_idx0, tl_out.d.bits.data)
        }
        when (wen && refill_cnt(0) === 1.U) {
          dataArraysB1(i).write(mem_idx1, tl_out.d.bits.data)
        }
      } else {
        // write a refill beat across both banks.
        mem_idx0 =
          Mux(refill_one_beat, (refill_row << log2Ceil(refillCycles)) | refill_cnt,
          b0Row(s0_vaddr_msked))
        mem_idx1 =
          Mux(refill_one_beat, (refill_row << log2Ceil(refillCycles)) | refill_cnt,
          b1Row(s0_vaddr_msked))

        when (wen) {
          val data = tl_out.d.bits.data
          dataArraysB0(i).write(mem_idx0, data(wordBits/2-1, 0))
          dataArraysB1(i).write(mem_idx1, data(wordBits-1, wordBits/2))
        }
      }
      if (enableICacheDelay) {
        s2_dout(i) := Cat(dataArraysB1(i).read(RegNext(mem_idx1), RegNext(!wen && s0_ren)),
                          dataArraysB0(i).read(RegNext(mem_idx0), RegNext(!wen && s0_ren)))
      } else if (!ENABLE_CEVICHE) {
        s2_dout(i) := RegNext(Cat(dataArraysB1(i).read(mem_idx1, !wen && s0_ren),
                                  dataArraysB0(i).read(mem_idx0, !wen && s0_ren)))
      } else {
        // Ceviche mode: both banks read in s1 at the rows of the line's address (same row functions as the stock s0 read).
        val s1_line_addr = Cat(0.U(1.W), s1_cev_line(idxBits - 1, 0), s1_vaddr(blockOffBits - 1, 0))
        val cev_ren      = s1_valid && s1_cev_hit && s1_cev_line(cevILineW - 1, idxBits) === i.U && !wen
        val ren          = Mux(cev_i_on, cev_ren, !wen && s0_ren)
        val raw = Cat(dataArraysB1(i).read(Mux(cev_i_on, b1Row(s1_line_addr), mem_idx1), ren),
                      dataArraysB0(i).read(Mux(cev_i_on, b0Row(s1_line_addr), mem_idx0), ren))
        s2_dout(i) := Mux(RegNext(cev_i_on), raw, RegNext(raw))
      }
    }
  }
  val s2_tag_hit = RegNext(s1_tag_hit)
  val s2_hit_way = OHToUInt(s2_tag_hit)
  val s2_bankid = RegNext(s1_bankid)
  val s2_way_mux = Mux1H(s2_tag_hit, s2_dout)

  val s2_unbanked_data = s2_way_mux
  val sz = s2_way_mux.getWidth
  val s2_bank0_data = s2_way_mux(sz/2-1,0)
  val s2_bank1_data = s2_way_mux(sz-1,sz/2)

  val s2_data =
    if (nBanks == 2) {
      Mux(s2_bankid,
        Cat(s2_bank0_data, s2_bank1_data),
        Cat(s2_bank1_data, s2_bank0_data))
    } else {
      s2_unbanked_data
    }

  io.resp.bits.ae := DontCare
  io.resp.bits.replay := DontCare
  io.resp.bits.data := s2_data
  // [ICPREDATK] hit side, own-fill gate of the domain term (REVIEW F3 "the same own-fill gate on the domain mismatch";
  // its own flop, as [ICPRED-R1] for the secret term below).  Without it the steered victim fetch's refetch after its
  // own refill would hit the line the label just made attacker and raise ICACHE_STATE atk=1 on itself, but only when
  // the line was cold: a residency artifact (only the predicting packet is labelled, fetch-buffer.scala fetch_dom_atk).
  // cf_ownfill_atk_q = "the last refill request was sent for an attacker-steered fetch whose line only the label made
  // attacker (refill_domain_reg = 0), and no fetch has hit since": loaded on refill_fire with the fill's full label
  // (cf_refill_dom_pred || the s2 label: exactly the value cf_refill_dom_pred takes on that edge), cleared by the first
  // s2 hit.  A line already attacker by the PC range / [ICREDIR] / [M3ATKWIN] does not load it, so its own refetch
  // flags exactly as today.  The first hit after the request is the fetch's own refetch unless a redirect killed it
  // (each f2 replay sets f1_clear); a redirect target that hits first clears the flop and is gated only if it is
  // attacker-steered itself (the [ICPRED-R1] residual).  Cost: 1 FF + 1 LUT (D: refill_fire, label, refill_domain_reg,
  // s2_valid && s2_hit, q); the gate is one more flop input on the LUT after the s2 flop (0 added levels on that flop's
  // path; the s2 label is the [ICPRED] f3 cone).  IFT off: unchanged.
  val cf_ownfill_atk_q = if (ENABLE_IFT) RegInit(false.B) else false.B
  if (ENABLE_IFT) {
    // [ICOWNHOLD] cleared by the first s2 hit the frontend TAKES: a hit while f3 is not ready is replayed, and the
    // replay is still the fetch's own refetch (only it reaches s2 while it replays), so it keeps the gate.
    when (refill_fire) { cf_ownfill_atk_q := (cf_refill_dom_pred || io.cf_s2_pred_atk.get) && !refill_domain_reg }
      .elsewhen (s2_valid && s2_hit && !io.cf_s2_hold.get) { cf_ownfill_atk_q := false.B }
  }
  io.resp.bits.icache_domain_mismatch := (if (ENABLE_IFT) s2_domain_mismatch && !(cf_ownfill_atk_q && io.cf_s2_pred_atk.get)
                                          else s2_domain_mismatch)
  // [ICPRED] hit side: a secret-steered fetch does not flag the line ITS OWN refill marked (its refetch after the
  // fill), and only that line.  [ICPRED-R1] (review 2026-11-04 F1): the label alone does not gate the flag.  Unlike a
  // window fetch (s_prop at dispatch through ghist_secret), the steered fetch's packet gets no s_prop from its
  // predecessor's flags (fetch-buffer.scala:396-399 label only the predicting packet), so dropping its flag on a line
  // marked by ANOTHER source would lose a sec=1 edge (RULES D1 "every edge with sec=1 sets s_prop").
  // cf_ownfill_q = "the last refill request was sent for a secret-steered fetch, and no fetch has hit since": loaded
  // with the s2 label on refill_fire (tl_out.a.fire, the cycle refill_valid is set), cleared by the first s2 hit.
  // While a refill is in flight the frontend only replays the missing fetch (each replay sets f1_clear, so no other
  // fetch reaches s2), so the first hit after the request is that fetch's own refetch unless a redirect killed it; a
  // redirect target that hits first clears the flag and is gated only if it is secret-steered itself.
  // Cost: 1 FF + 1 LUT (D: refill_fire, s2 label, s2_valid, s2_hit, q); the gate adds one flop input to the existing
  // LUT after the s2 flop: 0 added levels.  IFT off: unchanged.
  val cf_ownfill_q = if (ENABLE_IFT) RegInit(false.B) else false.B
  if (ENABLE_IFT) {
    when (refill_fire) { cf_ownfill_q := io.cf_s2_pred_sec.get }
      .elsewhen (s2_valid && s2_hit && !io.cf_s2_hold.get) { cf_ownfill_q := false.B }   // [ICOWNHOLD] as above
  }
  io.resp.bits.icache_secret_mismatch := (if (ENABLE_IFT) s2_secret_mismatch && !(cf_ownfill_q && io.cf_s2_pred_sec.get)
                                          else s2_secret_mismatch)
  io.resp.valid := s2_valid && s2_hit

  tl_out.a.valid := s2_miss && !refill_valid && !io.s2_kill
  tl_out.a.bits := edge_out.Get(
    fromSource = 0.U,
    toAddress = (refill_paddr >> blockOffBits) << blockOffBits,
    lgSize = lgCacheBlockBytes.U)._2
  tl_out.b.ready := true.B
  tl_out.c.valid := false.B
  tl_out.e.valid := false.B

  io.perf.acquire := tl_out.a.fire

  when (!refill_valid) { invalidated := false.B }
  when (refill_fire) { refill_valid := true.B }
  when (refill_done) { refill_valid := false.B }

  override def toString: String = BoomCoreStringPrefix(
    "==L1-ICache==",
    "Fetch bytes   : " + cacheParams.fetchBytes,
    "Block bytes   : " + (1 << blockOffBits),
    "Row bytes     : " + rowBytes,
    "Block Bytes   : " + outer.icacheParams.blockBytes,
    "Word bits     : " + wordBits,
    "RamDepth      : " + ramDepth,
    "Sets          : " + nSets,
    "Ways          : " + nWays,
    "Refill cycles : " + refillCycles,
    "RAMs          : (" +  wordBits/nBanks + " x " + nSets*refillCycles + ") using " + nBanks + " banks",
    "" + (if (nBanks == 2) "Dual-banked" else "Single-banked"),
    "I-TLB ways    : " + cacheParams.nTLBWays + "\n")
}


