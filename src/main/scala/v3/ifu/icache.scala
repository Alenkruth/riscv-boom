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

  // corefuzzing: ICache set/way reconfiguration indices (from icacheCSRCF at 0xbcb)
  val cf_icache_set_conf = Input(UInt(2.W))  // index into icacheSetOptions = Seq(64, 32, 16, 8)
  val cf_icache_way_conf = Input(UInt(2.W))  // index into cacheWayOptions  = Seq(8, 4, 2, 1)

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
  val icacheSetOptionsVec = VecInit(icacheSetOptions.map(_.U))
  val cacheWayOptionsVec  = VecInit(cacheWayOptions.map(_.U))
  val cf_icache_active_sets = icacheSetOptionsVec(io.cf_icache_set_conf)
  val cf_icache_active_ways = cacheWayOptionsVec(io.cf_icache_way_conf)
  val icache_set_mask = cf_icache_active_sets - 1.U

  val refill_paddr = RegEnable(io.s1_paddr, s1_valid && !(refill_valid || s2_miss))
  val refill_tag = refill_paddr(tagBits+untagBits-1,untagBits)
  // Mask refill set index to active sets so refills stay within active region
  val refill_idx = refill_paddr(untagBits-1,blockOffBits) & icache_set_mask
  val refill_one_beat = tl_out.d.fire && edge_out.hasData(tl_out.d.bits)

  io.req.ready := !refill_one_beat

  val (_, _, d_done, refill_cnt) = edge_out.count(tl_out.d)
  val refill_done = refill_one_beat && d_done
  tl_out.d.ready := true.B
  require (edge_out.manager.minLatency > 0)
  val replacer_LRU = outer.icacheParams.replacement_LRU
  val replacer_RAND = outer.icacheParams.replacement_RAND
  val repl_way_raw = if (isDM) 0.U else ( if (true) replacer_LRU.way else replacer_RAND.way)
  // Clamp replacement way to active ways (prevent evicting outside active region)
  val repl_way = Mux(repl_way_raw < cf_icache_active_ways, repl_way_raw, 0.U)

  // Masked set index for tag/data array reads (s0 stage uses virtual addr)
  val s0_set_idx_masked = (s0_vaddr(untagBits-1, blockOffBits) & icache_set_mask)(idxBits-1, 0)
  // Rebuild s0_vaddr with masked set index for use in data-array row functions (row/b0Row/b1Row)
  val s0_vaddr_msked = Cat(s0_vaddr(s0_vaddr.getWidth-1, untagBits), s0_set_idx_masked,
                           s0_vaddr(blockOffBits-1, 0))

  val tag_array = SyncReadMem(nSets, Vec(nWays, UInt(tagBits.W)))
  val tag_rdata = tag_array.read(s0_set_idx_masked, !refill_done && s0_valid)
  when (refill_done) {
    tag_array.write(refill_idx, VecInit(Seq.fill(nWays)(refill_tag)), Seq.tabulate(nWays)(repl_way === _.U))
  }

  // corefuzzing: per-way IFT domain BRAM — read in s0 alongside tag_array, result in s1.
  // Tracks which domain last FILLED each (set, way). On FPGA, infers as BRAM (not FFs).
  // vb_array invalidation makes all hits false → stale BRAM data is never consumed after flush.
  val ift_tag_meta = SyncReadMem(nSets, Vec(nWays, Bool()))
  val ift_rdata    = ift_tag_meta.read(s0_set_idx_masked, !refill_done && s0_valid)

  val vb_array = RegInit(0.U((nSets*nWays).W))
  when (refill_one_beat) {
    vb_array := vb_array.bitSet(Cat(repl_way, refill_idx), refill_done && !invalidated)
  }

  // corefuzzing: write ift_tag_meta on refill_done (same cycle as tag_array write, one way mask)
  val refill_domain_reg = RegEnable(io.s1_domain_id, s1_valid && !(refill_valid || s2_miss))
  when (refill_done) {
    ift_tag_meta.write(refill_idx, VecInit(Seq.fill(nWays)(refill_domain_reg)),
      Seq.tabulate(nWays)(repl_way === _.U))
  }

  when (io.invalidate) {
    vb_array := 0.U
    invalidated := true.B
    // ift_tag_meta not cleared: vb_array flush prevents any tag hits, so stale domain data is unreachable
  }

  val s2_dout   = Wire(Vec(nWays, UInt(wordBits.W)))
  val s1_bankid = Wire(Bool())

  for (i <- 0 until nWays) {
    val s1_idx = io.s1_paddr(untagBits-1,blockOffBits) & icache_set_mask
    val s1_tag = io.s1_paddr(tagBits+untagBits-1,untagBits)
    val s1_vb = vb_array(Cat(i.U, s1_idx))
    val tag = tag_rdata(i)
    // Gate hits for ways beyond the active way count
    s1_tag_hit(i) := s1_vb && tag === s1_tag && (i.U < cf_icache_active_ways)
  }
  assert(PopCount(s1_tag_hit) <= 1.U || !s1_valid)

  // corefuzzing: mux per-way IFT domain by hit way in s1 (ift_rdata was read from s0 address)
  val s1_way_domain      = Mux1H(s1_tag_hit, ift_rdata)
  val s1_domain_mismatch = s1_hit && (s1_way_domain =/= io.s1_domain_id)
  val s2_domain_mismatch = RegNext(s1_domain_mismatch && !io.s1_kill)

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

      val mem_idx = Mux(refill_one_beat, (refill_idx << log2Ceil(refillCycles)) | refill_cnt,
                    row(s0_vaddr_msked))
      when (wen) {
        dataArray.write(mem_idx, tl_out.d.bits.data)
      }
      if (enableICacheDelay)
        s2_dout(i) := dataArray.read(RegNext(mem_idx), RegNext(!wen && s0_ren))
      else
        s2_dout(i) := RegNext(dataArray.read(mem_idx, !wen && s0_ren))
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
          Mux(refill_one_beat, (refill_idx << (log2Ceil(refillCycles)-1)) | (refill_cnt >> 1.U),
          b0Row(s0_vaddr_msked))
        mem_idx1 =
          Mux(refill_one_beat, (refill_idx << (log2Ceil(refillCycles)-1)) | (refill_cnt >> 1.U),
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
          Mux(refill_one_beat, (refill_idx << log2Ceil(refillCycles)) | refill_cnt,
          b0Row(s0_vaddr_msked))
        mem_idx1 =
          Mux(refill_one_beat, (refill_idx << log2Ceil(refillCycles)) | refill_cnt,
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
      } else {
        s2_dout(i) := RegNext(Cat(dataArraysB1(i).read(mem_idx1, !wen && s0_ren),
                                  dataArraysB0(i).read(mem_idx0, !wen && s0_ren)))
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
  io.resp.bits.icache_domain_mismatch := s2_domain_mismatch
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


