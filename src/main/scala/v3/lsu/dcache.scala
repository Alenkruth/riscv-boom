//******************************************************************************
// Ported from Rocket-Chip
// See LICENSE.Berkeley and LICENSE.SiFive in Rocket-Chip for license details.
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.v3.lsu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket._

import boom.v3.common._
import boom.v3.exu.BrUpdateInfo
import boom.v3.util.{IsKilledByBranch, GetNewBrMask, BranchKillableQueue, IsOlder, UpdateBrMask, AgePriorityEncoder, WrapInc, Transpose} 

import boom.v3.util.{BoomCoreStringPrefix, appendModuleTag, addInfluencerBatchList, InfluencerCandidate}

// import test
// import freechips.rocketchip.rocket.constants.CoreFuzzingConstants

class BoomWritebackUnit(implicit edge: TLEdgeOut, p: Parameters) extends L1HellaCacheModule()(p)
  with HasBoomExtendedTag   // [reconf-fix] cfTagLSB / cfIdxLowBits
{
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new BoomWritebackReq(edge.bundle))) // input
    val meta_read = Decoupled(new BoomL1MetaRdReq) // ready - in, rest - output
    val resp = Output(Bool())
    val idx = Output(Valid(UInt()))
    val data_req = Decoupled(new L1DataReadReq) // ready - in, rest - output
    val data_resp = Input(UInt(encRowBits.W))
    val mem_grant = Input(Bool())
    val release = Decoupled(new TLBundleC(edge.bundle)) // ready - in, rest - output
    val lsu_release = Decoupled(new TLBundleC(edge.bundle))
  })

  // refillCycles = cacheDataBeats (I think it is the number of cycles it takes to access an entire cache block)
  val req = Reg(new BoomWritebackReq(edge.bundle))
  val s_invalid :: s_fill_buffer :: s_lsu_release :: s_active :: s_grant :: Nil = Enum(5)
  val state = RegInit(s_invalid)
  val r1_data_req_fired = RegInit(false.B)
  val r2_data_req_fired = RegInit(false.B)
  val r1_data_req_cnt = Reg(UInt(log2Up(refillCycles+1).W)) // refill cycles = 8*8 / 8
  val r2_data_req_cnt = Reg(UInt(log2Up(refillCycles+1).W))
  val data_req_cnt = RegInit(0.U(log2Up(refillCycles+1).W))
  val (_, last_beat, all_beats_done, beat_count) = edge.count(io.release)
  val wb_buffer = Reg(Vec(refillCycles, UInt(encRowBits.W)))
  val acked = RegInit(false.B)

  io.idx.valid       := state =/= s_invalid
  io.idx.bits        := req.idx
  io.release.valid   := false.B
  io.release.bits    := DontCare
  io.req.ready       := false.B
  io.meta_read.valid := false.B
  io.meta_read.bits  := DontCare
  io.data_req.valid  := false.B
  io.data_req.bits   := DontCare
  io.resp            := false.B
  io.lsu_release.valid := false.B
  io.lsu_release.bits := DontCare


  // [reconf-fix] extended tag overlaps the full index on bits 12:10, so slice the index
  // to its low log2(minSets)=4 bits: block addr = Cat(tag, idx[3:0]) (spec 31 overlap rule).
  val r_address = Cat(req.tag, req.idx(log2Ceil(dcacheSetOptions.min)-1, 0)) << blockOffBits
  val id = cfg.nMSHRs
  val probeResponse = edge.ProbeAck(
                          fromSource = id.U,
                          toAddress = r_address,
                          lgSize = lgCacheBlockBytes.U,
                          reportPermissions = req.param,
                          data = wb_buffer(data_req_cnt))

  val voluntaryRelease = edge.Release(
                          fromSource = id.U,
                          toAddress = r_address,
                          lgSize = lgCacheBlockBytes.U,
                          shrinkPermissions = req.param,
                          data = wb_buffer(data_req_cnt))._2


  when (state === s_invalid) {
    io.req.ready := true.B
    when (io.req.fire) {
      state := s_fill_buffer
      data_req_cnt := 0.U
      req := io.req.bits
      acked := false.B
    }
  } .elsewhen (state === s_fill_buffer) {
    io.meta_read.valid := data_req_cnt < refillCycles.U
    io.meta_read.bits.idx := req.idx
    io.meta_read.bits.tag := req.tag

    io.data_req.valid := data_req_cnt < refillCycles.U
    io.data_req.bits.way_en := req.way_en
    io.data_req.bits.addr := (if(refillCycles > 1)
                              Cat(req.idx, data_req_cnt(log2Up(refillCycles)-1,0))
                            else req.idx) << rowOffBits

    r1_data_req_fired := false.B
    r1_data_req_cnt   := 0.U
    r2_data_req_fired := r1_data_req_fired
    r2_data_req_cnt   := r1_data_req_cnt
    when (io.data_req.fire && io.meta_read.fire) {
      r1_data_req_fired := true.B
      r1_data_req_cnt   := data_req_cnt
      data_req_cnt := data_req_cnt + 1.U
    }
    when (r2_data_req_fired) {
      wb_buffer(r2_data_req_cnt) := io.data_resp
      when (r2_data_req_cnt === (refillCycles-1).U) {
        io.resp := true.B
        state := s_lsu_release
        data_req_cnt := 0.U
      }
    }
  } .elsewhen (state === s_lsu_release) {
    io.lsu_release.valid := true.B
    io.lsu_release.bits := probeResponse
    when (io.lsu_release.fire) {
     state := s_active
    }
  } .elsewhen (state === s_active) {
    io.release.valid := data_req_cnt < refillCycles.U
    io.release.bits := Mux(req.voluntary, voluntaryRelease, probeResponse)

    when (io.mem_grant) {
      acked := true.B
    }
    when (io.release.fire) {
      data_req_cnt := data_req_cnt + 1.U
    }
    when ((data_req_cnt === (refillCycles-1).U) && io.release.fire) {
      state := Mux(req.voluntary, s_grant, s_invalid)
    }
  } .elsewhen (state === s_grant) {
    when (io.mem_grant) {
      acked := true.B
    }
    when (acked) {
      state := s_invalid
    }
  }
}

class BoomProbeUnit(implicit edge: TLEdgeOut, p: Parameters) extends L1HellaCacheModule()(p)
  with HasBoomExtendedTag   // [reconf-fix] cfTagLSB / cfIdxLowBits
{
  val io = IO(new Bundle {
    val req = Flipped(Decoupled(new TLBundleB(edge.bundle)))
    val rep = Decoupled(new TLBundleC(edge.bundle))
    val meta_read = Decoupled(new BoomL1MetaRdReq)
    val meta_write = Decoupled(new BoomL1MetaWriteReq)
    val wb_req = Decoupled(new BoomWritebackReq(edge.bundle))
    val way_en = Input(UInt(nWays.W))
    val wb_rdy = Input(Bool()) // Is writeback unit currently busy? If so need to retry meta read when its done
    val mshr_rdy = Input(Bool()) // Is MSHR ready for this request to proceed?
    val mshr_wb_rdy = Output(Bool()) // Should we block MSHR writebacks while we finish our own?
    val block_state = Input(new ClientMetadata())
    val lsu_release = Decoupled(new TLBundleC(edge.bundle))

    val state = Output(Valid(UInt(coreMaxAddrBits.W)))
  })

  val (s_invalid :: s_meta_read :: s_meta_resp :: s_mshr_req ::
       s_mshr_resp :: s_lsu_release :: s_release :: s_writeback_req :: s_writeback_resp ::
       s_meta_write :: s_meta_write_resp :: Nil) = Enum(11)
  val state = RegInit(s_invalid)

  // according to OpenAI's GPT overlords, the TLBundleB is for probe channel
  // A for data request
  // C for data response
  // D for acknowledgment
  // E for errors and error signaling
  val req = Reg(new TLBundleB(edge.bundle))
  val req_idx = req.address(idxMSB, idxLSB)
  val req_tag = req.address >> cfTagLSB   // [reconf-fix] extended tag

  val way_en = Reg(UInt())
  val tag_matches = way_en.orR
  val old_coh = Reg(new ClientMetadata)
  val miss_coh = ClientMetadata.onReset
  val reply_coh = Mux(tag_matches, old_coh, miss_coh)
  val (is_dirty, report_param, new_coh) = reply_coh.onProbe(req.param)

  io.state.valid := state =/= s_invalid
  io.state.bits  := req.address

  io.req.ready := state === s_invalid
  io.rep.valid := state === s_release
  io.rep.bits := edge.ProbeAck(req, report_param)

  assert(!io.rep.valid || !edge.hasData(io.rep.bits),
    "ProbeUnit should not send ProbeAcks with data, WritebackUnit should handle it")

  io.meta_read.valid := state === s_meta_read
  io.meta_read.bits.idx := req_idx
  io.meta_read.bits.tag := req_tag
  io.meta_read.bits.way_en := ~(0.U(nWays.W))

  io.meta_write.valid := state === s_meta_write
  io.meta_write.bits.way_en := way_en
  io.meta_write.bits.idx := req_idx
  io.meta_write.bits.tag := req_tag
  io.meta_write.bits.data.tag := req_tag
  io.meta_write.bits.data.coh := new_coh

  io.wb_req.valid := state === s_writeback_req
  io.wb_req.bits.source := req.source
  io.wb_req.bits.idx := req_idx
  io.wb_req.bits.tag := req_tag
  io.wb_req.bits.param := report_param
  io.wb_req.bits.way_en := way_en
  io.wb_req.bits.voluntary := false.B


  io.mshr_wb_rdy := !state.isOneOf(s_release, s_writeback_req, s_writeback_resp, s_meta_write, s_meta_write_resp)

  io.lsu_release.valid := state === s_lsu_release
  io.lsu_release.bits  := edge.ProbeAck(req, report_param)

  // state === s_invalid
  when (state === s_invalid) {
    when (io.req.fire) {
      state := s_meta_read
      req := io.req.bits
    }
  } .elsewhen (state === s_meta_read) {
    when (io.meta_read.fire) {
      state := s_meta_resp
    }
  } .elsewhen (state === s_meta_resp) {
    // we need to wait one cycle for the metadata to be read from the array
    state := s_mshr_req
  } .elsewhen (state === s_mshr_req) {
    old_coh := io.block_state
    way_en := io.way_en
    // if the read didn't go through, we need to retry
    state := Mux(io.mshr_rdy && io.wb_rdy, s_mshr_resp, s_meta_read)
  } .elsewhen (state === s_mshr_resp) {
    state := Mux(tag_matches && is_dirty, s_writeback_req, s_lsu_release)
  } .elsewhen (state === s_lsu_release) {
    when (io.lsu_release.fire) {
      state := s_release
    }
  } .elsewhen (state === s_release) {
    when (io.rep.ready) {
      state := Mux(tag_matches, s_meta_write, s_invalid)
    }
  } .elsewhen (state === s_writeback_req) {
    when (io.wb_req.fire) {
      state := s_writeback_resp
    }
  } .elsewhen (state === s_writeback_resp) {
    // wait for the writeback request to finish before updating the metadata
    when (io.wb_req.ready) {
      state := s_meta_write
    }
  } .elsewhen (state === s_meta_write) {
    when (io.meta_write.fire) {
      state := s_meta_write_resp
    }
  } .elsewhen (state === s_meta_write_resp) {
    state := s_invalid
  }
}

class BoomL1MetaReadReq(implicit p: Parameters) extends BoomBundle()(p) {
  val req = Vec(memWidth, new BoomL1MetaRdReq)
}

// ==== [reconf-fix 2026-08-06] Extended tag for runtime set reconfiguration =========
// See claude-artifacts/31-extended-tag-spec.md.
//
// PROBLEM: the index moves with the geometry (addr[12:6] & mask, dcacheMaskIdx) but
// rocket's untagBits is pinned at 13, so for any geometry below 128 sets the bits
// between the two boundaries (addr[12:10]) live in NEITHER the index nor the tag.
// Two distinct blocks could then share (row, tag).  `full_idx_snap` existed as a
// side-channel to recover that identity; it is deleted by this change.
//
// FIX: start the tag at the MINIMUM-geometry boundary, so it always covers every bit
// above the smallest index.  cfTagLSB = blockOffBits + log2(min sets) = 6 + 4 = 10.
//   Theorem 1 (identity): every mask is >= 0xF, so row[3:0] == addr[9:6] always;
//     therefore block address == Cat(tag, row[3:0]) and (tag,row) is exact.
//   Theorem 2 (victim):  a victim's full index == Cat(victim_tag[2:0], row[3:0]),
//     which is how old_idx is derived now that full_idx_snap is gone.
// OVERLAP RULE: tag covers addr[MSB:10] and the full index covers addr[12:6], so they
// share bits 12:10.  Every address rebuilt from (tag, idx) must slice idx to its low
// log2(minSets)=4 bits: Cat(tag, idx(3,0)).
//
// Cost: tag widens by 3 bits (+3Kbit) and full_idx_snap disappears (-7Kbit) => net less
// hardware, no new pipeline stages.  BOOM-local: rocket-chip is not modified.  Rocket's
// L1MetadataArray is generic over T <: L1Metadata, and a wider tag cannot be obtained by
// subclassing L1Metadata, hence the local bundle + array copy below.
trait HasBoomExtendedTag extends freechips.rocketchip.util.CoreFuzzingConstants {
  this: HasL1HellaCacheParameters =>
  def cfIdxLowBits = log2Ceil(dcacheSetOptions.min)               // 4
  def cfTagLSB     = blockOffBits + cfIdxLowBits                  // 10
  def cfTagBits    = paddrBits - cfTagLSB                         // 3 wider than tagBits
}

class BoomL1Metadata(implicit p: Parameters) extends L1HellaCacheBundle()(p)
  with HasBoomExtendedTag
{
  val coh = new ClientMetadata
  val tag = UInt(cfTagBits.W)
}

object BoomL1Metadata {
  def apply(tag: Bits, coh: ClientMetadata)(implicit p: Parameters) = {
    val meta = Wire(new BoomL1Metadata)
    meta.tag := tag
    meta.coh := coh
    meta
  }
}

class BoomL1MetaWriteReq(implicit p: Parameters) extends L1HellaCacheBundle()(p)
  with HasBoomExtendedTag
{
  val idx    = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
  val tag    = UInt(cfTagBits.W)
  val data   = new BoomL1Metadata
}

// [reconf-fix] The extended tag also crosses module boundaries inside these bundles.
// Rocket's versions pin `tag` to tagBits, and Chisel's := silently truncates/zero-extends
// UInts, so leaving them would corrupt the tag WITHOUT any elaboration error.
class BoomL1MetaRdReq(implicit p: Parameters) extends L1HellaCacheBundle()(p)
  with HasBoomExtendedTag
{
  val idx    = UInt(idxBits.W)
  val way_en = UInt(nWays.W)
  val tag    = UInt(cfTagBits.W)
}

class BoomWritebackReq(val params: freechips.rocketchip.tilelink.TLBundleParameters)(implicit p: Parameters)
  extends L1HellaCacheBundle()(p) with HasBoomExtendedTag
{
  val tag       = Bits(cfTagBits.W)
  val idx       = Bits(idxBits.W)
  val source    = UInt(params.sourceBits.W)
  val param     = UInt(freechips.rocketchip.tilelink.TLPermissions.cWidth.W)
  val way_en    = Bits(nWays.W)
  val voluntary = Bool()
}

// Verbatim behaviour of rocket's L1MetadataArray (HellaCache.scala:360-393); the only
// change is the relaxed type bound, since the array never inspects .tag/.coh -- it uses
// cloneType/getWidth and io.read.bits.{idx,way_en} only.
class BoomL1MetadataArray[T <: Data](onReset: () => T)(implicit p: Parameters)
  extends L1HellaCacheModule()(p)
{
  val rstVal = onReset()
  val io = IO(new Bundle {
    val read  = Flipped(Decoupled(new BoomL1MetaRdReq))
    val write = Flipped(Decoupled(new BoomL1MetaWriteReq))
    val resp  = Output(Vec(nWays, rstVal.cloneType))
  })

  val rst_cnt = RegInit(0.U(log2Up(nSets+1).W))
  val rst     = rst_cnt < nSets.U
  val waddr   = Mux(rst, rst_cnt, io.write.bits.idx)
  val wdata   = Mux(rst, rstVal, io.write.bits.data.asInstanceOf[T]).asUInt
  val wmask   = Mux(rst || (nWays == 1).B, (-1).S, io.write.bits.way_en.asSInt).asBools
  val rmask   = Mux(rst || (nWays == 1).B, (-1).S, io.read.bits.way_en.asSInt).asBools
  when (rst) { rst_cnt := rst_cnt + 1.U }

  val metabits  = rstVal.getWidth
  val tag_array = SyncReadMem(nSets, Vec(nWays, UInt(metabits.W)))

  val wen = rst || io.write.valid
  when (wen) {
    tag_array.write(waddr, VecInit.fill(nWays)(wdata), wmask)
  }
  io.resp := tag_array.read(io.read.bits.idx, io.read.fire).map(_.asTypeOf(chiselTypeOf(rstVal)))

  io.read.ready  := !wen
  io.write.ready := !rst
}

class BoomL1DataReadReq(implicit p: Parameters) extends BoomBundle()(p) {
  val req = Vec(memWidth, new L1DataReadReq)
  val valid = Vec(memWidth, Bool())
}

// Adding definition again to reduce the time spent checking the NBDCache file
//class L1DataReadReq(implicit p: Parameters) extends L1HellaCacheBundle()(p) {
//  val way_en = Bits(nWays.W) 
//  val addr   = Bits(untagBits.W) // blockOffsetBits + idxBits
//}
//
//class L1DataWriteReq(implicit p: Parameters) extends L1DataReadReq()(p) {
//  val wmask  = Bits(rowWords.W)
//  val data   = Bits(encRowBits.W)
//}

abstract class AbstractBoomDataArray(implicit p: Parameters) extends BoomModule with HasL1HellaCacheParameters {
  val io = IO(new BoomBundle {
    val read  = Input(Vec(memWidth, Valid(new L1DataReadReq)))
    val write = Input(Valid(new L1DataWriteReq))
    val resp  = Output(Vec(memWidth, Vec(nWays, Bits(encRowBits.W))))
    val nacks = Output(Vec(memWidth, Bool()))
  })
  
  def pipeMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))

}
// core fuzzing specific implementations

class BoomL1MetaReadReqCF(implicit p: Parameters) extends BoomBundle()(p) {
  val req = Vec(memWidth, new L1MetaReadReq)
}

class BoomL1DataReadReqCF(implicit p: Parameters) extends BoomBundle()(p) {
  val req = Vec(memWidth, new L1DataReadReqCF)
  val valid = Vec(memWidth, Bool())
}
// core fuzzing specific AbstractBoomDataArray definition.
abstract class AbstractBoomDataArrayCF(implicit p:Parameters) extends BoomModule with HasL1HellaCacheParameters {
  val io = IO(new BoomBundle {
    // val read  = Input(Vec(memWidth, Valid(new L1DataReadReqCF)))
    val read = Input(Vec(memWidth, Valid(new L1DataReadReq))) // test
    // val write = Input(Valid(new L1DataWriteReqCF))
    val write = Input(Valid(new L1DataWriteReq)) //test
    val resp  = Output(Vec(memWidth, Vec(nWays, Bits(encRowBits.W))))
    val nacks = Output(Vec(memWidth, Bool()))
    val cf_debug_dcache_enable = Input(Bool())
  })
  
  def pipeMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))

}

class BoomDuplicatedDataArray(implicit p: Parameters) extends AbstractBoomDataArrayCF with CoreFuzzingConstants
{

  val nBanks = 1 // adding to help with the prints during compile
  
  val waddr = io.write.bits.addr >> rowOffBits
  // [dead-code 2026-08-13] cf_idx_mask / cf_set_mask / cf_idx / cf_waddr were computed here
  // but NEVER USED -- the SRAM below is written with the raw `waddr`, which is already
  // set-masked upstream by dcacheMaskAddr() (:683).  Kept as comments rather than deleted
  // because the HARDCODED 5-bit cf_set_mask (= 32 sets) reads like a live geometry bug and
  // cost real debugging time during the 2026-08 reconfiguration investigation.
  //   val cf_idx_mask      = -1.S(idxBits.W).asUInt
  //   val cf_idx_z_mask    = 0.U(idxBits.W)
  //   val cf_set_mask      = -1.S(5.W).asUInt
  //   val cf_idx           = ((waddr & cf_idx_mask) & cf_set_mask)(4, 0)
  //   val cf_waddr_tagbits = waddr >> idxBits
  //   val cf_waddr         = (cf_waddr_tagbits << idxBits) | cf_idx

  // for (w <- 0 until memWidth) {
  //   when(io.read(w).valid && io.cf_debug_dcache_enable) {
  //     printf("[DCACHE] ADDR(TAG+OFF) is 0x%x \n", io.read(w).bits.addr)
  //   }
  // }
  // when (io.write.valid && io.cf_debug_dcache_enable) {
  //   printf("[DCACHE] ADDR(TAG+OFF) is 0x%x \n", io.write.bits.addr)
  // }

  for (j <- 0 until memWidth) {

    val raddr = io.read(j).bits.addr >> rowOffBits
    // [dead-code 2026-08-13] read-side twin of the write-side block above: cf_ridx/cf_raddr
    // were computed but NEVER USED -- the array is read with the raw `raddr`, already
    // set-masked upstream by dcacheMaskAddr() (:683).  Kept as comments because the
    // hardcoded 5-bit cf_rset_mask reads like a live 32-set geometry bug and cost real
    // debugging time.  FIRRTL DCE'd them regardless.
    //   val cf_ridx_mask     = -1.S(idxBits.W).asUInt
    //   val cf_rset_mask     = -1.S(5.W).asUInt
    //   val cf_ridx          = (raddr & cf_ridx_mask) & cf_rset_mask
    //   val cf_raddr_tagbits = raddr >> idxBits
    //   val cf_raddr         = Cat(cf_raddr_tagbits, cf_ridx)
     
    for (w <- 0 until nWays) {
      val array = DescribedSRAM(
        name = s"array_${w}_${j}",
        desc = "Non-blocking DCache Data Array",
        size = nSets * refillCycles,
        data = Vec(rowWords, Bits(encDataBits.W))
      )
      when (io.write.bits.way_en(w) && io.write.valid) {
        val data = VecInit((0 until rowWords) map (i => io.write.bits.data(encDataBits*(i+1)-1,encDataBits*i)))
        array.write(waddr, data, io.write.bits.wmask.asBools)
      }
      io.resp(j)(w) := RegNext(array.read(raddr, io.read(j).bits.way_en(w) && io.read(j).valid).asUInt)
    }
    io.nacks(j) := false.B
  }
}

class BoomBankedDataArray(implicit p: Parameters) extends AbstractBoomDataArrayCF {

  val nBanks   = boomParams.numDCacheBanks
  val bankSize = nSets * refillCycles / nBanks
  require (nBanks >= memWidth)
  require (bankSize > 0)

  val bankBits    = log2Ceil(nBanks)
  val bankOffBits = log2Ceil(rowWords) + log2Ceil(wordBytes)
  val bidxBits    = log2Ceil(bankSize)
  val bidxOffBits = bankOffBits + bankBits

  // for (w <- 0 until memWidth) {
  //   when(io.read(w).valid && io.cf_debug_dcache_enable) {
  //     printf("[DCACHE] ADDR(TAG+OFF) is 0x%x \n", io.read(w).bits.addr)
  //   }
  // }
  // when (io.write.valid && io.cf_debug_dcache_enable) {
  //   printf("[DCACHE] ADDR(TAG+OFF) is 0x%x \n", io.write.bits.addr)
  // }
  //----------------------------------------------------------------------------------------------------

  val s0_rbanks = if (nBanks > 1) VecInit(io.read.map(r => (r.bits.addr >> bankOffBits)(bankBits-1,0))) else VecInit(0.U)
  val s0_wbank  = if (nBanks > 1) (io.write.bits.addr >> bankOffBits)(bankBits-1,0) else 0.U
  val s0_ridxs  = VecInit(io.read.map(r => (r.bits.addr >> bidxOffBits)(bidxBits-1,0)))
  val s0_widx   = (io.write.bits.addr >> bidxOffBits)(bidxBits-1,0)

  val s0_read_valids    = VecInit(io.read.map(_.valid))
  val s0_bank_conflicts = pipeMap(w => (0 until w).foldLeft(false.B)((c,i) => c || io.read(i).valid && s0_rbanks(i) === s0_rbanks(w)))
  val s0_do_bank_read   = s0_read_valids zip s0_bank_conflicts map {case (v,c) => v && !c}
  val s0_bank_read_gnts = Transpose(VecInit(s0_rbanks zip s0_do_bank_read map {case (b,d) => VecInit((UIntToOH(b) & Fill(nBanks,d)).asBools)}))
  val s0_bank_write_gnt = (UIntToOH(s0_wbank) & Fill(nBanks, io.write.valid)).asBools

  //----------------------------------------------------------------------------------------------------

  val s1_rbanks         = RegNext(s0_rbanks)
  val s1_ridxs          = RegNext(s0_ridxs)
  val s1_read_valids    = RegNext(s0_read_valids)
  val s1_pipe_selection = pipeMap(i => VecInit(PriorityEncoderOH(pipeMap(j =>
                            if (j < i) s1_read_valids(j) && s1_rbanks(j) === s1_rbanks(i)
                            else if (j == i) true.B else false.B))))
  val s1_ridx_match     = pipeMap(i => pipeMap(j => if (j < i) s1_ridxs(j) === s1_ridxs(i)
                                                    else if (j == i) true.B else false.B))
  val s1_nacks          = pipeMap(w => s1_read_valids(w) && (s1_pipe_selection(w).asUInt & ~s1_ridx_match(w).asUInt).orR)
  val s1_bank_selection = pipeMap(w => Mux1H(s1_pipe_selection(w), s1_rbanks))

  //----------------------------------------------------------------------------------------------------

  val s2_bank_selection = RegNext(s1_bank_selection)
  val s2_nacks          = RegNext(s1_nacks)

  for (w <- 0 until nWays) {
    val s2_bank_reads = Reg(Vec(nBanks, Bits(encRowBits.W)))

    for (b <- 0 until nBanks) {
      val array = DescribedSRAM(
        name = s"array_${w}_${b}",
        desc = "Non-blocking DCache Data Array",
        size = bankSize,
        data = Vec(rowWords, Bits(encDataBits.W))
      )
      val ridx = Mux1H(s0_bank_read_gnts(b), s0_ridxs)
      val way_en = Mux1H(s0_bank_read_gnts(b), io.read.map(_.bits.way_en))
      s2_bank_reads(b) := array.read(ridx, way_en(w) && s0_bank_read_gnts(b).reduce(_||_)).asUInt

      when (io.write.bits.way_en(w) && s0_bank_write_gnt(b)) {
        val data = VecInit((0 until rowWords) map (i => io.write.bits.data(encDataBits*(i+1)-1,encDataBits*i)))
        array.write(s0_widx, data, io.write.bits.wmask.asBools)
      }
    }

    for (i <- 0 until memWidth) {
      io.resp(i)(w) := s2_bank_reads(s2_bank_selection(i))
    }
  }

  io.nacks := s2_nacks
}

/**
 * Top level class wrapping a non-blocking dcache.
 *
 * @param hartid hardware thread for the cache
 */
class BoomNonBlockingDCache(staticIdForMetadataUseOnly: Int)(implicit p: Parameters) extends LazyModule
{
  private val tileParams = p(TileKey)
  protected val cfg = tileParams.dcache.get

  protected def cacheClientParameters = cfg.scratch.map(x => Seq()).getOrElse(Seq(TLMasterParameters.v1(
    name          = s"Core ${staticIdForMetadataUseOnly} DCache",
    sourceId      = IdRange(0, 1 max (cfg.nMSHRs + 1)),
    supportsProbe = TransferSizes(cfg.blockBytes, cfg.blockBytes))))

  protected def mmioClientParameters = Seq(TLMasterParameters.v1(
    name          = s"Core ${staticIdForMetadataUseOnly} DCache MMIO",
    sourceId      = IdRange(cfg.nMSHRs + 1, cfg.nMSHRs + 1 + cfg.nMMIOs),
    requestFifo   = true))

  val node = TLClientNode(Seq(TLMasterPortParameters.v1(
    cacheClientParameters ++ mmioClientParameters,
    minLatency = 1)))


  lazy val module = new BoomNonBlockingDCacheModule(this)

  def flushOnFenceI = cfg.scratch.isEmpty && !node.edges.out(0).manager.managers.forall(m => !m.supportsAcquireT || !m.executable || m.regionType >= RegionType.TRACKED || m.regionType <= RegionType.IDEMPOTENT)

  require(!tileParams.core.haveCFlush || cfg.scratch.isEmpty, "CFLUSH_D_L1 instruction requires a D$")
}


class BoomDCacheBundle(implicit p: Parameters, edge: TLEdgeOut) extends BoomBundle()(p) {
  val lsu   = Flipped(new LSUDMemIO)
}

class BoomNonBlockingDCacheModule(outer: BoomNonBlockingDCache) extends LazyModuleImp(outer)
  with HasL1HellaCacheParameters
  with HasBoomCoreParameters
  with freechips.rocketchip.util.CoreFuzzingConstants
  with HasBoomExtendedTag   // [reconf-fix] cfTagLSB / cfTagBits / cfIdxLowBits
{
  implicit val edge = outer.node.edges.out(0)
  val (tl_out, _) = outer.node.out(0)
  val io = IO(new BoomDCacheBundle)

  private val fifoManagers = edge.manager.managers.filter(TLFIFOFixer.allVolatile)
  fifoManagers.foreach { m =>
    require (m.fifoId == fifoManagers.head.fifoId,
      s"IOMSHRs must be FIFO for all regions with effects, but HellaCache sees ${m.nodePath.map(_.name)}")
  }

  def widthMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))

  // for (w <- 0 until memWidth){
  //   when (io.lsu.req.valid && io.lsu.cf_debug_dcache_enable){
  //     printf("[DCACHE] LSU request address - 0x%x + DCACHE configuration set=0x%x way=0x%x repl=0x%x \n", io.lsu.req.bits(w).bits.addr, io.lsu.cf_dcache_set_conf, io.lsu.cf_dcache_way_conf, io.lsu.cf_dcache_repl_conf)
  //   }
  // }
  
  // === Dynamic DCache Reconfiguration Support ===
  // Block size (cacheBlockBytes) is NOT reconfigurable per user requirement.
  val dcacheSetOptionsVec = VecInit(dcacheSetOptions.map(_.U))
  val cacheWayOptionsVec  = VecInit(cacheWayOptions.map(_.U))

  // ==== [reconf-fix 2026-08-06] Config commit gate ==========================
  // The 0xbc1 CSR value is a REQUEST; it is applied to the live mapping only on a
  // cycle with no cache transaction in flight.
  //
  // WHY (proven on waveform, doc 28/30): io.lsu.ordered (below) is
  // `mshrs.fence_rdy && !s1_valid && !s2_valid` — it has NO writeback-unit term, so a
  // software FENCE returns while a voluntary writeback is still in the WB unit.  The
  // csrw then changed the mask underneath it and the WB's data read (dcacheMaskAddr on
  // wb.io.data_req) fetched the WRONG physical row, writing that data back under the
  // evicted block's address: L2 ended up holding sweep-region zeros for a live line.
  // L2-initiated probes (continuous TSI tohost polling) are the same hazard and cannot
  // be ordered by any software fence.  Staging + apply-on-idle closes both.
  //
  // Only these two registers drive the live geometry.  cf_dcache_repl_conf is
  // deliberately NOT staged: a policy change remaps nothing, and dcache_clean()
  // depends on it taking effect immediately.
  // Timing/area: 2x2b of state; the idle term is an AND of existing quasi-static
  // signals; nothing is added to the s0-s2 datapath.  In a build without
  // reconfiguration these become plain wires off the (tied-off) CSR, so the whole
  // gate constant-folds out of the netlist.
  // The update logic lives further down (search "[reconf-fix] config commit"), after
  // mshrs/wb/prober/s1_valid/s2_valid exist; only the state is declared here because
  // the mask below consumes it.
  val cfg_set_applied = if (ENABLE_RECONF) RegInit(0.U(2.W))
                        else WireDefault(io.lsu.cf_dcache_set_conf)
  val cfg_way_applied = if (ENABLE_RECONF) RegInit(0.U(2.W))
                        else WireDefault(io.lsu.cf_dcache_way_conf)
  // Staged = a geometry change has been requested but not yet applied.  Level-derived
  // (no CSR write pulse needed): the CSR value only moves on a write, and the flag
  // self-clears when the values match again.
  val cfg_change = (io.lsu.cf_dcache_set_conf =/= cfg_set_applied) ||
                   (io.lsu.cf_dcache_way_conf =/= cfg_way_applied)

  val cf_dcache_active_sets = dcacheSetOptionsVec(cfg_set_applied)
  val cf_dcache_active_ways = cacheWayOptionsVec(cfg_way_applied)
  val dcache_set_mask = cf_dcache_active_sets - 1.U
  // Deferred assignment: driven in the "[reconf-fix] config commit" block below.
  val cfg_apply_now = WireDefault(false.B)

  // [reconf-fix 2026-08-06] full_idx_snap DELETED.  It existed only to recover the
  // identity bits that fell between the masked index and the pinned tag; the extended
  // tag (cfTagLSB=10) now carries them, so (row, tag) is exact -- Theorem 1, spec 31.

  // Mask just the set-index bits [untagBits-1 : blockOffBits] in a physical address.
  def dcacheMaskIdx(addr: UInt): UInt =
    (addr(untagBits-1, blockOffBits) & dcache_set_mask)(idxBits-1, 0)
  def dcacheMaskAddr(addr: UInt): UInt = {
    val idx_part = dcacheMaskIdx(addr)
    val off_part = addr(blockOffBits-1, 0)
    if (addr.getWidth > untagBits)
      Cat(addr(addr.getWidth-1, untagBits), idx_part, off_part)
    else
      Cat(idx_part, off_part)
  }

  val t_replay :: t_probe :: t_wb :: t_mshr_meta_read :: t_lsu :: t_prefetch :: Nil = Enum(6)

  val wb = Module(new BoomWritebackUnit)
  val prober = Module(new BoomProbeUnit)
  val mshrs = Module(new BoomMSHRFile)
  mshrs.io.clear_all    := io.lsu.force_order
  // [restored 2026-09-05] driver destroyed by an over-greedy probe strip; recovered from
  // the pre-damage elaborated Verilog, which named it exactly:
  //   .io_clear_prefetch_all (cfg_change)   @[dcache.scala:677:68]
  // [reconf-fix] staged geometry change retires parked prefetches (see mshrs.scala:596).
  mshrs.io.clear_prefetch_all := cfg_change
  // [restored 2026-09-05] also destroyed by the probe strip; recovered from the
  // pre-damage Verilog: .io_dcache_set_mask (_dcache_set_mask_T) @[dcache.scala:682:47]
  mshrs.io.dcache_set_mask := dcache_set_mask
  mshrs.io.brupdate     := io.lsu.brupdate
  mshrs.io.exception    := io.lsu.exception
  mshrs.io.rob_pnr_idx  := io.lsu.rob_pnr_idx
  mshrs.io.rob_head_idx := io.lsu.rob_head_idx

  // tags
  // [reconf-fix] BOOM-local metadata carrying the extended tag (see HasBoomExtendedTag).
  def onReset = BoomL1Metadata(0.U, ClientMetadata.onReset)
  val meta = Seq.fill(memWidth) { Module(new BoomL1MetadataArray(onReset _)) }
  // [reconf-fix Phase C] 3rd write port = invalidate-all walker (reconf builds
  // only — baseline elaborates the identical 2-port arbiter).
  val metaWriteArb = Module(new Arbiter(new BoomL1MetaWriteReq, if (ENABLE_RECONF) 3 else 2))
  // 0 goes to MSHR refills, 1 goes to prober, 2 (reconf only) invalidate walker
  val metaReadArb = Module(new Arbiter(new BoomL1MetaReadReq, 6))
  // 0 goes to MSHR replays, 1 goes to prober, 2 goes to wb, 3 goes to MSHR meta read,
  // 4 goes to pipeline, 5 goes to prefetcher

  metaReadArb.io.in := DontCare
  for (w <- 0 until memWidth) {
    meta(w).io.write.valid := metaWriteArb.io.out.fire
    meta(w).io.write.bits  := metaWriteArb.io.out.bits
    meta(w).io.read.valid  := metaReadArb.io.out.valid
    meta(w).io.read.bits   := metaReadArb.io.out.bits.req(w)
  }
  // fix from issue #667 in the riscv-boom repository
  metaReadArb.io.out.ready := meta.map(_.io.read.ready).reduce(_&&_)
  metaWriteArb.io.out.ready := meta.map(_.io.write.ready).reduce(_&&_)
  //metaReadArb.io.out.ready  := meta.map(_.io.read.ready).reduce(_||_)
  //metaWriteArb.io.out.ready := meta.map(_.io.write.ready).reduce(_||_)
  

  // data
  val data = Module(if (boomParams.numDCacheBanks == 1) new BoomDuplicatedDataArray else new BoomBankedDataArray)
  val dataWriteArb = Module(new Arbiter(new L1DataWriteReq, 2))
  // 0 goes to pipeline, 1 goes to MSHR refills
  val dataReadArb = Module(new Arbiter(new BoomL1DataReadReq, 3))
  // 0 goes to MSHR replays, 1 goes to wb, 2 goes to pipeline

  // corefuzzing address checker
  // val dcache_address_checker = Module(new AddressChecker)
  data.io.cf_debug_dcache_enable := io.lsu.cf_debug_dcache_enable

  dataReadArb.io.in := DontCare

  for (w <- 0 until memWidth) {
    data.io.read(w).valid := dataReadArb.io.out.bits.valid(w) && dataReadArb.io.out.valid
    data.io.read(w).bits  := dataReadArb.io.out.bits.req(w)
  }
  dataReadArb.io.out.ready := true.B

  data.io.write.valid := dataWriteArb.io.out.fire
  // assigning the wires individually to add the tag insertion
  // corefuzzing - ak
  //data.io.write.bits.addr := dataWriteArb.io.out.bits.addr
  // dcache_address_checker.io.in := dataWriteArb.io.out.bits.addr
  // data.io.write.bits.data := Cat(dcache_address_checker.io.out, 
  //data.io.write.bits.data := dataWriteArb.io.out.bits.data // (encDataBits-1, 0))
  //printf("\nTag value %x\n", dcache_address_checker.io.out)
  //printf("\nTag concatenated with the data value %x\n", Cat(dcache_address_checker.io.out, dataWriteArb.io.out.bits.data))
  //data.io.write.bits.wmask := dataWriteArb.io.out.bits.wmask
  //data.io.write.bits.way_en := dataWriteArb.io.out.bits.way_en
  // data.io.write.bits.p := dataWriteArb.io.out.bits.p

  data.io.write.bits  := dataWriteArb.io.out.bits
  dataWriteArb.io.out.ready := true.B

  // ------------
  // New requests

  io.lsu.req.ready := metaReadArb.io.in(4).ready && dataReadArb.io.in(2).ready
  metaReadArb.io.in(4).valid := io.lsu.req.valid
  dataReadArb.io.in(2).valid := io.lsu.req.valid

  // MetaRead(4) goes to the pipeline
  // DataRead(2) goes to the pipeline

  // ideally masking the fields/updating the inputs to the arbiter from the pipeline 
  // should trickle down to the requests from the MSHR, prefetchers, probers.

  // Writeback might be affected though. So, we might have to create and propagate a copy of the
  // original address to the writeback unit.
  for (w <- 0 until memWidth) {
    // Tag read for new requests (masked set index for runtime size restriction)
    metaReadArb.io.in(4).bits.req(w).idx    := dcacheMaskIdx(io.lsu.req.bits(w).bits.addr)
    metaReadArb.io.in(4).bits.req(w).way_en := DontCare
    metaReadArb.io.in(4).bits.req(w).tag    := DontCare
    // Data read for new requests (masked set index)
    dataReadArb.io.in(2).bits.valid(w)      := io.lsu.req.bits(w).valid
    dataReadArb.io.in(2).bits.req(w).addr   := dcacheMaskAddr(io.lsu.req.bits(w).bits.addr)
    dataReadArb.io.in(2).bits.req(w).way_en := ~0.U(nWays.W)
  }

  // ------------
  // MSHR Replays
  val replay_req = Wire(Vec(memWidth, new BoomDCacheReq))
  replay_req               := DontCare
  replay_req(0).uop        := mshrs.io.replay.bits.uop
  replay_req(0).addr       := mshrs.io.replay.bits.addr
  replay_req(0).data       := mshrs.io.replay.bits.data
  replay_req(0).is_hella   := mshrs.io.replay.bits.is_hella
  replay_req(0).cf_addr_sec.foreach(_ := false.B)   // [RESID] per-MSHR, see mshrs.scala
  replay_req(0).cf_addr_atk.foreach(_ := false.B)   // [ATKRES]
  replay_req(0).cf_res_v.foreach(_ := mshrs.io.replay.bits.cf_res_v.getOrElse(false.B))   // [RESID] miss-response edge
  replay_req(0).cf_res_pc.foreach(_ := mshrs.io.replay.bits.cf_res_pc.getOrElse(0.U))
  replay_req(0).cf_res_atk.foreach(_ := mshrs.io.replay.bits.cf_res_atk.getOrElse(false.B))   // [ATKEVICT]
  replay_req(0).cf_res_esec.foreach(_ := mshrs.io.replay.bits.cf_res_esec.getOrElse(false.B))
  mshrs.io.replay.ready    := metaReadArb.io.in(0).ready && dataReadArb.io.in(0).ready
  // Tag read for MSHR replays
  metaReadArb.io.in(0).valid              := mshrs.io.replay.valid
  metaReadArb.io.in(0).bits.req(0).idx    := dcacheMaskIdx(mshrs.io.replay.bits.addr)
  metaReadArb.io.in(0).bits.req(0).way_en := DontCare
  metaReadArb.io.in(0).bits.req(0).tag    := DontCare
  // Data read for MSHR replays
  dataReadArb.io.in(0).valid              := mshrs.io.replay.valid
  dataReadArb.io.in(0).bits.req(0).addr   := dcacheMaskAddr(mshrs.io.replay.bits.addr)
  dataReadArb.io.in(0).bits.req(0).way_en := mshrs.io.replay.bits.way_en
  dataReadArb.io.in(0).bits.valid         := widthMap(w => (w == 0).B)

  // -----------
// MSHR Meta read
val mshr_read_req = Wire(Vec(memWidth, new BoomDCacheReq))
mshr_read_req             := DontCare
mshr_read_req(0).uop      := NullMicroOp
mshr_read_req(0).addr     := Cat(mshrs.io.meta_read.bits.tag,
                                 mshrs.io.meta_read.bits.idx(cfIdxLowBits-1, 0)) << blockOffBits  // [reconf-fix] overlap
mshr_read_req(0).data     := DontCare
mshr_read_req(0).is_hella := false.B
mshr_read_req(0).cf_addr_sec.foreach(_ := false.B)   // [RESID]
mshr_read_req(0).cf_addr_atk.foreach(_ := false.B)   // [ATKRES]
mshr_read_req(0).cf_res_v.foreach(_ := false.B)
mshr_read_req(0).cf_res_atk.foreach(_ := false.B)   // [ATKEVICT]
mshr_read_req(0).cf_res_esec.foreach(_ := false.B)
metaReadArb.io.in(3).valid       := mshrs.io.meta_read.valid
metaReadArb.io.in(3).bits.req(0) := mshrs.io.meta_read.bits
metaReadArb.io.in(3).bits.req(0).idx := mshrs.io.meta_read.bits.idx & dcache_set_mask(idxBits-1, 0)
mshrs.io.meta_read.ready         := metaReadArb.io.in(3).ready

// -----------
// Write-backs
val wb_fire = wb.io.meta_read.fire && wb.io.data_req.fire
val wb_req = Wire(Vec(memWidth, new BoomDCacheReq))
wb_req             := DontCare
wb_req(0).uop      := NullMicroOp
// [reconf-fix] data_req.addr is {idx[6:0], off[5:0]}; keep only {idx[3:0], off[5:0]} so the
// extended tag supplies bits 12:10 exactly once.
wb_req(0).addr     := Cat(wb.io.meta_read.bits.tag,
                          wb.io.data_req.bits.addr(cfIdxLowBits+blockOffBits-1, 0))
wb_req(0).data     := DontCare
wb_req(0).is_hella := false.B
wb_req(0).cf_addr_sec.foreach(_ := false.B)   // [RESID]
wb_req(0).cf_addr_atk.foreach(_ := false.B)   // [ATKRES]
wb_req(0).cf_res_v.foreach(_ := false.B)
wb_req(0).cf_res_atk.foreach(_ := false.B)   // [ATKEVICT]
wb_req(0).cf_res_esec.foreach(_ := false.B)
// Tag read for write-back
metaReadArb.io.in(2).valid        := wb.io.meta_read.valid
metaReadArb.io.in(2).bits.req(0)  := wb.io.meta_read.bits
metaReadArb.io.in(2).bits.req(0).idx := wb.io.meta_read.bits.idx & dcache_set_mask(idxBits-1, 0)
wb.io.meta_read.ready := metaReadArb.io.in(2).ready && dataReadArb.io.in(1).ready
// Data read for write-back
dataReadArb.io.in(1).valid        := wb.io.data_req.valid
dataReadArb.io.in(1).bits.req(0)  := wb.io.data_req.bits
dataReadArb.io.in(1).bits.req(0).addr := dcacheMaskAddr(wb.io.data_req.bits.addr)
dataReadArb.io.in(1).bits.valid   := widthMap(w => (w == 0).B)
wb.io.data_req.ready  := metaReadArb.io.in(2).ready && dataReadArb.io.in(1).ready
assert(!(wb.io.meta_read.fire ^ wb.io.data_req.fire))

// -------
// Prober
val prober_fire  = prober.io.meta_read.fire
val prober_req   = Wire(Vec(memWidth, new BoomDCacheReq))
prober_req             := DontCare
prober_req(0).uop      := NullMicroOp
prober_req(0).addr     := Cat(prober.io.meta_read.bits.tag,
                              prober.io.meta_read.bits.idx(cfIdxLowBits-1, 0)) << blockOffBits  // [reconf-fix] overlap
prober_req(0).data     := DontCare
prober_req(0).is_hella := false.B
prober_req(0).cf_addr_sec.foreach(_ := false.B)   // [RESID]
prober_req(0).cf_addr_atk.foreach(_ := false.B)   // [ATKRES]
prober_req(0).cf_res_v.foreach(_ := false.B)
prober_req(0).cf_res_atk.foreach(_ := false.B)   // [ATKEVICT]
prober_req(0).cf_res_esec.foreach(_ := false.B)
// Tag read for prober
metaReadArb.io.in(1).valid       := prober.io.meta_read.valid
metaReadArb.io.in(1).bits.req(0) := prober.io.meta_read.bits
metaReadArb.io.in(1).bits.req(0).idx := prober.io.meta_read.bits.idx & dcache_set_mask(idxBits-1, 0)
//metaReadArb.io.in(1).bits.req(0).way_en := DontCare
// metaReadArb.io.in(1).bits.req(0).tag := getTag(prober_req(0).addr)
prober.io.meta_read.ready := metaReadArb.io.in(1).ready
// Prober does not need to read data array

// -------
// Prefetcher
val prefetch_fire = mshrs.io.prefetch.fire
val prefetch_req  = Wire(Vec(memWidth, new BoomDCacheReq))
prefetch_req    := DontCare
prefetch_req(0) := mshrs.io.prefetch.bits
// Tag read for prefetch
metaReadArb.io.in(5).valid              := mshrs.io.prefetch.valid
metaReadArb.io.in(5).bits.req(0).idx    := dcacheMaskIdx(mshrs.io.prefetch.bits.addr)
metaReadArb.io.in(5).bits.req(0).way_en := DontCare
metaReadArb.io.in(5).bits.req(0).tag    := DontCare // getTag(mshrs.io.prefetch.bits.addr)
mshrs.io.prefetch.ready := metaReadArb.io.in(5).ready
// Prefetch does not need to read data array

  val s0_valid = Mux(io.lsu.req.fire, VecInit(io.lsu.req.bits.map(_.valid)),
                 Mux(mshrs.io.replay.fire || wb_fire || prober_fire || prefetch_fire || mshrs.io.meta_read.fire,
                                        VecInit(1.U(memWidth.W).asBools), VecInit(0.U(memWidth.W).asBools)))
  val s0_req   = Mux(io.lsu.req.fire        , VecInit(io.lsu.req.bits.map(_.bits)),
                 Mux(wb_fire                  , wb_req,
                 Mux(prober_fire              , prober_req,
                 Mux(prefetch_fire            , prefetch_req,
                 Mux(mshrs.io.meta_read.fire, mshr_read_req
                                              , replay_req)))))
  val s0_type  = Mux(io.lsu.req.fire        , t_lsu,
                 Mux(wb_fire                  , t_wb,
                 Mux(prober_fire              , t_probe,
                 Mux(prefetch_fire            , t_prefetch,
                 Mux(mshrs.io.meta_read.fire, t_mshr_meta_read
                                              , t_replay)))))

  // Does this request need to send a response or nack
  val s0_send_resp_or_nack = Mux(io.lsu.req.fire, s0_valid,
    VecInit(Mux(mshrs.io.replay.fire && isRead(mshrs.io.replay.bits.uop.mem_cmd), 1.U(memWidth.W), 0.U(memWidth.W)).asBools))


  val s1_req          = RegNext(s0_req)
  for (w <- 0 until memWidth)
    s1_req(w).uop.br_mask := GetNewBrMask(io.lsu.brupdate, s0_req(w).uop)
  val s2_store_failed = Wire(Bool())
  val s1_valid = widthMap(w =>
                 RegNext(s0_valid(w)                                     &&
                         !IsKilledByBranch(io.lsu.brupdate, s0_req(w).uop) &&
                         !(io.lsu.exception && s0_req(w).uop.uses_ldq)   &&
                         !(s2_store_failed && io.lsu.req.fire && s0_req(w).uop.uses_stq),
                         init=false.B))
  for (w <- 0 until memWidth)
    assert(!(io.lsu.s1_kill(w) && !RegNext(io.lsu.req.fire) && !RegNext(io.lsu.req.bits(w).valid)))
  val s1_addr         = s1_req.map(_.addr)
  // Compare masked indices: prober.io.meta_write.bits.idx is the full unmasked probe idx;
  // the meta_write is overridden to the masked idx at the metaWriteArb (below), so the
  // nack must also compare against the masked version of each to avoid missed nacks when
  // a prober at full_idx=26 and an LSU at full_idx=10 share the same masked set.
  val s1_nack         = s1_addr.map(a => dcacheMaskIdx(a) === (prober.io.meta_write.bits.idx & dcache_set_mask(idxBits-1,0)) && !prober.io.req.ready)
  val s1_send_resp_or_nack = RegNext(s0_send_resp_or_nack)
  val s1_type         = RegNext(s0_type)

  val s1_mshr_meta_read_way_en = RegNext(mshrs.io.meta_read.bits.way_en)
  val s1_replay_way_en         = RegNext(mshrs.io.replay.bits.way_en) // For replays, the metadata isn't written yet
  val s1_wb_way_en             = RegNext(wb.io.data_req.bits.way_en)

  // tag check
  def wayMap[T <: Data](f: Int => T) = VecInit((0 until nWays).map(f))
  // [reconf-fix] compare from cfTagLSB(10), not untagBits(13): the tag must cover every
  // bit above the SMALLEST index so (row, tag) is exact at every geometry (Theorem 1).
  val s1_tag_eq_way = widthMap(i => wayMap((w: Int) => meta(i).io.resp(w).tag === (s1_addr(i) >> cfTagLSB)).asUInt)

  // [reconf-fix] s1_full_idx_snap / s1_full_idx_match DELETED (see above): the tag
  // comparison alone now disambiguates lines sharing a masked row.

  val s1_tag_match_way = widthMap(i =>
                         Mux(s1_type === t_replay, s1_replay_way_en,
                         Mux(s1_type === t_wb,     s1_wb_way_en,
                         Mux(s1_type === t_mshr_meta_read, s1_mshr_meta_read_way_en,
                           // Gate ways >= cf_dcache_active_ways.  The extended tag makes the
                           // tag compare sufficient to disambiguate same-row lines, so the
                           // old full_idx_match conjunct is gone (spec 31, Corollary 1).
                           wayMap((w: Int) => s1_tag_eq_way(i)(w) && meta(i).io.resp(w).coh.isValid() && (w.U < cf_dcache_active_ways)).asUInt))))

  // [reconf-fix 2026-08-12] Compare PHYSICAL rows: the WB unit occupies a masked row,
  // so at a reduced set count an aliasing access must also be nacked.
  val s1_wb_idx_matches = widthMap(i => ((s1_addr(i)(untagBits-1,blockOffBits) & dcache_set_mask(idxBits-1,0)) ===
                                         (wb.io.idx.bits & dcache_set_mask(idxBits-1,0))) && wb.io.idx.valid)

  val s2_req   = RegNext(s1_req)
  val s2_type  = RegNext(s1_type)
  val s2_valid = widthMap(w =>
                  RegNext(s1_valid(w) &&
                         !io.lsu.s1_kill(w) &&
                         !IsKilledByBranch(io.lsu.brupdate, s1_req(w).uop) &&
                         !(io.lsu.exception && s1_req(w).uop.uses_ldq) &&
                         !(s2_store_failed && (s1_type === t_lsu) && s1_req(w).uop.uses_stq)))
  for (w <- 0 until memWidth)
    s2_req(w).uop.br_mask := GetNewBrMask(io.lsu.brupdate, s1_req(w).uop)

  val s2_tag_match_way = RegNext(s1_tag_match_way)
  val s2_tag_match     = s2_tag_match_way.map(_.orR)
  val s2_hit_state     = widthMap(i => Mux1H(s2_tag_match_way(i), wayMap((w: Int) => RegNext(meta(i).io.resp(w).coh))))
  val s2_has_permission = widthMap(w => s2_hit_state(w).onAccess(s2_req(w).uop.mem_cmd)._1)
  val s2_new_hit_state  = widthMap(w => s2_hit_state(w).onAccess(s2_req(w).uop.mem_cmd)._3)

  val s2_hit = widthMap(w => (s2_tag_match(w) && s2_has_permission(w) && s2_hit_state(w) === s2_new_hit_state(w) && !mshrs.io.block_hit(w)) || s2_type.isOneOf(t_replay, t_wb))
  val s2_nack = Wire(Vec(memWidth, Bool()))
  assert(!(s2_type === t_replay && !s2_hit(0)), "Replays should always hit")
  assert(!(s2_type === t_wb && !s2_hit(0)), "Writeback should always see data hit")

  val s2_wb_idx_matches = RegNext(s1_wb_idx_matches)

  // lr/sc
  val debug_sc_fail_addr = RegInit(0.U)
  val debug_sc_fail_cnt  = RegInit(0.U(8.W))

  val lrsc_count = RegInit(0.U(log2Ceil(lrscCycles).W))
  val lrsc_valid = lrsc_count > lrscBackoff.U
  val lrsc_addr  = Reg(UInt())
  val s2_lr = s2_req(0).uop.mem_cmd === M_XLR && (!RegNext(s1_nack(0)) || s2_type === t_replay)
  val s2_sc = s2_req(0).uop.mem_cmd === M_XSC && (!RegNext(s1_nack(0)) || s2_type === t_replay)
  val s2_lrsc_addr_match = widthMap(w => lrsc_valid && lrsc_addr === (s2_req(w).addr >> blockOffBits))
  val s2_sc_fail = s2_sc && !s2_lrsc_addr_match(0)
  when (lrsc_count > 0.U) { lrsc_count := lrsc_count - 1.U }
  when (s2_valid(0) && ((s2_type === t_lsu && s2_hit(0) && !s2_nack(0)) ||
                     (s2_type === t_replay && s2_req(0).uop.mem_cmd =/= M_FLUSH_ALL))) {
    when (s2_lr) {
      lrsc_count := (lrscCycles - 1).U
      lrsc_addr := s2_req(0).addr >> blockOffBits
    }
    when (lrsc_count > 0.U) {
      lrsc_count := 0.U
    }
  }
  for (w <- 0 until memWidth) {
    when (s2_valid(w)                            &&
      s2_type === t_lsu                          &&
      !s2_hit(w)                                 &&
      !(s2_has_permission(w) && s2_tag_match(w)) &&
      s2_lrsc_addr_match(w)                      &&
      !s2_nack(w)) {
      lrsc_count := 0.U
    }
  }

  when (s2_valid(0)) {
    when (s2_req(0).addr === debug_sc_fail_addr) {
      when (s2_sc_fail) {
        debug_sc_fail_cnt := debug_sc_fail_cnt + 1.U
      } .elsewhen (s2_sc) {
        debug_sc_fail_cnt := 0.U
      }
    } .otherwise {
      when (s2_sc_fail) {
        debug_sc_fail_addr := s2_req(0).addr
        debug_sc_fail_cnt  := 1.U
      }
    }
  }
  assert(debug_sc_fail_cnt < 100.U, "L1DCache failed too many SCs in a row")

  val s2_data = Wire(Vec(memWidth, Vec(nWays, UInt(encRowBits.W))))
  for (i <- 0 until memWidth) {
    for (w <- 0 until nWays) {
      s2_data(i)(w) := data.io.resp(i)(w)
    }
  }

  val s2_data_muxed = widthMap(w => Mux1H(s2_tag_match_way(w), s2_data(w)))
  val s2_word_idx   = widthMap(w => if (rowWords == 1) 0.U else s2_req(w).addr(log2Up(rowWords*wordBytes)-1, log2Up(wordBytes)))

  // replacement policy — runtime-selectable via io.lsu.cf_dcache_repl_conf (owner ruling 149):
  //   0 = PseudoLRU (per-set, (nWays-1) bits) -- design default (BoomCoreParams.dcacheReplPolicy)
  //   1 = TrueLRU (per-set, (nWays*(nWays-1)/2) bits)
  //   2 = PseudoLRU alias of 0 (0x20 files keep working)
  //   3 = random (global 16-bit LFSR, no per-set state)
  // [r149] `cacheParams.replacement` is NOT random: rocket-chip HellaCache.scala:51-52 returns
  // one global PseudoLRU(nWays) tree.  Policy 3 therefore builds its own LFSR here (rocket-chip
  // untouched, so Rocket and BOOM v4 are unaffected).  way = LFSR low bits (0 logic levels).
  val replacer      = new RandomReplacement(nWays)     // true LFSR random (policy 3 only)
  val lru_replacer  = new TrueLRU(nWays)               // helper: get_replace_way / get_next_state
  val plru_replacer = new PseudoLRU(nWays)             // helper: get_replace_way / get_next_state
  // Per-set state memories — present address at s1 → result available at s2
  // [REPLBYP] patch 30 F2 (DF-plru-serial, root cause p4-plru-serial-rootcause.md 2.2 row 4):
  // the four per-set replacement memories (lru/plru_state_mem, ift_repl_sec/atk) are read in s1
  // and written in s2.  As SyncReadMems their read-during-write result depended on the memory
  // lowering: the Verilator flow (CIRCT + the vlsi_mem_gen model) registers the READ ADDRESS, so
  // the s2 value already contained the s2 write of the cycle before (new data); the FireSim flow
  // (Golden Gate SV) registers the READ DATA, so a back-to-back same-set pair read the old state
  // and the younger update overwrote the older one (the older update was lost on the FPGA only).
  // Now: combinational-read Mem + an explicit s1->s2 data register that forwards this cycle's
  // write when it hits the s1 set (write-first, cfReplRead).  This is exactly the Verilator
  // behaviour, now in every lowering.  FPGA: LUTRAM async read -> 1 LUT (forward mux) -> the same
  // s2 data FF as before, so the s2 victim path (state -> get_replace_way -> way_en -> MSHR
  // way_match/secondary_miss -> s2_nack -> cache_resp, census-3 W family) gains 0 levels.  The
  // forward select (write enable + 7-bit address compare) ends at that FF, not on W.
  // Write buses (one write port per memory), driven by the update block below ([REPLUPD]).
  val lru_state_mem  = Mem(nSets, UInt(lru_replacer.nBits.W))
  val plru_state_mem = Mem(nSets, UInt(plru_replacer.nBits.W))
  val cf_lru_we   = Wire(Bool())
  val cf_lru_wa   = Wire(UInt(idxBits.W))
  val cf_lru_wd   = Wire(UInt(lru_replacer.nBits.W))
  val cf_plru_we  = Wire(Bool())
  val cf_plru_wa  = Wire(UInt(idxBits.W))
  val cf_plru_wd  = Wire(UInt(plru_replacer.nBits.W))
  val cf_ift_we   = Wire(Bool())                         // ift_repl_sec/atk (IFT builds only)
  val cf_ift_wa   = Wire(UInt(idxBits.W))
  val cf_rsec_wd  = Wire(UInt((1 + inflPcBitsCF).W))
  val cf_ratk_wd  = Wire(UInt((1 + inflPcBitsCF).W))
  // s2 value of `mem` at s1 address `ra`: the stored value, or this cycle's write when it is to
  // `ra` (read enable `en` holds the register, as the SyncReadMem read enable held its output).
  private def cfReplRead(mem: Mem[UInt], we: Bool, wa: UInt, wd: UInt, ra: UInt, en: Bool): UInt =
    RegEnable(Mux(we && wa === ra, wd, mem.read(ra)), en)
  // [REPLRST] DF-lru0 (owner r141): the per-set replacement-state memories had no reset, so in
  // simulation (RANDOMIZE_MEM_INIT) and on any un-initialised RAM a TrueLRU matrix starts as a
  // random tournament with no sink; the empty candidate vector then selects way 0 and the set
  // stays at way 0 (task-reports/p4-lru-way0-rootcause.md).  A reset sweep like
  // L1MetadataArray's rst_cnt writes 0 to every set after reset: a consistent LRU order, a valid
  // PLRU state and "no taint / no record" for the IFT memories.  Functional memories under
  // ENABLE_RECONF, the IFT memories (ift_repl_sec/atk, ift_evf) under ENABLE_IFT; with neither
  // the counter does not exist.  No read gating: the meta array's own sweep holds io.write.ready
  // (and the LSU request path) off for the same nSets cycles after reset.
  // [r147] the design-default policy (DCACHE_REPL_DEFAULT, PLRU) is now per-set state in EVERY
  // config, so a reconf-off build also sweeps the memory of its fixed policy: lru_state_mem when
  // the default is TrueLRU, plru_state_mem when it is PseudoLRU (cf_repl_sweep_lru/_plru).
  val cf_repl_sweep_lru  = ENABLE_RECONF || DCACHE_REPL_DEFAULT == 1
  val cf_repl_sweep_plru = ENABLE_RECONF || DCACHE_REPL_DEFAULT == 0 || DCACHE_REPL_DEFAULT == 2
  val cf_repl_rst_cnt = if (cf_repl_sweep_lru || cf_repl_sweep_plru || ENABLE_IFT) Some(RegInit(0.U(log2Up(nSets + 1).W))) else None
  val cf_repl_rst     = cf_repl_rst_cnt.map(_ < nSets.U).getOrElse(false.B)
  val cf_repl_rst_idx = cf_repl_rst_cnt.map(_(log2Ceil(nSets) - 1, 0)).getOrElse(0.U(log2Ceil(nSets).W))
  cf_repl_rst_cnt.foreach(c => when (cf_repl_rst) { c := c + 1.U })

  // [reconf-fix doc 35] Per-lane replacement-state path.  memWidth=2, so each memory
  // lane reads its OWN set's state (2 read ports on each SyncReadMem), computes its OWN
  // victim, and updates through a single lane-arbitrated write port (below).  Previously
  // the whole path was hardwired to lane 0: lane-1 hits never refreshed recency and a
  // lane-1-only miss wrote its allocation into lane 0's set state (cross-set pollution).
  // This is a fidelity/determinism fix, NOT a correctness fix (any victim is
  // architecturally safe — dirty evictions write back, lookups search every way).
  // Read LRU/PLRU state for each lane's s1 set address; result arrives at s2.
  val s1_set_for_repl = widthMap(w => dcacheMaskIdx(s1_req(w).addr))
  val s2_lru_state    = widthMap(w => cfReplRead(lru_state_mem,  cf_lru_we,  cf_lru_wa,  cf_lru_wd,  s1_set_for_repl(w), s1_valid(w)))
  val s2_plru_state   = widthMap(w => cfReplRead(plru_state_mem, cf_plru_we, cf_plru_wa, cf_plru_wd, s1_set_for_repl(w), s1_valid(w)))
  // [RESID] step 15 (residue audit row 4): per-set replacement-state taint {v, PC[13:2]}.
  // Set when a secret-ADDRESS access updates the set's LRU/PLRU state (hit or allocation);
  // [EVFLAST] step 19: any other update of the set's state clears it (latest event wins).  An
  // allocation that evicts a valid line while it is set is recorded in ift_evf as secret.
  // Read/written exactly like lru_state_mem (s1 read, s2 write, same arbitrated port, [REPLBYP]).
  // [IFTGATE] step 17 (audit P21): every step-15/17 metadata memory exists only under
  // ENABLE_IFT; without it the s2 values are literal zeros, so a non-IFT config elaborates none
  // of them (its Verilog does not depend on DCE of an unwritten SyncReadMem).
  val ift_repl_sec  = if (ENABLE_IFT) Some(Mem(nSets, UInt((1 + inflPcBitsCF).W))) else None
  val s2_repl_sec   = widthMap(w => ift_repl_sec.map(m => cfReplRead(m, cf_ift_we, cf_ift_wa, cf_rsec_wd, s1_set_for_repl(w), s1_valid(w)))
                                                .getOrElse(0.U((1 + inflPcBitsCF).W)))
  // [RESID] (row 4b) the global random-replacement LFSR advances on every miss: a secret-address
  // miss shifts later random victim choices.  One register; [EVFLAST] step 19: every miss
  // replaces it (latest LFSR advance wins), see the update below.
  val cf_lfsr_sec_v  = RegInit(false.B)
  val cf_lfsr_sec_pc = Reg(UInt(inflPcBitsCF.W))
  // [ATKEVICT] step 17 (owner ruling 116 C): the attacker twins of the two replacement-state
  // taints.  ift_repl_atk {v, PC[13:2]}: the set's LRU/PLRU state was last advanced by an
  // attacker-domain access; cf_lfsr_atk: an attacker-domain miss advanced the global LFSR.
  // Same read/write port and the same latest-event-wins update as ift_repl_sec / cf_lfsr_sec.
  val ift_repl_atk   = if (ENABLE_IFT) Some(Mem(nSets, UInt((1 + inflPcBitsCF).W))) else None   // [IFTGATE]
  val s2_repl_atk    = widthMap(w => ift_repl_atk.map(m => cfReplRead(m, cf_ift_we, cf_ift_wa, cf_ratk_wd, s1_set_for_repl(w), s1_valid(w)))
                                                 .getOrElse(0.U((1 + inflPcBitsCF).W)))
  val cf_lfsr_atk_v  = RegInit(false.B)
  val cf_lfsr_atk_pc = Reg(UInt(inflPcBitsCF.W))
  // [RESID] (row 3) per-set eviction filter {v, victim tag, PC[13:2] of the secret cause}:
  // "the last secret-chosen eviction in this set removed line `tag`".  Read in s1, compared in
  // s2 against a missing request's tag; a match gives that miss's response a CACHE_EVICTION
  // sec=1 edge (via the MSHR) and clears the entry.  The s1-read/s2-write staleness of a
  // back-to-back same-set pair can only lose an edge, never invent one.
  // [ATKEVICT] step 17 (ruling 116 C) widens the entry by two flags (the old `v` is now `s`):
  //   {s, a, d, victim tag, PC[13:2]}   s = the eviction was secret-chosen (step 15)
  //                                     a = the eviction was attacker-caused: an attacker-domain
  //                                         allocation, or attacker-advanced LRU/PLRU/LFSR state
  //                                     d = a, and the ALLOCATING access itself was attacker-domain
  //   PC = the attacker access when a=1, else the secret cause.  The entry is live when s || a.
  // A miss on the victim tag: s -> CACHE_EVICTION {atk=0, sec=1} for any reader domain (step 15);
  // a and a domain-0 reader -> CACHE_EVICTION {atk=1, sec=s} (attacker PC) and, when d, the
  // loaded value's attacker taint (cf_mem_dataflow_atk, set at the MSHR).  1+1+1+cfTagBits+12 b.
  // [RUWEVF] patch 32 (DF-ruw-parity): read and written like the replacement memories
  // ([REPLBYP]): combinational-read Mem + cfReplRead (write-first forward of this cycle's write
  // into the s1->s2 data register).  As a SyncReadMem the FireSim SV returned the OLD entry to a
  // same-set read issued in the write cycle while the Verilator model returned the NEW one.  The
  // reachable case: primary A (tag T) consumes a record naming T and rewrites the entry with its
  // own eviction {victim V}; its secondary B (same line, one cycle behind) read the pre-A entry
  // on the FPGA only, matched T again and cleared A's new record (FPGA-only lost CACHE_EVICTION
  // edge for V, under-taint).  Now both lowerings see A's write (today's Verilator behaviour).
  // FPGA: LUTRAM async read -> forward mux (1 LUT) -> the existing s2 data FF; 0 levels after
  // the FF.  The forward select (write enable + 7-bit compare) ends at that FF, not on W.
  val iftEvfBits    = 3 + cfTagBits + inflPcBitsCF
  val ift_evf       = if (ENABLE_IFT) Some(Mem(nSets, UInt(iftEvfBits.W))) else None   // [IFTGATE]
  val cf_evf_we     = Wire(Bool())                       // [RUWEVF] the single write bus
  val cf_evf_wa     = Wire(UInt(idxBits.W))
  val cf_evf_wd     = Wire(UInt(iftEvfBits.W))
  val s2_evf        = widthMap(w => ift_evf.map(m => cfReplRead(m, cf_evf_we, cf_evf_wa, cf_evf_wd, s1_set_for_repl(w), s1_valid(w)))
                                           .getOrElse(0.U(iftEvfBits.W)))
  val s2_evf_s      = widthMap(w => s2_evf(w)(iftEvfBits - 1))
  val s2_evf_a      = widthMap(w => s2_evf(w)(iftEvfBits - 2))
  val s2_evf_d      = widthMap(w => s2_evf(w)(iftEvfBits - 3))
  val s2_evf_tagm   = widthMap(w => s2_evf(w)(cfTagBits + inflPcBitsCF - 1, inflPcBitsCF) === (s2_req(w).addr >> cfTagLSB))
  val s2_evf_atk    = widthMap(w => s2_evf_a(w) && s2_evf_tagm(w) && s2_req(w).uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U)
  val s2_evf_hit    = widthMap(w => (s2_evf_s(w) && s2_evf_tagm(w)) || s2_evf_atk(w))   // an edge fires
  val s2_evf_live   = widthMap(w => (s2_evf_s(w) || s2_evf_a(w)) && s2_evf_tagm(w))     // consume
  val s2_evf_pc     = widthMap(w => s2_evf(w)(inflPcBitsCF - 1, 0))

  // Compute victim way (one-hot) at s2 for each policy, inside the active ways, per lane
  // [reconf-fix 2.10f F2a] Victim chosen INSIDE the active ways, not clamped afterwards.
  // The old `& (active_ways - 1)` clamp pinned one fixed way per row whenever fewer than
  // 8 ways were active (bug report task-2.10f section 2.1).
  //   PLRU:    ways 0..k-1 are rocket's low subtree state(k-2,0) (Replacement.scala:191-205, :238-251);
  //            get_next_state already updates it as an exact k-way tree.
  //   TrueLRU: way i < k is the victim iff every other ACTIVE way is more recent.
  // k = nWays reproduces the previous victim exactly. The LRU result goes through a binary
  // index, so a garbage (Verilator-random) matrix still yields one in-range way.
  // cfPadW zero-extends to wayIdxW bits. At k = 2 both helpers return a Bool, and Chisel
  // cannot .pad a Bool, so the extension is an OR with a wayIdxW-bit zero.
  // Baseline (no reconfiguration) keeps the old expressions verbatim (else branches).
  require(cacheWayOptions.forall(k => isPow2(k)), "F2a assumes power-of-two way options")
  private val wayIdxW = log2Ceil(nWays)
  private def cfPadW(x: UInt): UInt = 0.U(wayIdxW.W) | x
  private def cfPlruWay(state: UInt): UInt =
    MuxLookup(cfg_way_applied, 0.U(wayIdxW.W))(cacheWayOptions.zipWithIndex.map { case (k, i) =>
      i.U -> (if (k == 1) 0.U(wayIdxW.W)
              else cfPadW(plru_replacer.get_replace_way(state(k-2, 0), k))) })
  // Rocket's packing (Replacement.scala:88-96): row i holds "way j more recent than way i", j > i.
  private def cfLruMoreRecent(state: UInt, i: Int, j: Int): Bool =
    state((0 until i).map(r => nWays - r - 1).sum + (j - i - 1))
  private def cfLruWay(state: UInt): UInt =
    MuxLookup(cfg_way_applied, 0.U(wayIdxW.W))(cacheWayOptions.zipWithIndex.map { case (k, i) =>
      i.U -> (if (k == 1) 0.U(wayIdxW.W)
              else cfPadW(OHToUInt(VecInit((0 until k).map { v =>
                     (((v + 1) until k).map(j => cfLruMoreRecent(state, v, j)) ++
                      (0 until v).map(l => !cfLruMoreRecent(state, l, v))).reduce(_ && _)
                   }).asUInt))) })

  val s2_random_way_en = widthMap(w => UIntToOH(replacer.way & (cf_dcache_active_ways - 1.U)))
  val s2_lru_way_en    = if (ENABLE_RECONF) widthMap(w => UIntToOH(cfLruWay(s2_lru_state(w)), nWays))
                         else widthMap(w => UIntToOH(lru_replacer.get_replace_way(s2_lru_state(w))   & (cf_dcache_active_ways - 1.U)))
  val s2_plru_way_en   = if (ENABLE_RECONF) widthMap(w => UIntToOH(cfPlruWay(s2_plru_state(w)), nWays))
                         else widthMap(w => UIntToOH(plru_replacer.get_replace_way(s2_plru_state(w)) & (cf_dcache_active_ways - 1.U)))
  // [r149] default (codes 0 and 2) = PseudoLRU; 1 = TrueLRU; 3 = LFSR random.
  val s2_repl_way_raw  = widthMap(w => MuxLookup(io.lsu.cf_dcache_repl_conf, s2_plru_way_en(w))(Seq(
    1.U -> s2_lru_way_en(w),
    3.U -> s2_random_way_en(w)
  )))
  // [NOPENDOVR] patch 32 (side item (a) of patch 30): the corefuzzing "MSHR way-collision fix"
  // (42760b05: if the victim way was pending in ANY MSHR, take the first free active way, and
  // nack the miss when every active way was pending) is removed; the victim is the policy's
  // raw way.  It guarded a collision that cannot happen: a primary allocation requires
  // !idx_match (mshrs.scala pri_val), and idx_match compares the MASKED row (reconf-fix
  // 2026-08-12), so at most one MSHR owns a physical row and two MSHRs never share (row, way).
  // pending_way_mask was the union over ALL rows, so it moved fills whenever another row's MSHR
  // (e.g. the next-line prefetch) held the same way, biased fills to the lowest free way, and
  // at a 1-way geometry it nacked every miss while any MSHR was busy.  Stock BOOM v3 has no
  // such override (boom-chipyard dcache.scala:718).  Timing: removes the override mux from the
  // census-3 W family (victim -> way_en -> MSHR way_match -> secondary_miss -> s2_nack_victim)
  // and s2_way_avail from s2_nack_miss / the MSHR valid (-1..2 levels, 0 added anywhere).
  // One bit per active way: bit w set iff w < cf_dcache_active_ways (one-hot bitmask,
  // NOT the numeric (ways-1) index mask — e.g. 2 active ways need 0b11, not 0b01).
  val s2_active_mask   = VecInit((0 until nWays).map(w => w.U < cf_dcache_active_ways)).asUInt
  if (ENABLE_RECONF) {
    for (w <- 0 until memWidth) {
      val v = s2_repl_way_raw(w)
      assert(!s2_valid(w) || (PopCount(v(nWays-1, 0)) === 1.U && (v >> nWays) === 0.U &&
                              (v(nWays-1, 0) & ~s2_active_mask) === 0.U),
             "D$ victim is not exactly one active way")
    }
  }
  val s2_replaced_way_en = s2_repl_way_raw
  val s2_repl_meta = widthMap(i => Mux1H(s2_replaced_way_en(i), wayMap((w: Int) => RegNext(meta(i).io.resp(w))).toSeq))
  // [reconf-fix] Full set index of the evicted block, reconstructed from its own tag
  // (Theorem 2, spec 31): F_victim = Cat(tag[2:0], row[3:0]).  row[3:0] == addr[9:6]
  // at every geometry because the smallest mask is 0xF, so this is exact.  The WB unit
  // needs the FULL 7-bit index for its data-array read; the Release address slices it.
  val s2_repl_full_idx = widthMap(i =>
    Cat(s2_repl_meta(i).tag(idxBits-cfIdxLowBits-1, 0),
        dcacheMaskIdx(s2_req(i).addr)(cfIdxLowBits-1, 0)))

  // nack because of incoming probe
  val s2_nack_hit    = RegNext(VecInit(s1_nack))
  // Nack when we hit something currently being evicted
  val s2_nack_victim = widthMap(w => s2_valid(w) &&  s2_hit(w) && mshrs.io.secondary_miss(w))
  // MSHRs not ready for request
  // Nack if no MSHR is ready OR if no active cache way is available (all active ways
  // are pending in other MSHRs).  The second condition gates the MSHR valid below, so
  // without also nacking here the LSU would wait for a response that was never issued.
  val s2_nack_miss   = widthMap(w => s2_valid(w) && !s2_hit(w) && !mshrs.io.req(w).ready)   // [NOPENDOVR]
  // Bank conflict on data arrays
  val s2_nack_data   = widthMap(w => data.io.nacks(w))
  // Can't allocate MSHR for same set currently being written back
  val s2_nack_wb     = widthMap(w => s2_valid(w) && !s2_hit(w) && s2_wb_idx_matches(w))

  s2_nack           := widthMap(w => (s2_nack_miss(w) || s2_nack_hit(w) || s2_nack_victim(w) || s2_nack_data(w) || s2_nack_wb(w)) && s2_type =/= t_replay)
  val s2_send_resp = widthMap(w => (RegNext(s1_send_resp_or_nack(w)) && !s2_nack(w) &&
                      (s2_hit(w) || (mshrs.io.req(w).fire && isWrite(s2_req(w).uop.mem_cmd) && !isRead(s2_req(w).uop.mem_cmd)))))
  val s2_send_nack = widthMap(w => (RegNext(s1_send_resp_or_nack(w)) && s2_nack(w)))
  for (w <- 0 until memWidth)
    assert(!(s2_send_resp(w) && s2_send_nack(w)))
  // [RESPSEL] patch 25 row 2d (timing, census3 W family segments 2-3; IFT builds only).
  // s2_nack_hp = the part of s2_nack that can be set on a HIT.  s2_nack_miss and s2_nack_wb both
  // AND in !s2_hit, so  s2_hit(w) => s2_nack(w) === s2_nack_hp(w)  (exact), and s2_nack_hp has no
  // MSHR-ready / kill term (block_hit and secondary_miss are address compares, not valids).
  // s2_resp_hit = the hit half of cache_resp.valid:
  //   cache_resp.valid && s2_hit = s2_valid && R && !s2_nack && s2_hit = s2_valid && R && s2_hit && !s2_nack_hp
  // (R = RegNext(s1_send_resp_or_nack)).  The other half, !s2_hit && cache_resp.valid, is the
  // store-only-miss ack (s2_send_resp's mshrs.io.req.fire term), the only one needing MSHR ready.
  val s2_nack_hp  = if (ENABLE_IFT) Some(widthMap(w => (s2_nack_hit(w) || s2_nack_victim(w) || s2_nack_data(w)) &&
                                                       s2_type =/= t_replay)) else None
  val s2_resp_hit = s2_nack_hp.map(nhp => widthMap(w => s2_valid(w) && RegNext(s1_send_resp_or_nack(w)) &&
                                                        s2_hit(w) && !nhp(w)))

  // hits always send a response
  // If MSHR is not available, LSU has to replay this request later
  // If MSHR is available and this is only a store(not a amo), we don't need to wait for resp later
  s2_store_failed := s2_valid(0) && s2_nack(0) && s2_send_nack(0) && s2_req(0).uop.uses_stq

  // Miss handling
  for (w <- 0 until memWidth) {
    // [2.10f round 2, timing (G7 finding 8)] s2_nack_victim(w) = s2_valid && s2_hit && ..., so
    // with !s2_hit(w) in the same AND it is redundant. The reconfig build drops it, so the MSHR
    // file's secondary_miss cone (which sees the victim through way_en) does not reach this
    // valid, the MSHR file's lane select (req_idx) and so the other lane's ready and nack.
    mshrs.io.req(w).valid := s2_valid(w)          &&
                            !s2_hit(w)            &&
                            !s2_nack_hit(w)       &&
                            (if (ENABLE_RECONF) true.B else !s2_nack_victim(w)) &&
                            !s2_nack_data(w)      &&
                            !s2_nack_wb(w)        &&
                             s2_type.isOneOf(t_lsu, t_prefetch)             &&
                            !IsKilledByBranch(io.lsu.brupdate, s2_req(w).uop) &&
                            !(io.lsu.exception && s2_req(w).uop.uses_ldq)   &&
                             (isPrefetch(s2_req(w).uop.mem_cmd) ||
                              isRead(s2_req(w).uop.mem_cmd)     ||
                              isWrite(s2_req(w).uop.mem_cmd))
    assert(!(mshrs.io.req(w).valid && s2_type === t_replay), "Replays should not need to go back into MSHRs")
    mshrs.io.req(w).bits             := DontCare
    mshrs.io.req(w).bits.uop         := s2_req(w).uop
    mshrs.io.req(w).bits.uop.br_mask := GetNewBrMask(io.lsu.brupdate, s2_req(w).uop)
    mshrs.io.req(w).bits.addr        := s2_req(w).addr
    mshrs.io.req(w).bits.tag_match   := s2_tag_match(w)
    mshrs.io.req(w).bits.old_meta    := Mux(s2_tag_match(w), BoomL1Metadata(s2_repl_meta(w).tag, s2_hit_state(w)), s2_repl_meta(w))  // [reconf-fix] extended-tag metadata
    // old_idx: full (unmasked) set index of the evicted block, used by MSHR for the correct
    // TL Release address.  At full config full_idx == masked_idx so this is a no-op.
    mshrs.io.req(w).bits.old_idx     := s2_repl_full_idx(w)
    mshrs.io.req(w).bits.way_en      := Mux(s2_tag_match(w), s2_tag_match_way(w), s2_replaced_way_en(w))

    mshrs.io.req(w).bits.data        := s2_req(w).data
    mshrs.io.req(w).bits.is_hella    := s2_req(w).is_hella
    mshrs.io.req_is_probe(w)         := s2_type === t_probe && s2_valid(w)
    // [RESID] step 15: secret-address bit and the eviction-filter result ride into the MSHR.
    mshrs.io.req(w).bits.cf_addr_sec.foreach(_ := s2_req(w).cf_addr_sec.getOrElse(false.B))
    mshrs.io.req(w).bits.cf_addr_atk.foreach(_ := s2_req(w).cf_addr_atk.getOrElse(false.B))   // [ATKRES] per-MSHR line mark
    // [VSECEV] step 17 (owner ruling 119 (2)): a miss on a tag a SECRET-chosen eviction removed
    // returns a secret VALUE (any reader domain, like its edge).  Rides on the primary's uop flag;
    // the MSHR applies it to every response it drains (mshrs.scala cf_rp_vsec).
    mshrs.io.req(w).bits.uop.cf_mem_sec_dataflow.foreach(_ := s2_req(w).uop.cf_mem_sec_dataflow.getOrElse(false.B) ||
                                                    (s2_evf_s(w) && s2_evf_tagm(w)))
    // [NACKRES] step 16 (ruling 111): an LSU load re-executed after a nack taken while a
    // secret-address miss held an MSHR arrives with cf_res_v/pc set (ldq cf_nack_res); if it
    // misses again it rides the same MSHR edge (the eviction filter's PC wins).  A secondary
    // that merges drops it: it already gets the asec residency edge of that MSHR (R1/R3).
    // [NACKATK] patch 44 (r111 extended, r173 (3)): the LSU's nack edge is now {v, atk, esec}
    // (lsu.scala cf_nack_res_atk): {1,0,1} secret, {1,1,0} attacker only, {1,1,1} both.  An
    // attacker-only nack edge is handed to the MSHR as {cf_res_v=0, cf_res_atk=1} (mshrs.scala
    // cf_rp_nk), so the MSHR's own secret residency edge keeps its PC and sec for the secondaries.
    // An eviction-filter hit still wins the PC; an attacker nack adds atk=1 to a secret-only filter
    // edge (sec stays 1).  With the LSU's atk = 0 (no attacker nack) every value is as before.
    val cf_nk_atk  = s2_req(w).cf_res_atk.getOrElse(false.B)
    val cf_nk_only = cf_nk_atk && !s2_req(w).cf_res_esec.getOrElse(false.B)
    mshrs.io.req(w).bits.cf_res_v.foreach(_ := s2_evf_hit(w) || (s2_req(w).cf_res_v.getOrElse(false.B) && !cf_nk_only))
    mshrs.io.req(w).bits.cf_res_pc.foreach(_ := Mux(s2_evf_hit(w), s2_evf_pc(w), s2_req(w).cf_res_pc.getOrElse(0.U)))
    // [ATKEVICT] step 17: the edge is attacker-caused (atk=1, sec = also secret-chosen) when the
    // filter entry is attacker and the reader is domain 0; otherwise {atk=0, sec=1} as before.
    mshrs.io.req(w).bits.cf_res_atk.foreach(_ := s2_evf_atk(w) || cf_nk_atk)
    mshrs.io.req(w).bits.cf_res_esec.foreach(_ := Mux(s2_evf_hit(w), !s2_evf_atk(w) || s2_evf_s(w),
                                                      s2_req(w).cf_res_esec.getOrElse(false.B)))
    // value taint (owner extension of C): the loaded value carries attacker taint when the
    // eviction's allocating access was attacker-domain (d).  Rides on the primary's uop flag;
    // the MSHR applies it to every response it drains (mshrs.scala cf_rp_vatk).
    mshrs.io.req(w).bits.uop.cf_mem_dataflow_atk.foreach(_ := s2_req(w).uop.cf_mem_dataflow_atk.getOrElse(false.B) ||
                                                    (s2_evf_atk(w) && s2_evf_d(w)))
  }

  // [RESID] step 15 (residue audit rows 3/4/4b): record secret-chosen evictions.
  // A PRIMARY allocation (fire && !idx_match: a secondary merges and evicts nothing) that
  // replaces a VALID line is secret-chosen when its own address is secret-derived, or when the
  // replacement state that picked the victim was last advanced by a secret-address access
  // (per-set LRU/PLRU taint, or the global LFSR taint under random replacement).
  val cf_repl_rand  = io.lsu.cf_dcache_repl_conf === 3.U   // [r149] LFSR random = code 3
  // [PRIALLOC] patch 32: cf_req_alloc = fire && cacheable && !idx_match (mshrs.scala), so an
  // uncacheable (MMIO) request, which allocates nothing, no longer records a phantom eviction
  // that, by [EVFLAST], also wiped the set's pending record (under-taint).
  val cf_pri_alloc  = widthMap(w => mshrs.io.cf_req_alloc(w))
  val cf_evicts     = widthMap(w => cf_pri_alloc(w) && !s2_tag_match(w) && s2_repl_meta(w).coh.isValid())
  val cf_rs_v       = widthMap(w => Mux(cf_repl_rand, cf_lfsr_sec_v,  s2_repl_sec(w)(inflPcBitsCF)))
  val cf_rs_pc      = widthMap(w => Mux(cf_repl_rand, cf_lfsr_sec_pc, s2_repl_sec(w)(inflPcBitsCF - 1, 0)))
  val cf_req_pc12   = widthMap(w => s2_req(w).uop.debug_pc(inflPcBitsCF + 1, 2))
  val cf_evict_sec  = widthMap(w => cf_evicts(w) && (s2_req(w).cf_addr_sec.getOrElse(false.B) || cf_rs_v(w)))
  val cf_evict_pc   = widthMap(w => Mux(s2_req(w).cf_addr_sec.getOrElse(false.B), cf_req_pc12(w), cf_rs_pc(w)))
  // [ATKEVICT] step 17 (ruling 116 C): attacker-caused evictions, the same rule with the domain
  // in place of the address taint.  d = the allocating access is attacker-domain.
  val cf_ra_v       = widthMap(w => Mux(cf_repl_rand, cf_lfsr_atk_v,  s2_repl_atk(w)(inflPcBitsCF)))
  val cf_ra_pc      = widthMap(w => Mux(cf_repl_rand, cf_lfsr_atk_pc, s2_repl_atk(w)(inflPcBitsCF - 1, 0)))
  val cf_req_atk    = widthMap(w => s2_req(w).uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= 0.U)
  // [MEMORDATK] patch 42 option E (r116 destination rule; the secret rule's own term): an access at an
  // ATTACKER-tainted address ([ATKRES] cf_addr_atk) chooses its eviction and advances the replacement
  // state like an attacker-domain access.  Record + edge only: the value-taint bit d stays domain-only
  // (owner extension of 116 C).  One OR on s2 flops, 0 levels.
  val cf_req_atki   = widthMap(w => cf_req_atk(w) || s2_req(w).cf_addr_atk.getOrElse(false.B))
  val cf_evict_d    = widthMap(w => cf_evicts(w) && cf_req_atk(w))
  val cf_evict_atk  = widthMap(w => cf_evicts(w) && (cf_req_atki(w) || cf_ra_v(w)))
  val cf_evict_apc  = widthMap(w => Mux(cf_req_atki(w), cf_req_pc12(w), cf_ra_pc(w)))
  // consumption: the miss whose tag was the recorded victim (it now gets the edge)
  val cf_evf_take   = widthMap(w => mshrs.io.req(w).fire && s2_evf_live(w))
  // [EVFLAST] step 19 (owner ruling, generic, replaces step 17's "an attacker-only eviction does
  // not overwrite a pending secret entry"): EVERY eviction of a valid line replaces the set's
  // record with its own attribution -- latest event wins, no first-wins, no waiting for a reader.
  // A secret-chosen or attacker-caused eviction writes {s, a, d, tag, PC}; any other eviction
  // writes 0 (the previous record is lost).  A consuming miss (take) clears it as before.
  val cf_evf_new    = widthMap(w => cf_evict_sec(w) || cf_evict_atk(w))
  // [PFPHANTOM] patch 32 (owner r163): a next-line prefetch fires as a primary allocation, but
  // with an empty rpq the MSHR parks the line (s_mem_finish -> s_prefetch) without evicting the
  // victim, and a cleared prefetch never evicts.  Its "eviction" therefore never replaces a LIVE
  // record: a clean one used to write 0, and a secret/attacker one (e.g. chosen by a secret-
  // tainted replacement state) used to overwrite the record with a still-resident victim; both
  // lost the set's real pending eviction (under-taint).  A prefetch fire now writes only when
  // the set has no live record and its eviction is secret/attacker-caused (a phantom record
  // names a resident line: it can only matter after that line is really evicted, which rewrites
  // the record).  Residual approximation (documented): a PROMOTED prefetch's eviction is not
  // recorded while an older record is live, and a promoted clean one does not clear it
  // (over-taint); the exact form records at promotion (MSHR s_prefetch -> s_meta_read).
  // 1-2 LUT on the evf write enable (pf and the forwarded s2_evf live bits are early).
  val cf_evf_pf     = widthMap(w => isPrefetch(s2_req(w).uop.mem_cmd))
  val cf_evf_pend   = widthMap(w => s2_evf_s(w) || s2_evf_a(w))
  val cf_evf_wr     = widthMap(w => (cf_evicts(w) && (!cf_evf_pf(w) || (cf_evf_new(w) && !cf_evf_pend(w)))) || cf_evf_take(w))
  if (ENABLE_IFT) {
    // one write port: lane 0 first; within a lane a new eviction beats the clear
    // [REPLRST] DF-lru0 (r141): the reset sweep shares the port (addr = counter, data = 0).
    // [RUWEVF] the same write, as an explicit bus so cfReplRead can forward it.
    val ew = PriorityEncoder(cf_evf_wr)
    cf_evf_we := cf_repl_rst || cf_evf_wr.reduce(_||_)
    cf_evf_wa := Mux(cf_repl_rst, cf_repl_rst_idx, dcacheMaskIdx(s2_req(ew).addr))
    // [EVFPCSEC] p4-r3 (t53 m55 / t64 m80): ONE PC per record, chosen SECRET-FIRST (as the BTB
    // records, r140/r146).  Atk-first named the attacker's own access as the PC of a sec=1 edge
    // whenever the set's replacement state was also attacker-advanced (any attacker prime), so
    // the flow graph got the attacker load as the "secret source".  Same two early inputs, the
    // select moves from cf_evict_atk to cf_evict_sec: 0 levels, ends at the ift_evf LUTRAM DI.
    cf_evf_wd := Mux(!cf_repl_rst && cf_evf_new(ew), Cat(cf_evict_sec(ew), cf_evict_atk(ew), cf_evict_d(ew), s2_repl_meta(ew).tag,
                                Mux(cf_evict_sec(ew), cf_evict_pc(ew), cf_evict_apc(ew))),
                     0.U(iftEvfBits.W))
    when (cf_evf_we) { ift_evf.get.write(cf_evf_wa, cf_evf_wd) }
    // [PFPHANTOM] deciding print (t85_pf_phantom): one line per ift_evf write or prefetch eviction,
    // debug builds only, run-time gated by debug CSR 0xbc0 bit 7 (with bit 0), like [REPLUPD].
    // pf/ev/pend/sup are lane bitmasks: prefetch fire, cf_evicts, live record, suppressed write.
    if (ENABLE_CF_DEBUG_PRINTF) {
      val cf_ev_cyc = RegInit(0.U(32.W))
      cf_ev_cyc := cf_ev_cyc + 1.U
      val cf_ev_pfe = VecInit((0 until memWidth).map(w => cf_evicts(w) && cf_evf_pf(w))).asUInt
      val cf_ev_sup = VecInit((0 until memWidth).map(w => cf_evicts(w) && cf_evf_pf(w) && !cf_evf_wr(w))).asUInt
      when ((cf_evf_we && !cf_repl_rst || cf_ev_pfe.orR) && io.lsu.cf_debug_repl_enable) {
        printf("[EVFWR] c=%d we=%d set=%d wd=%x pf=%b ev=%b pend=%b sup=%b set0=%d set1=%d\n",
          cf_ev_cyc, cf_evf_we, cf_evf_wa, cf_evf_wd, cf_ev_pfe, cf_evicts.asUInt,
          cf_evf_pend.asUInt, cf_ev_sup,
          dcacheMaskIdx(s2_req(0).addr), dcacheMaskIdx(s2_req(memWidth - 1).addr))
      }
    }
    // [EVFLAST] step 19: the LFSR taints name the LATEST advance of the LFSR.  Every MSHR fire
    // advances it (replacer.miss below), so every fire replaces the taint with its own
    // attribution: a secret-address / attacker-domain miss sets it (with its PC), a clean miss
    // clears it.  This subsumes the old "consumed by the next eviction" clear, because the
    // evicting allocation is itself an MSHR fire.  Two fires in one cycle: either sets it.
    val cf_lfsr_fire = mshrs.io.req.map(_.fire).reduce(_||_)
    // [ATKEVICT] LFSR attacker taint
    val cf_lfsr_aset = widthMap(w => mshrs.io.req(w).fire && cf_req_atki(w))   // [MEMORDATK] E
    when (cf_lfsr_fire) {
      cf_lfsr_atk_v  := cf_lfsr_aset.reduce(_||_)
      cf_lfsr_atk_pc := cf_req_pc12(PriorityEncoder(cf_lfsr_aset))
    }
    // LFSR secret taint (random policy)
    val cf_lfsr_set = widthMap(w => mshrs.io.req(w).fire && s2_req(w).cf_addr_sec.getOrElse(false.B))
    when (cf_lfsr_fire) {
      cf_lfsr_sec_v  := cf_lfsr_set.reduce(_||_)
      cf_lfsr_sec_pc := cf_req_pc12(PriorityEncoder(cf_lfsr_set))
    }
  } else {
    cf_evf_we := false.B
    cf_evf_wa := 0.U
    cf_evf_wd := 0.U
  }

  mshrs.io.meta_resp.valid      := !s2_nack_hit(0) || prober.io.mshr_wb_rdy
  mshrs.io.meta_resp.bits       := Mux1H(s2_tag_match_way(0), RegNext(meta(0).io.resp))
  // [reconf-fix doc 35] Per-lane replacement-state update through a SINGLE arbitrated
  // write port.  Each lane contributes an update request: a miss allocates its victim
  // way, a hit refreshes its accessed way (a lane can't be both — mshr.req.fire needs
  // !hit).  [REPLUPD] patch 30 F1: the ALLOCATING lane wins (see uw below); only a concurrent
  // hit's recency refresh can be dropped, which is the heuristic loss doc 35 intended.  dcache_clean()'s
  // sweep is serialized (one miss outstanding) so a clean never drops an update — preserving
  // the sweep's per-set coverage guarantee.  One write port on each state mem (no 2W).
  // replacer.miss advances the global random LFSR on ANY miss (policy-independent).
  when (mshrs.io.req.map(_.fire).reduce(_||_)) { replacer.miss }
  val s2_lsu_hit    = widthMap(w => s2_valid(w) && s2_hit(w) && s2_type === t_lsu && !s2_nack(w))
  // [r149] per-set state (LRU/PLRU) advances for every policy except LFSR random (code 3).
  // [PRIALLOC] patch 32 (side item (b) of patch 30): only a PRIMARY cacheable allocation
  // (mshrs.io.cf_req_alloc) or an LSU hit advances the per-set state.  A secondary merge
  // allocates nothing (the line goes to its primary's way, already made MRU by that primary),
  // and an MMIO request touches no line; both used to touch their raw victim way, protecting
  // a line nobody accessed, and (patch 30 F1) a secondary could take the port from a hit.
  // The global LFSR (replacer.miss) still advances on every fire, as in stock BOOM.
  val cf_alloc      = widthMap(w => mshrs.io.cf_req_alloc(w))
  val s2_repl_upd   = widthMap(w => (cf_alloc(w) || s2_lsu_hit(w)) && io.lsu.cf_dcache_repl_conf =/= 3.U)
  // [r149] PseudoLRU = code 0 or its alias 2 (bit 0 clear; code 3 is excluded by s2_repl_upd).
  val cf_repl_is_plru = !io.lsu.cf_dcache_repl_conf(0)
  // [reconf-fix doc35 ROOT CAUSE] advance state with the POLICY-CHOSEN victim (pre-MSHR-override
  // s2_repl_way_raw), NOT the post-override allocated way — else a PLRU fixpoint
  // (get_next_state(0x7d,way0)==0x7d) freezes the set state and victim collapses to way 0. See
  // reg-based version notes. Identical when no override fires.
  val s2_repl_way   = widthMap(w => Mux(cf_alloc(w),
                                        OHToUInt(s2_repl_way_raw(w)),
                                        OHToUInt(s2_tag_match_way(w)(nWays-1, 0))))
  // [REPLUPD] patch 30 F1 (DF-plru-serial): the write port goes to the lane whose MSHR request
  // FIRED, else to the first updating lane (hits).  Before, lane 0 always won, so a lane-0 hit
  // (e.g. a store commit) in the cycle of a lane-1 allocation dropped the ALLOCATION; serialized
  // same-set misses re-fire only on lane memWidth-1 (lsu can_fire_load_wakeup), so the next miss
  // picked the same victim.  No allocation update can be dropped: the MSHR file accepts at most
  // one request per cycle (mshrs.scala io.req(w).ready requires w === req_idx, one value), so
  // cf_repl_fire is one-hot or zero (asserted), and that lane always gets the port.  The IFT
  // twins (ift_repl_sec/atk) use the same uw, so they record the allocating access, not the hit.
  // Levels: uw = fire(1) || !upd(0) for memWidth 2 (5 inputs: fire0, fire1, hit0, conf[1:0]),
  // one LUT6 as before; fire(1) already drove the data (s2_repl_way) and enable (s2_repl_upd).
  // With no reset counter (no reconf, no IFT, a non-swept default) cf_repl_rst is false.B and the
  // Muxes fold, giving the former plain-update branch.
  val cf_repl_fire = cf_alloc   // [PRIALLOC] the allocating lane (one-hot or zero)
  assert(PopCount(cf_repl_fire) <= 1.U, "D$ replacement port: more than one MSHR request fired in a cycle")
  val upd = s2_repl_upd.reduce(_||_)
  val uw  = Mux(cf_repl_fire.reduce(_||_), PriorityEncoder(cf_repl_fire), PriorityEncoder(s2_repl_upd))
  val set = dcacheMaskIdx(s2_req(uw).addr)
  val way = s2_repl_way(uw)
  val cf_lru_next  = lru_replacer.get_next_state(s2_lru_state(uw), way)
  val cf_plru_next = plru_replacer.get_next_state(s2_plru_state(uw), way)
  val cf_lru_upd   = upd && io.lsu.cf_dcache_repl_conf === 1.U
  val cf_plru_upd  = upd && cf_repl_is_plru
  // [REPLRST] one write port per memory: the reset sweep (addr = counter, data = 0) or the update.
  // [r147] each functional memory is swept when its policy can be in use (reconf, or the
  // reconf-off fixed default); otherwise it is written by the plain update only.  With reconf
  // off cf_dcache_repl_conf is a constant, so the unused policy's write folds away.
  if (cf_repl_sweep_lru) {
    cf_lru_we := cf_repl_rst || cf_lru_upd
    cf_lru_wa := Mux(cf_repl_rst, cf_repl_rst_idx, set)
    cf_lru_wd := Mux(cf_repl_rst, 0.U(lru_replacer.nBits.W), cf_lru_next)
  } else {
    cf_lru_we := cf_lru_upd
    cf_lru_wa := set
    cf_lru_wd := cf_lru_next
  }
  if (cf_repl_sweep_plru) {
    cf_plru_we := cf_repl_rst || cf_plru_upd
    cf_plru_wa := Mux(cf_repl_rst, cf_repl_rst_idx, set)
    cf_plru_wd := Mux(cf_repl_rst, 0.U(plru_replacer.nBits.W), cf_plru_next)
  } else {
    cf_plru_we := cf_plru_upd
    cf_plru_wa := set
    cf_plru_wd := cf_plru_next
  }
  when (cf_lru_we)  { lru_state_mem.write(cf_lru_wa, cf_lru_wd) }
  when (cf_plru_we) { plru_state_mem.write(cf_plru_wa, cf_plru_wd) }
  // [RESID] replacement-state taint follows the same update (LRU/PLRU only: s2_repl_upd
  // requires conf =/= 3).  [EVFLAST] step 19 (owner, latest event wins): every update of the
  // set's replacement state replaces its taint with the updating access's own attribution --
  // a secret-address access sets it (its PC), any other access (hit or miss) clears it.
  if (ENABLE_IFT) {
    cf_ift_we  := cf_repl_rst || upd
    cf_ift_wa  := Mux(cf_repl_rst, cf_repl_rst_idx, set)
    cf_rsec_wd := Mux(!cf_repl_rst && s2_req(uw).cf_addr_sec.getOrElse(false.B), Cat(1.U(1.W), cf_req_pc12(uw)), 0.U((1 + inflPcBitsCF).W))
    // [ATKEVICT] attacker twin: an attacker-domain access sets it, any other access clears it
    cf_ratk_wd := Mux(!cf_repl_rst && cf_req_atki(uw),        Cat(1.U(1.W), cf_req_pc12(uw)), 0.U((1 + inflPcBitsCF).W))   // [MEMORDATK] E
    when (cf_ift_we) {
      ift_repl_sec.get.write(cf_ift_wa, cf_rsec_wd)
      ift_repl_atk.get.write(cf_ift_wa, cf_ratk_wd)
    }
  } else {
    cf_ift_we  := false.B
    cf_ift_wa  := 0.U
    cf_rsec_wd := 0.U
    cf_ratk_wd := 0.U
  }
  // [REPLUPD] patch 30 deciding print (root cause section 6): one line per replacement-state
  // update.  Compile-time ENABLE_CF_DEBUG_PRINTF and run-time debug CSR 0xbc0 bit 7 (with the
  // global bit 0; bit 7 is OFF at init, parameters.scala), so it is silent by default and absent
  // from FireSim/NoLog builds.  fire/hit/upd/byp are lane bitmasks (bit w = lane w); uw = lane given
  // the write port; way = the policy victim (raw) for an allocation, the hit way for a hit;
  // byp(w) = lane w's s2 state came from the [REPLBYP] forward; old/new = the active policy's
  // state of the set (TrueLRU when conf = 1, else PseudoLRU).
  if (ENABLE_CF_DEBUG_PRINTF) {
    val cf_ru_cyc = RegInit(0.U(32.W))
    cf_ru_cyc := cf_ru_cyc + 1.U
    val cf_ru_lru = io.lsu.cf_dcache_repl_conf === 1.U
    val cf_ru_byp = RegNext(VecInit((0 until memWidth).map(w => Mux(cf_ru_lru,
                      cf_lru_we  && cf_lru_wa  === s1_set_for_repl(w),
                      cf_plru_we && cf_plru_wa === s1_set_for_repl(w)))))
    when (upd && io.lsu.cf_debug_repl_enable) {
      printf("[REPLUPD] c=%d conf=%d upd=%b fire=%b hit=%b set0=%d set1=%d uw=%d way=%d byp=%b old=%x new=%x\n",
        cf_ru_cyc, io.lsu.cf_dcache_repl_conf, s2_repl_upd.asUInt, cf_repl_fire.asUInt, s2_lsu_hit.asUInt,
        dcacheMaskIdx(s2_req(0).addr), dcacheMaskIdx(s2_req(memWidth - 1).addr), uw, way, cf_ru_byp.asUInt,
        Mux(cf_ru_lru, s2_lru_state(uw), s2_plru_state(uw)), Mux(cf_ru_lru, cf_lru_next, cf_plru_next))
    }
  }
  tl_out.a <> mshrs.io.mem_acquire

  // probes and releases
  prober.io.req.valid   := tl_out.b.valid && !lrsc_valid
  tl_out.b.ready        := prober.io.req.ready && !lrsc_valid
  prober.io.req.bits    := tl_out.b.bits
  prober.io.way_en      := s2_tag_match_way(0)
  prober.io.block_state := s2_hit_state(0)
  metaWriteArb.io.in(1) <> prober.io.meta_write
  // Mask the prober's meta write idx to the active set range, same as the MSHR meta write.
  // Without this, a probe for full_idx=26 (masked_idx=10) would write coherence state to
  // meta[26] rather than meta[10], leaving the L1 with stale (pre-probe) permissions.
  metaWriteArb.io.in(1).bits.idx := prober.io.meta_write.bits.idx & dcache_set_mask(idxBits-1, 0)
  prober.io.mshr_rdy    := mshrs.io.probe_rdy
  // wb_rdy: compare masked indices so a WB for full_idx=10 and prober at full_idx=26 are
  // correctly seen as conflicting (same masked set slot).
  prober.io.wb_rdy      := ((prober.io.meta_write.bits.idx & dcache_set_mask(idxBits-1,0)) =/= (wb.io.idx.bits & dcache_set_mask(idxBits-1,0))) || !wb.io.idx.valid
  mshrs.io.prober_state := prober.io.state

  // refills
  when (tl_out.d.bits.source === cfg.nMSHRs.U) {
    // This should be ReleaseAck
    tl_out.d.ready := true.B
    mshrs.io.mem_grant.valid := false.B
    mshrs.io.mem_grant.bits  := DontCare
  } .otherwise {
    // This should be GrantData
    mshrs.io.mem_grant <> tl_out.d
  }

  dataWriteArb.io.in(1) <> mshrs.io.refill
  // Override refill write addr to use masked set (TileLink fetch uses full addr, SRAM write uses masked)
  dataWriteArb.io.in(1).bits.addr := dcacheMaskAddr(mshrs.io.refill.bits.addr)
  metaWriteArb.io.in(0) <> mshrs.io.meta_write
  // Override meta write idx to use masked set
  metaWriteArb.io.in(0).bits.idx := mshrs.io.meta_write.bits.idx & dcache_set_mask(idxBits-1, 0)

  // ==== [reconf-fix Phase C] L1 invalidate-all walker (RESTORE ONLY) =========
  // `csrw 0xbc4, 0x2` starts a full-array meta wipe through the normal meta
  // write port (arb port 2 = lowest priority: MSHR/prober writes win per-cycle
  // and the walk just stretches — bounded, since the restore context is
  // quiescent).  DISCARDS dirty data with NO writeback: legal only in the
  // checkpoint-restore flow, where every cached line is doomed and DRAM is
  // about to be re-loaded.  Misuse during normal execution loses committed
  // stores — restore-only discipline (see CSR definition, parameters.scala).
  // The walk deliberately covers ALL nSets physical rows and ALL nWays ways
  // regardless of the active geometry: rows/ways stranded outside the active
  // range are exactly the restore hazard being cleared.  The extended-tag and
  // IFT per-way structures are deliberately NOT cleared — they are consulted
  // only on valid hits/victims, and fills overwrite them.
  // Busy is exported to the core (0xbcd[17]) and holds io.lsu.ordered low
  // (below), so `csrw 0xbc4,0x2; fence` blocks until the walk completes — the
  // contract r30 and the DTM restore sequence rely on.
  // [QSREG R1] Two views of the walk: dcache_wipe_busy (incl. the trigger cycle) for the
  // 0xbcd[17] status bit and the walk-overlap assert, and the registered dcache_wipe_active
  // alone for io.lsu.ordered (proof at the ordered driver below).
  val (dcache_wipe_busy, dcache_wipe_active) = if (ENABLE_RECONF) {
    val wipe_active = RegInit(false.B)
    val wipe_cnt    = RegInit(0.U(log2Ceil(nSets).W))
    val wipe_start  = io.lsu.cf_cachectl_wen && io.lsu.cf_cachectl_wdata(1) && !wipe_active
    when (wipe_start) { wipe_active := true.B; wipe_cnt := 0.U }
    metaWriteArb.io.in(2).valid         := wipe_active
    metaWriteArb.io.in(2).bits          := DontCare
    metaWriteArb.io.in(2).bits.idx      := wipe_cnt
    metaWriteArb.io.in(2).bits.way_en   := ~0.U(nWays.W)
    metaWriteArb.io.in(2).bits.tag      := 0.U
    metaWriteArb.io.in(2).bits.data.tag := 0.U
    metaWriteArb.io.in(2).bits.data.coh := ClientMetadata.onReset
    // [reconf-fix 2.10f] The F3 walk's row invalidates also fire port 2 and step wipe_cnt.
    // Harmless: wipe_cnt is reset by wipe_start, and wipe_active is false during a walk.
    when (metaWriteArb.io.in(2).fire) {
      wipe_cnt := wipe_cnt + 1.U
      when (wipe_cnt === (nSets-1).U) { wipe_active := false.B }
    }
    // busy includes the trigger cycle itself (status bit / assert only; `ordered` uses the
    // registered wipe_active, see [QSREG R1] below).
    (wipe_active || wipe_start, wipe_active)
  } else (false.B, false.B)
  io.lsu.cf_dcache_wipe_busy := dcache_wipe_busy

  tl_out.e <> mshrs.io.mem_finish

  // writebacks
  val wbArb = Module(new Arbiter(new BoomWritebackReq(edge.bundle), 2))
  // 0 goes to prober, 1 goes to MSHR evictions
  wbArb.io.in(0)       <> prober.io.wb_req
  wbArb.io.in(1)       <> mshrs.io.wb_req
  wb.io.req            <> wbArb.io.out
  wb.io.data_resp      := s2_data_muxed(0)
  mshrs.io.wb_resp     := wb.io.resp
  wb.io.mem_grant      := tl_out.d.fire && tl_out.d.bits.source === cfg.nMSHRs.U

  val lsu_release_arb = Module(new Arbiter(new TLBundleC(edge.bundle), 2))
  io.lsu.release <> lsu_release_arb.io.out
  lsu_release_arb.io.in(0) <> wb.io.lsu_release
  lsu_release_arb.io.in(1) <> prober.io.lsu_release

  TLArbiter.lowest(edge, tl_out.c, wb.io.release, prober.io.rep)

  io.lsu.perf.release := edge.done(tl_out.c)
  io.lsu.perf.acquire := edge.done(tl_out.a)

  // load data gen
  val s2_data_word_prebypass = widthMap(w => s2_data_muxed(w) >> Cat(s2_word_idx(w), 0.U(log2Ceil(coreDataBits).W)))
  val s2_data_word = Wire(Vec(memWidth, UInt()))

  val loadgen = (0 until memWidth).map { w =>
    new LoadGen(s2_req(w).uop.mem_size, s2_req(w).uop.mem_signed, s2_req(w).addr,
                s2_data_word(w), s2_sc && (w == 0).B, wordBytes)
  }
  // Mux between cache responses and uncache responses
  val cache_resp   = Wire(Vec(memWidth, Valid(new BoomDCacheResp)))

  // corefuzzing: per-way IFT metadata BRAMs (FPGA-friendly, single writer each).
  //   ift_fill_meta  — written by MSHR fills only; tracks which domain last BROUGHT IN each line (timing channel → INFL_CACHE_EVICTION)
  //   ift_store_meta — written by store hits and MSHR store replays ([G9STMETA]); tracks which domain last STORED to each line  (data channel   → INFL_MEM_DATAFLOW)
  // Entry bit layout: [n+2]=atk  [n+1]=secret  [n]=domain  [n-1:0]=op_count   (n = uopIDCounterWidthCF)
  //
  // [R1e 2026-09-05] `atk` is a DEDICATED bit, exactly parallel to `secret`.
  // Before this, attacker influence was folded into the `domain` field
  // (`uop_dom | cf_attacker_influence`), CONFLATING two different questions:
  //   domain = WHO stored this line (ownership)
  //   atk    = is this line's CONTENT attacker-derived (influence)
  // A victim (domain 0) storing attacker-derived data is influence WITHOUT ownership,
  // and the union made iftDomain lie about who wrote the line.  Now separate.
  //
  // ASSUMED BEHAVIOUR -- documented deliberately; verify before relying on it:
  //  * [STEP1b] atk/secret are PER-DOUBLEWORD masks with LAST-WRITER-WINS per chunk.
  //    No accumulation.  A store clears its own DW if its data is clean; it cannot
  //    clear DWs it never wrote.  A refill rewrites all 8.  Supersedes R1f.
  //  * Sub-DOUBLEWORD precision is still coarse: sb/sh/sw set or clear the whole 8-byte
  //    chunk they land in, so a clean byte store CAN still clear a tainted byte sharing
  //    its doubleword.  ACCEPTED -- byte-granular masks would be 8x the state.
  //  * Fills carry atk = (fill domain === 1), i.e. OWNERSHIP ONLY.  The MSHR has no
  //    content-influence bit, so attacker-influenced DATA arriving via refill is still
  //    untracked (STEP 4 narrows this; the L2/DRAM has no tag so it cannot fully close).
  //  * KNOWN HAZARD: s2_store_entry is read in s1 and written in s2, so two stores to
  //    the same line in consecutive cycles may miss one update (STEP 5).  Can only
  //    LOSE taint, never fabricate it.
  //  * ift_fill_meta stays LINE-level: eviction really is a whole-line property.
  val CF_SUBLINE     = dcacheParams.blockBytes / 8        // 8 doublewords per 64B line
  // [METASLIM 2026-09-08] The two arrays carried the SAME 33-bit entry, but each reads
  // only a subset -- verified by enumerating every accessor call site:
  //   ift_fill_meta  reads domain (:1400), op_count (:1412), secMask (:1412).
  //                  NOTHING reads its atkMask -> 8 dead bits per entry.
  //   ift_store_meta reads atkMask/atkAt, secMask/secretAt, op_count.
  //                  NOTHING reads its domain -> 1 dead bit per entry.
  // `grep iftAtkMask(s2_fill_entry)` and `grep iftDomain(s2_store_entry)` both return
  // nothing.  This is coherent with the design note above: a fill has no per-doubleword
  // attacker story (eviction is a whole-line property), and a store's "which domain"
  // question is already answered by its atkMask.
  //
  // Split into two formats.  nSets=128, nWays=8:
  //   fill  128*25*8 = 25,600 b  (was 33,792)
  //   store 128*32*8 = 32,768 b  (was 33,792)
  //   saved 9,216 b of 67,584 = 13.6%
  // CAVEAT, not papered over: these are SyncReadMem.  If Vivado maps them to BRAM the
  // realised saving may be ZERO (BRAM comes in fixed widths); the 13.6% is real only for
  // distributed/LUT RAM, which 128-deep arrays usually get.  Believe the post-synthesis
  // utilisation report, not this comment.
  // [PCPROV 2026-09-15] the producer field is the filler's / last storer's PC[13:2]
  // (inflPcBitsCF = 12), not its op_count.  A line's age is unbounded, so an op_count
  // wraps and aliases; a PC never goes stale.  The two D$ edge types carry it through
  // the unchanged 18-bit slot as {op_count := PC[11:2], infl_type := base + PC[13:12]}
  // -- see CoreFuzzing.scala inflPcCode.  Meta shrinks 16 -> 12 b per line per array.
  //   fill  128*21*8 = 21,504 b   store 128*28*8 = 28,672 b   (-8,192 b vs op_count)
  val iftFillBits    = CF_SUBLINE + 1 + inflPcBitsCF        // {secMask, domain, pc[13:2]}
  // [WLEDGE]/[WINMEM] step 21 (owner r139): + edgeMask (F1) + {wsecEp, wsecMask, watkEp, watkMask}
  // (F2/F3): 28 -> 56 bits per way entry.
  // [STDW] patch 60 (V1 (a), owner 2026-10-19 16:3x; claude-artifacts/20-attacker-taint-lifecycle.md V1):
  // + stMask, IFT only, at the TOP of the entry (every older field keeps its offset): bit k = "DW k has been
  // stored to since this line was filled".  Cleared with the rest of the entry at the first refill beat (the
  // FILLINIT zero), set by every store to DW k (clean or not; [G9STMETA] hit and replay paths).  The
  // line-STATE value taint (r116 / r119: an attacker-domain fill, an attacker-address residency mark, an
  // attacker-caused eviction) now applies to a DW only while its bit is 0, so a clean overwrite clears it, as
  // atkMask already does for store data (P5 "value: overwrite").  The edges are unchanged (line-level record).
  // IFT off the width is unchanged (the dead ift_sm_last_dat register keeps its _RANDOM layout).
  val iftStoreBits   = CF_SUBLINE + CF_SUBLINE + inflPcBitsCF + CF_SUBLINE + 2 * (CF_SUBLINE + 2) +
                       (if (ENABLE_IFT) CF_SUBLINE else 0)
  def cfSubIdx(addr: UInt): UInt = addr(log2Up(dcacheParams.blockBytes)-1, 3)
  def cfSubOH(addr: UInt): UInt  = UIntToOH(cfSubIdx(addr))(CF_SUBLINE-1, 0)

  // ---- FILL format: {secMask[CF_SUBLINE], domain[1], pc[13:2]} ----
  def iftPcSlice(pc: UInt): UInt = pc(inflPcBitsCF + 1, 2)     // PC[13:2]
  def mkIftFill(domain: UInt, pc12: UInt, secMask: UInt): UInt =
    Cat(secMask(CF_SUBLINE-1,0), domain, pc12(inflPcBitsCF-1, 0))
  def iftFillDomain(e: UInt) : UInt = e(inflPcBitsCF)
  def iftFillPc(e: UInt)     : UInt = e(inflPcBitsCF - 1, 0)   // PC[13:2] of the filler
  def iftFillSecMask(e: UInt): UInt = e(inflPcBitsCF + CF_SUBLINE, inflPcBitsCF + 1)

  // ---- STORE format: {watkEp[2], watkMask, wsecEp[2], wsecMask, edgeMask, atkMask, secMask, pc[13:2]}
  //      (masks CF_SUBLINE bits each; low fields unchanged from step 20) ----
  // [WLEDGE] step 21 (owner r139, F1; amends step 17 [WLSEC] / r118 Q5): a whole-line secret mark
  // raises the influence EDGE only.  edgeMask bit k: "a reader of DW k gets MEM_DATAFLOW sec"
  // (with r117's sticky s_prop) WITHOUT a secret value; VALUE taint stays in secMask, per
  // doubleword.  A store to DW k clears edge bit k (r119: a clean overwrite makes k clean in
  // every sense); a [WLSEC] store sets all 8.  Cleared on refill with the rest of the entry.
  // [WINMEM] step 21 (owner r139, F2/F3): WINDOW-kind marks per doubleword (secret and attacker
  // windows) with the epoch of the store that set them.  A store keeps the old window bits only
  // when their epoch equals its own (otherwise they expired) and writes its own epoch.
  def mkIftStore(pc12: UInt, secMask: UInt, atkMask: UInt, edgeMask: UInt,
                 wsecMask: UInt, wsecEp: UInt, watkMask: UInt, watkEp: UInt, stMask: UInt): UInt = {
    val lo = Cat(watkEp(1,0), watkMask(CF_SUBLINE-1,0), wsecEp(1,0), wsecMask(CF_SUBLINE-1,0),
                 edgeMask(CF_SUBLINE-1,0), atkMask(CF_SUBLINE-1,0), secMask(CF_SUBLINE-1,0), pc12(inflPcBitsCF-1, 0))
    if (ENABLE_IFT) Cat(stMask(CF_SUBLINE-1,0), lo) else lo   // [STDW] patch 60
  }
  def iftPc(e: UInt)      : UInt = e(inflPcBitsCF - 1, 0)      // PC[13:2] of the last storer
  def iftSecMask(e: UInt) : UInt = e(inflPcBitsCF + CF_SUBLINE - 1, inflPcBitsCF)
  def iftAtkMask(e: UInt) : UInt = e(inflPcBitsCF + 2*CF_SUBLINE - 1, inflPcBitsCF + CF_SUBLINE)
  val iftEdgeLo = inflPcBitsCF + 2*CF_SUBLINE
  val iftWsecLo = iftEdgeLo + CF_SUBLINE
  val iftWatkLo = iftWsecLo + CF_SUBLINE + 2
  def iftEdgeMask(e: UInt): UInt = e(iftEdgeLo + CF_SUBLINE - 1, iftEdgeLo)
  def iftWsecMask(e: UInt): UInt = e(iftWsecLo + CF_SUBLINE - 1, iftWsecLo)
  def iftWsecEp(e: UInt)  : UInt = e(iftWsecLo + CF_SUBLINE + 1, iftWsecLo + CF_SUBLINE)
  def iftWatkMask(e: UInt): UInt = e(iftWatkLo + CF_SUBLINE - 1, iftWatkLo)
  def iftWatkEp(e: UInt)  : UInt = e(iftWatkLo + CF_SUBLINE + 1, iftWatkLo + CF_SUBLINE)
  // [STDW] patch 60: the stored-since-fill mask (IFT only; constant 0 IFT off)
  def iftStMask(e: UInt)  : UInt = if (ENABLE_IFT) e(iftStoreBits - 1, iftStoreBits - CF_SUBLINE) else 0.U(CF_SUBLINE.W)
  def iftStAt(e: UInt, addr: UInt): Bool = (iftStMask(e) & cfSubOH(addr)).orR
  // [PCPROV] one candidate per PC[13:12] value: infl_type_int must stay a Scala constant
  // (the batch constant-folds on it), so the 2 high bits select among 4 static codes.
  def pcCandidates(fire: Bool, pc12: UInt, base: Int, is_atk: Bool, is_secret: Bool): Seq[InfluencerCandidate] =
    (0 until 4).map { hi =>
      InfluencerCandidate(fire && pc12(inflPcBitsCF-1, inflPcBitsCF-2) === hi.U,
                          pc12(inflPcBitsCF-3, 0), inflPcCode(base, hi), is_atk, is_secret, is_pc = true)
    }
  // per-access accessors: test only the doubleword this access touches
  def iftSecretAt(e: UInt, addr: UInt): Bool = (iftSecMask(e) & cfSubOH(addr)).orR
  def iftAtkAt(e: UInt, addr: UInt)   : Bool = (iftAtkMask(e) & cfSubOH(addr)).orR
  // [WLEDGE] edge-only (whole-line) mark at this DW, or a value mark (which also has the edge)
  def iftEdgeAt(e: UInt, addr: UInt)  : Bool = ((iftEdgeMask(e) | iftSecMask(e)) & cfSubOH(addr)).orR
  // [WINMEM] live window mark at this DW: the epoch must match the READER's dispatch epoch (the
  // core then checks that against the current one at the register write)
  def iftWsecAt(e: UInt, addr: UInt, ep: UInt): Bool = (iftWsecMask(e) & cfSubOH(addr)).orR && iftWsecEp(e) === ep
  def iftWatkAt(e: UInt, addr: UInt, ep: UInt): Bool = (iftWatkMask(e) & cfSubOH(addr)).orR && iftWatkEp(e) === ep

  val ift_fill_meta  = Seq.fill(nWays)(SyncReadMem(nSets, UInt(iftFillBits.W)))
  val ift_store_meta = Seq.fill(nWays)(SyncReadMem(nSets, UInt(iftStoreBits.W)))

  // Per-way ift_store_meta write staging: collect from all memWidth ports, merge into
  // a single write per way after the loop.  Prevents Vivado Synth 8-4767 "multiple
  // writes via different ports" which dissolves SyncReadMem into FFs.
  val ift_sm_wr_valid = Seq.fill(nWays)(Wire(Vec(memWidth + 1, Bool())))
  val ift_sm_wr_idx   = Seq.fill(nWays)(Wire(Vec(memWidth + 1, UInt(idxBits.W))))
  val ift_sm_wr_data  = Seq.fill(nWays)(Wire(Vec(memWidth + 1, UInt(iftStoreBits.W))))
  for (way <- 0 until nWays) {
    ift_sm_wr_valid(way) := VecInit(Seq.fill(memWidth + 1)(false.B))
    ift_sm_wr_idx(way)   := DontCare
    ift_sm_wr_data(way)  := DontCare
  }
  // [STFWD] step 15 (residue audit row 1c, the s1-read/s2-write hazard): last cycle's merged
  // ift_store_meta write per way.  A store-meta read issued in the write cycle returns the old
  // entry, so two stores to one line on consecutive cycles lost the first one's DW bit.  The
  // s2 read is forwarded from here when the set matches (only a same-cycle write can be stale).
  // [STFWD-S1] Phase 4 Task 4.3b (gate review C5): the set compare runs in s1, against the
  // read's own index, and only its per-lane, per-way result is registered (ift_sm_fwd_hit,
  // driven in the merged-write block below).  s2 keeps just the 2:1 mux on a flop select.
  val ift_sm_last_dat = Reg(Vec(nWays, UInt(iftStoreBits.W)))
  val ift_sm_fwd_hit  = if (ENABLE_IFT) Some(RegInit(VecInit(Seq.fill(memWidth)(VecInit(Seq.fill(nWays)(false.B))))))
                        else None

  // [RESID] step 15 (residue audit rows 2/5/6/13): per-way line RESIDENCY metadata
  // {asec, PC[13:2]}: "this line's presence/recency was set by an access at a secret-derived
  // ADDRESS".  Kept apart from the content masks (ift_fill_meta.secMask = s_acc only, SECFIX):
  // the mark is keyed on ADDRESS taint, never on the requester's s_prop.  Writers, merged onto
  // one port per way:
  //   slot 0          clear on the first refill beat (a new line has no residency history)
  //   slot 1          set at the MSHR meta write when the primary or a merged secondary had a
  //                   secret address (set-only: a permission upgrade keeps the line's mark)
  //   slot 2+w        set by a secret-address load/store HIT on lane w
  //   [D1FIXA] priority: lanes (2+w) > slot 0 > slot 1; a losing slot 0/1 write is deferred
  //   (cf_prm_way / cf_prf_*), never dropped.  Lane 1 vs lane 0 on one way (R3) still drops.
  // Reader: every load (any domain) of a marked line gets CACHE_EVICTION {atk=0, sec=1,
  // PC = the secret-address access}.  [SECDEST] step 17 (owner ruling 116 A(2), answers
  // 2026-10-13) REPLACES step 15's "timing only, no value taint": the loaded VALUE is secret too
  // (preg secret + s_prop, via cf_mem_sec_dataflow; see the s2 response block and mshrs.scala).
  // [ATKRES] step 17 (owner ruling 119 (3)) widens the entry to {asec, aatk, PC[13:2]}: aatk =
  // the line was filled or touched by an access whose ADDRESS is attacker-tainted.  A victim
  // load of an aatk line gets CACHE_EVICTION {atk=1, sec=0, PC} (sharing the evict_fire
  // candidate) and attacker value taint.  PC = the secret cause when asec, else the attacker one.
  val iftResBits = 2 + inflPcBitsCF
  def iftResSec(e: UInt): Bool = e(inflPcBitsCF + 1)
  def iftResAtk(e: UInt): Bool = e(inflPcBitsCF)
  def iftResPc(e: UInt) : UInt = e(inflPcBitsCF - 1, 0)
  // [RUWRES] patch 32 (DF-ruw-parity): combinational-read Mem + a write-first s1->s2 data
  // register (as [REPLBYP]/[RUWEVF]).  As a SyncReadMem the FireSim SV returned the OLD entry to a
  // same-set read issued in the write cycle, the Verilator model the NEW one.  Reachable: a
  // secret-address hit A marks the line (slot 2+w) and a hit B on the same line one cycle behind
  // read the pre-A entry on the FPGA only, so B lost the residency edge and value taint, and B's
  // own read-modify-write mark (slot 2+w, {old_s || sec, old_a || atk, pc}) wrote A's mark away
  // (FPGA-only under-taint).  Now both lowerings see A's write (today's Verilator behaviour).
  val ift_res_meta    = if (ENABLE_IFT) Seq.fill(nWays)(Mem(nSets, UInt(iftResBits.W))) else Nil   // [IFTGATE]
  val ift_rm_wr_valid = Seq.fill(nWays)(Wire(Vec(memWidth + 2, Bool())))
  val ift_rm_wr_idx   = Seq.fill(nWays)(Wire(Vec(memWidth + 2, UInt(idxBits.W))))
  val ift_rm_wr_data  = Seq.fill(nWays)(Wire(Vec(memWidth + 2, UInt(iftResBits.W))))
  for (way <- 0 until nWays) {
    ift_rm_wr_valid(way) := VecInit(Seq.fill(memWidth + 2)(false.B))
    ift_rm_wr_idx(way)   := DontCare
    ift_rm_wr_data(way)  := DontCare
  }
  // [D1FIXA] D1 fix A (phase4/d1/rootcause.md, owner choice 2026-10-18; covers S1/S2/R1/R2).
  // Both per-way merged writers (ift_store_meta, ift_res_meta) gave slot 0, the refill first-beat
  // clear, priority.  The clear is for a NEW line in (refill set, way); a lane write in the same
  // cycle to the same WAY but ANOTHER set lost its write (S1 clean store -> stale secret mark,
  // S2 secret store -> lost mark, R1 lane residency mark lost, R2 MSHR fill mark lost).
  // Now LANE slots win.  A non-lane write that loses is DEFERRED, never dropped:
  //   cf_psm_way / cf_prm_way  one-hot way of a lost refill clear (store meta / res meta); both
  //                            share cf_pclr_idx (the same first beat writes both arrays)
  //   cf_prf_way/idx/dat       a lost MSHR fill mark (res meta slot 1), with its data
  // The deferred write re-enters its slot (the slot's "clear source" is
  // Mux(cf_pend_hold, pending, new)) and lands on the first cycle no lane slot writes its way.
  // cf_pend_hold (= any pending, one flop) holds every MSHR's refill beats and meta writes
  // (mshrs.scala [D1FIXA]), so (1) no new clear / fill mark arrives while one is pending (one
  // entry per kind is enough, and the slot source is exclusive), and (2) the MSHR whose clear is
  // pending stays in s_commit_line (its next beat is held), so the clear lands before any replay
  // or meta write of its line ([G9STMETA] order kept).  cacheDataBeats >= 2 is required for (2).
  // Liveness: cf_pend_starve (16 cycles pending) blocks new LSU requests (as wk_block does), so the
  // lane slots drain (MSHR replays are bounded: sec_rdy is off in s_drain_rpq, and held MSHRs
  // cannot enter it).  R3 (lane 1 vs lane 0 res mark, same way) is not covered (decided later).
  // IFT off: every one of these is a constant-false/zero wire; the merged writers do not exist.
  require(!ENABLE_IFT || cacheDataBeats >= 2, "[D1FIXA] the pending clear needs >= 2 refill beats")
  // Lane-first one-hot select over a writer's slot vector: slots [nLow, n) (lanes) win, then
  // [0, nLow) in index order.  Same LUT shape as the PriorityEncoderOH it replaces.
  private def cfLaneFirstOH(wv: UInt, nLow: Int): UInt = {
    val n   = wv.getWidth
    val ord = Cat(wv(nLow-1, 0), wv(n-1, nLow))            // lanes at the low (winning) end
    val oh  = PriorityEncoderOH(ord)
    Cat(oh(n-nLow-1, 0), oh(n-1, n-nLow))                  // back to slot order
  }
  val cf_pend_hold   = if (ENABLE_IFT) RegInit(false.B) else WireInit(false.B)
  val cf_pend_age    = if (ENABLE_IFT) RegInit(0.U(4.W)) else WireInit(0.U(4.W))
  val cf_pend_starve = if (ENABLE_IFT) RegInit(false.B) else WireInit(false.B)
  val cf_psm_way     = if (ENABLE_IFT) RegInit(0.U(nWays.W)) else WireInit(0.U(nWays.W))
  val cf_prm_way     = if (ENABLE_IFT) RegInit(0.U(nWays.W)) else WireInit(0.U(nWays.W))
  val cf_prf_way     = if (ENABLE_IFT) RegInit(0.U(nWays.W)) else WireInit(0.U(nWays.W))
  val cf_pclr_idx    = if (ENABLE_IFT) Reg(UInt(idxBits.W)) else WireInit(0.U(idxBits.W))
  val cf_prf_idx     = if (ENABLE_IFT) Reg(UInt(idxBits.W)) else WireInit(0.U(idxBits.W))
  val cf_prf_dat     = if (ENABLE_IFT) Reg(UInt(iftResBits.W)) else WireInit(0.U(iftResBits.W))
  mshrs.io.cf_ift_hold := cf_pend_hold   // a flop; constant false with IFT off
  // Per-lane slot valids / res-mark data, for the deferral next-state below (set where the lane
  // slots are written).  The pending next-state is written in terms of these, factored as
  // lane valid AND (flop / early terms), so a late lane valid is 1-2 LUTs from its pending flop.
  val cf_sm_lane_v = WireInit(VecInit(Seq.fill(memWidth)(false.B)))
  val cf_rm_lane_v = WireInit(VecInit(Seq.fill(memWidth)(false.B)))
  val cf_rm_lane_d = WireInit(VecInit(Seq.fill(memWidth)(0.U(iftResBits.W))))
  // Slot index sources.  A valid clear slot under cf_pend_hold is the pending one (no first beat
  // can fire under hold), otherwise the first beat; likewise for the fill slot.  The 2:1 mux has
  // a flop select and register-sourced data, so it settles in parallel with the late slot valids.
  val cf_clr_src_idx = Mux(cf_pend_hold, cf_pclr_idx, dcacheMaskIdx(mshrs.io.refill.bits.addr))
  val cf_fill_new_idx = mshrs.io.cf_meta_write_fill.bits.idx & dcache_set_mask(idxBits-1, 0)
  val cf_fill_src_idx = Mux(cf_pend_hold, cf_prf_idx, cf_fill_new_idx)
  // [RUWRES] the merged single write per way, shared by the write below and the forward.
  // [D1FIXA] lane slots (2+l) win, then slot 0 (refill clear), then slot 1 (MSHR fill mark).
  // The forward select uses the slot index SOURCES (as [STFWD-S1]): slot 0 = cf_clr_src_idx,
  // slot 1 = cf_fill_src_idx, slot 2+l = lane l's s2 set; each slot's ift_rm_wr_idx equals its
  // source whenever the slot is valid, so (wv & oh & eq).orR === rm_we && rm_wa === s1_set exactly.
  val ift_rm_wv  = (0 until nWays).map(way => ift_rm_wr_valid(way).asUInt)
  val ift_rm_oh  = (0 until nWays).map(way => cfLaneFirstOH(ift_rm_wv(way), 2))
  val ift_rm_sel = (0 until nWays).map(way => OHToUInt(ift_rm_oh(way)))   // probe printf only
  val ift_rm_we  = (0 until nWays).map(way => ift_rm_wv(way).orR)
  val ift_rm_wa  = (0 until nWays).map(way => Mux1H(ift_rm_oh(way), ift_rm_wr_idx(way)))
  val ift_rm_wd  = (0 until nWays).map(way => Mux1H(ift_rm_oh(way), ift_rm_wr_data(way)))
  private def cfResRead(way: Int, ra: UInt, en: Bool): UInt = {
    val eq = VecInit(Seq(cf_clr_src_idx === ra, cf_fill_src_idx === ra) ++
                     (0 until memWidth).map(l => dcacheMaskIdx(s2_req(l).addr) === ra)).asUInt
    val hit = (ift_rm_wv(way) & ift_rm_oh(way) & eq).orR
    // [D1FIXA] a deferred fill mark is forwarded until it lands: the array entry of that line is
    // a subset of cf_prf_dat (its clear landed before the MSHR left s_commit_line, and every lane
    // mark of the line since is merged into cf_prf_dat).  Flop-sourced select (8-b one-hot bit +
    // 7-b eq): the read mux gains 2 inputs in the same LUT, no level.
    val pf_hit = cf_prf_way(way) && cf_prf_idx === ra
    RegEnable(Mux(hit, ift_rm_wd(way), Mux(pf_hit, cf_prf_dat, ift_res_meta(way).read(ra))), en)
  }

  for (w <- 0 until memWidth) {
    cache_resp(w).valid := s2_valid(w) && s2_send_resp(w)

    // corefuzzing: build the response uop.
    // Read ift_fill_meta and ift_store_meta in s1 (SyncReadMem: address presented in s1, result available in s2).
    val s1_set        = dcacheMaskIdx(s1_req(w).addr)
    val s2_fill_ways  = VecInit((0 until nWays).map(i => ift_fill_meta(i).read(s1_set,  s1_valid(w))))
    val s2_store_ways = VecInit((0 until nWays).map(i => ift_store_meta(i).read(s1_set, s1_valid(w))))
    val s2_res_ways   = VecInit((0 until nWays).map(i =>                                         // [RESID]
                          if (ENABLE_IFT) cfResRead(i, s1_set, s1_valid(w)) else 0.U(iftResBits.W)))   // [IFTGATE] [RUWRES]
    // [STFWD] forward last cycle's write of the same set (see ift_sm_last_dat).
    // [STFWD-S1] the select is the registered s1 compare (ift_sm_fwd_hit): -2 levels (7-bit eq
    // of the masked s2 index, AND) ahead of Mux1H(s2_tag_match_way); proof at the driver.
    val s2_set_cf      = dcacheMaskIdx(s2_req(w).addr)   // [RESID] write index below
    val s2_store_ways_f = VecInit((0 until nWays).map(i =>
      Mux(ift_sm_fwd_hit.map(_(w)(i)).getOrElse(false.B), ift_sm_last_dat(i), s2_store_ways(i))))
    // Mux by one-hot hit way (known in s2)
    val s2_fill_entry  = Mux1H(s2_tag_match_way(w), s2_fill_ways)
    val s2_store_entry = Mux1H(s2_tag_match_way(w), s2_store_ways_f)
    val s2_res_entry   = Mux1H(s2_tag_match_way(w), s2_res_ways)   // [RESID]

    // [G9STMETA] (ruling 101, G-9) the per-doubleword store-metadata write, shared by the store
    // HIT path (t_lsu) and the MSHR REPLAY that performs a store which missed (t_replay).  Body
    // unchanged from the hit path ([STEP1b] last-writer-wins per DW).  For a replay the rpq
    // carries the store's domain folded into cf_attacker_influence ([G9STMETA] in mshrs.scala),
    // so st_atk is the same value the hit path computes.
    def iftStoreMetaWrite(): Unit = {
        val s2_set   = dcacheMaskIdx(s2_req(w).addr)
        // [STEP1b] per-DOUBLEWORD last-writer-wins.  This store updates ONLY the DW it
        // writes; every other DW's bit is carried through unchanged from the current entry.
        // That is NOT accumulation: within its own chunk the newest store wins outright, so
        // a clean store genuinely clears its chunk.  It simply cannot clear chunks it never
        // touched -- which is the bug R1f was working around by OR-ing the whole line.
        // domain stays ownership-only; atk/secret are the content questions.
        val sub_oh    = cfSubOH(s2_req(w).addr)
        val st_atk    = s2_req(w).uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)).orR || s2_req(w).uop.cf_attacker_influence.getOrElse(false.B)
        val st_sec    = s2_req(w).uop.cf_secret_propagation.getOrElse(false.B) || s2_req(w).uop.cf_secret_access.getOrElse(false.B)
        val new_atk_m = (iftAtkMask(s2_store_entry) & ~sub_oh) | Mux(st_atk, sub_oh, 0.U)
        // [WLSEC] step 17 (owner answer Q5): secret data stored to a non-secret address (the LSU
        // sets uop.cf_secret_transmission on the store request for exactly that case) marks
        // every DW of the line.  [WLEDGE] step 21 (owner r139, F1): the line-wide mark is the
        // EDGE mask only; the VALUE mask takes the stored DW alone (the per-DW rule before
        // step 17), so a reader of another DW gets MEM_DATAFLOW sec (sticky s_prop) but a clean
        // value.  The value-mask mux is gone (-1 level); the edge mask is one 2:1 mux, parallel.
        // [co2] review 3.9b, re-proved on step 21 (Phase 4 rebuild 1): every D$ write request
        // (t_lsu store hit, t_replay) comes from the LSU store commit, where cf_secret_transmission
        // = data_is_secret && !addr_in_secret and cf_secret_propagation = data_is_secret (window
        // kinds ride cf_opnd_*, never these bits); the rpq, replay_req and the MSHR request pass
        // both bits through unchanged, and hella requests carry NullMicroOp.  So s_tx implies
        // st_sec and the `st_sec &&` term was redundant.
        val st_wl      = s2_req(w).uop.cf_secret_transmission.getOrElse(false.B)
        val new_sec_m  = (iftSecMask(s2_store_entry) & ~sub_oh) | Mux(st_sec, sub_oh, 0.U)
        val new_edge_m = Mux(st_wl, Fill(CF_SUBLINE, 1.U(1.W)), iftEdgeMask(s2_store_entry) & ~sub_oh)
        // [WINMEM] step 21 (owner r139, F2/F3): the store's WINDOW taint (LSU: address, data or
        // membership, on the request uop's cf_opnd_*) marks its DW window-kind, never secMask /
        // atkMask; old window bits survive only within the store's own epoch.  Per kind: one
        // 2-bit compare, then the same set/clear LUT as the other masks (+1 level, parallel).
        val st_wsec    = s2_req(w).uop.cf_opnd_wsec.getOrElse(false.B)
        val st_watk    = s2_req(w).uop.cf_opnd_watk.getOrElse(false.B)
        val st_ep_s    = s2_req(w).uop.cf_wep_sec.getOrElse(0.U(2.W))
        val st_ep_a    = s2_req(w).uop.cf_wep_atk.getOrElse(0.U(2.W))
        val old_ws     = Mux(iftWsecEp(s2_store_entry) === st_ep_s, iftWsecMask(s2_store_entry), 0.U)
        val old_wa     = Mux(iftWatkEp(s2_store_entry) === st_ep_a, iftWatkMask(s2_store_entry), 0.U)
        val new_ws_m   = (old_ws & ~sub_oh) | Mux(st_wsec, sub_oh, 0.U)
        val new_wa_m   = (old_wa & ~sub_oh) | Mux(st_watk, sub_oh, 0.U)
        // [STDW] patch 60: every store marks its DW as overwritten since the fill (one OR, parallel to atkMask)
        val new_st_m   = iftStMask(s2_store_entry) | sub_oh
        val entry      = mkIftStore(iftPcSlice(s2_req(w).uop.debug_pc), new_sec_m, new_atk_m, new_edge_m,
                                    new_ws_m, st_ep_s, new_wa_m, st_ep_a, new_st_m)   // [PCPROV]
        cf_sm_lane_v(w) := true.B   // [D1FIXA]
        for (way <- 0 until nWays) {
          when (s2_tag_match_way(w)(way)) {
            ift_sm_wr_valid(way)(w + 1) := true.B
            ift_sm_wr_idx(way)(w + 1)   := s2_set
            ift_sm_wr_data(way)(w + 1)  := entry
          }
        }
    }

    val uop_resp_base = WireInit(s2_req(w).uop)
    uop_resp_base.cf_fu_bitmap.foreach(_ := s2_req(w).uop.cf_fu_bitmap.getOrElse(0.U(numModules.W)) | (1.U << dcacheTagCF.U))

    val uop_resp_final = WireInit(uop_resp_base)
    // [WINMEM] step 21: on the response cf_opnd_* means "the memory word read was window-marked"
    // (set below); the request's own value (a store's window taint, a load's address window,
    // which the LSU keeps in its LDQ entry) is not passed back.
    uop_resp_final.cf_opnd_wsec.foreach(_ := false.B)
    uop_resp_final.cf_opnd_watk.foreach(_ := false.B)
    // Task 3.2 (D16): the D$ edges travel on the response, not in the uop; zero unless a hit fires one
    cache_resp(w).bits.edges := 0.U.asTypeOf(new IftEdgeList)
    // [RESID] step 15 (residue audit row 1b): a load served by an MSHR REPLAY (t_replay, always
    // a hit on the way the MSHR filled) now gets the D$ edges too.  For a replay the fill meta
    // is the EVICTED line's (the MSHR writes it after draining) and the rpq zeroes the domain,
    // so evict_fire and memdf_fire stay hit-only; memsec reads the store meta of the new line
    // (cleared at the first refill beat, then written by this MSHR's store replays) and the
    // residency edge comes from the MSHR (s2_req.cf_res_v/pc), not from ift_res_meta.
    // [RESPSEL] patch 25: !s2_nack_hp == !s2_nack under s2_hit (exact), and keeps MSHR ready off the edges
    val cf_lsu_hit = s2_valid(w) && s2_hit(w) && s2_type === t_lsu && !s2_nack_hp.map(_(w)).getOrElse(s2_nack(w))
    val cf_rp_load = s2_valid(w) && s2_type === t_replay && isRead(s2_req(w).uop.mem_cmd)
    // [RESID] a secret-ADDRESS load/store hit marks the line it touched (slot 2+w).
    // [ATKRES] step 17: so did an attacker-ADDRESS hit (removed by [RESFILL] patch 61).  The marks OR into the line's entry
    // (read in s2, as the store meta is), so one kind never erases the other; the PC follows
    // the secret cause first.  2 OR + one 12-b 2:1 mux on the slot data.
    // [RESFILL] patch 61 (V2 (a), owner 2026-10-19 16:3x): the ATTACKER kind of the residency mark is set
    // at the FILL only (MSHR meta write, slot 1: an attacker-address primary or merged secondary), no longer
    // ORed by every attacker-address HIT (with ownership-seeded attacker AGU addresses that was every attacker
    // touch of a resident line, kept for the line's whole residency).  A hit's replacement-state influence is
    // recorded by ift_repl_atk (overwrite, the eviction attribution).  The secret kind ([RESID], r116 A(2)) is
    // unchanged.  A secret hit keeps the line's attacker bit (old value) and, as before, takes the PC.
    // Cost: -2 LUT inputs (when + data), 0 FF, 0 levels.  IFT off: cf_addr_* are None (constant 0).
    val cf_mark_sec = s2_req(w).cf_addr_sec.getOrElse(false.B)
    when (cf_lsu_hit && cf_mark_sec) {
      val old_s = iftResSec(s2_res_entry)
      val mk_pc = Mux(cf_mark_sec || !old_s, iftPcSlice(s2_req(w).uop.debug_pc), iftResPc(s2_res_entry))
      val mk_d  = Cat(old_s || cf_mark_sec, iftResAtk(s2_res_entry), mk_pc)
      cf_rm_lane_v(w) := true.B   // [D1FIXA]
      cf_rm_lane_d(w) := mk_d
      for (way <- 0 until nWays) {
        when (s2_tag_match_way(w)(way)) {
          ift_rm_wr_valid(way)(w + 2) := true.B
          ift_rm_wr_idx(way)(w + 2)   := s2_set_cf
          ift_rm_wr_data(way)(w + 2)  := mk_d
        }
      }
    }
    when (cf_lsu_hit || cf_rp_load) {
      val uop_dom   = s2_req(w).uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W))
      // [AMOLOAD 2026-09-08] An AMO READS memory and returns the old value to rd,
      // but isWrite() is true for AMOs (Consts.scala:90), so `!isWrite` classified
      // every AMO as a store and memdf_fire/memsec_fire could NEVER fire for one.
      // An AMO on an attacker-written (or secret) line returned untainted data.
      // Adding the isAMO term changes ONLY the AMO case -- every other mem_cmd
      // keeps its previous classification exactly.
      // (Known, deliberately NOT changed here: M_PFR prefetch is also counted as a
      //  "load" by !isWrite.  It is inert -- a prefetch has no destination register,
      //  so the taint it sets has no consumer.  One variable per validation run.)
      //
      // is_load and is_store are SEPARATE questions and an AMO answers YES to both.
      // Deriving the store predicate as `!is_load` (as this code did) forces them to be
      // complements, which is exactly why the AMO case could not be expressed: making
      // AMO a load would have silently stopped it updating ift_store_meta.
      //   is_load  -- does this op return memory data into a register?  (gates memdf/memsec)
      //   is_store -- does this op write memory?                       (gates the meta write)
      // `is_store` is bit-identical to the previous `!is_load`, so the store path is
      // UNCHANGED for every mem_cmd including AMO.
      val is_load   = !isWrite(s2_req(w).uop.mem_cmd) || isAMO(s2_req(w).uop.mem_cmd)
      val is_store  = isWrite(s2_req(w).uop.mem_cmd)
      // INFL_CACHE_EVICTION: victim hits a line last FILLED by attacker (timing channel, loads and stores)
      val evict_fire  = cf_lsu_hit && uop_dom === 0.U && iftFillDomain(s2_fill_entry) === 1.U
      // INFL_MEM_DATAFLOW: victim LOAD reads a line last STORED by attacker (data channel, loads only)
      // [R1e] tests the DEDICATED atk bit, not the domain field: a victim store of
      // attacker-derived data must fire this too, and it no longer forges `domain`.
      val memdf_fire  = cf_lsu_hit && uop_dom === 0.U && is_load && iftAtkAt(s2_store_entry, s2_req(w).addr)
      // Fix 3a: memsec_fire — any LOAD reading a line whose last store had s_prop/s_acc set.
      // Same-domain secret propagation via memory: is_atk=false, is_secret=true.
      // Gated by !memdf_fire to avoid two INFL_MEM_DATAFLOW entries for the same store
      // when the store is simultaneously attacker-domain AND secret-marked.
      // [WLEDGE] step 21 (owner r139, F1): the EDGE fires for a whole-line mark too (edgeMask)
      val memsec_fire = is_load && iftEdgeAt(s2_store_entry, s2_req(w).addr) && !memdf_fire
      // [RESID] residency: any-domain load on a line marked by a secret-address access.
      // At most 3 edges fire (evict, memdf|memsec, res), within the 4-slot list.
      // [NACKRES] step 16: on an LSU hit, s2_req.cf_res_v is the nack edge (ldq cf_nack_res);
      // it shares this candidate (same type/sec), the line's own residency PC wins when both.
      val res_line    = cf_lsu_hit && iftResSec(s2_res_entry)
      // [ATKRES] victim load hit on a line marked by an attacker-ADDRESS access
      val ratk_fire   = cf_lsu_hit && is_load && uop_dom === 0.U && iftResAtk(s2_res_entry)
      val res_fire    = is_load && (res_line || s2_req(w).cf_res_v.getOrElse(false.B))
      val res_pc      = Mux(res_line, iftResPc(s2_res_entry), s2_req(w).cf_res_pc.getOrElse(0.U))
      // [ATKEVICT] step 17: a replay's MSHR edge may be an attacker-caused eviction {atk=1,
      // sec = also secret-chosen}; the LSU drives cf_res_atk/esec = 0, so the nack edge and the
      // line residency stay {atk=0, sec=1}.  One AND/OR level on the candidate's flag inputs.
      // [NACKATK] patch 44: the LSU nack edge may now be {atk=1} (lsu.scala cf_nack_res_atk); on a
      // hit of a secret-resident line the line's PC wins and the edge is {atk, sec=1} (one PC,
      // secret-first).  The LSU drives atk=0 everywhere else, so with it 0: as before.
      val res_atk     = s2_req(w).cf_res_atk.getOrElse(false.B)
      val res_sec     = res_line || !res_atk || s2_req(w).cf_res_esec.getOrElse(false.B)

      // [PCPROV] 3 candidates -> 12: each D$ edge is one of four static type codes
      // selected by the producer PC[13:12]; exactly one of each group of 4 can fire.
      // Task 3.2 (D16): built on a ZERO base into the response's edge list (at most 3 fire,
      // so nothing drops; PC candidates never set aliased).  The LSU promotes
      // cf_attacker_influence from these edges (respEdgeAtk); Task 3.9 removed the copy of
      // that promotion on the response uop, which nothing read.
      val (resp_edges, _) = addInfluencerBatchList(0.U.asTypeOf(new IftEdgeList), s2_req(w).uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)),
        // [ATKRES] step 17: an attacker-ADDRESS-marked line shares the attacker CACHE_EVICTION
        // candidate (same class {atk=1}); the attacker-FILLED case keeps priority for PC/sec.
        pcCandidates(evict_fire || ratk_fire,
                     Mux(evict_fire, iftFillPc(s2_fill_entry), iftResPc(s2_res_entry)),
                     INFL_CACHE_EVICTION, true.B, evict_fire && iftFillSecMask(s2_fill_entry).orR) ++
        pcCandidates(memdf_fire,  iftPc(s2_store_entry),    INFL_MEM_DATAFLOW,   true.B,  iftEdgeAt(s2_store_entry, s2_req(w).addr)) ++   // [WLEDGE]
        pcCandidates(memsec_fire, iftPc(s2_store_entry),    INFL_MEM_DATAFLOW,   false.B, true.B) ++
        pcCandidates(res_fire,    res_pc,                   INFL_CACHE_EVICTION, res_atk, res_sec))   // [RESID]
      cache_resp(w).bits.edges := resp_edges
      // Set summary flags so writeback paths can skip scanning cf_influencer_list.
      when (memdf_fire)  { uop_resp_final.cf_mem_dataflow_atk.foreach(_ := true.B) }
      // [FIFO10] step 10 (R-STICKY): the secret-dataflow summary is set for EVERY load that reads a
      // secret-written chunk.  `!memdf_fire` in memsec_fire only de-duplicates the EDGE (the
      // memdf edge already carries is_secret); it used to suppress the summary bit as well when
      // the last writer was attacker-influenced and secret.
      // [WLEDGE] step 21: the VALUE flag keys on the per-DW value mask only (never edgeMask).
      when (is_load && iftSecretAt(s2_store_entry, s2_req(w).addr)) { uop_resp_final.cf_mem_sec_dataflow.foreach(_ := true.B) }
      // [WINMEM] step 21 (owner r139, F2): a load (or AMO) of a WINDOW-marked DW, in the reader's
      // window epoch, returns WINDOW taint on cf_opnd_* (LSU merges it into the load's uop).
      // No edge and no secret value: it never re-arms the window.
      when (is_load && iftWsecAt(s2_store_entry, s2_req(w).addr, s2_req(w).uop.cf_wep_sec.getOrElse(0.U(2.W)))) { uop_resp_final.cf_opnd_wsec.foreach(_ := true.B) }
      when (is_load && iftWatkAt(s2_store_entry, s2_req(w).addr, s2_req(w).uop.cf_wep_atk.getOrElse(0.U(2.W)))) { uop_resp_final.cf_opnd_watk.foreach(_ := true.B) }
      // [SECDEST] step 17 (owner ruling 116 A(2)): a load, in ANY domain, that reads a line
      // marked by a secret-ADDRESS access (ift_res_meta) returns a value that sits where the
      // secret sent it: value taint, not only the residency edge.  cf_mem_sec_dataflow is the
      // existing "loaded value is secret via D$ metadata" flag: LSU -> iresp.secret (preg
      // secret) -> core cf_s_prop_rob_upd + ROB sticky s_prop.  Keyed on the line's ADDRESS-taint
      // mark only (never on the requester's s_prop), and the nack half of res_fire (MSHR busy,
      // no line mark) is excluded.  Miss path: mshrs.scala sets the same flag on the rpq uop.
      when (is_load && res_line) { uop_resp_final.cf_mem_sec_dataflow.foreach(_ := true.B) }
      // [ATKEVICT] step 17 (owner extension of ruling 116 C): a victim LOAD hitting a line last
      // FILLED by an attacker-domain miss (evict_fire, ift_fill_meta domain) returns a value the
      // attacker placed there: attacker taint on the value (preg taint_atk via the LSU's
      // cf_mem_dataflow_atk term; the flag also gates the ROB's duplicate type-0 atk edge,
      // [MEMATKGATE]).  cf_attacker_influence is already promoted from the atk=1 edge.
      // [STDW] patch 60 (V1 (a)): ...only while this DW has not been stored to since the fill (iftStAt): a
      // store (clean or not) overwrote it, and atkMask / memdf now carry that store's own taint.  The
      // CACHE_EVICTION edge (evict_fire, a line-level record) is unchanged.  Levels: iftStAt is the same
      // Mux1H + 8:1 DW select as iftAtkAt (memdf), parallel; the AND joins the flag's OR LUT: 0 added.
      val st_dw = iftStAt(s2_store_entry, s2_req(w).addr)
      when (is_load && evict_fire && !st_dw) { uop_resp_final.cf_mem_dataflow_atk.foreach(_ := true.B) }
      // [ATKRES] step 17 (owner ruling 119 (3)): ...and on a line marked by an attacker-ADDRESS
      // access.  Miss path: mshrs.scala cf_rp_vatk.  [STDW] patch 60: per DW, as above.
      when (ratk_fire && !st_dw) { uop_resp_final.cf_mem_dataflow_atk.foreach(_ := true.B) }
      // [STDW] patch 60, MSHR replay (t_replay) loads: the replay uop carries the MSHR's line-level value
      // taint (mshrs.scala cf_rp_vatk: attacker-caused eviction or attacker-address fill).  The store meta of
      // the new line is valid here (zeroed at the first refill beat, then written by this MSHR's store
      // replays, which precede this load in rpq order), so: a DW a replayed store overwrote loses the
      // line-level taint and takes that store's own (atkMask, VALUE only: the memdf EDGE stays hit-only as
      // before).  Line-buffer (s_drain_rpq_loads) responses keep cf_rp_vatk unchanged: no store precedes them.
      when (ENABLE_IFT.B && cf_rp_load && is_load) {
        when (st_dw) { uop_resp_final.cf_mem_dataflow_atk.foreach(_ := false.B) }
        when (iftAtkAt(s2_store_entry, s2_req(w).addr)) { uop_resp_final.cf_mem_dataflow_atk.foreach(_ := true.B) }
      }

      // corefuzzing: ift_store_meta writes are collected per-way and merged after the
      // memWidth loop so each SyncReadMem sees exactly ONE write port (Vivado dissolves
      // multi-writer SyncReadMem into FFs — Synth 8-4767).  See merged write below.
      when (cf_lsu_hit && is_store) { iftStoreMetaWrite() }   // [G9STMETA] (replays: below)
    }
    // [G9STMETA] a store that MISSED reaches the data array only through the MSHR replay
    // (t_replay; it always hits, never nacks).  It now writes the same metadata.  The line's
    // store-metadata clear moved from the MSHR's final meta write to the first refill beat
    // (below), which precedes every replay of that MSHR, so this write is not zeroed.
    when (s2_valid(w) && s2_type === t_replay && isWrite(s2_req(w).uop.mem_cmd)) { iftStoreMetaWrite() }
    cache_resp(w).bits.uop      := uop_resp_final
    cache_resp(w).bits.data     := loadgen(w).data | s2_sc_fail
    cache_resp(w).bits.is_hella := s2_req(w).is_hella
    cache_resp(w).bits.cf_res_v.foreach(_ := false.B)   // [RESID] edges already built above
    cache_resp(w).bits.cf_res_pc.foreach(_ := 0.U)
    cache_resp(w).bits.cf_res_atk.foreach(_ := false.B)   // [ATKEVICT]
    cache_resp(w).bits.cf_res_esec.foreach(_ := false.B)
  }

  // [FILLINIT 2026-09-07] A refill MUST clear this line's store-metadata.
  // ift_store_meta is a SyncReadMem with NO reset and is written by STORE HITS ONLY,
  // so a line that is filled but never stored to carries RANDOM metadata -- and
  // memdf_fire / memsec_fire read exactly that array on every load hit.  MEASURED:
  // t31 v3, whose control block runs BEFORE any attacker code and shares no cache
  // line with it, still showed 614/1792 = 34% of its instructions attacker-tainted,
  // with 928 of 967 tainted records carrying NO influencer at all -- taint with no
  // edge behind it, i.e. garbage metadata reaching the writeback taint path.
  //
  // ZERO is the correct value, not the fill's own taint: a refill brings a line that
  // nothing in THIS cache has stored to, so its store-provenance is empty.  Step 4
  // wrote Fill(CF_SUBLINE, domain === 1) here instead -- the REQUESTER's ownership --
  // which is why it regressed (ty17 atk=1 925 -> 460): it wiped real per-DW bits and
  // set false ones.  Right idea, wrong value.
  // [D1FIXA] (corrects the old note here, which said the refill's PriorityEncoder priority was
  // safe because "any store racing it is on the tag being evicted": that holds only for a store
  // to the SAME set.  The merged writer is per WAY, so the race that mattered was a lane write to
  // the same way in ANOTHER set, a live line, whose write the clear dropped: D1 S1/S2/R1.)
  // Slot 0 now LOSES to the lane slots and is deferred (cf_psm_way / cf_prm_way + cf_pclr_idx)
  // until its way is free; the MSHRs are held meanwhile (cf_pend_hold), so it still lands
  // before every replay and meta write of its own line.
  // [G9STMETA] (ruling 101) the clear is issued on the FIRST REFILL BEAT (the MSHR installing
  // the new line's data, s_commit_line), not at the MSHR's final meta write (s_meta_write_req):
  // that write comes AFTER s_drain_rpq, so it zeroed the metadata of every replayed store.
  // The first beat precedes all replays of the same MSHR.  A permission upgrade / MSHR hit
  // that refills no data no longer clears the line (its content, and its taint, is kept).
  val cf_refill_first_beat = mshrs.io.refill.fire &&
    (if (rowOffBits < blockOffBits) mshrs.io.refill.bits.addr(blockOffBits-1, rowOffBits) === 0.U else true.B)
  // [D1FIXA] slot 0 = this cycle's first beat, or the deferred clear (exclusive: no beat fires
  // under cf_pend_hold).  Per way the valid is one LUT of (first beat, way_en, pending bit), as
  // deep as the old (first beat && way_en); the index is the flop-selected cf_clr_src_idx.
  val cf_clr_new = (0 until nWays).map(way => cf_refill_first_beat && mshrs.io.refill.bits.way_en(way))
  val cf_clr_sm  = (0 until nWays).map(way => cf_clr_new(way) || cf_psm_way(way))
  val cf_clr_rm  = (0 until nWays).map(way => cf_clr_new(way) || cf_prm_way(way))
  for (way <- 0 until nWays) {
    when (cf_clr_sm(way)) {
      ift_sm_wr_valid(way)(0) := true.B
      ift_sm_wr_idx(way)(0)   := cf_clr_src_idx
      ift_sm_wr_data(way)(0)  := 0.U(iftStoreBits.W)  // clean: no store has touched this line
    }
    // [RESID] a new line has no residency history either
    when (cf_clr_rm(way)) {
      ift_rm_wr_valid(way)(0) := true.B
      ift_rm_wr_idx(way)(0)   := cf_clr_src_idx
      ift_rm_wr_data(way)(0)  := 0.U(iftResBits.W)
    }
  }

  // corefuzzing: merged single-writer for ift_store_meta (one write port per way).
  // Stores use lane 0 only (lsu.scala), so the lane slots never collide with each other; the
  // only collision is a lane write with the slot-0 clear of ANOTHER set in the same way, and
  // [D1FIXA] the lane wins and the clear is deferred (see cf_psm_way).  One write port per way
  // keeps Vivado LUTRAM inference.
  //
  // IFT compile-time gate: when ENABLE_IFT=false, the write loop is elided and
  // FIRRTL DCE removes the staging signals (ift_sm_wr_*) and the ift_store_meta
  // SRAMs entirely — their reads in the memWidth loop above (via s2_store_ways)
  // constant-propagate to 0 and the consuming addInfluencerBatch candidates die.
  val ift_sm_oh = (0 until nWays).map(way => cfLaneFirstOH(ift_sm_wr_valid(way).asUInt, 1))   // [D1FIXA]
  if (ENABLE_IFT) {
    for (way <- 0 until nWays) {
      // [D1FIXA] lane slots (1..memWidth) beat slot 0 (the clear source); Mux1H by the lane-first
      // one-hot replaces the index mux by PriorityEncoder (same select width, same depth).
      val wv     = ift_sm_wr_valid(way).asUInt
      val first  = ift_sm_oh(way)
      val any_wr = wv.orR
      when (any_wr) {
        ift_store_meta(way).write(Mux1H(first, ift_sm_wr_idx(way)), Mux1H(first, ift_sm_wr_data(way)))
      }
      ift_sm_last_dat(way) := Mux1H(first, ift_sm_wr_data(way))      // [STFWD]
      // [STFWD-S1] (Task 4.3b, review C5) per lane w: "this cycle's merged write of this way
      // hits the set lane w reads in s1", registered for lane w's s2 mux.
      // Equivalence with step 15's s2 form  last_v && last_idx === dcacheMaskIdx(s2_req(w).addr),
      // last_v = RegNext(any_wr), last_idx = RegNext(ift_sm_wr_idx(way)(sel)):
      //  (1) the slot compares use the slot's index SOURCE, which is exactly what the slot's
      //      ift_sm_wr_idx is when the slot is valid: slot 0 = cf_clr_src_idx (FILLINIT block;
      //      [D1FIXA] the pending clear's index under cf_pend_hold, else the refill index),
      //      slot l+1 = dcacheMaskIdx(s2_req(l).addr) (iftStoreMetaWrite, both callers).
      //  (2) (wv & first & eq).orR = any_wr && eq(winner): `first` is the one-hot of the slot
      //      actually written ([D1FIXA] cfLaneFirstOH, lanes before slot 0); for wv = 0 both are 0.
      //  (3) s2_req = RegNext(s1_req) with addr unmodified (only br_mask is rewritten), so
      //      RegNext(dcacheMaskIdx(s1_req(w).addr)) equals dcacheMaskIdx(s2_req(w).addr) except
      //      when dcache_set_mask changed at that edge.  cfg_set_applied changes only at the end
      //      of a dcache_idle cycle (both apply sites), and dcache_idle requires !s1_valid, so
      //      s2_valid(w) = 0 on the first cycle of the new mask.  Every reader of
      //      s2_store_entry is gated by s2_valid(w) (cf_lsu_hit, cf_rp_load, the t_replay meta
      //      write), so the forwarded value is unobservable on that cycle.
      // Hence the s2 select, every s2_store_entry read and every record are unchanged.
      // Write side: the slot compares start at registers (s1/s2 addr, MSHR refill addr) and
      // run in parallel with ift_sm_wr_valid; the hit bit is one AND-OR after the valids,
      // no deeper than the PriorityEncoder -> index mux it replaces into last_idx.
      // Cost: memWidth x nWays FF (16) replace nWays x (1 + idxBits) FF (64); the s2 7-bit
      // compares (memWidth x nWays) become memWidth x (memWidth + 1) shared slot compares.
      // [D1FIXA] the slot-0 compare reads cf_clr_src_idx: a 2:1 mux with a flop select on a
      // register-sourced input, parallel with the late valids (the compare stays 2 levels from
      // registers + 1 mux level, still ahead of the valids it ANDs with).
      for (w <- 0 until memWidth) {
        val s1_set_w = dcacheMaskIdx(s1_req(w).addr)
        val eq = VecInit(Seq(cf_clr_src_idx === s1_set_w) ++
                         (0 until memWidth).map(l => dcacheMaskIdx(s2_req(l).addr) === s1_set_w)).asUInt
        ift_sm_fwd_hit.get(w)(way) := (wv & first & eq).orR
      }
    }

    // [SMDROP] D1 root-cause probe (debug build only, no logic effect): logs every cycle in which a
    // per-way merged writer (ift_store_meta / ift_res_meta) has more than one valid slot, i.e. a
    // write is dropped by the PriorityEncoder, plus each lane-0 store write in s2 and each refill
    // first beat, so a dropped clean-store write can be paired with its refill to the cycle.
    // Remove with the D1 fix (phase4/d1/rootcause.md).
    // [D1FIXA] kept for the fix's regression: sel/widx now name the slot actually WRITTEN (the
    // lane-first one-hot), and hold/psm/prm/prf show the deferral.  Under fix A a multi-valid
    // cycle is no longer a drop: the expected pattern is sel>=1 with the slot-0 clear deferred
    // (psm set on the next cycle).  sel=0 with a lane slot valid would be a lane write lost.
    // R3 (two res lane slots, same way) is the only remaining [RMDROP] drop.
    if (ENABLE_CF_DEBUG_PRINTF) {
      val cf_d1_cyc = RegInit(0.U(40.W))
      cf_d1_cyc := cf_d1_cyc + 1.U
      for (way <- 0 until nWays) {
        val smv = ift_sm_wr_valid(way).asUInt
        when (PopCount(smv) > 1.U) {
          printf("[SMDROP] cyc=%d way=%d wv=0x%x sel=%d widx=%d st_idx=%d st_pc=0x%x st_oc=%d s2type=%d st_sprop=%d st_stx=%d rf_fb=%d rf_addr=0x%x hold=%d psm=0x%x\n",
            cf_d1_cyc, way.U, smv, OHToUInt(ift_sm_oh(way)), Mux1H(ift_sm_oh(way), ift_sm_wr_idx(way)), ift_sm_wr_idx(way)(1),
            s2_req(0).uop.debug_pc, s2_req(0).uop.cf_op_count_id.getOrElse(0.U), s2_type,
            s2_req(0).uop.cf_secret_propagation.getOrElse(false.B), s2_req(0).uop.cf_secret_transmission.getOrElse(false.B),
            cf_refill_first_beat, mshrs.io.refill.bits.addr, cf_pend_hold, cf_psm_way)
        }
        when (PopCount(ift_rm_wv(way)) > 1.U) {
          printf("[RMDROP] cyc=%d way=%d wv=0x%x sel=%d i0=%d i1=%d i2=%d i3=%d hold=%d prm=0x%x prf=0x%x\n", cf_d1_cyc, way.U, ift_rm_wv(way), ift_rm_sel(way),
            ift_rm_wr_idx(way)(0), ift_rm_wr_idx(way)(1), ift_rm_wr_idx(way)(2), ift_rm_wr_idx(way)(3),
            cf_pend_hold, cf_prm_way, cf_prf_way)
        }
      }
      when (s2_valid(0) && isWrite(s2_req(0).uop.mem_cmd) && (s2_type === t_lsu || s2_type === t_replay)) {
        printf("[SMWR] cyc=%d pc=0x%x oc=%d set=%d tmw=0x%x hit=%d nack=%d type=%d\n", cf_d1_cyc, s2_req(0).uop.debug_pc,
          s2_req(0).uop.cf_op_count_id.getOrElse(0.U), dcacheMaskIdx(s2_req(0).addr), s2_tag_match_way(0).asUInt,
          s2_hit(0), s2_nack(0), s2_type)
      }
      when (cf_refill_first_beat) {
        printf("[RFB] cyc=%d addr=0x%x way_en=0x%x\n", cf_d1_cyc, mshrs.io.refill.bits.addr, mshrs.io.refill.bits.way_en)
      }
      // [D1FIXA] the liveness guard fired (a deferred write waited 16 cycles): expected never
      when (cf_pend_starve && !RegNext(cf_pend_starve, false.B)) {
        printf("[D1STARVE] cyc=%d psm=0x%x prm=0x%x prf=0x%x clr_idx=%d prf_idx=%d\n", cf_d1_cyc,
          cf_psm_way, cf_prm_way, cf_prf_way, cf_pclr_idx, cf_prf_idx)
      }
    }

    // [RESID] fill-set slot 1: the MSHR meta write of a secret-address fill (set-only)
    // [ATKRES] step 17: or of an attacker-address fill (the line was cleared at the refill beat,
    // so the write needs no merge)
    // [D1FIXA] or the deferred fill mark (cf_prf_*): exclusive with a new one (the MSHR meta
    // writes are held under cf_pend_hold), so the flop-selected 2:1 muxes pick the source.
    val cf_fill_new = mshrs.io.cf_meta_write_fill.valid &&
                      (mshrs.io.cf_meta_write_fill.bits.addr_sec || mshrs.io.cf_meta_write_fill.bits.addr_atk)
    val cf_fill_new_dat = Cat(mshrs.io.cf_meta_write_fill.bits.addr_sec,
                              mshrs.io.cf_meta_write_fill.bits.addr_atk,
                              mshrs.io.cf_meta_write_fill.bits.res_pc)
    val cf_fill_v = (0 until nWays).map(way =>
                      (cf_fill_new && mshrs.io.cf_meta_write_fill.bits.way_en(way)) || cf_prf_way(way))
    for (way <- 0 until nWays) {
      when (cf_fill_v(way)) {
        ift_rm_wr_valid(way)(1) := true.B
        ift_rm_wr_idx(way)(1)   := cf_fill_src_idx
        ift_rm_wr_data(way)(1)  := Mux(cf_pend_hold, cf_prf_dat, cf_fill_new_dat)
      }
    }
    // [RESID] merged single writer per way.  [D1FIXA] lane slots win (ift_rm_oh), then the clear,
    // then the fill mark; a losing clear / fill mark is deferred below (was: slot 0 won and a
    // lower slot's mark was dropped, D1 R1/R2).
    for (way <- 0 until nWays) {
      when (ift_rm_we(way)) {                                   // [RUWRES] bus defined at the decl
        ift_res_meta(way).write(ift_rm_wa(way), ift_rm_wd(way))
      }
    }

    // [D1FIXA] deferral state.  A slot-0/1 write "loses" when a higher slot of its way is valid;
    // the loser's pending bit is set for the next cycle, and it re-enters its slot every cycle
    // until it wins (no lane slot on its way).  Per way, next = (slot valid) AND (higher slot
    // valid), written lane-factored: lane valid(l) AND (s2_tag_match_way(l)(way) AND slot
    // valid(way)).  The bracket is flops / refill-side terms, so each late lane valid is one LUT
    // from the per-way flop and <= 2 from cf_pend_hold (the exact OR of the same terms): the same
    // depth as the existing ift_sm_fwd_hit D input, ending at a flop, not at the LUTRAM WE.
    val cf_clr_sm_oh = VecInit(cf_clr_sm).asUInt
    val cf_clr_rm_oh = VecInit(cf_clr_rm).asUInt
    val cf_fill_oh   = VecInit(cf_fill_v).asUInt
    val cf_sm_lane_w = (0 until memWidth).map(l => Mux(cf_sm_lane_v(l), s2_tag_match_way(l), 0.U))   // lane l's way, if it writes
    val cf_rm_lane_w = (0 until memWidth).map(l => Mux(cf_rm_lane_v(l), s2_tag_match_way(l), 0.U))
    val cf_sm_lanes  = cf_sm_lane_w.reduce(_ | _)
    val cf_rm_lanes  = cf_rm_lane_w.reduce(_ | _)
    val cf_psm_nxt = cf_clr_sm_oh & cf_sm_lanes
    val cf_prm_nxt = cf_clr_rm_oh & cf_rm_lanes
    val cf_prf_nxt = cf_fill_oh & (cf_rm_lanes | cf_clr_rm_oh)   // the fill mark also yields to the clear
    cf_psm_way := cf_psm_nxt
    cf_prm_way := cf_prm_nxt
    cf_prf_way := cf_prf_nxt
    cf_pend_hold := cf_psm_nxt.orR || cf_prm_nxt.orR || cf_prf_nxt.orR
    when (cf_refill_first_beat) { cf_pclr_idx := dcacheMaskIdx(mshrs.io.refill.bits.addr) }
    // A pending fill mark absorbs any lane mark written to its own line (same way and set) while
    // it waits, so landing it later cannot erase that mark.  The lane's data was built from a read
    // that already forwarded cf_prf_dat (cfResRead), so the OR is idempotent; the PC keeps the
    // lane rule (the secret cause first).  Lane 0 first, as the writer (R3).  The way/set compares
    // are flop-sourced (s2 tag-match way, s2 set, cf_prf_*); the late lane valid only selects the
    // mux into cf_prf_dat (new endpoint: flops only).
    val cf_prf_lane_hit = (0 until memWidth).map(l => cf_rm_lane_v(l) && (s2_tag_match_way(l) & cf_prf_way).orR &&
                                                     dcacheMaskIdx(s2_req(l).addr) === cf_prf_idx)
    def cfResMerge(old: UInt, nw: UInt): UInt =
      Cat(iftResSec(nw) || iftResSec(old), iftResAtk(nw) || iftResAtk(old),
          Mux(iftResSec(nw) || !iftResSec(old), iftResPc(nw), iftResPc(old)))
    when (cf_fill_new) {
      cf_prf_idx := cf_fill_new_idx
      cf_prf_dat := cf_fill_new_dat
    } .otherwise {
      cf_prf_dat := PriorityMux(cf_prf_lane_hit :+ true.B,
                                (0 until memWidth).map(l => cfResMerge(cf_prf_dat, cf_rm_lane_d(l))) :+ cf_prf_dat)
    }
    // Liveness guard: 16 cycles under hold -> block new LSU requests (registered, as wk_block)
    // until nothing is pending; lane writes then stop within 2 cycles and the deferral lands.
    cf_pend_age    := Mux(cf_pend_hold, cf_pend_age + (cf_pend_age =/= 15.U).asUInt, 0.U)
    cf_pend_starve := cf_pend_hold && cf_pend_age >= 14.U
    when (cf_pend_starve) { io.lsu.req.ready := false.B }
    assert(!(cf_pend_hold && (cf_refill_first_beat || mshrs.io.cf_meta_write_fill.valid)),
           "[D1FIXA] a refill beat or MSHR meta write fired while a deferred IFT meta write was pending")

    // corefuzzing: update ift_fill_meta at MSHR fill completion (meta_write), one way at a time.
    when (mshrs.io.cf_meta_write_fill.valid) {
      val fill_idx = mshrs.io.cf_meta_write_fill.bits.idx & dcache_set_mask(idxBits-1, 0)
      // [FILLPROBE 2026-09-09] Settle whether an ATTACKER LOAD tags a line.
      // t11 fires ty=12 when the attacker STORES but not when it LOADS, which the RTL
      // says should not matter: both meta_write.valid sites are STATE-based
      // (s_meta_clear / s_meta_write_req), neither is store-gated, and cf_req_domain is
      // just req.uop.cf_domain_id -- the same field cf_req_secret reads for the secret
      // path, which DOES work on loads.  So either the attacker's load never misses, or
      // the fill happens with the wrong domain.  This prints every fill so we can tell
      // which.  +verbose-gated; costs nothing in a normal run.
      // [WAYPROBE 2026-09-09] way_en added.  A set-conflict sweep can only evict the
      // target if it actually REPLACES every way of that set.  The replacement state is
      // read at S1 (lru/plru_state_mem.read(s1_set_for_repl)) and written at S2 (patch 30:
      // now write-first via [REPLBYP], and the allocating lane owns the write port, [REPLUPD]).
      // If the sweep's fills land on only 1-2 distinct ways, that is
      // why the target survives and why every "attacker loads do not tag" result so far
      // measured a HIT.  Printing the way makes eviction provable instead of assumed.
      // way_en printed as a RAW BITMASK, not OHToUInt: OHToUInt(0) == 0, so an empty
      // way_en (no way written at all) is indistinguishable from "way 0" -- exactly the
      // ambiguity left open by the TrueLRU result (1/8 ways, all reading as way 0).
      if (ENABLE_CF_DEBUG_PRINTF) {
        printf("\n[FILL] idx=%d wayen=0x%x tag=0x%x domain=%d secret=%d oc=%d pc12=0x%x\n",
          fill_idx, mshrs.io.cf_meta_write_fill.bits.way_en,
          mshrs.io.cf_meta_write_fill.bits.tag,
          mshrs.io.cf_meta_write_fill.bits.domain,
          mshrs.io.cf_meta_write_fill.bits.secret,
          mshrs.io.cf_meta_write_fill.bits.op_count,
          mshrs.io.cf_meta_write_fill.bits.pc12)
      }
      // [R1e] fills: atk = ownership of the filling domain only (see ASSUMED BEHAVIOUR).
      // [STEP1b] a refill REPLACES the whole line, so every DW mask bit is set from the
      // fill's own taint via Fill(CF_SUBLINE, bit).  This is the "clean full-line rewrite
      // clears it" case, and it is why the per-DW masks need no decay anywhere else.
      // [METASLIM] the 4th argument (atkMask) is GONE -- it had no reader.
      val entry    = mkIftFill(mshrs.io.cf_meta_write_fill.bits.domain,
                               mshrs.io.cf_meta_write_fill.bits.pc12,          // [PCPROV]
                               Fill(CF_SUBLINE, mshrs.io.cf_meta_write_fill.bits.secret.asUInt))
      for (way <- 0 until nWays) {
        when (mshrs.io.cf_meta_write_fill.bits.way_en(way)) {
          ift_fill_meta(way).write(fill_idx, entry)
        }
      }
    }
  }

  // [reconf-fix 2026-08-06] full_idx_snap fill-update DELETED along with the array
  // itself (see the note near the top of this file): the tag now carries the extra
  // 3 bits, so there is no snapshot to maintain.  Restored 2026-09-05 after an
  // accidental splice from HEAD reintroduced it.

  val uncache_resp = Wire(Valid(new BoomDCacheResp))
  // corefuzzing: stamp dcacheTagCF into MSHR (miss) responses and mark secret_transmission
  uncache_resp.bits                     := mshrs.io.resp.bits
  uncache_resp.bits.uop.cf_fu_bitmap.foreach(_ := mshrs.io.resp.bits.uop.cf_fu_bitmap.getOrElse(0.U(numModules.W)) | (1.U << dcacheTagCF.U))
  // Task 3.2 (D16): a miss/uncached response carries no D$ edge.  Zeroed here (after the bulk
  // copy) so the MSHR resp arbiter and respq copies of the field are unread and pruned.
  // [RESID] step 15 (residue audit rows 1b/3/5): ...except the residency/eviction edge the MSHR
  // attaches to a line-buffer (miss) response.  Built here once, on a zero base (folds).
  val (cf_unc_edges, _) = addInfluencerBatchList(0.U.asTypeOf(new IftEdgeList), mshrs.io.resp.bits.uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)),
    pcCandidates(mshrs.io.resp.bits.cf_res_v.getOrElse(false.B), mshrs.io.resp.bits.cf_res_pc.getOrElse(0.U), INFL_CACHE_EVICTION,
                 mshrs.io.resp.bits.cf_res_atk.getOrElse(false.B),                                     // [ATKEVICT]
                 !mshrs.io.resp.bits.cf_res_atk.getOrElse(false.B) || mshrs.io.resp.bits.cf_res_esec.getOrElse(false.B)))
  uncache_resp.bits.edges               := cf_unc_edges
  uncache_resp.bits.cf_res_v.foreach(_ := false.B)
  uncache_resp.bits.cf_res_pc.foreach(_ := 0.U)
  uncache_resp.bits.cf_res_atk.foreach(_ := false.B)
  uncache_resp.bits.cf_res_esec.foreach(_ := false.B)
  // s_tx is NOT set at MSHR completion: cache miss fill is not a transmission event.
  uncache_resp.valid    := mshrs.io.resp.valid
  mshrs.io.resp.ready := !(cache_resp.map(_.valid).reduce(_&&_)) // We can backpressure the MSHRs, but not cache hits

  val resp = WireInit(cache_resp)
  var uncache_responding = false.B
  for (w <- 0 until memWidth) {
    val uncache_respond = !cache_resp(w).valid && !uncache_responding
    if (ENABLE_IFT) {
      // [RESPSEL] patch 25 row 2d.  The lane-allocation rule (uncache_respond, from the exact
      // cache_resp.valid) is unchanged, so the MSHR response is delivered on the same lane in the
      // same cycle and mshrs.io.resp.ready is unchanged.  The store-only-miss ack leaves the lane
      // for io.lsu.st_ack (below), so a lane now carries a HIT (cache bits) or the uncache
      // response, and the BITS select is s2_resp_hit: register/address sourced, no MSHR ready,
      // no kill.  Old lane = cache_resp(w) if cache_resp(w).valid, else uncache if
      // uncache_respond, else cache_resp(w) (invalid).  New, case by case:
      //   hit lane (s2_resp_hit)          : valid 1, cache bits         -- identical
      //   uncache lane (uncache_respond)  : s2_resp_hit = 0 (it implies cache_resp.valid), so
      //                                     valid = uncache valid, uncache bits -- identical
      //   store-ack lane (valid && !hit)  : valid 0 here; io.lsu.st_ack(w) carries exactly the old
      //                                     io.lsu.resp(w) (same kill gating, same bits)
      //   idle lane                       : valid 0 (bits differ; nothing reads them: every LSU
      //                                     read of io.dmem.resp(w).bits is under its valid)
      resp(w).valid := s2_resp_hit.get(w) || (uncache_respond && uncache_resp.valid)
      resp(w).bits  := Mux(s2_resp_hit.get(w), cache_resp(w).bits, uncache_resp.bits)
    } else {
    when (uncache_respond) {
      resp(w) := uncache_resp
    }
    }
    uncache_responding = uncache_responding || uncache_respond
  }
  // [RESPSEL] store-only-miss acks (the old cache_resp lane content when !s2_hit), same kill
  // gating and br_mask update as io.lsu.resp.  Only lsu/hella stores (isWrite && !isRead) get
  // here, so uses_ldq is 0 (asserted); AMO/SC/LR are reads and stay on the hit/MSHR lane.
  io.lsu.st_ack.foreach { sa =>
    for (w <- 0 until memWidth) {
      val st_lane = cache_resp(w).valid && !s2_hit(w)
      sa(w).valid := st_lane &&
                     !(io.lsu.exception && cache_resp(w).bits.uop.uses_ldq) &&
                     !IsKilledByBranch(io.lsu.brupdate, cache_resp(w).bits.uop)
      sa(w).bits  := UpdateBrMask(io.lsu.brupdate, cache_resp(w).bits)
      assert(!(st_lane && cache_resp(w).bits.uop.uses_ldq), "[RESPSEL] store-only-miss ack for an LDQ uop")
    }
  }

  for (w <- 0 until memWidth) {
    io.lsu.resp(w).valid := resp(w).valid &&
                            !(io.lsu.exception && resp(w).bits.uop.uses_ldq) &&
                            !IsKilledByBranch(io.lsu.brupdate, resp(w).bits.uop)
    io.lsu.resp(w).bits  := UpdateBrMask(io.lsu.brupdate, resp(w).bits)

    io.lsu.nack(w).valid := s2_valid(w) && s2_send_nack(w) &&
                            !(io.lsu.exception && s2_req(w).uop.uses_ldq) &&
                            !IsKilledByBranch(io.lsu.brupdate, s2_req(w).uop)
    io.lsu.nack(w).bits  := UpdateBrMask(io.lsu.brupdate, s2_req(w))
    assert(!(io.lsu.nack(w).valid && s2_type =/= t_lsu))
  }

  // Store/amo hits
  val s3_req   = RegNext(s2_req(0))
  val s3_valid = RegNext(s2_valid(0) && s2_hit(0) && isWrite(s2_req(0).uop.mem_cmd) &&
                         !s2_sc_fail && !(s2_send_nack(0) && s2_nack(0)))
  for (w <- 1 until memWidth) {
    assert(!(s2_valid(w) && s2_hit(w) && isWrite(s2_req(w).uop.mem_cmd) &&
                         !s2_sc_fail && !(s2_send_nack(w) && s2_nack(w))),
      "Store must go through 0th pipe in L1D")
  }

  // For bypassing
  val s4_req   = RegNext(s3_req)
  val s4_valid = RegNext(s3_valid)
  val s5_req   = RegNext(s4_req)
  val s5_valid = RegNext(s4_valid)

  val s3_bypass = widthMap(w => s3_valid && ((s2_req(w).addr >> wordOffBits) === (s3_req.addr >> wordOffBits)))
  val s4_bypass = widthMap(w => s4_valid && ((s2_req(w).addr >> wordOffBits) === (s4_req.addr >> wordOffBits)))
  val s5_bypass = widthMap(w => s5_valid && ((s2_req(w).addr >> wordOffBits) === (s5_req.addr >> wordOffBits)))

  // Store -> Load bypassing
  for (w <- 0 until memWidth) {
    s2_data_word(w) := Mux(s3_bypass(w), s3_req.data,
                       Mux(s4_bypass(w), s4_req.data,
                       Mux(s5_bypass(w), s5_req.data,
                                         s2_data_word_prebypass(w))))
  }
  val amoalu   = Module(new AMOALU(xLen))
  amoalu.io.mask := new StoreGen(s2_req(0).uop.mem_size, s2_req(0).addr, 0.U, xLen/8).mask
  amoalu.io.cmd  := s2_req(0).uop.mem_cmd
  amoalu.io.lhs  := s2_data_word(0)
  amoalu.io.rhs  := s2_req(0).data


  s3_req.data := amoalu.io.out
  val s3_way   = RegNext(s2_tag_match_way(0))

  dataWriteArb.io.in(0).valid       := s3_valid
  dataWriteArb.io.in(0).bits.addr   := dcacheMaskAddr(s3_req.addr)
  dataWriteArb.io.in(0).bits.wmask  := UIntToOH(s3_req.addr.extract(rowOffBits-1,offsetlsb))
  dataWriteArb.io.in(0).bits.data   := Fill(rowWords, s3_req.data)
  dataWriteArb.io.in(0).bits.way_en := s3_way


  // ==== [RECONSTRUCTED 2026-09-05] ====================================================
  // FULL INCIDENT + EVERY ASSUMPTION: claude-artifacts/40-dcache-delete-incident-20260905.md
  // ASSUMPTIONS HERE (may bite):
  //  (a) dcache_idle's term list is transcribed from elaborated Verilog; the Scala
  //      spelling `!s1_valid.reduce(_||_)` is INFERRED.  Re-check if memWidth changes.
  //  (b) This block's PLACEMENT is chosen, not recovered.  Chisel is order-insensitive
  //      for these connections and the canonical Verilog diff is 0, but the original
  //      sat at source lines 1506-1521, not necessarily exactly here.
  //  (c) geom_changed may originally have been one RegNext of a tuple; same hardware.
  // This block was destroyed by an over-greedy probe strip and was NOT in git (HEAD is
  // 242 lines behind this working tree).  Recovered by transcription from the elaborated
  // pre-damage Verilog, which preserves both the logic and the source line numbers:
  //   dcache_idle      @[dcache.scala:1506:43, :1507:39, :1508:43, :1509:{23,41,46,49,67}]
  //   cfg_*_applied    @[dcache.scala:1512:23, :1513:23]
  //   geom_changed     @[dcache.scala:1519:52, :1520:52]
  //   assert           @[dcache.scala:1521:11]  (assertion text is verbatim from the Verilog)
  // Reference kept at /data/akrish/BACKUP-20260905-dcache-recovery/refverilog/.
  // Acceptance test: normalised Verilog diff vs that reference must be ZERO.

  // ordered must also go low while a geometry change is staged or a wipe is in flight,
  // otherwise a fence could retire across a reconfiguration.
  io.lsu.cf_asec_busy := mshrs.io.cf_asec_busy   // [NACKRES] step 16
  // [NACKATK] patch 44: the attacker busy summary and the per-lane MSHR/WB nack cause.  s2_nack is
  // miss (MSHR full / same set / no free way) | victim (hit on a line an MSHR is evicting) | wb
  // (same-set writeback) | hit (probe) | data (bank conflict); read only with a nack, "neither
  // probe nor bank" = one of the three MSHR/WB causes.  Both terms are registers (s2_nack_hit =
  // RegNext(s1_nack), data.io.nacks = RegNext(s1_nacks)), so this stays off the deep
  // MSHR-ready -> s2_nack path: 0 added levels.  A nack with an MSHR cause AND a probe/bank cause
  // in the same cycle records nothing (under-taint only).
  io.lsu.cf_aatk_busy.foreach(_ := mshrs.io.cf_aatk_busy.get)
  io.lsu.cf_nack_aatk.foreach(_ := mshrs.io.cf_nack_aatk.get)   // [NACKCONF] patch 68 (V12)
  io.lsu.cf_nack_mshr.foreach(v => for (w <- 0 until memWidth) {
    v(w) := !s2_nack_hit(w) && !s2_nack_data(w)
  })
  // [QSREG R1] (Phase 4 timing, r142) `ordered` uses the REGISTERED wipe_active, not
  // dcache_wipe_busy = wipe_active || wipe_start.  wipe_start is combinational from the CSR
  // write strobe (csr_exe_unit iresp -> CSRFile wen), and that term put the CSR execute uop at
  // the head of the worst routed path (CSR -> ordered -> LSU fire -> DTLB -> no_pending_mem ->
  // quiesce -> ROB reuse_cnt / BTB; 48 levels).  Equivalence: wipe_start can only be high in
  // the writeback cycle of a CSR instruction targeting 0xbc4.  Every CSR op decodes
  // is_unique=Y, flush_on_commit=Y (decode.scala:230-236), so (a) at its dispatch the ROB was
  // empty and fencei_rdy held, i.e. the STQ was empty; (b) in its writeback cycle it is still
  // in the ROB (rob.io.empty = 0) and every STQ entry is younger and uncommitted.  The
  // consumers of `ordered` in that cycle are therefore all masked: lsu clear_store /
  // force_order act only on a COMMITTED fence at the STQ head (none exists); fencei_rdy feeds
  // wait_for_empty_pipeline and pipeline_drained_strict, both ANDed/ORed with rob.io.empty
  // (core.scala wait_for_empty_pipeline, pipeline_drained_strict).  From the next cycle on
  // wipe_active is high, so both forms are equal on every cycle a consumer can observe.  A
  // fence younger than the csrw is refetched after the flush, long after wipe_active rose.
  // Levels: -8 on everything downstream of `ordered`; LUT ~-1.
  io.lsu.ordered := mshrs.io.fence_rdy && !s1_valid.reduce(_||_) && !s2_valid.reduce(_||_) &&
                    !cfg_change && !dcache_wipe_active

  // The cache is idle when no MSHR is outstanding, no writeback/probe is pending, and
  // nothing is in flight in s1/s2.  A staged geometry change is applied ONLY here.
  val dcache_idle = mshrs.io.cache_idle && !cf_pend_hold &&   // [D1FIXA] no deferred IFT write in flight
                    wb.io.req.ready &&
                    prober.io.req.ready &&
                    !s1_valid.reduce(_||_) && !s2_valid.reduce(_||_)

  // ==== [reconf-fix 2.10f F3] writeback-and-invalidate walk on every staged set/way change ====
  // Owner decision (ruling on Task 2.10f): cleanliness is a hardware property, not a software
  // sweep. On a staged 0xbc1 set/way change: hold off new D$ requests, drain, hold off probes,
  // then for every row of the OLD (still applied) geometry: read its metadata, write back every
  // Dirty way through the normal WB unit (voluntary Release, TtoN), and invalidate the row.
  // Only then apply. Invariant (design note section 1): no valid line ever sits outside the
  // active geometry, so after the apply the array is empty and nothing can be stranded.
  // Shares metaWriteArb port 2 with the 0xbc4 discard walker (the two never overlap; asserted).
  // Timing: no logic on s0-s2. LSU ready, prefetch and TL-B get one registered AND term each.
  // Design: claude-artifacts/ift-overhaul/execution/task-reports/task-2.10f-fix-design.md.
  val cf_dcache_reconf_status = if (ENABLE_RECONF) {
    val sIdle :: sDrain :: sQuiesce :: sRead :: sResp :: sSel :: sRead2 :: sTag :: sWb :: sInval :: sDone :: Nil = Enum(11)
    val wk_state  = RegInit(sIdle)
    val wk_block  = RegInit(false.B)               // LSU + prefetch requests held off: sDrain..sDone
    val wk_pblock = RegInit(false.B)               // TileLink B (probes) held off:    sQuiesce..sDone
    val wk_row    = Reg(UInt(idxBits.W))           // physical row under the OLD mask
    val wk_dirty  = Reg(UInt(nWays.W))             // Dirty ways of wk_row still to write back
    val wk_way    = Reg(UInt(log2Ceil(nWays).W))   // way being written back
    val wk_tag    = Reg(UInt(cfTagBits.W))         // its extended tag (22 bits in this config)

    val wk_resp     = meta(0).io.resp              // valid the cycle after our read fires
    val wk_rd       = wk_state === sRead || wk_state === sRead2
    val wk_rd_fire  = wk_rd && metaReadArb.io.in(3).fire
    // Registered term only. A 0xbc4 wipe and a walk never overlap: CSR writes are unique and
    // wait for fencei_rdy, and ordered is low during both (asserted below). Gating on
    // dcache_wipe_busy would route the CSR write strobe (wipe_start) into the meta write
    // enable and io.lsu.req.ready.
    val wk_inv      = wk_state === sInval
    val wk_inv_fire = wk_inv && metaWriteArb.io.in(2).fire
    val wk_wb       = wk_state === sWb
    val wk_last_row = wk_row === dcache_set_mask(idxBits-1, 0)
    // Elaborates to the constant TtoN; spelled like the MSHR's shrink_param (mshrs.scala:137) on purpose.
    val (_, wk_param, _) = ClientMetadata(ClientStates.Dirty).onCacheControl(M_FLUSH)

    switch (wk_state) {
      is (sIdle)    { when (cfg_change)  { wk_state := sDrain;   wk_block  := true.B } }
      is (sDrain)   { when (dcache_idle) { wk_state := sQuiesce; wk_pblock := true.B } }
      is (sQuiesce) { when (dcache_idle) { wk_state := sRead;    wk_row    := 0.U } }
      is (sRead)    { when (wk_rd_fire)  { wk_state := sResp } }
      is (sResp)    {
        wk_dirty := VecInit(wk_resp.map(_.coh.state === ClientStates.Dirty)).asUInt
        wk_state := sSel
      }
      is (sSel)     {
        when (wk_dirty === 0.U) { wk_state := sInval }
        .otherwise {
          val pe = PriorityEncoder(wk_dirty)
          wk_way   := pe
          wk_dirty := wk_dirty & ~UIntToOH(pe, nWays)
          wk_state := sRead2
        }
      }
      is (sRead2)   { when (wk_rd_fire) { wk_state := sTag } }
      is (sTag)     {
        // The re-check is a cheap guard. The dispatch rule already excludes a concurrent wipe.
        wk_tag   := wk_resp(wk_way).tag
        wk_state := Mux(wk_resp(wk_way).coh.state === ClientStates.Dirty, sWb, sSel)
      }
      is (sWb)      { when (wbArb.io.in(1).fire) { wk_state := sSel } }
      is (sInval)   {
        when (wk_inv_fire) { wk_row := wk_row + 1.U; wk_state := Mux(wk_last_row, sDone, sRead) }
      }
      is (sDone)    {
        when (dcache_idle) {                       // WB idle = last ReleaseAck received
          cfg_set_applied := io.lsu.cf_dcache_set_conf
          cfg_way_applied := io.lsu.cf_dcache_way_conf
          wk_state := sIdle; wk_block := false.B; wk_pblock := false.B
        }
      }
    }

    // Meta read: MSHR port 3. The MSHRs are idle for the whole walk (asserted below).
    when (wk_rd) {
      metaReadArb.io.in(3).valid           := true.B
      metaReadArb.io.in(3).bits.req(0).idx := wk_row
      mshrs.io.meta_read.ready             := false.B
    }
    // Row invalidate: port 2. way_en/tag/coh are already the wipe's constants (0xbc4 block above).
    when (wk_inv) {
      metaWriteArb.io.in(2).valid    := true.B
      metaWriteArb.io.in(2).bits.idx := wk_row
    }
    // Writeback: the MSHR eviction port of wbArb. Full index per Theorem 2 (spec 31), the same
    // reconstruction as s2_repl_full_idx; the WB data read masks it back to wk_row.
    when (wk_wb) {
      wbArb.io.in(1).valid          := true.B
      wbArb.io.in(1).bits.tag       := wk_tag
      wbArb.io.in(1).bits.idx       := Cat(wk_tag(idxBits-cfIdxLowBits-1, 0), wk_row(cfIdxLowBits-1, 0))
      wbArb.io.in(1).bits.param     := wk_param
      wbArb.io.in(1).bits.way_en    := UIntToOH(wk_way, nWays)
      wbArb.io.in(1).bits.source    := 0.U     // unused: the WB unit always sends with id = nMSHRs
      wbArb.io.in(1).bits.voluntary := true.B  // WB waits for ReleaseAck (s_grant) before going idle
      mshrs.io.wb_req.ready         := false.B
    }
    // Hold-offs. Last connect over the LSU req ready, prefetch, prober/TL-B and ordered drivers.
    when (wk_block) {
      io.lsu.req.ready              := false.B
      metaReadArb.io.in(5).valid    := false.B
      mshrs.io.prefetch.ready       := false.B
    }
    when (wk_pblock) {
      prober.io.req.valid := false.B
      tl_out.b.ready      := false.B
    }
    when (wk_state =/= sIdle) { io.lsu.ordered := false.B }

    // Simulation checks (no synthesized fan-out: assert-only logic is trimmed on FPGA).
    assert(!(wk_pblock && mshrs.io.req.map(_.fire).reduce(_||_)), "D$ walk: MSHR allocated during walk")
    assert(!(wk_pblock && s3_valid),                              "D$ walk: store hit during walk")
    // prober.io.req.valid is forced low under wk_pblock, so check the prober state instead:
    // the walk must never read or invalidate while an earlier probe is still in flight.
    assert(!((wk_rd || wk_state === sInval) && !prober.io.req.ready), "D$ walk: probe in flight during walk")
    assert(!(wk_state =/= sIdle && dcache_wipe_busy), "D$ walk overlaps a 0xbc4 wipe")
    assert(!(wk_state === sResp &&
             (VecInit(wk_resp.map(_.coh.isValid())).asUInt & ~s2_active_mask).orR),
           "D$ invariant: valid line outside the active ways")
    for (i <- 0 until nWays; j <- i + 1 until nWays)
      assert(!(wk_state === sResp && wk_resp(i).coh.isValid() && wk_resp(j).coh.isValid() &&
               wk_resp(i).tag === wk_resp(j).tag), "D$ invariant: two valid copies of one block in a row")
    val wk_cycles = RegInit(0.U(18.W))
    wk_cycles := Mux(wk_state === sIdle, 0.U, wk_cycles + 1.U)
    assert(!wk_cycles(17), "D$ reconfiguration walk exceeded 131072 cycles")

    // 0xbcd[16:12]: pending, applied way idx, applied set idx (doc 30 T4 layout; r33 reads it).
    Cat(cfg_change || wk_state =/= sIdle, cfg_way_applied, cfg_set_applied)
  } else {
    when (cfg_change && dcache_idle) {
      cfg_set_applied := io.lsu.cf_dcache_set_conf
      cfg_way_applied := io.lsu.cf_dcache_way_conf
    }
    0.U(5.W)
  }
  io.lsu.cf_dcache_reconf_status.foreach(_ := cf_dcache_reconf_status)

  // [reconf-fix 2.10f] With F3 a block can never have two valid copies (design section 1). Check it.
  if (ENABLE_RECONF) {
    for (w <- 0 until memWidth)
      assert(!(s2_valid(w) && s2_type === t_lsu && PopCount(s2_tag_match_way(w)(nWays-1, 0)) > 1.U),
             "D$: two valid copies of one block")
  }

  val geom_changed = (cfg_set_applied =/= RegNext(cfg_set_applied)) ||
                     (cfg_way_applied =/= RegNext(cfg_way_applied))
  assert(!(geom_changed && !RegNext(dcache_idle)),
    "D$ geometry changed while the cache was not idle")
  // ==== end reconstructed block =======================================================

  override def toString: String = BoomCoreStringPrefix(
    "==L1-DCache==",
    "Mem Width     : " +  memWidth,
    "EncRowBits    : " +  encRowBits,
    "Row bytes     : " +  rowBytes,
    "Block Bytes   : " +  dcacheParams.blockBytes,
    "Word bits     : " +  wordBits,
    "Sets          : " +  nSets,
    "Ways          : " +  nWays,
    "Refill cycles : " +  refillCycles,
    "RAMs          : (" +  wordBits/boomParams.numDCacheBanks + " x " + nSets*refillCycles + ") using " + boomParams.numDCacheBanks + " banks" +
    "" + (if (boomParams.numDCacheBanks == 2) "Dual-banked" else "Single-banked"),
    "D-TLB ways    : " + dcacheParams.nTLBWays + "\n")
}

    // "Fetch bytes   : " +  cacheParams.fetchBytes,
    // "Block bytes   : " +  (1 << blockOffBits),
    // "RamDepth      : " +  ramDepth,