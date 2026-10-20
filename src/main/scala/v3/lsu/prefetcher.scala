//******************************************************************************
// See LICENSE.Berkeley for license details.
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
import boom.v3.exu.BrResolutionInfo
import boom.v3.util.{IsKilledByBranch, GetNewBrMask, BranchKillableQueue, IsOlder, UpdateBrMask}



abstract class DataPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
  with CoreFuzzingConstants   // [PFTAINT] inflPcBitsCF
{
  val io = IO(new Bundle {
    val mshr_avail = Input(Bool())
    val req_val    = Input(Bool())
    val req_addr   = Input(UInt(coreMaxAddrBits.W))
    val req_coh    = Input(new ClientMetadata)

    val prefetch   = Decoupled(new BoomDCacheReq)
    // [PFTAINT] step 17 (owner answer Q7: prefetch in scope): the triggering miss's domain,
    // secret-address line mark and PC[13:2], registered with req_addr in the MSHR file.
    val req_dom    = Input(UInt(1.W))
    val req_asec   = Input(Bool())
    val req_aatk   = Input(Bool())   // [ATKRES] the triggering line's attacker-address mark
    val req_pc12   = Input(UInt(inflPcBitsCF.W))
  })
}

/**
  * Does not prefetch
  */
class NullPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends DataPrefetcher
{
  io.prefetch.valid := false.B
  io.prefetch.bits  := DontCare
}

/**
  * Next line prefetcher. Grabs the next line on a cache miss
  */
class NLPrefetcher(implicit edge: TLEdgeOut, p: Parameters) extends DataPrefetcher
{
  val req_valid = RegInit(false.B)
  val req_addr  = Reg(UInt(coreMaxAddrBits.W))
  val req_cmd   = Reg(UInt(M_SZ.W))
  // [R124GATE] patch 26 (ruling 124): the [PFTAINT] flops exist only with ENABLE_IFT; IFT off
  // the prefetch request is the plain NullMicroOp one (as before step 17).
  val req_dom   = if (ENABLE_IFT) Reg(UInt(1.W)) else WireInit(0.U(1.W))   // [PFTAINT]
  val req_asec  = if (ENABLE_IFT) Reg(Bool()) else WireInit(false.B)
  val req_aatk  = if (ENABLE_IFT) Reg(Bool()) else WireInit(false.B)
  val req_pc12  = if (ENABLE_IFT) Reg(UInt(inflPcBitsCF.W)) else WireInit(0.U(inflPcBitsCF.W))

  val mshr_req_addr = io.req_addr + cacheBlockBytes.U
  val cacheable = edge.manager.supportsAcquireBSafe(mshr_req_addr, lgCacheBlockBytes.U)
  when (io.req_val && cacheable) {
    req_valid := true.B
    req_addr  := mshr_req_addr
    req_cmd   := Mux(ClientStates.hasWritePermission(io.req_coh.state), M_PFW, M_PFR)
    if (ENABLE_IFT) {   // [R124GATE]
      req_dom   := io.req_dom    // [PFTAINT]
      req_asec  := io.req_asec
      req_aatk  := io.req_aatk
      req_pc12  := io.req_pc12
    }
  } .elsewhen (io.prefetch.fire) {
    req_valid := false.B
  }

  io.prefetch.valid            := req_valid && io.mshr_avail
  io.prefetch.bits             := DontCare
  io.prefetch.bits.addr        := req_addr
  io.prefetch.bits.uop         := NullMicroOp
  io.prefetch.bits.uop.mem_cmd := req_cmd
  io.prefetch.bits.data        := DontCare
  // [PFTAINT] step 17 (owner answer Q7, reverses the step-15 "out of scope"): the prefetch
  // carries the triggering miss's domain, PC and secret-address mark, so the prefetched line is
  // marked like the triggering line through the ordinary paths: fill_meta domain/PC (MSHR
  // cf_req_domain/pc12 read req.uop), ift_res_meta (req.cf_addr_sec at the meta write), and the
  // eviction / replacement-state taint of its allocation.  Its readers then get the same edges
  // and value taint.  op_count stays 0 (no uop of its own).
  if (ENABLE_IFT) {   // [R124GATE]
    io.prefetch.bits.uop.cf_domain_id.foreach(_ := req_dom)
    io.prefetch.bits.uop.debug_pc     := Cat(req_pc12, 0.U(2.W))
  }
  io.prefetch.bits.cf_addr_sec.foreach(_ := req_asec)
  io.prefetch.bits.cf_addr_atk.foreach(_ := req_aatk)   // [ATKRES]
  io.prefetch.bits.cf_res_v.foreach(_ := false.B)
  io.prefetch.bits.cf_res_atk.foreach(_ := false.B)   // [ATKEVICT]
  io.prefetch.bits.cf_res_esec.foreach(_ := false.B)
  io.prefetch.bits.cf_res_pc.foreach(_ := 0.U)
}
