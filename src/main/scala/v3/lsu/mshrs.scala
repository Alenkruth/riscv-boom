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
import freechips.rocketchip.util.CoreFuzzingConstants
import boom.v3.util.{IsKilledByBranch, GetNewBrMask, BranchKillableQueue, IsOlder, UpdateBrMask, AgePriorityEncoder, WrapInc}

class BoomDCacheReqInternal(implicit p: Parameters) extends BoomDCacheReq()(p)
  with HasL1HellaCacheParameters
{
  // miss info
  val tag_match = Bool()
  val old_meta  = new BoomL1Metadata   // [reconf-fix] extended tag
  // Full (unmasked) set index addr[12:6] of the evicted block, from full_idx_snap.
  // Used as wb_req.bits.idx so the TL Release targets the evicted block's physical address
  // rather than the miss request's (aliased) address.
  val old_idx   = UInt(idxBits.W)
  val way_en    = UInt(nWays.W)

  // Used in the MSHRs
  val sdq_id    = UInt(log2Ceil(cfg.nSDQ).W)
}


class BoomMSHR(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
  with HasL1HellaCacheParameters
  with CoreFuzzingConstants
  with HasBoomExtendedTag   // [reconf-fix] cfTagLSB / cfIdxLowBits
{
  val io = IO(new Bundle {
    val id = Input(UInt())

    val req_pri_val = Input(Bool())
    val req_pri_rdy = Output(Bool())
    val req_sec_val = Input(Bool())
    val req_sec_rdy = Output(Bool())

    val clear_prefetch = Input(Bool())
    val brupdate       = Input(new BrUpdateInfo)
    val exception    = Input(Bool())
    val rob_pnr_idx  = Input(UInt(robAddrSz.W))
    val rob_head_idx = Input(UInt(robAddrSz.W))

    val req          = Input(new BoomDCacheReqInternal)
    val req_is_probe = Input(Bool())

    val idx = Output(Valid(UInt()))
    val way = Output(Valid(UInt()))
    val tag = Output(Valid(UInt()))


    val mem_acquire = Decoupled(new TLBundleA(edge.bundle))

    val mem_grant   = Flipped(Decoupled(new TLBundleD(edge.bundle)))
    val mem_finish  = Decoupled(new TLBundleE(edge.bundle))

    val prober_state = Input(Valid(UInt(coreMaxAddrBits.W)))
    // [reconf-fix 2026-08-12] active set mask, for PHYSICAL-row comparisons
    val dcache_set_mask = Input(UInt(idxBits.W))

    val refill      = Decoupled(new L1DataWriteReq)

    val meta_write  = Decoupled(new BoomL1MetaWriteReq)
    val meta_read   = Decoupled(new BoomL1MetaRdReq)
    val meta_resp   = Input(Valid(new BoomL1Metadata))   // [reconf-fix]
    val wb_req      = Decoupled(new BoomWritebackReq(edge.bundle))

    // To inform the prefetcher when we are commiting the fetch of this line
    val commit_val  = Output(Bool())
    val commit_addr = Output(UInt(coreMaxAddrBits.W))
    val commit_coh  = Output(new ClientMetadata)

    // Reading from the line buffer
    val lb_read       = Decoupled(new LineBufferReadReq)
    val lb_resp       = Input(UInt(encRowBits.W))
    val lb_write      = Decoupled(new LineBufferWriteReq)

    // Replays go through the cache pipeline again
    val replay      = Decoupled(new BoomDCacheReqInternal)
    // Resp go straight out to the core
    val resp        = Decoupled(new BoomDCacheResp)

    // Writeback unit tells us when it is done processing our wb
    val wb_resp     = Input(Bool())

    val probe_rdy   = Output(Bool())
    // [reconf-fix 2026-08-06] Strict idle indication for the D$ config-commit gate.
    // NOT the same as fence_rdy: a fence tolerates a parked s_prefetch MSHR, but a
    // geometry remap tolerates nothing (a parked MSHR holds a way/idx binding taken
    // under the OLD mapping).  ENABLE_RECONF-only consumer; constant-folded away when
    // the gate is not built.
    val state_invalid = Output(Bool())
    // corefuzzing: domain of the stored request — valid while MSHR is active, used at meta_write time
    val cf_req_domain   = Output(UInt(1.W))
    val cf_req_op_count = Output(UInt(uopIDCounterWidthCF.W))
    val cf_req_secret   = Output(Bool())
  })

  // TODO: Optimize this. We don't want to mess with cache during speculation
  // s_refill_req      : Make a request for a new cache line
  // s_refill_resp     : Store the refill response into our buffer
  // s_drain_rpq_loads : Drain out loads from the rpq
  //                   : If miss was misspeculated, go to s_invalid
  // s_wb_req          : Write back the evicted cache line
  // s_wb_resp         : Finish writing back the evicted cache line
  // s_meta_write_req  : Write the metadata for new cache lne
  // s_meta_write_resp :

  val s_invalid :: s_refill_req :: s_refill_resp :: s_drain_rpq_loads :: s_meta_read :: s_meta_resp_1 :: s_meta_resp_2 :: s_meta_clear :: s_wb_meta_read :: s_wb_req :: s_wb_resp :: s_commit_line :: s_drain_rpq :: s_meta_write_req :: s_mem_finish_1 :: s_mem_finish_2 :: s_prefetched :: s_prefetch :: Nil = Enum(18)
  val state = RegInit(s_invalid)

  val req     = Reg(new BoomDCacheReqInternal)
  val req_idx = req.addr(untagBits-1, blockOffBits)
  // [reconf-fix] extended tag: covers every bit above the SMALLEST index (spec 31).
  val req_tag = req.addr >> cfTagLSB
  val req_block_addr = (req.addr >> blockOffBits) << blockOffBits
  val req_needs_wb = RegInit(false.B)

  val new_coh = RegInit(ClientMetadata.onReset)
  val (_, shrink_param, coh_on_clear) = req.old_meta.coh.onCacheControl(M_FLUSH)
  val grow_param = new_coh.onAccess(req.uop.mem_cmd)._2
  val coh_on_grant = new_coh.onGrant(req.uop.mem_cmd, io.mem_grant.bits.param)

  // We only accept secondary misses if the original request had sufficient permissions
  val (cmd_requires_second_acquire, is_hit_again, _, dirtier_coh, dirtier_cmd) =
    new_coh.onSecondaryAccess(req.uop.mem_cmd, io.req.uop.mem_cmd)

  val (_, _, refill_done, refill_address_inc) = edge.addr_inc(io.mem_grant)
  val sec_rdy = (!cmd_requires_second_acquire && !io.req_is_probe &&
                 !state.isOneOf(s_invalid, s_meta_write_req, s_mem_finish_1, s_mem_finish_2))// Always accept secondary misses

  val rpq = Module(new BranchKillableQueue(new BoomDCacheReqInternal, cfg.nRPQ, u => u.uses_ldq, false))
  rpq.io.brupdate := io.brupdate
  rpq.io.flush  := io.exception
  assert(!(state === s_invalid && !rpq.io.empty))

  rpq.io.enq.valid := ((io.req_pri_val && io.req_pri_rdy) || (io.req_sec_val && io.req_sec_rdy)) && !isPrefetch(io.req.uop.mem_cmd)
  rpq.io.enq.bits  := io.req
  // IFT LUT optimization (Change 4): zero cf_* fields not needed by MSHR/rpq logic.
  // rpq reads mem_cmd, mem_size, mem_signed; passes uop through io.resp.bits.uop
  // back to wb_resps (same preservation list as FU pipeline Change 2).
  //
  // [A4 2026-09-10] VERIFIED SAFE -- do NOT "fix" this by preserving the fields.
  // The concern was that this is the MEMIDFIX defect on the response path: a load that
  // MISSES is replayed out of the rpq, so its response uop would carry domain=0/opcount=0
  // and core.scala:1997
  //     iregfile.write_ports.bits.taint_atk := wbresp.bits.taint_atk ||
  //                                            (wbresp.bits.uop.cf_domain_id =/= 0.U)
  // would then clear attacker taint for a MISSING attacker load while a HITTING one kept
  // it -- which would gut Prime+Probe coverage, since priming misses by construction.
  //
  // It does not happen, because the LSU never forwards this uop.  lsu.scala:2090-2114 builds
  // the load response as `merged_base_cf = WireInit(ldq_base_cf)` -- the LDQ's CANONICAL uop,
  // which holds the true identity -- and then OR-merges only an explicit list from the
  // response uop (cf_fu_bitmap, cf_secret_access, cf_secret_transmission,
  // cf_mem_dataflow_atk, cf_mem_sec_dataflow, plus influencer slots).  cf_domain_id and
  // cf_op_count_id are deliberately NOT in that list, so they keep the LDQ's values and the
  // zeroing below is invisible downstream.  ldq_idx itself is not a cf_* field and survives,
  // which is what makes the LDQ lookup work at all.
  //
  // The uncached path (:576, `io.resp.bits.uop := req.uop`) never passes through the rpq and
  // is unaffected.  If lsu.scala:2092 ever stops basing the merge on the LDQ entry, this
  // zeroing becomes a live attacker-taint bug -- that line is the load-bearing one.
  rpq.io.enq.bits.uop.cf_speculated               := false.B
  rpq.io.enq.bits.uop.cf_op_count_id              := 0.U
  rpq.io.enq.bits.uop.cf_single_step              := false.B
  rpq.io.enq.bits.uop.cf_src_tainted              := false.B
  rpq.io.enq.bits.uop.cf_spec_branch_is_atk       := false.B
  rpq.io.enq.bits.uop.cf_atk_branch_ctr       := 0.U
  rpq.io.enq.bits.uop.cf_sec_branch_ctr       := 0.U
  rpq.io.enq.bits.uop.cf_spec_branch_op_id        := 0.U
  rpq.io.enq.bits.uop.cf_spec_branch_is_secret    := false.B
  rpq.io.enq.bits.uop.cf_cntd_valid               := false.B
  rpq.io.enq.bits.uop.cf_cntd_winner_op           := 0.U
  rpq.io.enq.bits.uop.cf_cntd_winner_atk          := false.B
  rpq.io.enq.bits.uop.cf_cntd_winner_sec          := false.B
  rpq.io.enq.bits.uop.cf_cntd_deny_count          := 0.U
  rpq.io.enq.bits.uop.cf_domain_id                := 0.U
  rpq.io.deq.ready := false.B


  val grantack = Reg(Valid(new TLBundleE(edge.bundle)))
  val refill_ctr  = Reg(UInt(log2Ceil(cacheDataBeats).W))
  val commit_line = Reg(Bool())
  val grant_had_data = Reg(Bool())
  val finish_to_prefetch = Reg(Bool())

  // Block probes if a tag write we started is still in the pipeline
  val meta_hazard = RegInit(0.U(2.W))
  when (meta_hazard =/= 0.U) { meta_hazard := meta_hazard + 1.U }
  when (io.meta_write.fire) { meta_hazard := 1.U }
  io.probe_rdy   := (meta_hazard === 0.U && (state.isOneOf(s_invalid, s_refill_req, s_refill_resp, s_drain_rpq_loads) || (state === s_meta_read && grantack.valid)))
  // corefuzzing: expose stored request domain for fill-completion tracking in dcache
  io.cf_req_domain   := req.uop.cf_domain_id
  io.cf_req_op_count := req.uop.cf_op_count_id
  io.cf_req_secret   := req.uop.cf_secret_propagation || req.uop.cf_secret_access
  io.idx.valid := state =/= s_invalid
  io.tag.valid := state =/= s_invalid
  io.way.valid := !state.isOneOf(s_invalid, s_prefetch)
  io.idx.bits := req_idx
  io.tag.bits := req_tag
  io.way.bits := req.way_en

  io.meta_write.valid    := false.B
  io.meta_write.bits     := DontCare
  io.req_pri_rdy         := false.B
  io.req_sec_rdy         := sec_rdy && rpq.io.enq.ready
  io.mem_acquire.valid   := false.B
  io.mem_acquire.bits    := DontCare
  io.refill.valid        := false.B
  io.refill.bits         := DontCare
  io.replay.valid        := false.B
  io.replay.bits         := DontCare
  io.wb_req.valid        := false.B
  io.wb_req.bits         := DontCare
  io.resp.valid          := false.B
  io.resp.bits           := DontCare
  io.commit_val          := false.B
  io.commit_addr         := req.addr
  io.commit_coh          := coh_on_grant
  io.meta_read.valid     := false.B
  io.meta_read.bits      := DontCare
  io.mem_finish.valid    := false.B
  io.mem_finish.bits     := DontCare
  io.lb_write.valid      := false.B
  io.lb_write.bits       := DontCare
  io.lb_read.valid       := false.B
  io.lb_read.bits        := DontCare
  io.mem_grant.ready     := false.B

  when (io.req_sec_val && io.req_sec_rdy) {
    req.uop.mem_cmd := dirtier_cmd
    when (is_hit_again) {
      new_coh := dirtier_coh
    }
  }

  def handle_pri_req(old_state: UInt): UInt = {
    val new_state = WireInit(old_state)
    grantack.valid := false.B
    refill_ctr := 0.U
    assert(rpq.io.enq.ready)
    req := io.req
    val old_coh   = io.req.old_meta.coh
    req_needs_wb := old_coh.onCacheControl(M_FLUSH)._1 // does the line we are evicting need to be written back
    when (io.req.tag_match) {
      val (is_hit, _, coh_on_hit) = old_coh.onAccess(io.req.uop.mem_cmd)
      when (is_hit) { // set dirty bit
        assert(isWrite(io.req.uop.mem_cmd))
        new_coh     := coh_on_hit
        new_state   := s_drain_rpq
      } .otherwise { // upgrade permissions
        new_coh     := old_coh
        new_state   := s_refill_req
      }
    } .otherwise { // refill and writeback if necessary
      new_coh     := ClientMetadata.onReset
      new_state   := s_refill_req
    }
    new_state
  }

  when (state === s_invalid) {
    io.req_pri_rdy := true.B
    grant_had_data := false.B

    when (io.req_pri_val && io.req_pri_rdy) {
      state := handle_pri_req(state)
    }
  } .elsewhen (state === s_refill_req) {
    io.mem_acquire.valid := true.B
    // TODO: Use AcquirePerm if just doing permissions acquire
    io.mem_acquire.bits  := edge.AcquireBlock(
      fromSource      = io.id,
      // [reconf-fix] overlap rule: slice idx to its low log2(minSets) bits
      toAddress       = Cat(req_tag, req_idx(cfIdxLowBits-1, 0)) << blockOffBits,
      lgSize          = lgCacheBlockBytes.U,
      growPermissions = grow_param)._2
    when (io.mem_acquire.fire) {
      state := s_refill_resp
    }
  } .elsewhen (state === s_refill_resp) {
    when (edge.hasData(io.mem_grant.bits)) {
      io.mem_grant.ready      := io.lb_write.ready
      io.lb_write.valid       := io.mem_grant.valid
      io.lb_write.bits.id     := io.id
      io.lb_write.bits.offset := refill_address_inc >> rowOffBits
      io.lb_write.bits.data   := io.mem_grant.bits.data
    } .otherwise {
      io.mem_grant.ready      := true.B
    }

    when (io.mem_grant.fire) {
      grant_had_data := edge.hasData(io.mem_grant.bits)
    }
    when (refill_done) {
      grantack.valid := edge.isRequest(io.mem_grant.bits)
      grantack.bits := edge.GrantAck(io.mem_grant.bits)
      state := Mux(grant_had_data, s_drain_rpq_loads, s_drain_rpq)
      assert(!(!grant_had_data && req_needs_wb))
      commit_line := false.B
      new_coh := coh_on_grant

    }
  } .elsewhen (state === s_drain_rpq_loads) {
    val drain_load = (isRead(rpq.io.deq.bits.uop.mem_cmd) &&
                     !isWrite(rpq.io.deq.bits.uop.mem_cmd) &&
                     (rpq.io.deq.bits.uop.mem_cmd =/= M_XLR)) // LR should go through replay
    // drain all loads for now
    val rp_addr = Cat(req_tag, req_idx(cfIdxLowBits-1, 0), rpq.io.deq.bits.addr(blockOffBits-1,0))
    val word_idx  = if (rowWords == 1) 0.U else rp_addr(log2Up(rowWords*coreDataBytes)-1, log2Up(wordBytes))
    val data      = io.lb_resp
    val data_word = data >> Cat(word_idx, 0.U(log2Up(coreDataBits).W))
    val loadgen = new LoadGen(rpq.io.deq.bits.uop.mem_size, rpq.io.deq.bits.uop.mem_signed,
      Cat(req_tag, req_idx(cfIdxLowBits-1, 0), rpq.io.deq.bits.addr(blockOffBits-1,0)),
      data_word, false.B, wordBytes)


    rpq.io.deq.ready       := io.resp.ready && io.lb_read.ready && drain_load
    io.lb_read.valid       := rpq.io.deq.valid && drain_load
    io.lb_read.bits.id     := io.id
    io.lb_read.bits.offset := rpq.io.deq.bits.addr >> rowOffBits

    io.resp.valid     := rpq.io.deq.valid && io.lb_read.fire && drain_load
    io.resp.bits.uop  := rpq.io.deq.bits.uop
    io.resp.bits.data := loadgen.data
    io.resp.bits.is_hella := rpq.io.deq.bits.is_hella
    when (rpq.io.deq.fire) {
      commit_line   := true.B
    }
      .elsewhen (rpq.io.empty && !commit_line)
    {
      when (!rpq.io.enq.fire) {
        state := s_mem_finish_1
        finish_to_prefetch := enablePrefetching.B
      }
    } .elsewhen (rpq.io.empty || (rpq.io.deq.valid && !drain_load)) {
      // io.commit_val is for the prefetcher. it tells the prefetcher that this line was correctly acquired
      // The prefetcher should consider fetching the next line
      io.commit_val := true.B
      state := s_meta_read
    }
  } .elsewhen (state === s_meta_read) {
    io.meta_read.valid := !io.prober_state.valid || !grantack.valid || ((io.prober_state.bits(untagBits-1,blockOffBits) & io.dcache_set_mask) =/= (req_idx & io.dcache_set_mask))
    io.meta_read.bits.idx := req_idx
    io.meta_read.bits.tag := req_tag
    io.meta_read.bits.way_en := req.way_en
    when (io.meta_read.fire) {
      state := s_meta_resp_1
    }
  } .elsewhen (state === s_meta_resp_1) {
    state := s_meta_resp_2
  } .elsewhen (state === s_meta_resp_2) {
    val needs_wb = io.meta_resp.bits.coh.onCacheControl(M_FLUSH)._1
    state := Mux(!io.meta_resp.valid, s_meta_read, // Prober could have nack'd this read
             Mux(needs_wb, s_meta_clear, s_commit_line))
  } .elsewhen (state === s_meta_clear) {
    io.meta_write.valid         := true.B
    io.meta_write.bits.idx      := req_idx
    io.meta_write.bits.data.coh := coh_on_clear
    io.meta_write.bits.data.tag := req_tag
    io.meta_write.bits.way_en   := req.way_en

    when (io.meta_write.fire) {
      state      := s_wb_req
    }
  } .elsewhen (state === s_wb_req) {
    io.wb_req.valid          := true.B

    io.wb_req.bits.tag       := req.old_meta.tag
    // Use the evicted block's full set index (stored at MSHR alloc time from full_idx_snap)
    // so the TL Release address is reconstructed correctly when the evicted block's full_idx
    // differs from the miss request's full_idx (set-masked aliasing).
    io.wb_req.bits.idx       := req.old_idx
    io.wb_req.bits.param     := shrink_param
    io.wb_req.bits.way_en    := req.way_en
    io.wb_req.bits.source    := io.id
    io.wb_req.bits.voluntary := true.B
    when (io.wb_req.fire) {
      state := s_wb_resp
    }
  } .elsewhen (state === s_wb_resp) {
    when (io.wb_resp) {
      state := s_commit_line
    }
  } .elsewhen (state === s_commit_line) {
    io.lb_read.valid       := true.B
    io.lb_read.bits.id     := io.id
    io.lb_read.bits.offset := refill_ctr

    io.refill.valid       := io.lb_read.fire
    io.refill.bits.addr   := req_block_addr | (refill_ctr << rowOffBits)
    io.refill.bits.way_en := req.way_en
    io.refill.bits.wmask  := ~(0.U(rowWords.W))
    io.refill.bits.data   := io.lb_resp
    when (io.refill.fire) {
      refill_ctr := refill_ctr + 1.U
      when (refill_ctr === (cacheDataBeats - 1).U) {
        state := s_drain_rpq
      }
    }
  } .elsewhen (state === s_drain_rpq) {
    io.replay <> rpq.io.deq
    io.replay.bits.way_en    := req.way_en
    io.replay.bits.addr := Cat(req_tag, req_idx(cfIdxLowBits-1, 0), rpq.io.deq.bits.addr(blockOffBits-1,0))
    when (io.replay.fire && isWrite(rpq.io.deq.bits.uop.mem_cmd)) {
      // Set dirty bit
      val (is_hit, _, coh_on_hit) = new_coh.onAccess(rpq.io.deq.bits.uop.mem_cmd)
      assert(is_hit, "We still don't have permissions for this store")
      new_coh := coh_on_hit
    }
    when (rpq.io.empty && !rpq.io.enq.valid) {
      state := s_meta_write_req
    }
  } .elsewhen (state === s_meta_write_req) {
    io.meta_write.valid         := true.B
    io.meta_write.bits.idx      := req_idx
    io.meta_write.bits.data.coh := new_coh
    io.meta_write.bits.data.tag := req_tag
    io.meta_write.bits.way_en   := req.way_en
    when (io.meta_write.fire) {
      state := s_mem_finish_1
      finish_to_prefetch := false.B
    }
  } .elsewhen (state === s_mem_finish_1) {
    io.mem_finish.valid := grantack.valid
    io.mem_finish.bits  := grantack.bits
    when (io.mem_finish.fire || !grantack.valid) {
      grantack.valid := false.B
      state := s_mem_finish_2
    }
  } .elsewhen (state === s_mem_finish_2) {
    state := Mux(finish_to_prefetch, s_prefetch, s_invalid)
  } .elsewhen (state === s_prefetch) {
    io.req_pri_rdy := true.B
    when ((io.req_sec_val && !io.req_sec_rdy) || io.clear_prefetch) {
      state := s_invalid
    } .elsewhen (io.req_sec_val && io.req_sec_rdy) {
      val (is_hit, _, coh_on_hit) = new_coh.onAccess(io.req.uop.mem_cmd)
      when (is_hit) { // Proceed with refill
        new_coh := coh_on_hit
        state := s_meta_read
      } .otherwise { // Reacquire this line
        new_coh := ClientMetadata.onReset
        state := s_refill_req
      }
    } .elsewhen (io.req_pri_val && io.req_pri_rdy) {
      grant_had_data := false.B
      state := handle_pri_req(state)
    }
  }
}

class BoomIOMSHR(id: Int)(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
  with HasL1HellaCacheParameters
{
  val io = IO(new Bundle {
    val req  = Flipped(Decoupled(new BoomDCacheReq))
    val resp = Decoupled(new BoomDCacheResp)
    val mem_access = Decoupled(new TLBundleA(edge.bundle))
    val mem_ack    = Flipped(Valid(new TLBundleD(edge.bundle)))

    // We don't need brupdate in here because uncacheable operations are guaranteed non-speculative
  })

  def beatOffset(addr: UInt) = addr.extract(beatOffBits-1, wordOffBits)

  def wordFromBeat(addr: UInt, dat: UInt) = {
    val shift = Cat(beatOffset(addr), 0.U((wordOffBits+log2Ceil(wordBytes)).W))
    (dat >> shift)(wordBits-1, 0)
  }

  val req = Reg(new BoomDCacheReq)
  val grant_word = Reg(UInt(wordBits.W))

  val s_idle :: s_mem_access :: s_mem_ack :: s_resp :: Nil = Enum(4)

  val state = RegInit(s_idle)
  io.req.ready := state === s_idle

  val loadgen = new LoadGen(req.uop.mem_size, req.uop.mem_signed, req.addr, grant_word, false.B, wordBytes)

  val a_source  = id.U
  val a_address = req.addr
  val a_size    = req.uop.mem_size
  val a_data    = Fill(beatWords, req.data)

  val get      = edge.Get(a_source, a_address, a_size)._2
  val put      = edge.Put(a_source, a_address, a_size, a_data)._2
  val atomics  = if (edge.manager.anySupportLogical) {
    MuxLookup(req.uop.mem_cmd, (0.U).asTypeOf(new TLBundleA(edge.bundle)))(Array(
      M_XA_SWAP -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.SWAP)._2,
      M_XA_XOR  -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.XOR) ._2,
      M_XA_OR   -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.OR)  ._2,
      M_XA_AND  -> edge.Logical(a_source, a_address, a_size, a_data, TLAtomics.AND) ._2,
      M_XA_ADD  -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.ADD)._2,
      M_XA_MIN  -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MIN)._2,
      M_XA_MAX  -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MAX)._2,
      M_XA_MINU -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MINU)._2,
      M_XA_MAXU -> edge.Arithmetic(a_source, a_address, a_size, a_data, TLAtomics.MAXU)._2))
  } else {
    // If no managers support atomics, assert fail if processor asks for them
    assert(state === s_idle || !isAMO(req.uop.mem_cmd))
    (0.U).asTypeOf(new TLBundleA(edge.bundle))
  }
  assert(state === s_idle || req.uop.mem_cmd =/= M_XSC)

  io.mem_access.valid := state === s_mem_access
  io.mem_access.bits  := Mux(isAMO(req.uop.mem_cmd), atomics, Mux(isRead(req.uop.mem_cmd), get, put))

  val send_resp = isRead(req.uop.mem_cmd)

  io.resp.valid     := (state === s_resp) && send_resp
  io.resp.bits.is_hella := req.is_hella
  io.resp.bits.uop  := req.uop
  io.resp.bits.data := loadgen.data

  when (io.req.fire) {
    req   := io.req.bits
    state := s_mem_access
  }
  when (io.mem_access.fire) {
    state := s_mem_ack
  }
  when (state === s_mem_ack && io.mem_ack.valid) {
    state := s_resp
    when (isRead(req.uop.mem_cmd)) {
      grant_word := wordFromBeat(req.addr, io.mem_ack.bits.data)
    }
  }
  when (state === s_resp) {
    when (!send_resp || io.resp.fire) {
      state := s_idle
    }
  }
}

class LineBufferReadReq(implicit p: Parameters) extends BoomBundle()(p)
  with HasL1HellaCacheParameters
{
  val id      = UInt(log2Ceil(nLBEntries).W)
  val offset  = UInt(log2Ceil(cacheDataBeats).W)
  def lb_addr = Cat(id, offset)
}

class LineBufferWriteReq(implicit p: Parameters) extends LineBufferReadReq()(p)
{
  val data   = UInt(encRowBits.W)
}

class LineBufferMetaWriteReq(implicit p: Parameters) extends BoomBundle()(p)
{
  val id   = UInt(log2Ceil(nLBEntries).W)
  val coh  = new ClientMetadata
  val addr = UInt(coreMaxAddrBits.W)
}

class LineBufferMeta(implicit p: Parameters) extends BoomBundle()(p)
  with HasL1HellaCacheParameters
{
  val coh  = new ClientMetadata
  val addr = UInt(coreMaxAddrBits.W)
}

class BoomMSHRFile(implicit edge: TLEdgeOut, p: Parameters) extends BoomModule()(p)
  with HasL1HellaCacheParameters
  with CoreFuzzingConstants
  with HasBoomExtendedTag   // [reconf-fix] cfTagLSB
{
  val io = IO(new Bundle {
    val req  = Flipped(Vec(memWidth, Decoupled(new BoomDCacheReqInternal))) // Req from s2 of DCache pipe
    val req_is_probe = Input(Vec(memWidth, Bool()))
    val resp = Decoupled(new BoomDCacheResp)
    val secondary_miss = Output(Vec(memWidth, Bool()))
    val block_hit = Output(Vec(memWidth, Bool()))

    val brupdate       = Input(new BrUpdateInfo)
    val exception    = Input(Bool())
    val rob_pnr_idx  = Input(UInt(robAddrSz.W))
    val rob_head_idx = Input(UInt(robAddrSz.W))

    val mem_acquire  = Decoupled(new TLBundleA(edge.bundle))
    val mem_grant    = Flipped(Decoupled(new TLBundleD(edge.bundle)))
    val mem_finish   = Decoupled(new TLBundleE(edge.bundle))

    val refill     = Decoupled(new L1DataWriteReq)
    val meta_write = Decoupled(new BoomL1MetaWriteReq)
    val meta_read  = Decoupled(new BoomL1MetaRdReq)
    val meta_resp  = Input(Valid(new BoomL1Metadata))   // [reconf-fix]
    val replay     = Decoupled(new BoomDCacheReqInternal)
    val prefetch   = Decoupled(new BoomDCacheReq)
    val wb_req     = Decoupled(new BoomWritebackReq(edge.bundle))

    val prober_state = Input(Valid(UInt(coreMaxAddrBits.W)))

    val clear_all = Input(Bool()) // Clears all uncommitted MSHRs to prepare for fence
    // [reconf-fix 2026-08-06] Kill parked prefetch MSHRs while a D$ geometry change is
    // staged.  A parked s_prefetch MSHR is not s_invalid, so without this the commit
    // gate would never see an idle cache and the quiesce would hang.  Its line is
    // already filled and GrantAck'd, so retiring it is pure state:=s_invalid with no
    // protocol action.  ENABLE_RECONF-only producer (tied false otherwise).
    val clear_prefetch_all = Input(Bool())

    val wb_resp   = Input(Bool())

    val fence_rdy = Output(Bool())
    // [reconf-fix] all cacheable MSHRs strictly idle — gate condition for applying a
    // staged D$ geometry change (IOMSHRs excluded: they never touch the cache arrays).
    val cache_idle = Output(Bool())
    val probe_rdy = Output(Bool())
    // corefuzzing: union of way_en bits of all active MSHRs (one-hot per way).
    // Used by the dcache to avoid assigning two concurrent misses to the same way,
    // which would cause the second MSHR's refill to overwrite the first, making the
    // first MSHR's subsequent replay miss and trigger assert(!(s2_type===t_replay && !s2_hit)).
    val pending_way_mask = Output(UInt(nWays.W))
    // [reconf-fix 2026-08-12] Active set mask (cf_dcache_active_sets-1), driven by the
    // dcache.  The per-set exclusion below MUST compare the PHYSICAL row a line occupies,
    // not the full untagged index: refills write `idx & dcache_set_mask` (dcache.scala
    // :1211 / :1208), so at a reduced set count two addresses differing only in the
    // dropped index bits land in the SAME row while an unmasked compare calls them
    // different sets.  Both then allocate, both refill the same row, and one line's tag
    // ends up over the other's data -- a silent wrong-data hit.
    val dcache_set_mask = Input(UInt(idxBits.W))
    // corefuzzing: fill-completion event — fires when an MSHR meta_write completes
    val cf_meta_write_fill = Output(Valid(new Bundle {
      val idx      = UInt(idxBits.W)
      val way_en   = UInt(nWays.W)
      val domain   = UInt(1.W)
      val op_count = UInt(uopIDCounterWidthCF.W)
      val secret   = Bool()
    }))
  })

  val req_idx = OHToUInt(io.req.map(_.valid))
  val req     = io.req(req_idx)
  val req_is_probe = io.req_is_probe(0)

  for (w <- 0 until memWidth)
    io.req(w).ready := false.B

  val prefetcher: DataPrefetcher = if (enablePrefetching) Module(new NLPrefetcher)
                                                     else Module(new NullPrefetcher)

  io.prefetch <> prefetcher.io.prefetch


  val cacheable = edge.manager.supportsAcquireBFast(req.bits.addr, lgCacheBlockBytes.U)

  // --------------------
  // The MSHR SDQ
  val sdq_val      = RegInit(0.U(cfg.nSDQ.W))
  val sdq_alloc_id = PriorityEncoder(~sdq_val(cfg.nSDQ-1,0))
  val sdq_rdy      = !sdq_val.andR
  val sdq_enq      = req.fire && cacheable && isWrite(req.bits.uop.mem_cmd)
  val sdq          = Mem(cfg.nSDQ, UInt(coreDataBits.W))

  when (sdq_enq) {
    sdq(sdq_alloc_id) := req.bits.data
  }

  // --------------------
  // The LineBuffer Data
  // Holds refilling lines, prefetched lines
  val lb = Mem(nLBEntries * cacheDataBeats, UInt(encRowBits.W))
  val lb_read_arb  = Module(new Arbiter(new LineBufferReadReq, cfg.nMSHRs))
  val lb_write_arb = Module(new Arbiter(new LineBufferWriteReq, cfg.nMSHRs))

  lb_read_arb.io.out.ready  := false.B
  lb_write_arb.io.out.ready := true.B

  val lb_read_data = WireInit(0.U(encRowBits.W))
  when (lb_write_arb.io.out.fire) {
    lb.write(lb_write_arb.io.out.bits.lb_addr, lb_write_arb.io.out.bits.data)
  } .otherwise {
    lb_read_arb.io.out.ready := true.B
    when (lb_read_arb.io.out.fire) {
      lb_read_data := lb.read(lb_read_arb.io.out.bits.lb_addr)
    }
  }
  def widthMap[T <: Data](f: Int => T) = VecInit((0 until memWidth).map(f))




  val idx_matches = Wire(Vec(memWidth, Vec(cfg.nMSHRs, Bool())))
  val tag_matches = Wire(Vec(memWidth, Vec(cfg.nMSHRs, Bool())))
  val way_matches = Wire(Vec(memWidth, Vec(cfg.nMSHRs, Bool())))

  val tag_match   = widthMap(w => Mux1H(idx_matches(w), tag_matches(w)))
  val idx_match   = widthMap(w => idx_matches(w).reduce(_||_))
  val way_match   = widthMap(w => Mux1H(idx_matches(w), way_matches(w)))

  // [dead-code 2026-08-13] wb_tag_list was declared and written (one entry per MSHR from
  // mshr.io.wb_req.bits.tag) but NEVER READ anywhere.  Removed to stop it reading like a
  // live writeback-tag tracking structure; the writeback carries its own tag/idx per MSHR
  // (io.wb_req.bits.tag / .idx), so nothing needs this list.
  //   val wb_tag_list = Wire(Vec(cfg.nMSHRs, UInt(cfTagBits.W)))

  val meta_write_arb = Module(new Arbiter(new BoomL1MetaWriteReq       , cfg.nMSHRs))
  val meta_read_arb  = Module(new Arbiter(new BoomL1MetaRdReq          , cfg.nMSHRs))
  val wb_req_arb     = Module(new Arbiter(new BoomWritebackReq(edge.bundle), cfg.nMSHRs))
  val replay_arb     = Module(new Arbiter(new BoomDCacheReqInternal    , cfg.nMSHRs))
  val resp_arb       = Module(new Arbiter(new BoomDCacheResp           , cfg.nMSHRs + nIOMSHRs))
  val refill_arb     = Module(new Arbiter(new L1DataWriteReq           , cfg.nMSHRs))

  val commit_vals    = Wire(Vec(cfg.nMSHRs, Bool()))
  val commit_addrs   = Wire(Vec(cfg.nMSHRs, UInt(coreMaxAddrBits.W)))
  val commit_cohs    = Wire(Vec(cfg.nMSHRs, new ClientMetadata))

  var sec_rdy   = false.B

  io.fence_rdy := true.B
  io.probe_rdy := true.B
  io.mem_grant.ready := false.B

  val mshr_alloc_idx = Wire(UInt())
  val pri_rdy = WireInit(false.B)
  val pri_val = req.valid && sdq_rdy && cacheable && !idx_match(req_idx)
  val mshrs = (0 until cfg.nMSHRs) map { i =>
    val mshr = Module(new BoomMSHR)
    mshr.io.id := i.U(log2Ceil(cfg.nMSHRs).W)
    mshr.io.dcache_set_mask := io.dcache_set_mask

    for (w <- 0 until memWidth) {
      // [reconf-fix 2026-08-12] Mask BOTH sides to the active geometry so "same set"
      // means "same PHYSICAL row".  Refills write `idx & dcache_set_mask` (dcache.scala
      // :1211/:1208), so at a reduced set count two addresses differing only in the
      // dropped index bits share a row while an unmasked compare calls them different
      // sets -- both allocate, both refill that row, and one line's tag ends up over the
      // other's data (silent wrong-data hit).  Strictly more conservative: a coarser
      // match only blocks a primary alloc or merges a secondary miss that also passes
      // tag_matches, and it restores the one-MSHR-per-row invariant way_matches' Mux1H
      // and the refill path rely on.
      idx_matches(w)(i) := mshr.io.idx.valid &&
                           (mshr.io.idx.bits & io.dcache_set_mask) ===
                           (io.req(w).bits.addr(untagBits-1,blockOffBits) & io.dcache_set_mask)
      // [reconf-fix] compare the extended tag (cfTagLSB), matching what the MSHR stores
      tag_matches(w)(i) := mshr.io.tag.valid && mshr.io.tag.bits === io.req(w).bits.addr >> cfTagLSB
      way_matches(w)(i) := mshr.io.way.valid && mshr.io.way.bits === io.req(w).bits.way_en
    }
    //   wb_tag_list(i) := mshr.io.wb_req.bits.tag   // [dead-code 2026-08-13] see decl



    mshr.io.req_pri_val  := (i.U === mshr_alloc_idx) && pri_val
    when (i.U === mshr_alloc_idx) {
      pri_rdy := mshr.io.req_pri_rdy
    }

    mshr.io.req_sec_val  := req.valid && sdq_rdy && tag_match(req_idx) && idx_matches(req_idx)(i) && cacheable
    mshr.io.req          := req.bits
    mshr.io.req_is_probe := req_is_probe
    mshr.io.req.sdq_id   := sdq_alloc_id

    // Clear because of a FENCE, a request to the same idx as a prefetched line,
    // a probe to that prefetched line, all mshrs are in use
    mshr.io.clear_prefetch := ((io.clear_all && !req.valid)||
      (req.valid && idx_matches(req_idx)(i) && cacheable && !tag_match(req_idx)) ||
      (req_is_probe && idx_matches(req_idx)(i)) ||
      io.clear_prefetch_all)   // [reconf-fix] staged geometry change: retire parked prefetches
    mshr.io.brupdate       := io.brupdate
    mshr.io.exception    := io.exception
    mshr.io.rob_pnr_idx  := io.rob_pnr_idx
    mshr.io.rob_head_idx := io.rob_head_idx

    mshr.io.prober_state := io.prober_state

    mshr.io.wb_resp      := io.wb_resp

    meta_write_arb.io.in(i) <> mshr.io.meta_write
    meta_read_arb.io.in(i)  <> mshr.io.meta_read
    mshr.io.meta_resp       := io.meta_resp
    wb_req_arb.io.in(i)     <> mshr.io.wb_req
    replay_arb.io.in(i)     <> mshr.io.replay
    refill_arb.io.in(i)     <> mshr.io.refill

    lb_read_arb.io.in(i)       <> mshr.io.lb_read
    mshr.io.lb_resp            := lb_read_data
    lb_write_arb.io.in(i)      <> mshr.io.lb_write

    commit_vals(i)  := mshr.io.commit_val
    commit_addrs(i) := mshr.io.commit_addr
    commit_cohs(i)  := mshr.io.commit_coh

    mshr.io.mem_grant.valid := false.B
    mshr.io.mem_grant.bits  := DontCare
    when (io.mem_grant.bits.source === i.U) {
      mshr.io.mem_grant <> io.mem_grant
    }

    sec_rdy   = sec_rdy || (mshr.io.req_sec_rdy && mshr.io.req_sec_val)
    resp_arb.io.in(i) <> mshr.io.resp

    when (!mshr.io.req_pri_rdy) {
      io.fence_rdy := false.B
    }
    for (w <- 0 until memWidth) {
      when (!mshr.io.probe_rdy && idx_matches(w)(i) && io.req_is_probe(w)) {
        io.probe_rdy := false.B
      }
    }

    mshr
  }

  // corefuzzing: OR together way_en of all active MSHRs so the dcache can avoid
  // picking the same replacement way for two concurrent misses to the same set.
  io.pending_way_mask := mshrs.map(m => Mux(m.io.way.valid, m.io.way.bits, 0.U)).reduce(_ | _)

  // [reconf-fix 2026-08-06] Strict all-idle for the D$ config-commit gate.
  // Compile-time gated: constant true when reconfiguration is not built, so the
  // gate (and this reduction) vanish from the baseline netlist.
  io.cache_idle := (if (ENABLE_RECONF) mshrs.map(_.io.state_invalid).reduce(_&&_) else true.B)

  // Try to round-robin the MSHRs
  val mshr_head      = RegInit(0.U(log2Ceil(cfg.nMSHRs).W))
  mshr_alloc_idx    := RegNext(AgePriorityEncoder(mshrs.map(m=>m.io.req_pri_rdy), mshr_head))
  when (pri_rdy && pri_val) { mshr_head := WrapInc(mshr_head, cfg.nMSHRs) }



  io.meta_write <> meta_write_arb.io.out
  io.meta_read  <> meta_read_arb.io.out
  io.wb_req     <> wb_req_arb.io.out
  // corefuzzing: fire fill-completion event when an MSHR's meta_write is accepted
  io.cf_meta_write_fill.valid         := meta_write_arb.io.out.fire
  io.cf_meta_write_fill.bits.idx      := meta_write_arb.io.out.bits.idx
  io.cf_meta_write_fill.bits.way_en   := meta_write_arb.io.out.bits.way_en
  io.cf_meta_write_fill.bits.tag      := meta_write_arb.io.out.bits.data.tag
  io.cf_meta_write_fill.bits.domain   := Mux1H(UIntToOH(meta_write_arb.io.chosen),
                                           VecInit(mshrs.map(_.io.cf_req_domain)))
  io.cf_meta_write_fill.bits.op_count := Mux1H(UIntToOH(meta_write_arb.io.chosen),
                                           VecInit(mshrs.map(_.io.cf_req_op_count)))
  io.cf_meta_write_fill.bits.secret   := Mux1H(UIntToOH(meta_write_arb.io.chosen),
                                           VecInit(mshrs.map(_.io.cf_req_secret)))

  val mmio_alloc_arb = Module(new Arbiter(Bool(), nIOMSHRs))


  var mmio_rdy = false.B

  val mmios = (0 until nIOMSHRs) map { i =>
    val id = cfg.nMSHRs + 1 + i // +1 for wb unit
    val mshr = Module(new BoomIOMSHR(id))

    mmio_alloc_arb.io.in(i).valid := mshr.io.req.ready
    mmio_alloc_arb.io.in(i).bits  := DontCare
    mshr.io.req.valid := mmio_alloc_arb.io.in(i).ready
    mshr.io.req.bits  := req.bits

    mmio_rdy = mmio_rdy || mshr.io.req.ready

    mshr.io.mem_ack.bits  := io.mem_grant.bits
    mshr.io.mem_ack.valid := io.mem_grant.valid && io.mem_grant.bits.source === id.U
    when (io.mem_grant.bits.source === id.U) {
      io.mem_grant.ready := true.B
    }

    resp_arb.io.in(cfg.nMSHRs + i) <> mshr.io.resp
    when (!mshr.io.req.ready) {
      io.fence_rdy := false.B
    }
    mshr
  }

  mmio_alloc_arb.io.out.ready := req.valid && !cacheable

  TLArbiter.lowestFromSeq(edge, io.mem_acquire, mshrs.map(_.io.mem_acquire) ++ mmios.map(_.io.mem_access))
  TLArbiter.lowestFromSeq(edge, io.mem_finish,  mshrs.map(_.io.mem_finish))

  val respq = Module(new BranchKillableQueue(new BoomDCacheResp, 4, u => u.uses_ldq, flow = false))
  respq.io.brupdate := io.brupdate
  respq.io.flush    := io.exception
  respq.io.enq      <> resp_arb.io.out
  io.resp           <> respq.io.deq

  for (w <- 0 until memWidth) {
    io.req(w).ready      := (w.U === req_idx) &&
      Mux(!cacheable, mmio_rdy, sdq_rdy && Mux(idx_match(w), tag_match(w) && sec_rdy, pri_rdy))
    io.secondary_miss(w) := idx_match(w) && way_match(w) && !tag_match(w)
    io.block_hit(w)      := idx_match(w) && tag_match(w)
  }
  io.refill         <> refill_arb.io.out

  val free_sdq = io.replay.fire && isWrite(io.replay.bits.uop.mem_cmd)

  io.replay <> replay_arb.io.out
  io.replay.bits.data := sdq(replay_arb.io.out.bits.sdq_id)

  when (io.replay.valid || sdq_enq) {
    sdq_val := sdq_val & ~(UIntToOH(replay_arb.io.out.bits.sdq_id) & Fill(cfg.nSDQ, free_sdq)) |
      PriorityEncoderOH(~sdq_val(cfg.nSDQ-1,0)) & Fill(cfg.nSDQ, sdq_enq)
  }

  prefetcher.io.mshr_avail    := RegNext(pri_rdy)
  prefetcher.io.req_val       := RegNext(commit_vals.reduce(_||_))
  prefetcher.io.req_addr      := RegNext(Mux1H(commit_vals, commit_addrs))
  prefetcher.io.req_coh       := RegNext(Mux1H(commit_vals, commit_cohs))
}
