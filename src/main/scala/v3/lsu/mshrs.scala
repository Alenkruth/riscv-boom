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
    // [D1FIXA] dcache.scala cf_pend_hold (a flop): a deferred IFT metadata write is pending.
    // Holds this MSHR's refill beats (s_commit_line) and meta writes (s_meta_clear,
    // s_meta_write_req).  Constant false with IFT off.
    val cf_ift_hold = Input(Bool())

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
    val cf_req_pc12     = Output(UInt(inflPcBitsCF.W))   // [PCPROV] filler PC[13:2] (wire off req.uop)
    val cf_req_secret   = Output(Bool())
    // [RESID] step 15: the line this MSHR fills was requested at a secret-derived ADDRESS by the
    // primary or by any merged secondary; PC[13:2] of the first such request.
    val cf_req_addr_sec = Output(Bool())
    val cf_req_addr_atk = Output(Bool())   // [ATKRES] attacker-address line mark
    val cf_req_res_pc   = Output(UInt(inflPcBitsCF.W))
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
  // [G9LIVE] patch 50: forward progress for the [G9STMETA] store-replay bubble (s_drain_rpq).
  // With the bubble, s_drain_rpq replays at most one store per 2 cycles, and the LSU gets the
  // D$ port in every bubble cycle, so a tight store loop to the missed line put one new
  // secondary store into the rpq per bubble: the rpq never emptied, the MSHR never left
  // s_drain_rpq, GrantAck was never sent and probe_rdy stayed 0 (an L2 probe of the line, e.g.
  // a TSI Get of tohost, starved forever; FPGA IFT bitstream, rv64ui-p-simple).  IFT builds
  // therefore refuse NEW secondaries while draining, exactly as upstream already does from
  // s_meta_write_req on: the request is nacked (s2_nack_miss), the LSU retries it, and block_hit
  // keeps it from hitting until the MSHR is s_invalid.  The drain is then bounded by the rpq
  // contents at entry (<= nRPQ entries, <= 2*nRPQ cycles).  The bubble itself, probe_rdy and the
  // prober/writeback interlocks are unchanged.  Timing: `state` is a 5-bit flop and the state
  // term stays one 5-input function of it, so 0 added levels on sec_rdy -> io.req.ready ->
  // s2_nack.  IFT off (no bubble) elaborates the original expression.
  val sec_rdy = (!cmd_requires_second_acquire && !io.req_is_probe &&
                 !state.isOneOf(s_invalid, s_meta_write_req, s_mem_finish_1, s_mem_finish_2) &&
                 (if (ENABLE_IFT) state =/= s_drain_rpq else true.B))// Always accept secondary misses

  // [AREA35] patch 35 (D): the 16-entry replay queue keeps its IFT uop fields in LUTRAM
  // (8 MSHRs x 16 entries; see BranchKillableQueue iftInRam).  Not on W: rpq.io.deq feeds
  // io.replay (D$ s0 -> registered s1), and sec_rdy reads only rpq.io.enq.ready (pointers).
  val rpq = Module(new BranchKillableQueue(new BoomDCacheReqInternal, cfg.nRPQ, u => u.uses_ldq, false, iftInRam = true))
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
  rpq.io.enq.bits.uop.cf_op_count_id.foreach(_ := 0.U)
  rpq.io.enq.bits.uop.cf_spec_branch_is_secret.foreach(_ := false.B)
  // [RDDOM] step 19: the rpq keeps ONE bit of the reader's domain (iftTagWidth = 1; any wider
  // bits stay constant 0 and fold).  It is read only by the attacker-address edge gate below and
  // is zeroed again on the resp/replay uop, so everything downstream still sees domain 0.
  // [R124GATE] patch 26 (r151): IFT off keeps the pre-step-19 constant 0 (no rpq domain bit).
  rpq.io.enq.bits.uop.cf_domain_id.foreach(_ := (if (ENABLE_IFT) (io.req.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= 0.U).asUInt
                                                      else 0.U))
  // [RESID] step 15: the residency bits are per-MSHR (req + the regs below), never per rpq entry.
  // Constant inputs: firtool folds the rpq storage for them away (same idiom as above).
  rpq.io.enq.bits.cf_addr_sec.foreach(_ := false.B)
  rpq.io.enq.bits.cf_addr_atk.foreach(_ := false.B)   // [ATKRES] per-MSHR, like cf_addr_sec
  rpq.io.enq.bits.cf_res_v.foreach(_ := false.B)
  rpq.io.enq.bits.cf_res_pc.foreach(_ := 0.U)
  rpq.io.enq.bits.cf_res_atk.foreach(_ := false.B)   // [ATKEVICT] per-MSHR too
  rpq.io.enq.bits.cf_res_esec.foreach(_ := false.B)
  // [G9STMETA] (ruling 101, G-9) the store replay writes D$ store metadata, whose attacker
  // bit is `domain || cf_attacker_influence` on the hit path.  The domain is zeroed above, so
  // fold it into the (kept) attacker bit for STORES only: one LUT, no extra rpq bit.  The
  // replayed store's response uop is never read for taint (the LSU uses the STQ uop).
  if (ENABLE_IFT) {
    rpq.io.enq.bits.uop.cf_attacker_influence.foreach(_ := io.req.uop.cf_attacker_influence.getOrElse(false.B) ||
      (isWrite(io.req.uop.mem_cmd) && io.req.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= 0.U))
  }
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
  // [reconf-fix] strict idle for the config-commit gate (see io declaration).
  io.state_invalid := state === s_invalid
  // corefuzzing: expose stored request domain for fill-completion tracking in dcache
  io.cf_req_domain   := req.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W))
  io.cf_req_op_count := req.uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
  // [R124GATE] patch 26: IFT off drives 0, so nothing reads req.uop.debug_pc (40 FF per MSHR).
  io.cf_req_pc12     := (if (ENABLE_IFT) req.uop.debug_pc(inflPcBitsCF + 1, 2) else 0.U)   // [PCPROV]
  // [SECFIX 2026-09-07] The refill blankets this bit across ALL 8 DWs of the line
  // (dcache.scala Fill(CF_SUBLINE, ...)), so it must describe the DATA, not the
  // REQUESTER.  cf_secret_access = the TLB says this ADDRESS is in the secret range,
  // which is a property of the line and legitimately covers it.  cf_secret_propagation
  // = this uop merely CARRIES secret-derived data; tagging the line it reads from is
  // a requester property and amplifies without bound: any s_prop-carrying load that
  // misses marked a whole 64B line secret, and every later load from that line
  // inherited it.  MEASURED: with the cf_mem_sec_dataflow merge enabled that drove
  // s_prop 50 -> 144,332 on spectre-v1.  The per-DW masks were never the problem --
  // the store path honours them; this refill path threw them away.
  io.cf_req_secret   := req.uop.cf_secret_access.getOrElse(false.B)
  // [RESID] step 15 (residue audit rows 2/3/5/13).  The primary's own bits live in `req`
  // (req := io.req at allocation): req.cf_addr_sec = the primary's address is secret-derived,
  // req.cf_res_v/pc = the D$ found the primary's tag in the per-set eviction filter (this line
  // was evicted by a secret-chosen eviction).  A merged secondary with a secret address is
  // recorded in cf_sec_asec/pc (first one wins).  cf_first_rp = the next rpq dequeue is the
  // primary itself (the rpq is FIFO and the primary is enqueued first unless it is a prefetch,
  // whose cf_addr_sec is 0).
  // [R124GATE] patch 26 (ruling 124): these five are flops only with ENABLE_IFT; IFT off they
  // are constant-0 wires (writes gated below) and the four cf_req_* outputs are 0.
  val cf_sec_asec    = if (ENABLE_IFT) RegInit(false.B) else WireInit(false.B)
  val cf_sec_asec_pc = if (ENABLE_IFT) Reg(UInt(inflPcBitsCF.W)) else WireInit(0.U(inflPcBitsCF.W))
  val cf_first_rp    = if (ENABLE_IFT) RegInit(false.B) else WireInit(false.B)
  val cf_pri_pc12    = req.uop.debug_pc(inflPcBitsCF + 1, 2)
  // [ATKRES] step 17 (owner ruling 119 (3)): the attacker-address twin of cf_sec_asec.
  val cf_sec_aatk    = if (ENABLE_IFT) RegInit(false.B) else WireInit(false.B)
  val cf_sec_aatk_pc = if (ENABLE_IFT) Reg(UInt(inflPcBitsCF.W)) else WireInit(0.U(inflPcBitsCF.W))
  if (ENABLE_IFT) {
    io.cf_req_addr_sec := req.cf_addr_sec.get || cf_sec_asec
    io.cf_req_addr_atk := req.cf_addr_atk.get || cf_sec_aatk
    // the mark's PC: the secret cause first, else the attacker-address access
    io.cf_req_res_pc   := Mux(req.cf_addr_sec.get, cf_pri_pc12, Mux(cf_sec_asec, cf_sec_asec_pc,
                          Mux(req.cf_addr_atk.get, cf_pri_pc12, cf_sec_aatk_pc)))
  } else {
    io.cf_req_addr_sec := false.B
    io.cf_req_addr_atk := false.B
    io.cf_req_res_pc   := 0.U
  }
  // Edge on a dequeued request's response: the line was secret-evicted (every request, the
  // primary included), or the primary's secret-address miss brought the line in (every request
  // but the primary itself -- its own latency is not influenced by its own address).
  // [ATKRES] ...or the primary's ATTACKER-address miss brought it in: {atk=1, sec=0} edge.
  val cf_rp_asec_e = req.cf_addr_sec.getOrElse(false.B) && !cf_first_rp
  // [RDDOM] step 19 (side finding, p3-postapply-markers): like the hit path (dcache ratk_fire,
  // uop_dom === 0), the attacker-address edge goes to VICTIM (domain 0) readers only.  The
  // reader is the dequeued rpq entry; one AND on the rpq output bit (nRPQ x 1 flop of storage).
  val cf_rp_rd_atk = rpq.io.deq.bits.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W))(0)
  val cf_rp_aatk_e = req.cf_addr_atk.getOrElse(false.B) && !cf_first_rp && !cf_rp_rd_atk
  // [NACKATK] patch 44 (r111 extended, r173 (3)): an attacker-only nack edge {atk=1, sec=0} of the
  // primary (a victim load nacked behind an attacker-influenced MSHR, then missed) arrives as
  // req {cf_res_v=0, cf_res_atk=1} (dcache.scala [NACKATK]; cf_res_atk was unused with cf_res_v=0).
  // It goes on every response to a VICTIM reader of this MSHR (the primary included; attacker
  // readers excluded as for cf_rp_aatk_e) as {atk=1}; when a residency edge of this MSHR also fires
  // (asec_e / aatk_e) that edge keeps its PC (the primary's) and gains atk=1, and sec stays asec_e
  // (secret-first, one edge).  With cf_rp_nk = 0 every output below is unchanged.
  val cf_rp_nk     = !req.cf_res_v.getOrElse(false.B) && req.cf_res_atk.getOrElse(false.B) && !cf_rp_rd_atk
  val cf_rp_res_v  = req.cf_res_v.getOrElse(false.B) || cf_rp_asec_e || cf_rp_aatk_e || cf_rp_nk
  val cf_rp_res_pc = Mux(req.cf_res_v.getOrElse(false.B) || (cf_rp_nk && !cf_rp_asec_e && !cf_rp_aatk_e),
                         req.cf_res_pc.getOrElse(0.U), cf_pri_pc12)
  val cf_rp_res_atk  = Mux(req.cf_res_v.getOrElse(false.B), req.cf_res_atk.getOrElse(false.B),  !cf_rp_asec_e || cf_rp_nk)
  // !cf_res_v: sec = !atk || esec = asec_e both with and without cf_rp_nk (was esec = 0, same sec)
  val cf_rp_res_esec = Mux(req.cf_res_v.getOrElse(false.B), req.cf_res_esec.getOrElse(false.B), cf_rp_asec_e)
  if (ENABLE_IFT) { when (rpq.io.deq.fire) { cf_first_rp := false.B } }   // [R124GATE]
  // [SECDEST] step 17 (owner ruling 116 A(2)), miss path: the line this MSHR fills is marked by
  // a secret-ADDRESS access (the same bit the meta write puts in ift_res_meta), so every value
  // read out of it is secret.  Set on the dequeued request's uop flag cf_mem_sec_dataflow
  // (line-buffer response and replay below); the D$/LSU/core then treat it like any other
  // D$-metadata secret value (preg secret + s_prop).  The primary's own value is A(1) anyway.
  // [VSECEV] step 17 (owner ruling 119 (2)): also a miss on a tag a SECRET-chosen eviction
  // removed (the D$ set req.uop.cf_mem_sec_dataflow from the eviction filter's s bit).
  val cf_rp_vsec   = io.cf_req_addr_sec || req.uop.cf_mem_sec_dataflow.getOrElse(false.B)
  // [ATKEVICT] step 17 (owner extension of ruling 116 C), miss path: the primary missed on a tag
  // that an attacker-domain allocation evicted (the D$ set req.uop.cf_mem_dataflow_atk from the
  // eviction filter, victim readers only).  Every value drained from this MSHR carries the
  // attacker taint, as every response already carries the edge (cf_rp_res_*).
  // [ATKRES] ...and a line marked by an attacker-ADDRESS access (ruling 119 (3)).  The rpq
  // zeroes the reader's domain, so attacker-domain readers get it too (their values are
  // attacker-tainted by the domain seed anyway).
  val cf_rp_vatk   = req.uop.cf_mem_dataflow_atk.getOrElse(false.B) || io.cf_req_addr_atk
  io.idx.valid := state =/= s_invalid
  io.tag.valid := state =/= s_invalid
  io.way.valid := !state.isOneOf(s_invalid, s_prefetch)
  io.idx.bits := req_idx
  io.tag.bits := req_tag
  io.way.bits := req.way_en

  io.meta_write.valid    := false.B
  io.meta_write.bits     := DontCare
  // [D1FIXA] (dcache.scala, D1 fix A) while the D$ holds a deferred ift_store_meta /
  // ift_res_meta write, no refill beat and no meta write may fire: a new first-beat clear or fill
  // mark would need a second pending entry, and the MSHR whose clear is pending must not reach
  // s_drain_rpq (replays) or s_meta_write_req before that clear lands.  Holding EVERY beat, not
  // only the first, also holds the s_commit_line -> s_drain_rpq step (it fires on the last beat)
  // and keeps the gate a single flop input: lb_read.valid / meta_write.valid become one LUT of
  // (state, cf_ift_hold) = 6 inputs, as deep as the 5-bit state decode they replace.  Typical
  // hold: 1 cycle (the next cycle without a lane write on that way); bound 16 + 2 cycles plus the
  // bounded replay drain (dcache.scala cf_pend_starve).
  val cf_hold = if (ENABLE_IFT) io.cf_ift_hold else false.B
  io.req_pri_rdy         := false.B
  io.req_sec_rdy         := sec_rdy && rpq.io.enq.ready
  io.mem_acquire.valid   := false.B
  io.mem_acquire.bits    := DontCare
  io.refill.valid        := false.B
  io.refill.bits         := DontCare
  io.replay.valid        := false.B
  io.replay.bits         := DontCare
  // [G9STMETA] a store replay fired last cycle (see s_drain_rpq)
  val cf_st_replay_last = RegNext(io.replay.fire && isWrite(io.replay.bits.uop.mem_cmd), false.B)
  io.wb_req.valid        := false.B
  io.wb_req.bits         := DontCare
  io.resp.valid          := false.B
  io.resp.bits           := DontCare
  io.resp.bits.edges     := 0.U.asTypeOf(new IftEdgeList)   // Task 3.2 (D16): a refill response has no D$ edge
  io.resp.bits.cf_res_v.foreach(_ := false.B)
  io.resp.bits.cf_res_pc.foreach(_ := 0.U)
  io.resp.bits.cf_res_atk.foreach(_ := false.B)   // [ATKEVICT]
  io.resp.bits.cf_res_esec.foreach(_ := false.B)
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
    // [SECDOM 2026-09-09] Accumulate the REQUESTER taint across the primary and
    // every accepted secondary.
    //
    // WHY: sec_rdy (above) accepts a request as a secondary whenever it needs no
    // additional permission -- which is exactly the case for a LOAD hitting an
    // outstanding miss.  Only the primary's uop is latched into `req`, so only the
    // primary's domain/secret ever reached io.cf_req_* and therefore the fill's IFT
    // entry.  A STORE to a shared line sets cmd_requires_second_acquire and is forced
    // to allocate a fresh primary, so stores tagged the line and loads silently did
    // not.  MEASURED (t37_atk_load_fill, TrueLRU): the attacker's load produced the
    // 17th fill at its set, yet 0 of 60 MSHR allocations carried domain=1.
    //
    // This mirrors the OR-accumulation the store path already does in ift_store_meta.
    // Cost is zero new state: `req` is an existing register and mem_cmd above is
    // already written from this same block -- only a mux on 18 bits that exist.
    //
    // op_count must NAME the op that set the bit, or the parser traces the wrong uop:
    // if the primary was clean and a tainted secondary is what raised domain/secret,
    // adopt that secondary's op_count.  First tainted contributor wins; an already
    // tainted primary keeps its own id.
    // [G2 2026-09-09] compile-time gated: a non-IFT build must not elaborate these muxes.
    // Convention follows dcache.scala:1511, whose comment notes the gate is what lets
    // FIRRTL DCE remove the machinery rather than relying on it to prove them dead.
    if (ENABLE_IFT) {
      val cf_sec_req_tainted = io.req.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)).orR || io.req.uop.cf_secret_access.getOrElse(false.B)
      val cf_pri_untainted   = !req.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)).orR && !req.uop.cf_secret_access.getOrElse(false.B)
      req.uop.cf_domain_id.foreach(_ := req.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) | io.req.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)))
      req.uop.cf_secret_access.foreach(_ := req.uop.cf_secret_access.getOrElse(false.B) || io.req.uop.cf_secret_access.getOrElse(false.B))
      when (cf_sec_req_tainted && cf_pri_untainted) {
        req.uop.cf_op_count_id.foreach(_ := io.req.uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)))
      }
      // [RESID] a secondary at a secret-derived address also makes this line's residency secret.
      // [R124GATE] these cf_sec_asec/aatk(_pc) writes are inside the G2 `if (ENABLE_IFT)` above, so
      // IFT off nothing drives the constant wires (no self-referencing when, no comb loop).
      when (io.req.cf_addr_sec.getOrElse(false.B) && !cf_sec_asec) {
        cf_sec_asec    := true.B
        cf_sec_asec_pc := io.req.uop.debug_pc(inflPcBitsCF + 1, 2)
      }
      when (io.req.cf_addr_atk.getOrElse(false.B) && !cf_sec_aatk) {   // [ATKRES]
        cf_sec_aatk    := true.B
        cf_sec_aatk_pc := io.req.uop.debug_pc(inflPcBitsCF + 1, 2)
      }
    }
  }

  def handle_pri_req(old_state: UInt): UInt = {
    val new_state = WireInit(old_state)
    grantack.valid := false.B
    refill_ctr := 0.U
    assert(rpq.io.enq.ready)
    // [ALLOCPROBE 2026-09-09] Which uop actually allocates the MSHR, and what domain does
    // it carry?  PROVEN downstream: an attacker STORE's fill carries domain=1 and the
    // store's op_count, while an attacker LOAD's fill carries domain=0 and its op_count
    // appears in NO fill -- even though the load misses, allocates, and commits with
    // domain=1.  Both cf_req_domain and cf_req_op_count read this same req.uop, so the
    // loss is here or upstream.  This prints the allocating uop so we can tell whether the
    // load allocates with domain=0 (lost before the MSHR) or never allocates a PRIMARY at
    // all (lands in the rpq as a secondary, where :158-167 explicitly clears cf fields).
    // [ALLOCPROBE2 2026-09-09] print mem_cmd and is_prefetch, NOT just isRead/isWrite.
    // TRAP this probe already caused once: M_XRD = "b00000" = 0, so a NullMicroOp (every
    // opless request -- writeback, probe, MSHR meta-read; dcache.scala:832/847/872) has
    // mem_cmd = 0 and reads back as isread=1.  The old probe therefore reported opless
    // requests as "loads with a stripped op_count", which is NOT a defect and nearly sent
    // us changing the rpq zeroing at :159/:172 for no reason.  mem_cmd disambiguates:
    // 0=M_XRD 1=M_XWR 2=M_PFR 3=M_PFW.  secret is printed too, because a fill with
    // secret=1 and oc=0 (seen in t38 pre-MEMIDFIX) had no owning op.  RESOLVED: same
    // root cause as the attacker-load bug; t38 now shows secret=1 oc=1169/1879.
    if (ENABLE_CF_DEBUG_PRINTF) {
      // [CFNOISE-COMMENTED] printf("\n[MSHRALLOC] domain=%d sec=%d oc=%d cmd=%d pf=%d isread=%d iswrite=%d\n",
        // [CFNOISE-COMMENTED] io.req.uop.cf_domain_id, io.req.uop.cf_secret_access, io.req.uop.cf_op_count_id,
        // [CFNOISE-COMMENTED] io.req.uop.mem_cmd, isPrefetch(io.req.uop.mem_cmd),
        // [CFNOISE-COMMENTED] isRead(io.req.uop.mem_cmd), isWrite(io.req.uop.mem_cmd))
    }
    req := io.req
    if (ENABLE_IFT) {   // [R124GATE]
      cf_sec_asec := false.B   // [RESID]
      cf_sec_aatk := false.B   // [ATKRES]
      cf_first_rp := !isPrefetch(io.req.uop.mem_cmd)
    }
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
    if (ENABLE_IFT) {   // [R124GATE] IFT off: the rpq uop's own bits, as before step 17
      io.resp.bits.uop.cf_mem_sec_dataflow.foreach(_ := rpq.io.deq.bits.uop.cf_mem_sec_dataflow.getOrElse(false.B) || cf_rp_vsec)   // [SECDEST]
      io.resp.bits.uop.cf_mem_dataflow_atk.foreach(_ := rpq.io.deq.bits.uop.cf_mem_dataflow_atk.getOrElse(false.B) || cf_rp_vatk)   // [ATKEVICT]
    }
    io.resp.bits.uop.cf_domain_id.foreach(_ := 0.U)   // [RDDOM] unchanged downstream (LSU uses the LDQ uop)
    io.resp.bits.cf_res_atk.foreach(_ := cf_rp_res_atk)   // [ATKEVICT]/[ATKRES] edge flags (per-MSHR)
    io.resp.bits.cf_res_esec.foreach(_ := cf_rp_res_esec)
    io.resp.bits.cf_res_v.foreach(_ := cf_rp_res_v)   // [RESID] line-buffer (miss) response edge
    io.resp.bits.cf_res_pc.foreach(_ := cf_rp_res_pc)
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
    io.meta_write.valid         := !cf_hold   // [D1FIXA]
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
    io.lb_read.valid       := !cf_hold   // [D1FIXA] refill.valid = lb_read.fire
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
    // [G9STMETA] (ruling 101) one bubble between two store replays of this MSHR.  A store
    // replay's metadata read-modify-write reads in s1 and writes in s2; a second store replay
    // to the same line one cycle later would read the SyncReadMem in the write cycle (old
    // data) and overwrite the first store's doubleword bit.  Store replays only follow a miss,
    // so the cost is one cycle per extra store to the missed line.
    if (ENABLE_IFT) {
      val st_rp_stall = cf_st_replay_last && isWrite(rpq.io.deq.bits.uop.mem_cmd)
      io.replay.valid  := rpq.io.deq.valid && !st_rp_stall
      rpq.io.deq.ready := io.replay.ready && !st_rp_stall
    }
    io.replay.bits.way_en    := req.way_en
    if (ENABLE_IFT) {   // [R124GATE] IFT off: the rpq uop's own bits, as before step 17
      io.replay.bits.uop.cf_mem_sec_dataflow.foreach(_ := rpq.io.deq.bits.uop.cf_mem_sec_dataflow.getOrElse(false.B) || cf_rp_vsec)   // [SECDEST]
      io.replay.bits.uop.cf_mem_dataflow_atk.foreach(_ := rpq.io.deq.bits.uop.cf_mem_dataflow_atk.getOrElse(false.B) || cf_rp_vatk)   // [ATKEVICT]
    }
    io.replay.bits.uop.cf_domain_id.foreach(_ := 0.U)   // [RDDOM] replays keep domain 0 in the D$ s2 checks
    io.replay.bits.cf_res_atk.foreach(_ := cf_rp_res_atk)   // [ATKEVICT]/[ATKRES]
    io.replay.bits.cf_res_esec.foreach(_ := cf_rp_res_esec)
    io.replay.bits.cf_res_v.foreach(_ := cf_rp_res_v)   // [RESID] replay (t_replay) response edge
    io.replay.bits.cf_res_pc.foreach(_ := cf_rp_res_pc)
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
    io.meta_write.valid         := !cf_hold   // [D1FIXA]
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
  io.resp.bits.edges := 0.U.asTypeOf(new IftEdgeList)   // Task 3.2 (D16): an uncached response has no D$ edge
  io.resp.bits.cf_res_v.foreach(_ := false.B)   // [RESID]
  io.resp.bits.cf_res_pc.foreach(_ := 0.U)
  io.resp.bits.cf_res_atk.foreach(_ := false.B)   // [ATKEVICT]
  io.resp.bits.cf_res_esec.foreach(_ := false.B)

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
    // [RESID] step 15 / [PRIALLOC] patch 32: req(w) fired AND allocated a PRIMARY cacheable MSHR
    // (fire && cacheable && !idx_match).  A secondary merges and evicts nothing; an uncacheable
    // request goes to an IOMSHR and touches no line.  Was cf_req_idx_match (fire && !it).
    val cf_req_alloc = Output(Vec(memWidth, Bool()))
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
    // [reconf-fix 2026-08-12] Active set mask (cf_dcache_active_sets-1), driven by the
    // dcache.  The per-set exclusion below MUST compare the PHYSICAL row a line occupies,
    // not the full untagged index: refills write `idx & dcache_set_mask` (dcache.scala
    // :1211 / :1208), so at a reduced set count two addresses differing only in the
    // dropped index bits land in the SAME row while an unmasked compare calls them
    // different sets.  Both then allocate, both refill the same row, and one line's tag
    // ends up over the other's data -- a silent wrong-data hit.
    val dcache_set_mask = Input(UInt(idxBits.W))
    // [D1FIXA] dcache.scala cf_pend_hold, to every cacheable MSHR (see BoomMSHR.io.cf_ift_hold)
    val cf_ift_hold = Input(Bool())
    // [NACKRES 2026-10-13, series-r step 16, owner ruling 111] registered: some valid MSHR
    // holds a secret-ADDRESS miss (primary req.cf_addr_sec or a merged secondary's), with the
    // PC[13:2] of that access (lowest MSHR wins).  The LSU tags a load the D$ nacks while
    // this is set (ldq cf_nack_res).  The WB unit drains the line such an MSHR evicts while
    // the MSHR is still valid, so WB occupancy is covered by the same OR.
    val cf_asec_busy = Output(Valid(UInt(inflPcBitsCF.W)))
    // [NACKATK] patch 44 (r111 extended, owner r173 (3)): registered: some valid MSHR holds an
    // ATTACKER-INFLUENCED request (primary attacker domain, or an attacker-tainted address of the
    // primary or a merged secondary: cf_req_addr_atk).  Its PC rides on cf_asec_busy.bits, which
    // is now the PC of the nack's blocking access, secret-first.  IFT only.
    val cf_aatk_busy = if (ENABLE_IFT) Some(Output(Bool())) else None
    // corefuzzing: fill-completion event — fires when an MSHR meta_write completes
    val cf_meta_write_fill = Output(Valid(new Bundle {
      val idx      = UInt(idxBits.W)
      val way_en   = UInt(nWays.W)
      val domain   = UInt(1.W)
      val op_count = UInt(uopIDCounterWidthCF.W)   // kept for the [FILL] printf only
      val pc12     = UInt(inflPcBitsCF.W)          // [PCPROV] what the line metadata stores
      val secret   = Bool()
      val addr_sec = Bool()                        // [RESID] secret-address fill (ift_res_meta)
      val addr_atk = Bool()                        // [ATKRES] attacker-address fill (ift_res_meta)
      val res_pc   = UInt(inflPcBitsCF.W)          // [RESID] PC[13:2] of that access
      // [TAGPROBE 2026-09-09] the line's TAG.  Needed because every taint-field-based
      // test for "did this access miss?" is circular when the field under investigation
      // is the one being stripped: a fill whose op_count was lost cannot be matched to
      // the op that caused it BY op_count.  The tag identifies the line directly and is
      // independent of every cf_* field.
      val tag      = UInt(cfTagBits.W)
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
    mshr.io.cf_ift_hold     := io.cf_ift_hold   // [D1FIXA]

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

  // [NOPENDOVR] patch 32: pending_way_mask (the global union of pending ways, which the D$ used
  // to move a victim off any pending way) is gone.  Two MSHRs can only collide on a way if they
  // own the same physical row, which pri_val's !idx_match (masked-row compare, reconf-fix
  // 2026-08-12) forbids.  This simulation check states that invariant: no two live MSHRs share
  // a masked row.  (s_prefetch holds the row too: idx.valid covers it.)
  for (i <- 0 until cfg.nMSHRs; j <- i + 1 until cfg.nMSHRs) {
    assert(!(mshrs(i).io.idx.valid && mshrs(j).io.idx.valid &&
             (mshrs(i).io.idx.bits & io.dcache_set_mask) === (mshrs(j).io.idx.bits & io.dcache_set_mask)),
           "D$ MSHR file: two live MSHRs own the same row")
  }

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
  io.cf_meta_write_fill.bits.pc12     := Mux1H(UIntToOH(meta_write_arb.io.chosen),
                                           VecInit(mshrs.map(_.io.cf_req_pc12)))   // [PCPROV]
  io.cf_meta_write_fill.bits.secret   := Mux1H(UIntToOH(meta_write_arb.io.chosen),
                                           VecInit(mshrs.map(_.io.cf_req_secret)))
  io.cf_meta_write_fill.bits.addr_sec := Mux1H(UIntToOH(meta_write_arb.io.chosen),
                                           VecInit(mshrs.map(_.io.cf_req_addr_sec)))   // [RESID]
  io.cf_meta_write_fill.bits.addr_atk := Mux1H(UIntToOH(meta_write_arb.io.chosen),
                                           VecInit(mshrs.map(_.io.cf_req_addr_atk)))   // [ATKRES]
  io.cf_meta_write_fill.bits.res_pc   := Mux1H(UIntToOH(meta_write_arb.io.chosen),
                                           VecInit(mshrs.map(_.io.cf_req_res_pc)))
  // [NACKRES] step 16: nMSHRs-input OR + priority PC select, registered (one cycle old).
  // [NACKPF] DF-m55 (r140): a parked s_prefetch MSHR is not a miss in progress (it is allocatable,
  // req_pri_rdy=1), so it must not hold cf_asec_busy; io.way.valid already excludes s_prefetch.
  // Limitation: the one same-set different-tag nack a parked secret prefetch causes (in the cycle
  // clear_prefetch releases it) gets no nack edge.
  val cf_asec_live = mshrs.map(m => m.io.way.valid && m.io.cf_req_addr_sec)
  io.cf_asec_busy.valid := RegNext(cf_asec_live.reduce(_ || _), false.B)
  // [NACKATK] patch 44 (r111 extended, r173 (3)): the attacker twin of cf_asec_live (same
  // way.valid gate, so a parked s_prefetch MSHR is excluded as in [NACKPF]).  The registered PC is
  // shared: every secret MSHR ranks above every attacker MSHR (secret-first, lowest index within
  // each), so .bits is unchanged whenever .valid is set; it carries the attacker MSHR's PC only
  // when no secret MSHR is live.  The attacker MSHR's PC is its PRIMARY's PC[13:2] (cf_req_pc12,
  // the access that holds the MSHR).  Registered, one cycle old, like cf_asec_busy.
  if (ENABLE_IFT) {
    val cf_aatk_live = mshrs.map(m => m.io.way.valid && (m.io.cf_req_domain(0) || m.io.cf_req_addr_atk))
    io.cf_aatk_busy.get := RegNext(cf_aatk_live.reduce(_ || _), false.B)
    io.cf_asec_busy.bits  := RegNext(PriorityMux(cf_asec_live ++ cf_aatk_live :+ true.B,
                                                 mshrs.map(_.io.cf_req_res_pc) ++ mshrs.map(_.io.cf_req_pc12) :+
                                                 0.U(inflPcBitsCF.W)))
  } else {
    io.cf_asec_busy.bits  := RegNext(PriorityMux(cf_asec_live :+ true.B,
                                                 mshrs.map(_.io.cf_req_res_pc) :+ 0.U(inflPcBitsCF.W)))
  }

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
    // [PRIALLOC] fire(w) implies w === req_idx, so `cacheable` (decoded from req = io.req(req_idx))
    // is lane w's.  Ends only at the D$ replacement / ift_evf write ports, not on the W family.
    io.cf_req_alloc(w)   := io.req(w).fire && cacheable && !idx_match(w)
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
  // [PFTAINT] step 17 (owner answer Q7): the triggering MSHR's domain, line mark and PC, muxed
  // and registered exactly like req_addr (one-hot mux of nMSHRs, no new level on req_addr).
  // [R124GATE] patch 26 (ruling 124): the four [PFTAINT] flops exist only with ENABLE_IFT.
  if (ENABLE_IFT) {
    prefetcher.io.req_dom     := RegNext(Mux1H(commit_vals, mshrs.map(_.io.cf_req_domain)))
    prefetcher.io.req_asec    := RegNext(Mux1H(commit_vals, mshrs.map(_.io.cf_req_addr_sec)))
    prefetcher.io.req_aatk    := RegNext(Mux1H(commit_vals, mshrs.map(_.io.cf_req_addr_atk)))   // [ATKRES]
    prefetcher.io.req_pc12    := RegNext(Mux1H(commit_vals, mshrs.map(m =>
                                   Mux(m.io.cf_req_addr_sec || m.io.cf_req_addr_atk, m.io.cf_req_res_pc, m.io.cf_req_pc12))))
  } else {
    prefetcher.io.req_dom     := 0.U
    prefetcher.io.req_asec    := false.B
    prefetcher.io.req_aatk    := false.B
    prefetcher.io.req_pc12    := 0.U
  }
  prefetcher.io.req_coh       := RegNext(Mux1H(commit_vals, commit_cohs))
}
