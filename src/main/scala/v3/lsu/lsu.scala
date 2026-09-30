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
import boom.v3.exu.{BrUpdateInfo, Exception, FuncUnitResp, CommitSignals, ExeUnitResp}

// fore corefuzzing - SpeculativePRintf and Sext
import boom.v3.util.{BoolToChar, AgePriorityEncoder, IsKilledByBranch, GetNewBrMask, WrapInc, IsOlder, UpdateBrMask, SpeculativePrintf}
import boom.v3.util.{Sext, appendModuleTag, addInfluencer, addInfluencerBatch, InfluencerCandidate, SatDropped}

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
{
  val addr  = UInt(coreMaxAddrBits.W)
  val data  = Bits(coreDataBits.W)
  val is_hella = Bool() // Is this the hellacache req? If so this is not tracked in LDQ or STQ
}

class BoomDCacheResp(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomUOP
{
  val data = Bits(coreDataBits.W)
  val is_hella = Bool()
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

  val ldq_full    = Output(Vec(coreWidth, Bool()))
  val stq_full    = Output(Vec(coreWidth, Bool()))
  // corefuzzing: op_count, domain, and secret-status of head entry for stall attribution
  val ldq_head_op_count  = Output(UInt(uopIDCounterWidthCF.W))
  val stq_head_op_count  = Output(UInt(uopIDCounterWidthCF.W))
  val ldq_head_domain    = Output(UInt(1.W))   // cf_domain_id of ldq_head entry
  val stq_head_domain    = Output(UInt(1.W))   // cf_domain_id of stq_head entry
  val ldq_head_is_secret = Output(Bool())      // ldq_head had s_acc=1 or s_prop=1
  val stq_head_is_secret = Output(Bool())      // stq_head had s_acc=1 or s_prop=1
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
  // corefuzzing: direct ROB s_acc update at TLB stage — sets cf_secret_access in the ROB entry
  // immediately when the effective address is found to be in the secret range, without waiting
  // for the dcache response.  Enables correct s_acc in [FLUSH] for speculative secret loads.
  val cf_s_acc_rob_upd = Output(Vec(memWidth, Valid(new CF_SAccUpdate)))
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

  // Speculatively safe load (barring memory ordering failure)
  val clr_unsafe      = Output(Vec(memWidth, Valid(UInt(robAddrSz.W))))

  // Tell the DCache to clear prefetches/speculating misses
  val fence_dmem   = Input(Bool())

  // Speculatively tell the IQs that we'll get load data back next cycle
  val spec_ld_wakeup = Output(Vec(memWidth, Valid(UInt(maxPregSz.W))))
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
  val cf_dcache_set_conf  = Input(UInt(dcacheParamsWidthCF.W))
  val cf_dcache_way_conf  = Input(UInt(dcacheParamsWidthCF.W))
  val cf_dcache_repl_conf = Input(UInt(dcacheParamsWidthCF.W))
  // [reconf-fix Phase C] 0xbc4 invalidate walker trigger + 0xbcd[17] busy
  val cf_cachectl_wen     = Input(Bool())
  val cf_cachectl_wdata   = Input(UInt(2.W))
  val cf_dcache_wipe_busy = Output(Bool())

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
{
  val addr                = Valid(UInt(coreMaxAddrBits.W))
  val addr_is_virtual     = Bool() // Virtual address, we got a TLB miss
  val addr_is_uncacheable = Bool() // Uncacheable, wait until head of ROB to execute
  // corefuzzing: taint of the ADDRESS operand (rs1) only -- NOT the uop's aggregate
  // cf_secret_propagation, which also accumulates forwarding and observability taint.
  val addr_is_secret      = Bool()
  // corefuzzing: ATTACKER taint of the address operand.  Same rule, same source.
  val addr_is_atk         = Bool()

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
  val addr_is_secret      = Bool()
  val addr_is_atk         = Bool()
  // corefuzzing: taint of the store's DATA operand (rs2) only.  Kept separate from
  // the uop's aggregate cf_secret_propagation, which also carries fetch/observability
  // taint and says nothing about the VALUE being stored.
  val data_is_secret      = Bool()
  // corefuzzing: ATTACKER taint of the store's DATA operand (rs2).
  val data_is_atk         = Bool()
  // corefuzzing: was this store's ADDRESS inside the secret range?  Recorded at TLB
  // time so the s_tx decision can be made later, at clr_bsy, when the DATA is known.
  val addr_in_secret      = Bool()
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
  var ld_enq_idx = ldq_tail
  var st_enq_idx = stq_tail

  val stq_nonempty = (0 until numStqEntries).map{ i => stq(i).valid }.reduce(_||_) =/= 0.U

  var ldq_full = Bool()
  var stq_full = Bool()

  for (w <- 0 until coreWidth)
  {
    ldq_full = WrapInc(ld_enq_idx, cf_ldq_active) === ldq_head
    io.core.ldq_full(w)    := ldq_full
    io.core.dis_ldq_idx(w) := ld_enq_idx

    stq_full = WrapInc(st_enq_idx, cf_stq_active) === stq_head
    io.core.stq_full(w)    := stq_full
    io.core.dis_stq_idx(w) := st_enq_idx

    val dis_ld_val = io.core.dis_uops(w).valid && io.core.dis_uops(w).bits.uses_ldq && !io.core.dis_uops(w).bits.exception
    val dis_st_val = io.core.dis_uops(w).valid && io.core.dis_uops(w).bits.uses_stq && !io.core.dis_uops(w).bits.exception
    when (dis_ld_val)
    {
      ldq(ld_enq_idx).valid                := true.B
      ldq(ld_enq_idx).bits.uop             := io.core.dis_uops(w).bits
      // corefuzzing: stamp ldqTagCF into cf_fu_bitmap at LDQ enqueue
      ldq(ld_enq_idx).bits.uop.cf_fu_bitmap := io.core.dis_uops(w).bits.cf_fu_bitmap | (1.U << ldqTagCF.U)
      // IFT LUT optimization (Change 8): zero cf_* fields not read by LSU logic.
      // PRESERVED: cf_op_count_id, cf_domain_id (head reads), cf_secret_access,
      //   cf_secret_propagation (head + TLB update), cf_secret_transmission (TLB update),
      //   cf_fu_bitmap (set above, merged at TLB+commit), cf_influencer_list + cf_infl_dropped
      //   (addInfluencer calls in DTLB/ordering-violation/STL-forward paths).
      // rob_uop holds the dispatch-time IFT state independently.
      ldq(ld_enq_idx).bits.uop.cf_speculated               := false.B
      ldq(ld_enq_idx).bits.uop.cf_attacker_influence       := false.B
      ldq(ld_enq_idx).bits.uop.cf_single_step              := false.B
      ldq(ld_enq_idx).bits.uop.cf_src_tainted              := false.B
      ldq(ld_enq_idx).bits.uop.cf_spec_branch_is_atk       := false.B
      ldq(ld_enq_idx).bits.uop.cf_atk_branch_ctr       := 0.U
      ldq(ld_enq_idx).bits.uop.cf_sec_branch_ctr       := 0.U
      ldq(ld_enq_idx).bits.uop.cf_spec_branch_op_id        := 0.U
      ldq(ld_enq_idx).bits.uop.cf_spec_branch_is_secret    := false.B
      ldq(ld_enq_idx).bits.uop.cf_cntd_valid               := false.B
      ldq(ld_enq_idx).bits.uop.cf_cntd_winner_op           := 0.U
      ldq(ld_enq_idx).bits.uop.cf_cntd_winner_atk          := false.B
      ldq(ld_enq_idx).bits.uop.cf_cntd_winner_sec          := false.B
      ldq(ld_enq_idx).bits.uop.cf_cntd_deny_count          := 0.U
      ldq(ld_enq_idx).bits.youngest_stq_idx  := st_enq_idx
      ldq(ld_enq_idx).bits.st_dep_mask     := next_live_store_mask

      ldq(ld_enq_idx).bits.addr.valid      := false.B
      ldq(ld_enq_idx).bits.addr_is_secret  := false.B
      ldq(ld_enq_idx).bits.addr_is_atk    := false.B
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
      // corefuzzing: stamp STQ/DTLB/DCache bits at STQ enqueue for non-fence stores
      // All committed non-fence stores in BOOM go through DTLB (STA) then DCache (store_commit).
      // Pre-mark these bits here so the ROB sees them at commit time via clr_bsy_cf_bitmap.
      val store_bitmap_extra = (1.U << stqTagCF.U) |
        Mux(!io.core.dis_uops(w).bits.is_fence,
            (1.U << dtlbTagCF.U) | (1.U << dcacheTagCF.U), 0.U)
      stq(st_enq_idx).bits.uop.cf_fu_bitmap := io.core.dis_uops(w).bits.cf_fu_bitmap | store_bitmap_extra
      // IFT LUT optimization (Change 8): same zeroing as LDQ above.
      stq(st_enq_idx).bits.uop.cf_speculated               := false.B
      stq(st_enq_idx).bits.uop.cf_attacker_influence       := false.B
      stq(st_enq_idx).bits.uop.cf_single_step              := false.B
      stq(st_enq_idx).bits.uop.cf_src_tainted              := false.B
      stq(st_enq_idx).bits.uop.cf_spec_branch_is_atk       := false.B
      stq(st_enq_idx).bits.uop.cf_atk_branch_ctr       := 0.U
      stq(st_enq_idx).bits.uop.cf_sec_branch_ctr       := 0.U
      stq(st_enq_idx).bits.uop.cf_spec_branch_op_id        := 0.U
      stq(st_enq_idx).bits.uop.cf_spec_branch_is_secret    := false.B
      stq(st_enq_idx).bits.uop.cf_cntd_valid               := false.B
      stq(st_enq_idx).bits.uop.cf_cntd_winner_op           := 0.U
      stq(st_enq_idx).bits.uop.cf_cntd_winner_atk          := false.B
      stq(st_enq_idx).bits.uop.cf_cntd_winner_sec          := false.B
      stq(st_enq_idx).bits.uop.cf_cntd_deny_count          := 0.U
      stq(st_enq_idx).bits.addr.valid := false.B
      stq(st_enq_idx).bits.addr_is_secret := false.B
      stq(st_enq_idx).bits.addr_is_atk   := false.B
      stq(st_enq_idx).bits.data_is_secret := false.B
      stq(st_enq_idx).bits.data_is_atk   := false.B
      stq(st_enq_idx).bits.addr_in_secret := false.B
      stq(st_enq_idx).bits.data.valid := false.B
      stq(st_enq_idx).bits.committed  := false.B
      stq(st_enq_idx).bits.succeeded  := false.B

      assert (st_enq_idx === io.core.dis_uops(w).bits.stq_idx, "[lsu] mismatch enq store tag.")
      assert (!stq(st_enq_idx).valid, "[lsu] Enqueuing uop is overwriting stq entries")
    }

    ld_enq_idx = Mux(dis_ld_val, WrapInc(ld_enq_idx, cf_ldq_active),
                                 ld_enq_idx)

    next_live_store_mask = Mux(dis_st_val, next_live_store_mask | (1.U << st_enq_idx),
                                           next_live_store_mask)
    st_enq_idx = Mux(dis_st_val, WrapInc(st_enq_idx, cf_stq_active),
                                 st_enq_idx)

    assert(!(dis_ld_val && dis_st_val), "A UOP is trying to go into both the LDQ and the STQ")
  }

  ldq_tail := ld_enq_idx
  stq_tail := st_enq_idx

  // corefuzzing: expose head entry op_count, domain, and secret-status for stall attribution
  io.core.ldq_head_op_count  := Mux(ldq(ldq_head).valid, ldq(ldq_head).bits.uop.cf_op_count_id, 0.U)
  io.core.stq_head_op_count  := Mux(stq(stq_head).valid, stq(stq_head).bits.uop.cf_op_count_id, 0.U)
  io.core.ldq_head_domain    := Mux(ldq(ldq_head).valid, ldq(ldq_head).bits.uop.cf_domain_id, 0.U)
  io.core.stq_head_domain    := Mux(stq(stq_head).valid, stq(stq_head).bits.uop.cf_domain_id, 0.U)
  io.core.ldq_head_is_secret := Mux(ldq(ldq_head).valid,
    ldq(ldq_head).bits.uop.cf_secret_access || ldq(ldq_head).bits.uop.cf_secret_propagation, false.B)
  io.core.stq_head_is_secret := Mux(stq(stq_head).valid,
    stq(stq_head).bits.uop.cf_secret_access || stq(stq_head).bits.uop.cf_secret_propagation, false.B)
  io.core.ldq_head_valid     := ldq(ldq_head).valid
  io.core.stq_head_valid     := stq(stq_head).valid
  // [HOLFIX] `succeeded` = the D$ has acked; until then the head holds the line.
  io.core.ldq_head_blocked   := ldq(ldq_head).valid && !ldq(ldq_head).bits.succeeded
  io.core.stq_head_blocked   := stq(stq_head).valid && !stq(stq_head).bits.succeeded
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
    dtlb.io.req_domain(w)           := exe_tlb_uop(w).cf_domain_id
    dtlb.io.req_secret(w)           := exe_tlb_uop(w).cf_secret_propagation || exe_tlb_uop(w).cf_secret_access
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
  // corefuzzing: `in_secret` is computed inside the TLB-stage loop below, but the STQ
  // address write lives in a LATER, separate for(w) block.  Export it per-way so the
  // store can record whether its ADDRESS landed in the secret range (Case A).
  val exe_tlb_in_secret = Wire(Vec(memWidth, Bool()))
  for (w <- 0 until memWidth) {
    // Stage 1: bitmap + secret_access, based on exe_tlb_uop (register-backed, no feedback)
    val uop_tlb_base = WireInit(exe_tlb_uop(w))
    uop_tlb_base.cf_fu_bitmap := exe_tlb_uop(w).cf_fu_bitmap | (1.U << dtlbTagCF.U)
    val secret_range_valid = io.core.cf_secret_end_addr =/= io.core.cf_secret_start_addr
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
    uop_tlb_base.cf_secret_access := exe_tlb_uop(w).cf_secret_access || is_secret_load
    // corefuzzing: TLB-stage FTQ update — mark this fetch packet as secret as soon as s_acc is known.
    // Fires speculatively (even for squashed memory ops) to capture transient secret access flows.
    io.core.cf_secret_ftq_updates(w).valid := exe_tlb_valid(w) && is_secret_load
    io.core.cf_secret_ftq_updates(w).bits  := exe_tlb_uop(w).ftq_idx
    // Direct ROB update: set s_acc immediately when address lands in secret range.
    // op_count_id is included to guard against ROB slot reuse: if this TLB-stage
    // update arrives after the original instruction was squashed and the slot reused,
    // the ROB validates op_count_id before applying cf_secret_access.
    io.core.cf_s_acc_rob_upd(w).valid               := exe_tlb_valid(w) && is_secret_load
    io.core.cf_s_acc_rob_upd(w).bits.rob_idx        := exe_tlb_uop(w).rob_idx
    io.core.cf_s_acc_rob_upd(w).bits.op_count_id    := exe_tlb_uop(w).cf_op_count_id
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
    val data_reg_secret  = exe_tlb_uop(w).cf_secret_propagation || prs1_live_secret || prs2_live_secret
    // s_tx Case A: store carrying secret-dependent data to a non-secret memory address.
    //   The secret escapes to attacker-accessible memory (anything outside the secret range).
    exe_tlb_in_secret(w) := in_secret
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
                           Mux(will_fire_load_retry(w),   ldq_retry_e.bits.addr_is_secret,
                           Mux(will_fire_sta_retry(w),    stq_retry_e.bits.addr_is_secret,
                                                          false.B)))
    val is_secret_addr_load = exe_tlb_uop(w).uses_ldq && addr_reg_secret && !in_secret
    // is_secret_bearing_store REMOVED here: it was computed from the aggregate and the
    // store's data is not knowable at TLB time.  The store's s_tx is now raised at
    // clr_bsy (Case A above).  Leaving it here would let the aggregate back in via
    // rob.scala:1037, which ORs the uop bit into the ROB entry.
    uop_tlb_base.cf_secret_transmission := exe_tlb_uop(w).cf_secret_transmission ||
                                           is_secret_addr_load
    // Gap 1 fix: emit direct ROB write-back for s_tx at TLB time.
    // Without this, a squashed probe load's ROB entry never shows s_tx=1 because the
    // pipeline uop wire is discarded when the instruction is killed before commit.
    // op_count_id guard (enforced in rob.scala handler) prevents stale-slot updates.
    io.core.cf_s_tx_rob_upd(w).valid            := exe_tlb_valid(w) && is_secret_addr_load
    io.core.cf_s_tx_rob_upd(w).bits.rob_idx     := exe_tlb_uop(w).rob_idx
    io.core.cf_s_tx_rob_upd(w).bits.op_count_id := exe_tlb_uop(w).cf_op_count_id
    // Stage 2: DTLB domain mismatch → INFL_DTLB_STATE (reads uop_tlb_base, writes uop_tlb_final)
    val uop_tlb_final = WireInit(uop_tlb_base)
    when (dtlb.io.resp_domain_mismatch(w) || dtlb.io.resp_secret_mismatch(w)) {
      // DTLB mismatch: victim using a TLB entry cached by attacker or secret instruction
      uop_tlb_final := addInfluencer(uop_tlb_base, 0.U, INFL_DTLB_STATE.U,
        is_atk    = dtlb.io.resp_domain_mismatch(w) && uop_tlb_base.cf_domain_id === 0.U,
        is_secret = dtlb.io.resp_secret_mismatch(w))
    }
    exe_tlb_uop_cf(w) := uop_tlb_final
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
  // [reconf-fix Phase C] invalidate-walker trigger down, busy back up
  io.dmem.cf_cachectl_wen      := io.core.cf_cachectl_wen
  io.dmem.cf_cachectl_wdata    := io.core.cf_cachectl_wdata
  io.core.cf_dcache_wipe_busy  := io.dmem.cf_dcache_wipe_busy

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
          printf("[LSU] lsu out valid and dmem.req.bits(%d).bits.addr - 0x%x\n", w.U, io.dmem.req.bits(w).bits.addr)
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

    io.dmem.s1_kill(w) := false.B

    when (will_fire_load_incoming(w)) {
      // if (ENABLE_CF_DEBUG_PRINTF) cf_dreq_src(w)         := 1.U
      dmem_req(w).valid      := !exe_tlb_miss(w) && !exe_tlb_uncacheable(w)
      dmem_req(w).bits.addr  := exe_tlb_paddr(w)
      dmem_req(w).bits.uop   := exe_tlb_uop_cf(w) // corefuzzing: use IFT-tagged uop

      s0_executing_loads(ldq_incoming_idx(w)) := dmem_req_fire(w)
      assert(!ldq_incoming_e(w).bits.executed)
    } .elsewhen (will_fire_load_retry(w)) {
      // if (ENABLE_CF_DEBUG_PRINTF) cf_dreq_src(w)         := 2.U
      dmem_req(w).valid      := !exe_tlb_miss(w) && !exe_tlb_uncacheable(w)
      dmem_req(w).bits.addr  := exe_tlb_paddr(w)
      dmem_req(w).bits.uop   := exe_tlb_uop_cf(w) // corefuzzing: use IFT-tagged uop

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
      dmem_req(w).bits.uop.cf_attacker_influence := stq_commit_e.bits.data_is_atk

      stq_execute_head                     := Mux(dmem_req_fire(w),
                                                WrapInc(stq_execute_head, cf_stq_active),
                                                stq_execute_head)

      stq(stq_execute_head).bits.succeeded := false.B
    } .elsewhen (will_fire_load_wakeup(w)) {
      // if (ENABLE_CF_DEBUG_PRINTF) cf_dreq_src(w)         := 4.U
      dmem_req(w).valid      := true.B
      dmem_req(w).bits.addr  := ldq_wakeup_e.bits.addr.bits
      dmem_req(w).bits.uop   := ldq_wakeup_e.bits.uop

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
        ldq(ldq_idx).bits.addr_is_secret    := exe_req(w).bits.secret  // rs1 taint at the AGU
        ldq(ldq_idx).bits.addr_is_atk       := exe_req(w).bits.taint_atk
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
      ldq(ldq_idx).bits.uop.cf_secret_propagation  :=
        ldq(ldq_idx).bits.uop.cf_secret_propagation || exe_tlb_uop_cf(w).cf_secret_propagation
      ldq(ldq_idx).bits.uop.cf_secret_access       :=
        ldq(ldq_idx).bits.uop.cf_secret_access || exe_tlb_uop_cf(w).cf_secret_access
      ldq(ldq_idx).bits.uop.cf_secret_transmission :=
        ldq(ldq_idx).bits.uop.cf_secret_transmission || exe_tlb_uop_cf(w).cf_secret_transmission
      ldq(ldq_idx).bits.uop.cf_fu_bitmap           :=
        ldq(ldq_idx).bits.uop.cf_fu_bitmap | exe_tlb_uop_cf(w).cf_fu_bitmap

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
        stq(stq_idx).bits.addr_is_secret := exe_req(w).bits.secret    // rs1 taint at the AGU
        stq(stq_idx).bits.addr_is_atk    := exe_req(w).bits.taint_atk
      }
      stq(stq_idx).bits.addr_in_secret := exe_tlb_in_secret(w)
      // corefuzzing: write IFT flags computed in TLB stage into STQ so clr_bsy carries them to ROB
      // same omission on the store path (see the LDQ note above)
      stq(stq_idx).bits.uop.cf_secret_propagation  :=
        stq(stq_idx).bits.uop.cf_secret_propagation || exe_tlb_uop_cf(w).cf_secret_propagation
      stq(stq_idx).bits.uop.cf_secret_transmission :=
        stq(stq_idx).bits.uop.cf_secret_transmission || exe_tlb_uop_cf(w).cf_secret_transmission
      stq(stq_idx).bits.uop.cf_secret_access :=
        stq(stq_idx).bits.uop.cf_secret_access || exe_tlb_uop_cf(w).cf_secret_access

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
      stq(sidx).bits.uop.cf_secret_propagation := stq(sidx).bits.uop.cf_secret_propagation ||
        Mux(will_fire_std_incoming(w) || will_fire_stad_incoming(w),
            exe_req(w).bits.data_secret,   // rs2 taint, NOT .secret (that is the ADDRESS)
            io.core.fp_stdata.bits.secret)
      // and record it on its own, so consumers that mean "the VALUE is secret" do not
      // have to read the aggregate uop bit (see stl_store_is_secret below).
      stq(sidx).bits.data_is_secret :=
        Mux(will_fire_std_incoming(w) || will_fire_stad_incoming(w),
            exe_req(w).bits.data_secret,
            io.core.fp_stdata.bits.secret)
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
      stq(sidx).bits.data_is_atk :=
        Mux(will_fire_std_incoming(w) || will_fire_stad_incoming(w),
            exe_req(w).bits.data_taint_atk,
            io.core.fp_stdata.bits.taint_atk)
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

  // Task 1: Clr ROB busy bit
  val clr_bsy_valid    = RegInit(widthMap(w => false.B))
  val clr_bsy_rob_idx  = Reg(Vec(memWidth, UInt(robAddrSz.W)))
  val clr_bsy_brmask   = Reg(Vec(memWidth, UInt(maxBrCount.W)))
  // corefuzzing: carry accumulated STQ uop cf_fu_bitmap to ROB so stores have dtlb+dcache bits
  val clr_bsy_cf_bmap  = Reg(Vec(memWidth, UInt(numModules.W)))
  // corefuzzing: carry cf_secret_transmission from STQ uop to ROB
  val clr_bsy_cf_stx_r = RegInit(widthMap(w => false.B))
  val clr_bsy_cf_sprop_r = RegInit(widthMap(w => false.B))

  for (w <- 0 until memWidth) {
    clr_bsy_valid      (w) := false.B
    clr_bsy_rob_idx    (w) := 0.U
    clr_bsy_brmask     (w) := 0.U
    clr_bsy_cf_bmap    (w) := 0.U
    clr_bsy_cf_stx_r   (w) := false.B
    clr_bsy_cf_sprop_r (w) := false.B


    when (fired_stad_incoming(w)) {
      clr_bsy_valid      (w) := mem_stq_incoming_e(w).valid           &&
                               !mem_tlb_miss(w)                       &&
                               !mem_stq_incoming_e(w).bits.uop.is_amo &&
                               !IsKilledByBranch(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      clr_bsy_rob_idx    (w) := mem_stq_incoming_e(w).bits.uop.rob_idx
      clr_bsy_brmask     (w) := GetNewBrMask(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      // corefuzzing: capture live stq cf_fu_bitmap (updated in EXE stage with dtlb/dcache bits)
      clr_bsy_cf_bmap    (w) := stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.uop.cf_fu_bitmap
      // Case A (2026-09-03): decide the store's s_tx HERE, not at the TLB stage.
      // clr_bsy fires once BOTH halves of the store have landed, so data_is_secret is
      // valid; the TLB stage could not know it for a split STA/STD, which is why the
      // old code fell back to the uop's aggregate cf_secret_propagation and marked any
      // store inside a tainted region as a transmission (e.g. `sd s0,40(sp)`, x507,
      // spilling a provably CLEAN s0).  A transmission is secret DATA reaching a
      // non-secret address -- both facts now live in the entry.
      clr_bsy_cf_sprop_r (w) := stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.data_is_secret
      clr_bsy_cf_stx_r   (w) := stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.data_is_secret &&
                               !stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.addr_in_secret
    } .elsewhen (fired_sta_incoming(w)) {
      clr_bsy_valid      (w) := mem_stq_incoming_e(w).valid            &&
                                mem_stq_incoming_e(w).bits.data.valid  &&
                               !mem_tlb_miss(w)                        &&
                               !mem_stq_incoming_e(w).bits.uop.is_amo  &&
                               !IsKilledByBranch(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      clr_bsy_rob_idx    (w) := mem_stq_incoming_e(w).bits.uop.rob_idx
      clr_bsy_brmask     (w) := GetNewBrMask(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      clr_bsy_cf_bmap    (w) := stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.uop.cf_fu_bitmap
      // Case A (2026-09-03): decide the store's s_tx HERE, not at the TLB stage.
      // clr_bsy fires once BOTH halves of the store have landed, so data_is_secret is
      // valid; the TLB stage could not know it for a split STA/STD, which is why the
      // old code fell back to the uop's aggregate cf_secret_propagation and marked any
      // store inside a tainted region as a transmission (e.g. `sd s0,40(sp)`, x507,
      // spilling a provably CLEAN s0).  A transmission is secret DATA reaching a
      // non-secret address -- both facts now live in the entry.
      clr_bsy_cf_sprop_r (w) := stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.data_is_secret
      clr_bsy_cf_stx_r   (w) := stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.data_is_secret &&
                               !stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.addr_in_secret
    } .elsewhen (fired_std_incoming(w)) {
      clr_bsy_valid      (w) := mem_stq_incoming_e(w).valid                 &&
                                mem_stq_incoming_e(w).bits.addr.valid       &&
                               !mem_stq_incoming_e(w).bits.addr_is_virtual  &&
                               !mem_stq_incoming_e(w).bits.uop.is_amo       &&
                               !IsKilledByBranch(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      clr_bsy_rob_idx    (w) := mem_stq_incoming_e(w).bits.uop.rob_idx
      clr_bsy_brmask     (w) := GetNewBrMask(io.core.brupdate, mem_stq_incoming_e(w).bits.uop)
      clr_bsy_cf_bmap    (w) := stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.uop.cf_fu_bitmap
      // Case A (2026-09-03): decide the store's s_tx HERE, not at the TLB stage.
      // clr_bsy fires once BOTH halves of the store have landed, so data_is_secret is
      // valid; the TLB stage could not know it for a split STA/STD, which is why the
      // old code fell back to the uop's aggregate cf_secret_propagation and marked any
      // store inside a tainted region as a transmission (e.g. `sd s0,40(sp)`, x507,
      // spilling a provably CLEAN s0).  A transmission is secret DATA reaching a
      // non-secret address -- both facts now live in the entry.
      clr_bsy_cf_sprop_r (w) := stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.data_is_secret
      clr_bsy_cf_stx_r   (w) := stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.data_is_secret &&
                               !stq(mem_stq_incoming_e(w).bits.uop.stq_idx).bits.addr_in_secret
    } .elsewhen (fired_sfence(w)) {
      clr_bsy_valid      (w) := (w == 0).B // SFence proceeds down all paths, only allow one to clr the rob
      clr_bsy_rob_idx    (w) := mem_incoming_uop(w).rob_idx
      clr_bsy_brmask     (w) := GetNewBrMask(io.core.brupdate, mem_incoming_uop(w))
      clr_bsy_cf_bmap    (w) := mem_incoming_uop(w).cf_fu_bitmap
      clr_bsy_cf_stx_r   (w) := false.B
    clr_bsy_cf_sprop_r (w) := false.B
    } .elsewhen (fired_sta_retry(w)) {
      clr_bsy_valid      (w) := mem_stq_retry_e.valid            &&
                                mem_stq_retry_e.bits.data.valid  &&
                               !mem_tlb_miss(w)                  &&
                               !mem_stq_retry_e.bits.uop.is_amo  &&
                               !IsKilledByBranch(io.core.brupdate, mem_stq_retry_e.bits.uop)
      clr_bsy_rob_idx    (w) := mem_stq_retry_e.bits.uop.rob_idx
      clr_bsy_brmask     (w) := GetNewBrMask(io.core.brupdate, mem_stq_retry_e.bits.uop)
      clr_bsy_cf_bmap    (w) := stq(mem_stq_retry_e.bits.uop.stq_idx).bits.uop.cf_fu_bitmap
      clr_bsy_cf_stx_r   (w) := stq(mem_stq_retry_e.bits.uop.stq_idx).bits.uop.cf_secret_transmission
      clr_bsy_cf_sprop_r (w) := stq(mem_stq_retry_e.bits.uop.stq_idx).bits.data_is_secret
    }

    io.core.clr_bsy(w).valid          := clr_bsy_valid(w) &&
                               !IsKilledByBranch(io.core.brupdate, clr_bsy_brmask(w)) &&
                               !io.core.exception && !RegNext(io.core.exception) && !RegNext(RegNext(io.core.exception))
    io.core.clr_bsy(w).bits           := clr_bsy_rob_idx(w)
    io.core.clr_bsy_cf_bitmap(w)      := clr_bsy_cf_bmap(w)
    io.core.clr_bsy_cf_stx(w)        := clr_bsy_cf_stx_r(w)
 // [PROBE-STRIPPED 2026-09-10] A1CLR -- A1 is CLOSED; probe retired.
    io.core.clr_bsy_cf_sprop(w)      := clr_bsy_cf_sprop_r(w)
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
  stdf_clr_bsy_valid    := false.B
  stdf_clr_bsy_rob_idx  := 0.U
  stdf_clr_bsy_brmask   := 0.U
  stdf_clr_bsy_cf_bmap  := 0.U
  stdf_clr_bsy_cf_stx   := false.B
  stdf_clr_bsy_cf_sprop := false.B
  when (fired_stdf_incoming) {
    val s_idx = mem_stdf_uop.stq_idx
    stdf_clr_bsy_valid   := stq(s_idx).valid                 &&
                            stq(s_idx).bits.addr.valid       &&
                            !stq(s_idx).bits.addr_is_virtual &&
                            !stq(s_idx).bits.uop.is_amo      &&
                            !IsKilledByBranch(io.core.brupdate, mem_stdf_uop)
    stdf_clr_bsy_rob_idx := mem_stdf_uop.rob_idx
    stdf_clr_bsy_brmask  := GetNewBrMask(io.core.brupdate, mem_stdf_uop)
    stdf_clr_bsy_cf_bmap := stq(s_idx).bits.uop.cf_fu_bitmap
    stdf_clr_bsy_cf_stx  := stq(s_idx).bits.uop.cf_secret_transmission
    stdf_clr_bsy_cf_sprop := stq(s_idx).bits.data_is_secret
  }



  io.core.clr_bsy(memWidth).valid           := stdf_clr_bsy_valid &&
                                    !IsKilledByBranch(io.core.brupdate, stdf_clr_bsy_brmask) &&
                                    !io.core.exception && !RegNext(io.core.exception) && !RegNext(RegNext(io.core.exception))
  io.core.clr_bsy(memWidth).bits            := stdf_clr_bsy_rob_idx
  io.core.clr_bsy_cf_bitmap(memWidth)       := stdf_clr_bsy_cf_bmap
  io.core.clr_bsy_cf_stx(memWidth)         := stdf_clr_bsy_cf_stx
  io.core.clr_bsy_cf_sprop(memWidth)       := stdf_clr_bsy_cf_sprop



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
  }
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
          when (stq(lcam_stq_idx(w)).bits.uop.cf_domain_id =/= l_bits.uop.cf_domain_id ||
                stq(lcam_stq_idx(w)).bits.uop.cf_secret_propagation ||
                stq(lcam_stq_idx(w)).bits.uop.cf_secret_access) {
            val stq_infl_uop = stq(lcam_stq_idx(w)).bits.uop
            ldq(i).bits.uop := addInfluencer(l_bits.uop, stq_infl_uop.cf_op_count_id, INFL_MEM_ORDER.U,
              is_atk = stq_infl_uop.cf_domain_id === 1.U,
              is_secret = stq_infl_uop.cf_secret_access || stq_infl_uop.cf_secret_propagation)
            // [MEMORD] The LDQ copy above is about to be destroyed by the order-fail flush
            // (ldq_head/tail := 0), so send the same edge to the ROB entry, where it lands
            // on the squashed load's [FLUSH] record.
            io.core.cf_memord_upd(w).valid             := true.B
            io.core.cf_memord_upd(w).bits.rob_idx      := l_bits.uop.rob_idx
            io.core.cf_memord_upd(w).bits.op_count_id  := l_bits.uop.cf_op_count_id
            io.core.cf_memord_upd(w).bits.prod_op_count := stq_infl_uop.cf_op_count_id
            io.core.cf_memord_upd(w).bits.is_atk       := stq_infl_uop.cf_domain_id === 1.U
            io.core.cf_memord_upd(w).bits.is_secret    := stq_infl_uop.cf_secret_access || stq_infl_uop.cf_secret_propagation
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
            when (lcam_uop(w).cf_domain_id =/= l_bits.uop.cf_domain_id ||
                  lcam_uop(w).cf_secret_propagation ||
                  lcam_uop(w).cf_secret_access) {
              ldq(i).bits.uop := addInfluencer(l_bits.uop, lcam_uop(w).cf_op_count_id, INFL_MEM_ORDER.U,
                is_atk = lcam_uop(w).cf_domain_id === 1.U,
                is_secret = lcam_uop(w).cf_secret_access || lcam_uop(w).cf_secret_propagation)
              // [MEMORD] same routing as site 1.  NOTE this site additionally requires
              // l_bits.observed (a coherence probe from another agent), so it is
              // structurally unreachable in a single-core sim -- wired for completeness.
              io.core.cf_memord_upd(w).valid             := true.B
              io.core.cf_memord_upd(w).bits.rob_idx      := l_bits.uop.rob_idx
              io.core.cf_memord_upd(w).bits.op_count_id  := l_bits.uop.cf_op_count_id
              io.core.cf_memord_upd(w).bits.prod_op_count := lcam_uop(w).cf_op_count_id
              io.core.cf_memord_upd(w).bits.is_atk       := lcam_uop(w).cf_domain_id === 1.U
              io.core.cf_memord_upd(w).bits.is_secret    := lcam_uop(w).cf_secret_access || lcam_uop(w).cf_secret_propagation
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
    val mo = io.core.cf_memord_upd(0)
    printf("[MEMORD] order_fail rob_idx=%d ldq_head=%d oc=%d prod_oc=%d atk=%d sec=%d valid=%d\n",
      ld_xcpt_uop.rob_idx, ldq_head, ld_xcpt_uop.cf_op_count_id,
      mo.bits.prod_op_count, mo.bits.is_atk, mo.bits.is_secret, mo.valid)
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
    io.core.spec_ld_wakeup(w).bits  := mem_incoming_uop(w).pdst
  }


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Writeback Cycle (St->Ld Forwarding Path)
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Handle Memory Responses and nacks
  // [AMOINFL 2026-09-08] Extracted VERBATIM from the load response path so the AMO path
  // can use the identical merge.  Previously the AMO branch assigned
  //   iresp.bits.uop := stq(...).bits.uop
  // wholesale, DISCARDING the dcache response uop's influencer list -- so an AMO that
  // read an attacker-written line got the summary tag (AMOFIX reads taint_atk straight
  // off the response bundle) but never the ty=17 MEM_DATAFLOW EDGE that says which store
  // produced it.  Measured in t32_amo_taint: atk=1 with all four slots v=0.
  //
  // ONE SOURCE DEFINITION ON PURPOSE.  Every defect this session came from a path being
  // fixed while its twin was not; a second hand-copied merge is that bug waiting to happen.
  // Chisel inlines a def, so this still elaborates one merge network per call site -- the
  // area is unchanged versus duplicating the text, and the source cannot drift.
  def cfMergeRespInfluencers(base: MicroOp, resp: MicroOp): MicroOp = {
    val lsu_base_cnt = PopCount(VecInit(base.cf_influencer_list.map(_.valid)))
    val resp_valid   = VecInit(resp.cf_influencer_list.map(_.valid))
    val resp_prefix  = (0 until numInfluencerSlotsCF).map { j =>
      if (j == 0) 0.U(4.W) else PopCount(VecInit(resp_valid.take(j)))
    }
    val resp_total = PopCount(resp_valid)
    val out = WireInit(base)
    // Drops contributed here: the merge overflowing this response's slots, plus any
    // the responding uop already carried.  Summed once onto base below --
    // accumulating into out by reading it would be a comb cycle, since it is a Wire.
    val lsu_drop_merge = WireDefault(0.U(3.W))
    val lsu_drop_resp  = WireDefault(0.U(3.W))
    when (base.cf_infl_dropped === 0.U && lsu_base_cnt +& resp_total > numInfluencerSlotsCF.U) {
      lsu_drop_merge := lsu_base_cnt +& resp_total - numInfluencerSlotsCF.U
    }
    // Precompute destination slot for each resp entry once; writers are one-hot per slot
    // by prefix-sum construction -> Mux1H is valid. Overflow check hoisted outside d-loop.
    val resp_dst_slot = (0 until numInfluencerSlotsCF).map { j => lsu_base_cnt + resp_prefix(j) }
    when (base.cf_infl_dropped === 0.U) {
      for (d <- 0 until numInfluencerSlotsCF) {
        val writers: Seq[Bool] = (0 until numInfluencerSlotsCF).map { j =>
          resp_valid(j) && (resp_dst_slot(j) === d.U)
        }
        val any_write = writers.reduce(_ || _)
        when (any_write) {
          out.cf_influencer_list(d).valid      := true.B
          out.cf_influencer_list(d).op_count   := Mux1H(writers, resp.cf_influencer_list.map(_.op_count))
          out.cf_influencer_list(d).infl_type  := Mux1H(writers, resp.cf_influencer_list.map(_.infl_type))
          out.cf_influencer_list(d).is_atk     := Mux1H(writers, resp.cf_influencer_list.map(_.is_atk))
          out.cf_influencer_list(d).is_secret  := Mux1H(writers, resp.cf_influencer_list.map(_.is_secret))
        }
      }
    }
    for (j <- 0 until numInfluencerSlotsCF) {
      when (resp.cf_influencer_list(j).valid && resp.cf_influencer_list(j).is_atk) {
        out.cf_attacker_influence := true.B
      }
    }
    when (resp.cf_infl_dropped =/= 0.U) { lsu_drop_resp := resp.cf_infl_dropped }
    out.cf_infl_dropped := SatDropped(base.cf_infl_dropped, lsu_drop_merge +& lsu_drop_resp)
    out
  }

  //----------------------------------
  for (w <- 0 until memWidth) {
    io.core.exe(w).iresp.valid := false.B
    io.core.exe(w).iresp.bits  := DontCare
    io.core.exe(w).fresp.valid := false.B
    io.core.exe(w).fresp.bits  := DontCare
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
        val send_fresp = ldq(ldq_idx).bits.uop.dst_rtype === RT_FLT

        // corefuzzing: merge CF fields from response UOP (which went through TLB+dcache pipeline
        // accumulating INFL_DTLB_STATE and INFL_CACHE_EVICTION) into the canonical LDQ base UOP.
        val ldq_base_cf = ldq(ldq_idx).bits.uop
        val resp_cf     = io.dmem.resp(w).bits.uop
        val merged_base_cf = WireInit(ldq_base_cf)
        merged_base_cf.cf_fu_bitmap          := ldq_base_cf.cf_fu_bitmap | resp_cf.cf_fu_bitmap
        merged_base_cf.cf_secret_access      := ldq_base_cf.cf_secret_access || resp_cf.cf_secret_access
        merged_base_cf.cf_secret_transmission := ldq_base_cf.cf_secret_transmission || resp_cf.cf_secret_transmission
        // [MERGEFIX 2026-09-07] VALIDATED. merged_base_cf = WireInit(ldq_base_cf), so any
        // field not merged from the dcache response uop keeps the LDQ's stale copy.
        // cf_mem_dataflow_atk is set by the dcache (dcache.scala:1398 from memdf_fire) but
        // was never merged, so it read as 0 in 69,848/69,848 responses and R1d's
        // `addr_is_atk || cf_mem_dataflow_atk` collapsed to addr_is_atk alone.
        // After this: bounds branch cond_atk 2/650 -> 650/650, committed shadow 26% -> 92%,
        // invariants exact (10/50/10, 4,495,125,000 cycles, exit 3).  See artifact 43.
        // DO NOT also merge cf_mem_sec_dataflow -- tried, caused a secret-taint explosion
        // (s_prop 50 -> 144,332).  That needs the D$ secret mask granularity fixed first.
        merged_base_cf.cf_mem_dataflow_atk := ldq_base_cf.cf_mem_dataflow_atk || resp_cf.cf_mem_dataflow_atk
        // [SECFIX 2026-09-07] The secret twin, re-enabled. The earlier explosion
        // (s_prop 50 -> 144,332) was NOT line granularity -- the per-DW masks are honoured
        // on the store path.  It was the REFILL blanketing the requester's carried secret
        // across all 8 DWs (mshrs.scala cf_req_secret, now fixed to the address property).
        // Without this merge core.scala:2191 / rob.scala:1140 read a permanently-0 field, so
        // memory-borne secret never sets s_prop -- t19_dcache_secret_inherit measured 0
        // records with s_prop=1 AND ty17 sec=1, i.e. it never tested what it documents.
        merged_base_cf.cf_mem_sec_dataflow := ldq_base_cf.cf_mem_sec_dataflow || resp_cf.cf_mem_sec_dataflow
        val dmem_resp_uop_cf = cfMergeRespInfluencers(merged_base_cf, resp_cf)

        io.core.exe(w).iresp.bits.uop  := dmem_resp_uop_cf
        io.core.exe(w).fresp.bits.uop  := dmem_resp_uop_cf
        io.core.exe(w).iresp.valid     := send_iresp
        io.core.exe(w).iresp.bits.data := io.dmem.resp(w).bits.data
        io.core.exe(w).iresp.bits.secret := dmem_resp_uop_cf.cf_secret_access ||
                                           dmem_resp_uop_cf.cf_mem_sec_dataflow
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
        io.core.exe(w).iresp.bits.taint_atk := ldq(io.dmem.resp(w).bits.uop.ldq_idx).bits.addr_is_atk ||
                                              dmem_resp_uop_cf.cf_mem_dataflow_atk
        io.core.exe(w).fresp.valid     := send_fresp
        io.core.exe(w).fresp.bits.data := io.dmem.resp(w).bits.data
        io.core.exe(w).fresp.bits.secret := dmem_resp_uop_cf.cf_secret_access ||
                                           dmem_resp_uop_cf.cf_mem_sec_dataflow
        io.core.exe(w).fresp.bits.taint_atk := ldq(io.dmem.resp(w).bits.uop.ldq_idx).bits.addr_is_atk ||
                                              dmem_resp_uop_cf.cf_mem_dataflow_atk   // [R1d] see iresp

        assert(send_iresp ^ send_fresp)
        dmem_resp_fired(w) := true.B

        ldq(ldq_idx).bits.succeeded      := io.core.exe(w).iresp.valid || io.core.exe(w).fresp.valid
        ldq(ldq_idx).bits.debug_wb_data  := io.dmem.resp(w).bits.data
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
          val amo_base_cf = stq(io.dmem.resp(w).bits.uop.stq_idx).bits.uop
          io.core.exe(w).iresp.bits.uop  := cfMergeRespInfluencers(amo_base_cf,
                                                                   io.dmem.resp(w).bits.uop)
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
            io.core.exe(w).iresp.bits.secret := stq(io.dmem.resp(w).bits.uop.stq_idx).bits.uop.cf_secret_access ||
                                                io.dmem.resp(w).bits.uop.cf_mem_sec_dataflow
            // [AMOTAINT 2026-09-08] Was ADDRESS taint only: an AMO reading attacker-tainted
            // memory returned CLEAN data.  Normal loads got the memory-dataflow term from the
            // merge fix (:1985); the AMO path was never updated to match.
            io.core.exe(w).iresp.bits.taint_atk := stq(io.dmem.resp(w).bits.uop.stq_idx).bits.addr_is_atk ||
                                                   io.dmem.resp(w).bits.uop.cf_mem_dataflow_atk

          stq(io.dmem.resp(w).bits.uop.stq_idx).bits.debug_wb_data := io.dmem.resp(w).bits.data
        }
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
      val stl_store_is_secret = stl_infl_uop.cf_secret_access || stq_e.bits.data_is_secret
      // ATTACKER: forwarding hands the loaded register the store's VALUE, so it also
      // hands over that value's attacker taint.  data_is_atk, not the uop's domain --
      // an attacker-written value forwarded into victim code is influence, not ownership.
      val stl_store_is_atk    = stq_e.bits.data_is_atk
      val fwd_s1 = WireInit(forward_uop)
      when (data_ready && live && (forward_uop.cf_domain_id =/= stl_infl_uop.cf_domain_id)) {
        fwd_s1 := addInfluencer(forward_uop, stl_infl_uop.cf_op_count_id, INFL_STL_FORWARD.U,
          is_atk = stl_infl_uop.cf_domain_id === 1.U,
          is_secret = stl_store_is_secret)
      }
      // Stage 2: secret propagation + same-domain attribution (input: fwd_s1 — no cycle).
      // Cross-domain case: s_prop only (influencer already in fwd_s1).
      // Same-domain secret case: build with_sprop from fwd_s1, then add attribution.
      val fwd_uop_cf = WireInit(fwd_s1)
      when (data_ready && live && stl_store_is_secret) {
        when (forward_uop.cf_domain_id === stl_infl_uop.cf_domain_id) {
          val with_sprop = WireInit(fwd_s1)
          with_sprop.cf_secret_propagation := true.B
          fwd_uop_cf := addInfluencer(with_sprop, stl_infl_uop.cf_op_count_id, INFL_STL_FORWARD.U,
            is_atk = false.B,
            is_secret = true.B)
        } .otherwise {
          fwd_uop_cf.cf_secret_propagation := true.B
        }
      }

      io.core.exe(w).iresp.valid := (fwd_uop_cf.dst_rtype === RT_FIX) && data_ready && live
      io.core.exe(w).fresp.valid := (fwd_uop_cf.dst_rtype === RT_FLT) && data_ready && live
      io.core.exe(w).iresp.bits.uop  := fwd_uop_cf
      io.core.exe(w).fresp.bits.uop  := fwd_uop_cf
      io.core.exe(w).iresp.bits.data := loadgen.data
      io.core.exe(w).iresp.bits.secret := stl_store_is_secret
      io.core.exe(w).iresp.bits.taint_atk := stl_store_is_atk
      io.core.exe(w).fresp.bits.data := loadgen.data
      io.core.exe(w).fresp.bits.secret := stl_store_is_secret
      io.core.exe(w).fresp.bits.taint_atk := stl_store_is_atk

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
        printf("MT %x %x %x %x %x %x %x\n",
          io.core.tsc_reg, uop.uopc, uop.mem_cmd, uop.mem_size, addr, stdata, wbdata)
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
      when (io.dmem.resp(w).valid && io.dmem.resp(w).bits.is_hella) {
        hella_state := h_ready

        io.hellacache.resp.valid       := true.B
        io.hellacache.resp.bits.addr   := hella_req.addr
        io.hellacache.resp.bits.tag    := hella_req.tag
        io.hellacache.resp.bits.cmd    := hella_req.cmd
        io.hellacache.resp.bits.signed := hella_req.signed
        io.hellacache.resp.bits.size   := hella_req.size
        io.hellacache.resp.bits.data   := io.dmem.resp(w).bits.data
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
      when (io.dmem.resp(w).valid && io.dmem.resp(w).bits.is_hella) {
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
  io.core.no_pending_mem := io.core.queues_empty && !io.dmem.req.valid
  // 
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
