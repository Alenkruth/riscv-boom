//******************************************************************************
// Copyright (c) 2012 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISCV Processor Datapath: Rename Logic
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Supports 1-cycle and 2-cycle latencies. (aka, passthrough versus registers between ren1 and ren2).
//    - ren1: read the map tables and allocate a new physical register from the freelist.
//    - ren2: read the busy table for the physical operands.
//
// Ren1 data is provided as an output to be fed directly into the ROB.

package boom.v3.exu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters

import boom.v3.common._
import boom.v3.util._
// CoreFuzzing constants (module tag ids)
import freechips.rocketchip.util._
import freechips.rocketchip.regmapper.RegField.r

/**
 * IO bundle to interface with the Register Rename logic
 *
 * @param plWidth pipeline width
 * @param numIntPregs number of int physical registers
 * @param numFpPregs number of FP physical registers
 * @param numWbPorts number of int writeback ports
 * @param numWbPorts number of FP writeback ports
 */
class RenameStageIO(
  val plWidth: Int,
  val numPhysRegs: Int,
  val numWbPorts: Int)
  (implicit p: Parameters) extends BoomBundle


/**
 * IO bundle to debug the rename stage
 */
class DebugRenameStageIO(val numPhysRegs: Int)(implicit p: Parameters) extends BoomBundle
{
  val freelist  = Bits(numPhysRegs.W)
  val isprlist  = Bits(numPhysRegs.W)
  val busytable = UInt(numPhysRegs.W)
}

abstract class AbstractRenameStage(
  plWidth: Int,
  numPhysRegs: Int,
  numWbPorts: Int)
  (implicit p: Parameters) extends BoomModule
  with CoreFuzzingConstants
{
  val io = IO(new Bundle {
    val ren_stalls = Output(Vec(plWidth, Bool()))

    val kill = Input(Bool())
    // corefuzzing: gate for speculative rename prints
    val cf_debug_rename_enable = Input(Bool())
    // corefuzzing: cycle-N mispredicting branch UOP (same cycle as b1.mispredict_mask)
    val cf_mispredict_uop = Input(Valid(new MicroOp))

    val dec_fire  = Input(Vec(plWidth, Bool())) // will commit state updates
    val dec_uops  = Input(Vec(plWidth, new MicroOp()))

    // physical specifiers available AND busy/ready status available.
    val ren2_mask = Vec(plWidth, Output(Bool())) // mask of valid instructions
    val ren2_uops = Vec(plWidth, Output(new MicroOp()))

    // branch resolution (execute)
    val brupdate = Input(new BrUpdateInfo())

    val dis_fire  = Input(Vec(coreWidth, Bool()))
    val dis_ready = Input(Bool())

    // wakeup ports
    val wakeups = Flipped(Vec(numWbPorts, Valid(new ExeUnitResp(xLen))))

    // commit stage
    val com_valids = Input(Vec(plWidth, Bool()))
    val com_uops = Input(Vec(plWidth, new MicroOp()))
    val rbk_valids = Input(Vec(plWidth, Bool()))
    val rollback = Input(Bool())

    val debug_rob_empty = Input(Bool())
    val debug = Output(new DebugRenameStageIO(numPhysRegs))

    // corefuzzing: pulse high for one cycle on QS_DRAINING→QS_FETCH transition to clear
    // taint_table / producer_domain_table / producer_secret_table for clean campaign boundaries
    val quiesce_flush = Input(Bool())

    // 3-bit index into pregFileSizeOptions for physical register file size reconfiguration
    val cf_preg_idx = Input(UInt(3.W))

    // Fix 5: retroactive commit-time taint. If a committing instruction's source pregs are
    // now tainted (C7 fired for a producer in a prior cycle) but cf_src_tainted was false
    // at rename time, expose this so core can OR it into the commit-log s_prop field.
    val com_late_taint          = Output(Vec(plWidth, Bool()))
    val com_late_taint_producer = Output(Vec(plWidth, UInt(uopIDCounterWidthCF.W)))
    // Fix 5c: late taint for stores/loads (no dst_rtype guard) — fires for any instruction
    // whose source pregs are now secret-tainted, regardless of whether it writes a register.
    // Used by core.scala to detect commit-time s_tx on stores.
    val com_late_taint_any      = Output(Vec(plWidth, Bool()))
  })

  io.ren_stalls.foreach(_ := false.B)
  io.debug := DontCare
  io.com_late_taint.foreach(_ := false.B)
  io.com_late_taint_producer.foreach(_ := 0.U)
  io.com_late_taint_any.foreach(_ := false.B)

  def BypassAllocations(uop: MicroOp, older_uops: Seq[MicroOp], alloc_reqs: Seq[Bool]): MicroOp

  //-------------------------------------------------------------
  // Pipeline State & Wires

  // Stage 1
  val ren1_fire       = Wire(Vec(plWidth, Bool()))
  val ren1_uops       = Wire(Vec(plWidth, new MicroOp))


  // Stage 2
  val ren2_fire       = io.dis_fire
  val ren2_ready      = io.dis_ready
  val ren2_valids     = Wire(Vec(plWidth, Bool()))
  val ren2_uops       = Wire(Vec(plWidth, new MicroOp))
  val ren2_alloc_reqs = Wire(Vec(plWidth, Bool()))


  //-------------------------------------------------------------
  // pipeline registers

  for (w <- 0 until plWidth) {
    // why don't we tag the uops here as they enter the rename stage?
    ren1_fire(w)          := io.dec_fire(w)
    ren1_uops(w)          := io.dec_uops(w)
  }

  for (w <- 0 until plWidth) {
    val r_valid  = RegInit(false.B)
    val r_uop    = Reg(new MicroOp)
    val next_uop = Wire(new MicroOp)

    next_uop := r_uop

    // corefuzzing: [FLUSH] for ren2 uops killed by branch mispredict. SRC=2 (rename).
    when (r_valid && IsKilledByBranch(io.brupdate, r_uop.br_mask) && io.cf_debug_rename_enable) {
      // corefuzzing: read pipeline register directly — fl/floc are separate fields.
      val ren_fu = r_uop
      val fl_cross_domain = io.cf_mispredict_uop.valid &&
        (io.cf_mispredict_uop.bits.cf_domain_id =/= ren_fu.cf_domain_id)
      val fl_op_count = Mux(io.cf_mispredict_uop.valid,
        io.cf_mispredict_uop.bits.cf_op_count_id, 0.U)
      val renBrInflFmt = (0 until numInfluencerSlotsCF).zipWithIndex.map{case(_,k) => s"I$k={v=%d,oc=%d,ty=%d,atk=%d,sec=%d,dc=%d}"}.mkString(" ")
      val renBrFmt = s"[FLUSH] 0x%x (0x%x) CF(domain=%d spec=%d atk=%d s_acc=%d s_prop=%d s_tx=%d opcount=%d spec_atk=%d spec_oc=%d fl=%d floc=%d) FU=0x%x SRC=%d INFL_FU=0x%x OVF=%d $renBrInflFmt\n"
      val renBrArgs = (0 until numInfluencerSlotsCF).flatMap(k => Seq[Bits](
        ren_fu.cf_influencer_list(k).valid,
        ren_fu.cf_influencer_list(k).op_count,
        ren_fu.cf_influencer_list(k).infl_type,
        ren_fu.cf_influencer_list(k).is_atk,
        ren_fu.cf_influencer_list(k).is_secret,
        ren_fu.cf_influencer_list(k).deny_count
      ))
      printf(renBrFmt, (Seq[Bits](
        Sext.apply(ren_fu.debug_pc(vaddrBits-1,0), xLen), ren_fu.debug_inst,
        ren_fu.cf_domain_id, ren_fu.cf_speculated, ren_fu.cf_attacker_influence,
        ren_fu.cf_secret_access, ren_fu.cf_secret_propagation, ren_fu.cf_secret_transmission,
        ren_fu.cf_op_count_id, ren_fu.cf_spec_branch_is_atk, ren_fu.cf_spec_branch_op_id,
        fl_cross_domain, fl_op_count,
        ren_fu.cf_fu_bitmap, 2.U, inflBitmapFromList(ren_fu.cf_influencer_list), ren_fu.cf_infl_overflow
      ) ++ renBrArgs): _*)
    }

    when (io.kill) {
      // corefuzzing: [FLUSH] for ren2 uops killed by pipeline flush (exception/ROB flush). SRC=2.
      when (r_valid && io.cf_debug_rename_enable) {
        val renKillInflFmt = (0 until numInfluencerSlotsCF).zipWithIndex.map{case(_,k) => s"I$k={v=%d,oc=%d,ty=%d,atk=%d,sec=%d,dc=%d}"}.mkString(" ")
        val renKillFmt = s"[FLUSH] 0x%x (0x%x) CF(domain=%d spec=%d atk=%d s_acc=%d s_prop=%d s_tx=%d opcount=%d spec_atk=%d spec_oc=%d fl=%d floc=%d) FU=0x%x SRC=%d INFL_FU=0x%x OVF=%d $renKillInflFmt\n"
        val renKillArgs = (0 until numInfluencerSlotsCF).flatMap(k => Seq[Bits](
          r_uop.cf_influencer_list(k).valid,
          r_uop.cf_influencer_list(k).op_count,
          r_uop.cf_influencer_list(k).infl_type,
          r_uop.cf_influencer_list(k).is_atk,
          r_uop.cf_influencer_list(k).is_secret,
          r_uop.cf_influencer_list(k).deny_count
        ))
        printf(renKillFmt, (Seq[Bits](
          Sext.apply(r_uop.debug_pc(vaddrBits-1,0), xLen), r_uop.debug_inst,
          r_uop.cf_domain_id, r_uop.cf_speculated, r_uop.cf_attacker_influence,
          r_uop.cf_secret_access, r_uop.cf_secret_propagation, r_uop.cf_secret_transmission,
          r_uop.cf_op_count_id, r_uop.cf_spec_branch_is_atk, r_uop.cf_spec_branch_op_id,
          0.U, 0.U,  // fl=0 floc=0: ROB flush has no flushing branch
          r_uop.cf_fu_bitmap, 2.U, inflBitmapFromList(r_uop.cf_influencer_list), r_uop.cf_infl_overflow
        ) ++ renKillArgs): _*)
      }
      r_valid := false.B
    } .elsewhen (ren2_ready) {
      r_valid := ren1_fire(w)
      next_uop := ren1_uops(w)
    } .otherwise {
      r_valid := r_valid && !ren2_fire(w) // clear bit if uop gets dispatched
      next_uop := r_uop
    }

    r_uop := GetNewUopAndBrMask(BypassAllocations(next_uop, ren2_uops, ren2_alloc_reqs), io.brupdate)

    ren2_valids(w) := r_valid
    ren2_uops(w)   := r_uop
  }

  //-------------------------------------------------------------
  // Outputs

  io.ren2_mask := ren2_valids


}


/**
 * Rename stage that connets the map table, free list, and busy table.
 * Can be used in both the FP pipeline and the normal execute pipeline.
 *
 * @param plWidth pipeline width
 * @param numWbPorts number of int writeback ports
 * @param numWbPorts number of FP writeback ports
 */
class RenameStage(
  plWidth: Int,
  numPhysRegs: Int,
  numWbPorts: Int,
  float: Boolean)
(implicit p: Parameters) extends AbstractRenameStage(plWidth, numPhysRegs, numWbPorts)(p)
{
  val pregSz = log2Ceil(numPhysRegs)
  val rtype = if (float) RT_FLT else RT_FIX
  // corefuzzing - setting up float/integer specific flags at compile time
  val moduleTagCF = if (float) frfTagCF else irfTagCF

  //-------------------------------------------------------------
  // Helper Functions

  def BypassAllocations(uop: MicroOp, older_uops: Seq[MicroOp], alloc_reqs: Seq[Bool]): MicroOp = {
    val bypassed_uop = Wire(new MicroOp)
    bypassed_uop := uop

    val bypass_hits_rs1 = (older_uops zip alloc_reqs) map { case (r,a) => a && r.ldst === uop.lrs1 }
    val bypass_hits_rs2 = (older_uops zip alloc_reqs) map { case (r,a) => a && r.ldst === uop.lrs2 }
    val bypass_hits_rs3 = (older_uops zip alloc_reqs) map { case (r,a) => a && r.ldst === uop.lrs3 }
    val bypass_hits_dst = (older_uops zip alloc_reqs) map { case (r,a) => a && r.ldst === uop.ldst }

    val bypass_sel_rs1 = PriorityEncoderOH(bypass_hits_rs1.reverse).reverse
    val bypass_sel_rs2 = PriorityEncoderOH(bypass_hits_rs2.reverse).reverse
    val bypass_sel_rs3 = PriorityEncoderOH(bypass_hits_rs3.reverse).reverse
    val bypass_sel_dst = PriorityEncoderOH(bypass_hits_dst.reverse).reverse

    val do_bypass_rs1 = bypass_hits_rs1.reduce(_||_)
    val do_bypass_rs2 = bypass_hits_rs2.reduce(_||_)
    val do_bypass_rs3 = bypass_hits_rs3.reduce(_||_)
    val do_bypass_dst = bypass_hits_dst.reduce(_||_)

    val bypass_pdsts = older_uops.map(_.pdst)

    when (do_bypass_rs1) { bypassed_uop.prs1       := Mux1H(bypass_sel_rs1, bypass_pdsts) }
    when (do_bypass_rs2) { bypassed_uop.prs2       := Mux1H(bypass_sel_rs2, bypass_pdsts) }
    when (do_bypass_rs3) { bypassed_uop.prs3       := Mux1H(bypass_sel_rs3, bypass_pdsts) }
    when (do_bypass_dst) { bypassed_uop.stale_pdst := Mux1H(bypass_sel_dst, bypass_pdsts) }

    bypassed_uop.prs1_busy := uop.prs1_busy || do_bypass_rs1
    bypassed_uop.prs2_busy := uop.prs2_busy || do_bypass_rs2
    bypassed_uop.prs3_busy := uop.prs3_busy || do_bypass_rs3

    if (!float) {
      bypassed_uop.prs3      := DontCare
      bypassed_uop.prs3_busy := false.B
    }

    bypassed_uop
  }

  //-------------------------------------------------------------
  // Rename Structures

  val maptable = Module(new RenameMapTable(
    plWidth,
    32,
    numPhysRegs,
    false,
    float))
  val freelist = Module(new RenameFreeList(
    plWidth,
    numPhysRegs,
    if (float) 32 else 31,
    if (float) fpPregFileSizeOptions else pregFileSizeOptions))
  val busytable = Module(new RenameBusyTable(
    plWidth,
    numPhysRegs,
    numWbPorts,
    false,
    float))



  val ren2_br_tags    = Wire(Vec(plWidth, Valid(UInt(brTagSz.W))))

  // Commit/Rollback
  val com_valids      = Wire(Vec(plWidth, Bool()))
  val rbk_valids      = Wire(Vec(plWidth, Bool()))

  for (w <- 0 until plWidth) {
    ren2_alloc_reqs(w)    := ren2_uops(w).ldst_val && ren2_uops(w).dst_rtype === rtype && ren2_fire(w)
    ren2_br_tags(w).valid := ren2_fire(w) && ren2_uops(w).allocate_brtag

    com_valids(w)         := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === rtype && io.com_valids(w)
    rbk_valids(w)         := io.com_uops(w).ldst_val && io.com_uops(w).dst_rtype === rtype && io.rbk_valids(w)
    ren2_br_tags(w).bits  := ren2_uops(w).br_tag
  }

  //-------------------------------------------------------------
  // Rename Table

  // Maptable inputs.
  val map_reqs   = Wire(Vec(plWidth, new MapReq(lregSz)))
  val remap_reqs = Wire(Vec(plWidth, new RemapReq(lregSz, pregSz)))

  // Generate maptable requests.
  for ((((ren1,ren2),com),w) <- (ren1_uops zip ren2_uops zip io.com_uops.reverse).zipWithIndex) {
    map_reqs(w).lrs1 := ren1.lrs1
    map_reqs(w).lrs2 := ren1.lrs2
    map_reqs(w).lrs3 := ren1.lrs3
    map_reqs(w).ldst := ren1.ldst

    remap_reqs(w).ldst := Mux(io.rollback, com.ldst      , ren2.ldst)
    remap_reqs(w).pdst := Mux(io.rollback, com.stale_pdst, ren2.pdst)
  }
  ren2_alloc_reqs zip rbk_valids.reverse zip remap_reqs map {
    case ((a,r),rr) => rr.valid := a || r}

  // val ren1_uops_tagged = Wire(Vec(plWidth, new MicroOp))
  // // corefuzzing - add regfile tags to the uop
  // for (w <- 0 until plWidth) {
  //   // Tag the uop on entry to the rename stage with the integer register
  //   // file tag. We append combinationally so there is no additional cycle
  //   // penalty; this records that the micro-op has entered the rename
  //   // register-file related logic.  
  //   // Only append the module tag when the uop actually enters the rename
  //   // stage (i.e., on the decode->rename handshake). Use the ren1_fire
  //   // signal which indicates the uop is being presented to rename.
  //   // val uop_tagged = Wire(new MicroOp)
  //   // uop_tagged := ren1_uops(w)
  //   // when (ren1_fire(w) && ren1_uops(w).cf_taint_module_id_1 =/= moduleTagCF.U) {
  //   //   ren1_uops_tagged(w) := appendModuleTag(moduleTagCF.U, ren1_uops(w))
  //   // }
  //   // .otherwise {
  //   //   ren1_uops_tagged(w) := ren1_uops(w)
  //   // }
  // }

  // Hook up inputs.
  maptable.io.map_reqs    := map_reqs
  maptable.io.remap_reqs  := remap_reqs
  maptable.io.ren_br_tags := ren2_br_tags
  maptable.io.brupdate      := io.brupdate
  maptable.io.rollback    := io.rollback 

  // Maptable outputs.
  for ((uop, w) <- ren1_uops.zipWithIndex) {
    val mappings = maptable.io.map_resps(w)

    uop.prs1       := mappings.prs1
    uop.prs2       := mappings.prs2
    uop.prs3       := mappings.prs3 // only FP has 3rd operand
    uop.stale_pdst := mappings.stale_pdst
  }



  //-------------------------------------------------------------
  // Free List

  // Freelist inputs.
  freelist.io.reqs := ren2_alloc_reqs
  freelist.io.dealloc_pregs zip com_valids zip rbk_valids map
    {case ((d,c),r) => d.valid := c || r}
  freelist.io.dealloc_pregs zip io.com_uops map
    {case (d,c) => d.bits := Mux(io.rollback, c.pdst, c.stale_pdst)}
  freelist.io.ren_br_tags := ren2_br_tags
  freelist.io.brupdate := io.brupdate
  freelist.io.debug.pipeline_empty := io.debug_rob_empty
  freelist.io.cf_preg_idx := io.cf_preg_idx

  assert (ren2_alloc_reqs zip freelist.io.alloc_pregs map {case (r,p) => !r || p.bits =/= 0.U} reduce (_&&_),
           "[rename-stage] A uop is trying to allocate the zero physical register.")

  // Freelist outputs.
  for ((uop, w) <- ren2_uops.zipWithIndex) {
    val preg = freelist.io.alloc_pregs(w).bits
    uop.pdst := Mux(uop.ldst =/= 0.U || float.B, preg, 0.U)
  }

  //-------------------------------------------------------------
  // Busy Table

  busytable.io.ren_uops := ren2_uops  // expects pdst to be set up.
  busytable.io.rebusy_reqs := ren2_alloc_reqs
  busytable.io.wb_valids := io.wakeups.map(_.valid)
  busytable.io.wb_pdsts := io.wakeups.map(_.bits.uop.pdst)

  assert (!(io.wakeups.map(x => x.valid && x.bits.uop.dst_rtype =/= rtype).reduce(_||_)),
   "[rename] Wakeup has wrong rtype.")

  for ((uop, w) <- ren2_uops.zipWithIndex) {
    val busy = busytable.io.busy_resps(w)

    uop.prs1_busy := uop.lrs1_rtype === rtype && busy.prs1_busy
    uop.prs2_busy := uop.lrs2_rtype === rtype && busy.prs2_busy
    uop.prs3_busy := uop.frs3_en && busy.prs3_busy

    val valid = ren2_valids(w)
    assert (!(valid && busy.prs1_busy && rtype === RT_FIX && uop.lrs1 === 0.U), "[rename] x0 is busy??")
    assert (!(valid && busy.prs2_busy && rtype === RT_FIX && uop.lrs2 === 0.U), "[rename] x0 is busy??")
  }

  //-------------------------------------------------------------
  // IFT Phase 2: Register Taint Table
  // taint_table(preg)           = 1: last writer was attacker domain or transitively tainted
  // producer_table(preg)        = op_count_id of that writer
  // producer_domain_table(preg) = true if that writer was from attacker domain (domain=1)
  // producer_secret_table(preg) = true if that writer had s_acc=1 or s_prop=1
  // taint_snaps: branch snapshots for rollback on misprediction

  val taint_table           = RegInit(VecInit(Seq.fill(numPhysRegs)(false.B)))
  val producer_table        = Reg(Vec(numPhysRegs, UInt(uopIDCounterWidthCF.W)))
  val producer_domain_table = RegInit(VecInit(Seq.fill(numPhysRegs)(false.B)))
  val producer_secret_table = RegInit(VecInit(Seq.fill(numPhysRegs)(false.B)))
  val taint_snaps           = Reg(Vec(maxBrCount, Vec(numPhysRegs, Bool())))

  // -- Read taint for each ren2 uop's source physical registers,
  //    with same-cycle forwarding from prior slots in the dispatch group.
  //    fwd_* reflects writes from slots 0..w-1 so that slot w sees same-cycle taints
  //    (e.g. fence.i re-fetch: xor/add/sd in same rename cycle — add/sd must see xor's taint).
  var fwd_taint:    Vec[Bool] = WireInit(taint_table)
  var fwd_producer: Vec[UInt] = WireInit(producer_table)
  var fwd_prod_atk: Vec[Bool] = WireInit(producer_domain_table)
  var fwd_prod_sec: Vec[Bool] = WireInit(producer_secret_table)
  for (w <- 0 until plWidth) {
    // Compute effective prs1/prs2/prs3 for taint lookup, mirroring BypassAllocations:
    // if a prior slot in this rename group writes to the same architectural register as
    // lrs1/lrs2, use that slot's pdst (the just-allocated physical register) for taint lookup.
    val prs1_eff = Wire(chiselTypeOf(ren2_uops(w).prs1))
    val prs2_eff = Wire(chiselTypeOf(ren2_uops(w).prs2))
    prs1_eff := ren2_uops(w).prs1
    prs2_eff := ren2_uops(w).prs2
    for (j <- 0 until w) {
      when (ren2_alloc_reqs(j) && ren2_uops(j).ldst === ren2_uops(w).lrs1) { prs1_eff := ren2_uops(j).pdst }
      when (ren2_alloc_reqs(j) && ren2_uops(j).ldst === ren2_uops(w).lrs2) { prs2_eff := ren2_uops(j).pdst }
    }
    val prs3_eff = if (float) {
      val e = Wire(chiselTypeOf(ren2_uops(w).prs3)); e := ren2_uops(w).prs3
      for (j <- 0 until w) { when (ren2_alloc_reqs(j) && ren2_uops(j).ldst === ren2_uops(w).lrs3) { e := ren2_uops(j).pdst } }
      e
    } else null
    val prs1_t = fwd_taint(prs1_eff)
    val prs2_t = fwd_taint(prs2_eff)
    val prs3_t = if (float) fwd_taint(prs3_eff) else false.B
    val any_tainted = (prs1_t && (ren2_uops(w).lrs1_rtype === rtype)) ||
                      (prs2_t && (ren2_uops(w).lrs2_rtype === rtype)) ||
                      (prs3_t.asBool && ren2_uops(w).frs3_en)
    // any_secret_tainted: true if ANY tainted source register's last writer had s_acc or s_prop.
    // Used to gate cf_secret_propagation — attacker-domain taints do NOT propagate s_prop.
    val prs3_sec = if (float) fwd_prod_sec(prs3_eff) else false.B
    val any_secret_tainted =
      (prs1_t && (ren2_uops(w).lrs1_rtype === rtype) && fwd_prod_sec(prs1_eff)) ||
      (prs2_t && (ren2_uops(w).lrs2_rtype === rtype) && fwd_prod_sec(prs2_eff)) ||
      (prs3_t.asBool && ren2_uops(w).frs3_en && prs3_sec)
    ren2_uops(w).cf_src_tainted := any_tainted
    // Pick one tainted source's producer metadata (priority: prs1 > prs2 > prs3)
    ren2_uops(w).cf_taint_producer_op := MuxCase(0.U, Seq(
      (prs1_t && (ren2_uops(w).lrs1_rtype === rtype)) -> fwd_producer(prs1_eff),
      (prs2_t && (ren2_uops(w).lrs2_rtype === rtype)) -> fwd_producer(prs2_eff)
    ) ++ (if (float) Seq((prs3_t.asBool && ren2_uops(w).frs3_en) -> fwd_producer(prs3_eff)) else Nil))
    ren2_uops(w).cf_taint_producer_is_atk := MuxCase(false.B, Seq(
      (prs1_t && (ren2_uops(w).lrs1_rtype === rtype)) -> fwd_prod_atk(prs1_eff),
      (prs2_t && (ren2_uops(w).lrs2_rtype === rtype)) -> fwd_prod_atk(prs2_eff)
    ) ++ (if (float) Seq((prs3_t.asBool && ren2_uops(w).frs3_en) -> fwd_prod_atk(prs3_eff)) else Nil))
    // is_secret = OR across all tainted sources: true if any tainted source came from a secret write.
    ren2_uops(w).cf_taint_producer_is_secret := any_secret_tainted
    // Advance forwarded state with this slot's write so next slots see it
    val step_taint    = WireInit(fwd_taint)
    val step_prod     = WireInit(fwd_producer)
    val step_prod_atk = WireInit(fwd_prod_atk)
    val step_prod_sec = WireInit(fwd_prod_sec)
    when (ren2_fire(w) && ren2_valids(w) && (ren2_uops(w).dst_rtype === rtype)) {
      step_taint(ren2_uops(w).pdst)    := ren2_uops(w).cf_domain_id === 1.U || any_tainted
      step_prod(ren2_uops(w).pdst)     := ren2_uops(w).cf_op_count_id
      // Propagate atk attribution transitively: if this instruction is domain=1 OR its source
      // register was written (directly or transitively) by an attacker instruction, mark the
      // dest as attacker-originated so downstream consumers see is_atk=true.
      step_prod_atk(ren2_uops(w).pdst) := ren2_uops(w).cf_domain_id === 1.U ||
                                          (any_tainted && ren2_uops(w).cf_taint_producer_is_atk)
      // cf_secret_propagation is set at dispatch (after ren2); use any_secret_tainted && victim as proxy
      // so that same-cycle consumers see the correct producer_secret state via step_prod_sec forwarding.
      val will_be_s_prop = any_secret_tainted && (ren2_uops(w).cf_domain_id === 0.U)
      step_prod_sec(ren2_uops(w).pdst) := ren2_uops(w).cf_secret_access || ren2_uops(w).cf_secret_propagation || will_be_s_prop
    }
    fwd_taint    = step_taint
    fwd_producer = step_prod
    fwd_prod_atk = step_prod_atk
    fwd_prod_sec = step_prod_sec
  }

  // -- Write taint at dispatch (ren2_fire = io.dis_fire) --
  for (w <- 0 until plWidth) {
    when (ren2_fire(w) && ren2_valids(w) && (ren2_uops(w).dst_rtype === rtype)) {
      val should_taint = ren2_uops(w).cf_domain_id === 1.U || ren2_uops(w).cf_src_tainted
      taint_table(ren2_uops(w).pdst)           := should_taint
      producer_table(ren2_uops(w).pdst)        := ren2_uops(w).cf_op_count_id
      producer_domain_table(ren2_uops(w).pdst) := ren2_uops(w).cf_domain_id === 1.U ||
                                                   (ren2_uops(w).cf_src_tainted && ren2_uops(w).cf_taint_producer_is_atk)
      // cf_secret_propagation is set at dispatch (core.scala), AFTER ren2, so it is
      // always false here.  Use cf_taint_producer_is_secret (= any_secret_tainted from
      // line 491) as a proxy: if any tainted source was secret-derived AND this is a
      // victim instruction, the dest will get s_prop at dispatch.  This mirrors the
      // will_be_s_prop logic in step_prod_sec (line 508) so that the Reg write matches
      // the same-cycle forwarding path.
      val will_be_s_prop_reg = ren2_uops(w).cf_taint_producer_is_secret &&
                               (ren2_uops(w).cf_domain_id === 0.U)
      producer_secret_table(ren2_uops(w).pdst) := ren2_uops(w).cf_secret_access || will_be_s_prop_reg
    }
  }

  // -- Clear taint at commit (stale physical register freed) --
  for (w <- 0 until plWidth) {
    when (io.com_valids(w) && !io.rollback) {
      taint_table(io.com_uops(w).stale_pdst)           := false.B
      producer_domain_table(io.com_uops(w).stale_pdst) := false.B
      producer_secret_table(io.com_uops(w).stale_pdst) := false.B
    }
  }

  // Fix 5: retroactive commit-time taint check with same-cycle forwarding.
  //
  // Problem: taint_table writes (C7, Fix 5d) take effect NEXT cycle (register semantics).
  // When multiple instructions commit in the same retire group (up to coreWidth), a
  // later slot's com_late_taint check reads the OLD taint_table — missing writes from
  // earlier slots in the same group.
  //
  // Solution: same-cycle forwarding within the commit group (same pattern as dispatch-time
  // fwd_taint/fwd_prod_sec in the rename loop above).  Build forwarded taint/secret views
  // that accumulate writes from C7 and Fix 5d for prior slots w'<w.
  //
  // Reconfiguration-safe: cf_active_width reduces the number of active commit slots, but
  // the forwarding loop iterates over all plWidth slots at elaboration time.  Inactive
  // slots have com_valid=false (will_commit gated by rob_val), so their C7/Fix5d writes
  // never fire and the forwarding naturally skips them.  ROB reconfiguration (cf_rob_entries)
  // only affects which rows are valid — not the per-bank commit width — so it has no
  // impact on this forwarding logic.

  // Seed forwarded views from the register values (previous cycle).
  // Use `var` chains (same pattern as dispatch-time fwd_taint) to avoid
  // combinational loops: each slot w reads from the state accumulated by
  // slots 0..w-1, then produces a new state for slot w+1.
  var com_fwd_taint:  Vec[Bool] = WireInit(taint_table)
  var com_fwd_secret: Vec[Bool] = WireInit(producer_secret_table)

  for (w <- 0 until plWidth) {
    val com_uop   = io.com_uops(w)
    val com_valid = io.com_valids(w) && !io.rollback

    // Read FORWARDED taint (includes writes from prior slots 0..w-1 only).
    val prs1_t    = com_fwd_taint(com_uop.prs1)
    val prs2_t    = com_fwd_taint(com_uop.prs2)
    val prs1_sec  = com_fwd_secret(com_uop.prs1)
    val prs2_sec  = com_fwd_secret(com_uop.prs2)
    val src_sec   = (prs1_t && prs1_sec) || (prs2_t && prs2_sec)

    // com_late_taint: fires for instructions with a destination register (s_prop in printf).
    val late = com_valid && !com_uop.cf_src_tainted &&
               (com_uop.dst_rtype === rtype) && src_sec
    io.com_late_taint(w) := late
    io.com_late_taint_producer(w) := Mux(prs1_t && prs1_sec,
                                         producer_table(com_uop.prs1),
                                         producer_table(com_uop.prs2))

    // com_late_taint_any: fires for ANY instruction including stores (no dst_rtype guard).
    val late_any = com_valid && !com_uop.cf_src_tainted && src_sec
    io.com_late_taint_any(w) := late_any

    // Fix 5d: propagate to taint_table register (takes effect next cycle).
    when (late) {
      taint_table(com_uop.pdst)           := true.B
      producer_table(com_uop.pdst)        := com_uop.cf_op_count_id
      producer_secret_table(com_uop.pdst) := true.B
    }

    // Same-cycle forwarding: produce NEW forwarded views for slot w+1.
    // C7 fires on (cf_secret_access || cf_src_tainted) — also forward that.
    val c7_fires = com_valid &&
                   (com_uop.cf_secret_access || com_uop.cf_src_tainted) &&
                   com_uop.dst_rtype === rtype
    val c7_secret = com_uop.cf_secret_access || com_uop.cf_taint_producer_is_secret

    val next_fwd_taint  = WireInit(com_fwd_taint)
    val next_fwd_secret = WireInit(com_fwd_secret)
    when (late || c7_fires) {
      next_fwd_taint(com_uop.pdst)  := true.B
      next_fwd_secret(com_uop.pdst) := Mux(late, true.B, c7_secret)
    }
    com_fwd_taint  = next_fwd_taint
    com_fwd_secret = next_fwd_secret
  }

  // -- C7: At commit, mark pdst as secret-tainted if instruction accessed secret memory or its
  // source registers were tainted (data-channel propagation).  We intentionally do NOT trigger
  // on cf_secret_propagation alone because that flag is also set for timing-channel events
  // (C5 queue-head stalls, BPD/fetch-path redirects) whose register results are not secret-derived.
  // taint_table fires on cf_src_tainted (any taint — attacker or secret) to track all data-flow
  // for REG_DATAFLOW attribution after fence.i refetch.
  // producer_secret_table ONLY fires when the taint is secret-originated: the instruction itself
  // accessed secret memory (cf_secret_access) or its source registers were themselves secret-tainted
  // (cf_taint_producer_is_secret). Pure attacker-domain taints must NOT set producer_secret_table.
  for (w <- 0 until plWidth) {
    when (io.com_valids(w) && !io.rollback &&
          (io.com_uops(w).cf_secret_access || io.com_uops(w).cf_src_tainted) &&
          io.com_uops(w).dst_rtype === rtype) {
      taint_table(io.com_uops(w).pdst)    := true.B
      producer_table(io.com_uops(w).pdst) := io.com_uops(w).cf_op_count_id
      producer_secret_table(io.com_uops(w).pdst) :=
        io.com_uops(w).cf_secret_access || io.com_uops(w).cf_taint_producer_is_secret
    }
  }

  // corefuzzing F1: memory data-flow taint — when a load writeback carries INFL_MEM_DATAFLOW,
  // the destination preg received attacker-originated data.  Mark it as tainted so that
  // instructions renamed AFTER this cycle see cf_src_tainted and get INFL_REG_DATAFLOW.
  // Fires at writeback time (earlier than C7 commit-time path) to capture more consumers.
  for (i <- 0 until numWbPorts) {
    when (io.wakeups(i).valid && io.wakeups(i).bits.uop.rf_wen &&
          io.wakeups(i).bits.uop.dst_rtype === rtype) {
      val wb_uop = io.wakeups(i).bits.uop
      val has_mem_df = wb_uop.cf_influencer_list.map(e =>
        e.valid && e.infl_type === INFL_MEM_DATAFLOW.U).reduce(_ || _)
      when (has_mem_df) {
        taint_table(wb_uop.pdst)           := true.B
        producer_domain_table(wb_uop.pdst) := true.B
        producer_table(wb_uop.pdst)        := wb_uop.cf_op_count_id
        // producer_secret_table not set: attacker-data taint, not secret-origin taint
      }
    }
  }

  // -- Branch snapshot: capture taint state after this dispatch group's allocations --
  // fwd_taint (built above) holds the complete post-dispatch taint state
  for (w <- 0 until plWidth) {
    when (ren2_fire(w) && ren2_uops(w).allocate_brtag) {
      taint_snaps(ren2_uops(w).br_tag) := fwd_taint
    }
  }

  // -- Restore on branch mispredict (highest priority: overrides element writes above) --
  when (io.brupdate.b2.mispredict) {
    taint_table := taint_snaps(io.brupdate.b2.uop.br_tag)
    // producer_domain_table and producer_secret_table don't need rollback:
    // taint_table rollback makes squashed pregs appear untainted, so their producer_* values
    // won't be consumed. New writes after rollback correctly overwrite producer_* entries.
  }

  // -- IFT quiesce flush (highest priority: fires when pipeline fully drained on quiesce) --
  // Clears taint state for clean campaign boundaries and correct PRF-resize semantics.
  // taint_snaps do NOT need clearing: no branches are in-flight when drained.
  when (io.quiesce_flush) {
    for (i <- 0 until numPhysRegs) {
      taint_table(i)           := false.B
      producer_table(i)        := 0.U
      producer_domain_table(i) := false.B
      producer_secret_table(i) := false.B
    }
  }

  //-------------------------------------------------------------
  // Outputs

  for (w <- 0 until plWidth) {
    val can_allocate = freelist.io.alloc_pregs(w).valid

    // Push back against Decode stage if Rename1 can't proceed.
    io.ren_stalls(w) := (ren2_uops(w).dst_rtype === rtype) && !can_allocate

    val bypassed_uop = Wire(new MicroOp)
    if (w > 0) bypassed_uop := BypassAllocations(ren2_uops(w), ren2_uops.slice(0,w), ren2_alloc_reqs.slice(0,w))
    else       bypassed_uop := ren2_uops(w)

    io.ren2_uops(w) := GetNewUopAndBrMask(bypassed_uop, io.brupdate)
    // corefuzzing: stamp irf/frf bitmap bit at rename output
    io.ren2_uops(w).cf_fu_bitmap := GetNewUopAndBrMask(bypassed_uop, io.brupdate).cf_fu_bitmap | (1.U << moduleTagCF.U)
  }

  //-------------------------------------------------------------
  // Debug signals

  io.debug.freelist  := freelist.io.debug.freelist
  io.debug.isprlist  := freelist.io.debug.isprlist
  io.debug.busytable := busytable.io.debug.busytable
}

class PredRenameStage(
  plWidth: Int,
  numPhysRegs: Int,
  numWbPorts: Int)
  (implicit p: Parameters) extends AbstractRenameStage(plWidth, numPhysRegs, numWbPorts)(p)
{
  def BypassAllocations(uop: MicroOp, older_uops: Seq[MicroOp], alloc_reqs: Seq[Bool]): MicroOp = {
    uop
  }

  ren2_alloc_reqs := DontCare

  val busy_table = RegInit(VecInit(0.U(ftqSz.W).asBools))
  val to_busy = WireInit(VecInit(0.U(ftqSz.W).asBools))
  val unbusy = WireInit(VecInit(0.U(ftqSz.W).asBools))

  val current_ftq_idx = Reg(UInt(log2Ceil(ftqSz).W))
  var next_ftq_idx = current_ftq_idx

  for (w <- 0 until plWidth) {
    io.ren2_uops(w) := ren2_uops(w)

    val is_sfb_br = ren2_uops(w).is_sfb_br && ren2_fire(w)
    val is_sfb_shadow = ren2_uops(w).is_sfb_shadow && ren2_fire(w)

    val ftq_idx = ren2_uops(w).ftq_idx
    when (is_sfb_br) {
      io.ren2_uops(w).pdst := ftq_idx
      to_busy(ftq_idx) := true.B
    }
    next_ftq_idx = Mux(is_sfb_br, ftq_idx, next_ftq_idx)

    when (is_sfb_shadow) {
      io.ren2_uops(w).ppred := next_ftq_idx
      io.ren2_uops(w).ppred_busy := (busy_table(next_ftq_idx) || to_busy(next_ftq_idx)) && !unbusy(next_ftq_idx)
    }
  }

  for (w <- 0 until numWbPorts) {
    when (io.wakeups(w).valid) {
      unbusy(io.wakeups(w).bits.uop.pdst) := true.B
    }
  }

  current_ftq_idx := next_ftq_idx

  busy_table := ((busy_table.asUInt | to_busy.asUInt) & ~unbusy.asUInt).asBools
}
