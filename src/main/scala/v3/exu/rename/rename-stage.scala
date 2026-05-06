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

  // Compact record of each dispatch slot's write to the taint tables.
  // Used to compute per-read bypass values without materializing the full table.
  val slot_taint_writes = Wire(Vec(plWidth, new Bundle {
    val valid    = Bool()
    val pdst     = UInt(log2Ceil(numPhysRegs).W)
    val taint    = Bool()
    val producer = UInt(uopIDCounterWidthCF.W)
    val prod_atk = Bool()
    val prod_sec = Bool()
  }))
  for (w <- 0 until plWidth) {
    slot_taint_writes(w).valid    := false.B
    slot_taint_writes(w).pdst     := DontCare
    slot_taint_writes(w).taint    := false.B
    slot_taint_writes(w).producer := DontCare
    slot_taint_writes(w).prod_atk := false.B
    slot_taint_writes(w).prod_sec := false.B
  }

  // Read taint for physical register `preg`, forwarding writes from slots 0..(beforeSlot-1).
  // pdst fans out to (beforeSlot × reads) destinations instead of numPhysRegs table entries.
  def bypassedTaint(preg: UInt, beforeSlot: Int): Bool = {
    var t: Bool = taint_table(preg)
    for (j <- 0 until beforeSlot) {
      t = Mux(slot_taint_writes(j).valid && slot_taint_writes(j).pdst === preg,
              slot_taint_writes(j).taint, t)
    }
    t
  }

  def bypassedProducer(preg: UInt, beforeSlot: Int): UInt = {
    var p: UInt = producer_table(preg)
    for (j <- 0 until beforeSlot) {
      p = Mux(slot_taint_writes(j).valid && slot_taint_writes(j).pdst === preg,
              slot_taint_writes(j).producer, p)
    }
    p
  }

  def bypassedProdAtk(preg: UInt, beforeSlot: Int): Bool = {
    var pa: Bool = producer_domain_table(preg)
    for (j <- 0 until beforeSlot) {
      pa = Mux(slot_taint_writes(j).valid && slot_taint_writes(j).pdst === preg,
               slot_taint_writes(j).prod_atk, pa)
    }
    pa
  }

  // D-G4 tight-wave bypass included here: a wakeup with cf_secret_access in the same cycle
  // as dispatch means consumers renamed this cycle would read stale false from the register.
  def bypassedProdSec(preg: UInt, beforeSlot: Int): Bool = {
    var ps: Bool = producer_secret_table(preg)
    for (i <- 0 until numWbPorts) {
      val wb = io.wakeups(i)
      ps = Mux(wb.valid && wb.bits.uop.rf_wen && wb.bits.uop.dst_rtype === rtype &&
               wb.bits.uop.cf_secret_access && wb.bits.uop.pdst === preg,
               true.B, ps)
    }
    for (j <- 0 until beforeSlot) {
      ps = Mux(slot_taint_writes(j).valid && slot_taint_writes(j).pdst === preg,
               slot_taint_writes(j).prod_sec, ps)
    }
    ps
  }

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

    val prs1_t = bypassedTaint(prs1_eff, w)
    val prs2_t = bypassedTaint(prs2_eff, w)
    val prs3_t = if (float) bypassedTaint(prs3_eff, w) else false.B

    val any_tainted = (prs1_t && (ren2_uops(w).lrs1_rtype === rtype)) ||
                      (prs2_t && (ren2_uops(w).lrs2_rtype === rtype)) ||
                      (prs3_t.asBool && ren2_uops(w).frs3_en)

    val prs1_sec = bypassedProdSec(prs1_eff, w)
    val prs2_sec = bypassedProdSec(prs2_eff, w)
    val prs3_sec = if (float) bypassedProdSec(prs3_eff, w) else false.B
    // any_secret_tainted: true if ANY tainted source register's last writer had s_acc or s_prop.
    // Used to gate cf_secret_propagation — attacker-domain taints do NOT propagate s_prop.
    val any_secret_tainted =
      (prs1_t && (ren2_uops(w).lrs1_rtype === rtype) && prs1_sec) ||
      (prs2_t && (ren2_uops(w).lrs2_rtype === rtype) && prs2_sec) ||
      (prs3_t.asBool && ren2_uops(w).frs3_en && prs3_sec)

    ren2_uops(w).cf_src_tainted := any_tainted
    // Pick one tainted source's producer metadata (priority: prs1 > prs2 > prs3)
    ren2_uops(w).cf_taint_producer_op := MuxCase(0.U, Seq(
      (prs1_t && (ren2_uops(w).lrs1_rtype === rtype)) -> bypassedProducer(prs1_eff, w),
      (prs2_t && (ren2_uops(w).lrs2_rtype === rtype)) -> bypassedProducer(prs2_eff, w)
    ) ++ (if (float) Seq((prs3_t.asBool && ren2_uops(w).frs3_en) -> bypassedProducer(prs3_eff, w)) else Nil))
    ren2_uops(w).cf_taint_producer_is_atk := MuxCase(false.B, Seq(
      (prs1_t && (ren2_uops(w).lrs1_rtype === rtype)) -> bypassedProdAtk(prs1_eff, w),
      (prs2_t && (ren2_uops(w).lrs2_rtype === rtype)) -> bypassedProdAtk(prs2_eff, w)
    ) ++ (if (float) Seq((prs3_t.asBool && ren2_uops(w).frs3_en) -> bypassedProdAtk(prs3_eff, w)) else Nil))
    ren2_uops(w).cf_taint_producer_is_secret := any_secret_tainted

    val new_taint    = ren2_uops(w).cf_domain_id === 1.U || any_tainted
    val new_prod_atk = ren2_uops(w).cf_domain_id === 1.U ||
                       (any_tainted && ren2_uops(w).cf_taint_producer_is_atk)
    val will_be_s_prop = any_secret_tainted && (ren2_uops(w).cf_domain_id === 0.U)
    val new_prod_sec = ren2_uops(w).cf_secret_access || ren2_uops(w).cf_secret_propagation || will_be_s_prop

    slot_taint_writes(w).valid    := ren2_fire(w) && ren2_valids(w) && (ren2_uops(w).dst_rtype === rtype)
    slot_taint_writes(w).pdst     := ren2_uops(w).pdst
    slot_taint_writes(w).taint    := new_taint
    slot_taint_writes(w).producer := ren2_uops(w).cf_op_count_id
    slot_taint_writes(w).prod_atk := new_prod_atk
    slot_taint_writes(w).prod_sec := new_prod_sec
  }

  // -- Write taint at dispatch (ren2_fire = io.dis_fire) --
  // IFT compile-time gate: when ENABLE_IFT=false the taint-table writes are
  // elided; the RegInit storage stays all-false, reads in the forwarding loop
  // above return false, all downstream cf_src_tainted/cf_taint_producer_* uop
  // fields collapse to false via constant propagation, and FIRRTL DCE removes
  // the register storage entirely.
  if (ENABLE_IFT) {
    // Write taint tables from dispatch slots (slot_taint_writes populated in the loop above).
    // Last slot wins on same-pdst conflict (Chisel last-connect semantics).
    for (w <- 0 until plWidth) {
      when (slot_taint_writes(w).valid) {
        taint_table(slot_taint_writes(w).pdst)           := slot_taint_writes(w).taint
        producer_table(slot_taint_writes(w).pdst)        := slot_taint_writes(w).producer
        producer_domain_table(slot_taint_writes(w).pdst) := slot_taint_writes(w).prod_atk
        producer_secret_table(slot_taint_writes(w).pdst) := slot_taint_writes(w).prod_sec
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

  // Default IO drivers for com_late_taint* — always fire so the rename-stage
  // output bundle has defined values even when ENABLE_IFT=false.  The IFT
  // commit-forwarding loop below overrides these via last-connect when enabled.
  for (w <- 0 until plWidth) {
    io.com_late_taint(w)          := false.B
    io.com_late_taint_producer(w) := 0.U
    io.com_late_taint_any(w)      := false.B
  }

  if (ENABLE_IFT) {
    // Commit-group forwarding: same-cycle bypass for C7 and Fix-5d writes within the retire group.
    // Replaces the WireInit var-chain with a compact write record + bypass-Mux reads.
    val slot_com_writes = Wire(Vec(plWidth, new Bundle {
      val valid  = Bool()
      val pdst   = UInt(log2Ceil(numPhysRegs).W)
      val taint  = Bool()
      val secret = Bool()
    }))
    for (w <- 0 until plWidth) {
      slot_com_writes(w).valid  := false.B
      slot_com_writes(w).pdst   := DontCare
      slot_com_writes(w).taint  := false.B
      slot_com_writes(w).secret := false.B
    }

    def comBypassedTaint(preg: UInt, beforeSlot: Int): Bool = {
      var t: Bool = taint_table(preg)
      for (j <- 0 until beforeSlot) {
        t = Mux(slot_com_writes(j).valid && slot_com_writes(j).pdst === preg,
                slot_com_writes(j).taint, t)
      }
      t
    }
    def comBypassedSecret(preg: UInt, beforeSlot: Int): Bool = {
      var s: Bool = producer_secret_table(preg)
      for (j <- 0 until beforeSlot) {
        s = Mux(slot_com_writes(j).valid && slot_com_writes(j).pdst === preg,
                slot_com_writes(j).secret, s)
      }
      s
    }

    for (w <- 0 until plWidth) {
      val com_uop   = io.com_uops(w)
      val com_valid = io.com_valids(w) && !io.rollback

      val prs1_t    = comBypassedTaint(com_uop.prs1, w)
      val prs2_t    = comBypassedTaint(com_uop.prs2, w)
      val prs3_t    = if (float) comBypassedTaint(com_uop.prs3, w)   else false.B
      val prs1_sec  = comBypassedSecret(com_uop.prs1, w)
      val prs2_sec  = comBypassedSecret(com_uop.prs2, w)
      val prs3_sec  = if (float) comBypassedSecret(com_uop.prs3, w)  else false.B
      val src_sec   = (prs1_t && prs1_sec) || (prs2_t && prs2_sec) ||
                      (prs3_t.asBool && prs3_sec.asBool && (if (float) com_uop.frs3_en else false.B))

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

      val c7_fires = com_valid &&
                     (com_uop.cf_secret_access || com_uop.cf_src_tainted) &&
                     com_uop.dst_rtype === rtype
      val c7_secret = com_uop.cf_secret_access || com_uop.cf_taint_producer_is_secret

      // Record this slot's write for downstream bypass reads.
      slot_com_writes(w).valid  := late || c7_fires
      slot_com_writes(w).pdst   := com_uop.pdst
      slot_com_writes(w).taint  := true.B
      slot_com_writes(w).secret := Mux(late, true.B, c7_secret)
    }
  }

  if (ENABLE_IFT) {
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
        val has_mem_df = wb_uop.cf_mem_dataflow_atk || wb_uop.cf_mem_sec_dataflow
        when (has_mem_df) {
          taint_table(wb_uop.pdst)           := true.B
          producer_domain_table(wb_uop.pdst) := true.B
          producer_table(wb_uop.pdst)        := wb_uop.cf_op_count_id
          // producer_secret_table not set: attacker-data taint, not secret-origin taint
        }
      }
    }

    // D-G4 fix: retroactive producer_secret_table update for tight-wave loads.
    // cf_secret_access is false at dispatch (set by LSU TLB stage later), so the dispatch-time
    // write at line 544 records false for secret loads.  The LDQ entry IS updated at TLB time
    // (lsu.scala:1097), so the writeback uop carries cf_secret_access=true.  Retroactively
    // correct the table here so consumers renamed after this cycle see the right is_secret bit.
    for (i <- 0 until numWbPorts) {
      when (io.wakeups(i).valid && io.wakeups(i).bits.uop.rf_wen &&
            io.wakeups(i).bits.uop.dst_rtype === rtype &&
            io.wakeups(i).bits.uop.cf_secret_access) {
        producer_secret_table(io.wakeups(i).bits.uop.pdst) := true.B
      }
    }

    // -- Branch snapshot: compute forwarded taint state once and store for any allocating slot --
    // snap_taint applies all slot_taint_writes in one pass (no WireInit chain).
    val snap_taint = Wire(Vec(numPhysRegs, Bool()))
    for (i <- 0 until numPhysRegs) {
      var eff: Bool = taint_table(i)
      for (j <- 0 until plWidth) {
        eff = Mux(slot_taint_writes(j).valid && (slot_taint_writes(j).pdst === i.U),
                  slot_taint_writes(j).taint, eff)
      }
      snap_taint(i) := eff
    }
    for (w <- 0 until plWidth) {
      when (ren2_fire(w) && ren2_uops(w).allocate_brtag) {
        taint_snaps(ren2_uops(w).br_tag) := snap_taint
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
  }  // end if (ENABLE_IFT)

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
