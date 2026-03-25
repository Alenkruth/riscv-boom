//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// RISC-V Processor Core
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// BOOM has the following (conceptual) stages:
//   if0 - Instruction Fetch 0 (next-pc select)
//   if1 - Instruction Fetch 1 (I$ access)
//   if2 - Instruction Fetch 2 (instruction return)
//   if3 - Instruction Fetch 3 (enqueue to fetch buffer)
//   if4 - Instruction Fetch 4 (redirect from bpd)
//   dec - Decode
//   ren - Rename1
//   dis - Rename2/Dispatch
//   iss - Issue
//   rrd - Register Read
//   exe - Execute
//   mem - Memory
//   sxt - Sign-extend
//   wb  - Writeback
//   com - Commit

package boom.v3.exu

import java.nio.file.{Paths}

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.tile.{TraceBundle}
import freechips.rocketchip.rocket.{Causes, PRV, TracedInstruction}
import freechips.rocketchip.util.{Str, UIntIsOneOf, CoreMonitorBundle, CoreFuzzingConstants}
import freechips.rocketchip.devices.tilelink.{PLICConsts, CLINTConsts}

import boom.v3.common._
import boom.v3.ifu.{GlobalHistory, HasBoomFrontendParameters}
import boom.v3.exu.FUConstants._
import boom.v3.util._

/**
 * Top level core object that connects the Frontend to the rest of the pipeline.
 */
class BoomCore()(implicit p: Parameters) extends BoomModule
  with HasBoomFrontendParameters // TODO: Don't add this trait
  with CoreFuzzingConstants
{
  val io = IO(new Bundle {
    val hartid = Input(UInt(hartIdLen.W))
    val interrupts = Input(new freechips.rocketchip.rocket.CoreInterrupts(false))
    val ifu = new boom.v3.ifu.BoomFrontendIO
    val ptw = Flipped(new freechips.rocketchip.rocket.DatapathPTWIO())
    val rocc = Flipped(new freechips.rocketchip.tile.RoCCCoreIO())
    val lsu = Flipped(new boom.v3.lsu.LSUCoreIO)
    val ptw_tlb = new freechips.rocketchip.rocket.TLBPTWIO()
    val trace = Output(new TraceBundle)
    val fcsr_rm = UInt(freechips.rocketchip.tile.FPConstants.RM_SZ.W)
  })

  io.ptw_tlb := DontCare
  io.ptw := DontCare
  io.ifu := DontCare

  //**********************************
  // construct all of the modules

  // Only holds integer-registerfile execution units.
  val exe_units = new boom.v3.exu.ExecutionUnits(fpu=false)
  val jmp_unit_idx = exe_units.jmp_unit_idx
  val jmp_unit = exe_units(jmp_unit_idx)

  // Meanwhile, the FP pipeline holds the FP issue window, FP regfile, and FP arithmetic units.
  var fp_pipeline: FpPipeline = null
  if (usingFPU) fp_pipeline = Module(new FpPipeline)

  // ********************************************************
  // Clear fp_pipeline before use
  if (usingFPU) {
    fp_pipeline.io.ll_wports := DontCare
    fp_pipeline.io.wb_valids := DontCare
    fp_pipeline.io.wb_pdsts  := DontCare
  }

  val numIrfWritePorts        = exe_units.numIrfWritePorts + memWidth
  val numLlIrfWritePorts      = exe_units.numLlIrfWritePorts
  val numIrfReadPorts         = exe_units.numIrfReadPorts

  val numFastWakeupPorts      = exe_units.count(_.bypassable)
  val numAlwaysBypassable     = exe_units.count(_.alwaysBypassable)

  val numIntIssueWakeupPorts  = numIrfWritePorts + numFastWakeupPorts - numAlwaysBypassable // + memWidth for ll_wb
  val numIntRenameWakeupPorts = numIntIssueWakeupPorts
  val numFpWakeupPorts        = if (usingFPU) fp_pipeline.io.wakeups.length else 0

  val decode_units     = for (w <- 0 until decodeWidth) yield { val d = Module(new DecodeUnit); d }
  val dec_brmask_logic = Module(new BranchMaskGenerationLogic(coreWidth))
  val rename_stage     = Module(new RenameStage(coreWidth, numIntPhysRegs, numIntRenameWakeupPorts, false))
  val fp_rename_stage  = if (usingFPU) Module(new RenameStage(coreWidth, numFpPhysRegs, numFpWakeupPorts, true)) else null
  val pred_rename_stage = Module(new PredRenameStage(coreWidth, ftqSz, 1))
  val rename_stages    = if (usingFPU) Seq(rename_stage, fp_rename_stage, pred_rename_stage) else Seq(rename_stage, pred_rename_stage)

  val mem_iss_unit     = Module(new IssueUnitCollapsing(memIssueParam, numIntIssueWakeupPorts))
  mem_iss_unit.suggestName("mem_issue_unit")
  val int_iss_unit     = Module(new IssueUnitCollapsing(intIssueParam, numIntIssueWakeupPorts))
  int_iss_unit.suggestName("int_issue_unit")

  val issue_units      = Seq(mem_iss_unit, int_iss_unit)
  val dispatcher       = Module(new BasicDispatcher)

  val iregfile         = Module(new RegisterFileSynthesizable(
                             numIntPhysRegs,
                             numIrfReadPorts,
                             numIrfWritePorts,
                             xLen,
                             Seq.fill(memWidth) {true} ++ exe_units.bypassable_write_port_mask)) // bypassable ll_wb
  val pregfile         = Module(new RegisterFileSynthesizable(
                            ftqSz,
                            exe_units.numIrfReaders,
                            1,
                            1,
                            Seq(true))) // The jmp unit is always bypassable
  pregfile.io := DontCare // Only use the IO if enableSFBOpt

  // wb arbiter for the 0th ll writeback
  // TODO: should this be a multi-arb?
  val ll_wbarb         = Module(new Arbiter(new ExeUnitResp(xLen), 1 +
                                                                   (if (usingFPU) 1 else 0) +
                                                                   (if (usingRoCC) 1 else 0)))
  val iregister_read   = Module(new RegisterRead(
                           issue_units.map(_.issueWidth).sum,
                           exe_units.withFilter(_.readsIrf).map(_.supportedFuncUnits).toSeq,
                           numIrfReadPorts,
                           exe_units.withFilter(_.readsIrf).map(x => 2).toSeq,
                           exe_units.numTotalBypassPorts,
                           jmp_unit.numBypassStages,
                           xLen))
  val rob              = Module(new Rob(
                           numIrfWritePorts + numFpWakeupPorts, // +memWidth for ll writebacks
                           numFpWakeupPorts))
  
  // Used to wakeup registers in rename and issue. ROB needs to listen to something else.
  val int_iss_wakeups  = Wire(Vec(numIntIssueWakeupPorts, Valid(new ExeUnitResp(xLen))))
  val int_ren_wakeups  = Wire(Vec(numIntRenameWakeupPorts, Valid(new ExeUnitResp(xLen))))
  val pred_wakeup  = Wire(Valid(new ExeUnitResp(1)))

  require (exe_units.length == issue_units.map(_.issueWidth).sum)

  //***********************************
  // Pipeline State Registers and Wires

  // Decode/Rename1 Stage
  val dec_valids = Wire(Vec(coreWidth, Bool()))  // are the decoded instruction valid? It may be held up though.
  val dec_uops   = Wire(Vec(coreWidth, new MicroOp()))
  val dec_fire   = Wire(Vec(coreWidth, Bool()))  // can the instruction fire beyond decode?
                                                    // (can still be stopped in ren or dis)
  val dec_ready  = Wire(Bool())
  val dec_xcpts  = Wire(Vec(coreWidth, Bool()))
  val ren_stalls = Wire(Vec(coreWidth, Bool()))

  // Rename2/Dispatch stage
  val dis_valids = Wire(Vec(coreWidth, Bool()))
  val dis_uops   = Wire(Vec(coreWidth, new MicroOp))
  val dis_fire   = Wire(Vec(coreWidth, Bool()))
  val dis_ready  = Wire(Bool())

  // Issue Stage/Register Read
  val iss_valids = Wire(Vec(exe_units.numIrfReaders, Bool()))
  val iss_uops   = Wire(Vec(exe_units.numIrfReaders, new MicroOp()))
  val bypasses   = Wire(Vec(exe_units.numTotalBypassPorts, Valid(new ExeUnitResp(xLen))))
  val pred_bypasses = Wire(Vec(jmp_unit.numBypassStages, Valid(new ExeUnitResp(1))))
  require(jmp_unit.bypassable)

  // --------------------------------------
  // Dealing with branch resolutions

  // The individual branch resolutions from each ALU
  val brinfos = Reg(Vec(coreWidth, new BrResolutionInfo()))

  // "Merged" branch update info from all ALUs
  // brmask contains masks for rapidly clearing mispredicted instructions
  // brindices contains indices to reset pointers for allocated structures
  //           brindices is delayed a cycle
  val brupdate  = Wire(new BrUpdateInfo)
  val b1    = Wire(new BrUpdateMasks)
  val b2    = Reg(new BrResolutionInfo)

  brupdate.b1 := b1
  brupdate.b2 := b2

  for ((b, a) <- brinfos zip exe_units.alu_units) {
    b := a.io.brinfo
    b.valid := a.io.brinfo.valid && !rob.io.flush.valid
  }
  b1.resolve_mask := brinfos.map(x => x.valid << x.uop.br_tag).reduce(_|_)
  b1.mispredict_mask := brinfos.map(x => (x.valid && x.mispredict) << x.uop.br_tag).reduce(_|_)

  // Find the oldest mispredict and use it to update indices
  var mispredict_val = false.B
  var oldest_mispredict = brinfos(0)
  for (b <- brinfos) {
    val use_this_mispredict = !mispredict_val ||
    b.valid && b.mispredict && IsOlder(b.uop.rob_idx, oldest_mispredict.uop.rob_idx, rob.io.rob_head_idx)

    mispredict_val = mispredict_val || (b.valid && b.mispredict)
    oldest_mispredict = Mux(use_this_mispredict, b, oldest_mispredict)
  }

  b2.mispredict  := mispredict_val
  b2.cfi_type    := oldest_mispredict.cfi_type
  b2.taken       := oldest_mispredict.taken
  b2.pc_sel      := oldest_mispredict.pc_sel
  b2.uop         := UpdateBrMask(brupdate, oldest_mispredict.uop)
  b2.jalr_target := RegNext(jmp_unit.io.brinfo.jalr_target)
  b2.target_offset := oldest_mispredict.target_offset

  val oldest_mispredict_ftq_idx = oldest_mispredict.uop.ftq_idx


  assert (!((brupdate.b1.mispredict_mask =/= 0.U || brupdate.b2.mispredict)
    && rob.io.commit.rollback), "Can't have a mispredict during rollback.")

  // corefuzzing
  // When we detect a mispredict, print a short summary line identifying the branch
  // that caused the flush. This prints before we propagate `brupdate` into the
  // frontend and other modules so it's shown ahead of the per-module speculative dumps.
  when (b2.mispredict) {
    // Print: PC, inst, cfi_type, br_tag, rob_idx, taken, jalr_target (if any)
    printf("[SPECULATIVE][MISPREDICT] pc=0x%x inst=0x%x cfi=%d br_tag=%d rob_idx=%d taken=%d target=0x%x\n",
      Sext.apply(oldest_mispredict.uop.debug_pc(vaddrBits-1,0), xLen),
      oldest_mispredict.uop.debug_inst,
      oldest_mispredict.cfi_type,
      oldest_mispredict.uop.br_tag,
      oldest_mispredict.uop.rob_idx,
      oldest_mispredict.taken,
      b2.jalr_target)
  }

  io.ifu.brupdate := brupdate

  for (eu <- exe_units) {
    eu.io.brupdate := brupdate
  }


  if (usingFPU) {
    fp_pipeline.io.brupdate := brupdate
  }

  // Load/Store Unit & ExeUnits
  val mem_units = exe_units.memory_units
  val mem_resps = mem_units.map(_.io.ll_iresp)
  for (i <- 0 until memWidth) {
    mem_units(i).io.lsu_io <> io.lsu.exe(i)
  }

  //-------------------------------------------------------------
  // Uarch Hardware Performance Events (HPEs)

  val perfEvents = new freechips.rocketchip.rocket.EventSets(Seq(
    new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
      ("exception", () => rob.io.com_xcpt.valid),
      ("nop",       () => false.B),
      ("nop",       () => false.B),
      ("nop",       () => false.B))),

    new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
//      ("I$ blocked",                        () => icache_blocked),
      ("nop",                               () => false.B),
      ("branch misprediction",              () => b2.mispredict),
      ("control-flow target misprediction", () => b2.mispredict &&
                                                  b2.cfi_type === CFI_JALR),
      ("flush",                             () => rob.io.flush.valid),
      ("branch resolved",                   () => b2.valid)
    )),

    new freechips.rocketchip.rocket.EventSet((mask, hits) => (mask & hits).orR, Seq(
      ("I$ miss",     () => io.ifu.perf.acquire),
      ("D$ miss",     () => io.lsu.perf.acquire),
      ("D$ release",  () => io.lsu.perf.release),
      ("ITLB miss",   () => io.ifu.perf.tlbMiss),
      ("DTLB miss",   () => io.lsu.perf.tlbMiss),
      ("L2 TLB miss", () => io.ptw.perf.l2miss)))))
  val csr = Module(new freechips.rocketchip.rocket.CSRFile(perfEvents, boomParams.customCSRs.decls))
  csr.io.inst foreach { c => c := DontCare }
  csr.io.rocc_interrupt := io.rocc.interrupt
  csr.io.mhtinst_read_pseudo := false.B

  val custom_csrs = Wire(new BoomCustomCSRs)
  custom_csrs.csrs.foreach { c => c.stall := false.B; c.set := false.B; c.sdata := DontCare }

  (custom_csrs.csrs zip csr.io.customCSRs).map { case (lhs, rhs) => lhs <> rhs }

  // for the fuzzycore project - AK
  // flag to print the debug log
  io.lsu.cf_debug_lsu_enable := custom_csrs.cf_debug_lsu_enable && custom_csrs.cf_debug_enable
  io.lsu.cf_debug_dcache_enable := custom_csrs.cf_debug_dcache_enable

  rob.io.cf_debug_rob_enable := custom_csrs.cf_debug_rob_enable && custom_csrs.cf_debug_enable
  // corefuzzing: drive cycle-N mispredict UOP so ROB flush log can annotate INFL_PIPELINE_FLUSH correctly
  rob.io.cf_mispredict_uop.valid := b1.mispredict_mask =/= 0.U
  rob.io.cf_mispredict_uop.bits  := oldest_mispredict.uop
  // io.ifu.cf_debug_log := custom_csrs.cf_debug_log

  // Assigning the CSR's output to the cf_tage_to_gshare signal in the frontend.
  io.ifu.cf_bpd_tage_to_gshare := custom_csrs.cf_bpd_tage_to_gshare
  // Assigning the dcache reconfiguration flags to the lsu
  io.lsu.cf_dcache_set_conf := custom_csrs.cf_dcache_set_conf
  io.lsu.cf_dcache_way_conf := custom_csrs.cf_dcache_way_conf
  io.lsu.cf_dcache_size_conf := custom_csrs.cf_dcache_size_conf
  io.lsu.cf_dcache_repl_conf := custom_csrs.cf_dcache_repl_conf
  io.lsu.cf_dcache_blocksize_conf := custom_csrs.cf_dcache_blocksize_conf
  // corefuzzing: wire secret address CSRs into LSU for cf_secret_access detection
  io.lsu.cf_secret_start_addr := custom_csrs.cf_secret_start_addr
  io.lsu.cf_secret_end_addr   := custom_csrs.cf_secret_end_addr

  // getting the rob entries from the CSR
  rob.io.cf_rob_entries := custom_csrs.cf_rob_entries 

  // printf("[CORE] dcacheCSR - 0x%x 0x%x 0x%x 0x%x \n", custom_csrs.cf_dcache_set_conf, custom_csrs.cf_dcache_way_conf, custom_csrs.cf_dcache_size_conf, custom_csrs.cf_dcache_repl_conf)

  // for corefuzzing, assigning CSR output to frontend for reconfigureFB
  io.ifu.reconfigureFB_rows_b0 := custom_csrs.reconfigureFB_rows_b0
  io.ifu.reconfigureFB_rows_b1 := custom_csrs.reconfigureFB_rows_b1
  // corefuzizng// corefuzizng// corefuzizng// corefuzizng
  // Wire cf_debug enables into frontend and core modules
  io.ifu.cf_debug_frontend_enable := custom_csrs.cf_debug_enable && custom_csrs.cf_debug_frontend_enable
  io.ifu.cf_debug_rob_enable      := custom_csrs.cf_debug_rob_enable && custom_csrs.cf_debug_enable
  // corefuzzing: attacker range for ICache/RAS domain tracking in frontend
  io.ifu.cf_attacker_start_addr   := custom_csrs.cf_attacker_start_addr
  io.ifu.cf_attacker_end_addr     := custom_csrs.cf_attacker_end_addr

  // Propagate core-level debug enables to rename and issue units
  rename_stage.io.cf_debug_rename_enable := custom_csrs.cf_debug_enable && custom_csrs.cf_debug_core_enable
  fp_rename_stage.io.cf_debug_rename_enable := custom_csrs.cf_debug_enable && custom_csrs.cf_debug_core_enable
  pred_rename_stage.io.cf_debug_rename_enable := custom_csrs.cf_debug_enable && custom_csrs.cf_debug_core_enable
  // corefuzzing: drive cycle-N mispredict UOP to rename stages (same signal as ROB)
  rename_stage.io.cf_mispredict_uop.valid      := b1.mispredict_mask =/= 0.U
  rename_stage.io.cf_mispredict_uop.bits       := oldest_mispredict.uop
  fp_rename_stage.io.cf_mispredict_uop.valid   := b1.mispredict_mask =/= 0.U
  fp_rename_stage.io.cf_mispredict_uop.bits    := oldest_mispredict.uop
  pred_rename_stage.io.cf_mispredict_uop.valid := b1.mispredict_mask =/= 0.U
  pred_rename_stage.io.cf_mispredict_uop.bits  := oldest_mispredict.uop

  mem_iss_unit.io.cf_debug_issue_enable := custom_csrs.cf_debug_enable && custom_csrs.cf_debug_core_enable
  int_iss_unit.io.cf_debug_issue_enable := custom_csrs.cf_debug_enable && custom_csrs.cf_debug_core_enable

  // LDQ/STQ reconfiguration: 3-bit indices into ldQueueEntryOptions / stQueueEntryOptions
  io.lsu.cf_ldq_idx := custom_csrs.cf_ldq_idx
  io.lsu.cf_stq_idx := custom_csrs.cf_stq_idx

  // Issue queue size reconfiguration: 2-bit index into issueQueueEntryOptions
  mem_iss_unit.io.cf_iq_idx := custom_csrs.cf_iq_idx
  int_iss_unit.io.cf_iq_idx := custom_csrs.cf_iq_idx
  if (usingFPU) {
    fp_pipeline.io.cf_iq_idx := custom_csrs.cf_iq_idx
  }

  // Physical register file size reconfiguration: 3-bit index into pregFileSizeOptions
  rename_stage.io.cf_preg_idx      := custom_csrs.cf_preg_idx
  fp_rename_stage.io.cf_preg_idx   := custom_csrs.cf_preg_idx
  pred_rename_stage.io.cf_preg_idx := custom_csrs.cf_preg_idx

  // corefuzzing
  // Quiescing control signals
  val cf_quiesce_core = Wire(Bool())
  val pipeline_drained = Wire(Bool())
  // Stricter pipeline drain: check fetch buffer, decode stage, and rename2/dispatch stage.
  // dispatch_stage_empty guards against is_unique CSR writes stuck at rename2 (waiting for
  // fencei_rdy) causing pipeline_drained_strict to fire spuriously — which would trigger
  // runaway QS_EXECUTING→QS_FETCH cycles and a "Pipeline has hung" assertion.
  val fetch_buffer_empty   = !io.ifu.fetchpacket.valid
  val decode_stage_empty   = !dec_valids.reduce(_||_)
  val dispatch_stage_empty = !dis_valids.reduce(_||_)
  val pipeline_drained_strict = rob.io.empty && io.lsu.queues_empty && io.lsu.no_pending_mem && io.lsu.fencei_rdy && fetch_buffer_empty && decode_stage_empty && dispatch_stage_empty

  // 4-state FSM for quiesce (cf_chill) control
  // QS_IDLE:      normal operation; fetch ungated
  // QS_DRAINING:  cf_chill=1; block fetch; wait for pipeline to drain
  // QS_FETCH:     pipeline drained; pulse allow_fetch for exactly ONE cycle
  // QS_EXECUTING: packet entered pipeline; block fetch; wait for full drain
  val QS_IDLE      = 0.U(2.W)
  val QS_DRAINING  = 1.U(2.W)
  val QS_FETCH     = 2.U(2.W)
  val QS_EXECUTING = 3.U(2.W)
  val qs_state = RegInit(QS_IDLE)

  switch (qs_state) {
    is (QS_IDLE) {
      when (cf_quiesce_core) { qs_state := QS_DRAINING }
    }
    is (QS_DRAINING) {
      when (!cf_quiesce_core)              { qs_state := QS_IDLE      }
      .elsewhen (pipeline_drained_strict)  { qs_state := QS_FETCH     }
    }
    is (QS_FETCH) {
      // Unconditionally advance; fetch packet now in flight
      qs_state := QS_EXECUTING
    }
    is (QS_EXECUTING) {
      when (!cf_quiesce_core)              { qs_state := QS_IDLE      }
      .elsewhen (pipeline_drained_strict)  { qs_state := QS_FETCH     }
    }
  }

  // for corefuzzing, quiescing the pipeline to enable reconfiguration
  cf_quiesce_core := custom_csrs.cf_chill

  // Determine if pipeline is drained by checking ROB empty and LSU status
  pipeline_drained := pipeline_drained_strict

  // Allow fetch only in IDLE (normal) or during the one-cycle FETCH pulse
  val allow_fetch = !cf_quiesce_core || (qs_state === QS_FETCH)
  io.ifu.allow_fetch := allow_fetch

  //val icache_blocked = !(io.ifu.fetchpacket.valid || RegNext(io.ifu.fetchpacket.valid))
  val icache_blocked = false.B 
  csr.io.counters foreach { c => c.inc := RegNext(perfEvents.evaluate(c.eventSel)) }
  
  // corefuzzing
  // Drive EXU debug enable into all execution units from CSRs
  for (eu <- exe_units) {
    eu.io.cf_debug_exu_enable := custom_csrs.cf_debug_enable && custom_csrs.cf_debug_core_enable
  }

    // corefuzzing
  // Wire FP pipeline cf_debug gates when FPU is enabled
  if (usingFPU) {
    fp_pipeline.io.cf_debug_exu_enable := custom_csrs.cf_debug_enable && custom_csrs.cf_debug_core_enable
    fp_pipeline.io.cf_debug_issue_enable := custom_csrs.cf_debug_enable && custom_csrs.cf_debug_core_enable
  }

  //****************************************
  // Time Stamp Counter & Retired Instruction Counter
  // (only used for printf and vcd dumps - the actual counters are in the CSRFile)
  val debug_tsc_reg = RegInit(0.U(xLen.W))
  val debug_irt_reg = RegInit(0.U(xLen.W))
  val debug_brs     = Reg(Vec(4, UInt(xLen.W)))
  val debug_jals    = Reg(Vec(4, UInt(xLen.W)))
  val debug_jalrs   = Reg(Vec(4, UInt(xLen.W)))

  for (j <- 0 until 4) {
    debug_brs(j) := debug_brs(j) + PopCount(VecInit((0 until coreWidth) map {i =>
      rob.io.commit.arch_valids(i) &&
      (rob.io.commit.uops(i).debug_fsrc === j.U) &&
      rob.io.commit.uops(i).is_br
    }))
    debug_jals(j) := debug_jals(j) + PopCount(VecInit((0 until coreWidth) map {i =>
      rob.io.commit.arch_valids(i) &&
      (rob.io.commit.uops(i).debug_fsrc === j.U) &&
      rob.io.commit.uops(i).is_jal
    }))
    debug_jalrs(j) := debug_jalrs(j) + PopCount(VecInit((0 until coreWidth) map {i =>
      rob.io.commit.arch_valids(i) &&
      (rob.io.commit.uops(i).debug_fsrc === j.U) &&
      rob.io.commit.uops(i).is_jalr
    }))
  }

  dontTouch(debug_brs)
  dontTouch(debug_jals)
  dontTouch(debug_jalrs)

  debug_tsc_reg := debug_tsc_reg + 1.U
  debug_irt_reg := debug_irt_reg + PopCount(rob.io.commit.arch_valids.asUInt)
  dontTouch(debug_tsc_reg)
  dontTouch(debug_irt_reg)

  //****************************************
  // Print-out information about the machine

  val issStr =
    if (enableAgePriorityIssue) " (Age-based Priority)"
    else " (Unordered Priority)"

  // val btbStr =
  //   if (enableBTB) ("" + boomParams.btb.nSets * boomParams.btb.nWays + " entries (" + boomParams.btb.nSets + " x " + boomParams.btb.nWays + " ways)")
  //   else 0
  val btbStr = ""

  val fpPipelineStr =
    if (usingFPU) fp_pipeline.toString
    else ""

  override def toString: String =
    (BoomCoreStringPrefix("====Overall Core Params====") + "\n"
    + exe_units.toString + "\n"
    + fpPipelineStr + "\n"
    + rob.toString + "\n"
    + BoomCoreStringPrefix(
        "===Other Core Params===",
        "Fetch Width           : " + fetchWidth,
        "Decode Width          : " + coreWidth,
        "Issue Width           : " + issueParams.map(_.issueWidth).sum,
        "ROB Size              : " + numRobEntries,
        "Issue Window Size     : " + issueParams.map(_.numEntries) + issStr,
        "Load/Store Unit Size  : " + numLdqEntries + "/" + numStqEntries,
        "Num Int Phys Registers: " + numIntPhysRegs,
        "Num FP  Phys Registers: " + numFpPhysRegs,
        "Max Branch Count      : " + maxBrCount)
    + iregfile.toString + "\n"
    + BoomCoreStringPrefix(
        "Num Slow Wakeup Ports : " + numIrfWritePorts,
        "Num Fast Wakeup Ports : " + exe_units.count(_.bypassable),
        "Num Bypass Ports      : " + exe_units.numTotalBypassPorts) + "\n"
    + BoomCoreStringPrefix(
        "DCache Ways           : " + dcacheParams.nWays,
        "DCache Sets           : " + dcacheParams.nSets,
        "DCache nMSHRs         : " + dcacheParams.nMSHRs,
        "ICache Ways           : " + icacheParams.nWays,
        "ICache Sets           : " + icacheParams.nSets,
        "D-TLB Ways            : " + dcacheParams.nTLBWays,
        "I-TLB Ways            : " + icacheParams.nTLBWays,
        "Paddr Bits            : " + paddrBits,
        "Vaddr Bits            : " + vaddrBits) + "\n"
    + BoomCoreStringPrefix(
        "Using FPU Unit?       : " + usingFPU.toString,
        "Using FDivSqrt?       : " + usingFDivSqrt.toString,
        "Using VM?             : " + usingVM.toString) + "\n") 

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Fetch Stage/Frontend ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------
  io.ifu.redirect_val         := false.B
  io.ifu.redirect_flush       := false.B

  // Breakpoint info
  io.ifu.status  := csr.io.status
  io.ifu.bp      := csr.io.bp
  io.ifu.mcontext := csr.io.mcontext
  io.ifu.scontext := csr.io.scontext

  io.ifu.flush_icache := (0 until coreWidth).map { i =>
    (rob.io.commit.arch_valids(i) && rob.io.commit.uops(i).is_fencei) ||
    (RegNext(dec_valids(i) && dec_uops(i).is_jalr && csr.io.status.debug))
  }.reduce(_||_)

  // TODO FIX THIS HACK
  // The below code works because of two quirks with the flush mechanism
  //  1 ) All flush_on_commit instructions are also is_unique,
  //      In the future, this constraint will be relaxed.
  //  2 ) We send out flush signals one cycle after the commit signal. We need to
  //      mux between one/two cycle delay for the following cases:
  //       ERETs are reported to the CSR two cycles before we send the flush
  //       Exceptions are reported to the CSR on the cycle we send the flush
  // This discrepency should be resolved elsewhere.
  when (RegNext(rob.io.flush.valid)) {
    io.ifu.redirect_val   := true.B
    io.ifu.redirect_flush := true.B
    val flush_typ = RegNext(rob.io.flush.bits.flush_typ)
    // Clear the global history when we flush the ROB (exceptions, AMOs, unique instructions, etc.)
    val new_ghist = WireInit((0.U).asTypeOf(new GlobalHistory))
    new_ghist.current_saw_branch_not_taken := true.B
    new_ghist.ras_idx := io.ifu.get_pc(0).entry.ras_idx
    io.ifu.redirect_ghist := new_ghist
    when (FlushTypes.useCsrEvec(flush_typ)) {
      io.ifu.redirect_pc  := Mux(flush_typ === FlushTypes.eret,
                                 RegNext(RegNext(csr.io.evec)),
                                 csr.io.evec)
    } .otherwise {
      val flush_pc = (AlignPCToBoundary(io.ifu.get_pc(0).pc, icBlockBytes)
                      + RegNext(rob.io.flush.bits.pc_lob)
                      - Mux(RegNext(rob.io.flush.bits.edge_inst), 2.U, 0.U))
      val flush_pc_next = flush_pc + Mux(RegNext(rob.io.flush.bits.is_rvc), 2.U, 4.U)
      io.ifu.redirect_pc := Mux(FlushTypes.useSamePC(flush_typ),
                                flush_pc, flush_pc_next)

    }
    io.ifu.redirect_ftq_idx := RegNext(rob.io.flush.bits.ftq_idx)
  } .elsewhen (brupdate.b2.mispredict && !RegNext(rob.io.flush.valid)) {
    val block_pc = AlignPCToBoundary(io.ifu.get_pc(1).pc, icBlockBytes)
    val uop_maybe_pc = block_pc | brupdate.b2.uop.pc_lob
    val npc = uop_maybe_pc + Mux(brupdate.b2.uop.is_rvc || brupdate.b2.uop.edge_inst, 2.U, 4.U)
    val jal_br_target = Wire(UInt(vaddrBitsExtended.W))
    jal_br_target := (uop_maybe_pc.asSInt + brupdate.b2.target_offset +
      (Fill(vaddrBitsExtended-1, brupdate.b2.uop.edge_inst) << 1).asSInt).asUInt
    val bj_addr = Mux(brupdate.b2.cfi_type === CFI_JALR, brupdate.b2.jalr_target, jal_br_target)
    val mispredict_target = Mux(brupdate.b2.pc_sel === PC_PLUS4, npc, bj_addr)
    io.ifu.redirect_val     := true.B
    io.ifu.redirect_pc      := mispredict_target
    io.ifu.redirect_flush   := true.B
    io.ifu.redirect_ftq_idx := brupdate.b2.uop.ftq_idx
    val use_same_ghist = (brupdate.b2.cfi_type === CFI_BR &&
                          !brupdate.b2.taken &&
                          bankAlign(block_pc) === bankAlign(npc))
    val ftq_entry = io.ifu.get_pc(1).entry
    val cfi_idx = (brupdate.b2.uop.pc_lob ^
      Mux(ftq_entry.start_bank === 1.U, 1.U << log2Ceil(bankBytes), 0.U))(log2Ceil(fetchWidth), 1)
    val ftq_ghist = io.ifu.get_pc(1).ghist
    val next_ghist = ftq_ghist.update(
      ftq_entry.br_mask.asUInt,
      brupdate.b2.taken,
      brupdate.b2.cfi_type === CFI_BR,
      cfi_idx,
      true.B,
      io.ifu.get_pc(1).pc,
      ftq_entry.cfi_is_call && ftq_entry.cfi_idx.bits === cfi_idx,
      ftq_entry.cfi_is_ret  && ftq_entry.cfi_idx.bits === cfi_idx)


    io.ifu.redirect_ghist   := Mux(
      use_same_ghist,
      ftq_ghist,
      next_ghist)
    io.ifu.redirect_ghist.current_saw_branch_not_taken := use_same_ghist
  } .elsewhen (rob.io.flush_frontend || brupdate.b1.mispredict_mask =/= 0.U) {
    io.ifu.redirect_flush   := true.B
  }

  // Tell the FTQ it can deallocate entries by passing youngest ftq_idx.
  val youngest_com_idx = (coreWidth-1).U - PriorityEncoder(rob.io.commit.valids.reverse)
  io.ifu.commit.valid := rob.io.commit.valids.reduce(_|_) || rob.io.com_xcpt.valid
  io.ifu.commit.bits  := Mux(rob.io.com_xcpt.valid,
                             rob.io.com_xcpt.bits.ftq_idx,
                             rob.io.commit.uops(youngest_com_idx).ftq_idx)

  // corefuzzing: multi-port FTQ secret marking at dispatch time (s_prop) and TLB time (s_acc via LSU).
  // Design principle: mark the FTQ entry as soon as the secret state is known, regardless of commit.
  // This captures transient flows — speculative instructions that touch secret state even if squashed.
  //
  // Ports 0..coreWidth-1: dispatch stage — s_prop is determined here (taint table, C5 queue stall).
  //   Fires for every dispatched instruction with cf_secret_propagation=true.
  //   Multiple slots can fire simultaneously for different FTQ entries — no conflict (Reg(Vec)).
  //
  // Ports coreWidth..coreWidth+memWidth-1: wired from LSU TLB stage (lsu.cf_secret_ftq_updates).
  //   Fires when in_secret is determined at address resolution — captures s_acc speculatively.
  for (w <- 0 until coreWidth) {
    io.ifu.cf_secret_ftq_updates(w).valid := dis_fire(w) && dis_uops(w).cf_secret_propagation
    io.ifu.cf_secret_ftq_updates(w).bits  := dis_uops(w).ftq_idx
  }
  for (w <- 0 until memWidth) {
    io.ifu.cf_secret_ftq_updates(coreWidth + w) := io.lsu.cf_secret_ftq_updates(w)
  }

  assert(!(rob.io.commit.valids.reduce(_|_) && rob.io.com_xcpt.valid),
    "ROB can't commit and except in same cycle!")

  for (i <- 0 until memWidth) {
    when (RegNext(io.lsu.exe(i).req.bits.sfence.valid)) {
      io.ifu.sfence := RegNext(io.lsu.exe(i).req.bits.sfence)
    }
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Branch Prediction ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Decode Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // track mask of finished instructions in the bundle
  // use this to mask out insts coming from FetchBuffer that have been finished
  // for example, back pressure may cause us to only issue some instructions from FetchBuffer
  // but on the next cycle, we only want to retry a subset
  val dec_finished_mask = RegInit(0.U(coreWidth.W))

  //-------------------------------------------------------------
  // Pull out instructions and send to the Decoders

  io.ifu.fetchpacket.ready := dec_ready
  val dec_fbundle = io.ifu.fetchpacket.bits

  //-------------------------------------------------------------
  // Decoders

  val single_step_active = cf_quiesce_core // && pipeline_drained - pipeline would not be drained when there is an instruction being executed
  // Propagate cf_single_step flag: set for all uops in fetch group during single-step
  val uop_with_step = Wire(Vec(coreWidth, new MicroOp()))
  
  for (w <- 0 until coreWidth) {
    dec_valids(w)                      := io.ifu.fetchpacket.valid && dec_fbundle.uops(w).valid &&
                      !dec_finished_mask(w)
  //  decode_units(w).io.wq.uop := dec_fbundle.uops(w).bits
    uop_with_step(w) := dec_fbundle.uops(w).bits
    uop_with_step(w).cf_single_step := single_step_active
    // corefuzzing: set cf_domain_id early so it propagates through rename pipeline registers
    // (needed for taint table writes in rename-stage.scala at ren2/dispatch time)
    val dec_pc = dec_fbundle.uops(w).bits.debug_pc
    val dec_in_attacker_range = (dec_pc >= custom_csrs.cf_attacker_start_addr) &&
                                (dec_pc <= custom_csrs.cf_attacker_end_addr) &&
                                (custom_csrs.cf_attacker_end_addr =/= custom_csrs.cf_attacker_start_addr)
    uop_with_step(w).cf_domain_id := dec_in_attacker_range.asUInt
    decode_units(w).io.enq.uop         := uop_with_step(w)
    decode_units(w).io.status          := csr.io.status
    decode_units(w).io.csr_decode      <> csr.io.decode(w)
    decode_units(w).io.interrupt       := csr.io.interrupt
    decode_units(w).io.interrupt_cause := csr.io.interrupt_cause

    dec_uops(w) := decode_units(w).io.deq.uop
  }
  // corefuzzing: [FLUSH] logging for decode-stage uops killed by branch mispredict or ROB flush.
  // SRC=1 (decode). Gated by cf_debug_rob_enable + cf_debug_enable.
  {
    val decFlushInflFmt = (0 until numInfluencerSlotsCF).zipWithIndex.map{case(_,k) => s"I$k={v=%d,oc=%d,ty=%d,atk=%d,sec=%d,dc=%d}"}.mkString(" ")
    val decFlushFmt = s"[FLUSH] 0x%x (0x%x) CF(domain=%d spec=%d atk=%d s_acc=%d s_prop=%d s_tx=%d opcount=%d spec_atk=%d spec_oc=%d) FU=0x%x SRC=%d INFL_FU=0x%x OVF=%d $decFlushInflFmt\n"
    def printDecFlush(uop: MicroOp): Unit = {
      val inflArgs = (0 until numInfluencerSlotsCF).flatMap(k => Seq[Bits](
        uop.cf_influencer_list(k).valid,
        uop.cf_influencer_list(k).op_count,
        uop.cf_influencer_list(k).infl_type,
        uop.cf_influencer_list(k).is_atk,
        uop.cf_influencer_list(k).is_secret,
        uop.cf_influencer_list(k).deny_count
      ))
      printf(decFlushFmt, (Seq[Bits](
        Sext(uop.debug_pc(vaddrBits-1,0), xLen), uop.debug_inst,
        uop.cf_domain_id, uop.cf_speculated, uop.cf_attacker_influence,
        uop.cf_secret_access, uop.cf_secret_propagation, uop.cf_secret_transmission,
        uop.cf_op_count_id, uop.cf_spec_branch_is_atk, uop.cf_spec_branch_op_id,
        uop.cf_fu_bitmap, 1.U, inflBitmapFromList(uop.cf_influencer_list), uop.cf_infl_overflow
      ) ++ inflArgs): _*)
    }
    for (w <- 0 until coreWidth) {
      when (custom_csrs.cf_debug_rob_enable && custom_csrs.cf_debug_enable) {
        // Path A: branch mispredict kills this instruction
        when (dec_valids(w) && IsKilledByBranch(brupdate, dec_uops(w))) {
          val dec_fu_base = dec_uops(w)
          val dec_fu = WireInit(dec_fu_base)
          when (brupdate.b2.mispredict &&
                brupdate.b2.uop.cf_domain_id =/= dec_fu_base.cf_domain_id) {
            val br_uop = brupdate.b2.uop
            dec_fu := addInfluencer(dec_fu_base, br_uop.cf_op_count_id, INFL_PIPELINE_FLUSH.U,
              is_atk = br_uop.cf_domain_id === 1.U,
              is_secret = br_uop.cf_secret_access || br_uop.cf_secret_propagation)
          }
          printDecFlush(dec_fu)
        }
        // Path B: ROB flush (exception, FENCE.I, etc.) — kills all in-flight decode instructions
        .elsewhen (dec_valids(w) && rob.io.flush.valid) {
          printDecFlush(dec_uops(w))
        }
      }
    }
  }
  //-------------------------------------------------------------
  // FTQ GetPC Port Arbitration

  val jmp_pc_req  = Wire(Decoupled(UInt(log2Ceil(ftqSz).W)))
  val xcpt_pc_req = Wire(Decoupled(UInt(log2Ceil(ftqSz).W)))
  val flush_pc_req = Wire(Decoupled(UInt(log2Ceil(ftqSz).W)))

  val ftq_arb = Module(new Arbiter(UInt(log2Ceil(ftqSz).W), 3))

  // Order by the oldest. Flushes come from the oldest instructions in pipe
  // Decoding exceptions come from youngest
  ftq_arb.io.in(0) <> flush_pc_req
  ftq_arb.io.in(1) <> jmp_pc_req
  ftq_arb.io.in(2) <> xcpt_pc_req

  // Hookup FTQ
  io.ifu.get_pc(0).ftq_idx := ftq_arb.io.out.bits
  ftq_arb.io.out.ready  := true.B

  // Branch Unit Requests (for JALs) (Should delay issue of JALs if this not ready)
  jmp_pc_req.valid := RegNext(iss_valids(jmp_unit_idx) && iss_uops(jmp_unit_idx).fu_code === FU_JMP)
  jmp_pc_req.bits  := RegNext(iss_uops(jmp_unit_idx).ftq_idx)

  jmp_unit.io.get_ftq_pc := DontCare
  jmp_unit.io.get_ftq_pc.pc               := io.ifu.get_pc(0).pc
  jmp_unit.io.get_ftq_pc.entry            := io.ifu.get_pc(0).entry
  jmp_unit.io.get_ftq_pc.next_val         := io.ifu.get_pc(0).next_val
  jmp_unit.io.get_ftq_pc.next_pc          := io.ifu.get_pc(0).next_pc


  // Frontend Exception Requests
  val xcpt_idx = PriorityEncoder(dec_xcpts)
  xcpt_pc_req.valid    := dec_xcpts.reduce(_||_)
  xcpt_pc_req.bits     := dec_uops(xcpt_idx).ftq_idx
  //rob.io.xcpt_fetch_pc := RegEnable(io.ifu.get_pc.fetch_pc, dis_ready)
  rob.io.xcpt_fetch_pc := io.ifu.get_pc(0).pc

  flush_pc_req.valid   := rob.io.flush.valid
  flush_pc_req.bits    := rob.io.flush.bits.ftq_idx

  // Mispredict requests (to get the correct target)
  io.ifu.get_pc(1).ftq_idx := oldest_mispredict_ftq_idx


  //-------------------------------------------------------------
  // Decode/Rename1 pipeline logic

  dec_xcpts := dec_uops zip dec_valids map {case (u,v) => u.exception && v}
  val dec_xcpt_stall = dec_xcpts.reduce(_||_) && !xcpt_pc_req.ready
  // stall fetch/dcode because we ran out of branch tags
  val branch_mask_full = Wire(Vec(coreWidth, Bool()))

  val dec_hazards = (0 until coreWidth).map(w =>
                      dec_valids(w) &&
                      (  !dis_ready
                      || rob.io.commit.rollback
                      || dec_xcpt_stall
                      || branch_mask_full(w)
                      || brupdate.b1.mispredict_mask =/= 0.U
                      || brupdate.b2.mispredict
                      || io.ifu.redirect_flush))

  val dec_stalls = dec_hazards.scanLeft(false.B) ((s,h) => s || h).takeRight(coreWidth)
  dec_fire := (0 until coreWidth).map(w => dec_valids(w) && !dec_stalls(w))

  // all decoders are empty and ready for new instructions
  dec_ready := dec_fire.last

  when (dec_ready || io.ifu.redirect_flush) {
    dec_finished_mask := 0.U
  } .otherwise {
    dec_finished_mask := dec_fire.asUInt | dec_finished_mask
  }

  //-------------------------------------------------------------
  // Branch Mask Logic

  dec_brmask_logic.io.brupdate := brupdate
  dec_brmask_logic.io.flush_pipeline := RegNext(rob.io.flush.valid)

  for (w <- 0 until coreWidth) {
    dec_brmask_logic.io.is_branch(w) := !dec_finished_mask(w) && dec_uops(w).allocate_brtag
    dec_brmask_logic.io.will_fire(w) :=  dec_fire(w) &&
                                         dec_uops(w).allocate_brtag // ren, dis can back pressure us
    dec_uops(w).br_tag  := dec_brmask_logic.io.br_tag(w)
    dec_uops(w).br_mask := dec_brmask_logic.io.br_mask(w)
  }

  branch_mask_full := dec_brmask_logic.io.is_full

  // corefuzzing: branch-domain tracking table.
  // When a branch/JALR is decoded and allocated a br_tag, record:
  //   spec_branch_atk_table(br_tag) = 1 if that branch is in attacker domain
  //   spec_branch_op_table(br_tag)  = cf_op_count_id of that branch
  // This allows computing, at dispatch, whether a uop is speculated under an
  // attacker-domain branch (cf_spec_branch_is_atk / cf_spec_branch_op_id).
  // No rollback needed: stale entries are harmless because freed tags will be
  // overwritten before they appear in any future uop's br_mask.
  val spec_branch_atk_table = Reg(Vec(maxBrCount, Bool()))
  val spec_branch_op_table  = Reg(Vec(maxBrCount, UInt(uopIDCounterWidthCF.W)))

  for (w <- 0 until coreWidth) {
    when (dec_fire(w) && dec_uops(w).allocate_brtag) {
      val tag = dec_brmask_logic.io.br_tag(w)
      spec_branch_atk_table(tag) := dec_uops(w).cf_domain_id =/= 0.U
      spec_branch_op_table(tag)  := dec_uops(w).cf_op_count_id
    }
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Register Rename Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Inputs
  for (rename <- rename_stages) {
    rename.io.kill := io.ifu.redirect_flush
    rename.io.brupdate := brupdate

    rename.io.debug_rob_empty := rob.io.empty

    rename.io.dec_fire := dec_fire
    rename.io.dec_uops := dec_uops

    rename.io.dis_fire := dis_fire
    rename.io.dis_ready := dis_ready

    rename.io.com_valids := rob.io.commit.valids
    rename.io.com_uops := rob.io.commit.uops
    rename.io.rbk_valids := rob.io.commit.rbk_valids
    rename.io.rollback := rob.io.commit.rollback
  }


  // Outputs
  dis_uops := rename_stage.io.ren2_uops
  dis_valids := rename_stage.io.ren2_mask
  ren_stalls := rename_stage.io.ren_stalls


  /**
   * TODO This is a bit nasty, but it's currently necessary to
   * split the INT/FP rename pipelines into separate instantiations.
   * Won't have to do this anymore with a properly decoupled FP pipeline.
   */
  for (w <- 0 until coreWidth) {
    val i_uop   = rename_stage.io.ren2_uops(w)
    val f_uop   = if (usingFPU) fp_rename_stage.io.ren2_uops(w) else NullMicroOp()
    val p_uop   = if (enableSFBOpt) pred_rename_stage.io.ren2_uops(w) else NullMicroOp()
    val f_stall = if (usingFPU) fp_rename_stage.io.ren_stalls(w) else false.B
    val p_stall = if (enableSFBOpt) pred_rename_stage.io.ren_stalls(w) else false.B

    // lrs1 can "pass through" to prs1. Used solely to index the csr file.
    dis_uops(w).prs1 := Mux(dis_uops(w).lrs1_rtype === RT_FLT, f_uop.prs1,
                        Mux(dis_uops(w).lrs1_rtype === RT_FIX, i_uop.prs1, dis_uops(w).lrs1))
    dis_uops(w).prs2 := Mux(dis_uops(w).lrs2_rtype === RT_FLT, f_uop.prs2, i_uop.prs2)
    dis_uops(w).prs3 := f_uop.prs3
    dis_uops(w).ppred := p_uop.ppred
    dis_uops(w).pdst := Mux(dis_uops(w).dst_rtype  === RT_FLT, f_uop.pdst,
                        Mux(dis_uops(w).dst_rtype  === RT_FIX, i_uop.pdst,
                                                               p_uop.pdst))
    dis_uops(w).stale_pdst := Mux(dis_uops(w).dst_rtype === RT_FLT, f_uop.stale_pdst, i_uop.stale_pdst)

    dis_uops(w).prs1_busy := i_uop.prs1_busy && (dis_uops(w).lrs1_rtype === RT_FIX) ||
                             f_uop.prs1_busy && (dis_uops(w).lrs1_rtype === RT_FLT)
    dis_uops(w).prs2_busy := i_uop.prs2_busy && (dis_uops(w).lrs2_rtype === RT_FIX) ||
                             f_uop.prs2_busy && (dis_uops(w).lrs2_rtype === RT_FLT)
    dis_uops(w).prs3_busy := f_uop.prs3_busy && dis_uops(w).frs3_en
    dis_uops(w).ppred_busy := p_uop.ppred_busy && dis_uops(w).is_sfb_shadow

    ren_stalls(w) := rename_stage.io.ren_stalls(w) || f_stall || p_stall
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Dispatch Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  //-------------------------------------------------------------
  // Rename2/Dispatch pipeline logic

  val dis_prior_slot_valid = dis_valids.scanLeft(false.B) ((s,v) => s || v)
  val dis_prior_slot_unique = (dis_uops zip dis_valids).scanLeft(false.B) {case (s,(u,v)) => s || v && u.is_unique}
  val wait_for_empty_pipeline = (0 until coreWidth).map(w => (dis_uops(w).is_unique || custom_csrs.disableOOO) &&
                                  (!rob.io.empty || !io.lsu.fencei_rdy || dis_prior_slot_valid(w)))
  val rocc_shim_busy = if (usingRoCC) !exe_units.rocc_unit.io.rocc.rxq_empty else false.B
  val wait_for_rocc = (0 until coreWidth).map(w =>
                        (dis_uops(w).is_fence || dis_uops(w).is_fencei) && (io.rocc.busy || rocc_shim_busy))
  val rxq_full = if (usingRoCC) exe_units.rocc_unit.io.rocc.rxq_full else false.B
  val block_rocc = (dis_uops zip dis_valids).map{case (u,v) => v && u.uopc === uopROCC}.scanLeft(rxq_full)(_||_)
  val dis_rocc_alloc_stall = (dis_uops.map(_.uopc === uopROCC) zip block_rocc) map {case (p,r) =>
                               if (usingRoCC) p && r else false.B}

  val dis_hazards = (0 until coreWidth).map(w =>
                      dis_valids(w) &&
                      (  !rob.io.ready
                      || ren_stalls(w)
                      || io.lsu.ldq_full(w) && dis_uops(w).uses_ldq
                      || io.lsu.stq_full(w) && dis_uops(w).uses_stq
                      || !dispatcher.io.ren_uops(w).ready
                      || wait_for_empty_pipeline(w)
                      || wait_for_rocc(w)
                      || dis_prior_slot_unique(w)
                      || dis_rocc_alloc_stall(w)
                      || brupdate.b1.mispredict_mask =/= 0.U
                      || brupdate.b2.mispredict
                      || io.ifu.redirect_flush))


  io.lsu.fence_dmem := (dis_valids zip wait_for_empty_pipeline).map {case (v,w) => v && w} .reduce(_||_)

  val dis_stalls = dis_hazards.scanLeft(false.B) ((s,h) => s || h).takeRight(coreWidth)
  dis_fire := dis_valids zip dis_stalls map {case (v,s) => v && !s}
  dis_ready := !dis_stalls.last

  //-------------------------------------------------------------
  // LDQ/STQ Allocation Logic

  for (w <- 0 until coreWidth) {
    // Dispatching instructions request load/store queue entries when they can proceed.
    dis_uops(w).ldq_idx := io.lsu.dis_ldq_idx(w)
    dis_uops(w).stq_idx := io.lsu.dis_stq_idx(w)
  }

  //-------------------------------------------------------------
  // corefuzzing: Set IFT fields at dispatch time
  // cf_domain_id: 1 if PC is in attacker address range (from CSR), 0 otherwise
  // cf_speculated: true if any branch is unresolved in the branch mask
  for (w <- 0 until coreWidth) {
    val pc = dis_uops(w).debug_pc
    val in_attacker_range = (pc >= custom_csrs.cf_attacker_start_addr) &&
                            (pc <= custom_csrs.cf_attacker_end_addr) &&
                            (custom_csrs.cf_attacker_end_addr =/= custom_csrs.cf_attacker_start_addr)
    dis_uops(w).cf_domain_id  := in_attacker_range.asUInt
    dis_uops(w).cf_speculated := dis_uops(w).br_mask =/= 0.U

    // corefuzzing: determine whether any outstanding branch in br_mask is attacker-domain.
    // Looks up spec_branch_atk_table for each set bit; takes the first (lowest br_tag) hit.
    val br_atk_bits = VecInit((0 until maxBrCount).map(i =>
      dis_uops(w).br_mask(i) && spec_branch_atk_table(i)))
    dis_uops(w).cf_spec_branch_is_atk := br_atk_bits.reduce(_ || _)
    dis_uops(w).cf_spec_branch_op_id  := Mux(
      br_atk_bits.reduce(_ || _),
      spec_branch_op_table(PriorityEncoder(br_atk_bits)),
      0.U)
  }

  // corefuzzing: Unified dispatch influencer computation.
  // Chains INFL_REG_DATAFLOW (taint), INFL_REG_PRESSURE (freelist), INFL_ROB_FULL, INFL_LDQ_FULL, INFL_STQ_FULL.
  // All additions use rename_stage.io.ren2_uops(w) as the cycle-free base to avoid
  // combinational loops that would arise from reading dis_uops(w) after writing it.
  val dis_stall_was_rob = (0 until coreWidth).map(w =>
    RegNext(dis_valids(w) && !dis_fire(w) && !rob.io.ready))
  val dis_stall_was_ldq = (0 until coreWidth).map(w =>
    RegNext(dis_valids(w) && !dis_fire(w) && io.lsu.ldq_full(w) && dis_uops(w).uses_ldq))
  val dis_stall_was_stq = (0 until coreWidth).map(w =>
    RegNext(dis_valids(w) && !dis_fire(w) && io.lsu.stq_full(w) && dis_uops(w).uses_stq))
  // C5: capture whether the blocking queue head was secret-dependent at stall time.
  // Used to propagate s_prop to the stalled instruction.
  val dis_stall_was_rob_secret = (0 until coreWidth).map(w =>
    RegNext(dis_valids(w) && !dis_fire(w) && !rob.io.ready && rob.io.rob_head_is_secret))
  val dis_stall_was_ldq_secret = (0 until coreWidth).map(w =>
    RegNext(dis_valids(w) && !dis_fire(w) && io.lsu.ldq_full(w) && dis_uops(w).uses_ldq && io.lsu.ldq_head_is_secret))
  val dis_stall_was_stq_secret = (0 until coreWidth).map(w =>
    RegNext(dis_valids(w) && !dis_fire(w) && io.lsu.stq_full(w) && dis_uops(w).uses_stq && io.lsu.stq_head_is_secret))
  // Capture the BLOCKING head's attributes during the stall cycle.
  // On the fire cycle, the queue head has already advanced; we need the stall-cycle head for correct influencer attribution.
  val dis_stall_rob_head_op_count = (0 until coreWidth).map(w =>
    RegEnable(rob.io.rob_head_op_count, dis_valids(w) && !dis_fire(w) && !rob.io.ready))
  val dis_stall_rob_head_domain   = (0 until coreWidth).map(w =>
    RegEnable(rob.io.rob_head_domain,   dis_valids(w) && !dis_fire(w) && !rob.io.ready))
  val dis_stall_rob_head_is_secret= (0 until coreWidth).map(w =>
    RegEnable(rob.io.rob_head_is_secret,dis_valids(w) && !dis_fire(w) && !rob.io.ready))
  val dis_stall_ldq_head_op_count = (0 until coreWidth).map(w =>
    RegEnable(io.lsu.ldq_head_op_count, dis_valids(w) && !dis_fire(w) && io.lsu.ldq_full(w) && dis_uops(w).uses_ldq))
  val dis_stall_ldq_head_domain   = (0 until coreWidth).map(w =>
    RegEnable(io.lsu.ldq_head_domain,   dis_valids(w) && !dis_fire(w) && io.lsu.ldq_full(w) && dis_uops(w).uses_ldq))
  val dis_stall_ldq_head_is_secret= (0 until coreWidth).map(w =>
    RegEnable(io.lsu.ldq_head_is_secret,dis_valids(w) && !dis_fire(w) && io.lsu.ldq_full(w) && dis_uops(w).uses_ldq))
  val dis_stall_stq_head_op_count = (0 until coreWidth).map(w =>
    RegEnable(io.lsu.stq_head_op_count, dis_valids(w) && !dis_fire(w) && io.lsu.stq_full(w) && dis_uops(w).uses_stq))
  val dis_stall_stq_head_domain   = (0 until coreWidth).map(w =>
    RegEnable(io.lsu.stq_head_domain,   dis_valids(w) && !dis_fire(w) && io.lsu.stq_full(w) && dis_uops(w).uses_stq))
  val dis_stall_stq_head_is_secret= (0 until coreWidth).map(w =>
    RegEnable(io.lsu.stq_head_is_secret,dis_valids(w) && !dis_fire(w) && io.lsu.stq_full(w) && dis_uops(w).uses_stq))
  // REG_PRESSURE: stall due to physical register freelist exhausted (INT or FP rename)
  val dis_stall_was_reg = (0 until coreWidth).map(w =>
    RegNext(dis_valids(w) && !dis_fire(w) && ren_stalls(w)))
  val dis_stall_reg_head_op_count = (0 until coreWidth).map(w =>
    RegEnable(rob.io.rob_head_op_count, dis_valids(w) && !dis_fire(w) && ren_stalls(w)))
  val dis_stall_reg_head_domain   = (0 until coreWidth).map(w =>
    RegEnable(rob.io.rob_head_domain,   dis_valids(w) && !dis_fire(w) && ren_stalls(w)))
  val dis_stall_reg_head_is_secret= (0 until coreWidth).map(w =>
    RegEnable(rob.io.rob_head_is_secret,dis_valids(w) && !dis_fire(w) && ren_stalls(w)))
  for (w <- 0 until coreWidth) {
    val pre = rename_stage.io.ren2_uops(w)  // cycle-free base
    // Taint paths:
    //   is_any_tainted  — tainted source from OTHER domain (domain=0 instr reading atk-written reg):
    //                     fires INFL_REG_DATAFLOW with is_atk=true (cross-domain attack influence).
    //   is_sec_tainted  — tainted source that had s_acc/s_prop: fires cf_secret_propagation.
    //                     NO domain gate: attacker instructions propagating secret data also get s_prop.
    //   reg_df_fire     — fires INFL_REG_DATAFLOW for BOTH cross-domain and same-domain-secret cases.
    //   reg_df_is_atk   — true ONLY for cross-domain cases (is_any_tainted). Same-domain secret
    //                     propagation uses is_atk=false, is_secret=true to distinguish it clearly.
    //   preg_src_secret — source preg was secret-tainted in-flight (preg_secret set, taint_table not yet).
    //                     Catches the case where producer's TLB fired AFTER consumer's dispatch but
    //                     BEFORE consumer's dispatch checks. Fires separate candidate with op_count=0.
    val is_any_tainted  = dis_uops(w).cf_src_tainted && dis_uops(w).cf_domain_id === 0.U
    val is_sec_tainted  = dis_uops(w).cf_src_tainted && dis_uops(w).cf_taint_producer_is_secret
    val reg_df_fire     = is_any_tainted || is_sec_tainted
    val reg_df_is_atk   = is_any_tainted && pre.cf_taint_producer_is_atk
    // In-flight preg_secret: check at dispatch whether source regs are already secret-tainted
    // (set by TLB stage of an in-flight producer that hasn't committed yet).
    // Only fire if not already covered by reg_df_fire (avoids duplicate INFL_REG_DATAFLOW entries).
    val preg_src_secret = preg_secret(pre.prs1) || preg_secret(pre.prs2)
    val preg_only_fire  = preg_src_secret && !reg_df_fire

    when (is_sec_tainted || preg_src_secret) { dis_uops(w).cf_secret_propagation := true.B }

    // Steps 1–5: inject dispatch-level influencers in parallel using addInfluencerBatch.
    // All candidates are evaluated from the same base (pre) simultaneously.
    // Step 6 (INFL_ISSUE_CONTENTION) is injected at issue time via ROB update bus.
    val dis_hol_ldq = dis_fire(w) && dis_uops(w).uses_ldq && io.lsu.ldq_head_valid &&
                      (io.lsu.ldq_head_domain =/= dis_uops(w).cf_domain_id)
    val dis_hol_stq = dis_fire(w) && dis_uops(w).uses_stq && io.lsu.stq_head_valid &&
                      (io.lsu.stq_head_domain =/= dis_uops(w).cf_domain_id)
    val dis_hol_any   = dis_hol_ldq || dis_hol_stq
    val hol_head_op   = Mux(dis_hol_ldq, io.lsu.ldq_head_op_count,      io.lsu.stq_head_op_count)
    val hol_is_atk    = Mux(dis_hol_ldq, io.lsu.ldq_head_domain === 1.U, io.lsu.stq_head_domain === 1.U)
    val hol_is_secret = Mux(dis_hol_ldq, io.lsu.ldq_head_is_secret,      io.lsu.stq_head_is_secret)

    val post_dis = addInfluencerBatch(pre, Seq(
      // reg_df_fire: cross-domain taint (is_atk=reg_df_is_atk) OR same-domain secret (is_atk=false).
      // reg_df_is_atk is true only for cross-domain cases; same-domain secret gets is_atk=false, is_secret=true.
      InfluencerCandidate(reg_df_fire,                         pre.cf_taint_producer_op,        INFL_REG_DATAFLOW.U, reg_df_is_atk,                         pre.cf_taint_producer_is_secret),
      // preg_only_fire: in-flight preg_secret at dispatch (producer not yet committed to taint_table).
      // op_count=0 (unknown at dispatch time — producer hasn't committed); is_atk=false (same-domain secret).
      InfluencerCandidate(preg_only_fire,                      0.U,                             INFL_REG_DATAFLOW.U, false.B,                               true.B),
      InfluencerCandidate(dis_fire(w) && dis_stall_was_reg(w), dis_stall_reg_head_op_count(w),  INFL_REG_PRESSURE.U, dis_stall_reg_head_domain(w) === 1.U,  dis_stall_reg_head_is_secret(w)),
      InfluencerCandidate(dis_fire(w) && dis_stall_was_rob(w), dis_stall_rob_head_op_count(w),  INFL_ROB_FULL.U,     dis_stall_rob_head_domain(w) === 1.U,  dis_stall_rob_head_is_secret(w)),
      InfluencerCandidate(dis_fire(w) && dis_stall_was_ldq(w), dis_stall_ldq_head_op_count(w),  INFL_LDQ_FULL.U,     dis_stall_ldq_head_domain(w) === 1.U,  dis_stall_ldq_head_is_secret(w)),
      InfluencerCandidate(dis_fire(w) && dis_stall_was_stq(w), dis_stall_stq_head_op_count(w),  INFL_STQ_FULL.U,     dis_stall_stq_head_domain(w) === 1.U,  dis_stall_stq_head_is_secret(w)),
      InfluencerCandidate(dis_hol_any,                          hol_head_op,                     INFL_MEM_HOL.U,      hol_is_atk,                            hol_is_secret),
    ))

    dis_uops(w).cf_influencer_list    := post_dis.cf_influencer_list
    dis_uops(w).cf_infl_overflow      := post_dis.cf_infl_overflow
    dis_uops(w).cf_attacker_influence := post_dis.cf_attacker_influence

    // C5: if the blocking queue head was secret-dependent, propagate s_prop to this instruction.
    when (dis_fire(w) && (dis_stall_was_rob_secret(w) || dis_stall_was_ldq_secret(w) || dis_stall_was_stq_secret(w))) {
      dis_uops(w).cf_secret_propagation := true.B
    }
  }

  //-------------------------------------------------------------
  // Rob Allocation Logic

  rob.io.enq_valids := dis_fire
  rob.io.enq_uops   := dis_uops
  rob.io.enq_partial_stall := dis_stalls.last // TODO come up with better ROB compacting scheme.
  rob.io.debug_tsc := debug_tsc_reg
  rob.io.csr_stall := csr.io.csr_stall

  // Minor hack: ecall and breaks need to increment the FTQ deq ptr earlier than commit, since
  // they write their PC into the CSR the cycle before they commit.
  // Since these are also unique, increment the FTQ ptr when they are dispatched
  when (RegNext(dis_fire.reduce(_||_) && dis_uops(PriorityEncoder(dis_fire)).is_sys_pc2epc)) {
    io.ifu.commit.valid := true.B
    io.ifu.commit.bits  := RegNext(dis_uops(PriorityEncoder(dis_valids)).ftq_idx)
  }

  for (w <- 0 until coreWidth) {
    // note: this assumes uops haven't been shifted - there's a 1:1 match between PC's LSBs and "w" here
    // (thus the LSB of the rob_idx gives part of the PC)
    if (coreWidth == 1) {
      dis_uops(w).rob_idx := rob.io.rob_tail_idx
    } else {
      dis_uops(w).rob_idx := Cat(rob.io.rob_tail_idx >> log2Ceil(coreWidth).U,
                               w.U(log2Ceil(coreWidth).W))
    }
  }

  //-------------------------------------------------------------
  // RoCC allocation logic
  if (usingRoCC) {
    for (w <- 0 until coreWidth) {
      // We guarantee only decoding 1 RoCC instruction per cycle
      dis_uops(w).rxq_idx := exe_units.rocc_unit.io.rocc.rxq_idx(w)
    }
  }

  //-------------------------------------------------------------
  // Dispatch to issue queues

  // Get uops from rename2
  for (w <- 0 until coreWidth) {
    dispatcher.io.ren_uops(w).valid := dis_fire(w)
    dispatcher.io.ren_uops(w).bits  := dis_uops(w)
  }

  var iu_idx = 0
  // Send dispatched uops to correct issue queues
  // Backpressure through dispatcher if necessary
  for (i <- 0 until issueParams.size) {
    if (issueParams(i).iqType == IQT_FP.litValue) {
       fp_pipeline.io.dis_uops <> dispatcher.io.dis_uops(i)
    } else {
       issue_units(iu_idx).io.dis_uops <> dispatcher.io.dis_uops(i)
       iu_idx += 1
    }
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Issue Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  require (issue_units.map(_.issueWidth).sum == exe_units.length)

  var iss_wu_idx = 1
  var ren_wu_idx = 1
  // The 0th wakeup port goes to the ll_wbarb
  int_iss_wakeups(0).valid := ll_wbarb.io.out.fire && ll_wbarb.io.out.bits.uop.dst_rtype === RT_FIX
  int_iss_wakeups(0).bits  := ll_wbarb.io.out.bits

  int_ren_wakeups(0).valid := ll_wbarb.io.out.fire && ll_wbarb.io.out.bits.uop.dst_rtype === RT_FIX
  int_ren_wakeups(0).bits  := ll_wbarb.io.out.bits

  for (i <- 1 until memWidth) {
    int_iss_wakeups(i).valid := mem_resps(i).valid && mem_resps(i).bits.uop.dst_rtype === RT_FIX
    int_iss_wakeups(i).bits  := mem_resps(i).bits

    int_ren_wakeups(i).valid := mem_resps(i).valid && mem_resps(i).bits.uop.dst_rtype === RT_FIX
    int_ren_wakeups(i).bits  := mem_resps(i).bits
    iss_wu_idx += 1
    ren_wu_idx += 1
  }

  // loop through each issue-port (exe_units are statically connected to an issue-port)
  for (i <- 0 until exe_units.length) {
    if (exe_units(i).writesIrf) {
      val fast_wakeup = Wire(Valid(new ExeUnitResp(xLen)))
      val slow_wakeup = Wire(Valid(new ExeUnitResp(xLen)))
      fast_wakeup := DontCare
      slow_wakeup := DontCare

      val resp = exe_units(i).io.iresp
      assert(!(resp.valid && resp.bits.uop.rf_wen && resp.bits.uop.dst_rtype =/= RT_FIX))

      // Fast Wakeup (uses just-issued uops that have known latencies)
      fast_wakeup.bits.uop := iss_uops(i)
      fast_wakeup.valid    := iss_valids(i) &&
                              iss_uops(i).bypassable &&
                              iss_uops(i).dst_rtype === RT_FIX &&
                              iss_uops(i).ldst_val &&
                              !(io.lsu.ld_miss && (iss_uops(i).iw_p1_poisoned || iss_uops(i).iw_p2_poisoned))

      // Slow Wakeup (uses write-port to register file)
      slow_wakeup.bits.uop := resp.bits.uop
      slow_wakeup.valid    := resp.valid &&
                                resp.bits.uop.rf_wen &&
                                !resp.bits.uop.bypassable &&
                                resp.bits.uop.dst_rtype === RT_FIX

      if (exe_units(i).bypassable) {
        int_iss_wakeups(iss_wu_idx) := fast_wakeup
        iss_wu_idx += 1
      }
      if (!exe_units(i).alwaysBypassable) {
        int_iss_wakeups(iss_wu_idx) := slow_wakeup
        iss_wu_idx += 1
      }

      if (exe_units(i).bypassable) {
        int_ren_wakeups(ren_wu_idx) := fast_wakeup
        ren_wu_idx += 1
      }
      if (!exe_units(i).alwaysBypassable) {
        int_ren_wakeups(ren_wu_idx) := slow_wakeup
        ren_wu_idx += 1
      }
    }
  }
  require (iss_wu_idx == numIntIssueWakeupPorts)
  require (ren_wu_idx == numIntRenameWakeupPorts)
  require (iss_wu_idx == ren_wu_idx)

  // jmp unit performs fast wakeup of the predicate bits
  require (jmp_unit.bypassable)
  pred_wakeup.valid := (iss_valids(jmp_unit_idx) &&
                        iss_uops(jmp_unit_idx).is_sfb_br &&
                        !(io.lsu.ld_miss && (iss_uops(jmp_unit_idx).iw_p1_poisoned || iss_uops(jmp_unit_idx).iw_p2_poisoned))
  )
  pred_wakeup.bits.uop := iss_uops(jmp_unit_idx)
  pred_wakeup.bits.fflags := DontCare
  pred_wakeup.bits.data := DontCare
  pred_wakeup.bits.predicated := DontCare

  // Perform load-hit speculative wakeup through a special port (performs a poison wake-up).
  issue_units map { iu =>
     iu.io.spec_ld_wakeup := io.lsu.spec_ld_wakeup
  }


  // Connect the predicate wakeup port
  issue_units map { iu =>
    iu.io.pred_wakeup_port.valid := false.B
    iu.io.pred_wakeup_port.bits := DontCare
  }
  if (enableSFBOpt) {
    int_iss_unit.io.pred_wakeup_port.valid := pred_wakeup.valid
    int_iss_unit.io.pred_wakeup_port.bits := pred_wakeup.bits.uop.pdst
  }


  // ----------------------------------------------------------------
  // Connect the wakeup ports to the busy tables in the rename stages

  for ((renport, intport) <- rename_stage.io.wakeups zip int_ren_wakeups) {
    renport <> intport
  }
  if (usingFPU) {
    for ((renport, fpport) <- fp_rename_stage.io.wakeups zip fp_pipeline.io.wakeups) {
       renport <> fpport
    }
  }
  if (enableSFBOpt) {
    pred_rename_stage.io.wakeups(0) := pred_wakeup
  } else {
    pred_rename_stage.io.wakeups := DontCare
  }

  // If we issue loads back-to-back endlessly (probably because we are executing some tight loop)
  // the store buffer will never drain, breaking the memory-model forward-progress guarantee
  // If we see a large number of loads saturate the LSU, pause for a cycle to let a store drain
  val loads_saturating = (mem_iss_unit.io.iss_valids(0) && mem_iss_unit.io.iss_uops(0).uses_ldq)
  val saturating_loads_counter = RegInit(0.U(5.W))
  when (loads_saturating) { saturating_loads_counter := saturating_loads_counter + 1.U }
  .otherwise { saturating_loads_counter := 0.U }
  val pause_mem = RegNext(loads_saturating) && saturating_loads_counter === ~(0.U(5.W))

  var iss_idx = 0
  var int_iss_cnt = 0
  var mem_iss_cnt = 0
  for (w <- 0 until exe_units.length) {
    var fu_types = exe_units(w).io.fu_types
    val exe_unit = exe_units(w)
    if (exe_unit.readsIrf) {
      if (exe_unit.supportedFuncUnits.muld) {
        // Supress just-issued divides from issuing back-to-back, since it's an iterative divider.
        // But it takes a cycle to get to the Exe stage, so it can't tell us it is busy yet.
        val idiv_issued = iss_valids(iss_idx) && iss_uops(iss_idx).fu_code_is(FU_DIV)
        fu_types = fu_types & RegNext(~Mux(idiv_issued, FU_DIV, 0.U))
      }

      if (exe_unit.hasMem) {
        iss_valids(iss_idx) := mem_iss_unit.io.iss_valids(mem_iss_cnt)
        iss_uops(iss_idx)   := mem_iss_unit.io.iss_uops(mem_iss_cnt)
        mem_iss_unit.io.fu_types(mem_iss_cnt) := Mux(pause_mem, 0.U, fu_types)
        mem_iss_cnt += 1
      } else {
        iss_valids(iss_idx) := int_iss_unit.io.iss_valids(int_iss_cnt)
        iss_uops(iss_idx)   := int_iss_unit.io.iss_uops(int_iss_cnt)
        int_iss_unit.io.fu_types(int_iss_cnt) := fu_types
        int_iss_cnt += 1
      }
      iss_idx += 1
    }
  }
  require(iss_idx == exe_units.numIrfReaders)

  issue_units.map(_.io.tsc_reg := debug_tsc_reg)
  issue_units.map(_.io.brupdate := brupdate)
  issue_units.map(_.io.flush_pipeline := RegNext(rob.io.flush.valid))

  // Load-hit Misspeculations
  require (mem_iss_unit.issueWidth <= 2)
  issue_units.map(_.io.ld_miss := io.lsu.ld_miss)

  mem_units.map(u => u.io.com_exception := RegNext(rob.io.flush.valid))

  // Wakeup (Issue & Writeback)
  for {
    iu <- issue_units
    (issport, wakeup) <- iu.io.wakeup_ports zip int_iss_wakeups
  }{
    issport.valid := wakeup.valid
    issport.bits.pdst := wakeup.bits.uop.pdst
    issport.bits.poisoned := wakeup.bits.uop.iw_p1_poisoned || wakeup.bits.uop.iw_p2_poisoned

    require (iu.io.wakeup_ports.length == int_iss_wakeups.length)
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Register Read Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Register Read <- Issue (rrd <- iss)
  iregister_read.io.rf_read_ports <> iregfile.io.read_ports
  iregister_read.io.prf_read_ports := DontCare
  if (enableSFBOpt) {
    iregister_read.io.prf_read_ports <> pregfile.io.read_ports
  }

  for (w <- 0 until exe_units.numIrfReaders) {
    iregister_read.io.iss_valids(w) :=
      iss_valids(w) && !(io.lsu.ld_miss && (iss_uops(w).iw_p1_poisoned || iss_uops(w).iw_p2_poisoned))
  }
  iregister_read.io.iss_uops := iss_uops
  iregister_read.io.iss_uops map { u => u.iw_p1_poisoned := false.B; u.iw_p2_poisoned := false.B }

  iregister_read.io.brupdate := brupdate
  iregister_read.io.kill   := RegNext(rob.io.flush.valid)

  iregister_read.io.bypass := bypasses
  iregister_read.io.pred_bypass := pred_bypasses

  //-------------------------------------------------------------
  // Privileged Co-processor 0 Register File
  // Note: Normally this would be bad in that I'm writing state before
  // committing, so to get this to work I stall the entire pipeline for
  // CSR instructions so I never speculate these instructions.

  val csr_exe_unit = exe_units.csr_unit

  // for critical path reasons, we aren't zero'ing this out if resp is not valid
  val csr_rw_cmd = csr_exe_unit.io.iresp.bits.uop.ctrl.csr_cmd
  val wb_wdata = csr_exe_unit.io.iresp.bits.data

  csr.io.rw.addr        := csr_exe_unit.io.iresp.bits.uop.csr_addr
  csr.io.rw.cmd         := freechips.rocketchip.rocket.CSR.maskCmd(csr_exe_unit.io.iresp.valid, csr_rw_cmd)
  csr.io.rw.wdata       := wb_wdata

  rob.io.csr_replay.valid := csr_exe_unit.io.iresp.valid && csr.io.rw_stall
  rob.io.csr_replay.bits.uop := csr_exe_unit.io.iresp.bits.uop
  rob.io.csr_replay.bits.cause := MINI_EXCEPTION_CSR_REPLAY
  rob.io.csr_replay.bits.badvaddr := DontCare

  // Extra I/O
  // Delay retire/exception 1 cycle
  csr.io.retire    := RegNext(PopCount(rob.io.commit.arch_valids.asUInt))
  csr.io.exception := RegNext(rob.io.com_xcpt.valid)
  // csr.io.pc used for setting EPC during exception or CSR.io.trace.

  csr.io.pc        := (boom.v3.util.AlignPCToBoundary(io.ifu.get_pc(0).com_pc, icBlockBytes)
                     + RegNext(rob.io.com_xcpt.bits.pc_lob)
                     - Mux(RegNext(rob.io.com_xcpt.bits.edge_inst), 2.U, 0.U))
  // Cause not valid for for CALL or BREAKPOINTs (CSRFile will override it).
  csr.io.cause     := RegNext(rob.io.com_xcpt.bits.cause)
  csr.io.ungated_clock := clock

  val tval_valid = csr.io.exception &&
    csr.io.cause.isOneOf(
      //Causes.illegal_instruction.U, we currently only write 0x0 for illegal instructions
      Causes.breakpoint.U,
      Causes.misaligned_load.U,
      Causes.misaligned_store.U,
      Causes.load_access.U,
      Causes.store_access.U,
      Causes.fetch_access.U,
      Causes.load_page_fault.U,
      Causes.store_page_fault.U,
      Causes.fetch_page_fault.U)

  csr.io.tval := Mux(tval_valid,
    RegNext(encodeVirtualAddress(rob.io.com_xcpt.bits.badvaddr, rob.io.com_xcpt.bits.badvaddr)), 0.U)

  // TODO move this function to some central location (since this is used elsewhere).
  def encodeVirtualAddress(a0: UInt, ea: UInt) =
    if (vaddrBitsExtended == vaddrBits) {
      ea
    } else {
      // Efficient means to compress 64-bit VA into vaddrBits+1 bits.
      // (VA is bad if VA(vaddrBits) != VA(vaddrBits-1)).
      val a = a0.asSInt >> vaddrBits
      val msb = Mux(a === 0.S || a === -1.S, ea(vaddrBits), !ea(vaddrBits-1))
      Cat(msb, ea(vaddrBits-1,0))
    }

  // reading requires serializing the entire pipeline
  csr.io.fcsr_flags.valid := rob.io.commit.fflags.valid
  csr.io.fcsr_flags.bits  := rob.io.commit.fflags.bits
  csr.io.set_fs_dirty.get := rob.io.commit.fflags.valid

  exe_units.withFilter(_.hasFcsr).map(_.io.fcsr_rm := csr.io.fcsr_rm)
  io.fcsr_rm := csr.io.fcsr_rm

  if (usingFPU) {
    fp_pipeline.io.fcsr_rm := csr.io.fcsr_rm
  }

  csr.io.hartid := io.hartid
  csr.io.interrupts := io.interrupts

  // we do not support the H-extension
  csr.io.htval := DontCare
  csr.io.gva := DontCare

// TODO can we add this back in, but handle reset properly and save us
//      the mux above on csr.io.rw.cmd?
//   assert (!(csr_rw_cmd =/= rocket.CSR.N && !exe_units(0).io.resp(0).valid),
//   "CSRFile is being written to spuriously.")

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Execute Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  iss_idx = 0
  var bypass_idx = 0
  for (w <- 0 until exe_units.length) {
    val exe_unit = exe_units(w)
    if (exe_unit.readsIrf) {
      exe_unit.io.req <> iregister_read.io.exe_reqs(iss_idx)

      if (exe_unit.bypassable) {
        for (i <- 0 until exe_unit.numBypassStages) {
          bypasses(bypass_idx) := exe_unit.io.bypass(i)
          bypass_idx += 1
        }
      }
      iss_idx += 1
    }
  }
  require (bypass_idx == exe_units.numTotalBypassPorts)
  for (i <- 0 until jmp_unit.numBypassStages) {
    pred_bypasses(i) := jmp_unit.io.bypass(i)
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Load/Store Unit ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // enqueue basic load/store info in Decode
  for (w <- 0 until coreWidth) {
    io.lsu.dis_uops(w).valid := dis_fire(w)
    io.lsu.dis_uops(w).bits  := dis_uops(w)
  }

  // tell LSU about committing loads and stores to clear entries
  io.lsu.commit                  := rob.io.commit

  // tell LSU that it should fire a load that waits for the rob to clear
  io.lsu.commit_load_at_rob_head := rob.io.com_load_is_at_rob_head

  //com_xcpt.valid comes too early, will fight against a branch that resolves same cycle as an exception
  io.lsu.exception := RegNext(rob.io.flush.valid)

  // Handle Branch Mispeculations
  io.lsu.brupdate := brupdate
  io.lsu.rob_head_idx := rob.io.rob_head_idx
  io.lsu.rob_pnr_idx  := rob.io.rob_pnr_idx

  io.lsu.tsc_reg := debug_tsc_reg


  if (usingFPU) {
    io.lsu.fp_stdata <> fp_pipeline.io.to_sdq
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Writeback Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  var w_cnt = 1
  iregfile.io.write_ports(0) := WritePort(ll_wbarb.io.out, ipregSz, xLen, RT_FIX)
  ll_wbarb.io.in(0) <> mem_resps(0)
  assert (ll_wbarb.io.in(0).ready) // never backpressure the memory unit.
  for (i <- 1 until memWidth) {
    iregfile.io.write_ports(w_cnt) := WritePort(mem_resps(i), ipregSz, xLen, RT_FIX)
    w_cnt += 1
  }

  for (i <- 0 until exe_units.length) {
    if (exe_units(i).writesIrf) {
      val wbresp = exe_units(i).io.iresp
      // do we need this though? 
      // is there a chance for the uop in the rob to wait for a port?
      // Make a local copy of the uop so we can append an IRF writeback tag
      //val wbresp_uop_copy = WireInit(wbresp.bits.uop)
      //when (wbresp.valid && wbresp.bits.uop.rf_wen) {
      //  wbresp_uop_copy.appendModuleTag(irfTagCF)
      //}
      val wbpdst = wbresp.bits.uop.pdst//wbresp_uop_copy.pdst
      val wbdata = wbresp.bits.data

      def wbIsValid(rtype: UInt) =
        wbresp.valid && wbresp.bits.uop.rf_wen && wbresp.bits.uop.dst_rtype === rtype
      val wbReadsCSR = wbresp.bits.uop.ctrl.csr_cmd =/= freechips.rocketchip.rocket.CSR.N

      iregfile.io.write_ports(w_cnt).valid     := wbIsValid(RT_FIX)
      iregfile.io.write_ports(w_cnt).bits.addr := wbpdst
      wbresp.ready := true.B
      if (exe_units(i).hasCSR) {
        iregfile.io.write_ports(w_cnt).bits.data := Mux(wbReadsCSR, csr.io.rw.rdata, wbdata)
      } else {
        iregfile.io.write_ports(w_cnt).bits.data := wbdata
      }

      assert (!wbIsValid(RT_FLT), "[fppipeline] An FP writeback is being attempted to the Int Regfile.")

      assert (!(wbresp.valid &&
        !wbresp.bits.uop.rf_wen &&
        wbresp.bits.uop.dst_rtype === RT_FIX),
        "[fppipeline] An Int writeback is being attempted with rf_wen disabled.")

      assert (!(wbresp.valid &&
        wbresp.bits.uop.rf_wen &&
        wbresp.bits.uop.dst_rtype =/= RT_FIX),
        "[fppipeline] writeback being attempted to Int RF with dst != Int type exe_units("+i+").iresp")
      w_cnt += 1
    }
  }
  require(w_cnt == iregfile.io.write_ports.length)

  if (enableSFBOpt) {
    pregfile.io.write_ports(0).valid     := jmp_unit.io.iresp.valid && jmp_unit.io.iresp.bits.uop.is_sfb_br
    pregfile.io.write_ports(0).bits.addr := jmp_unit.io.iresp.bits.uop.pdst
    pregfile.io.write_ports(0).bits.data := jmp_unit.io.iresp.bits.data
  }

  if (usingFPU) {
    // Connect IFPU
    fp_pipeline.io.from_int  <> exe_units.ifpu_unit.io.ll_fresp
    // Connect FPIU
    ll_wbarb.io.in(1)        <> fp_pipeline.io.to_int
    // Connect FLDs
    fp_pipeline.io.ll_wports <> exe_units.memory_units.map(_.io.ll_fresp).toSeq
  }
  if (usingRoCC) {
    require(usingFPU)
    ll_wbarb.io.in(2)       <> exe_units.rocc_unit.io.ll_iresp
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Commit Stage ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // Writeback
  // ---------
  // First connect the ll_wport
  val ll_uop = ll_wbarb.io.out.bits.uop
  rob.io.wb_resps(0).valid  := ll_wbarb.io.out.valid && !(ll_uop.uses_stq && !ll_uop.is_amo)
  rob.io.wb_resps(0).bits   <> ll_wbarb.io.out.bits
  rob.io.debug_wb_valids(0) := ll_wbarb.io.out.valid && ll_uop.dst_rtype =/= RT_X
  rob.io.debug_wb_wdata(0)  := ll_wbarb.io.out.bits.data
  var cnt = 1
  for (i <- 1 until memWidth) {
    val mem_uop = mem_resps(i).bits.uop
    rob.io.wb_resps(cnt).valid := mem_resps(i).valid && !(mem_uop.uses_stq && !mem_uop.is_amo)
    rob.io.wb_resps(cnt).bits  := mem_resps(i).bits
    rob.io.debug_wb_valids(cnt) := mem_resps(i).valid && mem_uop.dst_rtype =/= RT_X
    rob.io.debug_wb_wdata(cnt)  := mem_resps(i).bits.data
    cnt += 1
  }
  var f_cnt = 0 // rob fflags port index
  for (eu <- exe_units) {
    if (eu.writesIrf)
    {
      val resp   = eu.io.iresp
      val wb_uop = resp.bits.uop
      val data   = resp.bits.data

      rob.io.wb_resps(cnt).valid := resp.valid && !(wb_uop.uses_stq && !wb_uop.is_amo)
      rob.io.wb_resps(cnt).bits  <> resp.bits
      rob.io.debug_wb_valids(cnt) := resp.valid && wb_uop.rf_wen && wb_uop.dst_rtype === RT_FIX
      if (eu.hasFFlags) {
        rob.io.fflags(f_cnt) <> resp.bits.fflags
        f_cnt += 1
      }
      if (eu.hasCSR) {
        rob.io.debug_wb_wdata(cnt) := Mux(wb_uop.ctrl.csr_cmd =/= freechips.rocketchip.rocket.CSR.N,
          csr.io.rw.rdata,
          data)
      } else {
        rob.io.debug_wb_wdata(cnt) := data
      }
      cnt += 1
    }
  }

  require(cnt == numIrfWritePorts)
  if (usingFPU) {
    for ((wdata, wakeup) <- fp_pipeline.io.debug_wb_wdata zip fp_pipeline.io.wakeups) {
      rob.io.wb_resps(cnt) <> wakeup
      rob.io.fflags(f_cnt) <> wakeup.bits.fflags
      rob.io.debug_wb_valids(cnt) := wakeup.valid
      rob.io.debug_wb_wdata(cnt) := wdata
      cnt += 1
      f_cnt += 1

      assert (!(wakeup.valid && wakeup.bits.uop.dst_rtype =/= RT_FLT),
        "[core] FP wakeup does not write back to a FP register.")

      assert (!(wakeup.valid && !wakeup.bits.uop.fp_val),
        "[core] FP wakeup does not involve an FP instruction.")
    }
  }

  require (cnt == rob.numWakeupPorts)
  require (f_cnt == rob.numFpuPorts)

  // branch resolution
  rob.io.brupdate <> brupdate

  exe_units.map(u => u.io.status := csr.io.status)
  if (usingFPU)
    fp_pipeline.io.status := csr.io.status

  // Connect breakpoint info to memaddrcalcunit
  for (i <- 0 until memWidth) {
    mem_units(i).io.status   := csr.io.status
    mem_units(i).io.bp       := csr.io.bp
    mem_units(i).io.mcontext := csr.io.mcontext
    mem_units(i).io.scontext := csr.io.scontext
  }

  // LSU <> ROB
  rob.io.lsu_clr_bsy           := io.lsu.clr_bsy
  rob.io.lsu_clr_bsy_cf_bitmap := io.lsu.clr_bsy_cf_bitmap
  rob.io.lsu_clr_bsy_cf_stx   := io.lsu.clr_bsy_cf_stx
  rob.io.lsu_clr_unsafe        := io.lsu.clr_unsafe
  rob.io.lxcpt          <> io.lsu.lxcpt

  // corefuzzing: issue contention updates — flatten per-port outputs from all issue units
  rob.io.cf_issue_contention_upd := VecInit(issue_units.flatMap(u => u.io.cf_contention_upd))

  assert (!(csr.io.singleStep), "[core] single-step is unsupported.")


  //-------------------------------------------------------------
  // **** Flush Pipeline ****
  //-------------------------------------------------------------
  // flush on exceptions, miniexeptions, and after some special instructions

  if (usingFPU) {
    fp_pipeline.io.flush_pipeline := RegNext(rob.io.flush.valid)
  }

  for (w <- 0 until exe_units.length) {
    exe_units(w).io.req.bits.kill := RegNext(rob.io.flush.valid)
  }

  assert (!(rob.io.com_xcpt.valid && !rob.io.flush.valid),
    "[core] exception occurred, but pipeline flush signal not set!")

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Outputs to the External World ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------

  // detect pipeline freezes and throw error
  val idle_cycles = freechips.rocketchip.util.WideCounter(32)
  when (rob.io.commit.valids.asUInt.orR ||
        csr.io.csr_stall ||
        io.rocc.busy ||
        reset.asBool) {
    idle_cycles := 0.U
  }
  assert (!(idle_cycles.value(13)), "Pipeline has hung.")

  if (usingFPU) {
    fp_pipeline.io.debug_tsc_reg := debug_tsc_reg
  }

  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // **** Handle Cycle-by-Cycle Printouts ****
  //-------------------------------------------------------------
  //-------------------------------------------------------------


  // COMMIT_LOG_PRINTF is a compile-time flag (enabled for CoreFuzzingConfig via
  // WithBoomCommitLogPrintf).  The commit log always fires — it is NOT gated by
  // the runtime cf_debug_log CSR (0xbc1).  The CSR only controls the FLUSH-log
  // (ROB and fetch-buffer squash prints [FLUSH:ROB] / [FLUSH:FB]) via
  // cf_debug_rob_enable (bit 4).  Decode/rename no longer emit FLUSH lines.
  //
  // arch_valids(w) fires for architecturally committed instructions only.
  // These are by definition non-speculative at commit time (all branches prior
  // to the ROB head have resolved). The cf_speculated field records the
  // dispatch-time speculation state (br_mask≠0 when dispatched) but does NOT
  // mean the instruction is still speculative at commit.
  if (COMMIT_LOG_PRINTF) {
    var new_commit_cnt = 0.U

    for (w <- 0 until coreWidth) {
      val priv = RegNext(csr.io.status.prv) // erets change the privilege. Get the old one

      // To allow for diffs against spike :/
      def printf_inst(uop: MicroOp) = {
        when (uop.is_rvc) {
          printf("(0x%x)", uop.debug_inst(15,0))
        } .otherwise {
          printf("(0x%x)", uop.debug_inst)
        }
      }

      when (rob.io.commit.arch_valids(w)) {
        // ---------------------------------------------------------------------
        // BEGIN MOD: Commit logging extended for core-fuzzing (cf_*) fields
        // Non-IFT fields (priv, pc, inst, rd, wdata) are unchanged from the
        // original BOOM commit log format used for spike diffs.
        // ---------------------------------------------------------------------
        // NOTE: The original printf lines are preserved below, but commented
        // out to keep a record of the previous behavior. We now print the
        // standard fields followed by a compact summary of all `cf_*`
        // (corefuzzing) MicroOp fields available at commit time.
        //
        // Old code (commented):
        /*
        printf("%d 0x%x ",
          priv,
          Sext(rob.io.commit.uops(w).debug_pc(vaddrBits-1,0), xLen))
        printf_inst(rob.io.commit.uops(w))
        // corefuzzing
        // Print single-step flag for debug
        when (rob.io.commit.uops(w).cf_single_step) {
          printf(" [SSTEP]")
        }
        */

        // New: print the original minimal info first
        printf("%d 0x%x ",
          priv,
          Sext(rob.io.commit.uops(w).debug_pc(vaddrBits-1,0), xLen))
        printf_inst(rob.io.commit.uops(w))

        // New: Print corefuzzing fields. This prints a compact, human-
        // readable summary containing domain, speculation/attacker/secret
        // flags, op count, single-step, taint modules (up to 5), and the
        // predispatch taint queue (up to 5). Each taint entry is shown as
        // module:type:opcount. If some fields are zero, they will print as
        // zeros.
        printf(" CF(domain=%d spec=%d atk=%d s_acc=%d s_prop=%d s_tx=%d opcount=%d spec_atk=%d spec_oc=%d) ",
          rob.io.commit.uops(w).cf_domain_id,
          rob.io.commit.uops(w).cf_speculated,
          rob.io.commit.uops(w).cf_attacker_influence,
          rob.io.commit.uops(w).cf_secret_access,
          rob.io.commit.uops(w).cf_secret_propagation,
          rob.io.commit.uops(w).cf_secret_transmission,
          rob.io.commit.uops(w).cf_op_count_id,
          rob.io.commit.uops(w).cf_spec_branch_is_atk,
          rob.io.commit.uops(w).cf_spec_branch_op_id)

        // Print the single-step marker separately (preserves prior visible tag)
        when (rob.io.commit.uops(w).cf_single_step) {
          printf("[SSTEP] ")
        }

        // Print module bitmap, INFL_FU bitmap, and influencer slots (single atomic printf)
        val comInflFmt = (0 until numInfluencerSlotsCF).zipWithIndex.map{case(_,k) => s"I$k={v=%d,oc=%d,ty=%d,atk=%d,sec=%d,dc=%d}"}.mkString(" ")
        val comFmt = s"FU=0x%x INFL_FU=0x%x OVF=%d $comInflFmt"
        val comInflArgs = (0 until numInfluencerSlotsCF).flatMap(k => Seq[Bits](
          rob.io.commit.uops(w).cf_influencer_list(k).valid,
          rob.io.commit.uops(w).cf_influencer_list(k).op_count,
          rob.io.commit.uops(w).cf_influencer_list(k).infl_type,
          rob.io.commit.uops(w).cf_influencer_list(k).is_atk,
          rob.io.commit.uops(w).cf_influencer_list(k).is_secret,
          rob.io.commit.uops(w).cf_influencer_list(k).deny_count
        ))
        printf(comFmt, (Seq[Bits](
          rob.io.commit.uops(w).cf_fu_bitmap,
          inflBitmapFromList(rob.io.commit.uops(w).cf_influencer_list),
          rob.io.commit.uops(w).cf_infl_overflow
        ) ++ comInflArgs): _*)
        // END MOD: commit CF prints
        // ---------------------------------------------------------------------
        when (rob.io.commit.uops(w).dst_rtype === RT_FIX && rob.io.commit.uops(w).ldst =/= 0.U) {
          printf(" x%d 0x%x\n",
            rob.io.commit.uops(w).ldst,
            rob.io.commit.debug_wdata(w))
        } .elsewhen (rob.io.commit.uops(w).dst_rtype === RT_FLT) {
          printf(" f%d 0x%x\n",
            rob.io.commit.uops(w).ldst,
            rob.io.commit.debug_wdata(w))
        } .otherwise {
          printf("\n")
        }
      }
    }
  } else if (BRANCH_PRINTF) {
    val debug_ghist = RegInit(0.U(globalHistoryLength.W))
    when (rob.io.flush.valid && FlushTypes.useCsrEvec(rob.io.flush.bits.flush_typ)) {
      debug_ghist := 0.U
    }

    var new_ghist = debug_ghist

    for (w <- 0 until coreWidth) {
      when (rob.io.commit.arch_valids(w) &&
        (rob.io.commit.uops(w).is_br || rob.io.commit.uops(w).is_jal || rob.io.commit.uops(w).is_jalr)) {
        // for (i <- 0 until globalHistoryLength) {
        //   printf("%x", new_ghist(globalHistoryLength-i-1))
        // }
        // printf("\n")
        printf("%x %x %x %x %x %x\n",
          rob.io.commit.uops(w).debug_fsrc, rob.io.commit.uops(w).taken,
          rob.io.commit.uops(w).is_br, rob.io.commit.uops(w).is_jal,
          rob.io.commit.uops(w).is_jalr, Sext(rob.io.commit.uops(w).debug_pc(vaddrBits-1,0), xLen))

      }
      new_ghist = Mux(rob.io.commit.arch_valids(w) && rob.io.commit.uops(w).is_br,
        Mux(rob.io.commit.uops(w).taken, new_ghist << 1 | 1.U(1.W), new_ghist << 1),
        new_ghist)
    }
    debug_ghist := new_ghist
  }

  // TODO: Does anyone want this debugging functionality?
  val coreMonitorBundle = Wire(new CoreMonitorBundle(xLen, fLen))
  coreMonitorBundle := DontCare
  coreMonitorBundle.clock  := clock
  coreMonitorBundle.reset  := reset


  //-------------------------------------------------------------
  //-------------------------------------------------------------
  // Page Table Walker

  io.ptw.ptbr       := csr.io.ptbr
  io.ptw.status     := csr.io.status
  io.ptw.pmp        := csr.io.pmp
  io.ptw.sfence     := io.ifu.sfence

  //-------------------------------------------------------------
  //-------------------------------------------------------------

  io.rocc := DontCare
  io.rocc.exception := csr.io.exception && csr.io.status.xs.orR
  io.rocc.csrs <> csr.io.roccCSRs
  if (usingRoCC) {
    exe_units.rocc_unit.io.rocc.rocc         <> io.rocc
    exe_units.rocc_unit.io.rocc.dis_uops     := dis_uops
    exe_units.rocc_unit.io.rocc.rob_head_idx := rob.io.rob_head_idx
    exe_units.rocc_unit.io.rocc.rob_pnr_idx  := rob.io.rob_pnr_idx
    exe_units.rocc_unit.io.com_exception     := rob.io.flush.valid
    exe_units.rocc_unit.io.status            := csr.io.status

    for (w <- 0 until coreWidth) {
      exe_units.rocc_unit.io.rocc.dis_rocc_vals(w) := (
        dis_fire(w) &&
        dis_uops(w).uopc === uopROCC &&
        !dis_uops(w).exception
      )
    }
  }

  io.trace := DontCare
  io.trace.time := csr.io.time
  io.trace.insns map (t => t.valid := false.B)
  io.trace.custom.get.asInstanceOf[BoomTraceBundle].rob_empty := rob.io.empty

  if (trace) {
    for (w <- 0 until coreWidth) {
      // Delay the trace so we have a cycle to pull PCs out of the FTQ
      io.trace.insns(w).valid      := RegNext(rob.io.commit.arch_valids(w))

      // Recalculate the PC
      io.ifu.debug_ftq_idx(w) := rob.io.commit.uops(w).ftq_idx
      val iaddr = (AlignPCToBoundary(io.ifu.debug_fetch_pc(w), icBlockBytes)
                   + RegNext(rob.io.commit.uops(w).pc_lob)
                   - Mux(RegNext(rob.io.commit.uops(w).edge_inst), 2.U, 0.U))(vaddrBits-1,0)
      io.trace.insns(w).iaddr      := Sext(iaddr, xLen)

      def getInst(uop: MicroOp, inst: UInt): UInt = {
        Mux(uop.is_rvc, Cat(0.U(16.W), inst(15,0)), inst)
      }

      def getWdata(uop: MicroOp, wdata: UInt): UInt = {
        Mux((uop.dst_rtype === RT_FIX && uop.ldst =/= 0.U) || (uop.dst_rtype === RT_FLT), wdata, 0.U(xLen.W))
      }

      // use debug_insts instead of uop.debug_inst to use the rob's debug_inst_mem
      // note: rob.debug_insts comes 1 cycle later
      io.trace.insns(w).insn       := getInst(RegNext(rob.io.commit.uops(w)), rob.io.commit.debug_insts(w))
      io.trace.insns(w).wdata.map { _ := RegNext(getWdata(rob.io.commit.uops(w), rob.io.commit.debug_wdata(w))) }

      // Comment out this assert because it blows up FPGA synth-asserts
      // This tests correctedness of the debug_inst mem
      // when (RegNext(rob.io.commit.valids(w))) {
      //   assert(rob.io.commit.debug_insts(w) === RegNext(rob.io.commit.uops(w).debug_inst))
      // }
      // This tests correctedness of recovering pcs through ftq debug ports
      // when (RegNext(rob.io.commit.valids(w))) {
      //   assert(Sext(io.trace.insns(w).iaddr, xLen) ===
      //     RegNext(Sext(rob.io.commit.uops(w).debug_pc(vaddrBits-1,0), xLen)))
      // }

      // These csr signals do not exactly match up with the ROB commit signals.
      io.trace.insns(w).priv       := RegNext(Cat(RegNext(csr.io.status.debug), csr.io.status.prv))
      // Can determine if it is an interrupt or not based on the MSB of the cause
      io.trace.insns(w).exception  := RegNext(rob.io.com_xcpt.valid && !rob.io.com_xcpt.bits.cause(xLen - 1)) && (w == 0).B
      io.trace.insns(w).interrupt  := RegNext(rob.io.com_xcpt.valid && rob.io.com_xcpt.bits.cause(xLen - 1)) && (w == 0).B
      io.trace.insns(w).cause      := RegNext(rob.io.com_xcpt.bits.cause)
      io.trace.insns(w).tval       := RegNext(csr.io.tval)
    }
    dontTouch(io.trace)
  } else {
    io.ifu.debug_ftq_idx := DontCare
  }
}
