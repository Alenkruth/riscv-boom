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
import freechips.rocketchip.util.property.{cover => RCcover}
import freechips.rocketchip.devices.tilelink.{PLICConsts, CLINTConsts}
import midas.targetutils.AutoCounterCoverModuleAnnotation
import midas.targetutils.xdc.{XDC, XDCFiles}
import chisel3.experimental.annotate

import boom.v3.common._
import boom.v3.ifu.{GlobalHistory, HasBoomFrontendParameters, BpuResolveInfo, BpuSquashInfo}
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

  // IFT bridge: separate optional IO, present when ENABLE_IFT_BRIDGE=true or in the IFT
  // oracle mode. Kept outside the main bundle so the printf path is entirely unaffected.
  // [Task 2.10g, ruling 61] With WithIFTBridge the tile feeds it to the bridge (unchanged).
  // In the oracle mode (iftOracle: CoreFuzzingDMIConfig only, never a FireSim config) the
  // tile keeps it alive at its boundary, so the record readout is simulated and counted by
  // the BoomTile oracle as FireSim builds it. None otherwise, so every other config
  // (baseline, FireSim printf path) elaborates exactly as before.
  val ift_bridge_out = if (ENABLE_IFT_BRIDGE || IFT_ORACLE) Some(IO(Output(new IFTTileIO(coreWidth)))) else None
  // [V3LANE] SP-17 (record v3 design 5.5, 5.4): the STATE record from the tile's assembly register (valid + 256 b, no
  // ready: no processor stage waits on it) and the core values the tile packs into it.  Record/STATE builds only.
  val ift_state_in = if (ENABLE_IFT_BRIDGE || IFT_ORACLE) Some(IO(Input(Valid(UInt(256.W))))) else None
  val ift_core_out = if (ENABLE_IFT_BRIDGE || IFT_ORACLE) Some(IO(Output(new IftCoreToTile))) else None

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
                             Seq.fill(memWidth) {true} ++ exe_units.bypassable_write_port_mask, // bypassable ll_wb
                             coreWidth))   // [WINSHADOW] commit-time window-taint set ports
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
  // [WINEXP] step 21: fetch- or dispatch-time secret-window membership (record / [RASJAL] use);
  // dis_uops.cf_win_sec now carries the dispatch-time seed only.  Driven in the dispatch loop.
  val dis_win_sec_rec = Wire(Vec(coreWidth, Bool()))

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
  // [BRMASKREG]/[EXCREG] patch 25: the io.lsu-only replicas (driven below)
  val lsu_b1_rep  = if (ENABLE_IFT) Some(Wire(new BrUpdateMasks)) else None
  val lsu_exc_rep = if (ENABLE_IFT) Some(Wire(Bool())) else None

  brupdate.b1 := b1
  brupdate.b2 := b2

  // [BRMASKREG] patch 25 row 2c (timing, census3 W/R/H families; IFT builds only, so the IFT-off
  // Verilog is unchanged).  The b1 masks were a 4-lane shift/OR decode of the brinfos registers,
  // in series with every kill consumer (segment 1 of W: ~4.65 ns incl. the SLR2->1 hop).  They
  // are now registered from the brinfos NEXT-state, so every kill consumer starts at a flop.
  // EQUIVALENCE: brinfos is assigned unconditionally every cycle (brinfos := brinfos_next, no
  // enable, no reset), so brinfos(t) = brinfos_next(t-1) and, f being the stock decode,
  // RegNext(f(brinfos_next))(t) = f(brinfos_next(t-1)) = f(brinfos(t)) for every t >= 1.  Only the
  // power-on value (before the first clock edge, deep inside reset) differs; both are un-reset
  // X/random registers there.  Cycle- and record-identical.
  // [BRMASKREG] replica + [EXCREG]: a second copy of both masks and of RegNext(rob.io.flush.valid)
  // feeds ONLY io.lsu (LSU + D$), so the placer can put it in SLR1 next to the D$ s2 logic (the
  // census-3 REG_rep__19 hop was 2.81 ns).  Same D nets as the core copies (the extra SLR crossing
  // lands on the D side: brinfos D has +18.1 ns @30), so the copies are equal every cycle.
  // Vivado's synth would merge equivalent registers; dontTouch keeps them in the Verilog and the
  // midas XDC annotation emits `set_property DONT_TOUCH true` for them into
  // FireSim-generated.synthesis.xdc (read by cl_firesim/scripts/main.tcl into synth_fileset).
  // Non-FireSim flows drop the annotation (firtool --disable-annotation-unknown).
  if (ENABLE_IFT) {
    val brinfos_next = Wire(Vec(coreWidth, new BrResolutionInfo()))
    for ((n, a) <- brinfos_next zip exe_units.alu_units) {
      n := a.io.brinfo
      n.valid := a.io.brinfo.valid && !rob.io.flush.valid
      n.jal_res := a.io.brinfo.jal_res && !rob.io.flush.valid   // [SPECBPU] same flush gate as valid
    }
    brinfos := brinfos_next
    val rm_next = brinfos_next.map(x => x.valid << x.uop.br_tag).reduce(_|_)(maxBrCount-1, 0)
    val mm_next = brinfos_next.map(x => (x.valid && x.mispredict) << x.uop.br_tag).reduce(_|_)(maxBrCount-1, 0)
    b1.resolve_mask    := RegNext(rm_next)
    b1.mispredict_mask := RegNext(mm_next)
    val cf_lsu_brm_resolve    = RegNext(rm_next)
    val cf_lsu_brm_mispredict = RegNext(mm_next)
    val cf_lsu_exc_q          = RegNext(rob.io.flush.valid)
    for (r <- Seq(cf_lsu_brm_resolve, cf_lsu_brm_mispredict, cf_lsu_exc_q)) {
      dontTouch(r)
      XDC(XDCFiles.Synthesis, "set_property DONT_TOUCH true [get_cells -hierarchical -regexp .*{}_reg.*]", r)
    }
    lsu_b1_rep.get.resolve_mask    := cf_lsu_brm_resolve
    lsu_b1_rep.get.mispredict_mask := cf_lsu_brm_mispredict
    lsu_exc_rep.get                := cf_lsu_exc_q
  } else {
  for ((b, a) <- brinfos zip exe_units.alu_units) {
    b := a.io.brinfo
    b.valid := a.io.brinfo.valid && !rob.io.flush.valid
    b.jal_res := a.io.brinfo.jal_res && !rob.io.flush.valid   // [SPECBPU] same flush gate as valid
  }
  b1.resolve_mask := brinfos.map(x => x.valid << x.uop.br_tag).reduce(_|_)
  b1.mispredict_mask := brinfos.map(x => (x.valid && x.mispredict) << x.uop.br_tag).reduce(_|_)
  }

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
  // [SPECBPU] one register for the jump unit's jalr target, shared by b2 and the spec BPU update below
  // (rev2, review N6a: the brinfos(jl).jalr_target copy of the same D is no longer read; -40 FF).
  val jmp_jalr_target_q = RegNext(jmp_unit.io.brinfo.jalr_target)
  b2.jalr_target := jmp_jalr_target_q
  b2.target_offset := oldest_mispredict.target_offset

  val oldest_mispredict_ftq_idx = oldest_mispredict.uop.ftq_idx

  // [SPECBPU] (owner 2026-11-02, RULES C 'Speculative TAGE/BTB updates': "At resolution, all paths"; commit training
  // "Replace it"; "Always speculative"; "New direct path from brupdate"; 'Speculative BPU follow-ups': O1 "(a)
  // BIM+uBTB too", O2 "Measure first, JAL last").  The TAGE, BTB, BIM and uBTB train once per resolved CFI, when it
  // resolves here, wrong path included.  Source = the brinfos registers (the per-ALU
  // resolutions that brupdate.b1/b2 are built from): brupdate.b2 itself carries only the OLDEST MISPREDICT (above),
  // so a correctly predicted branch never appears in it.  JALs (no branch tag, not in brinfos.valid) come in on
  // brinfos.jal_res.  Wrong-path coverage: a lane younger than a mispredict resolved in an EARLIER cycle never
  // reaches brinfos (the ALU kill, IsKilledByBranch); lanes resolved in the same cycle as, or before, that
  // mispredict do (owner: wrong path included).
  // One update per cycle (the predictor has one write port per table): the OLDEST mispredicting lane by rob_idx
  // wins (rev2, review S1: the same lane brupdate.b2 redirects on, so the redirecting branch always trains; its
  // update carries the TAGE allocation and the BTB target correction; lane order is not age, so a younger
  // wrong-path mispredict in a lower lane must not win), else the lowest br / jalr lane, else (O2: JAL lanes
  // lowest, since f3 predecode already corrects a JAL target) the lowest JAL lane; the other lanes of the cycle are
  // not trained (no queue: a queued wrong-path update could read an FTQ entry the refetch has overwritten; design
  // note section 6).  The drop rate is measured on the debug build ([SPECBPU-DROP] below).
  // Levels (flop to flop inside the core; all inputs are flops: brinfos, rob_head / rob_head_lsb):
  //   L1 + carry: the 6 pairwise rob_idx compares and the 4 rob_idx < head compares (8 b: 1 LUT + 1 CARRY8 each);
  //   L2: per pair "lane j not mispredicting or lane i older" (valid j, mispredict j, lt, two head bits: 5
  //       inputs); the JAL-last fallback one-hot (12 flop bits, 2 LUT) runs in parallel and is ready at L2;
  //   L3: sel(i) = lm(i) & the 3 pair terms | fallback(i) (6 inputs, 1 LUT6);
  //   L4-L5: 4:1 Mux1H of the payload (2 LUT) -> the spec_bpu flops (about 85 FF).
  // 5 LUT + 1 CARRY8 (rev1: 4-5 LUT), against the existing b2 select from the same flops (3 serial IsOlder + 2:1
  // muxes). The flops drive the FTQ read address directly (the same core -> frontend route as get_pc(1).ftq_idx,
  // which is not registered at all).
  val spec_bpu_v = RegInit(false.B)
  val spec_bpu_q = Reg(new BpuResolveInfo)
  locally {
    val lv  = VecInit(brinfos.map(b => b.valid || b.jal_res)).asUInt
    val lm  = VecInit(brinfos.map(b => b.valid && b.mispredict)).asUInt
    val lb  = VecInit(brinfos.map(_.valid)).asUInt                      // br / jalr lanes
    val lj  = VecInit(brinfos.map(b => !b.valid && b.jal_res)).asUInt   // JAL lanes (O2: lowest priority)
    // Oldest mispredicting lane, one-hot (rev2, S1).  rob_idx are distinct among valid lanes, so the pair (i, j)
    // needs one compare: i older than j = IsOlder(r(i), r(j), head) = (r(i) < r(j)) ^ (r(i) < head) ^ (r(j) < head),
    // j older than i = its complement.  Each "< head" compare is shared by all pairs of its lane.
    val nl  = brinfos.length
    val rix = brinfos.map(_.uop.rob_idx)
    val rhd = rob.io.rob_head_idx
    val lth = rix.map(_ < rhd)
    val older = Array.tabulate(nl, nl) { (i, j) =>
      if (i < j) Some((rix(i) < rix(j)) ^ lth(i) ^ lth(j)) else None }
    def olderThan(i: Int, j: Int): Bool = if (i < j) older(i)(j).get else !older(j)(i).get
    val om  = VecInit((0 until nl).map { i =>
      ((0 until nl).filter(_ != i).map(j => !lm(j) || olderThan(i, j)) :+ lm(i)).reduce(_ && _) }).asUInt
    // = Mux(lm.orR, om, fallback): om is 0 without a mispredict and the fallback is masked with one, so an OR is
    // exact; written as AND-OR so that the fallback (flops only) settles in parallel with the compares (L3 above).
    val fb  = Mux(lb.orR, PriorityEncoderOH(lb), PriorityEncoderOH(lj)) & Fill(nl, !lm.orR)
    val sel = om | fb
    val jl  = exe_units.alu_units.indexWhere(_.hasJmpUnit)   // the only lane that resolves a jalr
    require(jl >= 0 && jl < brinfos.length, "[SPECBPU] the jump unit must be one of the brinfos lanes")
    val n = Wire(new BpuResolveInfo)
    n.ftq_idx       := Mux1H(sel, brinfos.map(_.uop.ftq_idx))
    n.pc_lob        := Mux1H(sel, brinfos.map(_.uop.pc_lob))
    n.edge_inst     := Mux1H(sel, brinfos.map(_.uop.edge_inst))
    n.is_br         := Mux1H(sel, brinfos.map(b => b.valid && b.cfi_type === CFI_BR))
    n.is_jalr       := Mux1H(sel, brinfos.map(b => b.valid && b.cfi_type === CFI_JALR))
    n.is_jal        := Mux1H(sel, brinfos.map(b => !b.valid && b.jal_res))
    n.taken         := Mux1H(sel, brinfos.map(_.taken))
    n.mispredict    := Mux1H(sel, brinfos.map(b => b.valid && b.mispredict))
    n.target_offset := Mux1H(sel, brinfos.map(_.target_offset.asUInt)).asSInt   // UInt Mux1H (inferred-width SInt)
    n.jalr_target   := jmp_jalr_target_q   // = brinfos(jl).jalr_target (same D, jl is the jump unit's lane; N6a)
    // IFT (RULES A; [SPECBPU-IFT]): the resolving CFI's own condition / target taint.  The FTQ ORs them with the
    // entry's labels (fetch-target-queue.scala), so the update is labelled as the commit update would have been
    // for this branch, without waiting for the compactor / [FTQATK] write of the same bits.
    // A JAL has no register operand: its lane's cond bits are not read (valid = 0 for a JAL).
    n.cond_secret.foreach(_ := Mux1H(sel, brinfos.map(b => b.valid && b.cond_secret)))
    n.cond_atk.foreach(_    := Mux1H(sel, brinfos.map(b => b.valid && b.cond_atk)))
    // [SPECBPU-REP] (spec-bpu-plan-final 6.3) debug printf builds only: the selected lane's op count (BpuResolveInfo.dbg_oc)
    n.dbg_oc.foreach(_ := Mux1H(sel, brinfos.map(_.uop.cf_op_count_id.get)))
    spec_bpu_v := lv.orR
    spec_bpu_q := n
    // [SPECBPU-DROP] (O2 "Measure first"): debug printf builds only (the IftSquashMerge gate: IFT on, CF debug
    // printfs on, no IFT bridge; NoLog / FireSim / IFT off elaborate nothing).  Printed in every cycle that drops a
    // lane: nd / ndj = dropped lanes / dropped JAL lanes in this cycle; res / drop / dropj = running totals from reset
    // of resolved lanes, dropped lanes and dropped JAL lanes (so the LAST line of a run gives the lane drop rate
    // drop / res up to that cycle; the [BPDS] count gives the trained lanes).  40-bit debug-only counters.
    // rev2 (review S1 measurement): ndm / dropm = dropped MISPREDICTING lanes in this cycle / running total (a
    // younger, wrong-path mispredict that lost to the oldest one; only cycles with more than one bit in mp).
    // [SPECBPU-SEL] asserts check the S1 select: exactly one lane is selected when any lane resolves, and with a
    // mispredict it is the lane brupdate.b2 redirects on (same rob_idx as oldest_mispredict).
    if (ENABLE_IFT && ENABLE_CF_DEBUG_PRINTF && !ENABLE_IFT_BRIDGE) {
      val cyc   = RegInit(0.U(40.W))
      val t_res = RegInit(0.U(40.W))
      val t_drp = RegInit(0.U(40.W))
      val t_drj = RegInit(0.U(40.W))
      val t_drm = RegInit(0.U(40.W))
      val nres  = PopCount(lv)
      val ndrp  = Mux(lv.orR, nres - 1.U, 0.U)
      val ndrj  = PopCount(lj & ~sel)
      val ndrm  = PopCount(lm & ~sel)
      cyc   := cyc + 1.U
      t_res := t_res + nres
      t_drp := t_drp + ndrp
      t_drj := t_drj + ndrj
      t_drm := t_drm + ndrm
      when (nres > 1.U) {
        printf("[SPECBPU-DROP] cyc=%d lanes=0x%x sel=0x%x mp=0x%x jal=0x%x nd=%d ndj=%d ndm=%d res=%d drop=%d dropj=%d dropm=%d\n",
          cyc, lv, sel, lm, lj, ndrp, ndrj, ndrm, t_res + nres, t_drp + ndrp, t_drj + ndrj, t_drm + ndrm)
      }
      assert(!lv.orR || PopCount(sel) === 1.U, "[SPECBPU-SEL] spec BPU lane select is not one-hot")
      assert(!lm.orR || Mux1H(sel, rix) === oldest_mispredict.uop.rob_idx,
        "[SPECBPU-SEL] spec BPU mispredict lane is not the b2 (oldest) mispredict")
    }
  }
  io.ifu.bpu_res.valid := spec_bpu_v
  io.ifu.bpu_res.bits  := spec_bpu_q


  assert (!((brupdate.b1.mispredict_mask =/= 0.U || brupdate.b2.mispredict)
    && rob.io.commit.rollback), "Can't have a mispredict during rollback.")

  // corefuzzing
  // When we detect a mispredict, print a short summary line identifying the branch
  // that caused the flush. This prints before we propagate `brupdate` into the
  // frontend and other modules so it's shown ahead of the per-module speculative dumps.
  // Gated by ENABLE_CF_DEBUG_PRINTF (compile-time): when false the printf and its
  // argument cone are never emitted, so this costs nothing in non-debug builds.
  if (ENABLE_CF_DEBUG_PRINTF) {
    when (b2.mispredict) {
      // Print: PC, inst, cfi_type, br_tag, rob_idx, taken, jalr_target (if any)
      // [CFNOISE-COMMENTED] printf("[SPECULATIVE][MISPREDICT] pc=0x%x inst=0x%x cfi=%d br_tag=%d rob_idx=%d taken=%d target=0x%x\n",
        // [CFNOISE-COMMENTED] Sext.apply(oldest_mispredict.uop.debug_pc(vaddrBits-1,0), xLen),
        // [CFNOISE-COMMENTED] oldest_mispredict.uop.debug_inst,
        // [CFNOISE-COMMENTED] oldest_mispredict.cfi_type,
        // [CFNOISE-COMMENTED] oldest_mispredict.uop.br_tag,
        // [CFNOISE-COMMENTED] oldest_mispredict.uop.rob_idx,
        // [CFNOISE-COMMENTED] oldest_mispredict.taken,
        // [CFNOISE-COMMENTED] b2.jalr_target)
    }
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

  // [IMPLWIN] step 20 (rulings 132/134/135/136/137): the sticky cf_ctrl_secret_ctx is REMOVED.
  // The decaying window that shadows the GHR (winCtr, [ARMRES] below the brinfos loop) is the
  // only control-flow scope: armed when a DATA-secret branch RESOLVES, aged by retiring
  // conditional branches younger than it (one per committed GHR shift, [GHRWIN] 62 / [GHRWIN2] 71).
  // Its members get s_prop / atk and a WINDOW-tainted
  // result register (uop.cf_win_*; regfile wsec/watk), a register-taint kind separate from
  // the data taint, which never re-arms the window.  Disarm holds it closed ([WINDISARM]).
  // [R124GATE] patch 26 (r151): a flop only with ENABLE_IFT; IFT off the secret window is disarmed.
  val cf_sec_disarm_c = if (ENABLE_IFT) RegNext(custom_csrs.cf_secret_start_addr === custom_csrs.cf_secret_end_addr, true.B)
                        else true.B

  // [DBGFREEZE] patch 85 (owner 2026-10-20: "how would a DMI halt end in a secret window? just make sure that does
  // not happen"; p4-85-author.md).  Debug-mode execution (debug ROM, abstract commands, program buffer: fesvr halts
  // the hart for every tohost poll, fesvr/dtm.cc read_chunk) is invisible to IFT:
  //   (a) its uops never arm, decay or join the secret / attacker windows: the window counters are frozen (decay
  //       gated in the shared GHR-shift chain cf_ghr_shift_q, patch 71) and every reader sees them closed
  //       (win_*_ctr below, also the frontend export);
  //   (b) they take no label at the sources: no operand taint (register-read), no fetch-time edges / domain
  //       (fetch-buffer.scala), no s_acc (lsu.scala), no dispatch-stall edges / C5 (dispatch loop), so nothing they
  //       train (FTQ secret/atk bit -> BPD/BTB/RAS, I$, D$ fill / store meta, STQ head bits) is secret or attacker;
  //       their commit records are masked and marked priv[2] = 1 (rob.scala);
  //   (c) the program's taint state survives the halt: window counter, epochs, regfile marks (nothing closes), the
  //       fflags taint (frozen below), and the taint of x8/x9, the only registers the debug ROM / Rocket DM abstract
  //       commands / fesvr / OpenOCD overwrite and restore (dvP/dvA/dvL below, register-read [DBGVT]).
  // Bracket: debug entry is an exception (full flush) and dret is unique + flush_on_commit, so while cf_dbg = 1
  // every uop from fetch to commit is a debug-mode uop.  cf_dbg is reg_debug one flop later (placement); it rises
  // at exception + 3, before the first debug-ROM fetch reaches the fetch buffer (+5), and falls at dret's CSR
  // execution + 2, before dret commits' flush lets a program uop dispatch.  Stores committed before entry drain
  // with their own (STQ) labels, and debug stores that drain after dret carry the clean labels made here, so no
  // time-based gate sits on the D$ write.  IFT off: constant false, everything below folds.
  val cf_dbg   = if (ENABLE_IFT) RegNext(csr.io.status.debug, false.B) else false.B
  val cf_dbg_q = if (ENABLE_IFT) RegNext(cf_dbg, false.B) else false.B


  // for the fuzzycore project - AK
  // flag to print the debug log
  io.lsu.cf_debug_lsu_enable := custom_csrs.cf_debug_lsu_enable && custom_csrs.cf_debug_enable
  io.lsu.cf_debug_dcache_enable := custom_csrs.cf_debug_dcache_enable
  io.lsu.cf_debug_repl_enable   := custom_csrs.cf_debug_repl_enable && custom_csrs.cf_debug_enable   // [REPLUPD] 0xbc0 bit 7

  rob.io.cf_debug_rob_enable := custom_csrs.cf_debug_rob_enable && custom_csrs.cf_debug_enable
  // corefuzzing: drive cycle-N mispredict UOP so ROB flush log can annotate INFL_PIPELINE_FLUSH correctly
  rob.io.cf_mispredict_uop.valid := b1.mispredict_mask =/= 0.U
  rob.io.cf_mispredict_uop.bits  := oldest_mispredict.uop
  // io.ifu.cf_debug_log := custom_csrs.cf_debug_log

  // Assigning the CSR's output to the cf_tage_to_gshare signal in the frontend.
  io.ifu.cf_bpd_tage_to_gshare := custom_csrs.cf_bpd_tage_to_gshare
  io.ifu.cf_icache_set_conf    := custom_csrs.cf_icache_set_conf
  io.ifu.cf_icache_way_conf    := custom_csrs.cf_icache_way_conf
  // Assigning the dcache reconfiguration flags to the lsu
  io.lsu.cf_dcache_set_conf  := custom_csrs.cf_dcache_set_conf
  io.lsu.cf_dcache_way_conf  := custom_csrs.cf_dcache_way_conf
  io.lsu.cf_dcache_repl_conf := custom_csrs.cf_dcache_repl_conf
  io.lsu.cf_ceviche_en.foreach(_     := custom_csrs.cf_ceviche_en)
  io.lsu.cf_ceviche_hard_d.foreach(_ := custom_csrs.cf_ceviche_hard_d)
  io.lsu.cf_ceviche_soft_d.foreach(_ := custom_csrs.cf_ceviche_soft_d)
  io.ifu.cf_ceviche_en.foreach(_     := custom_csrs.cf_ceviche_en)
  io.ifu.cf_ceviche_pol.foreach(_    := custom_csrs.cf_ceviche_pol)
  io.ifu.cf_ceviche_hard_i.foreach(_ := custom_csrs.cf_ceviche_hard_i)
  io.ifu.cf_ceviche_soft_i.foreach(_ := custom_csrs.cf_ceviche_soft_i)
  // [reconf-fix Phase C] 0xbc4 invalidate-walker trigger to the D$ (via LSU),
  // and the walker's busy published as 0xbcd[17] (hardware-set every cycle;
  // must come after the csrs.foreach defaults above so the connect wins).
  io.lsu.cf_cachectl_wen   := custom_csrs.cf_cachectl_wen
  io.lsu.cf_cachectl_wdata := custom_csrs.cf_cachectl_wdata
  // [reconf-fix 2.10f] [16:12] = D$ reconfiguration status (pending, applied way/set idx).
  custom_csrs.cf_dcache_status_set(Cat(io.lsu.cf_dcache_wipe_busy,
                                       io.lsu.cf_dcache_reconf_status.getOrElse(0.U(5.W)),
                                       0.U(12.W)))
  // [SIDECAR] Task 2.7: IFT statistics CSR 0xbce (D6 squash-record losses, D19 third drops).
  // Task 3.4 (D18): kill-record overflows (an IQ slot killed again while its previous record
  // still waits) are counted into the low half and the sticky flag [48]; [31:16] and [47:32]
  // are unchanged.  kill_ovf_cnt is updated in the kill block after the bridge wiring.
  val kill_ovf_cnt = RegInit(0.U(16.W))
  def satAdd16(a: UInt, b: UInt): UInt = { val s = a +& b; Mux(s(16), "hFFFF".U(16.W), s(15, 0)) }
  if (ENABLE_IFT) custom_csrs.cf_ift_stats_set(Cat(rob.io.ift_squash_ovf || kill_ovf_cnt.orR, rob.io.ift_squash_reuse_cnt,
                                                   rob.io.ift_third_drop_cnt, satAdd16(rob.io.ift_squash_ovf_cnt, kill_ovf_cnt)))
  // corefuzzing: wire secret address CSRs into LSU for cf_secret_access detection
  io.lsu.cf_secret_start_addr := custom_csrs.cf_secret_start_addr
  io.lsu.cf_dbg.foreach(_ := cf_dbg)   // [DBGFREEZE] patch 85 (b): no s_acc in debug mode (lsu.scala TLB stage)
  io.lsu.cf_secret_end_addr   := custom_csrs.cf_secret_end_addr
  // ---- Hardware defenses: CSR fan-out to the LSU (T0). STT's fan-out is C-CORE-1 (e) (registered copies, task T3s). ----
  io.lsu.dgl_ctl.foreach(_       := custom_csrs.doppelganger_en)                // Doppelganger (registered in the LSU, L1)
  io.lsu.sa_ctl_en.foreach(_     := custom_csrs.sa_ctl_en)                      // SpecASan SA-8
  io.lsu.sa_paint_wen.foreach(_  := custom_csrs.sa_paint_wen)
  io.lsu.sa_paint_data.foreach(_ := custom_csrs.sa_paint_data)
  io.lsu.sa_stat.foreach(s => custom_csrs.sa_stat_set(s))
  // [v5] task T1a (not T0) adds this line directly after the T0 block: ExecutionUnit.io.sa_en exists only after SA-1b
  mem_units.foreach(u => u.io.sa_en.foreach(_ := RegNext(custom_csrs.sa_ctl_en || custom_csrs.sa_tbi, false.B)))   // [SA-1b]

  // getting the rob entries from the CSR
  rob.io.cf_rob_entries := custom_csrs.cf_rob_entries
  // cf_rob_quiesce_reset is driven below, after quiesce_flush_pulse is defined

  // printf("[CORE] dcacheCSR - 0x%x 0x%x 0x%x \n", custom_csrs.cf_dcache_set_conf, custom_csrs.cf_dcache_way_conf, custom_csrs.cf_dcache_repl_conf)

  // Microarchitecture reconfiguration indices routed to frontend
  io.ifu.cf_fb_idx          := custom_csrs.cf_fetch_buffer_idx
  io.ifu.cf_ftq_idx         := custom_csrs.cf_ftq_idx
  io.ifu.cf_ras_idx         := custom_csrs.cf_ras_idx
  // [F-RAS-3] (ruling 144) active RAS size for the mispredict-path ghist repair below.  It must
  // equal the frontend's cf_ras_active (frontend.scala rasOptionsVec(io.cpu.cf_ras_idx)) in the
  // same cycle: io.ifu.cf_ras_idx is this same custom_csrs wire, connected straight to
  // frontend io.cpu (tile.scala `frontend.io.cpu <> core.io.ifu`), with no register between.
  // The CSR is a register, so this is a 4:1 mux of constants off a flop, off the brupdate path.
  // Reconf off: nRasEntries, the ghist.update default, so that Verilog is unchanged.
  val cf_ras_active_core = if (ENABLE_RECONF) VecInit(rasEntryCountOptions.map(_.U))(custom_csrs.cf_ras_idx)
                           else nRasEntries.U
  io.ifu.cf_btb_set_idx     := custom_csrs.cf_btb_set_idx
  io.ifu.cf_btb_way_idx     := custom_csrs.cf_btb_way_idx
  io.ifu.cf_tage_count_idx  := custom_csrs.cf_tage_count_idx
  io.ifu.cf_active_width    := custom_csrs.cf_active_width

  // Wire cf_debug enables into frontend and core modules
  io.ifu.cf_debug_frontend_enable := custom_csrs.cf_debug_enable && custom_csrs.cf_debug_frontend_enable
  io.ifu.cf_debug_rob_enable      := custom_csrs.cf_debug_rob_enable && custom_csrs.cf_debug_enable
  // corefuzzing: attacker range for ICache/RAS domain tracking in frontend
  io.ifu.cf_attacker_start_addr   := custom_csrs.cf_attacker_start_addr
  io.ifu.cf_attacker_end_addr     := custom_csrs.cf_attacker_end_addr
  // corefuzzing: secret range for ITLB secret shadow tracking in frontend
  io.ifu.cf_secret_start_addr     := custom_csrs.cf_secret_start_addr
  io.ifu.cf_secret_end_addr       := custom_csrs.cf_secret_end_addr

  // Propagate core-level debug enables to the issue units.  (Task 3.9: the rename stages'
  // cf_debug_rename_enable / cf_mispredict_uop inputs are gone; Task 3.3 deleted their only
  // readers, the commented-out SRC=2 [FLUSH] printfs.)
  mem_iss_unit.io.cf_debug_issue_enable := custom_csrs.cf_debug_enable && custom_csrs.cf_debug_core_enable
  int_iss_unit.io.cf_debug_issue_enable := custom_csrs.cf_debug_enable && custom_csrs.cf_debug_core_enable

  // LDQ/STQ reconfiguration: 3-bit indices into ldQueueEntryOptions / stQueueEntryOptions
  io.lsu.cf_ldq_idx := custom_csrs.cf_ldq_idx
  io.lsu.cf_stq_idx := custom_csrs.cf_stq_idx

  // Issue queue size reconfiguration: 2-bit index into issueQueueEntryOptions
  mem_iss_unit.io.cf_iq_idx := custom_csrs.cf_iq_idx
  int_iss_unit.io.cf_iq_idx := custom_csrs.cf_iq_idx
  // [WCAP] width CSR 0xbcc -> the INT / MEM issue-grant caps (min(N, 4) / min(N, 2)); FP (1 port) has no cap input.
  // Reconf only (the IQ port is None otherwise).
  mem_iss_unit.io.cf_width_idx.foreach(_ := custom_csrs.cf_core_width_idx)
  int_iss_unit.io.cf_width_idx.foreach(_ := custom_csrs.cf_core_width_idx)
  if (usingFPU) {
    fp_pipeline.io.cf_iq_idx := custom_csrs.cf_iq_idx
  }

  // Physical register file size reconfiguration: 3-bit index into pregFileSizeOptions
  rename_stage.io.cf_preg_idx      := custom_csrs.cf_preg_idx
  fp_rename_stage.io.cf_preg_idx   := custom_csrs.cf_preg_idx
  pred_rename_stage.io.cf_preg_idx := custom_csrs.cf_preg_idx

  // corefuzzing
  // Quiescing control signals.  Declared as outer-scope Wires so that consumers
  // elsewhere in core.scala (fencei gating, single_step_active, dispatch logic,
  // and the preg_secret / fp_preg_secret IFT clears) can reference them
  // unconditionally — the quiesce FSM body is wrapped in `if (ENABLE_RECONF)` and
  // drives these wires when reconf is enabled, or ties them to defaults otherwise.
  val cf_quiesce_core     = Wire(Bool())
  val pipeline_drained    = Wire(Bool())
  val quiesce_flush_pulse = Wire(Bool())
  val qs_draining         = Wire(Bool())

  if (ENABLE_RECONF) {
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
  // QS_FETCH:     pipeline drained; allow_fetch for 3 cycles (qs_fetch_cnt 0..2)
  // QS_EXECUTING: packet entered pipeline; block fetch; wait for full drain
  val QS_IDLE      = 0.U(2.W)
  val QS_DRAINING  = 1.U(2.W)
  val QS_FETCH     = 2.U(2.W)
  val QS_EXECUTING = 3.U(2.W)
  val qs_state = RegInit(QS_IDLE)
  // corefuzzing: counter to extend QS_FETCH from 1 to 3 cycles.
  // During QS_DRAINING the IFU backpressure loop oscillates s0→s1→s2, consuming the
  // first allow_fetch cycle replaying stale s2 data (f1_clear kills the advancement).
  // The f2 stage can only advance s0 to window-2 when s2_valid=true for window-1,
  // which happens 2 cycles after the first icache request.  With only 1 QS_FETCH cycle
  // (+ 1 qs_transition_pulse = 2 total), allow_fetch=0 by the time window-2 is needed.
  // 3 QS_FETCH cycles (4 total allow_fetch) guarantees both SSTEP windows are fetched
  // regardless of which backpressure phase occurs at the QS_DRAINING→QS_FETCH boundary.
  val qs_fetch_cnt = RegInit(0.U(2.W))

  // Guard against runaway QS_EXECUTING→QS_FETCH when pipeline empties immediately
  // after a [SSTEP] flush (before the new fetch arrives). Require that at least one
  // ROB flush has occurred (from the [SSTEP] commit) before allowing QS_EXECUTING
  // to advance back to QS_FETCH.
  val qs_saw_flush = RegInit(false.B)
  when (rob.io.flush.valid && (qs_state === QS_EXECUTING)) {
    qs_saw_flush := true.B
  } .elsewhen (qs_state =/= QS_EXECUTING) {
    qs_saw_flush := false.B
  }

  // corefuzzing: ICache-miss retry counter.
  // When an ICache reconfiguration (e.g., ICACHE_MIN→ICACHE_MAX via fence.i) leaves
  // the SSTEP Packet C address out of cache, the 4-cycle QS_FETCH window closes before
  // the L2 fill completes → QS_EXECUTING starts with sf=0, pds=1, fb=1 (SSTEP never
  // fetched). The IFU's backpressure oscillation keeps s0_vpc at the correct SSTEP PC
  // but cannot fire icache.req (allow_fetch=0). When the fill completes, no new req is
  // ever sent → permanent deadlock.
  // Fix: after 64 consecutive cycles of (no flush seen + pipeline empty + fb empty),
  // re-open QS_FETCH. The IFU then fires a new icache.req that hits the now-filled line.
  // The IFU backpressure oscillation guarantees s0_vpc = SSTEP PC at retry time.
  val qs_retry_cnt = RegInit(0.U(7.W))

  switch (qs_state) {
    is (QS_IDLE) {
      when (cf_quiesce_core) { qs_state := QS_DRAINING }
    }
    is (QS_DRAINING) {
      when (!cf_quiesce_core)             { qs_state := QS_IDLE }
      .elsewhen (pipeline_drained_strict) { qs_state := QS_FETCH; qs_fetch_cnt := 0.U }
    }
    is (QS_FETCH) {
      // Stay for 3 cycles (qs_fetch_cnt = 0, 1, 2) before entering QS_EXECUTING.
      // This gives the IFU enough allow_fetch cycles to icache-request both SSTEP windows.
      qs_fetch_cnt := qs_fetch_cnt + 1.U
      when (qs_fetch_cnt >= 2.U) {
        qs_state     := QS_EXECUTING
        qs_fetch_cnt := 0.U
      }
    }
    is (QS_EXECUTING) {
      when (!cf_quiesce_core) {
        qs_state      := QS_IDLE
        qs_retry_cnt  := 0.U
      } .elsewhen (pipeline_drained_strict && qs_saw_flush) {
        qs_state      := QS_FETCH
        qs_fetch_cnt  := 0.U
        qs_retry_cnt  := 0.U
      } .elsewhen (!qs_saw_flush && pipeline_drained_strict && fetch_buffer_empty) {
        // SSTEP not yet fetched (ICache miss during QS_FETCH). Count cycles waiting.
        // After 64 cycles the L2 fill should be complete; re-open QS_FETCH to retry.
        qs_retry_cnt := qs_retry_cnt + 1.U
        when (qs_retry_cnt >= 63.U) {
          qs_state     := QS_FETCH
          qs_fetch_cnt := 0.U
          qs_retry_cnt := 0.U
        }
      } .otherwise {
        qs_retry_cnt := 0.U
      }
    }
  }

  // for corefuzzing, quiescing the pipeline to enable reconfiguration
  // [DBGQS] patch 34 (DF-dmi-err4 / D-29, D-25): quiesce is off while the hart is in debug mode.
  // A 0x7c5=1 written through the debug program buffer (or a DMI halt landing inside a software
  // bracket) used to run the debug ROM under the quiesce fetch rules: cf_btb_quiesce turned the
  // ROM's taken loop branch into a mispredict that resolved with fetch blocked, and D-26 replaced
  // its target with a stale PC (program-buffer replays, early resume -> cmderr 4, or a hang).
  // In debug mode the FSM now returns to QS_IDLE, so fetch, prediction and fence.i are normal and
  // no quiesce pulse fires; cf_chill itself keeps its value.  On dret with cf_chill=1 the FSM
  // re-enters QS_DRAINING from QS_IDLE, as after a software csrwi 0x7c5,1.  Every DMI-delivered
  // reconfiguration write is safe on a halted hart without the drain (p4-34-author.md): the ROB
  // resets its pointers on the CSRRW's flush_commit, the LDQ resets on every flush, the STQ / FTQ /
  // RAS pointers self-heal through the dynamic WrapInc, the LSQ adopts its size only drained, the
  // PRF is a free-list mask, the I$ uses the extended tag and the D$ runs its own 2.10f walk.
  // Cost: one AND of two flops (cf_chill CSR register, rocket reg_debug); no new state.
  cf_quiesce_core := custom_csrs.cf_chill && !csr.io.status.debug

  // Determine if pipeline is drained by checking ROB empty and LSU status
  pipeline_drained := pipeline_drained_strict

  // Fetch is allowed during QS_FETCH (3 cycles) plus the qs_transition_pulse cycle = 4 total.
  // f3 JAL/JALR redirects are gated by cf_btb_quiesce in frontend.scala, preventing
  // speculative call-target fetches from corrupting RAS state before the flush fires.
  val qs_transition_pulse = ((qs_state === QS_DRAINING) && pipeline_drained_strict) ||
    ((qs_state === QS_EXECUTING) && pipeline_drained_strict && qs_saw_flush)
  val allow_fetch = !cf_quiesce_core || (qs_state === QS_FETCH) || qs_transition_pulse
  io.ifu.allow_fetch    := allow_fetch
  // Suppress BTB predictions during quiesce so stale entries trained by workload code
  // cannot redirect the IFU mid-bundle and kill SSTEP packets before they reach the ROB.
  io.ifu.cf_btb_quiesce := cf_quiesce_core

  // Debug: trace quiesce FSM state every cycle when quiesce is active.
  // corefuzzing [QS] quiesce debug log
  // Compile-time gated: when ENABLE_CF_DEBUG_PRINTF is false this block is never
  // emitted, so neither the printf nor its argument cone reaches synthesis.
  if (ENABLE_CF_DEBUG_PRINTF) {
    when (cf_quiesce_core || qs_state =/= QS_IDLE) {
      // [CFNOISE-COMMENTED] printf("[QS] st=%d af=%d sf=%d fc=%d rc=%d pds=%d (rob=%d lsq=%d nmem=%d fri=%d fb=%d dec=%d dis=%d) flush=%d fp=%d\n",
        // [CFNOISE-COMMENTED] qs_state, allow_fetch, qs_saw_flush, qs_fetch_cnt, qs_retry_cnt, pipeline_drained_strict,
        // [CFNOISE-COMMENTED] rob.io.empty, io.lsu.queues_empty, io.lsu.no_pending_mem, io.lsu.fencei_rdy,
        // [CFNOISE-COMMENTED] fetch_buffer_empty, decode_stage_empty, dispatch_stage_empty,
        // [CFNOISE-COMMENTED] rob.io.flush.valid, io.ifu.fetchpacket.valid)
    }
  }

  // IFT quiesce-flush: pulse for one cycle on QS_DRAINING → QS_FETCH transition
  // (pipeline_drained_strict just became true while still in QS_DRAINING).
  // Clears taint tables for clean campaign boundaries and PRF-resize semantics.
  quiesce_flush_pulse := (qs_state === QS_DRAINING) && pipeline_drained_strict
  qs_draining := (qs_state === QS_DRAINING)
  rename_stage.io.quiesce_flush    := quiesce_flush_pulse
  fp_rename_stage.io.quiesce_flush := quiesce_flush_pulse
  pred_rename_stage.io.quiesce_flush := quiesce_flush_pulse
  // Reset FTQ pointers when a ROB flush fires during quiesce (T=0, effect at T+1).
  // This ensures the FTQ enq_ptr=1 is visible BEFORE redirect_flush fires at T+1
  // and the IFU allocates a new entry. The previous approach (keyed on
  // pipeline_drained_strict) fired at T+1 — same cycle as redirect_flush — so the
  // IFU allocated at the OLD enq_ptr while the reset set enq_ptr:=1 one cycle later
  // (T+2), orphaning the in-flight fetch and causing a permanent hang.
  val ftq_reset_pulse = rob.io.flush.valid && cf_quiesce_core
  io.ifu.cf_ftq_quiesce_reset := ftq_reset_pulse
  // LSQ pointer reset: reset head/tail to 0 so WrapInc wraps correctly after CSR reconfiguration.
  // Fires on same pulse as quiesce_flush (QS_DRAINING→QS_FETCH, queues already empty).
  io.lsu.cf_lsq_quiesce_reset := quiesce_flush_pulse
  // ROB pointer reset: reset head/tail to 0 so WrapInc wraps correctly after cf_rob_entries CSR change.
  rob.io.cf_rob_quiesce_reset := quiesce_flush_pulse
  rob.io.cf_rob_entries_wen   := custom_csrs.cf_rob_entries_wen
  // RAS domain/secret shadow clear: prevents stale IFT bits from entries beyond the new active
  // RAS size from corrupting domain/secret attribution after a cf_ras_idx CSR reconfiguration.
  io.ifu.cf_ras_quiesce_flush := quiesce_flush_pulse
  } else {
    // Reconf disabled: no quiesce FSM, no pipeline drain, no pointer resets.
    // Drive all quiesce-related wires and IOs to neutral defaults so that
    // downstream consumers see "never in quiesce" state and produce identical
    // behavior to a non-reconfigurable BOOM.
    cf_quiesce_core     := false.B
    pipeline_drained    := false.B
    quiesce_flush_pulse := false.B
    qs_draining         := false.B
    io.ifu.allow_fetch          := true.B
    io.ifu.cf_btb_quiesce       := false.B
    io.ifu.cf_ftq_quiesce_reset := false.B
    io.ifu.cf_ras_quiesce_flush := false.B
    io.lsu.cf_lsq_quiesce_reset := false.B
    rob.io.cf_rob_quiesce_reset := false.B
    rob.io.cf_rob_entries_wen   := false.B
    rename_stage.io.quiesce_flush      := false.B
    fp_rename_stage.io.quiesce_flush   := false.B
    pred_rename_stage.io.quiesce_flush := false.B
  }

  // [DBGFREEZE] patch 85 (b): commit records of debug-mode uops are masked and marked (rob.scala)
  rob.io.cf_dbg.foreach(_ := cf_dbg)
  // [RECV3] SP-03 (design 2.8, 5.4): the 16-bit cycle stamp of every instruction record, free-running from reset and
  // wrapping at 2^16.  The tile's 48-bit counter (spec SP-16) shares the reset, so tile_cyc(15,0) == ift_cyc16 every
  // cycle.  COMMIT = the commit cycle (rob.scala commit packer), SQUASH = the capture cycle (rob.scala capture, SP-04),
  // KILL = the cycle the lane takes the kill record (kill packer, SP-05).  IFT only: 16 FF + a 16-bit incrementer.
  // IFT off: the constant 0 (no flop; the ROB port does not exist).
  val ift_cyc16 = WireInit(0.U(16.W))
  if (ENABLE_IFT) {
    val ift_cyc16_r = RegInit(0.U(16.W))
    ift_cyc16_r := ift_cyc16_r + 1.U
    ift_cyc16   := ift_cyc16_r
  }
  rob.io.ift_cyc.foreach(_ := ift_cyc16)
  io.ifu.cf_dbg.foreach(_ := cf_dbg)   // [DBGFREEZE] (b): no fetch-time edges / domain (fetch-buffer.scala)

  // corefuzzing: preg_secret — per-physical-register secret-taint bit array.
  // Updated EARLIER than taint_table (which only updates at commit via C7):
  //   - At TLB stage (for loads hitting the secret range): fired by cf_preg_secret_upd from LSU
  //   - At writeback (transitive): if any source preg was secret, set pdst's preg_secret
  // This allows in-flight consumers to receive cf_secret_propagation at writeback time,
  // before the producer commits (without requiring a fence.i between producer and consumer).
  // Cleared at quiesce_flush_pulse (same as taint_table) for clean campaign boundaries.
  //
  // IFT compile-time gating: the RegInit declarations are always present so that
  // unconditional readers elsewhere in core.scala see a well-typed Vec.  All write
  // sites (quiesce clear, TLB update, commit clear, writeback transitive) are
  // wrapped in `if (ENABLE_IFT) { ... }`.  When ENABLE_IFT=false, the registers
  // have no drivers — FIRRTL DCE elides them entirely and all readers collapse to
  // constant `false` via constant propagation.
  if (ENABLE_IFT) {
    when (quiesce_flush_pulse) {
    }
    // FP physical register secret-taint table: mirrors preg_secret for the FP register file.
    // Updated at: TLB stage (FP loads to secret range, routed via CF_PregSecretUpd.is_fp),
    //             writeback (transitive FP→FP taint and INT→FP conversions).
    // Cleared at quiesce_flush_pulse for clean IFT campaign boundaries.
    when (quiesce_flush_pulse) {
    }
  }

  //val icache_blocked = !(io.ifu.fetchpacket.valid || RegNext(io.ifu.fetchpacket.valid))
  val icache_blocked = false.B 
  csr.io.counters foreach { c => c.inc := RegNext(perfEvents.evaluate(c.eventSel)) }
  
  // corefuzzing
  // Drive EXU debug enable into all execution units from CSRs
  for (eu <- exe_units) {
    eu.io.cf_debug_exu_enable := custom_csrs.cf_debug_enable && custom_csrs.cf_debug_core_enable
    eu.io.cf_div2_en.foreach(_ := custom_csrs.cf_div2_en)   // [DIV2] registered inside the unit
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
  io.ifu.redirect_val                  := false.B
  io.ifu.redirect_flush                := false.B
  io.ifu.redirect_flush_is_mispredict  := false.B
  // [SPECBPU-REP] (owner 2026-11-02, RULES C 'Speculative BPU repair'): which CFIs of the redirect entry this redirect
  // squashes, for the FTQ undo log (read only with redirect_val).  Default for the cycles without a redirect.
  io.ifu.bpu_sq.pc_lob := 0.U
  io.ifu.bpu_sq.incl   := false.B

  // Breakpoint info
  io.ifu.status  := csr.io.status
  io.ifu.bp      := csr.io.bp
  io.ifu.mcontext := csr.io.mcontext
  io.ifu.scontext := csr.io.scontext

  // Suppress ICache flush during quiesce: SSTEP instructions are CSR writes that don't
  // modify instruction memory. The ICache should only be flushed after the final fence.i
  // that exits quiesce (cf_quiesce_core = 0 at that point, so the flush fires normally).
  io.ifu.flush_icache := (0 until coreWidth).map { i =>
    (rob.io.commit.arch_valids(i) && rob.io.commit.uops(i).is_fencei && !cf_quiesce_core) ||
    (RegNext(dec_valids(i) && dec_uops(i).is_jalr && csr.io.status.debug))
  }.reduce(_||_)
  // Ceviche (K2): the domain whose fence.i commits this cycle; 1 = attacker. A debug-mode jalr flush (no domain) is
  // reported as "all domains" by the I$ (I5) because it never coincides with a fence.i commit (debug mode).
  io.ifu.cf_cev_inval_dom.foreach(_ := (0 until coreWidth).map(i => rob.io.commit.arch_valids(i) && rob.io.commit.uops(i).is_fencei &&
                                         rob.io.commit.uops(i).cf_domain_id.get === 1.U).reduce(_||_))
  io.ifu.cf_cev_dbg_flush.foreach(_ := (0 until coreWidth).map(i => RegNext(dec_valids(i) && dec_uops(i).is_jalr && csr.io.status.debug)).reduce(_||_))

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
    // [SPECBPU-REP] the flushing uop's slot.  An exception or refetch flush does not commit that uop, so a CFI in that
    // very slot is squashed too (incl); eret and flush-on-commit (next) commit it.
    io.ifu.bpu_sq.pc_lob := RegNext(rob.io.flush.bits.pc_lob)
    io.ifu.bpu_sq.incl   := flush_typ === FlushTypes.xcpt || flush_typ === FlushTypes.refetch
  } .elsewhen (brupdate.b2.mispredict && !RegNext(rob.io.flush.valid)) {
    val block_pc = AlignPCToBoundary(io.ifu.get_pc(1).pc, icBlockBytes)
    val uop_maybe_pc = block_pc | brupdate.b2.uop.pc_lob
    val npc = uop_maybe_pc + Mux(brupdate.b2.uop.is_rvc || brupdate.b2.uop.edge_inst, 2.U, 4.U)
    val jal_br_target = Wire(UInt(vaddrBitsExtended.W))
    jal_br_target := (uop_maybe_pc.asSInt + brupdate.b2.target_offset +
      (Fill(vaddrBitsExtended-1, brupdate.b2.uop.edge_inst) << 1).asSInt).asUInt
    val bj_addr = Mux(brupdate.b2.cfi_type === CFI_JALR, brupdate.b2.jalr_target, jal_br_target)
    val mispredict_target = Mux(brupdate.b2.pc_sel === PC_PLUS4, npc, bj_addr)
    io.ifu.redirect_val                  := true.B
    io.ifu.redirect_pc                   := mispredict_target
    io.ifu.redirect_flush                := true.B
    io.ifu.redirect_flush_is_mispredict  := true.B
    io.ifu.redirect_ftq_idx              := brupdate.b2.uop.ftq_idx
    io.ifu.bpu_sq.pc_lob                 := brupdate.b2.uop.pc_lob   // [SPECBPU-REP] the mispredicted CFI stays
    io.ifu.bpu_sq.incl                   := false.B
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
      ftq_entry.cfi_is_ret  && ftq_entry.cfi_idx.bits === cfi_idx,
      cf_ras_active_core)


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

  // FTQ secret tag (cf_fetch_secret: the packet's BPD/BTB/RAS/I$/iTLB secret shadows at commit).
  // [FTQCFI] r170 B (DF-dt-sprop): it is set only by
  //   (i)   the CFI's own condition/target data taint: brinfos (br/jalr at resolution; a jal has
  //         no register source) and an sfb branch through jmp_unit (enableSFBOpt only);
  //   (ii)  window membership: the r160 range (29b), the patch-38 enqueue tag, [RASJAL] calls;
  //   (iii) a secret-range access in the packet (LSU TLB port, DF-ftq-lsu-port).
  // A packet-mate's secret WRITEBACK does not tag the packet (the C1 writeback feed is removed;
  // the ROB side of cf_s_prop_rob_upd is unchanged).  Bus ports 0..coreWidth-1 = this compactor,
  // coreWidth.. = the LSU port.  A compaction drop (more than coreWidth inputs in one cycle) only
  // misses a mark (FTQ writes are monotone).  brinfos, not brupdate.b2 (b2 = oldest mispredict).
  // [RASJAL] step 15: + coreWidth inputs (lowest priority) for dispatched JALs, see below.
  // Inputs: brinfos 0..coreWidth-1, [RASJAL] coreWidth..2*coreWidth-1, sfb 2*coreWidth.
  val ftq_taint_compactor = Module(new Compactor(2 * coreWidth + 1, coreWidth,
                                                 UInt(log2Ceil(ftqSz).W)))
  // sfb branch (r170 B): is_br excludes sfb (functional-unit.scala is_br), so it never reaches
  // brinfos; it resolves on jmp_unit's iresp (the predicate write).  Constant 0 without SFB.
  ftq_taint_compactor.io.in(2 * coreWidth).valid := (if (ENABLE_IFT)
    jmp_unit.io.iresp.valid && jmp_unit.io.iresp.bits.uop.is_sfb_br && jmp_unit.io.iresp.bits.secret else false.B)
  ftq_taint_compactor.io.in(2 * coreWidth).bits  := jmp_unit.io.iresp.bits.uop.ftq_idx
  // the branch path: tag at resolution, when secret-dependence is known.
  // MUST use brinfos (per-ALU resolution), NOT brupdate.b2 -- core.scala:226 sets
  //   b2.uop := UpdateBrMask(brupdate, oldest_mispredict.uop)
  // so b2 carries ONLY the oldest MISPREDICTED branch (and b2.valid is never even
  // assigned).  A correctly-predicted secret-dependent branch never appears there,
  // which is why the first version of this fix changed nothing at all.
  // brinfos(i) := alu_units(i).io.brinfo carries EVERY resolved branch.
  // [STEP1] accumulates one-hot br_tags of branches resolving this cycle with an
  // attacker-influenced condition; broadcast to the ROB to mark their shadows.
  // NOTE one-hot(br_tag), NOT br_mask: a branch's br_mask holds the OLDER branches it
  // is speculating under, not its own tag.
  // [STEP1 2026-09-06] R2+R3+R4: consume cond_atk (computed at functional-unit.scala:570
  // and read NOWHERE before this) and broadcast the resolving branch's tag so every
  // in-flight uop in its speculative shadow is marked attacker-influenced.
  // STEERING, not ownership: spectre-v1's bounds check is VICTIM code, so the old
  // ownership test (spec_branch_atk_table from cf_domain_id) is 0 exactly when it matters.
  // A REDUCTION, not an accumulator: `w := w | x` on a Wire is a combinational cycle.
  // one-hot(br_tag), NOT br_mask -- a branch's br_mask holds the OLDER branches it
  // speculates under, not its own tag.
  // AREA: gated so the 20-bit x 256-entry ROB compare vanishes when IFT is off.
  // rob.scala:171 states the convention -- the ROB port is driven unconditionally,
  // core.scala gates the driver; a constant 0 lets FIRRTL DCE strip the per-entry
  // `(br_mask & cf_br_atk_mask).orR` in every ROB row (~2.3k LUT6 + a 20-bit
  // broadcast net).  ENABLE_IFT is true here, so the emitted Verilog is unchanged.
  val cf_br_atk_mask_acc = if (ENABLE_IFT) {
    (0 until brinfos.length).map(i =>
      Mux(brinfos(i).valid && brinfos(i).cond_atk,
          UIntToOH(brinfos(i).uop.br_tag)(maxBrCount-1,0), 0.U(maxBrCount.W))
    ).reduce(_ | _)
  } else { 0.U(maxBrCount.W) }

  for (i <- 0 until brinfos.length) {
    // [GHLOCK] step 19: tag on the branch's CONDITION taint (register-read rs1/rs2 secret),
    // not on the uop's aggregate s_prop.  The aggregate includes the fetch-buffer GHR-window
    // bit (fetch-buffer.scala), so a branch fetched inside an open window re-tagged its FTQ
    // entry and re-armed the window at commit (fetch-target-queue.scala cf_ghist_secret_ctr):
    // a closed loop that never decays (t64 m80/m81: sbr=64 on 182,284 records to program end).
    // The FTQ fetch-secret bit also feeds the BPD/RAS secret shadows, so they arm on data too.
    ftq_taint_compactor.io.in(i).valid :=
      brinfos(i).valid && brinfos(i).cond_secret
    ftq_taint_compactor.io.in(i).bits  := brinfos(i).uop.ftq_idx
    // [FTQATK] patch 47 (r173 (5)): the attacker twin, one direct FTQ port per lane (no compactor:
    // brinfos is a register and the FTQ has a port per lane).  cond_atk = rs1/rs2 taint_atk (value
    // taint; the domain seed makes an attacker-written argument count).  Read only by the FTQ.
    io.ifu.cf_atk_ftq_updates.foreach { p =>
      p(i).valid := brinfos(i).valid && brinfos(i).cond_atk
      p(i).bits  := brinfos(i).uop.ftq_idx
    }
    // [IMPLWIN] step 20: the sticky control context that was set here is removed.
    // STEERING, not ownership: a mispredict is attacker-CAUSED when the branch's own
    // CONDITION is attacker-derived.  The previous test (branch in attacker domain) is
    // false for every steered gadget -- spectre-v1's bounds check is victim code.
    // NOTE: there is deliberately NO sticky attacker context here.  A bit meaning
    // "an attacker branch resolved at some point" never decays, and since the attacker
    // executes constantly it would tag essentially the whole program -- the same
    // pre-emptive shape that made the secret side tag 97% of a function and leak across
    // the clear_ift_csrs() phase boundary.  Attacker influence propagates through DATA
    // (regfile taint_atk -> operands -> results) and through STATE (predictor/cache
    // domain shadows raising an edge AT the cross-domain hit), both of which decay.
  }
  // [ARMRES] step 20 (rulings 134/137): the secret / attacker WINDOW lives here, in the core.
  // ARM: when a branch RESOLVES (brinfos is already a register stage after the ALU):
  //   secret  : a conditional branch whose condition is DATA-secret (brinfo.cond_secret =
  //             rs1/rs2 data taint; window taint never reaches it, ruling 135);
  //   attacker: an attacker-OWNED conditional branch.
  //   The counter := globalHistoryLength (64) and that branch becomes B; a later arming branch
  //   re-arms and replaces B (the youngest of one cycle's arming branches wins).
  // DECAY (r137): -1 per RETIRING conditional branch YOUNGER than B.  B's own retirement and
  //   older branches retiring after the arm do not count.  Cheapest compare: the commit slots
  //   are one ROB row (the head row), so before B retires a slot counts only if B is in the head
  //   row and the slot's bank is above B's bank (one row-index equality + coreWidth constant
  //   compares on the bank bits); once B has retired (or was killed / flushed: bpass) every
  //   retiring conditional branch counts.  PopCount over coreWidth, saturating subtract.
  //   The FTQ's retire-time counters (fetch-target-queue.scala) are no longer read.
  // Coverage of B, in program order (wrong-path fetches are also marked):
  //   (a) fetched while the window is open            -> fetch-buffer.scala, at fetch;
  //   (b) fetched before, dispatched at/after arming  -> dispatch below (win_*_now);
  //   (c) dispatched before arming, younger than B    -> [WINSHADOW], marked at writeback if it
  //       writes back after B resolves, else at commit (iregfile/fregfile win_set_*); their
  //       FTQ packets are tagged when B resolves ([WINFTQ] patch 29b).
  val winW = log2Ceil(globalHistoryLength + 1)
  require(winW <= 7, "window counter must fit the 7-bit sbr/abr fields")
  // [DBGFREEZE] patch 85 (a): no arm while the hart is in debug mode (the debug ROM's branches already resolve with
  // clean operands, the register-read mask; this also drops the arm of a flushed program branch resolving after the
  // entry, which re-arms when it re-executes after dret).  One more input on each arm LUT.
  // [JALRWIN] r205 (owner-approved addition): a jalr whose TARGET register carries the secret tag also opens the
  // window (brinfo.cond_secret = rs1 secret for a jalr, functional-unit.scala): the code it jumps to, and the
  // predictor state trained there, depend on the secret (switch tables, secret-indexed calls).  The window still
  // decays per committed GHR shift (patch 71); a jalr adds no shift, so its window lasts 64 shifts after it.
  // With the one tag (regfile [ONETAG]) a `ret` whose ra was written by an s_prop instruction (a jal inside an
  // open window) arms it too (flood item F-ret, OPEN O3).  One LUT input per lane, 0 levels.
  val win_arm_sec_v = brinfos.map(b => ENABLE_IFT.B && !cf_sec_disarm_c && !cf_dbg && b.valid && (b.uop.is_br || b.uop.is_jalr) && b.cond_secret)
  // [GHRWIN] patch 62 (V3 (a), owner 2026-10-19 16:3x): the attacker window also ARMS on a VICTIM conditional
  // branch whose condition is attacker-tainted (cond_atk = rs1/rs2 taint_atk, the [FTQATK] bit): its outcome is
  // attacker-steered and enters the GHR like an attacker branch's (BHI-style steering).  +1 LUT input per lane
  // on a brinfos-register term (0 levels on the counter, whose arm mux already waits on the youngest-select).
  // [JALRWIN] r205 O11 (owner 2026-10-20, FINAL, symmetric to the secret window): a jalr whose TARGET register carries
  // the attacker tag also arms the attacker window (cond_atk = rs1 taint_atk for a jalr).  An attacker-owned jalr with a
  // clean target does not (the owner's words: "attacker-tagged target").  +1 LUT input per lane, 0 levels.
  val win_arm_atk_v = brinfos.map(b => ENABLE_IFT.B && !cf_dbg && b.valid &&   // [DBGFREEZE]
                                       ((b.uop.is_br && (b.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 1.U || b.cond_atk)) ||
                                        (b.uop.is_jalr && b.cond_atk)))
  val win_arm_sec = win_arm_sec_v.reduce(_||_)
  val win_arm_atk = win_arm_atk_v.reduce(_||_)
  def winRow(x: UInt): UInt  = if (coreWidth == 1) x else (x >> log2Ceil(coreWidth))
  def winBank(x: UInt): UInt = if (coreWidth == 1) 0.U else x(log2Ceil(coreWidth)-1, 0)
  // [WINEXP] step 21 (owner r139, F3): each kind also keeps a 2-bit window EPOCH that advances
  // when its window CLOSES: the counter is open and reaches 0 this cycle by decay or disarm,
  // with no arm (an arm while open re-arms without a close, so live taint survives it).  It
  // updates on the same edge as the counter.  Window taint is stamped with the epoch (uop
  // cf_wep_* at dispatch, regfile and D$ store-meta entries) and is live only while it matches:
  // every uop in flight at the close is younger than the closing branch, i.e. outside the
  // window.  Aliasing after 4 closes can only revive stale taint (over-taint, the safe side).
  // [WINCLR] patch 35 C (r166): the register files no longer store the epoch; they flash-clear
  // on `close` (below), so the 4-close alias remains only for memory marks (STQ, D$ store meta).
  // Cost: a 2-bit register and incrementer per kind; close = 7-b zero detect AND the existing
  // ctr > cnt compare (not on the counter's own path).
  // [R124GATE] patch 26 (r151): the window state exists only with ENABLE_IFT; IFT off the
  // counter, epoch and close are constant 0 (winCtrIft is the unchanged step-20/21 logic).
  // [GHRWIN2] patch 71 (secret twin of V3 (a), owner 2026-10-19; re-based on live 2026-10-20, owner ruling: after 85):
  // BOTH windows now decay by committed GHR SHIFTS (patch 62 did the attacker one; the secret window kept the
  // per-branch count, which closes it early in branch-dense code: under-taint).  The shift detector of patch 62 does
  // not depend on the window (it models GlobalHistory.update over the committed conditional branches), so it is
  // built ONCE here and shared; each counter keeps only its own B-gate and re-arm discard.  Per lane, registered:
  // valid conditional branch, key = {ftq_idx, pc bank bit}.  A committed conditional branch is a shift iff its key
  // differs from the previous committed conditional branch's (last_v / last_k carry it across cycles; lanes in
  // commit order).  Explicit widths only (no getWidth on a RegNext / an inferred-width Reg): keyW = log2Ceil(ftqSz)
  // + 1 (the bank bit is a constant 0 when nBanks = 1).
  // [DBGFREEZE] patch 85 (a), kept for both windows: a branch retiring in debug mode (the debug ROM's polling loop)
  // is not a shift (`!cf_dbg` on vbr_q), so neither window ages in debug mode and last_v / last_k do not move
  // (the program's previous key survives the halt).  One more input on the existing valid && is_br LUT: 0 levels.
  // Cost: the 62 chain moves here unchanged (coreWidth x (1 + keyW) + 1 + keyW FF = 35 for coreWidth 4, ftqSz 32);
  // the secret counter adds its own gate / arm flops (coreWidth + 1 = 5 FF) and a 4-input PopCount, and loses its
  // old commit-valid PopCount.  Levels: the secret decay path now starts at flops (shallower, as 62).
  // IFT off: not elaborated (winCtrIft is IFT-only; the shared chain is constant false without ENABLE_IFT).
  // [GHRSAME] r205-10a (t206 S4, 2026-10-20): the {ftq_idx, bank} key counts one shift per FTQ entry and bank, but a
  // MISPREDICTED not-taken branch ends its FTQ entry inside a bank, and the frontend's repair (core.scala redirect,
  // `use_same_ghist`, upstream BOOM) re-uses the entry's history with current_saw_branch_not_taken when the
  // fall-through pc lies in the first bank of the 64-B line (block_pc there is aligned to icBlockBytes, not to the
  // packet).  The next entry's first bank then adds the ONE shift for both (GlobalHistory.update, frontend.scala:
  // first_bank_saw_not_taken), so a branch of that next entry in the line's first bank must not count again.  Without
  // this, every such pair counted 2 shifts for 1 and the window closed early (under-taint, P5): t206 S4 packs 64
  // cold-mispredicted not-taken bne's (BIM resets weakly taken), 4 of them at a line start: 64 counted, 60 real.
  // Rule (exact for the use_same_ghist case): a committed conditional branch whose key differs from the previous one
  // adds no shift when the previous one was NOT TAKEN with its fall-through (pc_lob + 2/4, the redirect's npc) inside
  // the line's first bank, and this one is in the NEXT FTQ entry and also in the line's first bank.  Residual (late,
  // safe): a not-taken branch followed in the same 8-B chunk by a taken jal/jalr back into that chunk also merges.
  // Other GHR moves the key cannot see and that are not counted (late, safe): the extra shift a flagged entry adds
  // with no branch of its own, and the ROB-flush redirect, which ZEROES the history (core.scala `new_ghist`).
  // Cost: per lane 2 FF (same-ghist candidate, first-bank bit) + 1 FF (last_s); per lane a 5-bit equality of the
  // lane's ftq_idx-1 (from the key flop) with the chain's previous ftq_idx and a 4-input AND: about 4 x 8 LUT.
  // Levels: the decrement starts at the key flop, in parallel with the previous-key mux chain; the equality sits
  // beside the existing key compare and folds into the same per-lane shift LUT: 0-1 level on a flop-to-flop path
  // (key flops -> popcount -> 7-bit counter), not on the commit path.  IFT off: not elaborated (as before).
  val cf_ghr_keyW = log2Ceil(ftqSz) + 1
  val cf_ghr_shift_q: Seq[Bool] = if (!ENABLE_IFT) Seq.fill(coreWidth)(false.B) else {
    def bankBit(u: MicroOp): UInt = if (nBanks == 1) 0.U(1.W) else u.pc_lob(log2Ceil(bankBytes))
    val lobW   = log2Ceil(icBlockBytes)
    val bnkW   = log2Ceil(bankBytes)
    val vbr_q  = (0 until coreWidth).map(w => RegNext(rob.io.commit.valids(w) && !cf_dbg && rob.io.commit.uops(w).is_br, false.B))
    val key_q  = Seq.fill(coreWidth)(Reg(UInt(cf_ghr_keyW.W)))
    // [GHRSAME] first-bank bit and same-ghist candidate (not taken, npc = pc_lob + 2/4 still in the line's first bank)
    val fb_q   = Seq.fill(coreWidth)(Reg(Bool()))
    val sg_q   = Seq.fill(coreWidth)(Reg(Bool()))
    for (w <- 0 until coreWidth) {
      val cu = rob.io.commit.uops(w)
      key_q(w) := Cat(cu.ftq_idx(log2Ceil(ftqSz) - 1, 0), bankBit(cu))
      fb_q(w)  := (if (lobW > bnkW) cu.pc_lob(lobW - 1, bnkW) === 0.U else true.B)
      sg_q(w)  := !cu.taken && ((cu.pc_lob(lobW - 1, 0) +& Mux(cu.is_rvc || cu.edge_inst, 2.U, 4.U)) < bankBytes.U)
    }
    val last_v = RegInit(false.B)
    val last_k = Reg(UInt(cf_ghr_keyW.W))
    val last_s = RegInit(false.B)
    var pv: Bool = last_v
    var pk: UInt = last_k
    var ps: Bool = last_s
    val sh = for (w <- 0 until coreWidth) yield {
      val kftq   = key_q(w)(cf_ghr_keyW - 1, 1)
      val merged = ps && fb_q(w) && WrapDec(kftq, ftqSz) === pk(cf_ghr_keyW - 1, 1)   // [GHRSAME]
      val s = vbr_q(w) && (!pv || (key_q(w) =/= pk && !merged))
      pv = pv || vbr_q(w)
      pk = Mux(vbr_q(w), key_q(w), pk)
      ps = Mux(vbr_q(w), sg_q(w), ps)
      s
    }
    last_v := pv
    last_k := pk
    last_s := ps
    sh
  }
  def winCtr(arms: Seq[Bool], clr: Bool): (UInt, UInt, Bool) =
    if (ENABLE_IFT) winCtrIft(arms, clr) else (0.U(winW.W), 0.U(2.W), false.B)
  def winCtrIft(arms: Seq[Bool], clr: Bool): (UInt, UInt, Bool) = {
    val ctr   = RegInit(0.U(winW.W))
    val ep    = RegInit(0.U(2.W))
    val b     = Reg(UInt(robAddrSz.W))
    val bpass = RegInit(true.B)
    val bmask = Reg(UInt(maxBrCount.W))
    val head  = rob.io.rob_head_idx
    var nv = false.B
    var nb = brinfos(0).uop.rob_idx
    var nm = brinfos(0).uop.br_mask
    for ((bi, a) <- brinfos zip arms) {
      val take = a && (!nv || IsOlder(nb, bi.uop.rob_idx, head))   // youngest arming branch
      nb = Mux(take, bi.uop.rob_idx, nb)
      nm = Mux(take, bi.uop.br_mask, nm)
      nv = nv || a
    }
    val b_head_row = winRow(b) === winRow(head)
    // [GHRWIN] patch 62 (V3 (a)), [GHRWIN2] patch 71: decay by the number of committed GHR SHIFTS younger than B.
    // GlobalHistory.update (frontend.scala) shifts the history once per fetch BANK whose executed part holds a
    // conditional branch (any number of not-taken ones, or the taken one); cf_ghr_shift_q (above, shared by both
    // windows, frozen in debug mode by 85) marks the committed conditional branches that add a shift, one cycle
    // late from flops.  The B-gate is registered with them: before B retires a lane counts only if B is in the head
    // row and the lane's bank is above B's (r137); after (bpass) every shift counts.  B's own bank: B is the previous
    // branch of a same-bank successor, so that successor adds no shift (exact).  A late decay only closes the window
    // late (P5: never early).  Residual (late, safe): an FTQ index that recurs after exactly ftqSz branch-free
    // packets merges two banks into one shift.
    val gate_q = (0 until coreWidth).map(w => RegNext(bpass || (b_head_row && winBank(b) < w.U), false.B))
    // a re-arm in cycle c discards cycle c's commits (the old rule: `when (nv) ctr := 64` wins over that
    // cycle's decrement); they are counted one cycle late here, so drop them explicitly (else the new
    // window would start up to coreWidth shifts short: an early close)
    val arm_q = RegNext(nv, false.B)
    val cnt = Mux(arm_q, 0.U, PopCount((0 until coreWidth).map(w => cf_ghr_shift_q(w) && gate_q(w))))
    val b_com  = b_head_row && (0 until coreWidth).map(w =>
      rob.io.commit.valids(w) && winBank(b) === w.U).reduce(_||_)
    val killed = (bmask & brupdate.b1.mispredict_mask) =/= 0.U
    bmask := bmask & ~brupdate.b1.resolve_mask
    when (b_com || killed || rob.io.flush.valid) { bpass := true.B }
    val ctr_gt = ctr > cnt
    ctr := Mux(ctr_gt, ctr - cnt, 0.U)
    when (nv) {
      ctr   := globalHistoryLength.U
      b     := nb
      bmask := nm & ~brupdate.b1.resolve_mask
      bpass := (nm & brupdate.b1.mispredict_mask) =/= 0.U
    }
    when (clr || !ENABLE_IFT.B) { ctr := 0.U }   // [WINDISARM] for the secret window
    val close = ENABLE_IFT.B && ctr =/= 0.U && (clr || (!nv && !ctr_gt))   // [WINEXP]
    when (close) { ep := ep + 1.U }
    (ctr, ep, close)
  }
  // [FLUSHCLOSE] r205-10d (owner ruling 2026-10-20): a full pipeline flush ZEROES the frontend GHR (the
  // `RegNext(rob.io.flush.valid)` redirect above: new_ghist = 0, for every ROB flush: exceptions, fence / fence.i,
  // every CSR instruction (decode.scala: all CSR ops and FENCE are unique + flush_on_commit), other unique flushes),
  // so it CLOSES both windows (counter := 0 through the existing clr input: epoch advance, shadow clear, as any
  // close).  EXCEPT debug mode (patch 85 keeps the windows across DMI halts and freezes them while halted):
  //   - debug ENTRY is an exception flush at cycle c; the CSR sees it at c+1 (csr.io.exception := RegNext(com_xcpt)),
  //     reg_debug rises at c+2 and cf_dbg at c+3 (85 bracket).  The close is therefore applied at c+3 and dropped when
  //     cf_dbg = 1 there.  c+3 is still before any refetched uop is observed: the frontend reads the exported counter
  //     at f3 (redirect c+1 -> f3 c+4) and the fetch buffer at c+5, dispatch later, so every reader sees the close;
  //   - flushes inside debug mode (debug ROM / program buffer) and dret's own flush (dret is unique + flush_on_commit;
  //     cf_dbg falls at its CSR execution + 2, so its commit flush lands within 3 cycles of cf_dbg = 1) are dropped by
  //     a 4-cycle debug history.  The first program flush after dret needs a refetch, dispatch and commit (> 4 cycles).
  // An arm in c+1..c+3 (a stale brinfos resolve of a flushed branch) is overridden: clr wins over the arm in winCtrIft.
  // Cost: 5 FF (2 debug-history, 3 flush-delay), 2 LUT; one more input on the secret clr OR, and the attacker counter's
  // clr goes from constant false to this flop term.  Levels: all from flops; on the 7-bit counter D path the clear is
  // the last term (the existing `when (clr)`), 0-1 LUT on a flop-to-flop path.  IFT off: cf_dbg is constant false and
  // winCtr is not elaborated.
  val cf_dbg_hist     = if (ENABLE_IFT) { val q2 = RegNext(cf_dbg_q, false.B); cf_dbg || cf_dbg_q || q2 || RegNext(q2, false.B) } else false.B
  val cf_fl_q1        = if (ENABLE_IFT) RegNext(rob.io.flush.valid && !cf_dbg_hist, false.B) else false.B
  val cf_fl_q3        = if (ENABLE_IFT) RegNext(RegNext(cf_fl_q1, false.B), false.B) else false.B
  val win_flush_close = cf_fl_q3 && !cf_dbg   // [FLUSHCLOSE] drops the debug-entry flush (cf_dbg = 1 at flush + 3)
  val (win_sec_ctr_r, win_sec_ep, win_sec_close) = winCtr(win_arm_sec_v, cf_sec_disarm_c || win_flush_close)   // [GHRWIN2] 71; [FLUSHCLOSE]
  val (win_atk_ctr_r, win_atk_ep, win_atk_close) = winCtr(win_arm_atk_v, win_flush_close)                      // [GHRWIN] 62; [FLUSHCLOSE]
  // [DBGFREEZE] patch 85 (a): the windows keep their state through debug mode (no decay, no arm, no close) but are
  // seen CLOSED by every reader while the hart is in debug mode: dispatch membership (win_*_now, d_win_*), the
  // record counters (dis_ift sbr/abr) and the frontend export (fetch-time ghist / FTQ enqueue tag / RAS push secret /
  // I$ fill secret).  After dret the readers see the preserved counter again, so the refetched program resumes in
  // the window it left.  Cost: 2 x 7 AND (the export) and one input on each `=/= 0` reduction: 0 levels there, one
  // LUT on the flop-sourced export (core -> frontend).  IFT off: cf_dbg = false, the Mux folds.
  val win_sec_ctr = Mux(cf_dbg, 0.U, win_sec_ctr_r)
  val win_atk_ctr = Mux(cf_dbg, 0.U, win_atk_ctr_r)
  io.ifu.cf_win_sec_ctr := win_sec_ctr
  io.ifu.cf_win_atk_ctr := win_atk_ctr
  val win_sec_now = win_sec_ctr =/= 0.U || win_arm_sec
  val win_atk_now = win_atk_ctr =/= 0.U || win_arm_atk
  // [L11CFATK] lean59 (Fable audit 2026-10-23 L11; RULES A "Record summary for both", U2 "all edges", D1 "Edge
  // is_secret = producer s_acc || s_prop"): the CONTROL_FLOW (ty 20) edge's producer is the branch that ARMED the
  // secret window, so its is_atk is that branch's attacker record (attacker domain || the issued uop's
  // cf_attacker_influence = dispatch record [M4RECSUM] || operand attacker taint, register-read), no longer the
  // dispatching consumer's own bit.  One flop holds the label of the last arm (the arm cycle's arming lanes, ORed:
  // several arms in one cycle are rare); in an arm cycle the dispatch group reads the new arms directly (they are
  // younger than the new B, which owns the window).  Not cleared at the close: a uop fetched in the window and
  // dispatched after it closed (fetch-time membership, d_win_sec through pre.cf_win_sec) reads the label of the
  // branch it was fetched under (approximate when a later arm replaced it: label noise only).
  // Cost: 1 FF, ~3 LUT (coreWidth AND-ORs + the mux).  Levels: brinfos registers -> 1 LUT per lane -> OR (1 LUT)
  // -> mux (select win_arm_sec, already built) -> the candidate's is_atk input, which feeds only the prefix
  // network whose outputs are registers (0 levels on dis_fire); the flop D is the same depth.  IFT off: constant 0
  // (the replaced term was the absent field's getOrElse(false.B)), nothing elaborated.  RECORD CHANGE.
  val win_sec_lbl: Bool = if (ENABLE_IFT) {
    val arm_lbl = (brinfos zip win_arm_sec_v).map { case (b, a) =>
      a && (b.uop.cf_attacker_influence.getOrElse(false.B) || b.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 1.U)
    }.reduce(_||_)
    val q = RegInit(false.B)
    when (win_arm_sec) { q := arm_lbl }
    Mux(win_arm_sec, arm_lbl, q)
  } else false.B

  // [WINSHADOW] step 20, case (c): uops dispatched before B resolved and younger than B were
  // fetched without the window bit and may already have written back (an `x = 1` right after
  // the branch executes long before it resolves).  They are the ROB entries between B and the
  // tail T at resolution.  One record per kind (WinShadowInfo, rob.scala): oldest B / newest T
  // on merge; the range dies when B is killed (its br_mask meets a mispredict), on a ROB flush,
  // when the head reaches T, or on disarm.  info.in(rob_idx) = younger than B and older than T.
  // Users: every register write port and bypass (writeback-time mark), the ROB (record s_prop /
  // atk at commit) and the commit-time set ports (entries that wrote back before B resolved).
  // Logic: <= 2 IsOlder per arming port; each user one or two IsOlder (8 b) on its rob_idx.
  // [WINEXP] step 21: the range also dies when its window CLOSES (every entry still in flight
  // then is younger than the closing branch, outside the window), and carries the epoch.
  // [R124GATE] patch 26 (r151): IFT off the shadow record is a constant-0 (invalid) wire.
  def winShadow(arms: Seq[Bool], clr: Bool, close: Bool, ep: UInt): WinShadowInfo =
    if (ENABLE_IFT) winShadowIft(arms, clr, close, ep) else 0.U.asTypeOf(new WinShadowInfo)
  def winShadowIft(arms: Seq[Bool], clr: Bool, close: Bool, ep: UInt): WinShadowInfo = {
    val v     = RegInit(false.B)
    val b     = Reg(UInt(robAddrSz.W))
    val bdone = Reg(Bool())
    val t     = Reg(UInt(robAddrSz.W))
    val left  = Reg(Bool())
    val arr   = Reg(Bool())   // [TAILROW] patch 29b: the head reached T's row after leaving it
    val full  = Reg(Bool())   // [WINTALL] the ROB was full at capture
    val mask  = Reg(UInt(maxBrCount.W))
    val head  = rob.io.rob_head_idx
    // arming ports that keep their shadow: not mispredicted (its shadow is wrong path and
    // refetched under (a)/(b)) and not killed this cycle by an older mispredict
    val ok = (brinfos zip arms).map { case (bi, a) =>
      a && !bi.mispredict && (bi.uop.br_mask & brupdate.b1.mispredict_mask) === 0.U }
    var nv = false.B
    var nb = brinfos(0).uop.rob_idx
    var nm = brinfos(0).uop.br_mask
    for ((bi, a) <- brinfos zip ok) {
      val take = a && (!nv || IsOlder(bi.uop.rob_idx, nb, head))   // oldest
      nb = Mux(take, bi.uop.rob_idx, nb)
      nm = Mux(take, bi.uop.br_mask, nm)
      nv = nv || a
    }
    // [AREA35] patch 35 (p4-35-area-rootcause.md, A): B commits on lane w iff that lane's
    // commit is valid and B is the head row's bank-w entry.  commit.valids(w) implies
    // com_idx = rob_head and the lane's rob_idx = Cat(rob_head, w) (proof: rob.scala [WINAGE]),
    // so this equals the old rob_idx === b compare on every cycle it is observed.  It is the
    // winCtr b_com form above.  The commit readout of rob_uop(*).rob_idx (64:1 x 8 b x 4 lanes)
    // had no other reader, so the 256 x 8 rob_idx flops die (-2048 FF).  Levels: register-sourced
    // (b, rob_head, rob_head_lsb) 6-bit + 2-bit compare, AND valid: -readout (about -4).
    val b_com  = winRow(b) === winRow(head) && (0 until coreWidth).map(w =>
      rob.io.commit.valids(w) && winBank(b) === w.U).reduce(_||_)
    val killed = (mask & brupdate.b1.mispredict_mask) =/= 0.U
    val nmask  = mask & ~brupdate.b1.resolve_mask
    val t_head = winRow(head) === winRow(t)
    // [TAILROW] patch 29b (DF-29a, owner r158): the range dies when the head INDEX reaches T,
    // not when the head enters T's ROW.  A capture with a partial tail row (enq_partial_stall:
    // T = {row, k}, k > 0) leaves members in T's row at banks < k; the step-20 rule killed the
    // range as soon as the head entered that row, while those members could still be in
    // flight, so they lost their writeback res() and their commit-time record s_prop / atk
    // (rob.scala inAge with v = 0).  Now, once the head has left T's row (left):
    //   reach: the head is in T's row at bank >= T's bank.  rob_head_lsb is the first VALID
    //          bank of the head row, so every member of that row (banks < k) has committed;
    //   arr && !t_head: the head reached T's row (arr) and has left it again (the row had no
    //          entry at banks >= k yet, so rob_head_lsb read 0 until it left).
    // While the head sits in T's row below T's bank, in() admits only that row's banks below
    // T's bank (idx - head < t - head), i.e. exactly the remaining members; nothing else is
    // allocated there (the tail is at or after T).  Full-row captures (k = 0) die on entry,
    // exactly as before.  Levels: head/t/left/arr are registers; reach is a 2-bit compare
    // beside the existing 6-bit t_head equality, folded into keep's LUT (v's D input only).
    // Cost: +1 FF, about 2 LUT per kind.
    val reach  = t_head && (if (coreWidth == 1) true.B else winBank(head) >= winBank(t))
    val keep   = v && !killed && !(left && (reach || (arr && !t_head)))
    when (nv) {
      t    := rob.io.rob_tail_idx
      full := rob.io.rob_tail_idx === head
      left := false.B
      arr  := false.B   // [TAILROW]
      v    := true.B
      when (!keep) {
        b := nb; mask := nm & ~brupdate.b1.resolve_mask; bdone := false.B
      } .elsewhen (!bdone && !b_com && IsOlder(nb, b, head)) {
        b := nb; mask := nm & ~brupdate.b1.resolve_mask
      } .otherwise {
        mask := nmask
        when (b_com) { bdone := true.B }
      }
    } .otherwise {
      v    := keep
      mask := nmask
      when (b_com) { bdone := true.B }
      when (!t_head) { left := true.B }
      when (left && t_head) { arr := true.B }   // [TAILROW]
    }
    when (clr || close || rob.io.flush.valid || !ENABLE_IFT.B) { v := false.B }
    val info = Wire(new WinShadowInfo)
    info.v     := v
    info.b     := b
    info.bdone := bdone
    info.t     := t
    // [WINTALL] Phase 4 Task 4.3c (gate review C10): "ROB full at capture" is the captured
    // rob_tail_idx === rob_head_idx, not "T is in the head row".  With B in flight the ROB is
    // not empty, so the equality holds only for a full ROB (head_lsb = 0 with a full head row,
    // tail_lsb = 0 after a full-row advance).  The step-20 form was also true for a partial
    // (enq_partial_stall) tail row shared with the head; the two differ only there.
    // Record-identical: the entries the old form admitted beyond T are exactly the entries
    // younger than or equal to T, i.e. dispatched in or after the capture cycle (a partial
    // tail row's banks >= tail_lsb are not yet allocated).  The capture cycle is an arm cycle
    // (nv needs an `ok` arm, a subset of win_arm_*), and v stays set only while the kind's
    // window stays open (close clears v; an arm cycle never closes), so all of them were
    // dispatched with cf_win_* = win_*_now = 1 and cf_wep = the current epoch: their
    // writeback res() is already true, the ROB already has their dispatch s_prop / atk
    // (d_win_*) and their commit uop already carries cf_win_*.  Attacker kind, domain-1 uops:
    // dispatch gives them no cf_win_atk, so the old form gave them a regfile watk mark the new
    // form does not; that mark is review C11 (no record effect: such a value carries the
    // producer's taint_atk through WritePort, which every consumer already turns into atk).
    // A full ROB with a partly retired head row (head_lsb > 0) now gives tall = 0, but T =
    // Cat(head row, 0) is then the youngest possible age (2^k - head_lsb), older than none of
    // the live entries, and no entry can be allocated at or after T until the head leaves
    // that row (t_head = 0: tall = 0 in both forms).  Keeping `t_head` in the term makes the
    // new tall exactly (old tall && full), so the analysis above is the whole difference.
    // Cost: +1 FF, one 8-bit eq (off the in() path: it feeds the flop), one LUT input.
    info.tall  := full && !left && t_head   // ROB was full at capture: every entry is older than T
    info.head  := head
    info.ep    := ep
    info
  }
  val wsh_sec = winShadow(win_arm_sec_v, cf_sec_disarm_c, win_sec_close, win_sec_ep)
  val wsh_atk = winShadow(win_arm_atk_v, false.B,         win_atk_close, win_atk_ep)
  // [WINCLR] patch 35 C (owner r166, replaces the regfile half of r140 (e) / [WINEXP] F3): the
  // register files keep a 1-bit window mark per entry and flash-clear every mark of a kind on the
  // edge that closes that kind's window ({atk, sec}), instead of storing and comparing the epoch.
  // Same value as the epoch compare (a mark written in cycle t is live at c > t iff no close in
  // [t, c-1]) except the epoch's 2-bit wrap: an entry left untouched across 4k closes used to
  // revive (over-taint); it now stays clear.  The epochs stay for uops (cf_wep_*, res()) and
  // memory (STQ, D$ store meta).  close is the signal that advances ep on the same edge.
  // [ONETAG] r205: the secret half never clears at the close (the tag stays until the register is overwritten)
  // [ONETAGATK] r205 U8: the attacker half never clears at the close either; the LSQ close clear (patch 64) is removed
  iregfile.io.cf_wclr := 0.U
  io.lsu.cf_wsh_sec.foreach(_ := wsh_sec)          // [FILLSEC] r205: shadow loads mark the lines they fill (lsu.scala)
  io.lsu.cf_wsh_atk.foreach(_ := wsh_atk)          // [ONETAGATK] r205 U8: the attacker twin (attacker shadow loads)
  pregfile.io.cf_wclr := 0.U   // [ONETAG] [ONETAGATK]
  rob.io.cf_wsh_sec := wsh_sec
  rob.io.cf_wsh_atk := wsh_atk

  // [WINFTQ] patch 29b (DF-step20 risk (i), owner r160; replaces patch 29's [WINGAP] register-
  // file writer records, about +7k LUT).  The FTQ index of the OLDEST secret-window arming
  // branch that resolved this cycle, with the same `ok` filter as the shadow capture (not
  // mispredicted: its shadow is wrong path; not killed by an older mispredict).  The FTQ sets
  // the secret bit of every entry younger than it (fetch-target-queue.scala), so the BPD/BTB/RAS
  // secret shadows trained by those packets are tagged even when a consumer of a register that
  // was written back before the resolution read it clean.  The missing ty-0 edges of such
  // consumers are rebuilt by the parser (ift_core.reconstruct_window_edges).  The register files are
  // not touched: 0 levels on the regfile write path.
  // Levels: brinfos registers -> coreWidth-1 IsOlder (8 b) + mux steps (the same chain as the
  // shadow's oldest-B select) -> FF.  Cost: 6 FF, about 30 LUT.
  // [WINFTQATK] (DF-ftq-atk-window-range, owner 2026-10-28 "Stage the RTL fix"; RULES A U8 "Yes, symmetric",
  // O11 "a jalr with an attacker-tagged target arms the attacker window"): the attacker twin.  The same select
  // over the ATTACKER-window arming lanes (win_arm_atk_v: an attacker-domain conditional branch, a victim
  // conditional branch with an attacker-tainted condition, a jalr with an attacker-tainted target), same `ok`
  // filter; the FTQ sets cf_fetch_atk on every entry younger than it, so a victim packet fetched after B and
  // enqueued before B resolved trains the BTB / uBTB / BIM / TAGE with the attacker label (bpdupdate
  // cf_domain_id |= cf_fetch_atk), exactly as a packet enqueued after the resolution ([M3ATKWIN]).  The old
  // note here ("the FTQ has no attacker bit") predates [FTQATK] / [M3ATKWIN].  Levels: as the secret port (a
  // parallel copy, from the same brinfos registers to its own flops).  Cost: 6 FF, about 30 LUT.
  if (ENABLE_IFT) {
    val wf_head = rob.io.rob_head_idx
    def winFtqRs(arms: Seq[Bool]): (Bool, UInt) = {
      val wf_ok = (brinfos zip arms).map { case (bi, a) =>
        a && !bi.mispredict && (bi.uop.br_mask & brupdate.b1.mispredict_mask) === 0.U }
      var wf_v = false.B
      var wf_b = brinfos(0).uop.rob_idx
      var wf_f = brinfos(0).uop.ftq_idx
      for ((bi, a) <- brinfos zip wf_ok) {
        val take = a && (!wf_v || IsOlder(bi.uop.rob_idx, wf_b, wf_head))   // oldest
        wf_b = Mux(take, bi.uop.rob_idx, wf_b)
        wf_f = Mux(take, bi.uop.ftq_idx, wf_f)
        wf_v = wf_v || a
      }
      (RegNext(wf_v, false.B), RegNext(wf_f))
    }
    val (wfs_v, wfs_f) = winFtqRs(win_arm_sec_v)
    io.ifu.cf_win_ftq_rs.get.valid := wfs_v
    io.ifu.cf_win_ftq_rs.get.bits  := wfs_f
    val (wfa_v, wfa_f) = winFtqRs(win_arm_atk_v)   // [WINFTQATK]
    io.ifu.cf_win_ftq_ra.get.valid := wfa_v
    io.ifu.cf_win_ftq_ra.get.bits  := wfa_f
  }

  // [RASJAL] step 15 (residue audit row 16): a JAL that links (call) under secret-dependent
  // control (cf_spec_branch_is_secret, or in the secret window cf_win_sec [IMPLWIN]; set in
  // the dispatch loop below; dis_uops is a Wire, so this reads the final value) tags its FTQ
  // packet.  JALs never reach brinfos (functional-unit.scala: brinfo.valid = is_br || is_jalr)
  // and have no source operands, so neither existing input could tag them, and the RAS slot the
  // call pushed stayed clean.  The FTQ's existing cfi_is_call gate then marks that RAS slot
  // (fetch-target-queue.scala late RAS write).  A non-linking JAL has ldst_val = 0.
  // [R124GATE] patch 26 (ruling 124): the two RegNexts exist only with ENABLE_IFT; IFT off ties
  // the inputs to a constant (valid 0), so the 4 x 6 flops go.
  for (w <- 0 until coreWidth) {
    // Registered: dis_fire is late in the cycle; one cycle of delay is harmless (monotone tag).
    if (ENABLE_IFT) {
      ftq_taint_compactor.io.in(coreWidth + w).valid := RegNext(
        dis_fire(w) && dis_uops(w).is_jal && dis_uops(w).ldst_val &&
        (dis_uops(w).cf_spec_branch_is_secret.getOrElse(false.B) || dis_win_sec_rec(w)), false.B)   // [IMPLWIN] window, not ctx
      ftq_taint_compactor.io.in(coreWidth + w).bits  := RegNext(dis_uops(w).ftq_idx)
    } else {
      ftq_taint_compactor.io.in(coreWidth + w).valid := false.B
      ftq_taint_compactor.io.in(coreWidth + w).bits  := 0.U
    }
  }
  for (w <- 0 until coreWidth) {
    ftq_taint_compactor.io.out(w).ready := true.B
    io.ifu.cf_secret_ftq_updates(w).valid := ftq_taint_compactor.io.out(w).valid
    io.ifu.cf_secret_ftq_updates(w).bits  := ftq_taint_compactor.io.out(w).bits
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

  val uop_with_step = Wire(Vec(coreWidth, new MicroOp()))
  val dec_ift       = Wire(Vec(coreWidth, new FetchIftInfo))
  
  for (w <- 0 until coreWidth) {
    dec_valids(w)                      := io.ifu.fetchpacket.valid && dec_fbundle.uops(w).valid &&
                      !dec_finished_mask(w)
  //  decode_units(w).io.wq.uop := dec_fbundle.uops(w).bits
    uop_with_step(w) := dec_fbundle.uops(w).bits
    dec_ift(w)             := dec_fbundle.ift(w)
    // [FIFO10] step 10 (section 2, G2-D): cf_domain_id is NOT recomputed here any more.  The
    // fetch buffer already computes it from the same PC and CSRs (fetch-buffer.scala, `< end`,
    // like the frontend, the LSU secret range and every other range test); this decode copy
    // was a second pair of vaddr comparators per lane and used `<= end`, labelling the
    // instruction AT the end address attacker here but victim at fetch.  The fetch-buffer value
    // rides the uop through decode and rename unchanged.
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
    val decFlushInflFmt = (0 until numInfluencerSlotsCF).zipWithIndex.map{case(_,k) => s"I$k={v=%d,oc=%d,ty=%d,atk=%d,sec=%d}"}.mkString(" ")
    val decFlushFmt = s"[FLUSH] 0x%x (0x%x) CF(domain=%d spec=%d atk=%d s_acc=%d s_prop=%d s_tx=%d opcount=%d spec_atk=%d spec_oc=%d fl=%d floc=%d) FU=0x%x SRC=%d INFL_FU=0x%x OVF=%d dcy=%d srob=%d sstq=%d alias=%d sftq=%d $decFlushInflFmt\n"   // [FTQHOLD] sftq: 0 here (printf commented out)
    // Task 3.3: the fetch list is no longer in the uop; fetchFlagsToList rebuilds it from the
    // FetchIftInfo flags (printf only).
    def printDecFlush(uop: MicroOp, ift: FetchIftInfo, fl: UInt, floc: UInt): Unit = {
      val dec_list = fetchFlagsToList(ift.fetch_flags, ift.any_is_victim, ift.btb_sec_pc)
      val inflArgs = (0 until numInfluencerSlotsCF).flatMap(k => Seq[Bits](
        dec_list.edges(k).valid,
        dec_list.edges(k).key,
        dec_list.edges(k).infl_type,
        dec_list.edges(k).is_atk,
        dec_list.edges(k).is_secret
      ))
      // [CFNOISE-COMMENTED] printf(decFlushFmt, (Seq[Bits](
        // [CFNOISE-COMMENTED] Sext(uop.debug_pc(vaddrBits-1,0), xLen), uop.debug_inst,
        // [CFNOISE-COMMENTED] uop.cf_domain_id, uop.cf_speculated, uop.cf_attacker_influence,
        // [CFNOISE-COMMENTED] uop.cf_secret_access, uop.cf_secret_propagation, uop.cf_secret_transmission,
        // [CFNOISE-COMMENTED] uop.cf_op_count_id, uop.cf_spec_branch_is_atk, uop.cf_spec_branch_op_id,
        // [CFNOISE-COMMENTED] fl, floc,
        // [CFNOISE-COMMENTED] uop.cf_fu_bitmap, 1.U, inflBitmapFromList(dec_list.edges), dec_list.dropped, uop.cf_cntd_deny_count, uop.cf_stall_cycles_rob, uop.cf_stall_cycles_stq, dec_list.aliased, 0.U /* [FTQHOLD] sftq */
      // [CFNOISE-COMMENTED] ) ++ inflArgs): _*)
    }
    for (w <- 0 until coreWidth) {
      // corefuzzing [FLUSH] decode-stage log — compile-time gated (see ENABLE_CF_DEBUG_PRINTF).
      if (ENABLE_CF_DEBUG_PRINTF) {
        when (custom_csrs.cf_debug_rob_enable && custom_csrs.cf_debug_enable) {
          // Path A: branch mispredict kills this instruction
          when (dec_valids(w) && IsKilledByBranch(brupdate, dec_uops(w))) {
            val dec_uop = dec_uops(w)
            val fl_cross_domain = brupdate.b2.mispredict &&
              (brupdate.b2.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= dec_uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)))
            val fl_op_count = Mux(brupdate.b2.mispredict, brupdate.b2.uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)), 0.U)
            printDecFlush(dec_uop, dec_ift(w), fl_cross_domain, fl_op_count)
          }
          // Path B: ROB flush (exception, FENCE.I, etc.) — kills all in-flight decode instructions
          .elsewhen (dec_valids(w) && rob.io.flush.valid) {
            printDecFlush(dec_uops(w), dec_ift(w), 0.U, 0.U)
          }
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
  // [ILLPROBE] r30 diagnosis: r30_flush_wipe takes mcause=2 (illegal instruction) at
  // 0x80001358, whose encoding (0x00151793, slli a5,a0,0x1) is perfectly legal, then
  // the handler recurses 1,066x on its own legal `jal`.  Print the CYCLE, the PC and
  // the INSTRUCTION WORD the core actually decoded: if the word is not the one in the
  // ELF, the frontend/I$ delivered garbage; if it matches, decode is rejecting a legal
  // encoding.  The cycle also lets a later +dump-start window a small FST -- a full
  // VCD of this run would be terabytes.
  val cf_ill_cyc = RegInit(0.U(48.W))
  cf_ill_cyc := cf_ill_cyc + 1.U
  for (w <- 0 until coreWidth) {
    when (dec_xcpts(w)) {
      // [CFNOISE-COMMENTED] printf("[ILL] cyc=%d pc=0x%x inst=0x%x cause=%d\n",
             // [CFNOISE-COMMENTED] cf_ill_cyc, dec_uops(w).debug_pc, dec_uops(w).inst, dec_uops(w).exc_cause)
    }
  }

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

  // all decoders are empty and ready for new instructions.
  // With ram_valid gating in fetch-buffer.scala, padding slots in a partial fetch packet
  // are invalid (dec_valids(w)=false for unwritten entries).  The original dec_fire.last
  // check deadlocks whenever the last physical slot is a padding entry (dec_fire.last
  // stays false forever).  Instead, fire dec_ready when every valid slot has either fired
  // this cycle (dec_fire(w)) or was already dispatched in a prior cycle (dec_finished_mask(w)).
  val all_valid_dispatched = (0 until coreWidth).map(w =>
    !dec_valids(w) || dec_fire(w) || dec_finished_mask(w)
  ).reduce(_ && _)
  dec_ready := all_valid_dispatched && io.ifu.fetchpacket.valid

  when (dec_ready || io.ifu.redirect_flush) {
    dec_finished_mask := 0.U
  } .otherwise {
    dec_finished_mask := dec_fire.asUInt | dec_finished_mask
  }

  // STT: the root is produced at dispatch (C-TU-1); every earlier uop copy carries a constant 0 (pruned)
  for (w <- 0 until coreWidth) { dec_uops(w).stt_gr.foreach(_ := 0.U.asTypeOf(new SttRoot)) }

  // [Doppelganger] decode init: the doppelganger uop fields are 0 on every decoded uop (constant through rename: pruned).
  // The prediction is attached at dispatch (block (c)); the request identity only on the LSU's dg request leg.
  for (w <- 0 until coreWidth) {
    dec_uops(w).dg_req.foreach(_ := false.B);  dec_uops(w).dg_tag.foreach(_ := 0.U)
    dec_uops(w).dg_pv.foreach(_ := false.B);   dec_uops(w).dg_pa.foreach(_ := 0.U)
    dec_uops(w).dg_psec.foreach(_ := false.B); dec_uops(w).dg_patk.foreach(_ := false.B)
  }
  // [Doppelganger] the address predictor (paper Figure 5 A/B): looked up with every decoded load PC (up to coreWidth per cycle),
  // trained by committed loads (the LSU's commit loop, L13).
  val dgl_pred = if (ENABLE_DOPPELGANGER) Some(Module(new DglPredictor)) else None
  dgl_pred.foreach { pr =>
    pr.io.en  := RegNext(custom_csrs.doppelganger_en, false.B)
    pr.io.dbg := custom_csrs.cf_debug_lsu_enable && custom_csrs.cf_debug_enable      // same value core.scala:404 drives into the LSU
    for (w <- 0 until coreWidth) {
      pr.io.lk_pc(w) := dec_uops(w).debug_pc
      pr.io.lk_v(w)  := dec_valids(w) && dec_uops(w).uses_ldq && !dec_uops(w).exception &&
                        dec_uops(w).mem_cmd === dec_uops(w).M_XRD
    }
    pr.io.lk_adv := dis_ready && !io.ifu.redirect_flush        // the REN2 register loads this decode group at this edge
    pr.io.train  := io.lsu.dgl_train.get
    custom_csrs.doppelganger_stat_set(Cat(0.U(4.W), pr.io.drops, io.lsu.dgl_stat.get))
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
  //   spec_branch_secret_table(br_tag) = the branch's own resolved condition taint (cleared at allocation)
  // (S3 D-S12: the attacker-branch tables spec_branch_atk_table / spec_branch_op_table left with the record fields).
  // No rollback needed: stale entries are harmless because freed tags will be
  // overwritten before they appear in any future uop's br_mask.
  val spec_branch_secret_table = Reg(Vec(maxBrCount, Bool()))

  // [DISPAR] patch 51 (B1): the write enable no longer waits for dec_fire (dis_ready ->
  // dec_stalls -> dec_fire was the tail of the 47-level path).  is_branch(w) && !is_full(w) is
  // register-derived.  Record-identical: (1) dec_fire(w) && allocate_brtag implies the new
  // enable (dec_valids(w) implies !dec_finished_mask(w), dec_fire(w) implies !is_full(w)), so the
  // fire-cycle write is unchanged; (2) on any other cycle br_tag(w) is a FREE tag: tags are chosen
  // from ~branch_mask (the live set, a register) accumulated over is_branch, so it differs from
  // every other slot's tag and from every live branch's tag, and !is_full excludes the default
  // tag 0.  A free tag is in no live uop's br_mask (resolution and kill clear it everywhere the
  // cycle the register drops it), so no dispatch-time lookup (br_atk_bits/br_sec_bits) that can
  // fire reads it, and it is rewritten by this same write on the cycle it is allocated (the only
  // way it becomes live).  The dispatch clear and the writeback set below still follow in
  // last-connect order, as before.
  for (w <- 0 until coreWidth) {
    when (dec_brmask_logic.io.is_branch(w) && !dec_brmask_logic.io.is_full(w)) {
      val tag = dec_brmask_logic.io.br_tag(w)
      // FIX 1 (2026-09-03): do NOT seed this from the aggregate.  cf_secret_propagation
      // carries (a) the branch condition's own data taint, (b) BPD/BTB observability taint
      // from the fetch buffer, and (c) implicit taint the branch INHERITED from sitting in
      // another secret branch's shadow.  Term (c) makes this table self-reinforcing:
      // branch A secret -> its whole shadow gets s_prop -> the loop-back branch B is in
      // that shadow -> B is marked secret -> B's shadow inherits it -> forever.  MEASURED
      // on t18: 507 of 512 commits of `sd s0,40(sp)`, which spills a provably CLEAN s0,
      // carried spec_sec=1.  At decode cf_secret_propagation is not yet computed anyway
      // (see the D-BP note below), so seed from the data-access bit only.
      // FIX 1b (2026-09-03, replaces 1a): ALLOCATE CLEAN.  The table must be seeded
      // false and set true only by the branch's OWN resolved condition taint at
      // writeback (:2084, sec1 = wb.bits.secret).  Seeding from any dispatch-time
      // aggregate re-admits BPD/BTB observability taint, which is what sustained the
      // runaway: Fix 1a subtracted only the INHERITED implicit term and moved
      // spec_sec on clean stores just 507 -> 447 (-12%), while costing genuine signal
      // (BPD sec=1 entries 2262 -> 1572).  Clearing here also prevents a reused br_tag
      // from inheriting the previous branch's secrecy.
      spec_branch_secret_table(tag) := false.B
    }
  }
  // D-BP fix: cf_secret_propagation is false at decode; re-write spec_branch_secret_table at
  // dispatch when cf_secret_propagation is correctly computed (after rename/taint lookup).
  // [DISPAR] patch 51 (B2): dis_valids, not dis_fire (removes 20 of the 41 readers of each
  // dis_fire_w).  Record-identical: a valid branch at dispatch owns its (live) tag and has not
  // executed, so between its decode allocation (which already wrote false) and its dispatch fire
  // nothing can set the bit: the only set is the writeback set below, for the tag OWNER's own
  // writeback (after dispatch) or a stale JALR whose writeback is in its b1 resolution cycle,
  // before the tag is freed and reallocated.  A killed branch leaves dispatch on the
  // redirect_flush (b2) cycle, before its tag is reallocated.  So clearing on its stall cycles too
  // writes false over false; the fire-cycle clear is unchanged and the writeback set still wins.
  for (w <- 0 until coreWidth) {
    when (dis_valids(w) && dis_uops(w).allocate_brtag) {
      // FIX 2 (2026-09-03): seed at ALLOCATION from the PC-indexed BPD-secret signal.
      // Fix 1b allocated clean and let only writeback (:2103) set the bit -- correct in
      // principle but structurally useless: br_tag is per-INSTANCE and freed at
      // resolution, so the bit lands after its own shadow has dispatched and is wiped
      // before the next iteration.  MEASURED: spec_sec went to 0 globally (was 21176)
      // while the secret branch still carried 529 ty9 + 529 ty10 edges -- the branch was
      // recognised, its shadow was not.
      // cf_fetch_bpd_secret IS PC-indexed (the BPD row is), so iteration N's commit-time
      // row training makes iteration N+1 mark the branch at allocation, early enough for
      // its shadow, and it decays when the row stops being secret.  NOT the aggregate:
      // that is what caused the original runaway.
      // Reverted from cf_fetch_bpd_secret (Fix 2): that seeded from the PREDICTOR ROW's
      // secrecy -- an observability property true of every branch in a secret-touched
      // region -- and tagged ~97% of the function.  Allocation stays clean; the reactive
      // cf_ctrl_secret_ctx below carries control influence instead.
      spec_branch_secret_table(dis_uops(w).br_tag) := false.B
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
    // Task 3.3: only the INT stage carries FetchIftInfo (dispatch reads rename_stage.io.ren2_ift);
    // the FP and predicate copies are unread and firtool prunes their registers.
    if (rename eq rename_stage) rename.io.dec_ift := dec_ift else rename.io.dec_ift := DontCare

    rename.io.dis_fire := dis_fire
    rename.io.dis_ready := dis_ready

    rename.io.com_valids := rob.io.commit.valids
    rename.io.com_uops := rob.io.commit.uops
    rename.io.rbk_valids := rob.io.commit.rbk_valids
    rename.io.rollback := rob.io.commit.rollback
  }


  // Outputs
  dis_uops := rename_stage.io.ren2_uops
  // [Doppelganger] the prediction of REN2 lane w (looked up while this group was in decode) rides the dispatch uop into the LDQ
  // entry (the paper's "predicted bit" and predicted address in the load queue).
  dgl_pred.foreach { pr => for (w <- 0 until coreWidth) {
    dis_uops(w).dg_pv.get := pr.io.pred(w).v
    dis_uops(w).dg_pa.get := pr.io.pred(w).addr
    dis_uops(w).dg_psec.foreach(_ := pr.io.pred(w).sec)
    dis_uops(w).dg_patk.foreach(_ := pr.io.pred(w).atk)
  } }
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

    // IFT taint merge: OR FP taint fields from fp_rename_stage into dis_uops.

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
                                  (!rob.io.empty || !io.lsu.fencei_rdy || (dis_prior_slot_valid(w) && !cf_quiesce_core)))
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


  // Also assert fence_dmem during QS_DRAINING so dcache MSHRs drain and
  // io.dmem.ordered becomes true, allowing fencei_rdy (and thus
  // pipeline_drained_strict) to fire.  Without this, nothing dispatches in
  // QS_DRAINING → fence_dmem stays false → MSHRs never drain → deadlock.
  io.lsu.fence_dmem := (dis_valids zip wait_for_empty_pipeline).map {case (v,w) => v && w} .reduce(_||_) ||
                       qs_draining

  val dis_stalls = dis_hazards.scanLeft(false.B) ((s,h) => s || h).takeRight(coreWidth)
  dis_fire := dis_valids zip dis_stalls map {case (v,s) => v && !s}
  dis_ready := !dis_stalls.last

  // CSR index guard: only indices 0-2 are valid (0→N=4, 1→N=2, 2→N=1); index 3 has no option.
  assert(custom_csrs.cf_core_width_idx <= 2.U,
    "[core-width] cf_core_width_idx must be 0, 1, or 2; index 3 is reserved")

  // Width throttle invariant: dispatch must never fire more than cf_active_width instructions
  // per cycle. If sub_row_mask has a bug, this fires before results are silently wrong.
  assert(PopCount(dis_fire) <= custom_csrs.cf_active_width,
    "[dispatch] fired more instructions than configured active width")

  //-------------------------------------------------------------
  // LDQ/STQ Allocation Logic

  for (w <- 0 until coreWidth) {
    // Dispatching instructions request load/store queue entries when they can proceed.
    dis_uops(w).ldq_idx := io.lsu.dis_ldq_idx(w)
    dis_uops(w).stq_idx := io.lsu.dis_stq_idx(w)
  }

  // STT (stt-plan-v4.md C-CORE-1 (b)): runtime class gates, registered once (declared here, before their first use in (d))
  val stt_g = if (ENABLE_STT) Some(Wire(new SttGates)) else None
  stt_g.foreach { g =>
    g.exp := RegNext(custom_csrs.stt_gate_exp, false.B)
    g.br  := RegNext(custom_csrs.stt_gate_br,  false.B)
    g.sta := RegNext(custom_csrs.stt_gate_sta, false.B)
    g.dx  := RegNext(custom_csrs.stt_dx,       false.B)    // [v6] DelayExecute
    g.fut := RegNext(custom_csrs.stt_fut,      false.B)    // [v6]
  }
  // the taint unit on the dispatch group; its gate root replaces dis_uops(w).stt_gr (last connect after the decode copy)
  val stt_tu = if (ENABLE_STT) Some(Module(new SttTaintUnit)) else None
  stt_tu.foreach { tu =>
    for (w <- 0 until coreWidth) {
      val u = dis_uops(w); val q = tu.io.req(w)
      q.lrs1 := u.lrs1;  q.lrs2 := u.lrs2;  q.lrs3 := u.lrs3
      q.lrs1_rtype := u.lrs1_rtype;  q.lrs2_rtype := u.lrs2_rtype;  q.frs3_en := u.frs3_en
      q.ldst := u.ldst;  q.dst_rtype := u.dst_rtype;  q.ldst_val := u.ldst_val
      q.is_ld   := u.uses_ldq && !u.exception             // exactly the loads the LSU allocates (lsu.scala:622-625)
      q.is_mem  := u.uses_ldq || u.uses_stq
      q.ldq_idx := u.ldq_idx
      q.br      := u.allocate_brtag
      q.br_tag  := u.br_tag
      tu.io.dis_valids(w) := dis_valids(w)
      tu.io.dis_fire(w)   := dis_fire(w)
      dis_uops(w).stt_gr.get := tu.io.gr(w)
    }
    tu.io.b2_mispredict := brupdate.b2.mispredict
    tu.io.b2_br_tag     := brupdate.b2.uop.br_tag
    tu.io.flush         := RegNext(rob.io.flush.valid, false.B)
    tu.io.ldq_safe      := io.lsu.stt_ldq_safe.get
    tu.io.ldq_head      := io.lsu.stt_ldq_head.get
  }

  //-------------------------------------------------------------
  // corefuzzing: Set IFT fields at dispatch time
  // cf_domain_id: 1 if PC is in attacker address range (from CSR), 0 otherwise
  // cf_speculated: true if any branch is unresolved in the branch mask
  // IFT overhaul Phase 3, Task 3.3: everything the ROB's dispatch column needs goes into
  // dis_ift (IftDispatchInfo) instead of the uop.  Ruling 74: cf_spec_branch_is_secret stays
  // in the uop (the s_prop set below and the dispatch column read it from there).
  val dis_ift = Wire(Vec(coreWidth, new IftDispatchInfo))
  for (w <- 0 until coreWidth) {
    // cf_domain_id set at decode from debug_pc vs CSR range; propagates intact through rename regs.
    dis_ift(w).speculated := dis_uops(w).br_mask =/= 0.U

    val br_sec_bits = VecInit((0 until maxBrCount).map(i =>
      dis_uops(w).br_mask(i) && spec_branch_secret_table(i)))
    dis_uops(w).cf_spec_branch_is_secret.foreach(_ := br_sec_bits.reduce(_ || _))
  }

  // corefuzzing: Unified dispatch influencer computation.
  // Chains INFL_REG_DATAFLOW (taint), INFL_REG_PRESSURE (freelist), INFL_ROB_FULL, INFL_LDQ_FULL, INFL_STQ_FULL.
  // All additions use rename_stage.io.ren2_uops(w) as the cycle-free base to avoid
  // combinational loops that would arise from reading dis_uops(w) after writing it.
  val dis_stall_was_rob = (0 until coreWidth).map(w =>
    RegNext(dis_valids(w) && !dis_fire(w) && !rob.io.ready))
  // [STALLLANE] F5 (owner 2026-10-26, RULES C "Stall-edge fixes"; task-reports/regpressure-check-20261026.md):
  // LDQ_FULL / STQ_FULL (and REG_PRESSURE below) tag EVERY lane blocked behind the stalling lane, as ROB_FULL /
  // IQ_FULL (global ready) already do.  A lane's own term is unchanged (the [DISPAR] fire-based *_full_obs);
  // an OLDER lane j < w contributes its own dispatch hazard term exactly as dis_hazards reads it
  // (dis_valids(j) && *_full(j) && uses_*(j): if it holds, lane j cannot fire, so lane w is blocked behind it).
  // Levels: the older-lane OR (cfStallOlder) has no dis_fire term: ren2 FFs / the LSU's pre-fire count select,
  // in parallel with dis_fire; the RegNext D LUT gains one input (valid, !fire, own obs, own uses, older OR:
  // 5 <= 6), so 0 added levels on dis_fire and on every path from it.  0 FF; (coreWidth-1) small LUTs per queue.
  // IFT off: the older term is constant false (the RegNexts and their D logic are exactly as before).
  def cfStallOlder(t: Int => Bool, w: Int): Bool =
    if (ENABLE_IFT && w > 0) (0 until w).map(j => dis_valids(j) && t(j)).reduce(_||_) else false.B
  val dis_stall_was_ldq = (0 until coreWidth).map(w =>
    RegNext(dis_valids(w) && !dis_fire(w) && (io.lsu.ldq_full_obs(w) && dis_uops(w).uses_ldq ||
                                              cfStallOlder(j => io.lsu.ldq_full(j) && dis_uops(j).uses_ldq, w))))
  val dis_stall_was_stq = (0 until coreWidth).map(w =>
    RegNext(dis_valids(w) && !dis_fire(w) && (io.lsu.stq_full_obs(w) && dis_uops(w).uses_stq ||
                                              cfStallOlder(j => io.lsu.stq_full(j) && dis_uops(j).uses_stq, w))))
  // C5: capture whether the blocking queue head was secret-dependent at stall time.
  // Used to propagate s_prop to the stalled instruction.
  // corefuzzing: dispatch-stall DURATION for the two dominant structural stalls.
  // dis_stall_was_* above is a single-cycle RegNext, so it says "blocked" but not
  // "for how long".  Count linearly here -- only coreWidth instances, ~48 flops --
  // and compress to a 3-bit Log2Bucket at the dis_fire capture below, so the
  // encoder is not replicated into every uop.
  val dis_stall_rob_cycles = (0 until coreWidth).map { w =>
    val c = RegInit(0.U(6.W))
    when (dis_valids(w) && !dis_fire(w) && !rob.io.ready) {
      c := Mux(c === 63.U, 63.U, c + 1.U)
    } .elsewhen (dis_fire(w)) { c := 0.U }
    c
  }
  // [HEADREG] (Phase 4 Tasks 4.1/4.2) the ROB and LSU head-status outputs are registered at
  // their source under ENABLE_IFT, so io.lsu.*_head_* / rob.io.rob_head_* at cycle t carry
  // the head as it was at t-1.  The stall captures below then need no register of their own,
  // and are record-identical.  Proof: every capture is read only through a candidate (or the
  // C5 s_prop term) gated by dis_fire(w) && dis_stall_was_X(w), where dis_stall_was_X(w) =
  // RegNext(stall_X(w)) and the capture was RegEnable(head, stall_X(w)).  On such a cycle t,
  // stall_X(w) held at t-1, so the old capture = head(t-1) = the registered head at t; and
  // the old RegNext(stall_X(w) && head_is_secret) = dis_stall_was_X(w) && registered
  // head_is_secret.  When the gate is low the value is unused (addInfluencerBatchList reads
  // a candidate's fields only under its cond).  Saves the 4 x 2 x 18 capture FF and 3 x 4
  // "was secret" FF (all now plain wires).
  // [R124GATE3] patch 28 (B51): with IFT off these captures are constant 0 (no register): their
  // only readers are the IFT influencer candidates and the C5 s_prop set (an Option field).
  def stallCap[T <: Data](x: T, en: Bool): T = if (ENABLE_IFT) x else 0.U.asTypeOf(x)
  def stallSec(was: Bool, en_and_sec: Bool, sec: Bool): Bool =
    if (ENABLE_IFT) was && sec else false.B
  val dis_stall_was_rob_secret = (0 until coreWidth).map(w => stallSec(dis_stall_was_rob(w),
    dis_valids(w) && !dis_fire(w) && !rob.io.ready && rob.io.rob_head_is_secret, rob.io.rob_head_is_secret))
  val dis_stall_was_ldq_secret = (0 until coreWidth).map(w => stallSec(dis_stall_was_ldq(w),
    dis_valids(w) && !dis_fire(w) && io.lsu.ldq_full_obs(w) && dis_uops(w).uses_ldq && io.lsu.ldq_head_is_secret,
    io.lsu.ldq_head_is_secret))
  val dis_stall_was_stq_secret = (0 until coreWidth).map(w => stallSec(dis_stall_was_stq(w),
    dis_valids(w) && !dis_fire(w) && io.lsu.stq_full_obs(w) && dis_uops(w).uses_stq && io.lsu.stq_head_is_secret,
    io.lsu.stq_head_is_secret))
  // Capture the BLOCKING head's attributes during the stall cycle.
  // On the fire cycle, the queue head has already advanced; we need the stall-cycle head for correct influencer attribution.
  val dis_stall_rob_en = (0 until coreWidth).map(w => dis_valids(w) && !dis_fire(w) && !rob.io.ready)
  val dis_stall_ldq_en = (0 until coreWidth).map(w => dis_valids(w) && !dis_fire(w) && io.lsu.ldq_full_obs(w) && dis_uops(w).uses_ldq)
  val dis_stall_stq_en = (0 until coreWidth).map(w => dis_valids(w) && !dis_fire(w) && io.lsu.stq_full_obs(w) && dis_uops(w).uses_stq)
  val dis_stall_rob_head_op_count  = (0 until coreWidth).map(w => stallCap(rob.io.rob_head_op_count,  dis_stall_rob_en(w)))
  val dis_stall_rob_head_domain    = (0 until coreWidth).map(w => stallCap(rob.io.rob_head_domain,    dis_stall_rob_en(w)))
  val dis_stall_rob_head_is_secret = (0 until coreWidth).map(w => stallCap(rob.io.rob_head_is_secret, dis_stall_rob_en(w)))
  val dis_stall_ldq_head_op_count  = (0 until coreWidth).map(w => stallCap(io.lsu.ldq_head_op_count,  dis_stall_ldq_en(w)))
  val dis_stall_ldq_head_domain    = (0 until coreWidth).map(w => stallCap(io.lsu.ldq_head_domain,    dis_stall_ldq_en(w)))
  val dis_stall_ldq_head_is_secret = (0 until coreWidth).map(w => stallCap(io.lsu.ldq_head_is_secret, dis_stall_ldq_en(w)))
  val dis_stall_stq_head_op_count  = (0 until coreWidth).map(w => stallCap(io.lsu.stq_head_op_count,  dis_stall_stq_en(w)))
  val dis_stall_stq_head_domain    = (0 until coreWidth).map(w => stallCap(io.lsu.stq_head_domain,    dis_stall_stq_en(w)))
  val dis_stall_stq_head_is_secret = (0 until coreWidth).map(w => stallCap(io.lsu.stq_head_is_secret, dis_stall_stq_en(w)))
  // REG_PRESSURE: stall due to physical register freelist exhausted (INT or FP rename)
  // [STALLLANE] F5: every lane blocked behind a freelist-stalled OLDER lane gets the edge too (a rd=x0 / store /
  // branch lane, or a lane whose own pre-selected preg was valid, had none).  ren_stalls(j) is the dis_hazards
  // term (rename FFs + 1-2 LUT), shallower than dis_fire; same level argument and IFT-off rule as LDQ/STQ above.
  // reg_stuck_en's OR over lanes is unchanged: a younger lane is set only when an older valid lane j with
  // ren_stalls(j) did not fire, and that lane's own bit is set in the same cycle.
  val dis_stall_was_reg = (0 until coreWidth).map(w =>
    RegNext(dis_valids(w) && !dis_fire(w) && (ren_stalls(w) || cfStallOlder(j => ren_stalls(j), w))))
  // [MEMORDATK] 42b: the ROB head (cycle t-2) was a load that was not committing; read only by the
  // ROB_FULL / REG_PRESSURE is_atk term below (rob_head_ld_atk).  IFT off: constant 0, no flop.
  val rob_ld_head_q = if (ENABLE_IFT) RegNext(rob.io.com_load_is_at_rob_head, false.B) else false.B
  val dis_stall_reg_en = (0 until coreWidth).map(w => dis_valids(w) && !dis_fire(w) && ren_stalls(w))
  // [REGSTUCK] patch 52d (t83 m105, phase4/patches/p4-52d-author.md): REG_PRESSURE names the head
  // that was STUCK, not the [HEADREG] head of the cycle before the resume.  A ROB_FULL stall ends
  // the cycle after the blocker row commits (rob.io.ready), so headReg at the fire cycle is the
  // blocker.  A freelist stall ends 2-3 cycles after that commit (rename-freelist.scala: dealloc
  // at commit c -> free_list bit at c+1 -> r_sel/r_valid at c+2; one more cycle when the freed
  // pregs land on another lane's selector), by which time the head has moved on: the edge named
  // the row AFTER the blocker (t83 m105 oc SECH+3; t83reg/t83lat2 oc SECH+5, sec=0).
  // Capture register (shared by all lanes): it tracks headReg every cycle while no lane was in a
  // REG stall, and during a REG stall only on cycles whose head did NOT commit (rob_nocom_q), so
  // it holds the last head that actually blocked.  Alignment: at cycle k the headReg outputs, the
  // ROB's com_load_is_at_rob_head, the LSU's headReg'd ldq_head_atk, rob_nocom_q and
  // dis_stall_was_reg all describe cycle k-1.  Levels: D = existing FFs (one 2-input AND for the
  // 42b load term), CE = 1 LUT over 5 FFs; the candidate reads FFs exactly as before (is_atk LUT
  // input count unchanged).  ~21 FF.  IFT off: constant 0, no flop (stallCap convention).
  val rob_nocom_q = if (ENABLE_IFT) RegNext(!rob.io.commit.valids.reduce(_||_), false.B) else false.B
  // [REGREL] F2/F3 (owner 2026-10-26, RULES C "Stall-edge fixes"; task-reports/regpressure-check-20261026.md
  // section 3): during a REG stall the capture now takes the stuck head on its RELEASE, i.e. on the first commit
  // after a no-commit cycle (at cycle k: commit at k-1, none at k-2), not on every no-commit cycle.
  //   F2: the head's labels are read as of its COMMIT cycle (headReg at k = head(k-1)), so the s_prop / atk the
  //       head receives in its own writeback cycle (rob.scala cf_s_prop_rob_upd, sprob atk, writeback sticky_atk:
  //       written at w, commit at w+1) are included, as ROB_FULL sees them.  The old rule kept comb(w) of the
  //       head (its last no-commit cycle), before that write (e.g. `ld secret; div` head: sec=0).
  //   F3: a younger head that is still busy one cycle after the blocker commits (a dependent op) is no longer
  //       captured on the c+1 no-commit cycle, so a c+3 freelist relief still names the blocker, not that op.
  // The 42b load term moves to rob_ld_head_q (= com_load_is_at_rob_head one cycle earlier: head is a load and
  // nothing commits at k-2): the same row, which did not commit at k-2 and is the row that commits at k-1
  // (the head moves only on commit).  ldq_head_atk (headReg) at k is the LDQ head of k-1 = that load (the LDQ
  // head advances after its commit).
  // Levels / cost: +1 FF (rob_nocom_q2, D = the existing rob_nocom_q FF).  CE = 1 LUT over 6 FF (rob_nocom_q,
  // rob_nocom_q2, the coreWidth=4 dis_stall_was_reg FFs), still one LUT6; the D inputs are existing FFs / the
  // existing rob_ld_head_q FF AND headReg'd ldq_head_atk (one 2-input AND, was the same AND on
  // com_load_is_at_rob_head).  Not on dis_fire (capture registers feed only the candidate's data legs).
  // IFT off: rob_nocom_q / rob_nocom_q2 are constant false and stuckCap is constant 0 (no flop), as before.
  val rob_nocom_q2 = if (ENABLE_IFT) RegNext(rob_nocom_q, false.B) else false.B
  val reg_stuck_en = (!rob_nocom_q && rob_nocom_q2) || !dis_stall_was_reg.reduce(_||_)
  def stuckCap[T <: Data](x: T): T =
    if (ENABLE_IFT) RegEnable(x, 0.U.asTypeOf(x), reg_stuck_en) else 0.U.asTypeOf(x)
  val reg_stuck_oc     = stuckCap(rob.io.rob_head_op_count)
  val reg_stuck_dom    = stuckCap(rob.io.rob_head_domain)
  val reg_stuck_sec    = stuckCap(rob.io.rob_head_is_secret)
  val reg_stuck_valatk = stuckCap(rob.io.rob_head_valatk.getOrElse(false.B))
  val reg_stuck_ldatk  = stuckCap(rob_ld_head_q && io.lsu.ldq_head_atk.getOrElse(false.B))   // [REGREL] k-2 load term
  val dis_stall_reg_head_op_count  = (0 until coreWidth).map(w => reg_stuck_oc)
  val dis_stall_reg_head_domain    = (0 until coreWidth).map(w => reg_stuck_dom)
  val dis_stall_reg_head_is_secret = (0 until coreWidth).map(w => reg_stuck_sec)
  // [FTQFULL] patch 59 (was 57, rebased on live without 55; owner 2026-10-19 item D3 and the 16:1x
  // ruling "is_atk / is_secret from the blocker's FULL bits"; root cause scratch/secret-trace-r3/fivecfg/
  // portsmash/N1/REPORT.md Phase C).  While the FTQ is full, fetch stops (frontend f4 gate) and
  // the FTQ frees entries only when the ROB head commits (io.ifu.commit above), so a ROB head
  // with a secret-dependent latency starves DISPATCH: no uop reaches dispatch, no dispatch
  // candidate can fire, and the delay of the next dispatched uop carried no edge (portsmash
  // width 1: the secret div holds the head 61-64 of the 82-98 FTQ-full cycles; 4-cycle leak).
  // Edge: the FIRST uop dispatched after an FTQ-starved interval gets a ROB_FULL (ty 5) edge
  // naming the ROB-head blocker (op_count) with the blocker's FULL bits (rob.scala rob_head_full_*,
  // true head bank, [ROBHEAD] patch 58): is_secret = s_acc || s_prop || commit-time secret window,
  // is_atk = (attacker domain || sticky atk || commit-time attacker window || value taint || the 42b
  // attacker-address load head) and this uop is a victim.  Same type as ROB_FULL because the
  // blocker is the same structure (the ROB head that does not retire); it is merged INTO the ROB_FULL
  // candidate (no new candidate: the 16-candidate prefix network and the 4-slot pressure are
  // unchanged).  The record tells the two apart by stall_cycles_rob = 0 (no ROB stall).
  //   starved(k-1) = cf_ftq_full(k-1) && no dispatch-valid uop at k-1 (both registered);
  //   capture: the [HEADREG] head fields, on the first starved cycle of an interval and then on
  //            the interval's first head RELEASE ([FTQREL] below; was: every starved no-commit cycle);
  //   pending: set by a starved cycle, cleared by the first dispatch FIRE (state, kept on
  //            dis_fire as the [DISPAR] stall counters); the candidate reads it (registered, so
  //            the capture is valid) on every valid lane ([FTQLANE]; was the lowest valid lane only).  Fires only when the edge has a
  //            flag: the blocker is secret, or attacker-flagged and this uop a victim (no flagless
  //            edges: slot pressure).
  // Levels: every term is an FF or 1 LUT over FFs (the per-lane ftq_edge: ftq_pend, the two capture
  // FFs and d_victim, a ren2 register); the candidate cond gains (ftq_edge && no lower lane valid) on
  // the LUT that already ANDs dis_valids(w) (<= 6 inputs, 0 added); oc / is_* get a
  // 2:1 select by the dis_stall_was_rob FF on their DATA legs (the deep dis_valids cond and the
  // prefix network select are unchanged).  Cost: 16 + 4 capture + 3 control FF (+1 FF frontend),
  // ~16 LUT oc select + ~8 (+2 x coreWidth FF / 2 LUT full-bit head outputs in rob.scala).
  // IFT off: constants (no flop), cond term && ENABLE_IFT.B.
  val ftq_full_q   = io.ifu.cf_ftq_full.getOrElse(false.B)
  val dis_none_q   = if (ENABLE_IFT) RegNext(!dis_valids.reduce(_||_), false.B) else false.B
  val ftq_starve_q = ftq_full_q && dis_none_q
  val ftq_pend     = if (ENABLE_IFT) RegInit(false.B) else false.B
  val ftq_active   = ftq_pend || ftq_starve_q
  if (ENABLE_IFT) ftq_pend := ftq_active && !dis_fire.reduce(_||_)
  // [FTQREL] (owner 2026-10-26 14:1x, decision (a), RULES C "Stall-edge fixes"; the F2/F3 shape of
  // task-reports/regpressure-check-20261026.md in the FTQ capture).  The old enable (every starved cycle whose
  // head did not commit) kept the head's comb state of its last no-commit cycle, i.e. BEFORE its own writeback-cycle
  // s_prop / atk (F2: a `ld secret; div` head gave sec=0, and since the edge needs a flag, NO edge), and a dependent
  // op still busy after the blocker committed replaced it while the frontend refilled (F3).  New enable:
  //   (i)  the first starved cycle of an interval (!ftq_pend), as before: the head that starts the starvation;
  //   (ii) the FIRST release of the interval: a commit at k-1 after a no-commit cycle at k-2 (the [REGREL] rule,
  //        rob_nocom_q2 shared), read as of its commit cycle (writeback labels included).
  // Only the FIRST release is kept (ftq_rel_done): the starvation is ended by the commit that frees an FTQ entry,
  // which the frontend sees 2-3 cycles later and dispatch 4-6 cycles later; in that refill window a dependent
  // op of the same fetch packet (busy 2-3 cycles after the blocker's writeback) releases too, while ftq_starve_q
  // is still high (the owner's plain form `ftq_starve_q && ((!rob_nocom_q && rob_nocom_q2) || !ftq_pend)` would
  // name it, depending on the refill latency).  The first release is the head that was stuck when the starvation
  // set in.  Residual: if that head stalled only briefly and does not free an FTQ entry (same packet as a later
  // long op), the later long op is not named (README ftq-addon).  The release capture is not gated by
  // ftq_starve_q (it needs ftq_pend), so the FTQ-full drop cannot race it.
  // The 42b load term reads rob_ld_head_q on a release (the [REGREL] k-2 alignment), com_load_is_at_rob_head on the
  // first starved cycle (as before).
  // Cost: +1 FF (ftq_rel_done; rob_nocom_q2 shared with [REGREL]).  CE = 1 LUT6 over FFs (ftq_full_q, dis_none_q,
  // ftq_pend, rob_nocom_q, rob_nocom_q2, ftq_rel_done).  ftq_rel_done's D has the same form and depth as ftq_pend's
  // (one LUT after dis_fire into a flop, an existing endpoint shape).  The ldatk D gains one 2:1 select by the
  // ftq_pend FF over two FFs (same LUT).  0 levels on dis_fire.  IFT off: all constants, as before.
  val ftq_rel      = !rob_nocom_q && rob_nocom_q2
  val ftq_rel_done = if (ENABLE_IFT) RegInit(false.B) else false.B
  val ftq_cap_en   = (ftq_starve_q && !ftq_pend) || (ftq_pend && ftq_rel && !ftq_rel_done)
  if (ENABLE_IFT) ftq_rel_done := ftq_active && !dis_fire.reduce(_||_) && (ftq_rel_done || (ftq_pend && ftq_rel))
  def ftqCap[T <: Data](x: T): T =
    if (ENABLE_IFT) RegEnable(x, 0.U.asTypeOf(x), ftq_cap_en) else 0.U.asTypeOf(x)
  val ftq_cap_oc   = ftqCap(rob.io.rob_head_op_count)
  val ftq_cap_sec  = ftqCap(rob.io.rob_head_full_sec.getOrElse(false.B))
  val ftq_cap_atk  = ftqCap(rob.io.rob_head_full_atk.getOrElse(false.B) ||
                            (Mux(ftq_pend, rob_ld_head_q, rob.io.com_load_is_at_rob_head) && io.lsu.ldq_head_atk.getOrElse(false.B)))
  // fires from the cycle AFTER the first capture (ftq_pend = 1): the capture is then valid, and a single
  // starved cycle immediately followed by a dispatch (a pipeline bubble, not starvation) gives no edge.
  // The flag test is per lane (ftq_edge, in the dispatch loop: is_atk needs a victim uop).
  val ftq_hot      = ENABLE_IFT.B && ftq_pend
  // [FTQLANE] (owner 2026-10-26 14:1x, decision (b)): the FTQ_FULL edge tags EVERY valid lane of the first dispatch
  // group after the starvation (each of them was delayed by it), like ROB_FULL / IQ_FULL and F5 [STALLLANE]; the
  // old lowest-valid-lane select (ftq_first) is removed: -3 small LUT, one fewer input on the lane cond LUT.  A lane
  // that is valid but does not fire on that cycle (another hazard) keeps that hazard's own edge: ftq_pend clears on
  // the first fire as before ([DISPAR] state).
  // [IQFULL 2026-10-13, series-r step 16, owner ruling 110] issue queue full / dispatcher not
  // ready.  [IQHEAD] patch 49 (owner r175, "IQ full should follow the same"): attributed like
  // ROB_FULL / LDQ_FULL / STQ_FULL: the edge names the refusing IQ's STALLING occupant (its oldest
  // slot, registered in the issue unit: issue-unit-age-ordered.scala [IQHEAD]) with that op's
  // op_count, is_atk = its attacker domain || attacker influence (slot cf_src_atk) and is_secret =
  // its s_acc || s_prop || cf_src_secret || cf_pend_sec; it fires on EVERY IQ-refusal end (no
  // occupant precondition).  Which IQ: the BasicDispatcher stalls a lane when ANY IQ is not ready,
  // so per lane the IQ the stalled uop itself needs is named first (if it refused), else the first
  // refusing IQ in the order mem, int, FP (issue_units order).  Captured on the stall cycle (RegEnable, as
  // the old summaries were), read on the fire cycle: 0 levels on the dispatch path (the D side is
  // register -> ~3 LUT -> FF).  (uopIDCounterWidthCF + 2) FF per lane, replacing the 2 summary FF.
  // [RSTICKY] step 17 unchanged: sec=1 sets sticky s_prop (dis_sec_any), atk=1 sticky atk
  // (dis_promoted_atk).  IFT off: no capture (stallCap convention), the candidate is dead as before.
  val cf_iq_ready = (0 until coreWidth).map(w =>
    issue_units.map(_.io.dis_uops(w).ready) ++ (if (usingFPU) Seq(fp_pipeline.io.dis_uops(w).ready) else Nil))
  val cf_iq_type  = issue_units.map(_.iqType) ++ (if (usingFPU) Seq(IQT_FP.litValue) else Nil)
  val dis_stall_iq_now = (0 until coreWidth).map(w =>
    dis_valids(w) && !dis_fire(w) && !dispatcher.io.ren_uops(w).ready)
  val dis_stall_was_iq = (0 until coreWidth).map(w => RegNext(dis_stall_iq_now(w), false.B))
  val dis_stall_iq_head = (0 until coreWidth).map { w =>
    if (ENABLE_IFT) {
      val heads   = issue_units.map(_.io.cf_iq_head.get) ++ (if (usingFPU) Seq(fp_pipeline.io.cf_iq_head.get) else Nil)
      val refused = cf_iq_ready(w).map(r => !r)
      val own     = (refused zip cf_iq_type).map { case (r, t) =>
        r && (rename_stage.io.ren2_uops(w).iq_type & t.U(IQT_SZ.W)) =/= 0.U }
      RegEnable(PriorityMux(own ++ refused, heads ++ heads), dis_stall_iq_now(w))
    } else 0.U.asTypeOf(new IftIqHead)
  }
  // [FBHOLD] portsmash-fb-stall-20261103 (DESIGN.md in claude-artifacts/ift-overhaul/execution/diffs/portsmash-fb-stall-20261103;
  // RULES A reading 4 "An instruction stalled behind one whose record is secret (s_acc or s_prop) gets s_prop itself;
  // the attacker side works the same way", and RULES C "Stall-edge fixes" F5: every lane blocked behind the stall gets
  // the edge).  A flagged (sec or atk) REG_PRESSURE / ROB_FULL / IQ_FULL / LDQ_FULL / STQ_FULL stall edge reached only
  // the dispatch group stalled in that cycle; the uops waiting behind it UPSTREAM (fetch buffer, decode, rename 1) were
  // delayed by the same stall and got nothing (portsmash: 61 of 363 leaking FPGA runs, attacker probe with no edge).
  // Mechanism: one hold register {v, lim, key, code, sec, atk}.  Every cycle in which a dispatch lane carries a FLAGGED
  // stall edge (cap), lim := the fetch buffer's next op_count (io.ifu.cf_fb_next_oc), so the held set is exactly the
  // uops with op_count < lim that are not in the stalled group itself (all of them were upstream during a flagged
  // stall cycle; op_counts are assigned in fetch order and dispatch is in order).  A later dispatch lane w with
  // op_count older than lim (fbh_older) gets the held edge for each held flag its own stall edges of this cycle do
  // not already carry (fbh_own_sec / fbh_own_atk; review r1 F1: a lane with a weaker own flagged edge, e.g. an
  // attacker-domain uop whose only own flag is the raw atk of an attacker head, kept no held sec): the captured
  // key / code / labels, is_atk only on a victim consumer ([VICGATE] F1), never flagless.
  // v clears on the first dispatch group with no held uop (fbh_pass) and on any front-end flush (the held uops are
  // gone; refetched uops get new, younger op_counts, so the flush term is a safety net only).
  // Label: the first flagged edge in the order REG, ROB, IQ (lane order), LDQ, STQ.  While v is set a new flagged
  // stall replaces the label only when it carries every flag the held one has, or when the held set has just
  // drained (fbh_pass in the same cycle: the old label must not reach uops that were never behind it; review r1 F2)
  // (fbh_take); lim always extends.
  // Compare width fbhW: the held span (lim - op_count) is at most fetch buffer + 3 x coreWidth (decode, rename 1,
  // rename 2), and v clears within one group after the span, so fbhW = log2Ceil(span) + 2 low bits are exact.
  // Levels: every term is FF-sourced (ren2 op_count, the capture FFs, the dis_stall_was_* FFs, the [HEADREG] head
  // FFs): fbh_older = one fbhW-bit carry chain, fbh_own_sec / fbh_own_atk = 2 LUT; the candidate cond = 1-2 LUT after them
  // (<= 4 levels, as d_win_sec's CONTROL_FLOW cond), feeding only the prefix network (register endpoints).  No
  // dis_fire term anywhere (fbh_pass uses dis_valids), so 0 levels on the dispatch loop.
  // Cost (coreWidth 4, fetch buffer 64): 1 + 9 + 16 + 3 + 2 = 31 FF; ~4 x 9-bit compares, a (4 + coreWidth):1 x 21-bit
  // capture mux, the cap LUTs, and one more candidate per lane in the dispatch batch.  IFT off: all constants, the
  // candidate cond is constant false, the frontend port does not exist: Verilog unchanged.
  val fbhW = math.min(uopIDCounterWidthCF, log2Ceil(numFetchBufferEntries + 3 * coreWidth) + 2)
  def fbhCode(t: Int): UInt = { require((t >> 3) == (INFL_ROB_FULL >> 3), "[FBHOLD] stall codes share bits [5:3]"); (t & 7).U(3.W) }
  // consumer-independent attacker labels of the blockers (the candidates AND them with d_victim per lane)
  val fbh_reg_atk = reg_stuck_dom === 1.U || reg_stuck_ldatk || reg_stuck_valatk
  val fbh_rob_atk = dis_stall_rob_head_domain(0) === 1.U || (rob_ld_head_q && io.lsu.ldq_head_atk.getOrElse(false.B)) ||
                    rob.io.rob_head_valatk.getOrElse(false.B)
  val fbh_ldq_atk = dis_stall_ldq_head_domain(0) === 1.U || io.lsu.ldq_head_atk.getOrElse(false.B)
  val fbh_stq_atk = dis_stall_stq_head_domain(0) === 1.U || io.lsu.stq_head_atk.getOrElse(false.B)
  // per lane: (flagged cond, key, code, sec, atk) of the stall edges lane w carries this cycle (= the dispatch
  // candidates' conds and keys; the head fields of REG / ROB / LDQ / STQ are the same for every lane)
  val fbh_src = (0 until coreWidth).map { w => Seq(
    (dis_valids(w) && dis_stall_was_reg(w), reg_stuck_oc,                   fbhCode(INFL_REG_PRESSURE), reg_stuck_sec,                   fbh_reg_atk),
    (dis_valids(w) && dis_stall_was_rob(w), dis_stall_rob_head_op_count(0), fbhCode(INFL_ROB_FULL),     dis_stall_rob_head_is_secret(0), fbh_rob_atk),
    (dis_valids(w) && dis_stall_was_iq(w),  dis_stall_iq_head(w).oc,        fbhCode(INFL_IQ_FULL),      dis_stall_iq_head(w).sec,        dis_stall_iq_head(w).atk),
    (dis_valids(w) && dis_stall_was_ldq(w), dis_stall_ldq_head_op_count(0), fbhCode(INFL_LDQ_FULL),     dis_stall_ldq_head_is_secret(0), fbh_ldq_atk),
    (dis_valids(w) && dis_stall_was_stq(w), dis_stall_stq_head_op_count(0), fbhCode(INFL_STQ_FULL),     dis_stall_stq_head_is_secret(0), fbh_stq_atk))
    .map { case (c, k, t, s, a) => (c && (s || a) && !cf_dbg && ENABLE_IFT.B, k, t, s, a) } }
  // lane w's own stall edges of this cycle carry sec / raw atk (the candidate ANDs atk with d_victim, as F1 does)
  val fbh_own_sec  = fbh_src.map(_.map { case (c, _, _, s, _) => c && s }.reduce(_ || _))
  val fbh_own_atk  = fbh_src.map(_.map { case (c, _, _, _, a) => c && a }.reduce(_ || _))
  val fbh_cap      = fbh_src.flatten.map(_._1).reduce(_ || _)
  def fbhAny(i: Int): Bool = fbh_src.map(_(i)._1).reduce(_ || _)
  val fbh_order = Seq((fbhAny(0), fbh_src(0)(0)), (fbhAny(1), fbh_src(0)(1))) ++
                  (0 until coreWidth).map(w => (fbh_src(w)(2)._1, fbh_src(w)(2))) ++
                  Seq((fbhAny(3), fbh_src(0)(3)), (fbhAny(4), fbh_src(0)(4)))
  val fbh_new = PriorityMux(fbh_order.map(_._1), fbh_order.map { case (_, (_, k, t, s, a)) =>
                  Cat(a, s, t, k.pad(uopIDCounterWidthCF)(uopIDCounterWidthCF - 1, 0)) })
  val fbh_n_key = fbh_new(uopIDCounterWidthCF - 1, 0)
  val fbh_n_code = fbh_new(uopIDCounterWidthCF + 2, uopIDCounterWidthCF)
  val fbh_n_sec = fbh_new(uopIDCounterWidthCF + 3)
  val fbh_n_atk = fbh_new(uopIDCounterWidthCF + 4)
  val fbh_v    = if (ENABLE_IFT) RegInit(false.B) else false.B
  val fbh_lim  = if (ENABLE_IFT) RegInit(0.U(fbhW.W)) else 0.U(fbhW.W)
  val fbh_key  = if (ENABLE_IFT) RegInit(0.U(uopIDCounterWidthCF.W)) else 0.U(uopIDCounterWidthCF.W)
  val fbh_code = if (ENABLE_IFT) RegInit(0.U(3.W)) else 0.U(3.W)
  val fbh_sec  = if (ENABLE_IFT) RegInit(false.B) else false.B
  val fbh_atk  = if (ENABLE_IFT) RegInit(false.B) else false.B
  // lane w's uop is older than lim (modular, fbhW bits): it was upstream during a flagged stall cycle
  val fbh_older = (0 until coreWidth).map(w =>
    ((rename_stage.io.ren2_uops(w).cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)))(fbhW - 1, 0) - fbh_lim)(fbhW - 1))
  val fbh_pass = dis_valids.reduce(_ || _) && (0 until coreWidth).map(w => !dis_valids(w) || !fbh_older(w)).reduce(_ && _)
  val fbh_take = fbh_cap && (!fbh_v || fbh_pass || ((fbh_n_sec || !fbh_sec) && (fbh_n_atk || !fbh_atk)))
  if (ENABLE_IFT) {
    fbh_v := !io.ifu.redirect_flush && (fbh_cap || (fbh_v && !fbh_pass))
    when (fbh_cap)  { fbh_lim := (io.ifu.cf_fb_next_oc.get)(fbhW - 1, 0) }
    when (fbh_take) { fbh_key := fbh_n_key; fbh_code := fbh_n_code; fbh_sec := fbh_n_sec; fbh_atk := fbh_n_atk }
  }
  // [CFWIN] patch 31: raise CONTROL_FLOW (ty 20) for attacker-window members too (see the candidate).
  val cfCtrlFlowAtkWin = false
  // [R117SWEEP] patch 33: per lane, OR of is_secret over every firing dispatch candidate (below)
  val dis_sec_any = WireInit(VecInit(Seq.fill(coreWidth)(false.B)))
  // [IQHEAD] patch 49: per lane, the dispatch-time attacker seed for the IQ slot (below)
  val dis_iq_atk  = WireInit(VecInit(Seq.fill(coreWidth)(false.B)))
  for (w <- 0 until coreWidth) {
    val pre = rename_stage.io.ren2_uops(w)  // cycle-free base
    // [ARMRES] step 20, coverage (b): dispatched while the window is open (or in its arming
    // cycle).  Every uop dispatched after B resolved is younger than B (in-order dispatch).
    val d_win_sec = pre.cf_win_sec.getOrElse(false.B) || win_sec_now
    val d_win_atk = pre.cf_win_atk.getOrElse(false.B) || (win_atk_now && pre.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U)
    // [WINEXP] step 21 (owner r139, F3): the RESULT-taint seed is dispatch-time membership only,
    // stamped with the dispatch epoch.  A uop fetched in the window but dispatched after it
    // closed is younger than the closing branch: outside the window, so no seed (its fetch-time
    // bit still gives the record s_prop / atk through d_win_*, and [RASJAL] keeps d_win_sec).
    // While a window stays open both definitions agree.
    dis_uops(w).cf_win_sec.foreach(_ := win_sec_now)
    dis_uops(w).cf_win_atk.foreach(_ := win_atk_now && pre.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U)
    // [ONETAG] r205: the secret epoch has no reader left (regfile, LSQ, STL and D$ dropped the secret window kind);
    // a constant stamp lets firtool remove the 2-bit field from every uop register it rides in.
    dis_uops(w).cf_wep_sec.foreach(_ := 0.U)
    dis_uops(w).cf_wep_atk.foreach(_ := 0.U)   // [ONETAGATK] r205 U8: no attacker epoch reader left
    dis_win_sec_rec(w)     := d_win_sec
    // corefuzzing: REG_DATAFLOW is no longer raised here.  It was driven from the
    // rename-stage taint/producer tables, which are gone (taint-follows-data).  The
    // influencer is now raised at WRITEBACK, when the taint actually arrives with the
    // value: core.scala's cf_s_prop_rob_upd sets the ROB row's sprob pending bit and
    // rob.scala drains it into an INFL_REG_DATAFLOW entry.  Dispatch is too early to
    // know the taint, which is precisely why the table version needed speculation.

    // Fix 2: implicit flow via secret-dependent branch also sets s_prop.
    // cf_spec_branch_is_secret=true means an outstanding branch in br_mask was itself
    // s_acc or s_prop; this instruction's execution path encodes the secret outcome.
    // [IMPLWIN] step 20: cf_ctrl_secret_ctx removed; window membership gives s_prop instead.
    when (dis_uops(w).cf_spec_branch_is_secret.getOrElse(false.B) || d_win_sec) {
      dis_uops(w).cf_secret_propagation.foreach(_ := true.B)
    }
    // (no context-based attacker marking -- see the note at the brinfos loop)
    // FIX 2: the implicit tag now carries an EDGE.  Previously this set s_prop with no
    // influencer, so downstream could not tell control-dependence from observability
    // taint -- which is exactly why it could not be used to separate the true sink
    // store from the false spill.  INFL_CONTROL_FLOW is an OBSERVABILITY-class edge in
    // ift_semantics terms: no bits move, the path itself carries the secret.

    // Steps 1–5: inject dispatch-level influencers in parallel using addInfluencerBatchList.
    // All candidates are evaluated from the same base simultaneously.
    // Step 6 (INFL_ISSUE_CONTENTION) is injected at issue time via ROB update bus.
    // G-HOL fix: also fire when the queue head carries secret taint (same-domain case)
    // [HOLFIX 2026-09-08] Added `*_head_blocked`.  The old condition asked only whether a
    // cross-domain op was PRESENT at the queue head -- and asked it on dis_fire, which is
    // true exactly when this uop was NOT blocked.  So the edge fired on coexistence, never
    // on blocking, and the attacker's loads sit at the LDQ head continuously: control PC
    // 0x800016c4 came out 637/644 attacker-influenced with every edge ty3 MEM_HOL.
    // Being at the head means nothing unless the head is actually holding the line, so the
    // head must not yet have succeeded.  INFL_LDQ_FULL(6) remains the separate, structural
    // "could not allocate" channel -- this one is specifically "the head is stuck".
    // [MEMORDATK] patch 42 (r116/r117 attacker destination rule): a head whose ADDRESS is attacker-tainted
    // has attacker-chosen latency (the [I3BUSYATK] rule of the FU-busy edge, ruling 100 I-3), so it
    // also holds a VICTIM consumer as an attacker influence.  The head bit is value taint only (the
    // LSU's addr_is_atk / address window bits), never a sticky summary: these edges cannot chain.
    val d_victim     = pre.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U
    val ldq_head_atk = io.lsu.ldq_head_atk.getOrElse(false.B) && d_victim
    val stq_head_atk = io.lsu.stq_head_atk.getOrElse(false.B) && d_victim
    val dis_hol_ldq = dis_valids(w) && dis_uops(w).uses_ldq && io.lsu.ldq_head_blocked &&
                      (io.lsu.ldq_head_domain =/= dis_uops(w).cf_domain_id.getOrElse(0.U(iftTagWidth.W)) || io.lsu.ldq_head_is_secret || ldq_head_atk)
    val dis_hol_stq = dis_valids(w) && dis_uops(w).uses_stq && io.lsu.stq_head_blocked &&
                      (io.lsu.stq_head_domain =/= dis_uops(w).cf_domain_id.getOrElse(0.U(iftTagWidth.W)) || io.lsu.stq_head_is_secret || stq_head_atk)
    val dis_hol_any   = dis_hol_ldq || dis_hol_stq
    val hol_head_op   = Mux(dis_hol_ldq, io.lsu.ldq_head_op_count,      io.lsu.stq_head_op_count)
    // [VICGATE] F1 (owner 2026-10-26, RULES C "Stall-edge fixes"): the attacker-DOMAIN term of the stall edges
    // (REG_PRESSURE, ROB_FULL, LDQ_FULL, STQ_FULL, MEM_HOL) is gated to VICTIM consumers, as IQ_FULL / FTQ_FULL and
    // the record terms of the same edges already are: an attacker-domain consumer takes no atk=1 slot from them
    // (its own record / domain carries the attacker label; its register taint is domain-seeded, regfile.scala).
    // ldq_head_atk / stq_head_atk already carry d_victim, so (dom || x_atk) && d_victim = the same is_atk LUT
    // (d_victim was already one of its inputs): 0 FF, 0 LUT, 0 levels; is_atk feeds only the prefix network's
    // register endpoints (never dis_fire).  IFT off: d_victim is constant true (cf_domain_id absent), so the
    // expression is unchanged.  RECORD CHANGE (fewer atk=1 slots / sticky atk on attacker-domain records).
    val hol_is_atk    = Mux(dis_hol_ldq, io.lsu.ldq_head_domain === 1.U || ldq_head_atk, io.lsu.stq_head_domain === 1.U || stq_head_atk) && d_victim
    val hol_is_secret = Mux(dis_hol_ldq, io.lsu.ldq_head_is_secret,      io.lsu.stq_head_is_secret)

    // Task 3.3: the base list is built here in one batch on a zero base: the 8 fetch
    // candidates (from the FetchIftInfo flags carried through rename) first, then the
    // dispatch candidates -- the order the fetch broadcast and this batch used to give.
    val fi = rename_stage.io.ren2_ift(w)
    val fetchCands = fetchFlagCandidates(fi.fetch_flags, fi.any_is_victim, fi.btb_sec_pc)
    // [MEMORDATK] 42b (review p4-42 m1, r168d): a VICTIM load at an attacker-tainted address that
    // holds the ROB head (it is then also the LDQ head: older loads leave the LDQ at commit) gives
    // REG_PRESSURE / ROB_FULL is_atk.  Source: the existing ROB output com_load_is_at_rob_head =
    // RegNext(head is a load && nothing commits), read through ONE more core-side flop
    // (rob_ld_head_q, outside this loop; no new ROB port or path, r98).  Alignment: these stalls end
    // when the head row commits in cycle c and dispatch fires in c+1, where the candidates read the
    // [HEADREG] head of c.  At c+1 com_load_is_at_rob_head reflects c, when the head IS committing
    // (= 0); its RegNext reflects c-1, when that same head was a load not committing (the head moves
    // only on commit).  ldq_head_atk (headReg) at c+1 is the LDQ head of c = that load.  A non-load
    // ROB head is covered by [ROBHEADATK] patch 48 below (r173 (6); r168d's accepted under-taint
    // closed through the EXISTING sprob column, no new ROB write path).  1 FF, one 2-input AND
    // shared by both candidates, +1 is_atk input.
    val rob_head_ld_atk = rob_ld_head_q && ldq_head_atk
    // [ROBHEADATK] patch 48 (r173 (6), task-reports/p4-owner-gap-check.md C-rob): a VICTIM head
    // whose own RESULT is attacker value-tainted (a div / FP div / any op on attacker operands:
    // attacker-chosen latency) gives a victim consumer REG_PRESSURE / ROB_FULL is_atk.  Source: the
    // ROB output rob_head_valatk = headReg(the head row's C_sprob is_atk, bank 0), aligned exactly
    // like the head op_count / domain the candidates already read ([HEADREG], same row).  It does
    // NOT subsume the 42b load term above: a load at an attacker-tainted address that MISSED gets
    // cf_mem_dataflow_atk from the MSHR (cf_rp_vatk) and no sp_atk (its ty17 edge instead), and a
    // load to x0 writes no register; so both terms stay, ORed into the same is_atk bit of the same
    // candidate (one edge, never counted twice).  Levels: one FF + d_victim (FF) on the existing
    // is_atk LUT (5 -> 6 inputs); is_atk feeds only the prefix network's register inputs.
    val rob_head_val_atk = rob.io.rob_head_valatk.getOrElse(false.B) && d_victim
    // [FTQFULL] patch 59: this lane's FTQ-starvation edge has a flag (blocker secret, or attacker-flagged
    // with a victim uop).  1 LUT over FFs (ftq_pend, ftq_cap_sec, ftq_cap_atk, d_victim).
    val ftq_edge = ftq_hot && (ftq_cap_sec || (ftq_cap_atk && d_victim))
    // [DISPAR] patch 51 (B3): the dispatch candidates, MEM_HOL, the C5 s_prop set and the two
    // stall-duration fields are gated by dis_valids(w), not dis_fire(w).  Each is DATA of lane w's
    // enqueue (dis_ift(w), dis_uops(w).cf_*, dis_sec_any(w)) and lands only through
    // rob.io.enq_valids(w) / dispatcher valid / io.lsu.dis_uops(w).valid, all = dis_fire(w); on a
    // fire cycle dis_valids(w) = 1, so the landed values are bit-identical, and a non-firing lane's
    // values are discarded either way.  Kept on dis_fire (state, not enqueue data): the
    // dis_stall_* RegNexts / enables, the ROB-stall counter, the IQ-head RegEnable, the
    // ftq_taint_compactor RegNext and the sys_pc2epc RegNext.
    val dispCands0 = Seq(
      // [REGSTUCK] 52d: the is_atk terms come from the same stuck-head capture (d_victim stays live).
      // [VICGATE] F1: the domain term is gated to victim consumers with the rest (see hol_is_atk above).
      InfluencerCandidate(dis_valids(w) && dis_stall_was_reg(w), dis_stall_reg_head_op_count(w),  INFL_REG_PRESSURE, (dis_stall_reg_head_domain(w) === 1.U || reg_stuck_ldatk || reg_stuck_valatk) && d_victim,  dis_stall_reg_head_is_secret(w)),
      // [FTQFULL] patch 59: the FTQ-starvation edge rides this candidate (see above); a real ROB_FULL
      // stall of the same uop has priority (its blocker is the more recent one).
      // [RECV3] SP-07 (design 2.5): the FTQ edge has its own code INFL_FTQ_FULL (0x14; v1 ROB_FULL with srob = 0).  Still
      // one candidate (no prefix-network growth): the code is chosen by dis_stall_was_rob(w), the flop that already
      // chooses the key and labels (codes 0x10 / 0x14 differ in bit 2 only).  Both codes are OC-keyed.
      InfluencerCandidate(dis_valids(w) && (dis_stall_was_rob(w) || ftq_edge),   // [FTQLANE] every lane
                          Mux(dis_stall_was_rob(w), dis_stall_rob_head_op_count(w), ftq_cap_oc),  INFL_ROB_FULL,
                          Mux(dis_stall_was_rob(w), (dis_stall_rob_head_domain(w) === 1.U || rob_head_ld_atk || rob_head_val_atk) && d_victim,   // [VICGATE] F1
                              ftq_cap_atk && d_victim),
                          Mux(dis_stall_was_rob(w), dis_stall_rob_head_is_secret(w), ftq_cap_sec),
                          infl_type_dyn = Some(Mux(dis_stall_was_rob(w), INFL_ROB_FULL.U(inflTypeWidthCF.W),
                                                                         INFL_FTQ_FULL.U(inflTypeWidthCF.W)))),
      // [MEMORDATK] patch 42: is_atk also when the queue head's address is attacker-tainted and this uop is a victim
      // [VICGATE] F1: domain term gated to victim consumers (ldq_head_atk / stq_head_atk already are).
      InfluencerCandidate(dis_valids(w) && dis_stall_was_ldq(w), dis_stall_ldq_head_op_count(w),  INFL_LDQ_FULL,     (dis_stall_ldq_head_domain(w) === 1.U || ldq_head_atk) && d_victim,  dis_stall_ldq_head_is_secret(w)),
      InfluencerCandidate(dis_valids(w) && dis_stall_was_stq(w), dis_stall_stq_head_op_count(w),  INFL_STQ_FULL,     (dis_stall_stq_head_domain(w) === 1.U || stq_head_atk) && d_victim,  dis_stall_stq_head_is_secret(w)),
      // [IQHEAD] patch 49 (r175): fires on every IQ-refusal end and names the refusing IQ's stalling
      // occupant (oc); is_atk when that op is attacker-domain or attacker-influenced and this uop is
      // a victim (R-STICKY promotes it), is_secret from that op's secret state (r117: sticky s_prop).
      // IFT off the cond stays constant 0 as before (the summaries were constant 0): no IFT-off change.
      InfluencerCandidate(dis_valids(w) && dis_stall_was_iq(w) && ENABLE_IFT.B,
                          dis_stall_iq_head(w).oc, INFL_IQ_FULL,
                          dis_stall_iq_head(w).atk && pre.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U, dis_stall_iq_head(w).sec),
      // [FBHOLD] portsmash-fb-stall-20261103: the held-behind stall edge (see fbh_* above): a uop that was upstream
      // (fetch buffer / decode / rename 1) during a flagged dispatch stall gets that stall's edge (same code, key and
      // labels; is_atk only on a victim consumer, never flagless) when its own stall edges lack a held flag (r1 F1).
      // Placed after IQ_FULL: newer than the fetch candidates and this uop's own stall edges in the age-ordered list.
      InfluencerCandidate(dis_valids(w) && fbh_v && fbh_older(w) &&
                          ((fbh_sec && !fbh_own_sec(w)) || (fbh_atk && d_victim && !fbh_own_atk(w))) && ENABLE_IFT.B,
                          fbh_key, INFL_REG_PRESSURE, fbh_atk && d_victim, fbh_sec,
                          infl_type_dyn = Some(Cat((INFL_ROB_FULL >> 3).U((inflTypeWidthCF - 3).W), fbh_code))),
      InfluencerCandidate(dis_hol_any,                          hol_head_op,                     INFL_MEM_HOL,      hol_is_atk,                            hol_is_secret),
      // FIX 2: implicit flow gets its own edge.  Placed LAST: addInfluencerBatch appends
      // and drops into 4 slots, so position is priority, and this must never evict a
      // REG_DATAFLOW value edge.  oc=0 -- the producing branch's op_count is recoverable
      // in software from the logged pregs, so no packet bloat (same rule as REG_DATAFLOW).
      // is_atk must come from the PRE-batch value: dis_uops(w).cf_attacker_influence is
      // ASSIGNED from dis_promoted_atk below, so reading it here closes a combinational loop
      // (candidate -> batch -> the signal the candidate read).  [L11CFATK] lean59: is_atk is now the arming
      // branch's label (win_sec_lbl, brinfos-sourced, above), which has no path from the batch either.
      // [CFWIN] patch 31: the edge also fires for SECRET-WINDOW membership (d_win_sec, the same
      // term that sets s_prop above).  Step 20 removed cf_ctrl_secret_ctx and left this edge on
      // cf_spec_branch_is_secret alone, which is constant 0: its table is set only by an INT
      // writeback of a branch-tag uop, and a conditional branch never writes back (and a JALR's
      // writeback is the cycle its tag resolves).  Kept per r74/r118 Q13; it is not repaired, see
      // p4-31-author.md.  Levels: d_win_sec (ren2 FF OR win_sec_now, ~3 levels from registers) is
      // shallower than dis_fire, so the OR folds into the LUT that already ANDs dis_fire: +0 on the
      // dispatch path; the cond feeds only the prefix network, whose outputs are registers (D6).
      // Summary bits are unchanged: d_win_sec already sets s_prop, so the record gains the edge
      // only (r117 holds: sec=1 edge -> s_prop already set).
      // Attacker twin: the attacker window arms on an attacker-OWNED branch (GHR shadow, the
      // retired GHIST_STATE channel carried by abr), not on an attacker-conditioned one, so its
      // members are not control-dependent on attacker data.  Elaboration switch, default off
      // (zero hardware); turning it on adds d_win_atk to cond and is_atk (owner decision).
      InfluencerCandidate(dis_valids(w) && (dis_uops(w).cf_spec_branch_is_secret.getOrElse(false.B) || d_win_sec ||
                                          (if (cfCtrlFlowAtkWin) d_win_atk else false.B)),
                          0.U, INFL_CONTROL_FLOW,
                          win_sec_lbl || (if (cfCtrlFlowAtkWin) d_win_atk else false.B),   // [L11CFATK] lean59: the arming branch's record
                          if (cfCtrlFlowAtkWin) (dis_uops(w).cf_spec_branch_is_secret.getOrElse(false.B) || d_win_sec) else true.B),   // [CFWIN]
    )
    // [DBGFREEZE] patch 85 (b): a debug-mode uop takes no dispatch edge.  Its blockers are other debug uops or
    // program stores still draining from the STQ; their sec / atk must not reach it (the edge list, dis_promoted_atk,
    // dis_sec_any).  One more input on each condition LUT; the conditions feed only the prefix network, whose outputs
    // are registers (0 levels on dis_fire).  The fetch candidates are clean at the source (fetch-buffer.scala).
    val dispCands = dispCands0.map(c => c.copy(cond = c.cond && !cf_dbg))
    // Timing (plan Task 3.3 Step 3): the 16-candidate prefix network feeds only register
    // inputs (the C_disp LUTRAM write and the sticky atk flop), not the dispatch ready loop.
    val (dis_base, dis_promoted_atk) = addInfluencerBatchList(0.U.asTypeOf(new IftEdgeList),
      pre.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)), fetchCands ++ dispCands)

    dis_ift(w).base           := dis_base
    dis_ift(w).sftq           := fi.sftq   // [RECV3] SP-09: sftq as a field (was fu bits 3..1)
    // [ARMRES] a uop that joined the window at dispatch logs the live counter (else 0)
    dis_ift(w).atk_branch_ctr := Mux(fi.atk_branch_ctr =/= 0.U, fi.atk_branch_ctr,
                                     Mux(pre.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) === 0.U, win_atk_ctr, 0.U))
    dis_ift(w).sec_branch_ctr := Mux(fi.sec_branch_ctr =/= 0.U, fi.sec_branch_ctr, win_sec_ctr)
    dis_uops(w).cf_attacker_influence.foreach(_ := pre.cf_attacker_influence.getOrElse(false.B) || dis_promoted_atk || d_win_atk)
    // [IQHEAD] patch 49 (r174): the IQ slot's attacker seed is the uop's attacker influence WITHOUT the
    // dispatch-edge promotion dis_promoted_atk (that contains IQ_FULL atk itself: an IQ_FULL atk edge
    // would make the refused uop an attacker-influenced occupant, which names the next refused uop,
    // and so on: a chain).  Mirrors the secret side, whose slot s_prop excludes dis_sec_any.  Fetch-
    // time influence (domain-mismatch fetch flags, attacker window at fetch) || dispatch-time
    // attacker-window membership; both are rename-stage / window registers (no dis_fire term), so
    // this is shallower than the field it replaces on the dispatcher copy.
    // [M4RECSUM] lean58 (Fable audit M4, owner 2026-10-23 "stage it"; RULES A "Record summary for both", reading 4,
    // U8): the IQ seed now carries the FULL attacker record known at dispatch, dis_promoted_atk included (ROB_FULL /
    // LDQ_FULL / STQ_FULL / REG_PRESSURE / MEM_HOL / IQ_FULL atk edges, the attacker C5 twin), so the issue-time
    // edge producers (contention winner, IQ_FULL head, FU_BUSY) and the writeback tag see it, as the ROB/LSQ heads
    // already do ([RECEDGE]).  This deliberately re-opens the patch-49 chain (an IQ_FULL atk edge makes the refused
    // uop an attacker occupant, which names the next refused uop): the owner's record-summary rule, flood check
    // required.  Levels: dis_promoted_atk is a prefix-network output; it now also feeds the dispatcher's IQ copy
    // (slot registers) besides the ROB/sticky flops: register endpoints, 0 levels on dis_fire.  RECORD CHANGE.
    if (ENABLE_IFT) dis_iq_atk(w) := pre.cf_attacker_influence.getOrElse(false.B) || dis_promoted_atk || d_win_atk
    // [R117SWEEP] patch 33 (r117 + r93): the secret twin of dis_promoted_atk.  Every FIRING
    // candidate with is_secret=1 sets the row's sticky s_prop, whether or not the 4-slot list kept
    // it (the ROB used to OR is_secret over the SURVIVING base entries only, so a fetch ICACHE /
    // BTB-displacement / IQ_FULL / REG_PRESSURE / MEM_HOL sec edge pushed out by four newer
    // candidates lost its summary bit).  Summary only: it rides to the ROB on the enqueue uop's
    // cf_secret_propagation (rob.io.enq_uops below), not on dis_uops, so the issue-time wakeup
    // secret and every value-taint consumer are unchanged.  Levels: one AND per candidate + a
    // 15-input OR (2 LUT) on candidate conditions the batch already computes; it feeds only the
    // ROB sticky flop (enqueue write), never dis_fire / ready (0 levels on the dispatch loop).
    if (ENABLE_IFT) dis_sec_any(w) := (fetchCands ++ dispCands).map(c => c.cond && c.is_secret).reduce(_ || _)

    // C5: if the blocking queue head was secret-dependent, propagate s_prop to this instruction.
    // [DBGFREEZE] patch 85 (b): not in debug mode (a draining secret program store at the STQ head would otherwise
    // put s_prop on a debug store, whose STQ uop bit is the stq_head_is_secret seen by program uops after dret).
    when (dis_valids(w) && !cf_dbg && (dis_stall_was_rob_secret(w) || dis_stall_was_ldq_secret(w) || dis_stall_was_stq_secret(w))) {
      dis_uops(w).cf_secret_propagation.foreach(_ := true.B)
    }
    // [M4RECSUM] lean58 (M4, secret side): every firing dispatch/fetch edge with sec=1 (dis_sec_any: MEM_HOL,
    // REG_PRESSURE, IQ_FULL, I$/BTB-displacement fetch flags, ...) now sets the uop's s_prop on dis_uops itself, so
    // the IQ slot, the issue-time edge producers, the LSU copy and the writeback tag see it (it reached only the ROB
    // sticky bit before; registers got it through the commit-time late set).  No candidate reads
    // dis_uops.cf_secret_propagation (no loop); every reader of dis_uops is a register.  RECORD CHANGE.
    if (ENABLE_IFT) { when (dis_sec_any(w)) { dis_uops(w).cf_secret_propagation.foreach(_ := true.B) } }
    // Duration of the dispatch stall behind the INFL_ROB_FULL influencer above.
    // Read on the fire cycle: the counter\'s clear is registered, so it still holds
    // the accumulated count this cycle.
    dis_ift(w).stall_cycles_rob := Mux(dis_valids(w) && dis_stall_was_rob(w), Log2Bucket(dis_stall_rob_cycles(w)), 0.U)
    // Head-of-line duration: how long a hostile store had already held the STQ head when
    // this uop dispatched.  Applies to loads as well as stores -- a dispatching load takes
    // st_dep_mask := next_live_store_mask, so it is ordered behind every live store, and
    // those bits clear only as stq_head advances.  The domain test lives here because the
    // LSU-side counter measures residency only and cannot see the consumer's domain.
    val stq_head_hostile = io.lsu.stq_head_valid &&
                           (io.lsu.stq_head_domain =/= dis_uops(w).cf_domain_id.getOrElse(0.U(iftTagWidth.W)) ||
                            io.lsu.stq_head_is_secret || stq_head_atk)   // [MEMORDATK] (42b m2: the MEM_HOL term above, r91)
    dis_ift(w).stall_cycles_stq := Mux(dis_valids(w) && stq_head_hostile, Log2Bucket(io.lsu.stq_hol_cycles), 0.U)
  }

  //-------------------------------------------------------------
  // Rob Allocation Logic

  rob.io.enq_valids := dis_fire
  rob.io.enq_uops   := dis_uops
  for (w <- 0 until coreWidth) { rob.io.enq_uops(w).stt_gr.foreach(_ := 0.U.asTypeOf(new SttRoot)) }   // STT: the ROB keeps none
  // [R117SWEEP] patch 33: the ROB reads the enqueue uop's cf_secret_propagation only as the row's
  // sticky s_prop seed (rob.scala; the rob_uop copy is zeroed and the exception-uop copy is never
  // read), so the dispatch-edge secret summary rides here: no new ROB wire (r98).
  if (ENABLE_IFT) {
    for (w <- 0 until coreWidth) {
      // [M4RECSUM] lean58: dis_uops already carries dis_sec_any; the OR is kept (idempotent) so this site reads as before
      rob.io.enq_uops(w).cf_secret_propagation.foreach(_ := dis_uops(w).cf_secret_propagation.getOrElse(false.B) || dis_sec_any(w))
      // [RECV3] SP-09 (design 3.2 bits 0-1, 3.3 item 1): the issue queue this uop is dispatched to, on the ROB enqueue copy
      // only (it lands in the sidecar dispatch column, as the v1 ROB bit did; the IQ / LSU / RoCC copies keep dis_uops'
      // bitmap, 0 at dispatch in v3).  iq_type bit 0 = IQT_INT, bit 1 = IQT_MEM (IQT_MFP = 6 sets MEM).  0 FF; one AND-OR
      // per bit on rename-register bits into the C_disp write data.
      rob.io.enq_uops(w).cf_fu_bitmap.foreach(_ := dis_uops(w).cf_fu_bitmap.get |
        Mux(dis_uops(w).iq_type(0), (1 << UNIT_INT_IQ).U(numModules.W), 0.U(numModules.W)) |
        Mux(dis_uops(w).iq_type(1), (1 << UNIT_MEM_IQ).U(numModules.W), 0.U(numModules.W)))
    }
  }
  rob.io.enq_ift    := dis_ift
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
    // [IQHEAD] patch 49: the IQs' copy carries the chain-free seed (the slot used to zero this field on
    // load, so it had no reader; the ROB and the LSU read dis_uops(w) directly and are unchanged)
    dispatcher.io.ren_uops(w).bits.cf_attacker_influence.foreach(_ := dis_iq_atk(w))
  }

  // STT C-CORE-1 (d) [v6]: the dispatch-time hold bit of one IQ copy. STT (bit 5 = 0): the gate root's t AND the class.
  // DelayExecute (bit 5 = 1, A13): the class AND "not yet at its own VP" (4.0b); the root is not consulted. own_vp at
  // dispatch: no older unresolved branch (the dispatching br_mask; a tag resolving this cycle is cleared by the slot one
  // cycle later) and, Futuristic [v7, P-F2], the PNR is at the uop (rob_idx == rob_pnr_idx: every older instruction is past
  // the PNR) and no load has an order failure pending (every load in the LDQ is older than a dispatching uop). Never into
  // an empty ROB: the PNR register is not updated while the ROB is empty (rob.scala:1882-1899); after an exception
  // rollback it can name bank b of the head row while lanes < b of the new group are older; the slot clears t a cycle later. [v8: stt-v7-review D1]
  // With g.dx = 1, g.br = 0 (C-CSR-1), so tx never names a branch or JALR.
  def sttHoldAtDis(d: MicroOp, tx: Bool): Bool = {
    val g      = stt_g.get
    val own_vp = d.br_mask === 0.U && (!g.fut || (d.rob_idx === rob.io.rob_pnr_idx && !rob.io.empty && !io.lsu.stt_fail_rob.get.valid))  // [v8: stt-v7-review D1]
    Mux(g.dx, tx && !own_vp, d.stt_grv.t && tx)
  }

  var iu_idx = 0
  // Send dispatched uops to correct issue queues
  // Backpressure through dispatcher if necessary
  for (i <- 0 until issueParams.size) {
    if (issueParams(i).iqType == IQT_FP.litValue) {
       fp_pipeline.io.dis_uops <> dispatcher.io.dis_uops(i)
       // STT C-CORE-1 (d): in the FP IQ only FDIV/FSQRT transmit; an FP store's data half (uopSTA) does not
       stt_g.foreach { g =>
         for (w <- 0 until issueParams(i).dispatchWidth) {
           val d  = dispatcher.io.dis_uops(i)(w).bits
           val tx = g.exp && d.fu_code_is(FU_FDV)
           fp_pipeline.io.dis_uops(w).bits.stt_gr.get.t := sttHoldAtDis(d, tx)          // [v6]
         }
       }
    } else {
       issue_units(iu_idx).io.dis_uops <> dispatcher.io.dis_uops(i)
       // STT C-CORE-1 (d): MEM IQ = loads / LR / AMO address (explicit) and store address generation (owner STT-4);
       // INT IQ = conditional branches and JALR/RET (implicit channels) and DIV/REM (variable latency, A7)
       stt_g.foreach { g =>
         for (w <- 0 until issueParams(i).dispatchWidth) {
           val d  = dispatcher.io.dis_uops(i)(w).bits
           val tx = if (issueParams(i).iqType == IQT_MEM.litValue)
                      ((d.uses_ldq || d.is_amo) && g.exp) || (d.uses_stq && !d.is_amo && !d.is_fence && g.sta)
                    else
                      (d.allocate_brtag && g.br) || (d.fu_code_is(FU_DIV) && g.exp)
           issue_units(iu_idx).io.dis_uops(w).bits.stt_gr.get.t := sttHoldAtDis(d, tx)  // [v6]
         }
       }
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
  val iss_wu_fast = scala.collection.mutable.Set[Int]()   // [IQHEAD] patch 49: fast (issue-time) wakeup ports
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
      // [WAKESEC] The result's secret is unknown at issue; the issuing uop's own aggregate is
      // the earliest conservative estimate (it already includes cf_src_secret from its
      // wakeups).  The exact value arrives with the slow wakeup and at register read
      // (D11, bug 2).
      fast_wakeup.bits.secret := iss_uops(i).cf_secret_propagation.getOrElse(false.B) || iss_uops(i).cf_secret_access.getOrElse(false.B)
      // [IQHEAD] patch 49: the attacker bit rides WRITEBACK wakeups only (r174); the fast wakeup's is
      // constant 0 (its operand taint_atk is unknown at issue), so the slots' fast-port terms fold away.
      if (ENABLE_IFT) fast_wakeup.bits.taint_atk := false.B
      fast_wakeup.valid    := iss_valids(i) &&
                              iss_uops(i).bypassable &&
                              iss_uops(i).dst_rtype === RT_FIX &&
                              iss_uops(i).ldst_val &&
                              !(io.lsu.ld_miss && (iss_uops(i).iw_p1_poisoned || iss_uops(i).iw_p2_poisoned))

      // Slow Wakeup (uses write-port to register file)
      slow_wakeup.bits.uop := resp.bits.uop
      slow_wakeup.bits.secret := resp.bits.secret   // [WAKESEC] (D11, bug 2)
      if (ENABLE_IFT) slow_wakeup.bits.taint_atk := resp.bits.taint_atk   // [IQHEAD] patch 49
      slow_wakeup.valid    := resp.valid &&
                                resp.bits.uop.rf_wen &&
                                !resp.bits.uop.bypassable &&
                                resp.bits.uop.dst_rtype === RT_FIX

      if (exe_units(i).bypassable) {
        int_iss_wakeups(iss_wu_idx) := fast_wakeup
        iss_wu_fast += iss_wu_idx
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
  pred_wakeup.bits.secret := iss_uops(jmp_unit_idx).cf_secret_propagation.getOrElse(false.B)
  pred_wakeup.bits.taint_atk := iss_uops(jmp_unit_idx).cf_attacker_influence.getOrElse(false.B)
  pred_wakeup.bits.predicated := DontCare
  pred_wakeup.bits.edges := 0.U.asTypeOf(new IftEdgeList)   // Task 3.2: no edges on a predicate wakeup

  // Perform load-hit speculative wakeup through a special port (performs a poison wake-up).
  // [WAKESEC] the bundle now carries {pdst, secret} (Phase 0 finding (b)).
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
  // [V3PAUSE] SP-20: pause_mem (declared above) into the mem IQ's LOAD_PAUSE deny path; false for the int IQ.  IFT only.
  issue_units.foreach(_.io.cf_pause_mem.foreach(_ := false.B))
  mem_iss_unit.io.cf_pause_mem.foreach(_ := pause_mem)
  issue_units.map(_.io.brupdate := brupdate)
  issue_units.map(_.io.flush_pipeline := RegNext(rob.io.flush.valid))

  // STT C-CORE-1 (e): VP flags and the debug enable to the IQs and the FP pipeline; model / implicit-rule bits to the LSU
  issue_units.foreach { iu =>
    iu.io.stt_ldq_safe.foreach(_ := io.lsu.stt_ldq_safe.get)
    iu.io.stt_dbg.foreach(_ := RegNext(custom_csrs.stt_dbg, false.B))
    iu.io.stt_dx.foreach(_ := stt_g.get.dx)                     // [v6] DelayExecute (registered in (b))
    iu.io.stt_fut.foreach(_ := stt_g.get.fut)                   // [v6]
    iu.io.stt_rob_head.foreach(_ := rob.io.rob_head_idx)        // [v6] ROB registers (Cat(rob_head, rob_head_lsb))
    iu.io.stt_rob_pnr.foreach(_ := rob.io.rob_pnr_idx)          // [v7] P-F2
    iu.io.stt_fail_rob.foreach(_ := io.lsu.stt_fail_rob.get)    // [v7] P-F2
  }
  if (usingFPU) {
    fp_pipeline.io.stt_ldq_safe.foreach(_ := io.lsu.stt_ldq_safe.get)
    fp_pipeline.io.stt_dbg.foreach(_ := RegNext(custom_csrs.stt_dbg, false.B))
    fp_pipeline.io.stt_dx.foreach(_ := stt_g.get.dx)            // [v6]
    fp_pipeline.io.stt_fut.foreach(_ := stt_g.get.fut)          // [v6]
    fp_pipeline.io.stt_rob_head.foreach(_ := rob.io.rob_head_idx)   // [v6]
    fp_pipeline.io.stt_rob_pnr.foreach(_ := rob.io.rob_pnr_idx)     // [v7] P-F2
    fp_pipeline.io.stt_fail_rob.foreach(_ := io.lsu.stt_fail_rob.get)   // [v7] P-F2
  }
  io.lsu.stt_fut.foreach(_  := RegNext(custom_csrs.stt_fut,  false.B))
  io.lsu.stt_impl.foreach(_ := RegNext(custom_csrs.stt_impl, false.B))
  io.lsu.stt_dbg.foreach(_  := RegNext(custom_csrs.stt_dbg,  false.B))
  rob.io.stt_noret.foreach(_ := io.lsu.stt_noret.get)                     // [amend-T1s] C-ROB-1 retire guard (R1s-a B2)
  if (ENABLE_STT) {
    val stt_c_blk = RegInit(0.U(32.W)); val stt_c_stl = RegInit(0.U(32.W))
    def sat(c: UInt, e: Bool) = Mux(e && c =/= ~0.U(32.W), c + 1.U, c)
    val any_blk = (issue_units.map(_.io.stt_any_blk.get) ++
                   (if (usingFPU) Seq(fp_pipeline.io.stt_any_blk.get) else Nil)).reduce(_ || _)
    stt_c_blk := sat(stt_c_blk, any_blk)
    stt_c_stl := sat(stt_c_stl, io.lsu.stt_stl_blk.get)
    custom_csrs.stt_stat_set(Cat(stt_c_stl, stt_c_blk))
  }

  // Load-hit Misspeculations
  require (mem_iss_unit.issueWidth <= 2)
  issue_units.map(_.io.ld_miss := io.lsu.ld_miss)

  // O1: aggregate non-pipelined FU occupancy taint to the issue units.  Only div and
  // fdivsqrt ever assert cf_fu_busy (pipelined units leave it invalid by construction),
  // so this is a small OR, not a per-port network.
  {
    val busy_units = (0 until exe_units.length).map(exe_units(_)).filter(u => u.hasDiv || u.hasDiv2 || u.hasFdiv)
    val any_busy   = if (busy_units.isEmpty) false.B
                     else busy_units.map(_.io.cf_fu_busy.valid).reduce(_||_)
    val busy_sel   = if (busy_units.isEmpty) 0.U
                     else PriorityEncoderOH(VecInit(busy_units.map(_.io.cf_fu_busy.valid)).asUInt)
    for (iu <- issue_units) {
      iu.io.cf_fu_busy.valid := any_busy
      if (busy_units.isEmpty) {
        iu.io.cf_fu_busy.bits := DontCare
        iu.io.cf_fu_busy.bits.is_atk := false.B
        iu.io.cf_fu_busy.bits.is_sec := false.B
        iu.io.cf_fu_busy.bits.fu_code := 0.U   // [I2BUSYFU]
        iu.io.cf_fu_busy.bits.multi   := false.B   // [DIV2]
        iu.io.cf_fu_busy.bits.any_atk := false.B
        iu.io.cf_fu_busy.bits.any_sec := false.B
      } else {
        // [I2BUSYFU] (ruling 100, I-2) the FU class mask of the units that are busy
        iu.io.cf_fu_busy.bits.fu_code  := busy_units.map(u => Mux(u.io.cf_fu_busy.valid,
          (if (u.hasDiv || u.hasDiv2) FU_DIV else 0.U(FUC_SZ.W)) | (if (u.hasFdiv) FU_FDV else 0.U(FUC_SZ.W)), 0.U(FUC_SZ.W))).reduce(_|_)
        iu.io.cf_fu_busy.bits.op_count := Mux1H(busy_sel, busy_units.map(_.io.cf_fu_busy.bits.op_count))
        iu.io.cf_fu_busy.bits.is_atk   := Mux1H(busy_sel, busy_units.map(_.io.cf_fu_busy.bits.is_atk))
        iu.io.cf_fu_busy.bits.is_sec   := Mux1H(busy_sel, busy_units.map(_.io.cf_fu_busy.bits.is_sec))
        // [DIV2] (owner 2026-11-03, no age compare) with two dividers the winner key above names the LOWER-INDEX busy
        // occupant (PriorityEncoderOH: unit 2 before unit 3); `multi` says a second occupant exists, and any_atk/any_sec
        // OR the labels over both.  One busy divider: multi = 0, any_* = the single occupant's label (today's values).
        if (busy_units.length > 1) {
          iu.io.cf_fu_busy.bits.multi   := PopCount(busy_units.map(_.io.cf_fu_busy.valid)) > 1.U
          iu.io.cf_fu_busy.bits.any_atk := busy_units.map(u => u.io.cf_fu_busy.valid && u.io.cf_fu_busy.bits.is_atk).reduce(_||_)
          iu.io.cf_fu_busy.bits.any_sec := busy_units.map(u => u.io.cf_fu_busy.valid && u.io.cf_fu_busy.bits.is_sec).reduce(_||_)
        } else {
          iu.io.cf_fu_busy.bits.multi   := false.B
          iu.io.cf_fu_busy.bits.any_atk := Mux1H(busy_sel, busy_units.map(_.io.cf_fu_busy.bits.is_atk))
          iu.io.cf_fu_busy.bits.any_sec := Mux1H(busy_sel, busy_units.map(_.io.cf_fu_busy.bits.is_sec))
        }
      }
    }
  }

  mem_units.map(u => u.io.com_exception := RegNext(rob.io.flush.valid))

  // Wakeup (Issue & Writeback)
  for {
    iu <- issue_units
    ((issport, wakeup), wi) <- (iu.io.wakeup_ports zip int_iss_wakeups).zipWithIndex
  }{
    issport.valid := wakeup.valid
    issport.bits.pdst := wakeup.bits.uop.pdst
    issport.bits.poisoned := wakeup.bits.uop.iw_p1_poisoned || wakeup.bits.uop.iw_p2_poisoned
    issport.bits.secret   := wakeup.bits.secret
    // [IQHEAD] patch 49 (r174, late-arriving attacker-tainted source): on a WRITEBACK wakeup (LL / load
    // / slow unit) the int regfile write's taint_atk seed (taint_atk || attacker-domain producer); 0 on
    // a fast wakeup (scope r174: writeback wakeup; residual in p4-49-author.md).  Read only by cf_src_atk.
    issport.bits.atk.foreach(_ := (if (iss_wu_fast.contains(wi)) false.B else
                                   wakeup.bits.taint_atk || wakeup.bits.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= 0.U))

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

  // [DBGFREEZE] patch 85 (b)+(c): register-read masks every operand taint of a debug-mode uop, and the program's
  // x8 / x9 taint is carried across the halt.  The debug ROM saves s0 in dscratch0 and restores it; fesvr / OpenOCD
  // save s0 and s1 through abstract commands (`sw sN, data0`), use them, and restore them from the host
  // (`lw sN, data0`, a DM MMIO load).  Taint does not survive those round trips, so the program would resume with
  // clean s0/s1 (an under-taint: the restored VALUE is the program's).  Per k in {x8, x9}:
  //   dvP(k)  the preg that holds the program's value of x(8+k): latched from the int map table
  //           (rename_stage.cf_map89) while the hart is in debug mode and no debug uop has dispatched yet (the
  //           committed map after the entry rollback), and again on the dret fall (the final restore's preg: the map
  //           is committed then, dret is unique and nothing younger has dispatched);
  //   dvA(k)  the program's taint of that register {watk, wsec, atk, sec}: in debug mode, OR of every debug read of
  //           dvP(k) (register-read cf_dbg_cap: the first save read of a value-restoring debugger, which must read
  //           the register before it can restore it), zeroed at entry unless still live from the previous halt;
  //   dvL(k)  dvP(k) is still the arch mapping: cleared when an x(8+k) writer commits (that commit frees dvP(k)).
  // After dret, register-read ORs dvA(k) into every operand read of dvP(k) while dvL(k) ([DBGVT]): the restored
  // register reads exactly as tainted as the one the halt interrupted, until the program overwrites it.  A window
  // close clears the window bits of dvA (the regfile marks flash-clear on the same edge).  Other registers are not
  // touched by these debuggers; a debugger WRITE of a register it did not read first is a harness value (clean).
  // Cost: 2 x (8 + 4 + 1) FF + 1 FF (dsp_seen); about 25 LUT here; register-read about 150 LUT (2 x 8-bit preg
  // compares per operand, shared by the capture and the apply).  IFT off: no ports, nothing elaborated.
  if (ENABLE_IFT) {
    iregister_read.io.cf_dbg.get := cf_dbg
    val map89    = rename_stage.cf_map89.get
    val dsp_seen = RegInit(false.B)
    when (!cf_dbg) { dsp_seen := false.B } .elsewhen (dis_fire.reduce(_||_)) { dsp_seen := true.B }
    val dbg_rise = cf_dbg && !cf_dbg_q
    val dbg_fall = !cf_dbg && cf_dbg_q
    val dvP = Reg(Vec(2, UInt(ipregSz.W)))
    val dvA = RegInit(VecInit(Seq.fill(2)(0.U(4.W))))
    val dvL = RegInit(VecInit(Seq.fill(2)(false.B)))
    // [ONETAG] [ONETAGATK] r205: no window kind and no close clear; the carried {watk, wsec} bits are 0 and stay 0
    val wclr = Fill(4, 1.U(1.W))   // {watk, wsec, atk, sec}
    for (k <- 0 until 2) {
      val wr = (0 until coreWidth).map { w =>
        val cu = rob.io.commit.uops(w)
        rob.io.commit.valids(w) && cu.ldst_val && cu.dst_rtype === RT_FIX && cu.ldst === (8 + k).U }.reduce(_||_)
      val cap = Mux(cf_dbg && dsp_seen && dvL(k), iregister_read.io.cf_dbg_cap.get(k), 0.U)
      dvA(k) := (Mux(dbg_rise && !dvL(k), 0.U, dvA(k)) | cap) & wclr
      when (wr) { dvL(k) := false.B }
      when ((cf_dbg && !dsp_seen) || dbg_fall) {
        dvP(k) := map89(k)
        dvL(k) := true.B
      }
      iregister_read.io.cf_dbg_preg.get(k).valid := dvL(k)
      iregister_read.io.cf_dbg_preg.get(k).bits  := dvP(k)
      iregister_read.io.cf_dbg_vt.get(k)         := dvA(k)
    }
  }

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

  // [FFTAINT] step 18 (owner ruling 118, G-2; design note ift-gaps-and-fixes 7.3): two sticky
  // taint bits for fflags (shared by fflags and fcsr[4:0]).
  //   SET   when a retiring secret / attacker-tainted FP op RAISES a flag (rob.io.cf_ff_set_*,
  //         the commit-time flag write; a "flag stayed 0" outcome is knowingly untagged).
  //   CLEAR both on any CSR write to fflags/fcsr: csrrw/csrrwi (fsflags, fscsr, csrw), and
  //         csrrs/csrrc (decode turns rs1/uimm = 0 into CSR.R) whose mask has a non-zero flag
  //         field wdata[4:0].  A set/clear that touches only fcsr.frm leaves the flags, and the
  //         bits, unchanged.  frm itself is not tracked.
  //   READ  (frflags/frcsr/csrr, and the old value returned by csrrw/csrrs/csrrc) returns the
  //         bits as the destination's VALUE taint: secret / taint_atk on the CSR unit's
  //         regfile write port and on its rob.io.wb_resps port (-> preg secret, s_prop update,
  //         sticky atk), see the writeback sections below.
  // CSR ops are unique (ROB empty), so a set from commit and a CSR write never share a cycle;
  // a read sees the bits before its own write (registered).  Logic: a 12-bit address compare
  // (2 consts), a 3-bit cmd decode and a 5-bit OR into each flop; the read taint is one AND
  // after a register.  csr_addr / csr_rw_cmd / wb_wdata are the same signals csr.io.rw uses.
  val cf_ff_sec = RegInit(false.B)
  val cf_ff_atk = RegInit(false.B)
  val cf_ff_csr_hit = csr_exe_unit.io.iresp.bits.uop.csr_addr === freechips.rocketchip.rocket.CSRs.fflags.U ||
                      csr_exe_unit.io.iresp.bits.uop.csr_addr === freechips.rocketchip.rocket.CSRs.fcsr.U
  val cf_ff_csr_rd  = cf_ff_csr_hit && csr_rw_cmd.isOneOf(freechips.rocketchip.rocket.CSR.R,
                        freechips.rocketchip.rocket.CSR.W, freechips.rocketchip.rocket.CSR.S,
                        freechips.rocketchip.rocket.CSR.C)
  val cf_ff_csr_wr  = csr_exe_unit.io.iresp.valid && cf_ff_csr_hit &&
                      (csr_rw_cmd === freechips.rocketchip.rocket.CSR.W ||
                       (csr_rw_cmd.isOneOf(freechips.rocketchip.rocket.CSR.S, freechips.rocketchip.rocket.CSR.C) &&
                        wb_wdata(4, 0).orR))
  // [DBGFREEZE] patch 85 (c): the program's fflags taint is frozen in debug mode (a debug-mode FP op or fflags write
  // neither sets nor clears it).  One more input on each flop's D LUT.
  if (ENABLE_IFT) {
    when (cf_dbg) {
    } .elsewhen (cf_ff_csr_wr) {
      cf_ff_sec := false.B
      cf_ff_atk := false.B
    } .otherwise {
      when (rob.io.cf_ff_set_sec) { cf_ff_sec := true.B }
      when (rob.io.cf_ff_set_atk) { cf_ff_atk := true.B }
    }
  }
  // value taint a CSR-unit result gets from reading fflags/fcsr (0 for any other CSR / op)
  val cf_ff_rd_sec = cf_ff_csr_rd && cf_ff_sec
  val cf_ff_rd_atk = cf_ff_csr_rd && cf_ff_atk

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
          // [WINSHADOW] a shadow uop executing after B resolved carries window taint already
          // [WINEXP] step 21: the bypass carries the producer's LIVE result window taint in
          // cf_win_*, and cf_opnd_* is cleared, so register-read's byp_win (win || opnd) reads it
          // unchanged.  `bypasses` feeds only iregister_read.  The epoch compare is one LUT in
          // parallel with in(); no added level on the bypass path.
          val byu = exe_unit.io.bypass(i).bits.uop
          // [ONETAG] r205 (RULES.md A reading 2): the bypassed value carries the ONE secret tag the regfile write
          // will store (regfile.scala WritePort): data taint || the producer's s_acc / s_prop || shadow membership.
          // This also closes the old bypass-vs-regfile difference (the shadow term was missing on the bypass).
          // Levels: 0 added on the bypass DATA bits.  The TAG bits gain in() (two 8-bit IsOlder carry chains + a LUT)
          // from the bypass uop's rob_idx flop into register-read's MuxCase, and the 2 x 19-bit window state plus
          // rob_head is broadcast to every such site (corrected lean58, timing-audit-20261023 section 5: the cost
          // that matters here is that fan-out and routing, not the level count).
          bypasses(bypass_idx).bits.secret := exe_unit.io.bypass(i).bits.secret || byu.cf_secret_access.getOrElse(false.B) ||
                                              byu.cf_secret_propagation.getOrElse(false.B) || wsh_sec.in(byu.rob_idx)
          bypasses(bypass_idx).bits.uop.cf_win_sec.foreach(_ := false.B)   // [ONETAG] no secret window kind
          // [ONETAGATK] r205 U8: the bypassed value carries the one attacker tag the regfile write stores
          bypasses(bypass_idx).bits.taint_atk := exe_unit.io.bypass(i).bits.taint_atk || byu.cf_attacker_influence.getOrElse(false.B) ||
                                                 wsh_atk.in(byu.rob_idx)
          bypasses(bypass_idx).bits.uop.cf_win_atk.foreach(_ := false.B)
          bypasses(bypass_idx).bits.uop.cf_opnd_wsec.foreach(_ := false.B)
          bypasses(bypass_idx).bits.uop.cf_opnd_watk.foreach(_ := false.B)
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
    io.lsu.dis_valids(w)     := dis_valids(w)   // [DISPAR] patch 51 (A): pre-stall valids for the slot-parallel LDQ/STQ allocation
    io.lsu.dis_uops(w).bits  := dis_uops(w)
  }

  // tell LSU about committing loads and stores to clear entries
  io.lsu.commit                  := rob.io.commit

  // tell LSU that it should fire a load that waits for the rob to clear
  io.lsu.commit_load_at_rob_head := rob.io.com_load_is_at_rob_head

  //com_xcpt.valid comes too early, will fight against a branch that resolves same cycle as an exception
  // [EXCREG] patch 25: IFT builds use the dedicated, DONT_TOUCH copy (same D, equal every cycle)
  if (ENABLE_IFT) { io.lsu.exception := lsu_exc_rep.get } else {
  io.lsu.exception := RegNext(rob.io.flush.valid)
  }

  // Handle Branch Mispeculations
  io.lsu.brupdate := brupdate
  lsu_b1_rep.foreach(r => io.lsu.brupdate.b1 := r)   // [BRMASKREG] patch 25: the replica masks
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
  // [F1WSH] lean58 timing F1 (timing-audit-20261023 section 6): the memory write ports take the window-shadow
  // membership the LSU computed per response candidate before its late select (io.lsu.cf_resp_w*, lsu.scala
  // cfRespWin), instead of running wsh_*.in() on the selected uop's rob_idx here.  Port 0 is the ll_wbarb output:
  // each arbiter input gets its own membership (the LSU bit for input 0; in() on the FP-to-int / RoCC uop, whose
  // rob_idx is not behind the LSU select, for the others) and the arbiter's own `chosen` picks it.  Exact:
  // chisel3 Arbiter drives io.out.bits = io.in(io.chosen).bits in every cycle, so the picked bit is
  // in(io.out.bits.uop.rob_idx) whenever in(0)'s bit is (lsu.scala argument); IFT off passes None (unchanged).
  val ll_win_in: Option[(Bool, Bool)] = if (ENABLE_IFT) {
    val srcs = Seq((io.lsu.cf_resp_wsec.get(0), io.lsu.cf_resp_watk.get(0))) ++
               ll_wbarb.io.in.drop(1).map(i => (wsh_sec.in(i.bits.uop.rob_idx), wsh_atk.in(i.bits.uop.rob_idx)))
    Some((VecInit(srcs.map(_._1))(ll_wbarb.io.chosen), VecInit(srcs.map(_._2))(ll_wbarb.io.chosen)))
  } else None
  iregfile.io.write_ports(0) := WritePort(ll_wbarb.io.out, ipregSz, xLen, RT_FIX, wsh_sec, wsh_atk, ll_win_in)   // [WINSHADOW]/[WINEXP] [F1WSH]
  ll_wbarb.io.in(0) <> mem_resps(0)
  assert (ll_wbarb.io.in(0).ready) // never backpressure the memory unit.
  for (i <- 1 until memWidth) {
    val mem_win_in = if (ENABLE_IFT) Some((io.lsu.cf_resp_wsec.get(i), io.lsu.cf_resp_watk.get(i))) else None   // [F1WSH]
    iregfile.io.write_ports(w_cnt) := WritePort(mem_resps(i), ipregSz, xLen, RT_FIX, wsh_sec, wsh_atk, mem_win_in)   // [WINSHADOW]/[WINEXP] [F1WSH]
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
      // [ONETAG] r205: the ONE secret tag (regfile.scala WritePort rule): data taint || writer s_acc / s_prop ||
      // window-shadow membership.  No window kind.  (lean58 note: rob_idx here comes from the FU response
      // registers, through the exe unit's own response select on units with several FUs; not behind the LSU select.
      // The memory ports take the LSU's precomputed bit, [F1WSH].)
      val wb_sec = wbresp.bits.secret || wbresp.bits.uop.cf_secret_access.getOrElse(false.B) ||
                   wbresp.bits.uop.cf_secret_propagation.getOrElse(false.B) || wsh_sec.in(wbresp.bits.uop.rob_idx)
      iregfile.io.write_ports(w_cnt).bits.secret := wb_sec
      iregfile.io.write_ports(w_cnt).bits.wsec := false.B   // [ONETAG]
      iregfile.io.write_ports(w_cnt).bits.watk := false.B   // [ONETAGATK] r205 U8
      // [ONETAGATK] the one attacker tag: value taint || ownership || writer atk record || attacker-shadow membership
      val wb_atk = wbresp.bits.taint_atk || (wbresp.bits.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= 0.U) ||
                   wbresp.bits.uop.cf_attacker_influence.getOrElse(false.B) || wsh_atk.in(wbresp.bits.uop.rob_idx)
   // [PROBE STRIPPED 2026-09-11] A1UP -- diagnostic only, purpose discharged.
      // // [A1UP 2026-09-11] TEMPORARY.  The [A1AGU]/[A1RR2] probes localised A1 to UPSTREAM of
      // // register-read: the secret bit is on a store's data register only 14 times in 223,239
      // // samples, while ATTACKER taint reaches the same register through the identical bypass
      // // structure 6,590 times.  So the question is whether PRODUCERS ever mark a value secret.
      // // Print every integer writeback: if wbsec is ~never 1 while wbatk often is, the defect
      // // is in whatever sets `secret` on a functional-unit result -- not in any bypass or
      // // register-read path, which are now both exonerated by measurement.
      // if (ENABLE_CF_DEBUG_PRINTF) {
        // when (wbIsValid(RT_FIX)) {
          // printf("\n[A1UP] pc=0x%x pdst=%d wbsec=%d wbatk=%d uacc=%d uprop=%d udom=%d\n",
            // wbresp.bits.uop.debug_pc, wbpdst,
            // wbresp.bits.secret, wbresp.bits.taint_atk,
            // wbresp.bits.uop.cf_secret_access, wbresp.bits.uop.cf_secret_propagation,
            // wbresp.bits.uop.cf_domain_id)
        // }
      // }
      iregfile.io.write_ports(w_cnt).bits.taint_atk := wb_atk   // [ONETAGATK]
      wbresp.ready := true.B
      if (exe_units(i).hasCSR) {
        iregfile.io.write_ports(w_cnt).bits.data := Mux(wbReadsCSR, csr.io.rw.rdata, wbdata)
        // [FFTAINT] step 18: a read of fflags/fcsr carries the sticky flags taint (this unit's
        // iresp is the one csr.io.rw reads, so cf_ff_rd_* already names this uop)
        iregfile.io.write_ports(w_cnt).bits.secret    := wb_sec || cf_ff_rd_sec   // [ONETAG]
        iregfile.io.write_ports(w_cnt).bits.taint_atk := wb_atk || cf_ff_rd_atk   // [ONETAGATK]
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
    // a secret-dependent branch condition yields a secret-dependent predicate:
    // predicated registers are tagged and propagate like any other value.
    // [ONETAG] r205: the one secret tag, as the integer ports
    pregfile.io.write_ports(0).bits.secret := jmp_unit.io.iresp.bits.secret || jmp_unit.io.iresp.bits.uop.cf_secret_propagation.getOrElse(false.B) ||
                                              wsh_sec.in(jmp_unit.io.iresp.bits.uop.rob_idx)
    pregfile.io.write_ports(0).bits.taint_atk := jmp_unit.io.iresp.bits.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= 0.U ||
                                                 jmp_unit.io.iresp.bits.uop.cf_attacker_influence.getOrElse(false.B) ||   // [ONETAGATK]
                                                 wsh_atk.in(jmp_unit.io.iresp.bits.uop.rob_idx)
    pregfile.io.write_ports(0).bits.wsec := false.B   // [ONETAG]
    pregfile.io.write_ports(0).bits.watk := false.B   // [ONETAGATK]
  }

  // [WINSHADOW] step 20, coverage (c), second half: a shadow uop that wrote back BEFORE B
  // resolved has an unmarked register; it is set when the uop commits (the ROB computes the
  // range test, cf_com_win_*).  This is the existing per-lane commit path: it cannot be
  // avoided, because the register was written before the window existed and nothing else
  // writes it again.  coreWidth set ports on each of the int and FP window-taint Mems.
  // [ONETAG] r205 (RULES.md A reading 2): the secret set port now sets the register's LATE secret tag for every
  // committing writer whose RECORD is secret (rob.io.cf_com_sec = commit-time s_acc || s_prop || shadow, the bits
  // the record carries), covering record bits that landed after the writeback (contention, MEM_ORDER, D$ edges,
  // the commit-time shadow).  Registered one cycle (the commit readout -> cross-module -> 192-way decode leg is
  // cut at the flop): a set one cycle late is safe, because a committed pdst is freed only when a LATER writer of
  // the same logical register commits, and its next writer reaches writeback >= 3 cycles after reallocation; that
  // writeback clears the bit (regfile.scala).  Readers between this writer's writeback and this set see the
  // writeback-time tag only (residual, host-recoverable from pdst/prs).  Cost: coreWidth x (1 + 8) FF per file.
  for (w <- 0 until coreWidth) {
    val cu  = rob.io.commit.uops(w)
    val cwr = rob.io.commit.valids(w) && cu.rf_wen && cu.ldst_val
    // RULES.md D3 (ENABLE_IFT gating, IFT-off Verilog unchanged): the flops exist only with IFT on
    val com_sec_set = if (ENABLE_IFT) RegNext(cwr && rob.io.cf_com_sec.get(w), false.B) else false.B
    val com_sec_rt  = if (ENABLE_IFT) RegNext(cu.dst_rtype) else cu.dst_rtype
    val com_sec_pd  = if (ENABLE_IFT) RegNext(cu.pdst) else cu.pdst
    iregfile.io.win_set_sec(w).valid := com_sec_set && com_sec_rt === RT_FIX
    iregfile.io.win_set_sec(w).bits  := com_sec_pd
    // [ONETAGATK] r205 U8: the late ATTACKER set for every committing writer whose record has atk (shares the
    // registered rtype / pdst of the secret set: +1 FF per lane)
    val com_atk_set = if (ENABLE_IFT) RegNext(cwr && rob.io.cf_com_atk.get(w), false.B) else false.B
    iregfile.io.win_set_atk(w).valid := com_atk_set && com_sec_rt === RT_FIX
    iregfile.io.win_set_atk(w).bits  := com_sec_pd
    if (usingFPU) {
      fp_pipeline.io.win_set_sec(w).valid := com_sec_set && com_sec_rt === RT_FLT   // [ONETAG]
      fp_pipeline.io.win_set_sec(w).bits  := com_sec_pd
      fp_pipeline.io.win_set_atk(w).valid := com_atk_set && com_sec_rt === RT_FLT   // [ONETAGATK]
      fp_pipeline.io.win_set_atk(w).bits  := com_sec_pd
    }
  }
  if (usingFPU) {
    fp_pipeline.io.wsh_sec := wsh_sec
    fp_pipeline.io.wsh_atk := wsh_atk
    fp_pipeline.io.cf_wclr := 0.U   // [WINCLR] patch 35 C; [ONETAG] [ONETAGATK] no close clear on either side
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
        // [FFTAINT] step 18: same read taint on the ROB writeback port, so the existing
        // writeback paths set s_prop (cf_s_prop_rob_upd via wb.bits.secret) and sticky atk
        // (R0 site via taint_atk) for the reading instruction
        rob.io.wb_resps(cnt).bits.secret    := resp.bits.secret || cf_ff_rd_sec
        rob.io.wb_resps(cnt).bits.taint_atk := resp.bits.taint_atk || cf_ff_rd_atk
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
  rob.io.cf_br_atk_mask := cf_br_atk_mask_acc   // [STEP1] attacker shadow broadcast
  // [BRSELF] the steering branch's OWN entry -- see rob.scala.  Gated with the mask
  // reduction so it vanishes with it when ENABLE_IFT=false.
  for (i <- 0 until coreWidth) {
    rob.io.cf_br_atk_self(i).valid            := (if (ENABLE_IFT) brinfos(i).valid && brinfos(i).cond_atk else false.B)
    rob.io.cf_br_atk_self(i).bits.rob_idx     := brinfos(i).uop.rob_idx
    rob.io.cf_br_atk_self(i).bits.op_count_id := brinfos(i).uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
  }

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
  rob.io.lsu_clr_bsy_cf_sprop := io.lsu.clr_bsy_cf_sprop
  rob.io.lsu_clr_bsy_cf_atk.foreach(_ := io.lsu.clr_bsy_cf_atk.get)   // [M7STATK] bundle61
  rob.io.lsu_clr_unsafe        := io.lsu.clr_unsafe
  rob.io.lxcpt          <> io.lsu.lxcpt

  // corefuzzing: issue contention updates — flatten per-port outputs from all issue units (mem, int, fp)
  rob.io.cf_issue_contention_upd := VecInit(
    issue_units.flatMap(u => u.io.cf_contention_upd) ++
    (if (usingFPU) fp_pipeline.io.cf_contention_upd else Nil)
  )
  rob.io.cf_lsu_s_acc_upd        := io.lsu.cf_s_acc_rob_upd
  // [MEMORD 2026-09-08] ty=8 MEM_ORDER edge from the LSU's order_fail sites.
  rob.io.cf_lsu_memord_upd       := io.lsu.cf_memord_upd
  rob.io.cf_lsu_rdedge_upd.foreach(_ := io.lsu.cf_rdedge_upd.get)   // [RDEDGE] patch 80

  // Default driver for rob.io.cf_s_prop_rob_upd — always fires so the IO has a
  // valid default even when IFT is disabled. The IFT writeback-transitive block
  // below conditionally overrides via last-connect semantics when ENABLE_IFT=true.
  rob.io.cf_s_prop_rob_upd.foreach { u =>
    u.valid := false.B; u.bits := DontCare
    u.bits.src_tainted := false.B   // never DontCare: this bit gates a ROB write
    u.bits.self_secret_acc := false.B
  }
  // Gap 1 fix (DOC:23): defaults for new IOs — overridden inside if (ENABLE_IFT) below.
  rob.io.cf_lsu_s_tx_upd.foreach { u => u.valid := false.B; u.bits := DontCare }

  if (ENABLE_IFT) {

  // Gap 1 fix (DOC:23): expose preg_secret to LSU so TLB stage can check live taint.
  // preg_secret is a Reg here; this is a register-read fan-out (192 wires), no logic depth.
  // Gap 1 fix: route LSU's cf_s_tx_rob_upd to ROB (mirrors cf_lsu_s_acc_upd at line 1913).
  rob.io.cf_lsu_s_tx_upd := io.lsu.cf_s_tx_rob_upd

  // Fix 1: clear preg_secret for the stale physical register freed at commit.
  // taint_table[stale_pdst] is already cleared in rename-stage's stale-pdst loop;
  // preg_secret lives here in core, so we clear it on the same commit event.
  for (w <- 0 until coreWidth) {
    when (rob.io.commit.valids(w) && !rob.io.commit.rollback) {
      when (rob.io.commit.uops(w).dst_rtype === RT_FLT) {
      } .otherwise {
      }
    }
  }

  // corefuzzing: preg_secret writeback-time transitive propagation.
  // For each integer writeback, if any source physical register was secret-tainted,
  // mark the destination register as secret-tainted too, and emit a cf_s_prop_rob_upd
  // to set cf_secret_propagation on the in-flight ROB entry.
  // Also clears preg_secret[pdst] when a non-secret instruction overwrites the register,
  // preventing stale taint from previous occupants of the same physical register.
  for (i <- 0 until rob.numWakeupPorts) {
    val wb     = rob.io.wb_resps(i)
    val wb_uop = wb.bits.uop
    val int_wb = wb.valid && wb_uop.rf_wen && wb_uop.dst_rtype === RT_FIX && wb_uop.ldst_val
    when (int_wb) {
      // [IMPLWIN] step 20 (ruling 135): a window-tainted OPERAND counts as secret for the
      // s_prop update / REG_DATAFLOW edge (the window's own seed, cf_win_sec, does not).
      val sec1 = wb.bits.secret || wb_uop.cf_opnd_wsec.getOrElse(false.B)   // taint arrived with the result
      val sec2 = false.B
      // Taint the destination if any source was secret or this load already has s_acc.
      // (cf_secret_access is already set on the wb_uop for secret-range loads via the
      // existing TLB-stage path; this handles the transitive ALU→ALU case.)
      // Fix 3b: detect secret-store→load data flow injected by dcache memsec_fire.
      // An INFL_MEM_DATAFLOW entry with is_atk=false, is_secret=true marks this case.
      val has_mem_sec_infl = wb_uop.cf_mem_sec_dataflow.getOrElse(false.B)
      val is_secret_wb = wb_uop.cf_secret_access.getOrElse(false.B) || sec1 || sec2 || has_mem_sec_infl
      when (is_secret_wb) {
        // Emit s_prop ROB update for transitive register taint, secret-mem data flow,
        // OR direct secret access (load from secret range needs retroactive s_prop on its ROB entry
        // so downstream consumers see the taint chain).
        when (sec1 || sec2 || has_mem_sec_infl || wb_uop.cf_secret_access.getOrElse(false.B)) {
          rob.io.cf_s_prop_rob_upd(i).valid            := true.B
          rob.io.cf_s_prop_rob_upd(i).bits.rob_idx     := wb_uop.rob_idx
          rob.io.cf_s_prop_rob_upd(i).bits.op_count_id := wb_uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
          // Tell the ROB WHY this fired.  A real source taint means (for a load)
          // that prs1 -- the address register -- is secret-derived: a genuine
          // transmission.  cf_secret_access alone is bookkeeping so the chain is
          // visible downstream; it transmits nothing and must NOT become
          // cf_secret_transmission.  See CF_SPropUpdate in micro-op.scala.
          rob.io.cf_s_prop_rob_upd(i).bits.src_tainted := sec1 || sec2 || has_mem_sec_infl
          rob.io.cf_s_prop_rob_upd(i).bits.self_secret_acc := wb_uop.cf_secret_access.getOrElse(false.B)
          // Residual D-BP fix: if this is a branch, retroactively mark it secret in
          // spec_branch_secret_table so speculative consumers dispatched after TLB fires
          // see the correct secret state.  Writing a freed/reallocated br_tag slot is
          // harmless — resolved branches have no remaining speculative consumers.
          when (wb_uop.allocate_brtag) {
            spec_branch_secret_table(wb_uop.br_tag) := true.B
            // (context is set from brinfos at resolution; this site cannot see
            //  branches -- it is gated on rf_wen && dst_rtype===RT_FIX && ldst_val)
          }
        }
      } .otherwise {
        // Non-secret instruction writing this pdst: clear any stale preg_secret bit.
      }
    }
    // FP writeback: transitive secret propagation through the FP register file.
    // Mirrors the INT path above, but checks fp_preg_secret for FP-typed sources
    // and handles INT→FP conversions (e.g. fcvt.d.w) by also checking preg_secret.
    val fp_wb = wb.valid && wb_uop.rf_wen && wb_uop.dst_rtype === RT_FLT && wb_uop.ldst_val
    when (fp_wb) {
      val fsec1 = wb.bits.secret || wb_uop.cf_opnd_wsec.getOrElse(false.B)   // [IMPLWIN]
      val fsec2 = false.B
      val fsec3 = false.B
      // INT→FP conversions: check INT source register in preg_secret
      val isec1 = false.B
      val isec2 = false.B
      val is_secret_fp_wb = wb_uop.cf_secret_access.getOrElse(false.B) || fsec1 || fsec2 || fsec3 || isec1 || isec2
      when (is_secret_fp_wb) {
        when (fsec1 || fsec2 || fsec3 || isec1 || isec2 || wb_uop.cf_secret_access.getOrElse(false.B)) {
          rob.io.cf_s_prop_rob_upd(i).valid            := true.B
          rob.io.cf_s_prop_rob_upd(i).bits.rob_idx     := wb_uop.rob_idx
          rob.io.cf_s_prop_rob_upd(i).bits.op_count_id := wb_uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))
          // Same rule as the INT path above: only a real source taint is a transmission.
          rob.io.cf_s_prop_rob_upd(i).bits.src_tainted := fsec1 || fsec2 || fsec3 || isec1 || isec2
          rob.io.cf_s_prop_rob_upd(i).bits.self_secret_acc := wb_uop.cf_secret_access.getOrElse(false.B)
        }
      } .otherwise {
      }
    }
  }
  }  // end if (ENABLE_IFT)

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
  // [reconf-fix 2.10f] a staged D$ geometry change (drain + walk) is progress, not a hang;
  // the D$ walker carries its own 2^17-cycle watchdog. No-op in the baseline (None).
  io.lsu.cf_dcache_reconf_status.foreach { s => when (s(4)) { idle_cycles := 0.U } }
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
  // IFT compile-time gate: only the CF EXTRAS appended to the commit log embed
  // cf_* fields and are meaningful only when ENABLE_IFT=true, so ENABLE_IFT now
  // gates that inner block (see the `if (ENABLE_CF_DEBUG_PRINTF && ENABLE_IFT)`
  // below) rather than the whole commit log. The spike-standard fields
  // (priv, pc, inst, rd, wdata) have no IFT dependence, so a non-IFT build can
  // still emit a commit log that is diffable against spike.
  // Equivalence: when ENABLE_IFT=true both conditions are unchanged from before,
  // so IFT builds produce bit-identical output.
  if (COMMIT_LOG_PRINTF) {
    var new_commit_cnt = 0.U

    for (w <- 0 until coreWidth) {
      val priv = RegNext(csr.io.status.prv) // erets change the privilege. Get the old one

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
          // [CFNOISE-COMMENTED] printf(" [SSTEP]")
        }
        */

        // [ONEPRINTF] patch 39: the commit record (head, inst, CF(...), influencer slots, register
        // write, newline) is emitted by ONE printf statement.  It used to take 4-6 statements, and
        // Verilator runs another module's one-statement print ([SQREC], [SQDROP], [FILL], [FLUSH])
        // or a [SPECULATIVE] piece between them in the same cycle, which tore the record across
        // physical lines and lost it (p4-r3: t53/t64 lbu, t64 m74, t91; up to ~100 per log).  The
        // pieces below are only format text and arguments, concatenated in the old print order with
        // the same specifiers and the same argument expressions (so the same widths), so a record
        // that was not torn before is byte-identical.  The inst and register-write variants pick the
        // statement (2 x 3 mutually exclusive `when` branches, one printf each), because a Mux would
        // change the printed width.  Printf only (COMMIT_LOG_PRINTF builds: Verilator debug); no
        // FireSim or NoLog config elaborates this block.
        val cuop = rob.io.commit.uops(w)
        val headFmt  = "%d 0x%x "
        val headArgs = Seq[Bits](priv, Sext(cuop.debug_pc(vaddrBits-1,0), xLen))
        var cfFmt  = ""
        var cfArgs = Seq[Bits]()
        // CF EXTRAS (corefuzzing additions to the commit log). Gated by
        // ENABLE_CF_DEBUG_PRINTF: when false, the commit log emits ONLY the
        // spike-standard fields (priv, pc, inst, rd, wdata) so it can be diffed
        // byte-for-byte against spike. Everything in here is a corefuzzing artifact.
        // ALSO gated by ENABLE_IFT: this block reads cf_* fields and the rename
        // stage's com_late_taint_any, which are only meaningful under IFT. This
        // is the IFT dependence that used to sit on the OUTER commit-log gate
        // (~line 2105); moving it here lets a non-IFT build emit the
        // spike-standard commit log while keeping the IFT-only cone elided.
        if (ENABLE_CF_DEBUG_PRINTF && ENABLE_IFT) {

          // New: Print corefuzzing fields. This prints a compact, human-
          // readable summary containing domain, speculation/attacker/secret
          // flags, op count, single-step, taint modules (up to 5), and the
          // predispatch taint queue (up to 5). Each taint entry is shown as
          // module:type:opcount. If some fields are zero, they will print as
          // zeros.
          // Fix 5b: OR in late-detected s_prop from retroactive commit-time taint check.
          // Merge INT and FP rename stages: either may detect late taint on source regs.
          val com_late_merged = rename_stage.io.com_late_taint_any(w) ||
            (if (usingFPU) fp_rename_stage.io.com_late_taint_any(w) else false.B)
          val com_s_prop_final = rob.io.commit.uops(w).cf_secret_propagation.getOrElse(false.B) || com_late_merged
          // Fix 5c: commit-time s_tx for stores/loads whose secret taint was only
          // detected retroactively.  Uses com_late_merged (no dst_rtype guard)
          // because stores have dst_rtype=RT_X and would be missed by com_late_taint.
          val com_late_s_tx = com_late_merged &&
                              !rob.io.commit.uops(w).cf_secret_access.getOrElse(false.B) &&
                              (rob.io.commit.uops(w).uses_stq || rob.io.commit.uops(w).uses_ldq)
          val com_s_tx_final = rob.io.commit.uops(w).cf_secret_transmission.getOrElse(false.B) || com_late_s_tx
          // pdst/prs1/prs2 + rtypes: the register operands, so the parser can rebuild the
          // dataflow graph itself.  A secret access resolves at TLB, long after its
          // consumers renamed, so a victim-only secret chain has no dispatch-time edge to
          // record; logging what each uop read and wrote lets the taint be derived after
          // the fact.  rtypes matter because unused prs fields still hold stale pregs.
          // [CFMOVE 2026-09-08] FU/INFL_FU/OVF/dcy/srob/sstq/alias moved INSIDE CF(...).
          // They are all cf_* uop taint state and belong with the rest of it; they were
          // split across two printfs, which also opened a window for the MEMTRACE printf
          // to interleave BETWEEN them and corrupt records.  parse_ift_log.py was made
          // position-independent to follow this, not the other way round.
          // [RECV3] SP-05 (design 7.3): the record v3 tokens.  cyc = ift_cyc16 in the commit cycle (= the COMMIT record's
          // cyc); fflags = the lane's FP flags (= record [118:114]); shadow_sec / shadow_atk = record [84] / [85] (v1 wsh,
          // and its attacker twin; 0 in debug mode, [DBGFREEZE]); FU = the 15-bit unit word; sftq = the record field (SP-09);
          // dc = the D$ footprint kind/set/way (SP-08), last, so no token lands between sstq= and alias= (ift_core
          // _UOP_CTR_RE reads them adjacently).  Dropped: spec_atk/spec_sec/spec_oc (N-D2), ce_repl (REPL_STATE slot type,
          // D15), INFL_FU (printf-only, no consumer; INDEX-A gap A-5).
          cfFmt = " CF(domain=%d spec=%d atk=%d s_acc=%d s_prop=%d s_tx=%d fflags=0x%x opcount=%d cyc=%d pdst=%d prs1=%d prs2=%d rt1=%d rt2=%d brmask=0x%x brtag=%d abr=%d sbr=%d shadow_sec=%d shadow_atk=%d FU=0x%x OVF=%d dcy=%d srob=%d sstq=%d alias=%d sftq=%d dc=%d/%d/%d) "
          cfArgs = Seq[Bits](
            rob.io.commit.uops(w).cf_domain_id.getOrElse(0.U(iftTagWidth.W)),
            rob.io.commit_ift(w).speculated,
            rob.io.commit.uops(w).cf_attacker_influence.getOrElse(false.B),
            rob.io.commit.uops(w).cf_secret_access.getOrElse(false.B),
            com_s_prop_final,
            com_s_tx_final,
            rob.io.commit_ift(w).fflags,
            rob.io.commit.uops(w).cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)),
            ift_cyc16,
            rob.io.commit.uops(w).pdst,
            rob.io.commit.uops(w).prs1,
            rob.io.commit.uops(w).prs2,
            rob.io.commit.uops(w).lrs1_rtype,
            rob.io.commit.uops(w).lrs2_rtype,
            // [BRMASK 2026-09-08] the full speculation shadow and this uop's own branch tag (printf only; see git history
            // of this block for the tag-reuse note)
            rob.io.commit.uops(w).br_mask,
            rob.io.commit.uops(w).br_tag,
            // [GHISTFIELD] GHR window counters (record abr / sbr)
            rob.io.commit_ift(w).atk_branch_ctr,
            rob.io.commit_ift(w).sec_branch_ctr,
            rob.io.cf_com_win_sec(w) && !cf_dbg,   // shadow_sec = record [84] (rob.scala com_sh_sec)
            rob.io.cf_com_win_atk(w) && !cf_dbg,   // shadow_atk = record [85] (rob.scala com_sh_atk)
            rob.io.commit.uops(w).cf_fu_bitmap.getOrElse(0.U(numModules.W)),
            rob.io.commit_ift(w).infl_dropped,
            rob.io.commit_ift(w).cntd_deny_count,
            rob.io.commit_ift(w).stall_cycles_rob,
            rob.io.commit_ift(w).stall_cycles_stq,
            rob.io.commit_ift(w).infl_oc_aliased,
            rob.io.commit_ift(w).sftq,
            rob.io.commit_ift(w).dc.kind,
            rob.io.commit_ift(w).dc.set,
            rob.io.commit_ift(w).dc.way)

          // [THINUOP] Task 3.5: the single-step marker printf was commented out and its
          // cf_single_step field left the MicroOp; the empty `when` is deleted.

          // [CNTDPROBE 2026-09-10] TEMPORARY -- proof obligation for deleting the
          // cf_cntd_winner_* uop fields (18b: op 16, atk 1, sec 1).
          //
          // Claim: they are a redundant second copy of what the ty=2
          // INFL_ISSUE_CONTENTION edge already carries.  Both are written from the SAME
          // ic_pending_winner_* registers (rob.scala:715-718 for the edge, :726-727 for
          // the uop fields), so they must agree bit-for-bit.
          //
          // They appear in NO existing log -- not on the bridge, not in any printf -- so
          // the claim cannot be checked against retained logs; it needs one run with them
          // printed.  FALSIFIABLE: any record carrying a ty=2 edge where
          //   edge.op_count != low inflOpCountWidthCF bits of winner_op,  OR
          //   edge.is_atk   != winner_atk,                                OR
          //   edge.is_secret!= winner_sec
          // refutes the claim and the fields stay.  DELETE THIS PROBE once it passes.
       // [PROBE-STRIPPED 2026-09-10] CNTD probe -- proof DISCHARGED (verify_cntd_redundancy.py:
          // the cf_cntd_* fields are live in issue-slot.scala:429-433 slot migration, so they
          // are KEPT, not reclaimed).  Commented out: it is not hardware and it slows the sim.
          // if (ENABLE_CF_DEBUG_PRINTF) {
            // // Gate v2: cf_cntd_valid is NEVER true at commit (measured: 0 probe lines
            // // against 1,057 ty=2 edges), so gating on it sampled nothing.  Gate on the
            // // fields under test instead -- any record where they carry data.
            // when (rob.io.commit.uops(w).cf_cntd_winner_op =/= 0.U ||
                  // rob.io.commit.uops(w).cf_cntd_winner_atk ||
                  // rob.io.commit.uops(w).cf_cntd_winner_sec ||
                  // rob.io.commit.uops(w).cf_cntd_valid) {
              // printf("\n[CNTD] oc=%d wop=%d watk=%d wsec=%d\n",
                // rob.io.commit.uops(w).cf_op_count_id,
                // rob.io.commit.uops(w).cf_cntd_winner_op,
                // rob.io.commit.uops(w).cf_cntd_winner_atk,
                // rob.io.commit.uops(w).cf_cntd_winner_sec)
            // }
          // }

          // Print module bitmap, INFL_FU bitmap, and influencer slots (single atomic printf)
          val comInflFmt = (0 until numInfluencerSlotsCF).zipWithIndex.map{case(_,k) => s"I$k={v=%d,oc=%d,ty=%d,atk=%d,sec=%d}"}.mkString(" ")
          // [CFMOVE] scalars moved into CF() above; this printf is now the influencer
          // VECTOR only -- a list, parsed by INFL_ENTRY_RE, not a CF scalar field.
          val comFmt = comInflFmt
          val comInflArgs = (0 until numInfluencerSlotsCF).flatMap(k => Seq[Bits](
            rob.io.commit_ift(w).influencer_list(k).valid,
            rob.io.commit_ift(w).influencer_list(k).key,
            rob.io.commit_ift(w).influencer_list(k).infl_type,
            rob.io.commit_ift(w).influencer_list(k).is_atk,
            rob.io.commit_ift(w).influencer_list(k).is_secret
          ))
          // [ONEPRINTF] the slots follow CF(...) directly, as before (no separator).
          cfFmt  = cfFmt + comFmt
          cfArgs = cfArgs ++ comInflArgs
          // END MOD: commit CF prints
          // ---------------------------------------------------------------------
        }
        // [ONEPRINTF] the one statement of this record: inst form (rvc: 16 bits, as the old
        // printf_inst) x register-write form.
        def emitRecord(tailFmt: String, tailArgs: Seq[Bits]): Unit = {
          when (cuop.is_rvc) {
            printf(headFmt + "(0x%x)" + cfFmt + tailFmt, (headArgs ++ Seq[Bits](cuop.debug_inst(15,0)) ++ cfArgs ++ tailArgs): _*)
          } .otherwise {
            printf(headFmt + "(0x%x)" + cfFmt + tailFmt, (headArgs ++ Seq[Bits](cuop.debug_inst) ++ cfArgs ++ tailArgs): _*)
          }
        }
        when (cuop.dst_rtype === RT_FIX && cuop.ldst =/= 0.U) {
          emitRecord(" x%d 0x%x\n", Seq[Bits](cuop.ldst, rob.io.commit.debug_wdata(w)))
        } .elsewhen (cuop.dst_rtype === RT_FLT) {
          emitRecord(" f%d 0x%x\n", Seq[Bits](cuop.ldst, rob.io.commit.debug_wdata(w)))
        } .otherwise {
          emitRecord("\n", Seq[Bits]())
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
        // [CFNOISE-COMMENTED] printf("%x %x %x %x %x %x\n",
          // [CFNOISE-COMMENTED] rob.io.commit.uops(w).debug_fsrc, rob.io.commit.uops(w).taken,
          // [CFNOISE-COMMENTED] rob.io.commit.uops(w).is_br, rob.io.commit.uops(w).is_jal,
          // [CFNOISE-COMMENTED] rob.io.commit.uops(w).is_jalr, Sext(rob.io.commit.uops(w).debug_pc(vaddrBits-1,0), xLen))

      }
      new_ghist = Mux(rob.io.commit.arch_valids(w) && rob.io.commit.uops(w).is_br,
        Mux(rob.io.commit.uops(w).taken, new_ghist << 1 | 1.U(1.W), new_ghist << 1),
        new_ghist)
    }
    debug_ghist := new_ghist
  }

  // IFT bridge: forward rob IFT outputs to tile-level IO when the bridge (or the Task 2.10g
  // oracle mode, see tile.scala) is enabled. Zero-overhead otherwise (ift_bridge_out=None).
  // [LATEPATCH] The PC40-era late-taint patch (bits 81/82) is removed: since PC32 those bits
  // are cf_op_count_id bits 3 and 4, so a live com_late_taint_any would corrupt them
  // (D11, bug 6; inert today because com_late_taint_any is tied to false.B).
  ift_bridge_out.foreach { out =>
    for (w <- 0 until coreWidth) {
      out.commit_valid(w)  := rob.io.ift_commit_valids(w)
      out.commit_record(w) := rob.io.ift_commit_records(w)
    }
    out.squash_ovf    := rob.io.ift_squash_ovf
    // squash_valid / squash_record: the shared lane below (Task 3.4)
  }

  // Task 3.4 (D18): kill-time contention records share the squash lane with the ROB's squash
  // records.  The three IQ drains are arbitrated, packed once (event_type 3, influencer slot 0
  // = the denier, cntd_deny_count = Log2Bucket of the linear count), then arbitrated against
  // rob.io.ift_squash; the ROB side's ready only fills IftSquashMerge's output queue (4b
  // section 7.8 credit rule), never the banks.  One register on the lane (spec 5.3).
  val kill_srcs = issue_units.map(_.io.cf_kill_rec) ++ (if (usingFPU) Seq(fp_pipeline.io.cf_kill_rec) else Nil)
  val kill_ovfs = issue_units.map(_.io.cf_kill_ovf) ++ (if (usingFPU) Seq(fp_pipeline.io.cf_kill_ovf) else Nil)
  if (ENABLE_IFT) {
    val kill_arb = Module(new RRArbiter(new IftKillRecord, kill_srcs.length))
    for ((a, src) <- kill_arb.io.in zip kill_srcs) { a <> src }
    val kr = kill_arb.io.out.bits
    // [RECV3] SP-05: the v3 KILL record (design 2.2-2.3, KILL column; IFTRecordV3.scala): an instruction record of the
    // killed uop (pc, op_count, domain); slot 0 = the denier {key = its op_count[11:0], type, atk, sec}; dcy = the
    // bucketed deny count; infl_dropped = the winners the slot's one-entry list replaced; atk = the denier's or any
    // replaced winner's attacker label ([FIFO10] step 10), s_prop the secret twin ([R117SWEEP] patch 33); all else 0
    // (dbg 0, kind 0).  cyc = ift_cyc16 now: the record is packed combinationally from kill_arb's output and registered
    // into the lane (sq_lane_r1) in the cycle the lane takes it, so the stamp is the lane-grant cycle (design 2.8, D14),
    // also when the lane holds it back (spec SP-17).  The v1 priv field (RegNext(csr.io.status.prv)) is gone.
    val kill_rec = IFTRecordV3.instr(
      IFTRecordV3.prefix(REC_KILL.U(4.W), 0.U(4.W), false.B, kr.domain, ift_cyc16, kr.op_count, kr.pc),
      atk = kr.winner_atk || kr.any_atk, s_acc = false.B, s_prop = kr.winner_sec || kr.any_sec.getOrElse(false.B),
      s_tx = false.B, shadow_sec = false.B, shadow_atk = false.B, spec = false.B, oc_aliased = false.B,
      infl_dropped = kr.dropped, dcy = Log2Bucket(kr.deny_count), srob = 0.U(3.W), sstq = 0.U(3.W), sftq = 0.U(3.W),
      dc_kind = 0.U(2.W), dc_set = 0.U(7.W), dc_way = 0.U(3.W), fflags = 0.U(5.W), unit = 0.U(numModules.W),
      pdst = 0.U(8.W), prs1 = 0.U(8.W), prs2 = 0.U(8.W), rt1 = 0.U(2.W), rt2 = 0.U(2.W), abr = 0.U(7.W), sbr = 0.U(7.W),
      slots = Seq(IFTRecordV3.slot(kr.winner_op, kr.infl_type, kr.winner_atk, kr.winner_sec)) ++ Seq.fill(3)(0.U(20.W)))

    val sq_arb = Module(new RRArbiter(UInt(256.W), 2))
    sq_arb.io.in(0) <> rob.io.ift_squash
    sq_arb.io.in(1).valid := kill_arb.io.out.valid
    sq_arb.io.in(1).bits  := kill_rec
    kill_arb.io.out.ready := sq_arb.io.in(1).ready
    // [V3LANE] SP-17 (design 5.5, option (b)): the STATE record (class 8) takes the squash/kill lane with priority.
    // sq_arb.io.out.ready := !state.valid: the RRArbiter grants nothing that cycle, the squash merge's output queue
    // and the kill hold registers wait (their existing wait-or-count rule); the STATE record has no storage, so it
    // is never the one that waits.  The 2:1 lane mux is the approved +1 site (design 6.4 / 08 section 8.2 d).
    // No ift_state_in (no bridge / oracle): the original three lines, unchanged Verilog.
    val st_v    = ift_state_in.map(_.valid).getOrElse(false.B)
    val st_bits = ift_state_in.map(_.bits).getOrElse(0.U(256.W))
    val (lane_v, lane_rec) = if (ift_state_in.isDefined) (st_v || sq_arb.io.out.valid, Mux(st_v, st_bits, sq_arb.io.out.bits))
                             else (sq_arb.io.out.valid, sq_arb.io.out.bits)
    sq_arb.io.out.ready   := (if (ift_state_in.isDefined) !st_v else true.B)
    val sq_lane_v1    = RegNext(lane_v, false.B)      // keeps the lane registered (spec 5.3)
    val sq_lane_r1    = RegEnable(lane_rec, lane_v)
    // [RECREG] (Phase 4 Task 4.3, ruling 44) one more stage, matching the commit lane's new
    // register in rob.scala, so a squash record keeps its place relative to the commit records
    // on the bridge token stream (both lanes now lag by one cycle more than before).  No
    // back-pressure exists past sq_arb (out.ready is constant true and the bridge has no ready
    // for this lane), so a plain register pair cannot drop or duplicate a record; the ROB side
    // (rob.io.ift_squash, Decoupled) is untouched.  Exactly the old (valid, record) stream one
    // cycle later: r2(t) = r1(t-1) by induction on the enable.  Cost: 257 FF, 0 LUT.
    val sq_lane_valid = RegNext(sq_lane_v1, false.B)
    val sq_lane_rec   = RegEnable(sq_lane_r1, sq_lane_v1)
    ift_bridge_out.foreach { out =>
      out.squash_valid  := sq_lane_valid
      out.squash_record := sq_lane_rec
    }
    // 0xbce [15:0] share: hold registers overwritten before their drain (one per IQ per cycle at most)
    kill_ovf_cnt := satAdd16(kill_ovf_cnt, PopCount(kill_ovfs))
    // [V3LANE] SP-17 (design 5.4, G10): the values the tile packs into the STATE record, all flops (sqovf16 is the
    // ROB's satAdd16 of two counters); 97 wires core -> tile, both in SLR2.
    ift_core_out.foreach { o =>
      o.dbg       := cf_dbg
      o.sqovf16   := rob.io.ift_squash_ovf_cnt
      o.killovf16 := kill_ovf_cnt
      o.tsc48     := debug_tsc_reg(47, 0)
      o.cyc16     := ift_cyc16
    }
    // [V3PRINT] SP-22: the STATE record as it enters the lane (the decoder accepts these lines, SP-23) and the lane
    // class checks (hard stop).  Debug-print builds with the STATE input only.
    if (ENABLE_CF_DEBUG_PRINTF && ift_state_in.isDefined) {
      val pa = boom.v3.util.CfStatePlusArg(true)
      when (pa && st_v) { printf("[STATE] %x\n", st_bits) }
      when (st_v) {
        assert(st_bits(3, 0) === 8.U && (st_bits(254) || st_bits(15, 12) === 0.U) && !(st_bits(10) && st_bits(11)) &&   // [15:12] = pf_id/pf_multi only with pf_drop (FIX-V2-11)
               (!st_bits(11) || st_bits(253, 247) === 0.U), "[V3LANE] malformed STATE record on the lane")
      }
      when (!st_v && sq_arb.io.out.valid) {
        assert(sq_arb.io.out.bits(3, 0) === 2.U || sq_arb.io.out.bits(3, 0) === 3.U, "[V3LANE] lane record is neither SQUASH nor KILL")
      }
    }
    // [V3LANE-END] SP-22 core prints and asserts are inserted above this line
    if (ENABLE_CF_DEBUG_PRINTF) {
      // Same tokens as the decoder's [KILLCNTD] line (dcy is the record's 3-bit bucket).
      // [R117SWEEP] patch 33: a trailing s_prop token (the record's s_prop bit) so the r117 kill
      // record fix is visible in Verilator logs; ift_core._KILLCNTD_RE uses search() and ignores it.
      when (kill_arb.io.out.fire && custom_csrs.cf_debug_rob_enable && custom_csrs.cf_debug_enable) {
        printf("[KILLCNTD] 0x%x CF(domain=%d opcount=%d) dcy=%d I0={v=1,oc=%d,ty=%d,atk=%d,sec=%d} s_prop=%d\n",
               kr.pc, kr.domain, kr.op_count, Log2Bucket(kr.deny_count),
               kr.winner_op, kr.infl_type, kr.winner_atk, kr.winner_sec, kr.winner_sec || kr.any_sec.getOrElse(false.B))
      }
    }
  } else {
    kill_srcs.foreach(_.ready := true.B)
    rob.io.ift_squash.ready := true.B
    ift_bridge_out.foreach { out => out.squash_valid := false.B; out.squash_record := 0.U }
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

  // ---------------------------------------------------------------------------
  // AutoCounter performance instrumentation (FireSim GoldenGate)
  //
  // AutoCounterCoverModuleAnnotation tells GoldenGate to instrument this module's
  // cover() calls. Without it the AutoCounterTransform silently discards all signals.
  //
  // Each cover() becomes one column in AUTOCOUNTERFILE*.csv, sampled every
  // read_rate target cycles (default 1M set in config_runtime*.yaml).
  //
  // IPC per CSV row  = (CommitSlot0 + CommitSlot1 + CommitSlot2 + CommitSlot3)
  //                    / read_rate
  // Average IPC over N rows = sum(CommitSlot*) / (N * read_rate)
  //
  // See claude-artifacts/21-autocounter-metrics.md for full signal reference.
  // ---------------------------------------------------------------------------
  annotate(new AutoCounterCoverModuleAnnotation(chisel3.Module.currentModule.get.toTarget))

  // --- Throughput ---
  // WireDefault pulls rob.io into BoomCore scope so target module matches clock for GoldenGate.
  for (w <- 0 until coreWidth) {
    val ac_commit_valid = WireDefault(rob.io.commit.arch_valids(w))
    RCcover(ac_commit_valid, s"BOOM_v3_CommitSlot$w",
      s"Instructions architecturally committed in ROB slot $w")
  }

  // --- Branch prediction ---
  RCcover(brupdate.b2.mispredict, "BOOM_v3_BrMispredict",
    "Branch resolved as mispredicted (pipeline flush imminent)")

  // --- Frontend stall ---
  RCcover(!io.ifu.fetchpacket.valid && !io.ifu.redirect_flush, "BOOM_v3_FetchStall",
    "Fetch buffer empty and no redirect in flight; decode starved")

  // --- Dispatch / structural stalls ---
  val dis_any_valid = dis_valids.reduce(_||_)
  RCcover(!rob.io.ready && dis_any_valid, "BOOM_v3_ROBFullStall",
    "ROB full; dispatch stalled with valid instructions waiting")
  RCcover(io.lsu.ldq_full.reduce(_||_) && dis_any_valid, "BOOM_v3_LDQFullStall",
    "Load queue full; dispatch stalled for a load instruction")
  RCcover(io.lsu.stq_full.reduce(_||_) && dis_any_valid, "BOOM_v3_STQFullStall",
    "Store queue full; dispatch stalled for a store instruction")

  // --- Memory subsystem ---
  RCcover(io.lsu.perf.acquire, "BOOM_v3_DCacheMiss",
    "L1 data-cache miss; TileLink acquire issued to L2")
  RCcover(io.lsu.perf.tlbMiss, "BOOM_v3_DTLBMiss",
    "Data TLB miss; page-table walk initiated")
  RCcover(io.ifu.perf.acquire, "BOOM_v3_ICacheMiss",
    "L1 instruction-cache miss; TileLink acquire issued to L2")
  RCcover(io.ifu.perf.tlbMiss, "BOOM_v3_ITLBMiss",
    "Instruction TLB miss; page-table walk initiated")
}
