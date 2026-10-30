//******************************************************************************
// Copyright (c) 2017 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.v3.common

import chisel3._
import chisel3.util.{RRArbiter, Queue}

import freechips.rocketchip.diplomacy.BundleBridgeSource

import scala.collection.mutable.{ListBuffer}

import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.devices.tilelink._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.prci._
import freechips.rocketchip.rocket._
import freechips.rocketchip.subsystem.{RocketCrossingParams}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.util._
import freechips.rocketchip.tile._

import boom.v3.exu._
import boom.v3.ifu._
import boom.v3.lsu._
import boom.v3.util.{BoomCoreStringPrefix}
import freechips.rocketchip.prci.ClockSinkParameters


case class BoomTileAttachParams(
  tileParams: BoomTileParams,
  crossingParams: RocketCrossingParams
) extends CanAttachTile {
  type TileType = BoomTile
  val lookup = PriorityMuxHartIdFromSeq(Seq(tileParams))
}


/**
 * BOOM tile parameter class used in configurations
 *
 */
case class BoomTileParams(
  core: BoomCoreParams = BoomCoreParams(),
  icache: Option[ICacheParams] = Some(ICacheParams()),
  dcache: Option[DCacheParams] = Some(DCacheParams()),
  btb: Option[BTBParams] = Some(BTBParams()),
  name: Option[String] = Some("boom_tile"),
  tileId: Int = 0
) extends InstantiableTileParams[BoomTile]
{
  require(icache.isDefined)
  require(dcache.isDefined)
  def instantiate(crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters): BoomTile = {
    new BoomTile(this, crossing, lookup)
  }
  val beuAddr: Option[BigInt] = None
  val blockerCtrlAddr: Option[BigInt] = None
  val boundaryBuffers: Boolean = false // if synthesized with hierarchical PnR, cut feed-throughs?
  val clockSinkParams: ClockSinkParameters = ClockSinkParameters()
  val baseName = name.getOrElse("boom_tile")
  val uniqueName = s"${baseName}_$tileId"
}

/**
 * BOOM tile
 *
 */
class BoomTile private(
  val boomParams: BoomTileParams,
  crossing: ClockCrossingType,
  lookup: LookupByHartIdImpl,
  q: Parameters)
  extends BaseTile(boomParams, crossing, lookup, q)
  with SinksExternalInterrupts
  with SourcesExternalNotifications
{

  // Private constructor ensures altered LazyModule.p is used implicitly
  def this(params: BoomTileParams, crossing: HierarchicalElementCrossingParamsLike, lookup: LookupByHartIdImpl)(implicit p: Parameters) =
    this(params, crossing.crossingType, lookup, p)

  val intOutwardNode = None
  val masterNode = TLIdentityNode()
  val slaveNode = TLIdentityNode()

  val tile_master_blocker =
    tileParams.blockerCtrlAddr
      .map(BasicBusBlockerParams(_, xBytes, masterPortBeatBytes, deadlock = true))
      .map(bp => LazyModule(new BasicBusBlocker(bp)))

  tile_master_blocker.foreach(lm => connectTLSlave(lm.controlNode, xBytes))

  // TODO: this doesn't block other masters, e.g. RoCCs
  tlOtherMastersNode := tile_master_blocker.map { _.node := tlMasterXbar.node } getOrElse { tlMasterXbar.node }
  masterNode :=* tlOtherMastersNode

  val cpuDevice: SimpleDevice = new SimpleDevice("cpu", Seq("ucb-bar,boom0", "riscv")) {
    override def parent = Some(ResourceAnchors.cpus)
    override def describe(resources: ResourceBindings): Description = {
      val Description(name, mapping) = super.describe(resources)
      Description(name, mapping ++
                        cpuProperties ++
                        nextLevelCacheProperty ++
                        tileProperties)
    }
  }

  ResourceBinding {
    Resource(cpuDevice, "reg").bind(ResourceAddress(tileId))
  }

  override def makeMasterBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = crossing match {
    case _: RationalCrossing =>
      if (!boomParams.boundaryBuffers) TLBuffer(BufferParams.none)
      else TLBuffer(BufferParams.none, BufferParams.flow, BufferParams.none, BufferParams.flow, BufferParams(1))
    case _ => TLBuffer(BufferParams.none)
  }

  override def makeSlaveBoundaryBuffers(crossing: ClockCrossingType)(implicit p: Parameters) = crossing match {
    case _: RationalCrossing =>
      if (!boomParams.boundaryBuffers) TLBuffer(BufferParams.none)
      else TLBuffer(BufferParams.flow, BufferParams.none, BufferParams.none, BufferParams.none, BufferParams.none)
    case _ => TLBuffer(BufferParams.none)
  }

  override lazy val module = new BoomTileModuleImp(this)

  // DCache
  lazy val dcache: BoomNonBlockingDCache = LazyModule(new BoomNonBlockingDCache(tileId))
  val dCacheTap = TLIdentityNode()
  tlMasterXbar.node := dCacheTap := TLWidthWidget(tileParams.dcache.get.rowBits/8) := visibilityNode := dcache.node


  // Frontend/ICache
  val frontend = LazyModule(new BoomFrontend(tileParams.icache.get, tileId))
  frontend.resetVectorSinkNode := resetVectorNexusNode
  tlMasterXbar.node := TLWidthWidget(tileParams.icache.get.rowBits/8) := frontend.masterNode

  require(tileParams.dcache.get.rowBits == tileParams.icache.get.rowBits)

  // ROCC
  val roccs = p(BuildRoCC).map(_(p))
  roccs.map(_.atlNode).foreach { atl => tlMasterXbar.node :=* atl }
  roccs.map(_.tlNode).foreach { tl => tlOtherMastersNode :=* tl }

  // IFT bridge: optional BundleBridgeSource exposed when enableIFTBridge=true.
  // CanHaveBoomIFTIO in chipyard creates a matching BundleBridgeSink for each such tile.
  val iftSourceNode: Option[BundleBridgeSource[boom.v3.exu.IFTTileIO]] =
    if (boomParams.core.enableIFTBridge)
      Some(BundleBridgeSource(() => new boom.v3.exu.IFTTileIO(boomParams.core.retireWidth)))
    else None
}

/**
 * BOOM tile implementation
 *
 * @param outer top level BOOM tile
 */
class BoomTileModuleImp(outer: BoomTile) extends BaseTileModuleImp(outer){

  val core = Module(new BoomCore()(outer.p))
  val lsu  = Module(new LSU()(outer.p, outer.dcache.module.edge))

  val ptwPorts         = ListBuffer(lsu.io.ptw, outer.frontend.module.io.ptw, core.io.ptw_tlb)

  val hellaCachePorts  = ListBuffer[HellaCacheIO]()

  outer.reportWFI(None) // TODO: actually report this?

  outer.decodeCoreInterrupts(core.io.interrupts) // Decode the interrupt vector

  // Pass through various external constants and reports
  outer.traceSourceNode.bundle <> core.io.trace
  outer.bpwatchSourceNode.bundle <> DontCare // core.io.bpwatch
  core.io.hartid := outer.hartIdSinkNode.bundle

  // IFT bridge: wire core IFT output to the BundleBridgeSource when enabled.
  outer.iftSourceNode.foreach { node =>
    node.bundle := core.ift_bridge_out.get
  }
  // [Task 2.10g, ruling 61; keep list narrowed in Task 2.10h, ruling 66] IFT oracle mode
  // (iftOracle): Verilator and OOC only. Only CoreFuzzingDMIConfig sets it (WithIFTOracle);
  // no FireSim config does, and BoomCoreParams forbids it together with the bridge. Keep at
  // the tile boundary exactly the fields a FireSim bridge module reads, and nothing else:
  //  - IFTBridgeModule.scala:62-85 reads commit_valid, commit_record, squash_valid and
  //    squash_record. It never reads squash_ovf (that bit stays live through 0xbce anyway).
  //  - TracerVBridgeModule (goldengateimplementations/TracerVBridge.scala) reads per lane
  //    valid (:29-31, :155), iaddr (:115-117 PC trigger, :155 stream) and insn (:126-128
  //    instruction trigger). It never reads wdata, priv, exception, interrupt, cause, tval
  //    or time. In the April 2026 FireSim synthesis each of those token channels keeps
  //    2 FF and no data bits, and the in-tile Rob has 160 LUTRAM (no debug_wdata memory).
  //    WithIFTOracle turns the core trace on, as WithTraceIO does in every FireSim config.
  // Without a consumer these are pruned by firtool and Vivado, and the BoomTile oracle
  // under-counts the FPGA build. Plain output ports plus dontTouch (a dontTouch on the
  // trace node's bundle would keep only an internal wire, which Vivado still prunes):
  // no logic is added.
  val ift_oracle_out =
    if (outer.boomParams.core.iftOracle) {
      val src = core.ift_bridge_out.get
      val o = IO(Output(new Bundle {
        val commit_valid  = chiselTypeOf(src.commit_valid)
        val commit_record = chiselTypeOf(src.commit_record)
        val squash_valid  = chiselTypeOf(src.squash_valid)
        val squash_record = chiselTypeOf(src.squash_record)
      }))
      o.commit_valid  := src.commit_valid
      o.commit_record := src.commit_record
      o.squash_valid  := src.squash_valid
      o.squash_record := src.squash_record
      dontTouch(o)
      Some(o)
    } else None
  val trace_oracle_insns =
    if (outer.boomParams.core.iftOracle) {
      val ins = outer.traceSourceNode.bundle.insns
      val o = IO(Output(Vec(ins.size, new Bundle {
        val valid = Bool()
        val iaddr = chiselTypeOf(ins.head.iaddr)
        val insn  = chiselTypeOf(ins.head.insn)
      })))
      for ((p, t) <- o.zip(ins)) { p.valid := t.valid; p.iaddr := t.iaddr; p.insn := t.insn }
      dontTouch(o)
      Some(o)
    } else None

  // [V3ASM] SP-16 (record v3 design 5.1, 5.2, 5.4, 5.5; 6.6): assembly of the one STATE record (class 8) per cycle.
  // All groups describe one event cycle E: D$ tap (E+1, SP-15) + two alignment registers here; frontend cut-out
  // (E+2, SP-18/19) + the cut-in register here; so in cycle A = E+3 the assembly register samples the record with
  // cyc = time - 3 = E.  The record leaves only in a cycle with a group event, a flag or a pending EPOCH; it takes
  // the core's squash/kill lane with priority (SP-17).  No ready anywhere: nothing waits on it (RULES B).
  // Time base: the core's free-running debug_tsc_reg (IftCoreToTile.tsc48), the counter every record class uses.
  // EPOCH every 2^cfEpochLog2 cycles, deferred to the first PRED-free cycle (gv_EPOCH and gv_PRED never both 1).
  if (outer.boomParams.core.enableIFTBridge || outer.boomParams.core.iftOracle) {
    val cfEpochLog2 = 14                                  // INDEX-C C-1: < 2^15 keeps the host unwrap exact
    val stDc  = outer.dcache.module.io.cf_state.get       // IftMemState, D$ tap register (E+1)
    val stFe  = outer.frontend.module.io.cf_state.get     // IftFeState, frontend cut-out register (E+2)
    val stCo  = core.ift_core_out.get                     // IftCoreToTile (core flops, no register here)
    val stBo  = core.ift_bridge_out.get                   // the 5 record slots as presented to the bridge / oracle
    val cf_state_al1 = RegNext(stDc, 0.U.asTypeOf(stDc))   // D$ alignment 1 (E+2)  (named cf_state_*: SP-27 timing pattern)
    val cf_state_al2 = RegNext(cf_state_al1, 0.U.asTypeOf(stDc))   // D$ alignment 2 (E+3)
    val cf_state_cin = RegNext(stFe, 0.U.asTypeOf(stFe))   // frontend cut-in (E+3)
    // records presented on the 5 slots in the cycles before this one (records_total, design 5.4)
    val stRecs = RegInit(0.U(32.W))
    stRecs := stRecs + chisel3.util.PopCount(stBo.commit_valid :+ stBo.squash_valid)
    // EPOCH: pending at reset and every 2^cfEpochLog2 cycles; emitted in the first cycle with gv_PRED = 0
    val stEpPend = RegInit(true.B)
    val stEpEmit = stEpPend && !cf_state_cin.pred.v
    when (stEpEmit) { stEpPend := false.B }
    when (stCo.tsc48(cfEpochLog2 - 1, 0).andR) { stEpPend := true.B }
    val stGv  = chisel3.util.Cat(stEpEmit, cf_state_cin.pred.v, cf_state_cin.ras.v, cf_state_cin.ic.v, cf_state_al2.l_v, cf_state_al2.m_v, cf_state_al2.r_v)   // bit 0 R .. bit 6 EPOCH
    val stCyc = stCo.tsc48(15, 0) - 3.U                   // the event cycle E (16-bit wrap)
    // PRED positions [253:135]: the EPOCH overlay (IFTRecordV3.stateEpoch, 119 b) in an EPOCH cycle, else the PRED
    // group; the SP-14 bundles are passed as asUInt (declaration order = the 5.1 range, SP-14 section 1).
    val stPredOrEpoch = Mux(stEpEmit,
      boom.v3.exu.IFTRecordV3.stateEpoch(stCo.tsc48, stRecs, stCo.sqovf16, stCo.killovf16),
      cf_state_cin.pred.g.asUInt)
    val stRec = boom.v3.exu.IFTRecordV3.state(stGv, stCo.dbg, stCyc, cf_state_al2.r.asUInt, cf_state_al2.m.asUInt, cf_state_al2.l.asUInt,
                                              cf_state_cin.ic.g.asUInt, cf_state_cin.ras.g.asUInt, stPredOrEpoch,
                                              cf_state_al2.pf_drop, cf_state_al2.wrport,
                                              cf_state_al2.pf_id, cf_state_al2.pf_multi)   // FIX-V2-11
    val stValid = stGv.orR || cf_state_al2.pf_drop || cf_state_al2.wrport
    val stAsmV  = RegNext(stValid, false.B)               // the assembly register (A -> A+1)
    val stAsmR  = chisel3.util.RegEnable(stRec, stValid)
    core.ift_state_in.get.valid := stAsmV
    core.ift_state_in.get.bits  := stAsmR
    // [V3PRINT] SP-22 (FT-4..FT-9; design 9.6-9.7): one line per group event, printed from the registers the record is
    // built from, each with c = the event cycle E (the D$ tap is seen at E+1, the frontend cut-out at E+2), so c equals
    // the record's unwrapped cyc.  Debug-print IFT builds only, +cf_state=1.  The source-side checks are the existing
    // [REPLUPD], [BTBW], [UBTBW], [TAGEW], [ICFILL], [D4RAS] prints.
    if (outer.boomParams.core.enableCfDebugPrintf && outer.boomParams.core.enableIFT) {
      val pa = boom.v3.util.CfStatePlusArg(true)
      val cD = stCo.tsc48 - 1.U
      val cF = stCo.tsc48 - 2.U
      when (pa && stDc.r_v) {
        printf("[MEM-R] c=%d kind=%d set=%d way=%d vv=%d upg=%d esec=%d eatk=%d sec=%d atk=%d dom=%d key=0x%x mshr=%d\n",
          cD, stDc.r.kind, stDc.r.set, stDc.r.way, stDc.r.vvalid, stDc.r.upg, stDc.r.esec, stDc.r.eatk,
          stDc.r.sec, stDc.r.atk, stDc.r.dom, stDc.r.key, stDc.r.mshr)
      }
      when (pa && stDc.m_v) {
        printf("[MEM-M] c=%d src=%d set=%d way=%d mshr=%d coh=%d sec=%d dom=%d\n",
          cD, stDc.m.src, stDc.m.set, stDc.m.way, stDc.m.mshr, stDc.m.coh, stDc.m.sec, stDc.m.dom)
      }
      when (pa && stDc.l_v)     { printf("[MEM-L] c=%d key=0x%x sec=%d atk=%d\n", cD, stDc.l.key, stDc.l.sec, stDc.l.atk) }
      when (pa && stDc.pf_drop) { printf("[MEM-PFDROP] c=%d id=%d multi=%d\n", cD, stDc.pf_id, stDc.pf_multi) }
      when (pa && stDc.wrport)  { printf("[MEM-WRPORT] c=%d\n", cD) }
      when (pa && stFe.ic.v) {
        printf("[FE-IC] c=%d inv=%d drop=%d dom=%d sec=%d set=%d way=%d hi=%d\n",
          cF, stFe.ic.g.inval, stFe.ic.g.drop, stFe.ic.g.dom, stFe.ic.g.sec, stFe.ic.g.set, stFe.ic.g.way, stFe.ic.g.hi)
      }
      when (pa && stFe.ras.v) {
        printf("[FE-RAS] c=%d push=%d sec=%d watk=%d dom=%d idx=%d addr12=0x%x\n",
          cF, stFe.ras.g.push, stFe.ras.g.sec, stFe.ras.g.watk, stFe.ras.g.dom, stFe.ras.g.idx, stFe.ras.g.addr12)
      }
      when (pa && stFe.pred.v) {
        val g = stFe.pred.g
        printf("[PRED-U] c=%d kind=%d pc12=0x%x brm=0x%x cfi=%d/%d tk=%d mp=%d br=%d jal=%d sec=%d dom=%d rst=%d " +
               "btb0=%d/0x%x/%d btb1=%d/0x%x/%d ubtb0=%d/%d ubtb1=%d/%d prov=0x%x alloc=%d/%d/%d udec=0x%x/%d nst=%d nsnt=%d h16=0x%x\n",
          cF, g.u_kind, g.u_pc12, g.u_brmask, g.u_cfi_v, g.u_cfi_idx, g.u_taken, g.u_mispred, g.u_isbr, g.u_isjal,
          g.u_sec, g.u_dom, g.btb_rst, g.btb0_v, g.btb0_set, g.btb0_way, g.btb1_v, g.btb1_set, g.btb1_way,
          g.ubtb0_v, g.ubtb0_way, g.ubtb1_v, g.ubtb1_way, g.tg_prov, g.tg_alloc_v, g.tg_alloc_table, g.tg_alloc_slot,
          g.tg_udec_mask, g.tg_udec_slot, g.tg_nst, g.tg_nsnt, g.tg_hist16)
      }
      when (pa && stEpEmit) {
        printf("[EPOCH] c48=%d recs=%d sqovf=%d killovf=%d\n", stCo.tsc48, stRecs, stCo.sqovf16, stCo.killovf16)
      }
      when (pa && stBo.squash_valid) {
        printf("[LANE] c=%d commits=%d cls=%d\n", stCo.tsc48, chisel3.util.PopCount(stBo.commit_valid), stBo.squash_record(3, 0))
      }
      assert(stCo.tsc48(15, 0) === stCo.cyc16, "[V3ASM] core ift_cyc16 != debug_tsc_reg(15,0): the record time bases differ")
    }
    // [V3ASM-END] SP-22 tile prints and asserts are inserted above this line
  }
  // Connect the core pipeline to other intra-tile modules
  outer.frontend.module.io.cpu <> core.io.ifu
  core.io.lsu <> lsu.io.core

  //fpuOpt foreach { fpu => core.io.fpu <> fpu.io } RocketFpu - not needed in boom
  core.io.rocc := DontCare

  // RoCC
  if (outer.roccs.size > 0) {
    val (respArb, cmdRouter) = {
      val respArb = Module(new RRArbiter(new RoCCResponse()(outer.p), outer.roccs.size))
      val cmdRouter = Module(new RoccCommandRouter(outer.roccs.map(_.opcodes))(outer.p))
      outer.roccs.zipWithIndex.foreach { case (rocc, i) =>
        ptwPorts ++= rocc.module.io.ptw
        rocc.module.io.cmd <> cmdRouter.io.out(i)
        val dcIF = Module(new SimpleHellaCacheIF()(outer.p))
        dcIF.io.requestor <> rocc.module.io.mem
        hellaCachePorts += dcIF.io.cache
        respArb.io.in(i) <> Queue(rocc.module.io.resp)
      }
      // first keep fpu ios unconnected
      val fp_ios = outer.roccs.map(r => {
        val roccio = r.module.io
        roccio.fpu_req.ready := true.B
        roccio.fpu_resp.valid := false.B
        roccio.fpu_resp.bits := DontCare
      })
      // Create this FPU just for RoCC
      val nFPUPorts = outer.roccs.filter(_.usesFPU).size
      if (nFPUPorts > 0) {
        val fpuOpt = outer.tileParams.core.fpu.map(params => Module(new freechips.rocketchip.tile.FPU(params)(outer.p)))
        // TODO: Check this FPU works properly
        fpuOpt foreach { fpu =>
          // This FPU does not get CPU requests
          fpu.io := DontCare
          fpu.io.fcsr_rm := core.io.fcsr_rm
          fpu.io.ll_resp_val := false.B
          fpu.io.valid := false.B
          fpu.io.killx := false.B
          fpu.io.killm := false.B

          val fpArb = Module(new InOrderArbiter(new FPInput()(outer.p), new FPResult()(outer.p), nFPUPorts))
          val fp_rocc_ios = outer.roccs.filter(_.usesFPU).map(_.module.io)
          fpArb.io.in_req <> fp_rocc_ios.map(_.fpu_req)
          fp_rocc_ios.zip(fpArb.io.in_resp).foreach {
            case (rocc, arb) => rocc.fpu_resp <> arb
          }
          fpu.io.cp_req <> fpArb.io.out_req
          fpArb.io.out_resp <> fpu.io.cp_resp
        }
      }
      (respArb, cmdRouter)
    }

    cmdRouter.io.in <> core.io.rocc.cmd
    outer.roccs.foreach(_.module.io.exception := core.io.rocc.exception)
    core.io.rocc.resp <> respArb.io.out
    core.io.rocc.busy <> (cmdRouter.io.busy || outer.roccs.map(_.module.io.busy).reduce(_||_))
    core.io.rocc.interrupt := outer.roccs.map(_.module.io.interrupt).reduce(_||_)
  }

  // PTW
  val ptw  = Module(new PTW(ptwPorts.length)(outer.dcache.node.edges.out(0), outer.p))
  core.io.ptw <> ptw.io.dpath
  ptw.io.requestor <> ptwPorts.toSeq
  ptw.io.mem +=: hellaCachePorts

   // LSU IO
  val hellaCacheArb = Module(new HellaCacheArbiter(hellaCachePorts.length)(outer.p))
  hellaCacheArb.io.requestor <> hellaCachePorts.toSeq
  lsu.io.hellacache <> hellaCacheArb.io.mem
  outer.dcache.module.io.lsu <> lsu.io.dmem

  // Generate a descriptive string
  val frontendStr = outer.frontend.module.toString
  val coreStr = core.toString
  val dcacheStr = outer.dcache.module.toString

  val boomTileStr =
    (BoomCoreStringPrefix(s"======BOOM Tile ${outer.tileId} Params======") + "\n"
    + frontendStr
    + coreStr 
    + dcacheStr + "\n")

  override def toString: String = boomTileStr

  print(boomTileStr)
}
