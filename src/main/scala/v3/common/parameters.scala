//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.v3.common

import chisel3._
import chisel3.util._

import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.subsystem.{MemoryPortParams}
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.devices.tilelink.{BootROMParams, CLINTParams, PLICParams}

import boom.v3.ifu._
import boom.v3.exu._
import boom.v3.lsu._
import os.write.over

/**
 * Default BOOM core parameters
 */
case class BoomCoreParams(
// DOC include start: BOOM Parameters
  pgLevels: Int = 3,
  fetchWidth: Int = 1,
  decodeWidth: Int = 1,
  numRobEntries: Int = 64,
  issueParams: Seq[IssueParams] = Seq(
    IssueParams(issueWidth=1, numEntries=16, iqType=IQT_MEM.litValue, dispatchWidth=1),
    IssueParams(issueWidth=2, numEntries=16, iqType=IQT_INT.litValue, dispatchWidth=1),
    IssueParams(issueWidth=1, numEntries=16, iqType=IQT_FP.litValue , dispatchWidth=1)),
  numLdqEntries: Int = 16,
  numStqEntries: Int = 16,
  numIntPhysRegisters: Int = 96,
  numFpPhysRegisters: Int = 64,
  maxBrCount: Int = 4,
  numFetchBufferEntries: Int = 16,
  enableAgePriorityIssue: Boolean = true,
  enablePrefetching: Boolean = false,
  enableFastLoadUse: Boolean = true,
  enableCommitMapTable: Boolean = false,
  enableFastPNR: Boolean = false,
  enableSFBOpt: Boolean = false,
  enableGHistStallRepair: Boolean = true,
  enableBTBFastRepair: Boolean = true,
  useAtomicsOnlyForIO: Boolean = false,
  ftq: FtqParameters = FtqParameters(),
  intToFpLatency: Int = 2,
  imulLatency: Int = 3,
  nPerfCounters: Int = 0,
  numRXQEntries: Int = 4,
  numRCQEntries: Int = 8,
  numDCacheBanks: Int = 1,
  nPMPs: Int = 8,
  enableICacheDelay: Boolean = false,

  /* branch prediction */
  enableBranchPrediction: Boolean = true,
  branchPredictor: Function2[BranchPredictionBankResponse, Parameters, Tuple2[Seq[BranchPredictorBank], BranchPredictionBankResponse]] = ((resp_in: BranchPredictionBankResponse, p: Parameters) => (Nil, resp_in)),
  globalHistoryLength: Int = 64,
  localHistoryLength: Int = 32,
  localHistoryNSets: Int = 128,
  bpdMaxMetaLength: Int = 120,
  numRasEntries: Int = 32,
  enableRasTopRepair: Boolean = true,

  /* more stuff */
  useCompressed: Boolean = true,
  useFetchMonitor: Boolean = true,
  bootFreqHz: BigInt = 0,
  fpu: Option[FPUParams] = Some(FPUParams(sfmaLatency=4, dfmaLatency=4)),
  usingFPU: Boolean = true,
  haveBasicCounters: Boolean = true,
  misaWritable: Boolean = false,
  mtvecInit: Option[BigInt] = Some(BigInt(0)),
  mtvecWritable: Boolean = true,
  haveCFlush: Boolean = false,
  mulDiv: Option[freechips.rocketchip.rocket.MulDivParams] = Some(MulDivParams(divEarlyOut=true)),
  nBreakpoints: Int = 0, // TODO Fix with better frontend breakpoint unit
  nL2TLBEntries: Int = 512,
  val nPTECacheEntries: Int = 8, // TODO: check
  nL2TLBWays: Int = 1,
  nLocalInterrupts: Int = 0,
  useNMI: Boolean = false,
  useAtomics: Boolean = true,
  useDebug: Boolean = true,
  useUser: Boolean = true,
  useSupervisor: Boolean = false,
  useHypervisor: Boolean = false,
  useVM: Boolean = true,
  useSCIE: Boolean = false,
  useRVE: Boolean = false,
  useBPWatch: Boolean = false,
  clockGate: Boolean = false,
  mcontextWidth: Int = 0,
  scontextWidth: Int = 0,
  trace: Boolean = false,

  /* debug stuff */
  enableCommitLogPrintf: Boolean = true,
  enableBranchPrintf: Boolean = true,
  enableMemtracePrintf: Boolean = true,

  /* IFT bridge: export IFT commit/squash records as tile IO for FireSim IFTBridge.
   * When true, BoomCore exposes ift_bridge_out IO and BoomTile creates a
   * BundleBridgeSource that CanHaveBoomIFTIO in chipyard sinks at the system level.
   * Has no effect on Verilator simulation (CoreFuzzingConfig leaves this false). */
  enableIFTBridge: Boolean = false,

  /* IFT: compile-time gate for DIFT tracking logic (domain tagging, taint
   * propagation, influencer tracking, secret detection, IFT commit log).
   * When false, all IFT state and logic is elided at FIRRTL emission time.
   * Set via WithIFT config fragment. */
  enableIFT: Boolean = false,

  /* Reconf: compile-time gate for runtime reconfigurability (dynamic structure
   * sizing CSRs, quiesce-for-resize FSM, index masking, pointer wrapping with
   * dynamic limits). When false, all reconfiguration CSRs are absent and
   * structures use their hardware-built maximum sizes.
   * Set via WithReconf config fragment. */
  enableReconf: Boolean = false

// DOC include end: BOOM Parameters
) extends freechips.rocketchip.tile.CoreParams
{
  require(!enableIFTBridge || enableIFT,
    "enableIFTBridge requires enableIFT (cannot export IFT records without IFT tracking)")

  override def traceCustom = Some(new BoomTraceBundle)
  val xLen = 64
  val haveFSDirty = true
  val pmpGranularity: Int = 4
  val instBits: Int = 16
  val lrscCycles: Int = 80 // worst case is 14 mispredicted branches + slop
  val retireWidth = decodeWidth
  val jumpInFrontend: Boolean = false // unused in boom
  val traceHasWdata = trace
  val useConditionalZero = false
  val useZba = false
  val useZbb = false
  val useZbs = false
  override val useVector = false
  override def customCSRs(implicit p: Parameters) = new BoomCustomCSRs
}

class BoomTraceBundle extends Bundle {
  val rob_empty = Bool()
}

/**
  * Defines custom BOOM CSRs
  */
class BoomCustomCSRs(implicit p: Parameters) extends freechips.rocketchip.tile.CustomCSRs
  with HasBoomCoreParameters 
  with CoreFuzzingConstants {
  override def chickenCSR = {
    val params = tileParams.core.asInstanceOf[BoomCoreParams]
    val mask = BigInt(
      tileParams.dcache.get.clockGate.toInt << 0 |
      params.clockGate.toInt << 1 |
      params.clockGate.toInt << 2 |
      1 << 3 | // Disable OOO when this bit is high
      1 << 4 // disable printing of corefuzzing debug logs (printfs) when this bit is low
    )
    val init = BigInt(
      tileParams.dcache.get.clockGate.toInt << 0 |
      params.clockGate.toInt << 1 |
      params.clockGate.toInt << 2 |
      0 << 3 | // Enable OOO at init
      1 << 4 // debug log is enabled at init
    )
    Some(CustomCSR(chickenCSRId, mask, Some(init)))
  }

  /**
  * Define a custom CSR for Reconfiguring the design at runtime with Machine
  * software. The address of the CSR is 0x7c2
  */
  /* We use multiple CSRs for the configurations
  * The first CSR - bpdCSR
  */
  override def bpdCSRCF = { 
    val mask = BigInt(1 << 2) // Switch Tage to Gshare
    val init = BigInt(0 << 2) // at Initialization
    Some(CustomCSR(bpdCSRIdCF, mask, Some(init)))
  }

  // second CSR - fetch_bufferCSR
  // 3-bit binary index into fetchBufferEntryOptions = Seq(64, 48, 32, 24, 16, 8)
  // init = 0x0 → index 0 = 64 entries (max, hardware-built size)
  override def fetchBufferCSRCF = {
    val mask = BigInt(0x7) // 3-bit index
    val init = BigInt(0x0) // index 0 = max (128 entries)
    Some(CustomCSR(fetchBufferCSRIdCF, mask, Some(init)))
  }
  def cf_fetch_buffer_idx = getOrElse(fetchBufferCSRCF, _.value(2,0), 0.U)

  // load/store queue CSR
  // bits [2:0] = LDQ index into ldQueueEntryOptions = Seq(64, 32, 24, 16, 8)
  // bits [5:3] = STQ index into stQueueEntryOptions = Seq(64, 32, 24, 16, 8)
  // init = 0x0 → both queues at index 0 = max (64 entries each)
  override def ldqStqCSRCF = {
    val mask = BigInt(0x3F) // 6-bit: 3 bits for LDQ + 3 bits for STQ
    val init = BigInt(0x0)  // index 0,0 = both at max (64 entries)
    Some(CustomCSR(ldqStqCSRIdCF, mask, Some(init)))
  }
  def cf_ldq_idx = getOrElse(ldqStqCSRCF, _.value(2,0), 0.U)
  def cf_stq_idx = getOrElse(ldqStqCSRCF, _.value(5,3), 0.U)
  // def reconfigureBPD = getOrElse(configureCSR, _.value(2), true.B)
  
  def cf_bpd_tage_to_gshare = getOrElse(bpdCSRCF, _.value(2), false.B)
  def disableOOO = getOrElse(chickenCSR, _.value(3), true.B)
  def marchid = CustomCSR.constant(CSRs.marchid, BigInt(2))
  // core fuzzing specific
  // def cf_debug_log = getOrElse(chickenCSR, _.value(4), true.B)

  // DCache reconfiguration CSR (0xbc1)
  // bits [1:0]: set index into dcacheSetOptions = Seq(128, 64, 32, 16)  — index 0 = max
  // bits [3:2]: way index into cacheWayOptions  = Seq(8, 4, 2, 1)       — index 0 = max
  // bits [5:4]: replacement policy (0=random, 1=TrueLRU, 2=PseudoLRU)
  // Block size is NOT reconfigurable (cacheBlockBytes fixed).
  override def dcacheCSRCF = {
    val mask = BigInt(0x3F)  // 6 bits: 2 for sets, 2 for ways, 2 for replacement policy
    val init = BigInt(0x0)   // index (0,0,0) = max sizes, random replacement
    Some(CustomCSR(dcacheCSRIdCF, mask, Some(init)))
  }
  // 2-bit index accessors (used directly in dcache.scala)
  def cf_dcache_set_conf  = getOrElse(dcacheCSRCF, _.value(1,0), 0.U)
  def cf_dcache_way_conf  = getOrElse(dcacheCSRCF, _.value(3,2), 0.U)
  def cf_dcache_repl_conf = getOrElse(dcacheCSRCF, _.value(5,4), 0.U)

  // ICache reconfiguration CSR (0xbcb)
  // bits [1:0]: set index into icacheSetOptions = Seq(64, 32, 16, 8)  — index 0 = max
  // bits [3:2]: way index into cacheWayOptions  = Seq(8, 4, 2, 1)     — index 0 = max
  override def icacheCSRCF = {
    val mask = BigInt(0xF)
    val init = BigInt(0x0)
    Some(CustomCSR(icacheCSRIdCF, mask, Some(init)))
  }
  def cf_icache_set_conf = getOrElse(icacheCSRCF, _.value(1,0), 0.U)
  def cf_icache_way_conf = getOrElse(icacheCSRCF, _.value(3,2), 0.U)
  
  override def debugCSRCF = {
  // CSR cf_debug_log 
  // id : 0xbc1
  // 0 - Core  Fuzzing debug enable
  // 1 - Dcache logs
  // 2 - LSU logs
  // 3 - Core logs
  // 4 - ROB logs
  // 5 - BPD logs

  // The generateCustomCSR function in rocket/CSR.scala (line 750) requires the mask 
  // value of a CSR is >= 0. Having the mask to be 0xffffffff violates that requirement
  // hence we forgo the MSB and we do not use it.
  // Moreover, we do not need 31 bits for the debug log enables. 
    val mask = BigInt(0x7FFFFFFF)
    val init = BigInt(
      1 << 0 | // global debug enable
      1 << 1 | // dcache log enabled at init
      1 << 2 | // lsu log enabled at init
      1 << 3 | // core log enabled at init
      1 << 4 | // rob log enabled at init
      1 << 5 | // bpd log enabled at init
      1 << 6   // frontend log enabled at init
    )
    Some(CustomCSR(debugCSRIdCF, mask, Some(init)))
  }
 
  override def robSizeCSRCF = {
    // 3-bit binary index into robEntryOptions = Seq(512, 256, 192, 128, 96, 64, 32)
    // init = 0x0 → index 0 = 512 entries (max, hardware-built size)
    val mask = BigInt(0x7)   // 3-bit index, 7 options (indices 0-6)
    val init = BigInt(0x0)   // index 0 = max (512 entries)
    Some(CustomCSR(robSizeCSRIdCF, mask, Some(init)))
  }

  def cf_rob_entries = getOrElse(robSizeCSRCF, _.value, 0.U)
  // move this to ROB
  // Helper to decode one-hot CSR value to actual entry count
  // val options = robEntryOptions.map(_.U)
  // val entryCount = options.zipWithIndex.map { case (size, idx) => Mux(robCSRVal(idx), size.U, 0.U) }.reduce(_ | _)
  // def cf_rob_rows = entryCount / coreWidth.U
  // quiesce csr

  override def chillCSRCF = {
    val mask = BigInt(0x1)
    val init = BigInt(0 << 0) // the core need not be quiesced at init.
    Some(CustomCSR(chillCSRIdCF, mask, Some(init)))
  }

  def cf_chill = getOrElse(chillCSRCF, _.value(0), false.B)

  override def attackerAddrStartCSRCF = {
    val mask = BigInt(0x7FFFFFFF)
    val init = BigInt(0x0) // the default value is 0x0 here 
    // the idea is that the program will write the offset of 
    // the attacker start address offset from the base address (0x80000000)
    Some(CustomCSR(attackerAddrStartCSRIdCF, mask, Some(init)))
  }

  override def attackerAddrEndCSRCF = {
    val mask = BigInt(0x7FFFFFFF)
    val init = BigInt(0x0) // the default value is 0x0 here 
    // the idea is that the program will write the offset of 
    // the attacker end address offset from the base address (0x80000000)
    // Question: do we need this offset to be from the start address?? probably not. I don't think we 
    // currently model such a large attacker code region
    Some(CustomCSR(attackerAddrEndCSRIdCF, mask, Some(init)))
  }

  def cf_attacker_start_addr = 0x80000000L.U(32.W) + getOrElse(attackerAddrStartCSRCF, _.value, 0.U)
  def cf_attacker_end_addr = 0x80000000L.U(32.W) + getOrElse(attackerAddrEndCSRCF, _.value, 0.U)

  override def secretAddrStartCSRCF = {
    val mask = BigInt(0x7FFFFFFF)
    val init = BigInt(0x0) // the default value is 0x0 here 
    // the idea is that the program will write the offset of 
    // the secret start address offset from the base address (0x80000000)
    Some(CustomCSR(secretAddrStartCSRIdCF, mask, Some(init)))
  }

  override def secretAddrEndCSRCF = {
    val mask = BigInt(0x7FFFFFFF)
    val init = BigInt(0x0) // the default value is 0x0 here 
    // the idea is that the program will write the offset of 
    // the secret end address offset from the base address (0x80000000)
    Some(CustomCSR(secretAddrEndCSRIdCF, mask, Some(init)))
  }

  def cf_secret_start_addr = 0x80000000L.U(32.W) + getOrElse(secretAddrStartCSRCF, _.value, 0.U)
  def cf_secret_end_addr = 0x80000000L.U(32.W) + getOrElse(secretAddrEndCSRCF, _.value, 0.U)

  override def attackStageCSRCF = {
    val mask = BigInt(0x3) // two bits bit 0 - attack start indicator. Bit 1 - start secret access tagging
    val init = BigInt(0x0)
    Some(CustomCSR(attackStageCSRIdCF, mask, Some(init)))
  }

  def cf_start_attack = getOrElse(attackStageCSRCF, _.value(0), false.B)
  def cf_start_secret_tagging = getOrElse(attackStageCSRCF, _.value(1), false.B)

  // FTQ size CSR — 2-bit index into ftQueueEntryOptions = Seq(32, 24, 16, 8)
  override def ftqSizeCSRCF = {
    val mask = BigInt(0x3)   // 2-bit index, 4 options
    val init = BigInt(0x0)   // index 0 = 32 entries (max)
    Some(CustomCSR(ftqSizeCSRIdCF, mask, Some(init)))
  }
  def cf_ftq_idx = getOrElse(ftqSizeCSRCF, _.value(1,0), 0.U)

  // RAS entry count CSR — 2-bit index into rasEntryCountOptions = Seq(32, 16, 8, 4)
  override def rasCountCSRCF = {
    val mask = BigInt(0x3)   // 2-bit index, 4 options
    val init = BigInt(0x0)   // index 0 = 32 entries (max)
    Some(CustomCSR(rasCountCSRIdCF, mask, Some(init)))
  }
  def cf_ras_idx = getOrElse(rasCountCSRCF, _.value(1,0), 0.U)

  // PRF size CSR — 3-bit index into pregFileSizeOptions = Seq(256, 128, 96, 64, 48)
  override def pregSizeCSRCF = {
    val mask = BigInt(0x7)   // 3-bit index, 5 options
    val init = BigInt(0x0)   // index 0 = 256 registers (max)
    Some(CustomCSR(pregSizeCSRIdCF, mask, Some(init)))
  }
  def cf_preg_idx = getOrElse(pregSizeCSRCF, _.value(2,0), 0.U)

  // Issue queue size CSR — 2-bit index into issueQueueEntryOptions = Seq(64, 32, 16, 8)
  override def issueQueueCSRCF = {
    val mask = BigInt(0x3)   // 2-bit index, 4 options
    val init = BigInt(0x0)   // index 0 = 64 entries (max)
    Some(CustomCSR(issueQueueCSRIdCF, mask, Some(init)))
  }
  def cf_iq_idx = getOrElse(issueQueueCSRCF, _.value(1,0), 0.U)

  // BTB config CSR — bits [1:0] = set index (btbSetOptions = Seq(128, 64, 32)),
  //                  bit [2] = way index (btbWayOptions = Seq(2, 1))
  override def btbConfigCSRCF = {
    val mask = BigInt(0x7)   // 3-bit
    val init = BigInt(0x0)   // index 0 = 128 sets, 2 ways (max)
    Some(CustomCSR(btbConfigCSRIdCF, mask, Some(init)))
  }
  def cf_btb_set_idx = getOrElse(btbConfigCSRCF, _.value(1,0), 0.U)
  def cf_btb_way_idx = getOrElse(btbConfigCSRCF, _.value(2),   0.U)

  // TAGE table count CSR — 3-bit index into tagetableCountOptions = Seq(7, 6, 5, 3, 2, 1)
  // cf_tage_active values >3 include table 3's slot; table 3 is excluded in TAGE mode
  // (it is the GShare bank, enabled only via bpdCSRCF bit[2]).
  // GShare mode is controlled separately by bpdCSRCF (0x7c2) bit[2].
  override def tageCountCSRCF = {
    val mask = BigInt(0x7)   // 3-bit index (max value = 7, fits in 3 bits)
    val init = BigInt(0x0)   // index 0 = cf_tage_active=7 = 6 real TAGE tables (max)
    Some(CustomCSR(tageCountCSRIdCF, mask, Some(init)))
  }
  def cf_tage_count_idx = getOrElse(tageCountCSRCF, _.value(2,0), 0.U)

  override def decls: Seq[CustomCSR] = super.decls :+ marchid

  def cf_debug_enable = getOrElse(debugCSRCF, _.value(0), true.B)
  def cf_debug_dcache_enable = getOrElse(debugCSRCF, _.value(1), false.B)
  def cf_debug_lsu_enable = getOrElse(debugCSRCF, _.value(2), false.B)
  def cf_debug_core_enable = getOrElse(debugCSRCF, _.value(3), true.B)
  def cf_debug_rob_enable = getOrElse(debugCSRCF, _.value(4), true.B)
  def cf_debug_bpd_enable = getOrElse(debugCSRCF, _.value(5), false.B)
  def cf_debug_frontend_enable = getOrElse(debugCSRCF, _.value(6), false.B)

  // Core-width reconfiguration CSR (0xbcc)
  // 2-bit index selects effective decode width: 0→N=4, 1→N=2, 2→N=1; index 3 is reserved.
  // Note: mask=0x3 is unavoidable (need bit[1] for index 2=N=1); hardware asserts idx<=2.
  override def coreWidthCSRCF = {
    val mask = BigInt(0x3)   // 2-bit; indices 0-2 valid, index 3 reserved
    val init = BigInt(0x0)   // index 0 = N=4 (full width, default)
    Some(CustomCSR(coreWidthCSRIdCF, mask, Some(init)))
  }
  def cf_core_width_idx = getOrElse(coreWidthCSRCF, _.value(1,0), 0.U)
  // Derive active width as a hardware signal: 0→4, 1→2, 2→1
  def cf_active_width: UInt = MuxLookup(cf_core_width_idx, coreWidth.U)(
    Seq(1.U -> 2.U, 2.U -> 1.U))
}

/**
 * Mixin trait to add BOOM parameters to expand other traits/objects/etc
 */
trait HasBoomCoreParameters extends freechips.rocketchip.tile.HasCoreParameters
{
  val boomParams: BoomCoreParams = tileParams.core.asInstanceOf[BoomCoreParams]

  //************************************
  // Superscalar Widths

  // fetchWidth provided by CoreParams class.
  // decodeWidth provided by CoreParams class.

  // coreWidth is width of decode, width of integer rename, width of ROB, and commit width
  val coreWidth = decodeWidth

  require (isPow2(fetchWidth))
  require (coreWidth <= fetchWidth)

  //************************************
  // Data Structure Sizes
  val numRobEntries = boomParams.numRobEntries       // number of ROB entries (e.g., 32 entries for R10k)
  val numRxqEntries = boomParams.numRXQEntries       // number of RoCC execute queue entries. Keep small since this holds operands and instruction bits
  val numRcqEntries = boomParams.numRCQEntries       // number of RoCC commit queue entries. This can be large since it just keeps a pdst
  val numLdqEntries = boomParams.numLdqEntries       // number of LAQ entries
  val numStqEntries = boomParams.numStqEntries       // number of SAQ/SDQ entries
  val maxBrCount    = boomParams.maxBrCount          // number of branches we can speculate simultaneously
  val ftqSz         = boomParams.ftq.nEntries        // number of FTQ entries
  val numFetchBufferEntries = boomParams.numFetchBufferEntries // number of instructions that stored between fetch&decode

  val numIntPhysRegs= boomParams.numIntPhysRegisters // size of the integer physical register file
  val numFpPhysRegs = boomParams.numFpPhysRegisters  // size of the floating point physical register file

  //************************************
  // Functional Units
  val usingFDivSqrt = boomParams.fpu.isDefined && boomParams.fpu.get.divSqrt

  val mulDivParams = boomParams.mulDiv.getOrElse(MulDivParams())
  val trace = boomParams.trace
  // TODO: Allow RV32IF
  require(!(xLen == 32 && usingFPU), "RV32 does not support fp")

  //************************************
  // Pipelining

  val imulLatency = boomParams.imulLatency
  val dfmaLatency = if (boomParams.fpu.isDefined) boomParams.fpu.get.dfmaLatency else 3
  val sfmaLatency = if (boomParams.fpu.isDefined) boomParams.fpu.get.sfmaLatency else 3
  // All FPU ops padded out to same delay for writeport scheduling.
  require (sfmaLatency == dfmaLatency)

  val intToFpLatency = boomParams.intToFpLatency

  //************************************
  // Issue Units

  val issueParams: Seq[IssueParams] = boomParams.issueParams
  val enableAgePriorityIssue = boomParams.enableAgePriorityIssue

  // currently, only support one of each.
  require (issueParams.count(_.iqType == IQT_FP.litValue) == 1 || !usingFPU)
  require (issueParams.count(_.iqType == IQT_MEM.litValue) == 1)
  require (issueParams.count(_.iqType == IQT_INT.litValue) == 1)

  val intIssueParam = issueParams.find(_.iqType == IQT_INT.litValue).get
  val memIssueParam = issueParams.find(_.iqType == IQT_MEM.litValue).get

  val intWidth = intIssueParam.issueWidth
  val memWidth = memIssueParam.issueWidth

  issueParams.map(x => require(x.dispatchWidth <= coreWidth && x.dispatchWidth > 0))

  //************************************
  // Load/Store Unit
  val dcacheParams: DCacheParams = tileParams.dcache.get
  val icacheParams: ICacheParams = tileParams.icache.get
  val icBlockBytes = icacheParams.blockBytes

  require(icacheParams.nSets <= 64, "Handling aliases in the ICache is buggy.")

  val enableFastLoadUse = boomParams.enableFastLoadUse
  val enablePrefetching = boomParams.enablePrefetching
  val nLBEntries = dcacheParams.nMSHRs

  //************************************
  // Branch Prediction
  val globalHistoryLength = boomParams.globalHistoryLength
  val localHistoryLength = boomParams.localHistoryLength
  val localHistoryNSets = boomParams.localHistoryNSets
  val bpdMaxMetaLength = boomParams.bpdMaxMetaLength

  def getBPDComponents(resp_in: BranchPredictionBankResponse, p: Parameters) = {
    boomParams.branchPredictor(resp_in, p)
  }

  val nRasEntries = boomParams.numRasEntries max 2
  val useRAS = boomParams.numRasEntries > 0
  val enableRasTopRepair = boomParams.enableRasTopRepair

  val useBPD = boomParams.enableBranchPrediction

  val useLHist = localHistoryNSets > 1 && localHistoryLength > 1

  //************************************
  // Extra Knobs and Features
  val enableCommitMapTable = boomParams.enableCommitMapTable
  require(!enableCommitMapTable) // TODO Fix the commit map table.
  val enableFastPNR = boomParams.enableFastPNR
  val enableSFBOpt = boomParams.enableSFBOpt
  val enableGHistStallRepair = boomParams.enableGHistStallRepair
  val enableBTBFastRepair = boomParams.enableBTBFastRepair

  //************************************
  // Implicitly calculated constants
  val numRobRows      = numRobEntries/coreWidth
  val robAddrSz       = log2Ceil(numRobRows) + log2Ceil(coreWidth)
  // the f-registers are mapped into the space above the x-registers
  val logicalRegCount = if (usingFPU) 64 else 32
  val lregSz          = log2Ceil(logicalRegCount)
  val ipregSz         = log2Ceil(numIntPhysRegs)
  val fpregSz         = log2Ceil(numFpPhysRegs)
  val maxPregSz       = ipregSz max fpregSz
  val ldqAddrSz       = log2Ceil(numLdqEntries)
  val stqAddrSz       = log2Ceil(numStqEntries)
  val lsuAddrSz       = ldqAddrSz max stqAddrSz
  val brTagSz         = log2Ceil(maxBrCount)

  require (numIntPhysRegs >= (32 + coreWidth))
  require (numFpPhysRegs >= (32 + coreWidth))
  require (maxBrCount >=2)
  require (numRobEntries % coreWidth == 0)
  require ((numLdqEntries-1) > coreWidth)
  require ((numStqEntries-1) > coreWidth)

  //***********************************
  // Debug printout parameters
  val COMMIT_LOG_PRINTF   = boomParams.enableCommitLogPrintf // dump commit state, for comparision against ISA sim
  val BRANCH_PRINTF       = boomParams.enableBranchPrintf // dump branch predictor results
  val MEMTRACE_PRINTF     = boomParams.enableMemtracePrintf // dump trace of memory accesses to L1D for debugging
  val ENABLE_IFT_BRIDGE   = boomParams.enableIFTBridge // export IFT records as tile IO for FireSim IFTBridge
  val ENABLE_IFT          = boomParams.enableIFT       // compile-time gate for DIFT tracking logic
  val ENABLE_RECONF       = boomParams.enableReconf    // compile-time gate for runtime reconfigurability

  //************************************
  // Other Non/Should-not-be sythesizable modules
  val useFetchMonitor = boomParams.useFetchMonitor

  //************************************
  // Non-BOOM parameters

  val corePAddrBits = paddrBits
  val corePgIdxBits = pgIdxBits
}
