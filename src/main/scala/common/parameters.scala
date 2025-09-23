//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.common

import chisel3._
import chisel3.util._

import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.subsystem.{MemoryPortParams}
import org.chipsalliance.cde.config.{Parameters, Field}
import freechips.rocketchip.devices.tilelink.{BootROMParams, CLINTParams, PLICParams}

import boom.ifu._
import boom.exu._
import boom.lsu._

/**
 * Default BOOM core parameters
 */
case class BoomCoreParams(
// DOC include start: BOOM Parameters
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
  enableMemtracePrintf: Boolean = true 

// DOC include end: BOOM Parameters
) extends freechips.rocketchip.tile.CoreParams
{
  override def traceCustom = Some(new BoomTraceBundle)
  val haveFSDirty = true
  val pmpGranularity: Int = 4
  val instBits: Int = 16
  val lrscCycles: Int = 80 // worst case is 14 mispredicted branches + slop
  val retireWidth = decodeWidth
  val jumpInFrontend: Boolean = false // unused in boom
  val useBitManip = false
  val useBitManipCrypto = false
  val useCryptoNIST = false
  val useCryptoSM = false
  val traceHasWdata = trace
  val useConditionalZero = false

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

  // alex-stuff
  // second CSR - fetch_bufferCSR
  override def fetch_bufferCSR = {
    val mask = BigInt(1 << 1 | 1 << 0) // bits 1 and 0 for numRows reconfiguration
    val init = BigInt(1 << 1 | 1 << 0) // at initialization
    Some(CustomCSR(fetch_bufferCSRId, mask, Some(init)))
  }

  // load/store queue CSR
  override def ldq_stq_CSR = {
    val mask = BigInt(1 << 3 | 1 << 2 | 1 << 1 | 1 << 0) // for right now, 2 bits (<<2) for stq length reconfig, 2 bits (<<0) for ldq len
    val init = BigInt(1 << 3 | 1 << 2 | 1 << 1 | 1 << 0) // at initialization
    Some(CustomCSR(ldq_stq_CSRId, mask, Some(init)))
  }

  def reconfig_stq_b1 = getOrElse(ldq_stq_CSR, _.value(3), true.B)
  def reconfig_stq_b0 = getOrElse(ldq_stq_CSR, _.value(2), true.B)
  def reconfig_ldq_b1 = getOrElse(ldq_stq_CSR, _.value(1), true.B)
  def reconfig_ldq_b0 = getOrElse(ldq_stq_CSR, _.value(0), true.B)
  def reconfigureFB_rows_b0 = getOrElse(fetch_bufferCSR, _.value(0), true.B)
  def reconfigureFB_rows_b1 = getOrElse(fetch_bufferCSR, _.value(1), true.B)
  // def reconfigureBPD = getOrElse(configureCSR, _.value(2), true.B)  - artifact from alex's branch. Recent changes to boom renamed this wire.

  // end alex stuff
    
  def cf_bpd_tage_to_gshare = getOrElse(bpdCSRCF, _.value(2), true.B)  

  def disableOOO = getOrElse(chickenCSR, _.value(3), true.B)
  def marchid = CustomCSR.constant(CSRs.marchid, BigInt(2))
  // core fuzzing specific
  // def cf_debug_log = getOrElse(chickenCSR, _.value(4), true.B)

  // defining masks for cf_dcache_csr
  val cf_dcache_set_shift = 0
  val cf_dcache_way_shift = 7
  val cf_dcache_size_shift = 15
  val cf_dcache_repl_shift = 23

  val cf_dcache_mask = 0xff
  val cf_dcache_init = 0x01
  override def dcacheCSRCF = {     

    // val mask = BigInt(
    //   cf_dcache_mask << cf_dcache_set_shift  |
    //   cf_dcache_mask << cf_dcache_way_shift  |
    //   cf_dcache_mask << cf_dcache_size_shift |
    //   cf_dcache_mask << cf_dcache_repl_shift
    // )
    val mask = BigInt(0x7fffffff)
    // val init = BigInt(
    //   cf_dcache_init << cf_dcache_set_shift  |
    //   cf_dcache_init << cf_dcache_way_shift  |
    //   cf_dcache_init << cf_dcache_size_shift |
    //   cf_dcache_init << cf_dcache_repl_shift
    // )
    val init = BigInt(0x0)
    
    Some(CustomCSR(dcacheCSRIdCF, mask, Some(init)))
  }  

  def cf_dcache_csr_val = getOrElse(dcacheCSRCF, _.value, 0x0.U)
  def cf_dcache_set_conf = cf_dcache_csr_val & cf_dcache_mask.U
  def cf_dcache_way_conf = (cf_dcache_csr_val >>  8.U) & cf_dcache_mask.U
  def cf_dcache_size_conf = (cf_dcache_csr_val >> 16.U) & cf_dcache_mask.U// def cf_dcache_way_conf = (cf_dcache_csr_val >> (cf_dcache_way_shift.U + 1.U)) & cf_dcache_mask.U
  def cf_dcache_repl_conf = (cf_dcache_csr_val >> 24.U) & cf_dcache_mask.U// def cf_dcache_size_conf = (cf_dcache_csr_val >> (cf_dcache_size_shift.U + 1.U)) & cf_dcache_mask.U
  // def cf_dcache_repl_conf = (cf_dcache_csr_val >> (cf_dcache_repl_shift.U + 1.U)) & cf_dcache_mask.U
// def decodeOneHot[T](csr: UInt, options: Seq[T]): UInt = {
//   val optVec = options.zipWithIndex.map { case (opt, i) => (csr(i), opt.U) }
//   optVec.tail.foldLeft(optVec.head._2) { case (sel, (cond, opt)) => Mux(cond.asBool, opt, sel) }
// }

// def getDCacheReconfParams(csr: UInt, blocksize_csr: UInt = 1.U) = {
//   val sets = decodeOneHot((csr >> 0) & 0xff.U, DCacheReconfOptions.setOptions)
//   val ways = decodeOneHot((csr >> 8) & 0xff.U, DCacheReconfOptions.wayOptions)
//   val size = decodeOneHot((csr >> 16) & 0xff.U, DCacheReconfOptions.sizeOptions)
//   val repl = decodeOneHot((csr >> 24) & 0xff.U, DCacheReconfOptions.replOptions)
//   val blocksize = decodeOneHot(blocksize_csr & 0x3.U, DCacheReconfOptions.blockSizeOptions)
//   (sets, ways, size, repl, blocksize)
// } 
  
  // val cf_dcache_blocksize_shift = 31
  val cf_dcache_blocksize_mask = 0x1
  override def cacheBlockSizeCSRCF = {
    val mask = BigInt(cf_dcache_blocksize_mask)
    val init = BigInt(0) // default: 16B
    Some(CustomCSR(cacheBlockSizeCSRIdCF, mask, Some(init)))
  }
  def cf_dcache_blocksize = getOrElse(cacheBlockSizeCSRCF, _.value(0), false.B)
  // def dcache_blocksize = boom.common.decodeOneHot(dcache_blocksize_csr_val & 0x3.U, boom.common.DCacheReconfOptions.blockSizeOptions)

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
    val mask = BigInt(0x7FFFFFFF)
    val init = BigInt(
      1 << 0 | // debug log is enabled at init
      0 << 1 | // dcache log is disabled at init
      0 << 2 | // lsu log is disabled at init
      1 << 3 | // core log is enabled at init
      1 << 4 | // rob log is enabled at init
      0 << 5 | // bpd log is disabled at init
      0 << 6 // frontend logs are disabled at init
    )
    Some(CustomCSR(debugCSRIdCF, mask, Some(init)))
  }
 
  override def robSizeCSRCF = {
    // Mask from parameters.scala
    // List of allowed ROB entry sizes
    // default is 130 for giga boom
    val mask = (BigInt(1) << robEntryOptions.length) - 1
    val init = BigInt(0) // Default: first option (130 entries)
    Some(CustomCSR(robSizeCSRIdCF, mask, Some(init)))
  }

  def cf_rob_entries = getOrElse(robSizeCSRCF, _.value, 0.U)
  // move this to ROB
  // Helper to decode one-hot CSR value to actual entry count
  // val options = robEntryOptions.map(_.U)
  // val entryCount = options.zipWithIndex.map { case (size, idx) => Mux(robCSRVal(idx), size.U, 0.U) }.reduce(_ | _)
  // def cf_rob_rows = entryCount / coreWidth.U

  override def decls: Seq[CustomCSR] = super.decls :+ marchid

  def cf_debug_enable = getOrElse(debugCSRCF, _.value(0), true.B)
  def cf_debug_dcache_enable = getOrElse(debugCSRCF, _.value(1), false.B)
  def cf_debug_lsu_enable = getOrElse(debugCSRCF, _.value(2), false.B)
  def cf_debug_core_enable = getOrElse(debugCSRCF, _.value(3), true.B)
  def cf_debug_rob_enable = getOrElse(debugCSRCF, _.value(4), true.B)
  def cf_debug_bpd_enable = getOrElse(debugCSRCF, _.value(5), false.B)
  def cf_debug_frontend_enable = getOrElse(debugCSRCF, _.value(6), false.B) 
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

  //************************************
  // Other Non/Should-not-be sythesizable modules
  val useFetchMonitor = boomParams.useFetchMonitor

  //************************************
  // Non-BOOM parameters

  val corePAddrBits = paddrBits
  val corePgIdxBits = pgIdxBits
}