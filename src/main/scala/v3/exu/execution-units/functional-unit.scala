//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Functional Units
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// If regfile bypassing is disabled, then the functional unit must do its own
// bypassing in here on the WB stage (i.e., bypassing the io.resp.data)
//
// TODO: explore possibility of conditional IO fields? if a branch unit... how to add extra to IO in subclass?

package boom.v3.exu

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util._
import freechips.rocketchip.tile
import freechips.rocketchip.rocket.{PipelinedMultiplier,BP,BreakpointUnit,Causes,CSR}

import boom.v3.common._
import boom.v3.ifu._
import boom.v3.util._

/**t
 * Functional unit constants
 */
object FUConstants
{
  // bit mask, since a given execution pipeline may support multiple functional units
  val FUC_SZ = 10
  val FU_X   = BitPat.dontCare(FUC_SZ)
  val FU_ALU =   1.U(FUC_SZ.W)
  val FU_JMP =   2.U(FUC_SZ.W)
  val FU_MEM =   4.U(FUC_SZ.W)
  val FU_MUL =   8.U(FUC_SZ.W)
  val FU_DIV =  16.U(FUC_SZ.W)
  val FU_CSR =  32.U(FUC_SZ.W)
  val FU_FPU =  64.U(FUC_SZ.W)
  val FU_FDV = 128.U(FUC_SZ.W)
  val FU_I2F = 256.U(FUC_SZ.W)
  val FU_F2I = 512.U(FUC_SZ.W)

  // FP stores generate data through FP F2I, and generate address through MemAddrCalc
  val FU_F2IMEM = 516.U(FUC_SZ.W)
}
import FUConstants._

/**
 * Class to tell the FUDecoders what units it needs to support
 *
 * @param alu support alu unit?
 * @param bru support br unit?
 * @param mem support mem unit?
 * @param muld support multiple div unit?
 * @param fpu support FP unit?
 * @param csr support csr writing unit?
 * @param fdiv support FP div unit?
 * @param ifpu support int to FP unit?
 */
class SupportedFuncUnits(
  val alu: Boolean  = false,
  val jmp: Boolean  = false,
  val mem: Boolean  = false,
  val muld: Boolean = false,
  val fpu: Boolean  = false,
  val csr: Boolean  = false,
  val fdiv: Boolean = false,
  val ifpu: Boolean = false)
{
}


/**
 * Bundle for signals sent to the functional unit
 *
 * @param dataWidth width of the data sent to the functional unit
 */
class FuncUnitReq(val dataWidth: Int)(implicit p: Parameters) extends BoomBundle
  with HasBoomUOP
{
  val numOperands = 3

  val rs1_data = UInt(dataWidth.W)
  val rs2_data = UInt(dataWidth.W)
  val rs3_data = UInt(dataWidth.W) // only used for FMA units
  // corefuzzing: per-operand taint.  Kept per-operand rather than pre-OR'd because
  // the LSU needs to know specifically whether the ADDRESS operand was secret.
  val rs1_secret = Bool()
  // corefuzzing: ATTACKER taint of the operands, carried beside the secret taint.
  // Without this the regfile's taint_atk is written but never read back, so attacker
  // influence cannot follow data -- and a STEERED gadget (attacker supplies the index,
  // victim's own code reads the secret) is indistinguishable from the program legitimately
  // touching its own secret.  spectre-v1 is exactly that shape: the whole gadget runs in
  // domain=0 and carried atk=0 on every record.
  val rs1_taint_atk = Bool()
  val rs2_taint_atk = Bool()
  val rs2_secret = Bool()
  val rs3_secret = Bool()
  val pred_data = Bool()

  val kill = Bool() // kill everything
}

/**
 * Bundle for the signals sent out of the function unit
 *
 * @param dataWidth data sent from the functional unit
 */
class FuncUnitResp(val dataWidth: Int)(implicit p: Parameters) extends BoomBundle
  with HasBoomUOP
{
  val predicated = Bool() // Was this response from a predicated-off instruction
  val data = UInt(dataWidth.W)
  // corefuzzing (taint-follows-data): the taint accompanying this result.  The FU
  // combines the taints of the operands it actually consumed; it then rides the
  // bypass and writeback paths with the value, so a consumer is tainted at the
  // moment it receives the data rather than by looking anything up.
  //
  // This carries everything the INFL_REG_DATAFLOW influencer needs, replacing the
  // rename-stage tables (taint_table / producer_table / producer_domain_table /
  // producer_secret_table) and their per-branch snapshots.  Snapshot/rollback is
  // not needed here at all: taint travelling with data cannot survive a squash,
  // because the data does not.
  val secret      = Bool()
  // corefuzzing: taint of the STORE DATA operand (rs2), kept SEPARATE from .secret
  // which carries the ADDRESS operand's taint.  The LSU needs both independently:
  // address taint -> s_tx (address-derived load); data taint -> STQ entry + dcache
  // line tag, which is what lets taint survive a spill/reload.
  val data_secret = Bool()
  // corefuzzing: ATTACKER taint of the store DATA operand (rs2), mirroring data_secret.
  val data_taint_atk = Bool()
  // corefuzzing: attacker taint of the RESULT (see FuncUnitReq.rs1_taint_atk).
  val taint_atk   = Bool()
  val fflags = new ValidIO(new FFlagsResp)
  val addr = UInt((vaddrBits+1).W) // only for maddr -> LSU
  val mxcpt = new ValidIO(UInt((freechips.rocketchip.rocket.Causes.all.max+2).W)) //only for maddr->LSU
  val sfence = Valid(new freechips.rocketchip.rocket.SFenceReq) // only for mcalc
}

/**
 * Branch resolution information given from the branch unit
 */
class BrResolutionInfo(implicit p: Parameters) extends BoomBundle
{
  val uop        = new MicroOp
  val valid      = Bool()
  val mispredict = Bool()
  val taken      = Bool()                     // which direction did the branch go?
  val cfi_type   = UInt(CFI_SZ.W)

  // Info for recalculating the pc for this branch
  val pc_sel     = UInt(2.W)

  val jalr_target = UInt(vaddrBitsExtended.W)
  val target_offset = SInt()
  // corefuzzing: taint of the branch's CONDITION operands, carried on resolution.
  // A branch has rf_wen=false, so it never appears as a writeback resp -- the site
  // that computes `sec1` (core.scala, gated on rf_wen && dst_rtype===RT_FIX &&
  // ldst_val) structurally EXCLUDES branches.  Resolution is the only place the
  // condition's taint exists, so it has to travel with brinfo.
  val cond_secret = Bool()
  // attacker taint of the CONDITION -- steering, not ownership.  See the branch unit.
  val cond_atk    = Bool()
}

class BrUpdateInfo(implicit p: Parameters) extends BoomBundle
{
  // On the first cycle we get masks to kill registers
  val b1 = new BrUpdateMasks
  // On the second cycle we get indices to reset pointers
  val b2 = new BrResolutionInfo
}

class BrUpdateMasks(implicit p: Parameters) extends BoomBundle
{
  val resolve_mask = UInt(maxBrCount.W)
  val mispredict_mask = UInt(maxBrCount.W)
}


/**
 * Abstract top level functional unit class that wraps a lower level hand made functional unit
 *
 * @param isPipelined is the functional unit pipelined?
 * @param numStages how many pipeline stages does the functional unit have
 * @param numBypassStages how many bypass stages does the function unit have
 * @param dataWidth width of the data being operated on in the functional unit
 * @param hasBranchUnit does this functional unit have a branch unit?
 */
abstract class FunctionalUnit(
  val isPipelined: Boolean,
  val numStages: Int,
  val numBypassStages: Int,
  val dataWidth: Int,
  val isJmpUnit: Boolean = false,
  val isAluUnit: Boolean = false,
  val isMemAddrCalcUnit: Boolean = false,
  val needsFcsr: Boolean = false)
  (implicit p: Parameters) extends BoomModule
  with CoreFuzzingConstants
{
  val io = IO(new Bundle {
    val req    = Flipped(new DecoupledIO(new FuncUnitReq(dataWidth)))
    val resp   = (new DecoupledIO(new FuncUnitResp(dataWidth)))

    val brupdate = Input(new BrUpdateInfo())

    val bypass = Output(Vec(numBypassStages, Valid(new ExeUnitResp(dataWidth))))

    // O1 (corefuzzing): occupancy taint.  A non-pipelined unit (div, fdivsqrt) holds
    // its issue slot for ~20-30 cycles; an independent op needing that unit cannot
    // issue at all, so no winner/loser pair exists and INFL_ISSUE_CONTENTION -- which
    // requires both -- can never fire.  That made variable-latency arithmetic an
    // invisible timing channel.  Pipelined units leave this invalid by construction.
    val cf_fu_busy = Output(Valid(new Bundle {
      // OPT (audit rank 2): inflOpCountWidthCF, not uopIDCounterWidthCF.  The influencer
      // entry stores only the LOW inflOpCountWidthCF bits (micro-op.scala:41) and the
      // parser reconstructs the rest, so the upper bits were routed out of every
      // non-pipelined FU and through the aggregation mux tree only to be discarded.
      // Behaviour-preserving by construction: the stored value is unchanged.
      val op_count = UInt(inflOpCountWidthCF.W)
      val is_atk   = Bool()
      val is_sec   = Bool()
    }))

    // only used by the fpu unit
    val fcsr_rm = if (needsFcsr) Input(UInt(tile.FPConstants.RM_SZ.W)) else null

    // only used by branch unit
    val brinfo     = if (isAluUnit) Output(new BrResolutionInfo()) else null
    val get_ftq_pc = if (isJmpUnit) Flipped(new GetPCFromFtqIO()) else null
    val status     = if (isMemAddrCalcUnit) Input(new freechips.rocketchip.rocket.MStatus()) else null

    // only used by memaddr calc unit
    val bp = if (isMemAddrCalcUnit) Input(Vec(nBreakpoints, new BP)) else null
    val mcontext = if (isMemAddrCalcUnit) Input(UInt(coreParams.mcontextWidth.W)) else null
    val scontext = if (isMemAddrCalcUnit) Input(UInt(coreParams.scontextWidth.W)) else null
  // corefuzzing: gate to enable speculative prints in functional units
  val cf_debug_exu_enable = Input(Bool())

  })

  // O1: invalid unless a non-pipelined unit overrides below.
  io.cf_fu_busy.valid := false.B
  io.cf_fu_busy.bits  := DontCare
  io.cf_fu_busy.bits.is_atk := false.B
  io.cf_fu_busy.bits.is_sec := false.B
  // Only the AGU carries a store-data operand; every other FU leaves this false.
  io.resp.bits.data_secret := false.B
  io.resp.bits.data_taint_atk := false.B

  io.bypass.foreach { b => b.valid := false.B; b.bits := DontCare }

  io.resp.valid := false.B
  io.resp.bits := DontCare

  if (isJmpUnit) {
    io.get_ftq_pc.ftq_idx := DontCare
  }
}

/**
 * Abstract top level pipelined functional unit
 *
 * Note: this helps track which uops get killed while in intermediate stages,
 * but it is the job of the consumer to check for kills on the same cycle as consumption!!!
 *
 * @param numStages how many pipeline stages does the functional unit have
 * @param numBypassStages how many bypass stages does the function unit have
 * @param earliestBypassStage first stage that you can start bypassing from
 * @param dataWidth width of the data being operated on in the functional unit
 * @param hasBranchUnit does this functional unit have a branch unit?
 */
abstract class PipelinedFunctionalUnit(
  numStages: Int,
  numBypassStages: Int,
  earliestBypassStage: Int,
  dataWidth: Int,
  isJmpUnit: Boolean = false,
  isAluUnit: Boolean = false,
  isMemAddrCalcUnit: Boolean = false,
  needsFcsr: Boolean = false
  )(implicit p: Parameters) extends FunctionalUnit(
    isPipelined = true,
    numStages = numStages,
    numBypassStages = numBypassStages,
    dataWidth = dataWidth,
    isJmpUnit = isJmpUnit,
    isAluUnit = isAluUnit,
    isMemAddrCalcUnit = isMemAddrCalcUnit,
    needsFcsr = needsFcsr)
{
  // Pipelined functional unit is always ready.
  io.req.ready := true.B

  // corefuzzing
  // helper to dump uop directly (modules have xLen and vaddrBits in scope)
  def dumpUop(unit: String, uop: MicroOp, enabled: Bool): Unit = {
    // Compile-time gated: when ENABLE_CF_DEBUG_PRINTF is false this body is never
    // elaborated, so the [SPECULATIVE][EXU] printf and its whole logic cone
    // (debug_pc/debug_inst/cf_* reads) never reach the FIRRTL. The signature is
    // unchanged, so every call site and FU subclass is untouched.
    if (ENABLE_CF_DEBUG_PRINTF) {
      // Modified: call new overload that accepts MicroOp to include cf_* fields in dumps
      // Old call (kept for traceability):
      // SpeculativePrintf.dump(unit, Sext.apply(uop.debug_pc(vaddrBits-1,0), xLen), uop.debug_inst, uop.is_rvc, enabled)
      SpeculativePrintf.dump(unit, Sext.apply(uop.debug_pc(vaddrBits-1,0), xLen), uop.debug_inst, uop.is_rvc, enabled, uop)
    }
  }

  if (numStages > 0) {
    val r_valids = RegInit(VecInit(Seq.fill(numStages) { false.B }))
    val r_uops   = Reg(Vec(numStages, new MicroOp()))
    // corefuzzing (taint-follows-data): taint pipelined alongside the uop so a
    // multi-cycle unit's result carries the taint of the operands it consumed.
    val r_secret = Reg(Vec(numStages, Bool()))
    val r_atk    = Reg(Vec(numStages, Bool()))

    // handle incoming request
    r_valids(0) := io.req.valid && !IsKilledByBranch(io.brupdate, io.req.bits.uop) && !io.req.bits.kill
    r_uops(0)   := io.req.bits.uop
    r_uops(0).br_mask := GetNewBrMask(io.brupdate, io.req.bits.uop)
    r_secret(0) := io.req.bits.rs1_secret || io.req.bits.rs2_secret || io.req.bits.rs3_secret
    // [SEEDFIX] see the note at r_atk_alu.
    r_atk(0)    := io.req.bits.rs1_taint_atk || io.req.bits.rs2_taint_atk || (io.req.bits.uop.cf_domain_id =/= 0.U)

    // IFT LUT optimization: zero cf_* fields not read by the FU pipeline or by
    // the ROB wb_resps merge handler. PRESERVED fields (used downstream):
    //   cf_fu_bitmap (FU OR's its own bit; rob.scala wb_resps OR-merges)
    //   cf_attacker_influence, cf_secret_access, cf_secret_transmission,
    //     cf_secret_propagation (rob.scala wb_resps conditional set-true)
    //   cf_influencer_list, cf_infl_dropped (rob.scala wb_infl_pending capture)
    // Stage 0 zeros propagate to stages 1..numStages-1 via r_uops(i):=r_uops(i-1).
    // [A3 2026-09-10] Do NOT zero identity on IFT builds.  MEMIDFIX fixed only the
    // numStages==0 pass-through (MemAddrCalc); this is the PIPELINED branch, and fdiv
    // has the same pattern.  A uop whose domain/op_count is erased here reaches the ROB
    // writeback merge anonymous, so any influencer edge or ROB update keyed on those
    // fields names the wrong op -- the same failure MEMIDFIX fixed one layer down.
    // Gated on ENABLE_IFT so non-IFT builds still constant-fold both fields away.
    if (!ENABLE_IFT) {
      r_uops(0).cf_domain_id               := 0.U
      r_uops(0).cf_op_count_id              := 0.U
    }
    r_uops(0).cf_speculated               := false.B
    r_uops(0).cf_single_step              := false.B
    r_uops(0).cf_src_tainted              := false.B
    r_uops(0).cf_spec_branch_is_atk       := false.B
    r_uops(0).cf_atk_branch_ctr       := 0.U
    r_uops(0).cf_sec_branch_ctr       := 0.U
    r_uops(0).cf_spec_branch_op_id        := 0.U
    r_uops(0).cf_spec_branch_is_secret    := false.B
    r_uops(0).cf_cntd_valid               := false.B
    r_uops(0).cf_cntd_winner_op           := 0.U
    r_uops(0).cf_cntd_winner_atk          := false.B
    r_uops(0).cf_cntd_winner_sec          := false.B
    r_uops(0).cf_cntd_deny_count          := 0.U

    // corefuzzing: set the FU bitmap bit and detect attacker-secret coexistence at stage 0
    {
      val fu = io.req.bits.uop.fu_code
      val newBit = MuxCase(0.U(numModules.W), Seq(
        ((fu & FU_ALU) =/= 0.U)                                           -> (1.U << aluTagCF.U),
        ((fu & FU_MUL) =/= 0.U)                                           -> (1.U << mulTagCF.U),
        ((fu & FU_DIV) =/= 0.U)                                           -> (1.U << divTagCF.U),
        ((fu & (FU_FPU | FU_FDV | FU_I2F | FU_F2I)) =/= 0.U)             -> (1.U << fpuTagCF.U),
        ((fu & FU_CSR) =/= 0.U)                                           -> (1.U << csrTagCF.U)
      ))
      when (io.req.valid && !IsKilledByBranch(io.brupdate, io.req.bits.uop) && !io.req.bits.kill) {
        r_uops(0).cf_fu_bitmap := io.req.bits.uop.cf_fu_bitmap | newBit
      }
    }

    // corefuzzing
    // If an incoming request is killed by a branch this cycle, non-destructively log it
    when (io.req.valid && IsKilledByBranch(io.brupdate, io.req.bits.uop)) {
      dumpUop("EXU", io.req.bits.uop, io.cf_debug_exu_enable)
    }

    // handle middle of the pipeline
    for (i <- 1 until numStages) {
  r_valids(i) := r_valids(i-1) && !IsKilledByBranch(io.brupdate, r_uops(i-1)) && !io.req.bits.kill
  r_uops(i)   := r_uops(i-1)
  r_uops(i).br_mask := GetNewBrMask(io.brupdate, r_uops(i-1))
      r_secret(i) := r_secret(i-1)
      r_atk(i)    := r_atk(i-1)

      if (numBypassStages > 0) {
        io.bypass(i-1).bits.uop := r_uops(i-1)
        io.bypass(i-1).bits.secret := r_secret(i-1)
      }
    }

    // handle outgoing (branch could still kill it)
    // consumer must also check for pipeline flushes (kills)
    io.resp.valid    := r_valids(numStages-1) && !IsKilledByBranch(io.brupdate, r_uops(numStages-1))
    io.resp.bits.predicated := false.B
    io.resp.bits.uop := r_uops(numStages-1)
    io.resp.bits.uop.br_mask := GetNewBrMask(io.brupdate, r_uops(numStages-1))
    io.resp.bits.secret := r_secret(numStages-1)
    io.resp.bits.taint_atk := r_atk(numStages-1)

    // bypassing (TODO allow bypass vector to have a different size from numStages)
    if (numBypassStages > 0 && earliestBypassStage == 0) {
      io.bypass(0).bits.uop := io.req.bits.uop
      io.bypass(0).bits.secret := io.req.bits.rs1_secret || io.req.bits.rs2_secret || io.req.bits.rs3_secret

      for (i <- 1 until numBypassStages) {
        io.bypass(i).bits.uop := r_uops(i-1)
        io.bypass(i).bits.secret := r_secret(i-1)
      }
    }
    // corefuzzing
    // Non-destructive logging for uops in pipeline stages that are being killed by a branch
    for (i <- 0 until numStages) {
      when (r_valids(i) && IsKilledByBranch(io.brupdate, r_uops(i))) {
        dumpUop("EXU", r_uops(i), io.cf_debug_exu_enable)
      }
    }
  } else {
    require (numStages == 0)
    // pass req straight through to response

    // valid doesn't check kill signals, let consumer deal with it.
    // The LSU already handles it and this hurts critical path.
    io.resp.valid    := io.req.valid && !IsKilledByBranch(io.brupdate, io.req.bits.uop)
    io.resp.bits.predicated := false.B
    io.resp.bits.uop := io.req.bits.uop
    io.resp.bits.uop.br_mask := GetNewBrMask(io.brupdate, io.req.bits.uop)

    // IFT LUT optimization: zero cf_* fields not read by the ROB wb_resps merge.
    // Same field set as the pipelined branch above. Has no register storage to
    // save, but consistent with the pipelined branch and lets the synthesizer
    // propagate constants out of the FU's resp.uop.
    //
    // [MEMIDFIX 2026-09-09] EXCEPT for the MemAddrCalc unit, which must keep
    // cf_domain_id and cf_op_count_id.  Its resp.uop becomes the LSU's exe_req, then
    // exe_tlb_uop -> exe_tlb_uop_cf -> the D$ request for the will_fire_load_incoming
    // path, and the D$ writes a refilled line's IFT tag FROM THAT uop
    // (dcache -> mshrs req.uop -> cf_req_domain/_op_count).  Zeroing them here made
    // every load-miss fill anonymous: the line came back tagged domain=0 oc=0, so an
    // attacker who only READS a line never marked it -- and Prime+Probe primes with
    // loads.
    //
    // MEASURED (t11_cache_evict, same line 0x80003280, same run):
    //     src=3(store_commit)  domain=1 oc=1205   <- STQ-sourced uop, intact
    //     src=1(load_incoming) domain=0 oc=0      <- this zeroing
    // and across t37: 332/332 load_incoming stripped, while 1002/1002 load_wakeup and
    // 132/132 store_commit were intact (those read the LDQ/STQ entry written at
    // dispatch, never passing through this unit).
    //
    // COST: none.  numStages == 0 here, so there is no register storage -- the comment
    // above says as much.  This only withholds constant propagation for two fields in
    // one unit.  cf_secret_access is NOT in this list and already survives, which is
    // why secret-tagged lines were tagged while attacker-tagged lines were not.
    // [G1 2026-09-09] gated on ENABLE_IFT.  Preserving these two fields is only useful
    // when DIFT is compiled in; a non-IFT build must still zero them so the synthesizer
    // can constant-fold fields nothing reads.  Convention follows dcache.scala:1511.
    if (!(isMemAddrCalcUnit && ENABLE_IFT)) {
      io.resp.bits.uop.cf_domain_id             := 0.U
      io.resp.bits.uop.cf_op_count_id           := 0.U
    }
    io.resp.bits.uop.cf_speculated               := false.B
    io.resp.bits.uop.cf_single_step              := false.B
    io.resp.bits.uop.cf_src_tainted              := false.B
    io.resp.bits.uop.cf_spec_branch_is_atk       := false.B
    io.resp.bits.uop.cf_atk_branch_ctr       := 0.U
    io.resp.bits.uop.cf_sec_branch_ctr       := 0.U
    io.resp.bits.uop.cf_spec_branch_op_id        := 0.U
    io.resp.bits.uop.cf_spec_branch_is_secret    := false.B
    io.resp.bits.uop.cf_cntd_valid               := false.B
    io.resp.bits.uop.cf_cntd_winner_op           := 0.U
    io.resp.bits.uop.cf_cntd_winner_atk          := false.B
    io.resp.bits.uop.cf_cntd_winner_sec          := false.B
    io.resp.bits.uop.cf_cntd_deny_count          := 0.U

    // corefuzzing
    // Non-destructive logging for non-pipelined functional unit kills
    when (io.req.valid && IsKilledByBranch(io.brupdate, io.req.bits.uop)) {
      dumpUop("EXU", io.req.bits.uop, io.cf_debug_exu_enable)
    }
  }
}

/**
 * Functional unit that wraps RocketChips ALU
 *
 * @param isBranchUnit is this a branch unit?
 * @param numStages how many pipeline stages does the functional unit have
 * @param dataWidth width of the data being operated on in the functional unit
 */
class ALUUnit(isJmpUnit: Boolean = false, numStages: Int = 1, dataWidth: Int)(implicit p: Parameters)
  extends PipelinedFunctionalUnit(
    numStages = numStages,
    numBypassStages = numStages,
    isAluUnit = true,
    earliestBypassStage = 0,
    dataWidth = dataWidth,
    isJmpUnit = isJmpUnit)
  with boom.v3.ifu.HasBoomFrontendParameters
{
  val uop = io.req.bits.uop

  // immediate generation
  val imm_xprlen = ImmGen(uop.imm_packed, uop.ctrl.imm_sel)

  // operand 1 select
  var op1_data: UInt = null
  if (isJmpUnit) {
    // Get the uop PC for jumps
    val block_pc = AlignPCToBoundary(io.get_ftq_pc.pc, icBlockBytes)
    val uop_pc = (block_pc | uop.pc_lob) - Mux(uop.edge_inst, 2.U, 0.U)

    op1_data = Mux(uop.ctrl.op1_sel.asUInt === OP1_RS1 , io.req.bits.rs1_data,
               Mux(uop.ctrl.op1_sel.asUInt === OP1_PC  , Sext(uop_pc, xLen),
                                                         0.U))
  } else {
    op1_data = Mux(uop.ctrl.op1_sel.asUInt === OP1_RS1 , io.req.bits.rs1_data,
                                                         0.U)
  }

  // operand 2 select
  val op2_data = Mux(uop.ctrl.op2_sel === OP2_IMM,  Sext(imm_xprlen.asUInt, xLen),
                 Mux(uop.ctrl.op2_sel === OP2_IMMC, io.req.bits.uop.prs1(4,0),
                 Mux(uop.ctrl.op2_sel === OP2_RS2 , io.req.bits.rs2_data,
                 Mux(uop.ctrl.op2_sel === OP2_NEXT, Mux(uop.is_rvc, 2.U, 4.U),
                                                    0.U))))

  val alu = Module(new freechips.rocketchip.rocket.ALU())

  alu.io.in1 := op1_data.asUInt
  alu.io.in2 := op2_data.asUInt
  alu.io.fn  := uop.ctrl.op_fcn
  alu.io.dw  := uop.ctrl.fcn_dw


  // Did I just get killed by the previous cycle's branch,
  // or by a flush pipeline?
  val killed = WireInit(false.B)
  when (io.req.bits.kill || IsKilledByBranch(io.brupdate, uop)) {
    killed := true.B
  }

  val rs1 = io.req.bits.rs1_data
  val rs2 = io.req.bits.rs2_data
  val br_eq  = (rs1 === rs2)
  val br_ltu = (rs1.asUInt < rs2.asUInt)
  val br_lt  = (~(rs1(xLen-1) ^ rs2(xLen-1)) & br_ltu |
                rs1(xLen-1) & ~rs2(xLen-1)).asBool

  val pc_sel = MuxLookup(uop.ctrl.br_type, PC_PLUS4)(
                 Seq(   BR_N   -> PC_PLUS4,
                        BR_NE  -> Mux(!br_eq,  PC_BRJMP, PC_PLUS4),
                        BR_EQ  -> Mux( br_eq,  PC_BRJMP, PC_PLUS4),
                        BR_GE  -> Mux(!br_lt,  PC_BRJMP, PC_PLUS4),
                        BR_GEU -> Mux(!br_ltu, PC_BRJMP, PC_PLUS4),
                        BR_LT  -> Mux( br_lt,  PC_BRJMP, PC_PLUS4),
                        BR_LTU -> Mux( br_ltu, PC_BRJMP, PC_PLUS4),
                        BR_J   -> PC_BRJMP,
                        BR_JR  -> PC_JALR
                        ))

  val is_taken = io.req.valid &&
                   !killed &&
                   (uop.is_br || uop.is_jalr || uop.is_jal) &&
                   (pc_sel =/= PC_PLUS4)

  // "mispredict" means that a branch has been resolved and it must be killed
  val mispredict = WireInit(false.B)

  val is_br          = io.req.valid && !killed && uop.is_br && !uop.is_sfb
  val is_jal         = io.req.valid && !killed && uop.is_jal
  val is_jalr        = io.req.valid && !killed && uop.is_jalr

  when (is_br || is_jalr) {
    if (!isJmpUnit) {
      assert (pc_sel =/= PC_JALR)
    }
    when (pc_sel === PC_PLUS4) {
      mispredict := uop.taken
    }
    when (pc_sel === PC_BRJMP) {
      mispredict := !uop.taken
    }
  }

  val brinfo = Wire(new BrResolutionInfo)

  // note: jal doesn't allocate a branch-mask, so don't clear a br-mask bit
  brinfo.valid          := is_br || is_jalr
  brinfo.mispredict     := mispredict
  brinfo.uop            := uop
  brinfo.cfi_type       := Mux(is_jalr, CFI_JALR,
                           Mux(is_br  , CFI_BR, CFI_X))
  brinfo.taken          := is_taken
  // rs1/rs2 ARE the condition for a conditional branch; for jalr rs1 is the target
  // register, which is equally a control-influencing value.
  brinfo.cond_secret    := io.req.bits.rs1_secret || io.req.bits.rs2_secret
  // ATTACKER STEERING: a mispredict is attacker-caused when the branch's own condition
  // is attacker-derived.  Ownership cannot express this -- spectre-v1's bounds check is
  // victim code that the attacker merely feeds.
  brinfo.cond_atk       := io.req.bits.rs1_taint_atk || io.req.bits.rs2_taint_atk
  brinfo.pc_sel         := pc_sel

  brinfo.jalr_target    := DontCare


  // Branch/Jump Target Calculation
  // For jumps we read the FTQ, and can calculate the target
  // For branches we emit the offset for the core to redirect if necessary
  val target_offset = imm_xprlen(20,0).asSInt
  brinfo.jalr_target := DontCare
  if (isJmpUnit) {
    def encodeVirtualAddress(a0: UInt, ea: UInt) = if (vaddrBitsExtended == vaddrBits) {
      ea
    } else {
      // Efficient means to compress 64-bit VA into vaddrBits+1 bits.
      // (VA is bad if VA(vaddrBits) != VA(vaddrBits-1)).
      val a = a0.asSInt >> vaddrBits
      val msb = Mux(a === 0.S || a === -1.S, ea(vaddrBits), !ea(vaddrBits-1))
      Cat(msb, ea(vaddrBits-1,0))
    }


    val jalr_target_base = io.req.bits.rs1_data.asSInt
    val jalr_target_xlen = Wire(UInt(xLen.W))
    jalr_target_xlen := (jalr_target_base + target_offset).asUInt
    val jalr_target = (encodeVirtualAddress(jalr_target_xlen, jalr_target_xlen).asSInt & -2.S).asUInt

    brinfo.jalr_target := jalr_target
    val cfi_idx = ((uop.pc_lob ^ Mux(io.get_ftq_pc.entry.start_bank === 1.U, 1.U << log2Ceil(bankBytes), 0.U)))(log2Ceil(fetchWidth),1)

    when (pc_sel === PC_JALR) {
      mispredict := !io.get_ftq_pc.next_val ||
                    (io.get_ftq_pc.next_pc =/= jalr_target) ||
                    !io.get_ftq_pc.entry.cfi_idx.valid ||
                    (io.get_ftq_pc.entry.cfi_idx.bits =/= cfi_idx)
    }
  }

  brinfo.target_offset := target_offset


  io.brinfo := brinfo



// Response
// TODO add clock gate on resp bits from functional units
//   io.resp.bits.data := RegEnable(alu.io.out, io.req.valid)
//   val reg_data = Reg(outType = Bits(width = xLen))
//   reg_data := alu.io.out
//   io.resp.bits.data := reg_data

  val r_val  = RegInit(VecInit(Seq.fill(numStages) { false.B }))
  val r_data = Reg(Vec(numStages, UInt(xLen.W)))
  // corefuzzing: ALU result taint, pipelined with the result it belongs to
  val r_secret_alu = Reg(Vec(numStages, Bool()))
  val r_atk_alu    = Reg(Vec(numStages, Bool()))
  val r_pred = Reg(Vec(numStages, Bool()))
  val alu_out = Mux(io.req.bits.uop.is_sfb_shadow && io.req.bits.pred_data,
    Mux(io.req.bits.uop.ldst_is_rs1, io.req.bits.rs1_data, io.req.bits.rs2_data),
    Mux(io.req.bits.uop.uopc === uopMOV, io.req.bits.rs2_data, alu.io.out))
  r_val (0) := io.req.valid
  r_data(0) := Mux(io.req.bits.uop.is_sfb_br, pc_sel === PC_BRJMP, alu_out)
  r_secret_alu(0) := io.req.bits.rs1_secret || io.req.bits.rs2_secret
  // [SEEDFIX 2026-09-07] Seed the OWNERSHIP bit at OPERAND CAPTURE, so it rides the
  // existing taint pipeline and reaches BOTH the regfile write and the BYPASS.
  // core.scala:1977 / regfile.scala:79 apply `|| cf_domain_id =/= 0` only when WRITING
  // the regfile, but register-read's bypass forwards the FU RESPONSE -- so a consumer
  // whose operand arrives by bypass saw the UNSEEDED value and dropped the taint.
  // MEASURED (t31): `mv a0,a5` in attacker code 140/140 tainted; the very next victim
  // instruction `mv a5,a0`, reading that same register via bypass, 0/114.  Attacker
  // influence therefore travelled by OWNERSHIP and through MEMORY only, and died at the
  // first cross-domain register hand-off.  spectre-v1 never saw it: its index travels
  // through attacker-WRITTEN memory, never the bypass.
  r_atk_alu(0)    := io.req.bits.rs1_taint_atk || io.req.bits.rs2_taint_atk || (io.req.bits.uop.cf_domain_id =/= 0.U)
  r_pred(0) := io.req.bits.uop.is_sfb_shadow && io.req.bits.pred_data
  for (i <- 1 until numStages) {
    r_val(i)  := r_val(i-1)
    r_data(i) := r_data(i-1)
    r_secret_alu(i) := r_secret_alu(i-1)
    r_atk_alu(i)    := r_atk_alu(i-1)
    r_pred(i) := r_pred(i-1)
  }
  io.resp.bits.data := r_data(numStages-1)
  io.resp.bits.secret := r_secret_alu(numStages-1)
  io.resp.bits.taint_atk := r_atk_alu(numStages-1)
  io.resp.bits.predicated := r_pred(numStages-1)
  // Bypass
  // for the ALU, we can bypass same cycle as compute
  require (numStages >= 1)
  require (numBypassStages >= 1)
  io.bypass(0).valid := io.req.valid
  io.bypass(0).bits.data := Mux(io.req.bits.uop.is_sfb_br, pc_sel === PC_BRJMP, alu_out)
  // taint of the ALU result = OR of its source taints, on the bypass path
  io.bypass(0).bits.secret := io.req.bits.rs1_secret || io.req.bits.rs2_secret
  for (i <- 1 until numStages) {
    io.bypass(i).valid := r_val(i-1)
    io.bypass(i).bits.data := r_data(i-1)
    io.bypass(i).bits.secret := r_secret_alu(i-1)
  }

  // Exceptions
  io.resp.bits.fflags.valid := false.B
}

/**
 * Functional unit that passes in base+imm to calculate addresses, and passes store data
 * to the LSU.
 * For floating point, 65bit FP store-data needs to be decoded into 64bit FP form
 */
class MemAddrCalcUnit(implicit p: Parameters)
  extends PipelinedFunctionalUnit(
    numStages = 0,
    numBypassStages = 0,
    earliestBypassStage = 0,
    dataWidth = 65, // TODO enable this only if FP is enabled?
    isMemAddrCalcUnit = true)
  with freechips.rocketchip.rocket.constants.MemoryOpConstants
  with freechips.rocketchip.rocket.constants.ScalarOpConstants
{
  // perform address calculation
  val sum = (io.req.bits.rs1_data.asSInt + io.req.bits.uop.imm_packed(19,8).asSInt).asUInt
  val ea_sign = Mux(sum(vaddrBits-1), ~sum(63,vaddrBits) === 0.U,
                                       sum(63,vaddrBits) =/= 0.U)
  val effective_address = Cat(ea_sign, sum(vaddrBits-1,0)).asUInt

  val store_data = io.req.bits.rs2_data

  // corefuzzing: the taint rides the VALUE that forms the address.  rs1_data is the
  // address operand and its taint arrives on the same request; without this the value
  // reaches the LSU and the secret bit is silently dropped here.  A load cannot execute
  // until this operand is ready, so this is exactly the moment its taint is known --
  // no wakeup snooping or retroactive bus is needed.
  io.resp.bits.secret := io.req.bits.rs1_secret
  // [SEEDFIX] see the note at r_atk_alu.
  io.resp.bits.taint_atk := io.req.bits.rs1_taint_atk || io.req.bits.rs2_taint_atk || (io.req.bits.uop.cf_domain_id =/= 0.U)
  io.resp.bits.data_secret := io.req.bits.rs2_secret
  io.resp.bits.data_taint_atk := io.req.bits.rs2_taint_atk
// [PROBE STRIPPED 2026-09-11] A1PROBE2 -- diagnostic only, purpose discharged.
  // // [A1PROBE2 2026-09-11] TEMPORARY.  A1 is CONFIRMED (199,008 store records, 0 with s_prop=1)
  // // but its recorded cause -- "no rs2 bypass cases" -- was REFUTED: the cases exist, the
  // // bypass network carries `secret`/`taint_atk`, and today's Verilog shows an uncollapsed rs2
  // // mux.  Every static link is wired, so the death point is NOT located.  Print, for STORE
  // // uops only, what actually arrives at the AGU and what leaves it.  Three outcomes, three
  // // different fixes: rs2sec=0 => the producer/bypass never delivered it; rs2sec=1 & dsec=0 =>
  // // lost in this unit; both 1 => lost downstream of the AGU.
  // if (ENABLE_CF_DEBUG_PRINTF) {
    // when (io.req.valid && io.req.bits.uop.uses_stq) {
      // printf("\n[A1AGU] pc=0x%x prs2=%d rs2sec=%d rs2atk=%d dsec=%d datk=%d sprop=%d\n",
        // io.req.bits.uop.debug_pc, io.req.bits.uop.prs2,
        // io.req.bits.rs2_secret, io.req.bits.rs2_taint_atk,
        // io.resp.bits.data_secret, io.resp.bits.data_taint_atk,
        // io.req.bits.uop.cf_secret_propagation)
    // }
  // }
  io.resp.bits.addr := effective_address
  io.resp.bits.data := store_data

  if (dataWidth > 63) {
    assert (!(io.req.valid && io.req.bits.uop.ctrl.is_std &&
      io.resp.bits.data(64).asBool === true.B), "65th bit set in MemAddrCalcUnit.")

    assert (!(io.req.valid && io.req.bits.uop.ctrl.is_std && io.req.bits.uop.fp_val),
      "FP store-data should now be going through a different unit.")
  }

  assert (!(io.req.bits.uop.fp_val && io.req.valid && io.req.bits.uop.uopc =/=
          uopLD && io.req.bits.uop.uopc =/= uopSTA),
          "[maddrcalc] assert we never get store data in here.")

  // Handle misaligned exceptions
  val size = io.req.bits.uop.mem_size
  val misaligned =
    (size === 1.U && (effective_address(0) =/= 0.U)) ||
    (size === 2.U && (effective_address(1,0) =/= 0.U)) ||
    (size === 3.U && (effective_address(2,0) =/= 0.U))

  val bkptu = Module(new BreakpointUnit(nBreakpoints))
  bkptu.io.status   := io.status
  bkptu.io.bp       := io.bp
  bkptu.io.pc       := DontCare
  bkptu.io.ea       := effective_address
  bkptu.io.mcontext := io.mcontext
  bkptu.io.scontext := io.scontext

  val ma_ld  = io.req.valid && io.req.bits.uop.uopc === uopLD && misaligned
  val ma_st  = io.req.valid && (io.req.bits.uop.uopc === uopSTA || io.req.bits.uop.uopc === uopAMO_AG) && misaligned
  val dbg_bp = io.req.valid && ((io.req.bits.uop.uopc === uopLD  && bkptu.io.debug_ld) ||
                                (io.req.bits.uop.uopc === uopSTA && bkptu.io.debug_st))
  val bp     = io.req.valid && ((io.req.bits.uop.uopc === uopLD  && bkptu.io.xcpt_ld) ||
                                (io.req.bits.uop.uopc === uopSTA && bkptu.io.xcpt_st))

  def checkExceptions(x: Seq[(Bool, UInt)]) =
    (x.map(_._1).reduce(_||_), PriorityMux(x))
  val (xcpt_val, xcpt_cause) = checkExceptions(List(
    (ma_ld,  (Causes.misaligned_load).U),
    (ma_st,  (Causes.misaligned_store).U),
    (dbg_bp, (CSR.debugTriggerCause).U),
    (bp,     (Causes.breakpoint).U)))

  io.resp.bits.mxcpt.valid := xcpt_val
  io.resp.bits.mxcpt.bits  := xcpt_cause
  assert (!(ma_ld && ma_st), "Mutually-exclusive exceptions are firing.")

  io.resp.bits.sfence.valid := io.req.valid && io.req.bits.uop.mem_cmd === M_SFENCE
  io.resp.bits.sfence.bits.rs1 := io.req.bits.uop.mem_size(0)
  io.resp.bits.sfence.bits.rs2 := io.req.bits.uop.mem_size(1)
  io.resp.bits.sfence.bits.addr := io.req.bits.rs1_data
  io.resp.bits.sfence.bits.asid := io.req.bits.rs2_data
}


/**
 * Functional unit to wrap lower level FPU
 *
 * Currently, bypassing is unsupported!
 * All FP instructions are padded out to the max latency unit for easy
 * write-port scheduling.
 */
class FPUUnit(implicit p: Parameters)
  extends PipelinedFunctionalUnit(
    numStages = p(tile.TileKey).core.fpu.get.dfmaLatency,
    numBypassStages = 0,
    earliestBypassStage = 0,
    dataWidth = 65,
    needsFcsr = true)
{
  val fpu = Module(new FPU())
  fpu.io.req.valid         := io.req.valid
  fpu.io.req.bits.uop      := io.req.bits.uop
  fpu.io.req.bits.rs1_data := io.req.bits.rs1_data
  fpu.io.req.bits.rs2_data := io.req.bits.rs2_data
  fpu.io.req.bits.rs3_data := io.req.bits.rs3_data
  fpu.io.req.bits.fcsr_rm  := io.fcsr_rm
  // corefuzzing
  fpu.io.cf_debug_exu_enable := io.cf_debug_exu_enable

  io.resp.bits.data              := fpu.io.resp.bits.data
  io.resp.bits.fflags.valid      := fpu.io.resp.bits.fflags.valid
  io.resp.bits.fflags.bits.uop   := io.resp.bits.uop
  io.resp.bits.fflags.bits.flags := fpu.io.resp.bits.fflags.bits.flags // kill me now
}

/**
 * Int to FP conversion functional unit
 *
 * @param latency the amount of stages to delay by
 */
class IntToFPUnit(latency: Int)(implicit p: Parameters)
  extends PipelinedFunctionalUnit(
    numStages = latency,
    numBypassStages = 0,
    earliestBypassStage = 0,
    dataWidth = 65,
    needsFcsr = true)
  with tile.HasFPUParameters
{
  val fp_decoder = Module(new UOPCodeFPUDecoder) // TODO use a simpler decoder
  val io_req = io.req.bits
  fp_decoder.io.uopc := io_req.uop.uopc
  val fp_ctrl = fp_decoder.io.sigs
  val fp_rm = Mux(ImmGenRm(io_req.uop.imm_packed) === 7.U, io.fcsr_rm, ImmGenRm(io_req.uop.imm_packed))
  val req = Wire(new tile.FPInput)
  val tag = fp_ctrl.typeTagIn

  req.viewAsSupertype(new tile.FPUCtrlSigs) := fp_ctrl

  req.rm := fp_rm
  req.in1 := unbox(io_req.rs1_data, tag, None)
  req.in2 := unbox(io_req.rs2_data, tag, None)
  req.in3 := DontCare
  req.typ := ImmGenTyp(io_req.uop.imm_packed)
  req.fmt := DontCare // FIXME: this may not be the right thing to do here
  req.fmaCmd := DontCare

  assert (!(io.req.valid && fp_ctrl.fromint && req.in1(xLen).asBool),
    "[func] IntToFP integer input has 65th high-order bit set!")

  assert (!(io.req.valid && !fp_ctrl.fromint),
    "[func] Only support fromInt micro-ops.")

  val ifpu = Module(new tile.IntToFP(intToFpLatency))
  ifpu.io.in.valid := io.req.valid
  ifpu.io.in.bits := req
  ifpu.io.in.bits.in1 := io_req.rs1_data
  val out_double = Pipe(io.req.valid, fp_ctrl.typeTagOut === D, intToFpLatency).bits

//io.resp.bits.data              := box(ifpu.io.out.bits.data, !io.resp.bits.uop.fp_single)
  io.resp.bits.data              := box(ifpu.io.out.bits.data, out_double)
  io.resp.bits.fflags.valid      := ifpu.io.out.valid
  io.resp.bits.fflags.bits.uop   := io.resp.bits.uop
  io.resp.bits.fflags.bits.flags := ifpu.io.out.bits.exc
}

/**
 * Iterative/unpipelined functional unit, can only hold a single MicroOp at a time
 * assumes at least one register between request and response
 *
 * TODO allow up to N micro-ops simultaneously.
 *
 * @param dataWidth width of the data to be passed into the functional unit
 */
abstract class IterativeFunctionalUnit(dataWidth: Int)(implicit p: Parameters)
  extends FunctionalUnit(
    isPipelined = false,
    numStages = 1,
    numBypassStages = 0,
    dataWidth = dataWidth)
{
  val r_uop = Reg(new MicroOp())
  // corefuzzing: taint held for the duration of the iterative op
  val r_secret = Reg(Bool())
  val r_atk    = Reg(Bool())

  val do_kill = Wire(Bool())
  do_kill := io.req.bits.kill // irrelevant default

  when (io.req.fire) {
    // update incoming uop
    do_kill := IsKilledByBranch(io.brupdate, io.req.bits.uop) || io.req.bits.kill
    r_uop := io.req.bits.uop
    r_uop.br_mask := GetNewBrMask(io.brupdate, io.req.bits.uop)
    r_secret := io.req.bits.rs1_secret || io.req.bits.rs2_secret || io.req.bits.rs3_secret
    // [SEEDFIX] see the note at r_atk_alu.
    r_atk    := io.req.bits.rs1_taint_atk || io.req.bits.rs2_taint_atk || (io.req.bits.uop.cf_domain_id =/= 0.U)
  } .otherwise {
    do_kill := IsKilledByBranch(io.brupdate, r_uop) || io.req.bits.kill
    r_uop.br_mask := GetNewBrMask(io.brupdate, r_uop)
  }

  // O1: publish occupancy taint.  `!io.req.ready` is exactly "this non-pipelined unit
  // is still working", driven by the sub-unit (e.g. div.io.req.ready).  r_uop/r_secret
  // already hold the occupant's identity and taint for the whole latency, so this
  // needs no new state -- only exposure.
  io.cf_fu_busy.valid        := !io.req.ready
  // truncate explicitly: cf_op_count_id is uopIDCounterWidthCF wide, and only the low
  // inflOpCountWidthCF bits are ever stored (see the port declaration above).
  io.cf_fu_busy.bits.op_count := r_uop.cf_op_count_id(inflOpCountWidthCF-1, 0)
  io.cf_fu_busy.bits.is_atk   := r_uop.cf_domain_id =/= 0.U
  io.cf_fu_busy.bits.is_sec   := r_secret || r_uop.cf_secret_access || r_uop.cf_secret_propagation

  // assumes at least one pipeline register between request and response
  io.resp.bits.uop := r_uop
  io.resp.bits.secret := r_secret
  io.resp.bits.taint_atk := r_atk
}

/**
 * Divide functional unit.
 *
 * @param dataWidth data to be passed into the functional unit
 */
class DivUnit(dataWidth: Int)(implicit p: Parameters)
  extends IterativeFunctionalUnit(dataWidth)
{

  // We don't use the iterative multiply functionality here.
  // Instead we use the PipelinedMultiplier
  val div = Module(new freechips.rocketchip.rocket.MulDiv(mulDivParams, width = dataWidth))

  // request
  div.io.req.valid    := io.req.valid && !this.do_kill
  div.io.req.bits.dw  := io.req.bits.uop.ctrl.fcn_dw
  div.io.req.bits.fn  := io.req.bits.uop.ctrl.op_fcn
  div.io.req.bits.in1 := io.req.bits.rs1_data
  div.io.req.bits.in2 := io.req.bits.rs2_data
  div.io.req.bits.tag := DontCare
  io.req.ready        := div.io.req.ready

  // Corefuzzing: OR in divTagCF when request is accepted (parent r_uop := io.req.bits.uop
  // happens first; this second when-block wins on r_uop.cf_fu_bitmap)
  when (io.req.fire) {
    r_uop.cf_fu_bitmap := io.req.bits.uop.cf_fu_bitmap | (1.U << divTagCF.U)
  }

  // handle pipeline kills and branch misspeculations
  div.io.kill         := this.do_kill

  // response
  io.resp.valid       := div.io.resp.valid && !this.do_kill
  div.io.resp.ready   := io.resp.ready
  io.resp.bits.data   := div.io.resp.bits.data
}

/**
 * Pipelined multiplier functional unit that wraps around the RocketChip pipelined multiplier
 *
 * @param numStages number of pipeline stages
 * @param dataWidth size of the data being passed into the functional unit
 */
class PipelinedMulUnit(numStages: Int, dataWidth: Int)(implicit p: Parameters)
  extends PipelinedFunctionalUnit(
    numStages = numStages,
    numBypassStages = 0,
    earliestBypassStage = 0,
    dataWidth = dataWidth)
{
  val imul = Module(new PipelinedMultiplier(xLen, numStages))
  // request
  imul.io.req.valid    := io.req.valid
  imul.io.req.bits.fn  := io.req.bits.uop.ctrl.op_fcn
  imul.io.req.bits.dw  := io.req.bits.uop.ctrl.fcn_dw
  imul.io.req.bits.in1 := io.req.bits.rs1_data
  imul.io.req.bits.in2 := io.req.bits.rs2_data
  imul.io.req.bits.tag := DontCare
  // response
  io.resp.bits.data    := imul.io.resp.bits.data
}
