//******************************************************************************
// Copyright (c) 2016 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// FDiv/FSqrt Unit
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.v3.exu

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.FPConstants._
import freechips.rocketchip.tile
import boom.v3.common._
import boom.v3.util._
import freechips.rocketchip.tile.HasFPUParameters
import freechips.rocketchip.util.uintToBitPat

/**
 * Decoder for FPU divide and square root signals
 */
class UOPCodeFDivDecoder(implicit p: Parameters) extends BoomModule
  with HasFPUParameters
{
  val io = IO(new Bundle {
    val uopc = Input(Bits(UOPC_SZ.W))
    val sigs = Output(new tile.FPUCtrlSigs())
  })

  val N = BitPat("b0")
  val Y = BitPat("b1")
  val X = BitPat("b?")

  val decoder = freechips.rocketchip.rocket.DecodeLogic(io.uopc,
    // Note: not all of these signals are used or necessary, but we're
    // constrained by the need to fit the rocket.FPU units' ctrl signals.
    //                                      swap12         fma
    //                                      | swap32       | div
    //                                      | | typeTagIn  | | sqrt
    //                           ldst       | | | typeTagOut | | wflags
    //                           | wen      | | | | from_int | | |
    //                           | | ren1   | | | | | to_int | | |
    //                           | | | ren2 | | | | | | fast | | |
    //                           | | | | ren3 | | | | | |  | | | |
    //                           | | | | |  | | | | | | |  | | | |
    /* Default */           List(X,X,X,X,X, X,X,X,X,X,X,X, X,X,X,X),
    Array(
      BitPat(uopFDIV_S)  -> List(X,X,Y,Y,X, X,X,S,S,X,X,X, X,Y,N,Y),
      BitPat(uopFDIV_D)  -> List(X,X,Y,Y,X, X,X,D,D,X,X,X, X,Y,N,Y),
      BitPat(uopFSQRT_S) -> List(X,X,Y,N,X, X,X,S,S,X,X,X, X,N,Y,Y),
      BitPat(uopFSQRT_D) -> List(X,X,Y,N,X, X,X,D,D,X,X,X, X,N,Y,Y)
    ): Array[(BitPat, List[BitPat])])

  val s = io.sigs
  val sigs = Seq(s.ldst, s.wen, s.ren1, s.ren2, s.ren3, s.swap12,
                 s.swap23, s.typeTagIn, s.typeTagOut, s.fromint, s.toint, s.fastpipe, s.fma,
                 s.div, s.sqrt, s.wflags)
  s.vec := false.B
  sigs zip decoder map {case(s,d) => s := d}
}

/**
 * fdiv/fsqrt is douple-precision. Must upconvert inputs and downconvert outputs
 * as necessary.  Must wait till killed uop finishes before we're ready again.
 * fdiv/fsqrt unit uses an unstable FIFO interface, and thus we must spend a
 * cycle buffering up an uop to provide slack between the issue queue and the
 * fdiv/fsqrt unit.  FDivUnit inherents directly from FunctionalUnit, because
 * UnpipelinedFunctionalUnit can only handle 1 inflight uop, whereas FDivUnit
 * contains up to 2 inflight uops due to the need to buffer the input as the
 * fdiv unit uses an unstable FIFO interface.
 * TODO extend UnpipelinedFunctionalUnit to handle a >1 uops inflight.
 *
 * @param isPipelined is the functional unit pipelined
 * @param numStages number of stages for the functional unit
 * @param numBypassStages number of bypass stages
 * @param dataWidth width of the data out of the functional unit
 */
class FDivSqrtUnit(implicit p: Parameters)
  extends FunctionalUnit(
    isPipelined = false,
    numStages = 1,
    numBypassStages = 0,
    dataWidth = 65,
    needsFcsr = true)
  with tile.HasFPUParameters
{
  //--------------------------------------
  // buffer inputs and upconvert as needed

  // provide a one-entry queue to store incoming uops while waiting for the fdiv/fsqrt unit to become available.
  val r_buffer_val = RegInit(false.B)
  val r_buffer_req = Reg(new FuncUnitReq(dataWidth=65))
  val r_buffer_fin = Reg(new tile.FPInput)

  val fdiv_decoder = Module(new UOPCodeFDivDecoder)
  fdiv_decoder.io.uopc := io.req.bits.uop.uopc

  // handle branch kill on queued entry
  r_buffer_val := !IsKilledByBranch(io.brupdate, r_buffer_req.uop) && !io.req.bits.kill && r_buffer_val
  r_buffer_req.uop.br_mask := GetNewBrMask(io.brupdate, r_buffer_req.uop)

  // handle incoming uop, including upconversion as needed, and push back if our input queue is already occupied
  io.req.ready := !r_buffer_val

  def upconvert(x: UInt) = {
    val s2d = Module(new hardfloat.RecFNToRecFN(inExpWidth = 8, inSigWidth = 24, outExpWidth = 11, outSigWidth = 53))
    s2d.io.in := x
    s2d.io.roundingMode := 0.U
    s2d.io.detectTininess := DontCare
    s2d.io.out
  }
  val in1_upconvert = upconvert(unbox(io.req.bits.rs1_data, false.B, Some(tile.FType.S)))
  val in2_upconvert = upconvert(unbox(io.req.bits.rs2_data, false.B, Some(tile.FType.S)))

  when (io.req.valid && !IsKilledByBranch(io.brupdate, io.req.bits.uop) && !io.req.bits.kill) {
    r_buffer_val := true.B
    r_buffer_req := io.req.bits
    r_buffer_req.uop.br_mask := GetNewBrMask(io.brupdate, io.req.bits.uop)
    // IFT LUT optimization: zero cf_* fields not read by FDivSqrtUnit or by ROB
    // wb_resps merge handler. Same field set as PipelinedFunctionalUnit (Change 2).
    // [A3 2026-09-10] identity preserved on IFT builds -- same reason as
    // functional-unit.scala; an FP-divide result reaching the ROB anonymous
    // makes any edge keyed on domain/op_count name the wrong op.
    if (!ENABLE_IFT) {
      r_buffer_req.uop.cf_domain_id.foreach(_ := 0.U)
      r_buffer_req.uop.cf_op_count_id.foreach(_ := 0.U)
    }
    r_buffer_req.uop.cf_spec_branch_is_secret.foreach(_ := false.B)
    // [FDIVFU] set the fpu bitmap bit (bit 22) the way the pipelined FU base does
    // (functional-unit.scala, stage-0 newBit); FDivSqrtUnit does not extend that base, so
    // FP divides committed without it (Phase 0 finding (d)).  Bit 12 (fpissq) is zeroed on
    // the issue-slot path for every op and is not FP-specific; left alone.
    r_buffer_req.uop.cf_fu_bitmap.foreach(_ := io.req.bits.uop.cf_fu_bitmap.getOrElse(0.U(numModules.W)) | (1.U << fpuTagCF.U))
    r_buffer_fin.viewAsSupertype(new tile.FPUCtrlSigs) := fdiv_decoder.io.sigs

    r_buffer_fin.rm := Mux(ImmGenRm(io.req.bits.uop.imm_packed) === 7.U, io.fcsr_rm, ImmGenRm(io.req.bits.uop.imm_packed))
    r_buffer_fin.typ := 0.U // unused for fdivsqrt
    val tag = fdiv_decoder.io.sigs.typeTagIn
    r_buffer_fin.in1 := unbox(io.req.bits.rs1_data, tag, Some(tile.FType.D))
    r_buffer_fin.in2 := unbox(io.req.bits.rs2_data, tag, Some(tile.FType.D))
    when (tag === S) {
      r_buffer_fin.in1 := in1_upconvert
      r_buffer_fin.in2 := in2_upconvert
    }
  }

  assert (!(r_buffer_val && io.req.valid), "[fdiv] a request is incoming while the buffer is already full.")

  //-----------
  // fdiv/fsqrt

  val divsqrt = Module(new hardfloat.DivSqrtRecF64)

  val r_divsqrt_val = RegInit(false.B)  // inflight uop?
  val r_divsqrt_killed = Reg(Bool())           // has inflight uop been killed?
  val r_divsqrt_fin = Reg(new tile.FPInput)
  val r_divsqrt_uop = Reg(new MicroOp)
  // [FDIVTAINT] result taint travels with the divide through both hand-offs (buffer ->
  // divsqrt -> output), the same rule as the integer IterativeFunctionalUnit
  // (functional-unit.scala r_secret/r_atk).  Without these, io.resp.bits.secret/taint_atk
  // stayed at the base class DontCare and folded to 0, so fdiv/fsqrt results were
  // written untainted.  2 flops per stage, inputs are registers: no new logic depth.
  // [R124GATE] patch 26 (ruling 124): these taint flops (and r_out_secret/atk below) exist only
  // with ENABLE_IFT; constant 0 otherwise.
  val r_divsqrt_secret = if (ENABLE_IFT) Reg(Bool()) else WireInit(false.B)
  val r_divsqrt_atk    = if (ENABLE_IFT) Reg(Bool()) else WireInit(false.B)

  // [FDIVBUSY] O1: publish occupancy, as the integer divider does (functional-unit.scala,
  // `io.cf_fu_busy.valid := !io.req.ready`).  `!io.req.ready` (= r_buffer_val) is exactly
  // "an FP divide cannot issue"; r_out_val only gates the hand-off and never blocks issue.
  // The occupant named is the computing divide (r_divsqrt_uop) when one is in flight, else
  // the buffered one, for parity with the integer driver; the operand-secret term came
  // from the buffered request (ruling 32; superseded by [I1BUSYSEC], ruling 100).  All
  // sources are registers (D11, bug 8).
  val cf_busy_occ = Mux(r_divsqrt_val, r_divsqrt_uop, r_buffer_req.uop)
  // [I1BUSYSEC] (ruling 100, I-1) the operand-secret term follows the named occupant: the
  // computing divide's operand secret, latched at the buffer -> divsqrt hand-off by
  // [FDIVTAINT] (r_divsqrt_secret, no extra flop), else the buffered request's.
  io.cf_fu_busy.valid         := !io.req.ready
  io.cf_fu_busy.bits.op_count := cf_busy_occ.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W))(inflOpCountWidthCF-1, 0)
  io.cf_fu_busy.bits.is_atk   := cf_busy_occ.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= 0.U
  // [R124GATE] IFT off: the port default (false, functional-unit.scala) stands.
  if (ENABLE_IFT) {
    // [MEMORDATK] patch 42 (ruling 100 I-3, [I3BUSYATK] in functional-unit.scala): the occupant's
    // attacker flag is its operand attacker taint OR ownership, as on the integer divider -- a divide
    // or sqrt on attacker-chosen operands has attacker-chosen latency.  r_divsqrt_atk already holds
    // that OR for the computing op; the buffered op's operand bits are flops: one LUT, 0 levels added.
    io.cf_fu_busy.bits.is_atk := Mux(r_divsqrt_val, r_divsqrt_atk,
                                     r_buffer_req.rs1_taint_atk || r_buffer_req.rs2_taint_atk) ||
                                 cf_busy_occ.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= 0.U
    io.cf_fu_busy.bits.is_sec := Mux(r_divsqrt_val, r_divsqrt_secret,
                                     r_buffer_req.rs1_secret || r_buffer_req.rs2_secret) ||
                                 cf_busy_occ.cf_secret_access.getOrElse(false.B) || cf_busy_occ.cf_secret_propagation.getOrElse(false.B)
  }

  // Need to buffer output until RF writeport is available.
  val output_buffer_available = Wire(Bool())

  val may_fire_input =
    r_buffer_val &&
    (r_buffer_fin.div || r_buffer_fin.sqrt) &&
    !r_divsqrt_val &&
    output_buffer_available

  val divsqrt_ready = Mux(divsqrt.io.sqrtOp, divsqrt.io.inReady_sqrt, divsqrt.io.inReady_div)
  divsqrt.io.inValid := may_fire_input // must be setup early
  divsqrt.io.sqrtOp := r_buffer_fin.sqrt
  divsqrt.io.a := r_buffer_fin.in1
  divsqrt.io.b := Mux(divsqrt.io.sqrtOp, r_buffer_fin.in1, r_buffer_fin.in2)
  divsqrt.io.roundingMode := r_buffer_fin.rm
  divsqrt.io.detectTininess := DontCare

  r_divsqrt_killed := r_divsqrt_killed || IsKilledByBranch(io.brupdate, r_divsqrt_uop) || io.req.bits.kill
  r_divsqrt_uop.br_mask := GetNewBrMask(io.brupdate, r_divsqrt_uop)

  when (may_fire_input && divsqrt_ready) {
    // Remove entry from the input buffer.
    // We don't have time to kill divsqrt request so must track if killed on entry.
    r_buffer_val := false.B
    r_divsqrt_val := true.B
    r_divsqrt_fin := r_buffer_fin
    r_divsqrt_uop := r_buffer_req.uop
    if (ENABLE_IFT) {   // [R124GATE]
      r_divsqrt_secret := r_buffer_req.rs1_secret || r_buffer_req.rs2_secret || r_buffer_req.rs3_secret
      r_divsqrt_atk    := r_buffer_req.rs1_taint_atk || r_buffer_req.rs2_taint_atk ||
                          (r_buffer_req.uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)) =/= 0.U)
    }
    r_divsqrt_killed := IsKilledByBranch(io.brupdate, r_buffer_req.uop) || io.req.bits.kill
    r_divsqrt_uop.br_mask := GetNewBrMask(io.brupdate, r_buffer_req.uop)
    // IFT LUT optimization: explicit zero (Vivado would also propagate from
    // r_buffer_req.uop's already-zero cf_* fields, but be explicit).
    // [A3 2026-09-10] identity preserved on IFT builds -- same reason as
    // functional-unit.scala; an FP-divide result reaching the ROB anonymous
    // makes any edge keyed on domain/op_count name the wrong op.
    if (!ENABLE_IFT) {
      r_divsqrt_uop.cf_domain_id.foreach(_ := 0.U)
      r_divsqrt_uop.cf_op_count_id.foreach(_ := 0.U)
    }
    r_divsqrt_uop.cf_spec_branch_is_secret.foreach(_ := false.B)
  }

  //-----------------------------------------
  // buffer output and down-convert as needed

  val r_out_val = RegInit(false.B)
  val r_out_uop = Reg(new MicroOp)
  val r_out_secret = if (ENABLE_IFT) Reg(Bool()) else WireInit(false.B)   // [FDIVTAINT] [R124GATE]
  val r_out_atk    = if (ENABLE_IFT) Reg(Bool()) else WireInit(false.B)   // [FDIVTAINT] [R124GATE]
  val r_out_flags_double = Reg(Bits())
  val r_out_wdata_double = Reg(Bits())

  output_buffer_available := !r_out_val

  r_out_uop.br_mask := GetNewBrMask(io.brupdate, r_out_uop)

  when (io.resp.ready || IsKilledByBranch(io.brupdate, r_out_uop) || io.req.bits.kill) {
    r_out_val := false.B
  }
  when (divsqrt.io.outValid_div || divsqrt.io.outValid_sqrt) {
    r_divsqrt_val := false.B

    r_out_val := !r_divsqrt_killed && !IsKilledByBranch(io.brupdate, r_divsqrt_uop) && !io.req.bits.kill
    r_out_uop := r_divsqrt_uop
    if (ENABLE_IFT) {   // [R124GATE]
      r_out_secret := r_divsqrt_secret   // [FDIVTAINT]
      r_out_atk    := r_divsqrt_atk      // [FDIVTAINT]
    }
    r_out_uop.br_mask := GetNewBrMask(io.brupdate, r_divsqrt_uop)
    // IFT LUT optimization: explicit zero (chained from r_divsqrt_uop above).
    // [A3 2026-09-10] identity preserved on IFT builds -- same reason as
    // functional-unit.scala; an FP-divide result reaching the ROB anonymous
    // makes any edge keyed on domain/op_count name the wrong op.
    if (!ENABLE_IFT) {
      r_out_uop.cf_domain_id.foreach(_ := 0.U)
      r_out_uop.cf_op_count_id.foreach(_ := 0.U)
    }
    r_out_uop.cf_spec_branch_is_secret.foreach(_ := false.B)
    r_out_wdata_double := sanitizeNaN(divsqrt.io.out, tile.FType.D)
    r_out_flags_double := divsqrt.io.exceptionFlags

    assert (r_divsqrt_val, "[fdiv] a response is being generated for no request.")
  }

  assert (!(r_out_val && (divsqrt.io.outValid_div || divsqrt.io.outValid_sqrt)),
    "[fdiv] Buffered output being overwritten by another output from the fdiv/fsqrt unit.")

  val downvert_d2s = Module(new hardfloat.RecFNToRecFN(
    inExpWidth = 11, inSigWidth = 53, outExpWidth = 8, outSigWidth = 24))
  downvert_d2s.io.in := r_out_wdata_double
  downvert_d2s.io.roundingMode := r_divsqrt_fin.rm
  downvert_d2s.io.detectTininess := DontCare
  val out_flags = r_out_flags_double | Mux(r_divsqrt_fin.typeTagIn === S, downvert_d2s.io.exceptionFlags, 0.U)

  io.resp.valid := r_out_val && !IsKilledByBranch(io.brupdate, r_out_uop)
  io.resp.bits.uop := r_out_uop
  io.resp.bits.secret    := r_out_secret   // [FDIVTAINT]
  io.resp.bits.taint_atk := r_out_atk      // [FDIVTAINT]
  io.resp.bits.data :=
    Mux(r_divsqrt_fin.typeTagIn === S,
      box(downvert_d2s.io.out, false.B),
      box(r_out_wdata_double, true.B))
  io.resp.bits.fflags.valid := io.resp.valid
  io.resp.bits.fflags.bits.uop := r_out_uop
  io.resp.bits.fflags.bits.uop.br_mask := GetNewBrMask(io.brupdate, r_out_uop)
  io.resp.bits.fflags.bits.flags := out_flags
}
