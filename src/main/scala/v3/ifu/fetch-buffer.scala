//******************************************************************************
// Copyright (c) 2018 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Fetch Buffer
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Takes a FetchBundle and converts into a vector of MicroOps.

package boom.v3.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.rocket.{MStatus, BP, BreakpointUnit}

import boom.v3.common._
import boom.v3.util.{BoolToChar, MaskUpper}

// This file has been modified to implement a CSR that modifies 
// the number of rows used in the fetch buffer - Alex, CoreFuzzing

/**
 * Bundle that is made up of converted MicroOps from the Fetch Bundle
 * input to the Fetch Buffer. This is handed to the Decode stage.
 */
class FetchBufferResp(implicit p: Parameters) extends BoomBundle
{
  val uops = Vec(coreWidth, Valid(new MicroOp()))
}

/**
 * Buffer to hold fetched packets and convert them into a vector of MicroOps
 * to give the Decode stage
 *
 * @param num_entries effectively the number of full-sized fetch packets we can hold.
 */
class FetchBuffer(implicit p: Parameters) extends BoomModule
  with HasBoomCoreParameters
  with HasBoomFrontendParameters
  with CoreFuzzingConstants
{
  val numEntries = numFetchBufferEntries
  val io = IO(new BoomBundle {
    val enq = Flipped(Decoupled(new FetchBundle()))
    val deq = new DecoupledIO(new FetchBufferResp())

    // Was the pipeline redirected? Clear/reset the fetchbuffer.
    val clear = Input(Bool())

    // adding for corefuzzing - alex
    val reconfigureFB_rows_b0 = Input(Bool())
    val reconfigureFB_rows_b1 = Input(Bool())
  })

  // original
  require (numEntries > fetchWidth)
  // modified for smallest num of entries used
  // require (numEntries/4 >= fetchWidth)
  require (numEntries % coreWidth == 0)
  val numRows = numEntries / coreWidth

  val ram = Reg(Vec(numEntries, new MicroOp))
  ram.suggestName("fb_uop_ram")
  val deq_vec = Wire(Vec(numRows, Vec(coreWidth, new MicroOp)))

  val head = RegInit(1.U(numRows.W))
  val tail = RegInit(1.U(numEntries.W))

  val maybe_full = RegInit(false.B)

  // fetch-buffer size fuzzing for CoreFuzzing project - alex
  // for Mega Boom:
  // decode width --> core width = 4
  // numFetchBufferEntries = 32
  // standard numRows = 32/4 = 8
  // four numrows reconfigurations: 4, 8, 12, 16
    // 0 --> 0.25*numRows = 2
    // 1 --> 0.5*numRows = 4
    // 2 --> 0.75*numRows = 6
    // 3 --> numRows = 8


  // registers for debugging - alex, corefuzzing
  val rowsUsed = Mux(io.reconfigureFB_rows_b1, Mux(io.reconfigureFB_rows_b0, (numRows).U, (3*(numRows/4)).U), Mux(io.reconfigureFB_rows_b0, (numRows/2).U, (numRows/4).U))
  dontTouch(rowsUsed)
  val rowNum_tail = Reg(UInt(5.W))
  val rowNum_head = Reg(UInt(5.W))
  dontTouch(rowNum_tail)
  dontTouch(rowNum_head)


  // used ChatGPT to write these switch statements (head and tail) for debugging - alex, corefuzzing
  // for numRows = 16
  for (i <- 0 until numRows) {
    when ((head & (0x00000001.U << (i)).asUInt) =/= 0.U) {
      rowNum_head := i.U
    }
  }
  
  // switch statements got out of control for tail - condensed with for loop
  // for numRows = 16 and coreWidth = 4
  for (i <- 0 until 16) {
    when ((tail & (0x000000000000000F.U << (i * 4)).asUInt) =/= 0.U) {
      rowNum_tail := i.U
    }
  }

  //-------------------------------------------------------------
  // **** Enqueue Uops ****
  //-------------------------------------------------------------
  // Step 1: Convert FetchPacket into a vector of MicroOps.
  // Step 2: Generate one-hot write indices.
  // Step 3: Write MicroOps into the RAM.

  // adjusting rotate left function to account for varying buffer dimensions - corefuzzing, alex
  def rotateLeft(in: UInt, k: Int) = {
    val n = in.getWidth
    val tail_rotate = Wire(UInt(n.W))
    when (io.reconfigureFB_rows_b1) {
      when(io.reconfigureFB_rows_b0){
        // numRows
        tail_rotate := Cat(in(n-k-1,0), in(n-1, n-k))
      }
      .otherwise{
        // 0.75*numRows
        tail_rotate := Cat(0.U((n/4).W), in((3*n/4)-k-1,0), in((3*n/4)-1, (3*n/4)-k))
      }
    }
    .otherwise {
      when(io.reconfigureFB_rows_b0){
        // 0.5*numRows
        tail_rotate := Cat(0.U((n/2).W), in((n/2)-k-1,0), in((n/2)-1, (n/2)-k))
      }
      .otherwise{
        // 0.25*numRows
        tail_rotate := Cat(0.U((3*n/4).W), in((n/4)-k-1,0), in((n/4)-1, (n/4)-k))
      }
    }   
    // return result - if hot bit got cut off by reconfiguration, reset to bit 0
    Mux(tail_rotate.orR, tail_rotate, 1.U(n.W))
  }

  // original function
  // def rotateLeft(in: UInt, k: Int) = {
  //   val n = in.getWidth
  //   Cat(in(n-k-1,0), in(n-1, n-k))
  // }

  // this mechanism is fine as long as the rotations are done with respect with the current buffer dimensions
  // adjusments to rotateLeft should correct the functionality - alex, corefuzzing
  val might_hit_head = (1 until fetchWidth).map(k => VecInit(rotateLeft(tail, k).asBools.zipWithIndex.filter
    {case (e,i) => i % coreWidth == 0}.map {case (e,i) => e}).asUInt).map(tail => head & tail).reduce(_|_).orR
  val at_head = (VecInit(tail.asBools.zipWithIndex.filter {case (e,i) => i % coreWidth == 0}
    .map {case (e,i) => e}).asUInt & head).orR
  val do_enq = !(at_head && maybe_full || might_hit_head)

  io.enq.ready := do_enq

  // Input microops.
  val in_mask = Wire(Vec(fetchWidth, Bool()))
  val in_uops = Wire(Vec(fetchWidth, new MicroOp()))

  // corefuzzing - ak
  // Running counter for CoreFuzzing uop IDs. This register holds the
  // next ID to assign for the first newly-created micro-op on an
  // enqueue. It is uopIDCounterWidthCF wide and will wrap naturally
  // on overflow. We update this register when we actually commit an
  // enqueue (see when (do_enq) below).
  val uopCount = RegInit(0.U(uopIDCounterWidthCF.W))
  dontTouch(uopCount)

  // Step 1: Convert FetchPacket into a vector of MicroOps.
  for (b <- 0 until nBanks) {
    for (w <- 0 until bankWidth) {
      val i = (b * bankWidth) + w

      val pc = (bankAlign(io.enq.bits.pc) + (i << 1).U)

      in_uops(i)                := DontCare
      in_mask(i)                := io.enq.valid && io.enq.bits.mask(i)
      in_uops(i).edge_inst      := false.B
      in_uops(i).debug_pc       := pc
      in_uops(i).pc_lob         := pc

      in_uops(i).is_sfb         := io.enq.bits.sfbs(i) || io.enq.bits.shadowed_mask(i)

      if (w == 0) {
        when (io.enq.bits.edge_inst(b)) {
          in_uops(i).debug_pc  := bankAlign(io.enq.bits.pc) + (b * bankBytes).U - 2.U
          in_uops(i).pc_lob    := bankAlign(io.enq.bits.pc) + (b * bankBytes).U
          in_uops(i).edge_inst := true.B
        }
      }
      in_uops(i).ftq_idx        := io.enq.bits.ftq_idx
      in_uops(i).inst           := io.enq.bits.exp_insts(i)
      in_uops(i).debug_inst     := io.enq.bits.insts(i)
      in_uops(i).is_rvc         := io.enq.bits.insts(i)(1,0) =/= 3.U
      in_uops(i).taken          := io.enq.bits.cfi_idx.bits === i.U && io.enq.bits.cfi_idx.valid

      in_uops(i).xcpt_pf_if     := io.enq.bits.xcpt_pf_if
      in_uops(i).xcpt_ae_if     := io.enq.bits.xcpt_ae_if
      in_uops(i).bp_debug_if    := io.enq.bits.bp_debug_if_oh(i)
      in_uops(i).bp_xcpt_if     := io.enq.bits.bp_xcpt_if_oh(i)

      in_uops(i).debug_fsrc     := io.enq.bits.fsrc
    }
  }

  // corefuzzing
  // Assign cf_op_count_id for each created micro-op.
  //
  // Because multiple micro-ops are created in parallel (up to
  // `fetchWidth`), we compute for each micro-op an offset equal to
  // the number of valid micro-ops that appear before it in the
  // current fetch packet. We use PopCount on the prefix of
  // `in_mask` to compute this prefix-count. The uop's cf_op_count_id
  // is then (uopCount + prefix_count). This guarantees that the
  // created micro-ops receive consecutive IDs in program order even
  // though they are formed in parallel. The addition wraps naturally
  // to the configured width `uopIDCounterWidthCF`.
  for (i <- 0 until fetchWidth) {
    val priorValids = if (i == 0) 0.U else PopCount(in_mask.slice(0, i)).asUInt
    // Resize priorValids to the counter width before adding
    val id = (uopCount + priorValids)(uopIDCounterWidthCF - 1, 0)
    in_uops(i).cf_op_count_id := id
  }

  // Step 2. Generate one-hot write indices.
  val enq_idxs = Wire(Vec(fetchWidth, UInt(numEntries.W)))

  // adjusted inc function for corefuzzing - alex
  // this now uses only a percentage of the available space in the buffer based on the fetch buffer CSR
  def inc(ptr: UInt) = {
    val n = ptr.getWidth
    val tail_rotate = Wire(UInt(n.W))
    when (io.reconfigureFB_rows_b1) {
      when(io.reconfigureFB_rows_b0){
        // numRows
        tail_rotate := Cat(ptr(n-2,0), ptr(n-1))
      }
      .otherwise{
        // 0.75*numRows
        tail_rotate := Cat(0.U((n/4).W), ptr((3*n/4)-2,0), ptr((3*n/4)-1))
      }
    }
    .otherwise {
      when(io.reconfigureFB_rows_b0){
        // 0.5*numRows
        tail_rotate := Cat(0.U((n/2).W), ptr((n/2)-2,0), ptr((n/2)-1))
      }
      .otherwise{
        // 0.25*numRows
        tail_rotate := Cat(0.U((3*n/4).W), ptr((n/4)-2,0), ptr((n/4)-1))
      }
    }   
    // return result - if hot bit got cut off by reconfiguration, reset to bit 0
    Mux(tail_rotate.orR, tail_rotate, 1.U(n.W))
  }

  var enq_idx = tail
  for (i <- 0 until fetchWidth) {
    enq_idxs(i) := enq_idx
    enq_idx = Mux(in_mask(i), inc(enq_idx), enq_idx)
    // original line - modified for corefuzzing - alex
    // enq_idx = Mux(in_mask(i), inc(enq_idx), enq_idx)
  }

  // Step 3: Write MicroOps into the RAM.
  for (i <- 0 until fetchWidth) {
    for (j <- 0 until numEntries) {
      when (do_enq && in_mask(i) && enq_idxs(i)(j)) {
        ram(j) := in_uops(i)
        // for debugging: printing ram index for every enqueue - alex
        printf(p"($rowsUsed, $coreWidth, enq, $j), ")
      }
    }
  }

  //-------------------------------------------------------------
  // **** Dequeue Uops ****
  //-------------------------------------------------------------

  // doesn't need modification for varying buffer dimensions because head and tail are rotated appropriately - alex, corefuzzing
  val tail_collisions = VecInit((0 until numEntries).map(i =>
                          head(i/coreWidth) && (!maybe_full || (i % coreWidth != 0).B))).asUInt & tail
  val slot_will_hit_tail = (0 until numRows).map(i => tail_collisions((i+1)*coreWidth-1, i*coreWidth)).reduce(_|_)
  val will_hit_tail = slot_will_hit_tail.orR

  val do_deq = io.deq.ready && !will_hit_tail

  val deq_valids = (~MaskUpper(slot_will_hit_tail)).asBools

  // Generate vec for dequeue read port.
  for (i <- 0 until numEntries) {
    deq_vec(i/coreWidth)(i%coreWidth) := ram(i)
  }

  io.deq.bits.uops zip deq_valids           map {case (d,v) => d.valid := v}
  io.deq.bits.uops zip Mux1H(head, deq_vec) map {case (d,q) => d.bits  := q}
  io.deq.valid := deq_valids.reduce(_||_)

  //-------------------------------------------------------------
  // **** Update State ****
  //-------------------------------------------------------------

  // rotating enq_idx appropriately for the given buffer dimensions results in a properly rotated tail - alex, corefuzzing
  when (do_enq) {
    tail := enq_idx
    when (in_mask.reduce(_||_)) {
      maybe_full := true.B
    }
    // corefuzzing
    // Increment the running uop counter by the number of micro-ops
    // actually enqueued in this cycle. We use PopCount on
    // `in_mask` to count them. This update happens only when the
    // enqueue actually fires (do_enq), so the counter remains stable
    // otherwise.
    uopCount := uopCount + PopCount(in_mask).asUInt
  }

  // modifying inc() function to account for varying buffer dimensions resuls in properly rotated head - alex, corefuzzing
  when (do_deq) {
    head := inc(head)
    maybe_full := false.B
    // debugging: printing the row index for each dequeue - alex
    printf(p"($rowsUsed, deq, $rowNum_head), ")
  }

  when (io.clear) {
    head := 1.U
    tail := 1.U
    maybe_full := false.B
  }

  // TODO Is this necessary?
  when (reset.asBool) {
    io.deq.bits.uops map { u => u.valid := false.B }
  }

}
