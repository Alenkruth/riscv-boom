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

package boom.v4.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.rocket.{MStatus, BP, BreakpointUnit}

import boom.v4.common._
import boom.v4.util.{BoolToChar, MaskUpper}

// starting to work on implementing custom CSRs for fetch-buffer size - alex

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
{
  // COREWIDTH MUST BE 4 FOR THIS TO WORK
  //adjusted numFetchBufferEntries for CoreFuzzing project - alex
  val numEntries = 64
  // previous value
  // val numEntries = numFetchBufferEntries
  val io = IO(new BoomBundle {
    val enq = Flipped(Decoupled(new FetchBundle()))
    val deq = new DecoupledIO(new FetchBufferResp())

    // Was the pipeline redirected? Clear/reset the fetchbuffer.
    val clear = Input(Bool())

    // adding for corefuzzing - alex
    val reconfigureFB_width = Input(Bool())
    val reconfigureFB_rows_b0 = Input(Bool())
    val reconfigureFB_rows_b1 = Input(Bool())
  })

  require (numEntries > fetchWidth)
  require (numEntries % coreWidth == 0)
  val numRows = numEntries / coreWidth // 16 for corefuzzing project - alex

  val ram = Reg(Vec(numEntries, new MicroOp))
  ram.suggestName("fb_uop_ram")
  val deq_vec = Wire(Vec(numRows, Vec(coreWidth, new MicroOp)))

  val head = RegInit(1.U(numRows.W)) // width of 16 for core fuzzing
  val tail = RegInit(1.U(numEntries.W)) // width of 64 for core fuzzing

  val maybe_full = RegInit(false.B)

  // fetch-buffer size fuzzing for CoreFuzzing project - alex
  
  // decode width --> core width for mega boom = 4
  // numFetchBufferEntries for mega boom = 32
  // standard numRows = 32/4 = 8
  // two width:
    // 0 --> 2
    // 1 --> 4
  // four numrows reconfigurations: 4, 8, 12, 16
    // 0 --> 4
    // 1 --> 8
    // 2 --> 12
    // 3 --> 16
  // --> max numEntries = 4*16 = 64

  // debugging registers:
  val rowsUsed = Reg(UInt(5.W))
  val widthUsed = Reg(UInt(3.W))
  val rowNum_tail = Reg(UInt(4.W))
  val rowNum_head = Reg(UInt(4.W))
  dontTouch(rowsUsed)
  dontTouch(widthUsed)
  dontTouch(rowNum_tail)
  dontTouch(rowNum_head)


  // used ChatGPT to write these switch statements (head and tail) for debugging - alex
  switch(head) {
    is("b0000000000000001".U) { rowNum_head := 0.U }
    is("b0000000000000010".U) { rowNum_head := 1.U }
    is("b0000000000000100".U) { rowNum_head := 2.U }
    is("b0000000000001000".U) { rowNum_head := 3.U }
    is("b0000000000010000".U) { rowNum_head := 4.U }
    is("b0000000000100000".U) { rowNum_head := 5.U }
    is("b0000000001000000".U) { rowNum_head := 6.U }
    is("b0000000010000000".U) { rowNum_head := 7.U }
    is("b0000000100000000".U) { rowNum_head := 8.U }
    is("b0000001000000000".U) { rowNum_head := 9.U }
    is("b0000010000000000".U) { rowNum_head := 10.U }
    is("b0000100000000000".U) { rowNum_head := 11.U }
    is("b0001000000000000".U) { rowNum_head := 12.U }
    is("b0010000000000000".U) { rowNum_head := 13.U }
    is("b0100000000000000".U) { rowNum_head := 14.U }
    is("b1000000000000000".U) { rowNum_head := 15.U }
  }

    switch(tail) {
    is("h0000000000000001".U) { rowNum_tail := 0.U }
    is("h0000000000000002".U) { rowNum_tail := 0.U }
    is("h0000000000000004".U) { rowNum_tail := 0.U }
    is("h0000000000000008".U) { rowNum_tail := 0.U }
    is("h0000000000000010".U) { rowNum_tail := 1.U }
    is("h0000000000000020".U) { rowNum_tail := 1.U }
    is("h0000000000000040".U) { rowNum_tail := 1.U }
    is("h0000000000000080".U) { rowNum_tail := 1.U }
    is("h0000000000000100".U) { rowNum_tail := 2.U }
    is("h0000000000000200".U) { rowNum_tail := 2.U }
    is("h0000000000000400".U) { rowNum_tail := 2.U }
    is("h0000000000000800".U) { rowNum_tail := 2.U }
    is("h0000000000001000".U) { rowNum_tail := 3.U }
    is("h0000000000002000".U) { rowNum_tail := 3.U }
    is("h0000000000004000".U) { rowNum_tail := 3.U }
    is("h0000000000008000".U) { rowNum_tail := 3.U }
    is("h0000000000010000".U) { rowNum_tail := 4.U }
    is("h0000000000020000".U) { rowNum_tail := 4.U }
    is("h0000000000040000".U) { rowNum_tail := 4.U }
    is("h0000000000080000".U) { rowNum_tail := 4.U }
    is("h0000000000100000".U) { rowNum_tail := 5.U }
    is("h0000000000200000".U) { rowNum_tail := 5.U }
    is("h0000000000400000".U) { rowNum_tail := 5.U }
    is("h0000000000800000".U) { rowNum_tail := 5.U }
    is("h0000000001000000".U) { rowNum_tail := 6.U }
    is("h0000000002000000".U) { rowNum_tail := 6.U }
    is("h0000000004000000".U) { rowNum_tail := 6.U }
    is("h0000000008000000".U) { rowNum_tail := 6.U }
    is("h0000000010000000".U) { rowNum_tail := 7.U }
    is("h0000000020000000".U) { rowNum_tail := 7.U }
    is("h0000000040000000".U) { rowNum_tail := 7.U }
    is("h0000000080000000".U) { rowNum_tail := 7.U }
    is("h0000000100000000".U) { rowNum_tail := 8.U }
    is("h0000000200000000".U) { rowNum_tail := 8.U }
    is("h0000000400000000".U) { rowNum_tail := 8.U }
    is("h0000000800000000".U) { rowNum_tail := 8.U }
    is("h0000001000000000".U) { rowNum_tail := 9.U }
    is("h0000002000000000".U) { rowNum_tail := 9.U }
    is("h0000004000000000".U) { rowNum_tail := 9.U }
    is("h0000008000000000".U) { rowNum_tail := 9.U }
    is("h0000010000000000".U) { rowNum_tail := 10.U }
    is("h0000020000000000".U) { rowNum_tail := 10.U }
    is("h0000040000000000".U) { rowNum_tail := 10.U }
    is("h0000080000000000".U) { rowNum_tail := 10.U }
    is("h0000100000000000".U) { rowNum_tail := 11.U }
    is("h0000200000000000".U) { rowNum_tail := 11.U }
    is("h0000400000000000".U) { rowNum_tail := 11.U }
    is("h0000800000000000".U) { rowNum_tail := 11.U }
    is("h0001000000000000".U) { rowNum_tail := 12.U }
    is("h0002000000000000".U) { rowNum_tail := 12.U }
    is("h0004000000000000".U) { rowNum_tail := 12.U }
    is("h0008000000000000".U) { rowNum_tail := 12.U }
    is("h0010000000000000".U) { rowNum_tail := 13.U }
    is("h0020000000000000".U) { rowNum_tail := 13.U }
    is("h0040000000000000".U) { rowNum_tail := 13.U }
    is("h0080000000000000".U) { rowNum_tail := 13.U }
    is("h0100000000000000".U) { rowNum_tail := 14.U }
    is("h0200000000000000".U) { rowNum_tail := 14.U }
    is("h0400000000000000".U) { rowNum_tail := 14.U }
    is("h0800000000000000".U) { rowNum_tail := 14.U }
    is("h1000000000000000".U) { rowNum_tail := 15.U }
    is("h2000000000000000".U) { rowNum_tail := 15.U }
    is("h4000000000000000".U) { rowNum_tail := 15.U }
    is("h8000000000000000".U) { rowNum_tail := 15.U }
  }

  when (io.reconfigureFB_width) {
    // printf("width 4, ")
    widthUsed := 4.U
  }
  .otherwise {
    // printf("width 2, ")
    widthUsed := 2.U
  }
  when (io.reconfigureFB_rows_b1) {
    when(io.reconfigureFB_rows_b0){
      // printf("rows 16\n")
      rowsUsed := 16.U
    }
    .otherwise{
      // printf("rows 12\n")
      rowsUsed := 12.U
    }
  }
  .otherwise {
    when(io.reconfigureFB_rows_b0){
      // printf("rows 8\n") 
      rowsUsed := 8.U 
    }
    .otherwise{
      // printf("rows 4\n")
      rowsUsed := 4.U
    }
  }

  //-------------------------------------------------------------
  // **** Enqueue Uops ****
  //-------------------------------------------------------------
  // Step 1: Convert FetchPacket into a vector of MicroOps.
  // Step 2: Generate one-hot write indices.
  // Step 3: Write MicroOps into the RAM.

  // adjusting rotate left function to account for varying buffer dimensions - corefuzzing, alex
  // TODO: what do I do about the case where the hot bit is out of range
  def rotateLeft(in: UInt, k: Int) = {
    // width of 64
    val tail_rotate = Wire(UInt(64.W))
    when (io.reconfigureFB_rows_b1) {
      when(io.reconfigureFB_rows_b0){
        // 16 rows
        tail_rotate := Cat(in(64-k-1,0), in(64-1, 64-k))
      }
      .otherwise{
        // 12 rows
        tail_rotate := Cat(0.U(16.W), in(48-k-1,0), in(48-1, 48-k))
      }
    }
    .otherwise {
      when(io.reconfigureFB_rows_b0){
        // 8 rows
        tail_rotate := Cat(0.U(32.W), in(32-k-1,0), in(32-1, 32-k))
      }
      .otherwise{
        // 4 rows
        tail_rotate := Cat(0.U(48.W), in(16-k-1,0), in(16-1, 16-k))
      }
    }   
    // return result - if hot bit got cut off by reconfiguration, reset to bit 0
    Mux(tail_rotate.orR, tail_rotate, 1.U(64.W))
  }

  // original function
  // def rotateLeft(in: UInt, k: Int) = {
  //   val n = in.getWidth
  //   Cat(in(n-k-1,0), in(n-1, n-k))
  // }

  // this mechanism is fine as long as the rotations are done with respect with the current buffer dimensions
  // adjusments to rotateLeft should correct the functionality - alex, corefuzzing
  // if using half width --> half fetch width, mechanism doesn't need to be modified because half a fetch will still fill up a whole row with half width
  val might_hit_head = (1 until fetchWidth).map(k => VecInit(rotateLeft(tail, k).asBools.zipWithIndex.filter
    {case (e,i) => i % coreWidth == 0}.map {case (e,i) => e}).asUInt).map(tail => head & tail).reduce(_|_).orR
  val at_head = (VecInit(tail.asBools.zipWithIndex.filter {case (e,i) => i % coreWidth == 0}
    .map {case (e,i) => e}).asUInt & head).orR
  val do_enq = !(at_head && maybe_full || might_hit_head)

  io.enq.ready := do_enq

  // Input microops.
  val in_mask = Wire(Vec(fetchWidth, Bool()))
  val in_uops = Wire(Vec(fetchWidth, new MicroOp()))

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

  // Step 2. Generate one-hot write indices.
  val enq_idxs = Wire(Vec(fetchWidth, UInt(numEntries.W)))

  // splitting inc function into two so that adjustments to the rotation based on the buffer dimensions can be made - alex, corefuzzing
  def inc_tail(ptr: UInt) = {
    // width of 64
    val tail_shift = Wire(UInt(64.W))
    when (io.reconfigureFB_rows_b1) {
      when(io.reconfigureFB_rows_b0){
        // 16 rows
        tail_shift := Cat(ptr(62, 0), ptr(63))
      }
      .otherwise{
        // 12 rows
        tail_shift := Cat(0.U(16.W), ptr(46, 0), ptr(47))
      }
    }
    .otherwise {
      when(io.reconfigureFB_rows_b0){
        // 8 rows
        tail_shift := Cat(0.U(32.W), ptr(30, 0), ptr(31)) 
      }
      .otherwise{
        // 4 rows
        tail_shift := Cat(0.U(48.W), ptr(14, 0), ptr(15))
      }
    }   
    // return result - if hot bit got cut off by reconfiguration, reset to bit 0
    Mux(tail_shift.orR, tail_shift, 1.U(64.W))
  }

  def inc_head(ptr: UInt) = {
    // width of 16
    val head_shift = Wire(UInt(16.W))
    when (io.reconfigureFB_rows_b1) {
      when(io.reconfigureFB_rows_b0){
        // 16 rows
        head_shift := Cat(ptr(14, 0), ptr(15))
      }
      .otherwise{
        // 12 rows
        head_shift := Cat(0.U(4.W), ptr(10, 0), ptr(11))
      }
    }
    .otherwise {
      when(io.reconfigureFB_rows_b0){
        // 8 rows
        head_shift := Cat(0.U(8.W), ptr(6, 0), ptr(7)) 
      }
      .otherwise{
        // 4 rows
        head_shift := Cat(0.U(12.W), ptr(2, 0), ptr(3))
      }
    }  
    // return result - if hot bit got cut off by reconfiguration, reset to bit 0
      Mux(head_shift.orR, head_shift, 1.U(16.W))  
  }
  // original function:
  // def inc(ptr: UInt) = {
  //   val n = ptr.getWidth
  //   Cat(ptr(n-2,0), ptr(n-1))
  // }

  var enq_idx = tail
  for (i <- 0 until fetchWidth) {
    enq_idxs(i) := enq_idx
    enq_idx = Mux(in_mask(i), inc_tail(enq_idx), enq_idx)
    // original line - modified for corefuzzing - alex
    // enq_idx = Mux(in_mask(i), inc(enq_idx), enq_idx)
  }

  // Step 3: Write MicroOps into the RAM.
  // core fuzzing - alex:
  // note: for width reconfigurations, instead of cutting off the 3rd and 4th item of the fetch packet at the input,
  // only the 1st and 2nd are inputted of the in_uops array - the 3rd and 4th are still handled with a width of 2 but not entered
  for (i <- 0 until fetchWidth) {
    for (j <- 0 until numEntries) {
      when (do_enq && in_mask(i) && enq_idxs(i)(j)) {
        ram(j) := in_uops(i)
        // for debugging: printing ram index for every enqueue - alex
        printf(p"($rowsUsed, $widthUsed, enq, $j), ")
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
  }

  // modifying inc() function to account for varying buffer dimensions resuls in properly rotated head - alex, corefuzzing
  when (do_deq) {
    head := inc_head(head)
    // original line, modified for corefuzzing - alex
    // head := inc(head)
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
