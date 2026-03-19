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

// for Corefuzzing
import freechips.rocketchip.util._

import boom.v3.common._
import boom.v3.exu.{BrUpdateInfo}
import boom.v3.util.{BoolToChar, MaskUpper, Sext, SpeculativePrintf, appendModuleTag, addInfluencer, inflBitmapFromList}
import freechips.rocketchip.util.CoreFuzzingConstants
// imports for corefuzzing

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
    val cf_debug_fetchbuf_enable = Input(Bool())
    // corefuzzing: branch resolution info for [FLUSH] logging on clear
    val brupdate            = Input(new BrUpdateInfo())
    val cf_debug_rob_enable = Input(Bool())

    // Was the pipeline redirected? Clear/reset the fetchbuffer.
    val clear = Input(Bool())

    // adding for corefuzzing - alex
    val reconfigureFB_rows_b0 = Input(Bool())
    val reconfigureFB_rows_b1 = Input(Bool())
  })

  // original
  require (numEntries > fetchWidth, "fetchbuffer.numEntries <= core.fetchwidth")
  // modified for smallest num of entries used
  // require (numEntries/4 >= fetchWidth)
  require (numEntries % coreWidth == 0, "fetchbuffer.numEntries % corewidth needs to be zero, my friend")
  val numRows = numEntries / coreWidth

  val ram = Reg(Vec(numEntries, new MicroOp))
  ram.suggestName("fb_uop_ram")
  val deq_vec = Wire(Vec(numRows, Vec(coreWidth, new MicroOp)))

  // corefuzzing: track which RAM slots actually hold valid (enqueued, not yet dequeued/cleared)
  // data. Used to suppress [FLUSH] printing for uninitialized or already-consumed slots.
  val ram_valid = RegInit(VecInit(Seq.fill(numEntries)(false.B)))


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


  // adding safe splice logic to fix indices going below zero
  def safeSlice(in: UInt, high: Int, low: Int): UInt = {
    val h = math.max(high, 0)
    val l = math.max(low, 0)
    if (h >= l) in(h, l) else 0.U
  }
  
  def rotateLeft(in: UInt, k: Int) = {
    val n = in.getWidth
    val tail_rotate = Wire(UInt(n.W))
  
    when(io.reconfigureFB_rows_b1) {
      when(io.reconfigureFB_rows_b0) {
        tail_rotate := Cat(in(n - k - 1, 0), in(n - 1, n - k))
      } .otherwise {
        val seg = n / 4 * 3
        tail_rotate := Cat(
          0.U((n / 4).W),
          safeSlice(in, (3 * n / 4) - k - 1, 0),
          safeSlice(in, (3 * n / 4) - 1, (3 * n / 4) - k)
        )
      }
    } .otherwise {
      when(io.reconfigureFB_rows_b0) {
        tail_rotate := Cat(
          0.U((n / 2).W),
          safeSlice(in, (n / 2) - k - 1, 0),
          safeSlice(in, (n / 2) - 1, (n / 2) - k)
        )
      } .otherwise {
        tail_rotate := Cat(
          0.U((3 * n / 4).W),
          safeSlice(in, (n / 4) - k - 1, 0),
          safeSlice(in, (n / 4) - 1, (n / 4) - k)
        )
      }
    }
    Mux(tail_rotate.orR, tail_rotate, 1.U(n.W))
  }

  // adjusting rotate left function to account for varying buffer dimensions - corefuzzing, alex
  // def rotateLeft(in: UInt, k: Int) = {
  //   val n = in.getWidth
  //   val tail_rotate = Wire(UInt(n.W))
  //   when (io.reconfigureFB_rows_b1) {
  //     when(io.reconfigureFB_rows_b0){
  //       // numRows
  //       tail_rotate := Cat(in(n-k-1,0), in(n-1, n-k))
  //     }
  //     .otherwise{
  //       // 0.75*numRows
  //       tail_rotate := Cat(0.U((n/4).W), in((3*n/4)-k-1,0), in((3*n/4)-1, (3*n/4)-k))
  //     }
  //   }
  //   .otherwise {
  //     when(io.reconfigureFB_rows_b0){
  //       // 0.5*numRows
  //       tail_rotate := Cat(0.U((n/2).W), in((n/2)-k-1,0), in((n/2)-1, (n/2)-k))
  //     }
  //     .otherwise{
  //       // 0.25*numRows
  //       tail_rotate := Cat(0.U((3*n/4).W), in((n/4)-k-1,0), in((n/4)-1, (n/4)-k))
  //     }
  //   }   
  //   // return result - if hot bit got cut off by reconfiguration, reset to bit 0
  //   Mux(tail_rotate.orR, tail_rotate, 1.U(n.W))
  // }

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
  // corefuzzing: block enqueue in the same cycle as io.clear to keep head/tail/ram_valid
  // consistent (all reset to initial values at the clock edge; a concurrent enqueue would
  // write ram/tail using the old pointers and then tail would snap back to 1.U, losing data).
  val do_enq = !(at_head && maybe_full || might_hit_head) && !io.clear

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
    // Only assign opcount to valid slots; invalid slots get 0 to avoid gaps
    in_uops(i).cf_op_count_id := Mux(in_mask(i), id, 0.U)
  }

  // corefuzzing: Initialize all IFT cf_* fields for each newly created micro-op.
  // cf_fu_bitmap gets the fbTagCF bit set to mark this uop passed through the fetch buffer.
  // All other cf_* fields are zeroed here; they are set later in the pipeline
  // (domain_id and speculated at dispatch, secret flags in LSU/DCache).
  // corefuzzing: FU bitmap base — all instructions pass through all IFU-stage modules.
  // ras/bpd/btb bits are set unconditionally (path tracking). Domain mismatch events
  // are tracked separately via the influencer list (INFL_RAS_STATE, INFL_BPD_STATE, etc.).
  for (i <- 0 until fetchWidth) {
    in_uops(i).cf_fu_bitmap            := (1.U << fbTagCF.U) | (1.U << icacheTagCF.U) | (1.U << itlbTagCF.U) | (1.U << ftqTagCF.U) | (1.U << rasTagCF.U) | (1.U << bpdTagCF.U) | (1.U << btbTagCF.U)
    in_uops(i).cf_domain_id            := 0.U
    in_uops(i).cf_speculated           := false.B
    in_uops(i).cf_attacker_influence   := false.B
    in_uops(i).cf_secret_access        := false.B
    in_uops(i).cf_secret_propagation   := false.B
    in_uops(i).cf_secret_transmission  := false.B
    in_uops(i).cf_single_step          := false.B
    in_uops(i).cf_influencer_list      := 0.U.asTypeOf(in_uops(i).cf_influencer_list)
    in_uops(i).cf_infl_overflow        := false.B
    in_uops(i).cf_src_tainted          := false.B
    in_uops(i).cf_taint_producer_op    := 0.U
  }

  // corefuzzing: inject ICache and RAS domain mismatch influencers for all fetch-packet uops.
  // Use explicit 0-valued base uop for influencer inputs to avoid circular Wire dependencies.
  for (i <- 0 until fetchWidth) {
    // base_uop copies in_uops(i) but overrides influencer fields with known-zero values,
    // breaking any feedback from later assignments back into addInfluencer.
    val base_uop = Wire(in_uops(i).cloneType)
    base_uop := in_uops(i)
    base_uop.cf_influencer_list := 0.U.asTypeOf(in_uops(i).cf_influencer_list)
    base_uop.cf_infl_overflow   := false.B

    // Chain: add ICACHE first, then RAS on top
    // For hardware state mismatches: if current uop is victim (domain=0), the HW state was
    // populated by attacker → is_atk=true.  is_secret not tracked at fetch-buffer level.
    val fb_is_victim = base_uop.cf_domain_id === 0.U
    val post_icache = addInfluencer(base_uop, 0.U, INFL_ICACHE_STATE.U,
      is_atk = fb_is_victim, is_secret = false.B)

    val mid = Wire(in_uops(i).cloneType)
    mid := base_uop
    when (io.enq.bits.icache_domain_mismatch) {
      mid.cf_influencer_list := post_icache.cf_influencer_list
      mid.cf_infl_overflow   := post_icache.cf_infl_overflow
    }

    val post_ras = addInfluencer(mid, 0.U, INFL_RAS_STATE.U,
      is_atk = fb_is_victim, is_secret = false.B)

    val mid2 = Wire(in_uops(i).cloneType)
    mid2 := mid
    when (io.enq.bits.ras_domain_mismatch) {
      mid2.cf_influencer_list := post_ras.cf_influencer_list
      mid2.cf_infl_overflow   := post_ras.cf_infl_overflow
    }

    // corefuzzing: chain BPD (TAGE) and BTB domain mismatch influencers
    val post_bpd = addInfluencer(mid2, 0.U, INFL_BPD_STATE.U,
      is_atk = fb_is_victim, is_secret = false.B)

    val mid3 = Wire(in_uops(i).cloneType)
    mid3 := mid2
    when (io.enq.bits.bpd_domain_mismatch) {
      mid3.cf_influencer_list := post_bpd.cf_influencer_list
      mid3.cf_infl_overflow   := post_bpd.cf_infl_overflow
    }

    val post_btb = addInfluencer(mid3, 0.U, INFL_BTB_STATE.U,
      is_atk = fb_is_victim, is_secret = false.B)

    val mid4 = Wire(in_uops(i).cloneType)
    mid4 := mid3
    when (io.enq.bits.btb_domain_mismatch) {
      mid4.cf_influencer_list := post_btb.cf_influencer_list
      mid4.cf_infl_overflow   := post_btb.cf_infl_overflow
    }

    // corefuzzing: BPD secret mismatch — entry was trained by a secret instruction → s_prop=1
    val post_bpd_secret = addInfluencer(mid4, 0.U, INFL_BPD_STATE.U,
      is_atk = false.B, is_secret = true.B)
    val mid4b = Wire(in_uops(i).cloneType)
    mid4b := mid4
    when (io.enq.bits.bpd_secret_mismatch) {
      mid4b.cf_influencer_list := post_bpd_secret.cf_influencer_list
      mid4b.cf_infl_overflow   := post_bpd_secret.cf_infl_overflow
      in_uops(i).cf_secret_propagation := true.B
    }

    // corefuzzing: BTB secret mismatch — entry was trained by a secret instruction → s_prop=1
    val post_btb_secret = addInfluencer(mid4b, 0.U, INFL_BTB_STATE.U,
      is_atk = false.B, is_secret = true.B)
    val mid4c = Wire(in_uops(i).cloneType)
    mid4c := mid4b
    when (io.enq.bits.btb_secret_mismatch) {
      mid4c.cf_influencer_list := post_btb_secret.cf_influencer_list
      mid4c.cf_infl_overflow   := post_btb_secret.cf_infl_overflow
      in_uops(i).cf_secret_propagation := true.B
    }

    // corefuzzing: ITLB domain mismatch influencer (ty=15)
    val post_itlb = addInfluencer(mid4c, 0.U, INFL_ITLB_STATE.U,
      is_atk = fb_is_victim, is_secret = false.B)

    val final_list = Wire(in_uops(i).cf_influencer_list.cloneType)
    val final_ovf  = Wire(Bool())
    final_list := mid4c.cf_influencer_list
    final_ovf  := mid4c.cf_infl_overflow
    when (io.enq.bits.itlb_domain_mismatch) {
      final_list := post_itlb.cf_influencer_list
      final_ovf  := post_itlb.cf_infl_overflow
    }

    in_uops(i).cf_influencer_list := final_list
    in_uops(i).cf_infl_overflow   := final_ovf
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
        ram(j) := in_uops(i) // in_uops_tagged(i)
        ram_valid(j) := true.B
        // for debugging: printing ram index for every enqueue - alex
        // printf(p"($rowsUsed, $coreWidth, enq, $j), ")
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
    // corefuzzing: invalidate the row being consumed so [FLUSH] won't print stale data
    for (j <- 0 until numEntries) {
      when (head(j / coreWidth)) { ram_valid(j) := false.B }
    }
    // debugging: printing the row index for each dequeue - alex
    // printf(p"($rowsUsed, deq, $rowNum_head), ")
  }

  when (io.clear) {
    // corefuzzing: [FLUSH] printing — one printf per valid entry, all in the io.clear cycle.
    // A single printf call is atomic in Verilator's multi-threaded mode.
    // Dump ALL valid entries (no filter): FB entries haven't yet accumulated influencers
    // since influencer injection happens at dispatch, well after the fetch buffer stage.
    when (io.cf_debug_rob_enable) {
      // Expand influencer slots as fixed-format fields {v=,oc=,ty=} so the entire entry is
      // one printf call — no per-slot conditional printf loop needed.
      val flushInflFmt = (0 until numInfluencerSlotsCF).zipWithIndex.map{case(_,k) => s"I$k={v=%d,oc=%d,ty=%d,atk=%d,sec=%d,dc=%d}"}.mkString(" ")
      val flushFmt = s"[FLUSH] 0x%x (0x%x) CF(domain=%d spec=%d atk=%d s_acc=%d s_prop=%d s_tx=%d opcount=%d spec_atk=%d spec_oc=%d) FU=0x%x SRC=%d INFL_FU=0x%x OVF=%d $flushInflFmt\n"
      for (i <- 0 until numEntries) {
        val base_uop = ram(i)
        val use_uop  = WireInit(base_uop)
        when (io.brupdate.b2.mispredict &&
              io.brupdate.b2.uop.cf_domain_id =/= base_uop.cf_domain_id) {
          val fb_br_uop = io.brupdate.b2.uop
          use_uop := addInfluencer(base_uop, fb_br_uop.cf_op_count_id, INFL_PIPELINE_FLUSH.U,
            is_atk = fb_br_uop.cf_domain_id === 1.U,
            is_secret = fb_br_uop.cf_secret_access || fb_br_uop.cf_secret_propagation)
        }
        when (ram_valid(i)) {
          val inflArgs = (0 until numInfluencerSlotsCF).flatMap(k => Seq[Bits](
            use_uop.cf_influencer_list(k).valid,
            use_uop.cf_influencer_list(k).op_count,
            use_uop.cf_influencer_list(k).infl_type,
            use_uop.cf_influencer_list(k).is_atk,
            use_uop.cf_influencer_list(k).is_secret,
            use_uop.cf_influencer_list(k).deny_count
          ))
          printf(flushFmt, (Seq[Bits](
            Sext.apply(use_uop.debug_pc(vaddrBits-1,0), xLen),
            use_uop.debug_inst,
            use_uop.cf_domain_id,
            use_uop.cf_speculated,
            use_uop.cf_attacker_influence,
            use_uop.cf_secret_access,
            use_uop.cf_secret_propagation,
            use_uop.cf_secret_transmission,
            use_uop.cf_op_count_id,
            use_uop.cf_spec_branch_is_atk,
            use_uop.cf_spec_branch_op_id,
            use_uop.cf_fu_bitmap,
            0.U, inflBitmapFromList(use_uop.cf_influencer_list),
            use_uop.cf_infl_overflow
          ) ++ inflArgs): _*)
        }
      }
    }

    head := 1.U
    tail := 1.U
    maybe_full := false.B
    ram_valid := VecInit(Seq.fill(numEntries)(false.B))
  }

  // TODO Is this necessary?
  when (reset.asBool) {
    io.deq.bits.uops map { u => u.valid := false.B }
  }

}
