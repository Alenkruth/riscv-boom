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
import boom.v3.util.{BoolToChar, MaskUpper, Sext, SpeculativePrintf, appendModuleTag, addInfluencer, addInfluencerBatch, InfluencerCandidate, inflBitmapFromList}
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

    // 3-bit binary index into fetchBufferEntryOptions = Seq(128, 64, 32, 24, 16, 8)
    val cf_fb_idx = Input(UInt(3.W))

    // Attacker address range — used to compute cf_domain_id at fetch-buffer fill time so
    // [FLUSH] SRC=0 entries carry the correct domain even before decode.
    val cf_attacker_start_addr = Input(UInt(vaddrBitsExtended.W))
    val cf_attacker_end_addr   = Input(UInt(vaddrBitsExtended.W))
    // 3-bit active width {4, 2, 1}: controls sub-row slicing and fetch cooldown
    val cf_active_width     = Input(UInt(3.W))
    // High when fetch cooldown is zero (gates IFU allow_fetch in the frontend)
    val fetch_throttle_gate = Output(Bool())
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
  // Sub-row slicing: which group of N slots is being served from the current head row.
  // Resets on flush (io.clear) so the next fetch starts from slot 0 of a fresh row.
  val sub_row_cnt = RegInit(0.U(log2Ceil(coreWidth).W))
  // Fetch cooldown: cycles remaining before IFU may issue the next fetch bundle.
  // Prevents IFU from over-filling the FetchBuffer relative to decode throughput.
  val fetch_cooldown_cnt = RegInit(0.U(3.W))

  // fetch-buffer size: runtime-reconfigurable via cf_fb_idx CSR
  // rows per option = fetchBufferEntryOptions.map(_ / coreWidth)
  val rowsOptionsVec = VecInit(fetchBufferEntryOptions.map(e => (e / coreWidth).U))
  val rowsUsed = WireInit(rowsOptionsVec(io.cf_fb_idx))
  dontTouch(rowsUsed)
  val rowNum_tail = Reg(UInt(5.W))
  val rowNum_head = Reg(UInt(5.W))
  dontTouch(rowNum_tail)
  dontTouch(rowNum_head)

  // groups_per_row: how many N-slot sub-groups fit in one coreWidth row.
  // Derived from coreWidth and active width so values update if parameters change.
  // N=coreWidth → 1 group (no slicing), N=coreWidth/2 → 2 groups, N=1 → coreWidth groups
  val groups_per_row = MuxLookup(io.cf_active_width, 1.U(3.W))(
    Seq(2.U -> (coreWidth/2).U, 1.U -> coreWidth.U))
  // last_group: true when this is the final sub-group of the current row
  // At N=coreWidth: groups_per_row=1, sub_row_cnt=0, 0===0 → always true (no change to head advance)
  val last_group = (sub_row_cnt === groups_per_row - 1.U)
  // fetch_cooldown_period: (fetchWidth/N) - 1; 0 at N=coreWidth so gate is always open.
  // Derived from fetchWidth/coreWidth parameters so values update if parameters change.
  val fetch_cooldown_period = MuxLookup(io.cf_active_width, 0.U(3.W))(
    Seq(2.U -> (fetchWidth/2 - 1).U, 1.U -> (fetchWidth - 1).U))

  // used ChatGPT to write these switch statements (head and tail) for debugging - alex, corefuzzing
  // for numRows = 16
  for (i <- 0 until numRows) {
    when ((head & (0x00000001.U << (i)).asUInt) =/= 0.U) {
      rowNum_head := i.U
    }
  }
  
  // debug: decode one-hot tail to row index (coreWidth=4 bits per row)
  for (i <- 0 until numRows) {
    when ((tail & (0xF.U << (i * 4)).asUInt) =/= 0.U) {
      rowNum_tail := i.U
    }
  }

  //-------------------------------------------------------------
  // **** Enqueue Uops ****
  //-------------------------------------------------------------
  // Step 1: Convert FetchPacket into a vector of MicroOps.
  // Step 2: Generate one-hot write indices.
  // Step 3: Write MicroOps into the RAM.


  // Rotate `in` (numEntries-wide tail pointer, one-hot) left by k positions within the active segment.
  // Parameterized from fetchBufferEntryOptions — no hardcoded sizes.
  // k is a Scala Int (compile-time constant), so all bit-slice bounds are elaboration-time constants.
  def rotateLeft(in: UInt, k: Int) = {
    val cases = fetchBufferEntryOptions.zipWithIndex.map { case (n, idx) =>
      val pad = numEntries - n
      val rotated = if (pad == 0) Cat(in(n-k-1, 0), in(n-1, n-k))
                    else Cat(0.U(pad.W), in(n-k-1, 0), in(n-1, n-k))
      (io.cf_fb_idx === idx.U) -> rotated
    }
    val r = MuxCase(0.U(numEntries.W), cases)
    Mux(r.orR, r, 1.U(numEntries.W))
  }

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
    // Compute domain_id from PC at fetch-buffer fill time (domain is purely PC-based).
    // This ensures [FLUSH] SRC=0 entries have the correct domain when clear fires.
    val fb_in_attacker = (io.cf_attacker_start_addr =/= io.cf_attacker_end_addr) &&
                         (in_uops(i).debug_pc >= io.cf_attacker_start_addr) &&
                         (in_uops(i).debug_pc < io.cf_attacker_end_addr)
    in_uops(i).cf_domain_id            := fb_in_attacker.asUInt
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

  // corefuzzing: inject fetch-side influencers in parallel using addInfluencerBatch.
  // All 8 candidates are evaluated from a single zero-base template and the result is
  // broadcast to all fetchWidth uops (~10 gate levels, computed once vs fetchWidth times).
  //
  // is_atk approximation: use any_is_victim = OR(domain=0 across all uops in packet).
  // This is conservative (never a false negative): if any uop is a victim, all uops get
  // is_atk=true for domain-mismatch influencers. Attacker-domain uops in a mixed-domain
  // packet may get is_atk=true (false positive), which is acceptable.
  val any_is_victim  = VecInit((0 until fetchWidth).map(i => in_uops(i).cf_domain_id === 0.U)).reduce(_ || _)
  val ras_pop_secret = io.enq.bits.ras_pop_secret

  // Zero-base template: ensures base_idx=0 so slot = prefix(k) only.
  val fb_base_tmpl = Wire(in_uops(0).cloneType)
  fb_base_tmpl := in_uops(0)
  fb_base_tmpl.cf_influencer_list    := 0.U.asTypeOf(in_uops(0).cf_influencer_list)
  fb_base_tmpl.cf_infl_overflow      := false.B
  // Break combinational cycle: addInfluencerBatch reads cf_attacker_influence via WireInit,
  // and the result is written back to in_uops.cf_attacker_influence.  Zero it here so the
  // batch output depends only on candidate conditions, not on its own output.
  // Semantics are preserved: in_uops(i).cf_attacker_influence is already false.B at line 250.
  fb_base_tmpl.cf_attacker_influence := false.B

  val fb_post = addInfluencerBatch(fb_base_tmpl, Seq(
    InfluencerCandidate(io.enq.bits.icache_domain_mismatch, 0.U, INFL_ICACHE_STATE.U, any_is_victim, false.B),
    InfluencerCandidate(io.enq.bits.ras_domain_mismatch,    0.U, INFL_RAS_STATE.U,    any_is_victim, false.B),
    InfluencerCandidate(ras_pop_secret,                      0.U, INFL_RAS_STATE.U,    false.B,       true.B),
    InfluencerCandidate(io.enq.bits.bpd_domain_mismatch,    0.U, INFL_BPD_STATE.U,    any_is_victim, false.B),
    InfluencerCandidate(io.enq.bits.bpd_secret_mismatch,    0.U, INFL_BPD_STATE.U,    false.B,       true.B),
    InfluencerCandidate(io.enq.bits.btb_domain_mismatch,    0.U, INFL_BTB_STATE.U,    any_is_victim, false.B),
    InfluencerCandidate(io.enq.bits.btb_secret_mismatch,    0.U, INFL_BTB_STATE.U,    false.B,       true.B),
    InfluencerCandidate(io.enq.bits.itlb_domain_mismatch,   0.U, INFL_ITLB_STATE.U,   any_is_victim, false.B),
    InfluencerCandidate(io.enq.bits.itlb_secret_mismatch,   0.U, INFL_ITLB_STATE.U,   false.B,       true.B),
  ))

  // Broadcast shared influencer list to all uops in the packet.
  for (i <- 0 until fetchWidth) {
    in_uops(i).cf_influencer_list := fb_post.cf_influencer_list
    in_uops(i).cf_infl_overflow   := fb_post.cf_infl_overflow
    when (fb_post.cf_attacker_influence) {
      in_uops(i).cf_attacker_influence := true.B
    }
    // Secret propagation: set when BPD or BTB was trained by a secret instruction
    when (io.enq.bits.bpd_secret_mismatch || io.enq.bits.btb_secret_mismatch) {
      in_uops(i).cf_secret_propagation := true.B
    }
  }

  // Step 2. Generate one-hot write indices.
  val enq_idxs = Wire(Vec(fetchWidth, UInt(numEntries.W)))

  // Advance head pointer (numRows-wide one-hot) by one row within the active segment.
  // Parameterized from fetchBufferEntryOptions — no hardcoded sizes.
  def inc(ptr: UInt) = {
    val cases = fetchBufferEntryOptions.zipWithIndex.map { case (n, idx) =>
      val rows = n / coreWidth
      val pad  = numRows - rows
      val rotated = if (rows == 1) Cat(0.U(pad.W), ptr(0))
                    else if (pad == 0) Cat(ptr(rows-2, 0), ptr(rows-1))
                    else Cat(0.U(pad.W), ptr(rows-2, 0), ptr(rows-1))
      (io.cf_fb_idx === idx.U) -> rotated
    }
    val r = MuxCase(0.U(numRows.W), cases)
    Mux(r.orR, r, 1.U(numRows.W))
  }

  // Use rotateLeft(enq_idx, 1) to advance the numEntries-wide entry-level tail pointer by one entry
  // within the active buffer segment.  inc() operates on the numRows-wide row-level head pointer
  // only; calling inc() on a wide tail would zero-extend and corrupt it.
  var enq_idx = tail
  for (i <- 0 until fetchWidth) {
    enq_idxs(i) := enq_idx
    enq_idx = Mux(in_mask(i), rotateLeft(enq_idx, 1), enq_idx)
  }

  // Pad the final tail pointer to the next row boundary.
  // A partial fetch packet (in_mask not all-ones) leaves enq_idx in the middle of a row.
  // Leaving tail there causes will_hit_tail=1 → do_deq=0 → pipeline deadlock.
  // Advance at most coreWidth-1 steps until we land on a row-start position.
  val enq_is_row_start = VecInit(
    (0 until numEntries).filter(_ % coreWidth == 0).map(j => enq_idx(j))).reduce(_ || _)
  var padded_enq_idx_v = enq_idx
  for (_ <- 1 until coreWidth) {
    val p_at_start = VecInit(
      (0 until numEntries).filter(_ % coreWidth == 0).map(j => padded_enq_idx_v(j))).reduce(_ || _)
    padded_enq_idx_v = Mux(p_at_start, padded_enq_idx_v, rotateLeft(padded_enq_idx_v, 1))
  }
  val padded_enq_idx = Mux(enq_is_row_start, enq_idx, padded_enq_idx_v)

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

  val deq_valids_mask = (~MaskUpper(slot_will_hit_tail)).asBools
  // Gate each deq slot by ram_valid for the current head row.
  // This prevents stale/uninitialized padding entries (left by row-boundary
  // padding of the tail pointer after a partial enqueue) from appearing valid.
  val head_ram_valid = VecInit((0 until coreWidth).map(j =>
    Mux1H(head, VecInit((0 until numRows).map(i => ram_valid(i * coreWidth + j))))))
  val deq_valids = VecInit(deq_valids_mask.zip(head_ram_valid).map { case (m, rv) => m && rv })

  // Generate vec for dequeue read port.
  for (i <- 0 until numEntries) {
    deq_vec(i/coreWidth)(i%coreWidth) := ram(i)
  }

  // Sub-row mask: slot w is active when it belongs to the current sub-group.
  // Uses compile-time Scala integer division (w/2) — hardware-friendly constant folding.
  // N=4: MuxCase default=true → all 4 slots always active (no slicing at full width)
  // N=2: sub_row_cnt=0 → slots 0,1 (w/2==0); sub_row_cnt=1 → slots 2,3 (w/2==1)
  // N=1: sub_row_cnt==w → exactly one slot active per cycle
  val sub_row_mask = VecInit((0 until coreWidth).map { w =>
    MuxCase(true.B, Seq(
      (io.cf_active_width === 2.U) -> (sub_row_cnt === (w/2).U),
      (io.cf_active_width === 1.U) -> (sub_row_cnt === w.U)
    ))
  })
  val deq_valids_sub = VecInit(deq_valids.zip(sub_row_mask).map { case (v, m) => v && m })
  io.deq.bits.uops zip deq_valids_sub        map {case (d,v) => d.valid := v}
  io.deq.bits.uops zip Mux1H(head, deq_vec)  map {case (d,q) => d.bits  := q}
  io.deq.valid := deq_valids_sub.reduce(_||_)

  //-------------------------------------------------------------
  // **** Update State ****
  //-------------------------------------------------------------

  // rotating enq_idx appropriately for the given buffer dimensions results in a properly rotated tail - alex, corefuzzing
  when (do_enq) {
    tail := padded_enq_idx
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
    when (last_group) {
      // Final sub-group of this row: advance head to the next row
      sub_row_cnt := 0.U
      head := inc(head)
      maybe_full := false.B
      // corefuzzing: invalidate the row being consumed so [FLUSH] won't print stale data
      for (j <- 0 until numEntries) {
        when (head(j / coreWidth)) { ram_valid(j) := false.B }
      }
    } .otherwise {
      // More sub-groups remain in this row: advance sub-row pointer and invalidate
      // the just-consumed slots so they won't appear in [FLUSH] output.
      sub_row_cnt := sub_row_cnt + 1.U
      for (j <- 0 until numEntries) {
        when (head(j / coreWidth) && sub_row_mask(j % coreWidth)) {
          ram_valid(j) := false.B
        }
      }
    }
    // debugging: printing the row index for each dequeue - alex
    // printf(p"($rowsUsed, deq, $rowNum_head), ")
  }

  // Sub-group empty skip: when N<coreWidth, a correctly-predicted taken CFI in sub-group 0
  // (slot 0 or 1 of a row) produces a partial packet with valid data ONLY in sub-group 0.
  // After do_deq fires once (sub_row_cnt 0→1), sub-group 1 (slots 2,3) has no valid data.
  // Since do_deq requires io.deq.valid (= deq_valids_sub.reduce), it can never fire again
  // → head never advances → pipeline deadlock.
  // Fix: advance sub_row_cnt (and head on last_group) whenever the current sub-group is
  // empty AND the buffer is not empty (will_hit_tail=false) AND not being cleared.
  // This is mutually exclusive with do_deq (do_deq requires io.deq.valid = true;
  // sub_group_empty requires it to be false). io.clear (below) still overrides both.
  val sub_group_empty = !deq_valids_sub.reduce(_||_) && !will_hit_tail && !io.clear

  when (sub_group_empty) {
    when (last_group) {
      sub_row_cnt := 0.U
      head        := inc(head)
      maybe_full  := false.B
      for (j <- 0 until numEntries) {
        when (head(j / coreWidth)) { ram_valid(j) := false.B }
      }
    } .otherwise {
      sub_row_cnt := sub_row_cnt + 1.U
      for (j <- 0 until numEntries) {
        when (head(j / coreWidth) && sub_row_mask(j % coreWidth)) {
          ram_valid(j) := false.B
        }
      }
    }
  }

  // Undefined width index guard: indices 0→N=coreWidth, 1→N=2, 2→N=1; index 3 is reserved.
  assert(io.cf_active_width === 1.U || io.cf_active_width === 2.U || io.cf_active_width === coreWidth.U,
    "[fetch-buffer] cf_active_width must be 1, 2, or coreWidth; index 3 is reserved")

  // Fetch cooldown: starts on each IFU enqueue; decays to 0 each cycle.
  // At N=coreWidth: fetch_cooldown_period=0, so cnt stays 0 and gate is always 1 (baseline unchanged).
  // On io.clear: reset below ensures IFU can fetch immediately after a redirect.
  when (io.enq.fire) {
    fetch_cooldown_cnt := fetch_cooldown_period
  } .elsewhen (fetch_cooldown_cnt > 0.U) {
    fetch_cooldown_cnt := fetch_cooldown_cnt - 1.U
  }
  io.fetch_throttle_gate := (fetch_cooldown_cnt === 0.U)

  when (io.clear) {
    // corefuzzing: [FLUSH] printing — one printf per valid entry, all in the io.clear cycle.
    // A single printf call is atomic in Verilator's multi-threaded mode.
    // Dump ALL valid entries (no filter): FB entries haven't yet accumulated influencers
    // since influencer injection happens at dispatch, well after the fetch buffer stage.
    when (io.cf_debug_rob_enable) {
      // Expand influencer slots as fixed-format fields {v=,oc=,ty=} so the entire entry is
      // one printf call — no per-slot conditional printf loop needed.
      val flushInflFmt = (0 until numInfluencerSlotsCF).zipWithIndex.map{case(_,k) => s"I$k={v=%d,oc=%d,ty=%d,atk=%d,sec=%d,dc=%d}"}.mkString(" ")
      val flushFmt = s"[FLUSH] 0x%x (0x%x) CF(domain=%d spec=%d atk=%d s_acc=%d s_prop=%d s_tx=%d opcount=%d spec_atk=%d spec_oc=%d fl=%d floc=%d) FU=0x%x SRC=%d INFL_FU=0x%x OVF=%d $flushInflFmt\n"
      for (i <- 0 until numEntries) {
        // corefuzzing: read RAM entry directly — no WireInit copy needed since we don't
        // modify the influencer list here.  fl/floc are separate fields (like spec_atk/spec_oc).
        val base_uop = ram(i)
        val fl_cross_domain = io.brupdate.b2.mispredict &&
          (io.brupdate.b2.uop.cf_domain_id =/= base_uop.cf_domain_id)
        val fl_op_count = Mux(io.brupdate.b2.mispredict,
          io.brupdate.b2.uop.cf_op_count_id, 0.U)
        when (ram_valid(i)) {
          val inflArgs = (0 until numInfluencerSlotsCF).flatMap(k => Seq[Bits](
            base_uop.cf_influencer_list(k).valid,
            base_uop.cf_influencer_list(k).op_count,
            base_uop.cf_influencer_list(k).infl_type,
            base_uop.cf_influencer_list(k).is_atk,
            base_uop.cf_influencer_list(k).is_secret,
            base_uop.cf_influencer_list(k).deny_count
          ))
          printf(flushFmt, (Seq[Bits](
            Sext.apply(base_uop.debug_pc(vaddrBits-1,0), xLen),
            base_uop.debug_inst,
            base_uop.cf_domain_id,
            base_uop.cf_speculated,
            base_uop.cf_attacker_influence,
            base_uop.cf_secret_access,
            base_uop.cf_secret_propagation,
            base_uop.cf_secret_transmission,
            base_uop.cf_op_count_id,
            base_uop.cf_spec_branch_is_atk,
            base_uop.cf_spec_branch_op_id,
            fl_cross_domain, fl_op_count,
            base_uop.cf_fu_bitmap,
            0.U, inflBitmapFromList(base_uop.cf_influencer_list),
            base_uop.cf_infl_overflow
          ) ++ inflArgs): _*)
        }
      }
    }

    head := 1.U
    tail := 1.U
    maybe_full := false.B
    ram_valid := VecInit(Seq.fill(numEntries)(false.B))
    sub_row_cnt        := 0.U
    fetch_cooldown_cnt := 0.U
  }

  // TODO Is this necessary?
  when (reset.asBool) {
    io.deq.bits.uops map { u => u.valid := false.B }
  }

}
