//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Rename FreeList
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.v3.exu

import chisel3._
import chisel3.util._
import boom.v3.common._
import boom.v3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.CoreFuzzingConstants

class RenameFreeList(
  val plWidth: Int,
  val numPregs: Int,
  val numLregs: Int,
  // Runtime reconfiguration option list for this freelist.  INT rename passes
  // pregFileSizeOptions; FP rename passes fpPregFileSizeOptions.  Each option
  // gets capped by `numPregs` at elaboration time, so lists whose max exceeds
  // the hardware size are safely clamped.
  val pregSizeOptions: Seq[Int])
  (implicit p: Parameters) extends BoomModule
  with CoreFuzzingConstants
{
  private val pregSz = log2Ceil(numPregs)
  private val n = numPregs

  val io = IO(new BoomBundle()(p) {
    // Physical register requests.
    val reqs          = Input(Vec(plWidth, Bool()))
    val alloc_pregs   = Output(Vec(plWidth, Valid(UInt(pregSz.W))))

    // Pregs returned by the ROB.
    val dealloc_pregs = Input(Vec(plWidth, Valid(UInt(pregSz.W))))

    // Branch info for starting new allocation lists.
    val ren_br_tags   = Input(Vec(plWidth, Valid(UInt(brTagSz.W))))

    // Mispredict info for recovering speculatively allocated registers.
    val brupdate        = Input(new BrUpdateInfo)

    val debug = new Bundle {
      val pipeline_empty = Input(Bool())
      val freelist = Output(Bits(numPregs.W))
      val isprlist = Output(Bits(numPregs.W))
    }

    // 3-bit index into pregFileSizeOptions for runtime physical register file size selection
    val cf_preg_idx = Input(UInt(3.W))
  })
  // The free list register array and its branch allocation lists.
  val free_list = RegInit(UInt(numPregs.W), ~(1.U(numPregs.W)))
  val br_alloc_lists = Reg(Vec(maxBrCount, UInt(numPregs.W)))

  // Runtime PRF size selection via `pregSizeOptions` constructor param.
  // INT rename passes pregFileSizeOptions = Seq(192, 128, 96, 64, 48).
  // FP  rename passes fpPregFileSizeOptions = Seq(96, 64, 48, 40, 40).
  // [DOCFIX 2026-09-12] The requirement below is the ORIGINAL, INCORRECT reasoning, kept to
  // explain the fix: require(numFpPhysRegs >= 32+coreWidth) = 36 ADMITTED 36, which deadlocked.
  // The require() does not account for the freelist's pre-selection depth (36 - 32 arch - 4
  // pre-selected = 0 free).  The real floor is 32 + 2*coreWidth = 40, which is why
  // fpPregFileSizeOptions now ends "40, 40".  Repro for the old bug: mm.riscv.
  // ORIGINAL (WRONG): require(numFpPhysRegs >= 32+coreWidth) = 36 at width 4, 36 the minimum
  // (4 free FP renames). 32 was degenerate — 32 arch FP regs fill all 32 pregs, 0 free ->
  // deadlock on the first FP write — and is no longer offered.
  // [DOCFIX 2026-09-12] RE-VERIFIED against the CURRENT gen-collateral/RenameFreeList_1.sv by
  // decoding the popcount of each _GEN mask: idx0..7 = 96,64,48,40,40,96,96,96.
  // So the built FP options are idx0..4 = 96,64,48,40,**40** — the floor is 40 and idx 4 is
  // SAFE.  The previous text here recorded idx4 = 36 from an older build; that is no longer
  // what is elaborated, and 0xbc7=4 must NOT be excluded from sweeps on account of it.
  // (Indices 3 and 4 deliberately share 40: rung 4 exists to sweep INT=48.)  The
  // three trailing 96s are padding for the unused 3-bit CSR encodings 5..7 — they select the
  // FULL file so an out-of-range index can never under-provision (cfClampIdx also clamps).
  // Precompute one mask per option as a constant; runtime selection is a small Mux
  // instead of a barrel-shifter + numPregs-wide subtractor (LUT optimization).
  // Every runtime option must leave at least one allocatable preg in the STEADY
  // STATE, not merely at reset.  Two consumers are permanent:
  //   numLregs  architectural mappings, once every architectural register is live
  //   plWidth   pregs latched in the r_sel pre-selection registers below
  // so the floor is numLregs + 2*plWidth, NOT numLregs + plWidth.
  // fp=36 at coreWidth 4 satisfied the old bound and still deadlocked: 36-32-4 = 0
  // available, the next FP write never renamed, and core.scala:2083 fired after 8192
  // idle cycles (verified 2026-08-28; repro riscv-tests benchmarks/mm.riscv).
  // That rung NO LONGER EXISTS: the floor was raised to 40 (= numLregs + 2*plWidth), so the
  // smallest FP option now leaves 40-32-4 = 4 allocatable.  Kept here as the rationale for
  // the bound, not as a description of a currently-reachable configuration.
  pregSizeOptions.foreach { sz =>
    val usable = (sz min numPregs)
    require(usable >= numLregs + 2 * plWidth,
      s"pregSizeOptions entry $sz leaves ${usable - numLregs - plWidth} allocatable " +
      s"pregs in steady state (numLregs=$numLregs, plWidth=$plWidth); need >= " +
      s"${numLregs + 2 * plWidth}. A value that only satisfies numLregs+plWidth " +
      s"deadlocks once all architectural registers are live.")
  }

  val preg_active_masks = VecInit(pregSizeOptions.map { sz =>
    val capped = sz min numPregs
    (((BigInt(1) << capped) - 1) & ((BigInt(1) << numPregs) - 1)).U(numPregs.W)
  })
  val preg_active_mask = preg_active_masks(io.cf_preg_idx)
  val masked_free_list = free_list & preg_active_mask

  // Select pregs from the masked free list.
  val sels = SelectFirstN(masked_free_list, plWidth)
  val sel_fire  = Wire(Vec(plWidth, Bool()))

  // Allocations seen by branches in each pipeline slot.
  val allocs = io.alloc_pregs map (a => UIntToOH(a.bits))
  val alloc_masks = (allocs zip io.reqs).scanRight(0.U(n.W)) { case ((a,r),m) => m | a & Fill(n,r) }

  // Masks that modify the freelist array.
  val sel_mask = (sels zip sel_fire) map { case (s,f) => s & Fill(n,f) } reduce(_|_)
  val br_deallocs = br_alloc_lists(io.brupdate.b2.uop.br_tag) & Fill(n, io.brupdate.b2.mispredict)
  val dealloc_mask = io.dealloc_pregs.map(d => UIntToOH(d.bits)(numPregs-1,0) & Fill(n,d.valid)).reduce(_|_) | br_deallocs

  val br_slots = VecInit(io.ren_br_tags.map(tag => tag.valid)).asUInt
  // Create branch allocation lists.
  for (i <- 0 until maxBrCount) {
    val list_req = VecInit(io.ren_br_tags.map(tag => UIntToOH(tag.bits)(i))).asUInt & br_slots
    val new_list = list_req.orR
    br_alloc_lists(i) := Mux(new_list, Mux1H(list_req, alloc_masks.slice(1, plWidth+1)),
                                       br_alloc_lists(i) & ~br_deallocs | alloc_masks(0))
  }

  // Update the free list.
  free_list := (free_list & ~sel_mask | dealloc_mask) & ~(1.U(numPregs.W))

  // Pipeline logic | hookup outputs.
  for (w <- 0 until plWidth) {
    val can_sel = sels(w).orR
    val r_valid = RegInit(false.B)
    val r_sel   = RegEnable(OHToUInt(sels(w)), sel_fire(w))

    r_valid := r_valid && !io.reqs(w) || can_sel
    sel_fire(w) := (!r_valid || io.reqs(w)) && can_sel

    io.alloc_pregs(w).bits  := r_sel
    io.alloc_pregs(w).valid := r_valid
  }

  io.debug.freelist := free_list | io.alloc_pregs.map(p => UIntToOH(p.bits) & Fill(n,p.valid)).reduce(_|_)
  io.debug.isprlist := 0.U  // TODO track commit free list.

  assert (!(io.debug.freelist & dealloc_mask).orR, "[freelist] Returning a free physical register.")
  assert (!io.debug.pipeline_empty || PopCount(io.debug.freelist) >= (numPregs - numLregs - 1).U,
    "[freelist] Leaking physical registers.")
}
