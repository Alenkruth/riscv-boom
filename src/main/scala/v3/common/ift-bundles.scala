//******************************************************************************
// Copyright (c) 2015 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// IFT sidecar bundles (IFT overhaul Phase 2, Task 2.1)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Column and readout bundles for the per-bank ROB IFT sidecar (RobIftBank in
// exu/rob-ift-sidecar.scala).  See claude-artifacts/ift-overhaul/00-spec.md
// section 5.1 (D3, D15, D17, D19).  Nothing here touches MicroOp.

package boom.v3.common

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.CoreFuzzingConstants
import boom.v3.exu.IssueContentionUpdate

object IftStamp { val W = 16 }   // D15

/** Everything the 256-bit record needs that the sidecar reconstructs at readout.
  * op_count, domain and ras_pred_xdom stay in rob_uop; the four sticky flags live in
  * flip-flops next to rob_val.  attacker_influence here is the EDGE-derived part only;
  * rob.scala ORs the sticky bit in. */
class IftCommitInfo(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val speculated            = Bool()
  // (record bit [75], cf_src_tainted, has had no writer since Phase 2; Task 3.9 removed the
  //  constant-false src_tainted field here, in IftDispColumn and in IftDispatchInfo.  The
  //  packer emits the 0 directly, so the record is unchanged.)
  val single_step           = Bool()
  val spec_branch_is_atk    = Bool()
  val spec_branch_is_secret = Bool()
  val spec_branch_op_id     = UInt(specBranchOpIdWidthCF.W)
  val stall_cycles_rob      = UInt(3.W)
  val stall_cycles_stq      = UInt(3.W)
  val atk_branch_ctr        = UInt(7.W)
  val sec_branch_ctr        = UInt(7.W)
  val fu_bitmap             = UInt(numModules.W)
  val influencer_list       = Vec(numInfluencerSlotsCF, new InfluencerEntry)
  val infl_dropped          = UInt(2.W)
  val infl_oc_aliased       = Bool()
  val cntd_deny_count       = UInt(3.W)
  val edge_atk              = Bool()      // [FIFO10] OR of is_atk over EVERY edge that reached the row (stored or dropped)
}

/** Written once at enqueue (D-column). */
class IftDispColumn(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val speculated            = Bool()
  val single_step           = Bool()
  val spec_branch_is_atk    = Bool()
  val spec_branch_is_secret = Bool()
  val spec_branch_op_id     = UInt(specBranchOpIdWidthCF.W)
  val stall_cycles_rob      = UInt(3.W)
  val stall_cycles_stq      = UInt(3.W)
  val atk_branch_ctr        = UInt(7.W)
  val sec_branch_ctr        = UInt(7.W)
  val base_list             = Vec(numInfluencerSlotsCF, new InfluencerEntry)
  val base_dropped          = UInt(2.W)
  val oc_aliased            = Bool()
  val fu_bitmap             = UInt(numModules.W)
  val alloc_stamp           = UInt(IftStamp.W.W)
}

/** One LSU-fed writeback port's contribution. */
class IftWbColumn(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val edges     = Vec(numInfluencerSlotsCF, new InfluencerEntry)
  val dropped   = UInt(2.W)
  val aliased   = Bool()
  val fu_bitmap = UInt(numModules.W)
  val stamp     = UInt(IftStamp.W.W)
}

/** A port that contributes only fu_bitmap bits (ALU/FP writeback ports, store clr_bsy). */
class IftBitmapColumn(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val fu_bitmap = UInt(numModules.W)
  val stamp     = UInt(IftStamp.W.W)
}

/** One influencer entry with its arrival stamp (issue contention, MEM_ORDER, later DTLB).
  * `hasDeny` adds the contention deny bucket; the MEM_ORDER columns omit it. */
class IftEdgeColumn(val hasDeny: Boolean = true)(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val entry       = new InfluencerEntry
  val deny_bucket = Option.when(hasDeny)(UInt(3.W))     // used by the contention column only
  // [FIFO10] step 10: denying winners the slot's one-entry contention list replaced (newest kept)
  val dropped     = Option.when(hasDeny)(UInt(2.W))     // contention column only
  val stamp       = UInt(IftStamp.W.W)
}

/** Fixed-content event (RAS edge): only the arrival stamp is stored. */
class IftStampColumn(implicit p: Parameters) extends BoomBundle {
  val stamp = UInt(IftStamp.W.W)
}

/** The sprob mark (writeback-time INFL_REG_DATAFLOW).  Spec 5.1.2 amendment (2026-10-08,
  * owner-approved): the entry's is_atk / is_secret come from the writeback source (the
  * result's attacker operand taint on a victim consumer; the core's s_prop condition), so the
  * readout emits the attacker register-dataflow edge lost on 2026-09-02 instead of a
  * hard-coded is_atk = false.  18 bits + the per-column valid flop (the spec's 19). */
class IftSprobColumn(implicit p: Parameters) extends BoomBundle {
  val is_atk    = Bool()
  val is_secret = Bool()
  val stamp     = UInt(IftStamp.W.W)
}

class IftRowReq(val rowW: Int)(implicit p: Parameters) extends BoomBundle {
  val row    = UInt(rowW.W)
  val domain = UInt(1.W)          // the row's cf_domain_id (for the RAS edge's is_atk)
}

/** Row-addressed writer payloads of the RobIftBank IO.  Named classes rather than
  * anonymous Bundles so member access is not reflective and the Vec elements have a
  * nominal type.  (Task 3.3: IftRowUop, the enqueue payload carrying a whole MicroOp, is
  * replaced by IftRowEnq below.) */
class IftRowBitmap(val rowW: Int)(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val row       = UInt(rowW.W)
  val fu_bitmap = UInt(numModules.W)
}

class IftRowIc(val rowW: Int)(implicit p: Parameters) extends BoomBundle {
  val row = UInt(rowW.W)
  val upd = new IssueContentionUpdate
}

class IftRowMemord(val rowW: Int)(implicit p: Parameters) extends BoomBundle {
  val row = UInt(rowW.W)
  val upd = new CF_MemOrdUpdate
}

class IftRowSprob(val rowW: Int)(implicit p: Parameters) extends BoomBundle {
  val row       = UInt(rowW.W)
  val is_atk    = Bool()
  val is_secret = Bool()
}

// ---------------------------------------------------------------- Task 2.10d (design 4b)
// The squash path captures each killed or rolled-back row RAW (every merge input, read in the
// capture cycle) into a per-bank FIFO in BRAM; one shared merge (IftSquashMerge) runs when the
// entry leaves its FIFO.  Design: execution/task-reports/task-2.10b-design.md section 7.

/** A C_wb word without fu_bitmap: rawRead ORs fu_bitmap into disp.fu_bitmap (spec 5.1.5
  * step 4), so the raw row does not carry it twice. */
class IftWbEdgeColumn(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val edges   = Vec(numInfluencerSlotsCF, new InfluencerEntry)
  val dropped = UInt(2.W)
  val aliased = Bool()
  val stamp   = UInt(IftStamp.W.W)
}

/** An ic or memord word without entry.valid.  The column's own valid bit says whether it
  * exists; the merge overwrites every slot's valid with its hit bit. */
class IftRawEdge(val hasDeny: Boolean)(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val op_count    = UInt(inflOpCountWidthCF.W)
  val infl_type   = UInt(inflTypeWidthCF.W)
  val is_atk      = Bool()
  val is_secret   = Bool()
  val deny_bucket = Option.when(hasDeny)(UInt(3.W))
  val stamp       = UInt(IftStamp.W.W)
}

/** Every merge input of one row, read in one cycle by RobIftBank.rawRead.  IftMerge is a pure
  * function of it.  147 + 9*19 + 7*37 + 4*92 + 2*34 + 17 + 1 + 2 = 1,033 bits for (9, 7, 4, 2) ([FIFO10])
  * (Task 3.9: the constant src_tainted bit left IftDispColumn, 148 -> 147).
  * ([FIFO10] step 10, owner ruling 94: the Task 3.2 DTLB columns `dt` (2 x 19 bits) are removed;
  *  + 2 bits ic_dropped: 1,033 bits.) */
class IftRawRow(val nSprob: Int, val nIc: Int, val nWb: Int, val nMemord: Int)(implicit p: Parameters)
    extends BoomBundle with CoreFuzzingConstants {
  val disp   = new IftDispColumn                         // fu_bitmap = disp | valid wb | valid fu | valid clr
  val sp     = Vec(nSprob,  Valid(new IftSprobColumn))
  val ic     = Vec(nIc,     Valid(new IftRawEdge(hasDeny = true)))
  val wb     = Vec(nWb,     Valid(new IftWbEdgeColumn))
  val mo     = Vec(nMemord, Valid(new IftRawEdge(hasDeny = false)))
  val ras    = Valid(new IftStampColumn)
  val domain = UInt(1.W)                                 // rob_uop(row).cf_domain_id at capture (RAS edge, record bit 69)
  // [FIFO10] step 10: saturating sum of the valid C_ic columns' `dropped` (replaced contention
  // winners), formed in rawRead so the raw row grows by 2 bits, not 7 x 2
  val ic_dropped = UInt(2.W)
}

/** Record fields that do not come from the merge, taken at capture.  116 bits. */
class IftSqSide(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val pc         = UInt(32.W)                            // debug_pc(31, 0), record [36:5]
  val inst       = UInt(32.W)                            // debug_inst, record [68:37]
  val op_count   = UInt(uopIDCounterWidthCF.W)
  val pdst       = UInt(maxPregSz.W)
  val prs1       = UInt(maxPregSz.W)
  val prs2       = UInt(maxPregSz.W)
  val lrs1_rtype = UInt(2.W)
  val lrs2_rtype = UInt(2.W)
  val s_acc      = Bool()                                // the four sticky flags of the row at capture
  val s_prop     = Bool()
  val s_tx       = Bool()
  val atk        = Bool()
  val priv       = UInt(3.W)                             // io.ift_priv at capture
  val cause      = Bool()                                // D2b: 0 branch kill (SRC=3), 1 rollback (SRC=4)
}

/** One raw squash FIFO entry: 1,033 + 116 = 1,149 bits (Task 3.2: + 2 x 19 DTLB; Task 3.9: - 1;
  * step 10: + 2 ic_dropped, - 2 x 19 DTLB; step 11: the FIFO is 512 deep = 16 RAMB36 (512 x 72) per bank, was 32 as 1K x 36). */
class IftSqEntry(val nSprob: Int, val nIc: Int, val nWb: Int, val nMemord: Int)(implicit p: Parameters)
    extends BoomBundle {
  val raw  = new IftRawRow(nSprob, nIc, nWb, nMemord)
  val side = new IftSqSide
}

/** Printf and check copies taken at capture.  Stored in their own memory, elaborated only in
  * Verilator printf builds (never with the IFT bridge), so nothing reads it in synthesis. */
class IftSqDbg(val shadow: Boolean)(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val en      = Bool()                                   // io.cf_debug_rob_enable at capture
  val pc      = UInt(vaddrBits.W)                        // debug_pc(vaddrBits-1, 0), the [FLUSH] pc
  val inst    = UInt(32.W)                               // expanded inst, the [FLUSH] inst
  val br_mask = UInt(maxBrCount.W)
  val br_tag  = UInt(brTagSz.W)
  val fl_dom  = UInt(1.W)                                // flush_mp_dom / _sec / _oc at capture
  val fl_sec  = Bool()
  val fl_oc   = UInt(uopIDCounterWidthCF.W)
  val old_rec = Option.when(shadow)(UInt(256.W))         // the capture-time packed record (shadow check)
}

//------------------------------------------------------------------------------
// Thin-uop shared bundles (IFT overhaul Phase 3, Task 3.1).  Declarations only:
// nothing instantiates these until Tasks 3.2 to 3.4.
//------------------------------------------------------------------------------

/** Fetch-time IFT information carried beside the uop from the fetch buffer to dispatch
  * (never into IQ/LSQ/FU/ROB rows).  fetch_flags bit order = the fetch-buffer candidate
  * order (fetch-buffer.scala:321-345): 0 icache_dom, 1 icache_sec, 2 ras_pop_secret, 3 bpd_dom,
  * 4 bpd_sec, 5 btb_dom, 6 btb_target_xdom, 7 btb_sec.  ([FIFO10] step 10, ruling 94: the ITLB
  * bits 8/9 are removed.)  [BTBDISP] step 16: bit 7 also covers the BTB/uBTB displacement
  * filter, and btb_sec_pc is PC[13:2] of the secret allocator of bit 7 (fetchFlagCandidates).
  * [BTBATK] DF-btbatk: bit 8 = fetch of a tag an attacker-domain allocation displaced (victim
  * packets only); btb_sec_pc is then the attacker allocator's PC unless bit 7 fires. */
class FetchIftInfo(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val fetch_flags    = UInt(9.W)
  val any_is_victim  = Bool()
  val atk_branch_ctr = UInt(7.W)
  val sec_branch_ctr = UInt(7.W)
  val single_step    = Bool()
  val btb_sec_pc     = UInt(inflPcBitsCF.W)
}

/** Influencer edges travelling on a memory response (D16) and on the ROB writeback port. */
class IftEdgeList(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val edges   = Vec(numInfluencerSlotsCF, new InfluencerEntry)
  val dropped = UInt(2.W)
  val aliased = Bool()
}

/** What dispatch hands the ROB for the dispatch column (replaces the uop fields).
  * Ruling 74: cf_spec_branch_is_secret stays in MicroOp, so it is not carried here. */
class IftDispatchInfo(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val speculated            = Bool()
  val single_step           = Bool()
  val spec_branch_is_atk    = Bool()
  val spec_branch_op_id     = UInt(specBranchOpIdWidthCF.W)
  val stall_cycles_rob      = UInt(3.W)
  val stall_cycles_stq      = UInt(3.W)
  val atk_branch_ctr        = UInt(7.W)
  val sec_branch_ctr        = UInt(7.W)
  val base                  = new IftEdgeList
}

/** Kill-time contention record from an issue slot (D18).  72 bits, +1 for [R117SWEEP] any_sec. */
class IftKillRecord(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val op_count    = UInt(uopIDCounterWidthCF.W)
  val pc          = UInt(32.W)
  val domain      = UInt(1.W)
  val winner_op   = UInt(inflOpCountWidthCF.W)
  val infl_type   = UInt(inflTypeWidthCF.W)
  val winner_atk  = Bool()
  val winner_sec  = Bool()
  val deny_count  = UInt(6.W)
  // [FIFO10] step 10 (R-FIFO / R-STICKY): winners the slot's one-entry list replaced, and the
  // OR of every denying winner's is_atk (record bit 71 of the kill record)
  val dropped     = UInt(2.W)
  val any_atk     = Bool()
  val any_sec     = if (ENABLE_IFT) Some(Bool()) else None   // [R117SWEEP] patch 33: OR of every denying winner's is_secret (kill record s_prop)
}

/** Slot-local contention state carried on the collapsing shift beside the uop (Task 3.4).
  * (Task 3.9: no src_secret here.  The leaving slot's registered secret already rides
  *  out_uop.cf_secret_propagation, which the slot ORs it into, so the destination's
  *  cf_src_secret load from in_uop.bits.cf_secret_propagation sees it.) */
class IssueCntdCarry(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val valid       = Bool()
  val winner_op   = UInt(inflOpCountWidthCF.W)
  val winner_atk  = Bool()
  val winner_sec  = Bool()
  val infl_type   = UInt(inflTypeWidthCF.W)
  val deny_count  = UInt(6.W)
  val dropped     = UInt(2.W)      // [FIFO10] replaced distinct winners (+ same-cycle extra denials), saturating
  val any_atk     = Bool()         // [FIFO10] OR of every denying winner's is_atk
  val any_sec     = if (ENABLE_IFT) Some(Bool()) else None   // [R117SWEEP] patch 33: OR of every denying winner's is_secret
  // [IQPEND] patch 41 (owner r167): the slot waits on a producer that ISSUED secret (issue-slot.scala
  // cf_pend_sec).  Read only by the IQ_FULL secret summary; carried beside the uop on a shift.
  val pend_sec    = if (ENABLE_IFT) Some(Bool()) else None
}

/** Thin writeback-port payload for the sidecar (replaced IftRowUop on io.wb after D16). */
class IftRowWb(val rowW: Int)(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val row       = UInt(rowW.W)
  val edges     = new IftEdgeList
  val fu_bitmap = UInt(numModules.W)
}

/** Thin enqueue payload for the sidecar's dispatch column (Task 3.3; replaces IftRowUop on
  * io.enq).  Ruling 74: cf_spec_branch_is_secret stays in MicroOp and is not in
  * IftDispatchInfo, so it travels here beside `info`. */
class IftRowEnq(val rowW: Int)(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val row                   = UInt(rowW.W)
  val oc                    = UInt(uopIDCounterWidthCF.W)
  val fu_bitmap             = UInt(numModules.W)
  val spec_branch_is_secret = Bool()
  val info                  = new IftDispatchInfo
}
