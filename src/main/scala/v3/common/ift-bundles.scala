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
  * op_count and domain stay in rob_uop ([RASEVERY]: ras_pred_xdom is in IftDispColumn); the four sticky flags live in
  * flip-flops next to rob_val.  attacker_influence here is the EDGE-derived part only;
  * rob.scala ORs the sticky bit in. */
class IftCommitInfo(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val speculated            = Bool()
  // (record bit [75], cf_src_tainted, had no writer since Phase 2; Task 3.9 removed the
  //  constant-false src_tainted field here, in IftDispColumn and in IftDispatchInfo.  Since
  //  [HW1BIT75] (owner R-b, 2026-10-20) the bit is the commit-time secret-window shadow bit,
  //  passed to uopToIFTBits by the ROB commit packer as an argument (rob.scala com_wsh), not
  //  held here: it is computed at commit and is never stored per ROB row.)
  val spec_branch_is_secret = Bool()
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
  val sftq                  = UInt(3.W)   // [RECV3] SP-09: the row's FTQ-full hold bucket (C_disp), record [101:99]
  val dc                    = new IftDcFoot   // [RECV3] SP-08: the newest LSU-fed writeback's D$ footprint (record [113:102])
  // [RECV3] SP-03: the committing uop's FP exception flags (record [118:114], design 2.3 "fflags", owner D4 "capture
  // it").  Set by the ROB commit readout (rob_head_fflags); 0 from IftMerge (SQUASH records carry 0) and on KILL.
  val fflags                = UInt(freechips.rocketchip.tile.FPConstants.FLAGS_SZ.W)
}

/** Written once at enqueue (D-column). */
class IftDispColumn(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val speculated            = Bool()
  val spec_branch_is_secret = Bool()
  val stall_cycles_rob      = UInt(3.W)
  val stall_cycles_stq      = UInt(3.W)
  val atk_branch_ctr        = UInt(7.W)
  val sec_branch_ctr        = UInt(7.W)
  val base_list             = Vec(numInfluencerSlotsCF, new InfluencerEntry)
  val base_dropped          = UInt(2.W)
  val oc_aliased            = Bool()
  val fu_bitmap             = UInt(numModules.W)
  val alloc_stamp           = UInt(IftStamp.W.W)
  // [RASEVERY] owner 2026-10-21: the row's cf_ras_pred_xdom at dispatch (a ret that popped a RAS
  // slot pushed by the other domain, or a victim ret that popped a stale slot).  IftMerge raises
  // the RAS edge from it on every such ret, committed or squashed (147 -> 148 bits).
  val ras_xdom              = Bool()
  val sftq                  = UInt(3.W)   // [RECV3] SP-09: record [101:99] (+3 LUTRAM bits per C_disp row)
}

/** One LSU-fed writeback port's contribution. */
class IftWbColumn(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val edges     = Vec(numInfluencerSlotsCF, new InfluencerEntry)
  val dropped   = UInt(2.W)
  val aliased   = Bool()
  val dc        = new IftDcFoot   // [RECV3] SP-08: the response's D$ footprint (+12 LUTRAM bits per C_wb column, owner D5)
  val fu_bitmap = UInt(numModules.W)
  val stamp     = UInt(IftStamp.W.W)
}

/** A port that contributes only fu_bitmap bits (ALU/FP writeback ports). */
class IftBitmapColumn(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val fu_bitmap = UInt(numModules.W)
  val stamp     = UInt(IftStamp.W.W)
}

/** [STTY0] bundle61: the store clr_bsy column (was IftBitmapColumn): + the store's 2 operand labels, from which the
  * merge builds the store's REG_DATAFLOW (ty0) edge.  +2 LUTRAM bits per C_clr column. */
class IftClrColumn(implicit p: Parameters) extends IftBitmapColumn {
  val is_atk    = Bool()
  val is_secret = Bool()
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

// [RASEVERY] IftStampColumn (the C_ras arrival stamp) is removed with C_ras: the RAS edge is
// IftDispColumn.ras_xdom, written at dispatch.

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

/** [STTY0] bundle61: the store clr_bsy payload = the bitmap plus the store's operand labels (data / address operand
  * secret taint = the producers' s_acc || s_prop, lsu.scala cfStSprop; attacker taint, cfStAtk). */
class IftRowClr(rowW: Int)(implicit p: Parameters) extends IftRowBitmap(rowW) {
  val is_atk    = Bool()
  val is_secret = Bool()
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
  val dc      = new IftDcFoot   // [RECV3] SP-08 (raw squash row: +12 bits per wb column)
  val stamp   = UInt(IftStamp.W.W)
}

/** An ic or memord word without entry.valid.  The column's own valid bit says whether it
  * exists; the merge overwrites every slot's valid with its hit bit. */
class IftRawEdge(val hasDeny: Boolean)(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val key         = UInt(inflKeyWidthCF.W)
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
  *  + 2 bits ic_dropped: 1,033 bits.)
  * [RASEVERY]: - 17 bits `ras` (C_ras valid + stamp), + 1 bit disp.ras_xdom: 1,017 bits. */
class IftRawRow(val nSprob: Int, val nIc: Int, val nWb: Int, val nMemord: Int)(implicit p: Parameters)
    extends BoomBundle with CoreFuzzingConstants {
  val disp   = new IftDispColumn                         // fu_bitmap = disp | valid wb | valid fu | valid clr
  val sp     = Vec(nSprob,  Valid(new IftSprobColumn))
  val ic     = Vec(nIc,     Valid(new IftRawEdge(hasDeny = true)))
  val wb     = Vec(nWb,     Valid(new IftWbEdgeColumn))
  val mo     = Vec(nMemord, Valid(new IftRawEdge(hasDeny = false)))
  val domain = UInt(1.W)                                 // rob_uop(row).cf_domain_id at capture (RAS edge, record bit 69)
  // [FIFO10] step 10: saturating sum of the valid C_ic columns' `dropped` (replaced contention
  // winners), formed in rawRead so the raw row grows by 2 bits, not 7 x 2
  val ic_dropped = UInt(2.W)
  // [STTY0] bundle61: OR of the store operand labels over the valid C_clr columns, formed in rawRead (raw row + 2 bits,
  // not 3 x (2 + stamp)): the store's REG_DATAFLOW (ty0) edge in IftMerge.  Squash raw entry 1168 -> 1170 bits, still
  // 17 RAMB36 (512 x 72) per bank.
  val cl_atk     = Bool()
  val cl_sec     = Bool()
}

/** Record fields that do not come from the merge, taken at capture.  [RECV3] SP-04: 97 bits (v1 116: - inst 32, - priv 3,
  * + stamp 16; cause renamed kind).  The SQUASH record (IftSquashPack) takes pc, op_count, pregs, rtypes, the four sticky
  * flags, the capture stamp and the kind from here, and its domain from the raw row. */
class IftSqSide(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val pc         = UInt(32.W)                            // debug_pc(31, 0), record [79:48]
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
  val stamp      = UInt(16.W)                            // [RECV3] io.ift_cyc in the capture cycle: the record's cyc (design 2.8)
  val kind       = Bool()                                // D2b / design 2.6: 0 branch kill (SRC=3), 1 rollback (SRC=4)
}

/** One raw squash FIFO entry (raw row + side).  [RECV3] after the whole V1 series: side 97 b; raw row 1,185 b (12-bit key and
  * 6-bit type, SP-06; D$ footprint per C_wb column, SP-08; 15-bit unit word + sftq, SP-09; -10 b C_disp, SP-09 3.13) = 1,272 bits, 18 RAMB36 (512 x 72)
  * per bank (v1: 1,170 bits, 17).  The IftSquashMerge println prints the elaborated width.  INDEX-A (stream A) gap A-2. */
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
  * (never into IQ/LSQ/FU/ROB rows).  fetch_flags bit order (NOT the candidate order since [G1V2]:
  * fetchFlagCandidates in util.scala puts bit 0 last): 0 icache_dom, 1 icache_sec, 2 ras_pop_secret, 3 bpd_dom,
  * 4 bpd_sec, 5 btb_dom, 6 retired (was btb_target_xdom / ty27, [TY27RM] 2026-10-22: constant 0), 7 btb_sec.  ([FIFO10] step 10, ruling 94: the ITLB
  * bits 8/9 are removed.)  [BTBDISP] step 16: bit 7 also covers the BTB/uBTB displacement
  * filter, and btb_sec_pc is PC[13:2] of the secret allocator of bit 7 (fetchFlagCandidates).
  * [BTBATK] DF-btbatk: bit 8 = fetch of a tag an attacker-domain allocation displaced (victim
  * packets only); btb_sec_pc is then the attacker allocator's PC unless bit 7 fires. */
class FetchIftInfo(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val fetch_flags    = UInt(9.W)
  val any_is_victim  = Bool()
  val atk_branch_ctr = UInt(7.W)
  val sec_branch_ctr = UInt(7.W)
  val btb_sec_pc     = UInt(inflPcBitsCF.W)
  val sftq           = UInt(3.W)   // [RECV3] SP-09: FTQ-full f4 hold bucket of the packet (fetch-buffer.scala ram_ift)
}

/** [RECV3] SP-08 (design 2.3, 6.1.1): the D$ footprint of a load's answer.  kind 0 none (no D$ data: forwarded, killed in
  * s1/s2, not executed, store), 1 array hit by the load's own access, 2 answered through the MSHR file (line-buffer drain
  * of a cacheable miss, or a late secondary's t_replay), 3 uncached / IOMSHR.  set / way: the line read (kinds 1, 2; 0 for
  * 0 and 3).  A descriptor only: whether the access changed the replacement state is the STATE record's R group. */
class IftDcFoot extends Bundle {
  val kind = UInt(2.W)
  val set  = UInt(7.W)
  val way  = UInt(3.W)
}
/** Influencer edges travelling on a memory response (D16) and on the ROB writeback port. */
class IftEdgeList(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val edges   = Vec(numInfluencerSlotsCF, new InfluencerEntry)
  val dropped = UInt(2.W)
  val aliased = Bool()
  // [RECV3] SP-08: the D$ footprint of the response this list rides on (set by the D$ / MSHR file only: every other list
  // builds on a zero base or copies a D$ response list whole, so it is 0 there, e.g. kind 0 on a forwarded load).  None IFT
  // off.  (Takes the place of the v1 ce_repl flag, removed by SP-06.)
  val dc      = if (ENABLE_IFT) Some(new IftDcFoot) else None
}

/** What dispatch hands the ROB for the dispatch column (replaces the uop fields).
  * Ruling 74: cf_spec_branch_is_secret stays in MicroOp, so it is not carried here. */
class IftDispatchInfo(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val speculated            = Bool()
  val stall_cycles_rob      = UInt(3.W)
  val stall_cycles_stq      = UInt(3.W)
  val atk_branch_ctr        = UInt(7.W)
  val sec_branch_ctr        = UInt(7.W)
  val base                  = new IftEdgeList
  val sftq                  = UInt(3.W)   // [RECV3] SP-09: from FetchIftInfo.sftq (core.scala dispatch)
}

/** Kill-time contention record from an issue slot (D18).  72 bits, +1 for [R117SWEEP] any_sec. */
class IftKillRecord(implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val op_count    = UInt(uopIDCounterWidthCF.W)
  val pc          = UInt(32.W)
  val domain      = UInt(1.W)
  val winner_op   = UInt(inflKeyWidthCF.W)
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
  val winner_op   = UInt(inflKeyWidthCF.W)
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
  val ras_xdom              = Bool()                 // [RASEVERY] io.enq_uops(w).cf_ras_pred_xdom -> C_disp
}
