// IFTBridgeIO.scala — IFT tile-level IO bundle and 256-bit record packing helper.
// Defined in the boom package so tile.scala can reference it without a firesim dep.
// The actual bridge BlackBox lives in firechip.bridgestubs (which depends on boom via chipyard).

package boom.v3.exu

import chisel3._
import chisel3.util._

import freechips.rocketchip.util.CoreFuzzingConstants

import boom.v3.common._

// ---------------------------------------------------------------------------
// IFTTileIO: output bundle from BoomCore to the tile-level BundleBridgeSource.
// All fields are plain UInts/Bools so this bundle has no firesim dependency.
// retireWidth matches the core's coreWidth (= decodeWidth in BoomCoreParams).
// ---------------------------------------------------------------------------
class IFTTileIO(val retireWidth: Int) extends Bundle {
  // One 256-bit record per commit slot (valid when commit_valid(w) is true)
  val commit_valid  = Vec(retireWidth, Bool())
  val commit_record = Vec(retireWidth, UInt(256.W))
  // One squash record draining from the ROB squash-pending bitvector (1/cycle)
  val squash_valid  = Bool()
  val squash_record = UInt(256.W)
  // Sticky overflow flag: set when a pending slot was written before drain cleared it
  val squash_ovf    = Bool()
}

// ---------------------------------------------------------------------------
// uopToIFTBits: pack one 256-bit IFT record from MicroOp fields.
//
// 256-bit layout (LSB = bit 0).  Regenerated in Task 2.10d (review F10) from the Cat below
// with the CoreFuzzingConstants widths (uopIDCounterWidthCF 16, inflOpCountWidthCF 10,
// specBranchOpIdWidthCF 8, inflTypeWidthCF 5, numModules 24) and maxPregSz 8; it agrees
// with base-attacks/fuzzer/boom_ift/decode_ift_bridge.py parse_record:
//
//   [1:0]     event_type            2b   01=commit, 10=squash, 11=cycle_anchor, 00=NOP
//   [4:2]     priv                  3b
//   [36:5]    pc                    32b  debug_pc[31:0]  ([PC32 2026-09-10], was 40b)
//   [68:37]   insn                  32b  debug_inst
//   [69]      cf_domain_id          1b
//   [70]      cf_speculated         1b
//   [71]      cf_attacker_influence 1b
//   [72]      cf_secret_access      1b
//   [73]      cf_secret_propagation 1b
//   [74]      cf_secret_transmission 1b
//   [75]      cf_src_tainted        1b
//   [77:76]   cf_infl_dropped       2b   saturating COUNT of discarded influencers
//                                        (0..3, 3 means ">=3"); was a 1b sticky flag
//   [93:78]   cf_op_count_id        16b  uopIDCounterWidthCF -- the ANCHOR every
//                                        truncated field below reconstructs against
//   [94]      cf_spec_branch_is_atk 1b
//   [95]      cf_spec_branch_is_secret 1b
//   [103:96]  cf_spec_branch_op_id  8b   specBranchOpIdWidthCF -- LOW bits only
//   [104]     cf_single_step        1b
//   [128:105] cf_fu_bitmap          24b  one bit per pipeline module (numModules=24)
//   [131:129] cf_cntd_deny_count    3b   Log2Bucket -- issue-port denial cycles
//   [134:132] cf_stall_cycles_rob   3b   Log2Bucket -- ROB-full dispatch stall cycles
//   [137:135] cf_stall_cycles_stq   3b   Log2Bucket -- STQ head-of-line residency
//   [138]     cf_infl_oc_aliased    1b   a truncated op_count on this record is OUT
//                                        of reconstructable range -- the stored low
//                                        bits reconstruct to the WRONG producer, so
//                                        do not build a flow through it
//   --- influencer[0..3]: 4 slots x 18 bits = 72 bits ---
//   [139]     influencer[0].valid   1b
//   [149:140] influencer[0].op_count 10b inflOpCountWidthCF -- LOW bits only
//   [154:150] influencer[0].infl_type 5b
//   [155]     influencer[0].is_atk  1b
//   [156]     influencer[0].is_secret 1b
//   [174:157] influencer[1]         18b  same layout
//   [192:175] influencer[2]         18b  same layout
//   [210:193] influencer[3]         18b  same layout
//   [218:211] pdst                   8b  maxPregSz
//   [226:219] prs1                   8b
//   [234:227] prs2                   8b
//   [236:235] lrs1_rtype             2b  is prs1 really read
//   [238:237] lrs2_rtype             2b  is prs2 really read
//   [245:239] cf_atk_branch_ctr      7b  GHR attacker window counter
//   [252:246] cf_sec_branch_ctr      7b  GHR secret window counter
//   [255:253] reserved/zero          3b  squash records (event_type 10): rob.scala ORs the
//                                        squash cause into bit [253] (0 branch kill, SRC=3;
//                                        1 exception rollback, SRC=4); [255:254] stay 0
//
// Total: fixedBits 167 (incl. pregs and rtypes) + 72 influencers + 14 GHR counters
// + 3 reserved = 256 bits.
//
// THESE INDICES ARE DERIVED, NOT AUTHORITATIVE.  They shift whenever any width changes
// -- they have already gone stale twice.  fixedBits/inflSlotBits below compute them from
// the CoreFuzzingConstants; if you change a width, regenerate this block rather than
// patching indices by hand, and update base-attacks/fuzzer/decode_ift_bridge.py in the
// same commit.  The record has no self-describing header, so a mismatch silently
// mis-decodes every field above the change point instead of erroring.
//
// Both truncated fields store only the LOW bits of an op_count_id and are recovered as
//     producer = own - ((own - stored) & ((1 << width) - 1))
// against this record's own cf_op_count_id, exact while the true distance is in range.
// Widths are sized from measured tails across ten workloads, not one -- see
// inflOpCountWidthCF in CoreFuzzing.scala.
// ---------------------------------------------------------------------------
object uopToIFTBits extends CoreFuzzingConstants {
  // [THINUOP] Phase 3 Task 3.5: every IFT field that left the MicroOp comes from `ift`; the four
  // sticky flags (s_acc, s_prop, s_tx, atk) come from `sticky`, with the attacker flag ORed with
  // ift.edge_atk (what IftApply did).  op_count / domain / pc / inst / pregs / rtypes stay on `uop`.
  def apply(uop: MicroOp, ift: IftCommitInfo, sticky: (Bool, Bool, Bool, Bool), event_type: UInt, priv: UInt): UInt = {
    val (s_acc, s_prop, s_tx, atk) = sticky
    // Per-slot bit width: valid(1) + op_count(inflOpCountWidthCF) + infl_type(5) +
    //                     is_atk(1) + is_secret(1)
    val inflSlotBits = 1 + inflOpCountWidthCF + inflTypeWidthCF + 1 + 1
    // Pack influencer entries (numInfluencerSlotsCF slots × inflSlotBits each), MSB→LSB
    val infl_bits = Cat(
      (numInfluencerSlotsCF - 1 to 0 by -1).map { k =>
        val e = ift.influencer_list(k)
        Cat(e.is_secret,             // 1b
            e.is_atk,                // 1b
            e.infl_type,             // 5b  (inflTypeWidthCF)
            e.op_count,              // inflOpCountWidthCF bits (low bits only)
            e.valid)                 // 1b
      }
    )
    // Fixed fields below influencer region: 2+3+40+32+8+uopIDCounterWidthCF+2+uopIDCounterWidthCF+1+24
    // 2 evt + 3 priv + 40 pc + 32 insn + 8 flags + 2 infl_dropped + op_count_id
    // + 2 spec_branch flags + spec_branch_op_id + 1 single_step + 24 fu_bitmap
    // + 3 duration counters x 3b
    // 2 evt + 3 priv + 40 pc + 32 insn + 7 single-bit flags + 2 infl_dropped
    // + op_count_id + 2 spec_branch flags + spec_branch_op_id + 1 single_step
    // + 24 fu_bitmap + 3 duration counters x 3b.  NOTE the flag group is 7, not 8:
    // the old 8 folded in the 1-bit cf_infl_overflow, which is now the separate 2-bit
    // cf_infl_dropped term.  Getting this wrong makes Cat() 255 bits, not 256.
    // Register operands, so the parser can rebuild the dataflow graph itself instead of
    // relying on the hardware to know a taint it cannot know in time.  A secret access is
    // only resolved at TLB, long after its consumers renamed, so no dispatch-time edge can
    // exist for a victim-only secret chain; recording WHICH registers each uop read and
    // wrote costs nothing here and lets the analysis happen where timing is irrelevant.
    //   pdst/prs1/prs2  physical registers -- NOT architectural: the log interleaves
    //                   squashed and committed instructions, so "last writer of a5" is
    //                   ambiguous, while a preg names exactly one producer.
    //   lrs*_rtype      whether that source is really read; prs fields are not cleared
    //                   for unused sources and still hold stale pregs.
    val pregBits  = uop.pdst.getWidth
    val fixedBits = 2 + 3 + 32 + 32 + 7 + 2 + uopIDCounterWidthCF + 2 + specBranchOpIdWidthCF + 1 + 24 + 3 + 3 + 3 + 1 + (3 * pregBits) + 4   // +1: cf_infl_oc_aliased; + pregs/rtypes
    val inflTotalBits = numInfluencerSlotsCF * inflSlotBits
    // [GHISTFIELD 2026-09-08] cf_atk_branch_ctr (7b) added below.  Budget check:
    // reservedBits was 9 with pregBits=8 (numIntPhysRegisters=192), so 7 fit with 2 left.
    // The SECRET counter is deliberately NOT on the bridge -- 14b does not fit -- it stays
    // in the Verilator commit/flush log only.  Stated plainly rather than silently dropped.
    // [PC32 2026-09-10] debug_pc narrowed 40b -> 32b, which paid for cf_sec_branch_ctr.
    // MEASURED before changing it: across a full spectre-v2 run (1,365,332 records) EVERY pc
    // on both COMMIT and FLUSH records had high-32 == 0, spanning 0x00010000 (bootrom) to
    // 0x80003454 (DRAM code).  These are bare-metal images of a few KB in a <4GiB map.
    // The assert below is the guard: if an image ever lands above 4GiB this FAILS LOUDLY
    // instead of silently truncating the anchor every downstream field reconstructs against.
    val reservedBits  = 256 - fixedBits - inflTotalBits - 7 - 7
    require(reservedBits >= 0,
      s"IFT bridge record overflows 256 bits: fixed=$fixedBits infl=$inflTotalBits " +
      s"ghist=14 (atk 7 + sec 7) -> reserved=$reservedBits.  A negative reserve SILENTLY " +
      s"SHIFTS every field and corrupts all downstream parsing, so this is a build error " +
      s"by design.")
    // Build 256-bit record (Cat: first arg = MSB, last arg = LSB)
    Cat(
      0.U(reservedBits.W),                 // [255:253]   3b  reserved (fills to 256)
      ift.sec_branch_ctr,               // [252:246]   7b  GHR SECRET window counter -- on the
                                           //      bridge as of PC32; previously log-only,
                                           //      which made half of descriptor.py's GHR
                                           //      novelty axis impossible on FPGA
      ift.atk_branch_ctr,               // [245:239]   7b  GHR attacker window counter;
                                           //      globalHistoryLength - v = retirements
                                           //      back to the tainting branch
      uop.lrs2_rtype,                      // [238:237]   2b  prs2 really read?
      uop.lrs1_rtype,                      // [236:235]   2b  prs1 really read?
      uop.prs2,                            // [234:227]   pregBits
      uop.prs1,                            // [226:219]   pregBits
      uop.pdst,                            // [218:211]   pregBits
      infl_bits,                           // [210:139]   influencer slots (slot 0 lowest)
      ift.infl_oc_aliased,              // [138]       1b  a truncated op_count is
                                           //                 out of reconstructable range
      ift.stall_cycles_stq,             // [137:135]   3b  Log2Bucket
      ift.stall_cycles_rob,             // [134:132]   3b  Log2Bucket
      ift.cntd_deny_count,                // [131:129]   3b  Log2Bucket ([DENYWIDTH]: the
                                           //      uop field is 6b; the ROB row holds the
                                           //      3-bit bucket, so slice to keep 256 bits)
      ift.fu_bitmap,                    // [128:105]  24b (numModules wide)
      ift.single_step,                  // [104]       1b
      ift.spec_branch_op_id,            // [103:96]    specBranchOpIdWidthCF bits (low bits)
      ift.spec_branch_is_secret,        // [95]        1b
      ift.spec_branch_is_atk,           // [94]        1b
      uop.cf_op_count_id.getOrElse(0.U(uopIDCounterWidthCF.W)),                  // [93:78]     uopIDCounterWidthCF bits (full width)
      ift.infl_dropped,                 // [77:76]     2b
      0.U(1.W),                         // [75]        1b  cf_src_tainted: no writer since Phase 2
                                           //      (Task 3.9 dropped the constant column bit)
      s_tx,          // [74]        1b
      s_prop,           // [73]        1b
      s_acc,                // [72]        1b
      atk || ift.edge_atk,           // [71]        1b
      ift.speculated,                   // [70]        1b
      uop.cf_domain_id.getOrElse(0.U(iftTagWidth.W)),                    // [69]        1b
      uop.debug_inst,                      // [68:37]    32b
      uop.debug_pc(31, 0),                 // [36:5]     32b  [PC32 2026-09-10] was 40b
      priv(2, 0),                          // [4:2]       3b
      event_type(1, 0)                     // [1:0]       2b
    )
  }
}
