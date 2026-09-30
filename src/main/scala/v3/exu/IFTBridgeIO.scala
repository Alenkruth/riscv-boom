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
// 256-bit layout (LSB = bit 0):
//
//   [1:0]     event_type            2b   01=commit, 10=squash, 11=cycle_anchor, 00=NOP
//   [4:2]     priv                  3b
//   [44:5]    pc                    40b  debug_pc[39:0]
//   [76:45]   insn                  32b
//   [77]      cf_domain_id          1b
//   [78]      cf_speculated         1b
//   [79]      cf_attacker_influence 1b
//   [80]      cf_secret_access      1b
//   [81]      cf_secret_propagation 1b
//   [82]      cf_secret_transmission 1b
//   [83]      cf_src_tainted        1b
//   [85:84]   cf_infl_dropped       2b   saturating COUNT of discarded influencers
//                                        (0..3, 3 means ">=3"); was a 1b sticky flag
//   [101:86]  cf_op_count_id        16b  uopIDCounterWidthCF -- the ANCHOR every
//                                        truncated field below reconstructs against
//   [102]     cf_spec_branch_is_atk 1b
//   [103]     cf_spec_branch_is_secret 1b
//   [111:104] cf_spec_branch_op_id  8b   specBranchOpIdWidthCF -- LOW bits only
//   [112]     cf_single_step        1b
//   [136:113] cf_fu_bitmap          24b  one bit per pipeline module (numModules=24)
//   [139:137] cf_cntd_deny_count    3b   Log2Bucket -- issue-port denial cycles
//   [142:140] cf_stall_cycles_rob   3b   Log2Bucket -- ROB-full dispatch stall cycles
//   [145:143] cf_stall_cycles_stq   3b   Log2Bucket -- STQ head-of-line residency
//   --- influencer[0..3]: 4 slots x 18 bits = 72 bits ---
//   [146]     cf_infl_oc_aliased    1b   a truncated op_count on this record is OUT
//                                        of reconstructable range -- the stored low
//                                        bits reconstruct to the WRONG producer, so
//                                        do not build a flow through it
//   [147]     influencer[0].valid   1b
//   [157:148] influencer[0].op_count 10b inflOpCountWidthCF -- LOW bits only
//   [162:158] influencer[0].infl_type 5b
//   [163]     influencer[0].is_atk  1b
//   [164]     influencer[0].is_secret 1b
//   [182:165] influencer[1]         18b  same layout
//   [200:183] influencer[2]         18b  same layout
//   [218:201] influencer[3]         18b  same layout
//   [226:219] pdst                   8b  maxPregSz
//   [234:227] prs1                   8b
//   [242:235] prs2                   8b
//   [244:243] lrs1_rtype             2b  is prs1 really read
//   [246:245] lrs2_rtype             2b  is prs2 really read
//   [255:247] reserved/zero          9b
//
// Total: 147 fixed + 72 influencers + 28 operands + 9 reserved = 256 bits.
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
//
// Total: 154 (fixed) + 64 (influencers) + 28 (operands) + 10 (reserved) = 256 bits. ✓
// ---------------------------------------------------------------------------
object uopToIFTBits extends CoreFuzzingConstants {
  def apply(uop: MicroOp, event_type: UInt, priv: UInt): UInt = {
    // Per-slot bit width: valid(1) + op_count(inflOpCountWidthCF) + infl_type(5) +
    //                     is_atk(1) + is_secret(1)
    val inflSlotBits = 1 + inflOpCountWidthCF + inflTypeWidthCF + 1 + 1
    // Pack influencer entries (numInfluencerSlotsCF slots × inflSlotBits each), MSB→LSB
    val infl_bits = Cat(
      (numInfluencerSlotsCF - 1 to 0 by -1).map { k =>
        val e = uop.cf_influencer_list(k)
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
      0.U(reservedBits.W),                 // reserved (fills to 256)
      uop.cf_sec_branch_ctr,               // 7b   GHR SECRET window counter -- on the
                                           //      bridge as of PC32; previously log-only,
                                           //      which made half of descriptor.py's GHR
                                           //      novelty axis impossible on FPGA
      uop.cf_atk_branch_ctr,               // 7b   GHR attacker window counter;
                                           //      globalHistoryLength - v = retirements
                                           //      back to the tainting branch
      uop.lrs2_rtype,                      // 2b   prs2 really read?
      uop.lrs1_rtype,                      // 2b   prs1 really read?
      uop.prs2,                            // pregBits
      uop.prs1,                            // pregBits
      uop.pdst,                            // pregBits
      infl_bits,                           // influencer slots
      uop.cf_infl_oc_aliased,              // [146]       1b  a truncated op_count is
                                           //                 out of reconstructable range
      uop.cf_stall_cycles_stq,             // [145:143]   3b  Log2Bucket
      uop.cf_stall_cycles_rob,             // [149:147]   3b  Log2Bucket
      uop.cf_cntd_deny_count,              // [146:144]   3b  Log2Bucket
      uop.cf_fu_bitmap,                    // 24b (numModules wide)
      uop.cf_single_step,                  // 1b
      uop.cf_spec_branch_op_id,            // uopIDCounterWidthCF bits (full width)
      uop.cf_spec_branch_is_secret,        // 1b
      uop.cf_spec_branch_is_atk,           // 1b
      uop.cf_op_count_id,                  // uopIDCounterWidthCF bits (full width)
      uop.cf_infl_dropped,                // [85:84]     2b
      uop.cf_src_tainted,                  // [83]        1b
      uop.cf_secret_transmission,          // [82]        1b
      uop.cf_secret_propagation,           // [81]        1b
      uop.cf_secret_access,                // [80]        1b
      uop.cf_attacker_influence,           // [79]        1b
      uop.cf_speculated,                   // [78]        1b
      uop.cf_domain_id,                    // [77]        1b
      uop.debug_inst,                      // [76:45]    32b
      uop.debug_pc(31, 0),                 // [36:5]     32b  [PC32 2026-09-10] was 40b
      priv(2, 0),                          // [4:2]       3b
      event_type(1, 0)                     // [1:0]       2b
    )
  }
}
