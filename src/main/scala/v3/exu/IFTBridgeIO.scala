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
//   [1:0]     event_type   2b  (01=commit, 10=squash, 11=cycle_anchor, 00=NOP/pad)
//   [4:2]     priv         3b
//   [44:5]    pc           40b (debug_pc[39:0])
//   [76:45]   insn         32b (debug_inst)
//   [77]      cf_domain_id
//   [78]      cf_speculated
//   [79]      cf_attacker_influence
//   [80]      cf_secret_access
//   [81]      cf_secret_propagation
//   [82]      cf_secret_transmission
//   [83]      cf_src_tainted
//   [84]      cf_infl_overflow
//   [100:85]  cf_op_count_id           (16b = uopIDCounterWidthCF)
//   [101]     cf_spec_branch_is_atk
//   [102]     cf_spec_branch_is_secret
//   [118:103] cf_spec_branch_op_id     (16b = uopIDCounterWidthCF)
//   [119]     cf_single_step
//   [143:120] cf_fu_bitmap             (24b = numModules)
//   --- influencer[0..2]: 3 × 28 bits = 84 bits ---
//   Per slot: valid(1)+op_count(16)+infl_type(5)+is_atk(1)+is_secret(1)+deny_count(4) = 28b
//   [144]     influencer[0].valid
//   [159:145] influencer[0].op_count   (16b)
//   [164:160] influencer[0].infl_type
//   [165]     influencer[0].is_atk
//   [166]     influencer[0].is_secret
//   [170:167] influencer[0].deny_count
//   [171]     influencer[1].valid
//   ...
//   [227:200] influencer[2]  (28b)
//   [255:228] reserved/zero  (28b)
//
// Total: 144 (fixed) + 84 (influencers) + 28 (reserved) = 256 bits. ✓
// ---------------------------------------------------------------------------
object uopToIFTBits extends CoreFuzzingConstants {
  def apply(uop: MicroOp, event_type: UInt, priv: UInt): UInt = {
    // Per-slot bit width: valid(1) + op_count(uopIDCounterWidthCF) + infl_type(5) +
    //                     is_atk(1) + is_secret(1) + deny_count(4)
    val inflSlotBits = 1 + uopIDCounterWidthCF + inflTypeWidthCF + 1 + 1 + 4
    // Pack influencer entries (numInfluencerSlotsCF slots × inflSlotBits each), MSB→LSB
    val infl_bits = Cat(
      (numInfluencerSlotsCF - 1 to 0 by -1).map { k =>
        val e = uop.cf_influencer_list(k)
        Cat(e.deny_count,            // 4b
            e.is_secret,             // 1b
            e.is_atk,                // 1b
            e.infl_type,             // 5b  (inflTypeWidthCF)
            e.op_count,              // uopIDCounterWidthCF bits (full width)
            e.valid)                 // 1b
      }
    )
    // Fixed fields below influencer region: 2+3+40+32+8+uopIDCounterWidthCF+2+uopIDCounterWidthCF+1+24
    val fixedBits = 2 + 3 + 40 + 32 + 8 + uopIDCounterWidthCF + 2 + uopIDCounterWidthCF + 1 + 24
    val inflTotalBits = numInfluencerSlotsCF * inflSlotBits
    val reservedBits  = 256 - fixedBits - inflTotalBits
    // Build 256-bit record (Cat: first arg = MSB, last arg = LSB)
    Cat(
      0.U(reservedBits.W),                 // reserved (fills to 256)
      infl_bits,                           // influencer slots
      uop.cf_fu_bitmap,                    // 24b (numModules wide)
      uop.cf_single_step,                  // 1b
      uop.cf_spec_branch_op_id,            // uopIDCounterWidthCF bits (full width)
      uop.cf_spec_branch_is_secret,        // 1b
      uop.cf_spec_branch_is_atk,           // 1b
      uop.cf_op_count_id,                  // uopIDCounterWidthCF bits (full width)
      uop.cf_infl_overflow,                // [84]        1b
      uop.cf_src_tainted,                  // [83]        1b
      uop.cf_secret_transmission,          // [82]        1b
      uop.cf_secret_propagation,           // [81]        1b
      uop.cf_secret_access,                // [80]        1b
      uop.cf_attacker_influence,           // [79]        1b
      uop.cf_speculated,                   // [78]        1b
      uop.cf_domain_id,                    // [77]        1b
      uop.debug_inst,                      // [76:45]    32b
      uop.debug_pc(39, 0),                 // [44:5]     40b
      priv(2, 0),                          // [4:2]       3b
      event_type(1, 0)                     // [1:0]       2b
    )
  }
}
