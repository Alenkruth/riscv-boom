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
  // [RECV3] One record per cycle on the shared lane: SQUASH (class 2), KILL (class 3) or STATE (class 8), see
  // IFTRecordV3.scala.  The commit slots carry COMMIT (class 1) or NOP (class 0, invalid lane).
  val squash_valid  = Bool()
  val squash_record = UInt(256.W)
  // Sticky overflow flag: set when a pending slot was written before drain cleared it
  val squash_ovf    = Bool()
}
