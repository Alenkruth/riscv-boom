//******************************************************************************
// Record v3 STATE record (class 8) bundles.  [V3STATE] SP-14.
// Design: claude-artifacts/ift-overhaul/reviews/record-v3-sink-design-20261025.md (sha1 338af393), 5.1, 5.4, 6.6.
//------------------------------------------------------------------------------
// Each group body's asUInt is EXACTLY its 5.1 bit range.  Chisel puts the FIRST declared field at the MSB, so the
// fields are declared in REVERSE 5.1 order; the comment on each field is its absolute 5.1 position.  Brace fields
// of 5.1 ({v, set, way} ...) are LSB first (v lowest), as the slot table 2.4.  IFTRecordV3.state / stateEpoch
// (exu/IFTRecordV3.scala, SP-02) place each group's asUInt at its 5.1 base unchanged.
// Plain Bundles with the 5.1 literal widths (the contract); every tap site requires the config to match.
// Only elaborated when ENABLE_IFT_BRIDGE || IFT_ORACLE (the ports below are None otherwise): IFT-off and
// bridge-less builds are unchanged.
//******************************************************************************

package boom.v3.common

import chisel3._
import chisel3.util._

/** R: D$ replacement state, one write port / one allocation per cycle.  [65:32], 34 b. */
class IftRGroup extends Bundle {
  val mshr   = UInt(3.W)   // [65:63] allocated MSHR (kind 2, 3), else 0
  val key    = UInt(12.W)  // [62:51] kind 1, 2: request op_count[11:0]; kind 3: trigger PC[13:2]
  val dom    = Bool()      // [50]    request domain (prefetch: the trigger's)
  val atk    = Bool()      // [49]    written replacement-state attacker taint
  val sec    = Bool()      // [48]    written replacement-state secret taint
  val eatk   = Bool()      // [47]    eviction attacker label
  val esec   = Bool()      // [46]    eviction secret label
  val upg    = Bool()      // [45]    allocation with tag match (permission upgrade, no victim)
  val vvalid = Bool()      // [44]    the chosen victim way holds a valid line
  val way    = UInt(3.W)   // [43:41] written way (UPGRADEWAY-corrected)
  val set    = UInt(7.W)   // [40:34] masked set
  val kind   = UInt(2.W)   // [33:32] 1 hit refresh, 2 allocation, 3 prefetch allocation
}

/** M: D$ tag/coherence write (metaWriteArb output).  [84:66], 19 b. */
class IftMGroup extends Bundle {
  val dom  = Bool()        // [84]    written domain (MSHR writes; 0 for prober/walker)
  val sec  = Bool()        // [83]    written secret mark (MSHR writes; 0 for prober/walker)
  val coh  = UInt(2.W)     // [82:81] written coherence state
  val mshr = UInt(3.W)     // [80:78] writing MSHR (src 0, 1), else 0
  val way  = UInt(3.W)     // [77:75] written way (src 3 walker: 0, it writes every way)
  val set  = UInt(7.W)     // [74:68] written set
  val src  = UInt(2.W)     // [67:66] 0 MSHR fill, 1 MSHR clear, 2 prober, 3 walker
}

/** L: D$ LFSR advance without allocation (replacement conf 3 only).  [98:85], 14 b. */
class IftLGroup extends Bundle {
  val atk = Bool()         // [98]
  val sec = Bool()         // [97]
  val key = UInt(12.W)     // [96:85] firing lane's request op_count[11:0]
}

/** IC: I$ refill or invalidate.  [113:99], 15 b. */
class IftIcGroup extends Bundle {
  val hi    = UInt(2.W)    // [113:112] refill_paddr(13,12)
  val way   = UInt(3.W)    // [111:109] repl_way
  val set   = UInt(6.W)    // [108:103] refill_idx
  val sec   = Bool()       // [102]     refill_secret_reg
  val dom   = Bool()       // [101]     refill_domain_reg
  val drop  = Bool()       // [100]     refilled with `invalidated` set (valid bit not written)
  val inval = Bool()       // [99]      io.invalidate (fence.i or debug-mode jalr)
}

/** RAS: the one RAS write port.  [134:114], 21 b. */
class IftRasGroup extends Bundle {
  val addr12 = UInt(12.W)  // [134:123] write_addr(13,2)
  val idx    = UInt(5.W)   // [122:118] write_idx
  val dom    = Bool()      // [117]     write_domain
  val watk   = Bool()      // [116]     write_watk
  val sec    = Bool()      // [115]     write_secret
  val push   = Bool()      // [114]     write_push (1 call push, 0 FTQ top repair)
}

/** PRED: one predictor update at its TAGE table-write cycle (SP-19).  [253:135], 119 b. */
class IftPredGroup extends Bundle {
  val tg_hist16      = UInt(16.W)  // [253:238] ghist.old_history(15,0)
  val tg_nsnt        = Bool()      // [237]     ghist.new_saw_branch_not_taken
  val tg_nst         = Bool()      // [236]     ghist.new_saw_branch_taken
  val tg_udec_slot   = UInt(3.W)   // [235:233] bank-slot of the u-only writes ({bank, slot})
  val tg_udec_mask   = UInt(7.W)   // [232:226] tables with a u-only write (update_u_mask && !update_mask)
  val tg_alloc_slot  = UInt(3.W)   // [225:223] bank-slot of the allocation
  val tg_alloc_table = UInt(3.W)   // [222:220] allocated table
  val tg_alloc_v     = Bool()      // [219]     an allocation was written
  val tg_prov        = UInt(24.W)  // [218:195] 8 x 3 b, bank-slot s at [197+3s:195+3s]: 0 none, t+1 = provider table t
  val ubtb1_way      = UInt(4.W)   // [194:191]
  val ubtb1_v        = Bool()      // [190]
  val ubtb0_way      = UInt(4.W)   // [189:186]
  val ubtb0_v        = Bool()      // [185]
  val btb1_way       = UInt(1.W)   // [184]
  val btb1_set       = UInt(7.W)   // [183:177]
  val btb1_v         = Bool()      // [176]
  val btb0_way       = UInt(1.W)   // [175]
  val btb0_set       = UInt(7.W)   // [174:168]
  val btb0_v         = Bool()      // [167]
  val u_pc12         = UInt(12.W)  // [166:155] update pc(13,2)
  val btb_rst        = Bool()      // [154]     a BTB re-reset (geometry change) starts in this cycle
  val u_kind         = Bool()      // [153]     0 FTQ commit update, 1 f4 BTB fast repair
  val u_dom          = Bool()      // [152]
  val u_sec          = Bool()      // [151]
  val u_isjal        = Bool()      // [150]
  val u_isbr         = Bool()      // [149]
  val u_mispred      = Bool()      // [148]
  val u_taken        = Bool()      // [147]
  val u_cfi_idx      = UInt(3.W)   // [146:144]
  val u_cfi_v        = Bool()      // [143]
  val u_brmask       = UInt(8.W)   // [142:135] br_mask (u_kind 1: btb_mispredicts)
}

/** EPOCH overlay on the PRED positions (gv_EPOCH = 1, gv_PRED = 0).  [253:135], 119 b. */
class IftEpochGroup extends Bundle {
  val zero7     = UInt(7.W)        // [253:247] 0 (reserved, checked by --check)
  val killovf16 = UInt(16.W)       // [246:231] core kill_ovf_cnt at emission
  val sqovf16   = UInt(16.W)       // [230:215] rob.io.ift_squash_ovf_cnt at emission
  val recs32    = UInt(32.W)       // [214:183] records presented on the 5 tile slots before the assembly cycle
  val cyc48     = UInt(48.W)       // [182:135] tile time base at the assembly cycle (= record cyc + 3)
}

/** D$ -> tile, the D$ tap register (SP-15).  76 b (FIX-V2-11: + pf_id, pf_multi). */
class IftMemState extends Bundle {
  val r_v     = Bool()
  val r       = new IftRGroup
  val m_v     = Bool()
  val m       = new IftMGroup
  val l_v     = Bool()
  val l       = new IftLGroup
  val pf_drop = Bool()             // STATE [254]
  // [V3STATE] FIX-V2-11 (owner open item, header [15:12]): which MSHR dropped its parked line (the lowest when several
  // did in the same cycle, e.g. a fence's clear_all) and whether more than one did.  0 when pf_drop = 0.
  val pf_id    = UInt(3.W)         // STATE [14:12]
  val pf_multi = Bool()            // STATE [15]
  val wrport  = Bool()             // STATE [255]
}

class IftIcTap   extends Bundle { val v = Bool(); val g = new IftIcGroup   }   // 16 b
class IftRasTap  extends Bundle { val v = Bool(); val g = new IftRasGroup  }   // 22 b
class IftPredTap extends Bundle { val v = Bool(); val g = new IftPredGroup }   // 120 b

/** frontend -> tile, the frontend cut-out register (SP-18).  158 b = the SLR2-SLR3 cut (08 section 8.4). */
class IftFeState extends Bundle {
  val ic   = new IftIcTap
  val ras  = new IftRasTap
  val pred = new IftPredTap
}

/** SP-19: the predictor-update fields taken at the bpd_update_arbiter output, piped to the TAGE write cycle.  50 b. */
class IftPredU extends Bundle {
  val v       = Bool()
  val brmask  = UInt(8.W)
  val cfi_v   = Bool()
  val cfi_idx = UInt(3.W)
  val taken   = Bool()
  val mispred = Bool()
  val isbr    = Bool()
  val isjal   = Bool()
  val sec     = Bool()
  val dom     = Bool()
  val kind    = Bool()
  val pc12    = UInt(12.W)
  val nst     = Bool()
  val nsnt    = Bool()
  val hist16  = UInt(16.W)
}

/** SP-19: one physical predictor bank's write-stage taps, aligned to the TAGE write cycle.  42 b.
  * Each component (BTB, uBTB, TAGE) drives only its own fields; the base class drives 0. */
class IftPredBankTap extends Bundle {
  val btb_v          = Bool()
  val btb_set        = UInt(7.W)
  val btb_way        = UInt(1.W)
  val btb_rst        = Bool()
  val ubtb_v         = Bool()
  val ubtb_way       = UInt(4.W)
  val tg_prov        = UInt(12.W)  // 4 x 3 b, bank slot w at [3w+2:3w]
  val tg_alloc_v     = Bool()
  val tg_alloc_table = UInt(3.W)
  val tg_alloc_slot  = UInt(2.W)
  val tg_udec_mask   = UInt(7.W)
  val tg_udec_slot   = UInt(2.W)
}

/** core -> tile (SP-17 drives, SP-16 reads).  97 b of wires, no register. */
class IftCoreToTile extends Bundle {
  val dbg       = Bool()           // cf_dbg (core.scala)
  val sqovf16   = UInt(16.W)       // rob.io.ift_squash_ovf_cnt
  val killovf16 = UInt(16.W)       // kill_ovf_cnt
  val tsc48     = UInt(48.W)       // debug_tsc_reg(47,0): the free-running cycle counter, the tile time base
  val cyc16     = UInt(16.W)       // SP-03's ift_cyc16 (read only by the SP-22 debug assert)
}
