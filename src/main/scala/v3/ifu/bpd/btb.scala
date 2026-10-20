package boom.v3.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.v3.common._
import boom.v3.util.{BoomCoreStringPrefix}

import scala.math.min

case class BoomBTBParams(
  nSets: Int = 128,
  nWays: Int = 2,
  offsetSz: Int = 13,
  extendedNSets: Int = 128
)


class BTBBranchPredictorBank(params: BoomBTBParams = BoomBTBParams())(implicit p: Parameters)
  extends BranchPredictorBank()(p)
  with freechips.rocketchip.util.CoreFuzzingConstants
{
  override val nSets         = params.nSets
  override val nWays         = params.nWays
  val tagSz         = vaddrBitsExtended - log2Ceil(nSets) - log2Ceil(fetchWidth) - 1
  val offsetSz      = params.offsetSz
  val extendedNSets = params.extendedNSets

  require(isPow2(nSets))
  require(isPow2(extendedNSets) || extendedNSets == 0)
  require(extendedNSets <= nSets)
  require(extendedNSets >= 1)

  class BTBEntry extends Bundle {
    val offset   = SInt(offsetSz.W)
    val extended = Bool()
  }
  val btbEntrySz = offsetSz + 1
  override def cfDrivesBtbPc: Boolean = ENABLE_IFT   // [BTBATK] composer PC select (predictor.scala)

  class BTBMeta extends Bundle {
    val is_br = Bool()
    val tag   = UInt(tagSz.W)
  }
  val btbMetaSz = tagSz + 1

  class BTBPredictMeta extends Bundle {
    val write_way = UInt(log2Ceil(nWays).W)
  }

  val s1_meta = Wire(new BTBPredictMeta)
  val f3_meta = RegNext(RegNext(s1_meta))


  io.f3_meta := f3_meta.asUInt

  override val metaSz = s1_meta.asUInt.getWidth

  val doing_reset = RegInit(true.B)
  val reset_idx   = RegInit(0.U(log2Ceil(nSets).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nSets-1).U) { doing_reset := false.B }

  // corefuzzing: re-arm reset when BTB set/way config changes.
  // SyncReadMem entries from the old config remain valid in SRAM.  With a smaller
  // set-count mask, two PCs that differ only in masked-out bits alias to the same set
  // with identical tags (tag = idx >> log2Ceil(nSets), fixed at max), producing false
  // hits that redirect the IFU to wrong PCs.  Reset takes nSets(=128) cycles; fetch
  // is blocked during QS_EXECUTING so the BTB is clean before execution resumes.
  val prev_btb_set_idx = RegNext(io.cf_btb_set_idx, 0.U)
  val prev_btb_way_idx = RegNext(io.cf_btb_way_idx, 0.U)
  when ((io.cf_btb_set_idx =/= prev_btb_set_idx) || (io.cf_btb_way_idx =/= prev_btb_way_idx)) {
    doing_reset := true.B
    reset_idx   := 0.U
  }

  // corefuzzing: runtime BTB set count selection
  val btbSetOptionsVec    = VecInit(btbSetOptions.map(_.U))
  val cf_btb_active_sets  = btbSetOptionsVec(io.cf_btb_set_idx)
  // Mask s0/s1_update indices to keep accesses within the active set range
  val s0_idx_masked        = s0_idx       & (cf_btb_active_sets - 1.U)
  val s1_update_idx_masked = s1_update_idx & (cf_btb_active_sets - 1.U)

  // corefuzzing: runtime BTB way count selection — gates READ hits to active ways only
  // Without this, stale entries in deactivated ways cause false BTB hits and corrupt IFT records
  val btbWayOptionsVec   = VecInit(btbWayOptions.map(_.U))
  val cf_btb_active_ways = btbWayOptionsVec(io.cf_btb_way_idx)

  val meta     = Seq.fill(nWays) { SyncReadMem(nSets, Vec(bankWidth, UInt(btbMetaSz.W))) }
  val btb      = Seq.fill(nWays) { SyncReadMem(nSets, Vec(bankWidth, UInt(btbEntrySz.W))) }
  val ebtb     = SyncReadMem(extendedNSets, UInt(vaddrBitsExtended.W))
  // corefuzzing: domain shadow — tracks domain of the fetch that last wrote each BTB entry
  val btb_domain  = Seq.fill(nWays) { SyncReadMem(nSets, Vec(bankWidth, UInt(1.W))) }
  // corefuzzing: secret shadow — 1 if entry was trained by a secret-dependent instruction
  val btb_secret  = Seq.fill(nWays) { SyncReadMem(nSets, Vec(bankWidth, Bool())) }
  // [EBTBDOM] patch 65 (V9, owner 2026-10-19 16:3x): domain shadow of the extended-target table.  ebtb holds
  // ONE target per set, shared by every way and slot whose entry has extended = 1, so an allocation of either
  // domain overwrites the target a resident entry of the other domain jumps to, while that entry's meta domain
  // is unchanged (no BTB_STATE edge: under-taint of a branch-target-injection path).  1 bit per ebtb set,
  // written with ebtb (the update's domain), read with it.  IFT only (no memory, no port IFT off).
  val ebtb_domain = if (ENABLE_IFT) Some(SyncReadMem(extendedNSets, UInt(1.W))) else None

  val mems = (((0 until nWays) map ({w:Int => Seq(
    (f"btb_meta_way$w", nSets, bankWidth * btbMetaSz),
    (f"btb_data_way$w", nSets, bankWidth * btbEntrySz))})).flatten ++ Seq(("ebtb", extendedNSets, vaddrBitsExtended)))

  val s1_req_rbtb    = VecInit(btb.map { b => VecInit(b.read(s0_idx_masked, s0_valid).map(_.asTypeOf(new BTBEntry))) })
  val s1_req_rmeta   = VecInit(meta.map { m => VecInit(m.read(s0_idx_masked, s0_valid).map(_.asTypeOf(new BTBMeta))) })
  val s1_req_rebtb   = ebtb.read(s0_idx_masked, s0_valid)
  val s1_req_rebdom  = ebtb_domain.map(_.read(s0_idx_masked, s0_valid)).getOrElse(0.U(1.W))   // [EBTBDOM] patch 65
  val s1_req_tag     = s1_idx >> log2Ceil(nSets)
  // corefuzzing: domain shadow read — same index/enable as btb/meta
  // [R124GATE3] patch 28 (B51): btb_domain is read/written only with ENABLE_IFT; IFT off it has no ports
  // (removed, like btb_secret) and the domain-mismatch output is 0.
  val s1_req_rdomain = if (ENABLE_IFT) VecInit(btb_domain.map { d => d.read(s0_idx_masked, s0_valid) })
                       else VecInit(Seq.fill(nWays)(VecInit(Seq.fill(bankWidth)(0.U(1.W)))))
  // corefuzzing: secret shadow read
  val s1_req_rsecret = VecInit(btb_secret.map { s => s.read(s0_idx_masked, s0_valid) })

  val s1_resp   = Wire(Vec(bankWidth, Valid(UInt(vaddrBitsExtended.W))))
  val s1_is_br  = Wire(Vec(bankWidth, Bool()))
  val s1_is_jal = Wire(Vec(bankWidth, Bool()))

  val s1_hit_ohs = VecInit((0 until bankWidth) map { i =>
    VecInit((0 until nWays) map { w =>
      // corefuzzing: gate by active ways — prevents stale entries in deactivated ways
      // from generating false hits when BTB is reconfigured to fewer ways
      s1_req_rmeta(w)(i).tag === s1_req_tag(tagSz-1,0) && (w.U < cf_btb_active_ways)
    })
  })
  val s1_hits     = s1_hit_ohs.map { oh => oh.reduce(_||_) }
  val s1_hit_ways = s1_hit_ohs.map { oh => PriorityEncoder(oh) }

  for (w <- 0 until bankWidth) {
    val entry_meta = s1_req_rmeta(s1_hit_ways(w))(w)
    val entry_btb  = s1_req_rbtb(s1_hit_ways(w))(w)
    // corefuzzing: suppress BTB hit during quiesce — RegNext because BTB SyncReadMem
    // delivers results at s1 (one cycle after the s0 request from the QS_FETCH window).
    // Without suppression, a stale entry trained by pre-quiesce workload execution can
    // redirect the IFU mid-bundle, killing Packet B/C before they enter the ROB.
    s1_resp(w).valid := !doing_reset && !RegNext(io.cf_btb_quiesce) && s1_valid && s1_hits(w)
    s1_resp(w).bits  := Mux(
      entry_btb.extended,
      s1_req_rebtb,
      (s1_pc.asSInt + (w << 1).S + entry_btb.offset).asUInt)
    s1_is_br(w)  := !doing_reset && s1_resp(w).valid &&  entry_meta.is_br
    s1_is_jal(w) := !doing_reset && s1_resp(w).valid && !entry_meta.is_br


    io.resp.f2(w) := io.resp_in(0).f2(w)
    io.resp.f3(w) := io.resp_in(0).f3(w)
    when (RegNext(s1_hits(w))) {
      io.resp.f2(w).predicted_pc := RegNext(s1_resp(w))
      io.resp.f2(w).is_br        := RegNext(s1_is_br(w))
      io.resp.f2(w).is_jal       := RegNext(s1_is_jal(w))
      when (RegNext(s1_is_jal(w))) {
        io.resp.f2(w).taken      := true.B
      }
    }
    when (RegNext(RegNext(s1_hits(w)))) {
      io.resp.f3(w).predicted_pc := RegNext(io.resp.f2(w).predicted_pc)
      io.resp.f3(w).is_br        := RegNext(io.resp.f2(w).is_br)
      io.resp.f3(w).is_jal       := RegNext(io.resp.f2(w).is_jal)
      when (RegNext(RegNext(s1_is_jal(w)))) {
        io.resp.f3(w).taken      := true.B
      }
    }
  }

  // corefuzzing: BTB domain mismatch — any hit entry was last updated by a different domain
  // [EBTBDOM] patch 65: ...or the hit entry jumps through ebtb (extended) and ebtb was last written by the other
  // domain.  entry.extended comes through the same hit-way select as the domain bit (parallel); the slot term
  // goes from 3 to 5 inputs, still one LUT after the selects: 0 levels (it ends at the f2 RegNext).
  val s1_btb_domain_mismatch = VecInit((0 until bankWidth).map { w =>
    s1_hits(w) && ((s1_req_rdomain(s1_hit_ways(w))(w) =/= s1_domain) ||
                   (s1_req_rbtb(s1_hit_ways(w))(w).extended && s1_req_rebdom =/= s1_domain))
  }).reduce(_||_)
  // corefuzzing: BTB secret mismatch — any hit entry was trained by a secret instruction
  // [BPDMASK] r170 A (DF-dt-sprop): a slot below the fetch PC, or a bank whose request is not
  // valid, is not part of the packet; its secret label raises no edge.
  def s1_btb_sec_slot(w: Int): Bool = s1_valid && s1_hits(w) && s1_req_rsecret(s1_hit_ways(w))(w).asBool && s1_mask(w)
  val s1_btb_secret_mismatch = VecInit((0 until bankWidth).map(w => s1_btb_sec_slot(w))).reduce(_||_)
  // pipeline 2 cycles to f3 (s1→f2→f3)
  // [BTBMAIN2 2026-09-11] TEMPORARY.  The BTB channel is PROVEN working: on t15 the probe
  // showed 9 mismatches -> 9 edges, including one victim fetch hitting an ATTACKER-planted
  // entry (the branch-target-injection direction).  Yet spectre-v2 -- the workload built to do
  // BTI -- yields 0 edges in 1,365,332 records.  Two possibilities, and they differ in what
  // they say about the attack: the attacker->victim alias is never FORMED, or it is formed and
  // the victim never HITS it.  Print every hit involving the attacker domain on either side.
  if (ENABLE_CF_DEBUG_PRINTF) {
    for (w <- 0 until bankWidth) {
      when (s1_hits(w) && (s1_domain === 1.U || s1_req_rdomain(s1_hit_ways(w))(w) === 1.U)) {
        // [CFNOISE-COMMENTED] printf("\n[BTBMH] w=%d edom=%d fdom=%d mism=%d\n",
          // [CFNOISE-COMMENTED] w.U, s1_req_rdomain(s1_hit_ways(w))(w), s1_domain,
          // [CFNOISE-COMMENTED] (s1_req_rdomain(s1_hit_ways(w))(w) =/= s1_domain))
      }
    }
  }
  io.f3_btb_domain_mismatch  := (if (ENABLE_IFT) RegNext(RegNext(s1_btb_domain_mismatch)) else false.B)   // [R124GATE3]
  // [R124GATE2] patch 27 (r124/B51): the two f3 flops only with ENABLE_IFT (IFT off the BTB
  // secret SRAM is elided, so they only delayed a constant).
  io.f3_btb_secret_mismatch  := (if (ENABLE_IFT) RegNext(RegNext(s1_btb_secret_mismatch)) else false.B)

  val alloc_way = if (nWays > 1) {
    val r_metas = Cat(VecInit(s1_req_rmeta.map { w => VecInit(w.map(_.tag)) }).asUInt, s1_req_tag(tagSz-1,0))
    val l = log2Ceil(nWays)
    val nChunks = (r_metas.getWidth + l - 1) / l
    val chunks = (0 until nChunks) map { i =>
      r_metas(min((i+1)*l, r_metas.getWidth)-1, i*l)
    }
    chunks.reduce(_^_)
  } else {
    0.U
  }
  // corefuzzing: clamp alloc_way to 0 when only 1 way is active (cf_btb_way_idx === 1)
  val alloc_way_clamped = Mux(io.cf_btb_way_idx === 1.U, 0.U, alloc_way)
  s1_meta.write_way := Mux(s1_hits.reduce(_||_),
    PriorityEncoder(s1_hit_ohs.map(_.asUInt).reduce(_|_)),
    alloc_way_clamped)

  val s1_update_cfi_idx = s1_update.bits.cfi_idx.bits
  val s1_update_meta    = s1_update.bits.meta.asTypeOf(new BTBPredictMeta)

  val max_offset_value = Cat(0.B, ~(0.U((offsetSz-1).W))).asSInt
  val min_offset_value = Cat(1.B,  (0.U((offsetSz-1).W))).asSInt
  val new_offset_value = (s1_update.bits.target.asSInt -
    (s1_update.bits.pc + (s1_update.bits.cfi_idx.bits << 1)).asSInt)
  val offset_is_extended = (new_offset_value > max_offset_value ||
                            new_offset_value < min_offset_value)


  val s1_update_wbtb_data  = Wire(new BTBEntry)
  s1_update_wbtb_data.extended := offset_is_extended
  s1_update_wbtb_data.offset   := new_offset_value
  val s1_update_wbtb_mask = (UIntToOH(s1_update_cfi_idx) &
    Fill(bankWidth, s1_update.bits.cfi_idx.valid && s1_update.valid && s1_update.bits.cfi_taken && s1_update.bits.is_commit_update))

  val s1_update_wmeta_mask = ((s1_update_wbtb_mask | s1_update.bits.br_mask) &
    (Fill(bankWidth, s1_update.valid && s1_update.bits.is_commit_update) |
     (Fill(bankWidth, s1_update.valid) & s1_update.bits.btb_mispredicts)
    )
  )
  val s1_update_wmeta_data = Wire(Vec(bankWidth, new BTBMeta))

  for (w <- 0 until bankWidth) {
    s1_update_wmeta_data(w).tag     := Mux(s1_update.bits.btb_mispredicts(w), 0.U, s1_update_idx >> log2Ceil(nSets))
    s1_update_wmeta_data(w).is_br   := s1_update.bits.br_mask(w)
  }

  for (w <- 0 until nWays) {
    when (doing_reset || s1_update_meta.write_way === w.U || (w == 0 && nWays == 1).B) {
      btb(w).write(
        Mux(doing_reset,
          reset_idx,
          s1_update_idx_masked),
        Mux(doing_reset,
          VecInit(Seq.fill(bankWidth) { 0.U(btbEntrySz.W) }),
          VecInit(Seq.fill(bankWidth) { s1_update_wbtb_data.asUInt })),
        Mux(doing_reset,
          (~(0.U(bankWidth.W))),
          s1_update_wbtb_mask).asBools
      )
      meta(w).write(
        Mux(doing_reset,
          reset_idx,
          s1_update_idx_masked),
        Mux(doing_reset,
          VecInit(Seq.fill(bankWidth) { 0.U(btbMetaSz.W) }),
          VecInit(s1_update_wmeta_data.map(_.asUInt))),
        Mux(doing_reset,
          (~(0.U(bankWidth.W))),
          s1_update_wmeta_mask).asBools
      )
      // corefuzzing: write domain shadow alongside meta (same index/mask)
      if (ENABLE_IFT) btb_domain(w).write(   // [R124GATE3]
        Mux(doing_reset, reset_idx, s1_update_idx_masked),
        Mux(doing_reset,
          VecInit(Seq.fill(bankWidth)(0.U(1.W))),
          VecInit(Seq.fill(bankWidth)(s1_update.bits.cf_domain_id.getOrElse(0.U(1.W))))),
        Mux(doing_reset, (~(0.U(bankWidth.W))), s1_update_wmeta_mask).asBools
      )
      // corefuzzing: write secret shadow alongside domain (same index/mask)
      // IFT gate: elided when ENABLE_IFT=false; btb_secret SRAMs become dead
      // state with no write driver → FIRRTL DCE removes them entirely.
      if (ENABLE_IFT) {
        btb_secret(w).write(
          Mux(doing_reset, reset_idx, s1_update_idx_masked),
          Mux(doing_reset,
            VecInit(Seq.fill(bankWidth)(false.B)),
            VecInit(Seq.fill(bankWidth)(s1_update.bits.cf_is_secret.getOrElse(false.B)))),
          Mux(doing_reset, (~(0.U(bankWidth.W))), s1_update_wmeta_mask).asBools
        )
      }
    }
  }
  when (s1_update_wbtb_mask =/= 0.U && offset_is_extended) {
    ebtb.write(s1_update_idx_masked, s1_update.bits.target)
    ebtb_domain.foreach(_.write(s1_update_idx_masked, s1_update.bits.cf_domain_id.getOrElse(0.U(1.W))))   // [EBTBDOM]
  }

  // [BTBDISP 2026-10-13, series-r step 16, owner ruling 110] BTB displacement residue, tracked
  // like the step-15 D$ eviction filter (dcache.scala ift_evf):
  //  * ift_btb_tagh (per way, per set, {v, tag[7:0]}): the low tag bits of the entry last
  //    written into (set, way).  Read at the update's s0 index, so at s1 the update knows
  //    which tag its write displaces.
  //  * [BTBATK] DF-btbatk: TWO displacement records per set, each {v, victim tag[7:0], pc[13:2]}
  //    (pc = the displacing allocator):
  //      ift_btb_evf  SECRET record   -- rewritten by a displacement whose update is secret
  //                                      (cf_is_secret);
  //      ift_btb_eva  ATTACKER record -- rewritten by a displacement whose update is
  //                                      attacker-domain (cf_domain_id == 1; the FTQ's
  //                                      cf_fetch_domain, the same bit btb_domain is written with).
  //    A secret attacker displacement rewrites both (same tag, same PC).  A displacement that is
  //    neither secret nor attacker clears both v bits.  A secret-only one leaves the attacker
  //    record alone and an attacker-only one leaves the secret record alone (DF-btbatk (B): in
  //    patch 22 an attacker non-secret displacement cleared a pending secret record).
  //    A displacement = a commit allocation (s1_upd_alloc) over a valid (set, way) whose tagh
  //    holds a different tag.  [BTBEVFLAST] DF-m60 (r140, the D$ [EVFLAST] rule): a fetch lookup
  //    never consumes either record (every fetch of the victim tag in that set carries the edge,
  //    wrong path included) until a later write of that record (below).
  //    [BTBSAMETAG] ruling 146 (iii), C1-(c), latest event wins: EVERY commit allocation
  //    (s1_upd_alloc: displacing, cold-way or same-tag re-allocation, any kind) clears a record of
  //    either kind whose victim tag[7:0] equals the newly allocated tag[7:0] (the victim is back in
  //    the BTB; its re-allocation is the latest event for that tag).  Each record therefore has a
  //    second read port at the update index (s0_update_idx, read with tagh).  This also ends the
  //    DF-m60 cold-way over-taint: a re-allocation of the victim tag into a never-written way is
  //    not a displacement, but it now clears the record naming that tag.
  //    Record write priority: reset sweep (v=0); else a displacement of the record's own kind
  //    (v=1, rewrite with {old tag, allocator PC}); else a clean displacement or a same-tag
  //    allocation (v=0).  An own-kind displacement never names the new tag (old tag =/= new tag),
  //    so the rewrite and the same-tag clear never conflict.
  //    v lives in the record word (LUTRAM), not in flops: the valid bit is written only at reset
  //    (the doing_reset sweep, which also re-arms on a set/way reconfiguration) and at a commit
  //    allocation, both single-index writes, so there are no valid flops, 7:128 write decode or
  //    128:1 read mux, and {v, tag, pc} is always read as one word.  Under a same-set
  //    read-during-write (back-to-back updates of one set, or a fetch of the set being written)
  //    SyncReadMem read-under-write is undefined: the read may return the word from before or
  //    after that write (firtool's Verilator model returns the new word); either way the word is
  //    self-consistent and the only effect is that the edge, or the same-tag clear, is seen one
  //    update earlier or later.
  //  * ift_btb_spc (per way, per set, pc[13:2]): PC of the last secret trainer of (set, way),
  //    so the existing hit edge (btb_secret, f3_btb_secret_mismatch) now carries the PC.
  // Edges (one FetchIftInfo candidate, util.scala fetchFlagCandidates bit 7/8): a fetch whose
  // tag[7:0] matches a valid record raises f3_btb_sec_disp (secret record) and/or
  // f3_btb_atk_disp (attacker record).  BTB_STATE PC-coded on ty 31/14/15/13 with sec=bit7,
  // atk=bit8 (bit 8 is gated by any_is_victim in the fetch buffer).  ONE PC field: priority
  // secret-trainer hit PC > secret record PC > attacker record PC (f3_btb_sec_pc).  A secret
  // attacker displacement wrote the same PC to both records, so its edge {atk=1, sec=1} is exact;
  // when a secret edge and an attacker record of a DIFFERENT allocator meet on one fetch, the
  // merged edge carries the secret PC (approximation, see the p4-btbatk report).
  // No value taint for the displacement edges (timing residue); r117 sticky s_prop / atk come
  // from the edge's sec / atk bits at dispatch / ROB enqueue.
  // Approximations (documented in the triage): 8-bit partial tag (aliases 1/256); one victim
  // per set per kind (the newest wins); tagh tracks commit writes only (a mispredict
  // invalidation leaves it valid); back-to-back updates of one set may read the tagh or record
  // word from before or after the previous update's write (SyncReadMem read-under-write is
  // undefined, see above); the hit PC is per (set, way), not per slot.
  // FPGA: tagh 2x128x9 b, spc 2x128x12 b, evf + eva 2x128x21 b LUTRAM, the records with a second
  // read port at the update index (2R1W: one more LUTRAM copy each); 0 FF (no valid flops);
  // read side = two 8-b compares beside the existing tag compare off the registered LUTRAM
  // outputs, a 3:1 PC select (one LUT6 per bit) into f2 flops; update side = the displacement
  // compare (tagh mux + 8-b) and, in parallel, two same-tag 8-b compares off the update-port
  // record reads, all into the two LUTRAM write enables (0 levels added).
  if (ENABLE_IFT) {
    val tag8W  = 8
    val setW   = log2Ceil(nSets)
    val recW   = 1 + tag8W + inflPcBitsCF                 // {v, victim tag[7:0], pc[13:2]}
    // [RUWBTB] patch 32 (DF-ruw-parity): tagh, evf and eva are combinational-read Mems with an
    // explicit write-first s0->s1 data register (cfFwdRead), the D$ [REPLBYP] form.  As
    // SyncReadMems the FireSim SV returned the OLD word to a same-set read issued in the write
    // cycle and the Verilator model the NEW one.  Reachable on the update port: commit update
    // k+1 (s0) reads tagh while update k (s1) writes it.  If k allocates A over X into way w and
    // k+1 allocates B into the same (set, way), the FPGA read X again: "A displaced by B" was
    // lost and X was recorded a second time (FPGA-only under-taint), and evf_same / eva_same
    // compared against the pre-k record (a stale record survived a same-tag clear).  The fetch
    // ports get the same forward, so every port keeps today's Verilator behaviour in both
    // lowerings.  (The comment above, "only shifts the edge by one update", held only for two
    // same-tag updates.)  ift_btb_spc is unchanged: its only reader is the fetch hit edge, whose
    // btb_secret/btb data have the same lowering, so each flow names the trainer of the
    // prediction it actually used.
    // FPGA: LUTRAM async read -> forward mux (1 LUT) -> the existing s1 data FF; 0 levels after
    // the FF.  The select (write enable + 7-bit compare) ends at that FF.  About 22 LUT (tagh) +
    // 2 x 2 x 21 (evf/eva, update + fetch ports) LUT, 0 FF.
    val ift_btb_tagh  = Seq.fill(nWays) { Mem(nSets, UInt((tag8W + 1).W)) }
    val ift_btb_spc   = Seq.fill(nWays) { SyncReadMem(nSets, UInt(inflPcBitsCF.W)) }
    val ift_btb_evf   = Mem(nSets, UInt(recW.W))  // secret record
    val ift_btb_eva   = Mem(nSets, UInt(recW.W))  // [BTBATK] attacker record
    // one write bus per memory (driven in the update block below)
    val tagh_we  = Wire(Vec(nWays, Bool()))
    val tagh_wa  = Wire(UInt())
    val tagh_wd  = Wire(UInt((tag8W + 1).W))
    val evf_we   = Wire(Bool())
    val eva_we   = Wire(Bool())
    val rec_wa   = Wire(UInt())
    val evf_wd   = Wire(UInt(recW.W))
    val eva_wd   = Wire(UInt(recW.W))
    def cfFwdRead(m: Mem[UInt], we: Bool, wa: UInt, wd: UInt, ra: UInt, en: Bool): UInt =
      RegEnable(Mux(we && wa === ra, wd, m.read(ra)), en)

    // ---- fetch side (s0 read, s1 use) ----
    val s1_ift_spc  = VecInit(ift_btb_spc.map(_.read(s0_idx_masked, s0_valid)))
    val s1_ift_evf  = cfFwdRead(ift_btb_evf, evf_we, rec_wa, evf_wd, s0_idx_masked, s0_valid)
    val s1_ift_eva  = cfFwdRead(ift_btb_eva, eva_we, rec_wa, eva_wd, s0_idx_masked, s0_valid)
    def recHit(r: UInt): Bool = s1_valid && !doing_reset && r(recW - 1) &&
                                r(recW - 2, inflPcBitsCF) === s1_req_tag(tag8W - 1, 0)
    val s1_evf_hit  = recHit(s1_ift_evf)
    val s1_eva_hit  = recHit(s1_ift_eva)
    val s1_sec_slot = (0 until bankWidth).map(w => s1_btb_sec_slot(w))   // [BPDMASK] same terms as the flag
    val s1_sec_pc   = Mux(s1_sec_slot.reduce(_||_),
      PriorityMux(s1_sec_slot, (0 until bankWidth).map(w => s1_ift_spc(s1_hit_ways(w)))),
      Mux(s1_evf_hit, s1_ift_evf(inflPcBitsCF - 1, 0), s1_ift_eva(inflPcBitsCF - 1, 0)))
    io.f3_btb_sec_disp := RegNext(RegNext(s1_evf_hit))
    io.f3_btb_atk_disp := RegNext(RegNext(s1_eva_hit))
    io.f3_btb_sec_pc   := RegNext(RegNext(s1_sec_pc))

    // ---- update side (s0 read, s1 write) ----
    val s0_update_idx_masked = s0_update_idx & (cf_btb_active_sets - 1.U)
    val s1_upd_tagh   = VecInit((0 until nWays).map(w =>
                          cfFwdRead(ift_btb_tagh(w), tagh_we(w), tagh_wa, tagh_wd, s0_update_idx_masked, s0_update_valid)))
    val s1_upd_alloc  = s1_update.valid && s1_update.bits.is_commit_update && s1_update_wmeta_mask =/= 0.U && !doing_reset
    val s1_upd_new8   = (s1_update_idx >> log2Ceil(nSets))(tag8W - 1, 0)
    val s1_upd_old    = s1_upd_tagh(s1_update_meta.write_way)
    val s1_upd_pc12   = (s1_update.bits.pc + (s1_update_cfi_idx << 1))(inflPcBitsCF + 1, 2)
    val s1_upd_sec    = s1_update.bits.cf_is_secret.getOrElse(false.B)
    val s1_upd_atk    = s1_update.bits.cf_domain_id.getOrElse(0.U(1.W)) === 1.U
    val s1_any_disp   = s1_upd_alloc && s1_upd_old(tag8W) && s1_upd_old(tag8W - 1, 0) =/= s1_upd_new8
    tagh_wa := Mux(doing_reset, reset_idx, s1_update_idx_masked)
    tagh_wd := Mux(doing_reset, 0.U((tag8W + 1).W), Cat(true.B, s1_upd_new8))
    for (w <- 0 until nWays) {
      val wsel = s1_update_meta.write_way === w.U || (nWays == 1).B
      tagh_we(w) := doing_reset || (s1_upd_alloc && wsel)
      when (tagh_we(w)) {
        ift_btb_tagh(w).write(tagh_wa, tagh_wd)
      }
      when (s1_upd_alloc && wsel && s1_upd_sec) {
        ift_btb_spc(w).write(s1_update_idx_masked, s1_upd_pc12)
      }
    }
    // [BTBATK] record writes: reset sweep (v=0), else a displacement of the record's own kind
    // (v=1, rewrite) or a clean displacement (v=0, clear).  No lookup write.
    // [BTBSAMETAG] ruling 146 (iii): any commit allocation also clears (v=0) a record of either
    // kind that names the newly allocated tag.  Update-port reads at the update index, same
    // index/enable as tagh.
    val s1_upd_evf   = cfFwdRead(ift_btb_evf, evf_we, rec_wa, evf_wd, s0_update_idx_masked, s0_update_valid)
    val s1_upd_eva   = cfFwdRead(ift_btb_eva, eva_we, rec_wa, eva_wd, s0_update_idx_masked, s0_update_valid)
    def recNames(r: UInt): Bool = r(recW - 1) && r(recW - 2, inflPcBitsCF) === s1_upd_new8
    val evf_same     = s1_upd_alloc && recNames(s1_upd_evf)
    val eva_same     = s1_upd_alloc && recNames(s1_upd_eva)
    val rec_body = Cat(s1_upd_old(tag8W - 1, 0), s1_upd_pc12)
    rec_wa := Mux(doing_reset, reset_idx, s1_update_idx_masked)
    evf_we := doing_reset || (s1_any_disp && (s1_upd_sec || !s1_upd_atk)) || evf_same
    eva_we := doing_reset || (s1_any_disp && (s1_upd_atk || !s1_upd_sec)) || eva_same
    evf_wd := Mux(doing_reset, 0.U(recW.W), Cat(s1_any_disp && s1_upd_sec, rec_body))
    eva_wd := Mux(doing_reset, 0.U(recW.W), Cat(s1_any_disp && s1_upd_atk, rec_body))
    when (evf_we) { ift_btb_evf.write(rec_wa, evf_wd) }
    when (eva_we) { ift_btb_eva.write(rec_wa, eva_wd) }
  }

}

