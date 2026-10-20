package boom.v3.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.v3.common._
import boom.v3.util.{BoomCoreStringPrefix, WrapInc}

import scala.math.min

case class BoomFAMicroBTBParams(
  nWays: Int = 16,
  offsetSz: Int = 13
)


class FAMicroBTBBranchPredictorBank(params: BoomFAMicroBTBParams = BoomFAMicroBTBParams())(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  override def cfDrivesBtbPc: Boolean = ENABLE_IFT   // [BTBATK] composer PC select (predictor.scala)
  override val nWays         = params.nWays
  val tagSz         = vaddrBitsExtended - log2Ceil(fetchWidth) - 1
  val offsetSz      = params.offsetSz
  val nWrBypassEntries = 2

  def bimWrite(v: UInt, taken: Bool): UInt = {
    val old_bim_sat_taken  = v === 3.U
    val old_bim_sat_ntaken = v === 0.U
    Mux(old_bim_sat_taken  &&  taken, 3.U,
      Mux(old_bim_sat_ntaken && !taken, 0.U,
      Mux(taken, v + 1.U, v - 1.U)))
  }

  require(isPow2(nWays))

  class MicroBTBEntry extends Bundle {
    val offset   = SInt(offsetSz.W)
  }

  class MicroBTBMeta extends Bundle {
    val is_br = Bool()
    val tag   = UInt(tagSz.W)
    val ctr   = UInt(2.W)
    // O4 (corefuzzing): the FA micro-BTB had NO domain or secret tracking, so an
    // attacker-written (or secret-dependent) micro-BTB target was invisible.  Stored
    // per way/slot alongside the tag -- this bank is PC-tagged, so provenance is exact.
    // [R124GATE3] patch 28 (B51): IFT-only, absent without ENABLE_IFT (same position when present).
    val domain = if (ENABLE_IFT) Some(UInt(1.W)) else None
    val secret = if (ENABLE_IFT) Some(Bool()) else None
  }

  class MicroBTBPredictMeta extends Bundle {
    val hits  = Vec(bankWidth, Bool())
    val write_way = UInt(log2Ceil(nWays).W)
  }

  val s1_meta = Wire(new MicroBTBPredictMeta)
  override val metaSz = s1_meta.asUInt.getWidth


  val meta     = RegInit((0.U).asTypeOf(Vec(nWays, Vec(bankWidth, new MicroBTBMeta))))
  val btb      = Reg(Vec(nWays, Vec(bankWidth, new MicroBTBEntry)))

  val mems = Nil

  val s1_req_tag   = s1_idx


  val s1_resp   = Wire(Vec(bankWidth, Valid(UInt(vaddrBitsExtended.W))))
  val s1_taken  = Wire(Vec(bankWidth, Bool()))
  val s1_is_br  = Wire(Vec(bankWidth, Bool()))
  val s1_is_jal = Wire(Vec(bankWidth, Bool()))

  val s1_hit_ohs = VecInit((0 until bankWidth) map { i =>
    VecInit((0 until nWays) map { w =>
      meta(w)(i).tag === s1_req_tag(tagSz-1,0)
    })
  })
  val s1_hits     = s1_hit_ohs.map { oh => oh.reduce(_||_) }
  val s1_hit_ways = s1_hit_ohs.map { oh => PriorityEncoder(oh) }
  val s1_ubtb_domain_mismatch = Wire(Vec(bankWidth, Bool()))
  val s1_ubtb_secret_mismatch = Wire(Vec(bankWidth, Bool()))

  for (w <- 0 until bankWidth) {
    val entry_meta = meta(s1_hit_ways(w))(w)
    s1_resp(w).valid := s1_valid && s1_hits(w)
    s1_resp(w).bits  := (s1_pc.asSInt + (w << 1).S + btb(s1_hit_ways(w))(w).offset).asUInt
    s1_is_br(w)      := s1_resp(w).valid &&  entry_meta.is_br
    s1_is_jal(w)     := s1_resp(w).valid && !entry_meta.is_br
    s1_taken(w)      := !entry_meta.is_br || entry_meta.ctr(1)

    s1_meta.hits(w)     := s1_hits(w)
    // O4: qualified on s1_hits -- this bank is tagged, so only a real hit means the
    // prediction actually came from that entry.
    s1_ubtb_domain_mismatch(w) := s1_hits(w) && (entry_meta.domain.getOrElse(0.U(1.W)) =/= s1_domain)
 // [BTBPROBE RETIRED 2026-09-10] purpose discharged: proved the channel WORKS
    // // [BTBPROBE 2026-09-10] TEMPORARY -- the hit half.  Printed on EVERY real hit, not only
    // // on mismatch: "mismatch never fired" and "the uBTB never hit at all" look identical
    // // downstream, and they have completely different fixes.  edom/fdom together say which.
    // if (ENABLE_CF_DEBUG_PRINTF) {
      // // Bounded to hits where EITHER side is the attacker domain.  Printing on every hit
      // // would flood (the uBTB hits on most fetches; spectre-v2 already emits 133MB), while
      // // this set is exactly the one that can produce a mismatch, and its absence is itself
      // // the answer: no lines at all => the uBTB never hits under a cross-domain condition.
      // when (s1_valid && s1_hits(w) && (s1_domain === 1.U || entry_meta.domain === 1.U)) {
        // printf("\n[UBTBH] w=%d edom=%d fdom=%d mism=%d esec=%d\n",
          // w.U, entry_meta.domain, s1_domain,
          // s1_ubtb_domain_mismatch(w), entry_meta.secret)
      // }
    // }
    // [BPDMASK] r170 A (DF-dt-sprop): a slot below the fetch PC, or a bank whose request is not
    // valid, is not part of the packet; its secret label raises no edge.
    s1_ubtb_secret_mismatch(w) := s1_valid && s1_hits(w) && entry_meta.secret.getOrElse(false.B) && s1_mask(w)
  }
  val alloc_way = {
    val r_metas = Cat(VecInit(meta.map(e => VecInit(e.map(_.tag)))).asUInt, s1_idx(tagSz-1,0))
    val l = log2Ceil(nWays)
    val nChunks = (r_metas.getWidth + l - 1) / l
    val chunks = (0 until nChunks) map { i =>
      r_metas(min((i+1)*l, r_metas.getWidth)-1, i*l)
    }
    chunks.reduce(_^_)
  }
  // O4: report as BTB state (composer ORs f3_btb_*_mismatch across all banks).
  // Two RegNexts to land at f3, matching btb.scala's timing.
  io.f3_btb_domain_mismatch := (if (ENABLE_IFT) RegNext(RegNext(s1_ubtb_domain_mismatch.reduce(_||_))) else false.B)   // [R124GATE3]
  // [R124GATE2] patch 27 (r124/B51): the two f3 flops only with ENABLE_IFT.
  io.f3_btb_secret_mismatch := (if (ENABLE_IFT) RegNext(RegNext(s1_ubtb_secret_mismatch.reduce(_||_))) else false.B)

  s1_meta.write_way := Mux(s1_hits.reduce(_||_),
    PriorityEncoder(s1_hit_ohs.map(_.asUInt).reduce(_|_)),
    alloc_way)

  for (w <- 0 until bankWidth) {
    io.resp.f1(w).predicted_pc := s1_resp(w)
    io.resp.f1(w).is_br        := s1_is_br(w)
    io.resp.f1(w).is_jal       := s1_is_jal(w)
    io.resp.f1(w).taken        := s1_taken(w)

    io.resp.f2(w) := RegNext(io.resp.f1(w))
    io.resp.f3(w) := RegNext(io.resp.f2(w))
  }
  io.f3_meta := RegNext(RegNext(s1_meta.asUInt))

  val s1_update_cfi_idx = s1_update.bits.cfi_idx.bits
  val s1_update_meta    = s1_update.bits.meta.asTypeOf(new MicroBTBPredictMeta)
  val s1_update_write_way = s1_update_meta.write_way

  val max_offset_value = (~(0.U)((offsetSz-1).W)).asSInt
  val min_offset_value = Cat(1.B, (0.U)((offsetSz-1).W)).asSInt
  val new_offset_value = (s1_update.bits.target.asSInt -
    (s1_update.bits.pc + (s1_update.bits.cfi_idx.bits << 1)).asSInt)

  val s1_update_wbtb_data     = Wire(new MicroBTBEntry)
  s1_update_wbtb_data.offset := new_offset_value
  val s1_update_wbtb_mask = (UIntToOH(s1_update_cfi_idx) &
    Fill(bankWidth, s1_update.bits.cfi_idx.valid && s1_update.valid && s1_update.bits.cfi_taken && s1_update.bits.is_commit_update))

  val s1_update_wmeta_mask = ((s1_update_wbtb_mask | s1_update.bits.br_mask) &
    Fill(bankWidth, s1_update.valid && s1_update.bits.is_commit_update))

  // Write the BTB with the target
  when (s1_update.valid && s1_update.bits.cfi_taken && s1_update.bits.cfi_idx.valid && s1_update.bits.is_commit_update) {
    btb(s1_update_write_way)(s1_update_cfi_idx).offset := new_offset_value
  }

  // Write the meta
  for (w <- 0 until bankWidth) {
    when (s1_update.valid && s1_update.bits.is_commit_update &&
      (s1_update.bits.br_mask(w) ||
        (s1_update_cfi_idx === w.U && s1_update.bits.cfi_taken && s1_update.bits.cfi_idx.valid))) {
      val was_taken = (s1_update_cfi_idx === w.U && s1_update.bits.cfi_idx.valid &&
        (s1_update.bits.cfi_taken || s1_update.bits.cfi_is_jal))

      meta(s1_update_write_way)(w).is_br := s1_update.bits.br_mask(w)
      meta(s1_update_write_way)(w).tag   := s1_update_idx
      meta(s1_update_write_way)(w).domain.foreach(_ := s1_update.bits.cf_domain_id.getOrElse(0.U(1.W)))
   // [BTBPROBE RETIRED 2026-09-10] purpose discharged: proved the channel WORKS
      // // [BTBPROBE 2026-09-10] TEMPORARY -- ty=10 BTB_STATE produced 0 edges on spectre-v2
      // // (a matched branch-target-injection workload with 459,321 domain=1 records and
      // // s_tx=100).  Slot pressure and the any_is_victim gate are already RULED OUT by data
      // // (OVF=0 on 99.7% of records; ty=11 RAS uses the same gate and fired 40,340 times), so
      // // either no entry is ever WRITTEN with domain=1, or no such entry is later HIT under
      // // the other domain.  This probe answers the write half.
      // if (ENABLE_CF_DEBUG_PRINTF) {
        // printf("\n[UBTBW] way=%d w=%d dom=%d sec=%d tag=0x%x\n",
          // s1_update_write_way, w.U, s1_update.bits.cf_domain_id,
          // s1_update.bits.cf_is_secret, s1_update_idx)
      // }
      meta(s1_update_write_way)(w).secret.foreach(_ := s1_update.bits.cf_is_secret.getOrElse(false.B))
      meta(s1_update_write_way)(w).ctr   := Mux(!s1_update_meta.hits(w),
        Mux(was_taken, 3.U, 0.U),
        bimWrite(meta(s1_update_write_way)(w).ctr, was_taken)
      )
    }
  }

  // [BTBDISP 2026-10-13, series-r step 16, owner ruling 110] uBTB displacement residue (the
  // fully associative twin of btb.scala's ift_btb_evf/eva).  [BTBATK] DF-btbatk: TWO records,
  // each {v, victim tag[7:0], pc[13:2]} (pc = the displacing allocator): the SECRET record
  // (evf_*) is rewritten by a displacing allocation whose update is secret (cf_is_secret), the
  // ATTACKER record (eva_*) by one whose update is attacker-domain (cf_domain_id == 1); a secret
  // attacker one rewrites both; one that is neither clears both; a secret-only one leaves the
  // attacker record alone and vice versa.  A displacing allocation = a commit update whose
  // written slot missed at prediction time over a non-empty entry holding another tag.
  // [BTBEVFLAST] DF-m60 (r140, the D$ [EVFLAST] rule): a fetch never consumes either record.
  // The uBTB is one set, so any displacement anywhere replaces / clears the record of its kind.
  // [BTBSAMETAG] ruling 146 (iii), C1-(c), latest event wins: every commit allocation (upd_any:
  // displacing, empty-entry or same-tag, any kind) clears a record of either kind whose victim
  // tag[7:0] equals the newly allocated tag[7:0]; an own-kind displacement in the same cycle
  // rewrites the record instead (it never names the new tag: old tag =/= new tag).  This also
  // ends the DF-m60 cold-entry over-taint (an allocation of the victim tag into an empty entry is
  // not a displacement, but it now clears the record).  ubtb_spc (per way) = PC of the last
  // secret trainer, so the existing hit edge carries the PC.  Edge, PC priority and merged-edge
  // rule: btb.scala [BTBATK].  Approximations: one victim per kind (newest wins), 8-bit tag,
  // tag[7:0] == 0 treated as empty, PC per way not per slot.  FPGA: 16x12 + 2x21 FF; update
  // side 8 x 64:1 mux (registered meta) + 8-b compare into flop enables (one shared compare,
  // a 2-input kind decode merged into the enables) and, in parallel, two 8-b same-tag compares
  // of the record flops against the registered update index (0 levels added); fetch side two
  // 8-b compares and a 16:1 one-hot PC select per slot beside the existing hit muxes, a 3:1 PC
  // select into f2 flops.
  if (ENABLE_IFT) {
    val tag8W    = 8
    val cfPcBits = 12                         // inflPcBitsCF (PC[13:2])
    val ubtb_spc = Reg(Vec(nWays, UInt(cfPcBits.W)))
    val evf_v    = RegInit(false.B)
    val evf_tag  = Reg(UInt(tag8W.W))
    val evf_pc   = Reg(UInt(cfPcBits.W))
    val eva_v    = RegInit(false.B)           // [BTBATK] attacker record
    val eva_tag  = Reg(UInt(tag8W.W))
    val eva_pc   = Reg(UInt(cfPcBits.W))

    // fetch side (s1)
    val s1_evf_hit = s1_valid && evf_v && evf_tag === s1_req_tag(tag8W - 1, 0)
    val s1_eva_hit = s1_valid && eva_v && eva_tag === s1_req_tag(tag8W - 1, 0)
    val s1_sec_any = s1_ubtb_secret_mismatch.reduce(_||_)
    val s1_sec_pc  = Mux(s1_sec_any,
      PriorityMux(s1_ubtb_secret_mismatch, (0 until bankWidth).map(w => Mux1H(s1_hit_ohs(w), ubtb_spc))),
      Mux(s1_evf_hit, evf_pc, eva_pc))
    io.f3_btb_sec_disp := RegNext(RegNext(s1_evf_hit))
    io.f3_btb_atk_disp := RegNext(RegNext(s1_eva_hit))
    io.f3_btb_sec_pc   := RegNext(RegNext(s1_sec_pc))

    // update side (s1): the slots the meta loop above writes
    val upd_wslot = (0 until bankWidth).map(w => s1_update.bits.br_mask(w) ||
      (s1_update_cfi_idx === w.U && s1_update.bits.cfi_taken && s1_update.bits.cfi_idx.valid))
    val upd_any   = s1_update.valid && s1_update.bits.is_commit_update && upd_wslot.reduce(_||_)
    val upd_is_sec = s1_update.bits.cf_is_secret.getOrElse(false.B)
    val upd_is_atk = s1_update.bits.cf_domain_id.getOrElse(0.U(1.W)) === 1.U
    val upd_sec   = upd_any && upd_is_sec
    val upd_pc12  = (s1_update.bits.pc + (s1_update_cfi_idx << 1))(cfPcBits + 1, 2)
    val old_tag8  = PriorityMux(upd_wslot, (0 until bankWidth).map(w => meta(s1_update_write_way)(w).tag(tag8W - 1, 0)))
    val old_miss  = PriorityMux(upd_wslot, (0 until bankWidth).map(w => !s1_update_meta.hits(w)))
    val new_tag8  = s1_update_idx(tag8W - 1, 0)
    val any_disp  = upd_any && old_miss && old_tag8 =/= 0.U && old_tag8 =/= new_tag8
    when (upd_sec) { ubtb_spc(s1_update_write_way) := upd_pc12 }
    // [BTBSAMETAG] same-tag clear first; an own-kind displacement below overrides it (last connect)
    when (upd_any && evf_v && evf_tag === new_tag8) { evf_v := false.B }
    when (upd_any && eva_v && eva_tag === new_tag8) { eva_v := false.B }
    when (any_disp && (upd_is_sec || !upd_is_atk)) { evf_v := upd_is_sec }
    when (any_disp && upd_is_sec)                  { evf_tag := old_tag8; evf_pc := upd_pc12 }
    when (any_disp && (upd_is_atk || !upd_is_sec)) { eva_v := upd_is_atk }
    when (any_disp && upd_is_atk)                  { eva_tag := old_tag8; eva_pc := upd_pc12 }
  }

}

