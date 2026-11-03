package boom.v3.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.v3.common._
import boom.v3.util.{BoomCoreStringPrefix, WrapInc}
import scala.math.min



class BIMMeta(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val bims  = Vec(bankWidth, UInt(2.W))
  // [SPECBPU-REP] (labels "Restore old label"): the fetch-time {domain, secret} of each slot of the row (IFT only)
  val lbls  = if (ENABLE_IFT) Some(Vec(bankWidth, UInt(2.W))) else None
}

case class BoomBIMParams(
  nSets: Int = 2048
)

class BIMBranchPredictorBank(params: BoomBIMParams = BoomBIMParams())(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  override val nSets = params.nSets
  // [SPECBPU] (owner 2026-11-02, RULES C 'Speculative BPU follow-ups' O1 "(a) BIM+uBTB too"): trained once per
  // resolved CFI at resolution (wrong path included) from io.spec_update; no commit training (predictor.scala
  // cf_upd_bus).
  override def trainsAtResolution: Boolean = true

  require(isPow2(nSets))

  val nWrBypassEntries = 2

  def bimWrite(v: UInt, taken: Bool): UInt = {
    val old_bim_sat_taken  = v === 3.U
    val old_bim_sat_ntaken = v === 0.U
    Mux(old_bim_sat_taken  &&  taken, 3.U,
      Mux(old_bim_sat_ntaken && !taken, 0.U,
      Mux(taken, v + 1.U, v - 1.U)))
  }
  val s2_meta           = Wire(new BIMMeta)
  override val metaSz   = s2_meta.asUInt.getWidth

  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nSets).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nSets-1).U) { doing_reset := false.B }


  val data  = SyncReadMem(nSets, Vec(bankWidth, UInt(2.W)))
  // O4 (corefuzzing): BIM had NO domain or secret tracking at all, so a bimodal
  // counter trained by the attacker (or by a secret-dependent branch) was invisible.
  // Same geometry, index and enable as `data` so provenance is exact -- BIM is
  // PC-indexed with no tags, so one entry corresponds to one PC slot.
  // OPT (audit rank 3): domain and secret were two separate single-bit SyncReadMems.
  // Both share index, write mask, reset behaviour and read enable, so packing them into
  // one {domain, secret} word is exactly equivalent and halves the memory instances and
  // their address-decode/port logic.  Same change as tage.scala's table_shadow.
  val data_shadow = SyncReadMem(nSets, Vec(bankWidth, UInt(2.W)))

  val mems = Seq(("bim", nSets, bankWidth * 2))

  val s2_req_rdata    = RegNext(data.read(s0_idx   , s0_valid))
  // [R124GATE3] patch 28 (B51): data_shadow is read/written and the domain/mismatch flops exist only with
  // ENABLE_IFT; IFT off the memory has no ports (removed) and the mismatch outputs are 0.
  val s2_req_rshadow  = if (ENABLE_IFT) RegNext(data_shadow.read(s0_idx, s0_valid))
                        else VecInit(Seq.fill(bankWidth)(0.U(2.W)))
  val s2_req_rdomain  = VecInit(s2_req_rshadow.map(_(1,1)))   // UInt(1.W), compared to s2_fetch_domain
  val s2_req_rsecret  = VecInit(s2_req_rshadow.map(_(0)))     // Bool
  val s2_fetch_domain = if (ENABLE_IFT) RegNext(s1_domain) else 0.U   // [R124GATE3]

  val s2_resp         = Wire(Vec(bankWidth, Bool()))

  for (w <- 0 until bankWidth) {

    s2_resp(w)        := s2_valid && s2_req_rdata(w)(1) && !doing_reset
    s2_meta.bims(w)   := s2_req_rdata(w)
    s2_meta.lbls.foreach(_(w) := s2_req_rshadow(w))   // [SPECBPU-REP]
  }

  // O4: BIM is a direction predictor, so its influence is reported as BPD state.
  // The composer ORs f3_bpd_*_mismatch across all banks, so driving these here is
  // all that is needed -- no new influencer type and no fetch-bundle plumbing.
  // Qualified on a valid, post-reset prediction: BIM has no tags, so without this
  // every read of an idle entry would report a mismatch.
  val s2_bim_live = s2_valid && !doing_reset
  // [ATKDMMASK] lean58 A5 (gap inventory 2026-10-23): the attacker-side twin of [BPDMASK]: a slot below the fetch
  // PC, or a bank whose request is not valid, is not part of the packet; its domain label raises no edge either.
  io.f3_bpd_domain_mismatch := (if (ENABLE_IFT) RegNext(s2_bim_live &&
    (0 until bankWidth).map(w => (s2_req_rdomain(w) =/= s2_fetch_domain) && s2_mask(w)).reduce(_||_)) else false.B)   // [R124GATE3]
  // [BPDMASK] r170 A (DF-dt-sprop): a slot below the fetch PC, or a bank whose request is not
  // valid, is not part of the packet; its secret label raises no edge.
  io.f3_bpd_secret_mismatch := (if (ENABLE_IFT) RegNext(s2_bim_live &&
    (0 until bankWidth).map(w => s2_req_rsecret(w) && s2_mask(w)).reduce(_||_)) else false.B)


  val s1_update_wdata   = Wire(Vec(bankWidth, UInt(2.W)))
  val s1_update_wmask   = Wire(Vec(bankWidth, Bool()))
  val s1_update_meta    = s1_update.bits.meta.asTypeOf(new BIMMeta)
  val s1_update_index   = s1_update_idx

  val wrbypass_idxs = Reg(Vec(nWrBypassEntries, UInt(log2Ceil(nSets).W)))
  val wrbypass      = Reg(Vec(nWrBypassEntries, Vec(bankWidth, UInt(2.W))))
  // [SPECBPU-B1] (review spec-bpu-review-20261102 N6, the BIM twin of tage.scala B1): one valid bit per bypass entry
  // and slot.  The row bypass stored DontCare for the slots an update did not write; with one CFI per resolution
  // update the packet-mates of a branch would read that value.  A slot's bypass word is used only when this entry
  // wrote it.  Cost per bank: 2 x bankWidth FF (reset) and about 12 LUT; +1 AND on the update-side hit select
  // (s1 registers -> data write), none on the fetch path.
  val wrbypass_v    = RegInit(VecInit(Seq.fill(nWrBypassEntries)(VecInit(Seq.fill(bankWidth)(false.B)))))
  val wrbypass_lbl  = if (ENABLE_IFT) Some(Reg(Vec(nWrBypassEntries, Vec(bankWidth, UInt(2.W))))) else None   // [SPECBPU-REP]
  val wrbypass_enq_idx = RegInit(0.U(log2Ceil(nWrBypassEntries).W))

  val wrbypass_hits = VecInit((0 until nWrBypassEntries) map { i =>
    !doing_reset &&
    wrbypass_idxs(i) === s1_update_index(log2Ceil(nSets)-1,0)
  })
  val wrbypass_hit = wrbypass_hits.reduce(_||_)
  val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)

  // [SPECBPU-REP] (owner 2026-11-02 'Speculative BPU repair'; spec-bpu-plan-final C-BIM-4) local undo log, one word per
  // record id: the counter (and IFT label) the write of that resolution update overwrote (bypass word of the slot,
  // else fetch-time), written at s1 together with that write.  A repair (s1) writes them back through the same port
  // and updates the bypass like a write; a relabel ([SPECBPU-RLB]) rewrites only the slot's label.
  class BimUndo extends Bundle {
    val c = UInt(2.W)
    val l = if (ENABLE_IFT) Some(UInt(2.W)) else None
  }
  val cf_blog    = Mem(cfSpecLogN, new BimUndo)
  val s1_slot    = s1_update.bits.cfi_idx.bits
  val s1_rp      = s1_update.valid && s1_update.bits.is_repair_update && s1_update.bits.cfi_idx.valid
  val s1_rlb     = s1_rp && s1_update.bits.cf_relabel.getOrElse(false.B)
  val s1_rst     = s1_rp && !s1_rlb
  val s1_bu      = cf_blog.read(s1_update.bits.cf_log_id)
  val s1_byp_use = VecInit((0 until bankWidth).map(w => wrbypass_hit && wrbypass_v(wrbypass_hit_idx)(w)))
  val s1_old_c   = VecInit((0 until bankWidth).map(w =>
                     Mux(s1_byp_use(w), wrbypass(wrbypass_hit_idx)(w), s1_update_meta.bims(w))))
  val s1_nlbl    = Cat(s1_update.bits.cf_domain_id.getOrElse(0.U(1.W)), s1_update.bits.cf_is_secret.getOrElse(false.B))
  val s1_wlbl    = Mux(s1_rst, s1_bu.l.getOrElse(0.U(2.W)), s1_nlbl)       // label written: train / restore / relabel
  when (!doing_reset && s1_update.valid && s1_update.bits.is_commit_update && s1_update.bits.cfi_idx.valid) {
    val wd = Wire(new BimUndo)
    wd.c := s1_old_c(s1_slot)
    wd.l.foreach(_ := Mux(s1_byp_use(s1_slot), wrbypass_lbl.get(wrbypass_hit_idx)(s1_slot),
                          s1_update_meta.lbls.get(s1_slot)))
    cf_blog.write(s1_update.bits.cf_log_id, wd)
  }



  for (w <- 0 until bankWidth) {
    s1_update_wmask(w)         := false.B
    s1_update_wdata(w)         := DontCare

    val update_pc = s1_update.bits.pc + (w << 1).U

    when (s1_update.bits.br_mask(w) ||
      (s1_update.bits.cfi_idx.valid && s1_update.bits.cfi_idx.bits === w.U)) {
      val was_taken = (
        s1_update.bits.cfi_idx.valid &&
        (s1_update.bits.cfi_idx.bits === w.U) &&
        (
          (s1_update.bits.cfi_is_br && s1_update.bits.br_mask(w) && s1_update.bits.cfi_taken) ||
          s1_update.bits.cfi_is_jal
        )
      )
      val old_bim_value = Mux(wrbypass_hit && wrbypass_v(wrbypass_hit_idx)(w),   // [SPECBPU-B1]
                              wrbypass(wrbypass_hit_idx)(w), s1_update_meta.bims(w))

      s1_update_wmask(w)     := true.B

      s1_update_wdata(w)     := bimWrite(old_bim_value, was_taken)
    }
    // [SPECBPU-REP] a repair / relabel writes exactly the logged slot (restore: its old counter); last connect
    when (s1_rp) {
      s1_update_wmask(w) := s1_slot === w.U
      s1_update_wdata(w) := s1_bu.c
    }


  }

  // [SPECBPU-REP] a restore writes the logged counter and label; [SPECBPU-RLB] a relabel writes only the label
  when (doing_reset || (s1_update.valid && s1_update.bits.is_commit_update) || s1_rst || s1_rlb) {
    when (doing_reset || !s1_rlb) {
    data.write(
      Mux(doing_reset, reset_idx, s1_update_index),
      Mux(doing_reset, VecInit(Seq.fill(bankWidth) { 2.U }), s1_update_wdata),
      Mux(doing_reset, (~(0.U(bankWidth.W))), s1_update_wmask.asUInt).asBools
    )
    }
    // O4: shadows written with the VALUE (not monotone), so a later non-secret /
    // other-domain update to the same entry UNTAINTS it -- matching the tage/btb
    // untaint semantics: when the flow breaks, the tag clears.
    if (ENABLE_IFT) data_shadow.write(   // [R124GATE3]
      Mux(doing_reset, reset_idx, s1_update_index),
      Mux(doing_reset, VecInit(Seq.fill(bankWidth)(0.U(2.W))),
                       VecInit(Seq.fill(bankWidth)(s1_wlbl))),   // [SPECBPU-REP] train / restore / relabel label
      Mux(doing_reset, (~(0.U(bankWidth.W))), s1_update_wmask.asUInt).asBools
    )
  }
  when (s1_update_wmask.reduce(_||_) && ((s1_update.valid && s1_update.bits.is_commit_update) || s1_rst)) {   // [SPECBPU-REP]
    when (wrbypass_hit) {
      // [SPECBPU-B1] only the written slots change; the others keep their word and valid bit
      for (w <- 0 until bankWidth) {
        when (s1_update_wmask(w)) {
          wrbypass  (wrbypass_hit_idx)(w) := s1_update_wdata(w)
          wrbypass_v(wrbypass_hit_idx)(w) := true.B
          wrbypass_lbl.foreach(_(wrbypass_hit_idx)(w) := s1_wlbl)   // [SPECBPU-REP]
        }
      }
    } .otherwise {
      wrbypass(wrbypass_enq_idx)      := s1_update_wdata
      wrbypass_v(wrbypass_enq_idx)    := s1_update_wmask            // [SPECBPU-B1] valid = the written slots only
      wrbypass_lbl.foreach(_(wrbypass_enq_idx) := VecInit(Seq.fill(bankWidth)(s1_wlbl)))   // [SPECBPU-REP]
      wrbypass_idxs(wrbypass_enq_idx) := s1_update_index
      wrbypass_enq_idx := WrapInc(wrbypass_enq_idx, nWrBypassEntries)
    }
  }
  if (CF_SPEC_RELABEL) {   // [SPECBPU-RLB] a relabel rewrites the label of a bypass word it covers
    when (s1_rlb && wrbypass_hit) {
      for (w <- 0 until bankWidth) {
        when (s1_update_wmask(w) && wrbypass_v(wrbypass_hit_idx)(w)) { wrbypass_lbl.get(wrbypass_hit_idx)(w) := s1_nlbl }
      }
    }
  }

  // [SINKFACTS86] patch 86 (all-sinks study Q6): every BIM write (since [SPECBPU]: the resolution update, one CFI
  // per update, wrong path included; it was the commit write), debug printf builds only (the
  // IftSquashMerge gate: IFT on, CF debug printfs on, no IFT bridge; IFT off / NoLog / FireSim elaborate nothing).
  // pc = the bank-aligned update pc (bank = pc[3]); idx = the row; wm = slot write mask; meta = the FETCH-time
  // counters the update carries; byp / bypv = the 2-entry row write bypass hit and that row; bypm = the written slots
  // whose old value came from the bypass ([SPECBPU-B1]: old = bypv for a slot in bypm, else meta; bim.scala
  // old_bim_value); new = the written counters (unwritten
  // slots printed 0).  The host replays these writes to get the TRUE row before each write: used =/= true is a
  // fetch-vs-update interleaving event, i.e. exactly where a commit-order model (models.py BIM) diverges.
  // cyc: a debug-only 40-bit counter from reset (same reset as the FTQ d4_cyc_q and dcache cf_d1_cyc).
  if (ENABLE_IFT && ENABLE_CF_DEBUG_PRINTF && !ENABLE_IFT_BRIDGE) {
    val cf_sf_cyc = RegInit(0.U(40.W))
    cf_sf_cyc := cf_sf_cyc + 1.U
    when (!doing_reset && ((s1_update.valid && s1_update.bits.is_commit_update) || s1_rst) && s1_update_wmask.reduce(_||_)) {
      printf("[BIMW] cyc=%d pc=0x%x idx=0x%x wm=0x%x meta=0x%x byp=%d bypv=0x%x new=0x%x bypm=0x%x k=%d\n", cf_sf_cyc,
        s1_update.bits.pc, s1_update_index(log2Ceil(nSets)-1, 0), s1_update_wmask.asUInt, s1_update_meta.bims.asUInt,
        wrbypass_hit, wrbypass(wrbypass_hit_idx).asUInt,
        VecInit((0 until bankWidth).map(w => Mux(s1_update_wmask(w), s1_update_wdata(w), 0.U(2.W)))).asUInt,
        VecInit((0 until bankWidth).map(w => s1_update_wmask(w) && wrbypass_hit && wrbypass_v(wrbypass_hit_idx)(w))).asUInt,
        s1_rst)   // [SPECBPU-REP] k = 1: a repair (restore) write
    }
  }

  for (w <- 0 until bankWidth) {
    io.resp.f2(w).taken := s2_resp(w)
    io.resp.f3(w).taken := RegNext(io.resp.f2(w).taken)
  }
  io.f3_meta := RegNext(s2_meta.asUInt)
}
