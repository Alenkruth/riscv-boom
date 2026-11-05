package boom.v3.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.v3.common._
import boom.v3.util.{BoomCoreStringPrefix}


class ComposedBranchPredictorBank(implicit p: Parameters) extends BranchPredictorBank()(p)
{

  val (components, resp) = getBPDComponents(io.resp_in(0), p)
  io.resp := resp


  var metas = 0.U(1.W)
  var meta_sz = 0
  for (c <- components) {
    c.io.f0_valid  := io.f0_valid
    c.io.f0_pc     := io.f0_pc
    c.io.f0_mask   := io.f0_mask
    c.io.f1_ghist  := io.f1_ghist
    c.io.f1_lhist  := io.f1_lhist
    c.io.f3_fire   := io.f3_fire
    if (c.metaSz > 0) {
      metas = (metas << c.metaSz) | c.io.f3_meta(c.metaSz-1,0)
    }

    meta_sz = meta_sz + c.metaSz

    // addition for the fuzzycore project - AK
    c.io.cf_bpd_tage_to_gshare := io.cf_bpd_tage_to_gshare
    // corefuzzing: runtime BTB/TAGE reconfig
    c.io.cf_btb_set_idx    := io.cf_btb_set_idx
    c.io.cf_btb_way_idx    := io.cf_btb_way_idx
    c.io.cf_tage_count_idx := io.cf_tage_count_idx
    c.io.cf_btb_quiesce    := io.cf_btb_quiesce

    // corefuzzing: wire domain to each component
    c.io.f0_domain_id := io.f0_domain_id
    c.io.f1_domain_id.foreach(_ := io.f1_domain_id.get)   // [F4S1RNG] lean58

  }
  require(meta_sz < bpdMaxMetaLength)
  io.f3_meta := metas
  // corefuzzing: aggregate domain and secret mismatches from all components
  io.f3_bpd_domain_mismatch := components.map(_.io.f3_bpd_domain_mismatch).reduce(_||_)
  io.f3_btb_domain_mismatch := components.map(_.io.f3_btb_domain_mismatch).reduce(_||_)
  io.f3_bpd_secret_mismatch := components.map(_.io.f3_bpd_secret_mismatch).reduce(_||_)
  io.f3_btb_secret_mismatch := components.map(_.io.f3_btb_secret_mismatch).reduce(_||_)
  // [ICPREDF3-TAGE] the bpd part that is not f3-only (BIM): what an s1 / s2 request label may carry (frontend.scala)
  io.f3_bpd_secret_early.foreach(_ := components.filterNot(_.cfBpdF3Only).map(_.io.f3_bpd_secret_mismatch).foldLeft(false.B)(_||_))
  io.f3_bpd_domain_early.foreach(_ := components.filterNot(_.cfBpdF3Only).map(_.io.f3_bpd_domain_mismatch).foldLeft(false.B)(_||_))
  // [BTBDISP] step 16: OR of the components (BTB, uBTB; the rest drive false); the PC of the
  // first component with a BTB secret edge.  All inputs are f3 registers.
  // [BTBPICK] ruling 146 (v): two-tier pick (predictor.scala CfBtbPcPick) over the BTB-type
  // components (cfDrivesBtbPc: BTB, uBTB; BTB > uBTB): the first with a SECRET BTB edge (hit or
  // secret record), else the first with any BTB edge (attacker record), else the last (default;
  // the PC is read only when fetch_flags bit 7 or 8 fires).  Inside a component secret PC >
  // attacker PC.
  io.f3_btb_sec_disp := components.map(_.io.f3_btb_sec_disp).reduce(_||_)
  io.f3_btb_atk_disp := components.map(_.io.f3_btb_atk_disp).reduce(_||_)
  private val cf_btb_comps = components.filter(_.cfDrivesBtbPc)
  private val cf_comp_sec  = cf_btb_comps.map(c => c.io.f3_btb_secret_mismatch || c.io.f3_btb_sec_disp)
  io.f3_btb_sec_pc   := (if (cf_btb_comps.isEmpty) 0.U(12.W) else CfBtbPcPick(cf_comp_sec,
    cf_btb_comps.zip(cf_comp_sec).map { case (c, s) => s || c.io.f3_btb_atk_disp },
    cf_btb_comps.map(_.io.f3_btb_sec_pc)))


  var update_meta = io.update.bits.meta
  for (c <- components.reverse) {
    c.io.update := io.update
    c.io.update.bits.meta := update_meta
    // corefuzzing: pass through the fetch domain and secret flag for shadow updates
    c.io.update.bits.cf_domain_id.foreach(_ := io.update.bits.cf_domain_id.getOrElse(0.U(1.W)))
    c.io.update.bits.cf_is_secret.foreach(_ := io.update.bits.cf_is_secret.getOrElse(false.B))
    update_meta = update_meta >> c.metaSz
  }
  // [SPECBPU] (owner 2026-11-02, RULES C 'Speculative TAGE/BTB updates'): the resolution-time bus, with the same
  // per-component meta shift (the FTQ rebuilds the full-width meta from its stored slice, zeros elsewhere).
  var spec_update_meta = io.spec_update.bits.meta
  for (c <- components.reverse) {
    c.io.spec_update := io.spec_update
    c.io.spec_update.bits.meta := spec_update_meta
    spec_update_meta = spec_update_meta >> c.metaSz
  }
  // [SPECBPU] the meta slice of the resolution-trained components in the packed f3_meta (component k sits at the
  // sum of the metaSz of the components after it in `components`, the order of the shift above).  They must be
  // contiguous: in WithTAGELBPD (loop, tage, btb, ubtb, bim) bim, ubtb, btb, tage are bits [0, 73) and the loop
  // (commit bus, inert) sits above them; WithBoom2BPD trains all three of its components.
  private val cf_spec_meta: (Int, Int) = {
    val offs = components.reverse.scanLeft(0)(_ + _.metaSz).zip(components.reverse)
    val sel  = offs.filter { case (_, c) => c.trainsAtResolution && c.metaSz > 0 }
    if (sel.isEmpty) (0, 0) else {
      val lo = sel.map(_._1).min
      val hi = sel.map { case (o, c) => o + c.metaSz }.max
      require(sel.map(_._2.metaSz).sum == hi - lo,
        "[SPECBPU] the resolution-trained predictor components must be adjacent in the meta layout")
      (lo, hi - lo)
    }
  }
  override def specMetaLo: Int = cf_spec_meta._1
  override def specMetaW: Int  = cf_spec_meta._2

  val mems = components.map(_.mems).flatten
  // [V3TAP-PRED] SP-19: each component drives only its own IftPredBankTap fields (0 elsewhere), so the bank's tap is
  // the OR of its components (constant zeros fold away).  Record/STATE builds only.
  io.cf_st.foreach(_ := components.map(_.io.cf_st.get.asUInt).reduce(_ | _).asTypeOf(new IftPredBankTap))

}
