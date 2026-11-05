package boom.v3.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.v3.common._
import boom.v3.util.{BoomCoreStringPrefix}

import scala.math.min

case class BoomLoopPredictorParams(
  nWays: Int = 4,
  threshold: Int = 7
)

class LoopBranchPredictorBank(implicit p: Parameters) extends BranchPredictorBank()(p)
{
  override def cfBpdF3Only: Boolean = true   // [ICPREDF3-TAGE] f3-only bpd mismatch (predictor.scala)
  val tagSz = 10
  override val nSets = 16



  class LoopMeta extends Bundle {
    val s_cnt   = UInt(10.W)
  }

  class LoopEntry extends Bundle {
    val tag   = UInt(tagSz.W)
    val conf  = UInt(3.W)
    val age   = UInt(3.W)
    val p_cnt = UInt(10.W)
    val s_cnt = UInt(10.W)
    // O4 (corefuzzing): the loop predictor had NO domain or secret tracking.  A
    // secret-dependent loop TRIP COUNT is a real channel -- the predicted exit
    // encodes it.  Stored per entry alongside the tag; this bank is PC-tagged, so
    // provenance is exact.
    // [R124GATE3] patch 28 (B51): IFT-only, absent without ENABLE_IFT (same position when present).
    val domain = if (ENABLE_IFT) Some(UInt(1.W)) else None
    val secret = if (ENABLE_IFT) Some(Bool()) else None
    // [LOOPRESET] lean58 A6 (gap inventory 2026-10-23, ledger 1023): set by the first update that writes the entry
    // (update_mispredict), cleared by the reset sweep.  A reset entry (tag 0, domain 0) has never been written, so
    // its domain is no one's state; without this bit an attacker fetch with pc[17:8] = 0 raised an empty ty9 edge.
    // IFT-only (None without ENABLE_IFT, so the stock entry is unchanged).  nSets x 1 FF per column.
    val written = if (ENABLE_IFT) Some(Bool()) else None
  }

  class LoopBranchPredictorColumn extends Module {



    val io = IO(new Bundle {
      val f2_req_valid = Input(Bool())
      val f2_req_idx  = Input(UInt())
      val f3_req_fire = Input(Bool())

      val f3_pred_in  = Input(Bool())
      val f3_pred     = Output(Bool())
      val f3_meta     = Output(new LoopMeta)

      val update_mispredict = Input(Bool())
      val update_repair     = Input(Bool())
      val update_idx   = Input(UInt())
      val update_resolve_dir = Input(Bool())
      val update_meta  = Input(new LoopMeta)
      // O4: provenance of the update, and the mismatch this column observes at f3
      val update_domain = Input(UInt(1.W))
      val update_secret = Input(Bool())
      val f3_req_domain = Input(UInt(1.W))
      val f3_domain_mismatch = Output(Bool())
      val f3_secret_mismatch = Output(Bool())
    })

    val doing_reset = RegInit(true.B)
    val reset_idx = RegInit(0.U(log2Ceil(nSets).W))
    reset_idx := reset_idx + doing_reset
    when (reset_idx === (nSets-1).U) { doing_reset := false.B }


    val entries = Reg(Vec(nSets, new LoopEntry))
    val f2_entry = WireInit(entries(io.f2_req_idx))
    when (io.update_repair && io.update_idx === io.f2_req_idx) {
      f2_entry.s_cnt := io.update_meta.s_cnt
    } .elsewhen (io.update_mispredict && io.update_idx === io.f2_req_idx) {
      f2_entry.s_cnt := 0.U
    }
    val f3_entry = RegNext(f2_entry)
    val f3_scnt  = Mux(io.update_repair && io.update_idx === RegNext(io.f2_req_idx),
      io.update_meta.s_cnt,
      f3_entry.s_cnt)
    val f3_tag   = RegNext(io.f2_req_idx(tagSz+log2Ceil(nSets)-1,log2Ceil(nSets)))
    // O4: qualified on the column's OWN tag match -- the same condition the prediction
    // path uses below -- so a mismatch is reported only when the prediction genuinely
    // came from this entry.
    // [LOOPRESET] the domain label is raised only by an entry an update has written (one more input on the flag LUT,
    // a flop: 0 levels).  The secret label needs no gate: a never-written entry has secret = 0 from the reset sweep.
    io.f3_domain_mismatch := (if (ENABLE_IFT) (f3_entry.tag === f3_tag) && f3_entry.written.get &&
                                              (f3_entry.domain.get =/= io.f3_req_domain)
                              else (f3_entry.tag === f3_tag) && (f3_entry.domain.getOrElse(0.U(1.W)) =/= io.f3_req_domain))
    io.f3_secret_mismatch := (f3_entry.tag === f3_tag) && f3_entry.secret.getOrElse(false.B)

    io.f3_pred := io.f3_pred_in
    io.f3_meta.s_cnt := f3_scnt

    when (f3_entry.tag === f3_tag) {
      when (f3_scnt === f3_entry.p_cnt && f3_entry.conf === 7.U) {
        io.f3_pred := !io.f3_pred_in
      }
    }


    val f4_fire  = RegNext(io.f3_req_fire)
    val f4_entry = RegNext(f3_entry)
    val f4_tag   = RegNext(f3_tag)
    val f4_scnt  = RegNext(f3_scnt)
    val f4_idx   = RegNext(RegNext(io.f2_req_idx))


    when (f4_fire) {
      when (f4_entry.tag === f4_tag) {
        when (f4_scnt === f4_entry.p_cnt && f4_entry.conf === 7.U) {
          entries(f4_idx).age   := 7.U
          entries(f4_idx).s_cnt := 0.U
        } .otherwise {
          entries(f4_idx).s_cnt := f4_scnt + 1.U
          entries(f4_idx).age   := Mux(f4_entry.age === 7.U, 7.U, f4_entry.age + 1.U)
        }
      }
    }


    val entry = entries(io.update_idx)
    val tag = io.update_idx(tagSz+log2Ceil(nSets)-1,log2Ceil(nSets))
    val tag_match = entry.tag === tag
    val ctr_match = entry.p_cnt === io.update_meta.s_cnt
    val wentry = WireInit(entry)

    when (io.update_mispredict && !doing_reset) {

      // Learned, tag match -> decrement confidence
      when (entry.conf === 7.U && tag_match) {
        wentry.s_cnt := 0.U
        wentry.conf  := 0.U

      // Learned, no tag match -> do nothing? Don't evict super-confident entries?
      } .elsewhen (entry.conf === 7.U && !tag_match) {

      // Confident, tag match, ctr_match -> increment confidence, reset counter
      } .elsewhen (entry.conf =/= 0.U && tag_match && ctr_match) {
        wentry.conf  := entry.conf + 1.U
        wentry.s_cnt := 0.U

      // Confident, tag match, no ctr match -> zero confidence, reset counter, set previous counter
      } .elsewhen (entry.conf =/= 0.U && tag_match && !ctr_match) {
        wentry.conf  := 0.U
        wentry.s_cnt := 0.U
        wentry.p_cnt := io.update_meta.s_cnt

      // Confident, no tag match, age is 0 -> replace this entry with our own, set our age high to avoid ping-pong
      } .elsewhen (entry.conf =/= 0.U && !tag_match && entry.age === 0.U) {
        wentry.tag   := tag
        wentry.conf  := 1.U
        wentry.s_cnt := 0.U
        wentry.p_cnt := io.update_meta.s_cnt

      // Confident, no tag match, age > 0 -> decrement age
      } .elsewhen (entry.conf =/= 0.U && !tag_match && entry.age =/= 0.U) {
        wentry.age := entry.age - 1.U

      // Unconfident, tag match, ctr match -> increment confidence
      } .elsewhen (entry.conf === 0.U && tag_match && ctr_match) {
        wentry.conf  := 1.U
        wentry.age   := 7.U
        wentry.s_cnt := 0.U

      // Unconfident, tag match, no ctr match -> set previous counter
      } .elsewhen (entry.conf === 0.U && tag_match && !ctr_match) {
        wentry.p_cnt := io.update_meta.s_cnt
        wentry.age   := 7.U
        wentry.s_cnt := 0.U

      // Unconfident, no tag match -> set previous counter and tag
      } .elsewhen (entry.conf === 0.U && !tag_match) {
        wentry.tag   := tag
        wentry.conf  := 1.U
        wentry.age   := 7.U
        wentry.s_cnt := 0.U
        wentry.p_cnt := io.update_meta.s_cnt
      }

      // [LOOPAGE] patch 69 (V14, owner 2026-10-19 16:3x): an update whose tag does not match and that does not
      // replace the entry (a learned entry: "do nothing"; a confident entry with age > 0: age - 1) leaves the
      // entry's tag / counters / confidence as they are, so it keeps its writer's domain (P5: a state entry takes
      // the domain of the update that writes it).  Every other branch (a tag match, or a replacement with the
      // new tag) takes the update's domain as before.  The secret shadow below keeps its rewrite (not in V14;
      // the same shape, listed as open).  About 2 LUT (a 2:1 on the 1-bit domain by terms the when-chain already
      // computes), 0 FF, 0 levels (it ends at the entries register).  IFT off: domain is None.
      val cf_no_owner = !tag_match && (entry.conf === 7.U || (entry.conf =/= 0.U && entry.age =/= 0.U))
      wentry.domain.foreach(_ := Mux(cf_no_owner, entry.domain.getOrElse(0.U(1.W)), io.update_domain))
      wentry.secret.foreach(_ := io.update_secret)
      wentry.written.foreach(_ := true.B)   // [LOOPRESET]
      entries(io.update_idx) := wentry
    } .elsewhen (io.update_repair && !doing_reset) {
      when (tag_match && !(f4_fire && io.update_idx === f4_idx)) {
        wentry.s_cnt := io.update_meta.s_cnt
        entries(io.update_idx) := wentry
      }
    }

    when (doing_reset) {
      entries(reset_idx) := (0.U).asTypeOf(new LoopEntry)
   }

    // [SINKFACTS86] patch 86 (all-sinks study Q1): is the loop predictor inert?  Debug printf builds only (the
    // IftSquashMerge gate: IFT on, CF debug printfs on, no IFT bridge; IFT off / NoLog / FireSim elaborate nothing).
    //  [LOOPU]    an update_mispredict / update_repair reached this column (the ONLY inputs that write tag, conf or
    //             p_cnt): expected never, because the FTQ never arms the mispredict/repair walk (Q2, [Q2ARM]).
    //  [LOOPV]    the count of entries with conf =/= 0 or tag =/= 0 changed (allocation or loss); nage = entries
    //             with age =/= 0 (the f4 path ages tag-0 entries only).  One line right after the 16-cycle reset
    //             sweep is expected (its RegNext compare sees the last uncleared entry); any line with nconf > 0 or
    //             ntag > 0 means the predictor is not inert.
    //  [LOOPFLIP] the column overrode the TAGE direction (needs conf = 7): expected never.
    if (ENABLE_IFT && ENABLE_CF_DEBUG_PRINTF && !ENABLE_IFT_BRIDGE) {
      val cf_sf_cyc = RegInit(0.U(40.W))
      cf_sf_cyc := cf_sf_cyc + 1.U
      val cf_nconf = PopCount(entries.map(_.conf =/= 0.U))
      val cf_ntag  = PopCount(entries.map(_.tag =/= 0.U))
      val cf_nage  = PopCount(entries.map(_.age =/= 0.U))
      when (!doing_reset && (cf_nconf =/= RegNext(cf_nconf) || cf_ntag =/= RegNext(cf_ntag))) {
        printf("[LOOPV] cyc=%d nconf=%d ntag=%d nage=%d\n", cf_sf_cyc, cf_nconf, cf_ntag, cf_nage)
      }
      when (!doing_reset && (io.update_mispredict || io.update_repair)) {
        printf("[LOOPU] cyc=%d mp=%d rp=%d idx=0x%x tagm=%d conf=%d\n", cf_sf_cyc, io.update_mispredict,
          io.update_repair, io.update_idx, tag_match, entry.conf)
      }
      when (io.f3_pred =/= io.f3_pred_in) {
        printf("[LOOPFLIP] cyc=%d tag=0x%x\n", cf_sf_cyc, f3_tag)
      }
    }

  }


  val columns = Seq.fill(bankWidth) { Module(new LoopBranchPredictorColumn) }
  val mems = Nil // TODO fix
  val f3_meta = Wire(Vec(bankWidth, new LoopMeta))
  override val metaSz = f3_meta.asUInt.getWidth

  val update_meta = s1_update.bits.meta.asTypeOf(Vec(bankWidth, new LoopMeta))
  // O4: the loop predictor is a direction predictor, so it reports as BPD state.
  // composer.scala ORs f3_bpd_*_mismatch across banks -- no new influencer type needed.
  // [ATKDMMASK] lean58 A5 (gap inventory 2026-10-23): the attacker-side twin of [BPDMASK]: a slot below the fetch
  // PC, or a bank whose request is not valid, is not part of the packet; its domain label raises no edge either.
  io.f3_bpd_domain_mismatch := (if (ENABLE_IFT) (0 until bankWidth).map(w =>
    columns(w).io.f3_domain_mismatch && s3_mask(w) && s3_valid).reduce(_||_)
    else columns.map(_.io.f3_domain_mismatch).reduce(_||_))   // IFT off: unchanged Verilog
  // [BPDMASK] r170 A (DF-dt-sprop): a slot below the fetch PC, or a bank whose request is not
  // valid, is not part of the packet; its secret label raises no edge.
  io.f3_bpd_secret_mismatch := (0 until bankWidth).map(w =>
    columns(w).io.f3_secret_mismatch && s3_mask(w) && s3_valid).reduce(_||_)

  for (w <- 0 until bankWidth) {
    columns(w).io.f2_req_valid := s2_valid
    columns(w).io.f2_req_idx  := s2_idx
    columns(w).io.f3_req_fire := (s3_valid && s3_mask(w) && io.f3_fire &&
      RegNext(io.resp_in(0).f2(w).predicted_pc.valid && io.resp_in(0).f2(w).is_br))

    columns(w).io.f3_pred_in  := io.resp_in(0).f3(w).taken
    columns(w).io.update_domain := s1_update.bits.cf_domain_id.getOrElse(0.U(1.W))
    columns(w).io.update_secret := s1_update.bits.cf_is_secret.getOrElse(false.B)
    // [R124GATE2] patch 27 (r124/B51): the two f3 domain flops only with ENABLE_IFT.
    columns(w).io.f3_req_domain := (if (ENABLE_IFT) RegNext(RegNext(s1_domain)) else 0.U)
    io.resp.f3(w).taken       := columns(w).io.f3_pred

    columns(w).io.update_mispredict      := (s1_update.valid &&
                                             s1_update.bits.br_mask(w) &&
                                             s1_update.bits.is_mispredict_update &&
                                             s1_update.bits.cfi_mispredicted)
    columns(w).io.update_repair          := (s1_update.valid &&
                                             s1_update.bits.br_mask(w) &&
                                            s1_update.bits.is_repair_update)
    columns(w).io.update_idx             := s1_update_idx
    columns(w).io.update_resolve_dir     := s1_update.bits.cfi_taken
    columns(w).io.update_meta            := update_meta(w)

    f3_meta(w) := columns(w).io.f3_meta
  }

  // [SINKFACTS86] patch 86 (all-sinks study Q1, the IFT side of "inert"): a FIRED f3 packet for which a loop
  // column reports a domain mismatch.  With no allocation, every entry keeps its reset value (tag 0, domain 0),
  // so this can only be an attacker-domain fetch whose packet index has tag bits idx[13:4] = 0 (pc[17:8] = 0)
  // matching a never-written entry: io.f3_bpd_domain_mismatch then raises fetch flag 3 (INFL_BPD_STATE,
  // is_atk = any_is_victim) with no state behind it.  dm = per-column mask, idx = s3_idx (pc >> 4), fdom = the
  // f3 fetch domain.  Same gate as the column printfs.
  if (ENABLE_IFT && ENABLE_CF_DEBUG_PRINTF && !ENABLE_IFT_BRIDGE) {
    val cf_sf_cyc = RegInit(0.U(40.W))
    cf_sf_cyc := cf_sf_cyc + 1.U
    val cf_dm = VecInit(columns.map(_.io.f3_domain_mismatch)).asUInt
    when (s3_valid && io.f3_fire && cf_dm =/= 0.U) {
      printf("[LOOPDM] cyc=%d idx=0x%x dm=0x%x fdom=%d\n", cf_sf_cyc, s3_idx, cf_dm, columns(0).io.f3_req_domain)
    }
  }

  io.f3_meta := f3_meta.asUInt

}
