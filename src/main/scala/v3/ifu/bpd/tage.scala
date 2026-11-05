package boom.v3.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.v3.common._
import boom.v3.util.{BoomCoreStringPrefix, MaskLower, WrapInc}

import scala.math.min





class TageResp extends Bundle {
  val ctr = UInt(3.W)
  val u   = UInt(2.W)
}

class TageTable(val nRows: Int, val tagSz: Int, val histLength: Int, val uBitPeriod: Int)
  (implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFrontendParameters
{
  require(histLength <= globalHistoryLength)

  val nWrBypassEntries = 2
  val io = IO( new Bundle {
    val f1_req_valid = Input(Bool())
    val f1_req_pc    = Input(UInt(vaddrBitsExtended.W))
    val f1_req_ghist = Input(UInt(globalHistoryLength.W))

    val f3_resp = Output(Vec(bankWidth, Valid(new TageResp)))

    val update_mask    = Input(Vec(bankWidth, Bool()))
    val update_taken   = Input(Vec(bankWidth, Bool()))
    val update_alloc   = Input(Vec(bankWidth, Bool()))
    val update_old_ctr = Input(Vec(bankWidth, UInt(3.W)))

    val update_pc    = Input(UInt())
    val update_hist  = Input(UInt())

    val update_u_mask = Input(Vec(bankWidth, Bool()))
    val update_u = Input(Vec(bankWidth, UInt(2.W)))

    // corefuzzing: domain tracking for TAGE shadow
    val f1_req_domain_id    = Input(UInt(1.W))   // domain of the current fetch at f1
    val update_cf_domain_id = Input(UInt(1.W))   // domain of the fetch being committed
    val f3_domain_mismatch  = Output(Vec(bankWidth, Bool()))  // hit entry written by different domain
    // corefuzzing: secret shadow — entry was trained by a secret-dependent instruction
    val update_cf_is_secret = Input(Bool())
    val f3_secret_mismatch  = Output(Vec(bankWidth, Bool()))  // hit entry trained by secret instr
    // [SPECBPU-REP] (owner 2026-11-02 'Speculative BPU repair'; spec-bpu-plan-final C-TAGE-3): repair / relabel
    // controls, registered in the bank like the other update inputs (cycle u+2), and the old value the provider
    // write of a resolution update used, registered here (u+3) for the bank's undo log
    val update_repair   = Input(Bool())       // the masked slot of this table is a repair write (restore or undo)
    val update_inval    = Input(Bool())       // ... an allocation undo: valid 0, ctr 0, label 0
    val update_used_ctr = Output(UInt(3.W))   // the old counter the provider write used (bypass word, else meta)
    val update_rp_lbl   = if (ENABLE_IFT) Some(Input(UInt(2.W))) else None                   // restored {dom, sec}
    val update_old_lbl  = if (ENABLE_IFT) Some(Input(Vec(bankWidth, UInt(2.W)))) else None   // fetch-time provider lbl
    val update_used_lbl = if (ENABLE_IFT) Some(Output(UInt(2.W))) else None                  // label of the old value
    val update_lbl_only = if (CF_SPEC_RELABEL) Some(Input(Bool())) else None               // [SPECBPU-RLB]
  })

  def compute_folded_hist(hist: UInt, l: Int) = {
    val nChunks = (histLength + l - 1) / l
    val hist_chunks = (0 until nChunks) map {i =>
      hist(min((i+1)*l, histLength)-1, i*l)
    }
    hist_chunks.reduce(_^_)
  }

  def compute_tag_and_hash(unhashed_idx: UInt, hist: UInt) = {
    val idx_history = compute_folded_hist(hist, log2Ceil(nRows))
    val idx = (unhashed_idx ^ idx_history)(log2Ceil(nRows)-1,0)
    val tag_history = compute_folded_hist(hist, tagSz)
    val tag = ((unhashed_idx >> log2Ceil(nRows)) ^ tag_history)(tagSz-1,0)
    (idx, tag)
  }

  def inc_ctr(ctr: UInt, taken: Bool): UInt = {
    Mux(!taken, Mux(ctr === 0.U, 0.U, ctr - 1.U),
                Mux(ctr === 7.U, 7.U, ctr + 1.U))
  }


  val doing_reset = RegInit(true.B)
  val reset_idx = RegInit(0.U(log2Ceil(nRows).W))
  reset_idx := reset_idx + doing_reset
  when (reset_idx === (nRows-1).U) { doing_reset := false.B }


  class TageEntry extends Bundle {
    val valid = Bool() // TODO: Remove this valid bit
    val tag = UInt(tagSz.W)
    val ctr = UInt(3.W)
  }


  val tageEntrySz = 1 + tagSz + 3

  val (s1_hashed_idx, s1_tag) = compute_tag_and_hash(fetchIdx(io.f1_req_pc), io.f1_req_ghist)

  val hi_us  = SyncReadMem(nRows, Vec(bankWidth, Bool()))
  val lo_us  = SyncReadMem(nRows, Vec(bankWidth, Bool()))
  val table  = SyncReadMem(nRows, Vec(bankWidth, UInt(tageEntrySz.W)))
  // corefuzzing: domain shadow — records the domain of the fetch that last wrote each row
  // OPT (audit rank 3): domain and secret were two separate single-bit SyncReadMems,
  // instantiated PER TAGE TABLE -- 14 memory instances across 7 tables where 7 two-bit
  // ones do the same job.  Both had identical index, write mask, reset behaviour and
  // read enable, so packing them into one {domain, secret} word is exactly equivalent
  // and halves the address-decode/port logic.
  val table_shadow  = SyncReadMem(nRows, Vec(bankWidth, UInt(2.W)))
  // corefuzzing: secret shadow — records whether the fetch that last wrote each row was secret

  val mems = Seq((f"tage_l$histLength", nRows, bankWidth * tageEntrySz))

  val s2_tag       = RegNext(s1_tag)

  val s2_req_rtage = VecInit(table.read(s1_hashed_idx, io.f1_req_valid).map(_.asTypeOf(new TageEntry)))
  val s2_req_rhius = hi_us.read(s1_hashed_idx, io.f1_req_valid)
  val s2_req_rlous = lo_us.read(s1_hashed_idx, io.f1_req_valid)
  val s2_req_rhits = VecInit(s2_req_rtage.map(e => e.valid && e.tag === s2_tag && !doing_reset))
  // corefuzzing: domain shadow read — same index/enable as main table
  // [R124GATE3] patch 28 (B51): the shadow memory is read/written and the domain/mismatch flops exist
  // only with ENABLE_IFT; IFT off table_shadow has no ports (removed) and the outputs are 0.
  val s2_req_rshadow  = if (ENABLE_IFT) table_shadow.read(s1_hashed_idx, io.f1_req_valid)
                        else VecInit(Seq.fill(bankWidth)(0.U(2.W)))
  val s2_req_rdomain  = VecInit(s2_req_rshadow.map(_(1,1)))
  val s2_fetch_domain = if (ENABLE_IFT) RegNext(io.f1_req_domain_id) else 0.U   // [R124GATE3]
  // corefuzzing: secret shadow read — same index/enable
  val s2_req_rsecret  = VecInit(s2_req_rshadow.map(_(0)))

  for (w <- 0 until bankWidth) {
    // This bit indicates the TAGE table matched here
    io.f3_resp(w).valid    := RegNext(s2_req_rhits(w))
    io.f3_resp(w).bits.u   := RegNext(Cat(s2_req_rhius(w), s2_req_rlous(w)))
    io.f3_resp(w).bits.ctr := RegNext(s2_req_rtage(w).ctr)
    // corefuzzing: domain mismatch if hit entry was written by a different domain
    io.f3_domain_mismatch(w) := (if (ENABLE_IFT) RegNext(s2_req_rhits(w) && (s2_req_rdomain(w) =/= s2_fetch_domain))
                                 else false.B)   // [R124GATE3]
    // corefuzzing: secret mismatch if hit entry was trained by a secret-dependent instruction
    io.f3_secret_mismatch(w) := (if (ENABLE_IFT) RegNext(s2_req_rhits(w) && s2_req_rsecret(w).asBool) else false.B)
  }

  val clear_u_ctr = RegInit(0.U((log2Ceil(uBitPeriod) + log2Ceil(nRows) + 1).W))
  when (doing_reset) { clear_u_ctr := 1.U } .otherwise { clear_u_ctr := clear_u_ctr + 1.U }

  val doing_clear_u = clear_u_ctr(log2Ceil(uBitPeriod)-1,0) === 0.U
  val doing_clear_u_hi = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 1.U
  val doing_clear_u_lo = doing_clear_u && clear_u_ctr(log2Ceil(uBitPeriod) + log2Ceil(nRows)) === 0.U
  val clear_u_idx = clear_u_ctr >> log2Ceil(uBitPeriod)

  val (update_idx, update_tag) = compute_tag_and_hash(fetchIdx(io.update_pc), io.update_hist)

  val update_wdata = Wire(Vec(bankWidth, new TageEntry))
  // [SPECBPU-REP] / [SPECBPU-RLB] (C-TAGE-3): a relabel writes the shadow only; a repair writes the logged counter
  // (restore) or an empty entry (allocation undo) with the logged / zero label
  val cf_lonly = io.update_lbl_only.getOrElse(false.B)
  val cf_nlbl  = Cat(io.update_cf_domain_id, (if (ENABLE_IFT) io.update_cf_is_secret else false.B))
  val cf_wlbl  = Mux(io.update_repair, Mux(io.update_inval, 0.U(2.W), io.update_rp_lbl.getOrElse(0.U(2.W))), cf_nlbl)

  table.write(
    Mux(doing_reset, reset_idx                                          , update_idx),
    Mux(doing_reset, VecInit(Seq.fill(bankWidth) { 0.U(tageEntrySz.W) }), VecInit(update_wdata.map(_.asUInt))),
    Mux(doing_reset, ~(0.U(bankWidth.W))                                , io.update_mask.asUInt & ~Fill(bankWidth, cf_lonly)).asBools
  )

  // corefuzzing: write {domain, secret} shadow in parallel with the main table.
  // IFT gate: with ENABLE_IFT=false the secret bit is a constant 0, which firtool folds
  // away -- the same dead-state elimination the split version relied on, minus a memory.
  val shadow_secret_bit = if (ENABLE_IFT) io.update_cf_is_secret else false.B
  if (ENABLE_IFT) {   // [R124GATE3]
  table_shadow.write(
    Mux(doing_reset, reset_idx, update_idx),
    Mux(doing_reset, VecInit(Seq.fill(bankWidth)(0.U(2.W))),
                     VecInit(Seq.fill(bankWidth)(cf_wlbl))),   // [SPECBPU-REP] train = cf_nlbl, repair, relabel
    Mux(doing_reset, ~(0.U(bankWidth.W)), io.update_mask.asUInt).asBools
  )
  }

  val update_hi_wdata = Wire(Vec(bankWidth, Bool()))
  hi_us.write(
    Mux(doing_reset, reset_idx, Mux(doing_clear_u_hi, clear_u_idx, update_idx)),
    Mux(doing_reset || doing_clear_u_hi, VecInit((0.U(bankWidth.W)).asBools), update_hi_wdata),
    Mux(doing_reset || doing_clear_u_hi, ~(0.U(bankWidth.W)), io.update_u_mask.asUInt).asBools
  )

  val update_lo_wdata = Wire(Vec(bankWidth, Bool()))
  lo_us.write(
    Mux(doing_reset, reset_idx, Mux(doing_clear_u_lo, clear_u_idx, update_idx)),
    Mux(doing_reset || doing_clear_u_lo, VecInit((0.U(bankWidth.W)).asBools), update_lo_wdata),
    Mux(doing_reset || doing_clear_u_lo, ~(0.U(bankWidth.W)), io.update_u_mask.asUInt).asBools
  )

  val wrbypass_tags    = Reg(Vec(nWrBypassEntries, UInt(tagSz.W)))
  val wrbypass_idxs    = Reg(Vec(nWrBypassEntries, UInt(log2Ceil(nRows).W)))
  val wrbypass         = Reg(Vec(nWrBypassEntries, Vec(bankWidth, UInt(3.W))))
  // [SPECBPU-B1] (review spec-bpu-review-20261102 B1; owner 2026-11-02 'Speculative BPU follow-ups'): one valid bit
  // per bypass entry and slot.  The bypass used to keep a whole row, so a slot outside update_mask stored
  // inc_ctr(DontCare, DontCare) and a later update of that slot (same row and tag) took that value instead of its
  // fetch-time provider counter.  The resolution update trains ONE slot per update, so the two branches of a packet
  // would corrupt each other on every iteration.  Now a slot's bypass word is used only when this entry wrote it.
  // Cost per table: 2 x bankWidth FF (reset) and about 12 LUT.  Levels: +1 AND on the existing hit select (update
  // side only, s1-registered inputs -> table write data), none on the fetch path.
  val wrbypass_v       = RegInit(VecInit(Seq.fill(nWrBypassEntries)(VecInit(Seq.fill(bankWidth)(false.B)))))
  // [SPECBPU-REP] (IFT) the label of each bypass word, so the undo log records the label the write overwrote
  val wrbypass_lbl     = if (ENABLE_IFT) Some(Reg(Vec(nWrBypassEntries, Vec(bankWidth, UInt(2.W))))) else None
  val wrbypass_enq_idx = RegInit(0.U(log2Ceil(nWrBypassEntries).W))

  val wrbypass_hits    = VecInit((0 until nWrBypassEntries) map { i =>
    !doing_reset &&
    wrbypass_tags(i) === update_tag &&
    wrbypass_idxs(i) === update_idx
  })
  val wrbypass_hit     = wrbypass_hits.reduce(_||_)
  val wrbypass_hit_idx = PriorityEncoder(wrbypass_hits)
  // [SPECBPU-B1] per slot: the hit entry holds this slot's counter
  val wrbypass_use     = VecInit((0 until bankWidth).map(w => wrbypass_hit && wrbypass_v(wrbypass_hit_idx)(w)))

  for (w <- 0 until bankWidth) {
    // [SPECBPU-REP] a repair writes the logged old counter (restore) or an empty entry (allocation undo)
    update_wdata(w).ctr   := Mux(io.update_repair, Mux(io.update_inval, 0.U, io.update_old_ctr(w)),
      Mux(io.update_alloc(w),
      Mux(io.update_taken(w), 4.U,
                              3.U
      ),
      Mux(wrbypass_use(w),    inc_ctr(wrbypass(wrbypass_hit_idx)(w), io.update_taken(w)),
                              inc_ctr(io.update_old_ctr(w), io.update_taken(w))
      )
    ))
    update_wdata(w).valid := !(io.update_repair && io.update_inval)
    update_wdata(w).tag   := update_tag

    update_hi_wdata(w)    := io.update_u(w)(1)
    update_lo_wdata(w)    := io.update_u(w)(0)
  }

  // [SPECBPU-REP] a restore updates the bypass like a write; an allocation undo and a relabel do not (below)
  when (io.update_mask.reduce(_||_) && !cf_lonly && !(io.update_repair && io.update_inval)) {
    when (wrbypass_hits.reduce(_||_)) {
      // [SPECBPU-B1] only the written slots change; the others keep their word and valid bit
      for (w <- 0 until bankWidth) {
        when (io.update_mask(w)) {
          wrbypass  (wrbypass_hit_idx)(w) := update_wdata(w).ctr
          wrbypass_v(wrbypass_hit_idx)(w) := true.B
          wrbypass_lbl.foreach(_(wrbypass_hit_idx)(w) := cf_wlbl)   // [SPECBPU-REP]
        }
      }
    } .otherwise {
      wrbypass     (wrbypass_enq_idx) := VecInit(update_wdata.map(_.ctr))
      wrbypass_v   (wrbypass_enq_idx) := io.update_mask           // [SPECBPU-B1] valid = the written slots only
      wrbypass_lbl.foreach(_(wrbypass_enq_idx) := VecInit(Seq.fill(bankWidth)(cf_wlbl)))   // [SPECBPU-REP]
      wrbypass_tags(wrbypass_enq_idx) := update_tag
      wrbypass_idxs(wrbypass_enq_idx) := update_idx
      wrbypass_enq_idx := WrapInc(wrbypass_enq_idx, nWrBypassEntries)
    }
  }
  // [SPECBPU-REP] an allocation undo drops the slot's bypass word (the entry is empty again)
  when (io.update_repair && io.update_inval && wrbypass_hit) {
    for (w <- 0 until bankWidth) { when (io.update_mask(w)) { wrbypass_v(wrbypass_hit_idx)(w) := false.B } }
  }
  // [SPECBPU-RLB] a relabel rewrites the label of a bypass word it covers
  if (CF_SPEC_RELABEL) {
    when (cf_lonly && wrbypass_hit) {
      for (w <- 0 until bankWidth) {
        when (io.update_mask(w) && wrbypass_v(wrbypass_hit_idx)(w)) { wrbypass_lbl.get(wrbypass_hit_idx)(w) := cf_nlbl }
      }
    }
  }
  // [SPECBPU-REP] the old value the provider write used (this slot's bypass word, else the fetch-time counter /
  // label): one slot per update (one CFI per update), registered (u+3) for the bank's undo log.  Same select as the
  // counter mux above (shared by synthesis); 4-way one-hot AND-OR -> 3 FF (+2 IFT).
  private val cf_prov_sel = VecInit((0 until bankWidth).map(w => io.update_mask(w) && !io.update_alloc(w)))
  io.update_used_ctr := RegNext(Mux1H(cf_prov_sel, (0 until bankWidth).map(w =>
    Mux(wrbypass_use(w), wrbypass(wrbypass_hit_idx)(w), io.update_old_ctr(w)))))
  io.update_used_lbl.foreach(_ := RegNext(Mux1H(cf_prov_sel, (0 until bankWidth).map(w =>
    Mux(wrbypass_use(w), wrbypass_lbl.get(wrbypass_hit_idx)(w), io.update_old_lbl.get(w))))))

  // [SINKFACTS86] patch 86 (all-sinks study Q6): every TAGE table write, debug printf builds only (the
  // IftSquashMerge gate: IFT on, CF debug printfs on, no IFT bridge; IFT off / NoLog / FireSim elaborate nothing).
  // h / ts = this table's history length / tag size (the pair names the table; two tables share h = 16);
  // pc = the bank-aligned update pc (bank = pc[3]); idx / tag = the hashed row and tag written; m = entry write
  // mask, al = allocations, tk = taken; old = the FETCH-time provider counter (non-allocating slots); byp / bypv =
  // the 2-entry write bypass hit and its row; bypm = the slots whose counter came from the bypass ([SPECBPU-B1]: used
  // = bypv for a slot in bypm, else old; a byp = 1 slot outside bypm uses old); new = written counters;
  // um / u = u-bit write mask and value; clr = {hi, lo} periodic u-clear in this cycle (it wins over the update's
  // u write of that half, tage.scala hi_us / lo_us).  [TAGECLR] marks the start of each u-clear sweep (the time
  // base of the u state).  The host replays the writes from the reset (ctr 0, valid 0): used =/= true is a
  // fetch-vs-update interleaving event.  Unwritten slots are printed 0.
  if (ENABLE_IFT && ENABLE_CF_DEBUG_PRINTF && !ENABLE_IFT_BRIDGE) {
    val cf_sf_cyc = RegInit(0.U(40.W))
    cf_sf_cyc := cf_sf_cyc + 1.U
    def cfMasked(m: Seq[Bool], v: Seq[UInt], w: Int): UInt = VecInit(m.zip(v).map { case (b, x) => Mux(b, x, 0.U(w.W)) }).asUInt
    when (!doing_reset && (io.update_mask.asUInt =/= 0.U || io.update_u_mask.asUInt =/= 0.U)) {
      printf("[TAGEW] cyc=%d pc=0x%x h=%d ts=%d idx=0x%x tag=0x%x m=0x%x al=0x%x tk=0x%x old=0x%x byp=%d bypv=0x%x new=0x%x um=0x%x u=0x%x clr=%d bypm=0x%x k=%d\n",
        cf_sf_cyc, io.update_pc, histLength.U, tagSz.U, update_idx, update_tag, io.update_mask.asUInt,
        io.update_alloc.asUInt & io.update_mask.asUInt, io.update_taken.asUInt & io.update_mask.asUInt,
        cfMasked((0 until bankWidth).map(w => io.update_mask(w) && !io.update_alloc(w)), io.update_old_ctr, 3),
        wrbypass_hit, wrbypass(wrbypass_hit_idx).asUInt,
        cfMasked(io.update_mask, update_wdata.map(_.ctr), 3),
        io.update_u_mask.asUInt, cfMasked(io.update_u_mask, io.update_u, 2),
        Cat(doing_clear_u_hi, doing_clear_u_lo),
        (wrbypass_use.asUInt & (io.update_mask.asUInt & ~io.update_alloc.asUInt)),
        Mux(cf_lonly, 2.U, Mux(io.update_repair, 1.U, 0.U)))   // [SPECBPU-REP] k = 0 train, 1 repair, 2 relabel
    }
    when (!doing_reset && doing_clear_u && clear_u_idx(log2Ceil(nRows)-1, 0) === 0.U) {
      printf("[TAGECLR] cyc=%d h=%d ts=%d hi=%d\n", cf_sf_cyc, histLength.U, tagSz.U, doing_clear_u_hi)
    }
  }



}


case class BoomTageParams(
  //                                           nSets, histLen, tagSz
  tableInfo: Seq[Tuple3[Int, Int, Int]] = Seq((  128,       2,     7),
                                              (  128,       4,     7),
                                              (  256,       8,     8),
                                              (  256,      16,     7), // for the fuzzy core - AK
                                              (  256,      16,     8),
                                              (  128,      32,     9),
                                              (  128,      64,     9)),
  uBitPeriod: Int = 2048
)


class TageBranchPredictorBank(params: BoomTageParams = BoomTageParams())(implicit p: Parameters)
  extends BranchPredictorBank()(p)
  with freechips.rocketchip.util.CoreFuzzingConstants
{
  val tageUBitPeriod = params.uBitPeriod
  override def cfBpdF3Only: Boolean = true   // [ICPREDF3-TAGE] f3-only bpd mismatch (predictor.scala)
  // [SPECBPU] (owner 2026-11-02, RULES C 'Speculative TAGE/BTB updates'): trained once per resolved CFI at
  // resolution (wrong path included) from io.spec_update; no commit training (predictor.scala cf_upd_bus).
  override def trainsAtResolution: Boolean = true
  val tageNTables    = params.tableInfo.size

  class TageMeta extends Bundle
  {
    val provider      = Vec(bankWidth, Valid(UInt(log2Ceil(tageNTables).W)))
    val alt_differs   = Vec(bankWidth, Output(Bool()))
    val provider_u    = Vec(bankWidth, Output(UInt(2.W)))
    val provider_ctr  = Vec(bankWidth, Output(UInt(3.W)))
    val allocate      = Vec(bankWidth, Valid(UInt(log2Ceil(tageNTables).W)))
    // [SPECBPU-REP] (labels "Restore old label"): the fetch-time {domain, secret} of the provider entry (IFT only)
    val provider_lbl  = if (ENABLE_IFT) Some(Vec(bankWidth, UInt(2.W))) else None
  }

  val f3_meta = Wire(new TageMeta)
  override val metaSz = f3_meta.asUInt.getWidth
  require(metaSz <= bpdMaxMetaLength)

  def inc_u(u: UInt, alt_differs: Bool, mispredict: Bool): UInt = {
    Mux(!alt_differs, u,
    Mux(mispredict, Mux(u === 0.U, 0.U, u - 1.U),
                    Mux(u === 3.U, 3.U, u + 1.U)))
  }

  val tt = params.tableInfo map {
    case (n, l, s) => {
      val t = Module(new TageTable(n, s, l, params.uBitPeriod))
      t.io.f1_req_valid       := RegNext(io.f0_valid)
      t.io.f1_req_pc          := RegNext(io.f0_pc)
      t.io.f1_req_ghist       := io.f1_ghist
      // corefuzzing: wire domain to table for domain shadow tracking
      t.io.f1_req_domain_id   := s1_domain
      (t, t.mems)
    }
  }
  val tables = tt.map(_._1)
  val mems = tt.map(_._2).flatten

  val f3_resps = VecInit(tables.map(_.io.f3_resp))

  val s1_update_meta = s1_update.bits.meta.asTypeOf(new TageMeta)
  val s1_update_mispredict_mask = UIntToOH(s1_update.bits.cfi_idx.bits) &
    Fill(bankWidth, s1_update.bits.cfi_mispredicted)

  val s1_update_mask  = WireInit((0.U).asTypeOf(Vec(tageNTables, Vec(bankWidth, Bool()))))
  val s1_update_u_mask  = WireInit((0.U).asTypeOf(Vec(tageNTables, Vec(bankWidth, UInt(1.W)))))

  val s1_update_taken   = Wire(Vec(tageNTables, Vec(bankWidth, Bool())))
  val s1_update_old_ctr = Wire(Vec(tageNTables, Vec(bankWidth, UInt(3.W))))
  val s1_update_alloc   = Wire(Vec(tageNTables, Vec(bankWidth, Bool())))
  val s1_update_u       = Wire(Vec(tageNTables, Vec(bankWidth, UInt(2.W))))

  s1_update_taken   := DontCare
  s1_update_old_ctr := DontCare
  s1_update_alloc   := DontCare
  s1_update_u       := DontCare

  // added for the fuzzycore project - AK
  // Create a val that will be written with the reconfigure signal from io
  val cf_bpd_tage_to_gshare = Wire(Bool())
  cf_bpd_tage_to_gshare := io.cf_bpd_tage_to_gshare

  // corefuzzing: runtime TAGE active table count selection
  // tagetableCountOptions = Seq(7,6,5,3,2,1): values are shifted past table 3 for
  // the upper options so that the real TAGE table count is correct (see CoreFuzzing.scala).
  // [STATIC-CLAMP] each option is capped at the built size: identical constants when the module is built at
  // the max option (the fuzzing build); a static smaller build with reconf off (index 0) gets its own size.
  val tageOptionsVec = VecInit(tagetableCountOptions.map(o => (o min tageNTables).U))
  val cf_tage_active = tageOptionsVec(io.cf_tage_count_idx)

  // Per-table enable mask: in GShare mode only table 3 is active; in TAGE mode
  // all tables except table 3 that are within the cf_tage_active range are active.
  // This mask is used for prediction, metadata tracking, and allocation.
  // [FPGA-TIMING-FIT] (owner 2026-11-03, RULES C 'FPGA timing fit'): the mask is a ONE-cycle registered copy of the
  // 0x7c2 bit 2 / 0xbca fields, the only copy in this bank, read by prediction (bank3 use, provider tracking,
  // allocation) and by the spec-BPU repair / undo masking (left_off), so read and write masking always agree
  // (04-reconfiguration-infrastructure.md masked-structure rules).  The CSR flop (core) -> SLR hop -> mask compare now
  // ends at this register instead of continuing through f3 -> s1 -> I$ (the AllDef worst paths); no level is added on
  // the fetch side (the mask is read from a flop).  A DMI write takes effect one cycle later, while the hart is halted.
  // Reset value = the mask at the CSRs' reset values (TAGE mode, count index 0); with reconfiguration off both inputs
  // are constants equal to those values, so the register folds to the same constant.
  val bank_enable_mask_init = (0 until tageNTables).map { i =>
    if (i != 3 && i < (tagetableCountOptions(0) min tageNTables)) BigInt(1) << i else BigInt(0) }.sum
  // RegInit + := (not RegNext(next, init)): RegNext drops the width (Chisel 6 cloneTypeWidth(Width())) and the
  // width queries below (allocatable_slots) need it.  Same register, same reset value.
  val bank_enable_mask = RegInit(bank_enable_mask_init.U(tageNTables.W))
  bank_enable_mask := VecInit((0 until tageNTables).map { i =>
    val is_gshare_table = (i.U === 3.U)
    Mux(cf_bpd_tage_to_gshare,
      is_gshare_table,                            // GShare mode: only table 3
      !is_gshare_table && (i.U < cf_tage_active)  // TAGE mode:  not-3, in range
    )
  }).asUInt

  // [SPECBPU-REP] the fetch domain at f3: TageTable's f3 mismatch flops compare the entry domain with
  // s2_fetch_domain = RegNext(f1 domain), so one more register gives the value they compared with
  val cf_s3_fdom = if (ENABLE_IFT) RegNext(RegNext(s1_domain)) else 0.U(1.W)
  for (w <- 0 until bankWidth) {
    var altpred = io.resp_in(0).f3(w).taken
    val final_altpred = WireInit(io.resp_in(0).f3(w).taken)
    var provided = false.B
    var provider = 0.U
    io.resp.f3(w).taken := io.resp_in(0).f3(w).taken

    for (i <- 0 until tageNTables) {
      val hit = f3_resps(i)(w).valid
      val ctr = f3_resps(i)(w).bits.ctr

      // Table 3 is the GShare bank: active only when cf_bpd_tage_to_gshare=1.
      // In TAGE mode (cf_bpd_tage_to_gshare=0), table 3 is always excluded.
      // bank_enable_mask (computed above) encodes both the GShare/TAGE selection
      // and the cf_tage_active range gate, so use it directly.
      val use_bank3 = bank_enable_mask(i)
      when (hit && use_bank3) {
        io.resp.f3(w).taken := Mux(ctr === 3.U || ctr === 4.U, altpred, ctr(2))
        final_altpred       := altpred
      }

      // Only track provided/provider/altpred for tables that are truly active
      // (matches use_bank3 so GShare mode never records non-table-3 as provider)
      provided = provided || (hit && use_bank3)
      provider = Mux(hit && use_bank3, i.U, provider)
      altpred  = Mux(hit && use_bank3, f3_resps(i)(w).bits.ctr(2), altpred)
    }
    f3_meta.provider(w).valid := provided
    f3_meta.provider(w).bits  := provider
    f3_meta.alt_differs(w)    := final_altpred =/= io.resp.f3(w).taken
    f3_meta.provider_u(w)     := f3_resps(provider)(w).bits.u
    f3_meta.provider_ctr(w)   := f3_resps(provider)(w).bits.ctr
    // [SPECBPU-REP] provider label from the table's f3 mismatch flops: secret = secret_mismatch (a hit), domain =
    // domain_mismatch ^ the compared fetch domain (domain_mismatch = hit && entry domain =/= fetch domain)
    f3_meta.provider_lbl.foreach(_(w) := Cat(VecInit(tables.map(_.io.f3_domain_mismatch(w))).asUInt(provider) ^ cf_s3_fdom,
                                             VecInit(tables.map(_.io.f3_secret_mismatch(w))).asUInt(provider)))

    // Create a mask of tables which did not hit our query, and also contain useless entries
    // and also uses a longer history than the provider
    // Restrict allocation to tables that are active under the current mode/count.
    // bank_enable_mask excludes table 3 in TAGE mode and all non-3 tables in GShare mode.
    val allocatable_slots = (
      VecInit(f3_resps.map(r => !r(w).valid && r(w).bits.u === 0.U)).asUInt &
      ~(MaskLower(UIntToOH(provider)) & Fill(tageNTables, provided)) &
      bank_enable_mask
    )
    val alloc_lfsr = random.LFSR(tageNTables max 2)

    val first_entry = PriorityEncoder(allocatable_slots)
    val masked_entry = PriorityEncoder(allocatable_slots & alloc_lfsr)
    val alloc_entry = Mux(allocatable_slots(masked_entry),
      masked_entry,
      first_entry)

    f3_meta.allocate(w).valid := allocatable_slots =/= 0.U
    f3_meta.allocate(w).bits  := alloc_entry

    val update_was_taken = (s1_update.bits.cfi_idx.valid &&
                            (s1_update.bits.cfi_idx.bits === w.U) &&
                            s1_update.bits.cfi_taken)
    when (s1_update.bits.br_mask(w) && s1_update.valid && s1_update.bits.is_commit_update) {
      when (s1_update_meta.provider(w).valid) {
        val provider = s1_update_meta.provider(w).bits

        s1_update_mask(provider)(w) := true.B
        s1_update_u_mask(provider)(w) := true.B

        val new_u = inc_u(s1_update_meta.provider_u(w),
                          s1_update_meta.alt_differs(w),
                          s1_update_mispredict_mask(w))
        s1_update_u      (provider)(w) := new_u
        s1_update_taken  (provider)(w) := update_was_taken
        s1_update_old_ctr(provider)(w) := s1_update_meta.provider_ctr(w)
        s1_update_alloc  (provider)(w) := false.B

      }
    }
  }
  when (s1_update.valid && s1_update.bits.is_commit_update && s1_update.bits.cfi_mispredicted && s1_update.bits.cfi_idx.valid) {
    val idx = s1_update.bits.cfi_idx.bits
    val allocate = s1_update_meta.allocate(idx)
    when (allocate.valid) {
      s1_update_mask (allocate.bits)(idx) := true.B
      s1_update_taken(allocate.bits)(idx) := s1_update.bits.cfi_taken
      s1_update_alloc(allocate.bits)(idx) := true.B

      s1_update_u_mask(allocate.bits)(idx) := true.B
      s1_update_u     (allocate.bits)(idx) := 0.U

    } .otherwise {
      val provider = s1_update_meta.provider(idx)
      val decr_mask = Mux(provider.valid, ~MaskLower(UIntToOH(provider.bits)), 0.U)

      for (i <- 0 until tageNTables) {
        when (decr_mask(i)) {
          s1_update_u_mask(i)(idx) := true.B
          s1_update_u     (i)(idx) := 0.U
        }
      }
    }

  }



  // [SPECBPU-REP] (owner 2026-11-02 'Speculative BPU repair'; spec-bpu-plan-final C-TAGE-5) local undo log, one word per
  // record id: what this bank's write of that resolution update overwrote.  Written at u+3 (the tables register the
  // old counter / label they used at u+3), read at the s1 of a repair (u'+1 >= u+4: the FTQ holds every repair 2
  // cycles after an allocation).  The slot is the repair's cfi_idx (the same bank split as the original update).
  // Restore: the provider entry gets its logged counter, u and label back; allocation undo: the allocated entry is
  // emptied (valid 0, u 0, label 0).  Left and counted ([TAGERP] left_ud): a failed allocation's u := 0.
  // [SPECBPU-RLB] relabel: the same entries' shadow only, with the update's (new) label.
  class TageUndo extends Bundle {
    val pv = Bool()                                    // the update trained a provider entry ...
    val pt = UInt(log2Ceil(tageNTables).W)             // ... in this table
    val pc = UInt(3.W)                                 // its old counter (bypass word, else fetch-time)
    val pu = UInt(2.W)                                 // its fetch-time u
    val av = Bool()                                    // the update allocated an entry ...
    val at = UInt(log2Ceil(tageNTables).W)             // ... in this table
    val ud = Bool()                                    // failed allocation (u := 0 above the provider: left)
    val pl = if (ENABLE_IFT) Some(UInt(2.W)) else None  // the provider's old {domain, secret}
  }
  val cf_tlog   = Mem(cfSpecLogN, new TageUndo)
  val s1_lslot  = s1_update.bits.cfi_idx.bits
  val s1_lw     = s1_update.valid && s1_update.bits.is_commit_update && s1_update.bits.cfi_idx.valid
  val s1_lu     = Wire(new TageUndo)
  s1_lu.pv := s1_update.bits.br_mask(s1_lslot) && s1_update_meta.provider(s1_lslot).valid
  s1_lu.pt := s1_update_meta.provider(s1_lslot).bits
  s1_lu.pc := 0.U                                       // taken from the provider table at u+3
  s1_lu.pu := s1_update_meta.provider_u(s1_lslot)
  s1_lu.av := s1_update.bits.cfi_mispredicted && s1_update_meta.allocate(s1_lslot).valid
  s1_lu.at := s1_update_meta.allocate(s1_lslot).bits
  s1_lu.ud := s1_update.bits.cfi_mispredicted && !s1_update_meta.allocate(s1_lslot).valid
  s1_lu.pl.foreach(_ := 0.U)                            // taken from the provider table at u+3
  val s3_lw  = RegNext(RegNext(s1_lw, false.B), false.B)
  val s3_lid = RegNext(RegNext(s1_update.bits.cf_log_id))
  val s3_lu  = RegNext(RegNext(s1_lu))
  when (s3_lw) {
    val wd = WireInit(s3_lu)
    wd.pc := VecInit(tables.map(_.io.update_used_ctr))(s3_lu.pt)
    wd.pl.foreach(_ := VecInit(tables.map(_.io.update_used_lbl.get))(s3_lu.pt))
    cf_tlog.write(s3_lid, wd)
  }
  val s1_rp  = s1_update.valid && s1_update.bits.is_repair_update && s1_update.bits.cfi_idx.valid
  val s1_rlb = s1_rp && s1_update.bits.cf_relabel.getOrElse(false.B)
  val s1_rst = s1_rp && !s1_rlb
  val s1_ru  = cf_tlog.read(s1_update.bits.cf_log_id)
  val s1_rp_restore = WireInit(VecInit(Seq.fill(tageNTables)(false.B)))
  val s1_rp_inval   = WireInit(VecInit(Seq.fill(tageNTables)(false.B)))
  when (s1_rp) {
    when (s1_ru.pv) {
      s1_update_mask   (s1_ru.pt)(s1_lslot) := true.B
      s1_update_alloc  (s1_ru.pt)(s1_lslot) := false.B
      s1_update_taken  (s1_ru.pt)(s1_lslot) := false.B
      s1_update_old_ctr(s1_ru.pt)(s1_lslot) := s1_ru.pc
      when (s1_rst) {
        s1_update_u_mask(s1_ru.pt)(s1_lslot) := true.B
        s1_update_u     (s1_ru.pt)(s1_lslot) := s1_ru.pu
        s1_rp_restore   (s1_ru.pt)           := true.B
      }
    }
    when (s1_ru.av) {
      s1_update_mask (s1_ru.at)(s1_lslot) := true.B
      s1_update_alloc(s1_ru.at)(s1_lslot) := true.B
      s1_update_taken(s1_ru.at)(s1_lslot) := false.B
      when (s1_rst) {
        s1_update_u_mask(s1_ru.at)(s1_lslot) := true.B
        s1_update_u     (s1_ru.at)(s1_lslot) := 0.U
        s1_rp_inval     (s1_ru.at)           := true.B
      }
    }
  }
  // one register each, shared by the 7 tables (the label is per slot, not per table)
  val cf_s2_rp_lbl  = RegNext(s1_ru.pl.getOrElse(0.U(2.W)))
  val cf_s2_old_lbl = RegNext(VecInit((0 until bankWidth).map(w =>
                        s1_update_meta.provider_lbl.map(_(w)).getOrElse(0.U(2.W)))))
  val cf_s2_lonly   = RegNext(s1_rlb, false.B)
  if (ENABLE_IFT && ENABLE_CF_DEBUG_PRINTF && !ENABLE_IFT_BRIDGE) {
    // [TAGERP] every repair (k=1) / relabel (k=2) this bank decodes; left_ud = running count of restored records whose
    // failed-allocation u := 0 stays (RULES C: left and counted)
    val cf_rp_cyc = RegInit(0.U(40.W))
    cf_rp_cyc := cf_rp_cyc + 1.U
    val c_ud = RegInit(0.U(40.W))
    when (s1_rst && s1_ru.ud) { c_ud := c_ud + 1.U }
    // [SPECBPU-REP] DMI reconfiguration measurement (owner 2026-11-02 ~21:0x 'Speculative BPU gaps and
    // reconfiguration': D-2 "Accept + measure", no RTL fix; spec-bpu-plan-final 4.8): off = this repair / relabel
    // writes a table the current mask disables (bit 0 its provider table pt, bit 1 its allocation table at): a record
    // logged before a 0xbca / 0x7c2 change; the write happens (prediction does not read the table).  left_off =
    // running count.
    val d_off = Cat(s1_ru.av && !bank_enable_mask(s1_ru.at), s1_ru.pv && !bank_enable_mask(s1_ru.pt))
    val c_off = RegInit(0.U(40.W))
    when (s1_rp && d_off =/= 0.U) { c_off := c_off + 1.U }
    when (s1_rp) {
      printf("[TAGERP] cyc=%d id=%d k=%d slot=%d pv=%d pt=%d pc=%d pu=%d av=%d at=%d ud=%d left_ud=%d off=%d " +
             "left_off=%d\n", cf_rp_cyc,
        s1_update.bits.cf_log_id, Mux(s1_rlb, 2.U, 1.U), s1_lslot, s1_ru.pv, s1_ru.pt, s1_ru.pc, s1_ru.pu, s1_ru.av,
        s1_ru.at, s1_ru.ud, c_ud + (s1_rst && s1_ru.ud), d_off, c_off + (d_off =/= 0.U))
    }
  }

  for (i <- 0 until tageNTables) {
    for (w <- 0 until bankWidth) {
      tables(i).io.update_mask(w)    := RegNext(s1_update_mask(i)(w))
      tables(i).io.update_taken(w)   := RegNext(s1_update_taken(i)(w))
      tables(i).io.update_alloc(w)   := RegNext(s1_update_alloc(i)(w))
      tables(i).io.update_old_ctr(w) := RegNext(s1_update_old_ctr(i)(w))

      tables(i).io.update_u_mask(w) := RegNext(s1_update_u_mask(i)(w))
      tables(i).io.update_u(w)      := RegNext(s1_update_u(i)(w))
    }
    tables(i).io.update_pc             := RegNext(s1_update.bits.pc)
    tables(i).io.update_hist           := RegNext(s1_update.bits.ghist)
    // corefuzzing: wire update domain and secret to table for shadow writes
    // [R124GATE3] patch 28: IFT off the update domain/secret flops do not exist (constant 0 into the table).
    tables(i).io.update_cf_domain_id   := (if (ENABLE_IFT) RegNext(s1_update.bits.cf_domain_id.getOrElse(0.U(1.W))) else 0.U)
    tables(i).io.update_cf_is_secret   := (if (ENABLE_IFT) RegNext(s1_update.bits.cf_is_secret.getOrElse(false.B)) else false.B)
    // [SPECBPU-REP] / [SPECBPU-RLB] repair / relabel controls and the fetch-time provider label (u+2, like the others)
    tables(i).io.update_repair := RegNext(s1_rp_restore(i) || s1_rp_inval(i), false.B)
    tables(i).io.update_inval  := RegNext(s1_rp_inval(i), false.B)
    tables(i).io.update_rp_lbl.foreach(_ := cf_s2_rp_lbl)
    tables(i).io.update_old_lbl.foreach(_ := cf_s2_old_lbl)
    tables(i).io.update_lbl_only.foreach(_ := cf_s2_lonly)
  }

  // corefuzzing: TAGE domain mismatch — any table hit a row last updated by a different domain
  // [ATKDMMASK] lean58 A5 (gap inventory 2026-10-23): the attacker-side twin of [BPDMASK]: a slot below the fetch
  // PC, or a bank whose request is not valid, is not part of the packet; its domain label raises no edge either.
  io.f3_bpd_domain_mismatch := (if (ENABLE_IFT) tables.map(t =>
    (t.io.f3_domain_mismatch.asUInt & s3_mask & Fill(bankWidth, s3_valid)).orR).reduce(_||_)
    else tables.map(t => t.io.f3_domain_mismatch.reduce(_||_)).reduce(_||_))   // IFT off: unchanged Verilog
  // corefuzzing: TAGE secret mismatch — any table hit a row trained by a secret instruction
  // [BPDMASK] r170 A (DF-dt-sprop): a slot below the fetch PC, or a bank whose request is not
  // valid, is not part of the packet; its secret label raises no edge.
  io.f3_bpd_secret_mismatch := tables.map(t =>
    (t.io.f3_secret_mismatch.asUInt & s3_mask & Fill(bankWidth, s3_valid)).orR).reduce(_||_)

  // [V3TAP-TAGE] SP-19 (design 5.1 tg_prov / tg_alloc / tg_udec): decode of the table-input registers (cycle u+2, the
  // cycle every TageTable writes).  Per slot w: provider = the table with update_mask && !update_alloc (0 none,
  // t+1 = table t); allocation = update_mask && update_alloc; u-only write = update_u_mask && !update_mask.
  // update_alloc is read only under update_mask (it is DontCare elsewhere).  One-hot AND-OR of flops into the PRED
  // tap register (frontend).  Outputs only.
  io.cf_st.foreach { s =>
    require(tageNTables == 7 && bankWidth == 4, "STATE PRED TAGE fields: 7 tables, 4 slots per bank (design 5.1)")
    val stM  = tables.map(_.io.update_mask)
    val stA  = tables.map(_.io.update_alloc)
    val stU  = tables.map(_.io.update_u_mask)
    val stProvOH = (0 until bankWidth).map(w => VecInit((0 until tageNTables).map(i => stM(i)(w) && !stA(i)(w))).asUInt)
    s.tg_prov := VecInit(stProvOH.map(oh => Mux(oh.orR, OHToUInt(oh) +& 1.U, 0.U)(2, 0))).asUInt   // slot w at [3w+2:3w]
    val stAl = (0 until tageNTables).map(i => (0 until bankWidth).map(w => stM(i)(w) && stA(i)(w)))
    s.tg_alloc_v     := stAl.flatten.reduce(_ || _)
    s.tg_alloc_table := OHToUInt(VecInit(stAl.map(_.reduce(_ || _))).asUInt)
    s.tg_alloc_slot  := OHToUInt(VecInit((0 until bankWidth).map(w => stAl.map(_(w)).reduce(_ || _))).asUInt)
    val stUd = (0 until tageNTables).map(i => (0 until bankWidth).map(w => stU(i)(w) && !stM(i)(w)))
    s.tg_udec_mask := VecInit(stUd.map(_.reduce(_ || _))).asUInt
    s.tg_udec_slot := OHToUInt(VecInit((0 until bankWidth).map(w => stUd.map(_(w)).reduce(_ || _))).asUInt)
    if (ENABLE_CF_DEBUG_PRINTF && ENABLE_IFT) {
      for (w <- 0 until bankWidth) { assert(PopCount(stProvOH(w)) <= 1.U, "[V3TAP-TAGE] two provider tables trained in one slot") }
      assert(PopCount(stAl.flatten) <= 1.U, "[V3TAP-TAGE] more than one TAGE allocation in one bank")
      assert(PopCount((0 until bankWidth).map(w => stUd.map(_(w)).reduce(_ || _))) <= 1.U, "[V3TAP-TAGE] u-only writes in two slots")
    }
  }
  //io.f3_meta := Cat(f3_meta.asUInt, micro.io.f3_meta(micro.metaSz-1,0), base.io.f3_meta(base.metaSz-1, 0))
  io.f3_meta := f3_meta.asUInt
}
