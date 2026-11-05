package boom.v3.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._

import boom.v3.common._
import boom.v3.util.{BoomCoreStringPrefix}




// A branch prediction for a single instruction
class BranchPrediction(implicit p: Parameters) extends BoomBundle()(p)
{
  // If this is a branch, do we take it?
  val taken           = Bool()

  // Is this a branch?
  val is_br           = Bool()
  // Is this a JAL?
  val is_jal          = Bool()
  // What is the target of his branch/jump? Do we know the target?
  val predicted_pc    = Valid(UInt(vaddrBitsExtended.W))


}

// A branch prediction for a entire fetch-width worth of instructions
// This is typically merged from individual predictions from the banked
// predictor
class BranchPredictionBundle(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val pc = UInt(vaddrBitsExtended.W)
  val preds = Vec(fetchWidth, new BranchPrediction)
  val meta = Output(Vec(nBanks, UInt(bpdMaxMetaLength.W)))
  val lhist = Output(Vec(nBanks, UInt(localHistoryLength.W)))
  // corefuzzing: TAGE/BTB entry was last updated by a different domain
  val bpd_domain_mismatch = if (ENABLE_IFT) Some(Bool()) else None
  val btb_domain_mismatch = if (ENABLE_IFT) Some(Bool()) else None
  // corefuzzing: TAGE/BTB entry was trained by a secret-dependent instruction
  val bpd_secret_mismatch = if (ENABLE_IFT) Some(Bool()) else None
  // [ICPREDF3-TAGE] the part of bpd_*_mismatch from the banks that are not f3-only (BIM): the s1 / s2 request label
  // (frontend.scala cf_f3_pred_*_pre).  bpd_*_mismatch stays the full OR (fetch buffer, f3 redirect target).
  val bpd_secret_early    = if (ENABLE_IFT) Some(Bool()) else None
  val bpd_domain_early    = if (ENABLE_IFT) Some(Bool()) else None
  val btb_secret_mismatch = if (ENABLE_IFT) Some(Bool()) else None
  // [BTBDISP] step 16: displacement-filter hit and PC[13:2] of the secret allocator
  // [R124GATE] patch 26 (ruling 124): the three BTB-displacement fields exist only with
  // ENABLE_IFT (None otherwise), so the IFT-off Queue1_BranchPredictionBundle ram loses their
  // 14 bits.  Same names and order when present, so the IFT-on Verilog is unchanged.
  val btb_sec_disp        = if (ENABLE_IFT) Some(Bool()) else None
  val btb_sec_pc          = if (ENABLE_IFT) Some(UInt(12.W)) else None
  // [BTBATK] DF-btbatk: displacement-filter hit on an ATTACKER record (PC in btb_sec_pc when no
  // secret edge fires; btb.scala [BTBATK])
  val btb_atk_disp        = if (ENABLE_IFT) Some(Bool()) else None
}


// A branch update for a fetch-width worth of instructions
class BranchPredictionUpdate(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  // Indicates that this update is due to a speculated misprediction
  // Local predictors typically update themselves with speculative info
  // Global predictors only care about non-speculative updates
  val is_mispredict_update = Bool()
  val is_repair_update = Bool()
  val btb_mispredicts = UInt(fetchWidth.W)
  def is_btb_mispredict_update = btb_mispredicts =/= 0.U
  def is_commit_update = !(is_mispredict_update || is_repair_update || is_btb_mispredict_update)

  val pc            = UInt(vaddrBitsExtended.W)
  // Mask of instructions which are branches.
  // If these are not cfi_idx, then they were predicted not taken
  val br_mask       = UInt(fetchWidth.W)
  // Which CFI was taken/mispredicted (if any)
  val cfi_idx       = Valid(UInt(log2Ceil(fetchWidth).W))
  // Was the cfi taken?
  val cfi_taken     = Bool()
  // Was the cfi mispredicted from the original prediction?
  val cfi_mispredicted = Bool()
  // Was the cfi a br?
  val cfi_is_br     = Bool()
  // Was the cfi a jal/jalr?
  val cfi_is_jal  = Bool()
  // Was the cfi a jalr
  val cfi_is_jalr = Bool()
  //val cfi_is_ret  = Bool()

  val ghist = new GlobalHistory
  val lhist = Vec(nBanks, UInt(localHistoryLength.W))


  // What did this CFI jump to?
  val target        = UInt(vaddrBitsExtended.W)

  val meta          = Vec(nBanks, UInt(bpdMaxMetaLength.W))

  // corefuzzing: domain of the fetch packet that generated this update
  val cf_domain_id  = if (ENABLE_IFT) Some(UInt(1.W)) else None
  // corefuzzing: fetch packet contained a secret-dependent instruction (s_acc or s_prop)
  val cf_is_secret  = if (ENABLE_IFT) Some(Bool()) else None
  // [SPECBPU-REP] the undo-log record id: the record a resolution update allocated, or the record a repair / relabel
  // update undoes / relabels (FTQ [SPECBPU-REP]); unused on the commit bus (DontCare there)
  val cf_log_id     = UInt(cfSpecLogW.W)
  // [SPECBPU-RLB] a relabel update (it also has is_repair_update = 1): rewrite only the labels of what record
  // cf_log_id wrote
  val cf_relabel    = if (CF_SPEC_RELABEL) Some(Bool()) else None
}

// A branch update to a single bank
class BranchPredictionBankUpdate(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val is_mispredict_update     = Bool()
  val is_repair_update         = Bool()

  val btb_mispredicts  = UInt(bankWidth.W)
  def is_btb_mispredict_update = btb_mispredicts =/= 0.U

  def is_commit_update = !(is_mispredict_update || is_repair_update || is_btb_mispredict_update)

  val pc               = UInt(vaddrBitsExtended.W)

  val br_mask          = UInt(bankWidth.W)
  val cfi_idx          = Valid(UInt(log2Ceil(bankWidth).W))
  val cfi_taken        = Bool()
  val cfi_mispredicted = Bool()

  val cfi_is_br        = Bool()
  val cfi_is_jal       = Bool()
  val cfi_is_jalr      = Bool()

  val ghist            = UInt(globalHistoryLength.W)
  val lhist            = UInt(localHistoryLength.W)

  val target           = UInt(vaddrBitsExtended.W)

  val meta             = UInt(bpdMaxMetaLength.W)

  // corefuzzing: domain of the fetch packet that generated this update
  val cf_domain_id     = if (ENABLE_IFT) Some(UInt(1.W)) else None
  // corefuzzing: fetch packet contained a secret-dependent instruction
  val cf_is_secret     = if (ENABLE_IFT) Some(Bool()) else None
  // [SPECBPU-REP] / [SPECBPU-RLB] see BranchPredictionUpdate
  val cf_log_id        = UInt(cfSpecLogW.W)
  val cf_relabel       = if (CF_SPEC_RELABEL) Some(Bool()) else None
}

class BranchPredictionRequest(implicit p: Parameters) extends BoomBundle()(p)
{
  val pc    = UInt(vaddrBitsExtended.W)
  val ghist = new GlobalHistory
}


class BranchPredictionBankResponse(implicit p: Parameters) extends BoomBundle()(p)
  with HasBoomFrontendParameters
{
  val f1 = Vec(bankWidth, new BranchPrediction)
  val f2 = Vec(bankWidth, new BranchPrediction)
  val f3 = Vec(bankWidth, new BranchPrediction)
}

/** [BTBPICK] ruling 146 (v), review M1: two-tier BTB PC pick over BTB-type sources (components
  * in a bank, or the banks of a packet), in priority order.  The first source with a SECRET BTB
  * edge (secret-trainer hit or secret record) wins; else the first source with ANY BTB edge
  * (attacker record); else the last source (default: the PC is read only when fetch_flags bit 7
  * or 8 fires).  Each source's own PC is already secret-first (btb.scala [BTBATK]).  Two sources:
  * choose the second = !sec0 && (sec1 || !any0), one shared LUT5 select + a 2:1 mux per bit
  * (+1 LUT level against the old 1-tier pick; registered inputs, f3_bpd_resp queue storage after
  * the predictor-level pick). */
object CfBtbPcPick {
  def apply(sec: Seq[Bool], any: Seq[Bool], pcs: Seq[UInt]): UInt = {
    require(sec.size == pcs.size && any.size == pcs.size && pcs.nonEmpty)
    if (pcs.size == 1) pcs.head
    else PriorityMux(sec ++ any.init :+ true.B, pcs ++ pcs.init :+ pcs.last)
  }
}

abstract class BranchPredictorBank(implicit p: Parameters) extends BoomModule()(p)
  with HasBoomFrontendParameters
{
  val metaSz = 0
  def nInputs = 1
  // [BTBATK] DF-btbatk: Scala-time flag, true only for the banks that drive f3_btb_sec_pc (BTB,
  // uBTB), so the composer's PC select sees only them (no constant-zero default term).
  def cfDrivesBtbPc: Boolean = false
  // [ICPREDF3-TAGE] (REVIEW-f3ras F4): Scala-time flag, true for the banks whose bpd_{secret,domain}_mismatch is known only
  // at f3 (TAGE, loop: table / column hit read in s2, flopped for f3).  The composer leaves them out of f3_bpd_*_early.
  def cfBpdF3Only: Boolean = false
  // [SPECBPU] (owner 2026-11-02, RULES C 'Speculative TAGE/BTB updates' + 'Speculative BPU follow-ups' O1 (a)): true
  // for the components that train at branch resolution (TAGE, BTB, BIM, uBTB): their update stage reads
  // io.spec_update (the FTQ's resolution-time update, one resolved CFI per update) instead of io.update (the FTQ
  // commit update + f4 fast repair, now read only by the loop predictor, which writes only on the mispredict /
  // repair updates of the disabled FTQ repair walk, so it stays inert as before).
  def trainsAtResolution: Boolean = false
  // [SPECBPU] the meta bits [specMetaLo, specMetaLo + specMetaW) the resolution update must carry (the composer
  // overrides it with the slice of its resolution-trained components); the FTQ stores only that slice.
  def specMetaLo: Int = 0
  def specMetaW: Int = 0

  val mems: Seq[Tuple3[String, Int, Int]]

  val io = IO(new Bundle {
    val f0_valid = Input(Bool())
    val f0_pc    = Input(UInt(vaddrBitsExtended.W))
    val f0_mask  = Input(UInt(bankWidth.W))
    // Local history not available until end of f1
    val f1_ghist = Input(UInt(globalHistoryLength.W))
    val f1_lhist = Input(UInt(localHistoryLength.W))

    val resp_in = Input(Vec(nInputs, new BranchPredictionBankResponse))
    val resp = Output(new BranchPredictionBankResponse)

    // Store the meta as a UInt, use width inference to figure out the shape
    val f3_meta = Output(UInt(bpdMaxMetaLength.W))

    val f3_fire = Input(Bool())

    val update = Input(Valid(new BranchPredictionBankUpdate))
    // [SPECBPU] the resolution-time update (read by the components with trainsAtResolution)
    val spec_update = Input(Valid(new BranchPredictionBankUpdate))

    // added for the fuzzycore project - AK
    val cf_bpd_tage_to_gshare = Input(Bool()) // signal from the custom CSR to reconfigure to gshare.

    // corefuzzing: runtime BTB/TAGE reconfiguration indices
    val cf_btb_set_idx    = Input(UInt(2.W))
    val cf_btb_way_idx    = Input(UInt(1.W))
    val cf_tage_count_idx = Input(UInt(3.W))
    // corefuzzing: suppress BTB predictions during quiesce fetch window
    val cf_btb_quiesce    = Input(Bool())

    // corefuzzing: fetch domain tracking for BPD/BTB domain mismatch detection
    val f0_domain_id           = Input(UInt(1.W))   // domain of current fetch (f0 stage); IFT off only (unused IFT on)
    // [F4S1RNG] lean58 timing F4: IFT on, the fetch domain arrives at s1, computed by the frontend from s1_vpc and a
    // registered copy of the range CSRs (frontend.scala) instead of from s0_vpc.  None IFT off.
    val f1_domain_id           = if (ENABLE_IFT) Some(Input(UInt(1.W))) else None
    val f3_bpd_domain_mismatch = Output(Bool())     // TAGE entry last updated by different domain
    val f3_btb_domain_mismatch = Output(Bool())     // BTB entry last updated by different domain
    // corefuzzing: secret shadow mismatch — predictor entry was trained by secret instruction
    val f3_bpd_secret_mismatch = Output(Bool())
    val f3_btb_secret_mismatch = Output(Bool())
    // [ICPREDF3-TAGE] composer only: bpd mismatch of the non-f3-only components (None IFT off; every other bank drives 0)
    val f3_bpd_secret_early    = if (ENABLE_IFT) Some(Output(Bool())) else None
    val f3_bpd_domain_early    = if (ENABLE_IFT) Some(Output(Bool())) else None
    // [BTBDISP] step 16 (owner ruling 110): the fetch hit the tag a secret allocation displaced
    // (BTB per set / uBTB one entry), and PC[13:2] of the secret allocator behind
    // f3_btb_secret_mismatch || f3_btb_sec_disp.  Edge only (FetchIftInfo bit 7, BTB_STATE sec).
    val f3_btb_sec_disp        = Output(Bool())
    val f3_btb_sec_pc          = Output(UInt(12.W))
    // [BTBATK] DF-btbatk: the fetch hit the tag an ATTACKER-domain allocation displaced; its PC is
    // f3_btb_sec_pc when neither f3_btb_secret_mismatch nor f3_btb_sec_disp fires.
    val f3_btb_atk_disp        = Output(Bool())
    // [V3TAP-PRED] SP-19 (design 5.1 PRED): this bank's write-stage taps aligned to the TAGE table-write cycle.  The
    // BTB, uBTB and TAGE components drive their own fields; every other component (and the base) drives 0.
    // Record/STATE builds only.
    val cf_st = if (ENABLE_IFT_BRIDGE || IFT_ORACLE) Some(Output(new IftPredBankTap)) else None
  })
  io.resp := io.resp_in(0)

  io.f3_meta := 0.U
  // corefuzzing: default to no mismatch; overridden by concrete banks
  io.f3_bpd_domain_mismatch := false.B
  io.f3_btb_domain_mismatch := false.B
  io.f3_bpd_secret_mismatch := false.B
  io.f3_btb_secret_mismatch := false.B
  io.f3_bpd_secret_early.foreach(_ := false.B)
  io.f3_bpd_domain_early.foreach(_ := false.B)
  io.f3_btb_sec_disp        := false.B
  io.f3_btb_sec_pc          := 0.U
  io.f3_btb_atk_disp        := false.B
  io.cf_st.foreach(_ := 0.U.asTypeOf(new IftPredBankTap))   // [V3TAP-PRED] default; BTB / uBTB / TAGE override fields

  val s0_idx       = fetchIdx(io.f0_pc)
  val s1_idx       = RegNext(s0_idx)
  val s2_idx       = RegNext(s1_idx)
  val s3_idx       = RegNext(s2_idx)

  val s0_valid = io.f0_valid
  val s1_valid = RegNext(s0_valid)
  val s2_valid = RegNext(s1_valid)
  val s3_valid = RegNext(s2_valid)

  val s0_mask = io.f0_mask
  val s1_mask = RegNext(s0_mask)
  val s2_mask = RegNext(s1_mask)
  val s3_mask = RegNext(s2_mask)

  val s0_pc = io.f0_pc
  val s1_pc = RegNext(s0_pc)

  // [SPECBPU] a resolution-trained component (TAGE, BTB, BIM, uBTB) takes its training from the resolution bus; the
  // update logic below and in the component is unchanged (a resolution update is a one-CFI packet update with
  // is_commit_update = 1).  The other bus is unread there (its s1 flops are removed by DCE).
  private val cf_upd_bus = if (trainsAtResolution) io.spec_update else io.update
  val s0_update     = cf_upd_bus
  val s0_update_idx = fetchIdx(cf_upd_bus.bits.pc)
  val s0_update_valid = cf_upd_bus.valid

  val s1_update     = RegNext(s0_update)
  val s1_update_idx = RegNext(s0_update_idx)
  val s1_update_valid = RegNext(s0_update_valid)

  // corefuzzing: fetch domain piped one cycle (f0→s1) for use in domain shadow comparisons
  // [F4S1RNG] lean58 timing F4: IFT on, the s1 domain is the frontend's s1 compare (io.f1_domain_id), equal every
  // cycle to the old per-bank RegNext(io.f0_domain_id) (proof in frontend.scala [F4S1RNG]); the per-component f0->s1
  // flops are gone.  IFT off: constant 0 as before.
  val s1_domain = if (ENABLE_IFT) io.f1_domain_id.get else 0.U   // [R124GATE3] patch 28

}



class BranchPredictor(implicit p: Parameters) extends BoomModule()(p)
 with HasBoomFrontendParameters
{
  val io = IO(new Bundle {

    // Requests and responses
    val f0_req = Input(Valid(new BranchPredictionRequest))

    val resp = Output(new Bundle {
      val f1 = new BranchPredictionBundle
      val f2 = new BranchPredictionBundle
      val f3 = new BranchPredictionBundle
    })

    val f3_fire = Input(Bool())

    // Update
    val update = Input(Valid(new BranchPredictionUpdate))
    // [SPECBPU] the FTQ's resolution-time update (TAGE / BTB / BIM / uBTB)
    val spec_update = Input(Valid(new BranchPredictionUpdate))

    // adding for core fuzzing - AK
    val cf_bpd_tage_to_gshare = Input(Bool())

    // corefuzzing: runtime BTB/TAGE reconfiguration indices
    val cf_btb_set_idx    = Input(UInt(2.W))
    val cf_btb_way_idx    = Input(UInt(1.W))
    val cf_tage_count_idx = Input(UInt(3.W))
    // corefuzzing: suppress BTB predictions during quiesce fetch window
    val cf_btb_quiesce    = Input(Bool())

    // corefuzzing: domain of the fetch request for BPD/BTB domain shadow comparisons
    val f0_req_domain_id = Input(UInt(1.W))   // IFT off only (IFT on: tied 0 by the frontend, unused)
    val f1_req_domain_id = if (ENABLE_IFT) Some(Input(UInt(1.W))) else None   // [F4S1RNG] lean58: the s1 fetch domain
    // [V3TAP-PRED] SP-19: per PHYSICAL bank b (banked_predictors(b)) write-stage taps, aligned to the TAGE write cycle.
    val cf_st = if (ENABLE_IFT_BRIDGE || IFT_ORACLE) Some(Output(Vec(nBanks, new IftPredBankTap))) else None
  })

  var total_memsize = 0
  val bpdStr = new StringBuilder
  bpdStr.append(BoomCoreStringPrefix("==Branch Predictor Memory Sizes==\n"))
  val banked_predictors = (0 until nBanks) map ( b => {
    val m = Module(if (useBPD) new ComposedBranchPredictorBank else new NullBranchPredictorBank)
    for ((n, d, w) <- m.mems) {
      bpdStr.append(BoomCoreStringPrefix(f"bank$b $n: $d x $w = ${d * w / 8}"))
      total_memsize = total_memsize + d * w / 8
    }
    m
  })
  // [SPECBPU] the meta slice the FTQ keeps for the resolution update (both banks are built alike)
  val specMetaLo = banked_predictors(0).specMetaLo
  val specMetaW  = banked_predictors(0).specMetaW
  bpdStr.append(BoomCoreStringPrefix(f"Total bpd size: ${total_memsize / 1024} KB\n"))
  override def toString: String = bpdStr.toString

  val banked_lhist_providers = Seq.fill(nBanks) { Module(if (localHistoryNSets > 0) new LocalBranchPredictorBank else new NullLocalBranchPredictorBank) }


  if (nBanks == 1) {
    banked_lhist_providers(0).io.f0_valid := io.f0_req.valid
    banked_lhist_providers(0).io.f0_pc    := bankAlign(io.f0_req.bits.pc)

    banked_predictors(0).io.f0_valid := io.f0_req.valid
    banked_predictors(0).io.f0_pc    := bankAlign(io.f0_req.bits.pc)
    banked_predictors(0).io.f0_mask  := fetchMask(io.f0_req.bits.pc)

    banked_predictors(0).io.f1_ghist := RegNext(io.f0_req.bits.ghist.histories(0))
    banked_predictors(0).io.f1_lhist := banked_lhist_providers(0).io.f1_lhist

    banked_predictors(0).io.resp_in(0)           := (0.U).asTypeOf(new BranchPredictionBankResponse)

    // adding for the fuzzycore project - AK
    banked_predictors(0).io.cf_bpd_tage_to_gshare := io.cf_bpd_tage_to_gshare
    // corefuzzing: runtime BTB/TAGE reconfig
    banked_predictors(0).io.cf_btb_set_idx    := io.cf_btb_set_idx
    banked_predictors(0).io.cf_btb_way_idx    := io.cf_btb_way_idx
    banked_predictors(0).io.cf_tage_count_idx := io.cf_tage_count_idx
    banked_predictors(0).io.cf_btb_quiesce    := io.cf_btb_quiesce

  } else {
    require(nBanks == 2)

    banked_predictors(0).io.resp_in(0)           := (0.U).asTypeOf(new BranchPredictionBankResponse)
    banked_predictors(1).io.resp_in(0)           := (0.U).asTypeOf(new BranchPredictionBankResponse)

    banked_predictors(0).io.f1_lhist := banked_lhist_providers(0).io.f1_lhist
    banked_predictors(1).io.f1_lhist := banked_lhist_providers(1).io.f1_lhist

    // adding for fuzzycore - AK
    banked_predictors(0).io.cf_bpd_tage_to_gshare := io.cf_bpd_tage_to_gshare
    banked_predictors(1).io.cf_bpd_tage_to_gshare := io.cf_bpd_tage_to_gshare
    // corefuzzing: runtime BTB/TAGE reconfig
    banked_predictors(0).io.cf_btb_set_idx    := io.cf_btb_set_idx
    banked_predictors(1).io.cf_btb_set_idx    := io.cf_btb_set_idx
    banked_predictors(0).io.cf_btb_way_idx    := io.cf_btb_way_idx
    banked_predictors(1).io.cf_btb_way_idx    := io.cf_btb_way_idx
    banked_predictors(0).io.cf_tage_count_idx := io.cf_tage_count_idx
    banked_predictors(1).io.cf_tage_count_idx := io.cf_tage_count_idx
    banked_predictors(0).io.cf_btb_quiesce    := io.cf_btb_quiesce
    banked_predictors(1).io.cf_btb_quiesce    := io.cf_btb_quiesce

    when (bank(io.f0_req.bits.pc) === 0.U) {
      banked_lhist_providers(0).io.f0_valid := io.f0_req.valid
      banked_lhist_providers(0).io.f0_pc    := bankAlign(io.f0_req.bits.pc)

      banked_lhist_providers(1).io.f0_valid := io.f0_req.valid
      banked_lhist_providers(1).io.f0_pc    := nextBank(io.f0_req.bits.pc)

      banked_predictors(0).io.f0_valid := io.f0_req.valid
      banked_predictors(0).io.f0_pc    := bankAlign(io.f0_req.bits.pc)
      banked_predictors(0).io.f0_mask  := fetchMask(io.f0_req.bits.pc)

      banked_predictors(1).io.f0_valid := io.f0_req.valid
      banked_predictors(1).io.f0_pc    := nextBank(io.f0_req.bits.pc)
      banked_predictors(1).io.f0_mask  := ~(0.U(bankWidth.W))
    } .otherwise {
      banked_lhist_providers(0).io.f0_valid := io.f0_req.valid && !mayNotBeDualBanked(io.f0_req.bits.pc)
      banked_lhist_providers(0).io.f0_pc    := nextBank(io.f0_req.bits.pc)

      banked_lhist_providers(1).io.f0_valid := io.f0_req.valid
      banked_lhist_providers(1).io.f0_pc    := bankAlign(io.f0_req.bits.pc)

      banked_predictors(0).io.f0_valid := io.f0_req.valid && !mayNotBeDualBanked(io.f0_req.bits.pc)
      banked_predictors(0).io.f0_pc    := nextBank(io.f0_req.bits.pc)
      banked_predictors(0).io.f0_mask  := ~(0.U(bankWidth.W))

      banked_predictors(1).io.f0_valid := io.f0_req.valid
      banked_predictors(1).io.f0_pc    := bankAlign(io.f0_req.bits.pc)
      banked_predictors(1).io.f0_mask  := fetchMask(io.f0_req.bits.pc)
    }
    when (RegNext(bank(io.f0_req.bits.pc) === 0.U)) {
      banked_predictors(0).io.f1_ghist  := RegNext(io.f0_req.bits.ghist.histories(0))
      banked_predictors(1).io.f1_ghist  := RegNext(io.f0_req.bits.ghist.histories(1))
    } .otherwise {
      banked_predictors(0).io.f1_ghist  := RegNext(io.f0_req.bits.ghist.histories(1))
      banked_predictors(1).io.f1_ghist  := RegNext(io.f0_req.bits.ghist.histories(0))
    }
  }


  for (i <- 0 until nBanks) {
    banked_lhist_providers(i).io.f3_taken_br := banked_predictors(i).io.resp.f3.map ( p =>
      p.is_br && p.predicted_pc.valid && p.taken
    ).reduce(_||_)
  }

  if (nBanks == 1) {
    io.resp.f1.preds    := banked_predictors(0).io.resp.f1
    io.resp.f2.preds    := banked_predictors(0).io.resp.f2
    io.resp.f3.preds    := banked_predictors(0).io.resp.f3
    io.resp.f3.meta(0)  := banked_predictors(0).io.f3_meta
    io.resp.f3.lhist(0) := banked_lhist_providers(0).io.f3_lhist

    banked_predictors(0).io.f3_fire := io.f3_fire
    banked_lhist_providers(0).io.f3_fire := io.f3_fire
  } else {
    require(nBanks == 2)
    val b0_fire = io.f3_fire && RegNext(RegNext(RegNext(banked_predictors(0).io.f0_valid)))
    val b1_fire = io.f3_fire && RegNext(RegNext(RegNext(banked_predictors(1).io.f0_valid)))
    banked_predictors(0).io.f3_fire := b0_fire
    banked_predictors(1).io.f3_fire := b1_fire

    banked_lhist_providers(0).io.f3_fire := b0_fire
    banked_lhist_providers(1).io.f3_fire := b1_fire



    // The branch prediction metadata is stored un-shuffled
    io.resp.f3.meta(0)    := banked_predictors(0).io.f3_meta
    io.resp.f3.meta(1)    := banked_predictors(1).io.f3_meta

    io.resp.f3.lhist(0)   := banked_lhist_providers(0).io.f3_lhist
    io.resp.f3.lhist(1)   := banked_lhist_providers(1).io.f3_lhist

    when (bank(io.resp.f1.pc) === 0.U) {
      for (i <- 0 until bankWidth) {
        io.resp.f1.preds(i)           := banked_predictors(0).io.resp.f1(i)
        io.resp.f1.preds(i+bankWidth) := banked_predictors(1).io.resp.f1(i)
      }
    } .otherwise {
      for (i <- 0 until bankWidth) {
        io.resp.f1.preds(i)           := banked_predictors(1).io.resp.f1(i)
        io.resp.f1.preds(i+bankWidth) := banked_predictors(0).io.resp.f1(i)
      }
    }

    when (bank(io.resp.f2.pc) === 0.U) {
      for (i <- 0 until bankWidth) {
        io.resp.f2.preds(i)           := banked_predictors(0).io.resp.f2(i)
        io.resp.f2.preds(i+bankWidth) := banked_predictors(1).io.resp.f2(i)
      }
    } .otherwise {
      for (i <- 0 until bankWidth) {
        io.resp.f2.preds(i)           := banked_predictors(1).io.resp.f2(i)
        io.resp.f2.preds(i+bankWidth) := banked_predictors(0).io.resp.f2(i)
      }
    }

    when (bank(io.resp.f3.pc) === 0.U) {
      for (i <- 0 until bankWidth) {
        io.resp.f3.preds(i)           := banked_predictors(0).io.resp.f3(i)
        io.resp.f3.preds(i+bankWidth) := banked_predictors(1).io.resp.f3(i)
      }
    } .otherwise {
      for (i <- 0 until bankWidth) {
        io.resp.f3.preds(i)           := banked_predictors(1).io.resp.f3(i)
        io.resp.f3.preds(i+bankWidth) := banked_predictors(0).io.resp.f3(i)
      }
    }
  }

  io.resp.f1.pc := RegNext(io.f0_req.bits.pc)
  io.resp.f2.pc := RegNext(io.resp.f1.pc)
  io.resp.f3.pc := RegNext(io.resp.f2.pc)

  // We don't care about meta from the f1 and f2 resps
  // Use the meta from the latest resp
  io.resp.f1.meta := DontCare
  io.resp.f2.meta := DontCare
  io.resp.f1.lhist := DontCare
  io.resp.f2.lhist := DontCare
  // corefuzzing: domain mismatch only meaningful at f3; f1/f2 set to false
  io.resp.f1.bpd_domain_mismatch.foreach(_ := false.B)
  io.resp.f2.bpd_domain_mismatch.foreach(_ := false.B)
  io.resp.f1.btb_domain_mismatch.foreach(_ := false.B)
  io.resp.f2.btb_domain_mismatch.foreach(_ := false.B)


  for (i <- 0 until nBanks) {
    banked_predictors(i).io.update.bits.is_mispredict_update := io.update.bits.is_mispredict_update
    banked_predictors(i).io.update.bits.is_repair_update     := io.update.bits.is_repair_update

    banked_predictors(i).io.update.bits.meta             := io.update.bits.meta(i)
    banked_predictors(i).io.update.bits.lhist            := io.update.bits.lhist(i)
    banked_predictors(i).io.update.bits.cfi_idx.bits     := io.update.bits.cfi_idx.bits
    banked_predictors(i).io.update.bits.cfi_taken        := io.update.bits.cfi_taken
    banked_predictors(i).io.update.bits.cfi_mispredicted := io.update.bits.cfi_mispredicted
    banked_predictors(i).io.update.bits.cfi_is_br        := io.update.bits.cfi_is_br
    banked_predictors(i).io.update.bits.cfi_is_jal       := io.update.bits.cfi_is_jal
    banked_predictors(i).io.update.bits.cfi_is_jalr      := io.update.bits.cfi_is_jalr
    banked_predictors(i).io.update.bits.target           := io.update.bits.target
    banked_predictors(i).io.update.bits.cf_log_id        := io.update.bits.cf_log_id   // [SPECBPU-REP] unused here
    banked_predictors(i).io.update.bits.cf_relabel.foreach(_ := false.B)                 // [SPECBPU-RLB] never here

    banked_lhist_providers(i).io.update.mispredict := io.update.bits.is_mispredict_update
    banked_lhist_providers(i).io.update.repair     := io.update.bits.is_repair_update
    banked_lhist_providers(i).io.update.lhist      := io.update.bits.lhist(i)
  }

  if (nBanks == 1) {
    banked_predictors(0).io.update.valid                 := io.update.valid
    banked_predictors(0).io.update.bits.pc               := bankAlign(io.update.bits.pc)
    banked_predictors(0).io.update.bits.br_mask          := io.update.bits.br_mask
    banked_predictors(0).io.update.bits.btb_mispredicts  := io.update.bits.btb_mispredicts
    banked_predictors(0).io.update.bits.cfi_idx.valid    := io.update.bits.cfi_idx.valid
    banked_predictors(0).io.update.bits.ghist            := io.update.bits.ghist.histories(0)

    banked_lhist_providers(0).io.update.valid := io.update.valid && io.update.bits.br_mask =/= 0.U
    banked_lhist_providers(0).io.update.pc    := bankAlign(io.update.bits.pc)
  } else {
    require(nBanks == 2)
    // Split the single update bundle for the fetchpacket into two updates
    // 1 for each bank.

    when (bank(io.update.bits.pc) === 0.U) {
      val b1_update_valid = io.update.valid &&
        (!io.update.bits.cfi_idx.valid || io.update.bits.cfi_idx.bits >= bankWidth.U)

      banked_lhist_providers(0).io.update.valid := io.update.valid && io.update.bits.br_mask(bankWidth-1,0) =/= 0.U
      banked_lhist_providers(1).io.update.valid := b1_update_valid && io.update.bits.br_mask(fetchWidth-1,bankWidth) =/= 0.U

      banked_lhist_providers(0).io.update.pc := bankAlign(io.update.bits.pc)
      banked_lhist_providers(1).io.update.pc := nextBank(io.update.bits.pc)

      banked_predictors(0).io.update.valid := io.update.valid
      banked_predictors(1).io.update.valid := b1_update_valid

      banked_predictors(0).io.update.bits.pc := bankAlign(io.update.bits.pc)
      banked_predictors(1).io.update.bits.pc := nextBank(io.update.bits.pc)

      banked_predictors(0).io.update.bits.br_mask := io.update.bits.br_mask
      banked_predictors(1).io.update.bits.br_mask := io.update.bits.br_mask >> bankWidth

      banked_predictors(0).io.update.bits.btb_mispredicts  := io.update.bits.btb_mispredicts
      banked_predictors(1).io.update.bits.btb_mispredicts  := io.update.bits.btb_mispredicts >> bankWidth

      banked_predictors(0).io.update.bits.cfi_idx.valid := io.update.bits.cfi_idx.valid && io.update.bits.cfi_idx.bits < bankWidth.U
      banked_predictors(1).io.update.bits.cfi_idx.valid := io.update.bits.cfi_idx.valid && io.update.bits.cfi_idx.bits >= bankWidth.U

      banked_predictors(0).io.update.bits.ghist := io.update.bits.ghist.histories(0)
      banked_predictors(1).io.update.bits.ghist := io.update.bits.ghist.histories(1)
    } .otherwise {
      val b0_update_valid = io.update.valid && !mayNotBeDualBanked(io.update.bits.pc) &&
        (!io.update.bits.cfi_idx.valid || io.update.bits.cfi_idx.bits >= bankWidth.U)

      banked_lhist_providers(1).io.update.valid := io.update.valid && io.update.bits.br_mask(bankWidth-1,0) =/= 0.U
      banked_lhist_providers(0).io.update.valid := b0_update_valid && io.update.bits.br_mask(fetchWidth-1,bankWidth) =/= 0.U

      banked_lhist_providers(1).io.update.pc := bankAlign(io.update.bits.pc)
      banked_lhist_providers(0).io.update.pc := nextBank(io.update.bits.pc)

      banked_predictors(1).io.update.valid := io.update.valid
      banked_predictors(0).io.update.valid := b0_update_valid

      banked_predictors(1).io.update.bits.pc := bankAlign(io.update.bits.pc)
      banked_predictors(0).io.update.bits.pc := nextBank(io.update.bits.pc)

      banked_predictors(1).io.update.bits.br_mask := io.update.bits.br_mask
      banked_predictors(0).io.update.bits.br_mask := io.update.bits.br_mask >> bankWidth

      banked_predictors(1).io.update.bits.btb_mispredicts  := io.update.bits.btb_mispredicts
      banked_predictors(0).io.update.bits.btb_mispredicts  := io.update.bits.btb_mispredicts >> bankWidth

      banked_predictors(1).io.update.bits.cfi_idx.valid := io.update.bits.cfi_idx.valid && io.update.bits.cfi_idx.bits < bankWidth.U
      banked_predictors(0).io.update.bits.cfi_idx.valid := io.update.bits.cfi_idx.valid && io.update.bits.cfi_idx.bits >= bankWidth.U

      banked_predictors(1).io.update.bits.ghist := io.update.bits.ghist.histories(0)
      banked_predictors(0).io.update.bits.ghist := io.update.bits.ghist.histories(1)
    }

  }

  // [SPECBPU] the resolution-time bus, split per bank exactly as io.update is above (a one-CFI update: the bank
  // holding the slot gets it; the other bank gets br_mask 0 / cfi_idx invalid, i.e. no TAGE or BTB write, or no
  // valid at all).  Levels: the update flops (FTQ, plus its target adder) -> bank(pc) 2:1 mux (1 LUT) -> s1 flops.
  for (i <- 0 until nBanks) {
    val d = banked_predictors(i).io.spec_update
    d.bits.is_mispredict_update := io.spec_update.bits.is_mispredict_update
    d.bits.is_repair_update     := io.spec_update.bits.is_repair_update
    d.bits.meta                 := io.spec_update.bits.meta(i)
    d.bits.lhist                := io.spec_update.bits.lhist(i)
    d.bits.cfi_idx.bits         := io.spec_update.bits.cfi_idx.bits
    d.bits.cfi_taken            := io.spec_update.bits.cfi_taken
    d.bits.cfi_mispredicted     := io.spec_update.bits.cfi_mispredicted
    d.bits.cfi_is_br            := io.spec_update.bits.cfi_is_br
    d.bits.cfi_is_jal           := io.spec_update.bits.cfi_is_jal
    d.bits.cfi_is_jalr          := io.spec_update.bits.cfi_is_jalr
    d.bits.target               := io.spec_update.bits.target
    d.bits.cf_log_id            := io.spec_update.bits.cf_log_id                         // [SPECBPU-REP]
    d.bits.cf_relabel.foreach(_ := io.spec_update.bits.cf_relabel.get)                    // [SPECBPU-RLB]
    d.bits.cf_domain_id.foreach(_ := io.spec_update.bits.cf_domain_id.getOrElse(0.U(1.W)))
    d.bits.cf_is_secret.foreach(_ := io.spec_update.bits.cf_is_secret.getOrElse(false.B))
  }
  if (nBanks == 1) {
    val d = banked_predictors(0).io.spec_update
    d.valid                := io.spec_update.valid
    d.bits.pc              := bankAlign(io.spec_update.bits.pc)
    d.bits.br_mask         := io.spec_update.bits.br_mask
    d.bits.btb_mispredicts := io.spec_update.bits.btb_mispredicts
    d.bits.cfi_idx.valid   := io.spec_update.bits.cfi_idx.valid
    d.bits.ghist           := io.spec_update.bits.ghist.histories(0)
  } else {
    val u  = io.spec_update
    val cv = u.bits.cfi_idx.valid
    val ci = u.bits.cfi_idx.bits
    val d0 = banked_predictors(0).io.spec_update
    val d1 = banked_predictors(1).io.spec_update
    when (bank(u.bits.pc) === 0.U) {
      d0.valid                := u.valid
      d1.valid                := u.valid && (!cv || ci >= bankWidth.U)
      d0.bits.pc              := bankAlign(u.bits.pc)
      d1.bits.pc              := nextBank(u.bits.pc)
      d0.bits.br_mask         := u.bits.br_mask
      d1.bits.br_mask         := u.bits.br_mask >> bankWidth
      d0.bits.btb_mispredicts := u.bits.btb_mispredicts
      d1.bits.btb_mispredicts := u.bits.btb_mispredicts >> bankWidth
      d0.bits.cfi_idx.valid   := cv && ci < bankWidth.U
      d1.bits.cfi_idx.valid   := cv && ci >= bankWidth.U
      d0.bits.ghist           := u.bits.ghist.histories(0)
      d1.bits.ghist           := u.bits.ghist.histories(1)
    } .otherwise {
      d1.valid                := u.valid
      d0.valid                := u.valid && !mayNotBeDualBanked(u.bits.pc) && (!cv || ci >= bankWidth.U)
      d1.bits.pc              := bankAlign(u.bits.pc)
      d0.bits.pc              := nextBank(u.bits.pc)
      d1.bits.br_mask         := u.bits.br_mask
      d0.bits.br_mask         := u.bits.br_mask >> bankWidth
      d1.bits.btb_mispredicts := u.bits.btb_mispredicts
      d0.bits.btb_mispredicts := u.bits.btb_mispredicts >> bankWidth
      d1.bits.cfi_idx.valid   := cv && ci < bankWidth.U
      d0.bits.cfi_idx.valid   := cv && ci >= bankWidth.U
      d1.bits.ghist           := u.bits.ghist.histories(0)
      d0.bits.ghist           := u.bits.ghist.histories(1)
    }
  }

  // corefuzzing: wire domain to all banked predictors and aggregate mismatch at f3
  for (i <- 0 until nBanks) {
    banked_predictors(i).io.f0_domain_id              := io.f0_req_domain_id
    banked_predictors(i).io.f1_domain_id.foreach(_ := io.f1_req_domain_id.get)   // [F4S1RNG]
    banked_predictors(i).io.update.bits.cf_domain_id.foreach(_ := io.update.bits.cf_domain_id.getOrElse(0.U(1.W)))
    banked_predictors(i).io.update.bits.cf_is_secret.foreach(_ := io.update.bits.cf_is_secret.getOrElse(false.B))
  }
  io.resp.f3.bpd_domain_mismatch.foreach(_ := banked_predictors.map(_.io.f3_bpd_domain_mismatch).reduce(_||_))
  io.resp.f3.btb_domain_mismatch.foreach(_ := banked_predictors.map(_.io.f3_btb_domain_mismatch).reduce(_||_))
  io.resp.f3.bpd_secret_mismatch.foreach(_ := banked_predictors.map(_.io.f3_bpd_secret_mismatch).reduce(_||_))
  io.resp.f3.btb_secret_mismatch.foreach(_ := banked_predictors.map(_.io.f3_btb_secret_mismatch).reduce(_||_))
  io.resp.f3.bpd_secret_early.foreach(_ := banked_predictors.map(_.io.f3_bpd_secret_early.get).reduce(_||_))   // [ICPREDF3-TAGE]
  io.resp.f3.bpd_domain_early.foreach(_ := banked_predictors.map(_.io.f3_bpd_domain_early.get).reduce(_||_))
  io.resp.f1.bpd_secret_early.foreach(_ := false.B)
  io.resp.f2.bpd_secret_early.foreach(_ := false.B)
  io.resp.f1.bpd_domain_early.foreach(_ := false.B)
  io.resp.f2.bpd_domain_early.foreach(_ := false.B)
  io.resp.f1.bpd_secret_mismatch.foreach(_ := false.B)
  io.resp.f2.bpd_secret_mismatch.foreach(_ := false.B)
  io.resp.f1.btb_secret_mismatch.foreach(_ := false.B)
  io.resp.f2.btb_secret_mismatch.foreach(_ := false.B)
  // [BTBDISP] step 16: OR of the banks; the PC of the first bank with a BTB secret edge
  // (registered bank outputs, 2:1 priority mux)
  // [BTBPICK] ruling 146 (v): two-tier pick (CfBtbPcPick): the first bank with a SECRET BTB edge,
  // else the first bank with any BTB edge (attacker record), else the last bank (default; the
  // PC is read only when fetch_flags bit 7 or 8 fires).  Bank 0 > bank 1.
  // [R124GATE] the fields are None without ENABLE_IFT (bundle note above): foreach is a no-op.
  io.resp.f3.btb_sec_disp.foreach(_ := banked_predictors.map(_.io.f3_btb_sec_disp).reduce(_||_))
  io.resp.f3.btb_atk_disp.foreach(_ := banked_predictors.map(_.io.f3_btb_atk_disp).reduce(_||_))
  private val cf_bank_sec = banked_predictors.map(b => b.io.f3_btb_secret_mismatch || b.io.f3_btb_sec_disp)
  io.resp.f3.btb_sec_pc.foreach(_ := CfBtbPcPick(cf_bank_sec,
    banked_predictors.zip(cf_bank_sec).map { case (b, s) => s || b.io.f3_btb_atk_disp },
    banked_predictors.map(_.io.f3_btb_sec_pc)))
  io.resp.f1.btb_sec_disp.foreach(_ := false.B)
  io.resp.f2.btb_sec_disp.foreach(_ := false.B)
  io.resp.f1.btb_atk_disp.foreach(_ := false.B)
  io.resp.f2.btb_atk_disp.foreach(_ := false.B)
  io.resp.f1.btb_sec_pc.foreach(_ := 0.U)
  io.resp.f2.btb_sec_pc.foreach(_ := 0.U)
  io.cf_st.foreach(o => for (b <- 0 until nBanks) { o(b) := banked_predictors(b).io.cf_st.get })   // [V3TAP-PRED]

  when (io.update.valid) {
    when (io.update.bits.cfi_is_br && io.update.bits.cfi_idx.valid) {
      assert(io.update.bits.br_mask(io.update.bits.cfi_idx.bits))
    }
  }
}

class NullBranchPredictorBank(implicit p: Parameters) extends BranchPredictorBank()(p) {
  val mems = Nil
}


