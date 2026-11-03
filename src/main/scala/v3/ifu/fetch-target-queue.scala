//******************************************************************************
// Copyright (c) 2015 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Fetch Target Queue (FTQ)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Each entry in the FTQ holds the fetch address and branch prediction snapshot state.
//
// TODO:
// * reduce port counts.

package boom.v3.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.{Parameters}
import freechips.rocketchip.util.{Str, CoreFuzzingConstants}

import boom.v3.common._
import boom.v3.exu._
import boom.v3.util._

/**
 * FTQ Parameters used in configurations
 *
 * @param nEntries # of entries in the FTQ
 */
case class FtqParameters(
  nEntries: Int = 16
)

/**
 * Bundle to add to the FTQ RAM and to be used as the pass in IO
 */
class FTQBundle(implicit p: Parameters) extends BoomBundle
  with HasBoomFrontendParameters
{
  // // TODO compress out high-order bits
  // val fetch_pc  = UInt(vaddrBitsExtended.W)
  // IDX of instruction that was predicted taken, if any
  val cfi_idx   = Valid(UInt(log2Ceil(fetchWidth).W))
  // Was the CFI in this bundle found to be taken? or not
  val cfi_taken = Bool()
  // Was this CFI mispredicted by the branch prediction pipeline?
  val cfi_mispredicted = Bool()
  // What type of CFI was taken out of this bundle
  val cfi_type = UInt(CFI_SZ.W)
  // mask of branches which were visible in this fetch bundle
  val br_mask   = UInt(fetchWidth.W)
  // This CFI is likely a CALL
  val cfi_is_call   = Bool()
  // This CFI is likely a RET
  val cfi_is_ret    = Bool()
  // Is the NPC after the CFI +4 or +2
  val cfi_npc_plus4 = Bool()
  // What was the top of the RAS that this bundle saw?
  val ras_top = UInt(vaddrBitsExtended.W)
  val ras_idx = UInt(log2Ceil(nRasEntries).W)

  // Which bank did this start from?
  val start_bank = UInt(1.W)

  // corefuzzing: domain of the fetch packet (0=victim, 1=attacker)
  val cf_fetch_domain = if (ENABLE_IFT) Some(UInt(1.W)) else None
  // corefuzzing: fetch packet contained a secret-dependent instruction (updated at commit)
  val cf_fetch_secret = if (ENABLE_IFT) Some(Bool()) else None
  // [FTQATK] patch 47 (r173 (5)): a CFI of this packet resolved with an attacker-tainted condition
  // or target (brinfos cond_atk); ORed into the domain sent with the predictor update
  val cf_fetch_atk = if (ENABLE_IFT) Some(Bool()) else None
  // [RASLBL] DF-ras-repair-label: the domain and secret labels of the RAS slot ras_top was read
  // from (FetchBundle.ras_top_dom/sec, f3), restored with ras_top by the top repair
  val ras_top_dom = if (ENABLE_IFT) Some(Bool()) else None
  val ras_top_sec = if (ENABLE_IFT) Some(Bool()) else None
  // [B60RAS] bundle60 (C): the slot's attacker-window label (ras.io.read_watk), restored with ras_top like ras_top_dom
  val ras_top_watk = if (ENABLE_IFT) Some(Bool()) else None

  // // Metadata for the branch predictor
  // val bpd_meta = Vec(nBanks, UInt(bpdMaxMetaLength.W))
}

/**
 * [SPECBPU] (owner 2026-11-02, RULES C 'Speculative TAGE/BTB updates') one resolved CFI, core -> FTQ: the lane chosen
 * from the brinfos registers (core.scala [SPECBPU]), registered in the core.  The FTQ reads the branch's entry (pc,
 * fetch-time ghist, predictor meta slice, labels) at ftq_idx and sends one TAGE/BTB/BIM/uBTB update.
 */
class BpuResolveInfo(implicit p: Parameters) extends BoomBundle
{
  val ftq_idx       = UInt(log2Ceil(ftqSz).W)
  val pc_lob        = UInt(log2Ceil(icBlockBytes).W)
  val edge_inst     = Bool()
  val is_br         = Bool()
  val is_jal        = Bool()
  val is_jalr       = Bool()
  val taken         = Bool()
  val mispredict    = Bool()
  val target_offset = SInt(21.W)                       // brinfo.target_offset (imm[20:0]) for br / jal
  val jalr_target   = UInt(vaddrBitsExtended.W)         // jump-unit lane only
  // IFT: the resolving CFI's own condition / target taint (brinfos cond_secret / cond_atk)
  val cond_secret   = if (ENABLE_IFT) Some(Bool()) else None
  val cond_atk      = if (ENABLE_IFT) Some(Bool()) else None
  // [SPECBPU-REP] (spec-bpu-plan-final 6.3) debug printf builds only (the [SINKFACTS86] gate): the resolving CFI's
  // cf_op_count_id (uopIDCounterWidthCF = 16 b, micro-op.scala), printed with its undo-log record so the directed
  // checker joins every record to the commit log (a repaired record's CFI never commits, a retired one's does)
  val dbg_oc        = if (ENABLE_IFT && ENABLE_CF_DEBUG_PRINTF && !ENABLE_IFT_BRIDGE) Some(UInt(16.W)) else None
}

/**
 * [SPECBPU-REP] (owner 2026-11-02, RULES C 'Speculative BPU repair'): the slot rule of a redirect, core -> FTQ, read
 * with io.redirect.  pc_lob = the redirecting uop's pc_lob (the mispredicted CFI, or the flushing uop); incl = that uop
 * itself does not commit (exception or refetch flush), so a CFI in its own slot is squashed too.
 */
class BpuSquashInfo(implicit p: Parameters) extends BoomBundle
{
  val pc_lob = UInt(log2Ceil(icBlockBytes).W)
  val incl   = Bool()
}

/**
 * IO to provide a port for a FunctionalUnit to get the PC of an instruction.
 * And for JALRs, the PC of the next instruction.
 */
class GetPCFromFtqIO(implicit p: Parameters) extends BoomBundle
{
  val ftq_idx   = Input(UInt(log2Ceil(ftqSz).W))

  val entry     = Output(new FTQBundle)
  val ghist     = Output(new GlobalHistory)

  val pc        = Output(UInt(vaddrBitsExtended.W))
  val com_pc    = Output(UInt(vaddrBitsExtended.W))

  // the next_pc may not be valid (stalled or still being fetched)
  val next_val  = Output(Bool())
  val next_pc   = Output(UInt(vaddrBitsExtended.W))
}

/**
 * Queue to store the fetch PC and other relevant branch predictor signals that are inflight in the
 * processor.
 *
 * @param num_entries # of entries in the FTQ
 */
class FetchTargetQueue(specMetaLo: Int = 0, specMetaW: Int = -1)(implicit p: Parameters) extends BoomModule
  with HasBoomCoreParameters
  with HasBoomFrontendParameters
  with CoreFuzzingConstants
{
  val num_entries = ftqSz
  private val idx_sz = log2Ceil(num_entries)

  val io = IO(new BoomBundle {
    // Enqueue one entry for every fetch cycle.
    val enq = Flipped(Decoupled(new FetchBundle()))
    // Pass to FetchBuffer (newly fetched instructions).
    val enq_idx = Output(UInt(idx_sz.W))
    // ROB tells us the youngest committed ftq_idx to remove from FTQ.
    val deq = Flipped(Valid(UInt(idx_sz.W)))

    // Give PC info to BranchUnit.
    val get_ftq_pc = Vec(2, new GetPCFromFtqIO())


    // Used to regenerate PC for trace port stuff in FireSim
    // Don't tape this out, this blows up the FTQ
    val debug_ftq_idx  = Input(Vec(coreWidth, UInt(log2Ceil(ftqSz).W)))
    val debug_fetch_pc = Output(Vec(coreWidth, UInt(vaddrBitsExtended.W)))

    val redirect = Input(Valid(UInt(idx_sz.W)))

    val brupdate = Input(new BrUpdateInfo)

  // corefuzzing: gate for speculative FTQ prints
  val cf_debug_ftq_enable = Input(Bool())

  // 2-bit index into ftQueueEntryOptions = Seq(32, 24, 16, 8) for runtime FTQ size selection
  val cf_ftq_idx = Input(UInt(2.W))
  // Pulse from core on QS_DRAINING→QS_FETCH: reset pointers to canonical state
  // so they are always within [0, cf_ftq_active-1] after a RECONFIG CSR write.
  val cf_ftq_quiesce_reset = Input(Bool())

  // corefuzzing: multi-port dispatch/execute-time feedback to mark FTQ entry as secret.
  // Port 0..coreWidth-1: dispatch stage (cf_secret_propagation known at dispatch)
  // Port coreWidth..coreWidth+memWidth-1: TLB stage (cf_secret_access determined at address resolution)
  // Flag is sticky (write-only true.B). Multiple ports write different entries per cycle safely
  // because ram is Reg(Vec(...)) — each element has an independent write enable.
  val cf_secret_ftq_updates = Flipped(Vec(coreWidth + memWidth, Valid(UInt(idx_sz.W))))
  // corefuzzing: for each cf_secret_ftq_updates port that fires on a call-type FTQ entry,
  // export the RAS slot index so the frontend can mark ras_secret retroactively.
  val cf_ras_secret_upd_valid = Output(Vec(coreWidth + memWidth, Bool()))
  val cf_ras_secret_upd_idx   = Output(Vec(coreWidth + memWidth, UInt(log2Ceil(nRasEntries).W)))
  // [WINDEAD] review 3.9b: cf_ghist_secret_active / cf_ghist_atk_active / cf_ghist_*_ctr_val removed
  // (unread since step 20; the window counters are io.cpu.cf_win_*_ctr from the core).
  // [WINFTQ] patch 29b (owner r160): the FTQ index of the OLDEST secret-window arming branch that
  // resolved (registered in core.scala), and the RAS late write of the window-range calls.
  val cf_win_ftq_rs  = if (ENABLE_IFT) Some(Input(Valid(UInt(idx_sz.W)))) else None
  // [WINFTQPOST] patch 38: the secret window is open (core winCtr counter != 0), for the enqueue tag
  val cf_win_sec_open = if (ENABLE_IFT) Some(Input(Bool())) else None
  // [M3ATKWIN] lean58 (Fable audit M3; RULES A U8 "symmetric", reading 1): the attacker window is open (core winCtr != 0)
  val cf_win_atk_open = if (ENABLE_IFT) Some(Input(Bool())) else None
  val cf_ras_win_upd = if (ENABLE_IFT) Some(Output(Valid(UInt(log2Ceil(nRasEntries).W)))) else None
  // [WINFTQATK] (DF-ftq-atk-window-range, owner 2026-10-28; RULES A U8, O11): the attacker twin of cf_win_ftq_rs
  // (the oldest ATTACKER-window arming branch that resolved, registered in core.scala), and the kind of the slot
  // sent on cf_ras_win_upd: bit 0 = secret window range (as before), bit 1 = attacker window range.
  val cf_win_ftq_ra   = if (ENABLE_IFT) Some(Input(Valid(UInt(idx_sz.W)))) else None
  val cf_ras_win_kind = if (ENABLE_IFT) Some(Output(UInt(2.W))) else None
  // [FTQATK] patch 47: one port per brinfos lane (core.scala), valid = a br/jalr resolved with
  // cond_atk; bits = its ftq_idx.  Direct ports, no compactor (brinfos is already a register).
  val cf_atk_ftq_updates = if (ENABLE_IFT) Some(Flipped(Vec(coreWidth, Valid(UInt(idx_sz.W))))) else None

    val bpdupdate = Output(Valid(new BranchPredictionUpdate))

    // [SPECBPU] the resolved CFI (core.scala, a register) and the predictor update built from its FTQ entry.
    val bpu_res        = Input(Valid(new BpuResolveInfo))
    val spec_bpdupdate = Output(Valid(new BranchPredictionUpdate))
    // [SPECBPU-REP] the redirect's slot rule (with io.redirect), for the undo-log squash test
    val bpu_sq         = Input(new BpuSquashInfo)
    // [SPECBPU-REP] debug printf builds only (the [SINKFACTS86] gate): the config-CSR indices this change reads,
    // {btb way, btb set (2), gshare, tage count (3), ftq (2)} (frontend), for the DMI reconfiguration measurement of
    // the undo log ([BPDC], spec-bpu-plan-final 4.8); no port in FireSim / NoLog / IFT-off builds
    val dbg_cfg        = if (ENABLE_IFT && ENABLE_CF_DEBUG_PRINTF && !ENABLE_IFT_BRIDGE) Some(Input(UInt(9.W))) else None

    val ras_update = Output(Bool())
    val ras_update_idx = Output(UInt(log2Ceil(nRasEntries).W))
    val ras_update_pc  = Output(UInt(vaddrBitsExtended.W))
    // [RASLBL] the repaired slot's saved labels (registered like ras_update_pc; read only with ras_update)
    val ras_update_dom = if (ENABLE_IFT) Some(Output(Bool())) else None
    val ras_update_sec = if (ENABLE_IFT) Some(Output(Bool())) else None
    val ras_update_watk = if (ENABLE_IFT) Some(Output(Bool())) else None   // [B60RAS]

    // [F-RAS-3b] (22c2, review B1) active RAS size = frontend cf_ras_active (rasOptionsVec(cf_ras_idx)),
    // the SAME wire the frontend uses at F1/F2/F3, so the FTQ's stored ghist.ras_idx advances in the
    // same window as the frontend pointer.  Port exists only with ENABLE_RECONF (reconf-off FTQ
    // Verilog unchanged: it keeps the nRasEntries constant).
    val cf_ras_active = if (ENABLE_RECONF) Some(Input(UInt(log2Ceil(nRasEntries + 1).W))) else None

  })
  val bpd_ptr    = RegInit(0.U(idx_sz.W))
  val deq_ptr    = RegInit(0.U(idx_sz.W))
  val enq_ptr    = RegInit(1.U(idx_sz.W))

  // Runtime FTQ size selection: ftQueueEntryOptions = Seq(32, 24, 16, 8)
  // [STATIC-CLAMP] each option is capped at the built size: identical constants when the module is built at
  // the max option (the fuzzing build); a static smaller build with reconf off (index 0) gets its own size.
  val ftqOptionsVec = VecInit(ftQueueEntryOptions.map(o => (o min num_entries).U))
  val cf_ftq_active = ftqOptionsVec(io.cf_ftq_idx)
  // [F-RAS-3b] active RAS window for the stored-ghist update below (reconf off: nRasEntries, the default).
  val cf_ftq_ras_active = io.cf_ras_active.getOrElse(nRasEntries.U)

  val full = ((WrapInc(WrapInc(enq_ptr, cf_ftq_active), cf_ftq_active) === bpd_ptr) ||
              (WrapInc(enq_ptr, cf_ftq_active) === bpd_ptr))


  val pcs      = Reg(Vec(num_entries, UInt(vaddrBitsExtended.W)))
  val meta     = SyncReadMem(num_entries, Vec(nBanks, UInt(bpdMaxMetaLength.W)))
  val ram      = Reg(Vec(num_entries, new FTQBundle))
  // [SPECBPU] a third copy for the resolution-time read (one copy per read port, as for the two existing ports).
  val ghist    = Seq.fill(3) { SyncReadMem(num_entries, new GlobalHistory) }
  // [SPECBPU] the BIM + uBTB + BTB + TAGE slice of each bank's meta (bits [specMetaLo, specMetaLo + specMetaW), the
  // composer's layout: [0, 73) in WithTAGELBPD), the only part the resolution update needs; written with meta at
  // enqueue, read at the resolving branch's ftq_idx.  The loop's meta (above bit 73) is not kept: the loop is not
  // trained at resolution.  32 x 2 x 73 b LUTRAM (registered address, asynchronous read).
  private val spMetaW = if (specMetaW < 0) bpdMaxMetaLength else specMetaW
  require(specMetaLo >= 0 && specMetaLo + spMetaW <= bpdMaxMetaLength, "[SPECBPU] spec meta slice out of range")
  val spec_meta = if (spMetaW > 0) Some(SyncReadMem(num_entries, Vec(nBanks, UInt(spMetaW.W)))) else None
  // [SPECBPU] the fetch pc (and, IFT on, the fetch domain, also written only at enqueue) for the resolution read:
  // a 32 x 41 b LUTRAM copy (7 RAM32M, registered address) instead of one more 32:1 mux of the pcs flop array
  // (about 9 LUT per bit, 360 LUT).  pcs and cf_fetch_domain are written only at enqueue (the redirect write-back
  // rewrites cf_fetch_domain with its own value), so the copy is always equal to them.
  private val spPcW = vaddrBitsExtended + (if (ENABLE_IFT) 1 else 0)
  val spec_pcs  = SyncReadMem(num_entries, UInt(spPcW.W))
  val lhist    = if (useLHist) {
    Some(SyncReadMem(num_entries, Vec(nBanks, UInt(localHistoryLength.W))))
  } else {
    None
  }

  // [D4RAS] D4 probe cycle counter (same reset as frontend d4_cyc, so the c= values align)
  val d4_cyc_q = if (ENABLE_CF_DEBUG_PRINTF) { val c = RegInit(0.U(40.W)); c := c + 1.U; c } else 0.U

  val do_enq = io.enq.fire


  // This register lets us initialize the ghist to 0
  val prev_ghist = RegInit((0.U).asTypeOf(new GlobalHistory))
  val prev_entry = RegInit((0.U).asTypeOf(new FTQBundle))
  val prev_pc    = RegInit(0.U(vaddrBitsExtended.W))
  when (do_enq) {

    pcs(enq_ptr)           := io.enq.bits.pc

    val new_entry = Wire(new FTQBundle)

    new_entry.cfi_idx   := io.enq.bits.cfi_idx
    // Initially, if we see a CFI, it is assumed to be taken.
    // Branch resolutions may change this
    new_entry.cfi_taken     := io.enq.bits.cfi_idx.valid
    new_entry.cfi_mispredicted := false.B
    new_entry.cfi_type      := io.enq.bits.cfi_type
    new_entry.cfi_is_call   := io.enq.bits.cfi_is_call
    new_entry.cfi_is_ret    := io.enq.bits.cfi_is_ret
    new_entry.cfi_npc_plus4 := io.enq.bits.cfi_npc_plus4
    new_entry.ras_top       := io.enq.bits.ras_top
    new_entry.ras_idx       := io.enq.bits.ghist.ras_idx
    new_entry.br_mask       := io.enq.bits.br_mask & io.enq.bits.mask
    new_entry.start_bank    := bank(io.enq.bits.pc)
    // corefuzzing: store fetch domain for bpdupdate path; secret flag updated lazily at commit
    new_entry.cf_fetch_domain.foreach(_ := io.enq.bits.cf_fetch_domain.getOrElse(0.U(1.W)))
    // [WINFTQPOST] patch 38 (p4-38-author.md, owner r158/r160): a packet ENQUEUED while the secret
    // window is open is younger than the arming branch B (B resolved before the counter opened; the
    // FTQ is in fetch order), so every uop in it is a window member and the predictor training and
    // RAS push it makes are results produced inside the window.  [WINFTQ] below covers only the
    // entries in flight when a CORRECTLY predicted B resolves; a packet fetched after a mispredicted
    // B (the redirect refetch) or after any B once its range is set was tagged only by a window-
    // operand writeback or a linking JAL, so a `j`, a jalr call or a branch on a clean register
    // trained the BTB / uBTB / BPD clean (t60 m60: br_s's displacement went in clean and cleared
    // both records).  Membership is the fetch-time bit (ghist_secret, f3 counter, carried in the
    // FetchBundle) OR the enqueue-time counter, so a packet that sat in f4 across the opening is
    // covered too.  A packet fetched in the window but dispatched after it closed is tagged
    // (over-taint, accepted: its record already has s_prop through the same fetch-time bit,
    // core.scala d_win_sec).  Wrong-path entries are dropped by the redirect, as before.
    // Levels: core winCtr register -> 7-bit zero detect (shared with f3 ghist_secret, 2 LUT) OR the
    // f4-registered ghist_secret -> new_entry.cf_fetch_secret, a DATA input of each entry's
    // existing write mux (the select, do_enq && enq_ptr === e, is unchanged): 0 added levels on
    // any existing path.  Cost: about 1 LUT per entry (the constant-0 data input becomes a signal).
    if (ENABLE_IFT) {
      new_entry.cf_fetch_secret.get := io.cf_win_sec_open.get || io.enq.bits.ghist_secret.getOrElse(false.B)
      // [M3ATKWIN] lean58 (Fable audit M3; RULES A U8 "symmetric", reading 1): a packet enqueued while the ATTACKER window is open trains the predictors
      // with the attacker label (bpdupdate cf_domain_id |= cf_fetch_atk), the twin of cf_fetch_secret above.  It is
      // also still set at resolution by an attacker-steered CFI ([FTQATK]).  0 added levels (a data input of the
      // entry's existing write mux), about 1 LUT per entry.  RECORD CHANGE.
      new_entry.cf_fetch_atk.get    := io.cf_win_atk_open.get || io.enq.bits.ghist_atk.getOrElse(false.B)
      new_entry.ras_top_dom.get     := io.enq.bits.ras_top_dom.get   // [RASLBL] beside ras_top
      new_entry.ras_top_sec.get     := io.enq.bits.ras_top_sec.get
      new_entry.ras_top_watk.get    := io.enq.bits.ras_top_watk.get   // [B60RAS]
    }

    val new_ghist = Mux(io.enq.bits.ghist.current_saw_branch_not_taken,
      io.enq.bits.ghist,
      prev_ghist.update(
        prev_entry.br_mask,
        prev_entry.cfi_taken,
        prev_entry.br_mask(prev_entry.cfi_idx.bits),
        prev_entry.cfi_idx.bits,
        prev_entry.cfi_idx.valid,
        prev_pc,
        prev_entry.cfi_is_call,
        prev_entry.cfi_is_ret,
        // [F-RAS-3b] wrap ras_idx in the ACTIVE window (was the 32-entry default): this stored
        // ghist is what the core's mispredict repair reads back (get_pc(1).ghist).
        cf_ftq_ras_active
      )
    )
    // [D4RAS] D4 probe (measurement only): the FTQ's sequential ghist chain (what a mispredict
    // redirect restores, core.scala get_pc(1).ghist) vs the f3 ras_idx carried in the packet (what
    // the f3 push/pop used and what the entry stores for the top repair, line 226).  Mismatch-only,
    // so no runtime gate (the FTQ sees only the frontend debug bit, which ENABLE_DEBUG leaves off).
    if (ENABLE_CF_DEBUG_PRINTF) {
      when (!io.enq.bits.ghist.current_saw_branch_not_taken && new_ghist.ras_idx =/= io.enq.bits.ghist.ras_idx) {
        printf("[D4RAS] c=%d DESYNC ftq=%d pc=0x%x f3idx=%d ftqidx=%d\n",
          d4_cyc_q, enq_ptr, io.enq.bits.pc, io.enq.bits.ghist.ras_idx, new_ghist.ras_idx)
      }
    }

    lhist.map( l => l.write(enq_ptr, io.enq.bits.lhist))
    ghist.map( g => g.write(enq_ptr, new_ghist))
    meta.write(enq_ptr, io.enq.bits.bpd_meta)
    spec_meta.foreach(_.write(enq_ptr, VecInit(io.enq.bits.bpd_meta.map(m => m(specMetaLo + spMetaW - 1, specMetaLo)))))
    spec_pcs.write(enq_ptr, (if (ENABLE_IFT) Cat(new_entry.cf_fetch_domain.get, io.enq.bits.pc) else io.enq.bits.pc))
    ram(enq_ptr) := new_entry

    prev_pc    := io.enq.bits.pc
    prev_entry := new_entry
    prev_ghist := new_ghist

    enq_ptr := WrapInc(enq_ptr, cf_ftq_active)
  }

  io.enq_idx := enq_ptr

  // corefuzzing: multi-port secret feedback — mark FTQ entry as soon as secret state is known.
  // All ports write true.B (monotone), so simultaneous writes to the same entry are harmless.
  // Also export RAS secret update for call-type entries so frontend can retroactively tag
  // the pushed RAS slot as secret.
  io.cf_ras_secret_upd_valid := VecInit(Seq.fill(coreWidth + memWidth)(false.B))
  io.cf_ras_secret_upd_idx   := VecInit(Seq.fill(coreWidth + memWidth)(0.U(log2Ceil(nRasEntries).W)))
  for ((u, i) <- io.cf_secret_ftq_updates.zipWithIndex) {
    when (u.valid) {
      ram(u.bits).cf_fetch_secret.foreach(_ := true.B)
      // If the FTQ entry had a call CFI, propagate its RAS write index for the secret shadow
      when (ram(u.bits).cfi_is_call) {
        io.cf_ras_secret_upd_valid(i) := true.B
        io.cf_ras_secret_upd_idx(i)   := ram(u.bits).ras_idx
      }
    }
  }

  io.bpdupdate.valid := false.B
  io.bpdupdate.bits  := DontCare

  when (io.deq.valid) {
    deq_ptr := io.deq.bits
  }

  // This register avoids a spurious bpd update on the first fetch packet
  val first_empty = RegInit(true.B)

  // We can update the branch predictors when we know the target of the
  // CFI in this fetch bundle

  val ras_update = WireInit(false.B)
  val ras_update_pc = WireInit(0.U(vaddrBitsExtended.W))
  val ras_update_idx = WireInit(0.U(log2Ceil(nRasEntries).W))
  io.ras_update     := RegNext(ras_update)
  io.ras_update_pc  := RegNext(ras_update_pc)
  io.ras_update_idx := RegNext(ras_update_idx)

  val bpd_update_mispredict = RegInit(false.B)
  val bpd_update_repair = RegInit(false.B)
  val bpd_repair_idx = Reg(UInt(log2Ceil(ftqSz).W))
  val bpd_end_idx = Reg(UInt(log2Ceil(ftqSz).W))
  val bpd_repair_pc = Reg(UInt(vaddrBitsExtended.W))

  val bpd_idx = Mux(io.redirect.valid, io.redirect.bits,
    Mux(bpd_update_repair || bpd_update_mispredict, bpd_repair_idx, bpd_ptr))
  val bpd_entry = RegNext(ram(bpd_idx))
  val bpd_ghist = ghist(0).read(bpd_idx, true.B)
  val bpd_lhist = if (useLHist) {
    lhist.get.read(bpd_idx, true.B)
  } else {
    VecInit(Seq.fill(nBanks) { 0.U })
  }
  val bpd_meta  = meta.read(bpd_idx, true.B) // TODO fix these SRAMs
  val bpd_pc    = RegNext(pcs(bpd_idx))
  val bpd_target = RegNext(pcs(WrapInc(bpd_idx, cf_ftq_active)))

  when (io.redirect.valid) {
    bpd_update_mispredict := false.B
    bpd_update_repair     := false.B
  } .elsewhen (RegNext(io.brupdate.b2.mispredict) && !RegNext(io.redirect.valid)) {
    // Suppress repair walk when redirect fired at T: at T+1, RegNext(redirect.valid)=true,
    // meaning enq_ptr already jumped backwards. The repair walk would process entries from
    // the now-invalid speculative path (including uninitialized slots at PC=0 with garbage
    // targets like 0xcf10000a), corrupting the BPD and causing thread_entry spin.
    // Since mispredict and redirect always co-fire at T, this effectively suppresses the
    // repair walk entirely. Cost: no repair BPD updates (acceptable for correctness).
    bpd_update_mispredict := true.B
    bpd_repair_idx        := RegNext(io.brupdate.b2.uop.ftq_idx)
    bpd_end_idx           := RegNext(enq_ptr)
    //corefuzzing
    // Non-destructive speculative logging: dump FTQ entries that will be invalidated
    // We compute the invalidated range [repair_idx, enq_ptr) (modulo num_entries)
    val start_idx = RegNext(io.brupdate.b2.uop.ftq_idx)
    val end_idx = RegNext(enq_ptr)
    for (j <- 0 until num_entries) {
      val jidx = j.U
      val in_range = Mux(start_idx <= end_idx, (start_idx <= jidx) && (jidx < end_idx), (jidx >= start_idx) || (jidx < end_idx))
      when (in_range) {
  SpeculativePrintf.dump("FTQ", Sext.apply(pcs(j)(vaddrBits-1,0), xLen), 0.U, false.B, io.cf_debug_ftq_enable)
      }
    }
  } .elsewhen (bpd_update_mispredict) {
    bpd_update_mispredict := false.B
    bpd_update_repair     := true.B
    bpd_repair_idx        := WrapInc(bpd_repair_idx, cf_ftq_active)
  } .elsewhen (bpd_update_repair && RegNext(bpd_update_mispredict)) {
    bpd_repair_pc         := bpd_pc
    bpd_repair_idx        := WrapInc(bpd_repair_idx, cf_ftq_active)
  } .elsewhen (bpd_update_repair) {
    bpd_repair_idx        := WrapInc(bpd_repair_idx, cf_ftq_active)
    when (WrapInc(bpd_repair_idx, cf_ftq_active) === bpd_end_idx ||
      bpd_pc === bpd_repair_pc)  {
      bpd_update_repair := false.B
    }

  }


  val do_commit_update     = (!bpd_update_mispredict &&
                              !bpd_update_repair &&
                               bpd_ptr =/= deq_ptr &&
                               enq_ptr =/= WrapInc(bpd_ptr, cf_ftq_active) &&
                              !io.brupdate.b2.mispredict &&
                              !io.redirect.valid && !RegNext(io.redirect.valid))
  val do_mispredict_update = bpd_update_mispredict
  val do_repair_update     = bpd_update_repair

  when (RegNext(do_commit_update || do_repair_update || do_mispredict_update)) {
    val cfi_idx = bpd_entry.cfi_idx.bits
    val valid_repair = bpd_pc =/= bpd_repair_pc

    io.bpdupdate.valid := (!first_empty &&
                           (bpd_entry.cfi_idx.valid || bpd_entry.br_mask =/= 0.U) &&
                           !(RegNext(do_repair_update) && !valid_repair))
    io.bpdupdate.bits.is_mispredict_update := RegNext(do_mispredict_update)
    io.bpdupdate.bits.is_repair_update     := RegNext(do_repair_update)
    io.bpdupdate.bits.pc      := bpd_pc
    io.bpdupdate.bits.btb_mispredicts := 0.U
    io.bpdupdate.bits.br_mask := Mux(bpd_entry.cfi_idx.valid,
      MaskLower(UIntToOH(cfi_idx)) & bpd_entry.br_mask, bpd_entry.br_mask)
    io.bpdupdate.bits.cfi_idx := bpd_entry.cfi_idx
    io.bpdupdate.bits.cfi_mispredicted := bpd_entry.cfi_mispredicted
    io.bpdupdate.bits.cfi_taken  := bpd_entry.cfi_taken
    io.bpdupdate.bits.target     := bpd_target
    io.bpdupdate.bits.cfi_is_br  := bpd_entry.br_mask(cfi_idx)
    io.bpdupdate.bits.cfi_is_jal := bpd_entry.cfi_type === CFI_JAL || bpd_entry.cfi_type === CFI_JALR
    io.bpdupdate.bits.ghist      := bpd_ghist
    io.bpdupdate.bits.lhist      := bpd_lhist
    io.bpdupdate.bits.meta       := bpd_meta
    // corefuzzing: carry fetch domain and secret flag to BPD shadow writes
    // [FTQATK] patch 47: | the attacker-steered CFI bit (one OR, before the predictors' s1 RegNext)
    io.bpdupdate.bits.cf_domain_id.foreach(_ := bpd_entry.cf_fetch_domain.getOrElse(0.U(1.W)) |
                                                bpd_entry.cf_fetch_atk.getOrElse(false.B).asUInt)
    io.bpdupdate.bits.cf_is_secret.foreach(_ := bpd_entry.cf_fetch_secret.getOrElse(false.B))
    // [WINDEAD] review 3.9b: the retire-time GHR window counters (cf_ghist_secret_ctr / cf_ghist_atk_ctr)
    // were removed; the implicit-flow window lives in the core since step 20 ([ARMRES], core.scala winCtr)
    // and nothing read the FTQ copies any more.

    first_empty := false.B
  }

  when (do_commit_update) {
    bpd_ptr := WrapInc(bpd_ptr, cf_ftq_active)
  }

  // [SINKFACTS86] patch 86 (all-sinks study Q1/Q2/Q5/Q6, owner 2026-10-20): debug printfs ONLY.  Gate = the
  // IftSquashMerge printf gate (rob.scala iftSqDbgOn): IFT on, CF debug printfs on, no IFT bridge.  IFT off,
  // CoreFuzzingDMINoLogConfig and every FireSim (bridge) build elaborate nothing here.  cyc = d4_cyc_q, a
  // counter from reset (the dcache [SMWR]/[RFB] cyc and the predictor [*W] cyc count from the same reset).
  //  [Q2MP]  every brupdate.b2.mispredict cycle: rv = the same-cycle FTQ redirect (Q2 premise: always 1, so the
  //          mispredict/repair walk arm above, RegNext(b2.mispredict) && !RegNext(redirect.valid), never fires),
  //          ridx = redirect ftq idx, midx = the mispredicted branch's ftq idx (they differ only when the ROB
  //          flush arm of core.scala owns the redirect).
  //  [Q2ARM] the walk armed (bpd_update_mispredict / bpd_update_repair): expected never (then the loop
  //          predictor, whose only allocating input is is_mispredict_update, stays empty: Q1).
  //  [BPDU]  every predictor update leaving the FTQ (the Q5 update rate; the join key for the [BIMW] / [BTBW] /
  //          [UBTBW] / [TAGEW] writes, which follow 1-2 cycles later in the banks).
  if (ENABLE_IFT && ENABLE_CF_DEBUG_PRINTF && !ENABLE_IFT_BRIDGE) {
    when (io.brupdate.b2.mispredict) {
      printf("[Q2MP] cyc=%d rv=%d ridx=%d midx=%d\n", d4_cyc_q, io.redirect.valid, io.redirect.bits,
        io.brupdate.b2.uop.ftq_idx)
    }
    when (bpd_update_mispredict || bpd_update_repair) {
      printf("[Q2ARM] cyc=%d mp=%d rp=%d ridx=%d eidx=%d\n", d4_cyc_q, bpd_update_mispredict, bpd_update_repair,
        bpd_repair_idx, bpd_end_idx)
    }
    when (io.bpdupdate.valid) {
      printf("[BPDU] cyc=%d pc=0x%x brm=0x%x cv=%d ci=%d tk=%d mp=%d br=%d jal=%d mpu=%d rpu=%d dom=%d sec=%d\n",
        d4_cyc_q, io.bpdupdate.bits.pc, io.bpdupdate.bits.br_mask, io.bpdupdate.bits.cfi_idx.valid,
        io.bpdupdate.bits.cfi_idx.bits, io.bpdupdate.bits.cfi_taken, io.bpdupdate.bits.cfi_mispredicted,
        io.bpdupdate.bits.cfi_is_br, io.bpdupdate.bits.cfi_is_jal, io.bpdupdate.bits.is_mispredict_update,
        io.bpdupdate.bits.is_repair_update, io.bpdupdate.bits.cf_domain_id.getOrElse(0.U(1.W)),
        io.bpdupdate.bits.cf_is_secret.getOrElse(false.B))
    }
  }

  // [SPECBPU] (owner 2026-11-02, RULES C 'Speculative TAGE/BTB updates' + 'Speculative BPU follow-ups'): the
  // resolution-time predictor update (TAGE, BTB, BIM, uBTB).
  // Cycle c: io.bpu_res (a core register) addresses the entry: pcs / ram are flop arrays (32:1 read mux, like
  // get_ftq_pc), spec_pcs / ghist(2) / spec_meta are SyncReadMem (data at c+1).  Cycle c+1: the update leaves on
  // io.spec_bpdupdate (predictor.scala io.spec_update, read by the TAGE, BTB, BIM and uBTB).
  // GHR (design note 1b): the update indexes / tags the TAGE with the branch's OWN fetch-time history, the ghist
  // stored at its enqueue (the same value the commit update read), never the live one.  ghist / meta / pcs of an
  // entry are written only at its enqueue; the entry is read at most 2 cycles after the branch resolved, and the
  // earliest re-enqueue of any entry after a redirect is at least 4 cycles away (redirect -> f0 .. f4 -> enqueue),
  // so a wrong-path branch whose entry a redirect frees still reads its own entry.  An on-path branch's entry is
  // not committed, so it lies in [bpd_ptr, enq_ptr) and cannot be overwritten (the `full` check).
  // The packet's predicted cfi fields are NOT used (the redirect write-back may be rewriting them): the outcome
  // comes from the resolving lane itself.  No ram field read here is written by that write-back except the
  // monotone labels (OR-kept there).
  // Encoding: a one-CFI packet update, which the predictor code consumes unchanged (is_commit_update = 1 on
  // their bus): br_mask = the branch's slot (0 for jal/jalr), cfi_idx = the slot (valid), cfi_taken /
  // cfi_mispredicted = the resolution, target = the resolved target.  TAGE: provider counter / u update for that
  // slot, allocation when mispredicted (as at commit).  BTB: meta (tag, is_br) for the slot; target when taken.
  // Levels (rev1, review S3: a SyncReadMem lowers to a REGISTERED ADDRESS + an asynchronous LUTRAM read; the data
  // is not registered): c: bpu_res flops -> the memories' address registers (route only); the 2 monotone label
  // bits: 32:1 mux of the ram flops (3 LUT) -> flops.  c+1: address registers -> LUTRAM read (RAM32M, about 1 LUT
  // delay) -> uop pc OR + 40-bit add (1 LUT + carry chain) -> jalr 2:1 (1 LUT) -> predictor bank split (bank(pc)
  // 2:1, nextBank carry; 1 LUT) -> bank s1_update flops, and -> fetchIdx -> the BTB's s0 IFT LUTRAM reads (btb.scala
  // cfFwdRead) -> forward mux -> flops.  The commit path into the same flops starts at the bpd_pc flop; this one
  // adds the LUTRAM read: +1 level plus the carry chain (D3 maximum; Phase 4 timing check, rev1 report section 5).
  // Cost: LUTRAM pc+dom 41 b, ghist 72 b, meta slice 2 x 73 b, 32 deep (about 44 RAM32M = 176 LUT) + their address
  // registers (3 x 5 FF); label read 18 LUT + 2 FF; request flops about 80 FF; target adder 40 LUT (5 CARRY8).
  // [SPECBPU-REP] the resolution update is built into sp_res; the undo-log block at the end of this module decides
  // whether it leaves (Q2 (a) drop filter) or a repair / relabel takes the free bus cycle, and drives
  // io.spec_bpdupdate.  sp_res_ftq / sp_res_slot export the CFI's FTQ index and packet slot (flops) to that block.
  val sp_res      = Wire(Valid(new BranchPredictionUpdate))
  val sp_res_ftq  = Wire(UInt(idx_sz.W))
  val sp_res_slot = Wire(UInt(log2Ceil(fetchWidth).W))
  // [SPECBPU-REP] (spec-bpu-plan-final 6.3) debug printf builds only: the resolving CFI's op count, for the log prints
  val sp_res_oc   = if (ENABLE_IFT && ENABLE_CF_DEBUG_PRINTF && !ENABLE_IFT_BRIDGE) Some(Wire(UInt(16.W))) else None
  locally {
    val r     = io.bpu_res
    val rq    = RegNext(r.bits)
    val rv    = RegNext(r.valid, false.B)
    val sp_pw = spec_pcs.read(r.bits.ftq_idx, r.valid)
    val sp_pc = sp_pw(vaddrBitsExtended - 1, 0)
    val sp_e  = ram(r.bits.ftq_idx)
    // [SPECBPU-REP] the entry's start_bank from the ram flops, registered: equal to bank(sp_pc) (both are set from the
    // enqueue pc), so the slot, and the undo-log squash filter on it, start at flops instead of the LUTRAM read
    val sp_sb = RegNext(sp_e.start_bank)
    val sp_gh = ghist(2).read(r.bits.ftq_idx, r.valid)
    val sp_mt = spec_meta.map(_.read(r.bits.ftq_idx, r.valid))
    // slot in the packet: the same function as the redirect write-back (new_cfi_idx above) and the jump unit
    val slot  = (rq.pc_lob ^ Mux(sp_sb === 1.U, 1.U << log2Ceil(bankBytes), 0.U))(log2Ceil(fetchWidth), 1)
    // resolved target: the core's redirect formula (core.scala mispredict_target), for every CFI kind
    val uop_maybe_pc = AlignPCToBoundary(sp_pc, icBlockBytes) | rq.pc_lob
    val br_target    = (uop_maybe_pc.asSInt + rq.target_offset +
                        (Fill(vaddrBitsExtended-1, rq.edge_inst) << 1).asSInt).asUInt
    val u = sp_res
    sp_res_ftq  := rq.ftq_idx
    sp_res_slot := slot
    sp_res_oc.foreach(_ := rq.dbg_oc.get)                // [SPECBPU-REP] debug builds only
    u.valid                     := rv
    u.bits                      := DontCare
    u.bits.is_mispredict_update := false.B
    u.bits.is_repair_update     := false.B
    u.bits.btb_mispredicts      := 0.U
    u.bits.pc                   := sp_pc
    u.bits.br_mask              := Mux(rq.is_br, UIntToOH(slot, fetchWidth), 0.U)
    u.bits.cfi_idx.valid        := true.B
    u.bits.cfi_idx.bits         := slot
    u.bits.cfi_taken            := rq.taken
    u.bits.cfi_mispredicted     := rq.mispredict
    u.bits.cfi_is_br            := rq.is_br
    u.bits.cfi_is_jal           := rq.is_jal || rq.is_jalr
    u.bits.cfi_is_jalr          := rq.is_jalr
    u.bits.target               := Mux(rq.is_jalr, rq.jalr_target, br_target)
    u.bits.ghist                := sp_gh
    u.bits.lhist                := VecInit(Seq.fill(nBanks) { 0.U(localHistoryLength.W) })
    u.bits.meta                 := VecInit((0 until nBanks).map { b =>
      sp_mt.map(m => (m(b) << specMetaLo).pad(bpdMaxMetaLength)).getOrElse(0.U(bpdMaxMetaLength.W)) })
    // [SPECBPU-IFT] (RULES A; closes scope gaps B and C for TAGE / BTB / BIM / uBTB): the labels the commit update
    // would carry for this entry so far (fetch domain | attacker-steered CFI / attacker window; secret window /
    // secret CFI / secret access in the packet), ORed with the resolving CFI's OWN condition taint, which reaches the
    // entry bits only through the compactor / [FTQATK] ports a cycle later.  Written into btb_domain / btb_secret /
    // ebtb / the BTB displacement records, the TAGE table_shadow, the BIM data_shadow, the uBTB meta domain / secret
    // and the uBTB displacement records by the existing write logic.  Levels: 2 bits of the
    // same 32:1 read mux -> flop -> 1 LUT OR.
    if (ENABLE_IFT) {
      val sp_dom = sp_pw(vaddrBitsExtended)            // the fetch domain, from the LUTRAM word
      val sp_sec = RegNext(sp_e.cf_fetch_secret.get)
      val sp_atk = RegNext(sp_e.cf_fetch_atk.get)
      u.bits.cf_domain_id.get := sp_dom | (sp_atk || rq.cond_atk.get).asUInt
      u.bits.cf_is_secret.get := sp_sec || rq.cond_secret.get
    }
    // [SPECBPU-REP] the [BPDS] print moved to the undo-log block: it prints only the updates that leave
  }

  io.enq.ready := RegNext(!full || do_commit_update)

  val redirect_idx = io.redirect.bits
  val redirect_entry = ram(redirect_idx)
  val redirect_new_entry = WireInit(redirect_entry)
  // [RASLBL] DF-ras-repair-label: the labels saved beside ras_top, from the same redirect_entry read
  // (32:1 mux per bit -> flop, parallel to the ras_update_pc path; no valid gate needed, the frontend
  // reads them only with ras_update).  2 x (about 11 LUT + 1 FF).
  io.ras_update_dom.foreach(_ := RegNext(redirect_entry.ras_top_dom.get))
  io.ras_update_sec.foreach(_ := RegNext(redirect_entry.ras_top_sec.get))
  io.ras_update_watk.foreach(_ := RegNext(redirect_entry.ras_top_watk.get))   // [B60RAS] same read, +1 (11 LUT + 1 FF)

  when (io.redirect.valid) {
    enq_ptr    := WrapInc(io.redirect.bits, cf_ftq_active)
    // [CF-FIX 2026-08-07, doc 34] bpd_ptr is the TRAILING pointer: the bpd_ptr-based
    // `full` check is what stops enqueue from lapping entries still referenced by
    // in-flight uops.  The previous code teleported it FORWARD on EVERY redirect
    // (`bpd_ptr := io.redirect.bits`, to cure a small-FTQ full-deadlock after
    // resize/quiesce pointer resets).  That destroyed the lap protection: wrong-path
    // runahead fetch could wrap the FTQ and OVERWRITE the entry of a branch stalled
    // on cold-miss operands; its eventual (correct) mispredict-recovery redirect then
    // read the NEW occupant's PC from get_pc(1) and steered the front end to a bogus
    // address that COMMITTED — the post-restore illegal-control-flow bug (both
    // flavors; captured live: BRRES misp=1 ftq=8 -> REDIR ftqpc=0x140b4).
    // Correct rule: on a normal redirect bpd_ptr must NOT move (upstream semantics —
    // the redirecting entry is always inside [bpd_ptr, enq_ptr), so it stays validly
    // trailing).  Only when that invariant is ALREADY broken (reachable only through
    // FTQ-resize/quiesce pointer resets — the deadlock the old code patched) is
    // bpd_ptr clamped to the redirect point.
    val bpd_trailing_valid =
      Mux(bpd_ptr <= enq_ptr,
          (io.redirect.bits >= bpd_ptr) && (io.redirect.bits < enq_ptr),
          (io.redirect.bits >= bpd_ptr) || (io.redirect.bits < enq_ptr))
    when (!bpd_trailing_valid) {
      bpd_ptr  := io.redirect.bits
    }

    when (io.brupdate.b2.mispredict) {
    val new_cfi_idx = (io.brupdate.b2.uop.pc_lob ^
      Mux(redirect_entry.start_bank === 1.U, 1.U << log2Ceil(bankBytes), 0.U))(log2Ceil(fetchWidth), 1)
      redirect_new_entry.cfi_idx.valid    := true.B
      redirect_new_entry.cfi_idx.bits     := new_cfi_idx
      redirect_new_entry.cfi_mispredicted := true.B
      redirect_new_entry.cfi_taken        := io.brupdate.b2.taken
      redirect_new_entry.cfi_is_call      := redirect_entry.cfi_is_call && redirect_entry.cfi_idx.bits === new_cfi_idx
      redirect_new_entry.cfi_is_ret       := redirect_entry.cfi_is_ret  && redirect_entry.cfi_idx.bits === new_cfi_idx
    }

    ras_update     := true.B
    ras_update_pc  := redirect_entry.ras_top
    ras_update_idx := redirect_entry.ras_idx

  } .elsewhen (RegNext(io.redirect.valid)) {
    prev_entry := RegNext(redirect_new_entry)
    prev_ghist := bpd_ghist
    prev_pc    := bpd_pc

    ram(RegNext(io.redirect.bits)) := RegNext(redirect_new_entry)
    // [WINFTQ] patch 29b: the write-back of the redirect entry carries the secret bit READ a cycle
    // earlier; a tag that landed in between (a cf_secret_ftq_updates port or the window range
    // below) was lost.  The bit is monotone, so the write keeps the current value too.
    if (ENABLE_IFT) {
      ram(RegNext(io.redirect.bits)).cf_fetch_secret.get :=
        RegNext(redirect_new_entry.cf_fetch_secret.get) || ram(RegNext(io.redirect.bits)).cf_fetch_secret.get
      // [FTQATK] patch 47: the same race for the attacker bit (a packet-mate's cond_atk port can
      // land between the read and this write-back).
      ram(RegNext(io.redirect.bits)).cf_fetch_atk.get :=
        RegNext(redirect_new_entry.cf_fetch_atk.get) || ram(RegNext(io.redirect.bits)).cf_fetch_atk.get
      // [RASLBL] the write-back would rewrite the saved RAS labels with the value read a cycle earlier
      // (equal: only the enqueue writes them, and enq_ptr has moved past this entry).  Keep them, so
      // their per-entry flops load only at enqueue (CE = the existing enqueue decode, D = the shared
      // enqueue data): no write mux per bit and no flop for RegNext(redirect_new_entry) of these bits.
      for (e <- 0 until num_entries) {
        when (RegNext(io.redirect.bits) === e.U) {
          ram(e).ras_top_dom.get := ram(e).ras_top_dom.get
          ram(e).ras_top_sec.get := ram(e).ras_top_sec.get
          ram(e).ras_top_watk.get := ram(e).ras_top_watk.get   // [B60RAS]
        }
      }
    }
  }

  // [FTQATK] patch 47 (r173 (5), DF-42-btbatk; task-reports/p4-owner-gap-check.md B): a VICTIM
  // packet whose br/jalr resolved with an attacker-tainted condition or target (cond_atk =
  // rs1/rs2 taint_atk) trains BTB / uBTB / BIM / TAGE / loop as attacker-influenced: the bit is
  // ORed into bpdupdate.cf_domain_id above, so every predictor domain shadow, the BTB / uBTB
  // attacker displacement record (r140/r146: written by an update with cf_domain_id 1, the
  // r168b "allocating access attacker-tainted" rule) and every existing domain-mismatch fetch
  // edge (victim packets only) work unchanged.  No RAS term: the pushed value is pc+4 of the
  // call, independent of its target data.  Same write discipline as cf_fetch_secret (monotone,
  // cleared at enqueue, kept across the redirect write-back above; placed after it so a set
  // in the write-back cycle wins, last connect).  Value taint only: a fetch
  // edge sets only the sticky cf_attacker_influence, which never re-enters taint_atk, so no
  // train -> edge -> train chain.
  // Levels: brinfos registers -> 5-bit ftq_idx decode AND valid (1 LUT) -> OR into the entry's
  // D mux (1 LUT) -> ram FF.  Off the fetch / prediction path.  Cost: 32 FF + about 5 LUT/entry.
  io.cf_atk_ftq_updates.foreach { ports =>
    for (u <- ports) {
      when (u.valid) { ram(u.bits).cf_fetch_atk.get := true.B }
    }
  }

  // [WINFTQ] patch 29b (DF-step20 risk (i), owner r160): when a secret-window arming branch B
  // resolves, every FTQ entry YOUNGER than B is window-secret (all younger uops are window
  // members), so its secret bit is set at once.  This replaces, for the BPD / BTB / RAS secret
  // shadows, the writeback-time tags that a consumer of a register written back BEFORE B
  // resolved could not raise (its operand read clean: the step-20 commit-time set).  Packets
  // fetched after the resolution are tagged at enqueue (patch 38) and by [RASJAL] calls (the
  // writeback feed is removed, r170 B [FTQCFI]).
  // Over-taint, accepted: a range entry on a path a younger branch then
  // redirects stays tagged until it is re-enqueued, and a queued RAS write of such a wrong-path
  // call can land on a slot a correct-path call re-pushed (both clear on the next push / enqueue).
  // Range: IsOlder(b, e, bpd_ptr) && IsOlder(e, enq_ptr, bpd_ptr) over the live region
  // [bpd_ptr, enq_ptr) (B is not committed, so it lies inside it); entries at or beyond the
  // active size are excluded.  B's own entry is tagged by the existing brinfos cond_secret input.
  // RAS: a call in a range entry pushed a slot that is now window-secret.  The range entries
  // holding a call are queued (pend) and sent one per cycle on one extra late-write port (the
  // existing per-port path, WrapInc(ras_idx) in frontend.scala); an entry re-enqueued before it
  // is sent is dropped from the queue (its slot was never pushed by that call any more).
  // Levels: rs is a register (core.scala RegNext); per entry 3 five-bit compares against
  // registers/constants -> 1 LUT (XOR/AND) -> the entry's existing cf_fetch_secret D mux (one
  // more term).  RAS queue: 32 FF -> PriorityEncoderOH -> 32:1 Mux1H of ras_idx -> FF.
  // Cost: about 4 LUT per entry (about 130 LUT), the queue 32 FF + about 60 LUT, 6 FF out.
  // [WINFTQATK] (DF-ftq-atk-window-range, owner 2026-10-28 "Stage the RTL fix"; RULES A U8 "Yes, symmetric", O11):
  // the ATTACKER twin of the range set above.  When an attacker-window arming branch B resolves (cf_win_ftq_ra,
  // core.scala: attacker-domain conditional branch, victim conditional branch with an attacker-tainted condition,
  // jalr with an attacker-tainted target; oldest B of the cycle, not mispredicted, not killed), every FTQ entry
  // younger than B gets cf_fetch_atk, so its commit-time predictor update carries cf_domain_id = 1 (bpdupdate,
  // above), the twin of cf_fetch_secret.  Before, cf_fetch_atk was set only at enqueue ([M3ATKWIN]: window open or
  // ghist_atk at f3) or by an attacker-operand CFI of the packet itself ([FTQATK]), so a VICTIM packet fetched after
  // B and enqueued before B resolved (a window member by age: its commit records have shadow_atk = 1) trained the
  // BTB / uBTB / BIM / TAGE / loop with u_dom = 0 (measured, ift_graph pred_model.writers on cfg19-plru: spectre-v1
  // 106, spectre-v2 358, spectreRSB 1 such updates; the secret twin 0).  Same range, same over-taint (an entry on a
  // path a younger branch redirects stays tagged until re-enqueued), same write discipline as cf_fetch_secret
  // (monotone set; the redirect write-back above ORs the current bit; this block is after it, so a set in the
  // write-back cycle wins).  B's own entry: an attacker-domain B is domain 1 already; a cond_atk B is set by
  // [FTQATK].
  // RAS: needed, for the same reason as the secret queue.  A call in a range entry pushed its slot while it was in
  // B's shadow, but before the attacker window counter opened, so ras_watk (frontend.scala: the f3 push writes the
  // counter's zero detect) is 0 there, and a VICTIM ret that pops that slot gets no RAS_STATE atk edge, while the
  // same call fetched one cycle after the resolution does (B60RAS).  The existing one-per-cycle queue is shared:
  // each entry has a secret and an attacker pending bit, the priority select runs over their OR, and the slot
  // goes out once with both bits (cf_ras_win_kind) on the existing late port; ras.scala sets ras_secret and/or
  // ras_watk.  The [WINFTQPOST] f4-crossing case is twinned too (a call packet enqueued while the attacker window
  // is open whose f3 push preceded the opening: ghist_atk = 0).
  // Levels: rs / ra are registers; per entry the "below enq_ptr" half of the range (2 compares + XOR/AND) is
  // shared by both kinds; the attacker half is one 5-bit compare against a constant plus 1 LUT (XOR/AND) -> one
  // more set term in the entry's existing cf_fetch_atk D logic (beside the coreWidth [FTQATK] decodes): <= +1
  // level on a flop-to-flop path off the fetch path.  RAS queue: the OR of the two pending bits enters the
  // existing PriorityEncoderOH (absorbed in its first LUT level, <= +1), the Mux1H of ras_idx is unchanged, the
  // two kind bits are (psel & pend).orR -> FF.
  // Cost (attacker half): about 3 LUT per entry (range term, set term, pending set/clear), 32 FF (pending bits),
  // 2 FF (kind), about 30 LUT (queue OR, kind reduce); the 29b secret half is unchanged in function.
  io.cf_ras_win_upd.foreach { o => o.valid := false.B; o.bits := 0.U }
  io.cf_ras_win_kind.foreach(_ := 0.U)
  if (ENABLE_IFT) {
    val rs       = io.cf_win_ftq_rs.get
    val ra       = io.cf_win_ftq_ra.get                // [WINFTQATK]
    val h        = bpd_ptr
    val b_lt_h   = rs.bits < h
    val a_lt_h   = ra.bits < h                         // [WINFTQATK]
    val enq_lt_h = enq_ptr < h
    // the entry is older than enq_ptr (inside the live region) and inside the active size: shared by both kinds
    val below = (0 until num_entries).map { e =>
      val E = e.U(idx_sz.W)
      ((E < enq_ptr) ^ (E < h) ^ enq_lt_h) && (E < cf_ftq_active)
    }
    val inr = (0 until num_entries).map { e =>
      val E = e.U(idx_sz.W)
      ((rs.bits < E) ^ b_lt_h ^ (E < h)) && below(e)
    }
    val ina = (0 until num_entries).map { e =>        // [WINFTQATK] younger than the attacker B
      val E = e.U(idx_sz.W)
      ((ra.bits < E) ^ a_lt_h ^ (E < h)) && below(e)
    }
    val pend  = RegInit(VecInit(Seq.fill(num_entries)(false.B)))
    val penda = RegInit(VecInit(Seq.fill(num_entries)(false.B)))   // [WINFTQATK] attacker-window pending call
    val pany  = pend.asUInt | penda.asUInt
    val psel  = PriorityEncoderOH(pany)
    io.cf_ras_win_upd.get.valid := RegNext(pany.orR, false.B)
    io.cf_ras_win_upd.get.bits  := RegNext(Mux1H(psel, ram.map(_.ras_idx)))
    io.cf_ras_win_kind.get      := RegNext(Cat((psel & penda.asUInt).orR, (psel & pend.asUInt).orR), 0.U)
    for (e <- 0 until num_entries) {
      when (psel(e)) { pend(e) := false.B; penda(e) := false.B }
      when (do_enq && enq_ptr === e.U) { pend(e) := false.B; penda(e) := false.B }
      when (rs.valid && inr(e)) {
        ram(e).cf_fetch_secret.get := true.B
        when (ram(e).cfi_is_call) { pend(e) := true.B }
      }
      when (ra.valid && ina(e)) {                      // [WINFTQATK]
        ram(e).cf_fetch_atk.get := true.B
        when (ram(e).cfi_is_call) { penda(e) := true.B }
      }
      // [WINFTQPOST] patch 38: a call packet enqueued while the window is open whose push at f3
      // preceded the opening (fetch-time bit 0: it sat in f4 across B's resolution) pushed a slot
      // the f3 push mark (frontend.scala) did not cover; queue it like a range call.  Rare (the
      // one or two packets in f3/f4 at the opening), so the one-per-cycle queue is not loaded.
      // Last connect: overrides the enqueue clear above for this entry.  Same per-entry enq_ptr
      // decode as that clear; about 1 LUT per entry.
      when (do_enq && enq_ptr === e.U && io.cf_win_sec_open.get &&
            !io.enq.bits.ghist_secret.getOrElse(false.B) && io.enq.bits.cfi_is_call) { pend(e) := true.B }
      // [WINFTQATK] the attacker twin (ras_watk is written at the f3 push from the same counter as ghist_atk)
      when (do_enq && enq_ptr === e.U && io.cf_win_atk_open.get &&
            !io.enq.bits.ghist_atk.getOrElse(false.B) && io.enq.bits.cfi_is_call) { penda(e) := true.B }
    }
    when (io.cf_ftq_quiesce_reset) { pend.foreach(_ := false.B); penda.foreach(_ := false.B) }
  }

  // On quiesce drain (QS_DRAINING→QS_FETCH), reset FTQ state to clear any
  // in-flight BPD tracking. enq_ptr := 1.U here is immediately overwritten at
  // T+1 by the redirect (redirect_flush = RegNext(flush.valid) in frontend),
  // which sets enq_ptr := WrapInc(redirect.bits, N). The critical fix is the
  // quiesce_reset_d1 block below, which fires at T+1 alongside the redirect and
  // sets bpd_ptr/deq_ptr to redirect.bits — making them one slot behind enq_ptr
  // so full=false regardless of where in the ring the redirect lands.
  val quiesce_reset_d1 = RegNext(io.cf_ftq_quiesce_reset, false.B)
  when (io.cf_ftq_quiesce_reset) {
    enq_ptr               := 1.U
    bpd_ptr               := 0.U
    deq_ptr               := 0.U
    first_empty           := true.B
    bpd_update_mispredict := false.B
    bpd_update_repair     := false.B
  }
  // T+1: the flush-triggered redirect fires (redirect_flush = RegNext(flush)).
  // Align bpd_ptr/deq_ptr with the new enq_ptr so FTQ full=false.
  // Without this, bpd_ptr=0 from the quiesce reset combined with
  // enq_ptr=WrapInc(redirect.bits, N) causes full=true when redirect.bits>=29.
  when (quiesce_reset_d1 && io.redirect.valid) {
    bpd_ptr := io.redirect.bits
    deq_ptr := io.redirect.bits
  }

  //-------------------------------------------------------------
  // **** [SPECBPU-REP] undo log of the resolution updates ****
  //-------------------------------------------------------------
  // Owner 2026-11-02 (RULES C 'Speculative BPU repair'): mechanism "A undo log", pre-write squash "Drop at the FTQ",
  // labels "Restore old label", scope "TAGE+BTB+BIM+uBTB"; repair yields to resolution updates on the bus
  // (spec-bpu-plan-final.md 4.2-4.6).  One record per resolution update that leaves
  // (live only while its CFI's FTQ entry is not fully committed); allocation order = index order mod cfSpecLogN.  Control in flops {v, sq, ftq, slot} (+ IFT
  // the label sent); payload in a 16-deep LUTRAM {pc, history, slot, cfi kind / outcome}.  Each predictor keeps its
  // own 16-deep undo word per record id with what its write of that update overwrote (tage / bim / btb / faubtb).
  //  * order base: lg_dq = deq_ptr of the previous cycle.  Every index the log compares lies
  //    in [lg_dq, enq_ptr]: a live record's entry, because a record is live only if its entry is not behind deq_ptr
  //    and the retire below frees it in the cycle deq_ptr has passed it (it then lies in [lg_dq, deq_ptr)); a redirect
  //    point, because it is
  //    an uncommitted uop (at or after deq_ptr of its own cycle); the CFI of an update leaving at u, because it
  //    resolved at u-3, wrote back at u-2 at the earliest and so committed at u-1 at the earliest, so deq_ptr(u-1) =
  //    lg_dq(u) is not past it.  IsOlder(a, b, lg_dq) therefore orders them exactly.  (bpd_ptr is
  //    no order base: a fast-committing CFI's entry is already behind it when its update leaves, so its record would
  //    never retire and a later redirect would "repair" a committed training.)
  //  * squash: a redirect (registered: sq1) marks every record younger than the redirecting uop: its entry is younger
  //    than the redirect entry R (IsOlder(R, f, lg_dq)), or it is entry R at a later slot (the same slot too for an
  //    exception / refetch flush, bpu_sq.incl).
  //  * Q2 (a) drop: a resolution update that a redirect of the last two cycles (sq1, sq2) already squashes does not
  //    leave and allocates nothing ([BPDX]).
  //  * allocate: every leaving update takes the next record slot (so every predictor's undo word at that id is its
  //    own); the record is live (lg_v) only if its CFI's entry is not behind deq_ptr.  A fully committed entry can
  //    never be squashed: its record starts retired (v = 0, counted nlg) and keeps only its label life.
  //  * retire: a non-squashed record whose entry deq_ptr has passed (IsOlder(f, deq_ptr, lg_dq)) is freed.
  //  * repair: the youngest squashed record (exact for several squashed writes to one entry) goes out as a repair
  //    update (is_repair_update = 1, cf_log_id = its id) in a cycle without a resolution update, never within 2
  //    cycles after an allocation (the TAGE undo word is written at u+3) and never in the 2 cycles after a redirect
  //    (sq1_v / sq2_v: the marks of a redirect land one cycle after sq1, so the pick registered in those cycles may
  //    miss a record that redirect squashed); it is freed then.
  //  * [SPECBPU-RLB] relabel (CF_SPEC_RELABEL): a non-squashed record whose entry's attacker / secret bit was set after
  //    its update left goes out as a relabel update with label = sent | entry bits (a label-only rewrite in every
  //    predictor), when no squashed record waits.  Its label life (lg_lv) outlasts the retire: an entry's bits grow
  //    only while its uops are in flight (window range, packet-mate cond_atk, dispatch / TLB-stage secret ports), so
  //    they are final when it commits; the record stays relabel-eligible until its entry index is enqueued again or
  //    its slot is reallocated (the prompt retire would otherwise race the relabel of the t116 J / K case).
  //  * overflow: an allocation over a valid record drops that record (counted).
  //  * reset: power-on only in the owner's flow.  A DMI reconfiguration (RULES C 'Reconfiguration is DMI only') runs in
  //    debug mode, where cf_ftq_quiesce_reset is 0 (core.scala cf_quiesce_core), so the log persists across it
  //    (measured: [BPDC]).  The FTQ quiesce pointer reset, reachable only from the in-program r01-r11 regression
  //    tests, moves the FTQ pointers, so the log clears with it (counted qclr); nothing relies on that clear.
  // Levels: sq flops -> 5-bit compares (1 LUT + carry-free XOR) -> record flops; allocation: sp_res flops + deq_ptr /
  // lg_dq -> IsOlder (2 LUT) -> AND res_ok (1) -> record write enables (1); picker: flops -> fired mask and AND (2
  // LUT) -> 16-bit leading one (3 LUT) -> 2:1 (1) -> pick flops; issue: pick valid + 5 flops -> rp_fire (1 LUT) -> 2:1
  // bus mux beside the LUTRAM read (pc / ghist / fields; the target sub-path is not muxed) -> predictor bank split ->
  // bank s1 flops; the drop filter (compares) reaches the s1 flops only through is_mispredict_update (no split logic
  // on it).  Cost (section 7): about 345 LUT, 215 FF (IFT +64 FF), 19 RAM32M; the IFT relabel about +480 LUT.
  require(isPow2(cfSpecLogN))
  val lgN = cfSpecLogN
  val lgW = cfSpecLogW
  val slW = log2Ceil(fetchWidth)
  class SpecLogPay extends Bundle {
    val pc     = UInt(vaddrBitsExtended.W)
    val hist   = UInt(globalHistoryLength.W)     // ghist.old_history (histories() reads only it and nst / nsnt)
    val nst    = Bool()                           // ghist.new_saw_branch_taken
    val nsnt   = Bool()                           // ghist.new_saw_branch_not_taken
    val slot   = UInt(log2Ceil(fetchWidth).W)
    val isbr   = Bool()
    val isjal  = Bool()                           // cfi_is_jal of the update (jal or jalr)
    val isjalr = Bool()
    val tk     = Bool()
    val mp     = Bool()
  }
  val lg_pay  = Mem(lgN, new SpecLogPay)
  val lg_v    = RegInit(VecInit(Seq.fill(lgN)(false.B)))
  val lg_sq   = RegInit(VecInit(Seq.fill(lgN)(false.B)))
  val lg_f    = Reg(Vec(lgN, UInt(idx_sz.W)))
  val lg_s    = Reg(Vec(lgN, UInt(slW.W)))
  val lg_tail = RegInit(0.U(lgW.W))
  val lg_lbl  = if (ENABLE_IFT) Some(Reg(Vec(lgN, UInt(2.W)))) else None   // {domain, secret} the record sent
  // the order base of every log compare: deq_ptr of the previous cycle (same reset value as deq_ptr)
  val lg_dq   = RegNext(deq_ptr, 0.U(idx_sz.W))

  // the squash point of the redirect, registered: sq1 = the redirect of the previous cycle, sq2 = the one before
  val rd_slot = (io.bpu_sq.pc_lob ^ Mux(redirect_entry.start_bank === 1.U, 1.U << log2Ceil(bankBytes), 0.U))(slW, 1)
  val sq1_v = RegNext(io.redirect.valid && !io.cf_ftq_quiesce_reset, false.B)
  val sq1_r = RegNext(io.redirect.bits)
  val sq1_s = RegNext(rd_slot)
  val sq1_i = RegNext(io.bpu_sq.incl)
  val sq2_v = RegNext(sq1_v, false.B)
  val sq2_r = RegNext(sq1_r)
  val sq2_s = RegNext(sq1_s)
  val sq2_i = RegNext(sq1_i)
  def sqHit(v: Bool, r: UInt, s: UInt, i: Bool, f: UInt, fs: UInt): Bool =
    v && (IsOlder(r, f, lg_dq) || (f === r && (fs > s || (i && fs === s))))

  // Q2 (a) "Drop at the FTQ": the resolution update leaves only if no registered redirect squashes it
  val res_drop1 = sqHit(sq1_v, sq1_r, sq1_s, sq1_i, sp_res_ftq, sp_res_slot)
  val res_drop2 = sqHit(sq2_v, sq2_r, sq2_s, sq2_i, sp_res_ftq, sp_res_slot)
  val res_ok    = sp_res.valid && !res_drop1 && !res_drop2
  // the CFI's entry is already fully committed (deq_ptr has passed it): the update trains and takes its
  // record slot, but the record starts retired (nothing can squash it)
  val res_cmt   = IsOlder(sp_res_ftq, deq_ptr, lg_dq)
  val lg_alloc  = res_ok
  val lg_hold   = RegNext(lg_alloc, false.B) || RegNext(RegNext(lg_alloc, false.B), false.B)

  // [SPECBPU-RLB] the record entries' attacker / secret bits (registered), the label life, and the records whose
  // label is behind their entry's bits
  val lg_lv   = if (CF_SPEC_RELABEL) Some(RegInit(VecInit(Seq.fill(lgN)(false.B)))) else None
  val lg_cur  = if (CF_SPEC_RELABEL) Some(RegNext(VecInit(lg_f.map(f =>
                  Cat(ram(f).cf_fetch_atk.get, ram(f).cf_fetch_secret.get))))) else None
  val lg_pend = VecInit((0 until lgN).map(i => if (CF_SPEC_RELABEL)
                  lg_lv.get(i) && !lg_sq(i) && (lg_cur.get(i) & ~lg_lbl.get(i)).orR else false.B)).asUInt

  // picker (registered): the youngest squashed record, else ([SPECBPU-RLB]) the lowest-index pending relabel.  The
  // record that fires this cycle is masked out of the next pick (rp_q_oh is the pick as a one-hot flop vector), so a
  // repair can go every free cycle, and a relabel never delays a repair (the functional repair stream is the same with
  // CF_SPEC_RELABEL on or off: a relabel takes only a cycle in which no repair was picked).  No pick fires in
  // the two cycles after a redirect (sq1_v, sq2_v): the redirect's marks reach lg_sq one cycle after sq1, so a pick
  // registered before them could restore an older squashed record ahead of a younger one that wrote the same entry.
  val rp_q_v  = RegInit(false.B)
  val rp_q_i  = Reg(UInt(lgW.W))
  val rp_q_oh = RegInit(0.U(lgN.W))
  val rp_q_rl = Reg(Bool())
  val rp_fire = rp_q_v && !sp_res.valid && !lg_hold && !sq1_v && !sq2_v
  val fired   = rp_q_oh & Fill(lgN, rp_fire)
  val sqv     = VecInit((0 until lgN).map(i => lg_v(i) && lg_sq(i))).asUInt & ~fired
  val pendv   = lg_pend & ~fired
  val younger = (UIntToOH(lg_tail, lgN) - 1.U)(lgN - 1, 0)       // indices below the tail: allocated most recently
  val sq_lo   = sqv & younger
  val ysq     = Mux(sq_lo.orR, Log2(sq_lo), Log2(sqv))
  val nxt_i   = Mux(sqv.orR, ysq, PriorityEncoder(pendv))
  rp_q_v  := (sqv.orR || pendv.orR) && !io.cf_ftq_quiesce_reset
  rp_q_i  := nxt_i
  rp_q_oh := UIntToOH(nxt_i, lgN)
  rp_q_rl := !sqv.orR
  val rp_pay = lg_pay.read(rp_q_i)

  val lg_ret = VecInit((0 until lgN).map(i => lg_v(i) && !lg_sq(i) && IsOlder(lg_f(i), deq_ptr, lg_dq)))
  for (i <- 0 until lgN) {
    when (lg_ret(i)) { lg_v(i) := false.B }                                                           // committed: retire
    when (lg_v(i) && sqHit(sq1_v, sq1_r, sq1_s, sq1_i, lg_f(i), lg_s(i))) { lg_sq(i) := true.B }    // squashed
    when (rp_fire && !rp_q_rl && rp_q_i === i.U) { lg_v(i) := false.B }                           // repaired
    if (CF_SPEC_RELABEL) {
      when (rp_fire && rp_q_rl && rp_q_i === i.U) { lg_lbl.get(i) := lg_lbl.get(i) | lg_cur.get(i) }  // relabelled
      // the label life ends when the entry index takes a new packet, or with the record's repair
      when ((do_enq && enq_ptr === lg_f(i)) || (rp_fire && !rp_q_rl && rp_q_i === i.U)) { lg_lv.get(i) := false.B }
    }
    when (lg_alloc && lg_tail === i.U) {                                                            // allocation wins
      lg_v(i)  := !res_cmt                                                // live only if its entry is uncommitted
      lg_sq(i) := false.B
      lg_f(i)  := sp_res_ftq
      lg_s(i)  := sp_res_slot
      lg_lbl.foreach(_(i) := Cat(sp_res.bits.cf_domain_id.get, sp_res.bits.cf_is_secret.get))
      lg_lv.foreach(_(i) := true.B)
    }
  }
  when (lg_alloc) {
    val w = Wire(new SpecLogPay)
    w.pc     := sp_res.bits.pc
    w.hist   := sp_res.bits.ghist.old_history
    w.nst    := sp_res.bits.ghist.new_saw_branch_taken
    w.nsnt   := sp_res.bits.ghist.new_saw_branch_not_taken
    w.slot   := sp_res_slot
    w.isbr   := sp_res.bits.cfi_is_br
    w.isjal  := sp_res.bits.cfi_is_jal
    w.isjalr := sp_res.bits.cfi_is_jalr
    w.tk     := sp_res.bits.cfi_taken
    w.mp     := sp_res.bits.cfi_mispredicted
    lg_pay.write(lg_tail, w)
    lg_tail := lg_tail + 1.U
  }
  when (io.cf_ftq_quiesce_reset) {          // last connect: a quiesce reset clears the log
    lg_v.foreach(_ := false.B)
    lg_lv.foreach(_.foreach(_ := false.B))
    lg_tail := 0.U
  }

  // the bus: the resolution update, or in a free cycle the picked repair / relabel (repair yields to resolution)
  val o = io.spec_bpdupdate
  o := sp_res
  // Q2 (a): a dropped resolution update keeps its bus cycle as a NON-training update (is_mispredict_update = 1, so
  // is_commit_update = 0 in every predictor; no predictor on this bus reads is_mispredict_update itself, the loop
  // is on io.update), which keeps the filter's compares off the bank s1 valid path (section 7, F3d).  An update of
  // an already committed CFI (res_cmt) trains normally and takes record lg_tail like any other (so the predictors'
  // undo words at that id are its own); that record is born retired.
  o.valid                     := sp_res.valid || rp_fire
  o.bits.is_mispredict_update := sp_res.valid && !res_ok
  o.bits.cf_log_id := Mux(rp_fire, rp_q_i, lg_tail)
  o.bits.cf_relabel.foreach(_ := rp_fire && rp_q_rl)
  when (rp_fire) {
    o.bits.is_repair_update               := true.B
    o.bits.pc                             := rp_pay.pc
    o.bits.br_mask                        := Mux(rp_pay.isbr && !rp_q_rl, UIntToOH(rp_pay.slot, fetchWidth), 0.U)
    o.bits.cfi_idx.valid                  := true.B
    o.bits.cfi_idx.bits                   := rp_pay.slot
    o.bits.cfi_taken                      := rp_pay.tk     && !rp_q_rl
    o.bits.cfi_mispredicted               := rp_pay.mp     && !rp_q_rl
    o.bits.cfi_is_br                      := rp_pay.isbr   && !rp_q_rl
    o.bits.cfi_is_jal                     := rp_pay.isjal  && !rp_q_rl
    o.bits.cfi_is_jalr                    := rp_pay.isjalr && !rp_q_rl
    o.bits.ghist                          := 0.U.asTypeOf(new GlobalHistory)
    o.bits.ghist.old_history              := rp_pay.hist
    o.bits.ghist.new_saw_branch_taken     := rp_pay.nst
    o.bits.ghist.new_saw_branch_not_taken := rp_pay.nsnt
    o.bits.meta                           := VecInit(Seq.fill(nBanks)(0.U(bpdMaxMetaLength.W)))
    if (ENABLE_IFT) {
      // repair: the label the record sent (the STATE PRED record names the training it undoes); relabel: sent | the
      // entry's bits now (the label every predictor rewrites)
      val ilbl = if (CF_SPEC_RELABEL) Mux(rp_q_rl, lg_lbl.get(rp_q_i) | lg_cur.get(rp_q_i), lg_lbl.get(rp_q_i))
                 else lg_lbl.get(rp_q_i)
      o.bits.cf_domain_id.get := ilbl(1)
      o.bits.cf_is_secret.get := ilbl(0)
    }
  }

  // debug printf builds only (the [SINKFACTS86] / IftSquashMerge gate: IFT on, CF debug printfs on, no IFT bridge):
  //  [BPDS] every resolution update that leaves (rev2 format + id = its record, lg = 1 if that record is live, 0 if
  //  the CFI's entry was already committed (born retired), oc = the CFI's op count);  [BPDX] one dropped at the FTQ (Q2 (a));  [BPDQ]
  //  every redirect that squashes records (n = records newly marked, msk = their ids);  [BPDF] every retire (the
  //  record's entry committed);  [BPDR] every repair (k=1) / relabel (k=2) with the running counters: occ / hw = log
  //  occupancy now / high water; ovf / ovfsq / ovfrl = allocations over a valid / squashed / relabel-pending record
  //  (lost undo / relabel); clb = repairs issued while a non-squashed record allocated after the repaired one is
  //  valid (upper bound of hazard H1, an on-path write the restore may clobber); rpr / rlb = repairs / relabels;
  //  fdrop / fdrop2 = FTQ drops / drops only sq2 caught (expected 0); sqtot = records marked squashed; qclr =
  //  squashed records cleared by the r01-r11 quiesce reset (0 under DMI); nlg = records born retired; stl = cycles in
  //  which a valid non-squashed record, or a leaving update, names an entry behind lg_dq (the order base broken:
  //  expected 0); rlx = relabel-pending records whose label life ended (entry re-enqueued) before the relabel.
  if (ENABLE_IFT && ENABLE_CF_DEBUG_PRINTF && !ENABLE_IFT_BRIDGE) {
    val occ    = PopCount(lg_v)
    val c_hw   = RegInit(0.U((lgW + 1).W))
    val c_ovf  = RegInit(0.U(40.W)); val c_ovfsq = RegInit(0.U(40.W)); val c_ovfrl = RegInit(0.U(40.W))
    val c_clb  = RegInit(0.U(40.W)); val c_rpr   = RegInit(0.U(40.W)); val c_rlb   = RegInit(0.U(40.W))
    val c_fd   = RegInit(0.U(40.W)); val c_fd2   = RegInit(0.U(40.W)); val c_sq    = RegInit(0.U(40.W))
    val c_qclr = RegInit(0.U(40.W)); val c_nlg   = RegInit(0.U(40.W)); val c_stl   = RegInit(0.U(40.W))
    val c_rlx  = RegInit(0.U(40.W))
    val lg_oc  = Reg(Vec(lgN, UInt(16.W)))                                  // the record's CFI op count
    when (lg_alloc) { lg_oc(lg_tail) := sp_res_oc.get }
    val ovf    = lg_alloc && lg_v(lg_tail)
    val pos    = (0 until lgN).map(j => j.U(lgW.W) - lg_tail)            // allocation age, 0 = oldest slot
    val rpos   = rp_q_i - lg_tail
    val clb    = rp_fire && !rp_q_rl && (0 until lgN).map(j => lg_v(j) && !lg_sq(j) && pos(j) > rpos).reduce(_ || _)
    val nsqv   = VecInit((0 until lgN).map(i => lg_v(i) && !lg_sq(i) &&
                   sqHit(sq1_v, sq1_r, sq1_s, sq1_i, lg_f(i), lg_s(i)))).asUInt
    val nsq    = PopCount(nsqv)
    // order-base check (outside the 2 cycles after a redirect, whose squashed entries lie beyond the new
    // enq_ptr until they are marked): an entry older than lg_dq, seen from enq_ptr, is a stale index
    val stl    = !sq1_v && !sq2_v && ((0 until lgN).map(i => lg_v(i) && !lg_sq(i) && IsOlder(lg_f(i), lg_dq, enq_ptr))
                   .reduce(_ || _) || (sp_res.valid && IsOlder(sp_res_ftq, lg_dq, enq_ptr)))
    val rlx    = PopCount((0 until lgN).map(i => do_enq && enq_ptr === lg_f(i) && lg_pend(i)))
    when (occ > c_hw)                    { c_hw    := occ }
    when (ovf)                           { c_ovf   := c_ovf + 1.U }
    when (ovf && (lg_sq(lg_tail) || nsqv(lg_tail))) { c_ovfsq := c_ovfsq + 1.U }
    when (lg_alloc && lg_pend(lg_tail))  { c_ovfrl := c_ovfrl + 1.U }
    when (clb)                           { c_clb   := c_clb + 1.U }
    when (rp_fire && !rp_q_rl)           { c_rpr   := c_rpr + 1.U }
    when (rp_fire && rp_q_rl)            { c_rlb   := c_rlb + 1.U }
    when (sp_res.valid && !res_ok)       { c_fd    := c_fd + 1.U }
    when (sp_res.valid && !res_drop1 && res_drop2) { c_fd2 := c_fd2 + 1.U }
    when (res_ok && res_cmt)             { c_nlg   := c_nlg + 1.U }
    when (stl)                           { c_stl   := c_stl + 1.U }
    c_sq  := c_sq + nsq
    c_rlx := c_rlx + rlx
    // exact accounting (D2 rule (1) sqtot = rpr + ovfsq + qclr): an allocation over a record marked in the
    // same cycle loses a repair too (lg_sq is set only next cycle, the allocation wins), and a quiesce reset counts
    // only the squashed records it clears (not the one repaired or overwritten in that cycle, counted as rpr / ovfsq)
    val qsq    = VecInit((0 until lgN).map(i => lg_v(i) && (lg_sq(i) || nsqv(i)))).asUInt & ~fired &
                 ~Mux(lg_alloc, UIntToOH(lg_tail, lgN), 0.U(lgN.W))
    when (io.cf_ftq_quiesce_reset)       { c_qclr  := c_qclr + PopCount(qsq) }
    // [SPECBPU-REP] DMI reconfiguration measurement (owner 2026-11-02 ~21:0x, RULES C 'Speculative BPU gaps and
    // reconfiguration': D-1 / D-2 "Accept + measure", no RTL fix; spec-bpu-plan-final 4.8).  The log, its pending
    // repairs and the label lives persist across a DMI config-CSR write.  [BPDC] at every change of the config indices
    // this change reads (io.dbg_cfg = {btb way, btb set, gshare, tage count, ftq}): cfg / old = new / previous value;
    // live / sq / lv / pend = valid records, of them squashed (repairs still to come), records with a label life,
    // pending relabels, at the change.  lg_pre(i) = record i was allocated before the last change; rpre / rlpre
    // (in [BPDC] and [BPDR]) = repairs / relabels issued since for such records (the writes D-1 / D-2 name); cfgc =
    // changes so far.  The CSRs reset to index 0, so dcfg_q resets to 0.
    val dcfg    = io.dbg_cfg.get
    val dcfg_q  = RegNext(dcfg, 0.U(9.W))
    val cfg_chg = dcfg =/= dcfg_q
    val lg_pre  = RegInit(VecInit(Seq.fill(lgN)(false.B)))
    val lvv     = lg_lv.map(_.asUInt).getOrElse(0.U(lgN.W))
    for (i <- 0 until lgN) {
      when (cfg_chg && (lg_v(i) || lvv(i))) { lg_pre(i) := true.B }
      when (lg_alloc && lg_tail === i.U)    { lg_pre(i) := false.B }                  // last connect: a new record
    }
    val pre_f   = rp_fire && lg_pre(rp_q_i)
    val c_cfgc  = RegInit(0.U(40.W)); val c_rpre  = RegInit(0.U(40.W)); val c_rlpre = RegInit(0.U(40.W))
    when (cfg_chg)           { c_cfgc  := c_cfgc + 1.U }
    when (pre_f && !rp_q_rl) { c_rpre  := c_rpre + 1.U }
    when (pre_f && rp_q_rl)  { c_rlpre := c_rlpre + 1.U }
    when (cfg_chg) {
      printf("[BPDC] cyc=%d cfg=0x%x old=0x%x live=%d sq=%d lv=%d pend=%d cfgc=%d rpre=%d rlpre=%d\n", d4_cyc_q,
        dcfg, dcfg_q, PopCount(lg_v), PopCount(VecInit((0 until lgN).map(i => lg_v(i) && lg_sq(i)))), PopCount(lvv),
        PopCount(lg_pend), c_cfgc + 1.U, c_rpre, c_rlpre)
    }
    when (res_ok) {
      printf("[BPDS] cyc=%d ftq=%d pc=0x%x slot=%d br=%d jal=%d jalr=%d tk=%d mp=%d tgt=0x%x dom=%d sec=%d id=%d " +
             "lg=%d oc=%d\n",
        d4_cyc_q, sp_res_ftq, sp_res.bits.pc, sp_res_slot, sp_res.bits.cfi_is_br,
        sp_res.bits.cfi_is_jal && !sp_res.bits.cfi_is_jalr, sp_res.bits.cfi_is_jalr, sp_res.bits.cfi_taken,
        sp_res.bits.cfi_mispredicted, sp_res.bits.target, sp_res.bits.cf_domain_id.get, sp_res.bits.cf_is_secret.get,
        lg_tail, !res_cmt, sp_res_oc.get)
    }
    when (sp_res.valid && !res_ok) {
      printf("[BPDX] cyc=%d ftq=%d pc=0x%x slot=%d by=%d\n", d4_cyc_q, sp_res_ftq, sp_res.bits.pc, sp_res_slot,
        Mux(res_drop1, 1.U, 2.U))
    }
    when (nsq =/= 0.U) {
      printf("[BPDQ] cyc=%d ridx=%d rslot=%d incl=%d n=%d sqtot=%d msk=0x%x\n", d4_cyc_q, sq1_r, sq1_s, sq1_i, nsq,
        c_sq + nsq, nsqv)
    }
    for (i <- 0 until lgN) {
      when (lg_ret(i)) {
        printf("[BPDF] cyc=%d id=%d ftq=%d slot=%d oc=%d\n", d4_cyc_q, i.U(lgW.W), lg_f(i), lg_s(i), lg_oc(i))
      }
    }
    when (rp_fire) {
      printf("[BPDR] cyc=%d id=%d k=%d ftq=%d slot=%d pc=0x%x br=%d jal=%d dom=%d sec=%d oc=%d occ=%d hw=%d ovf=%d " +
             "ovfsq=%d ovfrl=%d clb=%d rpr=%d rlb=%d fdrop=%d fdrop2=%d sqtot=%d qclr=%d nlg=%d stl=%d rlx=%d " +
             "pre=%d cfgc=%d rpre=%d rlpre=%d\n",
        d4_cyc_q, rp_q_i, Mux(rp_q_rl, 2.U, 1.U), lg_f(rp_q_i), rp_pay.slot, rp_pay.pc, rp_pay.isbr, rp_pay.isjal,
        o.bits.cf_domain_id.get, o.bits.cf_is_secret.get, lg_oc(rp_q_i), occ, c_hw, c_ovf, c_ovfsq, c_ovfrl,
        c_clb + clb, c_rpr + !rp_q_rl, c_rlb + rp_q_rl, c_fd, c_fd2, c_sq, c_qclr, c_nlg, c_stl, c_rlx,
        lg_pre(rp_q_i), c_cfgc, c_rpre + (pre_f && !rp_q_rl), c_rlpre + (pre_f && rp_q_rl))
    }
  }

  //-------------------------------------------------------------
  // **** Core Read PCs ****
  //-------------------------------------------------------------

  for (i <- 0 until 2) {
    val idx = io.get_ftq_pc(i).ftq_idx
    val next_idx = WrapInc(idx, cf_ftq_active)
    val next_is_enq = (next_idx === enq_ptr) && io.enq.fire
    val next_pc = Mux(next_is_enq, io.enq.bits.pc, pcs(next_idx))
    val get_entry = ram(idx)
    val next_entry = ram(next_idx)
    io.get_ftq_pc(i).entry     := RegNext(get_entry)
    if (i == 1)
      io.get_ftq_pc(i).ghist   := ghist(1).read(idx, true.B)
    else
      io.get_ftq_pc(i).ghist   := DontCare
    io.get_ftq_pc(i).pc        := RegNext(pcs(idx))
    io.get_ftq_pc(i).next_pc   := RegNext(next_pc)
    io.get_ftq_pc(i).next_val  := RegNext(next_idx =/= enq_ptr || next_is_enq)
    io.get_ftq_pc(i).com_pc    := RegNext(pcs(Mux(io.deq.valid, io.deq.bits, deq_ptr)))
  }

  for (w <- 0 until coreWidth) {
    io.debug_fetch_pc(w) := RegNext(pcs(io.debug_ftq_idx(w)))
  }
}
