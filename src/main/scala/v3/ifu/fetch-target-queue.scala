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

  // // Metadata for the branch predictor
  // val bpd_meta = Vec(nBanks, UInt(bpdMaxMetaLength.W))
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
class FetchTargetQueue(implicit p: Parameters) extends BoomModule
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
  val cf_ras_win_upd = if (ENABLE_IFT) Some(Output(Valid(UInt(log2Ceil(nRasEntries).W)))) else None
  // [FTQATK] patch 47: one port per brinfos lane (core.scala), valid = a br/jalr resolved with
  // cond_atk; bits = its ftq_idx.  Direct ports, no compactor (brinfos is already a register).
  val cf_atk_ftq_updates = if (ENABLE_IFT) Some(Flipped(Vec(coreWidth, Valid(UInt(idx_sz.W))))) else None

    val bpdupdate = Output(Valid(new BranchPredictionUpdate))

    val ras_update = Output(Bool())
    val ras_update_idx = Output(UInt(log2Ceil(nRasEntries).W))
    val ras_update_pc  = Output(UInt(vaddrBitsExtended.W))

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
  val ftqOptionsVec = VecInit(ftQueueEntryOptions.map(_.U))
  val cf_ftq_active = ftqOptionsVec(io.cf_ftq_idx)
  // [F-RAS-3b] active RAS window for the stored-ghist update below (reconf off: nRasEntries, the default).
  val cf_ftq_ras_active = io.cf_ras_active.getOrElse(nRasEntries.U)

  val full = ((WrapInc(WrapInc(enq_ptr, cf_ftq_active), cf_ftq_active) === bpd_ptr) ||
              (WrapInc(enq_ptr, cf_ftq_active) === bpd_ptr))


  val pcs      = Reg(Vec(num_entries, UInt(vaddrBitsExtended.W)))
  val meta     = SyncReadMem(num_entries, Vec(nBanks, UInt(bpdMaxMetaLength.W)))
  val ram      = Reg(Vec(num_entries, new FTQBundle))
  val ghist    = Seq.fill(2) { SyncReadMem(num_entries, new GlobalHistory) }
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
      new_entry.cf_fetch_atk.get    := false.B   // [FTQATK] set only at resolution (below)
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

  io.enq.ready := RegNext(!full || do_commit_update)

  val redirect_idx = io.redirect.bits
  val redirect_entry = ram(redirect_idx)
  val redirect_new_entry = WireInit(redirect_entry)

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
  io.cf_ras_win_upd.foreach { o => o.valid := false.B; o.bits := 0.U }
  if (ENABLE_IFT) {
    val rs       = io.cf_win_ftq_rs.get
    val h        = bpd_ptr
    val b_lt_h   = rs.bits < h
    val enq_lt_h = enq_ptr < h
    val inr = (0 until num_entries).map { e =>
      val E = e.U(idx_sz.W)
      ((rs.bits < E) ^ b_lt_h ^ (E < h)) && ((E < enq_ptr) ^ (E < h) ^ enq_lt_h) && (E < cf_ftq_active)
    }
    val pend = RegInit(VecInit(Seq.fill(num_entries)(false.B)))
    val psel = PriorityEncoderOH(pend.asUInt)
    io.cf_ras_win_upd.get.valid := RegNext(pend.asUInt.orR, false.B)
    io.cf_ras_win_upd.get.bits  := RegNext(Mux1H(psel, ram.map(_.ras_idx)))
    for (e <- 0 until num_entries) {
      when (psel(e)) { pend(e) := false.B }
      when (do_enq && enq_ptr === e.U) { pend(e) := false.B }
      when (rs.valid && inr(e)) {
        ram(e).cf_fetch_secret.get := true.B
        when (ram(e).cfi_is_call) { pend(e) := true.B }
      }
      // [WINFTQPOST] patch 38: a call packet enqueued while the window is open whose push at f3
      // preceded the opening (fetch-time bit 0: it sat in f4 across B's resolution) pushed a slot
      // the f3 push mark (frontend.scala) did not cover; queue it like a range call.  Rare (the
      // one or two packets in f3/f4 at the opening), so the one-per-cycle queue is not loaded.
      // Last connect: overrides the enqueue clear above for this entry.  Same per-entry enq_ptr
      // decode as that clear; about 1 LUT per entry.
      when (do_enq && enq_ptr === e.U && io.cf_win_sec_open.get &&
            !io.enq.bits.ghist_secret.getOrElse(false.B) && io.enq.bits.cfi_is_call) { pend(e) := true.B }
    }
    when (io.cf_ftq_quiesce_reset) { pend.foreach(_ := false.B) }
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
