//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// IFT squash-record FIFOs and shared merge (IFT overhaul Phase 2, Task 2.6; Task 2.10d design 4b)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Spec claude-artifacts/ift-overhaul/00-spec.md section 5.4.3 (squash FIFO), D5 (every
// ROB-resident squash produces a record), D6 (no stall: overflow is counted, never
// back-pressured into the core).  Instantiated by rob.scala under `if (ENABLE_IFT)` only.
// Task 2.10d (design 4b, execution/task-reports/task-2.10b-design.md section 7): each bank
// captures its killed or rolled-back row RAW into its own 1,024-deep FIFO in BRAM, and one
// merge serves the four FIFOs.  The old per-bank squash merge and IftSquashFifo are gone.

package boom.v3.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.CoreFuzzingConstants
import boom.v3.common._
import boom.v3.util.{Sext}

/** [RECV3] SP-04: the v3 SQUASH record (design 2.2-2.3, SQUASH column; IFTRecordV3.scala).  Shared by the shipped packer
  * (IftSquashMerge, from the FIFO entry) and the rob.scala capture-time shadow (Verilator printf builds), so the shadow
  * assert compares the same function of the same inputs.  dbg 0, shadow_sec/atk 0, fflags 0 (table 2.3); everything
  * else from the raw-row merge (`info`) and the side data. */
object IftSquashPack extends CoreFuzzingConstants {
  def apply(side: IftSqSide, domain: UInt, info: IftCommitInfo)(implicit p: Parameters): UInt =
    IFTRecordV3.instr(
      IFTRecordV3.prefix(REC_SQUASH.U(4.W), Cat(0.U(3.W), side.kind), false.B, domain, side.stamp, side.op_count, side.pc),
      atk = side.atk || info.edge_atk, s_acc = side.s_acc, s_prop = side.s_prop, s_tx = side.s_tx,
      shadow_sec = false.B, shadow_atk = false.B, spec = info.speculated, oc_aliased = info.infl_oc_aliased,
      infl_dropped = info.infl_dropped, dcy = info.cntd_deny_count, srob = info.stall_cycles_rob,
      sstq = info.stall_cycles_stq, sftq = info.sftq,
      dc_kind = info.dc.kind, dc_set = info.dc.set, dc_way = info.dc.way,
      fflags = 0.U(5.W), unit = info.fu_bitmap,
      pdst = side.pdst, prs1 = side.prs1, prs2 = side.prs2, rt1 = side.lrs1_rtype, rt2 = side.lrs2_rtype,
      abr = info.atk_branch_ctr, sbr = info.sec_branch_ctr,
      slots = info.influencer_list.map(e => IFTRecordV3.slot(e.key, e.infl_type, e.is_atk, e.is_secret)))
}
/** IO of IftSquashMerge (Task 2.10d, design 4b).  A named class so member access from
  * rob.scala is not reflective (Task 2.1 review convention). */
class IftSquashMergeIO(nBanks: Int, nSprob: Int, nIc: Int, nWb: Int, nMemord: Int, shadow: Boolean)
                      (implicit p: Parameters) extends BoomBundle {
  val cap     = Input(Vec(nBanks, Valid(new IftSqEntry(nSprob, nIc, nWb, nMemord))))
  val dbg     = Input(Vec(nBanks, new IftSqDbg(shadow)))       // printf and check copies only
  val deq     = Decoupled(UInt(256.W))                         // Task 3.4: from a 4-entry queue behind the
                                                               // registered record; ready never reaches the
                                                               // BRAM reads (credit rule, 4b section 7.8)
  val ovf_cnt = Output(UInt(16.W))                             // captures dropped on a full FIFO, saturating
}

/** Per-bank raw squash FIFOs in BRAM and one shared merge (design 4b).  Replaces IftSquashFifo.
  * The capture writes the raw row of a killed or rolled-back entry; the merge runs when the
  * entry leaves its FIFO.  Nothing here waits on commits or back-pressures the core (D6).
  *
  * @param dbg    elaborate the printf copies (IftSqDbg memories) and the printfs: Verilator
  *               printf builds only, never with the IFT bridge (review F7)
  * @param shadow also carry the capture-time packed record and assert equality (Verilator only)
  */
class IftSquashMerge(nBanks: Int, nSprob: Int, nIc: Int, nWb: Int, nMemord: Int,
                     dbg: Boolean, shadow: Boolean, depth: Int = 512)(implicit p: Parameters)
    extends BoomModule with CoreFuzzingConstants {
  require(isPow2(depth))
  require(dbg || !shadow, "the shadow check needs the printf copies")
  val io   = IO(new IftSquashMergeIO(nBanks, nSprob, nIc, nWb, nMemord, shadow))
  val ptrW = log2Ceil(depth)
  def entry() = new IftSqEntry(nSprob, nIc, nWb, nMemord)
  // Task 3.2: 1,186 bits with the DTLB columns (1,185 after Task 3.9); [FIFO10] step 10: 1,149 (DTLB columns removed, + ic_dropped);
  // step 11 (owner decision 2026-10-12): depth 1,024 -> 512, so each bank FIFO maps to RAMB36 in 512 x 72 simple-dual-port mode: 16 per bank, 64 total (was 32 / 128 as 1K x 36)
  println(s"[IftSquashMerge] raw entry ${entry().getWidth} bits x $depth x $nBanks banks" +
          s" = ${(entry().getWidth + 71) / 72} RAMB36 (512 x 72) per bank (dbg=$dbg shadow=$shadow)")

  // ---- raw FIFOs.  A read needs cnt > 0 and a write needs cnt < depth, so the two ports never
  //      address one entry in the same cycle: no read-under-write bypass is needed.
  val mem  = Seq.fill(nBanks)(SyncReadMem(depth, entry()))
  val dmem = if (dbg) Some(Seq.fill(nBanks)(SyncReadMem(depth, new IftSqDbg(shadow)))) else None
  val wr   = Seq.fill(nBanks)(RegInit(0.U(ptrW.W)))
  val rd   = Seq.fill(nBanks)(RegInit(0.U(ptrW.W)))
  val cnt  = Seq.fill(nBanks)(RegInit(0.U((ptrW + 1).W)))      // 0 .. depth; the top bit means full
  // [V3PRINT] SP-22 (design 5.5 / 9.7, FT-6): the maximum squash-FIFO occupancy over the banks, printed whenever it
  // grows; the last [SQOCC] line of a run is its maximum.  dbg builds only (IFT, debug prints, no bridge): 10 FF.
  if (dbg) {
    val pa  = boom.v3.util.CfStatePlusArg(true)
    val mx  = RegInit(0.U((ptrW + 1).W))
    val now = cnt.reduce((a, b) => Mux(a > b, a, b))
    when (now > mx) { mx := now }
    when (pa && now > mx) { printf("[SQOCC] max=%d\n", now) }
  }
  // The same arbiter class and policy as the old IftSquashFifo, on control only.  Its
  // lastGrant register has no reset (review F2): on the FPGA it starts at 0; in Verilator
  // RANDOMIZE_REG_INIT may start it elsewhere, which can rotate the lane order only.
  val arb  = Module(new RRArbiter(Bool(), nBanks))
  // arb.io.out.ready is the Task 3.4 credit rule, assigned after the output queue below.
  val drop = Wire(Vec(nBanks, Bool()))
  for (b <- 0 until nBanks) {
    val full = cnt(b)(ptrW)
    val push = io.cap(b).valid && !full            // as Queue(pipe = false): full rejects even when popping
    drop(b) := io.cap(b).valid && full
    when (push) {
      mem(b).write(wr(b), io.cap(b).bits)
      dmem.foreach(m => m(b).write(wr(b), io.dbg(b)))
      wr(b) := wr(b) + 1.U
    }
    arb.io.in(b).valid := cnt(b) =/= 0.U
    arb.io.in(b).bits  := false.B
    val pop = arb.io.in(b).fire
    when (pop) { rd(b) := rd(b) + 1.U }
    cnt(b) := cnt(b) + push.asUInt - pop.asUInt
  }
  // the same saturating overflow count as the old IftSquashFifo, on the same events
  val ovf     = RegInit(0.U(16.W))
  val ovf_sum = ovf +& PopCount(drop)
  ovf := Mux(ovf_sum(16), "hFFFF".U(16.W), ovf_sum(15, 0))
  io.ovf_cnt := ovf

  // ---- read, cycle t1: the granted bank presents its head; the BRAM registers address and enable.
  //      The grant is combinational from the cnt registers through the arbiter (review F3).
  val gnt  = arb.io.in.map(_.fire)
  val rdat = VecInit((0 until nBanks).map(b => mem(b).read(rd(b), gnt(b))))
  val rdbg = dmem.map(m => VecInit((0 until nBanks).map(b => m(b).read(rd(b), gnt(b)))))

  // ---- merge, cycle t1+1: BRAM output latch -> 4:1 bank mux -> merge -> pack
  val m_valid = RegNext(arb.io.out.fire, false.B)
  val m_sel   = RegEnable(arb.io.chosen, arb.io.out.fire)
  val e       = rdat(m_sel)
  val (info, _) = IftMerge(e.raw)                              // D19 is counted at capture (Rob)
  // [RECV3] SP-04: the v3 SQUASH record (IftSquashPack above): class 2, kind and capture stamp from the side data.
  val rec = IftSquashPack(e.side, e.raw.domain, info)

  // ---- lane, cycle t1+2: the record is registered once, in the output queue's entry (no
  //      merge logic on the path to the bridge).  Task 3.9 removed the separate o_valid/o_rec
  //      register stage that Task 3.4 had kept in front of the queue: it duplicated the
  //      queue's storage (257 flops) and added a cycle of lane latency; the credit rule below
  //      keeps the same guarantee with one in-flight stage instead of two.
  // ---- Task 3.4 (D18, 4b section 7.8, ruling 63 Q7): the squash lane is shared with the
  //      kill records in core.scala, so deq has a ready.  It enters ONLY this 4-entry queue;
  //      the BRAM reads are governed by the credit rule: a grant at t1 lands in oq at t1+1
  //      (the merge cycle), and at most one grant is in flight (m_valid at t1+1), so granting
  //      only while count + m_valid < 4 guarantees room for every in-flight record.  No BRAM
  //      read is wasted and no record is lost after the FIFO.  With the lane free the count
  //      stays <= 1 and the merge grants every cycle; with the lane taken the queue fills,
  //      grants stop, and the raw FIFOs absorb the captures (overflow counted, D6).
  val m_en = rdbg.map(dv => dv(m_sel).en)                       // printf builds: the [SQREC] enable rides along
  val oq = Module(new Queue(UInt((if (dbg) 257 else 256).W), 4))
  oq.io.enq.valid := m_valid
  oq.io.enq.bits  := m_en.map(en => Cat(en, rec)).getOrElse(rec)
  io.deq.valid    := oq.io.deq.valid
  io.deq.bits     := oq.io.deq.bits(255, 0)
  oq.io.deq.ready := io.deq.ready
  assert(!(oq.io.enq.valid && !oq.io.enq.ready), "[IftSquashMerge] output queue overflow: the credit rule is broken")
  arb.io.out.ready := (oq.io.count +& m_valid.asUInt) < 4.U

  // ---- printf builds: [FLUSH] from the shipped path, [SQREC], [SQDROP], [SQRAW], shadow check
  rdbg.foreach { dv =>
    val d     = dv(m_sel)
    val cause = e.side.kind
    when (m_valid && d.en) {
      val fl_cross_domain = !cause && (d.fl_dom =/= e.raw.domain)
      val fl_is_secret    = !cause && d.fl_sec
      val fl_op_count     = Mux(cause, 0.U, d.fl_oc)
      val inflFmt = (0 until numInfluencerSlotsCF).map(k => s"I$k={v=%d,oc=%d,ty=%d,atk=%d,sec=%d}").mkString(" ")
      val fmt = s"[FLUSH] 0x%x (0x%x) CF(domain=%d spec=%d atk=%d s_acc=%d s_prop=%d s_tx=%d opcount=%d cyc=%d fl=%d fl_sec=%d floc=%d FU=0x%x SRC=%d OVF=%d dcy=%d srob=%d sstq=%d alias=%d sftq=%d dc=%d/%d/%d pdst=%d prs1=%d prs2=%d rt1=%d rt2=%d brmask=0x%x brtag=%d abr=%d sbr=%d) $inflFmt\n"
      val inflArgs = (0 until numInfluencerSlotsCF).flatMap(k => Seq[Bits](
        info.influencer_list(k).valid, info.influencer_list(k).key, info.influencer_list(k).infl_type,
        info.influencer_list(k).is_atk, info.influencer_list(k).is_secret))
      printf(fmt, (Seq[Bits](
        Sext(d.pc, xLen), d.inst,
        e.raw.domain, info.speculated, e.side.atk || info.edge_atk,
        e.side.s_acc, e.side.s_prop, e.side.s_tx,
        e.side.op_count, e.side.stamp,                       // [RECV3] cyc = the record's capture stamp
        fl_cross_domain, fl_is_secret, fl_op_count,
        info.fu_bitmap, Mux(cause, 4.U, 3.U), info.infl_dropped,
        info.cntd_deny_count, info.stall_cycles_rob, info.stall_cycles_stq, info.infl_oc_aliased,
        info.sftq, info.dc.kind, info.dc.set, info.dc.way,   // [RECV3] sftq field (SP-09), D$ footprint (SP-08)
        e.side.pdst, e.side.prs1, e.side.prs2, e.side.lrs1_rtype, e.side.lrs2_rtype, d.br_mask, d.br_tag,
        info.atk_branch_ctr, info.sec_branch_ctr) ++ inflArgs): _*)
    }
    d.old_rec.foreach { o =>
      assert(!m_valid || rec === o, "[rob] 4b squash record differs from the capture-time record (Task 2.10d shadow)")
    }
    // The packed record exactly as io.ift_squash hands it to the core (bridge lane), for
    // enabled records only (review F6).  Task 3.4: on deq.fire, with the enable bit that
    // travelled through the output queue (bit 256 in printf builds).
    when (io.deq.fire && oq.io.deq.bits(256)) { printf("[SQREC] %x\n", io.deq.bits) }
    for (b <- 0 until nBanks) {
      when (drop(b)) {
        printf("[SQDROP] bank=%d SRC=%d opcount=%d pc=0x%x\n", b.U,
               Mux(io.cap(b).bits.side.kind, 4.U, 3.U), io.cap(b).bits.side.op_count, io.cap(b).bits.side.pc)
      }
      val hw = RegInit(0.U((ptrW + 1).W))
      when (cnt(b) > hw) { hw := cnt(b); printf("[SQRAW] bank=%d hw=%d\n", b.U, cnt(b)) }
    }
  }
}
