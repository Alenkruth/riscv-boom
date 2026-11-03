package boom.v3.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import boom.v3.common._

// [Doppelganger] dgl-plan-v4.md N1.  Paper 5 (A, B), Table 1: PC-indexed, full-PC-tagged stride predictor in address-prediction
// mode, 1024 entries x 8 ways, looked up by every decoded load, trained only by committed loads.
class DglTrainReq(implicit p: Parameters) extends BoomBundle {
  val pc   = UInt((coreMaxAddrBits-1).W)   // PC[coreMaxAddrBits-1:1]
  val addr = UInt(coreMaxAddrBits.W)       // committed address (LDQ addr.bits; = virtual with paging off)
  val sec  = Bool(); val atk = Bool()      // IFT labels of the committed address (0 without IFT)
}
class DglPred(implicit p: Parameters) extends BoomBundle {
  val v = Bool(); val addr = UInt(coreMaxAddrBits.W); val sec = Bool(); val atk = Bool()
}

class DglPredictor(implicit p: Parameters) extends BoomModule {
  val io = IO(new Bundle {
    val en     = Input(Bool())
    val dbg    = Input(Bool())
    val lk_pc  = Input(Vec(coreWidth, UInt(coreMaxAddrBits.W)))   // decode cycle
    val lk_v   = Input(Vec(coreWidth, Bool()))                     // decode cycle: a plain load in lane w
    val lk_adv = Input(Bool())                                     // decode cycle: REN2 loads this group at the edge
    val pred   = Output(Vec(coreWidth, new DglPred))               // REN2 cycle, lane w
    val train  = Input(Vec(coreWidth, Valid(new DglTrainReq)))     // commit cycle
    val drops  = Output(UInt(12.W))
  })
  val W  = doppelgangerWays; val S = doppelgangerSets; val IB = doppelgangerSetBits
  val AW = coreMaxAddrBits;  val PCW = AW - 1; val TW = PCW - IB; val SW = 32
  def setOf(pc: UInt): UInt = pc(IB-1, 0)          // pc = PC[AW-1:1]
  def tagOf(pc: UInt): UInt = pc(PCW-1, IB)        // full tag
  class LkWay extends Bundle { val v = Bool(); val tag = UInt(TW.W); val pred = UInt(AW.W); val ok = Bool(); val sec = Bool(); val atk = Bool() }
  class TrWay extends Bundle { val v = Bool(); val tag = UInt(TW.W); val last = UInt(AW.W); val stride = SInt(SW.W); val conf = UInt(2.W); val sec = Bool(); val atk = Bool() }
  class TrRow extends Bundle { val way = Vec(W, new TrWay); val plru = UInt((W-1).W) }
  val plru = new freechips.rocketchip.util.PseudoLRU(W)   // pure functions get_next_state / get_replace_way (its own state reg is unused: pruned)

  // ---- storage: one read copy per decode lane + one train copy; every copy written with whole rows (no masked writes) ----
  val lk_mem = Seq.fill(coreWidth)(SyncReadMem(S, Vec(W, new LkWay), SyncReadMem.ReadFirst))
  val tr_mem = SyncReadMem(S, new TrRow, SyncReadMem.ReadFirst)
  def lkRow(t: TrRow): Vec[LkWay] = VecInit(t.way.map { x =>
    val l = Wire(new LkWay)
    l.v := x.v; l.tag := x.tag; l.sec := x.sec; l.atk := x.atk
    l.pred := (x.last.zext + x.stride).asUInt(AW-1, 0)            // address-prediction mode: last committed + stride
    l.ok   := x.conf >= doppelgangerConfThreshold.U
    l })

  // ---- reset sweep: 128 cycles writing zero rows (BRAM content is not reset; Verilator randomizes it) ----
  val init_busy = RegInit(true.B); val init_idx = RegInit(0.U(IB.W))
  when (init_busy) { init_idx := init_idx + 1.U; when (init_idx === (S-1).U) { init_busy := false.B } }

  // ---- write stage T2 ----
  val wr_v   = RegInit(false.B); val wr_idx   = Reg(UInt(IB.W)); val wr_tr   = Reg(new TrRow)
  val wr_v_q = RegNext(wr_v, false.B); val wr_idx_q = RegNext(wr_idx); val wr_tr_q = RegNext(wr_tr)
  // [T1d-fix1 2026-11-01] ONE write call per memory: the init sweep and the train write were two .write() calls, which firtool emits as two
  // write ports (W0, W1) on one memory; Vivado cannot infer BRAM/LUTRAM for that ("multiple writes via different ports", Synth 8-3391)
  // and 77-112 kbit cannot dissolve into FFs. Same priority as before (init sweep first); +1 mux level on flop-sourced write data.
  val mw_en  = init_busy || wr_v
  val mw_idx = Mux(init_busy, init_idx, wr_idx)
  val mw_tr  = Mux(init_busy, 0.U.asTypeOf(new TrRow), wr_tr)
  val mw_lk  = Mux(init_busy, 0.U.asTypeOf(Vec(W, new LkWay)), lkRow(wr_tr))
  when (mw_en) {
    tr_mem.write(mw_idx, mw_tr)
    lk_mem.foreach(_.write(mw_idx, mw_lk))
  }

  // ---- lookup: decode-cycle read, REN2-cycle compare/select, held while REN2 stalls ----
  val lk_fresh = RegNext(io.lk_adv, false.B)
  val lk_live  = RegNext(!init_busy, false.B)
  for (w <- 0 until coreWidth) {
    val pc   = io.lk_pc(w)(AW-1, 1)
    val row  = lk_mem(w).read(setOf(pc), true.B)
    val qtag = RegNext(tagOf(pc))
    val qv   = RegNext(io.lk_v(w), false.B)
    val hits = VecInit(row.map(e => e.v && e.tag === qtag))
    val sel  = Mux1H(hits, row)
    val now  = Wire(new DglPred)
    now.v    := lk_live && io.en && qv && hits.asUInt.orR && sel.ok
    now.addr := sel.pred; now.sec := sel.sec; now.atk := sel.atk
    val hold = RegInit(0.U.asTypeOf(new DglPred))
    val out  = Mux(lk_fresh, now, hold)
    hold := out
    io.pred(w) := out
    assert(!(lk_live && PopCount(hits) > 1.U), "[Doppelganger] one tag in two ways")
  }

  // ---- training queue: coreWidth LUTRAM banks, round robin, order preserved; overflow dropped and counted ----
  val NB = coreWidth; val BD = doppelgangerTrainQueueDepth / NB; val BW = log2Ceil(NB); val DW = log2Ceil(BD)
  val tq    = Seq.fill(NB)(Mem(BD, new DglTrainReq))
  val tq_hd = RegInit(VecInit(Seq.fill(NB)(0.U(DW.W)))); val tq_tl = RegInit(VecInit(Seq.fill(NB)(0.U(DW.W))))
  val pb    = RegInit(0.U(BW.W)); val qb = RegInit(0.U(BW.W))
  val cnt   = RegInit(0.U(log2Ceil(doppelgangerTrainQueueDepth + 1).W))
  val drops = RegInit(0.U(12.W)); io.drops := drops
  val pop   = !init_busy && io.en && cnt =/= 0.U
  val pv    = io.train.map(t => t.valid && io.en)
  val older = pv.scanLeft(0.U((BW+1).W))((c, x) => c + x.asUInt)          // valid pushes in lower lanes
  val room  = doppelgangerTrainQueueDepth.U - cnt + pop.asUInt
  val acc   = (0 until NB).map(w => pv(w) && older(w) < room)
  for (k <- 0 until NB) {
    val sel = (0 until NB).map(w => acc(w) && (pb + older(w)(BW-1, 0)) === k.U)
    when (sel.reduce(_||_)) {                                              // one write port per bank
      tq(k).write(tq_tl(k), Mux1H(sel, io.train.map(_.bits)))
      tq_tl(k) := tq_tl(k) + 1.U
    }
  }
  val nacc = PopCount(acc); val nreq = PopCount(pv)
  pb  := pb + nacc(BW-1, 0)
  cnt := cnt + nacc - pop.asUInt
  when (nreq =/= nacc && drops =/= 0xFFF.U) { drops := drops + 1.U }       // a cycle with dropped pushes counts once
  val front = VecInit(tq.zipWithIndex.map { case (m, k) => m(tq_hd(k)) })(qb)   // asynchronous LUTRAM read
  when (pop) { tq_hd(qb) := tq_hd(qb) + 1.U; qb := qb + 1.U }

  // ---- train T0 (read) / T1 (compute) ----
  val t1_v  = RegNext(pop, false.B)
  val t1    = RegEnable(front, pop)
  val tr_rd = tr_mem.read(setOf(front.pc), pop)
  val t1_set = setOf(t1.pc); val t1_tag = tagOf(t1.pc)
  val row   = Mux(wr_v && wr_idx === t1_set, wr_tr, Mux(wr_v_q && wr_idx_q === t1_set, wr_tr_q, tr_rd))   // two-stage bypass
  val hits  = VecInit(row.way.map(e => e.v && e.tag === t1_tag))
  val hit   = hits.asUInt.orR
  val inv   = VecInit(row.way.map(e => !e.v))
  val way   = Mux(hit, OHToUInt(hits), Mux(inv.asUInt.orR, PriorityEncoder(inv), plru.get_replace_way(row.plru)))
  val e     = row.way(way)
  val diff  = t1.addr.zext - e.last.zext                                    // (AW+1)-bit signed
  val in_rng = diff >= (-(BigInt(1) << (SW-1))).S && diff <= ((BigInt(1) << (SW-1)) - 1).S
  val dsw   = diff(SW-1, 0).asSInt
  val same  = in_rng && dsw === e.stride
  val nw    = Wire(new TrWay)
  nw.v      := true.B; nw.tag := t1_tag; nw.last := t1.addr
  nw.stride := Mux(!hit || !in_rng, 0.S(SW.W), dsw)
  nw.conf   := Mux(!hit, 0.U, Mux(same, Mux(e.conf === 3.U, 3.U, e.conf + 1.U), 0.U))
  nw.sec    := (hit && e.sec) || t1.sec
  nw.atk    := (hit && e.atk) || t1.atk
  wr_v := t1_v
  when (t1_v) {
    wr_idx := t1_set
    for (k <- 0 until W) { wr_tr.way(k) := Mux(way === k.U, nw, row.way(k)) }
    wr_tr.plru := plru.get_next_state(row.plru, way)                        // replacement state changes only by training
  }
  if (ENABLE_CF_DEBUG_PRINTF) {
    when (io.dbg && t1_v) { printf("[Doppelganger] TRAIN pc=0x%x addr=0x%x way=%d hit=%d stride=%d conf=%d\n",
                                   Cat(t1.pc, 0.U(1.W)), t1.addr, way, hit, nw.stride, nw.conf) }
    for (w <- 0 until coreWidth) { when (io.dbg && lk_fresh && io.pred(w).v) {
      printf("[Doppelganger] LOOKUP lane=%d pred=0x%x\n", w.U, io.pred(w).addr) } }
  }
}
