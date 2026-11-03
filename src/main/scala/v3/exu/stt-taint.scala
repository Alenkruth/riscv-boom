package boom.v3.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import boom.v3.common._
import boom.v3.util._

/** STT: runtime class gates, registered once in the core (C-CORE-1 (b)). */
class SttGates extends Bundle {
  val exp = Bool()   // loads / AMO address, DIV/REM, FDIV/FSQRT
  val br  = Bool()   // conditional branches, JALR/RET
  val sta = Bool()   // store address generation
  val dx  = Bool()   // [v6] DelayExecute: hold = "not yet at its own VP" instead of "root tainted" (STT_CTL[5] && en)
  val fut = Bool()   // [v6] Futuristic model (STT_CTL[1]), read by the dispatch-time own-VP test
}

/** STT: the decode/rename fields of one dispatch lane that the taint unit reads. */
class SttDisReq(implicit p: Parameters) extends BoomBundle {
  val lrs1       = UInt(lregSz.W)
  val lrs2       = UInt(lregSz.W)
  val lrs3       = UInt(lregSz.W)
  val lrs1_rtype = UInt(2.W)
  val lrs2_rtype = UInt(2.W)
  val frs3_en    = Bool()
  val ldst       = UInt(lregSz.W)
  val dst_rtype  = UInt(2.W)
  val ldst_val   = Bool()
  val is_ld      = Bool()            // access instruction: uses_ldq && !exception (it owns an LDQ entry)
  val is_mem     = Bool()            // uses_ldq || uses_stq: the gate root is the rs1 (address) root (A2)
  val ldq_idx    = UInt(ldqAddrSz.W)
  val br         = Bool()            // allocate_brtag
  val br_tag     = UInt(brTagSz.W)
}

class SttTaintUnit(implicit p: Parameters) extends BoomModule
{
  val io = IO(new Bundle {
    val req           = Input(Vec(coreWidth, new SttDisReq))
    val dis_valids    = Input(Vec(coreWidth, Bool()))    // pre-stall valids of the REN2 group
    val dis_fire      = Input(Vec(coreWidth, Bool()))    // the fired prefix
    val b2_mispredict = Input(Bool())
    val b2_br_tag     = Input(UInt(brTagSz.W))
    val flush         = Input(Bool())                    // RegNext(rob.io.flush.valid)
    val ldq_safe      = Input(UInt(numLdqEntries.W))     // LSU register (C-LSU-0)
    val ldq_head      = Input(UInt(ldqAddrSz.W))         // LSU register
    val gr            = Output(Vec(coreWidth, new SttRoot))
  })
  val W = coreWidth

  def none: SttRoot = 0.U.asTypeOf(new SttRoot)
  def mk(t: Bool, idx: UInt): SttRoot = { val r = Wire(new SttRoot); r.t := t; r.idx := idx; r }
  // a root whose load is safe or whose entry is free reads untainted (the per-cycle untaint and the read check)
  def live(r: SttRoot): SttRoot = mk(r.t && !io.ldq_safe(r.idx), r.idx)
  // youngest of n roots, all pairs compared at once (one compare level): an untainted root never wins; among equal
  // indices the lower position wins, so exactly one tainted root wins when any is tainted (4.0: tainted roots are live
  // LDQ entries between head and tail, where IsOlder is a strict order)
  def youngestN(rs: Seq[SttRoot]): SttRoot = {
    val n = rs.length
    if (n == 1) rs.head else {
      val win = (0 until n).map { i =>
        (0 until n).filter(_ != i).map { j =>
          !(rs(j).t && (IsOlder(rs(i).idx, rs(j).idx, io.ldq_head) || (rs(i).idx === rs(j).idx && (j < i).B)))
        }.foldLeft(rs(i).t)(_ && _)
      }
      mk(rs.map(_.t).reduce(_ || _), Mux1H(win, rs.map(_.idx)))
    }
  }

  // ---------------- tables: INT entry 0 (x0) is constant 0 and never written; FP f0..f31 are all writable
  val int_tab  = RegInit(VecInit(Seq.fill(32)(0.U.asTypeOf(new SttRoot))))
  val fp_tab   = RegInit(VecInit(Seq.fill(32)(0.U.asTypeOf(new SttRoot))))
  val int_snap = Reg(Vec(maxBrCount, Vec(32, new SttRoot)))      // entry 0 never written nor read (pruned)
  val fp_snap  = Reg(Vec(maxBrCount, Vec(32, new SttRoot)))
  val int_e    = (0 until 32).map(r => if (r == 0) none else live(int_tab(r)))   // effective entries this cycle
  val fp_e     = (0 until 32).map(r => live(fp_tab(r)))

  // ---------------- sources: s = 0 (rs1), 1 (rs2), 2 (rs3, FP only when frs3_en)
  def sTy(w: Int, s: Int): UInt = s match {
    case 0 => io.req(w).lrs1_rtype
    case 1 => io.req(w).lrs2_rtype
    case _ => Mux(io.req(w).frs3_en, RT_FLT, RT_X) }
  def sRg(w: Int, s: Int): UInt = s match { case 0 => io.req(w).lrs1; case 1 => io.req(w).lrs2; case _ => io.req(w).lrs3 }
  def isReg(ty: UInt): Bool = ty === RT_FIX || ty === RT_FLT           // RT_X and RT_PAS (CSR pass-through) are no source
  def tabRoot(w: Int, s: Int): SttRoot =
    Mux(sTy(w, s) === RT_FIX, VecInit(int_e)(sRg(w, s)(4,0)),
    Mux(sTy(w, s) === RT_FLT, VecInit(fp_e)(sRg(w, s)(4,0)), none))

  // ---------------- same-group producers: prodOH(w)(s)(j) = lane j < w is the YOUNGEST older lane writing source s of w
  def writes(j: Int, ty: UInt, r: UInt): Bool =
    io.dis_valids(j) && io.req(j).ldst_val && io.req(j).dst_rtype === ty && io.req(j).ldst === r
  val prodOH: Array[Array[Seq[Bool]]] = Array.tabulate(W, 3) { (w, s) =>
    val hits = (0 until w).map(j => isReg(sTy(w, s)) && writes(j, sTy(w, s), sRg(w, s)))
    if (w == 0) Seq[Bool]() else PriorityEncoderOH(hits.reverse).reverse }
  val ingrp: Array[Array[Bool]] = Array.tabulate(W, 3)((w, s) => if (w == 0) false.B else prodOH(w)(s).reduce(_ || _))
  // dep(w)(j): lane w reads a value of lane j; clo(w)(j): transitively (j < w). Computed in increasing w.
  val dep = Array.tabulate(W, W)((w, j) => if (j >= w) false.B else (0 until 3).map(s => prodOH(w)(s)(j)).reduce(_ || _))
  val clo = Array.ofDim[Bool](W, W)
  for (w <- 0 until W; j <- 0 until W)
    clo(w)(j) = if (j >= w) false.B else (dep(w)(j) +: ((j + 1) until w).map(m => dep(w)(m) && clo(m)(j))).reduce(_ || _)

  // ---------------- roots
  val is_ld   = (0 until W).map(w => io.dis_valids(w) && io.req(w).is_ld)
  // own_tab(w): youngest TABLE root among w's sources that no older lane of the group produces
  val own_tab = (0 until W).map(w => youngestN((0 until 3).map(s => Mux(ingrp(w)(s), none, tabRoot(w, s)))))
  // ysrc(w): youngest root over ALL of w's sources (the paper's yrot = max over the sources, l.735-743), flattened:
  // a load of the group reachable from w dominates every table root (it is younger, 4.0) and the youngest such load is
  // reached without passing another load; otherwise only table roots of the reachable non-load lanes count.
  val ysrc = (0 until W).map { w =>
    if (w == 0) own_tab(0) else {
      val ld_c   = (0 until w).map(j => clo(w)(j) && is_ld(j))
      val ld_sel = PriorityEncoderOH(ld_c.reverse).reverse
      val ld_r   = mk(true.B, Mux1H(ld_sel, (0 until w).map(j => io.req(j).ldq_idx)))
      val tab_r  = youngestN(own_tab(w) +: (0 until w).map(j => Mux(clo(w)(j), own_tab(j), none)))
      Mux(ld_c.reduce(_ || _), ld_r, tab_r)
    }
  }
  // out(w): the root lane w's destination carries: a load is its own root (AccessInstrIdx), else its yrot (YRoT)
  val out = (0 until W).map(w => Mux(io.req(w).is_ld, mk(true.B, io.req(w).ldq_idx), ysrc(w)))
  // r1(w): the root of rs1 alone (the address of a memory op)
  val r1  = (0 until W).map(w => if (w == 0) tabRoot(0, 0) else Mux(ingrp(w)(0), Mux1H(prodOH(w)(0), out.take(w)), tabRoot(w, 0)))
  for (w <- 0 until W) io.gr(w) := Mux(io.req(w).is_mem, r1(w), ysrc(w))

  // ---------------- writes: lane order, later lanes win (as the map table's remap); after(k) = state after lanes 0..k-1.
  // Two scans: the table keeps what the FIRED lanes wrote; the snapshots use the VALID lanes, so the 8.8 k snapshot flops
  // never see dis_fire (the dispatch-stall family F3). A valid branch lane that does not fire rewrites its snapshot every
  // cycle until it fires; when it fires every valid older lane fires too, so its snapshot is then exact.
  def writeT(tab: Seq[SttRoot], w: Int, en: Bool, ty: UInt, isInt: Boolean): IndexedSeq[SttRoot] = (0 until 32).map { r =>
    val hit = en && io.req(w).ldst_val && io.req(w).dst_rtype === ty && io.req(w).ldst === r.U
    if (isInt && r == 0) none else Mux(hit, out(w), tab(r)) }
  val int_after  = (0 until W).scanLeft(int_e)((t, w) => writeT(t, w, io.dis_fire(w),   RT_FIX, true))
  val fp_after   = (0 until W).scanLeft(fp_e) ((t, w) => writeT(t, w, io.dis_fire(w),   RT_FLT, false))
  val int_afterv = (0 until W).scanLeft(int_e)((t, w) => writeT(t, w, io.dis_valids(w), RT_FIX, true))
  val fp_afterv  = (0 until W).scanLeft(fp_e) ((t, w) => writeT(t, w, io.dis_valids(w), RT_FLT, false))
  // per-branch snapshots: the state after the branch's own lane (a JALR's link register included), as
  // rename-maptable.scala:101-105. The tag of a valid REN2 branch belongs to no dispatched branch, so an early write is inert.
  for (w <- 0 until W) {
    when (io.dis_valids(w) && io.req(w).br) {
      for (r <- 1 until 32) int_snap(io.req(w).br_tag)(r) := int_afterv(w + 1)(r)
      for (r <- 0 until 32) fp_snap(io.req(w).br_tag)(r)  := fp_afterv(w + 1)(r)
    }
  }
  val int_nxt = WireInit(VecInit(int_after(W)))
  val fp_nxt  = WireInit(VecInit(fp_after(W)))
  when (io.b2_mispredict) {                                   // exact restore (l.766-768); no lane fires in this cycle
    for (r <- 1 until 32) int_nxt(r) := live(int_snap(io.b2_br_tag)(r))
    for (r <- 0 until 32) fp_nxt(r)  := live(fp_snap(io.b2_br_tag)(r))
  }
  when (io.flush) {                                           // all speculation gone: architectural registers are clean
    for (r <- 0 until 32) { int_nxt(r) := none; fp_nxt(r) := none }
  }
  int_nxt(0) := none
  int_tab := int_nxt
  fp_tab  := fp_nxt

  if (ENABLE_CF_DEBUG_PRINTF) {
    assert(!(io.b2_mispredict && io.dis_fire.reduce(_ || _)), "[STT] dispatch fired in the b2 cycle")
    assert(!(io.flush && io.dis_fire.reduce(_ || _)), "[STT] dispatch fired in the flush cycle")
  }
}
