// Lockstep equivalence bench for the Ceviche B1 + N1 CevCRF (key-sliced BRAM CAM, FREQ decode in the CRF) against the flop CRF it replaces.
// Task-reports/ceviche-bram-cam-20261105/b1/NOTE.md.  CevCRFLegacy below is the CRF of the live tree (generators/boom 9be80c21, ceviche.scala base
// sha1 c98081f6) unchanged except for its name; the old bench (ceviche-bench.scala) runs its CRF sections on it.
// Run (one sbt session, see NOTE.md): Test / runMain boom.v3.common.CevB1Bench [seed=<n>] [cycles=<n>] [section names]
// Every output of both CRFs is compared EVERY cycle in hardware (CevCrfPair.mm); the traffic obeys the cache protocols:
//   D$: key changes only by an allocating write; D$ keys are unique among valid lines; non-allocating writes carry the line's key.
//   I$: key changes only by the first-beat write (non-allocating, v = 0); refill_done (allocating, same key) comes >= 3 cycles later;
//       (key, domain) unique among valid lines; one refill at a time.
// The write queue occupancy is mirrored in Scala (QModel) so the generator never overflows the 8-entry queue (overflow has its own test).
package boom.v3.common

import chisel3._
import chisel3.util._
import chisel3.simulator.EphemeralSimulator._
import scala.util.Random

// One entry per cache line; entry index = line number (paper 4: registers = lines, 1:1; ambiguity 1.4-4).
class CevCRFLegacy(val nLines: Int, val keyBits: Int, val nSearch: Int, val isI: Boolean, val withSec: Boolean,
             val expireCycles: Int, val withTaint: Boolean = false, val withIftDom: Boolean = false) extends Module {
  val lineBits = log2Ceil(nLines)
  val io = IO(new Bundle {
    // ---- search (combinational within the caller's s1; caller registers the results) ----
    val sKey   = Input(Vec(nSearch, UInt(keyBits.W)))
    val sDom   = Input(Vec(nSearch, Bool()))            // I$: the requester's domain; D$: unused (masked search)
    val sShare = Input(Vec(nSearch, Bool()))            // I$: requester's policy S bit; D$: unused
    val sHit   = Output(Vec(nSearch, Bool()))
    val sLine  = Output(Vec(nSearch, UInt(lineBits.W)))
    val sCoh   = Output(Vec(nSearch, UInt(2.W)))        // D$ only (state of ClientMetadata); I$ drives 0
    val sDomOut= Output(Vec(nSearch, Bool()))           // owner domain of the matched line (IFT, prints)
    val sSec   = Output(Vec(nSearch, Bool()))           // I$ IFT secret mark of the matched line
    // [T-IFT v5] (I$ + IFT builds, withIftDom) the matched line's IFT domain: the value the stock ift_tag_meta holds for the line (the fill's
    // IFT domain incl. the [M3ATKWIN] attacker-fill term), kept apart from `dom` (the Ceviche owner domain, PC range only) so the I$ IFT reads
    // its own state in Ceviche mode, not the defense's.
    val sIftDom = if (withIftDom) Some(Output(Vec(nSearch, Bool()))) else None
    val sMatch = Output(Vec(nSearch, UInt(nLines.W)))   // the one-hot match vector (FREQ increment, registered by caller)
    val sRsv   = Output(Vec(nSearch, Bool()))           // the matched line is reserved as an in-flight victim
    // ---- FREQ ----
    val incVec = Input(UInt(nLines.W))                  // registered s1 match vector(s) ORed by the caller, gated by s2 validity
    // [T-IFT v4, B1] IFT replacement taint (D$ + IFT builds only, withTaint): the same vector as incVec restricted to the increments whose
    // access is secret- / attacker-attributed (the caller builds them exactly like incVec).
    val incSecVec = if (withTaint) Some(Input(UInt(nLines.W))) else None
    val incAtkVec = if (withTaint) Some(Input(UInt(nLines.W))) else None
    // ---- write (mirror of the cache's metadata write / I$ refill_done); at most one per cycle ----
    val wr = Input(Valid(new Bundle {
      val line  = UInt(lineBits.W)
      val key   = UInt(keyBits.W)
      val v     = Bool()
      val coh   = UInt(2.W)
      val alloc = Bool()          // allocating final write: set dom, freq := 5, s/inv/sec, clear rsv
      val dom   = Bool()
      val s     = Bool()
      val inv   = Bool()
      val sec   = Bool()
      val ts    = if (withTaint) Some(Bool()) else None   // [T-IFT v4, B1] the allocating MSHR's own secret / attacker terms (alloc only)
      val ta    = if (withTaint) Some(Bool()) else None
      val idom  = if (withIftDom) Some(Bool()) else None  // [T-IFT v5] the fill's IFT domain (alloc only)
    }))
    // ---- reservations (VSU grant; cleared by wr.alloc) ----
    val rsvSet = Input(Valid(UInt(lineBits.W)))
    val rsvSetDom = Input(Bool())                       // domain the reserved line is granted to (VSU requester), with rsvSet
    val rsvHold = Input(Valid(UInt(lineBits.W)))   // [R1c B1] D$: an upgrade MSHR reserves its own hit line (rsvDom := the line's dom: count unchanged)
    val rsvRel  = Input(Valid(UInt(lineBits.W)))   // [R1c B1] D$: an upgrade's final (non-allocating) write releases it
    // ---- bulk revocation ----
    val clearAll = Input(Bool())                        // mode switch / wipe end / I$ debug flush: v := 0, rsv := 0
    val clearDom = Input(Valid(Bool()))                 // I$ fence.i by domain d: v := 0 where dom == d || inv
    // ---- VSU candidate read port ----
    val cIdx   = Input(UInt(lineBits.W))
    val cV     = Output(Bool()); val cDom = Output(Bool()); val cFreq = Output(UInt(4.W)); val cRsv = Output(Bool())
    val cTS    = if (withTaint) Some(Output(Bool())) else None   // [T-IFT v4, B1] the candidate line's replacement taint (same 1:nLines mux as cFreq)
    val cTA    = if (withTaint) Some(Output(Bool())) else None
    // ---- free pool / occupancy ----
    val poolEmpty = Output(Bool())
    val freeIdx   = Output(UInt(lineBits.W))            // lowest-index free line (valid when !poolEmpty)
    val occ       = Output(Vec(2, UInt((lineBits + 1).W)))
  })
  val key  = Reg(Vec(nLines, UInt(keyBits.W)))
  val dom  = Reg(Vec(nLines, Bool()))
  val v    = RegInit(VecInit(Seq.fill(nLines)(false.B)))
  // [TF-LUTRAM] (FPGA timing fit, owner 2026-11-03, task-reports/fpga-timing-fit-20261103.md row 3 (a)): coh, sec and idom are
  // read ONLY at the matched line (the search ports) and written ONLY by the single io.wr port, never reset, never touched by
  // clearAll / clearDom / rsv* / the FREQ update, and never read in parallel (CAM, free pool, occupancy).  They therefore live in
  // combinational-read Mems (Vivado: distributed RAM, 1 write port + nSearch read ports) instead of nLines flops each, and the
  // nLines:1 Mux1H AND-OR trees become an index read at OHToUInt(match) (the sLine encoder, already built).  Semantics: identical.
  //   - write: same port, same enable, same cycle (a Mem write is visible from the next cycle, like the Reg it replaces);
  //   - read: combinational in the same cycle, old data during a same-cycle write (as the Reg); a miss drives 0 (the Mux1H
  //     value of an all-zero select), via the hit gate below;
  //   - no reset before or after (the Reg Vecs had none either, so a read of a never-written entry is equally undefined in both;
  //     in practice every valid line was written: coh by every wr, sec/idom by the allocating wr, and the CAM matches v lines only).
  // Only differs if the match is not one-hot (Mux1H ORs the entries, the index read takes the OR of the indices): that state is
  // excluded by construction (G3) and asserted in the D$ ("Ceviche D$: multiple CRF matches").
  // Kept in flops (each has a parallel reader or more than one writer): key, v (CAM / free pool / occ / bulk clears), dom (I$ CAM,
  // occ, clearDom), freq (per-line increment + tick every cycle), rsv (free pool, occ, clearAll), rsvDom (occ popcount; 2 writers
  // rsvHold + rsvSet), s (I$ CAM), inv (clearDom), tS / tA (written by the parallel incVec AND by wr.alloc in the same cycle).
  val coh  = if (!isI) Some(Mem(nLines, UInt(2.W))) else None   // D$ only (was a Reg Vec; the I$ never read it)
  val freq = Reg(Vec(nLines, UInt(4.W)))
  val rsv  = RegInit(VecInit(Seq.fill(nLines)(false.B)))
  val rsvDom = Reg(Vec(nLines, Bool()))                 // domain of the reservation; read only while rsv (no reset)
  val s    = Reg(Vec(nLines, Bool()))                   // I$ only
  val inv  = Reg(Vec(nLines, Bool()))                   // I$ only
  val sec  = if (withSec) Some(Mem(nLines, Bool())) else None   // I$ IFT only [TF-LUTRAM] (was a Reg Vec; unread when !withSec)
  // [T-IFT v4, B1] per-line replacement-state taint {secret, attacker}: the stock per-set ift_repl_sec/atk rule moved to the line, the
  // line being what the FREQ (access-driven) state belongs to.  Latest advancer wins: an increment overwrites it with that access's own
  // terms, the allocating write with the allocating MSHR's.  Read only for valid, unreserved candidates (no reset needed, like freq).
  val tS   = if (withTaint) Some(Reg(Vec(nLines, Bool()))) else None
  val tA   = if (withTaint) Some(Reg(Vec(nLines, Bool()))) else None
  val idom = if (withIftDom) Some(Mem(nLines, Bool())) else None   // [T-IFT v5] I$ IFT domain of the line (no reset: read only on a hit) [TF-LUTRAM]

  // ---- search: own match and (I$) shared match; one-hot encode with OR trees ----
  for (p <- 0 until nSearch) {
    val tagEq   = VecInit((0 until nLines).map(i => v(i) && key(i) === io.sKey(p))).asUInt
    val domVec  = dom.asUInt
    val m = if (!isI) tagEq                              // D$: domain-masked (CEV4-1 (a))
            else {
              // own copy: dom == requester; shared copy (paper 5.3): other domain, requester policy S && line S.
              // The requester domain is a late select (2:1 at the end of each tree), off the compare path.
              val own1 = tagEq & domVec;  val own0 = tagEq & ~domVec
              val shr1 = tagEq & ~domVec & s.asUInt; val shr0 = tagEq & domVec & s.asUInt
              val own  = Mux(io.sDom(p), own1, own0)
              val shr  = Mux(io.sDom(p), shr1, shr0) & Fill(nLines, io.sShare(p))
              Mux(own.orR, own, shr)                     // own copy first; at most one of each kind exists (G3)
            }
    val hit  = m.orR
    val line = OHToUInt(m)                               // OR-tree encoder (exactly one bit set on a hit)
    io.sMatch(p)  := m
    io.sHit(p)    := hit
    io.sLine(p)   := line
    // [TF-LUTRAM] index reads at the encoded match (one LUTRAM read port per search port), gated by the hit = the old Mux1H value.
    // Levels: encoder (already on the sLine path) + the LUTRAM address-to-data mux (RAM64 + F7/F8 + 1 LUT for 1024 lines) in place
    // of the 1024:1 AND-OR; about +1..2 LUT levels on sCoh (D$: ends at the s2_cev_coh flop) and sSec / sIftDom (I$ s1 IFT flags).
    io.sCoh(p)    := (if (!isI) Mux(hit, coh.get(line), 0.U) else 0.U)
    io.sDomOut(p) := Mux1H(m, dom)
    io.sSec(p)    := (if (withSec) hit && sec.get(line) else false.B)
    io.sIftDom.foreach(_(p) := hit && idom.get(line))   // [T-IFT v5] same index read as sSec [TF-LUTRAM]
    io.sRsv(p)    := Mux1H(m, rsv)                       // the matched line is a granted victim (D3b nack)
  }

  // ---- expiration tick and FREQ (paper 5.2): sat +1 on access, sat -1 at each interval, 5 on allocation ----
  val tickCnt = RegInit(0.U(log2Ceil(expireCycles).W)); tickCnt := tickCnt + 1.U
  val tick    = tickCnt === (expireCycles - 1).U
  for (i <- 0 until nLines) {
    val inc  = io.incVec(i)
    val up   = Mux(inc && freq(i) =/= 15.U, freq(i) + 1.U, freq(i))
    val down = Mux(tick && up =/= 0.U, up - 1.U, up)
    freq(i) := down
    when (io.wr.valid && io.wr.bits.alloc && io.wr.bits.line === i.U) { freq(i) := 5.U }   // last connect wins
  }
  if (withTaint) {   // [T-IFT v4, B1] one update LUT per line beside the FREQ update, enabled by the same per-line incVec bit
    for (i <- 0 until nLines) when (io.incVec(i)) { tS.get(i) := io.incSecVec.get(i); tA.get(i) := io.incAtkVec.get(i) }
  }

  // ---- write mirror ----
  when (io.wr.valid) {
    val l = io.wr.bits.line
    key(l) := io.wr.bits.key; v(l) := io.wr.bits.v
    coh.foreach(_.write(l, io.wr.bits.coh))              // [TF-LUTRAM] the Mems' only write port (same enable as before)
    when (io.wr.bits.alloc) { dom(l) := io.wr.bits.dom; s(l) := io.wr.bits.s; inv(l) := io.wr.bits.inv
                              sec.foreach(_.write(l, io.wr.bits.sec)); rsv(l) := false.B       // [TF-LUTRAM]
                              idom.foreach(_.write(l, io.wr.bits.idom.get))                    // [T-IFT v5] [TF-LUTRAM]
                              if (withTaint) { tS.get(l) := io.wr.bits.ts.get; tA.get(l) := io.wr.bits.ta.get } }   // [T-IFT v4, B1] after the increment: wins
  }
  when (io.rsvRel.valid)  { rsv(io.rsvRel.bits) := false.B }
  when (io.rsvHold.valid) { rsv(io.rsvHold.bits) := true.B; rsvDom(io.rsvHold.bits) := dom(io.rsvHold.bits) }
  when (io.rsvSet.valid)  { rsv(io.rsvSet.bits) := true.B; rsvDom(io.rsvSet.bits) := io.rsvSetDom }
  when (io.clearDom.valid) { for (i <- 0 until nLines) when (dom(i) === io.clearDom.bits || inv(i)) { v(i) := false.B } }
  when (io.clearAll) { v.foreach(_ := false.B); rsv.foreach(_ := false.B) }   // last: wins over a same-cycle write

  // ---- VSU candidate read (one 1:nLines mux per field) ----
  io.cV := v(io.cIdx); io.cDom := dom(io.cIdx); io.cFreq := freq(io.cIdx); io.cRsv := rsv(io.cIdx)
  if (withTaint) { io.cTS.get := tS.get(io.cIdx); io.cTA.get := tA.get(io.cIdx) }   // [T-IFT v4, B1] two more bit-slices of the same mux: 0 added levels

  // ---- free pool: invalid and not reserved (1:1 register = line; BOOM free-list style priority select) ----
  val free = VecInit((0 until nLines).map(i => !v(i) && !rsv(i))).asUInt
  io.poolEmpty := !free.orR
  io.freeIdx   := PriorityEncoder(free)

  // ---- occupancy (paper 5.2/6: per-domain counters): registered popcount, 2 stages ----
  // occ(d) = reserved lines granted to d (valid or not) + valid, unreserved lines of d (owner 2026-10-31, F1 and F5: a
  // granted line counts for its requester from the grant, a rebalancing victim leaves its owner's count at the grant, so
  // a count rises only at a grant). Shows the line state of 2 cycles ago; the VSU makes no decision in the 2 cycles after
  // a grant (N2 grantGap), so every earlier grant is counted when it decides.
  val vOnly = v.asUInt & ~rsv.asUInt
  val vd1 = (rsv.asUInt &  rsvDom.asUInt) | (vOnly &  dom.asUInt)
  val vd0 = (rsv.asUInt & ~rsvDom.asUInt) | (vOnly & ~dom.asUInt)
  val half = nLines / 2
  val p1a = RegNext(PopCount(vd1(half-1, 0))); val p1b = RegNext(PopCount(vd1(nLines-1, half)))
  val p0a = RegNext(PopCount(vd0(half-1, 0))); val p0b = RegNext(PopCount(vd0(nLines-1, half)))
  io.occ(1) := RegNext(p1a +& p1b); io.occ(0) := RegNext(p0a +& p0b)
}


// Both CRFs in one module: the new one gets the ports as they are, the legacy one the same inputs (the FREQ vectors decoded from
// {incEn, incLine} exactly as N1 does).  mm has one bit per output group that differs; 0 = equal.
class CevCrfPair(val nLines: Int, val keyBits: Int, val nSearch: Int, val isI: Boolean, val withSec: Boolean, val expire: Int,
                 val withTaint: Boolean, val withIftDom: Boolean) extends Module {
  val n = Module(new CevCRF(nLines, keyBits, nSearch, isI, withSec, expire, withTaint, withIftDom, dbgChecks = true))
  val o = Module(new CevCRFLegacy(nLines, keyBits, nSearch, isI, withSec, expire, withTaint, withIftDom))
  val in = IO(chiselTypeOf(n.io))
  val mm = IO(Output(UInt(12.W)))
  n.io <> in
  for (p <- 0 until nSearch) { o.io.sKey(p) := in.sKey(p); o.io.sDom(p) := in.sDom(p); o.io.sShare(p) := in.sShare(p) }
  def dec(en: Seq[Bool]): UInt = (0 until nSearch).map(w => Mux(en(w), UIntToOH(in.incLine(w), nLines), 0.U(nLines.W))).reduce(_ | _)
  o.io.incVec := dec(in.incEn)
  in.incSec.foreach(s => o.io.incSecVec.get := dec(s))
  in.incAtk.foreach(s => o.io.incAtkVec.get := dec(s))
  o.io.wr <> in.wr
  o.io.rsvSet <> in.rsvSet; o.io.rsvSetDom := in.rsvSetDom; o.io.rsvHold <> in.rsvHold; o.io.rsvRel <> in.rsvRel
  o.io.clearAll := in.clearAll; o.io.clearDom <> in.clearDom; o.io.cIdx := in.cIdx
  def ne(f: CevCRF => Seq[Data], g: CevCRFLegacy => Seq[Data]): Bool =
    f(n).zip(g(o)).map { case (a, b) => a.asUInt =/= b.asUInt }.foldLeft(false.B)(_ || _)
  val b = Wire(Vec(12, Bool()))
  b(0)  := ne(c => c.io.sHit, c => c.io.sHit)
  b(1)  := ne(c => c.io.sLine, c => c.io.sLine)
  b(2)  := ne(c => c.io.sCoh, c => c.io.sCoh)
  b(3)  := ne(c => c.io.sDomOut, c => c.io.sDomOut)
  b(4)  := ne(c => c.io.sSec, c => c.io.sSec)
  b(5)  := (if (withIftDom) ne(c => c.io.sIftDom.get, c => c.io.sIftDom.get) else false.B)
  b(6)  := ne(c => c.io.sRsv, c => c.io.sRsv)
  b(7)  := ne(c => Seq(c.io.cV, c.io.cDom, c.io.cFreq, c.io.cRsv), c => Seq(c.io.cV, c.io.cDom, c.io.cFreq, c.io.cRsv))
  b(8)  := (if (withTaint) ne(c => Seq(c.io.cTS.get, c.io.cTA.get), c => Seq(c.io.cTS.get, c.io.cTA.get)) else false.B)
  b(9)  := ne(c => Seq(c.io.poolEmpty), c => Seq(c.io.poolEmpty)) || (!o.io.poolEmpty && n.io.freeIdx =/= o.io.freeIdx)
  b(10) := ne(c => c.io.occ, c => c.io.occ)
  b(11) := false.B
  mm := b.asUInt
}

object CevB1Bench {
  private var checks = 0L
  private var fails  = 0L
  private def check(c: Boolean, msg: => String): Unit = { checks += 1; if (!c) { fails += 1; if (fails <= 100) println(s"[CEVB1] FAIL $msg") } }
  private def info(msg: String): Unit = println(s"[CEVB1] INFO $msg")
  private val MMNAMES = Seq("sHit", "sLine", "sCoh", "sDomOut", "sSec", "sIftDom", "sRsv", "cand(v,dom,freq,rsv)", "cand(tS,tA)", "pool/freeIdx", "occ", "-")

  final case class W(line: Int, key: BigInt, v: Boolean, coh: Int, alloc: Boolean, dom: Boolean, s: Boolean, inv: Boolean, sec: Boolean,
                     ts: Boolean, ta: Boolean, idom: Boolean)
  final case class In(sKey: Seq[BigInt], sDom: Seq[Boolean], sShare: Seq[Boolean], wr: Option[W] = None, rsvSet: Option[Int] = None,
                      rsvSetDom: Boolean = false, rsvHold: Option[Int] = None, rsvRel: Option[Int] = None, clearAll: Boolean = false,
                      clearDom: Option[Boolean] = None, cIdx: Int = 0, incEn: Seq[Boolean] = Nil, incLine: Seq[Int] = Nil,
                      incSec: Seq[Boolean] = Nil, incAtk: Seq[Boolean] = Nil)

  // Mirror of the RTL write queue (states 0 FREE, 1 WAIT, 2 ERASED, 3 DONE): occupancy for the generator, entries for coverage
  final class QModel(val depth: Int) {
    val st = Array.fill(depth)(0); val line = Array.fill(depth)(-1); val key = Array.fill(depth)(BigInt(-1)); val old = Array.fill(depth)(BigInt(-1))   // old = the key the line held (-1: invalid)
    var eh = 0; var qt = 0
    def canEnq: Boolean = st(qt) == 0 || st(qt) == 3
    def alive: Int = st.count(_ != 0)
    def pendingLine(l: Int): Boolean = (0 until depth).exists(e => st(e) != 0 && line(e) == l)
    def pendingKey(k: BigInt): Boolean = (0 until depth).exists(e => st(e) != 0 && key(e) == k)
    def pendingOld(k: BigInt): Boolean = (0 until depth).exists(e => st(e) != 0 && old(e) == k)
    def step(enq: Option[(Int, BigInt, BigInt)]): Unit = {
      val h = st(eh)
      for (e <- 0 until depth) if (st(e) == 3) st(e) = 0
      if (h == 1) st(eh) = 2 else if (h == 2) { st(eh) = 3; eh = (eh + 1) % depth }
      enq.foreach { case (l, k, o) => st(qt) = 1; line(qt) = l; key(qt) = k; old(qt) = o; qt = (qt + 1) % depth }
    }
  }

  // Flop-level image of the CRF state the protocols depend on
  final class Img(val n: Int, val isI: Boolean) {
    val key = Array.fill(n)(BigInt(-1)); val v = Array.fill(n)(false); val dom = Array.fill(n)(false); val known = Array.fill(n)(false)
    def okKey(l: Int, k: BigInt, d: Boolean): Boolean = !(0 until n).exists(j => j != l && v(j) && key(j) == k && (!isI || dom(j) == d))
    def apply(in: In): Unit = {
      in.wr.foreach { w => key(w.line) = w.key; v(w.line) = w.v; if (w.alloc) { dom(w.line) = w.dom; known(w.line) = true } }
      in.clearDom.foreach { d => for (i <- 0 until n) if (dom(i) == d) v(i) = false }   // inv bit never set by this bench
      if (in.clearAll) for (i <- 0 until n) v(i) = false
    }
  }

  final class Env(val dut: CevCrfPair, val seed: Long, val tag: String, val keys: IndexedSeq[BigInt]) {
    val r = new Random(seed)
    val n = dut.nLines; val isI = dut.isI; val np = dut.nSearch
    val q = new QModel(dut.n.qDepth); val img = new Img(n, isI)
    val hist = scala.collection.mutable.Queue[String]()
    var cyc = 0; var nextKeys: Seq[BigInt] = Seq.fill(np)(BigInt(0))
    // coverage
    var covBypassKey = 0L; var covPendKey = 0L; var covMaxAlive = 0; var covEnq = 0L; var covHit = 0L; var covSup = 0L
    val mmBits = Array.fill(12)(0L)
    // I$ refill state
    var rfLine = -1; var rfKey = BigInt(0); var rfDom = false; var rfWait = 0

    def pickKey(): BigInt = {
      val vl = (0 until n).filter(img.v(_))
      val x = r.nextDouble()
      if (x < 0.4 && vl.nonEmpty) img.key(vl(r.nextInt(vl.size)))
      else if (x < 0.5 && q.alive > 0) { val e = (0 until q.depth).filter(q.st(_) != 0); q.key(e(r.nextInt(e.size))) }
      else keys(r.nextInt(keys.size))
    }
    // the cycle: poke, compare, clock, update the images.  nextKeys = the s0 key of the NEXT cycle (sKey of this one is `cur`)
    def cycle(in: In, cur: Seq[BigInt]): Unit = {
      val io = dut.in
      for (p <- 0 until np) {
        io.sKey(p).poke(cur(p)); io.s0Key(p).poke(nextKeys(p))
        io.sDom(p).poke(in.sDom(p)); io.sShare(p).poke(in.sShare(p))
        io.incEn(p).poke(in.incEn.lift(p).getOrElse(false)); io.incLine(p).poke(BigInt(in.incLine.lift(p).getOrElse(0)))
        io.incSec.foreach(_(p).poke(in.incSec.lift(p).getOrElse(false))); io.incAtk.foreach(_(p).poke(in.incAtk.lift(p).getOrElse(false)))
      }
      val w = in.wr.getOrElse(W(0, BigInt(0), false, 0, false, false, false, false, false, false, false, false))
      io.wr.valid.poke(in.wr.isDefined)
      io.wr.bits.line.poke(BigInt(w.line)); io.wr.bits.key.poke(w.key); io.wr.bits.v.poke(w.v); io.wr.bits.coh.poke(BigInt(w.coh))
      io.wr.bits.alloc.poke(w.alloc); io.wr.bits.dom.poke(w.dom); io.wr.bits.s.poke(w.s); io.wr.bits.inv.poke(w.inv); io.wr.bits.sec.poke(w.sec)
      io.wr.bits.ts.foreach(_.poke(w.ts)); io.wr.bits.ta.foreach(_.poke(w.ta)); io.wr.bits.idom.foreach(_.poke(w.idom))
      io.rsvSet.valid.poke(in.rsvSet.isDefined); io.rsvSet.bits.poke(BigInt(in.rsvSet.getOrElse(0))); io.rsvSetDom.poke(in.rsvSetDom)
      io.rsvHold.valid.poke(in.rsvHold.isDefined); io.rsvHold.bits.poke(BigInt(in.rsvHold.getOrElse(0)))
      io.rsvRel.valid.poke(in.rsvRel.isDefined); io.rsvRel.bits.poke(BigInt(in.rsvRel.getOrElse(0)))
      io.clearAll.poke(in.clearAll)
      io.clearDom.valid.poke(in.clearDom.isDefined); io.clearDom.bits.poke(in.clearDom.getOrElse(false))
      io.cIdx.poke(BigInt(in.cIdx))
      val m = dut.mm.peekValue().asBigInt
      hist.enqueue(s"c$cyc cur=$cur next=$nextKeys wr=${in.wr} rsvSet=${in.rsvSet} hold=${in.rsvHold} rel=${in.rsvRel} clrAll=${in.clearAll} q=${q.st.mkString("")}/${q.line.mkString(",")}/${q.eh}/${q.qt}")
      if (hist.size > 14) hist.dequeue()
      if (m != 0) {
        if (fails < 2) hist.foreach(h => println("[CEVB1]   " + h))
        for (b <- 0 until 12) if (m.testBit(b)) mmBits(b) += 1
        check(false, s"$tag c$cyc mismatch bits=${(0 until 12).filter(m.testBit).map(MMNAMES(_))} sKey=$cur wr=${in.wr}")
      } else checks += 1
      // coverage: a search for a key that only the queue knows (bypass), or for the old key of a pending line (pend)
      if (!isI) for (p <- 0 until np) {
        if (q.pendingKey(cur(p))) covBypassKey += 1
        if (q.pendingOld(cur(p)) && !q.pendingKey(cur(p))) covPendKey += 1
      }
      covMaxAlive = covMaxAlive max q.alive
      dut.clock.step()
      val enq = in.wr.filter(x => if (isI) !x.alloc else x.alloc).map(x => (x.line, x.key, if (img.v(x.line)) img.key(x.line) else BigInt(-1)))
      if (enq.isDefined) covEnq += 1
      q.step(enq); img.apply(in); cyc += 1
    }

    def mkIn(wr: Option[W], extra: In => In = identity, cur: Seq[BigInt]): In = {
      val rsvSet  = if (r.nextDouble() < 0.2) Some(r.nextInt(n)) else None
      val known   = (0 until n).filter(img.known(_))
      val rsvHold = if (!isI && known.nonEmpty && r.nextDouble() < 0.1) Some(known(r.nextInt(known.size))) else None
      val rsvRel  = if (!isI && r.nextDouble() < 0.1) Some(r.nextInt(n)) else None
      val clearAll = r.nextDouble() < 0.004
      val clearDom = if (isI && r.nextDouble() < 0.01) Some(r.nextBoolean()) else None
      // increments: any (enable, line) pair; sec / atk imply the enable
      val incEn  = Seq.fill(np)(r.nextDouble() < 0.4)
      val incLine = Seq.fill(np)(r.nextInt(n))
      val incSec = incEn.map(e => e && r.nextBoolean()); val incAtk = incEn.map(e => e && r.nextBoolean())
      extra(In(cur, Seq.fill(np)(r.nextBoolean()), Seq.fill(np)(r.nextDouble() < 0.6), wr, rsvSet, r.nextBoolean(), rsvHold, rsvRel,
               clearAll, clearDom, r.nextInt(n), incEn, incLine, incSec, incAtk))
    }

    // ---- one random D$ cycle: pAlloc / pNon = probabilities of an allocating / non-allocating write this cycle
    def stepD(pAlloc: Double, pNon: Double): Unit = {
      val cur = nextKeys; nextKeys = Seq.fill(np)(pickKey())
      val x = r.nextDouble()
      val wr: Option[W] =
        if (x < pAlloc && q.canEnq) {
          val l = r.nextInt(n); val d = r.nextBoolean()
          var k = keys(r.nextInt(keys.size)); var t = 0
          while (!img.okKey(l, k, d) && t < 30) { k = keys(r.nextInt(keys.size)); t += 1 }
          val vv = img.okKey(l, k, d) && r.nextDouble() < 0.85
          Some(W(l, k, vv, r.nextInt(4), true, d, false, false, false, r.nextBoolean(), r.nextBoolean(), false))
        } else if (x < pAlloc + pNon) {
          val known = (0 until n).filter(img.known(_))
          if (known.isEmpty) None else {
            val l = known(r.nextInt(known.size)); val k = img.key(l)
            val vv = img.okKey(l, k, false) && r.nextBoolean()
            Some(W(l, k, vv, r.nextInt(4), false, img.dom(l), false, false, false, false, false, false))
          }
        } else None
      if (wr.exists(w => w.alloc && q.pendingLine(w.line))) covSup += 1
      cycle(mkIn(wr, cur = cur), cur)
    }

    // ---- one random I$ cycle: at most one refill in flight (first beat, then done after >= 3 cycles)
    def stepI(pStart: Double): Unit = {
      val cur = nextKeys; nextKeys = Seq.fill(np)(pickKey())
      var wr: Option[W] = None
      if (rfLine >= 0) {
        rfWait -= 1
        if (rfWait <= 0) {
          val vv = img.okKey(rfLine, rfKey, rfDom) && r.nextDouble() < 0.9
          wr = Some(W(rfLine, rfKey, vv, 0, true, rfDom, r.nextBoolean(), r.nextDouble() < 0.2, r.nextBoolean(), false, false, r.nextBoolean()))
          rfLine = -1
        }
      } else if (r.nextDouble() < pStart && q.alive == 0) {
        rfLine = r.nextInt(n); rfDom = r.nextBoolean(); rfKey = keys(r.nextInt(keys.size)); rfWait = 3 + r.nextInt(4)
        wr = Some(W(rfLine, rfKey, false, 0, false, rfDom, false, false, false, false, false, false))   // first beat: v = 0, rsv kept, key
      }
      cycle(mkIn(wr, cur = cur), cur)
    }
  }

  // run `cycles` cycles in phases of different write pressure, after an initialisation pass that writes every line once
  def runD(e: Env, cycles: Int): Unit = {
    // init pass: allocate every line (queue-gated, one per cycle when possible) so that all per-line state is defined in both CRFs
    var l = 0
    while (l < e.n) {
      val cur = e.nextKeys; e.nextKeys = Seq.fill(e.np)(e.pickKey())
      if (e.q.canEnq) {
        val k = BigInt(l * 7 + 3) % (BigInt(1) << e.dut.keyBits)
        val kk = if (e.img.okKey(l, k, false)) k else e.keys(0)
        val w = W(l, kk, e.img.okKey(l, kk, false), 0, true, e.r.nextBoolean(), false, false, false, e.r.nextBoolean(), e.r.nextBoolean(), false)
        e.cycle(e.mkIn(Some(w), cur = cur), cur); l += 1
      } else e.cycle(e.mkIn(None, cur = cur), cur)
    }
    var c = 0
    while (c < cycles) {
      val len = 100 + e.r.nextInt(400)
      val (pa, pn) = e.r.nextInt(5) match {
        case 0 => (0.95, 0.02)   // burst: allocation as fast as the queue takes it
        case 1 => (0.5, 0.3)
        case 2 => (0.15, 0.3)
        case 3 => (0.05, 0.05)
        case _ => (0.3, 0.1)
      }
      for (_ <- 0 until len) e.stepD(pa, pn)
      c += len
    }
    for (_ <- 0 until 20) e.stepD(0.0, 0.0)
  }
  def runI(e: Env, cycles: Int): Unit = {
    var l = 0   // init pass: one refill per line
    while (l < e.n) {
      val cur = e.nextKeys; e.nextKeys = Seq.fill(e.np)(e.pickKey())
      if (e.rfLine < 0) {
        e.rfLine = l; e.rfDom = e.r.nextBoolean(); e.rfKey = BigInt(l * 7 + 3) % (BigInt(1) << e.dut.keyBits); e.rfWait = 3; l += 1
        e.cycle(e.mkIn(Some(W(e.rfLine, e.rfKey, false, 0, false, e.rfDom, false, false, false, false, false, false)), cur = cur), cur)
      } else {
        e.rfWait -= 1
        if (e.rfWait <= 0) {
          val w = W(e.rfLine, e.rfKey, e.img.okKey(e.rfLine, e.rfKey, e.rfDom), 0, true, e.rfDom, e.r.nextBoolean(), e.r.nextDouble() < 0.2, e.r.nextBoolean(), false, false, e.r.nextBoolean())
          e.rfLine = -1; e.cycle(e.mkIn(Some(w), cur = cur), cur)
        } else e.cycle(e.mkIn(None, cur = cur), cur)
      }
    }
    while (e.rfLine >= 0) e.stepI(0.0)
    var c = 0
    while (c < cycles) {
      val len = 100 + e.r.nextInt(400)
      val ps = Seq(0.9, 0.3, 0.05, 0.5)(e.r.nextInt(4))
      for (_ <- 0 until len) e.stepI(ps)
      c += len
    }
    while (e.rfLine >= 0) e.stepI(0.0)
  }

  def report(e: Env): Unit =
    info(f"${e.tag}: ${e.cyc} cycles, enq=${e.covEnq} maxAlive=${e.covMaxAlive}/${e.q.depth} bypassKeyCycles=${e.covBypassKey} " +
         f"pendKeyCycles=${e.covPendKey} supersedeAllocs=${e.covSup} mismatchBits=${e.mmBits.mkString(",")}")

  // a section that must trip a hardware assertion: the simulation aborts with an exception
  def expectAbort(name: String)(body: => Unit): Unit = {
    val ok = try { body; false } catch { case _: Throwable => true }
    check(ok, s"$name: the assertion did not fire")
    info(s"$name: assertion fired = $ok")
  }

  def main(args: Array[String]): Unit = {
    val seed   = args.collectFirst { case a if a.startsWith("seed=") => a.drop(5).toLong }.getOrElse(1L)
    val cycles = args.collectFirst { case a if a.startsWith("cycles=") => a.drop(7).toInt }.getOrElse(20000)
    val only   = args.filterNot(a => a.startsWith("seed=") || a.startsWith("cycles=")).toSet
    def section(name: String)(body: => Unit): Unit = if ((only.isEmpty && name != "d-1024x26") || only(name)) {
      println(s"[CEVB1] BEGIN $name"); val f0 = fails
      try body catch { case t: Throwable => fails += 1; println(s"[CEVB1] FAIL $name: exception $t") }
      println(s"[CEVB1] END $name fails=${fails - f0}")
    }
    info(s"seed=$seed cycles=$cycles")
    def keysFor(bits: Int, cnt: Int, r: Random): IndexedSeq[BigInt] = Vector.fill(cnt)(BigInt(bits, r))
    // D$: (name, lines, keyBits, taint)
    for ((nm, nl, kb, tt, cy) <- Seq(("d-16x8", 16, 8, true, cycles), ("d-64x12", 64, 12, false, cycles), ("d-128x20", 128, 20, true, cycles),
                                     ("d-256x26", 256, 26, true, cycles / 2), ("d-512x26", 512, 26, true, cycles / 4),
                                     ("d-1024x26", 1024, 26, true, cycles / 4))) {   // d-1024x26: Verilator needs > 1 h for the pair (the legacy CRF alone 32 min): pass it by name to run
      section(nm) {
        simulate(new CevCrfPair(nl, kb, 2, false, false, 128 min (nl * 2), tt, false)) { dut =>
          dut.reset.poke(true); dut.clock.step(4); dut.reset.poke(false)
          val r0 = new Random(seed * 31 + nl)
          val e = new Env(dut, seed * 131 + nl, nm, keysFor(kb, if (nl <= 16) 24 else 3 * nl, r0))
          runD(e, cy); report(e)
          check(e.covEnq > 100 && e.covBypassKey > 10 && e.covPendKey > 10, s"$nm: coverage enq=${e.covEnq} byp=${e.covBypassKey} pend=${e.covPendKey}")
          check(e.covMaxAlive >= 6, s"$nm: queue never got deep (max ${e.covMaxAlive})")
        }
      }
    }
    for ((nm, nl, kb, cy) <- Seq(("i-16x8", 16, 8, cycles), ("i-128x20", 128, 20, cycles), ("i-512x26", 512, 26, cycles / 2))) {
      section(nm) {
        simulate(new CevCrfPair(nl, kb, 1, true, true, 64 min (nl * 2), false, true)) { dut =>
          dut.reset.poke(true); dut.clock.step(4); dut.reset.poke(false)
          val r0 = new Random(seed * 37 + nl)
          val e = new Env(dut, seed * 137 + nl, nm, keysFor(kb, if (nl <= 16) 10 else nl, r0))
          runI(e, cy); report(e)
          check(e.covEnq > 50, s"$nm: coverage enq=${e.covEnq}")
        }
      }
    }
    // queue overflow: more allocations than the engine drains must trip the overflow assertion (nothing back-pressures)
    section("overflow-assert") {
      expectAbort("overflow-assert") {
        simulate(new CevCrfPair(16, 8, 2, false, false, 8, false, false)) { dut =>
          dut.reset.poke(true); dut.clock.step(4); dut.reset.poke(false)
          val e = new Env(dut, 5, "overflow", Vector(BigInt(1)))
          for (i <- 0 until 40) {
            val cur = e.nextKeys
            val w = W(i % 16, BigInt(i % 200), false, 0, true, false, false, false, false, false, false, false)
            e.cycle(e.mkIn(Some(w), cur = cur), cur)   // no q.canEnq gating: the generator ignores the bound on purpose
          }
        }
      }
    }
    // one-hot assertion: two valid lines with the same key, then a search of it
    section("onehot-assert") {
      expectAbort("onehot-assert") {
        simulate(new CevCrfPair(16, 8, 2, false, false, 8, false, false)) { dut =>
          dut.reset.poke(true); dut.clock.step(4); dut.reset.poke(false)
          val e = new Env(dut, 6, "onehot", Vector(BigInt(1)))
          def al(l: Int, k: Int): Unit = { val cur = e.nextKeys; e.cycle(e.mkIn(Some(W(l, BigInt(k), true, 0, true, false, false, false, false, false, false, false)), cur = cur), cur) }
          for (l <- 0 until 16) al(l, 100 + l)
          for (_ <- 0 until 6) { val cur = e.nextKeys; e.cycle(e.mkIn(None, cur = cur), cur) }
          al(3, 77); al(9, 77)                             // duplicate valid key: forbidden by G3
          for (_ <- 0 until 8) { e.nextKeys = Seq(BigInt(77), BigInt(77)); val cur = e.nextKeys; e.cycle(e.mkIn(None, cur = cur), cur) }
        }
      }
    }
    println(s"[CEVB1] RESULT checks=$checks fails=$fails")
    if (fails != 0) throw new RuntimeException(s"CevB1Bench: $fails failed checks")
  }
}
