// Unit bench for CevCRF / CevVSU (ceviche-plan-v5.md N1, N2 as amended by ceviche-plan-v5-amend-T1c.md; plan 11 row T1c:
// search, FREQ, pool, Fig. 4 decisions, rebalancing timer; owner rulings 2026-10-31: F1 occupancy never exceeds the hard
// limit; F4 a grant made by a search reaches only the requester (id and domain) the search was started for, with the
// amended M3 round-robin pick, no starvation; F5 a reserved line counts for its reserving domain whether valid or not).
// Harness: chisel3.simulator.EphemeralSimulator (svsim + Verilator, shipped in the chisel 6.7 jar), so
// no build.sbt change is needed. This directory is not an sbt source directory of the `boom` project; the run command in
// claude-artifacts/ift-overhaul/execution/defense-clean-plans/diffs/T1c/APPLY.md adds it for one sbt session only.
// Every check compares the RTL against a cycle-exact Scala reference of the plan's N1/N2 text. Exit: a RuntimeException
// (non-zero sbt exit) when any check fails; the last line is "[CEVBENCH] RESULT checks=<n> fails=<m>".
// [T1c v4, R1c B1] adds the upgrade-hold port pair (rsvHold/rsvRel) and the VSU busy output: directed `upg-hold` block in crf-d-small,
// random hold/release in the CRF random mix, busy check in every VSU cycle, section pair-upg (held line never granted).
// Optional arguments: names of the sections to run (default all), "seed=<n>" (default 1).
package boom.v3.common

import chisel3._
import chisel3.util._
import chisel3.simulator.EphemeralSimulator._
import scala.util.Random

object CevBench {
  // ------------------------------------------------------------------ bookkeeping
  private var checks = 0L
  private var fails  = 0L
  private def check(c: Boolean, msg: => String): Unit = {
    checks += 1
    if (!c) { fails += 1; if (fails <= 200) println(s"[CEVBENCH] FAIL $msg") }
  }
  private def info(msg: String): Unit = println(s"[CEVBENCH] INFO $msg")
  private def pk(d: Data): BigInt  = d.peekValue().asBigInt
  private def pb(d: Data): Boolean = pk(d) != 0
  private def pi(d: Data): Int     = pk(d).toInt
  private def resetDut(m: Module): Unit = { m.reset.poke(true); m.clock.step(4); m.reset.poke(false) }

  // ================================================================== CevCRF reference model (plan N1)
  final case class Wr(line: Int, key: BigInt, v: Boolean, coh: Int, alloc: Boolean, dom: Boolean,
                      s: Boolean, inv: Boolean, sec: Boolean)
  final case class CrfIn(inc: BigInt = 0, wr: Option[Wr] = None, rsvSet: Option[Int] = None, clearAll: Boolean = false,
                         clearDom: Option[Boolean] = None, sKey: Seq[BigInt] = Nil, sDom: Seq[Boolean] = Nil,
                         sShare: Seq[Boolean] = Nil, cIdx: Int = 0, rsvSetDom: Boolean = false,
                         rsvHold: Option[Int] = None, rsvRel: Option[Int] = None)   // [R1c B1] upgrade hold / release

  final class CrfModel(val n: Int, val isI: Boolean, val withSec: Boolean, val expire: Int) {
    val key  = Array.fill(n)(BigInt(0)); val dom = Array.fill(n)(false); val v   = Array.fill(n)(false)
    val coh  = Array.fill(n)(0);         val freq = Array.fill(n)(0);    val rsv = Array.fill(n)(false)
    val s    = Array.fill(n)(false);     val inv = Array.fill(n)(false); val sec = Array.fill(n)(false)
    val rsvDom = Array.fill(n)(false)
    val known = Array.fill(n)(false)     // dom/freq/s/inv/sec defined (set by an allocating write; no reset in the RTL)
    var tickCnt = 0
    // popcount (occ0, occ1) of the state now, one and two cycles ago: the RTL shows the one of two cycles ago
    var pc0 = (0, 0); var pc1 = (0, 0); var pc2 = (0, 0)
    // owner F1 + F5 rulings: a reserved line counts for its reserving domain (valid or not), an unreserved valid line for
    // its own domain
    def counts(i: Int, d: Boolean): Boolean = if (rsv(i)) rsvDom(i) == d else v(i) && dom(i) == d
    def popc: (Int, Int) = ((0 until n).count(i => counts(i, false)), (0 until n).count(i => counts(i, true)))
    def occ: (Int, Int)  = pc2
    def free(i: Int): Boolean = !v(i) && !rsv(i)
    def poolEmpty: Boolean = !(0 until n).exists(free)
    def freeIdx: Int = (0 until n).find(free).getOrElse(n - 1)
    def search(k: BigInt, d: Boolean, sh: Boolean): Seq[Int] = {
      val eq = (0 until n).filter(i => v(i) && key(i) == k)
      if (!isI) eq                                            // D$: domain-masked
      else {
        val own = eq.filter(i => dom(i) == d)
        val shr = if (sh) eq.filter(i => dom(i) != d && s(i)) else Seq()
        if (own.nonEmpty) own else shr                         // own copy first
      }
    }
    def step(in: CrfIn): Unit = {
      val tick = tickCnt == expire - 1
      val clr  = in.clearDom.map(d => (0 until n).map(i => dom(i) == d || inv(i))).getOrElse(Seq.fill(n)(false))
      for (i <- 0 until n) {
        val up = if (in.inc.testBit(i) && freq(i) != 15) freq(i) + 1 else freq(i)
        freq(i) = if (tick && up != 0) up - 1 else up
      }
      in.wr.foreach { w =>
        key(w.line) = w.key; v(w.line) = w.v; coh(w.line) = w.coh
        if (w.alloc) {
          dom(w.line) = w.dom; s(w.line) = w.s; inv(w.line) = w.inv; sec(w.line) = w.sec; rsv(w.line) = false
          freq(w.line) = 5; known(w.line) = true
        }
      }
      in.rsvRel.foreach { r => rsv(r) = false }                              // [R1c B1] N1 statement order: rel, hold, set
      in.rsvHold.foreach { r => rsv(r) = true; rsvDom(r) = dom(r) }
      in.rsvSet.foreach { r => rsv(r) = true; rsvDom(r) = in.rsvSetDom }
      for (i <- 0 until n) if (clr(i)) v(i) = false
      if (in.clearAll) for (i <- 0 until n) { v(i) = false; rsv(i) = false }
      tickCnt = (tickCnt + 1) % expire
      pc2 = pc1; pc1 = pc0; pc0 = popc
    }
  }

  // One clock cycle of a CevCRF: drive `in`, compare every output of the current state with the model, clock, update.
  // fullScan: compare v/rsv/dom/FREQ of every line through the candidate read port (small instances only).
  private def crfCycle(dut: CevCRF, m: CrfModel, in: CrfIn, tag: String, cyc: Int, fullScan: Boolean): Unit = {
    val io = dut.io
    for (p <- 0 until dut.nSearch) {
      io.sKey(p).poke(in.sKey.lift(p).getOrElse(BigInt(0)))
      io.sDom(p).poke(in.sDom.lift(p).getOrElse(false))
      io.sShare(p).poke(in.sShare.lift(p).getOrElse(false))
    }
    io.incVec.poke(in.inc)
    val w = in.wr.getOrElse(Wr(0, BigInt(0), false, 0, false, false, false, false, false))
    io.wr.valid.poke(in.wr.isDefined)
    io.wr.bits.line.poke(BigInt(w.line)); io.wr.bits.key.poke(w.key); io.wr.bits.v.poke(w.v)
    io.wr.bits.coh.poke(BigInt(w.coh)); io.wr.bits.alloc.poke(w.alloc); io.wr.bits.dom.poke(w.dom)
    io.wr.bits.s.poke(w.s); io.wr.bits.inv.poke(w.inv); io.wr.bits.sec.poke(w.sec)
    io.rsvSet.valid.poke(in.rsvSet.isDefined); io.rsvSet.bits.poke(BigInt(in.rsvSet.getOrElse(0)))
    io.rsvSetDom.poke(in.rsvSetDom)
    io.rsvHold.valid.poke(in.rsvHold.isDefined); io.rsvHold.bits.poke(BigInt(in.rsvHold.getOrElse(0)))   // [R1c B1]
    io.rsvRel.valid.poke(in.rsvRel.isDefined);   io.rsvRel.bits.poke(BigInt(in.rsvRel.getOrElse(0)))
    io.clearAll.poke(in.clearAll)
    io.clearDom.valid.poke(in.clearDom.isDefined); io.clearDom.bits.poke(in.clearDom.getOrElse(false))
    val at = s"$tag c$cyc"
    // ---- search lanes ----
    for (p <- 0 until dut.nSearch) {
      val k  = in.sKey.lift(p).getOrElse(BigInt(0))
      val mm = m.search(k, in.sDom.lift(p).getOrElse(false), in.sShare.lift(p).getOrElse(false))
      check(mm.size <= 1, s"$at lane$p: bench invariant broken (model has ${mm.size} matches)")
      val hit = mm.nonEmpty; val l = mm.headOption.getOrElse(0)
      val gHit = pb(io.sHit(p)); val gLine = pi(io.sLine(p)); val gM = pk(io.sMatch(p))
      check(gHit == hit, s"$at lane$p key=$k sHit=$gHit exp=$hit")
      check(gM == mm.map(i => BigInt(1) << i).sum, s"$at lane$p sMatch=0x${gM.toString(16)} exp lines=$mm")
      check(gLine == (if (hit) l else 0), s"$at lane$p sLine=$gLine exp=${if (hit) l else 0}")
      check(pi(io.sCoh(p)) == (if (hit && !m.isI) m.coh(l) else 0), s"$at lane$p sCoh=${pi(io.sCoh(p))}")
      check(pb(io.sDomOut(p)) == (hit && m.dom(l)), s"$at lane$p sDomOut")
      check(pb(io.sSec(p)) == (hit && m.withSec && m.sec(l)), s"$at lane$p sSec")
      check(pb(io.sRsv(p)) == (hit && m.rsv(l)), s"$at lane$p sRsv")
    }
    // ---- free pool and occupancy ----
    check(pb(io.poolEmpty) == m.poolEmpty, s"$at poolEmpty=${pb(io.poolEmpty)} exp=${m.poolEmpty}")
    if (!m.poolEmpty) check(pi(io.freeIdx) == m.freeIdx, s"$at freeIdx=${pi(io.freeIdx)} exp=${m.freeIdx}")
    check((pi(io.occ(0)), pi(io.occ(1))) == m.occ, s"$at occ=(${pi(io.occ(0))},${pi(io.occ(1))}) exp=${m.occ}")
    // ---- candidate read port ----
    val idxs = if (fullScan) (0 until m.n) :+ in.cIdx else Seq(in.cIdx)
    for (c <- idxs) {
      io.cIdx.poke(BigInt(c))
      check(pb(io.cV) == m.v(c), s"$at line$c cV=${pb(io.cV)} exp=${m.v(c)}")
      check(pb(io.cRsv) == m.rsv(c), s"$at line$c cRsv=${pb(io.cRsv)} exp=${m.rsv(c)}")
      if (m.known(c)) {
        check(pb(io.cDom) == m.dom(c), s"$at line$c cDom")
        check(pi(io.cFreq) == m.freq(c), s"$at line$c cFreq=${pi(io.cFreq)} exp=${m.freq(c)}")
      }
    }
    dut.clock.step()
    m.step(in)
  }

  // Random operation mix that keeps the plan's uniqueness contract (D$: one valid line per block; I$: one per (block,
  // domain), G2/G3) and never makes a line valid before its first allocating write.
  private def randomCrfIn(m: CrfModel, r: Random, keys: IndexedSeq[BigInt], nSearch: Int): CrfIn = {
    val n = m.n
    val wr = if (r.nextDouble() < 0.5) {
      val l     = r.nextInt(n)
      val alloc = !m.known(l) || r.nextDouble() < 0.6
      val d     = if (alloc) r.nextBoolean() else m.dom(l)
      def okKey(kk: BigInt) = !(0 until n).exists(j => j != l && m.v(j) && m.key(j) == kk && (!m.isI || m.dom(j) == d))
      var vv = r.nextDouble() < 0.8
      var k  = keys(r.nextInt(keys.size))
      var tries = 0
      while (vv && !okKey(k) && tries < 20) { k = keys(r.nextInt(keys.size)); tries += 1 }
      if (!okKey(k)) vv = false
      Some(Wr(l, k, vv, r.nextInt(4), alloc, d, r.nextBoolean(), r.nextDouble() < 0.3, r.nextBoolean()))
    } else None
    val rsvSet   = if (r.nextDouble() < 0.2) Some(r.nextInt(n)) else None
    val inc      = (0 until n).foldLeft(BigInt(0))((a, i) => if (r.nextDouble() < 0.3) a.setBit(i) else a)
    val clearDom = if (r.nextDouble() < 0.02) Some(r.nextBoolean()) else None
    val clearAll = r.nextDouble() < 0.01
    // [R1c B1] hold only a known (allocated) line: rsvDom := dom(line) is defined there; release any line
    val knownL   = (0 until n).filter(i => m.known(i))
    val rsvHold  = if (knownL.nonEmpty && r.nextDouble() < 0.1) Some(knownL(r.nextInt(knownL.size))) else None
    val rsvRel   = if (r.nextDouble() < 0.1) Some(r.nextInt(n)) else None
    val valid    = (0 until n).filter(i => m.v(i))
    val sKey = Seq.fill(nSearch) {
      if (r.nextBoolean() && valid.nonEmpty) m.key(valid(r.nextInt(valid.size))) else keys(r.nextInt(keys.size))
    }
    CrfIn(inc, wr, rsvSet, clearAll, clearDom, sKey, Seq.fill(nSearch)(r.nextBoolean()),
          Seq.fill(nSearch)(r.nextDouble() < 0.6), r.nextInt(n), rsvSetDom = r.nextBoolean(),
          rsvHold = rsvHold, rsvRel = rsvRel)
  }

  private final class CrfBench(val dut: CevCRF, val m: CrfModel, val tag: String, val fullScan: Boolean) {
    var c = 0
    def cy(in: CrfIn): Unit = { crfCycle(dut, m, in, tag, c, fullScan); c += 1 }
    def idle(k: Int): Unit = for (_ <- 0 until k) cy(CrfIn())
    def wr(l: Int, k: BigInt, v: Boolean, alloc: Boolean, d: Boolean, coh: Int = 3, s: Boolean = false,
           inv: Boolean = false, sec: Boolean = false, inc: BigInt = 0, clearAll: Boolean = false,
           rsvSet: Option[Int] = None): Unit =
      cy(CrfIn(inc = inc, wr = Some(Wr(l, k, v, coh, alloc, d, s, inv, sec)), clearAll = clearAll, rsvSet = rsvSet))
    def alloc(l: Int, k: BigInt, d: Boolean, coh: Int = 3, s: Boolean = false, inv: Boolean = false,
              sec: Boolean = false): Unit = wr(l, k, true, true, d, coh, s, inv, sec)
    // (v, dom, freq, rsv) of line i through the candidate port (current state)
    def line(i: Int): (Boolean, Boolean, Int, Boolean) = {
      dut.io.cIdx.poke(BigInt(i)); (pb(dut.io.cV), pb(dut.io.cDom), pi(dut.io.cFreq), pb(dut.io.cRsv))
    }
    // (hit, line, coh, domOut, sec) of a search on lane p (current state)
    def srch(p: Int, k: BigInt, d: Boolean = false, sh: Boolean = false): (Boolean, Int, Int, Boolean, Boolean) = {
      dut.io.sKey(p).poke(k); dut.io.sDom(p).poke(d); dut.io.sShare(p).poke(sh)
      (pb(dut.io.sHit(p)), pi(dut.io.sLine(p)), pi(dut.io.sCoh(p)), pb(dut.io.sDomOut(p)), pb(dut.io.sSec(p)))
    }
    def occ: (Int, Int) = (pi(dut.io.occ(0)), pi(dut.io.occ(1)))
    def random(cycles: Int, r: Random, keys: IndexedSeq[BigInt]): Unit =
      for (_ <- 0 until cycles) cy(randomCrfIn(m, r, keys, dut.nSearch))
  }

  // ---------------------------------------------------------------- CRF directed tests, D$ kind (2 lanes)
  private def crfDirectedD(b: CrfBench): Unit = {
    val t = b.tag
    // reset
    check(!pb(b.dut.io.poolEmpty) && pi(b.dut.io.freeIdx) == 0, s"$t reset: pool not empty, freeIdx 0")
    check(b.occ == (0, 0), s"$t reset: occupancy 0")
    // allocation; search on both lanes; coherence and owner outputs; the D$ search ignores the requester domain
    b.alloc(5, 0x21, true, coh = 2); b.alloc(9, 0x33, false, coh = 3); b.alloc(15, 0x44, true, coh = 1)
    val s0 = b.srch(0, 0x33, d = true); val s1 = b.srch(1, 0x44)
    check(s0._1 && s0._2 == 9 && s0._3 == 3 && !s0._4, s"$t lane0 0x33 from domain 1 (domain-masked): $s0")
    check(s1._1 && s1._2 == 15 && s1._3 == 1 && s1._4, s"$t lane1 0x44: $s1")
    check(!b.srch(0, 0x55)._1, s"$t miss on an absent block")
    // a non-allocating write with v = 0 (clear / probe) removes the line from the search
    b.wr(9, 0x33, v = false, alloc = false, d = false)
    check(!b.srch(0, 0x33)._1, s"$t an invalid entry never matches")
    // free pool: lowest invalid and unreserved line; a reservation excludes it; alloc clears rsv; a non-alloc write does not
    check(pi(b.dut.io.freeIdx) == 0, s"$t freeIdx 0")
    b.cy(CrfIn(rsvSet = Some(0)))
    check(pi(b.dut.io.freeIdx) == 1 && b.line(0)._4, s"$t a reserved line leaves the pool")
    b.alloc(0, 0x60, false)
    check(!b.line(0)._4, s"$t the allocating write clears rsv")
    b.cy(CrfIn(rsvSet = Some(3)))
    b.wr(3, 0x61, v = false, alloc = false, d = false)
    check(b.line(3)._4 && pi(b.dut.io.freeIdx) != 3, s"$t a non-allocating write keeps rsv")
    for (l <- 0 until 16 if b.m.free(l)) b.alloc(l, 0x80 + l, l % 2 == 0)
    check(pb(b.dut.io.poolEmpty), s"$t pool empty when every line is valid or reserved")
    b.alloc(3, 0x83, true)
    b.idle(3)
    check(b.occ._1 + b.occ._2 == 16 && b.occ == b.m.popc, s"$t occupancy of a full cache: ${b.occ}")
    // occupancy is a registered popcount: it shows a change two cycles after the line state changes
    b.cy(CrfIn(clearAll = true)); b.idle(3)
    check(b.occ == (0, 0), s"$t occupancy 0 after clearAll")
    b.alloc(7, 0x90, true)
    val o1 = b.occ._2; b.idle(1); val o2 = b.occ._2; b.idle(1); val o3 = b.occ._2
    check(o1 == 0 && o2 == 0 && o3 == 1, s"$t occupancy latency (exp 0,0,1): $o1,$o2,$o3")
    // clearAll wins over a same-cycle write and reservation
    b.wr(6, 0x91, v = true, alloc = true, d = true, clearAll = true, rsvSet = Some(2))
    check(!b.line(6)._1 && !b.line(2)._4, s"$t clearAll wins over a same-cycle write and reservation")
    // FREQ: 5 at allocation, +1 per access saturating at 15, -1 per expiration interval saturating at 0
    b.alloc(4, 0x92, false)
    check(b.line(4)._3 == 5, s"$t FREQ 5 at allocation: ${b.line(4)._3}")
    var mx = 0
    for (_ <- 0 until 24) { b.cy(CrfIn(inc = BigInt(1) << 4)); mx = mx max b.line(4)._3 }
    check(mx == 15, s"$t FREQ saturates at 15 (max seen $mx)")
    var mn = 15
    for (_ <- 0 until b.m.expire * 17) { b.cy(CrfIn()); mn = mn min b.line(4)._3 }
    check(mn == 0 && b.line(4)._3 == 0, s"$t FREQ decays to 0 and stays (min $mn)")
    b.wr(4, 0x93, v = true, alloc = true, d = false, inc = BigInt(1) << 4)
    check(b.line(4)._3 == 5, s"$t allocation wins over a same-cycle increment")
    // clearDom (fence.i by domain): v := 0 where dom == d or the I bit is set
    b.cy(CrfIn(clearAll = true))
    b.alloc(1, 0xa1, false); b.alloc(2, 0xa2, true); b.alloc(3, 0xa3, true, inv = true); b.alloc(4, 0xa4, false, inv = true)
    b.cy(CrfIn(clearDom = Some(false)))
    check(!b.line(1)._1 && b.line(2)._1 && !b.line(3)._1 && !b.line(4)._1, s"$t clearDom(0) clears dom-0 and I-bit lines only")
    // owner F1 + F5 rulings: occ(d) = reserved lines granted to d (valid or not) + valid, unreserved lines of d (each
    // shown 2 cycles later); a count rises only at a grant
    b.cy(CrfIn(clearAll = true)); b.idle(3)
    b.cy(CrfIn(rsvSet = Some(2), rsvSetDom = true)); b.idle(2)
    check(b.occ == (0, 1), s"$t a free line reserved for domain 1 counts for domain 1: ${b.occ}")
    b.alloc(4, 0xb4, false); b.idle(2)
    check(b.occ == (1, 1), s"$t plus a valid domain-0 line: ${b.occ}")
    b.cy(CrfIn(rsvSet = Some(4), rsvSetDom = true)); b.idle(2)
    check(b.occ == (0, 2), s"$t F5: a reserved valid victim counts for the reserving domain from the grant: ${b.occ}")
    b.wr(4, 0xb4, v = false, alloc = false, d = false); b.idle(2)
    check(b.occ == (0, 2), s"$t the cleared victim still counts for the reserving domain: ${b.occ}")
    b.alloc(4, 0xc4, true); b.idle(2)
    check(b.occ == (0, 2) && !b.line(4)._4, s"$t re-keyed for domain 1: count unchanged, rsv cleared: ${b.occ}")
    b.alloc(9, 0xc9, true); b.idle(2)
    check(b.occ == (0, 3), s"$t plus a valid domain-1 line: ${b.occ}")
    b.cy(CrfIn(rsvSet = Some(9), rsvSetDom = true)); b.idle(2)
    check(b.occ == (0, 3), s"$t an own-line replacement keeps the count: ${b.occ}")
    b.wr(9, 0xc9, v = false, alloc = false, d = true); b.idle(2)      // L2 probe invalidates the reserved victim
    check(b.occ == (0, 3) && b.line(9)._4 && !b.line(9)._1 && !b.m.free(9),
          s"$t F5: a probed reserved line stays counted and out of the pool: ${b.occ}")
    b.alloc(9, 0xd9, true); b.idle(2)
    check(b.occ == (0, 3), s"$t re-keyed after the probe: count unchanged: ${b.occ}")
    b.wr(2, 0xc2, v = false, alloc = true, d = true); b.idle(2)
    check(b.occ == (0, 2), s"$t an allocating write with v = 0 releases the reservation: ${b.occ}")
    b.wr(9, 0xd9, v = false, alloc = false, d = true); b.idle(2)      // L2 probe of an unreserved line
    check(b.occ == (0, 1) && b.m.free(9), s"$t a probed unreserved line leaves the count and returns to the pool: ${b.occ}")
    b.cy(CrfIn(rsvSet = Some(7), rsvSetDom = false)); b.idle(2)
    check(b.occ == (1, 1), s"$t a free line reserved for domain 0: ${b.occ}")
    b.cy(CrfIn(clearAll = true)); b.idle(2)
    check(b.occ == (0, 0), s"$t clearAll drops the reservations from the count: ${b.occ}")
    // [R1c B1] upg-hold: an upgrade MSHR reserves its own valid hit line (rsvHold) until its final non-allocating write
    // releases it (rsvRel).  The hold keeps the line's domain count (rsvDom := dom); a held line is not free and a clear
    // (probe) of it keeps it out of the pool; the release returns it to the plain valid/free state.
    b.alloc(5, 0xe5, true); b.alloc(6, 0xe6, false); b.idle(3)
    check(b.occ == (1, 1), s"$t upg-hold: before ${b.occ}")
    b.cy(CrfIn(rsvHold = Some(5))); b.idle(2)
    check(b.line(5)._1 && b.line(5)._4 && b.line(5)._2 && b.occ == (1, 1), s"$t upg-hold: line 5 reserved, valid, domain 1, count unchanged: ${b.occ}")
    check(b.srch(0, 0xe5)._1 && pb(b.dut.io.sRsv(0)), s"$t upg-hold: the held line still hits and is shown reserved (D3b)")
    b.wr(5, 0xe5, v = true, alloc = false, d = true, coh = 3); b.idle(2)       // the upgrade's own non-allocating write keeps rsv
    check(b.line(5)._4 && b.occ == (1, 1), s"$t upg-hold: a non-allocating write keeps the hold: ${b.occ}")
    b.cy(CrfIn(rsvRel = Some(5))); b.idle(2)
    check(!b.line(5)._4 && b.line(5)._1 && b.occ == (1, 1), s"$t upg-hold: rsvRel clears the hold, line stays valid, count unchanged: ${b.occ}")
    b.cy(CrfIn(rsvHold = Some(6))); b.cy(CrfIn(rsvRel = Some(6), rsvHold = Some(5))); b.idle(2)   // rel and hold on different lines in one cycle
    check(!b.line(6)._4 && b.line(5)._4 && b.occ == (1, 1), s"$t upg-hold: independent hold/release in one cycle: ${b.occ}")
    b.cy(CrfIn(rsvRel = Some(5), rsvHold = Some(5))); b.idle(2)                    // same line: hold wins (statement order)
    check(b.line(5)._4, s"$t upg-hold: hold wins over a same-cycle release of the same line")
    b.cy(CrfIn(clearAll = true, rsvHold = Some(5))); b.idle(3)
    check(!b.line(5)._4 && !b.line(5)._1 && b.occ == (0, 0), s"$t upg-hold: clearAll wins over a same-cycle hold (B6 wipe end / mode switch): ${b.occ}")
  }

  // ---------------------------------------------------------------- CRF directed tests, I$ kind (1 lane, IFT sec)
  private def crfDirectedI(b: CrfBench): Unit = {
    val t = b.tag
    b.alloc(2, 0x50, false, coh = 0, s = false, sec = true)
    check(!b.srch(0, 0x50, d = true, sh = true)._1, s"$t strict: another domain's line without S misses")
    val o = b.srch(0, 0x50, d = false)
    check(o == (true, 2, 0, false, true), s"$t own line hits with its dom/sec, sCoh 0: $o")
    b.alloc(2, 0x50, false, coh = 0, s = true, sec = true)
    val sh = b.srch(0, 0x50, d = true, sh = true)
    check(sh == (true, 2, 0, false, true), s"$t shared copy hits when line S and requester S are set: $sh")
    check(!b.srch(0, 0x50, d = true, sh = false)._1, s"$t requester policy S = 0: miss")
    b.alloc(7, 0x50, true, coh = 0)
    val own1 = b.srch(0, 0x50, d = true, sh = true)
    check(own1 == (true, 7, 0, true, false), s"$t own copy wins over a shareable other-domain copy: $own1")
    check(b.srch(0, 0x50, d = false, sh = true)._2 == 2, s"$t domain 0 hits its own copy")
    // fence.i of domain 0: only domain-0 lines (and I-bit lines) are revoked
    b.cy(CrfIn(clearDom = Some(false)))
    check(!b.srch(0, 0x50, d = false, sh = true)._1, s"$t domain-0 copy revoked; domain-1 copy has S = 0: miss")
    check(b.srch(0, 0x50, d = true)._2 == 7 && b.srch(0, 0x50, d = true)._1, s"$t domain-1 copy survives domain-0 fence.i")
    b.alloc(3, 0x51, true, coh = 0, inv = true)
    b.cy(CrfIn(clearDom = Some(false)))
    check(!b.srch(0, 0x51, d = true)._1 && b.srch(0, 0x50, d = true)._1, s"$t I bit permits cross-domain revocation only")
  }

  // ---------------------------------------------------------------- CRF full fill (full-size instances)
  private def crfFill(b: CrfBench): Unit = {
    val t = b.tag; val n = b.m.n
    b.cy(CrfIn(clearAll = true))
    for (l <- 0 until n) b.alloc(l, BigInt(0x155555 + l), l % 2 == 1, coh = (l % 3) + 1)
    b.idle(3)
    check(pb(b.dut.io.poolEmpty), s"$t full: pool empty")
    check(b.occ == (n / 2, n / 2), s"$t full: occupancy ${b.occ} exp (${n / 2},${n / 2})")
    val keys = Seq(BigInt(0x155555 + n - 1), BigInt(0x155555 + n / 2)).take(b.dut.nSearch)
    b.cy(CrfIn(sKey = keys, sDom = Seq(true, false), sShare = Seq(false, false)))   // model-checked lines n-1, n/2
    val hi = b.srch(0, BigInt(0x155555 + n - 1), d = true)
    check(hi._1 && hi._2 == n - 1, s"$t full: highest line found: $hi")
  }

  // ================================================================== CevVSU reference (plan N2, Fig. 4, text reading)
  final class VArr(val n: Int) {
    val v = Array.fill(n)(false); val dom = Array.fill(n)(false); val freq = Array.fill(n)(0); val rsv = Array.fill(n)(false)
    def ok(i: Int, t: Boolean): Boolean = v(i) && !rsv(i) && dom(i) == t
    def randomize(r: Random): Unit = for (i <- 0 until n) {
      v(i) = r.nextDouble() < 0.8; dom(i) = r.nextBoolean(); freq(i) = r.nextInt(16); rsv(i) = r.nextDouble() < 0.15
    }
  }
  // Drives a stand-alone CevVSU; the CRF candidate port is served from `a`; `cnt` models rebCnt exactly.
  final class VEnv(val dut: CevVSU, val a: VArr, val interval: Int, val hardDef: Int, val softDef: Int) {
    var en = false; val occ = Array(0, 0); var poolEmpty = false; var freeIdx = 0; var hardCsr = 0; var softCsr = 0
    var cnt = 0; var reqValid = false; var reqDom = false; var reqId = 0
    var lastJ = -1
    var cyc = 0L; var f1 = false; var gap = false      // model of the VSU's fire1 / grantGap registers
    var lastLockV = false; var lastLockId = 0; var lastEnd = false      // F4 outputs of the last cycle()
    def HL: Int = if (hardCsr == 0) hardDef else hardCsr
    def SL: Int = if (softCsr == 0) softDef else softCsr
    private def pokeIns(): Unit = {
      val io = dut.io
      io.en.poke(en); io.occ(0).poke(BigInt(occ(0))); io.occ(1).poke(BigInt(occ(1))); io.poolEmpty.poke(poolEmpty)
      io.freeIdx.poke(BigInt(freeIdx)); io.hardCsr.poke(BigInt(hardCsr)); io.softCsr.poke(BigInt(softCsr))
      io.req.valid.poke(reqValid); io.req.bits.poke(reqDom); io.reqId.poke(BigInt(reqId))
      val ci = pi(io.cIdx)
      io.cV.poke(a.v(ci)); io.cDom.poke(a.dom(ci)); io.cFreq.poke(BigInt(a.freq(ci))); io.cRsv.poke(a.rsv(ci))
    }
    // one cycle: (ready, grant, rebal, searching, cIdx) of this cycle
    def cycle(): (Boolean, Int, Boolean, Boolean, Int) = {
      pokeIns()
      val io  = dut.io
      val out = (pb(io.req.ready), pi(io.grant), pb(io.rebal), pb(io.searching), pi(io.cIdx))
      lastLockV = pb(io.lock.valid); lastLockId = pi(io.lock.bits); lastEnd = pb(io.roundEnd)
      check(pb(io.busy) == (out._4 || gap), s"vsu c$cyc: busy=${pb(io.busy)} exp=${out._4 || gap} (search ${out._4}, grantGap $gap) [R1c B1]")
      check(lastLockV == out._4, s"vsu c$cyc: lock.valid=$lastLockV differs from searching=${out._4}")
      dut.clock.step(); cyc += 1
      val fire = out._1 && reqValid
      cnt = if (fire && out._3) 0 else if (!en) 0 else if (cnt < interval) cnt + 1 else cnt
      gap = fire || f1; f1 = fire
      out
    }
    // k cycles with no request (FSM idle)
    def idle(k: Int): Unit = {
      reqValid = false; pokeIns(); dut.clock.step(k); cyc += k
      cnt = if (!en) 0 else math.min(interval, cnt + k)
      if (k >= 1) { gap = k == 1 && f1; f1 = false }
    }
    def drain(): Unit = { reqValid = false; for (_ <- 0 until 14) cycle() }
  }

  // One request of domain rd held until granted. Reference decision per decide cycle (FSM idle): takeFree = free line and
  // occ(rd) < HL; else search target = other domain iff wantX (pool empty, occ(rd) < SL, occ(od) > SL, rebOk), else rd.
  // The candidates evaluated in a round are the indices presented in scan cycles 1..8 (cycle 0 shows a stale index);
  // the grant must be the first lowest-FREQ qualifying one. Returns Some((grant, rebal, rounds)) or None after maxRounds
  // failed rounds (the request is then dropped; call drain()).
  private def vsuEpisode(e: VEnv, rd: Boolean, tag: String, maxRounds: Int = 200): Option[(Int, Boolean, Int)] = {
    val a = e.a
    val rdI = if (rd) 1 else 0; val odI = 1 - rdI
    e.reqValid = true; e.reqDom = rd
    var rounds = 0; var tgt = rd; var cross = false; var cands = Vector.empty[Int]; var j = -1
    var res: Option[(Int, Boolean, Int)] = None
    var guard = 0; var dropped = false; var ends = 0
    while (res.isEmpty && !dropped && guard < (maxRounds + 2) * 14) {
      guard += 1
      val takeFree = !e.poolEmpty && e.occ(rdI) < e.HL
      val wantX    = e.poolEmpty && e.occ(rdI) < e.SL && e.occ(odI) > e.SL && e.cnt >= e.interval
      val freeNow  = e.freeIdx
      val blk      = e.gap                                           // a grant in either of the last 2 cycles
      val (ready, grant, rebal, srch, ci) = e.cycle()
      if (e.lastEnd) { ends += 1; check(srch, s"$tag: roundEnd outside a search") }
      if (srch) check(e.lastLockId == e.reqId, s"$tag: lock.bits ${e.lastLockId}, requester ${e.reqId} (F4)")
      if (!srch && blk) {                                            // F1: no decision while grantGap
        check(!ready, s"$tag: grant within 2 cycles of the previous grant")
      } else if (!srch) {                                            // decide cycle
        if (j > 0) check(!cands.exists(c => a.ok(c, tgt)), s"$tag: round ended without a grant although $cands had a qualifying line")
        if (takeFree) {
          check(ready && grant == freeNow && !rebal, s"$tag: free-pool grant expected (ready=$ready grant=$grant free=$freeNow rebal=$rebal)")
          res = Some((grant, rebal, rounds))
        } else {
          check(!ready, s"$tag: no grant in the decide cycle of a search")
          if (rounds >= maxRounds) dropped = true
          else { tgt = if (wantX) !rd else rd; cross = wantX; cands = Vector.empty; j = 0; rounds += 1 }
        }
      } else {
        if (j >= 1) cands :+= ci
        if (ready) {
          val okc = cands.filter(c => a.ok(c, tgt))
          check(okc.nonEmpty, s"$tag: grant $grant without a qualifying candidate in $cands (tgt=$tgt)")
          if (okc.nonEmpty) {
            val best = okc.minBy(c => a.freq(c))                      // first lowest FREQ (first on ties)
            check(grant == best, s"$tag: grant $grant expected $best (cands $cands, tgt $tgt)")
          }
          check(rebal == cross, s"$tag: rebal=$rebal expected $cross")
          check(j <= 10, s"$tag: grant at scan cycle $j (bound 10: 8 issue + 1 eval + 1 wait + done)")
          check(e.lastEnd, s"$tag: a search grant outside the roundEnd cycle (F4)")
          e.lastJ = j
          res = Some((grant, rebal, rounds))
        }
        j += 1
      }
    }
    e.reqValid = false
    if (res.isEmpty && !dropped) check(false, s"$tag: no grant after $guard cycles")
    check(ends == rounds, s"$tag: $rounds search rounds but $ends roundEnd cycles (F4)")
    res
  }

  private def vsuRandom(e: VEnv, r: Random, episodes: Int, tag: String): Unit = {
    val a = e.a; val n = a.n
    var nFree = 0; var nSearch = 0
    e.en = false
    for (ep <- 0 until episodes) {
      a.randomize(r)
      val rd = r.nextBoolean()
      e.poolEmpty = r.nextBoolean(); e.freeIdx = r.nextInt(n)
      e.occ(0) = r.nextInt(n + 1); e.occ(1) = r.nextInt(n + 1)
      e.hardCsr = if (r.nextDouble() < 0.6) 0 else 1 + r.nextInt(n)
      e.softCsr = if (r.nextDouble() < 0.6) 0 else 1 + r.nextInt(n)
      if (!(0 until n).exists(i => a.ok(i, rd))) { val i = r.nextInt(n); a.v(i) = true; a.rsv(i) = false; a.dom(i) = rd }
      vsuEpisode(e, rd, s"$tag ep$ep") match {
        case Some((g, _, rounds)) => if (rounds == 0) nFree += 1 else nSearch += 1; a.rsv(g) = true
        case None => check(false, s"$tag ep$ep: no grant")
      }
      e.cycle(); e.cycle()                                           // gap, no request
    }
    e.hardCsr = 0; e.softCsr = 0
    check(nFree >= episodes / 8 && nSearch >= episodes / 8, s"$tag: coverage free=$nFree search=$nSearch")
    info(s"$tag: $episodes episodes, free-pool grants $nFree, searches $nSearch, last round grant at scan cycle ${e.lastJ}")
  }

  private def vsuDirected(e: VEnv, tag: String): Unit = {
    val a = e.a; val n = a.n
    def setArr(): Unit = for (i <- 0 until n) { a.v(i) = true; a.dom(i) = i >= 4; a.freq(i) = (i * 5) % 16; a.rsv(i) = false }
    setArr()
    e.en = false
    // (1) free line and below the hard limit: grant in the request cycle
    e.poolEmpty = false; e.freeIdx = 6; e.occ(0) = 3; e.occ(1) = e.hardDef - 1
    val r1 = vsuEpisode(e, true, s"$tag hard-below")
    check(r1.exists(x => x._1 == 6 && x._3 == 0 && !x._2), s"$tag below the hard limit takes the free line: $r1")
    // F1: no decision in the 2 cycles after a grant: back-to-back free-pool grants are 3 cycles apart
    e.idle(3); val g0 = e.cyc
    vsuEpisode(e, true, s"$tag gap-a"); val g1 = e.cyc
    vsuEpisode(e, true, s"$tag gap-b"); val g2 = e.cyc
    check(g1 - g0 == 1 && g2 - g1 == 3, s"$tag grant spacing: first after ${g1 - g0} cycle(s) (exp 1), next ${g2 - g1} later (exp 3)")
    // (2) at the hard limit with free lines left: replace an own line
    e.occ(1) = e.hardDef
    val r2 = vsuEpisode(e, true, s"$tag hard-at")
    check(r2.exists(x => x._3 > 0 && a.dom(x._1) && !x._2), s"$tag at the hard limit: own-domain replacement: $r2")
    // (3) hard-limit CSR override
    e.hardCsr = 5; e.occ(1) = 5
    check(vsuEpisode(e, true, s"$tag hardCsr-at").exists(_._3 > 0), s"$tag hardCsr 5, occ 5: search")
    e.occ(1) = 4
    check(vsuEpisode(e, true, s"$tag hardCsr-below").exists(_._3 == 0), s"$tag hardCsr 5, occ 4: free line")
    e.hardCsr = 0
    // (4) no qualifying line of the requester's domain: rounds repeat without a grant (Fig. 4 retry)
    for (i <- 4 until n) a.rsv(i) = true
    e.occ(1) = e.hardDef
    val r4 = vsuEpisode(e, true, s"$tag retry", maxRounds = 4)
    check(r4.isEmpty, s"$tag no grant while every own line is reserved: $r4")
    e.drain(); check(!pb(e.dut.io.searching), s"$tag VSU idle after the dropped request")
    for (i <- 4 until n) a.rsv(i) = false
    check(vsuEpisode(e, true, s"$tag retry-recover").nonEmpty, s"$tag grant once an own line qualifies")
    // (5) rebalancing (Section 5.2 text): pool empty, requester (dom 0) below soft, dom 1 above soft, interval elapsed
    setArr()
    e.poolEmpty = true; e.occ(0) = 2; e.occ(1) = n - 2
    e.en = true
    val q1 = vsuEpisode(e, false, s"$tag rebal-early")
    check(q1.exists(x => !x._2 && !a.dom(x._1)), s"$tag before the interval: own replacement: $q1")
    e.idle(e.interval)
    val q2 = vsuEpisode(e, false, s"$tag rebal-due")
    check(q2.exists(x => x._2 && a.dom(x._1)), s"$tag after the interval: one cross-domain eviction of a dom-1 line: $q2")
    check(e.cnt <= 15, s"$tag the cross grant restarts the interval (cnt ${e.cnt})")
    val q3 = vsuEpisode(e, false, s"$tag rebal-again")
    check(q3.exists(x => !x._2), s"$tag right after a rebalancing eviction: own replacement: $q3")
    // soft CSR override: other domain no longer above the soft limit -> no rebalancing
    e.softCsr = n - 1; e.idle(e.interval)
    check(vsuEpisode(e, false, s"$tag softCsr").exists(x => !x._2), s"$tag softCsr ${n - 1}: occ(od) ${n - 2} not above: no rebalancing")
    e.softCsr = 0
    // requester at/above the soft limit -> no rebalancing
    e.occ(0) = e.softDef; e.idle(e.interval)
    check(vsuEpisode(e, false, s"$tag at-soft").exists(x => !x._2), s"$tag requester at its soft limit: no rebalancing")
    e.occ(0) = 2
    // en = 0 clears the interval counter
    e.en = false; e.idle(e.interval + 5); e.en = true; e.idle(e.interval / 4)
    check(vsuEpisode(e, false, s"$tag en-clear").exists(x => !x._2), s"$tag interval restarts at enable")
    e.idle(e.interval)
    check(vsuEpisode(e, false, s"$tag en-due").exists(x => x._2), s"$tag interval elapsed after enable: rebalancing")
    // pool not empty: free line regardless of the rebalancing state
    e.idle(e.interval); e.poolEmpty = false; e.freeIdx = 9
    check(vsuEpisode(e, false, s"$tag free-over-rebal").exists(x => x._1 == 9 && x._3 == 0), s"$tag free line preferred")
    e.en = false; e.idle(2)
  }

  // Owner F4 ruling ("Lock to the requester"), VSU side: the search latches the requester's id and domain at its start
  // (lock.valid / lock.bits); a grant made by the search reaches only that requester (same id, same domain); any other
  // presenter (another id, or the same id with the other domain) gets no grant until the search ends, and the next
  // decision is made for whoever is presented then. The bench presents a different requester in mid-search as an
  // unlocked arbiter would; with the amended M3 this cannot happen (pair-lock, pair-random).
  private def vsuLock(e: VEnv, tag: String): Unit = {
    val a = e.a; val n = a.n
    for (i <- 0 until n) { a.v(i) = true; a.dom(i) = i >= 4; a.freq(i) = (i * 7) % 16; a.rsv(i) = false }
    e.en = false; e.poolEmpty = true; e.hardCsr = 0; e.softCsr = 0
    e.occ(0) = 4; e.occ(1) = n - 4                       // cache full, no rebalancing: each request searches its own domain
    // Runs up to `maxCyc` cycles with presenter pres(t) = (valid, id, dom), until the first grant; checks every cycle that
    // a grant during a search goes to the latched (id, domain) and that lock.bits is the id presented at the search start.
    // Returns (cycle of the first fire, its id, its domain, its line, latched (id, dom) of the searches seen).
    def run(maxCyc: Int)(pres: Int => (Boolean, Int, Boolean)): (Int, Int, Boolean, Int, Vector[(Int, Boolean)]) = {
      e.reqValid = false; e.drain()
      var t = 0; var fired: Option[(Int, Int, Boolean, Int)] = None
      var prev = (false, 0, false); var prevSrch = false; var lat = (0, false); var lats = Vector.empty[(Int, Boolean)]
      while (fired.isEmpty && t < maxCyc) {
        val (vv, id, d) = pres(t); e.reqValid = vv; e.reqId = id; e.reqDom = d
        val (rdy, g, _, srch, _) = e.cycle()
        if (srch && !prevSrch) {                                        // first search cycle: latched = previous presenter
          lat = (prev._2, prev._3); lats :+= lat
          check(e.lastLockId == lat._1, s"$tag t$t: lock.bits ${e.lastLockId}, requester at the search start ${lat._1}")
        }
        if (srch) check(e.lastLockId == lat._1, s"$tag t$t: lock.bits changed in mid-search to ${e.lastLockId}")
        if (rdy && vv) {
          if (srch) {
            check(id == lat._1, s"$tag t$t: search grant to requester $id, search made for ${lat._1} (F4)")
            check(d == lat._2, s"$tag t$t: search grant to a domain-$d requester, search made for domain ${lat._2} (F4)")
            check(a.dom(g) == lat._2, s"$tag t$t: search grant of line $g (domain ${a.dom(g)}) for a domain-${lat._2} search")
          }
          fired = Some((t, id, d, g))
        }
        prev = (vv, id, d); prevSrch = srch; t += 1
      }
      e.reqValid = false
      fired match {
        case Some((ft, fid, fd, fg)) => (ft, fid, fd, fg, lats)
        case None => check(false, s"$tag: no grant in $maxCyc cycles"); (-1, -1, false, -1, lats)
      }
    }
    // (A) another requester of the other domain presented in mid-search: no grant to it in that search; its own search follows
    val ra = run(200)(t => if (t < 3) (true, 2, true) else (true, 1, false))
    check(ra._1 >= 12 && ra._2 == 1 && !ra._3 && !a.dom(ra._4) && ra._5.headOption.contains((2, true)) && ra._5.lift(1).contains((1, false)),
          s"$tag A: other-domain requester in mid-search: first fire $ra (exp requester 1, domain 0, after the round of 2)")
    // (B) the same id with the other domain (an MSHR re-allocated) in mid-search: no grant in that search
    val rb = run(200)(t => if (t < 3) (true, 2, true) else (true, 2, false))
    check(rb._1 >= 12 && rb._2 == 2 && !rb._3 && !a.dom(rb._4), s"$tag B: same id, domain switched in mid-search: $rb")
    // (C) another id of the same domain in mid-search: no grant in that search either (the grant goes to the requester)
    val rc = run(200)(t => if (t < 3) (true, 2, true) else (true, 3, true))
    check(rc._1 >= 12 && rc._2 == 3 && rc._3 && a.dom(rc._4) && rc._5.lift(1).contains((3, true)),
          s"$tag C: same domain, other id in mid-search: $rc")
    // (D) the requester held through the search: granted at the end of its first round with a line of its domain
    val rdd = run(200)(_ => (true, 2, true))
    check(rdd._1 == 11 && rdd._2 == 2 && rdd._3 && a.dom(rdd._4), s"$tag D: requester held: grant at the round end (t 11): $rdd")
    // (E) the requester withdraws in mid-search: no fire, the search ends (lock released) at t 11 and nothing is granted
    e.reqValid = false; e.drain()
    var endAt = -1; var anyFire = false
    for (t <- 0 until 16) {
      e.reqValid = t < 3; e.reqId = 2; e.reqDom = true
      val (rdy, _, _, _, _) = e.cycle()
      if (rdy && e.reqValid) anyFire = true
      if (e.lastEnd) endAt = t
    }
    check(!anyFire && endAt == 11 && !pb(e.dut.io.searching), s"$tag E: withdrawn requester: fire=$anyFire roundEnd at $endAt")
    e.reqValid = false; e.reqId = 0; e.drain()
    info(s"$tag: first fires A=$ra B=$rb C=$rc D=$rdd")
  }

  // ================================================================== CRF + VSU + M3 pick wired as amended D1 / M3 (one search lane)
  // nReq requesters ("MSHRs") reach the VSU through the amended M3 round-robin pick (ceviche-plan-v5-amend-T1c.md, v3
  // section M3, copied with the MSHR requests as bench inputs; owner F4 "Lock to the requester").
  class CevPairTB(val nLines: Int, keyBits: Int, hardDefault: Int, softDefault: Int, rebalInterval: Int, expire: Int,
                  val nReq: Int) extends Module {
    val lineBits = log2Ceil(nLines)
    val idW      = log2Ceil(nReq) max 1
    val io = IO(new Bundle {
      val en        = Input(Bool())
      val rv        = Input(Vec(nReq, Bool()))                // requester i asks for a line (MSHR cev_vreq.valid)
      val rdm       = Input(Vec(nReq, Bool()))                // its domain (MSHR cev_vreq.bits)
      val rr        = Output(Vec(nReq, Bool()))               // MSHR cev_vreq.ready (fire = rr(i) && rv(i))
      val pick      = Output(UInt(idW.W))
      val lockV     = Output(Bool())
      val lockId    = Output(UInt(idW.W))
      val roundEnd  = Output(Bool())
      val grant     = Output(UInt(lineBits.W))
      val rebal     = Output(Bool())
      val searching = Output(Bool())
      val wrValid   = Input(Bool())
      val wrLine    = Input(UInt(lineBits.W))
      val wrKey     = Input(UInt(keyBits.W))
      val wrDom     = Input(Bool())
      val wrV       = Input(Bool())
      val wrAlloc   = Input(Bool())
      val clearAll  = Input(Bool())
      val holdV     = Input(Bool())                           // [R1c B1] rsvHold (an upgrade MSHR reserves its hit line)
      val holdLine  = Input(UInt(lineBits.W))
      val relV      = Input(Bool())                           // [R1c B1] rsvRel (its final write)
      val relLine   = Input(UInt(lineBits.W))
      val busy      = Output(Bool())
      val sKey      = Input(UInt(keyBits.W))
      val sHit      = Output(Bool())
      val sLine     = Output(UInt(lineBits.W))
      val sRsv      = Output(Bool())
      val occ       = Output(Vec(2, UInt((lineBits + 1).W)))
      val poolEmpty = Output(Bool())
    })
    val crf = Module(new CevCRF(nLines, keyBits, 1, isI = false, withSec = false, expireCycles = expire))
    val vsu = Module(new CevVSU(nLines, hardDefault, softDefault, rebalInterval, nReq))
    vsu.io.hardCsr := 0.U; vsu.io.softCsr := 0.U
    vsu.io.occ := crf.io.occ; vsu.io.poolEmpty := crf.io.poolEmpty; vsu.io.freeIdx := crf.io.freeIdx
    crf.io.cIdx := vsu.io.cIdx
    vsu.io.cV := crf.io.cV; vsu.io.cDom := crf.io.cDom; vsu.io.cFreq := crf.io.cFreq; vsu.io.cRsv := crf.io.cRsv
    vsu.io.en := io.en
    // ---- amended M3 (T1c v3, owner F4): the M3 body with mshrs(i).io.cev_vreq = (io.rv(i), io.rdm(i), io.rr(i)),
    // io.cev_vend/cev_vlock/cev_vreqId = the VSU roundEnd / lock.valid / reqId (amended D1) ----
    val nV    = nReq
    val vq    = io.rv                                   // [T1c v4] was VecInit(io.rv): ambiguous overload in scalac
    val vPick = RegInit(0.U((log2Ceil(nV) max 1).W))
    val above = VecInit((0 until nV).map(i => vq(i) && i.U > vPick)).asUInt
    val vNext = Mux(above.orR, PriorityEncoder(above), PriorityEncoder(vq.asUInt))
    when (vsu.io.roundEnd || (!vsu.io.lock.valid && !vq(vPick))) { vPick := vNext }
    vsu.io.req.valid := vq(vPick)
    vsu.io.req.bits  := io.rdm(vPick)
    vsu.io.reqId     := vPick
    for (i <- 0 until nV) io.rr(i) := vsu.io.req.ready && vPick === i.U
    // ---- amended D1 ----
    crf.io.rsvSet.valid := vsu.io.req.fire; crf.io.rsvSet.bits := vsu.io.grant
    crf.io.rsvSetDom := vsu.io.req.bits
    crf.io.rsvHold.valid := io.holdV; crf.io.rsvHold.bits := io.holdLine
    crf.io.rsvRel.valid  := io.relV;  crf.io.rsvRel.bits  := io.relLine
    io.busy := vsu.io.busy
    io.pick := vPick; io.lockV := vsu.io.lock.valid; io.lockId := vsu.io.lock.bits; io.roundEnd := vsu.io.roundEnd
    io.grant := vsu.io.grant; io.rebal := vsu.io.rebal; io.searching := vsu.io.searching
    crf.io.sKey(0) := io.sKey; crf.io.sDom(0) := false.B; crf.io.sShare(0) := false.B
    io.sHit := crf.io.sHit(0); io.sLine := crf.io.sLine(0); io.sRsv := crf.io.sRsv(0)
    crf.io.incVec := 0.U
    crf.io.wr.valid := io.wrValid
    crf.io.wr.bits.line := io.wrLine; crf.io.wr.bits.key := io.wrKey; crf.io.wr.bits.v := io.wrV
    crf.io.wr.bits.coh := 0.U; crf.io.wr.bits.alloc := io.wrAlloc; crf.io.wr.bits.dom := io.wrDom
    crf.io.wr.bits.s := false.B; crf.io.wr.bits.inv := false.B; crf.io.wr.bits.sec := false.B
    crf.io.clearAll := io.clearAll; crf.io.clearDom.valid := false.B; crf.io.clearDom.bits := false.B
    io.occ := crf.io.occ; io.poolEmpty := crf.io.poolEmpty
  }

  // Drives a CevPairTB and mirrors the CRF line state (key, v, dom, rsv, rsvDom) exactly. Every cycle it checks:
  // - occupancy: the output equals the owner-ruling count (F1 + F5: reserved lines by the reserving domain, valid or not;
  //   unreserved valid lines by their domain) of the state two cycles ago; with `limit`, neither the output nor the true
  //   count of the current state exceeds it (the hard limit holds with grants in flight); no grant names a reserved line (G4);
  // - F4: the search latches the requester presented in its decide cycle (lock.bits); while it runs the pick does not move
  //   and a grant reaches only that requester with that domain (never a requester of the other domain);
  // - no starvation: a turn ends at a search end (owner lock.bits) or at a free-pool grant (owner the granted requester);
  //   a waiting requester sees at most nReq - 1 turns of other requesters before its own turn.
  // A requester's request drops in the cycle after its grant (the MSHR's cev_line_v); callers may raise it again.
  final class PEnv(val dut: CevPairTB, val tag: String, val limit: Option[Int]) {
    val n = dut.nLines; val nr = dut.nReq
    var cyc = 0L; var en = false
    val rv = Array.fill(nr)(false); val rdm = Array.fill(nr)(false)
    var wr: Option[(Int, BigInt, Boolean, Boolean, Boolean)] = None     // (line, key, dom, v, alloc)
    var sKey = BigInt(0); var clr = false
    var hold: Option[Int] = None; var rel: Option[Int] = None         // [R1c B1] rsvHold / rsvRel of this cycle
    private var fq1 = false; private var fq2 = false                  // a grant 1 / 2 cycles ago (VSU grantGap)
    val key = Array.fill(n)(BigInt(0)); val v = Array.fill(n)(false); val dom = Array.fill(n)(false)
    val rsv = Array.fill(n)(false); val rsvDom = Array.fill(n)(false)
    def trueOcc: (Int, Int) = {
      def c(d: Boolean) = (0 until n).count(i => if (rsv(i)) rsvDom(i) == d else v(i) && dom(i) == d)
      (c(false), c(true))
    }
    private var s1 = (0, 0); private var s2 = (0, 0)                  // true counts one and two cycles ago
    var maxOcc = (0, 0)
    private var nextKey = 1
    def fresh(): BigInt = { val k = nextKey; nextKey += 1; BigInt(k) }
    // F4 / starvation bookkeeping
    var lastSrch = false; var lastLockId = 0; var lastEnd = false; var lastPick = 0
    private var prevPick = 0; private var prevDom = false; private var lat = (0, false)
    var searchGrants = 0; var crossWait = 0                           // grants made by a search; cycles with an other-domain requester waiting on a search
    val turns = Array.fill(nr)(0); val waitCyc = Array.fill(nr)(0L); var maxTurns = 0; var maxWait = 0L
    def cycle(): (Option[Int], Int, Boolean) = {
      val io = dut.io
      io.en.poke(en)
      for (i <- 0 until nr) { io.rv(i).poke(rv(i)); io.rdm(i).poke(rdm(i)) }
      val w = wr.getOrElse((0, BigInt(0), false, false, false))
      io.wrValid.poke(wr.isDefined); io.wrLine.poke(BigInt(w._1)); io.wrKey.poke(w._2); io.wrDom.poke(w._3)
      io.wrV.poke(w._4); io.wrAlloc.poke(w._5); io.clearAll.poke(clr)
      io.holdV.poke(hold.isDefined); io.holdLine.poke(BigInt(hold.getOrElse(0)))
      io.relV.poke(rel.isDefined);   io.relLine.poke(BigInt(rel.getOrElse(0)))
      io.sKey.poke(sKey)
      val rr = (0 until nr).map(i => pb(io.rr(i)))
      val g = pi(io.grant); val rb = pb(io.rebal)
      val srch = pb(io.lockV); val lockId = pi(io.lockId); val end = pb(io.roundEnd); val pick = pi(io.pick)
      check(srch == pb(io.searching), s"$tag c$cyc: lock.valid differs from searching")
      check(pb(io.busy) == (srch || fq1 || fq2), s"$tag c$cyc: busy=${pb(io.busy)} exp=${srch || fq1 || fq2} [R1c B1]")
      check(rr.count(identity) <= 1 && rr.indices.forall(i => !rr(i) || i == pick), s"$tag c$cyc: ready to $rr with pick $pick")
      val o = occ; val s0 = trueOcc
      check(o == s2, s"$tag c$cyc: occupancy $o, expected $s2 (the count of the state two cycles ago)")
      limit.foreach { h =>
        check(o._1 <= h && o._2 <= h, s"$tag c$cyc: occupancy output $o exceeds the hard limit $h")
        check(s0._1 <= h && s0._2 <= h, s"$tag c$cyc: true occupancy $s0 exceeds the hard limit $h")
      }
      maxOcc = (maxOcc._1 max s0._1, maxOcc._2 max s0._2)
      val f = (0 until nr).find(i => rr(i) && rv(i))
      f.foreach { _ => check(!rsv(g), s"$tag c$cyc: line $g granted while reserved (G4)") }
      // F4: latched requester and domain; pick frozen; grant only to the latched requester and domain
      if (srch && !lastSrch) { lat = (prevPick, prevDom); check(lockId == lat._1, s"$tag c$cyc: lock.bits $lockId, presented at the search start ${lat._1}") }
      if (srch) {
        check(lockId == lat._1 && pick == lat._1, s"$tag c$cyc: lock.bits $lockId / pick $pick moved in mid-search (latched ${lat._1})")
        if ((0 until nr).exists(i => rv(i) && rdm(i) != lat._2)) crossWait += 1
        f.foreach { i =>
          searchGrants += 1
          check(i == lat._1, s"$tag c$cyc: search grant to requester $i, search made for requester ${lat._1} (F4)")
          check(rdm(i) == lat._2, s"$tag c$cyc: search grant to a domain-${rdm(i)} requester, search made for domain ${lat._2} (F4)")
          check(end, s"$tag c$cyc: search grant outside the roundEnd cycle")
        }
      }
      // no starvation (round-robin turns)
      val owner: Option[Int] = if (end) Some(lockId) else if (!srch) f else None
      owner.foreach { ow => for (j <- 0 until nr) { if (j == ow) turns(j) = 0 else if (rv(j)) turns(j) += 1 } }
      for (j <- 0 until nr) {
        if (rv(j)) waitCyc(j) += 1 else { turns(j) = 0; waitCyc(j) = 0 }
        check(turns(j) <= nr - 1, s"$tag c$cyc: requester $j waited ${turns(j)} turns of others (bound ${nr - 1})")
        maxTurns = maxTurns max turns(j); maxWait = maxWait max waitCyc(j)
      }
      lastSrch = srch; lastLockId = lockId; lastEnd = end; lastPick = pick
      prevPick = pick; prevDom = rdm(pick)
      dut.clock.step(); cyc += 1
      // mirror the CRF update of this edge: write first, then the reservation (N1 statement order)
      wr.foreach { case (l, k, d, vv, al) => key(l) = k; v(l) = vv; if (al) { dom(l) = d; rsv(l) = false } }
      rel.foreach { l => rsv(l) = false }                               // [R1c B1] N1 order: write, rel, hold, set
      hold.foreach { l => rsv(l) = true; rsvDom(l) = dom(l) }
      f.foreach { i => rsv(g) = true; rsvDom(g) = rdm(i); rv(i) = false; turns(i) = 0; waitCyc(i) = 0 }
      fq2 = fq1; fq1 = f.isDefined
      if (clr) for (i <- 0 until n) { v(i) = false; rsv(i) = false }   // clearAll last: wins
      s2 = s1; s1 = s0
      (f, g, rb)
    }
    def occ: (Int, Int) = (pi(dut.io.occ(0)), pi(dut.io.occ(1)))
    // Requester `who` asks for a line for domain d, then (writeIt) the line is re-keyed with a fresh block of domain d in
    // the next cycle, as the MSHR's final metadata write does, and 3 cycles pass (occupancy settles).
    // Returns (line, rebal, oldV, oldDom, fireCyc).
    def alloc(d: Boolean, tag: String, writeIt: Boolean = true, who: Int = 0): Option[(Int, Boolean, Boolean, Boolean, Long)] = {
      rv(who) = true; rdm(who) = d; wr = None
      var g: Option[(Int, Boolean, Long)] = None; var t = 0
      while (g.isEmpty && t < 3000) { val (f, gr, rb) = cycle(); if (f.contains(who)) g = Some((gr, rb, cyc - 1)); t += 1 }
      rv(who) = false
      g match {
        case None => check(false, s"$tag: no grant in 3000 cycles"); None
        case Some((l, rb, fc)) =>
          val pv = v(l); val pd = dom(l)
          if (writeIt) {
            if (pv) {                                               // a valid victim is marked reserved until re-keyed (D3b)
              dut.io.sKey.poke(key(l))
              check(pb(dut.io.sHit) && pi(dut.io.sLine) == l && pb(dut.io.sRsv), s"$tag: victim line $l not shown as reserved")
            }
            val k = fresh(); wr = Some((l, k, d, true, true)); cycle(); wr = None
            dut.io.sKey.poke(k)
            check(pb(dut.io.sHit) && pi(dut.io.sLine) == l && !pb(dut.io.sRsv), s"$tag: re-keyed line $l not found / still reserved")
            cycle(); cycle(); cycle()
          }
          Some((l, rb, pv, pd, fc))
      }
    }
  }

  private def pairScenario(e: PEnv, hl: Int, sl: Int, interval: Int, tag: String): Unit = {
    val n = e.n
    e.en = true; val enCyc = e.cyc
    // P1: domain 1 fills free lines up to its hard limit, lowest index first
    for (i <- 0 until hl) e.alloc(true, s"$tag P1.$i").foreach { case (l, rb, pv, _, _) =>
      check(!pv && !rb && l == i, s"$tag P1.$i: free line $i expected, got line $l (oldV $pv, rebal $rb)")
    }
    check(e.occ == (0, hl), s"$tag P1: occupancy ${e.occ}")
    // P2: at the hard limit with free lines left: replacement of an own line, occupancy unchanged
    e.alloc(true, s"$tag P2").foreach { case (l, rb, pv, pd, _) =>
      check(pv && pd && !rb && l < hl, s"$tag P2: own-domain replacement expected, got line $l (oldV $pv oldDom $pd)")
    }
    check(e.occ == (0, hl) && !pb(e.dut.io.poolEmpty), s"$tag P2: occupancy ${e.occ}")
    // P3: domain 0 takes the remaining free lines
    for (i <- hl until n) e.alloc(false, s"$tag P3.$i").foreach { case (l, _, pv, _, _) =>
      check(!pv && l == i, s"$tag P3.$i: free line $i expected, got $l")
    }
    check(pb(e.dut.io.poolEmpty) && e.occ == (n - hl, hl), s"$tag P3: full cache, occupancy ${e.occ}")
    // P4: cache full; domain 0 below soft, domain 1 above soft: one rebalancing eviction per interval until the limits meet
    var rebalAt = Vector.empty[Long]
    while (e.cyc < enCyc + 12 * interval) {
      e.alloc(false, s"$tag P4").foreach { case (l, rb, pv, pd, fc) =>
        check(pv, s"$tag P4: a full cache grants only valid lines (line $l)")
        if (rb) { check(pd, s"$tag P4: rebalancing victim $l must belong to domain 1"); rebalAt :+= fc }
        else check(!pd, s"$tag P4: non-rebalancing grant $l must be a domain-0 line")
      }
    }
    val expRebal = math.min(sl - (n - hl), hl - sl)
    check(rebalAt.size == expRebal, s"$tag P4: $expRebal rebalancing evictions expected, got ${rebalAt.size} at $rebalAt")
    check(rebalAt.headOption.forall(_ - enCyc >= interval), s"$tag P4: first rebalancing before one interval after enable")
    rebalAt.sliding(2).foreach { p => if (p.size == 2) check(p(1) - p(0) >= interval, s"$tag P4: rebalancing spacing ${p(1) - p(0)} < $interval") }
    check(e.occ == (n - hl + expRebal, hl - expRebal), s"$tag P4: final occupancy ${e.occ}")
    info(s"$tag: rebalancing evictions at cycles ${rebalAt.map(_ - enCyc)} after enable (interval $interval)")
  }

  // Owner F1 ruling ("Count reserved lines"): a granted line counts for the requester from the grant and the VSU decides
  // nothing in the 2 cycles after a grant, so grants still in flight cannot push a domain past its hard limit. Domain 1 at
  // HL - 1 asks again every cycle for 40 cycles while nothing is re-keyed (MSHRs waiting for L2): exactly one free-pool
  // grant, every later grant replaces a valid own line; then each valid victim is cleared (non-allocating v := 0, the D$
  // s_meta_clear write) and every line re-keyed. PEnv checks the limit every cycle.
  private def pairInflight(e: PEnv, hl: Int, tag: String): Unit = {
    e.en = true
    for (i <- 0 until hl - 1) e.alloc(true, s"$tag pre.$i")
    check(e.occ == (0, hl - 1), s"$tag: occupancy before the burst ${e.occ}")
    var grants = Vector.empty[(Int, Boolean, Boolean)]                  // (line, was valid, its domain)
    for (_ <- 0 until 40) {
      e.rv(0) = true; e.rdm(0) = true
      val (f, g, _) = e.cycle(); if (f.contains(0)) grants :+= ((g, e.v(g), e.dom(g)))
    }
    e.rv(0) = false
    val gl = grants.map(_._1)
    check(gl.distinct.size == gl.size, s"$tag: one line granted twice: $gl (G4)")
    check(grants.count(!_._2) == 1, s"$tag: exactly one free-pool grant expected at occupancy ${hl - 1}: $grants")
    check(grants.filter(_._2).forall(_._3), s"$tag: grants after the limit must replace own (domain-1) lines: $grants")
    check(grants.size >= 2, s"$tag: no own-line replacement within 40 cycles: $grants")
    for ((g, wasV, _) <- grants if wasV) { e.wr = Some((g, e.key(g), true, false, false)); e.cycle() }   // victim clear
    e.wr = None; e.cycle(); e.cycle(); e.cycle()
    check(e.occ == (0, hl), s"$tag: cleared victims still count for domain 1: ${e.occ}")
    for (g <- gl) { e.wr = Some((g, e.fresh(), true, true, true)); e.cycle() }
    e.wr = None; e.cycle(); e.cycle(); e.cycle()
    check(e.occ == (0, hl) && e.trueOcc == (0, hl), s"$tag: after re-keying ${e.occ}")
    info(s"$tag: 40-cycle request burst of domain 1 at occupancy ${hl - 1} (hard limit $hl): grants ${grants.map(_._1)} " +
         s"(${grants.count(!_._2)} free, ${grants.count(_._2)} own replacements); max true occupancy ${e.maxOcc}")
  }

  // Owner F4 ruling ("Lock to the requester") with the amended M3: requester 2 (domain 1, at its hard limit) starts a search
  // on a full cache; requester 0 (domain 0; the stock fixed-priority Arbiter would present it first) asks two cycles into
  // that search. The pick stays on requester 2 until the search ends, the grant (an own domain-1 line) goes to requester 2,
  // and requester 0 is served next (its own line, or a domain-1 line if it is a rebalancing grant).
  private def pairLock(e: PEnv, hl: Int, tag: String): Unit = {
    val n = e.n
    e.en = true
    for (i <- 0 until hl) e.alloc(true, s"$tag d1.$i")
    for (i <- hl until n) e.alloc(false, s"$tag d0.$i")
    check(pb(e.dut.io.poolEmpty) && e.occ == (n - hl, hl), s"$tag: full cache ${e.occ}")
    e.rv(2) = true; e.rdm(2) = true
    var t = 0; var started = -1; var raised = -1; var fire2 = -1; var fire0 = -1; var g2 = -1; var g0 = -1
    while ((fire2 < 0 || fire0 < 0) && t < 400) {
      if (started >= 0 && raised < 0 && t >= started + 2) { e.rv(0) = true; e.rdm(0) = false; raised = t }
      if (fire0 < 0 && raised >= 0) e.rv(0) = true
      if (fire2 < 0) e.rv(2) = true
      val wasV = (0 until n).map(e.v(_)); val wasDom = (0 until n).map(e.dom(_))
      val (f, g, rb) = e.cycle()
      if (started < 0 && e.lastSrch) { started = t; check(e.lastLockId == 2, s"$tag: search started for requester ${e.lastLockId}, exp 2") }
      f match {
        case Some(2) => fire2 = t; g2 = g
          check(e.lastEnd && e.lastLockId == 2 && wasV(g) && wasDom(g) && !rb, s"$tag: requester 2 got line $g (valid ${wasV(g)}, domain ${wasDom(g)}, rebal $rb) at t$t")
        case Some(0) => fire0 = t; g0 = g
          check(wasV(g) && wasDom(g) == rb, s"$tag: requester 0 got line $g (valid ${wasV(g)}, domain ${wasDom(g)}, rebal $rb)")
        case Some(i) => check(false, s"$tag: grant to requester $i, which never asked")
        case None =>
      }
      t += 1
    }
    for (i <- 0 until e.nr) e.rv(i) = false
    check(started >= 0 && raised > started && fire2 > raised && fire0 > fire2,
          s"$tag: search start t$started, requester 0 asks t$raised, requester 2 granted t$fire2 (line $g2), requester 0 t$fire0 (line $g0)")
    check(e.crossWait >= fire2 - raised, s"$tag: coverage: requester 0 waited ${e.crossWait} cycles on the search of requester 2")
    e.cycle(); e.cycle(); e.cycle()
    info(s"$tag: search for requester 2 from t$started, requester 0 (other domain) asks at t$raised, grants: 2 at t$fire2 (line $g2), 0 at t$fire0 (line $g0)")
  }

  // [R1c B1] upg-hold with the VSU: a valid line held by an upgrade MSHR is never granted (neither by the free pool nor by a
  // search) until its final write releases it.  Full cache (domain 1 at the hard limit, domain 0 the rest); line 2 (domain 1)
  // is held; 60 further domain-1 requests are granted and re-keyed (PEnv fails any grant of a reserved line, G4, and any
  // occupancy deviation), none names line 2, the count of domain 1 stays at the limit; then the release lets a later search
  // pick it (within 400 requests).
  private def pairUpg(e: PEnv, hl: Int, tag: String): Unit = {
    val n = e.n
    e.en = true
    for (i <- 0 until hl) e.alloc(true, s"$tag d1.$i")
    for (i <- hl until n) e.alloc(false, s"$tag d0.$i")
    check(pb(e.dut.io.poolEmpty) && e.occ == (n - hl, hl), s"$tag: full cache ${e.occ}")
    e.hold = Some(2); e.cycle(); e.hold = None; e.cycle(); e.cycle(); e.cycle()
    check(e.rsv(2) && e.occ == (n - hl, hl), s"$tag: line 2 held, count unchanged ${e.occ}")
    e.dut.io.sKey.poke(e.key(2))
    check(pb(e.dut.io.sHit) && pb(e.dut.io.sRsv), s"$tag: the held line hits and is shown reserved")
    var got2 = 0
    for (i <- 0 until 60) e.alloc(true, s"$tag held.$i").foreach { case (l, _, _, _, _) => if (l == 2) got2 += 1 }
    check(got2 == 0 && e.rsv(2), s"$tag: a search granted the held line $got2 times")
    check(e.occ == (n - hl, hl), s"$tag: count after the held phase ${e.occ}")
    e.wr = Some((2, e.key(2), true, true, false)); e.rel = Some(2); e.cycle(); e.wr = None; e.rel = None   // final write + release
    e.cycle(); e.cycle(); e.cycle()
    check(!e.rsv(2) && e.occ == (n - hl, hl), s"$tag: released, count unchanged ${e.occ}")
    var i = 0; var hit2 = false
    while (!hit2 && i < 400) { e.alloc(true, s"$tag rel.$i").foreach { case (l, _, _, _, _) => if (l == 2) hit2 = true }; i += 1 }
    check(hit2, s"$tag: the released line was never granted in 400 requests")
    info(s"$tag: held line 2 never granted in 60 requests, granted after release (request $i)")
  }

  // Random in-flight traffic with nReq requesters ("MSHRs", 70 % domain 1) behind the amended M3 pick: a free requester
  // asks with probability 0.15 per cycle; each granted line is re-keyed after a random 1-40 cycles, a valid victim first
  // cleared (non-allocating v := 0) after a random 1-20 cycles; in a cycle without such a write, an L2 probe invalidates a
  // random valid line with probability 0.04 (non-allocating v := 0; with F5 a probed reserved line stays counted). One CRF
  // write per cycle. Every 1500 cycles the requesters drain and clearAll empties the CRF, so the fill phase recurs.
  // PEnv checks the hard limit, the count, F4 (grant only to the latched requester and domain) and the turn bound every cycle.
  private def pairRandom(e: PEnv, hl: Int, cycles: Int, r: Random, tag: String): Unit = {
    final class Fl(val line: Int, val dom: Boolean, val slot: Int, var due: Long, var cleared: Boolean)
    val nr = e.nr
    val fl = scala.collection.mutable.ArrayBuffer[Fl]()
    var nGrant = 0; var nRebal = 0; var nVictim = 0; var nClear = 0; var nProbe = 0; var nextClear = 1500L
    e.en = true
    while (e.cyc < cycles) {
      val draining = e.cyc >= nextClear
      e.clr = draining && !e.rv.contains(true) && fl.isEmpty && !pb(e.dut.io.searching)
      if (e.clr) { nextClear += 1500; nClear += 1 }
      for (i <- 0 until nr)
        if (!draining && !e.rv(i) && !fl.exists(_.slot == i) && r.nextDouble() < 0.15) { e.rv(i) = true; e.rdm(i) = r.nextDouble() < 0.7 }
      e.wr = None
      fl.find(_.due <= e.cyc) match {
        case Some(f) =>
          if (e.v(f.line) && !f.cleared) {                                 // valid victim: clear it first
            e.wr = Some((f.line, e.key(f.line), f.dom, false, false)); f.cleared = true; f.due = e.cyc + 1 + r.nextInt(40)
          } else { e.wr = Some((f.line, e.fresh(), f.dom, true, true)); fl -= f }
        case None =>
          if (!e.clr && r.nextDouble() < 0.04) {                            // L2 probe of a random valid line
            val vl = (0 until e.n).filter(e.v(_))
            if (vl.nonEmpty) { val l = vl(r.nextInt(vl.size)); e.wr = Some((l, e.key(l), e.dom(l), false, false)); nProbe += 1 }
          }
      }
      val wasV = (0 until e.n).map(e.v(_)); val doms = e.rdm.clone()
      val (f, g, rb) = e.cycle()
      e.clr = false
      f.foreach { i =>
        nGrant += 1; if (rb) nRebal += 1; if (wasV(g)) nVictim += 1
        fl += new Fl(g, doms(i), i, e.cyc + 1 + r.nextInt(if (wasV(g)) 20 else 40), false)
      }
    }
    for (i <- 0 until nr) e.rv(i) = false
    check(nGrant >= 100 && nClear >= 2 && nProbe >= 50, s"$tag: coverage: $nGrant grants, $nClear clearAll phases, $nProbe probes")
    check(e.maxOcc._2 == hl, s"$tag: coverage: domain 1 never reached its hard limit $hl (max ${e.maxOcc})")
    check(e.searchGrants >= 20 && e.crossWait >= 20,
          s"$tag: coverage (F4): ${e.searchGrants} search grants, ${e.crossWait} cycles with an other-domain requester waiting on a search")
    info(s"$tag: $cycles cycles, $nr requesters, $nClear clearAll phases, $nProbe probes, $nGrant grants ($nVictim of a valid " +
         s"victim, $nRebal rebalancing, ${e.searchGrants} by a search), ${e.crossWait} cycles with an other-domain requester " +
         s"waiting on a search, max turns waited ${e.maxTurns} (bound ${nr - 1}), max wait ${e.maxWait} cycles, " +
         s"max true occupancy ${e.maxOcc}")
  }

  // ================================================================== main
  def main(args: Array[String]): Unit = {
    val seed = args.collectFirst { case a if a.startsWith("seed=") => a.drop(5).toLong }.getOrElse(1L)
    val only = args.filterNot(_.startsWith("seed=")).toSet
    def section(name: String)(body: => Unit): Unit = if (only.isEmpty || only(name)) {
      println(s"[CEVBENCH] BEGIN $name"); val f0 = fails
      try body catch { case t: Throwable => fails += 1; println(s"[CEVBENCH] FAIL $name: exception $t"); t.printStackTrace() }
      println(s"[CEVBENCH] END $name fails=${fails - f0}")
    }
    info(s"seed=$seed")

    section("crf-d-small") {
      simulate(new CevCRF(16, 8, 2, isI = false, withSec = false, expireCycles = 8)) { dut =>
        resetDut(dut)
        val b = new CrfBench(dut, new CrfModel(16, false, false, 8), "crf-d-small", fullScan = true)
        crfDirectedD(b)
        b.random(4000, new Random(seed), (0 until 24).map(BigInt(_)))
      }
    }
    section("crf-i-small") {
      simulate(new CevCRF(16, 8, 1, isI = true, withSec = true, expireCycles = 8)) { dut =>
        resetDut(dut)
        val b = new CrfBench(dut, new CrfModel(16, true, true, 8), "crf-i-small", fullScan = true)
        crfDirectedI(b)
        b.random(4000, new Random(seed + 1), (0 until 24).map(BigInt(_)))
      }
    }
    section("crf-d-full") {                                    // plan D1 geometry: 1024 lines, 26-bit key, 2 lanes, 128 cycles
      simulate(new CevCRF(1024, 26, 2, isI = false, withSec = false, expireCycles = 128)) { dut =>
        resetDut(dut)
        val r = new Random(seed + 2)
        val b = new CrfBench(dut, new CrfModel(1024, false, false, 128), "crf-d-full", fullScan = false)
        b.random(600, r, Vector.fill(96)(BigInt(26, r)))
        crfFill(b)
      }
    }
    section("crf-i-full") {                                    // plan I1 geometry: 512 lines, 26-bit key, 1 lane, 64 cycles
      simulate(new CevCRF(512, 26, 1, isI = true, withSec = true, expireCycles = 64)) { dut =>
        resetDut(dut)
        val r = new Random(seed + 3)
        val b = new CrfBench(dut, new CrfModel(512, true, true, 64), "crf-i-full", fullScan = false)
        b.random(600, r, Vector.fill(96)(BigInt(26, r)))
        crfFill(b)
      }
    }
    section("vsu-small") {
      simulate(new CevVSU(16, 12, 8, 200, nReq = 4)) { dut =>
        resetDut(dut)
        val e = new VEnv(dut, new VArr(16), 200, 12, 8)
        vsuDirected(e, "vsu-small")
        vsuRandom(e, new Random(seed + 4), 300, "vsu-small rnd")
      }
    }
    section("vsu-lock") {                                      // owner F4: grant only to the latched requester (id, domain)
      simulate(new CevVSU(16, 12, 8, 200, nReq = 4)) { dut =>
        resetDut(dut)
        vsuLock(new VEnv(dut, new VArr(16), 200, 12, 8), "vsu-lock")
      }
    }
    section("vsu-d-full") {                                    // plan D1: 1024 lines, HL 768, SL 512, interval 100,000, 8 MSHRs
      simulate(new CevVSU(1024, 1024 * 3 / 4, 1024 / 2, nReq = 8)) { dut =>
        resetDut(dut)
        val a = new VArr(1024)
        val e = new VEnv(dut, a, 100000, 768, 512)
        vsuRandom(e, new Random(seed + 5), 40, "vsu-d-full rnd")
        // the paper's 100,000-cycle interval with the 17-bit counter
        for (i <- 0 until 1024) { a.v(i) = true; a.dom(i) = i >= 100; a.freq(i) = i % 16; a.rsv(i) = false }
        e.poolEmpty = true; e.occ(0) = 100; e.occ(1) = 924; e.en = true
        e.idle(99000)
        check(vsuEpisode(e, false, "vsu-d-full rebal-early").exists(x => !x._2), "vsu-d-full: no rebalancing before 100,000 cycles")
        e.idle(e.interval - e.cnt)
        check(e.cnt == 100000, s"vsu-d-full: model counter ${e.cnt}")
        val x = vsuEpisode(e, false, "vsu-d-full rebal-due")
        check(x.exists(y => y._2 && a.dom(y._1)), s"vsu-d-full: rebalancing at 100,000 cycles: $x")
      }
    }
    section("vsu-i-full") {                                    // plan I1: 512 lines, HL 384, SL 256
      simulate(new CevVSU(512, 512 * 3 / 4, 512 / 2)) { dut =>
        resetDut(dut)
        vsuRandom(new VEnv(dut, new VArr(512), 100000, 384, 256), new Random(seed + 6), 40, "vsu-i-full rnd")
      }
    }
    section("pair-small") {
      simulate(new CevPairTB(16, 8, 12, 8, 60, 8, 4)) { dut =>
        resetDut(dut)
        pairScenario(new PEnv(dut, "pair-small", Some(12)), 12, 8, 60, "pair-small")
      }
    }
    section("pair-inflight") {
      simulate(new CevPairTB(16, 8, 12, 8, 60, 8, 4)) { dut =>
        resetDut(dut)
        pairInflight(new PEnv(dut, "pair-inflight", Some(12)), 12, "pair-inflight")
      }
    }
    section("pair-lock") {                                     // owner F4 with the amended M3 pick
      simulate(new CevPairTB(16, 8, 12, 8, 60, 8, 4)) { dut =>
        resetDut(dut)
        pairLock(new PEnv(dut, "pair-lock", Some(12)), 12, "pair-lock")
      }
    }
    section("pair-upg") {                                      // [R1c B1] upgrade hold / release with the VSU and the busy output
      simulate(new CevPairTB(16, 8, 12, 8, 60, 8, 4)) { dut =>
        resetDut(dut)
        pairUpg(new PEnv(dut, "pair-upg", Some(12)), 12, "pair-upg")
      }
    }
    section("pair-random") {
      simulate(new CevPairTB(16, 8, 12, 8, 60, 8, 4)) { dut =>
        resetDut(dut)
        pairRandom(new PEnv(dut, "pair-random", Some(12)), 12, 6000, new Random(seed + 7), "pair-random")
      }
    }

    println(s"[CEVBENCH] RESULT checks=$checks fails=$fails")
    if (fails != 0) throw new RuntimeException(s"CevBench: $fails failed checks")
  }
}
