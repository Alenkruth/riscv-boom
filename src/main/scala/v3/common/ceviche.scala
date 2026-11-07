// Ceviche capability register file (CevCRF) and victim search unit (CevVSU): ceviche-plan-v5.md blocks N1 and N2 as
// amended by ceviche-plan-v5-amend-T1c.md (owner 2026-10-31: F1 "Count reserved lines", F4 "Lock to the requester",
// F5 "Apply in T1c"; task T1c v4 = v3 + R1c review B1 rsvHold/rsvRel/busy). Elaborated only under ENABLE_CEVICHE (D1, I1 instantiate them); unit bench:
// src/test/scala/v3/common/ceviche-bench.scala.
package boom.v3.common
import chisel3._
import chisel3.util._

object CevKind extends Enumeration { val D, I = Value }

// [B1] The CRF key store is a key-sliced BRAM CAM (REPORT ceviche-bram-cam-20261105 section 3.1; owner order 2026-11-05).
// One CevCamRam is a RAMB36 used as 512 rows x 64 bits (read port) with a 1-bit write port: bit (row, c) = 1 when line
// (group*64 + c) holds a key whose slice value is `row`.  A search reads row slice_s(key) of every slice RAM; the match of a line is
// AND over the slices.  Verilog: src/main/resources/vsrc/CevCamRam.v (read-first, synchronous read, no output register).
class CevCamRam extends BlackBox with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clock  = Input(Clock())
    val rdAddr = Input(UInt(9.W))     // row (slice value), presented in s0
    val rdData = Output(UInt(64.W))   // the row, one cycle later (s1); old data when the same bit is written in the read cycle
    val wrEn   = Input(Bool())
    val wrAddr = Input(UInt(15.W))    // {row, line column}: bit address of the 1-bit write port
    val wrData = Input(Bool())
  })
  addResource("/vsrc/CevCamRam.v")
}

// One entry per cache line; entry index = line number (paper 4: registers = lines, 1:1; ambiguity 1.4-4).
//
// [B1] Search timing is unchanged: the match of lane p is ready in the caller's s1, as the flop CAM gave it.  The BRAM read address
// is the s0 key (`s0Key`, the signal the caller registers into `sKey`), so the row arrives in s1.  Reg semantics are kept exactly:
// a write at cycle t is seen by the searches of t+1 on.  The BRAM write port is slower (erase at E, set at E+1, readable by a
// search from S+2), so a key-changing write is covered by `pend` (masks the line's stale BRAM bits from t+1) and by the entry queue
// (compares the new key in s1, supplies line / dom / rsv).  Proofs: task-reports/ceviche-bram-cam-20261105/b1/NOTE.md.
//   D$  : key changes only at the allocating final write (R1c B4 (i)): that write enqueues {line, key}.
//   I$  : key changes only at the first refill beat (non-allocating write, v = 0); the refill_done write (alloc) repeats the key.
//         v = 0 until done, done >= first beat + 3 cycles, the set lands in time: no pend, no bypass (asserted).
// N1: the FREQ increment arrives as {lane enable, s2 line}; the one-hot vectors are decoded here (no nLines-wide bus at the boundary).
class CevCRF(val nLines: Int, val keyBits: Int, val nSearch: Int, val isI: Boolean, val withSec: Boolean,
             val expireCycles: Int, val withTaint: Boolean = false, val withIftDom: Boolean = false,
             val dbgChecks: Boolean = false) extends Module {
  val lineBits = log2Ceil(nLines)
  val io = IO(new Bundle {
    // ---- search (combinational within the caller's s1; caller registers the results) ----
    val sKey   = Input(Vec(nSearch, UInt(keyBits.W)))
    val s0Key  = Input(Vec(nSearch, UInt(keyBits.W)))   // [B1] the key of the cycle before: sKey == RegNext(s0Key) (asserted when dbgChecks)
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
    val sRsv   = Output(Vec(nSearch, Bool()))           // the matched line is reserved as an in-flight victim
    // ---- FREQ [N1] ----
    // Per lane: the access increments `incLine` when `incEn` (the caller's registered s1 hit, gated by s2 validity); the CRF decodes
    // the one-hot vectors (formerly the caller's registered sMatch ORed over the lanes).  incEn needs a hit, so incLine is the matched line.
    val incEn   = Input(Vec(nSearch, Bool()))
    val incLine = Input(Vec(nSearch, UInt(lineBits.W)))
    // [T-IFT v4, B1] IFT replacement taint (D$ + IFT builds only, withTaint): per lane, the increments whose access is secret- / attacker-
    // attributed (incSec / incAtk imply incEn).
    val incSec = if (withTaint) Some(Input(Vec(nSearch, Bool()))) else None
    val incAtk = if (withTaint) Some(Input(Vec(nSearch, Bool()))) else None
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
  // [B1] The key lives in the BRAM CAM (a bit per line and slice row) and in keyCopy, which supplies the old key to the erase.  keyCopy is
  // written at the set cycle only, so it always names the key whose bits the BRAM holds (or will hold after the set in flight).
  val keyCopy = Mem(nLines, UInt(keyBits.W))                // combinational read, 1 write port (set) + 1 read port (erase) [+ 1 debug read]
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
  // excluded by construction (G3) and asserted (dbgChecks).
  // Kept in flops (each has a parallel reader or more than one writer): v (CAM / free pool / occ / bulk clears), dom (I$ CAM,
  // occ, clearDom), freq (per-line increment + tick every cycle), rsv (free pool, occ, clearAll), rsvDom (occ popcount; 2 writers
  // rsvHold + rsvSet), s (I$ CAM), inv (clearDom), tS / tA (written by the parallel increment AND by wr.alloc in the same cycle).
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

  // ---- [B1] BRAM CAM geometry: slices of 9 key bits (9/9/8 for 26 bits), 64 lines per RAMB36 ----
  val sliceBits = 9
  val nSlices   = (keyBits + sliceBits - 1) / sliceBits
  val grpLines  = math.min(nLines, 64)
  val nGroups   = (nLines + 63) / 64
  val colBits   = log2Ceil(grpLines)
  def sliceOf(k: UInt, sl: Int): UInt = {
    val lo = sl * sliceBits; val hi = math.min(keyBits, lo + sliceBits) - 1
    k(hi, lo).pad(sliceBits)
  }
  // ram(p)(slice)(group): one copy per search lane (a RAMB36 has one read port), both written with the same address and data
  val ram = Seq.tabulate(nSearch, nSlices, nGroups)((_, _, _) => Module(new CevCamRam))
  for (p <- 0 until nSearch; sl <- 0 until nSlices; g <- 0 until nGroups) {
    ram(p)(sl)(g).io.clock  := clock
    ram(p)(sl)(g).io.rdAddr := sliceOf(io.s0Key(p), sl)      // s0 address: the row is read in s1
  }
  // the nLines-wide per-slice rows ANDed: key match of every line (v and pend are applied by the caller)
  def camRaw(p: Int): UInt = (0 until nSlices).map { sl =>
    Cat((0 until nGroups).reverse.map(g => ram(p)(sl)(g).io.rdData(grpLines - 1, 0)))
  }.reduce(_ & _)

  // ---- [B1] write queue and engine: erase (E), set (E+1) for a key-changing write, in order ----
  val qDepth = if (isI) 2 else 8                     // D$: nMSHRs = 8 (bound proof: NOTE.md); I$: one refill at a time
  val ST_FREE = 0.U(2.W); val ST_WAIT = 1.U(2.W); val ST_ERASED = 2.U(2.W); val ST_DONE = 3.U(2.W)
  val qSt   = RegInit(VecInit(Seq.fill(qDepth)(ST_FREE)))   // DONE = the set was issued last cycle: alive one more cycle (bypass / pend)
  val qLine = Reg(Vec(qDepth, UInt(lineBits.W)))
  val qKey  = Reg(Vec(qDepth, UInt(keyBits.W)))
  val qSup  = Reg(Vec(qDepth, Bool()))               // D$: a younger entry of the same line exists (youngest wins)
  val qV    = Reg(Vec(qDepth, Bool()))               // D$: copies of v / dom / rsv of the entry's line, kept equal to the line's flops
  val qDom  = Reg(Vec(qDepth, Bool()))
  val qRsv  = Reg(Vec(qDepth, Bool()))
  val ptrW  = log2Ceil(qDepth)
  val eh    = RegInit(0.U(ptrW.W))                   // oldest entry whose set is not issued yet
  val qt    = RegInit(0.U(ptrW.W))                   // next free slot
  // D$: the allocating write changes the key; I$: the first-beat write (non-allocating) does; the refill_done write repeats it
  val enq = io.wr.valid && (if (isI) !io.wr.bits.alloc else io.wr.bits.alloc)

  for (e <- 0 until qDepth) when (qSt(e) === ST_DONE) { qSt(e) := ST_FREE }
  val hSt      = qSt(eh)
  val engErase = hSt === ST_WAIT
  val engSet   = hSt === ST_ERASED
  val hLine    = qLine(eh)
  val engKey   = Mux(engSet, qKey(eh), keyCopy(hLine))   // erase: the key the BRAM holds for the line; set: the new key
  when (engErase) { qSt(eh) := ST_ERASED }
  when (engSet)   { qSt(eh) := ST_DONE; eh := eh + 1.U; keyCopy.write(hLine, qKey(eh)) }
  for (p <- 0 until nSearch; sl <- 0 until nSlices; g <- 0 until nGroups) {
    val r = ram(p)(sl)(g).io
    r.wrEn   := (engErase || engSet) && (if (nGroups == 1) true.B else hLine(lineBits - 1, colBits) === g.U)
    r.wrAddr := Cat(sliceOf(engKey, sl), hLine(colBits - 1, 0).pad(6))   // {row, column}: column padded to the 6-bit line slot of a RAMB36
    r.wrData := engSet
  }
  // enqueue (the entry is readable from the next cycle: the engine starts at t+1, pend / bypass cover t+1 on)
  assert(!(enq && qSt(qt) =/= ST_FREE && qSt(qt) =/= ST_DONE), "Ceviche CRF: write queue overflow")
  when (enq) {
    qSt(qt) := ST_WAIT; qLine(qt) := io.wr.bits.line; qKey(qt) := io.wr.bits.key
    qt := qt + 1.U
  }
  if (!isI) {
    // youngest wins: the older entries of the same line stop answering the bypass
    for (e <- 0 until qDepth) when (enq && qSt(e) =/= ST_FREE && qLine(e) === io.wr.bits.line) { qSup(e) := true.B }
    when (enq) { qSup(qt) := false.B }                  // after the loop: the new entry is the youngest (its slot may be a DONE entry of the same line)
    // copies: the same events, in the same priority, as the flops v / rsv (write, rel, hold, set, clearAll last); dom only at the alloc
    for (e <- 0 until qDepth) {
      when (io.wr.valid && !io.wr.bits.alloc && io.wr.bits.line === qLine(e)) { qV(e) := io.wr.bits.v }
      when (io.rsvRel.valid  && io.rsvRel.bits  === qLine(e)) { qRsv(e) := false.B }
      when (io.rsvHold.valid && io.rsvHold.bits === qLine(e)) { qRsv(e) := true.B }
      when (io.rsvSet.valid  && io.rsvSet.bits  === qLine(e)) { qRsv(e) := true.B }
      when (io.clearAll) { qV(e) := false.B; qRsv(e) := false.B }
    }
    when (enq) {   // the new entry starts from what the alloc leaves in the flops (v := wr.v, dom, rsv := 0), then the same-cycle events
      val l = io.wr.bits.line
      qV(qt)   := io.wr.bits.v && !io.clearAll
      qDom(qt) := io.wr.bits.dom
      qRsv(qt) := !io.clearAll && ((io.rsvHold.valid && io.rsvHold.bits === l) || (io.rsvSet.valid && io.rsvSet.bits === l))
    }
  }
  // pend: the line has an entry in the queue (its BRAM bits are stale or not readable yet): masked from the BRAM match
  val pend = if (!isI) Some(RegInit(VecInit(Seq.fill(nLines)(false.B)))) else None
  pend.foreach { pd =>
    // [B1 review] at most one entry is DONE in a cycle (one set per cycle; DONE lasts one cycle), so the clear is one port with one
    // nLines decoder (was one dynamic-index clear per entry: qDepth decoders ORed into every pend bit).  Same function: for the DONE
    // entry, "another live entry of its line" is a live entry that is not DONE.
    val isDone = VecInit(qSt.map(_ === ST_DONE))
    val dnLine = Mux1H(isDone, qLine)
    val others = (0 until qDepth).map(e => !isDone(e) && qSt(e) =/= ST_FREE && qLine(e) === dnLine).reduce(_ || _)
    when (isDone.asUInt.orR && !others && !(enq && io.wr.bits.line === dnLine)) { pd(dnLine) := false.B }
    when (enq) { pd(io.wr.bits.line) := true.B }
    if (dbgChecks) assert(PopCount(isDone) <= 1.U, "Ceviche CRF: more than one write queue entry DONE")
  }
  // I$: nothing may be pending when the done write comes (set issued at least a cycle before it) and the key is the first beat's
  if (isI) assert(!(io.wr.valid && io.wr.bits.alloc && (qSt.map(x => x === ST_WAIT || x === ST_ERASED).reduce(_ || _))),
                  "Ceviche I$ CRF: refill_done before the CAM set of the first beat")
  if (dbgChecks) {
    val kc   = keyCopy(io.wr.bits.line)
    if (isI) assert(!(io.wr.valid && io.wr.bits.alloc && kc =/= io.wr.bits.key), "Ceviche I$ CRF: refill_done key differs from the first beat's")
    else {
      val newest = (0 until qDepth).map(e => qSt(e) =/= ST_FREE && !qSup(e) && qLine(e) === io.wr.bits.line)
      val expKey = Mux(newest.reduce(_ || _), Mux1H(newest, qKey), kc)
      assert(!(io.wr.valid && !io.wr.bits.alloc && expKey =/= io.wr.bits.key), "Ceviche D$ CRF: a non-allocating write changes the key")
    }
    for (p <- 0 until nSearch) assert(RegNext(io.s0Key(p)) === io.sKey(p), "Ceviche CRF: sKey is not the registered s0Key")
  }

  // ---- search: own match and (I$) shared match; one-hot encode with OR trees ----
  for (p <- 0 until nSearch) {
    val raw    = camRaw(p)
    val tagEq0 = v.asUInt & raw
    val tagEq  = pend.map(pd => tagEq0 & ~pd.asUInt).getOrElse(tagEq0)
    val domVec = dom.asUInt
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
    // D$ bypass: an entry of the queue answers for its line (pend masked the BRAM bits of that line)
    val hitE = if (!isI) (0 until qDepth).map(e => qSt(e) =/= ST_FREE && !qSup(e) && qV(e) && qKey(e) === io.sKey(p))
               else Seq.empty[Bool]
    val bypHit = if (!isI) hitE.reduce(_ || _) else false.B
    val hit  = m.orR || bypHit
    val line = if (!isI) Mux(bypHit, Mux1H(hitE, qLine), OHToUInt(m)) else OHToUInt(m)   // OR-tree encoder (exactly one bit set on a hit)
    io.sHit(p)    := hit
    io.sLine(p)   := line
    // [TF-LUTRAM] index reads at the encoded match (one LUTRAM read port per search port), gated by the hit = the old Mux1H value.
    io.sCoh(p)    := (if (!isI) Mux(hit, coh.get(line), 0.U) else 0.U)
    io.sDomOut(p) := (if (!isI) Mux1H(m, dom) || Mux1H(hitE, qDom) else Mux1H(m, dom))
    io.sSec(p)    := (if (withSec) hit && sec.get(line) else false.B)
    io.sIftDom.foreach(_(p) := hit && idom.get(line))   // [T-IFT v5] same index read as sSec [TF-LUTRAM]
    io.sRsv(p)    := (if (!isI) Mux1H(m, rsv) || Mux1H(hitE, qRsv) else Mux1H(m, rsv))   // the matched line is a granted victim (D3b nack)
    // D13: at most one match (G3); an entry hit and a BRAM hit never coexist (pend masks the entry's line, keys are unique among valid lines)
    if (dbgChecks) assert(PopCount(Cat(m, if (!isI) Cat(hitE) else 0.U(1.W))) <= 1.U, "Ceviche CRF: multiple CRF matches")
  }

  // ---- [N1] FREQ increment vectors, decoded from {incEn, incLine} per lane ----
  def decode(en: Seq[Bool]): UInt =
    (0 until nSearch).map(w => Mux(en(w), UIntToOH(io.incLine(w), nLines), 0.U(nLines.W))).reduce(_ | _)
  val incVec    = decode(io.incEn)
  val incSecVec = io.incSec.map(x => decode(x))
  val incAtkVec = io.incAtk.map(x => decode(x))

  // ---- expiration tick and FREQ (paper 5.2): sat +1 on access, sat -1 at each interval, 5 on allocation ----
  val tickCnt = RegInit(0.U(log2Ceil(expireCycles).W)); tickCnt := tickCnt + 1.U
  val tick    = tickCnt === (expireCycles - 1).U
  for (i <- 0 until nLines) {
    val inc  = incVec(i)
    val up   = Mux(inc && freq(i) =/= 15.U, freq(i) + 1.U, freq(i))
    val down = Mux(tick && up =/= 0.U, up - 1.U, up)
    freq(i) := down
    when (io.wr.valid && io.wr.bits.alloc && io.wr.bits.line === i.U) { freq(i) := 5.U }   // last connect wins
  }
  if (withTaint) {   // [T-IFT v4, B1] one update LUT per line beside the FREQ update, enabled by the same per-line increment bit
    for (i <- 0 until nLines) when (incVec(i)) { tS.get(i) := incSecVec.get(i); tA.get(i) := incAtkVec.get(i) }
  }

  // ---- write mirror ----
  when (io.wr.valid) {
    val l = io.wr.bits.line
    v(l) := io.wr.bits.v                                 // [B1] the key goes through the write queue (enq) to the BRAM CAM and keyCopy
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
  if (!isI) assert(!io.clearDom.valid, "Ceviche D$ CRF: clearDom is not mirrored in the write queue copies")

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

// Paper 5.2 + Fig. 4 with the Section 5.2 text reading of rebalancing (plan 1.4-1/2/3).
class CevVSU(val nLines: Int, val hardDefault: Int, val softDefault: Int, val rebalInterval: Int = 100000,
             val nReq: Int = 1, val withTaint: Boolean = false) extends Module {
  val lineBits = log2Ceil(nLines)
  val cntBits  = lineBits + 1
  val reqIdW   = log2Ceil(nReq) max 1                   // requester id width (D$: nMSHRs, M3; I$: one refill)
  val io = IO(new Bundle {
    val req     = Flipped(Decoupled(Bool()))           // bits = requesting domain; fire = grant
    val grant   = Output(UInt(lineBits.W))              // valid in the req.fire cycle
    val rebal   = Output(Bool())                         // the grant is a cross-domain rebalancing eviction (debug)
    val hardCsr = Input(UInt(cntBits.W)); val softCsr = Input(UInt(cntBits.W))
    val occ     = Input(Vec(2, UInt(cntBits.W)))
    val poolEmpty = Input(Bool()); val freeIdx = Input(UInt(lineBits.W))
    val cIdx = Output(UInt(lineBits.W))
    val cV = Input(Bool()); val cDom = Input(Bool()); val cFreq = Input(UInt(4.W)); val cRsv = Input(Bool())
    // [T-IFT v4, B1] replacement taint of the evaluated candidates (CRF cTS/cTA) and its OR at a search grant (gS/gA, valid in the
    // req.fire cycle; 0 for a free-pool grant, which chooses nothing).  D$ + IFT builds only (withTaint).
    val cTS = if (withTaint) Some(Input(Bool())) else None; val cTA = if (withTaint) Some(Input(Bool())) else None
    val gS  = if (withTaint) Some(Output(Bool())) else None; val gA  = if (withTaint) Some(Output(Bool())) else None
    val en   = Input(Bool())                             // cev_on: rebalancing counter cleared while off
    val searching = Output(Bool())                       // debug
    val busy      = Output(Bool())                       // [R1c B1] a search runs or a grant was made in the last 2 cycles (D$ nacks upgrades meanwhile)
    // [T1c v3, owner F4] requester lock: the id of the presented requester (D$: the M3 pick; I$: 0); while a search runs
    // only the requester latched at its start (id and domain) can be granted; roundEnd = the search ends this cycle.
    val reqId    = Input(UInt(reqIdW.W))
    val lock     = Output(Valid(UInt(reqIdW.W)))
    val roundEnd = Output(Bool())
  })
  val HL = Mux(io.hardCsr === 0.U, hardDefault.U, io.hardCsr)
  val SL = Mux(io.softCsr === 0.U, softDefault.U, io.softCsr)
  val rd = io.req.bits; val od = !rd
  val rebCnt = RegInit(0.U(17.W))
  val rebOk  = rebCnt >= rebalInterval.U
  when (!io.en) { rebCnt := 0.U } .elsewhen (!rebOk) { rebCnt := rebCnt + 1.U }

  // occ is 2 cycles behind the CRF state (N1 popcount): no decision in the 2 cycles after a grant, so a grant is always
  // counted (as a reserved line) before the next takeFree / wantX test. grantGap = a grant in either of the last 2 cycles.
  val fire1    = RegNext(io.req.fire, false.B)
  val grantGap = RegNext(io.req.fire || fire1, false.B)

  val lfsr = random.LFSR(16, true.B)
  val sIdle :: sScan :: sDone :: Nil = Enum(3)
  val st     = RegInit(sIdle)
  val k      = Reg(UInt(4.W))                          // candidates issued (0..8)
  val tgtDom = Reg(Bool()); val cross = Reg(Bool())
  val bestV  = Reg(Bool()); val bestF = Reg(UInt(4.W)); val bestI = Reg(UInt(lineBits.W))
  val candQ  = Reg(UInt(lineBits.W)); val candQv = RegInit(false.B)   // one-cycle pipeline: index -> CRF fields
  val idQ    = Reg(UInt(reqIdW.W)); val rdQ = Reg(Bool())                // [F4] requester (id, domain) of this search
  // [T-IFT v4, B1] OR of the replacement taint over the candidates this search evaluated (target domain, valid, unreserved): the victim
  // choice depended on the FREQ of every one of them.  Cleared at search start; the +3 FF of the VSU are accS / accA.
  val accS = if (withTaint) Some(RegInit(false.B)) else None
  val accA = if (withTaint) Some(RegInit(false.B)) else None

  // decision (Fig. 4 top): free line and below hard limit -> take it; else search
  val takeFree = !io.poolEmpty && io.occ(rd) < HL
  val wantX    = io.poolEmpty && io.occ(rd) < SL && io.occ(od) > SL && rebOk     // text reading (1.4-1)

  io.req.ready := false.B; io.grant := io.freeIdx; io.rebal := false.B
  io.cIdx := candQ
  io.searching := st =/= sIdle
  io.busy := st =/= sIdle || grantGap                  // [R1c B1] 2 flops + state decode
  io.lock.valid := st =/= sIdle; io.lock.bits := idQ; io.roundEnd := st === sDone
  if (withTaint) { io.gS.get := accS.get && st === sDone; io.gA.get := accA.get && st === sDone }   // [T-IFT v4, B1] 1 LUT from flops

  switch (st) {
    is (sIdle) {
      when (io.req.valid && !grantGap) {               // the requester waits (holds req.valid) while grantGap
        when (takeFree) { io.req.ready := true.B; io.grant := io.freeIdx }
        .otherwise { st := sScan; k := 0.U; bestV := false.B; tgtDom := Mux(wantX, od, rd); cross := wantX; candQv := false.B
                     idQ := io.reqId; rdQ := rd
                     if (withTaint) { accS.get := false.B; accA.get := false.B } }
      }
    }
    is (sScan) {
      // issue one random candidate per cycle (8 total); evaluate the previous one
      when (k < 8.U) { candQ := lfsr(lineBits - 1, 0); candQv := true.B; k := k + 1.U } .otherwise { candQv := false.B }
      when (candQv) {
        val ok = io.cV && !io.cRsv && io.cDom === tgtDom
        when (ok && (!bestV || io.cFreq < bestF)) { bestV := true.B; bestF := io.cFreq; bestI := candQ }   // first on ties
        if (withTaint) when (ok) { accS.get := accS.get || io.cTS.get; accA.get := accA.get || io.cTA.get }   // [T-IFT v4, B1]
      }
      when (k === 8.U && !candQv) { st := sDone }
    }
    is (sDone) {
      // [F4] grant only to the requester the search was made for (same id, same domain); anyone else waits for sIdle
      when (bestV && io.reqId === idQ && rd === rdQ) {
        io.req.ready := true.B; io.grant := bestI; io.rebal := cross
        when (cross && io.req.valid) { rebCnt := 0.U }
      }
      st := sIdle                         // the search ends here, granted or not (the lock lasts one bounded round)
    }                                     // Fig. 4: no entry of the target domain -> re-decide and select 8 again
  }
  // a requester is held (req.valid stays high) until granted; sIdle re-evaluates takeFree / wantX after a failed round
  // (D$: the M3 round-robin pick may present another MSHR first; it is frozen while `lock.valid`).
}
