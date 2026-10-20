//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// ROB IFT sidecar (IFT overhaul Phase 2, Task 2.1)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
// Per-bank LUTRAM columns replacing the ROB's per-row IFT flip-flop state.  See
// claude-artifacts/ift-overhaul/00-spec.md section 5.1 and decisions D3, D15,
// D17, D19.  Not instantiated until Task 2.2.

package boom.v3.exu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.util.CoreFuzzingConstants
import boom.v3.common._
import boom.v3.util.{Log2Bucket, SatDropped}

/** IO of one RobIftBank.  Factored out of the module so rob.scala can wire it under
  * ENABLE_IFT (Task 2.2): the bank is instantiated inside `if (ENABLE_IFT)` and its
  * outputs are tied to constants otherwise.  All writers are already bank-matched and
  * guarded by the ROB.
  */
class RobIftBankIO(numRows: Int, wbPorts: Int, clrPorts: Int, sprobPorts: Int,
                   icPorts: Int, memordPorts: Int, guardPorts: Int, lsuWbCount: Int)
                  (implicit p: Parameters) extends BoomBundle with CoreFuzzingConstants {
  val rowW = log2Ceil(numRows)
  val stamp     = Input(UInt(IftStamp.W.W))
  val clear_all = Input(Bool())
  // writers (all already bank-matched and guarded by the ROB)
  val enq    = Input(Valid(new IftRowEnq(rowW)))
  // Task 3.2 (D16): the thin writeback payload (row, the response's edge list, fu_bitmap)
  val wb     = Input(Vec(wbPorts,     Valid(new IftRowWb(rowW))))
  val clr    = Input(Vec(clrPorts,    Valid(new IftRowBitmap(rowW))))
  val sprob  = Input(Vec(sprobPorts,  Valid(new IftRowSprob(rowW))))   // spec 5.1.2 amendment: row + is_atk + is_secret
  val ic     = Input(Vec(icPorts,     Valid(new IftRowIc(rowW))))
  val memord = Input(Vec(memordPorts, Valid(new IftRowMemord(rowW))))
  val ras    = Input(Valid(UInt(rowW.W)))
  // op_count guard reads (async)
  val guard_row = Input(Vec(guardPorts, UInt(rowW.W)))
  val guard_oc  = Output(Vec(guardPorts, UInt(uopIDCounterWidthCF.W)))
  // commit readout (async): the merged record fields of the committing row
  val commit     = Input(new IftRowReq(rowW))
  val commit_out = Output(new IftCommitInfo)
  // [ROBHEADATK] patch 48 (r173 (6)): the committing row has a C_sprob column with is_atk (the
  // victim row's own RESULT attacker value taint, rob.scala sp_atk), before the newest-two merge
  val commit_valatk = Output(Bool())
  // D19: the commit readout dropped at least one same-type update beyond the merged ones;
  // the ROB qualifies it with the actual commit and counts it.
  val commit_third = Output(Bool())
  // D15 check: age of the committing row in cycles (stamp - alloc_stamp); asserted < 65535 by the ROB
  val commit_age = Output(UInt(IftStamp.W.W))
  // Task 2.10d (design 4b): the squash row leaves the bank raw (async read of every merge
  // input); IftSquashMerge merges it when the entry leaves its raw FIFO.
  val squash   = Input(new IftRowReq(rowW))                       // capture row (sq_row) and its domain
  val sq_raw   = Output(new IftRawRow(sprobPorts, icPorts, lsuWbCount, memordPorts))
  val sq_third = Output(Bool())                                   // D19 for the captured row
}

/** Task 2.10d (design 4b): spec 5.1.5 steps 1 to 3 and 5 to 7 as a pure function of one raw
  * row.  Moved verbatim from RobIftBank.readout (only its sources are renamed).  Shared by
  * every bank's commit readout and by IftSquashMerge, so both produce the same fields for
  * the same row.  Returns the record fields and the D19 pulse (`third =/= 0`). */
object IftMerge extends CoreFuzzingConstants {
  /** Result of newestTwo ([FIFO10]): the newest and second-newest valid columns of one type with
    * their relative keys, and how many columns of the type are valid. */
  class Pick2[T <: Data](val v1: Bool, val c1: T, val k1: UInt,
                         val v2: Bool, val c2: T, val k2: UInt, val count: UInt) {
    val third: Bool = count > 2.U
  }

  /** [FIFO10] step 10 (R-FIFO): pick the NEWEST and second-newest valid columns by relative
    * stamp (was oldestTwo, which kept the two oldest and counted newer ones as dropped).
    * c1/k1 = the newest, c2/k2 = the second newest; older columns count as dropped (third).
    * Pairwise order matrix: n(n-1)/2 comparators computed once (index breaks ties), per-candidate
    * rank = number of valid candidates that FOLLOW it, so the two selects are one-hot by
    * construction and no stage is serial in n. */
  def newestTwo[T <: Data](valids: Seq[Bool], cols: Seq[T], keys: Seq[UInt]): Pick2[T] = {
    val n = valids.length
    require(n >= 1 && cols.length == n && keys.length == n)
    val prec = Array.ofDim[Bool](n, n)          // prec(i)(j): i precedes j
    for (i <- 0 until n; j <- i + 1 until n) {
      val le = keys(i) <= keys(j)
      prec(i)(j) = le
      prec(j)(i) = !le
    }
    val rank = (0 until n).map { i =>
      val behind = (0 until n).filter(_ != i).map(j => valids(j) && prec(i)(j))   // j is newer than i
      if (behind.isEmpty) 0.U else PopCount(behind)
    }
    val sel1  = (0 until n).map(i => valids(i) && rank(i) === 0.U)
    val sel2  = (0 until n).map(i => valids(i) && rank(i) === 1.U)
    val count = PopCount(valids)
    new Pick2(count =/= 0.U, Mux1H(sel1, cols), Mux1H(sel1, keys),
              count > 1.U,   Mux1H(sel2, cols), Mux1H(sel2, keys), count)
  }

  class Grp(val valid: Bool, val key: UInt, val prio: Int, val ents: Seq[(Bool, InfluencerEntry)])
  class Ent(val valid: Bool, val e: InfluencerEntry, val g: Int, val q: Int)

  /** The stored entry of an ic or memord column.  valid is the stored value (always written
    * true); the merge overwrites every slot's valid with its hit bit anyway. */
  def ent(x: IftRawEdge)(implicit p: Parameters): InfluencerEntry = {
    val e = Wire(new InfluencerEntry)
    e.valid := true.B; e.op_count := x.op_count; e.infl_type := x.infl_type
    e.is_atk := x.is_atk; e.is_secret := x.is_secret
    e
  }

  /** @param rasNow [RASBYP] patch 40: the RAS edge of this row is being written THIS cycle
    *        (commit readout only; false for the squash merges).  It is the newest edge of the
    *        row, so it is valid and its key is forced to the maximum (see raK). */
  def apply(r: IftRawRow, rasNow: Bool = false.B)(implicit p: Parameters): (IftCommitInfo, Bool) = {
    val disp = r.disp
    def rel(s: UInt): UInt = s - disp.alloc_stamp        // mod 2^16 (D15)

    // per-type column selection (D19 rule; [FIFO10] the two NEWEST are merged, the older counted)
    val sp  = newestTwo(r.sp.map(_.valid), r.sp.map(_.bits), r.sp.map(c => rel(c.bits.stamp)))
    val ic  = newestTwo(r.ic.map(_.valid), r.ic.map(_.bits), r.ic.map(c => rel(c.bits.stamp)))
    val wb  = newestTwo(r.wb.map(_.valid), r.wb.map(_.bits), r.wb.map(c => rel(c.bits.stamp)))
    val mo  = newestTwo(r.mo.map(_.valid), r.mo.map(_.bits), r.mo.map(c => rel(c.bits.stamp)))
    // [RASBYP] patch 40: a same-cycle RAS write (rasNow) arrives after every stored column, so
    // its key is all-ones: commit_age < 65535 (D15, asserted in the ROB) bounds every stored
    // key below it.  The OR sits on the comparator's input LUT (+0 levels on the stamp path).
    val raV = r.ras.valid || rasNow
    val raK = rel(r.ras.bits.stamp) | Fill(IftStamp.W, rasNow)

    // Spec 5.1.2 amendment (Task 2.4, ruling 38/Task 2.3 review): the sprob entry's
    // is_atk / is_secret come from the row's sprob columns.  The two oldest valid columns
    // OR-merge into the ONE entry keyed on the NEWEST column's stamp (sp.k1, [FIFO10]): a row may hold
    // a secret-driven mark and the attacker twin (a victim consumer of an attacker-tainted
    // operand) on different ports, or two marks of a re-issued uop; the entry is the same
    // op_count-0 REG_DATAFLOW mark either way, only its bits differ.  `flat` keeps one
    // sprob entry; a third column counts as dropped like the other types (ruling 30).
    val sprobEntry = Wire(new InfluencerEntry)
    sprobEntry.valid := true.B; sprobEntry.op_count := 0.U; sprobEntry.infl_type := INFL_REG_DATAFLOW.U
    sprobEntry.is_atk    := (sp.v1 && sp.c1.is_atk)    || (sp.v2 && sp.c2.is_atk)
    sprobEntry.is_secret := (sp.v1 && sp.c1.is_secret) || (sp.v2 && sp.c2.is_secret)
    val rasEntry = Wire(new InfluencerEntry)
    rasEntry.valid := true.B; rasEntry.op_count := 0.U; rasEntry.infl_type := INFL_RAS_STATE.U
    rasEntry.is_atk := r.domain === 0.U; rasEntry.is_secret := false.B
    // group order on ties = today's drain priority: sprob, ic, wb, memord, ras
    // ([FIFO10] step 10: the Task 3.2 dtlb groups (prio 5) are removed, ruling 94)
    val groups = Seq(
      new Grp(true.B,      0.U(IftStamp.W.W), 0, disp.base_list.map(e => (e.valid, e))),
      new Grp(sp.v1,       sp.k1,             1, Seq((true.B, sprobEntry))),
      new Grp(ic.v1,       ic.k1,             2, Seq((true.B, ent(ic.c1)))),
      new Grp(ic.v2,       ic.k2,             2, Seq((true.B, ent(ic.c2)))),
      new Grp(wb.v1,       wb.k1,             3, wb.c1.edges.map(e => (e.valid, e))),
      new Grp(wb.v2,       wb.k2,             3, wb.c2.edges.map(e => (e.valid, e))),
      new Grp(mo.v1,       mo.k1,             4, Seq((true.B, ent(mo.c1)))),
      new Grp(mo.v2,       mo.k2,             4, Seq((true.B, ent(mo.c2)))),
      new Grp(raV,         raK,               6, Seq((true.B, rasEntry))))
    val G = groups.length
    // One comparator per unordered pair (a < b).  On equal keys the lower prio wins,
    // then the lower group index, so the order is total; before(b, a) is the complement.
    val cmp = Array.ofDim[Bool](G, G)
    for (a <- 0 until G; b <- a + 1 until G) {
      val ga = groups(a); val gb = groups(b)
      val tie: Boolean = ga.prio <= gb.prio
      cmp(a)(b) = if (tie) ga.key <= gb.key else ga.key < gb.key
    }
    def before(a: Int, b: Int): Bool = {          // does group a precede group b
      val ga = groups(a); val gb = groups(b)
      ga.valid && gb.valid && (if (a < b) cmp(a)(b) else !cmp(b)(a))
    }
    // flatten; rank(e) = number of valid entries that precede e (0..19), fixed 5 bits
    val flat: Seq[Ent] = groups.zipWithIndex.flatMap { case (grp, gi) =>
      grp.ents.zipWithIndex.map { case ((ev, e), q) => new Ent(grp.valid && ev, e, gi, q) } }
    def entBefore(f: Ent, e: Ent): Bool = if (f.g == e.g) (f.q < e.q).B else before(f.g, e.g)
    val rank: Seq[UInt] = flat.indices.map { i =>
      PopCount(flat.indices.filter(_ != i).map(j => flat(j).valid && entBefore(flat(j), flat(i)))) }
    val total    = PopCount(flat.map(_.valid))                          // <= 20, 5 bits
    val over     = total > numInfluencerSlotsCF.U
    val dropHere = Mux(over, total - numInfluencerSlotsCF.U, 0.U)       // entries older than the four newest
    val out = Wire(new IftCommitInfo)
    for (j <- 0 until numInfluencerSlotsCF) {
      val target = dropHere + j.U
      val hit = flat.indices.map(i => flat(i).valid && rank(i) === target)   // one-hot by construction
      out.influencer_list(j) := Mux1H(hit, flat.map(_.e))
      out.influencer_list(j).valid := hit.reduce(_ || _)
    }
    // D19: a third same-type update counts as dropped (spec 5.1.5 step 1).  ic/wb/memord
    // merge two columns as separate entries; sprob OR-merges its two into one entry (the
    // mark is the same op_count-0 REG_DATAFLOW edge), but like the others it counts only
    // from the third column (ruling 30).
    val third   = PopCount(Seq(sp.third, ic.third, wb.third, mo.third))
    // [FIFO10] + the contention winners the issue slots replaced (r.ic_dropped)
    out.infl_dropped := SatDropped(disp.base_dropped,
      ((Mux(wb.v1, wb.c1.dropped, 0.U) +& Mux(wb.v2, wb.c2.dropped, 0.U)) +& r.ic_dropped) +& (dropHere +& third))
    out.fu_bitmap       := disp.fu_bitmap                 // spec 5.1.5 step 4: ORed in rawRead
    out.infl_oc_aliased := disp.oc_aliased || (wb.v1 && wb.c1.aliased) || (wb.v2 && wb.c2.aliased)
    // the NEWEST contention column's bucket ([FIFO10]: ic.c1 is the newest)
    out.cntd_deny_count := Mux(ic.v1, ic.c1.deny_bucket.get, 0.U)
    // [FIFO10] step 10 (R-STICKY): OR of is_atk over EVERY edge that reached the row, stored or
    // dropped (was: over the four stored slots, so a full list suppressed the record's bit 71).
    // Read from the raw row, independent of the selection: an about 40-input OR, 3 LUT levels,
    // in parallel with the merge.
    out.edge_atk := (disp.base_list.map(e => e.valid && e.is_atk) ++
                     r.sp.map(c => c.valid && c.bits.is_atk) ++
                     r.ic.map(c => c.valid && c.bits.is_atk) ++
                     r.wb.flatMap(c => c.bits.edges.map(e => c.valid && e.valid && e.is_atk)) ++
                     r.mo.map(c => c.valid && c.bits.is_atk) ++
                     Seq(raV && r.domain === 0.U)).reduce(_ || _)
    out.speculated := disp.speculated;  out.single_step := disp.single_step
    out.spec_branch_is_atk := disp.spec_branch_is_atk; out.spec_branch_is_secret := disp.spec_branch_is_secret
    out.spec_branch_op_id := disp.spec_branch_op_id
    out.stall_cycles_rob := disp.stall_cycles_rob; out.stall_cycles_stq := disp.stall_cycles_stq
    out.atk_branch_ctr := disp.atk_branch_ctr;  out.sec_branch_ctr := disp.sec_branch_ctr
    (out, third =/= 0.U)
  }
}

/** Task 2.10d: the record fields from the merge plus the row's four sticky flags (was
  * rob.scala applyIft).  [THINUOP] Task 3.5: only the fields that remain on the MicroOp are
  * written; every other record field is read from IftCommitInfo by uopToIFTBits (bridge record)
  * and the printfs.  Used by the commit readout (rob.scala applyIft) only: the rename stage and
  * the commit printf read these retained fields from io.commit.uops. */
object IftApply {
  def apply(u: MicroOp, s_acc: Bool, s_prop: Bool, s_tx: Bool, atk: Bool, i: IftCommitInfo): Unit = {
    u.cf_attacker_influence.foreach(_ := atk || i.edge_atk)
    u.cf_secret_access.foreach(_ := s_acc)
    u.cf_secret_propagation.foreach(_ := s_prop)
    u.cf_secret_transmission.foreach(_ := s_tx)
    u.cf_fu_bitmap.foreach(_ := i.fu_bitmap)
    u.cf_spec_branch_is_secret.foreach(_ := i.spec_branch_is_secret)
    // cf_op_count_id, cf_domain_id and cf_ras_pred_xdom come from rob_uop (spec 5.1.1), or from
    // the squash side fields (IftSquashMerge).
  }
}

/** Per-bank IFT sidecar: LUTRAM columns, one writer per port (D17), arrival-stamped,
  * merged at readout in arrival order with the four newest surviving (D3).
  *
  * @param numRows      rows in this bank (64)
  * @param wbPorts      number of ROB writeback ports (9)
  * @param lsuWbPorts   indices of the writeback ports that carry influencer lists (0,1,6,7)
  * @param clrPorts     store clr_bsy ports (3)
  * @param sprobPorts   cf_s_prop_rob_upd ports (9)
  * @param icPorts      issue-contention ports (7)
  * @param memordPorts  MEM_ORDER ports (2)
  * @param guardPorts   number of op_count guard read ports the ROB needs (21)
  */
class RobIftBank(numRows: Int, wbPorts: Int, lsuWbPorts: Seq[Int], clrPorts: Int,
                 sprobPorts: Int, icPorts: Int, memordPorts: Int, guardPorts: Int)
                (implicit p: Parameters) extends BoomModule with CoreFuzzingConstants {
  require(lsuWbPorts.nonEmpty && lsuWbPorts.distinct.length == lsuWbPorts.length &&
          lsuWbPorts.forall(i => i >= 0 && i < wbPorts),
          "lsuWbPorts must be distinct writeback port indices")
  val rowW = log2Ceil(numRows)
  // NOTE: the IO bundle is the named class `RobIftBankIO(numRows, wbPorts, clrPorts,
  // sprobPorts, icPorts, memordPorts, guardPorts, lsuWbCount)` above so rob.scala can wire it
  // under ENABLE_IFT (Task 2.2).
  val io = IO(new RobIftBankIO(numRows, wbPorts, clrPorts, sprobPorts, icPorts, memordPorts, guardPorts,
                               lsuWbPorts.length))

  // Writeback ports: the LSU-fed ones own a C_wb column (edge list + fu_bitmap), the
  // others a thin C_fu column (fu_bitmap only), spec 5.1.2.  Both indexed by position.
  val fuPorts = (0 until wbPorts).filterNot(lsuWbPorts.contains)
  val lsuPos  = lsuWbPorts.zipWithIndex.toMap
  val fuPos   = fuPorts.zipWithIndex.toMap

  // ---------------------------------------------------------------- columns
  val c_disp   = Mem(numRows, new IftDispColumn)
  // Task 2.10c (spec 5.1.4): the op_count column is replicated so each copy has at most
  // ocReadsPerCopy asynchronous read ports (a RAM64M8 gives 7 read ports plus the
  // read/write port).  One Mem with all guardPorts reads does not infer distributed RAM
  // (Vivado built it as flip-flops).  Every copy takes the same write in the same cycle.
  val ocReadsPerCopy = 7
  val ocCopies = (guardPorts + ocReadsPerCopy - 1) / ocReadsPerCopy max 1
  val c_oc     = Seq.fill(ocCopies)(Mem(numRows, UInt(uopIDCounterWidthCF.W)))
  val c_wb     = lsuWbPorts.map(_ => Mem(numRows, new IftWbColumn))
  val c_fu     = fuPorts.map(_ => Mem(numRows, new IftBitmapColumn))
  val c_clr    = Seq.fill(clrPorts)(Mem(numRows, new IftBitmapColumn))
  val c_sprob  = Seq.fill(sprobPorts)(Mem(numRows, new IftSprobColumn))
  val c_ic     = Seq.fill(icPorts)(Mem(numRows, new IftEdgeColumn(hasDeny = true)))
  val c_memord = Seq.fill(memordPorts)(Mem(numRows, new IftEdgeColumn(hasDeny = false)))
  val c_ras    = Mem(numRows, new IftStampColumn)

  def validVec() = RegInit(VecInit(Seq.fill(numRows)(false.B)))
  val v_wb     = lsuWbPorts.map(_ => validVec())
  val v_fu     = fuPorts.map(_ => validVec())
  val v_clr    = Seq.fill(clrPorts)(validVec())
  val v_sprob  = Seq.fill(sprobPorts)(validVec())
  val v_ic     = Seq.fill(icPorts)(validVec())
  val v_memord = Seq.fill(memordPorts)(validVec())
  val v_ras    = validVec()
  val allValids: Seq[Vec[Bool]] = v_wb ++ v_fu ++ v_clr ++ v_sprob ++ v_ic ++ v_memord ++ Seq(v_ras)

  // ---------------------------------------------------------------- writes
  for (i <- 0 until wbPorts) {
    when (io.wb(i).valid) {
      val b   = io.wb(i).bits
      val row = b.row
      if (lsuWbPorts.contains(i)) {
        // Task 3.2 (D16): the edges come from the response, not from a uop list copy
        val col = Wire(new IftWbColumn)
        col.edges     := b.edges.edges
        col.dropped   := b.edges.dropped
        col.aliased   := b.edges.aliased
        col.fu_bitmap := b.fu_bitmap
        col.stamp     := io.stamp
        c_wb(lsuPos(i)).write(row, col)
        v_wb(lsuPos(i))(row) := true.B
      } else {
        val col = Wire(new IftBitmapColumn)
        col.fu_bitmap := b.fu_bitmap
        col.stamp     := io.stamp
        c_fu(fuPos(i)).write(row, col)
        v_fu(fuPos(i))(row) := true.B
      }
    }
  }
  for (i <- 0 until clrPorts) {
    when (io.clr(i).valid) {
      val col = Wire(new IftBitmapColumn)
      col.fu_bitmap := io.clr(i).bits.fu_bitmap
      col.stamp     := io.stamp
      c_clr(i).write(io.clr(i).bits.row, col)
      v_clr(i)(io.clr(i).bits.row) := true.B
    }
  }
  for (i <- 0 until sprobPorts) {
    when (io.sprob(i).valid) {
      val col = Wire(new IftSprobColumn)
      col.is_atk    := io.sprob(i).bits.is_atk
      col.is_secret := io.sprob(i).bits.is_secret
      col.stamp     := io.stamp
      c_sprob(i).write(io.sprob(i).bits.row, col)
      v_sprob(i)(io.sprob(i).bits.row) := true.B
    }
  }
  for (i <- 0 until icPorts) {
    when (io.ic(i).valid) {
      val col = Wire(new IftEdgeColumn(hasDeny = true))
      col.entry.valid     := true.B
      col.entry.op_count  := io.ic(i).bits.upd.winner_op_count.pad(inflOpCountWidthCF)(inflOpCountWidthCF-1, 0)
      col.entry.infl_type := io.ic(i).bits.upd.infl_type
      col.entry.is_atk    := io.ic(i).bits.upd.winner_is_atk
      col.entry.is_secret := io.ic(i).bits.upd.winner_is_sec
      col.deny_bucket.get := Log2Bucket(io.ic(i).bits.upd.deny_count)
      col.dropped.get     := io.ic(i).bits.upd.dropped     // [FIFO10]
      col.stamp           := io.stamp
      c_ic(i).write(io.ic(i).bits.row, col)
      v_ic(i)(io.ic(i).bits.row) := true.B
    }
  }
  for (i <- 0 until memordPorts) {
    when (io.memord(i).valid) {
      val col = Wire(new IftEdgeColumn(hasDeny = false))
      col.entry.valid     := true.B
      col.entry.op_count  := io.memord(i).bits.upd.prod_op_count
      col.entry.infl_type := INFL_MEM_ORDER.U
      col.entry.is_atk    := io.memord(i).bits.upd.is_atk
      col.entry.is_secret := io.memord(i).bits.upd.is_secret
      col.stamp           := io.stamp
      c_memord(i).write(io.memord(i).bits.row, col)
      v_memord(i)(io.memord(i).bits.row) := true.B
    }
  }
  when (io.ras.valid) {
    val col = Wire(new IftStampColumn); col.stamp := io.stamp
    c_ras.write(io.ras.bits, col)
    v_ras(io.ras.bits) := true.B
  }
  // Enqueue LAST so its valid-clear wins over any same-cycle stale write to the row.
  when (io.enq.valid) {
    // Task 3.3: from the thin enqueue payload (IftRowEnq), not a MicroOp.
    val e    = io.enq.bits
    val info = e.info
    val d = Wire(new IftDispColumn)
    d.speculated            := info.speculated
    d.single_step           := info.single_step
    d.spec_branch_is_atk    := info.spec_branch_is_atk
    d.spec_branch_is_secret := e.spec_branch_is_secret      // ruling 74: from the uop
    d.spec_branch_op_id     := info.spec_branch_op_id
    d.stall_cycles_rob      := info.stall_cycles_rob
    d.stall_cycles_stq      := info.stall_cycles_stq
    d.atk_branch_ctr        := info.atk_branch_ctr
    d.sec_branch_ctr        := info.sec_branch_ctr
    d.base_list             := info.base.edges
    d.base_dropped          := info.base.dropped
    d.oc_aliased            := info.base.aliased
    d.fu_bitmap             := e.fu_bitmap | (1.U << robTagCF.U)
    d.alloc_stamp           := io.stamp
    c_disp.write(io.enq.bits.row, d)
    c_oc.foreach(_.write(io.enq.bits.row, e.oc))
    allValids.foreach(v => v(io.enq.bits.row) := false.B)
  }
  when (io.clear_all) { allValids.foreach(v => v.foreach(_ := false.B)) }

  // ---------------------------------------------------------------- guards
  // guard k reads copy k / 7 (7/7/5 for 19 guards); the compare stays in the ROB.
  for (k <- 0 until guardPorts) { io.guard_oc(k) := c_oc(k / ocReadsPerCopy).read(io.guard_row(k)) }

  // ---------------------------------------------------------------- raw read (Task 2.10d)
  def toRaw(c: IftEdgeColumn, hasDeny: Boolean): IftRawEdge = {
    val x = Wire(new IftRawEdge(hasDeny))
    x.op_count := c.entry.op_count; x.infl_type := c.entry.infl_type
    x.is_atk   := c.entry.is_atk;   x.is_secret := c.entry.is_secret
    if (hasDeny) x.deny_bucket.get := c.deny_bucket.get
    x.stamp    := c.stamp
    x
  }

  /** One asynchronous read per column (as the old readout did), the valid bits, and the
    * fu_bitmap OR of spec 5.1.5 step 4.  Everything IftMerge reads, nothing else. */
  def rawRead(req: IftRowReq): IftRawRow = {
    val row     = req.row
    val r       = Wire(new IftRawRow(sprobPorts, icPorts, lsuWbPorts.length, memordPorts))
    val disp    = c_disp.read(row)
    val wbCols  = c_wb.map(_.read(row))
    val fuCols  = c_fu.map(_.read(row))
    val clrCols = c_clr.map(_.read(row))
    r.disp := disp
    r.disp.fu_bitmap := VecInit(Seq(disp.fu_bitmap) ++
      wbCols.zip(v_wb).map   { case (c, v) => Mux(v(row), c.fu_bitmap, 0.U(numModules.W)) } ++
      fuCols.zip(v_fu).map   { case (c, v) => Mux(v(row), c.fu_bitmap, 0.U(numModules.W)) } ++
      clrCols.zip(v_clr).map { case (c, v) => Mux(v(row), c.fu_bitmap, 0.U(numModules.W)) }).reduceTree(_ | _)
    for (k <- 0 until sprobPorts)  { r.sp(k).valid := v_sprob(k)(row); r.sp(k).bits := c_sprob(k).read(row) }
    val icCols = c_ic.map(_.read(row))
    for (k <- 0 until icPorts)     { r.ic(k).valid := v_ic(k)(row);    r.ic(k).bits := toRaw(icCols(k), hasDeny = true) }
    // [FIFO10] replaced contention winners of every valid ic column, saturating at 3
    val icDropSum = icCols.zip(v_ic).map { case (c, v) => Mux(v(row), c.dropped.get, 0.U(2.W)) }.reduce(_ +& _)
    r.ic_dropped := Mux(icDropSum > 3.U, 3.U, icDropSum(1, 0))
    for (k <- lsuWbPorts.indices)  {
      r.wb(k).valid        := v_wb(k)(row)
      r.wb(k).bits.edges   := wbCols(k).edges
      r.wb(k).bits.dropped := wbCols(k).dropped
      r.wb(k).bits.aliased := wbCols(k).aliased
      r.wb(k).bits.stamp   := wbCols(k).stamp
    }
    for (k <- 0 until memordPorts) { r.mo(k).valid := v_memord(k)(row); r.mo(k).bits := toRaw(c_memord(k).read(row), hasDeny = false) }
    r.ras.valid := v_ras(row)
    r.ras.bits  := c_ras.read(row)
    r.domain    := req.domain
    r
  }

  /** D19 for a captured row: IftMerge's `third =/= 0`, computed from the valid bits alone. */
  def sqThird(r: IftRawRow): Bool = {
    def t(v: Seq[Bool]): Bool = PopCount(v) > 2.U
    t(r.sp.map(_.valid)) || t(r.ic.map(_.valid)) || t(r.wb.map(_.valid)) || t(r.mo.map(_.valid))
  }

  // commit: raw read and merge in the commit cycle (functionally the old readout)
  val cRaw = rawRead(io.commit)
  // [RASBYP] patch 40 (F-RAS-1 follow-up, t91 waveform 2026-10-15): the ROB writes the RAS edge
  // at brupdate.b2 (rob.scala [RASGATE]), and a ret that mispredicts can COMMIT IN THAT SAME
  // CYCLE (b1 unbusies it; BOOM's own b2 taken/fsrc fix-up at commit, rob.scala "Perform
  // Commit", exists for this case).  The commit readout is asynchronous on v_ras / c_ras, which
  // are written at the clock edge, so the edge was lost on every such ret: in t91 all 8 m93
  // victim rets AND all 16 attacker rets reached the sidecar port (m_*.io_ras_valid) in their
  // commit cycle and printed no ty 11.  Forward the same-cycle write to the commit readout.
  // Only the commit read needs it: a mispredicting ret is not squashed by its own redirect.
  // [SCFWD] patch 40b (D17 sweep, phase4/patches/p4-40b-review.md): C_ras is the ONLY column
  // whose write can land in its row's commit cycle.  Every other port writes at least one edge
  // before the row can commit: wb / fu / clr / sprob clear rob_bsy in the same cycle (commit
  // needs !rob_bsy, so it is one edge later); ic at issue, s_acc / s_tx at the TLB stage and
  // memord at the order-fail search all precede the writeback / clr_bsy of their row (or of
  // an older row it must wait for); br_atk_self and the [STEP1] broadcast sit at b1, while the
  // branch unbusies at b1 and commits at b2 at the earliest.  rob.scala [SCFWD] asserts this
  // in Verilator printf builds.  The squash readouts need no forwarding: a killed row is
  // captured one or more cycles after its kill, and the rollback walk starts 3 cycles after
  // the exception (rob_state), after the last b2 that can name its rows.
  // Levels: io.ras.valid (b2 regs -> rob_val / rob_uop 64:1 reads -> AND, about 4) and a 6-bit
  // compare against the commit row (register, in parallel) -> 1 LUT -> the merge's valid / key
  // inputs, which already wait on the 64:1 v_ras / c_ras reads and the 16-bit rel subtract.
  // Cost: 1 compare (2 LUT), the valid OR and 16 OR terms absorbed in existing LUTs: about 4 LUT.
  val rasNow = io.ras.valid && io.ras.bits === io.commit.row
  val (co, ct) = IftMerge(cRaw, rasNow)
  io.commit_out   := co
  io.commit_third := ct
  io.commit_age   := io.stamp - cRaw.disp.alloc_stamp
  // [ROBHEADATK] patch 48: OR over every sprob column of the raw row (not the merged entry, which
  // keeps only the two newest columns).  Levels: the existing async v/c reads of the commit row
  // -> AND/OR over sprobPorts columns (2 LUT) -> rob.scala headReg FF; off the merge path.
  io.commit_valatk := cRaw.sp.map(c => c.valid && c.bits.is_atk).reduce(_ || _)
  // squash: raw read only; the row is merged later by IftSquashMerge (Task 2.10d, design 4b)
  val sRaw = rawRead(io.squash)
  io.sq_raw   := sRaw
  io.sq_third := sqThird(sRaw)
}
