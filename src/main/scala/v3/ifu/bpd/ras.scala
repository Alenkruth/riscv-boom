//******************************************************************************
// Copyright (c) 2017 - 2019, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

package boom.v3.ifu

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config._
import freechips.rocketchip.subsystem._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.rocket._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.tile._
import freechips.rocketchip.util._
import freechips.rocketchip.util.property._


import boom.v3.common._
import boom.v3.exu.{CommitExceptionSignals, BranchDecode, BrUpdateInfo}
import boom.v3.util._

class BoomRAS(implicit p: Parameters) extends BoomModule()(p)
{
  val io = IO(new Bundle {
    val read_idx   = Input(UInt(log2Ceil(nRasEntries).W))
    val read_addr  = Output(UInt(vaddrBitsExtended.W))
    // corefuzzing: domain of the entry being read (1=attacker, 0=victim)
    val read_domain = Output(Bool())
    // corefuzzing: secret tag of the entry being read (true if the pushing call was secret)
    val read_secret = Output(Bool())

    val write_valid = Input(Bool())
    val write_idx   = Input(UInt(log2Ceil(nRasEntries).W))
    val write_addr  = Input(UInt(vaddrBitsExtended.W))
    // corefuzzing: domain of the call that pushes this entry
    val write_domain = Input(Bool())
    // corefuzzing: set false at push; true retroactively via late_write_secret when call is confirmed secret
    val write_secret = Input(Bool())

    // corefuzzing: late update ports — fired from FTQ when a secret instruction commits from
    // a call-containing fetch packet, marking the pushed RAS slot as secret.
    val late_write_secret_valid = Input(Vec(coreWidth + memWidth, Bool()))
    val late_write_secret_idx   = Input(Vec(coreWidth + memWidth, UInt(log2Ceil(nRasEntries).W)))
    // [WINFTQ] patch 29b: one more late port, the calls of a secret-window FTQ range
    val late_write_win = if (ENABLE_IFT) Some(Input(Valid(UInt(log2Ceil(nRasEntries).W)))) else None

    // corefuzzing: quiesce flush — clear all domain/secret shadow bits on campaign boundary.
    // Fired from core.scala on QS_DRAINING→QS_FETCH transition (quiesce_flush_pulse).
    val quiesce_flush = Input(Bool())

    // [RASPTR] patch 29 (F-RAS-1, owner r158): RAS POINTER influence.  atk_move: an attacker-
    // domain fetch packet pushed or popped this cycle (it moved the pointer).  write_push: this
    // write is a call push (false for the top repair).  read_stale: the entry being read was
    // written before the latest attacker pointer move.
    val atk_move   = Input(Bool())
    val write_push = Input(Bool())
    val read_stale = Output(Bool())
  })
  val ras = Reg(Vec(nRasEntries, UInt(vaddrBitsExtended.W)))
  // corefuzzing: per-entry domain shadow (1=pushed by attacker, 0=pushed by victim)
  val ras_domain = RegInit(VecInit(Seq.fill(nRasEntries)(false.B)))
  // corefuzzing: per-entry secret shadow (true=pushed during a secret fetch packet)
  val ras_secret = RegInit(VecInit(Seq.fill(nRasEntries)(false.B)))

  io.read_addr := Mux(RegNext(io.write_valid && io.write_idx === io.read_idx),
    RegNext(io.write_addr),
    RegNext(ras(io.read_idx)))

  // corefuzzing: forward domain and secret same as address for write-read hazard
  // [R124GATE3] patch 28 (B51): the read-side domain/secret flops only with ENABLE_IFT (IFT off
  // the shadow writes are already elided, so these only delayed constants); IFT off 0.
  io.read_domain := (if (ENABLE_IFT) Mux(RegNext(io.write_valid && io.write_idx === io.read_idx),
    RegNext(io.write_domain),
    RegNext(ras_domain(io.read_idx))) else false.B)
  io.read_secret := (if (ENABLE_IFT) Mux(RegNext(io.write_valid && io.write_idx === io.read_idx),
    RegNext(io.write_secret),
    RegNext(ras_secret(io.read_idx))) else false.B)

  when (io.write_valid) {
    ras(io.write_idx)        := io.write_addr
  }

  // IFT compile-time gate: ras_domain / ras_secret writes are elided when
  // ENABLE_IFT=false.  Reads stay unconditional so io.read_domain / io.read_secret
  // produce constant false (RegInit value), and FIRRTL DCE removes both shadow
  // register arrays along with their forwarding muxes.
  if (ENABLE_IFT) {
    when (io.write_valid) {
      ras_domain(io.write_idx) := io.write_domain
      ras_secret(io.write_idx) := io.write_secret  // reset to false; late_write_secret marks true later
    }

    // corefuzzing: late update — mark a slot secret when the associated call instruction is confirmed secret
    for (i <- 0 until (coreWidth + memWidth)) {
      when (io.late_write_secret_valid(i)) {
        ras_secret(io.late_write_secret_idx(i)) := true.B
      }
    }
    io.late_write_win.foreach { l => when (l.valid) { ras_secret(l.bits) := true.B } }   // [WINFTQ]

    // [RASPTR] patch 29 (F-RAS-1): an attacker that only MOVES the pointer (extra pops or pushes
    // without a matching return) makes a victim ret pop an entry the victim pushed for another
    // call: the slot's domain is 0, so ras_domain gives no edge.  The entry a ret reads is a
    // function of the pointer, and the pointer is attacker-influenced from the attacker's move
    // on; an entry pushed after that move is read back by its own matching ret independently of
    // the absolute pointer, an entry pushed before it is not.  So: every attacker push/pop marks
    // ALL entries stale (the stack is circular: every slot lies below the pointer), a call push
    // clears the entry it writes (the top repair does not: it restores old content), and the
    // read-side flag rides beside read_domain.  frontend.scala turns a stale read by a victim
    // ret into cf_ras_pred_xdom, so the ROB raises INFL_RAS_STATE atk=1 only if that ret
    // MISPREDICTS (doc 67 Part B gate): a balanced callback (attacker ret into its victim caller)
    // marks the victim's lower frames stale, but their rets predict correctly and stay silent.
    // Wrong-path attacker moves also mark (over-taint, mispredict-gated the same way).
    // Cost: nRasEntries FF + 1 LUT each, a nRasEntries:1 mux and 1 FF on the read side.
    // Levels: the bulk-set enable is the f3 push/pop condition AND the f3 attacker bit (+1 LUT
    // beside ras.io.write_valid, into FFs); the read flag is a register like read_domain.
    val ras_stale = RegInit(VecInit(Seq.fill(nRasEntries)(false.B)))
    when (io.atk_move) { ras_stale.foreach(_ := true.B) }
    when (io.write_valid && io.write_push) { ras_stale(io.write_idx) := false.B }

    // corefuzzing: quiesce flush — clear all domain/secret shadow bits for clean IFT campaign boundaries.
    // Prevents stale domain/secret bits in entries beyond the new active RAS size from corrupting IFT
    // records after a cf_ras_idx CSR reconfiguration.
    when (io.quiesce_flush) {
      for (i <- 0 until nRasEntries) {
        ras_domain(i) := false.B
        ras_secret(i) := false.B
        ras_stale(i)  := false.B
      }
    }
    // the entry's value after this cycle's updates, registered like read_domain
    io.read_stale := RegNext(Mux(io.write_valid && io.write_push && io.write_idx === io.read_idx, false.B,
                                 io.atk_move || ras_stale(io.read_idx)), false.B)
  } else {
    io.read_stale := false.B
  }
}
