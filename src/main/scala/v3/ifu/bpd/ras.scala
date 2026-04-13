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

    // corefuzzing: quiesce flush — clear all domain/secret shadow bits on campaign boundary.
    // Fired from core.scala on QS_DRAINING→QS_FETCH transition (quiesce_flush_pulse).
    val quiesce_flush = Input(Bool())
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
  io.read_domain := Mux(RegNext(io.write_valid && io.write_idx === io.read_idx),
    RegNext(io.write_domain),
    RegNext(ras_domain(io.read_idx)))
  io.read_secret := Mux(RegNext(io.write_valid && io.write_idx === io.read_idx),
    RegNext(io.write_secret),
    RegNext(ras_secret(io.read_idx)))

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

    // corefuzzing: quiesce flush — clear all domain/secret shadow bits for clean IFT campaign boundaries.
    // Prevents stale domain/secret bits in entries beyond the new active RAS size from corrupting IFT
    // records after a cf_ras_idx CSR reconfiguration.
    when (io.quiesce_flush) {
      for (i <- 0 until nRasEntries) {
        ras_domain(i) := false.B
        ras_secret(i) := false.B
      }
    }
  }
}
