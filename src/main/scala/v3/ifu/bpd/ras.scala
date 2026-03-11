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

    val write_valid = Input(Bool())
    val write_idx   = Input(UInt(log2Ceil(nRasEntries).W))
    val write_addr  = Input(UInt(vaddrBitsExtended.W))
    // corefuzzing: domain of the call that pushes this entry
    val write_domain = Input(Bool())
  })
  val ras = Reg(Vec(nRasEntries, UInt(vaddrBitsExtended.W)))
  // corefuzzing: per-entry domain shadow (1=pushed by attacker, 0=pushed by victim)
  val ras_domain = RegInit(VecInit(Seq.fill(nRasEntries)(false.B)))

  io.read_addr := Mux(RegNext(io.write_valid && io.write_idx === io.read_idx),
    RegNext(io.write_addr),
    RegNext(ras(io.read_idx)))

  // corefuzzing: forward domain same as address for write-read hazard
  io.read_domain := Mux(RegNext(io.write_valid && io.write_idx === io.read_idx),
    RegNext(io.write_domain),
    RegNext(ras_domain(io.read_idx)))

  when (io.write_valid) {
    ras(io.write_idx)        := io.write_addr
    ras_domain(io.write_idx) := io.write_domain
  }


}
