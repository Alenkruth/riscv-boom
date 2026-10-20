//******************************************************************************
// Copyright (c) 2013 - 2018, The Regents of the University of California (Regents).
// All Rights Reserved. See LICENSE and LICENSE.SiFive for license details.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
// Register File (Abstract class and Synthesizable RegFile)
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

package boom.v3.exu

import scala.collection.mutable.ArrayBuffer

import chisel3._
import chisel3.util._

import org.chipsalliance.cde.config.Parameters

import boom.v3.common._
import boom.v3.util.{BoomCoreStringPrefix}

/**
 * IO bundle for a register read port
 *
 * @param addrWidth size of register address in bits
 * @param dataWidth size of register in bits
 */
class RegisterFileReadPortIO(val addrWidth: Int, val dataWidth: Int)(implicit p: Parameters) extends BoomBundle
{
  val addr = Input(UInt(addrWidth.W))
  val data = Output(UInt(dataWidth.W))
  // corefuzzing (taint-follows-data): taint travelling WITH this value. Held in a
  // parallel 1-bit Mem on the same address, so it is RAM like the data -- unlike the
  // old preg_secret Vec-of-Reg, whose every read inferred a numPregs:1 mux.
  val secret    = Output(Bool())
  val taint_atk = Output(Bool())
  // [IMPLWIN] step 20: WINDOW taint (implicit flow), a separate kind from the DATA taint above.
  val wsec      = Output(Bool())
  val watk      = Output(Bool())
}

/**
 * IO bundle for the register write port
 *
 * @param addrWidth size of register address in bits
 * @param dataWidth size of register in bits
 */
class RegisterFileWritePort(val addrWidth: Int, val dataWidth: Int)(implicit p: Parameters) extends BoomBundle
{
  val addr = UInt(addrWidth.W)
  val data = UInt(dataWidth.W)
  // corefuzzing: taint written by the SAME port that writes the value, so a
  // reallocated register cannot inherit a stale bit.
  val secret    = Bool()
  val taint_atk = Bool()
  val wsec      = Bool()   // [IMPLWIN] window taint of the written value
  val watk      = Bool()
}

/**
 * Utility function to turn ExeUnitResps to match the regfile's WritePort I/Os.
 */
object WritePort
{
  def apply(enq: DecoupledIO[ExeUnitResp], addrWidth: Int, dataWidth: Int, rtype: UInt,
    wsh_sec: WinShadowInfo, wsh_atk: WinShadowInfo)   // [WINSHADOW] shadow mark, [WINEXP] epoch
    (implicit p: Parameters): Valid[RegisterFileWritePort] = {
     val wport = Wire(Valid(new RegisterFileWritePort(addrWidth, dataWidth)))

     wport.valid     := enq.valid && enq.bits.uop.dst_rtype === rtype
     wport.bits.addr := enq.bits.uop.pdst
     wport.bits.data := enq.bits.data
     wport.bits.secret    := enq.bits.secret
     // taint_atk is a property of the WRITER, not of the data, so it is taken from
     // the producing uop's domain here rather than pipelined through every FU stage.
     // This is exactly INFL_REG_DATAFLOW's stated intent: "victim reads physical reg
     // written by attacker uop".
     // corefuzzing: attacker taint now FOLLOWS DATA as well as recording ownership.
     // Ownership alone ("which domain wrote this") cannot express STEERING: in spectre-v1
     // the whole gadget runs in domain=0 because the mispredicted bounds check is the
     // VICTIM's code -- the attacker only supplies the index.  ORing the propagated
     // operand taint makes a value derived from attacker-written data carry the tag.
     wport.bits.taint_atk := enq.bits.taint_atk || (enq.bits.uop.cf_domain_id.getOrElse(0.U(1.W)) =/= 0.U)
     // [IMPLWIN] window taint: produced in the window, or from a window-tainted operand.
     // [WINEXP] step 21: only while the uop's dispatch epoch is current (WinShadowInfo.res).
     wport.bits.wsec      := wsh_sec.res(enq.bits.uop.cf_win_sec.getOrElse(false.B) || enq.bits.uop.cf_opnd_wsec.getOrElse(false.B),
                                         enq.bits.uop.cf_wep_sec.getOrElse(0.U(2.W)), enq.bits.uop.rob_idx)
     wport.bits.watk      := wsh_atk.res(enq.bits.uop.cf_win_atk.getOrElse(false.B) || enq.bits.uop.cf_opnd_watk.getOrElse(false.B),
                                         enq.bits.uop.cf_wep_atk.getOrElse(0.U(2.W)), enq.bits.uop.rob_idx)
     // s0/sp carry taint (PROBE J: reqsec=1 on every stack load), and `addi s0,sp,N`
     // propagates rs1, so one taint on sp is self-sustaining.  Print the writer.
     enq.ready       := true.B
     wport
  }
}

/**
 * Register file abstract class
 *
 * @param numRegisters number of registers
 * @param numReadPorts number of read ports
 * @param numWritePorts number of write ports
 * @param registerWidth size of registers in bits
 * @param bypassableArray list of write ports from func units to the read port of the regfile
 */
abstract class RegisterFile(
  numRegisters: Int,
  numReadPorts: Int,
  numWritePorts: Int,
  registerWidth: Int,
  bypassableArray: Seq[Boolean], // which write ports can be bypassed to the read ports?
  numWinSetPorts: Int = 0)
  (implicit p: Parameters) extends BoomModule
{
  val io = IO(new BoomBundle {
    val read_ports = Vec(numReadPorts, new RegisterFileReadPortIO(maxPregSz, registerWidth))
    val write_ports = Flipped(Vec(numWritePorts, Valid(new RegisterFileWritePort(maxPregSz, registerWidth))))
    // [WINSHADOW] step 20: commit-time "set window taint" ports (core.scala): a uop that was
    // already in flight when the arming branch resolved, younger than it, gets its result
    // register window-tainted when it commits.  Write-only, set to 1.
    val win_set_sec = Flipped(Vec(numWinSetPorts, Valid(UInt(maxPregSz.W))))
    val win_set_atk = Flipped(Vec(numWinSetPorts, Valid(UInt(maxPregSz.W))))
    // [WINCLR] patch 35 C (owner r166; replaces the [WINEXP] step-21 epoch stamp, r139 F3):
    // the window-close pulses {atk, sec} (core.scala winCtr `close`, the signal that advances
    // the kind's epoch on the same edge).  A pulse clears every window mark of its kind.
    val cf_wclr     = Input(UInt(2.W))
  })

  private val rf_cost = (numReadPorts + numWritePorts) * (numReadPorts + 2*numWritePorts)
  private val type_str = if (registerWidth == fLen+1) "Floating Point" else "Integer"
  override def toString: String = BoomCoreStringPrefix(
    "==" + type_str + " Regfile==",
    "Num RF Read Ports     : " + numReadPorts,
    "Num RF Write Ports    : " + numWritePorts,
    "RF Cost (R+W)*(R+2W)  : " + rf_cost,
    "Bypassable Units      : " + bypassableArray)
}

/**
 * A synthesizable model of a Register File. You will likely want to blackbox this for more than modest port counts.
 *
 * @param numRegisters number of registers
 * @param numReadPorts number of read ports
 * @param numWritePorts number of write ports
 * @param registerWidth size of registers in bits
 * @param bypassableArray list of write ports from func units to the read port of the regfile
 */
class RegisterFileSynthesizable(
   numRegisters: Int,
   numReadPorts: Int,
   numWritePorts: Int,
   registerWidth: Int,
   bypassableArray: Seq[Boolean],
   numWinSetPorts: Int = 0)
   (implicit p: Parameters)
   extends RegisterFile(numRegisters, numReadPorts, numWritePorts, registerWidth, bypassableArray, numWinSetPorts)
{
  // --------------------------------------------------------------

  val regfile = Mem(numRegisters, UInt(registerWidth.W))
  // corefuzzing taint shadow: {taint_atk, secret}, packed beside the data in RAM.
  // Deliberately TWO bits, not three.  The influencer's producer op_count is NOT
  // carried: it is recovered in post-processing from the logged pregs
  // (ift-tests/preg_dataflow.py), so paying inflOpCountWidthCF bits per read port,
  // write port and FU stage would buy nothing.  `secret` must be in hardware --
  // it depends on the execute-time TLB range check, which no log field implies --
  // and `taint_atk` rides along for downstream cross-domain attribution.
  val cfTaintW = 2   // {taint_atk, secret}
  // [R124GATE2] patch 27 (r124/B51): like regfile_wsec/watk below, declared always but read and
  // written only with ENABLE_IFT, so IFT off it has no ports and is removed; the read-port
  // secret/taint_atk outputs are 0 and the write ports' secret/taint_atk bits lose their reader.
  val regfile_secret = Mem(numRegisters, UInt(cfTaintW.W))
  // [IMPLWIN] window taint, one array per kind so the commit-time set ports write one entry.
  // [WINCLR] patch 35 C (owner r166): each entry is ONE mark bit, flash-cleared (every entry of
  // the kind) on the edge whose cycle closes that kind's window; the clear has priority over the
  // write and set ports of the same cycle (their bits belong to the closing window).  It replaces
  // the step-21 {epoch[2:1], bit[0]} entry read through winLive = bit && epoch == current:
  //   value: a mark last written in cycle t reads 1 at c > t iff no close fell in [t, c-1]; the
  //     epoch form also read 1 when the closes in [t, c-1] were a non-zero multiple of 4 (2-bit
  //     wrap, over-taint).  That case is the only difference (p4-35-author.md).
  //   storage: Vivado already built these from flops (Synth 8-4767, multi-write-port), so a Reg
  //     vector is the same structure at 1/3 the width (-2 FF per entry and kind) with no epoch
  //     compare after the read mux (-1 level on the read path).  Reset to 0 ([WINRST] patch 41,
  //     below): the initial architectural pregs are read before any write; every other entry is
  //     written (write port, same cycle as its data) before a consumer can read it (busy table),
  //     and preg 0 reads are masked in register-read (rs*_is_p0).
  //   clear: io.cf_wclr(k) drives the flops' synchronous clear (FDRE R); the write-port D logic
  //     and the pdst row decode (census-3 W path) are unchanged apart from the narrower data.
  // [R124GATE] patch 26 (r151): built only with ENABLE_IFT; IFT off read_win and wsec/watk are 0.
  // [WINRST] patch 41 (p4-35 review MINOR 2): reset to 0.  Without it Verilator's random init left
  //   a never-written preg (the initial architectural map) live with p = 1/2 until the first
  //   close.  On the FPGA the flops already start at INIT 0.  The reset shares the flops' FDRE R
  //   pin with the [WINCLR] close (R = reset | close); reset is a register output, so it is one
  //   more input to the close cone's last LUT (4 of 6 inputs used), not a new level.
  val regfile_wsec = if (ENABLE_IFT) Some(RegInit(VecInit(Seq.fill(numRegisters)(false.B)))) else None
  val regfile_watk = if (ENABLE_IFT) Some(RegInit(VecInit(Seq.fill(numRegisters)(false.B)))) else None
  private val wixW = 1 max log2Ceil(numRegisters)
  // [41c] no getWidth: read_addrs below are RegNext of the port address, whose width Chisel leaves
  // INFERRED (RegNext clones a Bits type without its width), so a.getWidth threw "Width of
  // RegisterFileSynthesizable.read_addrs_0: Reg[UInt] is unknown" at elaboration (FP regfile).  The
  // port address is never narrower than wixW (numRegisters <= 2^addrWidth), so the low-bit slice is
  // always legal and is a no-op when the widths are equal; IFT-only, IFT-off Verilog unchanged.
  def wix(a: UInt): UInt = a(wixW - 1, 0)   // the Mem's address width

  // --------------------------------------------------------------
  // Read ports.

  val read_data = Wire(Vec(numReadPorts, UInt(registerWidth.W)))
  val read_secret = Wire(Vec(numReadPorts, UInt(cfTaintW.W)))
  val read_win    = Wire(Vec(numReadPorts, UInt(2.W)))   // {watk, wsec}

  // Register the read port addresses to give a full cycle to the RegisterRead Stage (if desired).
  val read_addrs = io.read_ports.map(p => RegNext(p.addr))

  for (i <- 0 until numReadPorts) {
    read_data(i)   := regfile(read_addrs(i))
      read_secret(i) := (if (ENABLE_IFT) regfile_secret(read_addrs(i)) else 0.U)   // [R124GATE2]
      read_win(i)    := (if (ENABLE_IFT) Cat(regfile_watk.get(wix(read_addrs(i))),   // [WINCLR] no epoch compare
                                              regfile_wsec.get(wix(read_addrs(i))))
                         else 0.U)
  }

  // --------------------------------------------------------------
  // Bypass out of the ALU's write ports.
  // We are assuming we cannot bypass a writer to a reader within the regfile memory
  // for a write that occurs at the end of cycle S1 and a read that returns data on cycle S1.
  // But since these bypasses are expensive, and not all write ports need to bypass their data,
  // only perform the w->r bypass on a select number of write ports.

  require (bypassableArray.length == io.write_ports.length)

  if (bypassableArray.reduce(_||_)) {
    val bypassable_wports = ArrayBuffer[Valid[RegisterFileWritePort]]()
    io.write_ports zip bypassableArray map { case (wport, b) => if (b) { bypassable_wports += wport} }

    for (i <- 0 until numReadPorts) {
      val bypass_ens = bypassable_wports.map(x => x.valid &&
        x.bits.addr === read_addrs(i))

      val bypass_data = Mux1H(VecInit(bypass_ens.toSeq), VecInit(bypassable_wports.map(_.bits.data).toSeq))
        // taint rides the SAME bypass select as the data
        // taint rides the SAME bypass select as the data
        val bypass_taint = Mux1H(VecInit(bypass_ens.toSeq),
          VecInit(bypassable_wports.map(x => Cat(x.bits.taint_atk, x.bits.secret)).toSeq))
        val sel_taint = Mux(bypass_ens.reduce(_|_), bypass_taint, read_secret(i))
        val bypass_win = Mux1H(VecInit(bypass_ens.toSeq),
          VecInit(bypassable_wports.map(x => Cat(x.bits.watk, x.bits.wsec)).toSeq))
        val sel_win = Mux(bypass_ens.reduce(_|_), bypass_win, read_win(i))
        io.read_ports(i).wsec := (if (ENABLE_IFT) sel_win(0) else false.B)   // [R124GATE]
        io.read_ports(i).watk := (if (ENABLE_IFT) sel_win(1) else false.B)

      io.read_ports(i).data   := Mux(bypass_ens.reduce(_|_), bypass_data,   read_data(i))
        io.read_ports(i).secret    := (if (ENABLE_IFT) sel_taint(0) else false.B)   // [R124GATE2]
        io.read_ports(i).taint_atk := (if (ENABLE_IFT) sel_taint(1) else false.B)
    }
  } else {
    for (i <- 0 until numReadPorts) {
      io.read_ports(i).data   := read_data(i)
        io.read_ports(i).secret    := (if (ENABLE_IFT) read_secret(i)(0) else false.B)   // [R124GATE2]
        io.read_ports(i).taint_atk := (if (ENABLE_IFT) read_secret(i)(1) else false.B)
        io.read_ports(i).wsec      := (if (ENABLE_IFT) read_win(i)(0) else false.B)   // [R124GATE]
        io.read_ports(i).watk      := (if (ENABLE_IFT) read_win(i)(1) else false.B)
    }
  }

  // --------------------------------------------------------------
  // Write ports.

  for (wport <- io.write_ports) {
    when (wport.valid) {
      regfile(wport.bits.addr) := wport.bits.data
      if (ENABLE_IFT) regfile_secret(wport.bits.addr) := Cat(wport.bits.taint_atk, wport.bits.secret)   // [R124GATE2]
      // [WINEXP] the port's bit is already live-checked (WinShadowInfo.res); [WINCLR] store it
      if (ENABLE_IFT) {   // [R124GATE]
        regfile_wsec.get(wix(wport.bits.addr)) := wport.bits.wsec
        regfile_watk.get(wix(wport.bits.addr)) := wport.bits.watk
      }
    }
  }
  // [WINSHADOW] commit-time sets (never the same cycle as this register's own writeback: a
  // uop commits after it writes back, and its pdst is not reallocated before it commits).
  if (ENABLE_IFT) {   // [R124GATE]
    for (s <- io.win_set_sec) { when (s.valid) { regfile_wsec.get(wix(s.bits)) := true.B } }
    for (s <- io.win_set_atk) { when (s.valid) { regfile_watk.get(wix(s.bits)) := true.B } }
    // [WINCLR] patch 35 C: the close clears last, so it wins over this cycle's writes and sets.
    when (io.cf_wclr(0)) { regfile_wsec.get.foreach(_ := false.B) }
    when (io.cf_wclr(1)) { regfile_watk.get.foreach(_ := false.B) }
  }

  // ensure there is only 1 writer per register (unless to preg0)
  if (numWritePorts > 1) {
    for (i <- 0 until (numWritePorts - 1)) {
      for (j <- (i + 1) until numWritePorts) {
        assert(!io.write_ports(i).valid ||
               !io.write_ports(j).valid ||
               (io.write_ports(i).bits.addr =/= io.write_ports(j).bits.addr) ||
               (io.write_ports(i).bits.addr === 0.U), // note: you only have to check one here
          "[regfile] too many writers a register")
      }
    }
  }
}
