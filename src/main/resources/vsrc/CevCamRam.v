// CevCamRam: one RAMB36 as a CAM slice (ceviche B1).  512 rows x 64 bits on the read port, 1 bit x 32768 on the write port
// (the asymmetric simple-dual-port template of UG901).  Bit (row r, column c) = ram[{r, c}].
// - Synchronous read, one clock, no output register: rdData in cycle t+1 is row rdAddr of cycle t.
// - READ_FIRST: a read of the row whose bit is written in the same cycle returns the OLD bit (nonblocking write, read of the old array).
// - Contents are zero after configuration / simulation start (the initial loop); there is no reset: the CAM bits and the
//   key copy in the CRF stay consistent across a target reset, the line valid flops are what reset.
// One source for Verilator and the FPGA flow (Vivado infers RAMB36E2 SDP from this template).
module CevCamRam(
  input         clock,
  input  [8:0]  rdAddr,
  output reg [63:0] rdData,
  input         wrEn,
  input  [14:0] wrAddr,
  input         wrData
);
  (* ram_style = "block" *) reg ram [0:32767];
  integer i;
  initial begin
    for (i = 0; i < 32768; i = i + 1) ram[i] = 1'b0;
    rdData = 64'b0;
  end
  always @(posedge clock) begin
    if (wrEn) ram[wrAddr] <= wrData;
    for (i = 0; i < 64; i = i + 1) rdData[i] <= ram[{rdAddr, i[5:0]}];
  end
endmodule
