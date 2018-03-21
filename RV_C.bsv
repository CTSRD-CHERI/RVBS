// 2018, Alexandre Joannou, University of Cambridge

import Printf :: *;
import List :: *;
import BitPat :: *;
import BID :: *;

import RV_Common :: *;
import RV_I :: *;

/*
  example

   12    13 12      10 9      7 6   5 4      2 1  0
  +--------+----------+--------+-----+--------+----+
  | funct3 |  offset  |  rs1'  | imm |  rs2'  | op |
  +--------+----------+--------+-----+--------+----+

*/

// BitPat guarded variable predicates
function Bool neq (Bit#(n) x, Bit#(n) y) = x != y;

`ifdef XLEN32

/////////////////////////////////
// Load and Store Instructions //
////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////
// Stack-Pointer-Based Loads and Stores //
//////////////////////////////////////////
/*
  CI-type

   15    13   12  11          7 6            2 1  0
  +--------+-----+-------------+--------------+----+
  | funct3 | imm |    rd/rs1   |     imm      | op |
  +--------+-----+-------------+--------------+----+

*/

// funct3 = C.LWSP = 010
// op = C2 = 10
function List#(Action) instrC_LWSP (RVState s, Bit#(1) imm5, Bit#(5) rd, Bit#(3) imm4_2, Bit#(2) imm7_6) =
  load(s, LoadArgs{name: "lw",  numBytes: 4, sgnExt: True}, zeroExtend({imm7_6, imm5, imm4_2, 2'b00}), 2, rd);

//TODO C.LQSP
//TODO C.FLWSP
//TODO C.FLDSP

/*
  CSS-type

   15    13  12               7 6            2 1  0
  +--------+-------------------+--------------+----+
  | funct3 |        imm        |     rs2      | op |
  +--------+-------------------+--------------+----+

*/

// funct3 = C.SWSP = 110
// op = C2 = 10
function List#(Action) instrC_SWSP (RVState s, Bit#(4) imm5_2, Bit#(2) imm7_6, Bit#(5) rs2) =
  store(s, StrArgs{name: "sw", numBytes: 4}, zeroExtend({imm7_6,imm5_2[3]}), rs2, 2, {imm5_2[2:0], 2'b00});

//TODO C.SQSP
//TODO C.FSWSP
//TODO C.FSDSP

module [InstrDefModule] mkRV32C#(RVState s) ();

  defineInstr("c.lwsp", pat(n(3'b010), v, gv(neq(0)), v, v, n(2'b10)), instrC_LWSP(s));
  defineInstr("c.swsp", pat(n(3'b110), v, v, v, n(2'b10)), instrC_SWSP(s));

endmodule
`endif // XLEN32

////////////////////////////////////////////////////////////////////////////////

`ifdef XLEN64

/////////////////////////////////
// Load and Store Instructions //
////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////
// Stack-Pointer-Based Loads and Stores //
//////////////////////////////////////////
/*
  CI-type

   15    13   12  11          7 6            2 1  0
  +--------+-----+-------------+--------------+----+
  | funct3 | imm |    rd/rs1   |     imm      | op |
  +--------+-----+-------------+--------------+----+

*/

// funct3 = C.LDSP = 011
// op = C2 = 10
function List#(Action) instrC_LDSP (RVState s, Bit#(1) imm5, Bit#(5) rd, Bit#(2) imm4_3, Bit#(3) imm8_6) =
  load(s, LoadArgs{name: "ld",  numBytes: 8, sgnExt: True}, zeroExtend({imm8_6, imm5, imm4_3, 3'b000}), 2, rd);

/*
  CSS-type

   15    13  12               7 6            2 1  0
  +--------+-------------------+--------------+----+
  | funct3 |        imm        |     rs2      | op |
  +--------+-------------------+--------------+----+

*/

// funct3 = C.SDSP = 111
// op = C2 = 10
function List#(Action) instrC_SDSP (RVState s, Bit#(3) imm5_3, Bit#(3) imm8_6, Bit#(5) rs2) =
  store(s, StrArgs{name: "sd", numBytes: 8}, zeroExtend({imm8_6,imm5_3[2]}), rs2, 2, {imm5_3[1:0], 3'b000});

module [InstrDefModule] mkRV64C#(RVState s) ();

  defineInstr("c.ldsp", pat(n(3'b011), v, gv(neq(0)), v, v, n(2'b10)), instrC_LDSP(s));
  defineInstr("c.sdsp", pat(n(3'b111), v, v, v, n(2'b10)), instrC_SDSP(s));

endmodule
`endif // XLEN64
