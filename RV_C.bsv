// 2018, Alexandre Joannou, University of Cambridge

import BitPat :: *;
import Printf :: *;
import BID :: *;

import RV_Common :: *;

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
module [InstrDefModule] mkRV32C#(RVArchState s, RVDMem mem) ();

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
  function Action instrC_LWSP (Bit#(1) imm5, Bit#(5) rd, Bit#(3) imm4_2, Bit#(2) imm7_6) = action
    //TODO logInstCI(s.pc, "c.lwsp", rd, imm);
  endaction;
  defineInstr("c.lwsp", pat(n(3'b010), v, gv(neq(0)), v, v, n(2'b10)), instrC_LWSP);

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
  function Action instrC_SWSP (Bit#(4) imm5_2, Bit#(2) imm7_6, Bit#(5) rs2) = action
    //TODO logInstCSS(s.pc, "c.swsp", rs2, imm);
  endaction;
  defineInstr("c.swsp", pat(n(3'b110), v, v, v, n(2'b10)), instrC_SWSP);

//TODO C.SQSP
//TODO C.FSWSP
//TODO C.FSDSP

endmodule
`endif // XLEN32

////////////////////////////////////////////////////////////////////////////////

`ifdef XLEN64
module [InstrDefModule] mkRV64C#(RVArchState s, RVDMem mem) ();

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
  function Action instrC_LDSP (Bit#(1) imm5, Bit#(5) rd, Bit#(3) imm4_2, Bit#(2) imm7_6) = action
    //TODO logInstCI(s.pc, "c.ldsp", rd, imm);
  endaction;
  defineInstr("c.ldsp", pat(n(3'b011), v, gv(neq(0)), v, v, n(2'b10)), instrC_LDSP);

/*
  CSS-type

   15    13  12               7 6            2 1  0
  +--------+-------------------+--------------+----+
  | funct3 |        imm        |     rs2      | op |
  +--------+-------------------+--------------+----+

*/

  // funct3 = C.SDSP = 111
  // op = C2 = 10
  function Action instrC_SDSP (Bit#(4) imm5_2, Bit#(2) imm7_6, Bit#(5) rs2) = action
    //TODO logInstCSS(s.pc, "c.sdsp", rs2, imm);
  endaction;
  defineInstr("c.sdsp", pat(n(3'b111), v, v, v, n(2'b10)), instrC_SDSP);

  endmodule
`endif // XLEN64
