// 2018, Alexandre Joannou, University of Cambridge

import Printf :: *;
import List :: *;
import BitPat :: *;
import BID :: *;

import RV_Common :: *;
import RV_I :: *;

// RVC 3-bit reg identifier to RVI 5-bit reg identifier
function Bit#(5) regID (Bit#(3) x) = {2'b01, x};
// BitPat guarded variable predicates
function Bool eq (Bit#(n) x, Bit#(n) y) = x == y;
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

/////////////////////////////////////
// Register-Based Loads and Stores //
/////////////////////////////////////

/*
  CL-type

   15    13 12   10 9        7 6   5 4       2 1  0
  +--------+-------+----------+-----+---------+----+
  | funct3 |  imm  |   rs1'   | imm |   rd'   | op |
  +--------+-------+----------+-----+---------+----+

*/

// funct3 = C.LW = 010
// op = C0 = 00
function List#(Action) instrC_LW (RVState s, Bit#(3) o5_3, Bit#(3) rs1_, Bit#(1) o2, Bit#(1) o6, Bit#(3) rd_) =
  load(s, LoadArgs{name: "lw",  numBytes: 4, sgnExt: True}, zeroExtend({o6, o5_3, o2, 2'b00}), regID(rs1_), regID(rd_));

//TODO C.LQ
//TODO C.FLW
//TODO C.FLD

/*
  CS-type

   15    13 12   10 9        7 6   5 4       2 1  0
  +--------+-------+----------+-----+---------+----+
  | funct3 |  imm  |   rs1'   | imm |   rs2'  | op |
  +--------+-------+----------+-----+---------+----+

*/

// funct3 = C.SW = 110
// op = C0 = 00
function List#(Action) instrC_SW (RVState s, Bit#(3) o5_3, Bit#(3) rs1_, Bit#(1) o2, Bit#(1) o6, Bit#(3) rs2_) =
  store(s, StrArgs{name: "sw", numBytes: 4}, zeroExtend({o6, o5_3[2]}), regID(rs2_), regID(rs1_), {o5_3[1:0], o2, 2'b00});

//TODO C.SQ
//TODO C.FSW
//TODO C.FSD

///////////////////////////////////
// Control Transfer Instructions //
////////////////////////////////////////////////////////////////////////////////

/*
  CJ-type

   15    13 12                               2 1  0
  +--------+----------------------------------+----+
  | funct3 |                imm               | op |
  +--------+----------------------------------+----+

*/

// funct3 = C.J = 101
// op = C1 = 01
function Action instrC_J (RVState s, Bit#(11) i) = action
  Bit#(11) offset = {i[10], i[6], i[8:7], i[4], i[5], i[9], i[0], i[3:1]};
  instrJAL(s, offset[10], offset[9:0], offset[10], signExtend(offset[10]), 0);
endaction;

`ifndef XLEN64
  // funct3 = C.JAL = 001
  // op = C1 = 01
  function Action instrC_JAL (RVState s, Bit#(11) i) = action
    Bit#(11) offset = {i[10], i[6], i[8:7], i[4], i[5], i[9], i[0], i[3:1]};
    instrJAL(s, offset[10], offset[9:0], offset[10], signExtend(offset[10]), 1);
  endaction;
`endif

/*
  CR-type

   15         12 11           7 6            2 1  0
  +-------------+--------------+--------------+----+
  |    funct4   |    rd/rs1    |     rs2      | op |
  +-------------+--------------+--------------+----+

*/

//XXX use for rs2 ?

// funct4 = C.JR = 1000
// op = C2 = 10
function Action instrC_JR (RVState s, Bit#(5) rs1, Bit#(5) rs2) =
  instrJALR(s, 0, rs1, 0);

// funct4 = C.JALR = 1001
// op = C2 = 10
function Action instrC_JALR (RVState s, Bit#(5) rs1, Bit#(5) rs2) =
  instrJALR(s, 0, rs1, 1);

/*
  CB-type

   15    13 12      10 9      7 6            2 1  0
  +--------+----------+--------+--------------+----+
  | funct3 |  offset  |  rs1'  |    offset    | op |
  +--------+----------+--------+--------------+----+

*/

// funct3 = C.BEQZ = 110
// op = C1 = 01
function Action instrC_BEQZ (RVState s, Bit#(3) i7_5, Bit#(3) rs1_, Bit#(5) i4_0) = action
  Bit#(12) o = signExtend({i7_5[2], i4_0[4:3], i4_0[0], i7_5[1:0], i4_0[2:1],1'b0});
  instrBEQ(s, o[11], o[9:4], 0, regID(rs1_), o[3:0], o[10]);
endaction;

// funct3 = C.BNEZ = 111
// op = C1 = 01
function Action instrC_BNEZ (RVState s, Bit#(3) i7_5, Bit#(3) rs1_, Bit#(5) i4_0) = action
  Bit#(12) o = signExtend({i7_5[2], i4_0[4:3], i4_0[0], i7_5[1:0], i4_0[2:1],1'b0});
  instrBNE(s, o[11], o[9:4], 0, regID(rs1_), o[3:0], o[10]);
endaction;

////////////////////////////////////////////////////////////////////////////////

module [InstrDefModule] mkRV32C#(RVState s) ();

  defineInstr("c.lwsp", pat(n(3'b010), v, gv(neq(0)), v, v, n(2'b10)), instrC_LWSP(s));
  defineInstr("c.swsp", pat(n(3'b110), v, v, v, n(2'b10)), instrC_SWSP(s));
  defineInstr("c.lw",   pat(n(3'b010), v, v, v, v, v, n(2'b00)), instrC_LW(s));
  defineInstr("c.sw",   pat(n(3'b110), v, v, v, v, v, n(2'b00)), instrC_SW(s));
  defineInstr("c.j",    pat(n(3'b101), v, n(2'b01)), instrC_J(s));
  defineInstr("c.jal",  pat(n(3'b001), v, n(2'b01)), instrC_JAL(s));
  defineInstr("c.jr",   pat(n(4'b1000), gv(neq(0)), gv(eq(0)), n(2'b10)), instrC_JR(s));
  defineInstr("c.jalr", pat(n(4'b1001), gv(neq(0)), gv(eq(0)), n(2'b10)), instrC_JALR(s));
  defineInstr("c.beqz", pat(n(3'b110), v, v, v, n(2'b01)), instrC_BEQZ(s));
  defineInstr("c.bnez", pat(n(3'b111), v, v, v, n(2'b01)), instrC_BNEZ(s));

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

/////////////////////////////////////
// Register-Based Loads and Stores //
/////////////////////////////////////

/*
  CL-type

   15    13 12   10 9        7 6   5 4       2 1  0
  +--------+-------+----------+-----+---------+----+
  | funct3 |  imm  |   rs1'   | imm |   rd'   | op |
  +--------+-------+----------+-----+---------+----+

*/

// funct3 = C.LD = 011
// op = C0 = 00
function List#(Action) instrC_LD (RVState s, Bit#(3) o5_3, Bit#(3) rs1', Bit#(2) o7_6, Bit#(3) rd') =
  load(s, LoadArgs{name: "ld",  numBytes: 8, sgnExt: True}, zeroExtend({o7_6, o5_3, 3'b000}), regID(rs1'), regID(rd'));

/*
  CS-type

   15    13 12   10 9        7 6   5 4       2 1  0
  +--------+-------+----------+-----+---------+----+
  | funct3 |  imm  |   rs1'   | imm |   rs2'  | op |
  +--------+-------+----------+-----+---------+----+

*/

// funct3 = C.SD = 111
// op = C0 = 00
function List#(Action) instrC_SD (RVState s, Bit#(3) o5_3, Bit#(3) rs1', Bit#(2) o7_6, Bit#(3) rs2') =
  store(s, StrArgs{name: "sd", numBytes: 8}, zeroExtend({o7_6, o5_3[2]}), regID(rs2'), regID(rs1'), {o5_3[1:0], 3'b000});

////////////////////////////////////////////////////////////////////////////////

module [InstrDefModule] mkRV64C#(RVState s) ();

  defineInstr("c.ldsp", pat(n(3'b011), v, gv(neq(0)), v, v, n(2'b10)), instrC_LDSP(s));
  defineInstr("c.sdsp", pat(n(3'b111), v, v, v, n(2'b10)), instrC_SDSP(s));
  defineInstr("c.ld",   pat(n(3'b011), v, v, v, n(2'b00)), instrC_LD(s));
  defineInstr("c.sd",   pat(n(3'b111), v, v, v, v, n(2'b00)), instrC_SD(s));

endmodule
`endif // XLEN64
