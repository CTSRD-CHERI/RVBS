/*-
 * Copyright (c) 2018 Alexandre Joannou
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * @BERI_LICENSE_HEADER_START@
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERI_LICENSE_HEADER_END@
 */

import Printf :: *;
import List :: *;
import BitPat :: *;
import BID :: *;

import RV_Common :: *;
import RV_I :: *;

// RVC 3-bit reg identifier to RVI 5-bit reg identifier
function Bit#(5) regID (Bit#(3) x) = {2'b01, x};
// BitPat guarded variable predicates
function Bool ez (Bit#(n) x) = x == 0;
function Bool eq2 (Bit#(n) x) = x == 2;
function Bool neq (Bit#(n) x, Bit#(n) y) = x != y;
function Bool nez (Bit#(n) x) = x != 0;
function Bool n0_n2 (Bit#(n) x) = x != 0 && x != 2;
function Bool nzimm (Bit#(16) x) = x[12] != 0 || x[6:2] != 0;

`ifdef XLEN32

function Action instrC_Illegal(RVState s) = action
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- c.illegal", s.pc));
  trap(s, IllegalInst, action s.csrs.mtval <= 0; endaction);
endaction;

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
  Bit#(20) i20 = {signExtend(i[10]),i[6],i[8:7],i[4],i[5],i[0],i[9],i[3:1]};
  instrJAL(s, i20[19], i20[9:0], i20[10], i20[18:11], 0);
endaction;

`ifndef XLEN64
  // funct3 = C.JAL = 001
  // op = C1 = 01
  function Action instrC_JAL (RVState s, Bit#(11) i) = action
    Bit#(20) i20 = {signExtend(i[10]),i[6],i[8:7],i[4],i[5],i[0],i[9],i[3:1]};
    instrJAL(s, i20[19], i20[9:0], i20[10], i20[18:11], 1);
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
  Bit#(12) o = signExtend({i7_5[2], i4_0[4:3], i4_0[0], i7_5[1:0], i4_0[2:1]});
  instrBEQ(s, o[11], o[9:4], 0, regID(rs1_), o[3:0], o[10]);
endaction;

// funct3 = C.BNEZ = 111
// op = C1 = 01
function Action instrC_BNEZ (RVState s, Bit#(3) i7_5, Bit#(3) rs1_, Bit#(5) i4_0) = action
  Bit#(12) o = signExtend({i7_5[2], i4_0[4:3], i4_0[0], i7_5[1:0], i4_0[2:1]});
  instrBNE(s, o[11], o[9:4], 0, regID(rs1_), o[3:0], o[10]);
endaction;

////////////////////////////////////////
// Integer Computational Instructions //
////////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////
// Integer Constant-Generation Instructions //
//////////////////////////////////////////////

/*
  CI-type

   15    13   12  11          7 6            2 1  0
  +--------+-----+-------------+--------------+----+
  | funct3 | imm |    rd/rs1   |     imm      | op |
  +--------+-----+-------------+--------------+----+

*/

// funct3 = C.LI = 010
// op = C1 = 01
function Action instrC_LI (RVState s, Bit#(1) imm5, Bit#(5) rd, Bit#(5) imm4_0) =
  instrADDI(s, signExtend({imm5, imm4_0}), 0, rd);

// funct3 = C.LUI = 011
// op = C1 = 01
function Action instrC_LUI (RVState s, Bit#(1) imm5, Bit#(5) rd, Bit#(5) imm4_0) =
  instrLUI(s, signExtend({imm5, imm4_0}), rd);

///////////////////////////////////////////
// Integer Register-Immediate Operations //
///////////////////////////////////////////

/*
  CI-type

   15    13   12  11          7 6            2 1  0
  +--------+-----+-------------+--------------+----+
  | funct3 | imm |    rd/rs1   |     imm      | op |
  +--------+-----+-------------+--------------+----+

*/

// funct3 = C.ADDI = 000
// op = C1 = 01
function Action instrC_ADDI (RVState s, Bit#(1) imm5, Bit#(5) rd, Bit#(5) imm4_0) =
  instrADDI(s, signExtend({imm5, imm4_0}), rd, rd);

// funct3 = C.ADDI16SP = 011
// op = C1 = 01
function Action instrC_ADDI16SP (RVState s, Bit#(1) i5, Bit#(5) rd, Bit#(5) i4_0) =
  instrADDI(s, signExtend({i5, i4_0[2:1], i4_0[3], i4_0[0], i4_0[4], 4'b0000}), 2, 2);

// funct3 = C.SLLI = 000
// op = C2 = 10
function Action instrC_SLLI (RVState s, Bit#(1) i5, Bit#(5) rd, Bit#(5) i4_0) =
  instrSLLI(s, zeroExtend(i4_0), rd, rd);

`ifndef XLEN128
/*
  CIW-type

   15    13 12                     5 4       2 1  0
  +--------+------------------------+---------+----+
  | funct3 |           imm          |   rd'   | op |
  +--------+------------------------+---------+----+

*/

// funct3 = C.ADDI4SPN = 000
// op = C0 = 00
function Action instrC_ADDI4SPN (RVState s, Bit#(8) i, Bit#(3) rd_) =
  instrADDI(s, zeroExtend({i[5:2], i[7:6], i[0], i[1], 2'b00}), 2, regID(rd_));
`endif

/*
  CB-type

   15    13  12  11    10 9        7 6       2 1  0
  +--------+----+--------+----------+---------+----+
  | funct3 | i5 | funct2 | rd'/rs1' |  i4_0   | op |
  +--------+----+--------+----------+---------+----+

*/

// funct3 = C.SRLI = 100
// funct2 = 00
// op = C1 = 01
function Action instrC_SRLI (RVState s, Bit#(1) i5, Bit#(3) rd_, Bit#(5) i4_0) =
  instrSRLI (s, zeroExtend(i4_0), regID(rd_), regID(rd_));

// funct3 = C.SRAI = 100
// funct2 = 01
// op = C1 = 01
function Action instrC_SRAI (RVState s, Bit#(1) i5, Bit#(3) rd_, Bit#(5) i4_0) =
  instrSRAI (s, zeroExtend(i4_0), regID(rd_), regID(rd_));

// funct3 = C.ANDI = 100
// funct2 = 10
// op = C1 = 01
function Action instrC_ANDI (RVState s, Bit#(1) i5, Bit#(3) rd_, Bit#(5) i4_0) =
  instrANDI (s, signExtend({i5, i4_0}), regID(rd_), regID(rd_));

//////////////////////////////////////////
// Integer Register-Register Operations //
//////////////////////////////////////////

/*
  CR-type

   15         12 11           7 6            2 1  0
  +-------------+--------------+--------------+----+
  |    funct4   |    rd/rs1    |     rs2      | op |
  +-------------+--------------+--------------+----+

*/

// funct4 = C.MV = 1000
// op = C2 = 10
function Action instrC_MV (RVState s, Bit#(5) rd, Bit#(5) rs2) =
  instrADD(s, rs2, 0, rd);

// funct4 = C.ADD = 1001
// op = C2 = 10
function Action instrC_ADD (RVState s, Bit#(5) rd, Bit#(5) rs2) =
  instrADD(s, rs2, rd, rd);

/*
  CS-type

   15            10 9        7 6   5 4       2 1  0
  +----------------+----------+-----+---------+----+
  |     funct6     |   rs1'   |funct|   rs2'  | op |
  +----------------+----------+-----+---------+----+

*/

// funct6 = C.AND = 100011
// funct = 11
// op = C1 = 01
function Action instrC_AND (RVState s, Bit#(3) rd_, Bit#(3) rs2_) =
  instrAND(s, regID(rs2_), regID(rd_), regID(rd_));

// funct6 = C.OR = 100011
// funct = 10
// op = C1 = 01
function Action instrC_OR (RVState s, Bit#(3) rd_, Bit#(3) rs2_) =
  instrOR(s, regID(rs2_), regID(rd_), regID(rd_));

// funct6 = C.XOR = 100011
// funct = 01
// op = C1 = 01
function Action instrC_XOR (RVState s, Bit#(3) rd_, Bit#(3) rs2_) =
  instrXOR(s, regID(rs2_), regID(rd_), regID(rd_));

// funct6 = C.SUB = 100011
// funct = 00
// op = C1 = 01
function Action instrC_SUB (RVState s, Bit#(3) rd_, Bit#(3) rs2_) =
  instrSUB(s, regID(rs2_), regID(rd_), regID(rd_));

////////////////////////////////////////////////////////////////////////////////

module [InstrDefModule] mkRV32C#(RVState s) ();

  defineInstr("c.lwsp",             pat(n(3'b010), v, gv(nez), v, v, n(2'b10)), instrC_LWSP(s));
  defineInstr("c.swsp",             pat(n(3'b110), v, v, v, n(2'b10)), instrC_SWSP(s));
  defineInstr("c.lw",               pat(n(3'b010), v, v, v, v, v, n(2'b00)), instrC_LW(s));
  defineInstr("c.sw",               pat(n(3'b110), v, v, v, v, v, n(2'b00)), instrC_SW(s));
  defineInstr("c.j",                pat(n(3'b101), v, n(2'b01)), instrC_J(s));
`ifndef XLEN64
  defineInstr("c.jal",              pat(n(3'b001), v, n(2'b01)), instrC_JAL(s));
`endif
  defineInstr("c.jr",               pat(n(4'b1000), gv(nez), gv(ez), n(2'b10)), instrC_JR(s));
  defineInstr("c.jalr",             pat(n(4'b1001), gv(nez), gv(ez), n(2'b10)), instrC_JALR(s));
  defineInstr("c.beqz",             pat(n(3'b110), v, v, v, n(2'b01)), instrC_BEQZ(s));
  defineInstr("c.bnez",             pat(n(3'b111), v, v, v, n(2'b01)), instrC_BNEZ(s));
  defineInstr("c.li",               pat(n(3'b010), v, gv(nez), v, n(2'b01)), instrC_LI(s));
  defineInstr("c.lui",      guarded(pat(n(3'b011), v, gv(n0_n2), v, n(2'b01)), nzimm), instrC_LUI(s));
  defineInstr("c.addi",     guarded(pat(n(3'b000), v, gv(nez), v, n(2'b01)), nzimm), instrC_ADDI(s));
  defineInstr("c.addi16sp", guarded(pat(n(3'b011), v, gv(eq2), v, n(2'b01)), nzimm), instrC_ADDI16SP(s));
  defineInstr("c.slli",             pat(n(3'b000), gv(ez), gv(nez), v, n(2'b10)), instrC_SLLI(s));
`ifndef XLEN128
  defineInstr("c.addi4spn",         pat(n(3'b000), gv(nez), v, n(2'b00)), instrC_ADDI4SPN(s));
`endif
  defineInstr("c.srli",             pat(n(3'b100), gv(ez), n(2'b00), v, v, n(2'b01)), instrC_SRLI(s));
  defineInstr("c.srai",             pat(n(3'b100), gv(ez), n(2'b01), v, v, n(2'b01)), instrC_SRAI(s));
  defineInstr("c.andi",             pat(n(3'b100), v, n(2'b10), v, v, n(2'b01)), instrC_ANDI(s));
  defineInstr("c.mv",               pat(n(4'b1000), gv(nez), gv(nez), n(2'b10)), instrC_MV(s));
  defineInstr("c.add",              pat(n(4'b1001), gv(nez), gv(nez), n(2'b10)), instrC_ADD(s));
  defineInstr("c.and",              pat(n(6'b100011), v, n(2'b11), v, n(2'b01)), instrC_AND(s));
  defineInstr("c.or",               pat(n(6'b100011), v, n(2'b10), v, n(2'b01)), instrC_OR(s));
  defineInstr("c.xor",              pat(n(6'b100011), v, n(2'b01), v, n(2'b01)), instrC_XOR(s));
  defineInstr("c.sub",              pat(n(6'b100011), v, n(2'b00), v, n(2'b01)), instrC_SUB(s));
  // Defined Illegal Instruction
  defineInstr("illegal",            pat(n(16'h0000)), instrC_Illegal(s));
  // NOP Instruction
  defineInstr("c.nop",              pat(n(3'b000), gv(ez), gv(ez), gv(ez), n(2'b01)), instrC_ADDI(s));
  // Breakpoint Instruction
  defineInstr("c.ebreak",           pat(n(4'b1001), n(10'b0), n(2'b10)), instrEBREAK(s));

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
function List#(Action) instrC_LD (RVState s, Bit#(3) o5_3, Bit#(3) rs1_, Bit#(2) o7_6, Bit#(3) rd_) =
  load(s, LoadArgs{name: "ld",  numBytes: 8, sgnExt: True}, zeroExtend({o7_6, o5_3, 3'b000}), regID(rs1_), regID(rd_));

/*
  CS-type

   15    13 12   10 9        7 6   5 4       2 1  0
  +--------+-------+----------+-----+---------+----+
  | funct3 |  imm  |   rs1'   | imm |   rs2'  | op |
  +--------+-------+----------+-----+---------+----+

*/

// funct3 = C.SD = 111
// op = C0 = 00
function List#(Action) instrC_SD (RVState s, Bit#(3) o5_3, Bit#(3) rs1_, Bit#(2) o7_6, Bit#(3) rs2_) =
  store(s, StrArgs{name: "sd", numBytes: 8}, zeroExtend({o7_6, o5_3[2]}), regID(rs2_), regID(rs1_), {o5_3[1:0], 3'b000});

///////////////////////////////////////////
// Integer Register-Immediate Operations //
///////////////////////////////////////////

/*
  CI-type

   15    13   12  11          7 6            2 1  0
  +--------+-----+-------------+--------------+----+
  | funct3 | imm |    rd/rs1   |     imm      | op |
  +--------+-----+-------------+--------------+----+

*/

// funct3 = C.ADDIW = 001
// op = C1 = 01
function Action instrC_ADDIW (RVState s, Bit#(1) imm5, Bit#(5) rd, Bit#(5) imm4_0) =
  instrADDIW(s, signExtend({imm5, imm4_0}), rd, rd);

//////////////////////////////////////////
// Integer Register-Register Operations //
//////////////////////////////////////////

/*
  CS-type

   15            10 9        7 6   5 4       2 1  0
  +----------------+----------+-----+---------+----+
  |     funct6     |   rs1'   |funct|   rs2'  | op |
  +----------------+----------+-----+---------+----+

*/

// funct6 = C.ADDW = 100111
// funct = 01
// op = C1 = 01
function Action instrC_ADDW (RVState s, Bit#(3) rd_, Bit#(3) rs2_) =
  instrADDW(s, regID(rs2_), regID(rd_), regID(rd_));

// funct6 = C.SUBW = 100111
// funct = 00
// op = C1 = 01
function Action instrC_SUBW (RVState s, Bit#(3) rd_, Bit#(3) rs2_) =
  instrSUBW(s, regID(rs2_), regID(rd_), regID(rd_));

////////////////////////////////////////////////////////////////////////////////

module [InstrDefModule] mkRV64C#(RVState s) ();

  defineInstr("c.ldsp",  pat(n(3'b011), v, gv(neq(0)), v, v, n(2'b10)), instrC_LDSP(s));
  defineInstr("c.sdsp",  pat(n(3'b111), v, v, v, n(2'b10)), instrC_SDSP(s));
  defineInstr("c.ld",    pat(n(3'b011), v, v, v, v, n(2'b00)), instrC_LD(s));
  defineInstr("c.sd",    pat(n(3'b111), v, v, v, v, n(2'b00)), instrC_SD(s));
  defineInstr("c.addiw", pat(n(3'b001), v, gv(nez), v, n(2'b01)), instrC_ADDIW(s));
  defineInstr("c.slli",  pat(n(3'b000), v, gv(nez), v, n(2'b10)), instrC_SLLI(s));
  defineInstr("c.srli",  pat(n(3'b100), v, n(2'b00), v, v, n(2'b01)), instrC_SRLI(s));
  defineInstr("c.srai",  pat(n(3'b100), v, n(2'b01), v, v, n(2'b01)), instrC_SRAI(s));
  defineInstr("c.addw",  pat(n(6'b100111), v, n(2'b01), v, n(2'b01)), instrC_ADDW(s));
  defineInstr("c.subw",  pat(n(6'b100111), v, n(2'b00), v, n(2'b01)), instrC_SUBW(s));

endmodule
`endif // XLEN64
