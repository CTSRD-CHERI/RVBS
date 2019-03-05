/*-
 * Copyright (c) 2018-2019 Alexandre Joannou
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

import Vector :: *;
import Printf :: *;

import Recipe :: *;
import BID :: *;
import BlueBasics :: *;
import BlueUtils :: *;
import BitPat :: *;

import RVBS_Types :: *;
import RVBS_Trap :: *;
import RVBS_TraceUtils :: *;
import RVBS_TraceInsts :: *;
import RVBS_MemAccess :: *;

////////////////////////////////////////
// Integer Computational Instructions //
////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////
// Integer Register-Immediate Instructions //
/////////////////////////////////////////////
/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
*/

// funct3 = ADDIW = 000
// opcode = OP-IMM-32 = 0011011
// XXX pseudo-op: SEXT.W
function Action instrADDIW (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, signExtend(s.rGPR(rs1)[31:0] + signExtend(imm)));
  logInst(s, fmtInstI("addiw", rd, rs1, imm));
endaction;

/*
  I-type - shifts by a constant

   31              25 24              20 19    15 14    12 11     7 6        0
  +------------------+------------------+--------+--------+--------+----------+
  |     imm[11:5]    |     imm[4:0]     |   rs1  | funct3 |   rd   |  opcode  |
  +------------------+------------------+--------+--------+--------+----------+
*/

// imm[11:6] = 000000
// funct3 = SLLI = 001
// opcode = OP-IMM = 0010011
function Action instrSLLI64 (RVState s, Bit#(6) imm5_0, Bit#(5) rs1, Bit#(5) rd) = action
  // TODO check MXL and imm[5] for exception in RV32I mode
  s.wGPR(rd, s.rGPR(rs1) << imm5_0);
  logInst(s, fmtInstI("slli", rd, rs1, zeroExtend(imm5_0)));
endaction;

// imm[11:6] = 000000
// funct3 = SRLI = 101
// opcode = OP-IMM = 0010011
function Action instrSRLI64 (RVState s, Bit#(6) imm5_0, Bit#(5) rs1, Bit#(5) rd) = action
  // TODO check MXL and imm[5] for exception in RV32I mode
  s.wGPR(rd, s.rGPR(rs1) >> imm5_0);
  logInst(s, fmtInstI("srli", rd, rs1, zeroExtend(imm5_0)));
endaction;

// imm[11:6] = 010000
// funct3 = SRAI = 101
// opcode = OP-IMM = 0010011
function Action instrSRAI64 (RVState s, Bit#(6) imm5_0, Bit#(5) rs1, Bit#(5) rd) = action
  // TODO check MXL and imm[5] for exception in RV32I mode
  s.wGPR(rd, arithRightShift(s.rGPR(rs1), imm5_0));
  logInst(s, fmtInstI("srai", rd, rs1, zeroExtend(imm5_0)));
endaction;

// imm[11:5] = 0000000
// funct3 = SLLIW = 001
// opcode = OP-IMM-32 = 0011011
function Action instrSLLIW (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, signExtend(s.rGPR(rs1)[31:0] << imm4_0));
  logInst(s, fmtInstI("slliw", rd, rs1, zeroExtend(imm4_0)));
endaction;

// imm[11:5] = 0000000
// funct3 = SRLIW = 101
// opcode = OP-IMM-32 = 0011011
function Action instrSRLIW (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, signExtend(s.rGPR(rs1)[31:0] >> imm4_0));
  logInst(s, fmtInstI("srliw", rd, rs1, zeroExtend(imm4_0)));
endaction;

// imm[11:5] = 0100000
// funct3 = SRAIW = 101
// opcode = OP-IMM-32 = 0011011
function Action instrSRAIW (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, signExtend(arithRightShift(s.rGPR(rs1)[31:0], imm4_0)));
  logInst(s, fmtInstI("sraiw", rd, rs1, zeroExtend(imm4_0)));
endaction;

//////////////////////////////////////////
// Integer Register-Register Operations //
//////////////////////////////////////////
/*
  R-type

   31                        25 24    20 19    15 14    12 11     7 6        0
  +----------------------------+--------+--------+--------+--------+----------+
  |           funct7           |   rs2  |   rs1  | funct3 |   rd   |  opcode  |
  +----------------------------+--------+--------+--------+--------+----------+
*/

// funct7 = 0000000
// funct3 = ADDW = 000
// opcode = OP-32 = 0111011
function Action instrADDW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, signExtend(s.rGPR(rs1)[31:0] + s.rGPR(rs2)[31:0]));
  logInst(s, fmtInstR("addw", rd, rs1, rs2));
endaction;

// funct7 = 0100000
// funct3 = SUBW = 000
// opcode = OP-32 = 0111011
function Action instrSUBW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, signExtend(s.rGPR(rs1)[31:0] - s.rGPR(rs2)[31:0]));
  logInst(s, fmtInstR("subw", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = SLLW = 001
// opcode = OP-32 = 0111011
function Action instrSLLW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(5) shiftAmnt = truncate(s.rGPR(rs2));
  s.wGPR(rd, signExtend(s.rGPR(rs1)[31:0] << shiftAmnt));
  logInst(s, fmtInstR("sllw", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = SRLW = 101
// opcode = OP-32 = 0111011
function Action instrSRLW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(5) shiftAmnt = truncate(s.rGPR(rs2));
  s.wGPR(rd, signExtend(s.rGPR(rs1)[31:0] >> shiftAmnt));
  logInst(s, fmtInstR("srlw", rd, rs1, rs2));
endaction;

// funct7 = 0100000
// funct3 = SRAW = 101
// opcode = OP-32 = 0111011
function Action instrSRAW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(5) shiftAmnt = truncate(s.rGPR(rs2));
  s.wGPR(rd, signExtend(arithRightShift(s.rGPR(rs1)[31:0], shiftAmnt)));
  logInst(s, fmtInstR("sraw", rd, rs1, rs2));
endaction;

module [ISADefModule] mkBase_RV64I#(RVState s) ();

  defineInstEntry("addiw", pat(v, v, n(3'b000), v, n(7'b0011011)), instrADDIW(s));
  defineInstEntry("slli",  pat(n(6'b000000), v, v, n(3'b001), v, n(7'b0010011)), instrSLLI64(s));
  defineInstEntry("srli",  pat(n(6'b000000), v, v, n(3'b101), v, n(7'b0010011)), instrSRLI64(s));
  defineInstEntry("srai",  pat(n(6'b010000), v, v, n(3'b101), v, n(7'b0010011)), instrSRAI64(s));
  defineInstEntry("slliw", pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0011011)), instrSLLIW(s));
  defineInstEntry("srliw", pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0011011)), instrSRLIW(s));
  defineInstEntry("sraiw", pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0011011)), instrSRAIW(s));
  defineInstEntry("addw",  pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0111011)), instrADDW(s));
  defineInstEntry("subw",  pat(n(7'b0100000), v, v, n(3'b000), v, n(7'b0111011)), instrSUBW(s));
  defineInstEntry("sllw",  pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0111011)), instrSLLW(s));
  defineInstEntry("srlw",  pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0111011)), instrSRLW(s));
  defineInstEntry("sraw",  pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0111011)), instrSRAW(s));
  defineInstEntry("lwu",   pat(v, v, n(3'b110), v, n(7'b0000011)), load(s, LoadArgs{name: "lwu", numBytes: 4, sgnExt: False}));
  defineInstEntry("ld",    pat(v, v, n(3'b011), v, n(7'b0000011)), load(s, LoadArgs{name: "ld",  numBytes: 8, sgnExt: True}));
  defineInstEntry("sd",    pat(v, v, v, n(3'b011), v, n(7'b0100011)), store(s, StrArgs{name: "sd", numBytes: 8}));

endmodule
