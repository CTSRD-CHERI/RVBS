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

/////////////////////////
// Unknown Instruction //
////////////////////////////////////////////////////////////////////////////////

function Action unknownInst(RVState s, Bit#(32) inst) = action
  Bit#(XLEN) mask = ~((~0) << (s.instByteSz<<3));
  raiseException(s, IllegalInst, mask & zeroExtend(inst));
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- UNKNOWN INSTRUCTION 0x%0x", s.pc, inst));
endaction;

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

// funct3 = ADDI = 000
// opcode = OP-IMM = 0010011
// XXX pseudo-op: MV, NOP
function Action instrADDI (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, s.rGPR(rs1) + signExtend(imm));
  logInst(s, fmtInstI("addi", rd, rs1, imm));
endaction;

// funct3 = SLTI = 010
// opcode = OP-IMM = 0010011
function Action instrSLTI (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, signedLT(s.rGPR(rs1),signExtend(imm)) ? 1 : 0);
  logInst(s, fmtInstI("slti", rd, rs1, imm));
endaction;

// funct3 = SLTIU = 011
// opcode = OP-IMM = 0010011
// XXX pseudo-op: SEQZ
function Action instrSLTIU (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, (s.rGPR(rs1) < signExtend(imm)) ? 1 : 0);
  logInst(s, fmtInstI("sltiu", rd, rs1, imm));
endaction;

// funct3 = ANDI = 111
// opcode = OP-IMM = 0010011
function Action instrANDI (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, s.rGPR(rs1) & signExtend(imm));
  logInst(s, fmtInstI("andi", rd, rs1, imm));
endaction;

// funct3 = ORI = 110
// opcode = OP-IMM = 0010011
function Action instrORI (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, s.rGPR(rs1) | signExtend(imm));
  logInst(s, fmtInstI("ori", rd, rs1, imm));
endaction;

// funct3 = XORI = 100
// opcode = OP-IMM = 0010011
// XXX pseudo-op: NOT
function Action instrXORI (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, s.rGPR(rs1) ^ signExtend(imm));
  logInst(s, fmtInstI("xori", rd, rs1, imm));
endaction;

/*
  I-type - shifts by a constant

   31              25 24              20 19    15 14    12 11     7 6        0
  +------------------+------------------+--------+--------+--------+----------+
  |     imm[11:5]    |     imm[4:0]     |   rs1  | funct3 |   rd   |  opcode  |
  +------------------+------------------+--------+--------+--------+----------+
*/

// imm[11:5] = 0000000
// funct3 = SLLI = 001
// opcode = OP-IMM = 0010011
function Action instrSLLI (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, s.rGPR(rs1) << imm4_0);
  logInst(s, fmtInstI("slli", rd, rs1, zeroExtend(imm4_0)));
endaction;

// imm[11:5] = 0000000
// funct3 = SRLI = 101
// opcode = OP-IMM = 0010011
function Action instrSRLI (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, s.rGPR(rs1) >> imm4_0);
  logInst(s, fmtInstI("srli", rd, rs1, zeroExtend(imm4_0)));
endaction;

// imm[11:5] = 0100000
// funct3 = SRAI = 101
// opcode = OP-IMM = 0010011
function Action instrSRAI (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, arithRightShift(s.rGPR(rs1), imm4_0));
  logInst(s, fmtInstI("srai", rd, rs1, zeroExtend(imm4_0)));
endaction;

/*
  U-type

   31                                                   12 11     7 6        0
  +-------------------------------------------------------+--------+----------+
  |                       imm[31:12]                      |   rd   |  opcode  |
  +-------------------------------------------------------+--------+----------+
*/

// opcode = LUI = 0110111
function Action instrLUI (RVState s, Bit#(20) imm, Bit#(5) rd) = action
  s.wGPR(rd, signExtend({imm, 12'b0}));
  logInst(s, fmtInstU("lui", rd, imm));
endaction;

// opcode = AUIPC = 0010111
function Action instrAUIPC (RVState s, Bit#(20) imm, Bit#(5) rd) = action
  s.wGPR(rd, s.pc + signExtend({imm, 12'b0}));
  logInst(s, fmtInstU("auipc", rd, imm));
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
// funct3 = ADD = 000
// opcode = OP = 0110011
function Action instrADD (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, s.rGPR(rs1) + s.rGPR(rs2));
  logInst(s, fmtInstR("add", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = SLT = 010
// opcode = OP = 0110011
function Action instrSLT (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, (signedLT(s.rGPR(rs1), s.rGPR(rs2))) ? 1 : 0);
  logInst(s, fmtInstR("slt", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = SLTU = 011
// opcode = OP = 0110011
function Action instrSLTU (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, (s.rGPR(rs1) < s.rGPR(rs2)) ? 1 : 0);
  logInst(s, fmtInstR("sltu", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = AND = 111
// opcode = OP = 0110011
function Action instrAND (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, s.rGPR(rs1) & s.rGPR(rs2));
  logInst(s, fmtInstR("and", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = OR = 110
// opcode = OP = 0110011
function Action instrOR (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, s.rGPR(rs1) | s.rGPR(rs2));
  logInst(s, fmtInstR("or", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = XOR = 100
// opcode = OP = 0110011
function Action instrXOR (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, s.rGPR(rs1) ^ s.rGPR(rs2));
  logInst(s, fmtInstR("xor", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = SLL = 001
// opcode = OP = 0110011
function Action instrSLL (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.rGPR(rs2));
  s.wGPR(rd, s.rGPR(rs1) << shiftAmnt);
  logInst(s, fmtInstR("sll", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = SRL = 101
// opcode = OP = 0110011
function Action instrSRL (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.rGPR(rs2));
  s.wGPR(rd, s.rGPR(rs1) >> shiftAmnt);
  logInst(s, fmtInstR("srl", rd, rs1, rs2));
endaction;

// funct7 = 0100000
// funct3 = SUB = 000
// opcode = OP = 0110011
function Action instrSUB (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.wGPR(rd, s.rGPR(rs1) - s.rGPR(rs2));
  logInst(s, fmtInstR("sub", rd, rs1, rs2));
endaction;

// funct7 = 0100000
// funct3 = SRA = 101
// opcode = OP = 0110011
function Action instrSRA (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.rGPR(rs2));
  s.wGPR(rd, arithRightShift(s.rGPR(rs1), shiftAmnt));
  logInst(s, fmtInstR("sra", rd, rs1, rs2));
endaction;

///////////////////////////////////
// Control Transfer Instructions //
////////////////////////////////////////////////////////////////////////////////

/////////////////////////
// Unconditional Jumps //
/////////////////////////
/*
  J-type

     31    30              21    20   19                12 11     7 6        0
  +-------+------------------+-------+--------------------+--------+----------+
  |imm[20]|     imm[10:1]    |imm[11]|     imm[19:12]     |   rd   |  opcode  |
  +-------+------------------+-------+--------------------+--------+----------+
*/

// opcode = JAL = 1101111
function Action instrJAL(RVState s, Bit#(1) imm20, Bit#(10) imm10_1, Bit#(1) imm11, Bit#(8) imm19_12, Bit#(5) rd) = action
  Bit#(XLEN) imm = {signExtend(imm20),imm19_12,imm11,imm10_1,1'b0};
  Bit#(XLEN) tgt = s.pc + imm;
  if (isInstAligned(tgt)) begin
    s.pc <= tgt;
    s.wGPR(rd, s.pc + s.instByteSz);
  end else raiseException(s, InstAddrAlign, tgt);
  logInst(s, fmtInstJ("jal", rd, imm));
endaction;

/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
*/

// funct3 = 000
// opcode = JALR = 1100111
function Action instrJALR (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(XLEN) tgt = s.rGPR(rs1) + signExtend(imm);
  tgt[0] = 0;
  if (isInstAligned(tgt)) begin
    s.pc <= tgt;
    s.wGPR(rd, s.pc + s.instByteSz);
  end else raiseException(s, InstAddrAlign, tgt);
  logInst(s, fmtInstI("jalr", rd, rs1, imm));
endaction;

//////////////////////////
// Conditional Branches //
//////////////////////////

/*
  B-type

     31    30      25 24    20 19    15 14    12 11       8    7    6        0
  +-------+----------+--------+--------+--------+----------+-------+----------+
  |imm[12]| imm[10:5]|   rs2  |   rs1  | funct3 | imm[4:1] |imm[11]|  opcode  |
  +-------+----------+--------+--------+--------+----------+-------+----------+
*/

// Note from the RISC-V ISA document:
// BGT, BGTU, BLE, and BLEU can be synthesized by reversing the operands
// to BLT, BLTU, BGE, and BGEU, respectivelly.

function Action branchCommon(RVState s, Bit#(XLEN) tgt) = action
  if (isInstAligned(tgt)) s.pc <= tgt; else raiseException(s, InstAddrAlign, tgt);
endaction;

// funct3 = BEQ = 000
// opcode = 1100011
function Action instrBEQ (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (s.rGPR(rs1) == s.rGPR(rs2)) branchCommon(s, s.pc + imm);
  logInst(s, fmtInstB("beq", rs1, rs2, imm));
endaction;

// funct3 = BNE = 001
// opcode = 1100011
function Action instrBNE (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (s.rGPR(rs1) != s.rGPR(rs2)) branchCommon(s, s.pc + imm);
  logInst(s, fmtInstB("bne", rs1, rs2, imm));
endaction;

// funct3 = BLT = 100
// opcode = 1100011
function Action instrBLT (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (signedLT(s.rGPR(rs1), s.rGPR(rs2))) branchCommon(s, s.pc + imm);
  logInst(s, fmtInstB("blt", rs1, rs2, imm));
endaction;

// funct3 = BLTU = 110
// opcode = 1100011
function Action instrBLTU (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (s.rGPR(rs1) < s.rGPR(rs2)) branchCommon(s, s.pc + imm);
  logInst(s, fmtInstB("bltu", rs1, rs2, imm));
endaction;

// funct3 = BGE = 101
// opcode = 1100011
function Action instrBGE (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (signedGE(s.rGPR(rs1), s.rGPR(rs2))) branchCommon(s, s.pc + imm);
  logInst(s, fmtInstB("bge", rs1, rs2, imm));
endaction;

// funct3 = BGEU = 111
// opcode = 1100011
function Action instrBGEU (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (s.rGPR(rs1) >= s.rGPR(rs2)) branchCommon(s, s.pc + imm);
  logInst(s, fmtInstB("bgeu", rs1, rs2, imm));
endaction;

//////////////////
// Memory Model //
////////////////////////////////////////////////////////////////////////////////

// funct3 = FENCE = 000
// opcode = 0001111
function Action instrFENCE(RVState s, Bit#(4) pred, Bit#(4) succ) = action
  //TODO
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- fence 0b%4b, 0b%4b", s.pc, pred, succ));
endaction;

`ifdef SUPERVISOR_MODE
// funct7 = SFENCE.VMA = 0001001
// funct3 = PRIV = 000
// opcode = SYSTEM = 1110011
function Action instrSFENCE_VMA(RVState s, Bit#(5) rs2, Bit#(5) rs1) = action
  if (s.currentPrivLvl == S && s.csrs.mstatus.tvm) raiseException(s, IllegalInst);
  //TODO
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- sfence.vma %0d, %0d", s.pc, rs1, rs2));
endaction;
`endif

//////////////////////////////////////
// Environment Call and Breakpoints //
////////////////////////////////////////////////////////////////////////////////

// ECALL
function Action instrECALL(RVState s) = action
  ExcCode code = case (s.currentPrivLvl)
    U: ECallFromU;
    S: ECallFromS;
    M: ECallFromM;
  endcase;
  raiseException(s, code);
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- ecall", s.pc));
endaction;

// EBREAK
function Action instrEBREAK(RVState s) = action
  raiseException(s, Breakpoint);
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- ebreak", s.pc));
endaction;

module [ISADefModule] mkBase_RV32I#(RVState s) ();

  defineUnkInstEntry(unknownInst(s));
  defineInstEntry("addi",    pat(v, v, n(3'b000), v, n(7'b0010011)), instrADDI(s));
  defineInstEntry("slti",    pat(v, v, n(3'b010), v, n(7'b0010011)), instrSLTI(s));
  defineInstEntry("sltiu",   pat(v, v, n(3'b011), v, n(7'b0010011)), instrSLTIU(s));
  defineInstEntry("andi",    pat(v, v, n(3'b111), v, n(7'b0010011)), instrANDI(s));
  defineInstEntry("ori",     pat(v, v, n(3'b110), v, n(7'b0010011)), instrORI(s));
  defineInstEntry("xori",    pat(v, v, n(3'b100), v, n(7'b0010011)), instrXORI(s));
  defineInstEntry("slli",    pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0010011)), instrSLLI(s));
  defineInstEntry("srli",    pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0010011)), instrSRLI(s));
  defineInstEntry("srai",    pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0010011)), instrSRAI(s));
  defineInstEntry("lui",     pat(v, v, n(7'b0110111)), instrLUI(s));
  defineInstEntry("auipc",   pat(v, v, n(7'b0010111)), instrAUIPC(s));
  defineInstEntry("add",     pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0110011)), instrADD(s));
  defineInstEntry("slt",     pat(n(7'b0000000), v, v, n(3'b010), v, n(7'b0110011)), instrSLT(s));
  defineInstEntry("sltu",    pat(n(7'b0000000), v, v, n(3'b011), v, n(7'b0110011)), instrSLTU(s));
  defineInstEntry("and",     pat(n(7'b0000000), v, v, n(3'b111), v, n(7'b0110011)), instrAND(s));
  defineInstEntry("or",      pat(n(7'b0000000), v, v, n(3'b110), v, n(7'b0110011)), instrOR(s));
  defineInstEntry("xor",     pat(n(7'b0000000), v, v, n(3'b100), v, n(7'b0110011)), instrXOR(s));
  defineInstEntry("sll",     pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0110011)), instrSLL(s));
  defineInstEntry("srl",     pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0110011)), instrSRL(s));
  defineInstEntry("sub",     pat(n(7'b0100000), v, v, n(3'b000), v, n(7'b0110011)), instrSUB(s));
  defineInstEntry("sra",     pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0110011)), instrSRA(s));
  defineInstEntry("jal",     pat(v, v, v, v, v, n(7'b1101111)),instrJAL(s));
  defineInstEntry("jalr",    pat(v, v, n(3'b000), v, n(7'b1100111)), instrJALR(s));
  defineInstEntry("beq",     pat(v, v, v, v, n(3'b000), v, v, n(7'b1100011)), instrBEQ(s));
  defineInstEntry("bne",     pat(v, v, v, v, n(3'b001), v, v, n(7'b1100011)), instrBNE(s));
  defineInstEntry("blt",     pat(v, v, v, v, n(3'b100), v, v, n(7'b1100011)), instrBLT(s));
  defineInstEntry("bltu",    pat(v, v, v, v, n(3'b110), v, v, n(7'b1100011)), instrBLTU(s));
  defineInstEntry("bge",     pat(v, v, v, v, n(3'b101), v, v, n(7'b1100011)), instrBGE(s));
  defineInstEntry("bgeu",    pat(v, v, v, v, n(3'b111), v, v, n(7'b1100011)), instrBGEU(s));
  defineInstEntry("lb",      pat(v, v, n(3'b000), v, n(7'b0000011)), load(s, LoadArgs{name: "lb",  numBytes: 1, sgnExt: True}));
  defineInstEntry("lbu",     pat(v, v, n(3'b100), v, n(7'b0000011)), load(s, LoadArgs{name: "lbu", numBytes: 1, sgnExt: False}));
  defineInstEntry("lh",      pat(v, v, n(3'b001), v, n(7'b0000011)), load(s, LoadArgs{name: "lh",  numBytes: 2, sgnExt: True}));
  defineInstEntry("lhu",     pat(v, v, n(3'b101), v, n(7'b0000011)), load(s, LoadArgs{name: "lhu", numBytes: 2, sgnExt: False}));
  defineInstEntry("lw",      pat(v, v, n(3'b010), v, n(7'b0000011)), load(s, LoadArgs{name: "lw",  numBytes: 4, sgnExt: True}));
  defineInstEntry("sb",      pat(v, v, v, n(3'b000), v, n(7'b0100011)), store(s, StrArgs{name: "sb", numBytes: 1}));
  defineInstEntry("sh",      pat(v, v, v, n(3'b001), v, n(7'b0100011)), store(s, StrArgs{name: "sh", numBytes: 2}));
  defineInstEntry("sw",      pat(v, v, v, n(3'b010), v, n(7'b0100011)), store(s, StrArgs{name: "sw", numBytes: 4}));
  defineInstEntry("fence",   pat(n(4'b0000), v, v, n(5'b00000), n(3'b000), n(5'b00000), n(7'b0001111)), instrFENCE(s));
  `ifdef SUPERVISOR_MODE
  defineInstEntry("sfence.vma", pat(n(7'b0001001), v, v, n(3'b000), n(5'b00000), n(7'b1110011)), instrSFENCE_VMA(s));
  `endif
  defineInstEntry("ecall",   pat(n(12'b000000000000), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrECALL(s));
  defineInstEntry("ebreak",  pat(n(12'b000000000001), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrEBREAK(s));

endmodule
