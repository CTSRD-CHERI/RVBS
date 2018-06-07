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

import Vector :: *;
import BitPat :: *;
import Printf :: *;
import BID :: *;

import RV_Common :: *;

`ifdef XLEN32
/////////////////////////
// Unknown Instruction //
////////////////////////////////////////////////////////////////////////////////

function Action mtvalWrite(RVState s, Bit#(XLEN) val) = action
  s.csrs.mtval <= val;
endaction;

function Action unknownInst(RVState s, Bit#(32) inst) = action
  Bit#(XLEN) mask = ~((~0) << (s.instByteSz<<3));
  trap(s, IllegalInst, mtvalWrite(s, mask & zeroExtend(inst)));
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
  s.regFile[rd] <= s.regFile[rs1] + signExtend(imm);
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "addi", rd, rs1, imm);
endaction;

// funct3 = SLTI = 010
// opcode = OP-IMM = 0010011
function Action instrSLTI (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= signedLT(s.regFile[rs1],signExtend(imm)) ? 1 : 0;
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "slti", rd, rs1, imm);
endaction;

// funct3 = SLTIU = 011
// opcode = OP-IMM = 0010011
// XXX pseudo-op: SEQZ
function Action instrSLTIU (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= (s.regFile[rs1] < signExtend(imm)) ? 1 : 0;
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "sltiu", rd, rs1, imm);
endaction;

// funct3 = ANDI = 111
// opcode = OP-IMM = 0010011
function Action instrANDI (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= s.regFile[rs1] & signExtend(imm);
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "andi", rd, rs1, imm);
endaction;

// funct3 = ORI = 110
// opcode = OP-IMM = 0010011
function Action instrORI (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= s.regFile[rs1] | signExtend(imm);
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "ori", rd, rs1, imm);
endaction;

// funct3 = XORI = 100
// opcode = OP-IMM = 0010011
// XXX pseudo-op: NOT
function Action instrXORI (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= s.regFile[rs1] ^ signExtend(imm);
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "xori", rd, rs1, imm);
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
  s.regFile[rd] <= s.regFile[rs1] << imm4_0;
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "slli", rd, rs1, zeroExtend(imm4_0));
endaction;

// imm[11:5] = 0000000
// funct3 = SRLI = 101
// opcode = OP-IMM = 0010011
function Action instrSRLI (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= s.regFile[rs1] >> imm4_0;
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "srli", rd, rs1, zeroExtend(imm4_0));
endaction;

// imm[11:5] = 0100000
// funct3 = SRAI = 101
// opcode = OP-IMM = 0010011
function Action instrSRAI (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= arithRightShift(s.regFile[rs1], imm4_0);
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "srai", rd, rs1, zeroExtend(imm4_0));
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
  s.regFile[rd] <= signExtend({imm, 12'b0});
  s.pc <= s.pc + s.instByteSz;
  logInstU(s.pc, "lui", rd, imm);
endaction;

// opcode = AUIPC = 0010111
function Action instrAUIPC (RVState s, Bit#(20) imm, Bit#(5) rd) = action
  s.regFile[rd] <= s.pc + signExtend({imm, 12'b0});
  s.pc <= s.pc + s.instByteSz;
  logInstU(s.pc, "auipc", rd, imm);
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
  s.regFile[rd] <= s.regFile[rs1] + s.regFile[rs2];
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "add", rd, rs1, rs2);
endaction;

// funct7 = 0000000
// funct3 = SLT = 010
// opcode = OP = 0110011
function Action instrSLT (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= (signedLT(s.regFile[rs1], s.regFile[rs2])) ? 1 : 0;
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "slt", rd, rs1, rs2);
endaction;

// funct7 = 0000000
// funct3 = SLTU = 011
// opcode = OP = 0110011
function Action instrSLTU (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= (s.regFile[rs1] < s.regFile[rs2]) ? 1 : 0;
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "sltu", rd, rs1, rs2);
endaction;

// funct7 = 0000000
// funct3 = AND = 111
// opcode = OP = 0110011
function Action instrAND (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= s.regFile[rs1] & s.regFile[rs2];
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "and", rd, rs1, rs2);
endaction;

// funct7 = 0000000
// funct3 = OR = 110
// opcode = OP = 0110011
function Action instrOR (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= s.regFile[rs1] | s.regFile[rs2];
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "or", rd, rs1, rs2);
endaction;

// funct7 = 0000000
// funct3 = XOR = 100
// opcode = OP = 0110011
function Action instrXOR (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= s.regFile[rs1] ^ s.regFile[rs2];
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "xor", rd, rs1, rs2);
endaction;

// funct7 = 0000000
// funct3 = SLL = 001
// opcode = OP = 0110011
function Action instrSLL (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.regFile[rs2]);
  s.regFile[rd] <= s.regFile[rs1] << shiftAmnt;
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "sll", rd, rs1, rs2);
endaction;

// funct7 = 0000000
// funct3 = SRL = 101
// opcode = OP = 0110011
function Action instrSRL (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.regFile[rs2]);
  s.regFile[rd] <= s.regFile[rs1] >> shiftAmnt;
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "srl", rd, rs1, rs2);
endaction;

// funct7 = 0100000
// funct3 = SUB = 000
// opcode = OP = 0110011
function Action instrSUB (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= s.regFile[rs1] - s.regFile[rs2];
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "sub", rd, rs1, rs2);
endaction;

// funct7 = 0100000
// funct3 = SRA = 101
// opcode = OP = 0110011
function Action instrSRA (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.regFile[rs2]);
  s.regFile[rd] <= arithRightShift(s.regFile[rs1], shiftAmnt);
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "sra", rd, rs1, rs2);
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
    s.regFile[rd] <= s.pc + s.instByteSz;
  end else trap(s, InstAddrAlign, mtvalWrite(s, tgt));
  logInstJ(s.pc, "jal", rd, imm);
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
  Bit#(XLEN) tgt = s.regFile[rs1] + signExtend(imm);
  tgt[0] = 0;
  if (isInstAligned(tgt)) begin
    s.pc <= tgt;
    s.regFile[rd] <= s.pc + s.instByteSz;
  end else trap(s, InstAddrAlign, mtvalWrite(s, tgt));
  logInstI(s.pc, "jalr", rd, rs1, imm);
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
  if (isInstAligned(tgt)) s.pc <= tgt; else trap(s, InstAddrAlign, mtvalWrite(s, tgt));
endaction;

// funct3 = BEQ = 000
// opcode = 1100011
function Action instrBEQ (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (s.regFile[rs1] == s.regFile[rs2]) branchCommon(s, s.pc + imm); else s.pc <= s.pc + s.instByteSz;
  logInstB(s.pc, "beq", rs1, rs2, imm);
endaction;

// funct3 = BNE = 001
// opcode = 1100011
function Action instrBNE (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (s.regFile[rs1] != s.regFile[rs2]) branchCommon(s, s.pc + imm); else s.pc <= s.pc + s.instByteSz;
  logInstB(s.pc, "bne", rs1, rs2, imm);
endaction;

// funct3 = BLT = 100
// opcode = 1100011
function Action instrBLT (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (signedLT(s.regFile[rs1], s.regFile[rs2])) branchCommon(s, s.pc + imm); else s.pc <= s.pc + s.instByteSz;
  logInstB(s.pc, "blt", rs1, rs2, imm);
endaction;

// funct3 = BLTU = 110
// opcode = 1100011
function Action instrBLTU (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (s.regFile[rs1] < s.regFile[rs2]) branchCommon(s, s.pc + imm); else s.pc <= s.pc + s.instByteSz;
  logInstB(s.pc, "bltu", rs1, rs2, imm);
endaction;

// funct3 = BGE = 101
// opcode = 1100011
function Action instrBGE (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (signedGE(s.regFile[rs1], s.regFile[rs2])) branchCommon(s, s.pc + imm); else s.pc <= s.pc + s.instByteSz;
  logInstB(s.pc, "bge", rs1, rs2, imm);
endaction;

// funct3 = BGEU = 111
// opcode = 1100011
function Action instrBGEU (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (s.regFile[rs1] >= s.regFile[rs2]) branchCommon(s, s.pc + imm); else s.pc <= s.pc + s.instByteSz;
  logInstB(s.pc, "bgeu", rs1, rs2, imm);
endaction;

/////////////////////////////////
// Load and Store Instructions //
////////////////////////////////////////////////////////////////////////////////

/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
*/
typedef struct { String name; Integer numBytes; Bool sgnExt; } LoadArgs;
function List#(Action) load(RVState s, LoadArgs args, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = list(
  action
    Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
  `ifdef PMP
    PMPReq req = PMPReq{addr: toPAddr(addr), numBytes: fromInteger(args.numBytes), reqType: READ};
    s.pmp.lookup[1].put(req);
    printTLogPlusArgs("itrace", fshow(req));
    logInstI(s.pc, sprintf("%s (pmp lookup step)", args.name), rd, rs1, imm);
  endaction, action
    PMPRsp rsp <- s.pmp.lookup[1].get();
    MemReq#(PAddr, Bit#(XLEN)) req = tagged ReadReq {addr: rsp.addr, numBytes: fromInteger(args.numBytes)};
    printTLogPlusArgs("itrace", fshow(rsp));
  `else
    MemReq#(PAddr, Bit#(XLEN)) req = tagged ReadReq {addr: toPAddr(addr), numBytes: fromInteger(args.numBytes)};
  `endif
    s.dmem.sendReq(req);
    printTLogPlusArgs("itrace", fshow(req));
    logInstI(s.pc, sprintf("%s (mem req step)", args.name), rd, rs1, imm);
  endaction, action
    let rsp <- s.dmem.getRsp();
    case (rsp) matches
      tagged ReadRsp .r: begin
        Bool isNeg = unpack(r[(args.numBytes*8)-1]);
        Bit#(XLEN) mask = (~0) << args.numBytes*8;
        s.regFile[rd] <= (args.sgnExt && isNeg) ? r | mask : r & ~mask;
      end
    endcase
    s.pc <= s.pc + s.instByteSz;
    printTLogPlusArgs("itrace", fshow(rsp));
    logInstI(s.pc, sprintf("%s (mem rsp step)", args.name), rd, rs1, imm);
  endaction);

/*
  S-type

   31                        25 24    20 19    15 14    12 11     7 6        0
  +----------------------------+--------+--------+--------+--------+----------+
  |         imm[11:5]          |   rs2  |   rs1  | funct3 |imm[4:0]|  opcode  |
  +----------------------------+--------+--------+--------+--------+----------+
*/
typedef struct { String name; Integer numBytes; } StrArgs;
function List#(Action) store(RVState s, StrArgs args, Bit#(7) imm11_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) imm4_0);
  Bit#(XLEN) imm = {signExtend(imm11_5), imm4_0};
  return list(action
    Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
  `ifdef PMP
    PMPReq req = PMPReq{addr: toPAddr(addr), numBytes: fromInteger(args.numBytes), reqType: WRITE};
    s.pmp.lookup[1].put(req);
    printTLogPlusArgs("itrace", fshow(req));
    logInstS(s.pc, sprintf("%s (pmp lookup step)", args.name), rs1, rs2, imm);
  endaction, action
    PMPRsp rsp <- s.pmp.lookup[1].get();
    printTLogPlusArgs("itrace", fshow(rsp));
    MemReq#(PAddr, Bit#(XLEN)) req = tagged WriteReq {addr: rsp.addr, byteEnable: ~((~0) << args.numBytes), data: s.regFile[rs2]};
  `else
    MemReq#(PAddr, Bit#(XLEN)) req = tagged WriteReq {addr: toPAddr(addr), byteEnable: ~((~0) << args.numBytes), data: s.regFile[rs2]};
  `endif
    s.dmem.sendReq(req);
    s.pc <= s.pc + s.instByteSz;
    printTLogPlusArgs("itrace", fshow(req));
    logInstS(s.pc, sprintf("%s (mem req step)", args.name), rs1, rs2, imm);
  endaction);
endfunction

//////////////////
// Memory Model //
////////////////////////////////////////////////////////////////////////////////

// funct3 = FENCE = 000
// opcode = 0001111
function Action instrFENCE(RVState s, Bit#(4) pred, Bit#(4) succ) = action
  //TODO
  s.pc <= s.pc + s.instByteSz;
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- fence 0b%4b, 0b%4b", s.pc, pred, succ));
endaction;

// funct3 = FENCE.I = 001
// opcode = 0001111
function Action instrFENCE_I(RVState s) = action
  //TODO
  s.pc <= s.pc + s.instByteSz;
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- fence.i", s.pc));
endaction;

//////////////////////////////////////////////
// Control and Status Register Instructions //
////////////////////////////////////////////////////////////////////////////////

/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
*/

`define instCSRCommon\
  if (s.currentPrivLvl >= toPrivLvl(imm[9:8])) begin\
    // XXX for some reason, bluespec doesn't like this way to write it:\
    // s.regFile[rd] <- s.csrs.req(r);\
    let val <- s.csrs.req(r);\
    s.regFile[rd] <= val;\
    s.pc <= s.pc + s.instByteSz;\
  end else trap(s, IllegalInst);

// funct3 = CSRRW = 001
// opcode = 1110011
// pseudo-op CSRW
function Action instrCSRRW(RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  /* TODO
  `If rd = x0, then the instruction shall not read the CSR and shall not cause any of the side-effects that might occur on a CSR read.`
  Do the write side effect take place ?
  */
  let r = (rd == 0) ? rwCSRReqNoRead(imm, s.regFile[rs1]) : rwCSRReq(imm, s.regFile[rs1]);
  `instCSRCommon
  logInstCSR(s.pc, "csrrw", rd, rs1, imm);
endaction;

// funct3 = CSRRS = 010
// opcode = 1110011
// pseudo-op CSRR
// XXX RDCYCLE[H], RDTIME[H], RDINSTRET[H]
function Action instrCSRRS(RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  /* TODO
  `if rs1 = x0, then the instruction will not write to the CSR at all, and so shall not cause any of the side effects that might otherwise occur on a CSR write, such as raising illegal instruction exceptions on accesses to read-only CSR.`
  Do the read side effect take place ?
  */
  let r = (rs1 == 0) ? rsCSRReqNoWrite(imm, s.regFile[rs1]) : rsCSRReq(imm, s.regFile[rs1]);
  `instCSRCommon
  logInstCSR(s.pc, "csrrs", rd, rs1, imm);
endaction;

// funct3 = CSRRC = 011
// opcode = 1110011
function Action instrCSRRC(RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  /* TODO
  `if rs1 = x0, then the instruction will not write to the CSR at all, and so shall not cause any of the side effects that might otherwise occur on a CSR write, such as raising illegal instruction exceptions on accesses to read-only CSR.`
  Do the read side effect take place ?
  */
  let r = (rs1 == 0) ? rcCSRReqNoWrite(imm, s.regFile[rs1]) : rcCSRReq(imm, s.regFile[rs1]);
  `instCSRCommon
  logInstCSR(s.pc, "csrrc", rd, rs1, imm);
endaction;

// funct3 = CSRRWI = 101
// opcode = 1110011
function Action instrCSRRWI(RVState s, Bit#(12) imm, Bit#(5) zimm, Bit#(5) rd) = action
  /* TODO
  `if rd = x0, then the instruction shall not read the CSR and shall not cause any of the side-effects that might occur on a CSR read.`
  Do the write side effect take place ?
  */
  let r = (rd == 0) ? rwCSRReqNoRead(imm, zeroExtend(zimm)) : rwCSRReq(imm, zeroExtend(zimm));
  `instCSRCommon
  logInstCSR(s.pc, "csrrwi", rd, zimm, imm);
endaction;

// funct3 = CSRRSI = 110
// opcode = 1110011
function Action instrCSRRSI(RVState s, Bit#(12) imm, Bit#(5) zimm, Bit#(5) rd) = action
  /* TODO
  `if the the uimm[4:0] (zimm) field is zero, then these instructions will not write to the CSR, and shall not cause any of the side effects that might otherwise occur on a CSR write.`
  Do the read side effect take place ?
  */
  let r = (zimm == 0) ? rsCSRReqNoWrite(imm, zeroExtend(zimm)) : rsCSRReq(imm, zeroExtend(zimm));
  `instCSRCommon
  logInstCSR(s.pc, "csrrsi", rd, zimm, imm);
endaction;

// funct3 = CSRRCI = 111
// opcode = 1110011
function Action instrCSRRCI(RVState s, Bit#(12) imm, Bit#(5) zimm, Bit#(5) rd) = action
  /* TODO
  `if the the uimm[4:0] (zimm) field is zero, then these instructions will not write to the CSR, and shall not cause any of the side effects that might otherwise occur on a CSR write.`
  Do the read side effect take place ?
  */
  let r = (zimm == 0) ? rcCSRReqNoWrite(imm, zeroExtend(zimm)) : rcCSRReq(imm, zeroExtend(zimm));
  `instCSRCommon
  logInstCSR(s.pc, "csrrci", rd, zimm, imm);
endaction;

`undef instCSRCommon

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
  trap(s, code);
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- ecall", s.pc));
endaction;

// EBREAK
function Action instrEBREAK(RVState s) = action
  trap(s, Breakpoint);
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- ebreak", s.pc));
endaction;

module [InstrDefModule] mkRV32I#(RVState s) ();

  defineUnkInstr(unknownInst(s));
  defineInstr("addi",    pat(v, v, n(3'b000), v, n(7'b0010011)), instrADDI(s));
  defineInstr("slti",    pat(v, v, n(3'b010), v, n(7'b0010011)), instrSLTI(s));
  defineInstr("sltiu",   pat(v, v, n(3'b011), v, n(7'b0010011)), instrSLTIU(s));
  defineInstr("andi",    pat(v, v, n(3'b111), v, n(7'b0010011)), instrANDI(s));
  defineInstr("ori",     pat(v, v, n(3'b110), v, n(7'b0010011)), instrORI(s));
  defineInstr("xori",    pat(v, v, n(3'b100), v, n(7'b0010011)), instrXORI(s));
  defineInstr("slli",    pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0010011)), instrSLLI(s));
  defineInstr("srli",    pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0010011)), instrSRLI(s));
  defineInstr("srai",    pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0010011)), instrSRAI(s));
  defineInstr("lui",     pat(v, v, n(7'b0110111)), instrLUI(s));
  defineInstr("auipc",   pat(v, v, n(7'b0010111)), instrAUIPC(s));
  defineInstr("add",     pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0110011)), instrADD(s));
  defineInstr("slt",     pat(n(7'b0000000), v, v, n(3'b010), v, n(7'b0110011)), instrSLT(s));
  defineInstr("sltu",    pat(n(7'b0000000), v, v, n(3'b011), v, n(7'b0110011)), instrSLTU(s));
  defineInstr("and",     pat(n(7'b0000000), v, v, n(3'b111), v, n(7'b0110011)), instrAND(s));
  defineInstr("or",      pat(n(7'b0000000), v, v, n(3'b110), v, n(7'b0110011)), instrOR(s));
  defineInstr("xor",     pat(n(7'b0000000), v, v, n(3'b100), v, n(7'b0110011)), instrXOR(s));
  defineInstr("sll",     pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0110011)), instrSLL(s));
  defineInstr("srl",     pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0110011)), instrSRL(s));
  defineInstr("sub",     pat(n(7'b0100000), v, v, n(3'b000), v, n(7'b0110011)), instrSUB(s));
  defineInstr("sra",     pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0110011)), instrSRA(s));
  defineInstr("jal",     pat(v, v, v, v, v, n(7'b1101111)),instrJAL(s));
  defineInstr("jalr",    pat(v, v, n(3'b000), v, n(7'b1100111)), instrJALR(s));
  defineInstr("beq",     pat(v, v, v, v, n(3'b000), v, v, n(7'b1100011)), instrBEQ(s));
  defineInstr("bne",     pat(v, v, v, v, n(3'b001), v, v, n(7'b1100011)), instrBNE(s));
  defineInstr("blt",     pat(v, v, v, v, n(3'b100), v, v, n(7'b1100011)), instrBLT(s));
  defineInstr("bltu",    pat(v, v, v, v, n(3'b110), v, v, n(7'b1100011)), instrBLTU(s));
  defineInstr("bge",     pat(v, v, v, v, n(3'b101), v, v, n(7'b1100011)), instrBGE(s));
  defineInstr("bgeu",    pat(v, v, v, v, n(3'b111), v, v, n(7'b1100011)), instrBGEU(s));
  defineInstr("lb",      pat(v, v, n(3'b000), v, n(7'b0000011)), load(s, LoadArgs{name: "lb",  numBytes: 1, sgnExt: True}));
  defineInstr("lbu",     pat(v, v, n(3'b100), v, n(7'b0000011)), load(s, LoadArgs{name: "lbu", numBytes: 1, sgnExt: False}));
  defineInstr("lh",      pat(v, v, n(3'b001), v, n(7'b0000011)), load(s, LoadArgs{name: "lh",  numBytes: 2, sgnExt: True}));
  defineInstr("lhu",     pat(v, v, n(3'b101), v, n(7'b0000011)), load(s, LoadArgs{name: "lhu", numBytes: 2, sgnExt: False}));
  defineInstr("lw",      pat(v, v, n(3'b010), v, n(7'b0000011)), load(s, LoadArgs{name: "lw",  numBytes: 4, sgnExt: True}));
  defineInstr("sb",      pat(v, v, v, n(3'b000), v, n(7'b0100011)), store(s, StrArgs{name: "sb", numBytes: 1}));
  defineInstr("sh",      pat(v, v, v, n(3'b001), v, n(7'b0100011)), store(s, StrArgs{name: "sh", numBytes: 2}));
  defineInstr("sw",      pat(v, v, v, n(3'b010), v, n(7'b0100011)), store(s, StrArgs{name: "sw", numBytes: 4}));
  defineInstr("fence",   pat(n(4'b0000), v, v, n(5'b00000), n(3'b000), n(5'b00000), n(7'b0001111)), instrFENCE(s));
  defineInstr("fence.i", pat(n(4'b0000), n(4'b0000), n(4'b0000), n(5'b00000), n(3'b001), n(5'b00000), n(7'b0001111)), instrFENCE_I(s));
  defineInstr("csrrw",   pat(v, v, n(3'b001), v, n(7'b1110011)), instrCSRRW(s));
  defineInstr("csrrs",   pat(v, v, n(3'b010), v, n(7'b1110011)), instrCSRRS(s));
  defineInstr("csrrc",   pat(v, v, n(3'b011), v, n(7'b1110011)), instrCSRRC(s));
  defineInstr("csrrwi",  pat(v, v, n(3'b101), v, n(7'b1110011)), instrCSRRWI(s));
  defineInstr("csrrsi",  pat(v, v, n(3'b110), v, n(7'b1110011)), instrCSRRSI(s));
  defineInstr("csrrci",  pat(v, v, n(3'b111), v, n(7'b1110011)), instrCSRRCI(s));
  defineInstr("ecall",   pat(n(12'b000000000000), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrECALL(s));
  defineInstr("ebreak",  pat(n(12'b000000000001), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrEBREAK(s));

endmodule
`endif // XLEN32

////////////////////////////////////////////////////////////////////////////////

`ifdef XLEN64

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
  s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] + signExtend(imm));
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "addiw", rd, rs1, imm);
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
  s.regFile[rd] <= s.regFile[rs1] << imm5_0;
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "slli", rd, rs1, zeroExtend(imm5_0));
endaction;

// imm[11:6] = 000000
// funct3 = SRLI = 101
// opcode = OP-IMM = 0010011
function Action instrSRLI64 (RVState s, Bit#(6) imm5_0, Bit#(5) rs1, Bit#(5) rd) = action
  // TODO check MXL and imm[5] for exception in RV32I mode
  s.regFile[rd] <= s.regFile[rs1] >> imm5_0;
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "srli", rd, rs1, zeroExtend(imm5_0));
endaction;

// imm[11:6] = 010000
// funct3 = SRAI = 101
// opcode = OP-IMM = 0010011
function Action instrSRAI64 (RVState s, Bit#(6) imm5_0, Bit#(5) rs1, Bit#(5) rd) = action
  // TODO check MXL and imm[5] for exception in RV32I mode
  s.regFile[rd] <= arithRightShift(s.regFile[rs1], imm5_0);
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "srai", rd, rs1, zeroExtend(imm5_0));
endaction;

// imm[11:5] = 0000000
// funct3 = SLLIW = 001
// opcode = OP-IMM-32 = 0011011
function Action instrSLLIW (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] << imm4_0);
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "slliw", rd, rs1, zeroExtend(imm4_0));
endaction;

// imm[11:5] = 0000000
// funct3 = SRLIW = 101
// opcode = OP-IMM-32 = 0011011
function Action instrSRLIW (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] >> imm4_0);
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "srliw", rd, rs1, zeroExtend(imm4_0));
endaction;

// imm[11:5] = 0100000
// funct3 = SRAIW = 101
// opcode = OP-IMM-32 = 0011011
function Action instrSRAIW (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= signExtend(arithRightShift(s.regFile[rs1][31:0], imm4_0));
  s.pc <= s.pc + s.instByteSz;
  logInstI(s.pc, "sraiw", rd, rs1, zeroExtend(imm4_0));
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
  s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] + s.regFile[rs2][31:0]);
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "addw", rd, rs1, rs2);
endaction;

// funct7 = 0100000
// funct3 = SUBW = 000
// opcode = OP-32 = 0111011
function Action instrSUBW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] - s.regFile[rs2][31:0]);
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "subw", rd, rs1, rs2);
endaction;

// funct7 = 0000000
// funct3 = SLLW = 001
// opcode = OP-32 = 0111011
function Action instrSLLW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(5) shiftAmnt = truncate(s.regFile[rs2]);
  s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] << shiftAmnt);
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "sllw", rd, rs1, rs2);
endaction;

// funct7 = 0000000
// funct3 = SRLW = 101
// opcode = OP-32 = 0111011
function Action instrSRLW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(5) shiftAmnt = truncate(s.regFile[rs2]);
  s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] >> shiftAmnt);
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "srlw", rd, rs1, rs2);
endaction;

// funct7 = 0100000
// funct3 = SRAW = 101
// opcode = OP-32 = 0111011
function Action instrSRAW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(5) shiftAmnt = truncate(s.regFile[rs2]);
  s.regFile[rd] <= signExtend(arithRightShift(s.regFile[rs1][31:0], shiftAmnt));
  s.pc <= s.pc + s.instByteSz;
  logInstR(s.pc, "sraw", rd, rs1, rs2);
endaction;

module [InstrDefModule] mkRV64I#(RVState s) ();

  defineInstr("addiw", pat(v, v, n(3'b000), v, n(7'b0011011)), instrADDIW(s));
  defineInstr("slli",  pat(n(6'b000000), v, v, n(3'b001), v, n(7'b0010011)), instrSLLI64(s));
  defineInstr("srli",  pat(n(6'b000000), v, v, n(3'b101), v, n(7'b0010011)), instrSRLI64(s));
  defineInstr("srai",  pat(n(6'b010000), v, v, n(3'b101), v, n(7'b0010011)), instrSRAI64(s));
  defineInstr("slliw", pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0011011)), instrSLLIW(s));
  defineInstr("srliw", pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0011011)), instrSRLIW(s));
  defineInstr("sraiw", pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0011011)), instrSRAIW(s));
  defineInstr("addw",  pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0111011)), instrADDW(s));
  defineInstr("subw",  pat(n(7'b0100000), v, v, n(3'b000), v, n(7'b0111011)), instrSUBW(s));
  defineInstr("sllw",  pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0111011)), instrSLLW(s));
  defineInstr("srlw",  pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0111011)), instrSRLW(s));
  defineInstr("sraw",  pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0111011)), instrSRAW(s));
  defineInstr("lwu",   pat(v, v, n(3'b110), v, n(7'b0000011)), load(s, LoadArgs{name: "lwu", numBytes: 4, sgnExt: False}));
  defineInstr("ld",    pat(v, v, n(3'b011), v, n(7'b0000011)), load(s, LoadArgs{name: "ld",  numBytes: 8, sgnExt: True}));
  defineInstr("sd",    pat(v, v, v, n(3'b011), v, n(7'b0100011)), store(s, StrArgs{name: "sd", numBytes: 8}));

endmodule
`endif // XLEN64
