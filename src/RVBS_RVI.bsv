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
import Printf :: *;

import BID :: *;
import BlueBasics :: *;
import BlueUtils :: *;
import BitPat :: *;

import RVBS_Types :: *;
import RVBS_Trap :: *;
import RVBS_Traces :: *;

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
  s.regFile.r[rd] <= s.regFile.r[rs1] + signExtend(imm);
  logInst(s.pc, fmtInstI("addi", rd, rs1, imm));
endaction;

// funct3 = SLTI = 010
// opcode = OP-IMM = 0010011
function Action instrSLTI (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= signedLT(s.regFile.r[rs1],signExtend(imm)) ? 1 : 0;
  logInst(s.pc, fmtInstI("slti", rd, rs1, imm));
endaction;

// funct3 = SLTIU = 011
// opcode = OP-IMM = 0010011
// XXX pseudo-op: SEQZ
function Action instrSLTIU (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= (s.regFile.r[rs1] < signExtend(imm)) ? 1 : 0;
  logInst(s.pc, fmtInstI("sltiu", rd, rs1, imm));
endaction;

// funct3 = ANDI = 111
// opcode = OP-IMM = 0010011
function Action instrANDI (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= s.regFile.r[rs1] & signExtend(imm);
  logInst(s.pc, fmtInstI("andi", rd, rs1, imm));
endaction;

// funct3 = ORI = 110
// opcode = OP-IMM = 0010011
function Action instrORI (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= s.regFile.r[rs1] | signExtend(imm);
  logInst(s.pc, fmtInstI("ori", rd, rs1, imm));
endaction;

// funct3 = XORI = 100
// opcode = OP-IMM = 0010011
// XXX pseudo-op: NOT
function Action instrXORI (RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= s.regFile.r[rs1] ^ signExtend(imm);
  logInst(s.pc, fmtInstI("xori", rd, rs1, imm));
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
  s.regFile.r[rd] <= s.regFile.r[rs1] << imm4_0;
  logInst(s.pc, fmtInstI("slli", rd, rs1, zeroExtend(imm4_0)));
endaction;

// imm[11:5] = 0000000
// funct3 = SRLI = 101
// opcode = OP-IMM = 0010011
function Action instrSRLI (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= s.regFile.r[rs1] >> imm4_0;
  logInst(s.pc, fmtInstI("srli", rd, rs1, zeroExtend(imm4_0)));
endaction;

// imm[11:5] = 0100000
// funct3 = SRAI = 101
// opcode = OP-IMM = 0010011
function Action instrSRAI (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= arithRightShift(s.regFile.r[rs1], imm4_0);
  logInst(s.pc, fmtInstI("srai", rd, rs1, zeroExtend(imm4_0)));
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
  s.regFile.r[rd] <= signExtend({imm, 12'b0});
  logInst(s.pc, fmtInstU("lui", rd, imm));
endaction;

// opcode = AUIPC = 0010111
function Action instrAUIPC (RVState s, Bit#(20) imm, Bit#(5) rd) = action
  s.regFile.r[rd] <= s.pc + signExtend({imm, 12'b0});
  logInst(s.pc, fmtInstU("auipc", rd, imm));
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
  s.regFile.r[rd] <= s.regFile.r[rs1] + s.regFile.r[rs2];
  logInst(s.pc, fmtInstR("add", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = SLT = 010
// opcode = OP = 0110011
function Action instrSLT (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= (signedLT(s.regFile.r[rs1], s.regFile.r[rs2])) ? 1 : 0;
  logInst(s.pc, fmtInstR("slt", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = SLTU = 011
// opcode = OP = 0110011
function Action instrSLTU (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= (s.regFile.r[rs1] < s.regFile.r[rs2]) ? 1 : 0;
  logInst(s.pc, fmtInstR("sltu", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = AND = 111
// opcode = OP = 0110011
function Action instrAND (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= s.regFile.r[rs1] & s.regFile.r[rs2];
  logInst(s.pc, fmtInstR("and", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = OR = 110
// opcode = OP = 0110011
function Action instrOR (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= s.regFile.r[rs1] | s.regFile.r[rs2];
  logInst(s.pc, fmtInstR("or", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = XOR = 100
// opcode = OP = 0110011
function Action instrXOR (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= s.regFile.r[rs1] ^ s.regFile.r[rs2];
  logInst(s.pc, fmtInstR("xor", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = SLL = 001
// opcode = OP = 0110011
function Action instrSLL (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.regFile.r[rs2]);
  s.regFile.r[rd] <= s.regFile.r[rs1] << shiftAmnt;
  logInst(s.pc, fmtInstR("sll", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = SRL = 101
// opcode = OP = 0110011
function Action instrSRL (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.regFile.r[rs2]);
  s.regFile.r[rd] <= s.regFile.r[rs1] >> shiftAmnt;
  logInst(s.pc, fmtInstR("srl", rd, rs1, rs2));
endaction;

// funct7 = 0100000
// funct3 = SUB = 000
// opcode = OP = 0110011
function Action instrSUB (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= s.regFile.r[rs1] - s.regFile.r[rs2];
  logInst(s.pc, fmtInstR("sub", rd, rs1, rs2));
endaction;

// funct7 = 0100000
// funct3 = SRA = 101
// opcode = OP = 0110011
function Action instrSRA (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.regFile.r[rs2]);
  s.regFile.r[rd] <= arithRightShift(s.regFile.r[rs1], shiftAmnt);
  logInst(s.pc, fmtInstR("sra", rd, rs1, rs2));
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
    s.regFile.r[rd] <= s.pc + s.instByteSz;
  end else trap(s, InstAddrAlign, mtvalWrite(s, tgt));
  logInst(s.pc, fmtInstJ("jal", rd, imm));
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
  Bit#(XLEN) tgt = s.regFile.r[rs1] + signExtend(imm);
  tgt[0] = 0;
  if (isInstAligned(tgt)) begin
    s.pc <= tgt;
    s.regFile.r[rd] <= s.pc + s.instByteSz;
  end else trap(s, InstAddrAlign, mtvalWrite(s, tgt));
  logInst(s.pc, fmtInstI("jalr", rd, rs1, imm));
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
  if (s.regFile.r[rs1] == s.regFile.r[rs2]) branchCommon(s, s.pc + imm);
  logInst(s.pc, fmtInstB("beq", rs1, rs2, imm));
endaction;

// funct3 = BNE = 001
// opcode = 1100011
function Action instrBNE (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (s.regFile.r[rs1] != s.regFile.r[rs2]) branchCommon(s, s.pc + imm);
  logInst(s.pc, fmtInstB("bne", rs1, rs2, imm));
endaction;

// funct3 = BLT = 100
// opcode = 1100011
function Action instrBLT (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (signedLT(s.regFile.r[rs1], s.regFile.r[rs2])) branchCommon(s, s.pc + imm);
  logInst(s.pc, fmtInstB("blt", rs1, rs2, imm));
endaction;

// funct3 = BLTU = 110
// opcode = 1100011
function Action instrBLTU (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (s.regFile.r[rs1] < s.regFile.r[rs2]) branchCommon(s, s.pc + imm);
  logInst(s.pc, fmtInstB("bltu", rs1, rs2, imm));
endaction;

// funct3 = BGE = 101
// opcode = 1100011
function Action instrBGE (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (signedGE(s.regFile.r[rs1], s.regFile.r[rs2])) branchCommon(s, s.pc + imm);
  logInst(s.pc, fmtInstB("bge", rs1, rs2, imm));
endaction;

// funct3 = BGEU = 111
// opcode = 1100011
function Action instrBGEU (RVState s, Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
  Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
  if (s.regFile.r[rs1] >= s.regFile.r[rs2]) branchCommon(s, s.pc + imm);
  logInst(s.pc, fmtInstB("bgeu", rs1, rs2, imm));
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
    VAddr vaddr = s.regFile.r[rs1] + signExtend(imm);
  `ifdef RVFI_DII
    s.mem_addr[0] <= vaddr;
  `endif
  `ifdef SUPERVISOR_MODE
    let req = aReqRead(vaddr, args.numBytes, Invalid);
    s.dvm.sink.put(req);
    itrace(s.pc, fshow(req));
    logInst(s.pc, fmtInstI(args.name, rd, rs1, imm), "vmTranslate lookup step");
  endaction, action
    let rsp <- s.dvm.source.get();
    itrace(s.pc, fshow(rsp));
    PAddr paddr = rsp.addr;
  `else
    PAddr paddr = toPAddr(vaddr);
  `endif
  `ifdef PMP
  `ifdef SUPERVISOR_MODE
    let req = aReqRead(paddr, args.numBytes, rsp.mExc);
  `else
    let req = aReqRead(paddr, args.numBytes, Invalid);
  `endif
    s.dpmp.sink.put(req);
    itrace(s.pc, fshow(req));
    logInst(s.pc, fmtInstI(args.name, rd, rs1, imm), "pmp lookup step");
  endaction, action
    let rsp <- s.dpmp.source.get();
    itrace(s.pc, fshow(rsp));
    MemReq#(PAddr, Bit#(XLEN)) req = tagged ReadReq {addr: rsp.addr, numBytes: fromInteger(args.numBytes)};
  `else
    MemReq#(PAddr, Bit#(XLEN)) req = tagged ReadReq {addr: paddr, numBytes: fromInteger(args.numBytes)};
  `endif
    s.dmem.sink.put(req);
    itrace(s.pc, fshow(req));
    logInst(s.pc, fmtInstI(args.name, rd, rs1, imm), "mem req step");
  endaction, action
    let rsp <- s.dmem.source.get();
    case (rsp) matches
      tagged ReadRsp .r: begin
        Bool isNeg = unpack(r[(args.numBytes*8)-1]);
        Bit#(XLEN) mask = (~0) << args.numBytes*8;
        s.regFile.r[rd] <= (args.sgnExt && isNeg) ? r | mask : r & ~mask;
      end
      tagged BusError: action trap(s, LoadAccessFault); endaction
    endcase
    itrace(s.pc, fshow(rsp));
    logInst(s.pc, fmtInstI(args.name, rd, rs1, imm), "mem rsp step");
  endaction);
// TODO deal with exceptions

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
    VAddr vaddr = s.regFile.r[rs1] + signExtend(imm);
  `ifdef RVFI_DII
    s.mem_addr[0] <= vaddr;
  `endif
  `ifdef SUPERVISOR_MODE
    let req = aReqWrite(vaddr, args.numBytes, Invalid);
    s.dvm.sink.put(req);
    itrace(s.pc, fshow(req));
    logInst(s.pc, fmtInstS(args.name, rs1, rs2, imm), "vmTranslate lookup step");
  endaction, action
    let rsp <- s.dvm.source.get();
    itrace(s.pc, fshow(rsp));
    PAddr paddr = rsp.addr;
  `else
    PAddr paddr = toPAddr(vaddr);
  `endif
  `ifdef PMP
  `ifdef SUPERVISOR_MODE
    let req = aReqWrite(paddr, args.numBytes, rsp.mExc);
  `else
    let req = aReqWrite(paddr, args.numBytes, Invalid);
  `endif
    s.dpmp.sink.put(req);
    itrace(s.pc, fshow(req));
    logInst(s.pc, fmtInstS(args.name, rs1, rs2, imm), "pmp lookup step");
  endaction, action
    let rsp <- s.dpmp.source.get();
    itrace(s.pc, fshow(rsp));
    MemReq#(PAddr, Bit#(XLEN)) req = tagged WriteReq {addr: rsp.addr, byteEnable: ~((~0) << args.numBytes), data: s.regFile.r[rs2]};
  `else
    MemReq#(PAddr, Bit#(XLEN)) req = tagged WriteReq {addr: paddr, byteEnable: ~((~0) << args.numBytes), data: s.regFile.r[rs2]};
  `endif
    s.dmem.sink.put(req);
  `ifdef RVFI_DII
    s.mem_wdata[0] <= req.WriteReq.data;
    s.mem_wmask[0] <= req.WriteReq.byteEnable;
  `endif
    itrace(s.pc, fshow(req));
    logInst(s.pc, fmtInstS(args.name, rs1, rs2, imm), "mem req step");
  endaction, action
    let rsp <- s.dmem.source.get();
    case (rsp) matches
      tagged WriteRsp .w: noAction;
      tagged BusError: action trap(s, StrAMOAccessFault); endaction
    endcase
    itrace(s.pc, fshow(rsp));
    logInst(s.pc, fmtInstS(args.name, rs1, rs2, imm), "mem rsp step");
  endaction);
endfunction
// TODO deal with exceptions

//////////////////
// Memory Model //
////////////////////////////////////////////////////////////////////////////////

// funct3 = FENCE = 000
// opcode = 0001111
function Action instrFENCE(RVState s, Bit#(4) pred, Bit#(4) succ) = action
  //TODO
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- fence 0b%4b, 0b%4b", s.pc, pred, succ));
endaction;

// funct3 = FENCE.I = 001
// opcode = 0001111
function Action instrFENCE_I(RVState s) = action
  //TODO
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- fence.i", s.pc));
endaction;

`ifdef SUPERVISOR_MODE
// funct7 = SFENCE.VMA = 0001001
// funct3 = PRIV = 000
// opcode = SYSTEM = 1110011
function Action instrSFENCE_VMA(RVState s, Bit#(5) rs2, Bit#(5) rs1) = action
  if (s.currentPrivLvl == S && s.csrs.mstatus.tvm) trap(s, IllegalInst);
  //TODO
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- sfence.vma %0d, %0d", s.pc, rs1, rs2));
endaction;
`endif

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
  Bool shouldTrap; // is the csr access authorized?\
  shouldTrap = s.currentPrivLvl < toPrivLvl(r.idx[9:8]); // privilege level access\
  shouldTrap = shouldTrap || (r.rEffects != NOWRITE && r.idx[11:10] == 2'b11); // writes to read-only registers\
  shouldTrap = shouldTrap || (r.idx == 12'h180 && s.currentPrivLvl == S && s.csrs.mstatus.tvm); // satp register accessed with TVM = 1\
  if (shouldTrap) trap(s, IllegalInst);\
  else begin\
    // XXX for some reason, bluespec doesn't like this way to write it:\
    // s.regFile.r[rd] <- s.csrs.req(r);\
    let val <- s.csrs.req(r);\
    s.regFile.r[rd] <= val;\
  end

// funct3 = CSRRW = 001
// opcode = 1110011
// pseudo-op CSRW
function Action instrCSRRW(RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  /* TODO
  `If rd = x0, then the instruction shall not read the CSR and shall not cause any of the side-effects that might occur on a CSR read.`
  Do the write side effect take place ?
  */
  let r = (rd == 0) ? rwCSRReqNoRead(imm, s.regFile.r[rs1]) : rwCSRReq(imm, s.regFile.r[rs1]);
  `instCSRCommon
  //logInst(s.pc, fmtInstI("csrrw", rd, rs1, imm), csrName(imm));
  logInst(s.pc, fmtInstI("csrrw", rd, rs1, imm), $format("rs1 (0x%0x) into ", s.regFile.r[rs1], csrName(imm)));
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
  let r = (rs1 == 0) ? rsCSRReqNoWrite(imm, s.regFile.r[rs1]) : rsCSRReq(imm, s.regFile.r[rs1]);
  `instCSRCommon
  //logInst(s.pc, fmtInstI("csrrs", rd, rs1, imm), csrName(imm));
  logInst(s.pc, fmtInstI("csrrs", rd, rs1, imm), $format("rs1 (0x%0x) into ", s.regFile.r[rs1], csrName(imm)));
endaction;

// funct3 = CSRRC = 011
// opcode = 1110011
function Action instrCSRRC(RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  /* TODO
  `if rs1 = x0, then the instruction will not write to the CSR at all, and so shall not cause any of the side effects that might otherwise occur on a CSR write, such as raising illegal instruction exceptions on accesses to read-only CSR.`
  Do the read side effect take place ?
  */
  let r = (rs1 == 0) ? rcCSRReqNoWrite(imm, s.regFile.r[rs1]) : rcCSRReq(imm, s.regFile.r[rs1]);
  `instCSRCommon
  //logInst(s.pc, fmtInstI("csrrc", rd, rs1, imm), csrName(imm));
  logInst(s.pc, fmtInstI("csrrc", rd, rs1, imm), $format("rs1 (0x%0x) into ", s.regFile.r[rs1], csrName(imm)));
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
  logInst(s.pc, fmtInstI("csrrwi", rd, zimm, imm), csrName(imm));
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
  logInst(s.pc, fmtInstI("csrrsi", rd, zimm, imm), csrName(imm));
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
  logInst(s.pc, fmtInstI("csrrci", rd, zimm, imm), csrName(imm));
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

module [ISADefModule] mkRV32I#(RVState s) ();

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
  defineInstEntry("fence.i", pat(n(4'b0000), n(4'b0000), n(4'b0000), n(5'b00000), n(3'b001), n(5'b00000), n(7'b0001111)), instrFENCE_I(s));
  `ifdef SUPERVISOR_MODE
  defineInstEntry("sfence.vma", pat(n(7'b0001001), v, v, n(3'b000), n(5'b00000), n(7'b1110011)), instrSFENCE_VMA(s));
  `endif
  defineInstEntry("csrrw",   pat(v, v, n(3'b001), v, n(7'b1110011)), instrCSRRW(s));
  defineInstEntry("csrrs",   pat(v, v, n(3'b010), v, n(7'b1110011)), instrCSRRS(s));
  defineInstEntry("csrrc",   pat(v, v, n(3'b011), v, n(7'b1110011)), instrCSRRC(s));
  defineInstEntry("csrrwi",  pat(v, v, n(3'b101), v, n(7'b1110011)), instrCSRRWI(s));
  defineInstEntry("csrrsi",  pat(v, v, n(3'b110), v, n(7'b1110011)), instrCSRRSI(s));
  defineInstEntry("csrrci",  pat(v, v, n(3'b111), v, n(7'b1110011)), instrCSRRCI(s));
  defineInstEntry("ecall",   pat(n(12'b000000000000), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrECALL(s));
  defineInstEntry("ebreak",  pat(n(12'b000000000001), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrEBREAK(s));

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
  s.regFile.r[rd] <= signExtend(s.regFile.r[rs1][31:0] + signExtend(imm));
  logInst(s.pc, fmtInstI("addiw", rd, rs1, imm));
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
  s.regFile.r[rd] <= s.regFile.r[rs1] << imm5_0;
  logInst(s.pc, fmtInstI("slli", rd, rs1, zeroExtend(imm5_0)));
endaction;

// imm[11:6] = 000000
// funct3 = SRLI = 101
// opcode = OP-IMM = 0010011
function Action instrSRLI64 (RVState s, Bit#(6) imm5_0, Bit#(5) rs1, Bit#(5) rd) = action
  // TODO check MXL and imm[5] for exception in RV32I mode
  s.regFile.r[rd] <= s.regFile.r[rs1] >> imm5_0;
  logInst(s.pc, fmtInstI("srli", rd, rs1, zeroExtend(imm5_0)));
endaction;

// imm[11:6] = 010000
// funct3 = SRAI = 101
// opcode = OP-IMM = 0010011
function Action instrSRAI64 (RVState s, Bit#(6) imm5_0, Bit#(5) rs1, Bit#(5) rd) = action
  // TODO check MXL and imm[5] for exception in RV32I mode
  s.regFile.r[rd] <= arithRightShift(s.regFile.r[rs1], imm5_0);
  logInst(s.pc, fmtInstI("srai", rd, rs1, zeroExtend(imm5_0)));
endaction;

// imm[11:5] = 0000000
// funct3 = SLLIW = 001
// opcode = OP-IMM-32 = 0011011
function Action instrSLLIW (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= signExtend(s.regFile.r[rs1][31:0] << imm4_0);
  logInst(s.pc, fmtInstI("slliw", rd, rs1, zeroExtend(imm4_0)));
endaction;

// imm[11:5] = 0000000
// funct3 = SRLIW = 101
// opcode = OP-IMM-32 = 0011011
function Action instrSRLIW (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= signExtend(s.regFile.r[rs1][31:0] >> imm4_0);
  logInst(s.pc, fmtInstI("srliw", rd, rs1, zeroExtend(imm4_0)));
endaction;

// imm[11:5] = 0100000
// funct3 = SRAIW = 101
// opcode = OP-IMM-32 = 0011011
function Action instrSRAIW (RVState s, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= signExtend(arithRightShift(s.regFile.r[rs1][31:0], imm4_0));
  logInst(s.pc, fmtInstI("sraiw", rd, rs1, zeroExtend(imm4_0)));
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
  s.regFile.r[rd] <= signExtend(s.regFile.r[rs1][31:0] + s.regFile.r[rs2][31:0]);
  logInst(s.pc, fmtInstR("addw", rd, rs1, rs2));
endaction;

// funct7 = 0100000
// funct3 = SUBW = 000
// opcode = OP-32 = 0111011
function Action instrSUBW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile.r[rd] <= signExtend(s.regFile.r[rs1][31:0] - s.regFile.r[rs2][31:0]);
  logInst(s.pc, fmtInstR("subw", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = SLLW = 001
// opcode = OP-32 = 0111011
function Action instrSLLW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(5) shiftAmnt = truncate(s.regFile.r[rs2]);
  s.regFile.r[rd] <= signExtend(s.regFile.r[rs1][31:0] << shiftAmnt);
  logInst(s.pc, fmtInstR("sllw", rd, rs1, rs2));
endaction;

// funct7 = 0000000
// funct3 = SRLW = 101
// opcode = OP-32 = 0111011
function Action instrSRLW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(5) shiftAmnt = truncate(s.regFile.r[rs2]);
  s.regFile.r[rd] <= signExtend(s.regFile.r[rs1][31:0] >> shiftAmnt);
  logInst(s.pc, fmtInstR("srlw", rd, rs1, rs2));
endaction;

// funct7 = 0100000
// funct3 = SRAW = 101
// opcode = OP-32 = 0111011
function Action instrSRAW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(5) shiftAmnt = truncate(s.regFile.r[rs2]);
  s.regFile.r[rd] <= signExtend(arithRightShift(s.regFile.r[rs1][31:0], shiftAmnt));
  logInst(s.pc, fmtInstR("sraw", rd, rs1, rs2));
endaction;

module [ISADefModule] mkRV64I#(RVState s) ();

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
`endif // XLEN64
