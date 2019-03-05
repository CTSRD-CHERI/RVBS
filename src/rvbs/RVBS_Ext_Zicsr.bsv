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
  if (shouldTrap) raiseException(s, IllegalInst);\
  else begin\
    // XXX for some reason, bluespec doesn't like this way to write it:\
    // s.rGPR(rd) <- s.csrs.req(r);\
    let val <- s.csrs.req(r);\
    s.wGPR(rd, val);\
  end

// funct3 = CSRRW = 001
// opcode = 1110011
// pseudo-op CSRW
function Action instrCSRRW(RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  /* TODO
  `If rd = x0, then the instruction shall not read the CSR and shall not cause any of the side-effects that might occur on a CSR read.`
  Do the write side effect take place ?
  */
  let r = (rd == 0) ? rwCSRReqNoRead(imm, s.rGPR(rs1)) : rwCSRReq(imm, s.rGPR(rs1));
  `instCSRCommon
  //logInst(s, fmtInstI("csrrw", rd, rs1, imm), csrName(imm));
  logInst(s, fmtInstI("csrrw", rd, rs1, imm), $format("rs1 (0x%0x) into ", s.rGPR(rs1), csrName(imm)));
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
  let r = (rs1 == 0) ? rsCSRReqNoWrite(imm, s.rGPR(rs1)) : rsCSRReq(imm, s.rGPR(rs1));
  `instCSRCommon
  //logInst(s, fmtInstI("csrrs", rd, rs1, imm), csrName(imm));
  logInst(s, fmtInstI("csrrs", rd, rs1, imm), $format("rs1 (0x%0x) into ", s.rGPR(rs1), csrName(imm)));
endaction;

// funct3 = CSRRC = 011
// opcode = 1110011
function Action instrCSRRC(RVState s, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
  /* TODO
  `if rs1 = x0, then the instruction will not write to the CSR at all, and so shall not cause any of the side effects that might otherwise occur on a CSR write, such as raising illegal instruction exceptions on accesses to read-only CSR.`
  Do the read side effect take place ?
  */
  let r = (rs1 == 0) ? rcCSRReqNoWrite(imm, s.rGPR(rs1)) : rcCSRReq(imm, s.rGPR(rs1));
  `instCSRCommon
  //logInst(s, fmtInstI("csrrc", rd, rs1, imm), csrName(imm));
  logInst(s, fmtInstI("csrrc", rd, rs1, imm), $format("rs1 (0x%0x) into ", s.rGPR(rs1), csrName(imm)));
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
  logInst(s, fmtInstI("csrrwi", rd, zimm, imm), csrName(imm));
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
  logInst(s, fmtInstI("csrrsi", rd, zimm, imm), csrName(imm));
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
  logInst(s, fmtInstI("csrrci", rd, zimm, imm), csrName(imm));
endaction;

`undef instCSRCommon

module [ISADefModule] mkExt_Zicsr#(RVState s) ();

  defineInstEntry("csrrw",   pat(v, v, n(3'b001), v, n(7'b1110011)), instrCSRRW(s));
  defineInstEntry("csrrs",   pat(v, v, n(3'b010), v, n(7'b1110011)), instrCSRRS(s));
  defineInstEntry("csrrc",   pat(v, v, n(3'b011), v, n(7'b1110011)), instrCSRRC(s));
  defineInstEntry("csrrwi",  pat(v, v, n(3'b101), v, n(7'b1110011)), instrCSRRWI(s));
  defineInstEntry("csrrsi",  pat(v, v, n(3'b110), v, n(7'b1110011)), instrCSRRSI(s));
  defineInstEntry("csrrci",  pat(v, v, n(3'b111), v, n(7'b1110011)), instrCSRRCI(s));

endmodule
