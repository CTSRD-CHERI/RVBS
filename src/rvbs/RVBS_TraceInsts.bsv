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

import BID :: *;
import BlueUtils :: *;
import RVBS_StateTypes :: *;
import RVBS_BasicTypes :: *;
import RVBS_TraceUtils :: *;

function Action itrace (RVState s, Fmt msg) =
  `ifdef FULL_ITRACE
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- ", s.pc, msg, " -- ", fullReport(s)));
  `else
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- ", s.pc, msg));
  `endif

typeclass LogInst#(type a); a logInst; endtypeclass

instance LogInst#(function Action f(RVState s, Fmt y));
  function logInst(s, msg) = itrace(s, msg);
endinstance

instance LogInst#(function Action f(RVState s, Fmt y, Fmt z));
  function logInst(s, msg0, msg1) = itrace(s, $format(msg0, "  \t--\t", msg1));
endinstance

instance LogInst#(function Action f(RVState s, Fmt y, String z));
  function logInst(s, msg0, msg1) = itrace(s, $format(msg0, "  \t--\t", msg1));
endinstance

/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+

function a patIType (Bit#(3) funct3, Bit#(7) opcode) =
  pat(v, v, n(funct3), v, n(opcode))
// RHS arguments: (Bit#(12) imm11_0, Bit#(5) rs1, Bit#(5) rd)

  I-type - shifts by a constant

   31              25 24              20 19    15 14    12 11     7 6        0
  +------------------+------------------+--------+--------+--------+----------+
  |     imm[11:5]    |     imm[4:0]     |   rs1  | funct3 |   rd   |  opcode  |
  +------------------+------------------+--------+--------+--------+----------+

function a patITypeShamt (Bit#(7) imm_11_5, Bit#(3) funct3, Bit#(7) opcode) =
  pat(n(imm_11_5), v, v, n(funct3), v, n(opcode))
// RHS arguments: (Bit#(7) imm11_5, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd)
*/
function Fmt fmtInstI(String i, Bit#(5) rd, Bit#(5) rs1, Bit#(12) imm) =
  $format(i, "\t", rName(rd), ", ", rName(rs1), ", 0x%0x", imm);
/*
  R-type

   31                        25 24    20 19    15 14    12 11     7 6        0
  +----------------------------+--------+--------+--------+--------+----------+
  |           funct7           |   rs2  |   rs1  | funct3 |   rd   |  opcode  |
  +----------------------------+--------+--------+--------+--------+----------+

function BitPat#() patRType (Bit#(7) funct7, Bit#(3) funct3, Bit#(7) opcode) =
  return pat(n(funct7), v, v, n(funct3), v, n(opcode))
// RHS arguments: (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd)
*/
function Fmt fmtInstR(String i, Bit#(5) rd, Bit#(5) rs1, Bit#(5) rs2) =
  $format(i, "\t", rName(rd), ", ", rName(rs1), ", ", rName(rs2));
/*
  U-type

   31                                                   12 11     7 6        0
  +-------------------------------------------------------+--------+----------+
  |                       imm[31:12]                      |   rd   |  opcode  |
  +-------------------------------------------------------+--------+----------+

function a patUType (Bit#(7) opcode) =
  pat(v, v, n(opcode))
// RHS arguments: (Bit#(20) imm31_12, Bit#(5) rd)
*/
function Fmt fmtInstU(String i, Bit#(5) rd, Bit#(20) imm) =
  $format(i, "\t", rName(rd), ", 0x%0x", imm);
/*
  J-type

     31    30              21    20   19                12 11     7 6        0
  +-------+------------------+-------+--------------------+--------+----------+
  |imm[20]|     imm[10:1]    |imm[11]|     imm[19:12]     |   rd   |  opcode  |
  +-------+------------------+-------+--------------------+--------+----------+

function a patJType (Bit#(7) opcode) =
  pat(v, v, v, v, v, n(opcode))
// RHS arguments: (Bit#(1) imm20, Bit#(10) imm10_1, Bit#(1) imm11, Bit#(8) imm19_12, Bit#(5) rd)
*/
function Fmt fmtInstJ(String i, Bit#(5) rd, Bit#(XLEN) imm) =
  $format(i, "\t", rName(rd), ", 0x%0x", imm);
/*
  B-type

     31    30      25 24    20 19    15 14    12 11       8    7    6        0
  +-------+----------+--------+--------+--------+----------+-------+----------+
  |imm[12]| imm[10:5]|   rs2  |   rs1  | funct3 | imm[4:1] |imm[11]|  opcode  |
  +-------+----------+--------+--------+--------+----------+-------+----------+

function a patBType (Bit#(3) funct3, Bit#(7) opcode) =
  pat(v, v, v, v, n(funct3), v, v, n(opcode))
// RHS arguments: (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11)
*/
function Fmt fmtInstB(String i, Bit#(5) rs1, Bit#(5) rs2, Bit#(XLEN) imm) =
  $format(i, "\t", rName(rs1),", ", rName(rs2), ", 0x%0x", imm);
/*
  S-type

   31                        25 24    20 19    15 14    12 11     7 6        0
  +----------------------------+--------+--------+--------+--------+----------+
  |         imm[11:5]          |   rs2  |   rs1  | funct3 |imm[4:0]|  opcode  |
  +----------------------------+--------+--------+--------+--------+----------+

function a patSType (Bit#(3) funct3, Bit#(7) opcode) =
  pat(v, v, v, n(funct3), v, n(opcode))
// RHS arguments: (Bit#(7) imm11_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) imm4_0)
*/
function Fmt fmtInstS(String i, Bit#(5) rs1, Bit#(5) rs2, Bit#(XLEN) imm) =
  $format(i, "\t", rName(rs1),", ", rName(rs2), ", 0x%0x", imm);

`ifdef RVXCHERI
/*
  Three-op-type

   31                        25 24     20 19    15 14   12 11     7 6        0
  +----------------------------+---------+--------+-------+--------+----------+
  |           funct7           | {r,c}s2 |   cs1  |   0   | {r,c}d |   0x5b   |
  +----------------------------+---------+--------+-------+--------+----------+

*/
function Fmt fmtInstXcheri3op(String i, TraceRegType rd_cd,
                                        TraceRegType cs1,
                                        TraceRegType rs_cs2) =
  $format(i, "\t", traceReg(rd_cd), ", ", traceReg(cs1), ", ", traceReg(rs_cs2));
/*
  SrcDst-type

   31                       25 24    20 19     15 14   12 11     7 6        0
  +---------------------------+--------+---------+-------+--------+----------+
  |            0x7f           |  func  | {r,c}s1 |   0   | {r,c}d |   0x5b   |
  +---------------------------+--------+---------+-------+--------+----------+

*/
function Fmt fmtInstXcheriSrcDst(String i, TraceRegType rd_cd, TraceRegType rs_cs1) =
  $format(i, "\t", traceReg(rd_cd), ", ", traceReg(rs_cs1));
/*
  Store-type

   31                       25 24    20 19      15 14   12 11     7 6        0
  +---------------------------+---------+---------+-------+--------+----------+
  |            0x7c           | {r,c}s2 | {r,c}s1 |   0   |  func  |   0x5b   |
  +---------------------------+---------+---------+-------+--------+----------+

*/
function Fmt fmtInstXcheriStore(String i, TraceRegType rs_cs1, TraceRegType rs_cs2) =
  $format(i, "\t", traceReg(rs_cs2), ", ", traceReg(rs_cs1));
`endif
