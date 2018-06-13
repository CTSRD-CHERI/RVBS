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
import RV_BasicTypes :: *;

///////////////////
// Logging utils //
////////////////////////////////////////////////////////////////////////////////

// pretty printing and logging utils
function Fmt gpRegName(Bit#(5) r) = $format("x%0d", r);
function Fmt abiRegName(Bit#(5) r) = case (r)
  0: $format("zero");
  1: $format("ra");
  2: $format("sp");
  3: $format("gp");
  4: $format("tp");
  5, 6, 7: $format("t%0d", r - 5);
  8: $format("s0/fp");
  9: $format("s1");
  10, 11, 12, 13, 14, 15, 16, 17: $format("a%0d", r - 10);
  18, 19, 20, 21, 22, 23, 24, 25, 26, 27: $format("s%0d", r - 16);
  28, 29, 30, 31: $format("t%0d", r - 25);
endcase;
`ifdef PRINT_ABI_REG_NAME
function Fmt rName(Bit#(5) r) = abiRegName(r);
`else
function Fmt rName(Bit#(5) r) = gpRegName(r);
`endif

function Action itrace (Bit#(XLEN) pc, Fmt msg) =
  printTLogPlusArgs("itrace", $format("pc: 0x%0x -- ", pc, msg));

typeclass LogInst#(type a); a logInst; endtypeclass

instance LogInst#(function Action f(Bit#(XLEN) x, Fmt y));
  function logInst(pc, msg) = itrace(pc, msg);
endinstance

instance LogInst#(function Action f(Bit#(XLEN) x, Fmt y, Fmt z));
  function logInst(pc, msg0, msg1) = itrace(pc, $format(msg0, " -- ", msg1));
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

// CSRs logging
function Fmt csrName(Bit#(12) idx) = case (idx)
  `ifdef USER_MODE
  /*
  12'h000: $format("ustatus");
  12'h004: $format("uie");
  12'h005: $format("utvec");
  12'h040: $format("uscratch");
  12'h041: $format("uepc");
  12'h042: $format("ucause");
  12'h043: $format("utval");
  12'h044: $format("uip");
  */
  `endif
  //12'h001: $format("fflags");
  //12'h002: $format("frm");
  //12'h003: $format("fcsr");
  `ifdef SUPERVISOR_MODE
  12'h100: $format("sstatus");
  12'h102: $format("sedeleg");
  12'h103: $format("sideleg");
  12'h104: $format("sie");
  12'h105: $format("stvec");
  //12'h106: $format("scounteren");
  12'h140: $format("sscratch");
  12'h141: $format("sepc");
  12'h142: $format("scause");
  12'h143: $format("stval");
  12'h144: $format("sip");
  12'h180: $format("satp");
  `endif
  12'h300: $format("mstatus");
  12'h301: $format("misa");
  12'h302: $format("medeleg");
  12'h303: $format("mideleg");
  12'h304: $format("mie");
  12'h305: $format("mtvec");
  //12'h306: $format("mcounteren");
  //12'h323, 12'h324: // TODO through to 12'h33F
  //  $format("mhpmevent%0d", idx - 12'h320);
  12'h340: $format("mscratch");
  12'h341: $format("mepc");
  12'h342: $format("mcause");
  12'h343: $format("mtval");
  12'h344: $format("mip");
  12'h3A0, 12'h3A1, 12'h3A2, 12'h3A3:
    $format("pmpcfg%0d", idx - 12'h3A0);
  12'h3B0, 12'h3B1, 12'h3B2, 12'h3B3, 12'h3B4, 12'h3B5, 12'h3B6, 12'h3B7, 12'h3B8, 12'h3B9, 12'h3BA, 12'h3BB, 12'h3BC, 12'h3BD, 12'h3BE, 12'h3BF:
    $format("pmpaddr%0d", idx - 12'h3B0);
  //12'7A0: $format("tselect");
  //12'7A1: $format("tdata1");
  //12'7A2: $format("tdata2");
  //12'7A3: $format("tdata3");
  //12'7B0: $format("dcsr");
  //12'7B1: $format("dpc");
  //12'7B2: $format("dscratch");
  //12'hB00: $format("mcycle");
  //12'hB02: $format("minstret");
  //12'hB03, 12'hB04, 12'hB05, 12'hB06, 12'hB07, 12'hB08, 12'hB09, 12'hB0A, 12'hB0B, 12'hB0C, 12'hB0D, 12'hB0E, 12'hB0F: // TODO through to 12'hB1F
  //  $format("mhpmcounter%0d", idx - 12'hB00);
  `ifndef XLEN64
  //12'hB80: $format("mcycleh");
  //12'hB82: $format("minsreth");
  //12'hB83, 12'hB84, 12'hB85, 12'hB86, 12'hB87, 12'hB88, 12'hB89, 12'hB8A, 12'hB8B, 12'hB8C, 12'hB8D, 12'hB8E, 12'hB8F: // TODO through to 12'hB9F
  //  $format("hpmcounter%0dh", idx - 12'hB80);
  `endif
  12'hC00: $format("cycle");
  //12'hC01: $format("time");
  //12'hC02: $format("insret");
  //12'hC03, 12'hC04, 12'hC05, 12'hC06, 12'hC07, 12'hC08, 12'hC09, 12'hC0A, 12'hC0B, 12'hC0C, 12'hC0D, 12'hC0E, 12'hC0F: // TODO through to 12'hC1F
  //  $format("hpmcounter%0d", idx - 12'hC00);
  `ifndef XLEN64
  //12'hC80: $format("cycleh");
  //12'hC81: $format("timeh");
  //12'hC82: $format("insreth");
  //12'hC83, 12'hC84, 12'hC85, 12'hC86, 12'hC87, 12'hC88, 12'hC89, 12'hC8A, 12'hC8B, 12'hC8C, 12'hC8D, 12'hC8E, 12'hC8F: // TODO through to 12'hC9F
  //  $format("hpmcounter%0dh", idx - 12'hC80);
  `endif
  12'hF11: $format("mvendorid");
  12'hF12: $format("marchid");
  12'hF13: $format("mimpid");
  12'hF14: $format("mhartid");
  default: $format("unknown");
endcase;
