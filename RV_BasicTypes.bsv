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

// static parameters
Bool static_HAS_M_MODE = True;

`ifdef SUPERVISOR_MODE
Bool static_HAS_S_MODE = True;
`else
Bool static_HAS_S_MODE = False;
`endif

`ifdef USER_MODE
Bool static_HAS_U_MODE = True;
`else
Bool static_HAS_U_MODE = False;
`endif

Bool static_HAS_I_EXT  = True;

`ifdef RVM
Bool static_HAS_M_EXT  = True;
`else
Bool static_HAS_M_EXT  = False;
`endif

`ifdef RVC
Bool static_HAS_C_EXT  = True;
`else
Bool static_HAS_C_EXT  = False;
`endif

`ifdef RVN
Bool static_HAS_N_EXT  = True;
`else
Bool static_HAS_N_EXT  = False;
`endif

///////////////////////////////////
// Utility modules and functions //
////////////////////////////////////////////////////////////////////////////////

typedef 32 InstSz;

`ifdef XLEN64
typedef 64 XLEN;
typedef 56 PAddrSz;
`else
typedef 32 XLEN;
typedef 34 PAddrSz;
`endif
typedef Bit#(XLEN) VAddr;
typedef Bit#(PAddrSz) PAddr;
function PAddr toPAddr (VAddr addr);
  Bit#(TMax#(PAddrSz, XLEN)) tmp = zeroExtend(addr);
  return truncate(tmp);
endfunction

//TODO for SLL instruction, use something like this:
// typedef TSub#(TLog#(XLEN), 1) BitShAmnt;

// casts between types in the same Bits class
function a cast (b x) provisos (Bits#(a,n), Bits#(b,n)) = unpack(pack(x));

// alignment test
`ifdef RVC
function Bool isInstAligned(Bit#(sz) x) provisos (Add#(1, a__, sz)) = x[0] == 0;
`else
function Bool isInstAligned(Bit#(sz) x) provisos (Add#(2, a__, sz)) = x[1:0] == 0;
`endif

// privilege levels
typedef enum {U = 2'b00, S = 2'b01, Res = 2'b10, M = 2'b11} PrivLvl deriving (Bits, Eq, FShow);
instance Ord#(PrivLvl);
  function Ordering compare(PrivLvl a, PrivLvl b);
    if (a == b) return EQ;
    else if (a == Res) return LT;
    else if (b == Res) return GT;
    else return compare(pack(a), pack(b));
  endfunction
endinstance

// effective XLEN mode
typedef enum {XLUNK = 2'b00, XL32 = 2'b01, XL64 = 2'b10, XL128 = 2'b11} XLMode deriving (Bits, Eq, FShow);
instance Literal#(XLMode);
  function fromInteger (x) = case (x)
    32: XL32;
    64: XL64;
    128: XL128;
    default: XLUNK;
  endcase;
  function inLiteralRange (x, i);
    return (i == 32 || x == 64 || x == 128);
  endfunction
endinstance
`ifdef XLEN64
XLMode nativeXLEN = XL64;
`else
XLMode nativeXLEN = XL32;
`endif

// machine interrupt/exception codes
typedef enum {
  USoftInt = 0, SSoftInt = 1, MSoftInt = 3,
  UtimerInt = 4, STimerInt = 5, MTimerInt = 7,
  UExtInt = 8, SExtInt = 9, MExtInt = 11
} IntCode deriving (Bits, Eq, FShow);
typedef enum {
  InstAddrAlign = 0, InstAccessFault = 1, IllegalInst = 2,
  Breakpoint = 3, LoadAddrAlign = 4, LoadAccessFault = 5,
  StrAMOAddrAlign = 6, StrAMOAccessFault = 7,
  ECallFromU = 8, ECallFromS = 9, ECallFromM = 11,
  InstPgFault = 12, LoadPgFault = 13, StrAMOPgFault = 15
} ExcCode deriving (Bits, Eq, FShow);

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
function Action logInstI(Bit#(XLEN) pc, String i, Bit#(5) rd, Bit#(5) rs1, Bit#(12) imm) =
  printTLogPlusArgs("itrace",
    $format("pc: 0x%0x -- ", pc, i,"\t", rName(rd), ", ", rName(rs1), ", 0x%0x", imm)
  );
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
function Action logInstR(Bit#(XLEN) pc, String i, Bit#(5) rd, Bit#(5) rs1, Bit#(5) rs2) =
  printTLogPlusArgs("itrace",
    $format("pc: 0x%0x -- ", pc, i,"\t", rName(rd), ", ", rName(rs1), ", ", rName(rs2))
  );
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
function Action logInstU(Bit#(XLEN) pc, String i, Bit#(5) rd, Bit#(20) imm) =
  printTLogPlusArgs("itrace",
    $format("pc: 0x%0x -- ", pc, i,"\t", rName(rd), ", 0x%0x", imm)
  );
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
function Action logInstJ(Bit#(XLEN) pc, String i, Bit#(5) rd, Bit#(XLEN) imm) =
  printTLogPlusArgs("itrace",
    $format("pc: 0x%0x -- ", pc, i,"\t", rName(rd), ", 0x%0x", imm)
  );
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
function Action logInstB(Bit#(XLEN) pc, String i, Bit#(5) rs1, Bit#(5) rs2, Bit#(XLEN) imm) =
  printTLogPlusArgs("itrace",
    $format("pc: 0x%0x -- ", pc, i,"\t", rName(rs1), ", ", rName(rs2), ", 0x%0x", imm)
  );
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
function Action logInstS(Bit#(XLEN) pc, String i, Bit#(5) rs1, Bit#(5) rs2, Bit#(XLEN) imm) =
  printTLogPlusArgs("itrace",
    $format("pc: 0x%0x -- ", pc, i,"\t", rName(rs1), ", ", rName(rs2), ", 0x%0x", imm)
  );

// CSRs logging
function Fmt csrName(Bit#(12) idx) = case (idx)
  12'h000: $format("ustatus");
  12'h004: $format("uie");
  12'h005: $format("utvec");
  12'h040: $format("uscratch");
  12'h041: $format("uepc");
  12'h042: $format("ucause");
  12'h043: $format("utval");
  12'h044: $format("uip");
  12'h300: $format("mstatus");
  12'h301: $format("misa");
  12'h302: $format("medeleg");
  12'h303: $format("mideleg");
  12'h304: $format("mie");
  12'h305: $format("mtvec");
  12'h306: $format("mcounteren");
  12'h340: $format("mscratch");
  12'h341: $format("mepc");
  12'h342: $format("mcause");
  12'h343: $format("mtval");
  12'h344: $format("mip");
  12'h3A0, 12'h3A1, 12'h3A2, 12'h3A3: $format("pmpcfg%0d", idx - 12'h3A0);
  12'h3B0, 12'h3B1, 12'h3B2, 12'h3B3, 12'h3B4, 12'h3B5, 12'h3B6, 12'h3B7, 12'h3B8, 12'h3B9, 12'h3BA, 12'h3BB, 12'h3BC, 12'h3BD, 12'h3BE, 12'h3BF:
    $format("pmpaddr%0d", idx - 12'h3B0);
  12'hC00: $format("cycle");
  12'hC01: $format("time");
  12'hC02: $format("insret");
  12'hC03, 12'hC04, 12'hC05, 12'hC06, 12'hC07, 12'hC08, 12'hC09, 12'hC0A, 12'hC0B, 12'hC0C, 12'hC0D, 12'hC0E, 12'hC0F:
    $format("hpmcounter%0d", idx - 12'hC00);
  12'hC80: $format("cycleh");
  12'hC81: $format("timeh");
  12'hC82: $format("insreth");
  12'hC83, 12'hC84, 12'hC85, 12'hC86, 12'hC87, 12'hC88, 12'hC89, 12'hC8A, 12'hC8B, 12'hC8C, 12'hC8D, 12'hC8E, 12'hC8F:
    $format("hpmcounter%0dh", idx - 12'hC80);
  12'hF11: $format("mvendorid");
  12'hF12: $format("marchid");
  12'hF13: $format("mimpid");
  12'hF14: $format("mhartid");
  default: $format("unknown");
endcase;
function Action logInstCSR(Bit#(XLEN) pc, String i, Bit#(5) rd, Bit#(5) rs1, Bit#(12) imm) =
  printTLogPlusArgs("itrace",
    $format("pc: 0x%0x -- ", pc, i,"\t", rName(rd), ", ", rName(rs1), ", 0x%0x", imm, " (", csrName(imm), ")")
  );
