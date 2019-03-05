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

import Printf :: *;
import List :: *;

import BID :: *;
import Recipe :: *;
import BlueUtils :: *;
import BitPat :: *;

import RVBS_Types :: *;
import RVBS_Trap :: *;
import RVBS_Base_RV64I :: *;
import RVBS_MemAccess :: *;

// RVC 3-bit reg identifier to RVI 5-bit reg identifier
function Bit#(5) regID (Bit#(3) x) = {2'b01, x};
// BitPat guarded variable predicates
function Bool ez (Bit#(n) x) = x == 0;
function Bool eq2 (Bit#(n) x) = x == 2;
function Bool neq (Bit#(n) x, Bit#(n) y) = x != y;
function Bool nez (Bit#(n) x) = x != 0;
function Bool n0_n2 (Bit#(n) x) = x != 0 && x != 2;
function Bool nzimm (Bit#(16) x) = x[12] != 0 || x[6:2] != 0;

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
function Recipe instrC_LDSP (RVState s, Bit#(1) imm5, Bit#(5) rd, Bit#(2) imm4_3, Bit#(3) imm8_6) =
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
function Recipe instrC_SDSP (RVState s, Bit#(3) imm5_3, Bit#(3) imm8_6, Bit#(5) rs2) =
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
function Recipe instrC_LD (RVState s, Bit#(3) o5_3, Bit#(3) rs1_, Bit#(2) o7_6, Bit#(3) rd_) =
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
function Recipe instrC_SD (RVState s, Bit#(3) o5_3, Bit#(3) rs1_, Bit#(2) o7_6, Bit#(3) rs2_) =
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

// funct3 = C.SLLI = 000
// op = C2 = 10
function Action instrC_SLLI64 (RVState s, Bit#(1) i5, Bit#(5) rd, Bit#(5) i4_0) =
  instrSLLI64(s, zeroExtend({i5, i4_0}), rd, rd);

// funct3 = C.SRLI = 100
// funct2 = 00
// op = C1 = 01
function Action instrC_SRLI64 (RVState s, Bit#(1) i5, Bit#(3) rd_, Bit#(5) i4_0) =
  instrSRLI64 (s, zeroExtend({i5,i4_0}), regID(rd_), regID(rd_));

// funct3 = C.SRAI = 100
// funct2 = 01
// op = C1 = 01
function Action instrC_SRAI64 (RVState s, Bit#(1) i5, Bit#(3) rd_, Bit#(5) i4_0) =
  instrSRAI64 (s, zeroExtend({i5,i4_0}), regID(rd_), regID(rd_));

module [ISADefModule] mkExt_64_C#(RVState s) ();

  defineInstEntry("c.ldsp",  pat(n(3'b011), v, gv(neq(0)), v, v, n(2'b10)), instrC_LDSP(s));
  defineInstEntry("c.sdsp",  pat(n(3'b111), v, v, v, n(2'b10)), instrC_SDSP(s));
  defineInstEntry("c.ld",    pat(n(3'b011), v, v, v, v, n(2'b00)), instrC_LD(s));
  defineInstEntry("c.sd",    pat(n(3'b111), v, v, v, v, n(2'b00)), instrC_SD(s));
  defineInstEntry("c.addiw", pat(n(3'b001), v, gv(nez), v, n(2'b01)), instrC_ADDIW(s));
  defineInstEntry("c.slli",  pat(n(3'b000), v, gv(nez), v, n(2'b10)), instrC_SLLI64(s));
  defineInstEntry("c.srli",  pat(n(3'b100), v, n(2'b00), v, v, n(2'b01)), instrC_SRLI64(s));
  defineInstEntry("c.srai",  pat(n(3'b100), v, n(2'b01), v, v, n(2'b01)), instrC_SRAI64(s));
  defineInstEntry("c.addw",  pat(n(6'b100111), v, n(2'b01), v, n(2'b01)), instrC_ADDW(s));
  defineInstEntry("c.subw",  pat(n(6'b100111), v, n(2'b00), v, n(2'b01)), instrC_SUBW(s));

endmodule
