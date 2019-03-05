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
import BitPat :: *;
import BlueUtils :: *;

import RVBS_Types :: *;
import RVBS_TraceInsts :: *;

// div helper
function t safeDiv(t a, t b) provisos (Arith#(t), Eq#(t)) = a / ((b == 0) ? 1 : b);
function t safeRem(t a, t b) provisos (Arith#(t), Eq#(t)) = a % ((b == 0) ? 1 : b);

/*
  R-type

   31                        25 24    20 19    15 14    12 11     7 6        0
  +----------------------------+--------+--------+--------+--------+----------+
  |           funct7           |   rs2  |   rs1  | funct3 |   rd   |  opcode  |
  +----------------------------+--------+--------+--------+--------+----------+

*/

// funct7 = MULDIV = 0000001
// funct3 = MULW = 000
// opcode = OP = 0111011
function Action instrMULW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(32) tmp = truncate(s.rGPR(rs1)) * truncate(s.rGPR(rs2));
  s.wGPR(rd, signExtend(tmp));
  logInst(s, fmtInstR("mulw", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = DIVW = 100
// opcode = OP = 0111011
function Action instrDIVW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  if (s.rGPR(rs2) == 0) s.wGPR(rd, ~0);
  else begin
    Int#(32) tmp = safeDiv(unpack(truncate(s.rGPR(rs1))), unpack(truncate(s.rGPR(rs2))));
    s.wGPR(rd, signExtend(pack(tmp)));
  end
  logInst(s, fmtInstR("divw", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = DIVUW = 101
// opcode = OP = 0111011
function Action instrDIVUW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  if (s.rGPR(rs2) == 0) s.wGPR(rd, ~0);
  else begin
    Bit#(32) tmp = safeDiv(truncate(s.rGPR(rs1)), truncate(s.rGPR(rs2)));
    s.wGPR(rd, signExtend(tmp));
  end
  logInst(s, fmtInstR("divuw", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = REMW = 110
// opcode = OP = 0111011
function Action instrREMW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  if (s.rGPR(rs2) == 0) s.wGPR(rd, s.rGPR(rs1));
  else begin
    Int#(32) tmp = safeRem(unpack(truncate(s.rGPR(rs1))), unpack(truncate(s.rGPR(rs2))));
    s.wGPR(rd, signExtend(pack(tmp)));
  end
  logInst(s, fmtInstR("remw", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = REMUW = 111
// opcode = OP = 0111011
function Action instrREMUW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  if (s.rGPR(rs2) == 0) s.wGPR(rd, s.rGPR(rs1));
  else begin
    Bit#(32) tmp = safeRem(truncate(s.rGPR(rs1)), truncate(s.rGPR(rs2)));
    s.wGPR(rd, signExtend(tmp));
  end
  logInst(s, fmtInstR("remuw", rd, rs1, rs2));
endaction;

////////////////////////////////////////////////////////////////////////////////

module [ISADefModule] mkExt_64_M#(RVState s) ();

  defineInstEntry("mulw",  pat(n(7'b0000001), v, v, n(3'b000), v, n(7'b0111011)), instrMULW(s));
  defineInstEntry("divw",  pat(n(7'b0000001), v, v, n(3'b100), v, n(7'b0111011)), instrDIVW(s));
  defineInstEntry("divuw", pat(n(7'b0000001), v, v, n(3'b101), v, n(7'b0111011)), instrDIVUW(s));
  defineInstEntry("remw",  pat(n(7'b0000001), v, v, n(3'b110), v, n(7'b0111011)), instrREMW(s));
  defineInstEntry("remuw", pat(n(7'b0000001), v, v, n(3'b111), v, n(7'b0111011)), instrREMUW(s));

endmodule
