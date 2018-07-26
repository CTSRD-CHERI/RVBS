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

import RV_Common :: *;

// div helper
function t safeDiv(t a, t b) provisos (Arith#(t), Eq#(t)) = a / ((b == 0) ? 1 : b);
function t safeRem(t a, t b) provisos (Arith#(t), Eq#(t)) = a % ((b == 0) ? 1 : b);

`ifdef XLEN32

/*
  R-type

   31                        25 24    20 19    15 14    12 11     7 6        0
  +----------------------------+--------+--------+--------+--------+----------+
  |           funct7           |   rs2  |   rs1  | funct3 |   rd   |  opcode  |
  +----------------------------+--------+--------+--------+--------+----------+

*/

// funct7 = MULDIV = 0000001
// funct3 = MUL = 000
// opcode = OP = 0110011
function Action instrMUL (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(TMul#(XLEN, 2)) tmp = zeroExtend(s.regFile[rs1]) * zeroExtend(s.regFile[rs2]);
  s.regFile[rd] <= truncate(tmp);
  s.pc <= s.pc + s.instByteSz;
  logInst(s.pc, fmtInstR("mul", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = MULH = 001
// opcode = OP = 0110011
function Action instrMULH (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Int#(TMul#(XLEN, 2)) tmp = unpack(signExtend(s.regFile[rs1])) * unpack(signExtend(s.regFile[rs2]));
  s.regFile[rd] <= truncateLSB(pack(tmp));
  s.pc <= s.pc + s.instByteSz;
  logInst(s.pc, fmtInstR("mulh", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = MULHSU = 010
// opcode = OP = 0110011
function Action instrMULHSU (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Int#(TMul#(XLEN, 2)) tmp = unpack(signExtend(s.regFile[rs1])) * unpack(zeroExtend(s.regFile[rs2]));
  s.regFile[rd] <= truncateLSB(pack(tmp));
  s.pc <= s.pc + s.instByteSz;
  logInst(s.pc, fmtInstR("mulhsu", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = MULHU = 011
// opcode = OP = 0110011
function Action instrMULHU (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Bit#(TMul#(XLEN, 2)) tmp = zeroExtend(s.regFile[rs1]) * zeroExtend(s.regFile[rs2]);
  s.regFile[rd] <= truncateLSB(tmp);
  s.pc <= s.pc + s.instByteSz;
  logInst(s.pc, fmtInstR("mulhu", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = DIV = 100
// opcode = OP = 0110011
function Action instrDIV (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  if (s.regFile[rs2] == 0) s.regFile[rd] <= ~0;
  else begin
    Int#(XLEN) tmp = safeDiv(unpack(s.regFile[rs1]), unpack(s.regFile[rs2]));
    s.regFile[rd] <= pack(tmp);
  end
  s.pc <= s.pc + s.instByteSz;
  logInst(s.pc, fmtInstR("div", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = DIVU = 101
// opcode = OP = 0110011
function Action instrDIVU (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= (s.regFile[rs2] == 0) ? ~0 : safeDiv(s.regFile[rs1], s.regFile[rs2]);
  s.pc <= s.pc + s.instByteSz;
  logInst(s.pc, fmtInstR("divu", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = REM = 110
// opcode = OP = 0110011
function Action instrREM (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  Int#(XLEN) tmp = unpack(s.regFile[rs1]) % unpack(s.regFile[rs2]); // XXX use safeRem ?
  s.regFile[rd] <= (s.regFile[rs2] == 0) ? s.regFile[rs1] : pack(tmp);
  s.pc <= s.pc + s.instByteSz;
  logInst(s.pc, fmtInstR("rem", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = REMU = 111
// opcode = OP = 0110011
function Action instrREMU (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  s.regFile[rd] <= (s.regFile[rs2] == 0) ? s.regFile[rs1] : s.regFile[rs1] % s.regFile[rs2]; // XXX
  s.pc <= s.pc + s.instByteSz;
  logInst(s.pc, fmtInstR("remu", rd, rs1, rs2));
endaction;

////////////////////////////////////////////////////////////////////////////////

module [InstrDefModule] mkRV32M#(RVState s) ();

  defineInstr("mul",    pat(n(7'b0000001), v, v, n(3'b000), v, n(7'b0110011)), instrMUL(s));
  defineInstr("mulh",   pat(n(7'b0000001), v, v, n(3'b001), v, n(7'b0110011)), instrMULH(s));
  defineInstr("mulhsu", pat(n(7'b0000001), v, v, n(3'b010), v, n(7'b0110011)), instrMULHSU(s));
  defineInstr("mulhu",  pat(n(7'b0000001), v, v, n(3'b011), v, n(7'b0110011)), instrMULHU(s));
  defineInstr("div",    pat(n(7'b0000001), v, v, n(3'b100), v, n(7'b0110011)), instrDIV(s));
  defineInstr("divu",   pat(n(7'b0000001), v, v, n(3'b101), v, n(7'b0110011)), instrDIVU(s));
  defineInstr("rem",    pat(n(7'b0000001), v, v, n(3'b110), v, n(7'b0110011)), instrREM(s));
  defineInstr("remu",   pat(n(7'b0000001), v, v, n(3'b111), v, n(7'b0110011)), instrREMU(s));

endmodule

`endif // XLEN32

////////////////////////////////////////////////////////////////////////////////

`ifdef XLEN64

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
  Bit#(32) tmp = truncate(s.regFile[rs1]) * truncate(s.regFile[rs2]);
  s.regFile[rd] <= signExtend(tmp);
  s.pc <= s.pc + s.instByteSz;
  logInst(s.pc, fmtInstR("mulw", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = DIVW = 100
// opcode = OP = 0111011
function Action instrDIVW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  if (s.regFile[rs2] == 0) s.regFile[rd] <= ~0;
  else begin
    Int#(32) tmp = safeDiv(unpack(truncate(s.regFile[rs1])), unpack(truncate(s.regFile[rs2])));
    s.regFile[rd] <= signExtend(pack(tmp));
  end
  s.pc <= s.pc + s.instByteSz;
  logInst(s.pc, fmtInstR("divw", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = DIVUW = 101
// opcode = OP = 0111011
function Action instrDIVUW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  if (s.regFile[rs2] == 0) s.regFile[rd] <= ~0;
  else begin
    Bit#(32) tmp = safeDiv(truncate(s.regFile[rs1]), truncate(s.regFile[rs2]));
    s.regFile[rd] <= signExtend(tmp);
  end
  s.pc <= s.pc + s.instByteSz;
  logInst(s.pc, fmtInstR("divuw", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = REMW = 110
// opcode = OP = 0111011
function Action instrREMW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  if (s.regFile[rs2] == 0) s.regFile[rd] <= s.regFile[rs1];
  else begin
    Int#(32) tmp = safeRem(unpack(truncate(s.regFile[rs1])), unpack(truncate(s.regFile[rs2])));
    s.regFile[rd] <= signExtend(pack(tmp));
  end
  s.pc <= s.pc + s.instByteSz;
  logInst(s.pc, fmtInstR("remw", rd, rs1, rs2));
endaction;

// funct7 = MULDIV = 0000001
// funct3 = REMUW = 111
// opcode = OP = 0111011
function Action instrREMUW (RVState s, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
  if (s.regFile[rs2] == 0) s.regFile[rd] <= s.regFile[rs1];
  else begin
    Bit#(32) tmp = safeRem(truncate(s.regFile[rs1]), truncate(s.regFile[rs2]));
    s.regFile[rd] <= signExtend(tmp);
  end
  s.pc <= s.pc + s.instByteSz;
  logInst(s.pc, fmtInstR("remuw", rd, rs1, rs2));
endaction;

////////////////////////////////////////////////////////////////////////////////

module [InstrDefModule] mkRV64M#(RVState s) ();

  defineInstr("mulw",  pat(n(7'b0000001), v, v, n(3'b000), v, n(7'b0111011)), instrMULW(s));
  defineInstr("divw",  pat(n(7'b0000001), v, v, n(3'b100), v, n(7'b0111011)), instrDIVW(s));
  defineInstr("divuw", pat(n(7'b0000001), v, v, n(3'b101), v, n(7'b0111011)), instrDIVUW(s));
  defineInstr("remw",  pat(n(7'b0000001), v, v, n(3'b110), v, n(7'b0111011)), instrREMW(s));
  defineInstr("remuw", pat(n(7'b0000001), v, v, n(3'b111), v, n(7'b0111011)), instrREMUW(s));

endmodule

`endif // XLEN64
