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

import BitPat :: *;
import BID :: *;

import RV_BasicTypes :: *;
import RV_Traces :: *;
import RV_CSRTypes :: *;
import RV_State :: *;

////////////////
// Trap logic //
////////////////////////////////////////////////////////////////////////////////

// Global Interrupt-Enable and Privilege stack push
function Action pushStatusStack(Reg#(Status) status, PrivLvl from, PrivLvl to) = action
  Status newval = status;
  case (to)
    M: begin
      newval.mpie = newval.mie;
      newval.mpp  = pack(from);
    end
    S: begin
      newval.spie = newval.sie;
      newval.spp  = truncate(pack(from));
    end
    U: newval.upie = newval.uie;
    default: noAction;
  endcase
  status <= newval;
endaction;
// Global Interrupt-Enable and Privilege stack pop
function ActionValue#(PrivLvl) popStatusStack(Reg#(Status) status, PrivLvl from) = actionvalue
  Status newval = status;
  PrivLvl to = from;
  case (from)
    M: begin
      newval.mie = newval.mpie;
      to         = unpack(newval.mpp);
      newval.mpp = (static_HAS_U_MODE) ? pack(U) : pack(M);
    end
    S: begin
      newval.sie = newval.spie;
      to         = unpack({1'b0, newval.spp});
      newval.spp = (static_HAS_U_MODE) ? truncate(pack(U)): truncate(pack(M)); // XXX check spec here... Shouldn't it be "lowest supported priv mode" rather than "U if supported, M otherwise"?
    end
    U: newval.uie = newval.upie; // (and stay in U-mode)
    default: noAction;
  endcase
  status <= newval;
  return to;
endactionvalue;

function Action general_trap(PrivLvl toLvl, Cause cause, RVState s, Action specific_behaviour) = action
  specific_behaviour;
  // Global Interrupt-Enable Stack and latch current privilege level
  pushStatusStack(s.csrs.mstatus, s.currentPrivLvl, toLvl);
  // others
  case (toLvl)
    M: begin
      s.csrs.mcause <= cause;
      s.csrs.mepc.addr <= truncateLSB(s.pc);
    end
    `ifdef SUPERVISOR_MODE
    S: begin
      s.csrs.scause <= cause;
      s.csrs.sepc.addr <= truncateLSB(s.pc);
    end
    `endif
    `ifdef RVN
    U: begin
      // TODO s.csrs.ucause <= cause;
      // TODO s.csrs.uepc.addr <= truncateLSB(s.pc);
    end
    `endif
    default: terminateSim(s, $format("TRAP INTO UNKNOWN PRIVILEGE MODE ", fshow(s.currentPrivLvl)));
  endcase
  s.currentPrivLvl <= M;
  printTLogPlusArgs("itrace", $format(">>> TRAP <<< -- mcause <= ", fshow(cause), ", mepc <= 0x%0x, pc <= 0x%0x", s.pc, s.csrs.mtvec));
endaction;

typeclass Trap#(type a); a trap; endtypeclass

instance Trap#(function Action f(RVState s, ExcCode code));
  function Action trap(RVState s, ExcCode code) =
    general_trap(M, Exception(code), s, action
      if (s.csrs.mtvec.mode >= 2) terminateSim(s, $format("TRAP WITH UNKNOWN MTVEC MODE ", fshow(s.csrs.mtvec.mode)));
      else s.pc <= {s.csrs.mtvec.base, 2'b00};
    endaction);
endinstance

instance Trap#(function Action f(RVState s, ExcCode code, Action side_effect));
  function Action trap(RVState s, ExcCode code, Action side_effect) = action
    side_effect;
    trap(s, code);
  endaction;
endinstance

/*
instance Trap#(function Action f(RVState s, IntCode code));
  function Action trap(RVState s, IntCode code) =
    general_trap(M, Interrupt(code), s, action
      Bit#(XLEN) tgt = {s.csrs.mtvec.base, 2'b00};
      case (s.csrs.mtvec.mode)
        Direct: s.pc <= tgt;
        Vectored: s.pc <= tgt + zeroExtend({pack(code),2'b00});
        default: terminateSim(s, $format("TRAP WITH UNKNOWN MTVEC MODE ", fshow(s.csrs.mtvec.mode)));
      endcase
    endaction);
endinstance
*/

function Action assignM (Reg#(a) r, ActionValue#(a) av) =
  action a tmp <- av; r <= tmp; endaction;

module [InstrDefModule] mkRVTrap#(RVState s) ();
/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |                funct12              |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
*/

  // funct12 = MRET = 001100000010
  // rs1 = 00000
  // funct3 = PRIV = 000
  // rd = 00000
  // opcode = SYSTEM = 1110011
  function Action instrMRET () = action
    if (s.currentPrivLvl < M) begin
      trap(s, IllegalInst);
      logInst(s.pc, $format("mret"));
    end else begin
      PrivLvl toLvl <- popStatusStack(s.csrs.mstatus, M);
      s.currentPrivLvl <= toLvl;
      s.pc <= pack(s.csrs.mepc);
      logInst(s.pc, $format("mret"), fshow(s.currentPrivLvl) + $format(" -> ") + fshow(toLvl));
    end
  endaction;
  defineInstr("mret", pat(n(12'b001100000010), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrMRET);

  `ifdef SUPERVISOR_MODE
  // funct12 = SRET = 000100000010
  // rs1 = 00000
  // funct3 = PRIV = 000
  // rd = 00000
  // opcode = SYSTEM = 1110011
  function Action instrSRET () = action
    if (s.currentPrivLvl < S) begin
      trap(s, IllegalInst);
      logInst(s.pc, $format("sret"));
    end else begin
      PrivLvl toLvl <- popStatusStack(s.csrs.mstatus, S);
      s.currentPrivLvl <= toLvl;
      s.pc <= pack(s.csrs.sepc);
      logInst(s.pc, $format("sret"), fshow(s.currentPrivLvl) + $format(" -> ") + fshow(toLvl));
    end
  endaction;
  defineInstr("sret", pat(n(12'b000100000010), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrSRET);
  `endif

  `ifdef USER_MODE
  // funct12 = URET = 000000000010
  // rs1 = 00000
  // funct3 = PRIV = 000
  // rd = 00000
  // opcode = SYSTEM = 1110011
  function Action instrURET () = action
    if (s.currentPrivLvl < U || !static_HAS_N_EXT) trap(s, IllegalInst);
    else assignM(s.currentPrivLvl, popStatusStack(s.csrs.mstatus, U));
    // trace
    logInst(s.pc, $format("uret"));
  endaction;
  defineInstr("uret", pat(n(12'b000000000010), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrURET);
  `endif

  // funct12 = WFI = 000100000101
  // rs1 = 00000
  // funct3 = PRIV = 000
  // rd = 00000
  // opcode = SYSTEM = 1110011
  function Action instrWFI () = action
    Bool limit_reached = True;
    case (s.currentPrivLvl) matches
      U &&& (!static_HAS_N_EXT): action trap(s, IllegalInst); endaction
      S &&& (s.csrs.mstatus.tw == 1 && limit_reached): action trap(s, IllegalInst); endaction
      default: s.pc <= s.pc + s.instByteSz;
    endcase
    logInst(s.pc, $format("wfi"), $format("IMPLEMENTED AS NOP"));
  endaction;
  defineInstr("wfi", pat(n(12'b000100000101), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrWFI);

endmodule
