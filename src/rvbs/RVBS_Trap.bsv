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

import BID :: *;
import Recipe :: *;
import BlueUtils :: *;
import BitPat :: *;
import RVBS_Types :: *;
import RVBS_TraceInsts :: *;

////////////////
// Trap logic //
////////////////////////////////////////////////////////////////////////////////

// Global Interrupt-Enable and Privilege stack push
function Action pushStatusStack(CSR_Ifc#(Status) status, PrivLvl from, PrivLvl to) = action
  Status newval = status;
  case (to)
    M: begin
      newval.mie  = False;
      newval.mpie = status.mie;
      newval.mpp  = pack(from);
    end
    S: begin
      newval.sie  = False;
      newval.spie = status.sie;
      newval.spp  = truncate(pack(from));
    end
    U: begin
      newval.uie  = False;
      newval.upie = status.uie;
    end
    default: noAction;
  endcase
  status <= newval;
endaction;
// Global Interrupt-Enable and Privilege stack pop
function ActionValue#(PrivLvl) popStatusStack(CSR_Ifc#(Status) status, PrivLvl from) = actionvalue
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

function Action general_trap(PrivLvl toLvl, TrapCode trapCode, VAddr epc, RVState s) = action
  // Global Interrupt-Enable Stack and latch current privilege level
  pushStatusStack(s.csrs.mstatus, s.currentPrivLvl, toLvl);
  // others
  case (toLvl)
    M: begin
      s.csrs.mcause <= trapCode;
      s.csrs.mepc.addr <= truncateLSB(epc);
    end
    `ifdef SUPERVISOR_MODE
    S: begin
      s.csrs.scause <= trapCode;
      s.csrs.sepc.addr <= truncateLSB(epc);
    end
    `endif
    `ifdef RVN
    U: begin
      // TODO s.csrs.ucause <= trapCode;
      // TODO s.csrs.uepc.addr <= truncateLSB(epc);
    end
    `endif
    default: terminateSim(s, $format("TRAP INTO UNKNOWN PRIVILEGE MODE ", fshow(s.currentPrivLvl)));
  endcase
  s.currentPrivLvl <= M;
  printTLogPlusArgs("itrace", $format(">>> TRAP <<< -- mcause <= ", fshow(trapCode), ", mepc <= 0x%0x, pc <= 0x%0x", epc, s.csrs.mtvec));
endaction;

function Action raiseIFetchException(RVState s, ExcCode code) = action
  s.pendingIFetchException[1] <= Valid(code);
endaction;

typeclass RaiseException#(type a); a raiseException; endtypeclass

instance RaiseException#(function Action f(RVState s, ExcCode code));
  function Action raiseException(RVState s, ExcCode code) = action
    s.pendingException[0] <= Valid(tuple2(code, Invalid));
  endaction;
endinstance

instance RaiseException#(function Action f(RVState s, ExcCode code, Bit#(XLEN) tval));
  function Action raiseException(RVState s, ExcCode code, Bit#(XLEN) tval) = action
    s.pendingException[0] <= Valid(tuple2(code, Valid(tval)));
  endaction;
endinstance

function Action raiseMemException(RVState s, ExcCode code) = action
  s.pendingMemException[0] <= Valid(code);
endaction;

`ifdef RVXCHERI
typeclass RaiseCapException#(type a); a raiseCapException; endtypeclass

instance RaiseCapException#(function Action f(RVState s, CapExcCode exc, Bit#(6) idx));
  function raiseCapException(s, exc, idx) = action
    // TODO set cheri cause etc...
    raiseException(s, CHERIFault);
  endaction;
endinstance

instance RaiseCapException#(function Action f(RVState s, CapExcCode exc, Bit#(5) idx));
  function raiseCapException(s, exc, idx) = raiseCapException(s, exc, {1'b0, idx});
endinstance

typeclass RaiseMemCapException#(type a); a raiseMemCapException; endtypeclass

instance RaiseMemCapException#(function Action f(RVState s, CapExcCode exc, Bit#(6) idx));
  function raiseMemCapException(s, exc, idx) = action
    // TODO set cheri cause etc...
    raiseMemException(s, CHERIFault);
  endaction;
endinstance

instance RaiseMemCapException#(function Action f(RVState s, CapExcCode exc, Bit#(5) idx));
  function raiseMemCapException(s, exc, idx) = raiseMemCapException(s, exc, {1'b0, idx});
endinstance

instance RaiseMemCapException#(function Recipe f(a x, b y, c z)) provisos (RaiseMemCapException#(function Action g(a x, b y, c z)));
  function raiseMemCapException(x, y, z) = rAct(raiseMemCapException(x, y, z));
endinstance

typeclass RaiseIFetchCapException#(type a); a raiseIFetchCapException; endtypeclass

instance RaiseIFetchCapException#(function Action f(RVState s, CapExcCode exc));
  function raiseIFetchCapException(s, exc) = action
    // TODO set cheri cause etc...
    raiseIFetchException(s, CHERIFault);
  endaction;
endinstance

instance RaiseIFetchCapException#(function Recipe f(a x, b y)) provisos (RaiseIFetchCapException#(function Action g(a x, b y)));
  function raiseIFetchCapException(x, y) = rAct(raiseIFetchCapException(x, y));
endinstance
`endif

function Action raiseMemTokException(RVState s, ExcToken excToken) = action
  `ifdef RVXCHERI
  if (excToken.excCode == CHERIFault)
    raiseMemCapException(s, excToken.capExcCode, excToken.capIdx);
  else
  `endif
  raiseMemException(s, excToken.excCode);
endaction;

function Action raiseIFetchTokException(RVState s, ExcToken excToken) = action
  `ifdef RVXCHERI
  if (excToken.excCode == CHERIFault)
    raiseIFetchCapException(s, excToken.capExcCode);
  else
  `endif
  raiseIFetchException(s, excToken.excCode);
endaction;

function Maybe#(IntCode) checkIRQ (RVState s);
  Bool lvl_ie = case (s.currentPrivLvl)
    M: s.csrs.mstatus.mie;
    S: s.csrs.mstatus.sie;
    U: s.csrs.mstatus.uie;
    default: True;
  endcase;
  Maybe#(IntCode) intCode = Invalid;
  if (lvl_ie) begin
    // order: MEI, MSI, MTI, SEI, SSI, STI, UEI, USI, UTI
    if (s.csrs.mip.meip && s.csrs.mie.meie) intCode = Valid(MExtInt);
    else if (s.csrs.mip.msip && s.csrs.mie.msie) intCode = Valid(MSoftInt);
    else if (s.csrs.mip.mtip && s.csrs.mie.mtie) intCode = Valid(MTimerInt);
    else if (s.csrs.mip.seip && s.csrs.mie.seie) intCode = Valid(SExtInt);
    else if (s.csrs.mip.ssip && s.csrs.mie.ssie) intCode = Valid(SSoftInt);
    else if (s.csrs.mip.stip && s.csrs.mie.stie) intCode = Valid(STimerInt);
    else if (s.csrs.mip.ueip && s.csrs.mie.ueie) intCode = Valid(UExtInt);
    else if (s.csrs.mip.usip && s.csrs.mie.usie) intCode = Valid(USoftInt);
    else if (s.csrs.mip.utip && s.csrs.mie.utie) intCode = Valid(UTimerInt);
  end
  return intCode;
endfunction

function Action assignM (Reg#(a) r, ActionValue#(a) av) =
  action a tmp <- av; r <= tmp; endaction;

module [ISADefModule] mkRVTrap#(RVState s) ();
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
      raiseException(s, IllegalInst);
      logInst(s, $format("mret"));
    end else begin
      PrivLvl toLvl <- popStatusStack(s.csrs.mstatus, M);
      s.currentPrivLvl <= toLvl;
      s.pc <= pack(s.csrs.mepc);
      logInst(s, $format("mret"), fshow(s.currentPrivLvl) + $format(" -> ") + fshow(toLvl));
    end
  endaction;
  defineInstEntry("mret", pat(n(12'b001100000010), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrMRET);

  `ifdef SUPERVISOR_MODE
  // funct12 = SRET = 000100000010
  // rs1 = 00000
  // funct3 = PRIV = 000
  // rd = 00000
  // opcode = SYSTEM = 1110011
  function Action instrSRET () = action
    if (s.currentPrivLvl < S || (s.currentPrivLvl == S && s.csrs.mstatus.tsr)) begin
      raiseException(s, IllegalInst);
      logInst(s, $format("sret"));
    end else begin
      PrivLvl toLvl <- popStatusStack(s.csrs.mstatus, S);
      s.currentPrivLvl <= toLvl;
      s.pc <= pack(s.csrs.sepc);
      logInst(s, $format("sret"), fshow(s.currentPrivLvl) + $format(" -> ") + fshow(toLvl));
    end
  endaction;
  defineInstEntry("sret", pat(n(12'b000100000010), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrSRET);
  `endif

  `ifdef USER_MODE
  //XXX TODO N extension...
  // funct12 = URET = 000000000010
  // rs1 = 00000
  // funct3 = PRIV = 000
  // rd = 00000
  // opcode = SYSTEM = 1110011
  function Action instrURET () = action
    if (s.currentPrivLvl < U || !static_HAS_N_EXT) raiseException(s, IllegalInst);
    else assignM(s.currentPrivLvl, popStatusStack(s.csrs.mstatus, U));
    // trace
    logInst(s, $format("uret"));
  endaction;
  defineInstEntry("uret", pat(n(12'b000000000010), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrURET);
  `endif

  // funct12 = WFI = 000100000101
  // rs1 = 00000
  // funct3 = PRIV = 000
  // rd = 00000
  // opcode = SYSTEM = 1110011
  function Action instrWFI () = action
    Bool limit_reached = True;
    case (s.currentPrivLvl) matches
      U &&& (!static_HAS_N_EXT): action raiseException(s, IllegalInst); endaction
      S &&& (s.csrs.mstatus.tw && limit_reached): action raiseException(s, IllegalInst); endaction
      default: noAction;
    endcase
    logInst(s, $format("wfi"), $format("IMPLEMENTED AS NOP"));
  endaction;
  defineInstEntry("wfi", pat(n(12'b000100000101), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrWFI);

  // general functionalities
  //////////////////////////////////////////////////////////////////////////////
  Maybe#(IntCode) irqCode = checkIRQ(s);
  Bool isTrap = isValid(s.pendingIFetchException[2]) ||
                isValid(s.pendingException[1]) ||
                isValid(s.pendingMemException[1]) ||
                isValid(irqCode);
  defineInterEntry(Guarded { guard: isTrap, val: action
    // general info
    let isIFetchException = isValid(s.pendingIFetchException[0]);
    let isStdException = isValid(s.pendingException[0]);
    let isMemException = isValid(s.pendingMemException[0]);
    let isException = isStdException || isMemException;
    let ifetchexc = s.pendingIFetchException[0].Valid;
    match {.exc, .maybe_tval} = s.pendingException[0].Valid;
    let memexc = s.pendingMemException[0].Valid;
    TrapCode code = Interrupt(irqCode.Valid);
    if (isIFetchException) code = Exception(ifetchexc);
    else if (isStdException) code = Exception(exc);
    else if (isMemException) code = Exception(memexc);
    // handle general trap behaviour
    general_trap(M, code, s.pc, s);
    // potential tval latching
    if (isStdException && isValid(maybe_tval)) s.csrs.mtval <= maybe_tval.Valid;
    // handle pc update
    Bit#(XLEN) tgt = {s.csrs.mtvec.base, 2'b00};
    case (s.csrs.mtvec.mode) matches
      Direct: s.pc <= tgt;
      Vectored &&& isException: s.pc <= tgt;
      Vectored &&& (!isException): s.pc <= tgt + zeroExtend({pack(irqCode.Valid),2'b00});
      default: terminateSim(s, $format("TRAP WITH UNKNOWN MTVEC MODE ", fshow(s.csrs.mtvec.mode)));
    endcase
    // reset transient state
    s.pendingIFetchException[0] <= Invalid;
    s.pendingException[0] <= Invalid;
    s.pendingMemException[0] <= Invalid;
    `ifdef RVFI_DII
    s.exc_tgt[0] <= Valid(tgt);
    `endif
  endaction});

endmodule
