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

import FIFOF :: *;
import SpecialFIFOs :: *;
import List :: *;
import ClientServer :: *;
import GetPut :: *;

import Recipe :: *;
import BID :: *;
import BlueBasics :: *;
import BlueUtils :: *;
import AXI4Lite :: *;
import RVBS :: *;

typedef SizeOf#(PAddr) ADDR_sz;
typedef TMax#(IMemWidth, DMemWidth) DATA_sz;

////////////////
// Interfaces //
////////////////////////////////////////////////////////////////////////////////

interface RVBS_Ifc;
  // probing interfaces
  method Bit#(XLEN) peekPC();
  method Bit#(XLEN) peekCtrlCSR();
  interface BIDProbes probes;
  // riscv interfaces
  method Action setMSIP(Bool irq);
  method Action setMTIP(Bool irq);
  method Action setMEIP(Bool irq);
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMaster0;
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMaster1;
endinterface

(* always_ready, always_enabled *)
interface RVBS_Ifc_Synth;
  // probing interfaces
  method Bit#(XLEN) peekPC();
  method Bit#(XLEN) peekCtrlCSR();
  interface BIDProbes probes;
  // riscv interfaces
  method Action setMSIP(Bool irq);
  method Action setMTIP(Bool irq);
  method Action setMEIP(Bool irq);
  interface AXILiteMasterSynth#(ADDR_sz, DATA_sz) axiLiteMaster0;
  interface AXILiteMasterSynth#(ADDR_sz, DATA_sz) axiLiteMaster1;
endinterface

/////////////////////////////////
// Internal memory to AXI shim //
////////////////////////////////////////////////////////////////////////////////

interface MemShim;
  interface Array#(Mem#(PAddr, Bit#(DATA_sz))) internal;
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMaster0;
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMaster1;
endinterface
module mkMemShim (MemShim);

  // 2 AXI shims
  List#(AXILiteShim#(ADDR_sz, DATA_sz)) shim <- replicateM(2, mkAXILiteShim);
  // 2 memory interfaces
  Mem#(Bit#(ADDR_sz), Bit#(DATA_sz)) m[2];
  for (Integer i = 0; i < 2; i = i + 1) begin
    // discard write responses
    rule drainBChannel; let _ <- shim[i].slave.b.get; endrule
    // convert requests/responses
    m[i] = interface Mem;
      interface request = interface Put;
        method put (req) = action
          case (req) matches
            tagged ReadReq .r: shim[i].slave.ar.put(toAXIARLiteFlit(req));
            tagged WriteReq .w: begin
              shim[i].slave.aw.put(toAXIAWLiteFlit(req));
              shim[i].slave.w.put(toAXIWLiteFlit(req));
            end
          endcase
        endaction;
      endinterface;
      interface response = interface Get;
        method get = actionvalue
          let rsp <- shim[i].slave.r.get;
          return fromAXIRLiteFlit(rsp);
        endactionvalue;
      endinterface;
    endinterface;
  end
  // wire up interfaces
  interface internal = m;
  interface axiLiteMaster0 = shim[0].master;
  interface axiLiteMaster1 = shim[1].master;

endmodule

/////////////////
// RVBS module //
////////////////////////////////////////////////////////////////////////////////

(* synthesize *)
module mkRVBS#(parameter VAddr reset_pc) (RVBS_Ifc);

  // create the memory shim
  let mem <- mkMemShim;
  `ifdef SUPERVISOR_MODE
  Mem#(PAddr, Bit#(IMemWidth)) imem[2] <- virtualize(mem.internal[0], 2);
  Mem#(PAddr, Bit#(DMemWidth)) dmem[2] <- virtualize(mem.internal[1], 2);
  RVState s <- mkState(reset_pc, imem[1], dmem[1], imem[0], dmem[0]);
  `else
  RVState s <- mkState(reset_pc, mem.internal[0], mem.internal[1]);
  `endif

  // initialization
  module [InstrDefModule] mkInit#(RVState st) ();
    defineInit(rSeq(rBlock(action
      st.regFile[10] <= 0;
    endaction, action
      st.regFile[11] <= 'h00004000;
    endaction)));
  endmodule
  // instanciating simulator
  let modList = list(mkInit, mkRVTrap, mkRV32I);
  `ifdef RVM
    modList = append(modList, list(mkRV32M));
  `endif
  `ifdef RVC
    modList = append(modList, list(mkRV32C));
  `endif
  `ifdef XLEN64
  modList = append(modList, list(mkRV64I));
    `ifdef RVM
      modList = append(modList, list(mkRV64M));
    `endif
    `ifdef RVC
      modList = append(modList, list(mkRV64C));
    `endif
  `endif
  let bid_probes <- mkISASim(s, modList);

  method Bit#(XLEN) peekPC() = s.pc;
  method Bit#(XLEN) peekCtrlCSR() = s.csrs.ctrl;
  interface probes = bid_probes;
  method setMSIP = s.csrs.setMSIP;
  method setMTIP = s.csrs.setMTIP;
  method setMEIP = s.csrs.setMEIP;
  interface axiLiteMaster0 = mem.axiLiteMaster0;
  interface axiLiteMaster1 = mem.axiLiteMaster1;

endmodule

(* synthesize *)
module rvbs#(parameter VAddr reset_pc) (RVBS_Ifc_Synth);
  let ifc <- mkRVBS(reset_pc);
  let m0 <- toAXILiteMasterSynth(ifc.axiLiteMaster0);
  let m1 <- toAXILiteMasterSynth(ifc.axiLiteMaster1);
  method peekPC = ifc.peekPC;
  method peekCtrlCSR = ifc.peekCtrlCSR;
  interface probes = ifc.probes;
  method setMSIP = ifc.setMSIP;
  method setMTIP = ifc.setMTIP;
  method setMEIP = ifc.setMEIP;
  interface axiLiteMaster0 = m0;
  interface axiLiteMaster1 = m1;
endmodule
