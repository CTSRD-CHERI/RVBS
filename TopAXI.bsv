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

import BID :: *;
import BlueBasics :: *;
import BlueUtils :: *;
import AXI :: *;
import RVBS :: *;

typedef SizeOf#(PAddr) ADDR_sz;
typedef TMax#(IMemWidth, DMemWidth) DATA_sz;

(* always_ready, always_enabled *)
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

// Internal memory to AXI shim
interface MemShim;
  interface Mem2#(PAddr, Bit#(IMemWidth), Bit#(DMemWidth)) internal;
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMaster0;
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMaster1;
endinterface
module mkMemShim (MemShim);

  // 2 AXI shims
  List#(AXILiteMasterShim#(ADDR_sz, DATA_sz)) shim <- replicateM(2, mkAXILiteMasterShim);
  // 2 memory interfaces
  List#(Mem#(Bit#(ADDR_sz), Bit#(DATA_sz))) m = replicate(2, ?);
  for (Integer i = 0; i < 2; i = i + 1) begin
    // discard write responses
    rule drainBChannel; shim[i].bff.deq; endrule
    // convert requests/responses
    m[i] = interface Mem;
      interface request = interface Put;
        method put (req) = action
          case (req) matches
            tagged ReadReq .r: shim[i].arff.enq(toAXIARLiteFlit(req));
            tagged WriteReq .w: begin
              shim[i].awff.enq(toAXIAWLiteFlit(req));
              shim[i].wff.enq(toAXIWLiteFlit(req));
            end
          endcase
        endaction;
      endinterface;
      interface response = interface Get;
        method get = actionvalue
          shim[i].rff.deq; return fromAXIRLiteFlit(shim[i].rff.first);
        endactionvalue;
      endinterface;
    endinterface;
  end
  // wire up interfaces
  interface internal = interface Mem2;
    interface p0 = m[0];
    interface p1 = m[1];
  endinterface;
  interface axiLiteMaster0 = shim[0].master;
  interface axiLiteMaster1 = shim[1].master;

endmodule

(* synthesize *)
module rvbs#(parameter VAddr reset_pc) (RVBS_Ifc);

  // create the memory shim
  let mem <- mkMemShim;
  `ifdef SUPERVISOR_MODE
  Mem#(PAddr, Bit#(IMemWidth)) imem[2] <- virtualize(mem.internal.p0, 2);
  Mem#(PAddr, Bit#(DMemWidth)) dmem[2] <- virtualize(mem.internal.p1, 2);
  RVState s <- mkState(reset_pc, imem[1], dmem[1], imem[0], dmem[0]);
  `else
  RVState s <- mkState(reset_pc, mem.internal.p0, mem.internal.p1);
  `endif

  // instanciating simulator
  let modList = list(mkRVTrap, mkRV32I);
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
