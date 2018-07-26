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
import BlueUtils :: *;
import AXI :: *;
import RV_State :: *;
import RV_Common :: *;
import RV_I :: *;
`ifdef RVM
import RV_M :: *;
`endif
`ifdef RVC
import RV_C :: *;
`endif

typedef SizeOf#(PAddr) ADDR_sz;
typedef TMax#(IMemWidth, DMemWidth) DATA_sz;

(* always_ready *)
interface RVBS_Ifc;
  method Bit#(XLEN) peekPC();
  method Bit#(XLEN) peekCtrlCSR();
  interface BIDProbes probes;
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMaster0;
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMaster1;
endinterface

// Internal memory to AXI shim
interface RVBSMem_Ifc;
  interface Mem2#(PAddr, Bit#(IMemWidth), Bit#(DMemWidth)) internal;
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMaster0;
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMaster1;
endinterface
module rvbsMem (RVBSMem_Ifc);

  // one FIFOF per AXI channel
  let awff0 <- mkFIFOF;
  let wff0  <- mkFIFOF;
  let arff0 <- mkFIFOF;
  let rff0  <- mkFIFOF;
  let awff1 <- mkFIFOF;
  let wff1  <- mkFIFOF;
  let arff1 <- mkFIFOF;
  let rff1  <- mkFIFOF;

  // turn FIFOFs to AXI interfaces
  let awifc0 <- toAXIAWLiteMaster(awff0);
  let wifc0  <- toAXIWLiteMaster(wff0);
  let bifc0  <- mkSink;
  let arifc0 <- toAXIARLiteMaster(arff0);
  let rifc0  <- toAXIRLiteMaster(rff0);
  let awifc1 <- toAXIAWLiteMaster(awff1);
  let wifc1  <- toAXIWLiteMaster(wff1);
  let bifc1  <- mkSink;
  let arifc1 <- toAXIARLiteMaster(arff1);
  let rifc1  <- toAXIRLiteMaster(rff1);

  // connect to internal interface
  interface internal = interface Mem2;
    interface Mem p0;
      interface request = interface Put; method put (req) = action
        case (req) matches
          tagged ReadReq .r: arff0.enq(req);
          tagged WriteReq .w: begin
            awff0.enq(req);
            wff0.enq(req);
          end
        endcase
      endaction; endinterface;
      interface response = interface Get; method get = actionvalue
        rff0.deq; return rff0.first;
      endactionvalue; endinterface;
    endinterface
    interface Mem p1;
      interface request = interface Put; method put (req) = action
        case (req) matches
          tagged ReadReq .r: arff1.enq(req);
          tagged WriteReq .w: begin
            awff1.enq(req);
            wff1.enq(req);
          end
        endcase
      endaction; endinterface;
      interface response = interface Get; method get = actionvalue
        rff1.deq; return rff1.first;
      endactionvalue; endinterface;
    endinterface
  endinterface;

  interface axiLiteMaster0 = interface AXILiteMaster;
    interface aw = awifc0;
    interface w  = wifc0;
    interface b  = bifc0;
    interface ar = arifc0;
    interface r  = rifc0;
  endinterface;

  interface axiLiteMaster1 = interface AXILiteMaster;
    interface aw = awifc1;
    interface w  = wifc1;
    interface b  = bifc1;
    interface ar = arifc1;
    interface r  = rifc1;
  endinterface;

endmodule

(* synthesize *)
module rvbs#(parameter VAddr reset_pc) (RVBS_Ifc);

  // instanciating memory subsystem
  let mem <- rvbsMem;
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
  interface axiLiteMaster0 = mem.axiLiteMaster0;
  interface axiLiteMaster1 = mem.axiLiteMaster1;

endmodule
