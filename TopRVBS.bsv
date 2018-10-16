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

import FIFO :: *;
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
`ifdef RVFI_DII
import RVFI_DII :: *;
`endif

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
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMasterInst;
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMasterData;
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
  interface AXILiteMasterSynth#(ADDR_sz, DATA_sz) axiLiteMasterInst;
  interface AXILiteMasterSynth#(ADDR_sz, DATA_sz) axiLiteMasterData;
endinterface

/////////////////////////////////
// Internal memory to AXI shim //
////////////////////////////////////////////////////////////////////////////////

interface MemShim;
  interface Array#(Mem#(PAddr, Bit#(DATA_sz))) internal;
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMasterInst;
  interface AXILiteMaster#(ADDR_sz, DATA_sz) axiLiteMasterData;
endinterface
module mkMemShim (MemShim);

  // 2 AXI shims
  List#(AXILiteShim#(ADDR_sz, DATA_sz)) shim <- replicateM(2, mkAXILiteShim);
  // 2 memory interfaces
  Mem#(Bit#(ADDR_sz), Bit#(DATA_sz)) m[2];
  for (Integer i = 0; i < 2; i = i + 1) begin
    // which response ?
    let expectWriteRsp <- mkFIFO;
    let rspFF <- mkBypassFIFO;
    // drain responses
    (* mutually_exclusive = "drainBChannel, drainRChannel"*)
    rule drainBChannel (expectWriteRsp.first);
      let tmp <- shim[i].slave.b.get;
      rspFF.enq(fromAXIBLiteFlit(tmp));
      expectWriteRsp.deq;
    endrule
    rule drainRChannel (!expectWriteRsp.first);
      let tmp <- shim[i].slave.r.get;
      rspFF.enq(fromAXIRLiteFlit(tmp));
      expectWriteRsp.deq;
    endrule
    // convert requests/responses
    m[i] = interface Mem;
      interface request = interface Put;
        method put (req) = action
          case (req) matches
            tagged ReadReq .r: begin
              shim[i].slave.ar.put(toAXIARLiteFlit(req));
              expectWriteRsp.enq(False);
            end
            tagged WriteReq .w: begin
              shim[i].slave.aw.put(toAXIAWLiteFlit(req));
              shim[i].slave.w.put(toAXIWLiteFlit(req));
              expectWriteRsp.enq(True);
            end
          endcase
        endaction;
      endinterface;
      interface response = toGet(rspFF);
    endinterface;
  end
  // wire up interfaces
  interface internal = m;
  interface axiLiteMasterInst = shim[0].master;
  interface axiLiteMasterData = shim[1].master;

endmodule

/////////////////
// RVBS module //
////////////////////////////////////////////////////////////////////////////////

module [Module] mkRVBSCore#(RVState s,
                   function ISADefModule#(Empty) init (RVState st),
                   function ISADefModule#(Empty) iFetch (RVState st))
  (BIDProbes);

  // instanciating simulator
  let modList = list(init, iFetch, mkRVTrap, mkRV32I);
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

  return bid_probes;

endmodule

`ifndef RVFI_DII
(* synthesize *)
module mkRVBS#(parameter VAddr reset_pc) (RVBS_Ifc);

  // create the memory shim
  let mem <- mkMemShim;
  // prepare state
  `ifdef SUPERVISOR_MODE
  Mem#(PAddr, Bit#(IMemWidth)) imem[2] <- virtualize(mem.internal[0], 2);
  Mem#(PAddr, Bit#(DMemWidth)) dmem[2] <- virtualize(mem.internal[1], 2);
  RVState s <- mkState(reset_pc, imem[1], dmem[1], imem[0], dmem[0]);
  `else
  RVState s <- mkState(reset_pc, mem.internal[0], mem.internal[1]);
  `endif
  // initialization
  module [ISADefModule] mkRVInit#(RVState st) ();
    defineInitEntry(rSeq(rBlock(action
      st.regFile.r[10] <= 0;
    endaction, action
      st.regFile.r[11] <= 'h00004000;
    endaction)));
  endmodule
  // instanciating simulator
  let bid_probes <- mkRVBSCore(s, mkRVInit, mkRVIFetch);

  method Bit#(XLEN) peekPC() = s.pc;
  method Bit#(XLEN) peekCtrlCSR() = s.csrs.ctrl;
  interface probes = bid_probes;
  method setMSIP = s.csrs.setMSIP;
  method setMTIP = s.csrs.setMTIP;
  method setMEIP = s.csrs.setMEIP;
  interface axiLiteMasterInst = mem.axiLiteMasterInst;
  interface axiLiteMasterData = mem.axiLiteMasterData;

endmodule

(* synthesize *)
module rvbs#(parameter VAddr reset_pc) (RVBS_Ifc_Synth);
  let ifc <- mkRVBS(reset_pc);
  let m0 <- toAXILiteMasterSynth(ifc.axiLiteMasterInst);
  let m1 <- toAXILiteMasterSynth(ifc.axiLiteMasterData);
  method peekPC = ifc.peekPC;
  method peekCtrlCSR = ifc.peekCtrlCSR;
  interface probes = ifc.probes;
  method setMSIP = ifc.setMSIP;
  method setMTIP = ifc.setMTIP;
  method setMEIP = ifc.setMEIP;
  interface axiLiteMasterInst = m0;
  interface axiLiteMasterData = m1;
endmodule

`else // RVFI_DII

(* synthesize *)
module mkRVBS (Empty);

  // create the RVFI-DII bridge
  let bridge <- mkRVFI_DII_Bridge("RVFI_DII", 5000);
  // create a memory
  module memShim (Array#(Mem#(PAddr, Bit#(DATA_sz))));
    Mem#(PAddr, Bit#(DATA_sz)) mem[2] <- mkMemSimWithOffset(2, 'h80000000, 'h10000, "/dev/null");
    // 2 memory interfaces
    Mem#(PAddr, Bit#(DATA_sz)) m[2];
    for (Integer i = 0; i < 2; i = i + 1) begin
      let   rspFF <- mkBypassFIFO;
      let errorFF <- mkFIFO;
      // get responses
      rule drainMemRsp(!errorFF.first);
        let tmp <- mem[i].response.get;
        rspFF.enq(tmp);
        errorFF.deq;
      endrule
      rule errorRsp(errorFF.first);
        rspFF.enq(BusError);
        errorFF.deq;
      endrule
      // convert requests/responses
      m[i] = interface Mem;
        interface request = interface Put;
          method put (req) = action
            case (req) matches
              tagged ReadReq .r &&& (r.addr >= 'h80000000 && r.addr < 'h8001000): begin
                mem[i].request.put(req);
                errorFF.enq(False);
              end
              tagged WriteReq .w &&& (w.addr >= 'h80000000 && w.addr < 'h8001000): begin
                mem[i].request.put(req);
                errorFF.enq(False);
              end
              default: errorFF.enq(True);
            endcase
          endaction;
        endinterface;
        interface response = toGet(rspFF);
      endinterface;
    end
    return m;
  endmodule
  Mem#(PAddr, Bit#(DATA_sz)) mem[2] <- memShim(reset_by bridge.new_rst);
  // prepare state
  `ifdef SUPERVISOR_MODE
  Mem#(PAddr, Bit#(IMemWidth)) imem[2] <- virtualize(mem[0], 2, reset_by bridge.new_rst);
  Mem#(PAddr, Bit#(DMemWidth)) dmem[2] <- virtualize(mem[1], 2, reset_by bridge.new_rst);
  RVState s <- mkState(?, imem[1], dmem[1], imem[0], dmem[0], reset_by bridge.new_rst);
  `else
  RVState s <- mkState(?, mem[0], mem[1], bridge, reset_by bridge.new_rst);
  `endif
  // initialization
  module [ISADefModule] mkRVInit#(RVState st) (Empty);
    Reg#(Bit#(6)) cnt <- mkRegU;
    defineInitEntry(rSeq(rBlock(
      printTLogPlusArgs("itrace", "-------- Reseting --------"),
      action s.pc <= 'h8000000; endaction, action s.pc.commit; endaction,
      writeReg(cnt, 0),
      rWhile(cnt < 32, rAct(action
      s.regFile.r[cnt] <= 0;
      cnt <= cnt + 1;
    endaction))
  )));
  endmodule
  // instanciating simulator
  let bid_probes <- mkRVBSCore(s, mkRVInit, mkRVIFetch_RVFI_DII, reset_by bridge.new_rst);

endmodule
`endif
