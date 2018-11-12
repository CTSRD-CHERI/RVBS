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

import        FIFOF :: *;
import SpecialFIFOs :: *;
import  Connectable :: *;

import   BlueBasics :: *;
import    BlueUtils :: *;
import          AXI :: *;
import       Recipe :: *;
import          BID :: *;
import         RVBS :: *;
import    RVBS_Core :: *;
import        CLINT :: *;

// provided interfaces
export RVBS(..);
export SOC(..);
export RVBS_synth(..);
export RVBS_CLINT(..);
export SOC_NO_CLINT(..);
export RVBS_CLINT_synth(..);
// provided wrappers
export mkRVBS;
export mkRVBS_synth;
export mkRVBS_CLINT;
export mkRVBS_CLINT_synth;

`ifdef RVXCHERI
`define AXI_PARAMS PAddrWidth, 128, 0, 1, 0, 0, 1
`else
`define AXI_PARAMS PAddrWidth, 128, 0, 0, 0, 0, 0
`endif
////////////////
// Interfaces //
////////////////////////////////////////////////////////////////////////////////

interface RVBS;
  // probing interfaces
  method Bit#(XLEN) peekPC;
  method Bit#(XLEN) peekCtrlCSR;
  interface BIDProbes probes;
  // riscv interfaces
  method Action setMSIP(Bool irq);
  method Action setMTIP(Bool irq);
  method Action setMEIP(Bool irq);
  interface AXILiteMaster#(`AXI_PARAMS) instAXILiteMaster;
  interface AXILiteMaster#(`AXI_PARAMS) dataAXILiteMaster;
endinterface

(* always_ready, always_enabled *)
interface SOC;
  interface AXILiteSlave#(`AXI_PARAMS) instAXILiteSlave;
  interface AXILiteSlave#(`AXI_PARAMS) dataAXILiteSlave;
  method Bool peekMEIP;
  method Bool peekMTIP;
  method Bool peekMSIP;
endinterface
instance Connectable#(RVBS, SOC);
  module mkConnection#(RVBS rvbs, SOC soc) (Empty);
    mkConnection(rvbs.instAXILiteMaster, soc.instAXILiteSlave);
    mkConnection(rvbs.dataAXILiteMaster, soc.dataAXILiteSlave);
    rule connect_interrupts;
      rvbs.setMEIP(soc.peekMEIP);
      rvbs.setMTIP(soc.peekMTIP);
      rvbs.setMSIP(soc.peekMSIP);
    endrule
  endmodule
endinstance

(* always_ready, always_enabled *)
interface RVBS_synth;
  // probing interfaces
  method Bit#(XLEN) peekPC;
  method Bit#(XLEN) peekCtrlCSR;
  interface BIDProbes probes;
  // riscv interfaces
  method Action setMSIP(Bool irq);
  method Action setMTIP(Bool irq);
  method Action setMEIP(Bool irq);
  interface AXILiteMasterSynth#(`AXI_PARAMS) instAXILiteMaster;
  interface AXILiteMasterSynth#(`AXI_PARAMS) dataAXILiteMaster;
endinterface

interface RVBS_CLINT;
  // probing interfaces
  method Bit#(XLEN) peekPC;
  method Bit#(XLEN) peekCtrlCSR;
  interface BIDProbes probes;
  // riscv interfaces
  method Action setMEIP(Bool irq);
  interface AXILiteMaster#(`AXI_PARAMS) instAXILiteMaster;
  interface AXILiteMaster#(`AXI_PARAMS) dataAXILiteMaster;
endinterface

(* always_ready, always_enabled *)
interface SOC_NO_CLINT;
  interface AXILiteSlave#(`AXI_PARAMS) instAXILiteSlave;
  interface AXILiteSlave#(`AXI_PARAMS) dataAXILiteSlave;
  method Bool peekMEIP;
endinterface
instance Connectable#(RVBS_CLINT, SOC_NO_CLINT);
  module mkConnection#(RVBS_CLINT rvbs, SOC_NO_CLINT soc) (Empty);
    mkConnection(rvbs.instAXILiteMaster, soc.instAXILiteSlave);
    mkConnection(rvbs.dataAXILiteMaster, soc.dataAXILiteSlave);
    rule connect_interrupts;
      rvbs.setMEIP(soc.peekMEIP);
    endrule
  endmodule
endinstance

(* always_ready, always_enabled *)
interface RVBS_CLINT_synth;
  // probing interfaces
  method Bit#(XLEN) peekPC;
  method Bit#(XLEN) peekCtrlCSR;
  interface BIDProbes probes;
  // riscv interfaces
  method Action setMEIP(Bool irq);
  interface AXILiteMasterSynth#(`AXI_PARAMS) instAXILiteMaster;
  interface AXILiteMasterSynth#(`AXI_PARAMS) dataAXILiteMaster;
endinterface

/////////////////////////////////
// Internal memory to AXI shim //
////////////////////////////////////////////////////////////////////////////////

interface RVMemShim;
  interface Array#(RVMem) internalMem;
  interface AXILiteMaster#(`AXI_PARAMS) instAXILiteMaster;
  interface AXILiteMaster#(`AXI_PARAMS) dataAXILiteMaster;
endinterface
module mkRVMemShim (RVMemShim);

  // 2 AXI shims
  List#(AXILiteShim#(`AXI_PARAMS)) shim <- replicateM(2, mkAXILiteShim);
  // 2 memory interfaces
  RVMem m[2];
  for (Integer i = 0; i < 2; i = i + 1) begin
    // which response ?
    let expectWriteRsp <- mkFIFOF;
    let rspFF <- mkBypassFIFOF;
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
    m[i] = interface RVMem;
      interface sink = interface Sink;
        method canPut = expectWriteRsp.notFull;
        method put (req) = action
          case (req) matches
            tagged RVReadReq .r: begin
              shim[i].slave.ar.put(toAXIARLiteFlit(req));
              expectWriteRsp.enq(False);
            end
            tagged RVWriteReq .w: begin
              shim[i].slave.aw.put(toAXIAWLiteFlit(req));
              shim[i].slave.w.put(toAXIWLiteFlit(req));
              expectWriteRsp.enq(True);
            end
          endcase
        endaction;
      endinterface;
      interface source = toSource(rspFF);
    endinterface;
  end
  // wire up interfaces
  interface internalMem = m;
  interface instAXILiteMaster = shim[0].master;
  interface dataAXILiteMaster = shim[1].master;

endmodule

////////////////////////////
// Simple RVBS top module //
////////////////////////////////////////////////////////////////////////////////

(* synthesize *)
module mkRVBS#(parameter VAddr reset_pc) (RVBS);

  // create the memory shim
  let mem <- mkRVMemShim;
  // prepare state
  `ifdef SUPERVISOR_MODE
  RVMem imem[2] <- virtualize(mem.internalMem[0], 2);
  RVMem dmem[2] <- virtualize(mem.internalMem[1], 2);
  RVState s <- mkState(reset_pc, imem[1], dmem[1], imem[0], dmem[0]);
  `else
  RVState s <- mkState(reset_pc, mem.internalMem[0], mem.internalMem[1]);
  `endif
  // initialization
  module [ISADefModule] mkRVInit#(RVState st) (Empty);
    defineInitEntry(rSeq(rBlock(action
      st.regFile.r[10] <= 0;
    endaction, action
      st.regFile.r[11] <= 'h00004000;
    endaction)));
  endmodule
  // instanciating simulator
  let bid_probes <- mkRVBSCore(s, mkRVInit, mkRVIFetch);

  method      peekPC = s.pc;
  method peekCtrlCSR = s.csrs.ctrl;
  interface   probes = bid_probes;
  method     setMSIP = s.csrs.setMSIP;
  method     setMTIP = s.csrs.setMTIP;
  method     setMEIP = s.csrs.setMEIP;
  interface instAXILiteMaster = mem.instAXILiteMaster;
  interface dataAXILiteMaster = mem.dataAXILiteMaster;

endmodule

(* synthesize *)
module mkRVBS_synth#(parameter VAddr reset_pc) (RVBS_synth);
  let ifc <- mkRVBS(reset_pc);
  let m0 <- toAXILiteMasterSynth(ifc.instAXILiteMaster);
  let m1 <- toAXILiteMasterSynth(ifc.dataAXILiteMaster);
  method      peekPC = ifc.peekPC;
  method peekCtrlCSR = ifc.peekCtrlCSR;
  interface   probes = ifc.probes;
  method     setMSIP = ifc.setMSIP;
  method     setMTIP = ifc.setMTIP;
  method     setMEIP = ifc.setMEIP;
  interface instAXILiteMaster = m0;
  interface dataAXILiteMaster = m1;
endmodule

/////////////////////////////
// RVBS + CLINT top module //
////////////////////////////////////////////////////////////////////////////////

(* synthesize *)
module mkRVBS_CLINT#(parameter VAddr reset_pc) (RVBS_CLINT);



  let  rvbs <- mkRVBS(reset_pc);
  let clint <- mkAXILiteCLINT;
  `ifndef RVXCHERI
  AXILiteSlave#(`AXI_PARAMS) clintSlave = clint.axiLiteSlave;
  `else
  AXILiteSlave#(`AXI_PARAMS) clintSlave = dropUserFields(clint.axiLiteSlave);
  `endif
  let  shim <- mkAXILiteShim;
  let clintWriteRspFF <- mkFIFOF;
  let  clintReadRspFF <- mkFIFOF;

  rule connectWriteReq (rvbs.dataAXILiteMaster.aw.canGet &&
                     rvbs.dataAXILiteMaster.w.canGet);
    let awflit <- rvbs.dataAXILiteMaster.aw.get;
    let  wflit <- rvbs.dataAXILiteMaster.w.get;
    if (awflit.awaddr >= 'h02000000 && awflit.awaddr < 'h02001000) begin
      clintSlave.aw.put(awflit);
      clintSlave.w.put(wflit);
      clintWriteRspFF.enq(True);
    end else begin
      shim.slave.aw.put(awflit);
      shim.slave.w.put(wflit);
      clintWriteRspFF.enq(False);
    end
  endrule

  rule connectClintB (clintSlave.b.canGet && clintWriteRspFF.first);
    let bflit <- clintSlave.b.get;
    rvbs.dataAXILiteMaster.b.put(bflit);
    clintWriteRspFF.deq;
  endrule

  rule connectShimB (shim.slave.b.canGet && !clintWriteRspFF.first);
    let bflit <- shim.slave.b.get;
    rvbs.dataAXILiteMaster.b.put(bflit);
    clintWriteRspFF.deq;
  endrule

  rule connectAR (rvbs.dataAXILiteMaster.ar.canGet);
    let arflit <- rvbs.dataAXILiteMaster.ar.get;
    if (arflit.araddr >= 'h02000000 && arflit.araddr < 'h02001000) begin
      clintSlave.ar.put(arflit);
      clintReadRspFF.enq(True);
    end else begin
      shim.slave.ar.put(arflit);
      clintReadRspFF.enq(False);
    end
  endrule

  rule connectClintR (clintSlave.r.canGet && clintReadRspFF.first);
    let rflit <- clintSlave.r.get;
    rvbs.dataAXILiteMaster.r.put(rflit);
    clintReadRspFF.deq;
  endrule

  rule connectShimR (shim.slave.r.canGet && !clintReadRspFF.first);
    let rflit <- shim.slave.r.get;
    rvbs.dataAXILiteMaster.r.put(rflit);
    clintReadRspFF.deq;
  endrule

  rule connectMSIP; rvbs.setMSIP(clint.peekMSIP); endrule
  rule connectMTIP; rvbs.setMTIP(clint.peekMTIP); endrule

  method      peekPC = rvbs.peekPC;
  method peekCtrlCSR = rvbs.peekCtrlCSR;
  interface   probes = rvbs.probes;
  method     setMEIP = rvbs.setMEIP;
  interface instAXILiteMaster = rvbs.instAXILiteMaster;
  interface dataAXILiteMaster = shim.master;

endmodule

(* synthesize *)
module mkRVBS_CLINT_synth#(parameter VAddr reset_pc) (RVBS_CLINT_synth);
  let ifc <- mkRVBS_CLINT(reset_pc);
  let m0 <- toAXILiteMasterSynth(ifc.instAXILiteMaster);
  let m1 <- toAXILiteMasterSynth(ifc.dataAXILiteMaster);
  method      peekPC = ifc.peekPC;
  method peekCtrlCSR = ifc.peekCtrlCSR;
  interface   probes = ifc.probes;
  method     setMEIP = ifc.setMEIP;
  interface instAXILiteMaster = m0;
  interface dataAXILiteMaster = m1;
endmodule

`undef AXI_PARAMS
