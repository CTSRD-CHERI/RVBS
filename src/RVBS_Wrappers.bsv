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
`define AXI4_PARAMS PAddrWidth, 128, 0, 1, 0, 0, 1
`else
`define AXI4_PARAMS PAddrWidth, 128, 0, 0, 0, 0, 0
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
  interface AXI4Lite_Master#(`AXI4_PARAMS) instAXI4Lite_Master;
  interface AXI4Lite_Master#(`AXI4_PARAMS) dataAXI4Lite_Master;
endinterface

(* always_ready, always_enabled *)
interface SOC;
  interface AXI4Lite_Slave#(`AXI4_PARAMS) instAXI4Lite_Slave;
  interface AXI4Lite_Slave#(`AXI4_PARAMS) dataAXI4Lite_Slave;
  method Bool peekMEIP;
  method Bool peekMTIP;
  method Bool peekMSIP;
endinterface
instance Connectable#(RVBS, SOC);
  module mkConnection#(RVBS rvbs, SOC soc) (Empty);
    mkConnection(rvbs.instAXI4Lite_Master, soc.instAXI4Lite_Slave);
    mkConnection(rvbs.dataAXI4Lite_Master, soc.dataAXI4Lite_Slave);
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
  interface AXI4Lite_Master_Synth#(`AXI4_PARAMS) instAXI4Lite_Master;
  interface AXI4Lite_Master_Synth#(`AXI4_PARAMS) dataAXI4Lite_Master;
endinterface

interface RVBS_CLINT;
  // probing interfaces
  method Bit#(XLEN) peekPC;
  method Bit#(XLEN) peekCtrlCSR;
  interface BIDProbes probes;
  // riscv interfaces
  method Action setMEIP(Bool irq);
  interface AXI4Lite_Master#(`AXI4_PARAMS) instAXI4Lite_Master;
  interface AXI4Lite_Master#(`AXI4_PARAMS) dataAXI4Lite_Master;
endinterface

(* always_ready, always_enabled *)
interface SOC_NO_CLINT;
  interface AXI4Lite_Slave#(`AXI4_PARAMS) instAXI4Lite_Slave;
  interface AXI4Lite_Slave#(`AXI4_PARAMS) dataAXI4Lite_Slave;
  method Bool peekMEIP;
endinterface
instance Connectable#(RVBS_CLINT, SOC_NO_CLINT);
  module mkConnection#(RVBS_CLINT rvbs, SOC_NO_CLINT soc) (Empty);
    mkConnection(rvbs.instAXI4Lite_Master, soc.instAXI4Lite_Slave);
    mkConnection(rvbs.dataAXI4Lite_Master, soc.dataAXI4Lite_Slave);
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
  interface AXI4Lite_Master_Synth#(`AXI4_PARAMS) instAXI4Lite_Master;
  interface AXI4Lite_Master_Synth#(`AXI4_PARAMS) dataAXI4Lite_Master;
endinterface

//////////////////////////////////
// Internal memory to AXI4 shim //
////////////////////////////////////////////////////////////////////////////////

interface RVMemShim;
  interface Array#(RVMem) internalMem;
  interface AXI4Lite_Master#(`AXI4_PARAMS) instAXI4Lite_Master;
  interface AXI4Lite_Master#(`AXI4_PARAMS) dataAXI4Lite_Master;
endinterface
typedef enum {READ, WRITE} RspType deriving (Bits, Eq);
module mkRVMemShim (RVMemShim);

  // 2 AXI4 shims
  List#(AXI4Lite_Shim#(`AXI4_PARAMS)) shim <- replicateM(2, mkAXI4LiteShim);
  // 2 memory interfaces
  RVMem m[2];
  for (Integer i = 0; i < 2; i = i + 1) begin
    FIFOF#(RspType) nextRsp <- mkFIFOF;
    let reqFF <- mkBypassFIFOF;
    let rspFF <- mkBypassFIFOF;
    let pendingExcFF <- mkFIFOF;
    let readReqNext <- mkReg(False);
    let readRspNext <- mkReg(Invalid);
    let pendingReadFF <- mkFIFOF;
    let writeReqNext <- mkReg(Invalid);
    let writeRspNext <- mkReg(Invalid);
    let pendingWriteFF <- mkFIFOF;
    // forward requests to AXI
    rule handleReq (!(readReqNext || isValid(writeReqNext)));
      case (reqFF.first) matches
        // Handle read requests
        tagged RVReadReq .r: begin
          // check for need of 2 AXI4Lite requests
          Bool needMore = False;
          Bit#(6) firstOut = zeroExtend(r.addr[3:0]) + zeroExtend(readBitPO(r.numBytes));
          if (firstOut > 16) begin
            readReqNext <= True;
            needMore = True;
          end
          else reqFF.deq;
          // send first AXI4Lite request
          shim[i].slave.ar.put(AXI4Lite_ARFlit{
            araddr: pack(r.addr), arprot: 0, aruser: 0
          });
          nextRsp.enq(READ);
          pendingReadFF.enq(tuple2(r.addr[3:0], needMore));
        end
        // Handle write requests
        tagged RVWriteReq .w: begin
          // check for need of 2 AXI4Lite requests
          Bit#(6) firstIn  = zeroExtend(w.addr[3:0]) + zeroExtend(pack(countZerosLSB(w.byteEnable)));
          Bit#(6) firstOut = zeroExtend(w.addr[3:0]) + (16 - zeroExtend(pack(countZerosMSB(w.byteEnable))));
          Bool needMore = (firstIn < 16 && firstOut > 16);
          // initialise values to write
          Bit#(8) dataShift = zeroExtend(w.addr[3:0]) << 3;
          Bit#(5)   beShift = zeroExtend(w.addr[3:0]);
          Bit#(128)   wData = pack(w.data) << dataShift;
          Bit#(16)    wStrb = w.byteEnable << beShift;
          if (firstIn > 16) begin
            dataShift = (16 - zeroExtend(w.addr[3:0])) << 3;
            beShift = (16 - zeroExtend(w.addr[3:0]));
            wData = pack(w.data) >> dataShift;
            wStrb = w.byteEnable >> beShift;
          end
          // send AXI4Lite request
          shim[i].slave.aw.put(toAXI4Lite_AWFlit(reqFF.first));
          shim[i].slave.w.put(AXI4Lite_WFlit{
            wdata: wData,
            wstrb: wStrb,
            `ifdef RVXCHERI
            wuser: (needMore) ? 0 : w.captag
            `else
            wuser: 0
            `endif
          });
          nextRsp.enq(WRITE);
          pendingWriteFF.enq(needMore);
          if (needMore) writeReqNext <= Valid(16 - zeroExtend(w.addr[3:0]));
          else reqFF.deq;
        end
      endcase
    endrule
    rule handleNextReadReq (readReqNext && !isValid(writeReqNext));
      shim[i].slave.ar.put(AXI4Lite_ARFlit{
        araddr: pack(reqFF.first.RVReadReq.addr + 16), arprot: 0, aruser: 0
      });
      nextRsp.enq(READ);
      reqFF.deq;
      readReqNext <= False;
    endrule
    rule handleNextWriteReq (isValid(writeReqNext) && !readReqNext);
      Bit#(5) beShift = writeReqNext.Valid;
      Bit#(8) dataShift = zeroExtend(beShift) << 3;
      shim[i].slave.aw.put(toAXI4Lite_AWFlit(reqFF.first));
      shim[i].slave.w.put(AXI4Lite_WFlit{
        wdata: reqFF.first.RVWriteReq.data >> dataShift,
        wstrb: reqFF.first.RVWriteReq.byteEnable >> beShift,
        wuser: 0
      });
      nextRsp.enq(WRITE);
      reqFF.deq;
      writeReqNext <= Invalid;
    endrule
    // drain write responses
    rule drainBChannel (!pendingExcFF.notEmpty && nextRsp.first == WRITE && !isValid(writeRspNext));
      let tmp <- get(shim[i].slave.b);
      nextRsp.deq;
      if (!pendingWriteFF.first) begin
        rspFF.enq(Right(fromAXI4Lite_BFlit(tmp)));
        pendingWriteFF.deq;
      end else writeRspNext <= Valid(tmp);
    endrule
    rule drainBChannelNext (!pendingExcFF.notEmpty && nextRsp.first == WRITE && isValid(writeRspNext));
      nextRsp.deq;
      pendingWriteFF.deq;
      writeRspNext <= Invalid;
      let tmp <- get(shim[i].slave.b);
      let rsp = writeRspNext.Valid;
      if (tmp.bresp matches OKAY) rspFF.enq(Right(fromAXI4Lite_BFlit(rsp)));
      else rspFF.enq(Right(RVBusError));
    endrule
    // drain read responses
    rule drainRChannel (!pendingExcFF.notEmpty && nextRsp.first == READ && !isValid(readRspNext));
      nextRsp.deq;
      let tmp <- get(shim[i].slave.r);
      match {.offset, .needMore} = pendingReadFF.first;
      Bit#(7) shiftAmnt = zeroExtend(offset) << 3;
      tmp.rdata = tmp.rdata >> shiftAmnt;
      if (!needMore) begin
        rspFF.enq(Right(fromAXI4Lite_RFlit(tmp)));
        pendingReadFF.deq;
      end else readRspNext <= Valid(tmp);
    endrule
    rule drainRChannelNext (!pendingExcFF.notEmpty && nextRsp.first == READ && isValid(readRspNext));
      nextRsp.deq;
      pendingReadFF.deq;
      readRspNext <= Invalid;
      let tmp <- get(shim[i].slave.r);
      let rsp = readRspNext.Valid;
      match {.offset, .needMore} = pendingReadFF.first;
      Bit#(7) shiftAmnt = (16 - zeroExtend(offset)) << 3;
      if (tmp.rresp matches OKAY) begin
        rsp.rdata = (rsp.rdata & ~(~0 << shiftAmnt)) | (tmp.rdata << shiftAmnt);
        rspFF.enq(Right(fromAXI4Lite_RFlit(rsp)));
      end else rspFF.enq(Right(RVBusError));
    endrule
    // drain exception FIFO
    rule drainException (pendingExcFF.notEmpty);
      pendingExcFF.deq;
      let exc = pendingExcFF.first;
      rspFF.enq(Left(exc));
    endrule
    // convert requests/responses
    m[i] = interface RVMem;
      interface sink = interface Sink;
        method canPut = nextRsp.notFull;
        method put (e_req) = case (e_req) matches
          tagged Left .excTok: pendingExcFF.enq(excTok);
          tagged Right .req: reqFF.enq(req);
        endcase;
      endinterface;
      interface source = toSource(rspFF);
    endinterface;
  end
  // wire up interfaces
  interface internalMem = m;
  interface instAXI4Lite_Master = shim[0].master;
  interface dataAXI4Lite_Master = shim[1].master;

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
      st.wGPR(10, 0);
    endaction, action
      st.wGPR(11, 'h00004000);
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
  interface instAXI4Lite_Master = mem.instAXI4Lite_Master;
  interface dataAXI4Lite_Master = mem.dataAXI4Lite_Master;

endmodule

(* synthesize *)
module mkRVBS_synth#(parameter VAddr reset_pc) (RVBS_synth);
  let ifc <- mkRVBS(reset_pc);
  let m0 <- toAXI4Lite_Master_Synth(ifc.instAXI4Lite_Master);
  let m1 <- toAXI4Lite_Master_Synth(ifc.dataAXI4Lite_Master);
  method      peekPC = ifc.peekPC;
  method peekCtrlCSR = ifc.peekCtrlCSR;
  interface   probes = ifc.probes;
  method     setMSIP = ifc.setMSIP;
  method     setMTIP = ifc.setMTIP;
  method     setMEIP = ifc.setMEIP;
  interface instAXI4Lite_Master = m0;
  interface dataAXI4Lite_Master = m1;
endmodule

/////////////////////////////
// RVBS + CLINT top module //
////////////////////////////////////////////////////////////////////////////////

(* synthesize *)
module mkRVBS_CLINT#(parameter VAddr reset_pc) (RVBS_CLINT);



  let  rvbs <- mkRVBS(reset_pc);
  let clint <- mkAXI4LiteCLINT;
  `ifndef RVXCHERI
  AXI4Lite_Slave#(`AXI4_PARAMS) clintSlave = clint.axiLiteSlave;
  `else
  AXI4Lite_Slave#(`AXI4_PARAMS) clintSlave = dropUserFields(clint.axiLiteSlave);
  `endif
  let  shim <- mkAXI4LiteShim;
  let clintWriteRspFF <- mkFIFOF;
  let  clintReadRspFF <- mkFIFOF;

  rule connectWriteReq (rvbs.dataAXI4Lite_Master.aw.canPeek &&
                     rvbs.dataAXI4Lite_Master.w.canPeek);
    let awflit <- get(rvbs.dataAXI4Lite_Master.aw);
    let  wflit <- get(rvbs.dataAXI4Lite_Master.w);
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

  rule connectClintB (clintSlave.b.canPeek && clintWriteRspFF.first);
    let bflit <- get(clintSlave.b);
    rvbs.dataAXI4Lite_Master.b.put(bflit);
    clintWriteRspFF.deq;
  endrule

  rule connectShimB (shim.slave.b.canPeek && !clintWriteRspFF.first);
    let bflit <- get(shim.slave.b);
    rvbs.dataAXI4Lite_Master.b.put(bflit);
    clintWriteRspFF.deq;
  endrule

  rule connectAR (rvbs.dataAXI4Lite_Master.ar.canPeek);
    let arflit <- get(rvbs.dataAXI4Lite_Master.ar);
    if (arflit.araddr >= 'h02000000 && arflit.araddr < 'h02001000) begin
      clintSlave.ar.put(arflit);
      clintReadRspFF.enq(True);
    end else begin
      shim.slave.ar.put(arflit);
      clintReadRspFF.enq(False);
    end
  endrule

  rule connectClintR (clintSlave.r.canPeek && clintReadRspFF.first);
    let rflit <- get(clintSlave.r);
    rvbs.dataAXI4Lite_Master.r.put(rflit);
    clintReadRspFF.deq;
  endrule

  rule connectShimR (shim.slave.r.canPeek && !clintReadRspFF.first);
    let rflit <- get(shim.slave.r);
    rvbs.dataAXI4Lite_Master.r.put(rflit);
    clintReadRspFF.deq;
  endrule

  rule connectMSIP; rvbs.setMSIP(clint.peekMSIP); endrule
  rule connectMTIP; rvbs.setMTIP(clint.peekMTIP); endrule

  method      peekPC = rvbs.peekPC;
  method peekCtrlCSR = rvbs.peekCtrlCSR;
  interface   probes = rvbs.probes;
  method     setMEIP = rvbs.setMEIP;
  interface instAXI4Lite_Master = rvbs.instAXI4Lite_Master;
  interface dataAXI4Lite_Master = shim.master;

endmodule

(* synthesize *)
module mkRVBS_CLINT_synth#(parameter VAddr reset_pc) (RVBS_CLINT_synth);
  let ifc <- mkRVBS_CLINT(reset_pc);
  let m0 <- toAXI4Lite_Master_Synth(ifc.instAXI4Lite_Master);
  let m1 <- toAXI4Lite_Master_Synth(ifc.dataAXI4Lite_Master);
  method      peekPC = ifc.peekPC;
  method peekCtrlCSR = ifc.peekCtrlCSR;
  interface   probes = ifc.probes;
  method     setMEIP = ifc.setMEIP;
  interface instAXI4Lite_Master = m0;
  interface dataAXI4Lite_Master = m1;
endmodule

`undef AXI4_PARAMS
