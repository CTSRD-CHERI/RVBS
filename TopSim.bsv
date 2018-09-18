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
import DefaultValue :: *;
import SpecialFIFOs :: *;
import ClientServer :: *;
import GetPut :: *;
import Connectable :: *;
import List :: *;
import Vector :: *;
import LFSR :: *;

import ListExtra :: *;
import Routable :: *;
import BlueUtils :: *;
import SourceSink :: *;
import AXI4Lite :: *;
import TopRVBS :: *;
import CLINT :: *;

`ifdef XLEN64
typedef 56 ADDR_sz;
typedef 64 DATA_sz;
`else
typedef 34 ADDR_sz;
typedef 32 DATA_sz;
`endif

// memory subsystem
////////////////////////////////////////////////////////////////////////////////
(* always_ready, always_enabled *)
interface RVBS_Mem_Slave;
  interface AXILiteSlave#(ADDR_sz, DATA_sz) axiLiteSlave0;
  interface AXILiteSlave#(ADDR_sz, DATA_sz) axiLiteSlave1;
  method Bool peekMEIP;
  method Bool peekMTIP;
  method Bool peekMSIP;
endinterface
instance Connectable#(RVBS_Ifc, RVBS_Mem_Slave);
  module mkConnection#(RVBS_Ifc c, RVBS_Mem_Slave m) (Empty);
    mkConnection(c.axiLiteMaster0, m.axiLiteSlave0);
    mkConnection(c.axiLiteMaster1, m.axiLiteSlave1);
    rule connect_interrupts;
      c.setMEIP(m.peekMEIP);
      c.setMTIP(m.peekMTIP);
      c.setMSIP(m.peekMSIP);
    endrule
  endmodule
endinstance

typedef 2 NMASTERS;
typedef 1 NSLAVES;
`define MASTER_T AXILiteMaster#(ADDR_sz, DATA_sz)
`define SLAVE_T AXILiteSlave#(ADDR_sz, DATA_sz)
module memoryMap (RVBS_Mem_Slave);
  // input shims
  AXILiteShim#(ADDR_sz, DATA_sz) shim0 <- mkAXILiteShim;
  AXILiteShim#(ADDR_sz, DATA_sz) shim1 <- mkAXILiteShim;
  // clint
  AXILiteCLINT#(ADDR_sz, DATA_sz) clint <- mkAXILiteCLINT;
  // interconnect
  Vector#(NMASTERS, `MASTER_T) ms;
  ms[0] = shim0.master;
  ms[1] = shim1.master;
  Vector#(NSLAVES, `SLAVE_T) ss;
  ss[0] = clint.axiLiteSlave;
  MappingTable#(NSLAVES, ADDR_sz) maptab = newVector;
  maptab[0] = Range{base: 'h02000000, size: 'h10000};
  mkAXILiteBus(maptab, ms, ss);
  // interfaces
  interface axiLiteSlave0 = shim0.slave;
  interface axiLiteSlave1 = shim1.slave;
  method Bool peekMEIP = False;
  method Bool peekMTIP = clint.peekMTIP;
  method Bool peekMSIP = clint.peekMSIP;
endmodule
`undef MASRTER_T
`undef SLAVE_T

// local memory wrapper
////////////////////////////////////////////////////////////////////////////////
module localMemWrapper#(RVBS_Ifc rvbs) (RVBS_Ifc);

  // memory module
  `ifdef MEM_IMG
  String memimg = `MEM_IMG;
  `else
  String memimg = "test-prog.hex";
  `endif
  `ifdef MEM_SIZE
  Integer memsize = `MEM_SIZE;
  `else
  Integer memsize = 'h10000;
  `endif
  Integer membase = 'h80000000;
  Mem2#(Bit#(ADDR_sz), Bit#(DATA_sz), Bit#(DATA_sz)) mem <- mkSharedMem2(memsize, memimg);

  // mem req helpers
  function MemReq#(Bit#(ADDR_sz), Bit#(DATA_sz))
    fromAXILiteToWriteReq(AWLiteFlit#(ADDR_sz) aw, WLiteFlit#(DATA_sz) w, Int#(ADDR_sz) offset) =
      WriteReq {addr: pack(unpack(aw.awaddr) + offset), byteEnable: w.wstrb, data: w.wdata};
  function MemReq#(Bit#(ADDR_sz), Bit#(DATA_sz))
    fromAXILiteToReadReq(ARLiteFlit#(ADDR_sz) ar, Int#(ADDR_sz) offset) =
      ReadReq {addr: pack(unpack(ar.araddr) + offset), numBytes: fromInteger(valueOf(DATA_sz)/8)};

  // interfaces
  AXILiteMaster#(ADDR_sz, DATA_sz) masters[2];

  // per interface behaviour
  for (Integer i = 0; i < 2; i = i + 1) begin
    // appropriate rvbs axi interface
    let master = (i == 0) ? rvbs.axiLiteMaster0 : rvbs.axiLiteMaster1;
    // memory related state
    Bool canRsp;
    `ifdef MEM_DELAY
    // artificial delay
    Reg#(Bool) seeded <- mkReg(False);
    Reg#(Bit#(5)) delay_count <- mkReg(0);
    let delay_cmp <- mkLFSR_16;
    rule init_delay (!seeded); delay_cmp.seed('h11); seeded <= True; endrule
    let delayff <- mkFIFOF;
    canRsp = seeded;
    `else
    canRsp = True;
    `endif
    // appropriate memory port
    let p = (i == 0) ? mem.p0 : mem.p1;
    // outshim
    let outshim <- mkAXILiteShim;
    masters[i] = outshim.master;
    // forward requests to appropriate slave
    rule writeReq;
      let awflit <- master.aw.get;
      let  wflit <- master.w.get;
      if (awflit.awaddr >= fromInteger(membase) &&
          awflit.awaddr < fromInteger(membase + memsize)) begin
        p.request.put(fromAXILiteToWriteReq(awflit, wflit, -fromInteger(membase)));
        master.b.put(defaultValue);
      end else begin
        outshim.slave.aw.put(awflit);
        outshim.slave.w.put(wflit);
      end
    endrule
    rule readReq;
      let arflit <- master.ar.get;
      if (arflit.araddr >= fromInteger(membase) &&
          arflit.araddr < fromInteger(membase + memsize))
        p.request.put(fromAXILiteToReadReq(arflit, -fromInteger(membase)));
      else outshim.slave.ar.put(arflit);
    endrule
    // forward response from memory
    rule memReadRsp(canRsp);
      let rsp <- p.response.get;
    `ifndef MEM_DELAY
      master.r.put(RLiteFlit{rdata: rsp.ReadRsp, rresp: OKAY});
    endrule
    `else
      delayff.enq(tuple2(rsp, delay_cmp.value[15:11]));
      delay_cmp.next;
    endrule
    rule delayedMemReadRsp;
      match {.rsp, .d} = delayff.first;
      if (delay_count >= d) begin
        delay_count <= 0;
        delayff.deq;
        master.r.put(RLiteFlit{rdata: rsp.ReadRsp, rresp: OKAY});
      end else delay_count <= delay_count + 1;
    endrule
    `endif
    // forward response from outside master port
    (* descending_urgency = "memReadRsp, readRsp" *)
    rule readRsp(outshim.slave.r.canGet);
      let flit <- outshim.slave.r.get;
      master.r.put(flit);
    endrule
    (* descending_urgency = "writeReq, writeRsp" *)
    rule writeRsp(outshim.slave.b.canGet);
      let flit <- outshim.slave.b.get;
      master.b.put(flit);
    endrule

  end

  // probing interfaces
  method peekPC      = rvbs.peekPC;
  method peekCtrlCSR = rvbs.peekCtrlCSR;
  interface probes   = rvbs.probes;
  // riscv interfaces
  method setMSIP = rvbs.setMSIP;
  method setMTIP = rvbs.setMTIP;
  method setMEIP = rvbs.setMEIP;
  interface axiLiteMaster0 = masters[0];
  interface axiLiteMaster1 = masters[1];

endmodule

// simulation top module
////////////////////////////////////////////////////////////////////////////////
module top (Empty);
  // RESET PC
  Bit#(DATA_sz) reset_pc = 'h80000000;
  // RVBS instance
  let rvbs <- mkRVBS(reset_pc);
  let wrapped <- localMemWrapper(rvbs);
  // mem map
  let memMap <- memoryMap;
  // plug things in
  mkConnection(wrapped, memMap);
endmodule
