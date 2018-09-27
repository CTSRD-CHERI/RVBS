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
import CharIO :: *;

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
typedef 3 NSLAVES;
`define MASTER_T AXILiteMaster#(ADDR_sz, DATA_sz)
`define SLAVE_T AXILiteSlave#(ADDR_sz, DATA_sz)
// mem req helpers
function AWLiteFlit#(ADDR_sz) offsetAWFlit(
  AWLiteFlit#(ADDR_sz) f,
  Int#(ADDR_sz) o) = AWLiteFlit {
    awaddr: pack(unpack(f.awaddr) + o), awprot: f.awprot
  };
function ARLiteFlit#(ADDR_sz) offsetARFlit(
  ARLiteFlit#(ADDR_sz) f,
  Int#(ADDR_sz) o) = ARLiteFlit {
    araddr: pack(unpack(f.araddr) + o), arprot: f.arprot
  };
module offsetSlave#(`SLAVE_T s, Integer offset) (`SLAVE_T);
  interface aw = interface Sink;
    method canPut = s.aw.canPut;
    method put(x) = s.aw.put(offsetAWFlit(x, fromInteger(offset)));
  endinterface;
  interface w  = s.w;
  interface b  = s.b;
  interface ar = interface Sink;
    method canPut = s.ar.canPut;
    method put(x) = s.ar.put(offsetARFlit(x, fromInteger(offset)));
  endinterface;
  interface r  = s.r;
endmodule

module memoryMap (RVBS_Mem_Slave);
  // input shims
  AXILiteShim#(ADDR_sz, DATA_sz) shim0 <- mkAXILiteShim;
  AXILiteShim#(ADDR_sz, DATA_sz) shim1 <- mkAXILiteShim;
  // DTB
  `ifdef DTB_IMG
  String dtbimg = `DTB_IMG;
  `else
  String dtbimg = "dtb.hex";
  `endif
  AXILiteSlave#(ADDR_sz, DATA_sz) tmp <- mkAXILiteMem('h2000, dtbimg);
  AXILiteSlave#(ADDR_sz, DATA_sz) dtb <- offsetSlave(tmp, -'h00004000);
  // CharIO
  AXILiteSlave#(ADDR_sz, DATA_sz) charIO <- mkAXILiteCharIO;
  // clint
  AXILiteCLINT#(ADDR_sz, DATA_sz) clint <- mkAXILiteCLINT;
  // interconnect
  Vector#(NMASTERS, `MASTER_T) ms;
  ms[0] = shim0.master;
  ms[1] = shim1.master;
  Vector#(NSLAVES, `SLAVE_T) ss;
  ss[0] = dtb;
  ss[1] = charIO;
  ss[2] = clint.axiLiteSlave;
  MappingTable#(NSLAVES, ADDR_sz) maptab = newVector;
  maptab[0] = Range{base: 'h00004000, size: 'h02000};
  maptab[1] = Range{base: 'h10000000, size: 'h01000};
  maptab[2] = Range{base: 'h02000000, size: 'h10000};
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
  Integer memsize = 'h10000000;
  `endif
  Integer membase = 'h80000000;
  AXILiteSlave#(ADDR_sz, DATA_sz) mem[2] <- mkAXILiteSharedMem2(memsize, memimg);

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
    // outshim
    let outshim <- mkAXILiteShim;
    masters[i] = outshim.master;
    // forward requests to appropriate slave
    rule writeReq;
      let awflit <- master.aw.get;
      let  wflit <- master.w.get;
      if (awflit.awaddr >= fromInteger(membase) &&
          awflit.awaddr < fromInteger(membase + memsize)) begin
        mem[i].aw.put(offsetAWFlit(awflit, -fromInteger(membase)));
        mem[i].w.put(wflit);
      end else begin
        outshim.slave.aw.put(awflit);
        outshim.slave.w.put(wflit);
      end
    endrule
    rule readReq;
      let arflit <- master.ar.get;
      if (arflit.araddr >= fromInteger(membase) &&
          arflit.araddr < fromInteger(membase + memsize))
        mem[i].ar.put(offsetARFlit(arflit, -fromInteger(membase)));
      else outshim.slave.ar.put(arflit);
    endrule
    // forward response from memory
    rule memWriteRsp;
      let bflit <- mem[i].b.get;
      master.b.put(bflit);
    endrule
    // forward response from memory
    rule memReadRsp(canRsp);
      let rsp <- mem[i].r.get;
    `ifndef MEM_DELAY
      master.r.put(rsp);
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
        master.r.put(rsp);
      end else delay_count <= delay_count + 1;
    endrule
    `endif
    // forward response from outside master port
    (* descending_urgency = "memReadRsp, readRsp" *)
    rule readRsp(outshim.slave.r.canGet);
      let flit <- outshim.slave.r.get;
      master.r.put(flit);
    endrule
    (* descending_urgency = "writeReq, memWriteRsp, writeRsp" *)
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
