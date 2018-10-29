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
typedef  0 USER_sz;
`else
typedef 34 ADDR_sz;
typedef 32 DATA_sz;
typedef  0 USER_sz;
`endif

// memory subsystem
////////////////////////////////////////////////////////////////////////////////
(* always_ready, always_enabled *)
interface RVBS_Mem_Slave;
  interface AXILiteSlave#(ADDR_sz, DATA_sz, USER_sz) axiLiteSlaveInst;
  interface AXILiteSlave#(ADDR_sz, DATA_sz, USER_sz) axiLiteSlaveData;
  method Bool peekMEIP;
  method Bool peekMTIP;
  method Bool peekMSIP;
endinterface
instance Connectable#(RVBS_Ifc, RVBS_Mem_Slave);
  module mkConnection#(RVBS_Ifc c, RVBS_Mem_Slave m) (Empty);
    mkConnection(c.axiLiteMasterInst, m.axiLiteSlaveInst);
    mkConnection(c.axiLiteMasterData, m.axiLiteSlaveData);
    rule connect_interrupts;
      c.setMEIP(m.peekMEIP);
      c.setMTIP(m.peekMTIP);
      c.setMSIP(m.peekMSIP);
    endrule
  endmodule
endinstance

`define MASTER_T AXILiteMaster#(ADDR_sz, DATA_sz, USER_sz)
`define SLAVE_T AXILiteSlave#(ADDR_sz, DATA_sz, USER_sz)
// mem req helpers
function AWLiteFlit#(ADDR_sz, USER_sz) offsetAWFlit(
  AWLiteFlit#(ADDR_sz, USER_sz) f,
  Int#(ADDR_sz) o) = AWLiteFlit {
    awaddr: pack(unpack(f.awaddr) + o), awprot: f.awprot, awuser: f.awuser
  };
function ARLiteFlit#(ADDR_sz, USER_sz) offsetARFlit(
  ARLiteFlit#(ADDR_sz, USER_sz) f,
  Int#(ADDR_sz) o) = ARLiteFlit {
    araddr: pack(unpack(f.araddr) + o), arprot: f.arprot, aruser: f.aruser
  };
function `SLAVE_T offsetSlave(`SLAVE_T s, Integer offset) = interface AXILiteSlave;
  interface aw = interface Sink;
    method canPut = s.aw.canPut;
    method put(x) = s.aw.put(offsetAWFlit(x, fromInteger(-offset)));
  endinterface;
  interface w  = s.w;
  interface b  = s.b;
  interface ar = interface Sink;
    method canPut = s.ar.canPut;
    method put(x) = s.ar.put(offsetARFlit(x, fromInteger(-offset)));
  endinterface;
  interface r  = s.r;
endinterface;

module simMemoryMap (RVBS_Mem_Slave);
  `define NMASTERS 1
  `define NSLAVES 4
  // input shim
  AXILiteShim#(ADDR_sz, DATA_sz, USER_sz) shimData <- mkAXILiteShim;
  // DTB
  `ifdef DTB_IMG
  String dtbimg = `DTB_IMG;
  `else
  String dtbimg = "dtb.hex";
  `endif
  AXILiteSlave#(ADDR_sz, DATA_sz, 0) dtb <- mkAXILiteMem('h2000, Valid(dtbimg));
  // CharIO
  AXILiteSlave#(ADDR_sz, DATA_sz, 0) charIO <- mkAXILiteSocketCharIO("CHAR_IO", 6000);
  // clint
  AXILiteCLINT#(ADDR_sz, DATA_sz) clint <- mkAXILiteCLINT;
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
  AXILiteSlave#(ADDR_sz, DATA_sz, 0) mem[2] <- mkAXILiteSharedMem2(memsize, Valid(memimg));
  // interconnect
  Vector#(`NMASTERS, `MASTER_T) ms;
  ms[0] = shimData.master;
  Vector#(`NSLAVES, `SLAVE_T) ss;
  ss[0] = offsetSlave(dtb,                'h00004000);
  ss[1] = offsetSlave(charIO,             'h10000000);
  ss[2] = offsetSlave(clint.axiLiteSlave, 'h02000000);
  ss[3] = offsetSlave(mem[1],             'h80000000);
  MappingTable#(`NSLAVES, ADDR_sz) maptab = newVector;
  maptab[0] = Range{base: 'h00004000, size: 'h02000};
  maptab[1] = Range{base: 'h10000000, size: 'h01000};
  maptab[2] = Range{base: 'h02000000, size: 'h10000};
  maptab[3] = Range{base: 'h80000000, size: fromInteger(memsize)};
  mkAXILiteBus(maptab, ms, ss);
  // interfaces
  interface axiLiteSlaveInst = offsetSlave(mem[0], 'h80000000);
  interface axiLiteSlaveData = shimData.slave;
  method Bool peekMEIP = False;
  method Bool peekMTIP = clint.peekMTIP;
  method Bool peekMSIP = clint.peekMSIP;
  `undef NMASTERS
  `undef NSLAVES
endmodule

module testMemoryMap (RVBS_Mem_Slave);
  `define NMASTERS 1
  `define NSLAVES 2
  // input shim
  AXILiteShim#(ADDR_sz, DATA_sz, USER_sz) shimData <- mkAXILiteShim;
  // memory module
  `ifdef MEM_IMG
  String memimg = `MEM_IMG;
  `else
  String memimg = "test-prog.hex";
  `endif
  AXILiteSlave#(ADDR_sz, DATA_sz, 0) mem[2] <- mkAXILiteSharedMem2('h10000, Valid(memimg));
  // tester
  module mkAXILiteTester (AXILiteSlave#(ADDR_sz, DATA_sz, 0));
    let shim <- mkAXILiteShim;
    rule doWrite;
      let awflit <- shim.master.aw.get;
      let wflit  <- shim.master.w.get;
      if (wflit.wstrb[0] == 1 && wflit.wdata[0] == 1) $display("TEST SUCCESS");
      else $display("TEST FAILURE");
      $finish(0);
    endrule
    rule doRead;
      let arflit <- shim.master.ar.get;
      $display("tester should not be read");
      $finish(0);
    endrule
    return shim.slave;
  endmodule
  AXILiteSlave#(ADDR_sz, DATA_sz, 0) tester <- mkAXILiteTester;
  // interconnect
  Vector#(`NMASTERS, `MASTER_T) ms;
  ms[0] = shimData.master;
  Vector#(`NSLAVES, `SLAVE_T) ss;
  ss[0] = offsetSlave(mem[1], 'h80000000);
  ss[1] = tester;
  MappingTable#(`NSLAVES, ADDR_sz) maptab = newVector;
  maptab[0] = Range{base: 'h80000000, size: 'h01000};
  maptab[1] = Range{base: 'h80001000, size: 'h01000};
  mkAXILiteBus(maptab, ms, ss);
  // interfaces
  interface axiLiteSlaveInst = offsetSlave(mem[0], 'h80000000);
  interface axiLiteSlaveData = shimData.slave;
  method Bool peekMEIP = False;
  method Bool peekMTIP = False;
  method Bool peekMSIP = False;
  `undef NMASTERS
  `undef NSLAVES
endmodule

`undef MASRTER_T
`undef SLAVE_T

// simulation top module
////////////////////////////////////////////////////////////////////////////////
module top (Empty);
  // RESET PC
  Bit#(DATA_sz) reset_pc = 'h80000000;
  // RVBS instance
  let rvbs <- mkRVBS(reset_pc);
  // mem map
  `ifdef ISA_TEST
  let memoryMap <- testMemoryMap;
  `else
  let memoryMap <- simMemoryMap;
  `endif
  // plug things in
  mkConnection(rvbs, memoryMap);
endmodule
