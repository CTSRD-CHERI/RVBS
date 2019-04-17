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

import        Vector :: *;
import   Connectable :: *;

import          RVBS :: *;
import RVBS_Wrappers :: *;
import           AXI :: *;
import      Routable :: *;
import    BlueBasics :: *;
import     BlueUtils :: *;

`define MEM_AXI4_PARAMS PAddrWidth, 128, 0, 0, 0, 0, 0
`ifdef RVXCHERI
`define AXI4_PARAMS PAddrWidth, 128, 0, 1, 0, 0, 1
`else
`define AXI4_PARAMS PAddrWidth, 128, 0, 0, 0, 0, 0
`endif
`define MASTER_T AXI4Lite_Master#(`AXI4_PARAMS)
`define SLAVE_T  AXI4Lite_Slave#(`AXI4_PARAMS)

// memory subsystem
////////////////////////////////////////////////////////////////////////////////

module mkTestSOC (SOC_NO_CLINT);
  `define NMASTERS 1
  `define NSLAVES 2
  // input shim
  AXI4Lite_Shim#(`AXI4_PARAMS) shimData <- mkAXI4LiteShim;
  // memory module
  `ifdef MEM_IMG
  String memimg = `MEM_IMG;
  `else
  String memimg = "test-prog.hex";
  `endif
  AXI4Lite_Slave#(`MEM_AXI4_PARAMS) tmp[2] <- mkAXI4LiteSharedMem2('h10000, Valid(memimg));
  `SLAVE_T mem[2];
  `ifdef RVXCHERI
  mem[0] = zeroUserFields(tmp[0]);
  mem[1] = zeroUserFields(tmp[1]);
  `else
  mem = tmp;
  `endif
  // tester
  module mkAXI4LiteTester (`SLAVE_T);
    let shim <- mkAXI4LiteShim;
    rule doWrite;
      let awflit <- get(shim.master.aw);
      let wflit  <- get(shim.master.w);
      if (wflit.wstrb[0] == 1 && wflit.wdata[0] == 1) $display("TEST SUCCESS");
      else $display("TEST FAILURE");
      $finish(0);
    endrule
    rule doRead;
      let arflit <- get(shim.master.ar);
      $display("tester should not be read");
      $finish(0);
    endrule
    return shim.slave;
  endmodule
  `SLAVE_T tester <- mkAXI4LiteTester;
  // interconnect
  Vector#(`NMASTERS, `MASTER_T) ms;
  ms[0] = shimData.master;
  Vector#(`NSLAVES, `SLAVE_T) ss;
  ss[0] = offsetSlave(mem[1], 'h80000000);
  ss[1] = tester;
  MappingTable#(`NSLAVES, PAddrWidth) maptab = newVector;
  maptab[0] = Range{base: 'h80000000, size: 'h01000};
  maptab[1] = Range{base: 'h80001000, size: 'h01000};
  mkAXI4LiteBus(routeFromMappingTable(maptab), ms, ss);
  // interfaces
  interface instAXI4Lite_Slave = offsetSlave(mem[0], 'h80000000);
  interface dataAXI4Lite_Slave = shimData.slave;
  method Bool peekMEIP = False;
  `undef NMASTERS
  `undef NSLAVES
endmodule

// simulation top module
////////////////////////////////////////////////////////////////////////////////
module mkRVBS_isa_test (Empty);
  // RESET PC
  let reset_pc = 'h80000000;
  // RVBS instance
  let rvbs <- mkRVBS_CLINT(reset_pc);
  // mem map
  let memoryMap <- mkTestSOC;
  // plug things in
  mkConnection(rvbs, memoryMap);
endmodule

`undef AXI4_PARAMS
`undef MASTER_T
`undef SLAVE_T
