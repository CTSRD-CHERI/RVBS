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

import        Vector :: *;
import   Connectable :: *;

import           AXI :: *;
import      Routable :: *;
import    BlueBasics :: *;
import     BlueUtils :: *;
import          RVBS :: *;
import RVBS_Wrappers :: *;

typedef PAddrWidth ADDR_sz;
typedef        128 DATA_sz;
typedef          0 AWUSER_sz;
typedef          0 WUSER_sz;
typedef          0 BUSER_sz;
typedef          0 ARUSER_sz;
typedef          0 RUSER_sz;

`define AXI_PARAMS ADDR_sz, DATA_sz,\
                   AWUSER_sz, WUSER_sz, BUSER_sz, ARUSER_sz, RUSER_sz
`define MASTER_T   AXILiteMaster#(`AXI_PARAMS)
`define SLAVE_T    AXILiteSlave#(`AXI_PARAMS)

// memory subsystem
////////////////////////////////////////////////////////////////////////////////

module mkTestSOC (SOC_NO_CLINT);
  `define NMASTERS 1
  `define NSLAVES 2
  // input shim
  AXILiteShim#(`AXI_PARAMS) shimData <- mkAXILiteShim;
  // memory module
  `ifdef MEM_IMG
  String memimg = `MEM_IMG;
  `else
  String memimg = "test-prog.hex";
  `endif
  AXILiteSlave#(`AXI_PARAMS) mem[2] <- mkAXILiteSharedMem2('h10000, Valid(memimg));
  // tester
  module mkAXILiteTester (AXILiteSlave#(`AXI_PARAMS));
    let shim <- mkAXILiteShim;
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
  AXILiteSlave#(`AXI_PARAMS) tester <- mkAXILiteTester;
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
  interface instAXILiteSlave = offsetSlave(mem[0], 'h80000000);
  interface dataAXILiteSlave = shimData.slave;
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

`undef AXI_PARAMS
`undef MASTER_T
`undef SLAVE_T
