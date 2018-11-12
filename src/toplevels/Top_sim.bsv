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
import     BlueUtils :: *;
import        CharIO :: *;
import         CLINT :: *;
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

module mkSimSOC (SOC);
  `define NMASTERS 1
  `define NSLAVES 4
  // input shim
  AXILiteShim#(`AXI_PARAMS) shimData <- mkAXILiteShim;
  // DTB
  `ifdef DTB_IMG
  String dtbimg = `DTB_IMG;
  `else
  String dtbimg = "dtb.hex";
  `endif
  AXILiteSlave#(`AXI_PARAMS) dtb <- mkAXILiteMem('h2000, Valid(dtbimg));
  // CharIO
  AXILiteSlave#(`AXI_PARAMS) charIO <- mkAXILiteSocketCharIO("CHAR_IO", 6000);
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
  AXILiteSlave#(`AXI_PARAMS) mem[2] <- mkAXILiteSharedMem2(memsize, Valid(memimg));
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
  interface instAXILiteSlave = offsetSlave(mem[0], 'h80000000);
  interface dataAXILiteSlave = shimData.slave;
  method Bool peekMEIP = False;
  method Bool peekMTIP = clint.peekMTIP;
  method Bool peekMSIP = clint.peekMSIP;
  `undef NMASTERS
  `undef NSLAVES
endmodule

// simulation top module
////////////////////////////////////////////////////////////////////////////////
module mkRVBS_sim (Empty);
  // RESET PC
  let reset_pc = 'h80000000;
  // RVBS instance
  let rvbs <- mkRVBS(reset_pc);
  // mem map
  let memoryMap <- mkSimSOC;
  // plug things in
  mkConnection(rvbs, memoryMap);
endmodule

`undef AXI_PARAMS
`undef MASTER_T
`undef SLAVE_T
