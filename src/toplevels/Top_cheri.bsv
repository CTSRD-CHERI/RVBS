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

import           AXI :: *;
import      Routable :: *;
import          RVBS :: *;
import RVBS_Wrappers :: *;
import      CHERISOC :: *;

typedef 2 NMASTERS;
typedef 4 SID_sz;
typedef TSub#(SID_sz, TLog#(NMASTERS)) MID_sz;

typedef PAddrWidth ADDR_sz;
typedef        128 DATA_sz;
typedef          0 AWUSER_sz;
typedef          1 WUSER_sz;
typedef          0 BUSER_sz;
typedef          0 ARUSER_sz;
typedef          1 RUSER_sz;

`define AXI_MASTER_PARAMS MID_sz, ADDR_sz, DATA_sz,\
                   AWUSER_sz, WUSER_sz, BUSER_sz, ARUSER_sz, RUSER_sz
`define AXI_SLAVE_PARAMS  SID_sz, ADDR_sz, DATA_sz,\
                   AWUSER_sz, WUSER_sz, BUSER_sz, ARUSER_sz, RUSER_sz

// simulation top module
////////////////////////////////////////////////////////////////////////////////
module mkRVBS_sim (Empty);
  // RESET PC
  let reset_pc = 'h80000000;
  // RVBS instance
  let rvbs <- mkRVBS_CLINT(reset_pc);
  // cheriSOC
  let cheriSOC <- mkCHERISOC;
  // plug things in
  connectIFCs(rvbs, cheriSOC);
endmodule

module connectIFCs#(RVBS_CLINT rvbs, CHERISOC cheriSOC) (Empty);
  // interconnect
  Vector#(NMASTERS, AXI4_Master#(`AXI_MASTER_PARAMS)) ms;
  ms[0] <- fromAXI4Lite_Master(rvbs.instAXI4Lite_Master);
  ms[1] <- fromAXI4Lite_Master(rvbs.dataAXI4Lite_Master);
  Vector#(1, AXI4_Slave#(`AXI_SLAVE_PARAMS)) ss;
  ss[0] = expandAXI4_Slave_Addr(cheriSOC.slave);
  MappingTable#(1, ADDR_sz) maptab = newVector;
  maptab[0] = Range{base: 'h00000000, size: 'h100000000};
  mkAXI4Bus(routeFromMappingTable(maptab), ms, ss);
  rule connect_interrupts;
    rvbs.setMEIP(unpack(cheriSOC.peekIRQs[0]));
  endrule
endmodule

`undef AXI_MASTER_PARAMS
`undef AXI_SLAVE_PARAMS
