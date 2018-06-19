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
import List :: *;

import BID :: *;
import RV_State :: *;
import RV_Common :: *;
import RV_I :: *;
`ifdef RVM
import RV_M :: *;
`endif
`ifdef RVC
import RV_C :: *;
`endif

(* always_ready *)
interface RVBSProbes;
  method Bit#(XLEN) peekPC();
  method Bit#(XLEN) peekCtrlCSR();
endinterface

module rvbs (RVBSProbes);

  `ifdef MEM_IMG
  String memimg = `MEM_IMG;
  `else
  String memimg = "test-prog.hex";
  `endif
  `ifdef MEM_SIZE
  Integer memsize = `MEM_SIZE;
  `else
  Integer memsize = 16384;
  `endif
  Mem2#(PAddr, Bit#(IMemWidth), Bit#(DMemWidth)) mem <- mkSharedMem2(memsize, memimg);
  `ifdef SUPERVISOR_MODE
  RVState s <- mkState(mem.p0, mem.p1, mem.p0, mem.p1);
  `else
  RVState s <- mkState(mem.p0, mem.p1);
  `endif

  // instanciating simulator
  let modList = list(mkRVTrap, mkRV32I);
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
  mkISASim(s, modList);

  method Bit#(XLEN) peekPC() = s.pc;
  method Bit#(XLEN) peekCtrlCSR() = s.csrs.ctrl;

endmodule
