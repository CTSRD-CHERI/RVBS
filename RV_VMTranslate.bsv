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
import SpecialFIFOs :: *;

import BID :: *;
import RV_BasicTypes :: *;
import RV_CSRTypes :: *;
import RV_PMPTypes :: *;
import RV_VMTranslateTypes :: *;

module mkVMLookup#(CSRs csrs
  , Mem#(PAddr, Bit#(n)) mem
  `ifdef PMP
  , PMPLookup pmp
  `endif
  ) (VMLookup);
  FIFO#(VMRsp) rsp <- mkBypassFIFO;
  // lookup method
  function Action lookup (VMReq req) = action
    // TODO
    `ifdef XLEN64
    if (csrs.mstatus.sxl == XL64)
    case (csrs.satp.mode)
      BARE: rsp.enq(VMRsp {addr: toPAddr(req.addr)});
      default: rsp.enq(VMRsp {addr: toPAddr(req.addr)});
    endcase
    else begin
    `endif
    case (csrs.satp.mode)
      BARE: rsp.enq(VMRsp {addr: toPAddr(req.addr)});
      default: rsp.enq(VMRsp {addr: toPAddr(req.addr)});
    endcase
    `ifdef XLEN64
     end
    `endif
  endaction;
  // build the multiple lookup interfaces
  VMLookup ifc;
  ifc.put = lookup;
  ifc.get = actionvalue rsp.deq(); return rsp.first(); endactionvalue;
  // returning interface
  return ifc;
endmodule
