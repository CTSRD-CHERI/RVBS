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
import Vector :: *;
import DefaultValue :: *;

import BID :: *;
import RV_Types :: *;

module mkPMP#(Integer width, PrivLvl plvl) (PMP);

  PMP pmp;
  FIFO#(PMPRsp) rsp[width];
  for (Integer i  = 0; i < width; i = i + 1) rsp[i] <- mkBypassFIFO;
  // mapped CSRs
  pmp.cfg <- replicateM(mkPMPCfgIfcReg);
  pmp.addr <- replicateM(mkReg(defaultValue));
  // lookup method
  function Action lookup (Integer i, PMPReq req) = action
    // inner helper for zipwith
    function PMPRsp doLookup (PMPCfg cfg1, Bit#(SmallPASz) a1, Bit#(SmallPASz) a0);
      // authorisation after match
      Bool auth = (!cfg1.l && plvl == M) ? True :
        (case (req.reqType)
          READ: return cfg1.r;
          WRITE: return cfg1.w;
          IFETCH: return cfg1.x;
          default: return False;
        endcase);
      // prepare match entry
      PMPRsp matchRsp = PMPRsp {matched: True, authorized: auth, addr: req.addr};
      // matching
      PAddr mask = ((~0) << 3) << countZerosLSB(~a0); // 3 because bottom 2 bits + 1 terminating 0
      PAddr baseAddr = req.addr;
      PAddr topAddr = req.addr + zeroExtend(readBitPO(req.numBytes));
      case (cfg1.a)
        // Top Of Range mode
        TOR: return ({a0,2'b00} <= baseAddr && topAddr <= {a1,2'b00}) ? matchRsp : defaultValue;
        // Naturally Aligned Power Of Two region (4-bytes region)
        NA4: return (a0 == truncateLSB(baseAddr) && a0 == truncateLSB(topAddr)) ? matchRsp : defaultValue;
        // Naturally Aligned Power Of Two region (>= 8-bytes region)
        NAPOT: return (({a0,2'b00} & mask) == (baseAddr & mask) && ({a0,2'b00} & mask) == (topAddr & mask)) ? matchRsp : defaultValue;
        default: return defaultValue; // OFF
      endcase
    endfunction
    // return first match or default response
    function isMatch(x) = x.matched;
    function Bit#(SmallPASz) getAddr(Reg#(PMPAddr) x) = x.address;
    Vector#(16, Bit#(SmallPASz)) addrs = map(getAddr, pmp.addr);
    rsp[i].enq(fromMaybe(
      PMPRsp {matched: False, authorized: (plvl == M), addr: req.addr},
      find(isMatch, zipWith3(doLookup, concat(readVReg(pmp.cfg)), addrs, shiftInAt0(addrs,0)))
    ));
  endaction;
  // build the multiple lookup interfaces
  PMPLookup ifc[width];
  for (Integer i  = 0; i < width; i = i + 1) begin
    ifc[i].put = lookup(i);
    ifc[i].get = actionvalue rsp[i].deq(); return rsp[i].first(); endactionvalue;
  end
  pmp.lookup = ifc;
  // returning PMP interface
  return pmp;

endmodule
