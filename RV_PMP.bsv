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
import UniqueWrappers :: * ;
import ClientServer :: * ;
import GetPut :: * ;

import BID :: *;
import RV_Types :: *;

module mkPMPLookup#(CSRs csrs, PrivLvl plvl) (PMPLookup);

  FIFO#(AddrRsp#(PAddr)) rsp <- mkBypassFIFO;
  // lookup method
  function lookup (req);
    // authorisation after match
    Maybe#(ExcCode) excCode = case (req.reqType)
      READ: return Valid(LoadAccessFault);
      WRITE: return Valid(StrAMOAccessFault);
      IFETCH: return Valid(InstAccessFault);
      default: return Invalid; // XXX TODO should never happen, put an assertion
    endcase;
    // inner helper for zipwith
    function Maybe#(AddrRsp#(PAddr)) doLookup (PMPCfg cfg1, Bit#(SmallPAWidth) a1, Bit#(SmallPAWidth) a0);
      Maybe#(ExcCode) mExc = (!cfg1.l && plvl == M) ? Invalid :
        (case (req.reqType) matches
          READ &&& cfg1.r: return Invalid;
          WRITE &&& cfg1.w: return Invalid;
          IFETCH &&& cfg1.x: return Invalid;
          default: excCode;
        endcase);
      // prepare match entry
      let matchRsp = AddrRsp {addr: req.addr, mExc: mExc};
      // matching
      PAddr mask = ((~0) << 3) << countZerosLSB(~a0); // 3 because bottom 2 bits + 1 terminating 0
      PAddr baseAddr = req.addr;
      PAddr topAddr = req.addr + zeroExtend(readBitPO(req.numBytes));
      case (cfg1.a)
        // Top Of Range mode
        TOR: return ({a0,2'b00} <= baseAddr && topAddr <= {a1,2'b00}) ? Valid(matchRsp) : Invalid;
        // Naturally Aligned Power Of Two region (4-bytes region)
        NA4: return (a0 == truncateLSB(baseAddr) && a0 == truncateLSB(topAddr)) ? Valid(matchRsp) : Invalid;
        // Naturally Aligned Power Of Two region (>= 8-bytes region)
        NAPOT: return (({a0,2'b00} & mask) == (baseAddr & mask) && ({a0,2'b00} & mask) == (topAddr & mask)) ? Valid(matchRsp) : Invalid;
        default: return Invalid; // OFF
      endcase
    endfunction
    // return first match or default response
    function Bit#(SmallPAWidth) getAddr(Reg#(PMPAddr) x) = x.address;
    Vector#(16, Bit#(SmallPAWidth)) addrs = map(getAddr, csrs.pmpaddr);
    let pmpMatches = zipWith3(doLookup, concat(readVReg(csrs.pmpcfg)), addrs, shiftInAt0(addrs,0));
    return fromMaybe(
      AddrRsp {addr: req.addr, mExc: plvl == M ? Invalid : excCode},
      fromMaybe(Invalid, find(isValid, pmpMatches)) // flatten the Maybe#(Maybe#(AddrRsp))
    );
  endfunction
  // build the lookup interface
  let lookupWrapper <- mkUniqueWrapper(lookup);
  interface request = interface Put; method put (req) = action
    if (isValid(req.mExc)) rsp.enq(AddrRsp {addr: ?, mExc: req.mExc}); // always pass down incomming exception without further side effects
    else composeM(lookupWrapper.func, rsp.enq)(req); // XXX The bluespec reference guide seams to have the arguments the wrong way around for composeM
    endaction; endinterface;
  interface response = interface Get; method get = actionvalue
    rsp.deq();
    return rsp.first();
  endactionvalue; endinterface;

endmodule
