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
import SpecialFIFOs :: *;
import Vector :: *;
import DefaultValue :: *;
import UniqueWrappers :: * ;

import BlueUtils :: *;
import SourceSink :: *;
import MasterSlave :: *;

import RVBS_Types :: *;

module mkPMPLookup#(Vector#(16, PMPCfg) pmpcfgs, Vector#(16, PMPAddr) pmpaddrs, PrivLvl plvl) (PMPLookup);

  let rsp <- mkBypassFIFOF;
  // lookup method
  function lookup (req);
    // authorisation after match
    ExcCode excCode = case (req.reqType)
      READ: return LoadAccessFault;
      WRITE: return StrAMOAccessFault;
      IFETCH: return InstAccessFault;
    endcase;
    // inner helper for zipwith
    //function Maybe#(Either#(ExcToken, PAddr)) doLookup (PMPCfg cfg1, Bit#(SmallPAWidth) a1, Bit#(SmallPAWidth) a0);
    function doLookup (cfg1, a1, a0);
      // privilege level and R, W, X checks
      let isExc =(!(!cfg1.l && plvl == M) &&
                  ((req.reqType == READ   && !cfg1.r) ||
                   (req.reqType == WRITE  && !cfg1.w) ||
                   (req.reqType == IFETCH && !cfg1.x)));
      // matching
      PAddr mask = ((~0) << 3) << countZerosLSB(~a0); // 3 because bottom 2 bits + 1 terminating 0
      PAddr baseAddr = req.addr;
      PAddr topAddr = req.addr + zeroExtend(readBitPO(req.numBytes));
      function checkCond(c) = c ? Valid(isExc ? Left(craftExcToken(excCode)) : Right(baseAddr)) : Invalid;
      case (cfg1.a)
        // Top Of Range mode
        TOR: return checkCond({a0,2'b00} <= baseAddr && topAddr <= {a1,2'b00});
        // Naturally Aligned Power Of Two region (4-bytes region)
        NA4: return checkCond(a0 == truncateLSB(baseAddr) && a0 == truncateLSB(topAddr));
        // Naturally Aligned Power Of Two region (>= 8-bytes region)
        NAPOT: return checkCond(({a0,2'b00} & mask) == (baseAddr & mask) && ({a0,2'b00} & mask) == (topAddr & mask));
        default: return Invalid; // OFF
      endcase
    endfunction
    // return first match or default response
    function Bit#(SmallPAWidth) getAddr(PMPAddr x) = x.address;
    Vector#(16, Bit#(SmallPAWidth)) addrs = map(getAddr, pmpaddrs);
    let pmpMatches = zipWith3(doLookup, pmpcfgs, addrs, shiftInAt0(addrs,0));
    return fromMaybe(
      (plvl != M ? Left(craftExcToken(excCode)) : Right(req.addr)),
      fromMaybe(Invalid, find(isValid, pmpMatches)) // flatten the Maybe#(Maybe#(AddrRsp))
    );
  endfunction
  // build the lookup interface
  let lookupWrapper <- mkUniqueWrapper(lookup);
  interface sink = interface Sink;
    method canPut = rsp.notFull;
    method put (e_req) = case (e_req) matches
      tagged Left .excTok: rsp.enq(Left(excTok)); // always pass down incomming exception without further side effects
      tagged Right .req: composeM(lookupWrapper.func, rsp.enq)(req); // XXX The bluespec reference guide seams to have the arguments the wrong way around for composeM
    endcase;
  endinterface;
  interface source = toSource(rsp);

endmodule
