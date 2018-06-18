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

import ConfigReg :: *;

import BID :: *;
import Recipe :: *;
import RV_Traces :: *;
import RV_Types :: *;
import RV_CSRs :: *;
`ifdef PMP
import RV_PMP :: *;
`endif

////////////////////////////////
// RISC-V architectural state //
////////////////////////////////////////////////////////////////////////////////

module [Module] mkState#(Mem2#(PAddr, Bit#(InstSz), Bit#(XLEN)) mem) (RVState);
  RVState s;

  //s.currentPrivLvl <- mkReg(M);
  s.currentPrivLvl <- mkConfigReg(M);
  s.currentXLEN = case (s.currentPrivLvl)
    M: s.csrs.misa.mxl;
    `ifdef XLEN64 // MAX_XLEN > 32
    S: s.csrs.mstatus.sxl;
    U: s.csrs.mstatus.uxl;
    `else
    S, U: XL32;
    `endif
    default: XLUNK;
  endcase;

  s.pc <- mkPC(0);
  s.instByteSz <- mkBypassRegU;
  s.regFile <- mkRegFileZ;
  s.csrs <- mkCSRs();
  `ifdef PMP
  s.pmp <- mkPMP(2, s.csrs, s.currentPrivLvl); // PMP with two lookup interfaces
  `endif
  s.imem = mem.p0;
  s.dmem = mem.p1;
  s.fetchInst <- compile(rPar(rBlock(
    action
    `ifdef PMP
      PMPReq req = PMPReq{addr: toPAddr(s.pc.next), numBytes: 4, reqType: READ};
      s.pmp[0].put(req);
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
    endaction, action
      PMPRsp rsp <- s.pmp[0].get();
      MemReq#(PAddr, Bit#(InstSz)) req = tagged ReadReq {addr: rsp.addr, numBytes: 4};
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(rsp)));
    `else
      MemReq#(PAddr, Bit#(InstSz)) req = tagged ReadReq {addr: toPAddr(s.pc.next), numBytes: 4};
    `endif
      s.imem.sendReq(req);
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
    endaction)));

  return s;
endmodule
