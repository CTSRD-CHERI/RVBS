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
`ifdef SUPERVISOR_MODE
import RV_VMTranslate :: *;
`endif

////////////////////////////////
// RISC-V architectural state //
////////////////////////////////////////////////////////////////////////////////

module [Module] mkState#(
  Mem#(PAddr, Bit#(IMemWidth)) imem
  , Mem#(PAddr, Bit#(DMemWidth)) dmem
  `ifdef SUPERVISOR_MODE
  , Mem#(PAddr, Bit#(IVMMemWidth)) ivmmem
  , Mem#(PAddr, Bit#(DVMMemWidth)) dvmmem
  `endif
  ) (RVState);
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
  // basic state
  s.pc <- mkPC(0);
  s.instByteSz <- mkBypassRegU;
  s.regFile <- mkRegFileZ;
  s.csrs <- mkCSRs();
  // Memory interfaces
  s.imem = imem;
  s.dmem = dmem;
  `ifdef SUPERVISOR_MODE
  s.ivmmem = ivmmem;
  s.dvmmem = dvmmem;
  `endif
  // PMP lookup interfaces
  `ifdef PMP
  PMPLookup pmp0 <- mkPMPLookup(s.csrs, s.currentPrivLvl);
  PMPLookup pmp1 <- mkPMPLookup(s.csrs, s.currentPrivLvl);
  `ifdef SUPERVISOR_MODE
  //PMPLookup ipmp[2] <- virtualize(pmp0, 2);
  //PMPLookup dpmp[2] <- virtualize(pmp1, 2);
  s.ipmp <- mkPMPLookup(s.csrs, s.currentPrivLvl);
  s.dpmp <- mkPMPLookup(s.csrs, s.currentPrivLvl);
  s.ivmpmp = pmp0;
  s.dvmpmp = pmp1;
  `else
  s.ipmp = pmp0;
  s.dpmp = pmp1;
  `endif
  `endif
  // Virtual Memory lookup interfaces
  `ifdef SUPERVISOR_MODE
  `ifdef PMP
  s.ivm <- mkVMLookup(s.csrs, s.ivmmem, s.ivmpmp);
  s.dvm <- mkVMLookup(s.csrs, s.dvmmem, s.dvmpmp);
  `else
  s.ivm <- mkVMLookup(s.csrs, s.ivmmem);
  s.dvm <- mkVMLookup(s.csrs, s.dvmmem);
  `endif
  `endif
  // Instruction fetch machine
  s.fetchInst <- compile(rPar(rBlock(
    action
      VAddr vaddr = s.pc.next;
    `ifdef SUPERVISOR_MODE
      VMReq req = aReqIFetch(vaddr, 4, Invalid);
      s.ivm.put(req);
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
    endaction, action
      VMRsp rsp <- s.ivm.get();
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(rsp)));
      PAddr paddr = rsp.addr;
    `else
      PAddr paddr = toPAddr(vaddr);
    `endif
    `ifdef PMP
    `ifdef SUPERVISOR_MODE
      PMPReq req = aReqIFetch(paddr, 4, rsp.mExc);
    `else
      PMPReq req = aReqIFetch(paddr, 4, Invalid);
    `endif
      s.ipmp.put(req);
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
    endaction, action
      PMPRsp rsp <- s.ipmp.get();
      MemReq#(PAddr, Bit#(IMemWidth)) req = tagged ReadReq {addr: rsp.addr, numBytes: 4};
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(rsp)));
    `else
      MemReq#(PAddr, Bit#(IMemWidth)) req = tagged ReadReq {addr: paddr, numBytes: 4};
    `endif
      s.imem.sendReq(req);
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
    endaction)));
    // TODO deal with exceptions

  return s;
endmodule
