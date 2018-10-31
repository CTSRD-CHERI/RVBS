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
import Vector :: *;

import BID :: *;
import BlueUtils :: *;
import SourceSink :: *;
import MasterSlave :: *;
import Recipe :: *;
import RVBS_Traces :: *;
import RVBS_Types :: *;
import RVBS_CSRs :: *;
import RVBS_Trap :: *;
`ifdef PMP
import RVBS_PMP :: *;
`endif
`ifdef SUPERVISOR_MODE
import RVBS_VMTranslate :: *;
`endif
`ifdef RVFI_DII
import RVFI_DII_Bridge :: *;
import FIFO :: *;
import ClientServer :: *;
import GetPut :: *;
`endif

////////////////////////////////
// RISC-V architectural state //
////////////////////////////////////////////////////////////////////////////////

module [Module] mkState#(
  VAddr reset_pc,
  RVMem imem
  , RVMem dmem
  `ifdef SUPERVISOR_MODE
  , RVMem ivmmem
  , RVMem dvmmem
  `endif
  `ifdef RVFI_DII
  , RVFI_DII_Bridge rvfi_dii_bridge
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
  s.pc <- mkArchReg(reset_pc);
  s.instByteSz <- mkBypassRegU;
  s.isTrap <- mkCReg(2, False);
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
  function preInstView(x) = x.preInstView[0];
  let pmpcfgs  = concat(map(preInstView, s.csrs.pmpcfg));
  let pmpaddrs = map(preInstView, s.csrs.pmpaddr);
  PMPLookup pmp0 <- mkPMPLookup(pmpcfgs, pmpaddrs, s.currentPrivLvl);
  PMPLookup pmp1 <- mkPMPLookup(pmpcfgs, pmpaddrs, s.currentPrivLvl);
  `ifdef SUPERVISOR_MODE
  /*
  PMPLookup ipmp[2] <- virtualize(pmp0, 2);
  PMPLookup dpmp[2] <- virtualize(pmp1, 2);
  s.ipmp = ipmp[1];
  s.dpmp = dpmp[1];
  s.ivmpmp = ipmp[0];
  s.dvmpmp = dpmp[0];
  */
  s.ipmp = pmp0;
  s.dpmp = pmp1;
  s.ivmpmp <- mkPMPLookup(pmpcfgs, pmpaddrs, s.currentPrivLvl);
  s.dvmpmp <- mkPMPLookup(pmpcfgs, pmpaddrs, s.currentPrivLvl);
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
  `ifdef RVFI_DII
  s.iFF   <- mkFIFO;
  s.count <- mkReg(0);
  s.mem_addr  <- mkCReg(2, 0);
  s.mem_wdata <- mkCReg(2, 0);
  s.mem_wmask <- mkCReg(2, 0);
  s.rvfi_dii_bridge = rvfi_dii_bridge;
  `endif

  return s;
endmodule

// Instruction fetch
module [ISADefModule] mkRVIFetch#(RVState s) ();
  function Recipe instFetch(RVState s, Sink#(Bit#(InstWidth)) snk) =
  rPipe(rBlock(
      rFastSeq(rBlock(
      action
        VAddr vaddr = s.pc.late;
      `ifdef SUPERVISOR_MODE
        let req = aReqIFetch(vaddr, 4, Invalid);
        s.ivm.sink.put(req);
        printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
      endaction, action
        let rsp <- s.ivm.source.get;
        printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(rsp)));
        PAddr paddr = rsp.addr;
      `else
        PAddr paddr = toPAddr(vaddr);
      `endif
      `ifdef PMP
      `ifdef SUPERVISOR_MODE
        let req = aReqIFetch(paddr, 4, rsp.mExc);
      `else
        let req = aReqIFetch(paddr, 4, Invalid);
      `endif
        s.ipmp.sink.put(req);
        printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
      endaction, action
        let rsp <- s.ipmp.source.get;
        RVMemReq req = RVReadReq {addr: rsp.addr, numBytes: 4};
        printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(rsp)));
      `else
        RVMemReq req = RVReadReq {addr: paddr, numBytes: 4};
      `endif
        s.imem.sink.put(req);
        printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
      endaction)),
      action
        let rsp <- s.imem.source.get;
        case (rsp) matches
          tagged RVReadRsp .val: begin
            let newInstSz = (val[1:0] == 2'b11) ? 4 : 2;
            asIfc(s.pc.early) <= s.pc + newInstSz;
            s.instByteSz <= newInstSz;
            snk.put(truncate(val));
          end
          default: snk.put(?);
        endcase
      endaction
  ));
  // instruction fetching definition
  defineFetchInstEntry(instFetch(s));
endmodule

`ifdef RVFI_DII
// RVFI-DII Instruction fetch
module [ISADefModule] mkRVIFetch_RVFI_DII#(RVState s) ();
  function Recipe instFetch(RVState s, Sink#(Bit#(InstWidth)) snk) =
  rPipe(rBlock(action
      let inst <- s.rvfi_dii_bridge.inst.request.get;
      s.iFF.enq(inst);
    endaction, action
      asIfc(s.pc.early) <= s.pc + 4;
      s.instByteSz <= 4;
      snk.put(s.iFF.first);
    endaction
  ));
  // instruction fetching definition
  defineFetchInstEntry(instFetch(s));
endmodule
`endif
