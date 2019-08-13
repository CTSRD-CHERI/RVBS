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

import ConfigReg :: *;
import Vector :: *;
import SpecialFIFOs :: *;

import BlueUtils :: *;
import SourceSink :: *;
import MasterSlave :: *;
import Recipe :: *;
import RVBS_TraceUtils :: *;
import RVBS_Types :: *;
import RVBS_CSRs :: *;
import RVBS_Trap :: *;
`ifdef PMP
import RVBS_PMP :: *;
`endif
`ifdef SUPERVISOR_MODE
import RVBS_VMTranslate :: *;
`endif
`ifdef RVXCHERI
import CHERICap :: *;
`endif

`ifdef RVFI_DII
import RVFI_DII_Bridge :: *;
import FIFO :: *;
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
  , RVFI_DII_Bridge#(XLEN, XLEN, 0) rvfi_dii_bridge
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
  s.pendingIFetchException <- mkConfigCReg(3, Invalid);
  s.pendingException <- mkCReg(2, Invalid);
  s.pendingMemException <- mkCReg(2, Invalid);
  `ifdef RVXCHERI
  s.pendingIFetchCapException <- mkConfigCReg(3, Invalid);
  s.pendingCapException <- mkCReg(2, Invalid);
  s.pendingMemCapException <- mkCReg(2, Invalid);
  `endif
  `ifdef RVXCHERI
  CapType nCap = nullCap;
  CapType yCap = almightyCap;
  s.regFile <- mkRegFileInitZ(nCap, nCap);
  function readGPR(i); return getAddr(s.regFile.r[i]); endfunction
  s.rGPR = readGPR;
  function writeGPR(i, x) = action
    s.regFile.r[i] <= nullWithAddr(x);
    printTLogPlusArgs("itrace", $format(rName(i)," <= 0x%0x", x));
  endaction;
  s.wGPR = writeGPR;
  function readCR(i); return s.regFile.r[i]; endfunction
  s.rCR = readCR;
  function writeCR(i, x) = action
    s.regFile.r[i] <= x;
    printTLogPlusArgs("itrace", $format("c%0d <= ", i, showCHERICap(x)));
  endaction;
  s.wCR = writeCR;
  `else
  s.regFile <- mkRegFileZ;
  function readGPR(i); return s.regFile.r[i]; endfunction
  s.rGPR = readGPR;
  function writeGPR(i, x) = action
    s.regFile.r[i] <= x;
    printTLogPlusArgs("itrace", $format(rName(i), " <= 0x%0x", x));
  endaction;
  s.wGPR = writeGPR;
  `endif
  s.csrs <- mkCSRs();
  `ifdef RVXCHERI
  // CHERI specific state
  s.pcc       <- mkArchReg(setAddr(yCap, reset_pc).value);
  s.ddc       <- mkArchReg(yCap);
  `ifdef RVN
  s.utcc      <- mkArchReg(yCap);
  s.utdc      <- mkArchReg(nCap);
  s.uscratchc <- mkArchReg(nCap);
  s.uepcc     <- mkArchReg(yCap);
  `endif
  `ifdef SUPERVISOR_MODE
  s.stcc      <- mkArchReg(yCap);
  s.stdc      <- mkArchReg(nCap);
  s.sscratchc <- mkArchReg(nCap);
  s.sepcc     <- mkArchReg(yCap);
  `endif
  s.mtcc      <- mkArchReg(yCap);
  s.mtdc      <- mkArchReg(nCap);
  s.mscratchc <- mkArchReg(nCap);
  s.mepcc     <- mkArchReg(yCap);
  function getCapSpecial(idx) = case (idx)
    0:  Valid(tuple4(U, False,  True, asIfc(s.pcc)));
    1:  Valid(tuple4(U, False, False, asIfc(s.ddc)));
    `ifdef RVN
    4:  Valid(tuple4(U,  True, False, asIfc(s.utcc)));
    5:  Valid(tuple4(U,  True, False, asIfc(s.utdc)));
    6:  Valid(tuple4(U,  True, False, asIfc(s.uscratchc)));
    7:  Valid(tuple4(U,  True, False, asIfc(s.uepcc)));
    `endif
  `ifdef SUPERVISOR_MODE
    12: Valid(tuple4(S,  True, False, asIfc(s.stcc)));
    13: Valid(tuple4(S,  True, False, asIfc(s.stdc)));
    14: Valid(tuple4(S,  True, False, asIfc(s.sscratchc)));
    15: Valid(tuple4(S,  True, False, asIfc(s.sepcc)));
    `endif
    28: Valid(tuple4(M,  True, False, asIfc(s.mtcc)));
    29: Valid(tuple4(M,  True, False, asIfc(s.mtdc)));
    30: Valid(tuple4(M,  True, False, asIfc(s.mscratchc)));
    31: Valid(tuple4(M,  True, False, asIfc(s.mepcc)));
    default: Invalid;
  endcase;
  s.getCSpecial = getCapSpecial;
  `endif
  function Action upPC(VAddr newpc) = action
    s.pc <= newpc;
    `ifdef RVXCHERI
    Exact#(CapType) tmp = setOffset(s.pcc, newpc);
    //XXX TODO check rep somehow?
    s.pcc <= tmp.value;
    `endif
  endaction;
  s.updatePC = upPC;
  // Memory interfaces
  s.readMem  <- mkBypassFIFOF;
  s.writeMem <- mkBypassFIFOF;
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
  s.exc_tgt   <- mkCReg(2, Invalid);
  s.mem_addr  <- mkCReg(2, 0);
  s.mem_wdata <- mkCReg(2, 0);
  s.mem_wmask <- mkCReg(2, 0);
  s.rvfi_dii_bridge = rvfi_dii_bridge;
  `endif

  return s;
endmodule
