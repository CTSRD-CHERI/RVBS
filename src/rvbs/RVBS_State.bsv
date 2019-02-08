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
import CHERICC :: *;
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
  , RVFI_DII_Bridge#(XLEN,0) rvfi_dii_bridge
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
  s.pendingIFetchException <- mkCReg(3, Invalid);
  s.pendingException <- mkCReg(2, Invalid);
  s.pendingMemException <- mkCReg(2, Invalid);
  `ifdef RVXCHERI
  RawCap nCap = nullCap;
  RawCap yCap = almightyCap;
  s.regFile <- mkRegFileInitZ(Data(pack(nCap)), Data(pack(nCap)));
  function readGPR(i); return truncate(s.regFile.r[i].Data); endfunction
  s.rGPR = readGPR;
  function writeGPR(i, x) = action
    s.regFile.r[i] <= Data(zeroExtend(x));
    printTLogPlusArgs("itrace", $format(rName(i)," <= 0x%0x", x));
  endaction;
  s.wGPR = writeGPR;
  function readCR(i); return s.regFile.r[i]; endfunction
  s.rCR = readCR;
  function writeCR(i, x) = action
    s.regFile.r[i] <= x;
    printTLogPlusArgs("itrace", $format("c%0d <= ", i, fshow(x)));
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
  s.pcc  <- mkArchReg(Cap(yCap));
  s.ddc  <- mkArchReg(Cap(yCap));
  s.utcc      <- mkArchReg(Data(pack(nCap)));
  s.uscratchc <- mkArchReg(Data(pack(nCap)));
  s.uepcc     <- mkArchReg(Data(pack(nCap)));
  s.stcc      <- mkArchReg(Data(pack(nCap)));
  s.sscratchc <- mkArchReg(Data(pack(nCap)));
  s.sepcc     <- mkArchReg(Data(pack(nCap)));
  s.mtcc      <- mkArchReg(Data(pack(nCap)));
  s.mscratchc <- mkArchReg(Data(pack(nCap)));
  s.mepcc     <- mkArchReg(Data(pack(nCap)));
  function getCapSpecial(idx) = case (idx)
    0: Valid(asIfc(s.pcc));
    1: Valid(asIfc(s.ddc));
    4: Valid(asIfc(s.utcc));
    6: Valid(asIfc(s.uscratchc));
    7: Valid(asIfc(s.uepcc));
    12: Valid(asIfc(s.stcc));
    14: Valid(asIfc(s.sscratchc));
    15: Valid(asIfc(s.sepcc));
    28: Valid(asIfc(s.mtcc));
    30: Valid(asIfc(s.mscratchc));
    31: Valid(asIfc(s.mepcc));
    default: Invalid;
  endcase;
  s.getCSpecial = getCapSpecial;
  `endif
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
  s.mem_addr  <- mkCReg(2, 0);
  s.mem_wdata <- mkCReg(2, 0);
  s.mem_wmask <- mkCReg(2, 0);
  s.rvfi_dii_bridge = rvfi_dii_bridge;
  `endif

  return s;
endmodule
