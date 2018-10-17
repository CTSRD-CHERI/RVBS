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

import Vector :: *;
import ClientServer :: *;
import GetPut :: *;

import BID :: *;
import BlueUtils :: *;
import Recipe :: *;
import RVBS_Traces :: *;
import RVBS_BasicTypes :: *;
import RVBS_CSRTypes :: *;
`ifdef PMP
import RVBS_PMPTypes :: *;
`endif
`ifdef SUPERVISOR_MODE
import RVBS_VMTranslateTypes :: *;
`endif
`ifdef RVFI_DII
import RVFI_DII :: *;
import FIFO :: *;
`endif

////////////////////////////////
// RISC-V architectural state //
////////////////////////////////////////////////////////////////////////////////

// state type
typedef struct {
  Reg#(PrivLvl) currentPrivLvl;
  XLMode currentXLEN;

  ArchReg#(VAddr) pc;
  Reg#(VAddr) instByteSz;
  ArchRegFile#(32, Bit#(XLEN)) regFile;
  CSRs csrs;
  Mem#(PAddr, Bit#(IMemWidth)) imem;
  Mem#(PAddr, Bit#(DMemWidth)) dmem;
  Mem#(PAddr, Bit#(IVMMemWidth)) ivmmem;
  Mem#(PAddr, Bit#(DVMMemWidth)) dvmmem;
  `ifdef PMP
  PMPLookup ipmp;
  PMPLookup dpmp;
  `ifdef SUPERVISOR_MODE
  PMPLookup ivmpmp;
  PMPLookup dvmpmp;
  `endif
  `endif
  `ifdef SUPERVISOR_MODE
  VMLookup ivm;
  VMLookup dvm;
  `endif
  `ifdef RVFI_DII
  FIFO#(Bit#(InstWidth)) iFF;
  Reg#(Bit#(64)) count;
  Array#(Reg#(VAddr)) mem_addr;
  Array#(Reg#(Bit#(DMemWidth))) mem_wdata;
  Array#(Reg#(Bit#(TDiv#(DMemWidth, 8)))) mem_wmask;
  RVFI_DII_Bridge rvfi_dii_bridge;
  `endif
} RVState;

// State instance
instance State#(RVState);

  function lightReport = fullReport;
  function fullReport (s);
    Fmt str = $format("regfile\n");
    for (Integer i = 0; i < 6; i = i + 1) begin
      for (Integer j = 0; j < 5; j = j + 1) begin
        Bit#(5) ridx = fromInteger(i*5+j);
        str = str + $format(rName(ridx),": 0x%8x\t", s.regFile.r[ridx]);
      end
      str = str + $format("\n");
    end
    str = str + $format(rName(5'd30),": 0x%8x\t", s.regFile.r[30]);
    str = str + $format(rName(5'd31),": 0x%8x", s.regFile.r[31]);
    str = str + $format("\npc = 0x%8x", s.pc);
    str = str + $format(" - privilege mode = ", fshow(s.currentPrivLvl));
    return str;
  endfunction
  function commit (s) = action
    `ifdef RVFI_DII
    // first do the  RVFI_DII reporting
    s.iFF.deq;
    s.count <= s.count + 1;
    s.rvfi_dii_bridge.inst.response.put(RVFI_DII_Execution{
      rvfi_order: s.count,
      rvfi_trap:  ?,
      rvfi_halt:  ?,
      rvfi_intr:  ?,
      rvfi_insn:  s.iFF.first,
      rvfi_rs1_addr:  ?,
      rvfi_rs2_addr:  ?,
      rvfi_rs1_data:  ?,
      rvfi_rs2_data:  ?,
      rvfi_pc_rdata:  s.pc,
      rvfi_pc_wdata:  s.pc.late,
      rvfi_mem_wdata: s.mem_wdata[1],
      rvfi_rd_addr:   s.regFile.rd_idx,
      rvfi_rd_wdata:  s.regFile.rd_new_val,
      rvfi_mem_addr:  s.mem_addr[1],
      rvfi_mem_rmask: ?,
      rvfi_mem_wmask: s.mem_wmask[1],
      rvfi_mem_rdata: ?
    });
    // reset the cregs
    s.mem_addr[1]  <= 0;
    s.mem_wdata[1] <= 0;
    s.mem_wmask[1] <= 0;
    `endif
    // do the stateful commits
    s.pc.commit;
    s.regFile.commit;
  endaction;

endinstance
