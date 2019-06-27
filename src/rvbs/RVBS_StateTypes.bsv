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

`ifdef RVXCHERI
`define RISCV
//XXX TODO FIXME add a typeclass method that handles getting otypeMax
`ifdef XLEN64
`define CAP128
//typedef 64 CC_ADDR;
//typedef 46 CC_BOUNDS;
//typedef  6 CC_EXP;
typedef 18 CC_OTYPE;
`else
`define CAP64
//typedef 32 CC_ADDR;
//typedef 16 CC_BOUNDS;
//typedef  6 CC_EXP;
typedef  4 CC_OTYPE;
`endif
import CHERICap :: *;
import CHERICC_Fat :: *;
//import CHERICC :: *;

import Vector :: *;
import FIFOF :: *;

import BID :: *;
import BlueUtils :: *;
import Recipe :: *;
import RVBS_BasicTypes :: *;
import RVBS_MemTypes :: *;
import RVBS_CSRTypes :: *;
import RVBS_TraceUtils :: *;
`ifdef PMP
import RVBS_PMPTypes :: *;
`endif
`ifdef SUPERVISOR_MODE
import RVBS_VMTranslateTypes :: *;
`endif
`ifdef RVFI_DII
import RVFI_DII :: *;
import ClientServer :: *;
import GetPut :: *;
import FIFO :: *;
`endif

////////////////////////////////
// RISC-V architectural state //
////////////////////////////////////////////////////////////////////////////////

`ifdef RVXCHERI
//typedef CHERICCCap#(CC_ADDR, CC_BOUNDS, CC_EXP, CC_OTYPE) CapType;
typedef CapPipe CapType;
typedef TAdd#(XLEN, XLEN) CapNoValidSz;
Bit#(CC_OTYPE) otypeMax = ~0;
// Capability handle helper types
typedef union tagged {
  Tuple2#(Bit#(5), CapType) CapAccessHandle;
  VAddr DDCAccessHandle;
  VAddr PCCAccessHandle;
} MemAccessHandle deriving (Bits);

function Tuple3#(Bit#(6), CapType, VAddr) unpackHandle(CapType ddc, CapType pcc, MemAccessHandle h);
  Bit#(6) idx = ?;
  CapType cap = ?;
  VAddr vaddr = ?;
  case (h) matches
    tagged CapAccessHandle {.h_idx, .h_cap}: begin
      idx = zeroExtend(h_idx);
      cap = h_cap;
      vaddr = getAddr(cap);
    end
    tagged DDCAccessHandle .h_addr: begin
      idx = 6'b100001; // this is DDC
      cap = ddc;
      vaddr = truncate(getBase(cap)) + h_addr;
    end
    tagged PCCAccessHandle .h_addr: begin
      idx = 6'b100000; // this is PCC
      cap = pcc;
      vaddr = truncate(getBase(cap)) + h_addr;
    end
  endcase
  return tuple3(idx, cap, vaddr);
endfunction

function Bool inCapMode (CapType cap) = getFlags(cap)[0] == 1;

`endif

// state type
typedef struct {
  Reg#(PrivLvl) currentPrivLvl;
  XLMode currentXLEN;

  ArchReg#(VAddr) pc;
  Reg#(VAddr) instByteSz;
  Array#(Reg#(Maybe#(ExcCode))) pendingIFetchException;
  Array#(Reg#(Maybe#(Tuple2#(ExcCode, Maybe#(Bit#(XLEN)))))) pendingException;
  Array#(Reg#(Maybe#(Tuple2#(ExcCode, Bit#(XLEN))))) pendingMemException;
  `ifdef RVXCHERI
  ArchRegFile#(32, CapType) regFile;
  `else
  ArchRegFile#(32, Bit#(XLEN)) regFile;
  `endif
  function Bit#(XLEN) f(Bit#(5) idx) rGPR;
  function Action f(Bit#(5) idx, Bit#(XLEN) data) wGPR;
  `ifdef RVXCHERI
  function CapType f(Bit#(5) idx) rCR;
  function Action f(Bit#(5) idx, CapType data) wCR;
  `endif
  CSRs csrs;
  `ifdef RVXCHERI
  ArchReg#(CapType) pcc;
  ArchReg#(CapType) ddc;
  ArchReg#(CapType) utcc;
  ArchReg#(CapType) uscratchc;
  ArchReg#(CapType) uepcc;
  ArchReg#(CapType) stcc;
  ArchReg#(CapType) sscratchc;
  ArchReg#(CapType) sepcc;
  ArchReg#(CapType) mtcc;
  ArchReg#(CapType) mscratchc;
  ArchReg#(CapType) mepcc;
  function Maybe#(ArchReg#(CapType)) f(Bit#(5) idx) getCSpecial;
  FIFOF#(Tuple6#(MemAccessHandle, VAddr, Bit#(5), BitPO#(4), Bool, Bool)) readMem;
  FIFOF#(Tuple5#(MemAccessHandle, VAddr, BitPO#(4), Bit#(128), Bool)) writeMem;
  `else
  FIFOF#(Tuple5#(VAddr, VAddr, Bit#(5), BitPO#(4), Bool)) readMem;
  FIFOF#(Tuple4#(VAddr, VAddr, BitPO#(4), Bit#(128))) writeMem;
  `endif
  RVMem imem;
  RVMem dmem;
  RVMem ivmmem;
  RVMem dvmmem;
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
  Array#(Reg#(Maybe#(VAddr))) exc_tgt;
  Array#(Reg#(VAddr)) mem_addr;
  Array#(Reg#(Bit#(DMemWidth))) mem_wdata;
  Array#(Reg#(Bit#(TDiv#(DMemWidth, 8)))) mem_wmask;
  RVFI_DII_Bridge#(XLEN,0) rvfi_dii_bridge;
  `endif
} RVState;

// State instance
instance State#(RVState);

  function lightReport = fullReport;
  function fullReport (s);
    Fmt str = $format("regfile\n");
    `ifdef RVXCHERI
    for (Integer i = 0; i < 32; i = i + 1)
      str = str + $format(rName(fromInteger(i)),": ", showCHERICap(s.regFile.r[i]));
    `else
    for (Integer i = 0; i < 6; i = i + 1) begin
      for (Integer j = 0; j < 5; j = j + 1) begin
        Bit#(5) ridx = fromInteger(i*5+j);
        str = str + $format(rName(ridx),": 0x%8x\t", s.regFile.r[ridx]);
      end
      str = str + $format("\n");
    end
    str = str + $format(rName(5'd30),": 0x%8x\t", s.regFile.r[30]);
    str = str + $format(rName(5'd31),": 0x%8x", s.regFile.r[31]);
    `endif
    str = str + $format("\npc = 0x%8x", s.pc);
    str = str + $format(" - privilege mode = ", fshow(s.currentPrivLvl));
    return str;
  endfunction
  function commit (s) = action
    `ifdef RVFI_DII
    // first do the  RVFI_DII reporting
    s.iFF.deq;
    s.count <= s.count + 1;
    // TODO Update to new BSV-RVFI-DII bridge and parameterize the struct on XLEN
    s.rvfi_dii_bridge.client.report.put(RVFI_DII_Execution{
      rvfi_order: s.count,
      rvfi_trap:  isValid(s.exc_tgt[1]),
      rvfi_halt:  ?,
      rvfi_intr:  ?,
      rvfi_insn:  s.iFF.first,
      rvfi_rs1_addr:  ?,
      rvfi_rs2_addr:  ?,
      rvfi_rs1_data:  ?,
      rvfi_rs2_data:  ?,
      rvfi_pc_rdata:  s.pc,
      rvfi_pc_wdata:  isValid(s.exc_tgt[1]) ? s.exc_tgt[1].Valid : s.pc.late,
      rvfi_mem_wdata: s.mem_wdata[1],
      rvfi_rd_addr:   s.regFile.rd_idx,
      `ifdef RVXCHERI
      rvfi_rd_wdata:  getAddr(s.regFile.rd_new_val),
      `else
      rvfi_rd_wdata:  s.regFile.rd_new_val,
      `endif
      rvfi_mem_addr:  s.mem_addr[1],
      rvfi_mem_rmask: ?,
      rvfi_mem_wmask: s.mem_wmask[1],
      rvfi_mem_rdata: ?
    });
    // reset the cregs
    s.exc_tgt[1]   <= Invalid;
    s.mem_addr[1]  <= 0;
    s.mem_wdata[1] <= 0;
    s.mem_wmask[1] <= 0;
    `endif
    // perform architectural stateful changes
    s.pc.commit;
    s.regFile.commit;
    `ifdef RVXCHERI
    s.pcc.commit;
    s.ddc.commit;
    s.utcc.commit;
    s.uscratchc.commit;
    s.uepcc.commit;
    s.stcc.commit;
    s.sscratchc.commit;
    s.sepcc.commit;
    s.mtcc.commit;
    s.mscratchc.commit;
    s.mepcc.commit;
    `endif
  endaction;

endinstance
