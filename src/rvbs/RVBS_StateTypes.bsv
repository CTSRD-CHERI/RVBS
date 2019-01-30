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
`ifdef RVXCHERI
import CHERICap :: *;
import CHERICC :: *;
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
`ifdef XLEN64
typedef 64 CC_ADDR;
typedef 46 CC_BOUNDS;
typedef  6 CC_EXP;
typedef  6 CC_OTYPE;
`else
typedef 32 CC_ADDR;
typedef 16 CC_BOUNDS;
typedef  6 CC_EXP;
typedef  2 CC_OTYPE;
`endif
typedef CHERICCCap#(CC_ADDR, CC_BOUNDS, CC_EXP, CC_OTYPE) RawCap;
Bit#(CC_OTYPE) otypeMax = ~0;
typedef union tagged {
  RawCap Cap;
  Bit#(TAdd#(XLEN, XLEN)) Data;
} CapType deriving (Bits);
instance FShow#(CapType);
  function fshow (ct) = case (ct) matches
    tagged Cap  .cap:  return $format("Tag: True ",  showCHERICap(cap));
    tagged Data .data: begin
      RawCap tmp = unpack(pack(data));
      return $format("Tag: False ", showCHERICap(tmp));
    end
  endcase;
endinstance
// Helper functions
function Bool isCap(CapType cap) = case (cap) matches
  tagged Cap ._: return True;
  default: return False;
endcase;
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
      vaddr = truncate(getAddr(cap.Cap));
    end
    tagged DDCAccessHandle .h_addr: begin
      idx = 6'b100001; // this is DDC
      cap = ddc;
      vaddr = truncate(getBase(cap.Cap)) + h_addr;
    end
    tagged PCCAccessHandle .h_addr: begin
      idx = 6'b100000; // this is PCC
      cap = pcc;
      vaddr = truncate(getBase(cap.Cap)) + h_addr;
    end
  endcase
  return tuple3(idx, cap, vaddr);
endfunction
`endif

// state type
typedef struct {
  Reg#(PrivLvl) currentPrivLvl;
  XLMode currentXLEN;

  ArchReg#(VAddr) pc;
  Reg#(VAddr) instByteSz;
  Array#(Reg#(Bool)) isTrap;
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
  FIFOF#(Tuple5#(MemAccessHandle, Bit#(5), BitPO#(4), Bool, Bool)) readMem;
  FIFOF#(Tuple4#(MemAccessHandle, BitPO#(4), Bit#(128), Bool)) writeMem;
  `else
  FIFOF#(Tuple4#(VAddr, Bit#(5), BitPO#(4), Bool)) readMem;
  FIFOF#(Tuple3#(VAddr, BitPO#(4), Bit#(128))) writeMem;
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
  Array#(Reg#(VAddr)) mem_addr;
  Array#(Reg#(Bit#(DMemWidth))) mem_wdata;
  Array#(Reg#(Bit#(TDiv#(DMemWidth, 8)))) mem_wmask;
  RVFI_DII_Bridge#(XLEN) rvfi_dii_bridge;
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
    // TODO Update to new BSV-RVFI-DII bridge and parameterize the struct on XLEN
    s.rvfi_dii_bridge.inst.response.put(RVFI_DII_Execution{
      rvfi_order: s.count,
      rvfi_trap:  s.isTrap[1],
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
      `ifdef RVXCHERI
      rvfi_rd_wdata:  truncate(s.regFile.rd_new_val.Data),
      `else
      rvfi_rd_wdata:  s.regFile.rd_new_val,
      `endif
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
    // reset transient state
    s.isTrap[1] <= False;
    // do the stateful commits
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
