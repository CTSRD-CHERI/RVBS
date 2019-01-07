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

import Recipe :: *;
import BlueUtils :: *;
import BlueBasics :: *;

import CHERICap :: *;

import RVBS_Types :: *;
import RVBS_Trap :: *;
import RVBS_TraceInsts :: *;

// Read access
////////////////////////////////////////////////////////////////////////////////

function Recipe doReadMem(
  RVState s,
  `ifndef RVXCHERI
  VAddr vaddr,
  `else
  MemAccessHandle handle,
  `endif
  Bit#(5) dest,
  BitPO#(4) numBytes,
  Bool sgnExt
  `ifdef RVXCHERI
  , Bool capRead
  `endif
);
  `ifdef RVXCHERI
  match {.capIdx, .cap, .vaddr} = unpackHandle(s.ddc, handle);
  return rFastSeq(rBlock(
  rIfElse (!isCap(cap), capTrap(s, CapExcTag, capIdx),
  rIfElse (getSealed(cap.Cap), capTrap(s, CapExcSeal, capIdx),
  rIfElse (!getPerms(cap.Cap).permitLoad, capTrap(s, CapExcPermLoad, capIdx),
  rIfElse (capRead && !getPerms(cap.Cap).permitLoadCap, capTrap(s, CapExcPermLoadCap, capIdx),
  rIfElse (zeroExtend(vaddr) < getBase(cap.Cap), capTrap(s, CapExcLength, capIdx),
  rIfElse (zeroExtend(vaddr) + zeroExtend(readBitPO(numBytes)) > getTop(cap.Cap), capTrap(s, CapExcLength, capIdx),
    rFastSeq(rBlock(
  `else
  return rFastSeq(rBlock(
  `endif
    action
    `ifdef RVFI_DII
      s.mem_addr[0] <= vaddr;
    `endif
    `ifdef SUPERVISOR_MODE
      let req = aReqRead(vaddr, numBytes, Invalid);
      s.dvm.sink.put(req);
      itrace(s, fshow(req));
    endaction, action
      let rsp <- s.dvm.source.get();
      itrace(s, fshow(rsp));
      PAddr paddr = rsp.addr;
    `else
      PAddr paddr = toPAddr(vaddr);
    `endif
    `ifdef PMP
    `ifdef SUPERVISOR_MODE
      let req = aReqRead(paddr, numBytes, rsp.mExc);
    `else
      let req = aReqRead(paddr, numBytes, Invalid);
    `endif
      s.dpmp.sink.put(req);
      itrace(s, fshow(req));
    endaction, action
      let rsp <- s.dpmp.source.get();
      itrace(s, fshow(rsp));
      RVMemReq req = RVReadReq {addr: rsp.addr, numBytes: numBytes};
    `else
      RVMemReq req = RVReadReq {addr: paddr, numBytes: numBytes};
    `endif
      s.dmem.sink.put(req);
      itrace(s, fshow(req));
    endaction, action
      let rsp <- s.dmem.source.get();
      case (rsp) matches
        tagged RVReadRsp .r: begin
          `ifdef RVXCHERI
          match {.captag, .data} = r;
          RawCap newCap = unpack(truncate(data));
          `else
          let data = r;
          `endif
          let topIdx = {readBitPO(numBytes), 3'b000};
          Bool isNeg = unpack(data[topIdx-1]);
          Bit#(XLEN) mask = (~0) << topIdx;
          `ifdef RVXCHERI
          let newData = Data(pack(newCap));
          if (captag == 1) newData = Cap(newCap);
          if (capRead) s.wCR(dest, newData);
          else
          `endif
          s.wGPR(dest, (sgnExt && isNeg) ? truncate(data) | mask : truncate(data) & ~mask);
        end
        tagged RVBusError: action trap(s, LoadAccessFault); endaction
      endcase
      itrace(s, fshow(rsp));
    endaction
  `ifdef RVXCHERI
  ))))))))));
  `else
  ));
  `endif
endfunction
// TODO deal with exceptions

function Recipe readData(
  RVState s,
  LoadArgs args,
  VAddr vaddr,
  Bit#(5) dest
) =
  `ifndef RVXCHERI
  rAct(s.readMem.enq(tuple4(vaddr, dest, fromInteger(args.numBytes), args.sgnExt)));
  `else
  rAct(s.readMem.enq(tuple5(DDCAccessHandle(vaddr), dest, fromInteger(args.numBytes), args.sgnExt, False)));
  `endif

`ifdef RVXCHERI
function Recipe readCap(
  RVState s,
  LoadArgs args,
  VAddr vaddr,
  Bit#(5) dest
) = rAct(s.readMem.enq(tuple5(DDCAccessHandle(vaddr), dest, fromInteger(args.numBytes), args.sgnExt, True)));

function Recipe capReadData(
  RVState s,
  LoadArgs args,
  Bit#(5) capIdx,
  Bit#(5) dest
) = rAct(s.readMem.enq(tuple5(CapAccessHandle(tuple2(capIdx, s.rCR(capIdx))), dest, fromInteger(args.numBytes), args.sgnExt, False)));

function Recipe capReadCap(
  RVState s,
  LoadArgs args,
  Bit#(5) capIdx,
  Bit#(5) dest
) = rAct(s.readMem.enq(tuple5(CapAccessHandle(tuple2(capIdx, s.rCR(capIdx))), dest, fromInteger(args.numBytes), args.sgnExt, True)));
`endif

// Write access
////////////////////////////////////////////////////////////////////////////////

function Recipe doWriteMem(
  RVState s,
  `ifndef RVXCHERI
  VAddr vaddr,
  `else
  MemAccessHandle handle,
  `endif
  BitPO#(4) numBytes,
  Bit#(128) wdata
  `ifdef RVXCHERI
  , Bool capWrite
  `endif
);
  `ifdef RVXCHERI
  match {.capIdx, .cap, .vaddr} = unpackHandle(s.ddc, handle);
  return rFastSeq(rBlock(
  rIfElse (!isCap(cap), capTrap(s, CapExcTag, capIdx),
  rIfElse (getSealed(cap.Cap), capTrap(s, CapExcSeal, capIdx),
  rIfElse (!getPerms(cap.Cap).permitStore, capTrap(s, CapExcPermStore, capIdx),
  rIfElse (capWrite && !getPerms(cap.Cap).permitStoreCap, capTrap(s, CapExcPermStoreCap, capIdx),
  rIfElse (zeroExtend(vaddr) < getBase(cap.Cap), capTrap(s, CapExcLength, capIdx),
  rIfElse (zeroExtend(vaddr) + zeroExtend(readBitPO(numBytes)) > getTop(cap.Cap), capTrap(s, CapExcLength, capIdx),
    rFastSeq(rBlock(
  `else
  return rFastSeq(rBlock(
  `endif
    action
    `ifdef RVFI_DII
      s.mem_addr[0] <= vaddr;
    `endif
    `ifdef SUPERVISOR_MODE
      let req = aReqWrite(vaddr, readBitPO(numBytes), Invalid);
      s.dvm.sink.put(req);
      itrace(s, fshow(req));
    endaction, action
      let rsp <- s.dvm.source.get();
      itrace(s, fshow(rsp));
      PAddr paddr = rsp.addr;
    `else
      PAddr paddr = toPAddr(vaddr);
    `endif
    `ifdef PMP
    `ifdef SUPERVISOR_MODE
      let req = aReqWrite(paddr, readBitPO(numBytes), rsp.mExc);
    `else
      let req = aReqWrite(paddr, readBitPO(numBytes), Invalid);
    `endif
      s.dpmp.sink.put(req);
      itrace(s, fshow(req));
    endaction, action
      let rsp <- s.dpmp.source.get();
      itrace(s, fshow(rsp));
      RVMemReq req = RVWriteReq {
        addr: rsp.addr,
        byteEnable: ~((~0) << readBitPO(numBytes)),
        data: wdata
        `ifdef RVXCHERI
        , captag: pack(capWrite)
        `endif
      };
    `else
      RVMemReq req = RVWriteReq {
        addr: paddr,
        byteEnable: ~((~0) << readBitPO(numBytes)),
        data: wdata
        `ifdef RVXCHERI
        , captag: pack(capWrite)
        `endif
      };
    `endif
      s.dmem.sink.put(req);
    `ifdef RVFI_DII
      s.mem_wdata[0] <= truncate(req.RVWriteReq.data);
      s.mem_wmask[0] <= truncate(req.RVWriteReq.byteEnable);
    `endif
      itrace(s, fshow(req));
    endaction, action
      let rsp <- s.dmem.source.get();
      case (rsp) matches
        tagged RVWriteRsp .w: noAction;
        tagged RVBusError: action trap(s, StrAMOAccessFault); endaction
      endcase
      itrace(s, fshow(rsp));
    endaction
  `ifdef RVXCHERI
  ))))))))));
  `else
  ));
  `endif
endfunction
// TODO deal with exceptions

function Recipe writeData(
  RVState s,
  StrArgs args,
  VAddr vaddr,
  Bit#(128) wdata
) =
  `ifndef RVXCHERI
  rAct(s.writeMem.enq(tuple3(vaddr, fromInteger(args.numBytes), wdata)));
  `else
  rAct(s.writeMem.enq(tuple4(DDCAccessHandle(vaddr), fromInteger(args.numBytes), wdata, False)));
  `endif

`ifdef RVXCHERI
function Recipe writeCap(
  RVState s,
  StrArgs args,
  VAddr vaddr,
  CapType cap
) = rAct(s.writeMem.enq(tuple4(DDCAccessHandle(vaddr), fromInteger(args.numBytes), zeroExtend(pack(cap.Data)), isCap(cap))));

function Recipe capWriteData(
  RVState s,
  StrArgs args,
  Bit#(5) capIdx,
  Bit#(128) wdata
) = rAct(s.writeMem.enq(tuple4(CapAccessHandle(tuple2(capIdx, s.rCR(capIdx))), fromInteger(args.numBytes), wdata, False)));

function Recipe capWriteCap(
  RVState s,
  StrArgs args,
  Bit#(5) capIdx,
  CapType wcap
) = rAct(s.writeMem.enq(tuple4(CapAccessHandle(tuple2(capIdx, s.rCR(capIdx))), fromInteger(args.numBytes), zeroExtend(pack(wcap.Data)), isCap(wcap))));
`endif
