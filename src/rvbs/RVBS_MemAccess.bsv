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

import Recipe :: *;
import BlueUtils :: *;
import BlueBasics :: *;

import CHERICap :: *;

import RVBS_Types :: *;
import RVBS_Trap :: *;
import RVBS_Traces :: *;

// Read access
////////////////////////////////////////////////////////////////////////////////

function Recipe readMem(
  RVState s,
  LoadArgs args,
  VAddr vaddr,
  Bit#(5) dest
  `ifdef RVXCHERI
  , Bool capRead
  `endif
) = rFastSeq(rBlock(
  action
  `ifdef RVFI_DII
    s.mem_addr[0] <= vaddr;
  `endif
  `ifdef SUPERVISOR_MODE
    let req = aReqRead(vaddr, args.numBytes, Invalid);
    s.dvm.sink.put(req);
    itrace(s.pc, fshow(req));
  endaction, action
    let rsp <- s.dvm.source.get();
    itrace(s.pc, fshow(rsp));
    PAddr paddr = rsp.addr;
  `else
    PAddr paddr = toPAddr(vaddr);
  `endif
  `ifdef PMP
  `ifdef SUPERVISOR_MODE
    let req = aReqRead(paddr, args.numBytes, rsp.mExc);
  `else
    let req = aReqRead(paddr, args.numBytes, Invalid);
  `endif
    s.dpmp.sink.put(req);
    itrace(s.pc, fshow(req));
  endaction, action
    let rsp <- s.dpmp.source.get();
    itrace(s.pc, fshow(rsp));
    RVMemReq req = RVReadReq {addr: rsp.addr, numBytes: fromInteger(args.numBytes)};
  `else
    RVMemReq req = RVReadReq {addr: paddr, numBytes: fromInteger(args.numBytes)};
  `endif
    s.dmem.sink.put(req);
    itrace(s.pc, fshow(req));
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
        Bool isNeg = unpack(data[(args.numBytes*8)-1]);
        Bit#(XLEN) mask = (~0) << args.numBytes*8;
        `ifdef RVXCHERI
        let newData = Data(pack(newCap));
        if (captag == 1) newData = Cap(newCap);
        if (capRead) s.wCR(dest, newData);
        else
        `endif
        s.wGPR(dest, (args.sgnExt && isNeg) ? truncate(data) | mask : truncate(data) & ~mask);
      end
      tagged RVBusError: action trap(s, LoadAccessFault); endaction
    endcase
    itrace(s.pc, fshow(rsp));
  endaction
));
// TODO deal with exceptions

function Recipe readData(
  RVState s,
  LoadArgs args,
  VAddr vaddr,
  Bit#(5) dest
) =
  `ifndef RVXCHERI
  readMem(s, args, vaddr, dest);
  `else
  readMem_cap_check(s, args, 6'b100001, s.ddc, truncate(getBase(s.ddc.Cap)) + vaddr, dest, False);
  `endif
  // XXX 6'b100001 is ddc

`ifdef RVXCHERI
function Recipe readCap(
  RVState s,
  LoadArgs args,
  VAddr vaddr,
  Bit#(5) dest
) = readMem_cap_check(s, args, 6'b100001, s.ddc, truncate(getBase(s.ddc.Cap)) + vaddr, dest, True);
// XXX 6'b100001 is ddc

function Recipe capReadData(
  RVState s,
  LoadArgs args,
  Bit#(5) capIdx,
  CapType cap,
  VAddr vaddr,
  Bit#(5) dest
) = readMem_cap_check(s, args, zeroExtend(capIdx), cap, vaddr, dest, False);

function Recipe capReadCap(
  RVState s,
  LoadArgs args,
  Bit#(5) capIdx,
  CapType cap,
  VAddr vaddr,
  Bit#(5) dest
) = readMem_cap_check(s, args, zeroExtend(capIdx), cap, vaddr, dest, True);

function Recipe readMem_cap_check(
  RVState s,
  LoadArgs args,
  Bit#(6) capIdx,
  CapType cap,
  VAddr vaddr,
  Bit#(5) dest,
  Bool capRead
) = rFastSeq(rBlock(
  rIfElse (!isCap(cap), capTrap(s, CapExcTag, capIdx),
  rIfElse (getSealed(cap.Cap), capTrap(s, CapExcSeal, capIdx),
  rIfElse (!getPerms(cap.Cap).permitLoad, capTrap(s, CapExcPermLoad, capIdx),
  rIfElse (capRead && !getPerms(cap.Cap).permitLoadCap, capTrap(s, CapExcPermLoadCap, capIdx),
  rIfElse (zeroExtend(vaddr) < getBase(cap.Cap), capTrap(s, CapExcLength, capIdx),
  rIfElse (zeroExtend(vaddr) + fromInteger(args.numBytes) > getTop(cap.Cap), capTrap(s, CapExcLength, capIdx),
    readMem(s, args, vaddr, dest, capRead)))))))
));
`endif

// Write access
////////////////////////////////////////////////////////////////////////////////

function Recipe writeMem(RVState s, StrArgs args, VAddr vaddr, Bit#(128) wdata
    `ifdef RVXCHERI
    , Bool isCap
    `endif
  ) = rFastSeq(rBlock(action
  `ifdef RVFI_DII
    s.mem_addr[0] <= vaddr;
  `endif
  `ifdef SUPERVISOR_MODE
    let req = aReqWrite(vaddr, args.numBytes, Invalid);
    s.dvm.sink.put(req);
    itrace(s.pc, fshow(req));
  endaction, action
    let rsp <- s.dvm.source.get();
    itrace(s.pc, fshow(rsp));
    PAddr paddr = rsp.addr;
  `else
    PAddr paddr = toPAddr(vaddr);
  `endif
  `ifdef PMP
  `ifdef SUPERVISOR_MODE
    let req = aReqWrite(paddr, args.numBytes, rsp.mExc);
  `else
    let req = aReqWrite(paddr, args.numBytes, Invalid);
  `endif
    s.dpmp.sink.put(req);
    itrace(s.pc, fshow(req));
  endaction, action
    let rsp <- s.dpmp.source.get();
    itrace(s.pc, fshow(rsp));
    RVMemReq req = RVWriteReq {
      addr: rsp.addr,
      byteEnable: ~((~0) << args.numBytes),
      data: wdata
      `ifdef RVXCHERI
      , captag: pack(isCap)
      `endif
    };
  `else
    RVMemReq req = RVWriteReq {
      addr: paddr,
      byteEnable: ~((~0) << args.numBytes),
      data: wdata
      `ifdef RVXCHERI
      , captag: pack(isCap)
      `endif
    };
  `endif
    s.dmem.sink.put(req);
  `ifdef RVFI_DII
    s.mem_wdata[0] <= truncate(req.RVWriteReq.data);
    s.mem_wmask[0] <= truncate(req.RVWriteReq.byteEnable);
  `endif
    itrace(s.pc, fshow(req));
  endaction, action
    let rsp <- s.dmem.source.get();
    case (rsp) matches
      tagged RVWriteRsp .w: noAction;
      tagged RVBusError: action trap(s, StrAMOAccessFault); endaction
    endcase
    itrace(s.pc, fshow(rsp));
  endaction
));
// TODO deal with exceptions

function Recipe writeData(
  RVState s,
  StrArgs args,
  VAddr vaddr,
  Bit#(128) wdata
) =
  `ifndef RVXCHERI
  writeMem(s, args, vaddr, wdata);
  `else
  writeMem_cap_check(s, args, 6'b100001, s.ddc, truncate(getBase(s.ddc.Cap)) + vaddr, wdata, False);
  `endif
  // XXX 6'b100001 is ddc

`ifdef RVXCHERI
function Recipe writeCap(
  RVState s,
  StrArgs args,
  VAddr vaddr,
  CapType cap
) = writeMem_cap_check(s, args, 6'b100001, s.ddc, truncate(getBase(s.ddc.Cap)) + vaddr, zeroExtend(pack(cap.Data)), isCap(cap));
// XXX 6'b100001 is ddc

function Recipe capWriteData(
  RVState s,
  StrArgs args,
  Bit#(5) capIdx,
  CapType cap,
  VAddr vaddr,
  Bit#(128) wdata
) = writeMem_cap_check(s, args, zeroExtend(capIdx), cap, vaddr, wdata, False);

function Recipe capWriteCap(
  RVState s,
  StrArgs args,
  Bit#(5) capIdx,
  CapType cap,
  VAddr vaddr,
  CapType wcap
) = writeMem_cap_check(s, args, zeroExtend(capIdx), cap, vaddr, zeroExtend(pack(wcap.Data)), isCap(wcap));

function Recipe writeMem_cap_check(
  RVState s,
  StrArgs args,
  Bit#(6) capIdx,
  CapType cap,
  VAddr vaddr,
  Bit#(128) wdata,
  Bool capWrite
) = rFastSeq(rBlock(
  rIfElse (!isCap(cap), capTrap(s, CapExcTag, capIdx),
  rIfElse (getSealed(cap.Cap), capTrap(s, CapExcSeal, capIdx),
  rIfElse (!getPerms(cap.Cap).permitStore, capTrap(s, CapExcPermStore, capIdx),
  rIfElse (capWrite && !getPerms(cap.Cap).permitStoreCap, capTrap(s, CapExcPermStoreCap, capIdx),
  rIfElse (zeroExtend(vaddr) < getBase(cap.Cap), capTrap(s, CapExcLength, capIdx),
  rIfElse (zeroExtend(vaddr) + fromInteger(args.numBytes) > getTop(cap.Cap), capTrap(s, CapExcLength, capIdx),
    writeMem(s, args, vaddr, wdata, capWrite)))))))
));
`endif
