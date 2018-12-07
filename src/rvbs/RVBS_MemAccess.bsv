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

import BlueBasics :: *;

import RVBS_Types :: *;
import RVBS_Trap :: *;
import RVBS_Traces :: *;

// Read access

function List#(Action) readMem(
    RVState s,
    LoadArgs args,
    VAddr vaddr,
    Bit#(5) dest
    `ifdef RVXCHERI
    , Bool capRead
    `endif
  ) = list(
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
        `else
        let data = r;
        `endif
        Bool isNeg = unpack(data[(args.numBytes*8)-1]);
        Bit#(XLEN) mask = (~0) << args.numBytes*8;
        `ifdef RVXCHERI
        let newData = Data(data);
        if (captag) newData = Cap(unpack(data));
        if (capRead) s.wCR(dest, newData);
        else
        `endif
        s.wGPR(dest, (args.sgnExt && isNeg) ? truncate(data) | mask : truncate(data) & ~mask);
      end
      tagged RVBusError: action trap(s, LoadAccessFault); endaction
    endcase
    itrace(s.pc, fshow(rsp));
  endaction);
// TODO deal with exceptions

function List#(Action) readData(
  RVState s,
  LoadArgs args,
  VAddr vaddr,
  Bit#(5) dest
) = readMem(s, args, vaddr, dest
  `ifdef RVXCHERI
  , False
  `endif
);

`ifdef RVXCHERI
function List#(Action) readCap(
  RVState s,
  LoadArgs args,
  VAddr vaddr,
  Bit#(5) dest
) = readMem(s, args, vaddr, dest, True);
`endif

// Write access

function List#(Action) writeMem(RVState s, StrArgs args, VAddr vaddr, Bit#(128) wdata
    `ifdef RVXCHERI
    , Bool isCap
    `endif
  );
  return list(action
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
  endaction);
endfunction
// TODO deal with exceptions

function List#(Action) writeData(
  RVState s,
  StrArgs args,
  VAddr vaddr,
  Bit#(128) wdata
) = writeMem(s, args, vaddr, wdata
  `ifdef RVXCHERI
  , False
  `endif
);

`ifdef RVXCHERI
function List#(Action) writeCap(
  RVState s,
  LoadArgs args,
  VAddr vaddr,
  CapType cap
) = writeMem(s, args, vaddr, cap.Data, isCap(cap));
`endif
