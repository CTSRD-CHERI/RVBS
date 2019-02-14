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

import FIFOF :: *;

import Recipe :: *;
import BlueUtils :: *;
import BlueBasics :: *;

import RVBS_Types :: *;
import RVBS_Trap :: *;
import RVBS_TraceInsts :: *;

`ifdef RVXCHERI
import CHERICap :: *;
`endif

`ifdef RVXCHERI
function Maybe#(CapExcCode) memCapChecks(
  RVMemReqType reqType,
  CapType cap,
  VAddr vaddr,
  BitPO#(4) numBytes,
  Bool capAccess);
  if (!isCap(cap)) return Valid(CapExcTag);
  else if (!isUnsealed(cap.Cap)) return Valid(CapExcSeal);
  else if (reqType == READ   && !getHardPerms(cap.Cap).permitLoad) return Valid(CapExcPermLoad);
  else if (reqType == WRITE  && !getHardPerms(cap.Cap).permitStore) return Valid(CapExcPermStore);
  else if (reqType == READ   && capAccess && !getHardPerms(cap.Cap).permitLoadCap) return Valid(CapExcPermLoadCap);
  else if (reqType == WRITE  && capAccess && !getHardPerms(cap.Cap).permitStoreCap) return Valid(CapExcPermStoreCap);
  else if (zeroExtend(vaddr) < getBase(cap.Cap)) return Valid(CapExcLength);
  else if (zeroExtend(vaddr) + zeroExtend(readBitPO(numBytes)) > getTop(cap.Cap)) return Valid(CapExcLength);
  else return Invalid;
endfunction
function Maybe#(CapExcCode) ifetchCapChecks(
  CapType cap,
  VAddr vaddr,
  BitPO#(4) numBytes,
  Bool capAccess);
  if (!isCap(cap)) return Valid(CapExcTag);
  else if (!isUnsealed(cap.Cap)) return Valid(CapExcSeal);
  else if (!getHardPerms(cap.Cap).permitExecute) return Valid(CapExcPermExe);
  else if (zeroExtend(vaddr) < getBase(cap.Cap)) return Valid(CapExcLength);
  else if (zeroExtend(vaddr) + zeroExtend(readBitPO(numBytes)) > getTop(cap.Cap)) return Valid(CapExcLength);
  else return Invalid;
endfunction
`endif

// Read access
////////////////////////////////////////////////////////////////////////////////

function Either#(ExcToken, a) excOrArg (Maybe#(ExcToken) m_tok, a x);
  if (isValid(m_tok)) return Left(m_tok.Valid);
  else return Right(x);
endfunction

function Recipe doReadMemCore(
  Maybe#(ExcToken) m_excToken,
  RVMemReqType reqType,
  function Recipe rWrap(Recipe r, Action a),
  function Action rspCallBack (Either#(ExcToken, RVMemRsp) rsp),
  `ifdef SUPERVISOR_MODE
  VMLookup vm,
  `endif
  `ifdef PMP
  PMPLookup pmp,
  `endif
  RVMem mem,
  RVState s,
  VAddr vaddr,
  BitPO#(4) numBytes
) = rWrap(rFastSeq(rBlock(action
    `ifdef RVFI_DII
    s.mem_addr[0] <= vaddr;
    `endif
    `ifdef SUPERVISOR_MODE
    let req = excOrArg(m_excToken, aReq(reqType, vaddr, numBytes));
    vm.sink.put(req);
    //itrace(s, fshow(req));
  endaction, action
    let rAddr <- get(vm.source);
    //itrace(s, fshow(rsp));
    `else
    let rAddr = excOrArg(m_excToken, toPAddr(vaddr));
    `endif
    `ifdef PMP
    let req = craftAReq(reqType, rAddr, numBytes);
    pmp.sink.put(req);
    //itrace(s, fshow(req));
  endaction, action
    let rAddr <- get(pmp.source);
    //itrace(s, fshow(rsp));
    `endif
    let req = case (rAddr) matches
      tagged Left .excTok: return Left(excTok);
      tagged Right .checkedAddr:
        return Right(RVReadReq {addr: checkedAddr, numBytes: numBytes});
    endcase;
    mem.sink.put(req);
    //itrace(s, fshow(req));
  endaction)), action
    let rsp <- get(mem.source);
    rspCallBack(rsp);
    //itrace(s, fshow(rsp));
  endaction
);
// TODO deal with exceptions
function Recipe wrapPipe(Recipe r, Action a) = rPipe(rBlock(r, rAct(a)));
function Recipe wrapFastSeq(Recipe r, Action a) = rFastSeq(rBlock(r, rAct(a)));

function Recipe doIFetchMem(
  Maybe#(ExcToken) m_excToken,
  function Action rspCallBack (Either#(ExcToken, RVMemRsp) rsp),
  RVState s,
  VAddr vaddr,
  BitPO#(4) numBytes
) = doReadMemCore(
      m_excToken,
      IFETCH,
      wrapPipe,
      rspCallBack,
      `ifdef SUPERVISOR_MODE
      s.ivm,
      `endif
      `ifdef PMP
      s.ipmp,
      `endif
      s.imem,
      s,
      vaddr,
      numBytes);

function Recipe doReadMem(
  Maybe#(ExcToken) m_excToken,
  function Action rspCallBack (Either#(ExcToken, RVMemRsp) rsp),
  RVState s,
  VAddr vaddr,
  BitPO#(4) numBytes
) = doReadMemCore(
      m_excToken,
      READ,
      wrapFastSeq,
      rspCallBack,
      `ifdef SUPERVISOR_MODE
      s.dvm,
      `endif
      `ifdef PMP
      s.dpmp,
      `endif
      s.dmem,
      s,
      vaddr,
      numBytes);

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
  Maybe#(ExcToken) m_excToken,
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
  match {.capIdx, .cap, .vaddr} = unpackHandle(s.ddc, s.pcc, handle);
  return rFastSeq(rBlock(
  rIfElse (!isCap(cap), raiseMemCapException(s, CapExcTag, capIdx),
  rIfElse (!isUnsealed(cap.Cap), raiseMemCapException(s, CapExcSeal, capIdx),
  rIfElse (!getHardPerms(cap.Cap).permitStore, raiseMemCapException(s, CapExcPermStore, capIdx),
  rIfElse (capWrite && !getHardPerms(cap.Cap).permitStoreCap, raiseMemCapException(s, CapExcPermStoreCap, capIdx),
  rIfElse (zeroExtend(vaddr) < getBase(cap.Cap), raiseMemCapException(s, CapExcLength, capIdx),
  rIfElse (zeroExtend(vaddr) + zeroExtend(readBitPO(numBytes)) > getTop(cap.Cap), raiseMemCapException(s, CapExcLength, capIdx),
    rFastSeq(rBlock(
  `else
  return rFastSeq(rBlock(
  `endif
    action
    `ifdef RVFI_DII
      s.mem_addr[0] <= vaddr;
    `endif
    `ifdef SUPERVISOR_MODE
      let req = excOrArg(m_excToken, aReqWrite(vaddr, numBytes));
      s.dvm.sink.put(req);
      //itrace(s, fshow(req));
    endaction, action
      let rsp <- get(s.dvm.source);
      //itrace(s, fshow(rsp));
    `else
      let rsp = excOrArg(m_excToken, toPAddr(vaddr));
    `endif
    `ifdef PMP
      let req = craftAReq(WRITE, rsp, numBytes);
      s.dpmp.sink.put(req);
      //itrace(s, fshow(req));
    endaction, action
      let rsp <- get(s.dpmp.source);
      //itrace(s, fshow(rsp));
    `endif
      let req = case (rsp) matches
        tagged Left .excTok: return Left(excTok);
        tagged Right .checkedAddr:
          return Right(RVWriteReq{
            addr: checkedAddr,
            byteEnable: ~((~0) << readBitPO(numBytes)),
            data: wdata
            `ifdef RVXCHERI
            , captag: pack(capWrite)
            `endif
          });
      endcase;
      s.dmem.sink.put(req);
    `ifdef RVFI_DII
      s.mem_wdata[0] <= truncate(req.Right.RVWriteReq.data);
      s.mem_wmask[0] <= truncate(req.Right.RVWriteReq.byteEnable);
    `endif
      //itrace(s, fshow(req));
    endaction, action
      let rsp <- get(s.dmem.source);
      case (rsp) matches
        tagged Left .excTok: raiseMemTokException(s, excTok);
        tagged Right .memRsp: case (memRsp) matches
          tagged RVWriteRsp .w: noAction;
          tagged RVBusError: action raiseMemException(s, StrAMOAccessFault); endaction
        endcase
      endcase
      //itrace(s, fshow(rsp));
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
