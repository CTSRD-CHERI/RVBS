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

/////////////////////////////////
// Load and Store Instructions //
////////////////////////////////////////////////////////////////////////////////

/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
*/
function Recipe load(RVState s, LoadArgs args, Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
  readData(s, args, s.rGPR(rs1) + signExtend(imm), rd);

/*
  S-type

   31                        25 24    20 19    15 14    12 11     7 6        0
  +----------------------------+--------+--------+--------+--------+----------+
  |         imm[11:5]          |   rs2  |   rs1  | funct3 |imm[4:0]|  opcode  |
  +----------------------------+--------+--------+--------+--------+----------+
*/
function Recipe store(RVState s, StrArgs args, Bit#(7) imm11_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) imm4_0);
  Bit#(XLEN) imm = {signExtend(imm11_5), imm4_0};
  return writeData(s, args, zeroExtend(s.rGPR(rs2)), s.rGPR(rs1) + signExtend(imm));
endfunction

///////////////////
// inner helpers //
////////////////////////////////////////////////////////////////////////////////

function Either#(ExcToken, a) excOrArg (Maybe#(ExcToken) m_tok, a x);
  if (isValid(m_tok)) return Left(m_tok.Valid);
  else return Right(x);
endfunction

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
  if (!isValidCap(cap)) return Valid(CapExcTag);
  else if (!isUnsealed(cap)) return Valid(CapExcSeal);
  else if (reqType == READ   && !getHardPerms(cap).permitLoad) return Valid(CapExcPermLoad);
  else if (reqType == WRITE  && !getHardPerms(cap).permitStore) return Valid(CapExcPermStore);
  else if (reqType == READ   && capAccess && !getHardPerms(cap).permitLoadCap) return Valid(CapExcPermLoadCap);
  else if (reqType == WRITE  && capAccess && !getHardPerms(cap).permitStoreCap) return Valid(CapExcPermStoreCap);
  else if (zeroExtend(vaddr) < getBase(cap)) return Valid(CapExcLength);
  else if (zeroExtend(vaddr) + zeroExtend(readBitPO(numBytes)) > getTop(cap)) return Valid(CapExcLength);
  else return Invalid;
endfunction
function Maybe#(CapExcCode) ifetchCapChecks(
  CapType cap,
  VAddr vaddr,
  BitPO#(4) numBytes,
  Bool capAccess);
  if (!isValidCap(cap)) return Valid(CapExcTag);
  else if (!isUnsealed(cap)) return Valid(CapExcSeal);
  else if (!getHardPerms(cap).permitExecute) return Valid(CapExcPermExe);
  else if (zeroExtend(vaddr) < getBase(cap)) return Valid(CapExcLength);
  else if (zeroExtend(vaddr) + zeroExtend(readBitPO(numBytes)) > getTop(cap)) return Valid(CapExcLength);
  else return Invalid;
endfunction
`endif

// Read access
////////////////////////////////////////////////////////////////////////////////

function Recipe doReadMemCore(
  `ifdef SUPERVISOR_MODE
  VMLookup vm,
  `endif
  `ifdef PMP
  PMPLookup pmp,
  `endif
  RVMem mem,
  Tuple4#(Maybe#(ExcToken), RVMemReqType, VAddr, BitPO#(4)) args,
  Sink#(Either#(ExcToken, RVMemRsp)) rspSink
);
  match {.m_excToken, .reqType, .vaddr, .numBytes} = args;
  return rPipe(rBlock(
    rFastSeq(rBlock(action
      `ifdef SUPERVISOR_MODE
      let req = excOrArg(m_excToken, aReq(reqType, vaddr, numBytes));
      vm.sink.put(req);
    endaction, action
      let rAddr <- get(vm.source);
      `else
      let rAddr = excOrArg(m_excToken, toPAddr(vaddr));
      `endif
      `ifdef PMP
      let req = craftAReq(reqType, rAddr, numBytes);
      pmp.sink.put(req);
    endaction, action
      let rAddr <- get(pmp.source);
      `endif
      let req = case (rAddr) matches
        tagged Left .excTok: return Left(excTok);
        tagged Right .checkedAddr:
          return Right(RVReadReq {addr: checkedAddr, numBytes: numBytes});
      endcase;
      mem.sink.put(req);
    endaction)), action
      let rsp <- get(mem.source);
      rspSink.put(rsp);
    endaction));
endfunction

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

function Recipe doWriteMemCore(
  `ifdef SUPERVISOR_MODE
  VMLookup vm,
  `endif
  `ifdef PMP
  PMPLookup pmp,
  `endif
  RVMem mem,
  `ifndef RVXCHERI
  Tuple4#(Maybe#(ExcToken), VAddr, BitPO#(4), Bit#(128)) args,
  `else
  Tuple5#(Maybe#(ExcToken), VAddr, BitPO#(4), Bit#(128), Bool) args,
  `endif
  Sink#(Either#(ExcToken, RVMemRsp)) rspSink
);
  `ifndef RVXCHERI
  match {.m_excToken, .vaddr, .numBytes, .wdata} = args;
  `else
  match {.m_excToken, .vaddr, .numBytes, .wdata, .capWrite} = args;
  `endif
  return rFastSeq(rBlock(action
    `ifdef SUPERVISOR_MODE
    let req = excOrArg(m_excToken, aReqWrite(vaddr, numBytes));
    vm.sink.put(req);
  endaction, action
    let rsp <- get(vm.source);
    `else
    let rsp = excOrArg(m_excToken, toPAddr(vaddr));
    `endif
    `ifdef PMP
    let req = craftAReq(WRITE, rsp, numBytes);
    pmp.sink.put(req);
  endaction, action
    let rsp <- get(pmp.source);
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
    mem.sink.put(req);
  endaction, action
    let rsp <- get(mem.source);
    rspSink.put(rsp);
  endaction));
endfunction

function Recipe writeData(
  RVState s,
  StrArgs args,
  Bit#(128) wdata,
  VAddr vaddr
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
  CapType cap,
  VAddr vaddr
);
  Bit#(CapNoValidSz) capBits = truncate(pack(cap));
  return rAct(s.writeMem.enq(tuple4(
           DDCAccessHandle(vaddr),
           fromInteger(args.numBytes),
           zeroExtend(capBits),
           isValidCap(cap))));
endfunction

function Recipe capWriteData(
  RVState s,
  StrArgs args,
  Bit#(128) wdata,
  Bit#(5) capIdx
) = rAct(s.writeMem.enq(tuple4(CapAccessHandle(tuple2(capIdx, s.rCR(capIdx))), fromInteger(args.numBytes), wdata, False)));

function Recipe capWriteCap(
  RVState s,
  StrArgs args,
  CapType wcap,
  Bit#(5) capIdx
);
  Bit#(CapNoValidSz) capBits = truncate(pack(wcap));
  return rAct(s.writeMem.enq(tuple4(
           CapAccessHandle(tuple2(capIdx, s.rCR(capIdx))),
           fromInteger(args.numBytes),
           zeroExtend(capBits),
           isValidCap(wcap))));
endfunction
`endif
