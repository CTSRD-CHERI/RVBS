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
import SpecialFIFOs :: *;
//import UniqueWrappers :: * ;

import BlueBasics :: *;
import BlueUtils :: *;
import Recipe :: *;
import RVBS_BasicTypes :: *;
import RVBS_MemTypes :: *;
import RVBS_CSRTypes :: *;
import RVBS_PMPTypes :: *;
import RVBS_VMTranslateTypes :: *;

interface PageWalker;
  method Action lookup (AddrReq#(VAddr) r);
endinterface

`ifndef XLEN64 // MAX_XLEN = 32

/////////////////
// Sv32 walker //
/////////////////

`define PTESIZE 4
`define PAGESIZE 2**12

typedef struct {
  Bit#(10) vpn1;
  Bit#(10) vpn0;
  Bit#(12) pgoffset;
} Sv32VAddr deriving (Bits, FShow);

typedef struct {
  Bit#(12) ppn1;
  Bit#(10) ppn0;
  Bit#(12) pgoffset;
} Sv32PAddr deriving (Bits, FShow);

typedef struct {
  Bit#(12) ppn1;
  Bit#(10) ppn0;
  Bit#(2) rsw;
  Bool d;
  Bool a;
  Bool g;
  Bool u;
  Bool x;
  Bool w;
  Bool r;
  Bool v;
} Sv32PTE deriving (Bits, FShow);

module [Module] mkSv32PageWalker#(
  FIFOF#(Either#(ExcToken, PAddr)) rsp
  , SATP satp
  , RVMem mem
  `ifdef PMP
  , PMPLookup pmp
  `endif
  ) (PageWalker);

  // required page walker mechanism "variable"
  Reg#(Bool) activeLookup[2] <- mkCReg(2, False);
  Reg#(Bit#(1))    i[2] <- mkCReg(2, 1);
  Reg#(Sv32PAddr)  a[2] <- mkCReg(2, ?);
  Reg#(Sv32VAddr) va[2] <- mkCReg(2, ?);
  Reg#(RVMemReqType) rType[2] <- mkCReg(2, ?);
  //PulseWire startReq <- mkPulseWire;

  // physical memory request machine
  // XXX TODO sort out physical memory addr width (34)
  PAddr pteAddr = unpack(pack(a[1]) + (((i[1] == 1) ? zeroExtend(va[1].vpn1) : zeroExtend(va[1].vpn0)) << log2(`PTESIZE)));
  RecipeFSM memReq <- mkRecipeFSM(rPar(rBlock(
    `ifdef PMP
    action
      pmp.sink.put(Right(AddrReq{
        addr: pteAddr,
        numBytes: `PTESIZE,
        reqType: rType[1]
      }));
    endaction, action
      let r <- get(pmp.source);
      let e_req = case (r) matches
        tagged Left .excTok:
          return Left(excTok);
        tagged Right .checkedAddr:
          return Right(RVReadReq{addr: checkedAddr, numBytes: `PTESIZE});
      endcase;
    `else
    action
      let e_req = Right(RVReadReq{addr: pteAddr, numBytes: `PTESIZE});
    `endif
      printTLogPlusArgs("debug", $format("DEBUG - a[1] = 0x%0x", a[1]));
      printTLogPlusArgs("debug", $format("DEBUG - i[1] = %0d", i[1]));
      printTLogPlusArgs("debug", $format("DEBUG - va[1].vpn1 = 0x%0x", va[1].vpn1));
      printTLogPlusArgs("debug", $format("DEBUG - va[1].vpn0 = 0x%0x", va[1].vpn0));
      printTLogPlusArgs("debug", $format("DEBUG - log2(PTESIZE) = %0d", log2(`PTESIZE)));
      //printTLogPlusArgs("vmem", $format("VMEM - Sv32 mem access, sending ", fshow(e_req)));
      mem.sink.put(e_req);
    endaction
  )));

    // rule observing the received pte
    let pgFault = case (rType[1])
      READ: return LoadPgFault;
      WRITE: return StrAMOPgFault;
      IFETCH: return InstPgFault;
    endcase;
    let pgFaultRsp = Left(craftExcToken(pgFault));
    rule checkPTE (activeLookup[1]);
      printTLogPlusArgs("vmem", $format("VMEM - Sv32 checkPTE rule"));
      function finishLookup(x) = action
        //printTLogPlusArgs("vmem", $format("VMEM - Sv32 checkPTE rule - returning ", fshow(x)));
        activeLookup[1] <= False;
        rsp.enq(x);
      endaction;
      let tmp <- get(mem.source);
      case (tmp) matches
        tagged Left .excTok: begin $display("Received a non read response when fetching a PTE"); $finish(0); end
        tagged Right .memRsp: case (memRsp) matches
          `ifndef RVXCHERI
          tagged RVReadRsp .r: begin
          `else
          tagged RVReadRsp {._, .r}: begin
          `endif
            Sv32PTE pte = unpack(truncate(r));
            printTLogPlusArgs("vmem", $format("VMEM - Sv32 checkPTE rule - ", fshow(pte)));
            // invalid pte
            if (!pte.v || (!pte.r && pte.w)) finishLookup(pgFaultRsp);
            // leaf pte
            else if (pte.r || pte.x) begin
              finishLookup(Right({pte.ppn1, (i[1] > 0) ? va[1].vpn0 : pte.ppn0, va[1].pgoffset}));
              // TODO more checks on pte and stuff
              //if(exc) rsp.enq(exc);
              //else rsp.enq(PAddr);
            // non leaf pte
            end else begin
              if (i[0] == 0) finishLookup(pgFaultRsp);
              else begin
                i[0] <= i[0] - 1;
                a[0] <= unpack(zeroExtend({pte.ppn1, pte.ppn0}) << log2(`PAGESIZE));
                //startReq.send();
                memReq.trigger;
              end
            end
          end
          default: begin $display("Received a non read response when fetching a PTE"); $finish(0); end
        endcase
      endcase
    endrule

    //rule doStartReq (startReq);
    //  memReq.start();
    //endrule

    // lookup method
    method Action lookup (AddrReq#(VAddr) r) if (!activeLookup[0]);
      i[0] <= 1;
      a[0] <= unpack(zeroExtend(satp.ppn) << log2(`PAGESIZE));
      va[0] <= unpack(r.addr);
      rType[0] <= r.reqType;
      activeLookup[0] <= True;
      //startReq.send();
      memReq.trigger;
      printTLogPlusArgs("vmem", "VMEM - Sv32 starting lookup");
    endmethod

endmodule

`endif

module [Module] mkVMLookup#(CSRs csrs
  , RVMem mem
  `ifdef PMP
  , PMPLookup pmp
  `endif
  ) (VMLookup);
  // local module instances
  let rsp <- mkBypassFIFOF;
  `ifndef XLEN64 // MAX_XLEN = 32
  PageWalker sv32PageWalker <- mkSv32PageWalker(
    rsp
    , csrs.satp
    , mem
    `ifdef PMP
    , pmp
    `endif
    );
  `endif
  // lookup method
  function Action lookup (AddrReq#(VAddr) req) = action
    // TODO
    printTLogPlusArgs("vmem", $format("VMEM - lookup ", fshow(req)));
    printTLogPlusArgs("vmem", $format("VMEM - lookup ", fshow(csrs.satp)));
    case (csrs.satp.mode)
      BARE: begin
        rsp.enq(Right(toPAddr(req.addr)));
        printTLogPlusArgs("vmem", $format("VMEM - BARE mode, returning ", fshow(toPAddr(req.addr))));
      end
      `ifdef XLEN64 // MAX_XLEN > 32
      `else // MAX_XLEN = 32
      SV32: sv32PageWalker.lookup(req);
      `endif
      default: begin $display("Unsupported STAP mode for translation. Should not happen."); $finish(0); end // XXX TODO add assertion
    endcase
  endaction;
  // build the lookup interface
  interface sink = interface Sink;
    method canPut = rsp.notFull;
    method put (e_req) = case (e_req) matches
      tagged Left .excTok: action
        printTLogPlusArgs("vmem", $format("VMEM - incomming exception %s, pass it down", fshow(excTok)));
        rsp.enq(Left(excTok)); // always pass down incomming exception without further side effects
      endaction
      tagged Right .req: lookup(req);
      //else rsp.enq(AddrRsp {addr: toPAddr(req.addr), mExc: Invalid});
    endcase;
  endinterface;
  interface source = toSource(rsp);

endmodule
