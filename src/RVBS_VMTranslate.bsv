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
import SpecialFIFOs :: *;
//import UniqueWrappers :: * ;

import BlueBasics :: *;
import BlueUtils :: *;
import Recipe :: *;
import RVBS_BasicTypes :: *;
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
  FIFOF#(AddrRsp#(PAddr)) rsp
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
      pmp.sink.put(AddrReq {
        addr: pteAddr,
        numBytes: `PTESIZE,
        reqType: rType[1],
        mExc: Invalid
      });
    endaction, action
      let r <- pmp.source.get();
      if (isValid(r.mExc)) rsp.enq(r); // terminate early on PMP exception
      else begin
        RVMemReq req = RVReadReq {addr: r.addr, numBytes: `PTESIZE};
    `else
    action
      RVMemReq req = RVReadReq {addr: pteAddr, numBytes: `PTESIZE};
    `endif
      printTLogPlusArgs("debug", $format("DEBUG - a[1] = 0x%0x", a[1]));
      printTLogPlusArgs("debug", $format("DEBUG - i[1] = %0d", i[1]));
      printTLogPlusArgs("debug", $format("DEBUG - va[1].vpn1 = 0x%0x", va[1].vpn1));
      printTLogPlusArgs("debug", $format("DEBUG - va[1].vpn0 = 0x%0x", va[1].vpn0));
      printTLogPlusArgs("debug", $format("DEBUG - log2(PTESIZE) = %0d", log2(`PTESIZE)));
      printTLogPlusArgs("vmem", $format("VMEM - Sv32 mem access, sending ", fshow(req)));
      mem.sink.put(req);
    `ifdef PMP
    end
    `endif
    endaction
    )));

    // rule observing the received pte
    let pgFault = case (rType[1])
      READ: return Valid(LoadPgFault);
      WRITE: return Valid(StrAMOPgFault);
      IFETCH: return Valid(InstPgFault);
    endcase;
    let pgFaultRsp = AddrRsp {addr: ?, mExc: pgFault};
    rule checkPTE (activeLookup[1]);
      printTLogPlusArgs("vmem", $format("VMEM - Sv32 checkPTE rule"));
      function finishLookup(x) = action
        printTLogPlusArgs("vmem", $format("VMEM - Sv32 checkPTE rule - returning ", fshow(x)));
        activeLookup[1] <= False;
        rsp.enq(x);
      endaction;
      let tmp <- mem.source.get();
      case (tmp) matches
        tagged RVReadRsp .r: begin
          Sv32PTE pte = unpack(truncate(r));
          printTLogPlusArgs("vmem", $format("VMEM - Sv32 checkPTE rule - ", fshow(pte)));
          // invalid pte
          if (!pte.v || (!pte.r && pte.w)) finishLookup(pgFaultRsp);
          // leaf pte
          else if (pte.r || pte.x) begin
            finishLookup(AddrRsp {
              addr: {pte.ppn1, (i[1] > 0) ? va[1].vpn0 : pte.ppn0, va[1].pgoffset},
              mExc: Invalid
            });
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
        default: begin $display("Received a non read response when fetching a PTE"); $finish(0); end // XXX TODO add assertion
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
  FIFOF#(AddrRsp#(PAddr)) rsp <- mkBypassFIFOF;
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
        let r = AddrRsp {addr: toPAddr(req.addr), mExc: Invalid};
        rsp.enq(r);
        printTLogPlusArgs("vmem", $format("VMEM - BARE mode, returning ", fshow(r)));
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
    method put (req) = action
      if (isValid(req.mExc)) begin
        printTLogPlusArgs("vmem", $format("VMEM - incomming %s, pass it down", fshow(req.mExc)));
        rsp.enq(AddrRsp {addr: ?, mExc: req.mExc}); // always pass down incomming exception without further side effects
      end else lookup(req);
      //else rsp.enq(AddrRsp {addr: toPAddr(req.addr), mExc: Invalid});
    endaction;
  endinterface;
  interface source = toSource(rsp);

endmodule
