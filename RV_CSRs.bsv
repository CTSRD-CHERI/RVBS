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

import DefaultValue :: *;
import Printf :: *;
import Vector :: *;
import BID :: *;

import RV_BasicTypes :: *;
import RV_CSRTypes :: *;
`ifdef PMP
import RV_PMP :: *;
`endif

///////////////////////////
// Interface to the CSRs //
////////////////////////////////////////////////////////////////////////////////

typedef enum {RW, RS, RC} CSRReqType deriving (Eq, FShow);
typedef enum {ALL, NOREAD, NOWRITE} CSRReqEffects deriving (Eq, FShow);

typedef struct {
  Bit#(12) idx;
  Bit#(XLEN) val;
  CSRReqType rType;
  CSRReqEffects rEffects;
} CSRReq deriving (FShow);

instance DefaultValue#(CSRReq);
  function CSRReq defaultValue =
    CSRReq { idx: ?, val: ?, rType: RW, rEffects: ALL };
endinstance
function CSRReq rwCSRReq(Bit#(12) i, Bit#(XLEN) v) =
  CSRReq { idx: i, val: v, rType: RW, rEffects: ALL };
function CSRReq rwCSRReqNoRead(Bit#(12) i, Bit#(XLEN) v) =
  CSRReq { idx: i, val: v, rType: RW, rEffects: NOREAD };
function CSRReq rsCSRReq(Bit#(12) i, Bit#(XLEN) v) =
  CSRReq { idx: i, val: v, rType: RS, rEffects: ALL };
function CSRReq rsCSRReqNoWrite(Bit#(12) i, Bit#(XLEN) v) =
  CSRReq { idx: i, val: v, rType: RS, rEffects: NOWRITE };
function CSRReq rcCSRReq(Bit#(12) i, Bit#(XLEN) v) =
  CSRReq { idx: i, val: v, rType: RC, rEffects: ALL };
function CSRReq rcCSRReqNoWrite(Bit#(12) i, Bit#(XLEN) v) =
  CSRReq { idx: i, val: v, rType: RC, rEffects: NOWRITE };

typedef struct {

  // machine information registers
  //////////////////////////////////////////////////////////////////////////////
  Reg#(VendorID)   mvendorid;
  Reg#(Bit#(XLEN)) marchid;
  Reg#(Bit#(XLEN)) mimpid;
  Reg#(Bit#(XLEN)) mhartid;

  // machine trap setup registers
  //////////////////////////////////////////////////////////////////////////////
  Reg#(Status)     mstatus;
  Reg#(ISA)        misa;
  Reg#(MEDeleg)    medeleg;
  Reg#(IDeleg)     mideleg;
  Reg#(IE)         mie;
  Reg#(TVec)       mtvec;
  // TODO mcounteren

  // machine trap handling
  //////////////////////////////////////////////////////////////////////////////
  Reg#(Bit#(XLEN)) mscratch;
  Reg#(EPC)        mepc;
  Reg#(Cause)      mcause;
  Reg#(Bit#(XLEN)) mtval;
  Reg#(IP)         mip;

  // machine protection and translation
  //////////////////////////////////////////////////////////////////////////////
  `ifdef PMP
  // pmpcfg0, pmpcfg1, pmpcfg2, pmpcfg3
  `ifdef XLEN64
  Vector#(2, Reg#(Vector#(8, PMPCfg))) pmpcfg;
  `else
  Vector#(4, Reg#(Vector#(4, PMPCfg))) pmpcfg;
  `endif
  // pmpaddr0, pmpaddr1, ..., pmpaddr15
  Vector#(16, Reg#(PMPAddr)) pmpaddr;
  `endif

  `ifdef SUPERVISOR_MODE
  // supervisor trap setup
  //////////////////////////////////////////////////////////////////////////////
  // sstatus -- S-view of mstatus;
  Reg#(SEDeleg)    sedeleg;
  Reg#(IDeleg)     sideleg;
  // sie -- S-view of mie
  Reg#(TVec)       stvec;
  // TODO scounteren

  // supervisor trap handling
  //////////////////////////////////////////////////////////////////////////////
  Reg#(Bit#(XLEN)) sscratch;
  Reg#(EPC)        sepc;
  Reg#(Cause)      scause;
  Reg#(Bit#(XLEN)) stval;
  // sip -- S-view of mip

  // supervisor protection and translation
  //////////////////////////////////////////////////////////////////////////////
  Reg#(SATP) satp;
  `endif

  // user trap setup registers
  //////////////////////////////////////////////////////////////////////////////
  // ustatus
  // uie
  // utvec

  // user trap handling
  //////////////////////////////////////////////////////////////////////////////
  // uscratch
  // uepc
  // ucause
  // utval
  // uip

  // user counters/timers
  //////////////////////////////////////////////////////////////////////////////
  Reg#(Bit#(64)) cycle;
  // time
  //Reg#(Bit#(64)) instret;
  // hpmcounter3
  // hpmcounter4
  // ...
  // hpmcounter31

  // XXX for debug purposes:
  Reg#(Bit#(XLEN)) ctrl;

  // CSR request
  //////////////////////////////////////////////////////////////////////////////
  function ActionValue#(Bit#(XLEN)) doReq (CSRReq r) req;

} CSRs;

module mkRegUndef#(String name) (Reg#(a));
  method a _read() =
    error(sprintf("%s register read but not initialised", name));
  method Action _write(a val) =
    error(sprintf("%s register written but not initialised", name));
endmodule

//////////////////////////
// CSRs' implementation //
////////////////////////////////////////////////////////////////////////////////
`ifdef PMP
module mkCSRs#(PMP pmp)(CSRs);
`else
module mkCSRs(CSRs);
`endif

  // instance of the CSRs struct
  CSRs csrs;

  // machine information registers
  //////////////////////////////////////////////////////////////////////////////
  csrs.mvendorid <- mkReg(defaultValue); // mvendorid 12'hF11
  csrs.marchid   <- mkReg(0); // marchid 12'hF12
  csrs.mimpid    <- mkReg(0); // mimpid 12'hF13
  csrs.mhartid   <- mkReg(0); // mhartid 12'hF14

  // machine trap setup registers
  //////////////////////////////////////////////////////////////////////////////
  csrs.mstatus <- mkReg(defaultValue); // mstatus 12'h300
  csrs.misa    <- mkReg(defaultValue); // misa 12'h301
  csrs.medeleg <- mkRegUndef("medeleg");
  csrs.mideleg <- mkRegUndef("mideleg");
  if (static_HAS_S_MODE || (static_HAS_U_MODE && static_HAS_N_EXT)) begin
    csrs.medeleg <- mkReg(defaultValue); // medeleg 12'h302
    csrs.mideleg <- mkReg(defaultValue); // mideleg 12'h303
  end
  csrs.mie     <- mkReg(defaultValue); // mie 12'h304
  csrs.mtvec   <- mkReg(defaultValue); // mtvec 12'h305
  // mcounteren 12'h306

  // machine trap handling
  //////////////////////////////////////////////////////////////////////////////
  csrs.mscratch <- mkRegU; // mscratch 12'h340
  csrs.mepc     <- mkReg(defaultValue); // mepc 12'h341
  csrs.mcause   <- mkRegU; // mcause 12'h342
  csrs.mtval    <- mkRegU; // mtval 12'h343
  csrs.mip      <- mkReg(defaultValue); // mip 12'h344

  // machine protection and translation
  //////////////////////////////////////////////////////////////////////////////
  `ifdef PMP
  // pmpcfg0 12'h3A0
  // pmpcfg1 12'h3A1 (RV32 only)
  // pmpcfg2 12'h3A2
  // pmpcfg3 12'h3A3 (RV32 only)
  // pmpaddr0 12'h3B0
  // pmpaddr1 12'h3B1
  // ...
  // pmpaddr15 12'h3BF
  csrs.pmpcfg = pmp.cfg;
  csrs.pmpaddr = pmp.addr;
  `endif

  // machine counter / timers
  //////////////////////////////////////////////////////////////////////////////
  // mcycle 12'hB00
  // minsret 12'hB02
  // mhpmcounter3 12'hB03 (and 12'hB83 in RV32)
  // mhpmcounter4 12'hB04 (and 12'hB84 in RV32)
  // ...
  // mhpmcounter31 12'B1F (and 12'hB9F in RV32)

  // machine counter setup
  //////////////////////////////////////////////////////////////////////////////
  // mhpmevent3 12'h323
  // mhpmevent4 12'h324
  // ...
  // mhpmevent31 12'h33F

  // debug / trace registers (shared with debug mode)
  //////////////////////////////////////////////////////////////////////////////
  // tselect 12'h7A0
  // tdata1 12'h7A1
  // tdata2 12'h7A2
  // tdata3 12'h7A3

  // debug mode registers
  //////////////////////////////////////////////////////////////////////////////
  // dcsr 12'h7B0
  // dpc 12'h7B1
  // dscratch 12'h7B2

  `ifdef SUPERVISOR_MODE
  // supervisor trap setup
  //////////////////////////////////////////////////////////////////////////////
  // sstatus 12'h100 -- S-view of mstatus
  csrs.sedeleg <- mkRegUndef("sedeleg");
  csrs.sideleg <- mkRegUndef("sideleg");
  if (static_HAS_U_MODE && static_HAS_N_EXT) begin
    csrs.sedeleg <- mkReg(defaultValue); // sedeleg 12'h102
    csrs.sideleg <- mkReg(defaultValue); // sideleg 12'h103
  end
  // sie 12'h104 -- S-view of mie
  csrs.stvec   <- mkReg(defaultValue); // stvec 12'h105
  // TODO scounteren 12'h106

  // supervisor trap handling
  //////////////////////////////////////////////////////////////////////////////
  csrs.sscratch <- mkRegU; // sscratch 12'h140
  csrs.sepc     <- mkReg(defaultValue); // sepc 12'h141
  csrs.scause   <- mkRegU; // scause 12'h142
  csrs.stval    <- mkRegU; // stval 12'h143
  // sip 12'h144 -- S-view of mip

  // supervisor protection and translation
  //////////////////////////////////////////////////////////////////////////////
  csrs.satp     <- mkReg(defaultValue); // satp 12h'180
  `endif

  // user trap setup registers
  //////////////////////////////////////////////////////////////////////////////
  // ustatus 12'h000
  // uie 12'h004
  // utvec 12'h005

  // user trap handling
  //////////////////////////////////////////////////////////////////////////////
  // uscratch 12'h040
  // uepc 12'h041
  // ucause 12'h042
  // utval 12'h043
  // uip 12'h044

  // user counters/timers
  //////////////////////////////////////////////////////////////////////////////
  csrs.cycle <- mkReg(0); // cycle 12'hC00 (and 12'hC80 in RV32)
  rule cycle_count;
    csrs.cycle <= csrs.cycle + 1;
  endrule
  // time 12'hC01 (and 12'hC81 in RV32)
  // csrs.instret <- mkCommittedInstCnt; // insret 12'hC02 (and 12'hC82 in RV32)
  // hpmcounter3 12'hC03 (and 12'hC83 in RV32)
  // hpmcounter4 12'hC04 (and 12'hC84 in RV32)
  // ...
  // hpmcounter31 12'hC1F (and 12'hC9F in RV32)

  // XXX for debug purposes:
  csrs.ctrl <- mkReg(0); // ctrl 12'hCC0

  // CSR requests
  function ActionValue#(Bit#(XLEN)) readUpdateCSR(Reg#(csr_t) csr, CSRReq r)
    provisos (FShow#(csr_t), Bits#(csr_t, XLEN),
              LegalizeRead#(csr_t), LegalizeWrite#(csr_t)) = actionvalue
    csr_t tmpval = legalizeRead(csr);
    Bit#(XLEN) retval = pack(tmpval);
    if (r.rEffects != NOWRITE) begin
      csr_t newval = ?;
      case (r.rType)
        RW: newval = unpack(r.val);
        RS: newval = unpack(pack(csr) | r.val);
        RC: newval = unpack(pack(csr) & ~r.val);
      endcase
      csr_t newcsr = legalizeWrite(pack(csr), newval);
      csr <= newcsr;
      printTLogPlusArgs("CSRs", $format(fshow(csr) + $format(" -> ") + fshow(newcsr)));
    end else printTLogPlusArgs("CSRs", $format("reading value 0x%0x from CSR", retval));
    return retval;
  endactionvalue;
  function ActionValue#(csr_t0) readUpdateMultiViewCSR(Reg#(csr_t1) csr, CSRReq r)
    provisos(FShow#(csr_t1), Bits#(csr_t1, XLEN), Bits#(csr_t0, XLEN),
             Lower#(csr_t1, csr_t0), Lift#(csr_t0, csr_t1)) = actionvalue
    csr_t0 retval = lower(csr);
    if (r.rEffects != NOWRITE) begin
      csr_t0 newval = ?;
      case (r.rType)
        RW: newval = unpack(r.val);
        RS: newval = unpack(pack(csr) | r.val);
        RC: newval = unpack(pack(csr) & ~r.val);
      endcase
      csr_t1 newcsr = lift(pack(csr), newval);
      csr <= newcsr;
      printTLogPlusArgs("CSRs", $format(fshow(csr) + $format(" -> ") + fshow(newcsr)));
    end else printTLogPlusArgs("CSRs", $format("reading value 0x%0x from CSR", retval));
    return retval;
  endactionvalue;
  function ActionValue#(Bit#(XLEN)) req (CSRReq r) = actionvalue
    Bit#(XLEN) ret = ?;
    `define CSRUpdate(x) ret <- readUpdateCSR(x,r);
    `define MVCSRUpdate(x, y) begin x tmp <- readUpdateMultiViewCSR(y,r); ret = pack(tmp); end
    case (r.idx) matches// TODO sort out individual behaviours for each CSR
      `ifdef SUPERVISOR_MODE
      12'h100: `MVCSRUpdate(SStatus, csrs.mstatus)
      12'h102 &&& (static_HAS_U_MODE && static_HAS_N_EXT):
        `CSRUpdate(csrs.sedeleg)
      12'h103 &&& (static_HAS_U_MODE && static_HAS_N_EXT):
        `CSRUpdate(csrs.sideleg)
      12'h104: `MVCSRUpdate(SIE, csrs.mie)
      12'h105: `CSRUpdate(csrs.stvec)
      12'h140: `CSRUpdate(csrs.sscratch)
      12'h141: `CSRUpdate(csrs.sepc)
      12'h142: `CSRUpdate(csrs.scause)
      12'h143: `CSRUpdate(csrs.stval)
      12'h144: `MVCSRUpdate(SIP, csrs.mip)
      12'h180: `CSRUpdate(csrs.satp)
      `endif
      12'h300: `MVCSRUpdate(MStatus, csrs.mstatus)
      12'h301: `CSRUpdate(csrs.misa)
      12'h302 &&& (static_HAS_S_MODE || (static_HAS_U_MODE && static_HAS_N_EXT)):
        `CSRUpdate(csrs.medeleg)
      12'h303 &&& (static_HAS_S_MODE || (static_HAS_U_MODE && static_HAS_N_EXT)):
        `CSRUpdate(csrs.mideleg)
      12'h304: `MVCSRUpdate(MIE, csrs.mie)
      12'h305: `CSRUpdate(csrs.mtvec)
      12'h340: `CSRUpdate(csrs.mscratch)
      12'h341: `CSRUpdate(csrs.mepc)
      12'h342: `CSRUpdate(csrs.mcause)
      12'h343: `CSRUpdate(csrs.mtval)
      12'h344: `MVCSRUpdate(MIP, csrs.mip)
      `ifdef PMP
      `ifdef XLEN64
      12'h3A0: `CSRUpdate(csrs.pmpcfg[0])
      12'h3A2: `CSRUpdate(csrs.pmpcfg[1])
      `else
      .x &&& (12'h3A0 >= x && x <= 12'h3A3):
        `CSRUpdate(csrs.pmpcfg[x - 12'h3A0])
      `endif
      .x &&& (12'h3B0 >= x && x <= 12'h3BF):
        `CSRUpdate(csrs.pmpaddr[x - 12'h3B0])
      `endif
      12'hF11: `CSRUpdate(csrs.mvendorid)
      12'hF12: `CSRUpdate(csrs.marchid)
      12'hF13: `CSRUpdate(csrs.mimpid)
      12'hF14: `CSRUpdate(csrs.mhartid)
      12'hC00: ret = csrs.cycle[valueOf(XLEN)-1:0];
      //12'hC02: ret = csrs.instret[valueOf(XLEN)-1:0];
      // RV32I only
      //'hC80: ret = cycle[63:32];
      //XXX hack for test suite
      12'hCC0: begin // test success
        csrs.ctrl[7:0] <= r.val[7:0];
        if (genC) begin $display("TEST SUCCESS"); $finish(0); end
      end
      12'hCC1: if (genC) begin $display("TEST FAILURE"); $finish(0); end // test failure
      default: begin
        ret = ?;
        printLog($format("CSR 0x%0x unimplemented - ", r.idx, fshow(r)));
      end
    endcase
    return ret;
  endactionvalue;
  csrs.req = req;

  return csrs;

endmodule
