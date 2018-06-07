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
  Bit#(n) val;
  CSRReqType rType;
  CSRReqEffects rEffects;
} CSRReq#(numeric type n) deriving (FShow);

instance DefaultValue#(CSRReq#(n));
  function CSRReq#(n) defaultValue =
    CSRReq { idx: ?, val: ?, rType: RW, rEffects: ALL };
endinstance
function CSRReq#(n) rwCSRReq(Bit#(12) i, Bit#(n) v) =
  CSRReq { idx: i, val: v, rType: RW, rEffects: ALL };
function CSRReq#(n) rwCSRReqNoRead(Bit#(12) i, Bit#(n) v) =
  CSRReq { idx: i, val: v, rType: RW, rEffects: NOREAD };
function CSRReq#(n) rsCSRReq(Bit#(12) i, Bit#(n) v) =
  CSRReq { idx: i, val: v, rType: RS, rEffects: ALL };
function CSRReq#(n) rsCSRReqNoWrite(Bit#(12) i, Bit#(n) v) =
  CSRReq { idx: i, val: v, rType: RS, rEffects: NOWRITE };
function CSRReq#(n) rcCSRReq(Bit#(12) i, Bit#(n) v) =
  CSRReq { idx: i, val: v, rType: RC, rEffects: ALL };
function CSRReq#(n) rcCSRReqNoWrite(Bit#(12) i, Bit#(n) v) =
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
  Reg#(EDeleg)     medeleg;
  Reg#(IDeleg)     mideleg;
  Reg#(IE)         mie;
  Reg#(TVec)       mtvec;
  // mcounteren

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

  // supervisor trap setup
  //////////////////////////////////////////////////////////////////////////////
  // sstatus;
  // sedeleg
  // sideleg
  // sie
  // stvec;
  // scounteren

  // supervisor trap handling
  //////////////////////////////////////////////////////////////////////////////
  // sscratch;
  // sepc;
  // scause;
  // stval;
  // sip

  // supervisor protection and translation
  //////////////////////////////////////////////////////////////////////////////
  // satp

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
  function ActionValue#(Bit#(XLEN)) doReq (CSRReq#(XLEN) r) req;

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
module mkCSRs#(PrivLvl currLvl
`ifdef PMP
,PMP pmp
`endif
)(CSRs);

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
  function ActionValue#(csr_t) readUpdateCSR(Reg#(csr_t) csr, CSRReq#(XLEN) r)
    provisos (Bits#(csr_t, XLEN)) = actionvalue
    csr_t retval = csr;
    if (r.rEffects != NOWRITE) begin
      csr_t newval = ?;
      case (r.rType)
        RW: newval = unpack(r.val);
        RS: newval = unpack(pack(csr) | r.val);
        RC: newval = unpack(pack(csr) & ~r.val);
      endcase
      csr <= newval;
      printTLogPlusArgs("CSRs", $format("overwriting CSR old value 0x%0x with new value 0x%0x", pack(csr), newval));
    end else printTLogPlusArgs("CSRs", $format("reading value 0x%0x from CSR", retval));
    return retval;
  endactionvalue;
  function ActionValue#(csr_t0) readUpdateMultiViewCSR(Reg#(csr_t1) csr, CSRReq#(XLEN) r)
    provisos(Bits#(csr_t1, XLEN), Bits#(csr_t0, XLEN),
             Lower#(csr_t1, csr_t0), Lift#(csr_t0, csr_t1)) = actionvalue
    csr_t0 retval = lower(csr);
    if (r.rEffects != NOWRITE) begin
      csr_t0 newval = ?;
      case (r.rType)
        RW: newval = unpack(r.val);
        RS: newval = unpack(pack(csr) | r.val);
        RC: newval = unpack(pack(csr) & ~r.val);
      endcase
      csr <= lift(pack(csr), newval, currLvl);
      printTLogPlusArgs("CSRs", $format("overwriting CSR old value 0x%0x with new value 0x%0x", pack(csr), newval));
    end else printTLogPlusArgs("CSRs", $format("reading value 0x%0x from CSR", retval));
    return retval;
  endactionvalue;
  function ActionValue#(Bit#(XLEN)) req (CSRReq#(XLEN) r) = actionvalue
    Bit#(XLEN) ret = ?;
    `define CSRUpdate(x, y) begin x tmp <- readUpdateCSR(y,r); ret = pack(tmp); end
    `define MVCSRUpdate(x, y) begin x tmp <- readUpdateMultiViewCSR(y,r); ret = pack(tmp); end
    case (r.idx) matches// TODO sort out individual behaviours for each CSR
      `ifdef SUPERVISOR_MODE
      12'h100: `MVCSRUpdate(SStatus, csrs.mstatus)
      `endif
      12'h300: `MVCSRUpdate(MStatus, csrs.mstatus)
      12'h301: `MVCSRUpdate(MISA, csrs.misa)
      12'h302 &&& (static_HAS_S_MODE || (static_HAS_U_MODE && static_HAS_N_EXT)):
        `MVCSRUpdate(MEDeleg, csrs.medeleg)
      12'h303 &&& (static_HAS_S_MODE || (static_HAS_U_MODE && static_HAS_N_EXT)):
        `MVCSRUpdate(MIDeleg, csrs.mideleg)
      12'h304: `MVCSRUpdate(MIE, csrs.mie)
      12'h305: `MVCSRUpdate(MTVec, csrs.mtvec)
      12'h340: `CSRUpdate(Bit#(XLEN), csrs.mscratch)
      12'h341: `MVCSRUpdate(MEPC, csrs.mepc)
      12'h342: `MVCSRUpdate(MCause, csrs.mcause)
      12'h343: `CSRUpdate(Bit#(XLEN), csrs.mtval)
      12'h344: `MVCSRUpdate(MIP, csrs.mip)
      `ifdef PMP
      `ifdef XLEN64
      12'h3A0: `CSRUpdate(Vector#(8, PMPCfg), csrs.pmpcfg[0])
      12'h3A2: `CSRUpdate(Vector#(8, PMPCfg), csrs.pmpcfg[1])
      `else
      .x &&& (12'h3A0 >= x && x <= 12'h3A3):
        `CSRUpdate(Vector#(4, PMPCfg), csrs.pmpcfg[x - 12'h3A0])
      `endif
      .x &&& (12'h3B0 >= x && x <= 12'h3BF):
        `CSRUpdate(PMPAddr, csrs.pmpaddr[x - 12'h3B0])
      `endif
      12'hF11: `MVCSRUpdate(MVendorID, csrs.mvendorid)
      12'hF12: `CSRUpdate(Bit#(XLEN), csrs.marchid)
      12'hF13: `CSRUpdate(Bit#(XLEN), csrs.mimpid)
      12'hF14: `CSRUpdate(Bit#(XLEN), csrs.mhartid)
      12'hC00: ret = csrs.cycle[valueOf(XLEN)-1:0];
      //12'hC02: ret = csrs.instret[valueOf(XLEN)-1:0];
      // RV32I only
      //'hC80: ret = cycle[63:32];
      //XXX hack for test suite
      12'hCC0: begin // test success
        csrs.ctrl[7:0] <= r.val[7:0];
        if (genC) begin
          $display("TEST SUCCESS");
          $finish(0);
        end
      end
      12'hCC1: begin // test failure
        $display("TEST FAILURE");
        $finish(0);
      end
      default: begin
        ret = ?;
        printLog($format("CSR 0x%0x unimplemented - ", r.idx, fshow(r)));
      end
    endcase
    `undef CSRUpdate
    `undef MVCSRUpdate
    return ret;
  endactionvalue;
  csrs.req = req;

  return csrs;

endmodule
