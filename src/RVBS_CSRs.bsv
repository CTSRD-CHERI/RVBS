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
import ConfigReg :: *;
import Vector :: *;

import BlueUtils :: *;
import RVBS_Types :: *;
`ifdef PMP
import RVBS_PMPTypes :: *;
`endif

//////////////////////////////
// CSR_Ifc's implementation //
//////////////////////////////

module mkCSRCore#(Integer m, Integer n, module#(Reg#(csr_t)) mkTheCSR)(CSR_Ifc#(csr_t))
  provisos (Bits#(csr_t, csr_sz));
  // internal module decalration
  Reg#(csr_t)        r <- mkTheCSR;
  Wire#(csr_t)  pre[m] <- mkDCWire(m, r._read);
  Wire#(csr_t) inst[2] <- mkDCWire(2, (m > 0) ? pre[m-1]._read : r._read);
  Wire#(csr_t) post[n] <- mkDCWire(n, inst[1]._read);
  RWire#(csr_t)   last <- mkRWire;

  // update the register
  rule update_reg; case (last.wget) matches
    tagged Valid .newVal: r <= newVal;
    tagged Invalid: r <= (n > 0) ? post[n-1]._read : inst[1]._read;
  endcase endrule

  // populating interfaces
  interface  preInstView = pre;
  interface     instView = interface Reg;
    method _read  = inst[0]._read;
    method _write = inst[1]._write;
  endinterface;
  interface postInstView = post;
  method  _read = r._read;
  method _write = last.wset;
endmodule

module mkCSR#(csr_t dflt)(CSR_Ifc#(csr_t)) provisos (Bits#(csr_t, csr_sz));
  let ifc <- mkCSRCore(0,0,mkConfigReg(dflt));
  return ifc;
endmodule
module mkCSRU(CSR_Ifc#(csr_t)) provisos (Bits#(csr_t, csr_sz));
  let ifc <- mkCSRCore(0,0,mkConfigRegU);
  return ifc;
endmodule
module mkCSRUndef#(String name)(CSR_Ifc#(csr_t)) provisos (Bits#(csr_t, csr_sz));
  Reg#(csr_t) r <- mkRegUndef(name);
  interface  preInstView = ?;
  interface     instView = ?;
  interface postInstView = ?;
  method  _read = r._read;
  method _write = r._write;
endmodule

//////////////////////////
// CSRs' implementation //
////////////////////////////////////////////////////////////////////////////////
module mkCSRs(CSRs);

  // instance of the CSRs struct
  CSRs csrs;

  // machine information registers
  //////////////////////////////////////////////////////////////////////////////
  csrs.mvendorid <- mkCSR(defaultValue); // mvendorid 12'hF11
  csrs.marchid   <- mkCSR(0); // marchid 12'hF12
  csrs.mimpid    <- mkCSR(0); // mimpid 12'hF13
  csrs.mhartid   <- mkCSR(0); // mhartid 12'hF14

  // machine trap setup registers
  //////////////////////////////////////////////////////////////////////////////
  csrs.mstatus <- mkCSR(defaultValue); // mstatus 12'h300
  csrs.misa    <- mkCSR(defaultValue); // misa 12'h301
  csrs.medeleg <- mkCSRUndef("medeleg");
  csrs.mideleg <- mkCSRUndef("mideleg");
  if (static_HAS_S_MODE || (static_HAS_U_MODE && static_HAS_N_EXT)) begin
    csrs.medeleg <- mkCSR(defaultValue); // medeleg 12'h302
    csrs.mideleg <- mkCSR(defaultValue); // mideleg 12'h303
  end
  csrs.mie     <- mkCSR(defaultValue); // mie 12'h304
  csrs.mtvec   <- mkCSR(defaultValue); // mtvec 12'h305
  // mcounteren 12'h306

  // machine trap handling
  //////////////////////////////////////////////////////////////////////////////
  csrs.mscratch <- mkCSRU; // mscratch 12'h340
  csrs.mepc     <- mkCSR(defaultValue); // mepc 12'h341
  csrs.mcause   <- mkCSRU; // mcause 12'h342
  csrs.mtval    <- mkCSRU; // mtval 12'h343
  csrs.mip      <- mkCSRCore(1, 0, mkConfigReg(defaultValue)); // mip 12'h344

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
  csrs.pmpcfg  <- replicateM(mkPMPCfgIfcReg);
  csrs.pmpaddr <- replicateM(mkConfigReg(defaultValue));
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
    csrs.sedeleg <- mkCSR(defaultValue); // sedeleg 12'h102
    csrs.sideleg <- mkCSR(defaultValue); // sideleg 12'h103
  end
  // sie 12'h104 -- S-view of mie
  csrs.stvec   <- mkCSR(defaultValue); // stvec 12'h105
  // TODO scounteren 12'h106

  // supervisor trap handling
  //////////////////////////////////////////////////////////////////////////////
  csrs.sscratch <- mkCSRU; // sscratch 12'h140
  csrs.sepc     <- mkCSR(defaultValue); // sepc 12'h141
  csrs.scause   <- mkCSRU; // scause 12'h142
  csrs.stval    <- mkCSRU; // stval 12'h143
  // sip 12'h144 -- S-view of mip

  // supervisor protection and translation
  //////////////////////////////////////////////////////////////////////////////
  csrs.satp <- mkCSR(defaultValue); // satp 12h'180
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
  csrs.cycle <- mkCSR(0); // cycle 12'hC00 (and 12'hC80 in RV32)
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
  csrs.ctrl <- mkCSR(0); // ctrl 12'hCC0

  // IRQs
  Wire#(Bool) msip_w <- mkDWire(csrs.mip.msip);
  Wire#(Bool) mtip_w <- mkDWire(csrs.mip.mtip);
  Wire#(Bool) meip_w <- mkDWire(csrs.mip.meip);
  function doSetMSIP(irq) = action msip_w <= irq; endaction;
  csrs.setMSIP = doSetMSIP;
  function doSetMTIP(irq) = action mtip_w <= irq; endaction;
  csrs.setMTIP = doSetMTIP;
  function doSetMEIP(irq) = action meip_w <= irq; endaction;
  csrs.setMEIP = doSetMEIP;
  rule setIRQs;
    let new_mip = csrs.mip;
    new_mip.msip = msip_w;
    new_mip.mtip = mtip_w;
    new_mip.meip = meip_w;
    asReg(csrs.mip.preInstView[0]) <= new_mip;
  endrule

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
      //printTLogPlusArgs("CSRs", $format("newval before legalize: 0x%0x", newval));
      csr_t newcsr = legalizeWrite(pack(csr), newval);
      //printTLogPlusArgs("CSRs", $format("newval after legalize: 0x%0x", pack(newcsr)));
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
      12'h100: `MVCSRUpdate(SStatus, csrs.mstatus.instView)
      12'h102 &&& (static_HAS_U_MODE && static_HAS_N_EXT):
        `CSRUpdate(csrs.sedeleg.instView)
      12'h103 &&& (static_HAS_U_MODE && static_HAS_N_EXT):
        `CSRUpdate(csrs.sideleg.instView)
      12'h104: `MVCSRUpdate(SIE, csrs.mie.instView)
      12'h105: `CSRUpdate(csrs.stvec.instView)
      12'h140: `CSRUpdate(csrs.sscratch.instView)
      12'h141: `CSRUpdate(csrs.sepc.instView)
      12'h142: `CSRUpdate(csrs.scause.instView)
      12'h143: `CSRUpdate(csrs.stval.instView)
      12'h144: `MVCSRUpdate(SIP, csrs.mip.instView)
      12'h180: `CSRUpdate(csrs.satp.instView)
      `endif
      12'h300: `MVCSRUpdate(MStatus, csrs.mstatus.instView)
      12'h301: `CSRUpdate(csrs.misa.instView)
      12'h302 &&& (static_HAS_S_MODE || (static_HAS_U_MODE && static_HAS_N_EXT)):
        `CSRUpdate(csrs.medeleg.instView)
      12'h303 &&& (static_HAS_S_MODE || (static_HAS_U_MODE && static_HAS_N_EXT)):
        `CSRUpdate(csrs.mideleg.instView)
      12'h304: `MVCSRUpdate(MIE, csrs.mie.instView)
      12'h305: `CSRUpdate(csrs.mtvec.instView)
      12'h340: `CSRUpdate(csrs.mscratch.instView)
      12'h341: `CSRUpdate(csrs.mepc.instView)
      12'h342: `CSRUpdate(csrs.mcause.instView)
      12'h343: `CSRUpdate(csrs.mtval.instView)
      12'h344: `MVCSRUpdate(MIP, csrs.mip.instView)
      `ifdef PMP
      `ifdef XLEN64
      12'h3A0: `CSRUpdate(csrs.pmpcfg[0].instView)
      12'h3A2: `CSRUpdate(csrs.pmpcfg[1].instView)
      `else
      .x &&& (12'h3A0 >= x && x <= 12'h3A3):
        `CSRUpdate(csrs.pmpcfg[x - 12'h3A0].instView)
      `endif
      .x &&& (12'h3B0 >= x && x <= 12'h3BF):
        `CSRUpdate(csrs.pmpaddr[x - 12'h3B0].instView)
      `endif
      12'hF11: `CSRUpdate(csrs.mvendorid.instView)
      12'hF12: `CSRUpdate(csrs.marchid.instView)
      12'hF13: `CSRUpdate(csrs.mimpid.instView)
      12'hF14: `CSRUpdate(csrs.mhartid.instView)
      12'hC00: ret = csrs.cycle.instView[valueOf(XLEN)-1:0];
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
