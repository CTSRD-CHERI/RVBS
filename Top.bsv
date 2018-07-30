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
import ClientServer :: *;
import GetPut :: *;
import Connectable :: *;
import List :: *;
import LFSR :: *;

import BlueUtils :: *;
import AXI :: *;
import TopAXI :: *;

`ifdef XLEN64
typedef 56 ADDR_sz;
typedef 64 DATA_sz;
`else
typedef 34 ADDR_sz;
typedef 32 DATA_sz;
`endif

// memory subsystem module
(* always_ready *)
interface RVBS_Mem_Slave;
  interface AXILiteSlave#(ADDR_sz, DATA_sz) axiLiteSlave0;
  interface AXILiteSlave#(ADDR_sz, DATA_sz) axiLiteSlave1;
endinterface
instance Connectable#(RVBS_Ifc, RVBS_Mem_Slave);
  module mkConnection#(RVBS_Ifc m, RVBS_Mem_Slave s) (Empty);
    mkConnection(m.axiLiteMaster0, s.axiLiteSlave0);
    mkConnection(m.axiLiteMaster1, s.axiLiteSlave1);
  endmodule
endinstance
module mem (RVBS_Mem_Slave);

  `ifdef MEM_IMG
  String memimg = `MEM_IMG;
  `else
  String memimg = "test-prog.hex";
  `endif
  `ifdef MEM_SIZE
  Integer memsize = `MEM_SIZE;
  `else
  Integer memsize = 'h10000;
  `endif
  Mem2#(Bit#(ADDR_sz), Bit#(DATA_sz), Bit#(DATA_sz)) mem <- mkSharedMem2(memsize, memimg);

  function MemReq#(Bit#(ADDR_sz), Bit#(DATA_sz))
    fromAXILiteToWriteReq(AWLiteFlit#(ADDR_sz) aw, WLiteFlit#(DATA_sz) w) =
      WriteReq {addr: aw.awaddr, byteEnable: w.wstrb, data: w.wdata};
  function MemReq#(Bit#(ADDR_sz), Bit#(DATA_sz))
    fromAXILiteToReadReq(ARLiteFlit#(ADDR_sz) ar) =
      ReadReq {addr: ar.araddr, numBytes: fromInteger(valueOf(DATA_sz)/8)};
  AXILiteSlave#(ADDR_sz, DATA_sz) ifc[2];

  for (Integer i = 0; i < 2; i = i + 1) begin

    Bool canRsp;
    `ifdef MEM_DELAY
    // artificial delay
    Reg#(Bool) seeded <- mkReg(False);
    Reg#(Bit#(5)) delay_count <- mkReg(0);
    let delay_cmp <- mkLFSR_16;
    rule init_delay (!seeded); delay_cmp.seed('h11); seeded <= True; endrule
    let delayff <- mkFIFOF;
    canRsp = seeded;
    `else
    canRsp = True;
    `endif
    // forward requests/response from/to appropriate FIFOF
    let p = (i == 0) ? mem.p0 : mem.p1;
    let awff <- mkBypassFIFOF;
    let wff  <- mkBypassFIFOF;
    let arff <- mkBypassFIFOF;
    let rff  <- mkBypassFIFOF;
    rule writeReq;
      p.request.put(fromAXILiteToWriteReq(awff.first, wff.first));
      awff.deq;
      wff.deq;
    endrule
    rule readReq;
      p.request.put(fromAXILiteToReadReq(arff.first));
      arff.deq;
    endrule
    rule readRsp(canRsp);
      let rsp <- p.response.get;
    `ifndef MEM_DELAY
      rff.enq(RLiteFlit{rdata: rsp.ReadRsp, rresp: 00});
    endrule
    `else
      delayff.enq(tuple2(rsp, delay_cmp.value[15:11]));
      delay_cmp.next;
    endrule
    rule delayedReadRsp;
      match {.rsp, .d} = delayff.first;
      if (delay_count >= d) begin
        delay_count <= 0;
        delayff.deq;
        rff.enq(RLiteFlit{rdata: rsp.ReadRsp, rresp: 00});
      end else delay_count <= delay_count + 1;
    endrule
    `endif

    // wire up interface
    let awifc <- toAXIAWLiteSlave(awff);
    let wifc  <- toAXIWLiteSlave(wff);
    let bifc  <- mkNullSource;
    let arifc <- toAXIARLiteSlave(arff);
    let rifc  <- toAXIRLiteSlave(rff);
    ifc[i] = interface AXILiteSlave;
      interface aw = awifc;
      interface w  = wifc;
      interface b  = bifc;
      interface ar = arifc;
      interface r  = rifc;
    endinterface;

  end

  interface axiLiteSlave0 = ifc[0];
  interface axiLiteSlave1 = ifc[1];

endmodule

module top (Empty);
  // RESET PC
  Bit#(DATA_sz) reset_pc = 0;
  // RVBS instance
  let master0 <- rvbs(reset_pc);
  // MEM instance
  let slave0  <- mem;
  // plug things in
  mkConnection(master0, slave0);
endmodule
