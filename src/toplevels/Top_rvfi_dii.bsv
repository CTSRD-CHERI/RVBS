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

import        FIFOF :: *;
import SpecialFIFOs :: *;

import     RVFI_DII :: *;
import       Recipe :: *;
import   BlueBasics :: *;
import    BlueUtils :: *;
import          BID :: *;
import         RVBS :: *;
import    RVBS_Core :: *;
`ifdef RVXCHERI
import     CHERICap :: *;
import      CHERICC :: *;
`endif

(* synthesize *)
module mkRVBS_rvfi_dii (Empty);

  // create the RVFI-DII bridge
  let bridge <- mkRVFI_DII_Bridge("RVFI_DII", 5000);
  // create a memory
  module memShim (Array#(RVMem));
    `ifdef RVXCHERI
    Mem#(PAddr, Bit#(1)) mem_tag[2] <- mkMemSimWithOffset(2, 'h80000000, 'h10000, Invalid);
    `endif
    Mem#(PAddr, Bit#(128)) mem[2] <- mkMemSimWithOffset(2, 'h80000000, 'h10000, Invalid);
    // 2 memory interfaces
    RVMem m[2];
    for (Integer i = 0; i < 2; i = i + 1) begin
      let   rspFF <- mkBypassFIFOF;
      let errorFF <- mkFIFOF;
      let excFF <- mkFIFOF;
      // get responses
      rule drainMemRsp(!excFF.notEmpty && !errorFF.first);
        let rsp <- get(mem[i].source);
        `ifdef RVXCHERI
        let tag <- get(mem_tag[i].source);
        `endif
        case (rsp) matches
          tagged ReadRsp .data: rspFF.enq(Right(RVReadRsp(
            `ifdef RVXCHERI
            tuple2(tag.ReadRsp, data)
            `else
            data
            `endif
          )));
          tagged WriteRsp: rspFF.enq(Right(RVWriteRsp));
        endcase
        errorFF.deq;
      endrule
      rule errorRsp(!excFF.notEmpty && errorFF.first);
        rspFF.enq(Right(RVBusError));
        errorFF.deq;
      endrule
      rule exceptionRsp(excFF.notEmpty);
        rspFF.enq(Left(excFF.first));
        excFF.deq;
      endrule
      // convert requests/responses
      m[i] = interface RVMem;
        interface sink = interface Sink;
          method canPut = errorFF.notFull;
          method put (e_req) = action
            case (e_req) matches
              tagged Left .excTok: excFF.enq(excTok);
              tagged Right .req: case (req) matches
                tagged RVReadReq .r &&& (r.addr >= 'h80000000 && r.addr < 'h80010000): begin
                  mem[i].sink.put(ReadReq{addr: r.addr, numBytes: r.numBytes});
                  `ifdef RVXCHERI
                  mem_tag[i].sink.put(ReadReq{addr: r.addr, numBytes: 1});
                  `endif
                  errorFF.enq(False);
                end
                tagged RVWriteReq .w &&& (w.addr >= 'h80000000 && w.addr < 'h80010000): begin
                  mem[i].sink.put(WriteReq{
                    addr: w.addr, byteEnable: w.byteEnable, data: w.data
                  });
                  `ifdef RVXCHERI
                  mem_tag[i].sink.put(WriteReq{
                    addr: w.addr, byteEnable: 1, data: w.captag
                  });
                  `endif
                  errorFF.enq(False);
                end
                default: errorFF.enq(True);
              endcase
            endcase
          endaction;
        endinterface;
        interface source = toSource(rspFF);
      endinterface;
    end
    return m;
  endmodule
  RVMem mem[2] <- memShim(reset_by bridge.new_rst);
  // prepare state
  `ifdef SUPERVISOR_MODE
  RVMem imem[2] <- virtualize(mem[0], 2, reset_by bridge.new_rst);
  RVMem dmem[2] <- virtualize(mem[1], 2, reset_by bridge.new_rst);
  RVState s <- mkState(?, imem[1], dmem[1], imem[0], dmem[0], bridge, reset_by bridge.new_rst);
  `else
  RVState s <- mkState(?, mem[0], mem[1], bridge, reset_by bridge.new_rst);
  `endif
  // initialization
  module [ISADefModule] mkRVInit#(RVState st) (Empty);
    Reg#(Bit#(6)) cnt <- mkRegU;
    defineInitEntry(rSeq(rBlock(
      printTLogPlusArgs("itrace", "-------- Reseting --------"),
      action s.pc <= 'h80000000; endaction, action s.pc.commit; endaction,
      `ifdef RVXCHERI
      action RawCap c0 = nullCap; s.wCR(0, Data(pack(c0))); endaction,
      s.regFile.commit,
      writeReg(cnt, 1),
      rWhile(cnt < 32, rFastSeq(rBlock(
        action RawCap c0 = nullCap; s.wCR(truncate(cnt), Data(pack(c0))); endaction,
        action
          s.regFile.commit;
          cnt <= cnt + 1;
        endaction
      )))
      `else
      writeReg(cnt, 0),
      rWhile(cnt < 32, rFastSeq(rBlock(
        s.wGPR(truncate(cnt), 0),
        action
          s.regFile.commit;
          cnt <= cnt + 1;
        endaction
      )))
      `endif
    )));
  endmodule
  // instanciating simulator
  let _ <- mkRVBSCore(s, mkRVInit, mkRVIFetch_RVFI_DII, reset_by bridge.new_rst);

endmodule
