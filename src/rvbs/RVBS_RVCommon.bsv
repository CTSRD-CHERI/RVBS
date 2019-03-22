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

import BID :: *;
import BlueBasics :: *;
import BlueUtils :: *;
import BitPat :: *;
import Recipe :: *;
import RVBS_Trap :: *;
import RVBS_Types :: *;
import RVBS_MemAccess :: *;

`ifdef RVFI_DII
import RVFI_DII_Bridge :: *;
import FIFO :: *;
import ClientServer :: *;
import GetPut :: *;
`endif

//XXX TEMPORARY- FIXME XXX//
`ifdef RVXCHERI
`ifdef XLEN64
import CHERICap :: *;
`endif
`endif
//XXX TEMPORARY- FIXME XXX//

module [ISADefModule] mkRVCommon#(RVState s) (Empty);

  // instanciate memory access state machines
  match {.dataReadRules, .dataReadFSM} <- mkRecipeFSMSlaveRules(doReadMemCore(
    `ifdef SUPERVISOR_MODE
    s.dvm,
    `endif
    `ifdef PMP
    s.dpmp,
    `endif
    s.dmem
  ));
  match {.dataWriteRules, .dataWriteFSM} <- mkRecipeFSMSlaveRules(
    doWriteMemCore(
      `ifdef SUPERVISOR_MODE
      s.dvm,
      `endif
      `ifdef PMP
      s.dpmp,
      `endif
      s.dmem
  ));
  addRules(rJoinMutuallyExclusive(dataReadRules, dataWriteRules));

  // Memory commons
  `ifdef RVXCHERI
  match {.rHandle, .rDest, .rNumBytes, .rSgnExt, .rCapAccess} = s.readMem.first;
  match {.rCapIdx, .rCap, .rVaddr} = unpackHandle(s.ddc, s.pcc, rHandle);
  match {.wHandle, .wNumBytes, .wData, .wCapAccess} = s.writeMem.first;
  match {.wCapIdx, .wCap, .wVaddr} = unpackHandle(s.ddc, s.pcc, wHandle);
  `else
  match {.rVaddr, .rDest, .rNumBytes, .rSgnExt} = s.readMem.first;
  match {.wVaddr, .wNumBytes, .wData} = s.writeMem.first;
  `endif
  // call back for read responses
  function readCallBack(rsp) = action
    case (rsp) matches
      tagged Left .excTok: raiseMemTokException(s, excTok);
      tagged Right .memRsp: case (memRsp) matches
        tagged RVReadRsp .r: begin
          `ifdef RVXCHERI
          match {.captag, .data} = r;
          Bit#(CapNoValidSz) capData = truncate(data);
          //XXX TEMPORARY- FIXME XXX//
          `ifdef XLEN64
          CapType newCap = CHERICap::cast({captag, capData});
          `else
          CapType newCap = unpack({captag, capData});
          `endif
          //XXX TEMPORARY- FIXME XXX//
          `else
          let data = r;
          `endif
          let topIdx = {readBitPO(rNumBytes), 3'b000};
          Bool isNeg = unpack(data[topIdx-1]);
          Bit#(XLEN) mask = (~0) << topIdx;
          `ifdef RVXCHERI
          if (rCapAccess) s.wCR(rDest, newCap);
          else
          `endif
          s.wGPR(rDest, (rSgnExt && isNeg) ? truncate(data) | mask : truncate(data) & ~mask);
        end
        tagged RVBusError: action raiseMemException(s, LoadAccessFault); endaction
      endcase
    endcase
  endaction;
  // call back for write responses
  function writeCallBack(rsp) = action
    case (rsp) matches
      tagged Left .excTok: raiseMemTokException(s, excTok);
      tagged Right .memRsp: case (memRsp) matches
        tagged RVWriteRsp .w: noAction;
        tagged RVBusError: action raiseMemException(s, StrAMOAccessFault); endaction
      endcase
    endcase
  endaction;
  // prepare exception tokens
  let rExcTok = Invalid;
  let wExcTok = Invalid;
  `ifdef RVXCHERI
  // check for potential capability exceptions
  let m_rCapExc = memCapChecks(READ, rCap, rVaddr, rNumBytes, rCapAccess);
  if (isValid(m_rCapExc)) rExcTok = Valid(ExcToken{
    excCode: CHERIFault,
    capExcCode: m_rCapExc.Valid,
    capIdx: rCapIdx
  });
  let m_wCapExc = memCapChecks(WRITE, wCap, wVaddr, wNumBytes, wCapAccess);
  if (isValid(m_wCapExc)) wExcTok = Valid(ExcToken{
    excCode: CHERIFault,
    capExcCode: m_wCapExc.Valid,
    capIdx: wCapIdx
  });
  `endif
  // handle mem requests on epilogue
  defineEpiEntry(rOneMatch(list(s.readMem.notEmpty, s.writeMem.notEmpty),
                           list(
                             // handle reads
                             rFastSeq(rBlock(
                               dataReadFSM.sink.put(tuple4(rExcTok, READ, rVaddr, rNumBytes)),
                               action
                                 let rsp <- get(dataReadFSM.source);
                                 readCallBack(rsp);
                                 s.readMem.deq;
                               endaction
                             )),
                             // handle writes
                             rFastSeq(rBlock(
                               `ifdef RVXCHERI
                               dataWriteFSM.sink.put(tuple5(wExcTok, wVaddr, wNumBytes, wData, wCapAccess)),
                               `else
                               dataWriteFSM.sink.put(tuple4(wExcTok, wVaddr, wNumBytes, wData)),
                               `endif
                               action
                                 let rsp <- get(dataWriteFSM.source);
                                 writeCallBack(rsp);
                                 s.writeMem.deq;
                               endaction
                             ))
                           ),
                           rAct(noAction)));
endmodule

// Instruction fetch
////////////////////////////////////////////////////////////////////////////////
module [ISADefModule] mkRVIFetch#(RVState s) ();
  // memory read recipe function
  let iFetchFF <- mkBypassFIFOF;
  let iFetchCore = doReadMemCore(
    `ifdef SUPERVISOR_MODE
    s.ivm,
    `endif
    `ifdef PMP
    s.ipmp,
    `endif
    s.imem
  );
  // preparing exception token
  let excTok = Invalid;
  `ifdef RVXCHERI
  let m_ifetchCapExc = ifetchCapChecks(s.pcc, s.pc, 4, False);
  if (isValid(m_ifetchCapExc)) excTok = Valid(ExcToken{
    excCode: CHERIFault,
    capExcCode: m_ifetchCapExc.Valid,
    capIdx: 6'b100000 // this is PCC
  });
  `endif
  function Recipe instFetch(RVState s, Sink#(Bit#(InstWidth)) snk);
    // call back for ifetch responses
    function ifetchCallBack(rsp) = action
      let newInst = 'h00000013; // default to NOP (defined as addi, x0, x0, 0)
      case (rsp) matches
        tagged Left .excTok: raiseIFetchTokException(s, excTok);
        tagged Right .memRsp: case (memRsp) matches
          tagged RVReadRsp .val: begin
            `ifdef RVXCHERI
            match {.captag, .data} = val;
            `else
            let data = val;
            `endif
            let newInstSz = (data[1:0] == 2'b11) ? 4 : 2;
            asIfc(s.pc.early) <= s.pc + newInstSz;
            s.instByteSz <= newInstSz;
            newInst = truncate(data);
          end
          default: begin
            raiseIFetchException(s, InstAccessFault);
          end
        endcase
      endcase
      snk.put(newInst);
    endaction;
    return rFastSeq(rBlock(
      iFetchCore(tuple4(excTok, IFETCH, s.pc.late, 4), toSink(iFetchFF)),
      action
        let rsp <- get(iFetchFF);
        ifetchCallBack(rsp);
      endaction
    ));
  endfunction
  // instruction fetching definition
  defineFetchInstEntry(instFetch(s));
endmodule

`ifdef RVFI_DII
// RVFI-DII Instruction fetch
////////////////////////////////////////////////////////////////////////////////
module [ISADefModule] mkRVIFetch_RVFI_DII#(RVState s) ();
  function Recipe instFetch(RVState s, Sink#(Bit#(InstWidth)) snk) =
  rPipe(rBlock(action
      let inst <- s.rvfi_dii_bridge.client.getInst(0);
      s.iFF.enq(inst);
    endaction, action
      asIfc(s.pc.early) <= s.pc + 4;
      s.instByteSz <= 4;
      snk.put(s.iFF.first);
    endaction
  ));
  // instruction fetching definition
  defineFetchInstEntry(instFetch(s));
endmodule
`endif
