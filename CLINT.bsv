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
import Vector :: *;
import FIFOF :: *;

import AXI :: *;

function Bit#(n) merge(Bit#(n) old_val, Bit#(n) new_val, Bit#(TDiv#(n, 8)) be)
  provisos (Bits#(Vector::Vector#(TDiv#(n, 8), Bit#(8)), n));
  Vector#(TDiv#(n, 8),Bit#(8)) old_vec = unpack(old_val);
  Vector#(TDiv#(n, 8),Bit#(8)) new_vec = unpack(new_val);
  Vector#(TDiv#(n, 8),Bool)     be_vec = unpack(be);
  function mergeByte(b_old, b_new, b) = (b) ? b_new : b_old;
  return pack(zipWith3(mergeByte, old_vec, new_vec, be_vec));
endfunction

interface CLINT#(numeric type addr_sz, numeric type data_sz);
  interface AXILiteSlave#(addr_sz, data_sz) axiLiteSlave;
  method Bool peekMSIP;
  method Bool peekMTIP;
endinterface
module mkCLINT (CLINT#(addr_sz, data_sz))
  provisos (
    Add#(a__, data_sz, 64),
    Add#(b__, TDiv#(data_sz, 8), 8),
    Add#(c__, 1, data_sz),
    DefaultValue#(BLiteFlit),
    DefaultValue#(RLiteFlit#(data_sz))
  );
  // local state
  AXILiteSlaveShim#(addr_sz, data_sz) shim <- mkAXILiteSlaveShim;
  Reg#(Bit#(64)) r_mtime <- mkReg(0); // XXX mkRegU
  Reg#(Bit#(64)) r_mtimecmp <- mkRegU;
  Reg#(Bool) r_msip <- mkReg(False); // XXX mkRegU
  Reg#(Bool) r_mtip <- mkRegU;
  // timer rules
  rule count_time; r_mtime <= r_mtime + 1; endrule
  rule compare; if (r_mtime >= r_mtimecmp) r_mtip <= True; endrule
  // AXI write request handling
  rule writeReq;
    // get request
    shim.awff.deq;
    shim.wff.deq;
    let awflit = shim.awff.first;
    let  wflit = shim.wff.first;
    // handle request
    BLiteFlit bflit = defaultValue;
    case (awflit.awaddr[15:0])
      16'h0000: r_msip <= unpack(wflit.wdata[0] & wflit.wstrb[0]);
      16'h4000: begin
        r_mtimecmp <=
          merge(r_mtimecmp, zeroExtend(wflit.wdata), zeroExtend(wflit.wstrb));
        r_mtip <= False;
      end
      16'hBFF8: bflit.bresp = SLVERR;
      default: bflit.bresp = SLVERR;
    endcase
    // put response
    shim.bff.enq(bflit);
  endrule
  // AXI read request handling
  rule readReq;
    // get request
    shim.arff.deq;
    let arflit = shim.arff.first;
    // handle request
    RLiteFlit#(data_sz) rflit = defaultValue;
    case (arflit.araddr[15:0])
      16'h0000: rflit.rdata = zeroExtend(pack(r_msip));
      16'h4000: rflit.rdata = truncate(r_mtimecmp); // XXX TODO get appropriate size
      16'hBFF8: rflit.rdata = truncate(r_mtime); // XXX TODO same
      default: rflit.rresp = SLVERR;
    endcase
    // put response
    shim.rff.enq(rflit);
  endrule
  // wire up interfaces
  interface axiLiteSlave = shim.slave;
  method peekMSIP = r_msip;
  method peekMTIP = r_mtip;
endmodule
