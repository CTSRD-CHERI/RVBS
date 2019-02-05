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

import RVBS_BasicTypes :: *;
import BlueBasics :: *;
import BlueUtils :: *;
import AXI :: *;

`ifdef XLEN64
typedef 56 PAddrWidth;
`else
typedef 34 PAddrWidth;
`endif
typedef Bit#(XLEN) VAddr;
typedef Bit#(PAddrWidth) PAddr;
function PAddr toPAddr (VAddr addr);
  Bit#(TMax#(PAddrWidth, XLEN)) tmp = zeroExtend(addr);
  return truncate(tmp);
endfunction

typedef 32 InstWidth;
typedef XLEN IMemWidth;
typedef XLEN DMemWidth;
typedef XLEN IVMMemWidth;
typedef XLEN DVMMemWidth;

// RV load/store/ifetch util types
typedef enum {READ, WRITE, IFETCH} RVMemReqType deriving (Bits, Eq, FShow);
typedef struct { String name; Integer numBytes; Bool sgnExt; } LoadArgs;
typedef struct { String name; Integer numBytes; } StrArgs;

typedef struct
{
  addr_t addr;
  BitPO#(4) numBytes;
  RVMemReqType reqType;
  Maybe#(ExcCode) mExc;
} AddrReq#(type addr_t) deriving (Bits, FShow);
instance NeedRsp#(AddrReq#(addr_t)); function needRsp(req) = True; endinstance
function AddrReq#(addr_t) aReq(RVMemReqType rt, addr_t a, BitPO#(4) n, Maybe#(ExcCode) mE) =
  AddrReq {addr: a, numBytes: n, reqType: rt, mExc: mE};
function AddrReq#(addr_t) aReqRead(addr_t a, BitPO#(4) n, Maybe#(ExcCode) mE) = aReq(READ, a, n, mE);
function AddrReq#(addr_t) aReqWrite(addr_t a, BitPO#(4) n, Maybe#(ExcCode) mE) = aReq(WRITE, a, n, mE);
function AddrReq#(addr_t) aReqIFetch(addr_t a, BitPO#(4) n, Maybe#(ExcCode) mE) = aReq(IFETCH, a, n, mE);
typedef struct {
  addr_t addr;
  Maybe#(ExcCode) mExc;
} AddrRsp#(type addr_t) deriving (Bits, FShow);

typedef Slave#(AddrReq#(addr_req), AddrRsp#(addr_rsp))  AddrLookup#(type addr_req, type addr_rsp);

// Memory interface
// supports data of 128 bits (16 bytes)
typedef union tagged {
  struct {
    PAddr addr;
    BitPO#(4) numBytes;
  } RVReadReq;
  struct {
    PAddr addr;
    Bit#(16) byteEnable;
    Bit#(128) data;
    `ifdef RVXCHERI
    Bit#(1) captag;
    `endif
  } RVWriteReq;
} RVMemReq deriving (Bits, FShow);

instance NeedRsp#(RVMemReq);
  function needRsp(r) = True;
endinstance

instance ToAXI4Lite_AWFlit#(RVMemReq, PAddrWidth, user_sz);
  function toAXI4Lite_AWFlit(x);
    let w = x.RVWriteReq;
    return AXI4Lite_AWFlit {awaddr: pack(w.addr), awprot: 0, awuser: 0};
  endfunction
endinstance

instance ToAXI4Lite_WFlit#(RVMemReq, 128, user_sz)
  `ifdef RVXCHERI
  provisos (Add#(0, user_sz, 1))
  `endif
  ;
  function toAXI4Lite_WFlit(x);
    let w = x.RVWriteReq;
    return AXI4Lite_WFlit {
      wdata: pack(w.data), wstrb: w.byteEnable,
      `ifdef RVXCHERI
      wuser: w.captag
      `else
      wuser: 0
      `endif
    };
  endfunction
endinstance

instance ToAXI4Lite_ARFlit#(RVMemReq, PAddrWidth, user_sz);
  function toAXI4Lite_ARFlit(x);
    let r = x.RVReadReq;
    return AXI4Lite_ARFlit {araddr: pack(r.addr), arprot: 0, aruser: 0};
  endfunction
endinstance

typedef union tagged {
  `ifdef RVXCHERI
  Tuple2#(Bit#(1), Bit#(128)) RVReadRsp;
  `else
  Bit#(128) RVReadRsp;
  `endif
  void RVWriteRsp;
  void RVBusError;
} RVMemRsp deriving (Bits, FShow);

instance FromAXI4Lite_RFlit#(RVMemRsp, 128, user_sz)
  `ifdef RVXCHERI
  provisos (Add#(0, user_sz, 1))
  `endif
  ;
  function fromAXI4Lite_RFlit(x) = case (x.rresp)
    OKAY:
      `ifdef RVXCHERI
      RVReadRsp(tuple2(x.ruser, unpack(x.rdata)));
      `else
      RVReadRsp(unpack(x.rdata));
      `endif
    default: RVBusError;
  endcase;
endinstance

instance FromAXI4Lite_BFlit#(RVMemRsp, user_sz);
  function fromAXI4Lite_BFlit(x) = case (x.bresp)
    OKAY: RVWriteRsp;
    default: RVBusError;
  endcase;
endinstance

typedef Slave#(RVMemReq, RVMemRsp) RVMem;
