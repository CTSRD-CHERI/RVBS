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

import RV_BasicTypes :: *;

typedef enum {
  OFF = 2'b00, TOR = 2'b01, NA4 = 2'b10, NAPOT = 2'b11
} AddrMatchMode deriving (Bits, Eq, FShow);

typedef struct {
  Bool l;
  Bit#(2) wiri;
  AddrMatchMode a;
  Bool x;
  Bool w;
  Bool r;
} PMPCfg deriving (Bits, FShow);
instance DefaultValue#(PMPCfg);
  function defaultValue = PMPCfg {
    l: False, wiri: 2'b00, a: OFF, x: True, w: True, r: True
  };
endinstance
typedef Vector#(n, PMPCfg) PMPCfgIfc#(numeric type n);
module mkPMPCfgIfcReg (Reg#(PMPCfgIfc#(n)));
  //Vector#(n, Reg#(PMPCfg)) cfgs <- replicateM(mkReg(defaultValue));
  Vector#(n, Reg#(PMPCfg)) cfgs <- replicateM(mkConfigReg(defaultValue));
  method Action _write(PMPCfgIfc#(n) vals) = action
    function Action doWrite(Reg#(PMPCfg) r, PMPCfg v) = action
      if (!r.l) r <= v;
    endaction;
    joinActions(zipWith(doWrite, cfgs, vals));
  endaction;
  method PMPCfgIfc#(n) _read() = readVReg(cfgs);
endmodule

typedef TSub#(PAddrWidth, 2) SmallPAWidth;
typedef struct {
  `ifdef XLEN64
  Bit#(10) wiri;
  `endif
  Bit#(SmallPAWidth) address;
} PMPAddr deriving (Bits, FShow);
instance DefaultValue#(PMPAddr);
  function defaultValue = PMPAddr {
    `ifdef XLEN64
    wiri: 0,
    `endif
    address: 0
  };
endinstance

typedef AddrReq#(PAddr) PMPReq;
typedef AddrRsp#(PAddr) PMPRsp;
typedef AddrLookup#(PMPReq, PMPRsp) PMPLookup;
