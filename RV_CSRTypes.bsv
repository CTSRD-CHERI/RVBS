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

import RV_BasicTypes :: *;

//////////////////////
// CSRs projections //
////////////////////////////////////////////////////////////////////////////////

/*
                           CSR projections
+----------------------------------------------------------------------+

                                          +---+  lower     +---+
 MACHINE                                  | M <------------+ M |
                                          +-+-+   (id)     +-^-+
                                     lower1 |                |
+---------------------------------+  (zero) |                | lift1
                                            |                | (mask)
                         +---+  lower     +-v-+    lift    +-+-+
 SUPERVISOR              | S <------------+ S +------------> S |
                         +-+-+   (id)     +-^-+ (legalize) +---+
                    lower1 |                |
+----------------+  (zero) |                | lift1
                           |                | (mask)
                         +-v-+    lift    +-+-+
 USER                    | U +------------> U |
                         +---+ (legalize) +---+

+-----------------------------------------------------------------------+

For each CSR type view, define instances for Lift, Lift1, Lower and Lower1
Typeclasses allow us to define transitions between subsequent hierarchy levels
A -> B, B -> C, C -> D etc... and automatically derive the transformation from
A -> D as (((A -> B) -> C) -> D) applying each individual steps.

*/

//////////////////////
// lowering classes //
//////////////////////
// Lower1, one level lowering
typeclass Lower1#(type a, type b) dependencies (b determines a);
  function b lower1(a val);
endtypeclass
// Lower, multi-level lowering.
typeclass Lower#(type a, type b);
  function b lower(a val); endtypeclass

// XXX Lower1 instance, single level transformation, probably zeroing...
// XXX Define each base case Lower instance as same level a -> a legalize, probably do nothing...
// Recursive multi-level Lower instance
instance Lower#(a, b) provisos (Lower#(a,x), Lower1#(x,b));
  function b lower(a val);
    x tmp = lower(val); // lower accross levels or same level (do nothing...)
    return lower1(tmp); // lower between two distinct levels
  endfunction
endinstance

/////////////////////
// lifting classes //
/////////////////////
// Lift1, one level lifting
typeclass Lift1#(type a, type b) dependencies (b determines a);
  function b lift1(Bit#(XLEN) oldval, a newval, PrivLvl curlvl);
endtypeclass
// Lift, multi-level lifting.
typeclass Lift#(type a, type b);
  function b lift(Bit#(XLEN) oldval, a newval, PrivLvl curlvl);
endtypeclass

// XXX Lift1 instance, single level transformation, probably masking...
// XXX Define each base case Lift instance as a same level a -> a legalize
// Recursive multi-level Lift instance
instance Lift#(a, b) provisos (Lift#(a,x), Lift1#(x,b));
  function b lift(Bit#(XLEN) oldval, a newval, PrivLvl curlvl);
    x tmp = lift(oldval, newval, curlvl); // lift accross levels or same level legalize
    return lift1(oldval, tmp, curlvl); // lift between two distinct levels
  endfunction
endinstance

//////////////////////////////
// helper projection macros //
//////////////////////////////
`define defLower1(a, b)\
instance Lower1#(a, b) provisos (Bits#(a, n), Bits#(b, n));\
  function lower1(x) = cast(x); endinstance
`define defLower(a, b)\
instance Lower#(a, b) provisos (Bits#(a, n), Bits#(b, n));\
  function lower(x) = cast(x); endinstance
`define defLowerBase(a)\
instance Lower#(a, a); function lower = id; endinstance

`define defLift1(a, b)\
instance Lift1#(a, b) provisos (Bits#(a, n), Bits#(b, n));\
  function lift1(x,y,z) = cast(y); endinstance
`define defLift(a, b)\
instance Lift#(a, b) provisos (Bits#(a, n), Bits#(b, n));\
  function lift(x,y,z) = cast(y); endinstance
`define defLiftBase(a)\
instance Lift#(a, a); function lift(x,y,z) = y; endinstance

// Machine mode macros (assumes existance of 't')
`define defM(t) typedef struct { t val; } M``t deriving (Bits);
`define defLower1M(t) `defLower1(t, M``t)
`define defLift1M(t) `defLift1(M``t, t)
`define defAllM(t) `defM(t)\
                   `defLower1M(t)\
                   `defLowerBase(t)\
                   `defLift1M(t)\
                   `defLiftBase(M``t)

// Supervisor mode macros (assumes existance of 't' and 'M``t')
// XXX

///////////////
// CSR Types //
////////////////////////////////////////////////////////////////////////////////

///////////
// Cause //
///////////
typedef union tagged {
  IntCode Interrupt;
  ExcCode Exception;
} Cause deriving (Eq);
instance Bits#(Cause, XLEN);
  function Bit#(XLEN) pack (Cause c) = case (c) matches // n must be at leas 4 + 1
    tagged Interrupt .i: {1'b1, zeroExtend(pack(i))};
    tagged Exception .e: {1'b0, zeroExtend(pack(e))};
  endcase;
  function Cause unpack (Bit#(XLEN) c) = (c[valueOf(XLEN)-1] == 1'b1) ?
    tagged Interrupt unpack(truncate(c)) :
    tagged Exception unpack(truncate(c));
endinstance
instance FShow#(Cause);
  function Fmt fshow(Cause cause) = case (cause) matches
    tagged Interrupt .i: $format(fshow(i), " (interrupt)");
    tagged Exception .e: $format(fshow(e), " (exception)");
  endcase;
endinstance
//function Bool isValidCause(Cause c) = case (c) matches
function Bool isValidCause(Bit#(XLEN) c) = case (unpack(c)) matches
  tagged Interrupt .i: case (i)
    USoftInt, SSoftInt, MSoftInt,
    UtimerInt, STimerInt, MTimerInt,
    UExtInt, SExtInt, MExtInt: True;
    default: False;
  endcase
  tagged Exception .e: case (e)
    InstAddrAlign, InstAccessFault, IllegalInst,
    Breakpoint, LoadAddrAlign, LoadAccessFault,
    StrAMOAddrAlign, StrAMOAccessFault,
    ECallFromU, ECallFromS, ECallFromM,
    InstPgFault, LoadPgFault, StrAMOPgFault: True;
    default: False;
  endcase
endcase;
`defAllM(Cause)

/////////
// ISA //
/////////
`ifdef XLEN64
Bit#(2) nativeXLEN = 2'd2;
`else
Bit#(2) nativeXLEN = 2'd1;
`endif
typedef struct { Bit#(2) mxl; Bit#(TSub#(XLEN,28)) res; Bit#(26) extensions; }
  ISA deriving (Bits, FShow);
instance DefaultValue#(ISA);
  function ISA defaultValue() = ISA {
    mxl: nativeXLEN,
    res: ?, // WIRI
    extensions: 26'b00000000000000000100000000
  };
endinstance
`defM(ISA)
`defLower1M(ISA)
`defLowerBase(ISA)
`defLift1M(ISA)
instance Lift#(MISA, MISA);
  function MISA lift(Bit#(XLEN) x, MISA y, PrivLvl _);
    let newval = y.val;
    if (newval.mxl != nativeXLEN) newval.mxl = nativeXLEN; // only support native XLEN
    return MISA { val: newval };
  endfunction
endinstance

//////////////
// VendorID //
//////////////
typedef struct { Bit#(TSub#(XLEN,7)) bank; Bit#(7) offset; }
  VendorID deriving (Bits, FShow);
instance DefaultValue#(VendorID);
  function VendorID defaultValue() = VendorID {bank: 0, offset: 0};
endinstance
`defAllM(VendorID)

////////////
// Status //
////////////
function Bit#(2) xl_field(Integer xlen) = case (xlen)
  128: 2'b11; // 3
  64: 2'b10;  // 2
  32: 2'b01;  // 1
  default: 2'b00;
endcase;
typedef struct {
  Bit#(1) sd;
  `ifdef XLEN64 // MAX_XLEN > 32
  Bit#(TSub#(XLEN,37)) res4; // WPRI
  Bit#(2) sxl;
  Bit#(2) uxl;
  Bit#(9) res3; // WPRI
  `else // MAX_XLEN == 32
  Bit#(8) res3; // WPRI
  `endif
  Bit#(1) tsr;
  Bit#(1) tw;
  Bit#(1) tvm;
  Bit#(1) mxr;
  Bit#(1) sum;
  Bit#(1) mprv;
  Bit#(2) xs;
  Bit#(2) fs;
  Bit#(2) mpp;
  Bit#(2) res2; // WPRI
  Bit#(1) spp;
  Bit#(1) mpie;
  Bit#(1) res1; // WPRI
  Bit#(1) spie;
  Bit#(1) upie;
  Bit#(1) mie;
  Bit#(1) res0; // WPRI
  Bit#(1) sie;
  Bit#(1) uie;
} Status deriving (Bits, FShow);
instance DefaultValue#(Status);
  function Status defaultValue() = Status {
    sd: 0,
    `ifdef XLEN64 // MAX_XLEN > 32
    res4: 0, sxl: xl_field(valueOf(XLEN)), uxl: xl_field(valueOf(XLEN)), res3: 0,
    `else // MAX_XLEN == 32
    res3: 0,
    `endif
    tsr: 0, tw: 0, tvm: 0, mxr: 0, sum: 0, mprv: 0,
    xs: 0, fs: 0,
    mpp: pack(M), res2: 0, spp: ?,
    mpie: ?, res1: 0, spie: ?, upie: ?,
    mie: 0, res0: 0, sie: 0, uie: 0
  };
endinstance
`defM(Status)
`defLower1M(Status)
`defLowerBase(Status)
`defLift1M(Status)
instance Lift#(MStatus, MStatus);
  function MStatus lift(Bit#(XLEN) x, MStatus y, PrivLvl _);
    Status oldval = unpack(x);
    Status newval = y.val;
    `ifdef XLEN64 // MAX_XLEN > 32
    if (!static_HAS_S_MODE) newval.sxl = 0;
    if (!static_HAS_U_MODE) newval.uxl = 0;
    `endif
    if (!static_HAS_S_MODE &&         unpack(newval.mpp) == S) newval.mpp = oldval.mpp;
    if (!static_HAS_S_MODE && unpack({1'b0, newval.spp}) == S) newval.spp = oldval.spp;
    if (!static_HAS_U_MODE &&         unpack(newval.mpp) == U) newval.mpp = oldval.mpp;
    if (!static_HAS_U_MODE && unpack({1'b0, newval.spp}) == U) newval.spp = oldval.spp;
    //$display("DEBUG ==== mstatus.mpp was ", fshow(oldval.mpp), ", is now ", fshow(newval.mpp));
    return MStatus { val: newval };
  endfunction
endinstance

/////////
// EPC //
/////////
typedef struct {
  Bit#(XLEN) addr;
} EPC deriving (Bits, FShow);
instance DefaultValue#(EPC);
  function EPC defaultValue() = EPC{addr: {?,2'b00}}; // must not trigger unaligned inst fetch exception
endinstance
`defM(EPC)
`defLower1M(EPC)
`defLowerBase(EPC)
`defLift1M(EPC)
instance Lift#(MEPC, MEPC);
  function MEPC lift(Bit#(XLEN) x, MEPC y, PrivLvl _);
    EPC newval = y.val;
    if (newval.addr[1:0] != 0) newval.addr[1:0] = 0; // must not trigger unaligned inst fetch exception
    return MEPC { val: newval };
  endfunction
endinstance

//////////
// TVec //
//////////
typedef enum {Direct, Vectored, Res} TVecMode deriving (Eq, FShow);
instance Bits#(TVecMode, 2);
  function Bit#(2) pack (TVecMode mode) = case (mode)
    Direct: 2'b00;
    Vectored: 2'b01;
    Res: 2'b11;
    default: 2'b11;
  endcase;
  function TVecMode unpack (Bit#(2) mode) = case (mode)
    2'b00: Direct;
    2'b01: Vectored;
    default: Res;
  endcase;
endinstance
instance Ord#(TVecMode);
  function Ordering compare(TVecMode a, TVecMode b) = compare(pack(a), pack(b));
endinstance
instance Literal#(TVecMode);
  function TVecMode fromInteger (Integer x) = case (x)
    0: Direct;
    1: Vectored;
    2, 3: Res;
    default: error(sprintf("Invalid TVecMode literal %0d. Use {0, 1, 2, 3}."));
  endcase;
  function Bool inLiteralRange (TVecMode _, Integer x) = (x >= 0 && x < 4);
endinstance
typedef struct { Bit#(TSub#(XLEN,2)) base;  TVecMode mode; }
  TVec deriving (Bits, FShow);
instance DefaultValue#(TVec);
  function TVec defaultValue() = TVec {base: 0, mode: Direct};
endinstance
`defM(TVec)
`defLower1M(TVec)
`defLowerBase(TVec)
`defLift1M(TVec)
instance Lift#(MTVec, MTVec);
  function MTVec lift(Bit#(XLEN) x, MTVec y, PrivLvl _);
    TVec oldval = unpack(x);
    TVec newval = y.val;
    if (newval.mode != Direct || newval.mode != Vectored)
      newval.mode = oldval.mode;
    return MTVec { val: newval };
  endfunction
endinstance

////////////
// EDeleg //
////////////
typedef struct {Bit#(XLEN) val;} EDeleg deriving (Bits);
instance DefaultValue#(EDeleg);
  function EDeleg defaultValue() = EDeleg {val: 0};
endinstance
`defM(EDeleg)
`defLower1M(EDeleg)
`defLowerBase(EDeleg)
`defLift1M(EDeleg)
instance Lift#(MEDeleg, MEDeleg);
  function MEDeleg lift(Bit#(XLEN) x, MEDeleg y, PrivLvl _);
    Bit#(XLEN) newval = y.val.val;
    newval[11] = 0;
    return MEDeleg { val: EDeleg { val: newval }};
  endfunction
endinstance

////////////
// IDeleg //
////////////
typedef struct {Bit#(XLEN) val;} IDeleg deriving (Bits);
instance DefaultValue#(IDeleg);
  function IDeleg defaultValue() = IDeleg {val: 0};
endinstance
`defAllM(IDeleg)

////////
// IP //
////////
typedef struct {
  Bit#(TSub#(XLEN,12)) res3;
  Bool meip;
  Bool res2;
  Bool seip;
  Bool ueip;
  Bool mtip;
  Bool res1;
  Bool stip;
  Bool utip;
  Bool msip;
  Bool res0;
  Bool ssip;
  Bool usip;
} IP deriving (Bits, FShow);
instance DefaultValue#(IP); // XXX does spec actually specify reboot value ?
  function IP defaultValue() = IP {
    res3: 0,
    meip: False,
    res2: False,
    seip: False,
    ueip: False,
    mtip: False,
    res1: False,
    stip: False,
    utip: False,
    msip: False,
    res0: False,
    ssip: False,
    usip: False
  };
endinstance
`defM(IP)
`defLower1M(IP)
`defLowerBase(IP)
`defLift1M(IP)
instance Lift#(MIP, MIP);
  function MIP lift(Bit#(XLEN) x, MIP y, PrivLvl lvl);
    IP oldval = unpack(x);
    IP newval = y.val;
    // software interrupts
    newval.msip = oldval.msip; // TODO
    if (lvl < S) newval.ssip = oldval.ssip;
    if (lvl < U) newval.usip = oldval.usip;
    // timer interrupts
    newval.mtip = oldval.mtip; // TODO
    if (lvl != M) begin
      newval.stip = oldval.stip;
      newval.utip = oldval.utip;
    end
    // external interrupts
    newval.meip = oldval.meip; // TODO
    if (lvl != M) begin
      newval.seip = oldval.seip;
      newval.ueip = oldval.ueip;
    end
    // reserved WIRI fields
    newval.res0 = False;
    newval.res1 = False;
    newval.res2 = False;
    newval.res3 = 0;
    // fold value
    return MIP { val: newval };
  endfunction
endinstance

////////
// IE //
////////
typedef struct {
  Bit#(TSub#(XLEN,12)) res3;
  Bool meie;
  Bool res2;
  Bool seie;
  Bool ueie;
  Bool mtie;
  Bool res1;
  Bool stie;
  Bool utie;
  Bool msie;
  Bool res0;
  Bool ssie;
  Bool usie;
} IE deriving (Bits, FShow);
instance DefaultValue#(IE); // XXX does spec actually specify reboot value ?
  function IE defaultValue() = IE {
    res3: 0,
    meie: False,
    res2: False,
    seie: False,
    ueie: False,
    mtie: False,
    res1: False,
    stie: False,
    utie: False,
    msie: False,
    res0: False,
    ssie: False,
    usie: False
  };
endinstance
`defAllM(IE)

// undefine macros
`undef defM
`undef defLower1M
`undef defLowerM
`undef defLift1M
`undef defLiftM
`undef defAllM
`undef defLower1
`undef defLower
`undef defLowerBase
`undef defLift1
`undef defLift
`undef defLiftBase
`undef defM
`undef defLower1M
`undef defLift1M
`undef defAllM
