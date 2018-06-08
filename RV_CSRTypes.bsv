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
Note: The two step process of lower+lower1 and lift+lift1 is simply due to a
BSV limitation (In the recursive instance, not using a separate Lift1 or Lower1
typeclass leads to a compiler error)
+----------------------------------------------------------------------+

                                          +---+  lower     +---+
 MACHINE                                  | M <------------+ M |
                                          +-+-+  (zero)    +-^-+
                                     lower1 |                |
+---------------------------------+         |                | lift1
                                            |                |
                         +---+  lower     +-v-+    lift    +-+-+
 SUPERVISOR              | S <------------+ S +------------> S |
                         +-+-+  (zero)    +-^-+ (legalize) +---+
                    lower1 |                |
+----------------+         |                | lift1
                           |                |
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
// Lower1, one level lowering (simple cast)
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
// Lift1, one level lifting (simple cast)
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
`define defLowerBase(a)\
instance Lower#(a, a); function lower = id; endinstance

`define defLift1(a, b)\
instance Lift1#(a, b) provisos (Bits#(a, n), Bits#(b, n));\
  function lift1(x,y,z) = cast(y); endinstance
`define defLiftBase(a)\
instance Lift#(a, a); function lift(x,y,z) = y; endinstance

// Machine mode macros (assumes existance of 't')
`define defM(t)\
  typedef struct { t val; } M``t deriving (Bits);\
  `defLower1(t, M``t)\
  `defLift1(M``t, t)
`define defAllM(t)\
  `defM(t)\
  `defLowerBase(t)\
  `defLiftBase(M``t)

// Supervisor mode macros (assumes existance of 't' and 'M``t')
`define defS(t)\
  typedef struct { t val; } S``t deriving (Bits);\
  `defLower1(M``t, S``t)\
  `defLift1(S``t, M``t)
`define defAllS(t)\
  `defS(t)\
  `defLowerBase(M``t)\
  `defLiftBase(S``t)

///////////////
// CSR Types //
////////////////////////////////////////////////////////////////////////////////

////////////
// Status //
////////////
typedef struct {
  Bit#(1) sd;
  `ifdef XLEN64 // MAX_XLEN > 32
  Bit#(TSub#(XLEN,37)) res4; // WPRI
  XLMode sxl;
  XLMode uxl;
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
    res4: 0, sxl: nativeXLEN, uxl: nativeXLEN, res3: 0,
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
`defLowerBase(Status)
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
    return MStatus { val: newval };
  endfunction
endinstance
`ifdef SUPERVISOR_MODE
`defS(Status)
`defLowerBase(MStatus)
instance Lift#(SStatus, SStatus);
  function SStatus lift(Bit#(XLEN) x, SStatus y, PrivLvl _);
    Status oldval = unpack(x);
    Status newval = y.val;
    newval.mie = oldval.mie;
    newval.mpie = oldval.mpie;
    newval.mpp = oldval.mpp;
    newval.mprv = oldval.mprv;
    newval.tvm = oldval.tvm;
    newval.tw = oldval.tw;
    newval.tsr = oldval.tsr;
    `ifdef XLEN64 // MAX_XLEN > 32
    newval.sxl = oldval.sxl;
    `endif
    return SStatus { val: newval };
  endfunction
endinstance
`endif

/////////
// ISA //
/////////
typedef struct {
  Bool extZ;
  Bool extY;
  Bool extX;
  Bool extW;
  Bool extV;
  Bool extU;
  Bool extT;
  Bool extS;
  Bool extR;
  Bool extQ;
  Bool extP;
  Bool extO;
  Bool extN;
  Bool extM;
  Bool extL;
  Bool extK;
  Bool extJ;
  Bool extI;
  Bool extH;
  Bool extG;
  Bool extF;
  Bool extE;
  Bool extD;
  Bool extC;
  Bool extB;
  Bool extA;
} Extensions deriving (Bits, Eq, FShow);
instance DefaultValue#(Extensions);
  function Extensions defaultValue() = Extensions {
    extZ: False,
    extY: False,
    extX: False,
    extW: False,
    extV: False,
    extU: static_HAS_U_MODE,
    extT: False,
    extS: static_HAS_S_MODE,
    extR: False,
    extQ: False,
    extP: False,
    extO: False,
    extN: static_HAS_N_EXT,
    extM: static_HAS_M_EXT,
    extL: False,
    extK: False,
    extJ: False,
    extI: True,
    extH: False,
    extG: False,
    extF: False,
    extE: False,
    extD: False,
    extC: static_HAS_C_EXT,
    extB: False,
    extA: False
  };
endinstance
typedef struct { XLMode mxl; Bit#(TSub#(XLEN,28)) res; Extensions extensions; }
  ISA deriving (Bits, FShow);
instance DefaultValue#(ISA);
  function ISA defaultValue() = ISA {
    mxl: nativeXLEN,
    res: ?, // WIRI
    extensions: defaultValue
  };
endinstance
`defM(ISA)
`defLowerBase(ISA)
instance Lift#(MISA, MISA);
  function MISA lift(Bit#(XLEN) x, MISA y, PrivLvl _);
    let newval = y.val;
    newval.mxl = nativeXLEN; // no support for dynamic XLMode change
    newval.extensions = defaultValue; // no support for dynamic Extensions change
    return MISA { val: newval };
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
`defLowerBase(EDeleg)
instance Lift#(MEDeleg, MEDeleg);
  function MEDeleg lift(Bit#(XLEN) x, MEDeleg y, PrivLvl _);
    Bit#(XLEN) newval = y.val.val;
    newval[11] = 0;
    return MEDeleg { val: EDeleg { val: newval }};
  endfunction
endinstance
`ifdef SUPERVISOR_MODE
// XXX TODO
`defAllS(EDeleg)
`endif

////////////
// IDeleg //
////////////
typedef struct {Bit#(XLEN) val;} IDeleg deriving (Bits);
instance DefaultValue#(IDeleg);
  function IDeleg defaultValue() = IDeleg {val: 0};
endinstance
`defAllM(IDeleg)
`ifdef SUPERVISOR_MODE
// XXX TODO
`defAllS(IDeleg)
`endif

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
`ifdef SUPERVISOR_MODE
// XXX TODO
`defAllS(IE)
`endif

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
`defLowerBase(TVec)
instance Lift#(MTVec, MTVec);
  function MTVec lift(Bit#(XLEN) x, MTVec y, PrivLvl _);
    TVec oldval = unpack(x);
    TVec newval = y.val;
    if (newval.mode != Direct || newval.mode != Vectored)
      newval.mode = oldval.mode;
    return MTVec { val: newval };
  endfunction
endinstance
`ifdef SUPERVISOR_MODE
// XXX TODO
`defAllS(TVec)
`endif

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
`defLowerBase(EPC)
instance Lift#(MEPC, MEPC);
  function MEPC lift(Bit#(XLEN) x, MEPC y, PrivLvl _);
    EPC newval = y.val;
    if (newval.addr[1:0] != 0) newval.addr[1:0] = 0; // must not trigger unaligned inst fetch exception
    return MEPC { val: newval };
  endfunction
endinstance
`ifdef SUPERVISOR_MODE
// XXX TODO
`defAllS(EPC)
`endif

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
`ifdef SUPERVISOR_MODE
// XXX TODO
`defAllS(Cause)
`endif

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
`defLowerBase(IP)
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
`ifdef SUPERVISOR_MODE
// XXX TODO
`defAllS(IP)
`endif

//////////////
// VendorID //
//////////////
typedef struct { Bit#(TSub#(XLEN,7)) bank; Bit#(7) offset; }
  VendorID deriving (Bits, FShow);
instance DefaultValue#(VendorID);
  function VendorID defaultValue() = VendorID {bank: 0, offset: 0};
endinstance
`defAllM(VendorID)
