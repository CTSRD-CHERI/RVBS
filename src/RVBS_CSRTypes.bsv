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

import RVBS_BasicTypes :: *;
import RVBS_PMPTypes :: *;

//////////////////////
// Legalize classes //
////////////////////////////////////////////////////////////////////////////////

typeclass LegalizeRead#(type a);
  function a legalizeRead(a x);
endtypeclass
typeclass LegalizeWrite#(type a);
  function a legalizeWrite(Bit#(XLEN) oldval, a newval);
endtypeclass

instance LegalizeRead#(a);
  function legalizeRead = id;
endinstance
instance LegalizeWrite#(a);
  function legalizeWrite(x, y) = y;
endinstance

//////////////////////
// CSRs projections //
////////////////////////////////////////////////////////////////////////////////

/*
                           CSR projections
Some RISC-V CSR can be accessed through restricted views in different privilege
modes. The following typeclasses are provided to assist in defining these views.
Note: The two step process of lower+lower1 and lift+lift1 is simply due to a
BSV limitation (In the recursive instance, not using a separate Lift1 or Lower1
typeclass leads to a compiler error)
+----------------------------------------------------------------------+

                                          +---+    lower   +---+
 MACHINE                                  | M <------------+ M |
                                          +-+-+(legalizeRd)+-^-+
                                     lower1 |                |
+---------------------------------+         |                | lift1
                                            |                |
                         +---+    lower   +-v-+    lift    +-+-+
 SUPERVISOR              | S <------------+ S +------------> S |
                         +-+-+(legalizeRd)+-^-+(legalizeWr)+---+
                    lower1 |                |
+----------------+         |                | lift1
                           |                |
                         +-v-+    lift    +-+-+
 USER                    | U +------------> U |
                         +---+(legalizeRw)+---+

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
  function b lift1(a val);
endtypeclass
// Lift, multi-level lifting.
typeclass Lift#(type a, type b);
  function b lift(Bit#(XLEN) oldval, a newval);
endtypeclass

// XXX Lift1 instance, single level transformation, probably masking...
// XXX Define each base case Lift instance as a same level a -> a legalize
// Recursive multi-level Lift instance
instance Lift#(a, b) provisos (Lift#(a,x), Lift1#(x,b));
  function b lift(Bit#(XLEN) oldval, a newval);
    x tmp = lift(oldval, newval); // lift accross levels or same level legalize
    return lift1(tmp); // lift between two distinct levels
  endfunction
endinstance

//////////////////////////////
// helper projection macros //
//////////////////////////////
`define defLower1(a, b)\
instance Lower1#(a, b) provisos (Bits#(a, n), Bits#(b, n));\
  function lower1(x) = cast(x); endinstance
`define defLowerBase(a)\
instance Lower#(a, a); function lower = legalizeRead; endinstance

`define defLift1(a, b)\
instance Lift1#(a, b) provisos (Bits#(a, n), Bits#(b, n));\
  function lift1(x) = cast(x); endinstance
`define defLiftBase(a)\
instance Lift#(a, a); function lift = legalizeWrite; endinstance

// Machine mode macros (assumes existance of 't')
`define defM(t)\
  typedef struct { t val; } M``t deriving (Bits, FShow);\
  `defLower1(t, M``t)\
  `defLift1(M``t, t)
`define defAllM(t)\
  `defM(t)\
  `defLowerBase(t)\
  `defLiftBase(M``t)

// Supervisor mode macros (assumes existance of 't' and 'M``t')
`define defS(t)\
  typedef struct { t val; } S``t deriving (Bits, FShow);\
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
  Bool sd;
  `ifdef XLEN64 // MAX_XLEN > 32
  Bit#(TSub#(XLEN,37)) res4; // WPRI
  XLMode sxl; // WARL
  XLMode uxl; // WARL
  Bit#(9) res3; // WPRI
  `else // MAX_XLEN == 32
  Bit#(8) res3; // WPRI
  `endif
  Bool tsr;
  Bool tw;
  Bool tvm;
  Bool mxr;
  Bool sum;
  Bool mprv;
  Bit#(2) xs;
  Bit#(2) fs;
  Bit#(2) mpp;
  Bit#(2) res2; // WPRI
  Bit#(1) spp;
  Bool mpie;
  Bool res1; // WPRI
  Bool spie;
  Bool upie;
  Bool mie;
  Bool res0; // WPRI
  Bool sie;
  Bool uie;
} Status deriving (Bits);
instance FShow#(Status);
  function fshow(x) = $format("Status {")
    + $format("sd.%1b", x.sd)
    `ifdef XLEN64 // MAX_XLEN > 32
    + $format("|sxl.") + fshow(x.sxl)
    + $format("|uxl.") + fshow(x.uxl)
    `endif
    + $format("|tsr.%1b", x.tsr)
    + $format("|tw.%1b", x.tw)
    + $format("|tvm.%1b", x.tvm)
    + $format("|mxr.%1b", x.mxr)
    + $format("|sum.%1b", x.sum)
    + $format("|mprv.%1b", x.mprv)
    + $format("|xs.%2b", x.xs)
    + $format("|fs.%2b", x.fs)
    + $format("|mpp.") + fshow(toPrivLvl(x.mpp))
    + $format("|spp.") + fshow(toPrivLvl({1'b0,x.spp}))
    + $format("|mpie.%1b", x.mpie)
    + $format("|spie.%1b", x.spie)
    + $format("|upie.%1b", x.upie)
    + $format("|mie.%1b", x.mie)
    + $format("|sie.%1b", x.sie)
    + $format("|uie.%1b", x.uie)
    + $format("}");
endinstance
instance DefaultValue#(Status);
  function Status defaultValue() = Status {
    sd: False,
    `ifdef XLEN64 // MAX_XLEN > 32
    res4: 0, sxl: nativeXLEN, uxl: nativeXLEN, res3: 0,
    `else // MAX_XLEN == 32
    res3: 0,
    `endif
    tsr: False, tw: False, tvm: False, mxr: False, sum: False, mprv: False,
    xs: 0, fs: 0,
    mpp: pack(M), res2: 0, spp: ?,
    mpie: ?, res1: False, spie: ?, upie: ?,
    mie: False, res0: False, sie: False, uie: False
  };
endinstance
`defAllM(Status)
instance LegalizeWrite#(MStatus);
  function legalizeWrite(x, y);
    Status oldval = unpack(x);
    Status newval = y.val;
    `ifdef XLEN64 // MAX_XLEN > 32
    // only XLEN 32 or 64 supported
    if (newval.sxl != XL32 && newval.sxl != XL64) newval.sxl = oldval.sxl;
    if (newval.uxl != XL32 && newval.uxl != XL64) newval.uxl = oldval.uxl;
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
`defAllS(Status)
instance LegalizeRead#(SStatus);
  function legalizeRead(x);
    let ret = x.val;
    ret.mie  = False;
    ret.mpie = False;
    ret.mpp  = 0;
    ret.mprv = False;
    ret.tvm  = False;
    ret.tw   = False;
    ret.tsr  = False;
    `ifdef XLEN64 // MAX_XLEN > 32
    ret.sxl  = 0;
    `endif
    return SStatus { val: ret };
  endfunction
endinstance
instance LegalizeWrite#(SStatus);
  function legalizeWrite(x, y);
    Status oldval = unpack(x);
    Status newval = y.val;
    newval.mie  = oldval.mie;
    newval.mpie = oldval.mpie;
    newval.mpp  = oldval.mpp;
    newval.mprv = oldval.mprv;
    newval.tvm  = oldval.tvm;
    newval.tw   = oldval.tw;
    newval.tsr  = oldval.tsr;
    `ifdef XLEN64 // MAX_XLEN > 32
    newval.sxl  = oldval.sxl;
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
} Extensions deriving (Bits, Eq);
instance FShow#(Extensions);
  function fshow(x) = $format("Extensions {")
    + $format("Z.%1b", pack(x.extZ))
    + $format("|Y.%1b", pack(x.extY))
    + $format("|X.%1b", pack(x.extX))
    + $format("|W.%1b", pack(x.extW))
    + $format("|V.%1b", pack(x.extV))
    + $format("|U.%1b", pack(x.extU))
    + $format("|T.%1b", pack(x.extT))
    + $format("|S.%1b", pack(x.extS))
    + $format("|R.%1b", pack(x.extR))
    + $format("|Q.%1b", pack(x.extQ))
    + $format("|P.%1b", pack(x.extP))
    + $format("|O.%1b", pack(x.extO))
    + $format("|N.%1b", pack(x.extN))
    + $format("|M.%1b", pack(x.extM))
    + $format("|L.%1b", pack(x.extL))
    + $format("|K.%1b", pack(x.extK))
    + $format("|J.%1b", pack(x.extJ))
    + $format("|I.%1b", pack(x.extI))
    + $format("|H.%1b", pack(x.extH))
    + $format("|G.%1b", pack(x.extG))
    + $format("|F.%1b", pack(x.extF))
    + $format("|E.%1b", pack(x.extE))
    + $format("|D.%1b", pack(x.extD))
    + $format("|C.%1b", pack(x.extC))
    + $format("|B.%1b", pack(x.extB))
    + $format("|A.%1b", pack(x.extA))
    + $format("}");
endinstance
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
instance LegalizeWrite#(ISA);
  function ISA legalizeWrite(Bit#(XLEN) x, ISA y);
    let newval = y;
    newval.mxl = nativeXLEN; // no support for dynamic XLMode change
    newval.extensions = defaultValue; // no support for dynamic Extensions change
    return newval;
  endfunction
endinstance

////////////
// EDeleg //
////////////
`define defEDeleg(x)\
typedef struct {Bit#(XLEN) val;} x``EDeleg deriving (Bits, FShow);\
instance DefaultValue#(x``EDeleg);\
  function x``EDeleg defaultValue() = x``EDeleg {val: 0};\
endinstance
`defEDeleg(M)
instance LegalizeWrite#(MEDeleg);
  function MEDeleg legalizeWrite(Bit#(XLEN) x, MEDeleg y);
    Bit#(XLEN) newval = y.val;
    newval[11] = 0;
    return MEDeleg { val: newval };
  endfunction
endinstance
`ifdef SUPERVISOR_MODE
`defEDeleg(S)
instance LegalizeWrite#(SEDeleg);
  function SEDeleg legalizeWrite(Bit#(XLEN) x, SEDeleg y);
    Bit#(XLEN) newval = y.val;
    newval[11:9] = 0;
    return SEDeleg { val: newval };
  endfunction
endinstance
`endif

////////////
// IDeleg //
////////////
typedef struct {Bit#(XLEN) val;} IDeleg deriving (Bits, FShow);
instance DefaultValue#(IDeleg);
  function IDeleg defaultValue() = IDeleg {val: 0};
endinstance

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
} IP deriving (Bits);
instance FShow#(IP);
  function fshow(x) = $format("IP {")
    + $format("me.%1b", x.meip)
    + $format("|se.%1b", x.seip)
    + $format("|ue.%1b", x.ueip)
    + $format("|mt.%1b", x.mtip)
    + $format("|st.%1b", x.stip)
    + $format("|ut.%1b", x.utip)
    + $format("|ms.%1b", x.msip)
    + $format("|ss.%1b", x.ssip)
    + $format("|us.%1b", x.usip)
    + $format("}");
endinstance
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
`defAllM(IP)
instance LegalizeWrite#(MIP);
  function legalizeWrite(x, y);
    IP newval = y.val;
    IP ret = unpack(x);
    // software interrupts
    ret.ssip = newval.ssip;
    ret.usip = newval.usip;
    // timer interrupts
    ret.stip = newval.stip;
    ret.utip = newval.utip;
    // external interrupts
    ret.seip = newval.seip;
    ret.ueip = newval.ueip;
    return MIP { val: ret };
  endfunction
endinstance
`ifdef SUPERVISOR_MODE
`defAllS(IP)
instance LegalizeWrite#(SIP);
  function legalizeWrite(x, y);
    IP newval = y.val;
    IP ret = unpack(x);
    // software interrupts
    ret.ssip = newval.ssip;
    ret.usip = newval.usip;
    // timer interrupts
    ret.stip = newval.stip;
    ret.utip = newval.utip;
    // external interrupts
    ret.seip = newval.seip;
    ret.ueip = newval.ueip;
    return SIP { val: ret };
  endfunction
endinstance
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
} IE deriving (Bits);
instance FShow#(IE);
  function fshow(x) = $format("IE {")
    + $format("me.%1b", x.meie)
    + $format("|se.%1b", x.seie)
    + $format("|ue.%1b", x.ueie)
    + $format("|mt.%1b", x.mtie)
    + $format("|st.%1b", x.stie)
    + $format("|ut.%1b", x.utie)
    + $format("|ms.%1b", x.msie)
    + $format("|ss.%1b", x.ssie)
    + $format("|us.%1b", x.usie)
    + $format("}");
endinstance
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
typedef struct { Bit#(TSub#(XLEN,2)) base;  TVecMode mode; } TVec deriving (Bits);
instance FShow#(TVec);
  function fshow(x) = $format("TVec {0x%0x - ", {x.base,2'b00}, fshow(x.mode), "}");
endinstance
instance DefaultValue#(TVec);
  function TVec defaultValue() = TVec {base: 0, mode: Direct};
endinstance
instance LegalizeWrite#(TVec);
  function legalizeWrite(x, y);
    TVec oldval = unpack(x);
    TVec newval = y;
    if (newval.mode != Direct || newval.mode != Vectored)
      newval.mode = oldval.mode;
    return newval;
  endfunction
endinstance

/////////
// EPC //
/////////
typedef struct {
  `ifdef RVC
  Bit#(TSub#(XLEN, 1)) addr;
  Bit#(1) z;
  `else
  Bit#(TSub#(XLEN, 2)) addr;
  Bit#(2) z;
  `endif
} EPC deriving (Bits);
instance FShow#(EPC);
  function fshow(x) = $format("EPC {0x%0x}", pack(x));
endinstance
instance DefaultValue#(EPC);
  function EPC defaultValue() = EPC{addr: ?, z:0}; // must not trigger unaligned inst fetch exception
endinstance
instance LegalizeWrite#(EPC);
  function legalizeWrite(x, y);
    EPC newval = unpack(x);
    newval.addr = y.addr;
    return newval;
  endfunction
endinstance

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
    tagged Interrupt .i: $format(fshow(i) + $format(" (interrupt %0d)", pack(i)));
    tagged Exception .e: $format(fshow(e) + $format(" (exception %0d)", pack(e)));
  endcase;
endinstance
//function Bool isValidCause(Cause c) = case (c) matches
function Bool isValidCause(Bit#(XLEN) c) = case (unpack(c)) matches
  tagged Interrupt .i: case (i)
    USoftInt, SSoftInt, MSoftInt,
    UTimerInt, STimerInt, MTimerInt,
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

//////////////
// VendorID //
//////////////
typedef struct { Bit#(TSub#(XLEN,7)) bank; Bit#(7) offset; }
  VendorID deriving (Bits, FShow);
instance DefaultValue#(VendorID);
  function VendorID defaultValue() = VendorID {bank: 0, offset: 0};
endinstance

`ifdef SUPERVISOR_MODE
//////////
// SATP //
//////////
`ifdef XLEN64 // MAX_XLEN > 32
typedef enum { BARE = 4'h0, SV39 = 4'h8, SV48 = 4'h9, SV57 = 4'ha, SV64 = 4'hb } VMMode64 deriving (Bits, Eq, FShow);
`else
typedef enum { BARE = 1'b0, SV32 = 1'b1 } VMMode32 deriving (Bits, Eq, FShow);
`endif
typedef struct {
  `ifdef XLEN64 // MAX_XLEN > 32
  VMMode64 mode;
  Bit#(16) asid;
  Bit#(44) ppn;
  `else
  VMMode32 mode;
  Bit#(9) asid;
  Bit#(22) ppn;
  `endif
} SATP deriving (Bits, FShow);
instance DefaultValue#(SATP);
  function defaultValue() = SATP { mode: BARE, asid: 0, ppn: 0 };
endinstance
instance LegalizeWrite#(SATP);
  function legalizeWrite(oldval, newval);
    `ifdef XLEN64 // MAX_XLEN > 32
    // "if satp is written with an unsupported MODE, the entire write has no effect; no fields in satp are modified"
    // TODO (only bare is currently supported)
    return (newval.mode != BARE) ? unpack(oldval) : newval;
    `else
    return newval;
    `endif
  endfunction
endinstance
`endif

///////////////////////////
// Interface to the CSRs //
////////////////////////////////////////////////////////////////////////////////

typedef enum {RW, RS, RC} CSRReqType deriving (Eq, FShow);
typedef enum {ALL, NOREAD, NOWRITE} CSRReqEffects deriving (Eq, FShow);

typedef struct {
  Bit#(12) idx;
  Bit#(XLEN) val;
  CSRReqType rType;
  CSRReqEffects rEffects;
} CSRReq deriving (FShow);

instance DefaultValue#(CSRReq);
  function CSRReq defaultValue =
    CSRReq { idx: ?, val: ?, rType: RW, rEffects: ALL };
endinstance
function CSRReq rwCSRReq(Bit#(12) i, Bit#(XLEN) v) =
  CSRReq { idx: i, val: v, rType: RW, rEffects: ALL };
function CSRReq rwCSRReqNoRead(Bit#(12) i, Bit#(XLEN) v) =
  CSRReq { idx: i, val: v, rType: RW, rEffects: NOREAD };
function CSRReq rsCSRReq(Bit#(12) i, Bit#(XLEN) v) =
  CSRReq { idx: i, val: v, rType: RS, rEffects: ALL };
function CSRReq rsCSRReqNoWrite(Bit#(12) i, Bit#(XLEN) v) =
  CSRReq { idx: i, val: v, rType: RS, rEffects: NOWRITE };
function CSRReq rcCSRReq(Bit#(12) i, Bit#(XLEN) v) =
  CSRReq { idx: i, val: v, rType: RC, rEffects: ALL };
function CSRReq rcCSRReqNoWrite(Bit#(12) i, Bit#(XLEN) v) =
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
  Reg#(MEDeleg)    medeleg;
  Reg#(IDeleg)     mideleg;
  Reg#(IE)         mie;
  Reg#(TVec)       mtvec;
  // TODO mcounteren

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
  Vector#(2, Reg#(PMPCfgIfc#(8))) pmpcfg;
  `else
  Vector#(4, Reg#(PMPCfgIfc#(4))) pmpcfg;
  `endif
  // pmpaddr0, pmpaddr1, ..., pmpaddr15
  Vector#(16, Reg#(PMPAddr)) pmpaddr;
  `endif

  `ifdef SUPERVISOR_MODE
  // supervisor trap setup
  //////////////////////////////////////////////////////////////////////////////
  // sstatus -- S-view of mstatus;
  Reg#(SEDeleg)    sedeleg;
  Reg#(IDeleg)     sideleg;
  // sie -- S-view of mie
  Reg#(TVec)       stvec;
  // TODO scounteren

  // supervisor trap handling
  //////////////////////////////////////////////////////////////////////////////
  Reg#(Bit#(XLEN)) sscratch;
  Reg#(EPC)        sepc;
  Reg#(Cause)      scause;
  Reg#(Bit#(XLEN)) stval;
  // sip -- S-view of mip

  // supervisor protection and translation
  //////////////////////////////////////////////////////////////////////////////
  Reg#(SATP) satp;
  `endif

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

  // IRQs
  //////////////////////////////////////////////////////////////////////////////
  function Action doSetMSIP (Bool irw) setMSIP;
  function Action doSetMTIP (Bool irw) setMTIP;
  function Action doSetMEIP (Bool irw) setMEIP;

  // CSR request
  //////////////////////////////////////////////////////////////////////////////
  function ActionValue#(Bit#(XLEN)) doReq (CSRReq r) req;

} CSRs;
