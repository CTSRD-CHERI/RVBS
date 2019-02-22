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

package CHERICC;

import CHERICap :: *;

export CHERICCCap;
export CHERICCBounds;

`define div2(x) TDiv#(x, 2)
`define sub2(x) TSub#(x, 2)
`define i(x)    valueOf(x)

// CHERICCBounds Bounds type
////////////////////////////////////////////////////////////////////////////////
// CHERICC compressed bounds type
typedef union tagged {
  struct {
    Bit#(1) lenMSB;
    Bit#(`sub2(base_)) top;
    Bit#(base_) base;
  } Exp0;
  struct {
    Bit#(TSub#(`sub2(base_), `div2(e_))) top;
    Bit#(TSub#(base_, `div2(e_))) base;
    Bit#(e_) e;
  } EmbeddedExp;
  struct {
    Bit#(TSub#(`sub2(base_), TAdd#(`div2(t_), `div2(e_)))) top;
    Bit#(TSub#(base_, TAdd#(`div2(t_), `div2(e_)))) base;
    Bit#(t_) otype;
    Bit#(e_) e;
  } Sealed;
} CHERICCBounds#(numeric type base_, numeric type e_, numeric type t_);

instance Bits#(CHERICCBounds#(b_, e_, t_), TMul#(b_, 2)) provisos(
    // in pack
    Add#(TDiv#(e_, 2), a__, e_), // truncates on e
    Add#(TDiv#(t_, 2), b__, t_), // truncates on t
    Add#(2, c__, b_), // 2 bits stolen from top
    // in unpack
    Add#(d__, TDiv#(e_, 2), TMul#(b_, 2)), // truncates raw into e
    Add#(e__, TDiv#(t_, 2), TMul#(b_, 2)), // truncates raw into t
    Add#(2, f__, TSub#(
                   TSub#(
                     TAdd#(b_,
                           TAdd#(
                             TDiv#(t_, 2),
                             TDiv#(e_, 2))),
                     TDiv#(e_, 2)),
                     TDiv#(t_, 2)))
  );
  function pack(ccbounds) =
    case (ccbounds) matches
      tagged Exp0 .x: return {{{1'b0, x.lenMSB}, x.top}, x.base};
      tagged EmbeddedExp .x: begin
        Bit#(`div2(e_)) eHi = truncateLSB(x.e);
        Bit#(`div2(e_)) eLo = truncate(x.e);
        return {{2'b10, x.top, eHi}, {x.base, eLo}};
      end
      tagged Sealed .x: begin
        Bit#(`div2(t_)) tHi = truncateLSB(x.otype);
        Bit#(`div2(t_)) tLo = truncate(x.otype);
        Bit#(`div2(e_)) eHi = truncateLSB(x.e);
        Bit#(`div2(e_)) eLo = truncate(x.e);
        return {{2'b11, x.top, tHi, eHi}, {x.base, tLo, eLo}};
        /*
        Bit#(TMul#(b_, 2)) acc = 0;
        acc = acc | zeroExtend(2'b11);
        acc = (acc << `i(b_)-2-`i(t_)/2-`i(e_)/2) | zeroExtend(x.top);
        acc = (acc << `i(t_)/2) | zeroExtend(tHi);
        acc = (acc << `i(e_)/2) | zeroExtend(eHi);
        acc = (acc << `i(b_)-`i(t_)/2-`i(e_)/2) | zeroExtend(x.base);
        acc = (acc << `i(t_)/2) | zeroExtend(tLo);
        acc = (acc << `i(e_)/2) | zeroExtend(eLo);
        return acc;
        */
      end
    endcase;
  function unpack(raw);
    if (raw[2*`i(b_)-1] == 0) return Exp0 {
      lenMSB: raw[2*`i(b_)-2],
      top:    raw[2*`i(b_)-3:`i(b_)],
      base:   raw[`i(b_)-1:0]
    };
    else if (raw[2*`i(b_)-2] == 0) begin
      Bit#(`div2(e_)) eHi = truncate(raw >> `i(b_));
      Bit#(`div2(e_)) eLo = truncate(raw);
      // XXX Bit#(e_) new_e = {eHi, eLo}; XXX simpler provisos with equiv line below
      Bit#(e_) new_e = zeroExtend(eLo) | zeroExtend(eHi) << `i(e_)/2;
      return EmbeddedExp {
        top:    raw[2*`i(b_)-3:`i(b_)+`i(e_)/2],
        base:   raw[`i(b_)-1:`i(e_)/2],
        e:      new_e
      };
    end else begin
      Bit#(`div2(t_)) tHi = truncate(raw >> (`i(b_)+(`i(e_)/2)));
      Bit#(`div2(t_)) tLo = truncate(raw >> (`i(e_)/2));
      // XXX Bit#(t_) new_t = {tHi, tLo}; XXX simpler provisos with equiv line below
      Bit#(t_) new_t = zeroExtend(tLo) | zeroExtend(tHi) << `i(t_)/2;
      Bit#(`div2(e_)) eHi = truncate(raw >> `i(b_));
      Bit#(`div2(e_)) eLo = truncate(raw);
      // XXX Bit#(e_) new_e = {eHi, eLo}; XXX simpler provisos with equiv line below
      Bit#(e_) new_e = zeroExtend(eLo) | zeroExtend(eHi) << `i(e_)/2;
      return Sealed {
        top:    raw[2*`i(b_)-3:`i(b_)+`i(e_)/2+`i(t_)/2],
        base:   raw[`i(b_)-1:`i(e_)/2+`i(t_)/2],
        otype:  new_t,
        e:      new_e
      };
    end
  endfunction
endinstance

// CHERICC capability type
////////////////////////////////////////////////////////////////////////////////

`define CCSoftPerms Bit#(4)
`define AllPermsSz TAdd#(SizeOf#(`CCSoftPerms), SizeOf#(HardPerms))

typedef struct {
  Bool isCap;
  `CCSoftPerms softperms;
  HardPerms hardperms;
  Bit#(TSub#(addr_, TAdd#(bounds_, `AllPermsSz))) res; // 15 permission bits and bounds_ bits to deduct
  CHERICCBounds#(`div2(bounds_), e_, t_) bounds;
  Bit#(addr_) addr;
} CHERICCCap#(numeric type addr_, numeric type bounds_, numeric type e_, numeric type t_);

instance Bits#(CHERICCCap#(addr_, bounds_, e_, t_),
               TAdd#(1, TAdd#(addr_, TAdd#(bounds_, TAdd#(res_, `AllPermsSz))))) provisos(
    Bits#(CHERICCBounds#(TDiv#(bounds_, 2), e_, t_), bounds_),
    Add#(TAdd#(bounds_, `AllPermsSz), res_, addr_)
  );
  function pack(cap);
    Bit#(1)                         isCap = pack(cap.isCap);
    Bit#(SizeOf#(`CCSoftPerms)) softperms = cap.softperms;
    Bit#(SizeOf#(HardPerms))    hardperms = pack(cap.hardperms);
    Bit#(res_)                        res = cap.res;
    Bit#(bounds_)                  bounds = pack(cap.bounds);
    Bit#(addr_)                      addr = cap.addr;
    return {isCap, softperms, hardperms, res, bounds, addr};
  endfunction
  //function pack(cap) = {cap.softperms, pack(cap.perms), cap.res, pack(cap.bounds), cap.addr};
  function unpack(raw) = CHERICCCap {
    isCap:     unpack(msb(raw)),
    softperms: raw[2*`i(addr_)-1:2*`i(addr_)-`i(SizeOf#(`CCSoftPerms))],
    hardperms: unpack(raw[2*`i(addr_)-5:2*`i(addr_)-`i(`AllPermsSz)]),
    res:       raw[2*`i(addr_)-`i(`AllPermsSz)-1:`i(addr_)+`i(bounds_)],
    bounds:    unpack(raw[`i(addr_)+`i(bounds_)-1:`i(addr_)]),
    addr:      raw[`i(addr_)-1:0]
  };
endinstance

`undef AllPermsSz
`undef CCSoftPerms

// CHERICCCap inner helpers
////////////////////////////////////////////////////////////////////////////////

CHERICCCap#(addr_, bounds_, e_, t_) almightyCC = CHERICCCap {
  isCap: True,
  softperms: ~0,
  hardperms: unpack(~0),
  res: 0,
  bounds: EmbeddedExp {
            top:  0, // implied top bits of 01
            base: 0,
            // position the 1 of top in the addr_'th bit
            e:    fromInteger(`i(addr_)-((`i(bounds_)/2)-2))
          },
  addr: 0
};

CHERICCCap#(addr_, bounds_, e_, t_) nullCC = CHERICCCap {
  isCap: False,
  softperms: 0,
  hardperms: unpack(0),
  res: 0,
  bounds: EmbeddedExp {
            top:  0, // implied top bits of 01
            base: 0,
            e:    fromInteger(`i(addr_)-((`i(bounds_)/2)-2)) // position the 1 of top in the addr_'th bit
          },
  addr: 0
};

function Bit#(e_) getExpCC(CHERICCCap#(addr_, bounds_, e_, t_) cap);
  case (cap.bounds) matches
    tagged Exp0 .b: return 0;
    tagged EmbeddedExp .b: return b.e;
    tagged Sealed .b: return b.e;
  endcase
endfunction

function Bit#(3) getRepBoundCC(CHERICCCap#(addr_, bounds_, e_, t_) cap)
  provisos (Add#(3, a__, `div2(bounds_))) =
  truncateLSB(cap.bounds.Exp0.base) - 3'b001; // always 1/8th of representable space below object

function Int#(2) getRegionCorrectionCC(Bit#(3) a, Bit#(3) b, Bit#(3) rep) =
  ((b < rep) == (a < rep)) ? 0 : (((b < rep) && (a >= rep)) ? 1 : -1);

function Bit#(`div2(bounds_))
  getTopFieldCC(CHERICCCap#(addr_, bounds_, e_, t_) cap);
  Bit#(2) c_carry = 2'b00;
  Bit#(2) c_len   = 2'b01;
  Bit#(`sub2(`div2(bounds_))) partialTop = 0;
  case (cap.bounds) matches
    tagged Exp0 .b: begin
      if (zeroExtend(b.top) < b.base) c_carry = 2'b01;
      c_len = {1'b0, b.lenMSB};
      partialTop = b.top;
    end
    tagged EmbeddedExp .b: begin
      if (zeroExtend(b.top) < b.base) c_carry = 2'b01;
      partialTop = {b.top, 0};
    end
    tagged Sealed .b: begin
      if (zeroExtend(b.top) < b.base) c_carry = 2'b01;
      partialTop = {b.top, 0};
    end
  endcase
  return {truncateLSB(cap.bounds.Exp0.base) + c_carry + c_len, partialTop};
endfunction

function Bit#(`div2(bounds_))
  getBaseFieldCC(CHERICCCap#(addr_, bounds_, e_, t_) cap) =
  case (cap.bounds) matches
    tagged Exp0 .b:         b.base;
    tagged EmbeddedExp .b: {b.base, 0};
    tagged Sealed .b:      {b.base, 0};
  endcase;

// CHERICCCap CHERICap instance
////////////////////////////////////////////////////////////////////////////////
instance CHERICap#(CHERICCCap#(addr_, bounds_, e_, t_), t_, addr_) provisos (
    Add#(3, a__, `div2(bounds_)), // 3 bits of bounds for 1/8th of rep space
    Add#(3, b__, addr_), // same for addr
    Add#(c__, TAdd#(2, `div2(bounds_)), addr_), // for base correction
    Add#(d__, TAdd#(2, `div2(bounds_)), TAdd#(addr_, 1)), // for top 2 bits of Int#(2) correction
    Add#(e__, `div2(bounds_), addr_), // slice addr into smaller bounds field
    Add#(f__, `div2(bounds_), TAdd#(addr_, 1)), // same for addr+1
    Add#(g__, e_, TLog#(TAdd#(1, addr_))) // can fit result of countZerosMSB in e_
  );
  //////////////////////////////////////////////////////////////////////////////
  function isValidCap(cap) = cap.isCap;
  //////////////////////////////////////////////////////////////////////////////
  function setValidCap(cap, v);
    cap.isCap = v;
    return cap;
  endfunction
  //////////////////////////////////////////////////////////////////////////////
  function getHardPerms(cap) = cap.hardperms;
  //////////////////////////////////////////////////////////////////////////////
  function setHardPerms(cap, hardperms);
    cap.hardperms = hardperms;
    return cap;
  endfunction
  //////////////////////////////////////////////////////////////////////////////
  function getSoftPerms(cap) = zeroExtend(cap.softperms);
  //////////////////////////////////////////////////////////////////////////////
  function setSoftPerms(cap, softperms);
    cap.softperms = truncate(softperms);
    return cap;
  endfunction
  //////////////////////////////////////////////////////////////////////////////
  function getKind(cap) = case (cap.bounds) matches
    tagged Sealed ._: return SEALED_WITH_TYPE;
    default: return UNSEALED;
  endcase;
  //////////////////////////////////////////////////////////////////////////////
  function getType(cap) = case (cap.bounds) matches
    tagged Sealed .b: return zeroExtend(b.otype);
    default: return -1;
  endcase;
  //////////////////////////////////////////////////////////////////////////////
  function setType(cap, otype);
    let new_cap = cap;
    let isExact = True;
    case (cap.bounds) matches
      tagged Sealed .b: if (otype == -1) begin
        //Bit#(addr_) addrBits = cap.address >> b.e;
        //let baseMid = addrBits[`sub1(TAdd#(`div2(t_), `div2(e))):`div2(e_)];
        //let baseLo  = addrBits[`sub1(`div2(e_)):0];
        //let topMid  = baseMid;
        //let topLo   = baseLo;
        let baseHi = b.base;
        let  topHi = b.top;
        if (b.e == 0) new_cap.bounds = Exp0 {
          lenMSB: 1,
          top:    {topHi, 0},
          base:   {baseHi, 0}
        };
        else new_cap.bounds = EmbeddedExp {
          top:  {topHi,  0},
          base: {baseHi, 0},
          e:    b.e
        };
      end
      default: if (otype != -1) begin
        Bit#(e_) new_e = case (cap.bounds) matches
          tagged EmbeddedExp .b: b.e;
          default: 0;
        endcase;
        new_cap.bounds = Sealed {
          top:   truncateLSB(cap.bounds.Exp0.top),
          base:  truncateLSB(cap.bounds.Exp0.base),
          otype: otype,
          e:     new_e
        };
        Bit#(`div2(t_)) zero = 0;
        isExact = cap.bounds.Exp0.top[`i(t_)/2-1:0]  == zero &&
                  cap.bounds.Exp0.base[`i(t_)/2-1:0] == zero;
      end
    endcase
    return Exact{exact: isExact, value: new_cap};
  endfunction
  //////////////////////////////////////////////////////////////////////////////
  function getAddr(cap) = cap.addr;
  //////////////////////////////////////////////////////////////////////////////
  function setAddr(cap) = error("setAddr unimplemented");
  //////////////////////////////////////////////////////////////////////////////
  function getOffset(cap) = zeroExtend(getAddr(cap)) - getBase(cap);
  //////////////////////////////////////////////////////////////////////////////
  function setOffset(cap, offset);
    Bit#(`div2(bounds_)) e0m = ~(~0 << ((`i(t_)/2)+(`i(e_)/2)));
    Bit#(TSub#(`div2(bounds_), `div2(e_))) eem = ~(~0 << (`i(t_)/2));
    // extract specific useful values
    Bit#(e_) e = getExpCC(cap);
    Bit#(e_) almighty_e = fromInteger(`i(addr_)-((`i(bounds_)/2)-2)); // position the 1 of top in the addr_'th bit
    Bit#(addr_) i = offset - getOffset(cap);
    Bit#(`div2(bounds_)) imid = truncate(i >> e);
    Bit#(`div2(bounds_)) amid = truncate(cap.addr >> e);
    Bit#(`div2(bounds_)) r    = {getRepBoundCC(cap), 0};
    // perform inRange and inLimit tests
    Bit#(addr_) mask = ~0 << (e + fromInteger(`i(bounds_)/2));
    Bool inRange  = ((i & mask) == mask) || ((i & mask) == 0);
    Bool inLimits = (i >= 0) ? imid < (r - amid - 1) :
                               imid >= (r - amid) && r != amid;
    Bool isExact = ((inRange && inLimits) || e >= almighty_e);
    // perform the offset update
    let new_cap = cap;
    new_cap.addr = truncate(getBase(cap) + offset);
    return Exact{exact: isExact, value: new_cap};
  endfunction
  //////////////////////////////////////////////////////////////////////////////
  function getBase(cap);
    let baseCC = getBaseFieldCC(cap);
    let e = getExpCC(cap);
    let correction = getRegionCorrectionCC(truncateLSB(cap.addr),
                                           truncateLSB(baseCC),
                                           getRepBoundCC(cap));
    Bit#(addr_) mask = ~0 << (e + fromInteger(`i(bounds_)/2));
    Bit#(addr_) acc = cap.addr & mask;
    return acc + (signExtend({pack(correction), baseCC}) << e);
  endfunction
  //////////////////////////////////////////////////////////////////////////////
  function getTop(cap);
    let topCC = getTopFieldCC(cap);
    let e = getExpCC(cap);
    let correction = getRegionCorrectionCC(truncateLSB(cap.addr),
                                           truncateLSB(topCC),
                                           getRepBoundCC(cap));
    Bit#(TAdd#(addr_, 1)) mask = ~0 << (e + fromInteger(`i(bounds_)/2));
    Bit#(TAdd#(addr_, 1)) acc = zeroExtend(cap.addr) & mask;
    return acc + (signExtend({pack(correction), topCC}) << e);
  endfunction
  //////////////////////////////////////////////////////////////////////////////
  function getLength(cap) = getTop(cap) - zeroExtend(getBase(cap));
  //////////////////////////////////////////////////////////////////////////////
  function setBounds(cap, length);
    let new_cap = cap;
    let isExact = True;
    // deriving new exponent
    Bit#(TLog#(TAdd#(1, addr_))) e =
      pack(fromInteger(`i(addr_))
      - countZerosMSB(length >> ((`i(bounds_)/2)-1)));
    // deriving the new base
    Bit#(`div2(bounds_)) newBase = truncate(cap.addr >> e);
    // deriving the new top
    Bit#(TAdd#(addr_, 1)) fullTop = zeroExtend(cap.addr) + zeroExtend(length);
    Bit#(`div2(bounds_)) newTop = truncate(fullTop >> e);
    // fold the derived values back in the new cap
    if (e == 0) begin
      new_cap.bounds = Exp0 {
        lenMSB: length[(`i(bounds_)/2)-2],
        top:    truncate(newTop),
        base:   newBase
      };
    end else begin
      // slice the top and base values appropriately
      Bit#(TSub#(`sub2(`div2(bounds_)), `div2(e_))) upperTop = truncateLSB(newTop);
      Bit#(TSub#(`div2(bounds_), `div2(e_))) upperBase = truncateLSB(newBase);
      // take care of loss of significant bits in the bits stolen/dropped from fullTop
      Bit#(TAdd#(addr_, 1)) mask = ~(~0 << (e + fromInteger(`i(e_)/2)));
      if ((fullTop & mask) != 0) upperTop = upperTop + 1;
      new_cap.bounds = EmbeddedExp {
        top:  upperTop,
        base: upperBase,
        e:    truncate(e)
      };
      // check for exact or not
      Bit#(addr_) exactMask = ~(~0 << (e - fromInteger(`i(bounds_)/2 - `i(e_)/2 - 1)));
      if ((cap.addr & exactMask) != 0) isExact = False;
      if   ((length & exactMask) != 0) isExact = False;
    end
    return Exact{exact: isExact, value: new_cap};
  endfunction
  //////////////////////////////////////////////////////////////////////////////
  function nullWithAddr(x);
    let cap = nullCap;
    cap.addr = x;
    return cap;
  endfunction
  //////////////////////////////////////////////////////////////////////////////
  function almightyCap = almightyCC;
  //////////////////////////////////////////////////////////////////////////////
  function nullCap = nullCC;
  //////////////////////////////////////////////////////////////////////////////
endinstance

`undef div2
`undef sub2
`undef i

endpackage
