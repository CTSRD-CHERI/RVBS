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

package CHERICap;

// CHERI capability typeclass
////////////////////////////////////////////////////////////////////////////////

// Hardware permission bits
typedef struct {
    Bool accesSysRegs;
    Bool permitUnseal;
    Bool permitCCall;
    Bool permitSeal;
    Bool permitStoreLocalCap;
    Bool permitStoreCap;
    Bool permitLoadCap;
    Bool permitStore;
    Bool permitLoad;
    Bool permitExecute;
    Bool global;
} Perms deriving(Bits, Eq, FShow);
instance Bitwise#(Perms);
  function \& (x1, x2) = unpack(pack(x1) & pack(x2));
  function \| (x1, x2) = unpack(pack(x1) | pack(x2));
  function \^ (x1, x2) = unpack(pack(x1) ^ pack(x2));
  function \~^ (x1, x2) = unpack(pack(x1) ~^ pack(x2));
  function \^~ (x1, x2) = unpack(pack(x1) ^~ pack(x2));
  function invert (x) = unpack(invert (pack(x))); //XXX Bluespec ref guide uses x1 here but simply x for other single arg methods...
  function \<< (x1, x2) = unpack(pack(x1) << x2);
  function \>> (x1, x2) = unpack(pack(x1) >> x2);
  function msb (x) = msb(pack(x));
  function lsb (x) = lsb(pack(x));
endinstance

typedef Bit#(4) UPerms;

typedef TAdd#(SizeOf#(UPerms), SizeOf#(Perms)) AllPermsSz;

typeclass CHERICap#(type t, numeric type ot, numeric type n) dependencies (t determines (ot, n));
  `define BigBit Bit#(TAdd#(n, 1))
  function UPerms   getUPerms    (t cap);
  function t        setUPerms    (t cap, UPerms uperms);
  function Perms    getPerms     (t cap);
  function t        setPerms     (t cap, Perms perms);
  function Bool     getSealed    (t cap);
  function t        setSealed    (t cap, Bool sealed);
  function Bool     canRepSealed (t cap, Bool sealed) =
    canRepCap(cap, sealed, getOffset(cap));
  function Bit#(ot) getType      (t cap);
  function t        setType      (t cap, Bit#(ot) otype);
  function `BigBit  getAddr      (t cap);
  function `BigBit  getOffset    (t cap) = getAddr(cap) - getBase(cap);
  function t        setOffset    (t cap, `BigBit offset);
  function Bool     canRepOffset (t cap, `BigBit offset) =
    canRepCap(cap, getSealed(cap), offset);
  function `BigBit  getBase      (t cap);
  function `BigBit  getTop       (t cap);
  function `BigBit  getLength    (t cap);
  function t        setBounds    (t cap, Bit#(n) length);
  function Bool     canRepBounds (t cap, Bit#(n) length);
  function Bool     canRepCap    (t cap, Bool sealed, `BigBit offset);
  function t        almightyCap;
  function t        nullCap;
  `undef BigBit
endtypeclass

function Fmt showCHERICap(t cap) provisos (CHERICap#(t, ot, n));
  return $format( "UPerms: 0x%0x", getUPerms(cap)) +
         $format(" Perms: 0x%0x", getPerms(cap)) +
         $format(" Sealed: ", fshow(getSealed(cap))) +
         $format(" Type: %0d", getType(cap)) +
         $format(" Addr: 0x%0x", getAddr(cap)) +
         $format(" Base: 0x%0x", getBase(cap)) +
         $format(" Length: 0x%0x", getLength(cap));
endfunction

endpackage
