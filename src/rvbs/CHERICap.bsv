/*-
 * Copyright (c) 2018-2019 Alexandre Joannou
 * Copyright (c) 2019 Peter Rugg
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
// Permission bits

typedef Bit#(16) SoftPerms;
typedef struct {
  Bool     accessSysRegs;
  Bool     permitUnseal;
  Bool     permitCCall;
  Bool     permitSeal;
  Bool     permitStoreLocalCap;
  Bool     permitStoreCap;
  Bool     permitLoadCap;
  Bool     permitStore;
  Bool     permitLoad;
  Bool     permitExecute;
  Bool     global;
} HardPerms deriving(Bits, Eq, FShow);

instance Bitwise#(HardPerms);
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

// Type to return the result of an operation along with whether the operation was exact
// In cases where no sensible inexact representation exists, the only guarantee is that
// the tag bit is not set.
typedef struct {
  Bool exact;
  t    value;
} Exact #(type t);

typedef enum {
  UNSEALED,
  SENTRY,
  RES0,
  RES1,
  SEALED_WITH_TYPE
} Kind deriving (Eq, FShow);

typeclass CHERICap#(type t, numeric type ot, numeric type n)
  dependencies (t determines (ot, n));

  // Return whether the Capability is valid
  function Bool isValidCap (t cap);
  // Set the capability as valid. All fields left unchanged
  function t setValidCap (t cap, Bool valid);

  // Get the hardware permissions
  function HardPerms getHardPerms (t cap);
  // Set the hardware permissions
  function t setHardPerms (t cap, HardPerms hardperms);
  // Get the software permissions
  function SoftPerms getSoftPerms (t cap);
  // Set the software permissions
  function t setSoftPerms (t cap, SoftPerms softperms);
  // Get the architectural permissions
  function Bit#(31) getPerms (t cap) =
    zeroExtend({pack(getSoftPerms(cap)), 4'h0, pack(getHardPerms(cap))});

  // Get the kind of the capability, i.e. whether it is sealed, sentry, unsealed, ...
  function Kind getKind (t cap);
  // Helper methods for identifying specific kinds
  function Bool isUnsealed (t cap) = getKind(cap) == UNSEALED;
  function Bool isSentry (t cap) = getKind(cap) == SENTRY;
  function Bool isSealedWithType (t cap) = getKind(cap) == SEALED_WITH_TYPE;
  function Bool isSealed (t cap) = getKind(cap) == SEALED_WITH_TYPE || getKind(cap) == SENTRY;

  // Get the type field, including implicitly whether the cap is sealed/sentry
  function Bit#(ot) getType (t cap);
  // Set the type field, including implicitly sealing/unsealing the capability
  // In the event the new type makes the cap unrepresentable
  function Exact#(t) setType (t cap, Bit#(ot) otype);
  // Get the address pointed to by the capability

  function Bit#(n) getAddr (t cap);
  // Set the address of the capability. Result invalid if not exact
  function Exact#(t) setAddr (t cap, Bit#(n) addr);

  // Get the offset of the capability
  function Bit#(n) getOffset (t cap) = getAddr(cap) - getBase(cap);
  // Set the offset of the capability. Result invalid if not exact
  function Exact#(t) setOffset (t cap, Bit#(n) offset);

  // Get the base
  function Bit#(n) getBase (t cap);
  // Get the top
  function Bit#(TAdd#(n, 1)) getTop (t cap);
  // Get the length
  function Bit#(TAdd#(n, 1)) getLength (t cap);

  // Set the length of the capability. Inexact: result length may be different to requested
  function Exact#(t) setBounds (t cap, Bit#(n) length);

  // Returns a null cap with an address set to the argument
  function t nullWithAddr (Bit#(n) addr);

  // Return the maximally permissive capability (initial register state)
  function t almightyCap;
  // Return the null capability
  function t nullCap;

endtypeclass

function Fmt showCHERICap(t cap) provisos (CHERICap#(t, ot, n));
  return $format( "Valid: 0x%0x", isValidCap(cap)) +
         $format(" Perms: 0x%0x", getPerms(cap)) +
         $format(" Kind: ", fshow(getKind(cap))) +
         (isSealedWithType(cap) ? $format(" Type: %0d", getType(cap)) : $format("")) +
         $format(" Addr: 0x%0x", getAddr(cap)) +
         $format(" Base: 0x%0x", getBase(cap)) +
         $format(" Length: 0x%0x", getLength(cap));
endfunction

typeclass Cast #(type src, type dest);
  function dest cast (src x);
endtypeclass

endpackage
