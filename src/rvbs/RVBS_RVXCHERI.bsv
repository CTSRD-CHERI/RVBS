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

import         BID :: *;
import      BitPat :: *;

import    CHERICap :: *;

import  RVBS_Types :: *;
import   RVBS_Trap :: *;
import RVBS_Traces :: *;

// XXX placeholders
////////////////////////////////////////////////////////////////////////////////
typedef enum {
  CapExcNone              = 'h00, // None
  CapExcLength            = 'h01, // Length Violation
  CapExcTag               = 'h02, // Tag Violation
  CapExcSeal              = 'h03, // Seal Violation
  CapExcType              = 'h04, // Type Violation
  CapExcCall              = 'h05, // Call Trap
  CapExcRet               = 'h06, // Return Trap
  CapExcUnderflowTSS      = 'h07, // Underflow of trusted system stack
  CapExcUser              = 'h08, // User-defined Permision Violation
  CapExcTLBNoStore        = 'h09, // TLB prohibits store capability
  CapExcInexact           = 'h0a, // Requested bounds cannot be represented exactly
  CapExcGlobal            = 'h10, // Global Violation
  CapExcPermExe           = 'h11, // Permit_Execute Violation
  CapExcPermLoad          = 'h12, // Permit_Load Violation
  CapExcPermStore         = 'h13, // Permit_Store Violation
  CapExcPermLoadCap       = 'h14, // Permit_Load_Capability Violation
  CapExcPermStoreCap      = 'h15, // Permit_Store_Capability Violation
  CapExcPermStoreLocalCap = 'h16, // Permit_Store_Local_Capability Violation
  CapExcPermSeal          = 'h17, // Permit_Seal Violation
  CapExcAccessSysReg      = 'h18, // Access_System_Registers Violation
  CapExcPermCCall         = 'h19, // Premit_CCall Violation
  CapExcPermCCallIDC      = 'h1a, // Premit_CCall IDC Violation
  CapExcPermUnseal        = 'h1c  // Premit_Unseal Violation
} CapExcCode deriving (Bits, Eq, FShow);
function Action capTrap (RVState s, CapExcCode exc, Bit#(5) idx) = noAction;
function Action notImplemented(String str) = action
  $display(str + " is not currently implemented");
  $finish(0);
endaction;
// Helper functions
////////////////////////////////////////////////////////////////////////////////

function Bool isCap(CapType cap) = case (cap) matches
  tagged Cap ._: return True;
  default: return False;
endcase;

// Capability inspection instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXCHERI_CGetPerm(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  let cap = s.rCR(cb).Cap;
  s.wGPR(rd, {0, getUPerms(cap), pack(getPerms(cap))});
  //XXX logInst(s.pc, fmtInstXCHERI("cgetperm", rd, cb));
endaction;

function Action instrXCHERI_CGetType(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  let cap = s.rCR(cb).Cap;
  if (getSealed(cap)) s.wGPR(rd, zeroExtend(getType(cap)));
  else s.wGPR(rd, ~0);
  //XXX logInst(s.pc, fmtInstXCHERI("cgettype", rd, cb));
endaction;

function Action instrXCHERI_CGetBase(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, truncate(getBase(s.rCR(cb).Cap)));
  //XXX logInst(s.pc, fmtInstXCHERI("cgetbase", rd, cb));
endaction;

function Action instrXCHERI_CGetLen(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  let len = getLength(s.rCR(cb).Cap);
  if (msb(len) == 1) s.wGPR(rd, ~0);
  else s.wGPR(rd, truncate(len));
  //XXX logInst(s.pc, fmtInstXCHERI("cgetlen", rd, cb));
endaction;

function Action instrXCHERI_CGetTag(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, zeroExtend(pack(isCap(s.rCR(cb)))));
  //XXX logInst(s.pc, fmtInstXCHERI("cgettag", rd, cb));
endaction;

function Action instrXCHERI_CGetSealed(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, zeroExtend(pack(getSealed(s.rCR(cb).Cap))));
  //XXX logInst(s.pc, fmtInstXCHERI("cgetsealed", rd, cb));
endaction;

function Action instrXCHERI_CGetOffset(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, truncate(getOffset(s.rCR(cb).Cap)));
  //XXX logInst(s.pc, fmtInstXCHERI("cgetoffset", rd, cb));
endaction;

function Action instrXCHERI_CGetAddr(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, truncate(getAddr(s.rCR(cb).Cap)));
  //XXX logInst(s.pc, fmtInstXCHERI("cgetaddr", rd, cb));
endaction;

// Capability modification instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXCHERI_CSeal(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) cd) = action
  let ct_reg = s.rCR(ct);
  let cap_ct = ct_reg.Cap;
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  if (!isCap(cs_reg)) capTrap(s, CapExcTag, cs);
  else if (!isCap(ct_reg)) capTrap(s, CapExcTag, ct);
  else if (getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else if (getSealed(cap_ct)) capTrap(s, CapExcSeal, ct);
  else if (!getPerms(cap_ct).permitSeal) capTrap(s, CapExcPermSeal, ct);
  else if (getOffset(cap_ct) >= getLength(cap_ct)) capTrap(s, CapExcLength, ct);
  else if (getBase(cap_ct) + getOffset(cap_ct) > zeroExtend(otypeMax)) capTrap(s, CapExcLength, ct); //XXX large ineq in spec
  else if (!canRepSealed(cap_cs, True)) capTrap(s, CapExcInexact, cs);
  else begin
    let new_cap = setSealed(cap_cs, True);
    new_cap = setType(new_cap, truncate(getBase(cap_ct) + getOffset(cap_ct)));
    s.wCR(cd, Cap(new_cap));
    //XXX logInst(s.pc, fmtInstXCHERI("cseal", ct, cs, cd));
  end
endaction;

function Action instrXCHERI_CUnseal(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) cd) = action
  let ct_reg = s.rCR(ct);
  let cap_ct = ct_reg.Cap;
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  if (!isCap(cs_reg)) capTrap(s, CapExcTag, cs);
  else if (!isCap(ct_reg)) capTrap(s, CapExcTag, ct);
  else if (!getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else if (getSealed(cap_ct)) capTrap(s, CapExcSeal, ct);
  else if (getBase(cap_ct) + getOffset(cap_ct) != zeroExtend(getType(cap_cs))) capTrap(s, CapExcType, ct);
  else if (!getPerms(cap_ct).permitUnseal) capTrap(s, CapExcPermUnseal, ct);
  else if (getOffset(cap_ct) >= getLength(cap_ct)) capTrap(s, CapExcLength, ct);
  else begin
    let new_cap = setSealed(cap_cs, False);
    new_cap  = setType(new_cap, 0);
    Perms p  = getPerms(new_cap);
    p.global = p.global && getPerms(cap_ct).global;
    new_cap  = setPerms(new_cap, p);
    s.wCR(cd, Cap(new_cap));
    //XXX logInst(s.pc, fmtInstXCHERI("cunseal", ct, cs, cd));
  end
endaction;

function Action instrXCHERI_CAndPerm(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  if (!isCap(cs_reg)) capTrap(s, CapExcTag, cs);
  else if (getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else begin
    let rt_val = s.rGPR(rt);
    Perms   newPerms = unpack(pack(getPerms(cap_cs)) & rt_val[valueOf(SizeOf#(Perms))-1:0]);
    UPerms newUPerms = getUPerms(cap_cs) & rt_val[valueOf(TAdd#(SizeOf#(UPerms), SizeOf#(Perms)))-1:valueOf(SizeOf#(Perms))];
    let new_cap = setPerms(cap_cs, newPerms);
    new_cap = setUPerms(new_cap, newUPerms);
    s.wCR(cd, Cap(new_cap));
    //XXX logInst(s.pc, fmtInstXCHERI("candperms", ct, cs, cd));
  end
endaction;

function Action instrXCHERI_CSetOffset(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  let rt_val = s.rGPR(rt);
  if (isCap(cs_reg) && getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else if (!canRepOffset(cap_cs, zeroExtend(rt_val))) begin
    RawCap n_cap = nullCap;
    s.wCR(cd, Data(pack(setOffset(n_cap, getBase(cap_cs) + zeroExtend(rt_val)))));
    //XXX logInst(s.pc, fmtInstXCHERI("csetoffset", ct, cs, cd));
  end else begin
    s.wCR(cd, Cap(setOffset(cap_cs, zeroExtend(rt_val))));
    //XXX logInst(s.pc, fmtInstXCHERI("csetoffset", ct, cs, cd));
  end
endaction;

function Action instrXCHERI_CIncOffset(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  let rt_val = s.rGPR(rt);
  if (isCap(cs_reg) && getSealed(cap_cs) /*XXX*/ && rt_val != 0 /*XXX real CMOVE inst?*/) capTrap(s, CapExcSeal, cs);
  else if (!canRepOffset(cap_cs, getOffset(cap_cs) + zeroExtend(rt_val))) begin
    RawCap n_cap = nullCap;
    s.wCR(cd, Data(pack(setOffset(n_cap, getBase(cap_cs) + getOffset(cap_cs) + zeroExtend(rt_val)))));
    //XXX logInst(s.pc, fmtInstXCHERI("cincoffset", ct, cs, cd));
  end else begin
    s.wCR(cd, Cap(setOffset(cap_cs, getOffset(cap_cs) + zeroExtend(rt_val))));
    //XXX logInst(s.pc, fmtInstXCHERI("cincoffset", ct, cs, cd));
  end
endaction;

function Action instrXCHERI_CIncOffsetImmediate(RVState s, Bit#(12) inc, Bit#(5) cs, Bit#(5) cd) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  if (isCap(cs_reg) && getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else if (!canRepOffset(cap_cs, getOffset(cap_cs) + signExtend(inc))) begin
    RawCap n_cap = nullCap;
    s.wCR(cd, Data(pack(setOffset(n_cap, getBase(cap_cs) + getOffset(cap_cs) + signExtend(inc)))));
    //XXX logInst(s.pc, fmtInstXCHERI("cincoffsetimmediate", ct, cs, cd));
  end else begin
    s.wCR(cd, Cap(setOffset(cap_cs, getOffset(cap_cs) + signExtend(inc))));
    //XXX logInst(s.pc, fmtInstXCHERI("cincoffsetimmediate", ct, cs, cd));
  end
endaction;

function Action instrXCHERI_CSetBounds(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  let rt_val = s.rGPR(rt);
  let addr = getBase(cap_cs) + getOffset(cap_cs);
  if (!isCap(cs_reg)) capTrap(s, CapExcTag, cs);
  else if (getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else if (addr < getBase(cap_cs)) capTrap(s, CapExcLength, cs);
  else if (addr + zeroExtend(rt_val) > getBase(cap_cs) + getLength(cap_cs)) capTrap(s, CapExcLength, cs);
  else begin
    s.wCR(cd, Cap(setBounds(cap_cs, rt_val)));
    //XXX logInst(s.pc, fmtInstXCHERI("csetbounds", ct, cs, cd));
  end
endaction;

function Action instrXCHERI_CSetBoundsExact(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  let rt_val = s.rGPR(rt);
  let addr = getBase(cap_cs) + getOffset(cap_cs);
  if (!isCap(cs_reg)) capTrap(s, CapExcTag, cs);
  else if (getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else if (addr < getBase(cap_cs)) capTrap(s, CapExcLength, cs);
  else if (addr + zeroExtend(rt_val) > getBase(cap_cs) + getLength(cap_cs)) capTrap(s, CapExcLength, cs);
  else if (!canRepBounds(cap_cs, rt_val)) capTrap(s, CapExcInexact, cs);
  else begin
    s.wCR(cd, Cap(setBounds(cap_cs, rt_val)));
    //XXX logInst(s.pc, fmtInstXCHERI("csetboundsexact", ct, cs, cd));
  end
endaction;

function Action instrXCHERI_CSetBoundsImmediate(RVState s, Bit#(12) req_length, Bit#(5) cs, Bit#(5) cd) = action //XXX wrong way around in isa doc
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  let addr = getBase(cap_cs) + getOffset(cap_cs);
  if (!isCap(cs_reg)) capTrap(s, CapExcTag, cs);
  else if (getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else if (addr < getBase(cap_cs)) capTrap(s, CapExcLength, cs);
  else if (addr + zeroExtend(req_length) > getBase(cap_cs) + getLength(cap_cs)) capTrap(s, CapExcLength, cs);
  else begin
    s.wCR(cd, Cap(setBounds(cap_cs, zeroExtend(req_length))));
    //XXX logInst(s.pc, fmtInstXCHERI("csetboundimmediate", ct, cs, cd));
  end
endaction;

function Action instrXCHERI_CClearTag(RVState s, Bit#(5) cs, Bit#(5) cd) = action
  s.wCR(cd, Data(s.rCR(cs).Data));
  //XXX logInst(s.pc, fmtInstXCHERI("ccleartag", ct, cs, cd));
endaction;

function Action instrXCHERI_CBuildCap(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) cd) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  let cap_ct = s.rCR(ct).Cap;
  if (!isCap(cs_reg)) capTrap(s, CapExcTag, cs);
  else if (getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else if (getBase(cap_ct) < getBase(cap_cs)) capTrap(s, CapExcLength, cs);
  else if (getBase(cap_ct) + getLength(cap_ct) > getBase(cap_cs) + getLength(cap_cs)) capTrap(s, CapExcLength, cs);
  else if (getLength(cap_ct) < 0) capTrap(s, CapExcLength, ct);
  else if ((getPerms(cap_ct) & getPerms(cap_cs)) != getPerms(cap_ct)) capTrap(s, CapExcUser, cs);
  else if ((getUPerms(cap_ct) & getUPerms(cap_cs)) != getUPerms(cap_ct)) capTrap(s, CapExcUser, cs);
  else begin
    RawCap new_cap = setOffset(cap_cs, getBase(cap_ct) - getBase(cap_cs));
    new_cap = setBounds(new_cap, truncate(getLength(cap_ct)));
    new_cap = setPerms(new_cap, getPerms(cap_ct));
    new_cap = setUPerms(new_cap, getUPerms(cap_ct));
    new_cap = setOffset(new_cap, getOffset(cap_ct));
    new_cap = setSealed(new_cap, False);
    s.wCR(cd, Cap(new_cap));
    //XXX logInst(s.pc, fmtInstXCHERI("cbuildcap", ct, cs, cd));
  end
endaction;

function Action instrXCHERI_CCopyType(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) cd) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  let cap_ct = s.rCR(ct).Cap;
  if (!isCap(cs_reg)) capTrap(s, CapExcTag, cs);
  else if (getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else if (!getSealed(cap_ct)) s.wCR(cd, Cap(setOffset(nullCap, ~0)));
  else if (zeroExtend(getType(cap_ct)) < getBase(cap_cs)) capTrap(s, CapExcLength, cs);
  else if (zeroExtend(getType(cap_ct)) >= getBase(cap_cs) + getLength(cap_cs)) capTrap(s, CapExcLength, cs);
  else begin
    s.wCR(cd, Cap(setOffset(cap_cs, zeroExtend(getType(cap_ct)) - getBase(cap_cs))));
    //XXX logInst(s.pc, fmtInstXCHERI("ccopytype", ct, cs, cd));
  end
endaction;

function Action instrXCHERI_CCSeal(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) cd) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  let ct_reg = s.rCR(ct);
  let cap_ct = ct_reg.Cap;
  Bit#(CC_ADDR) negOne = ~0;
  if (!isCap(cs_reg)) capTrap(s, CapExcTag, cs);
  else if (!isCap(ct_reg)) s.wCR(cd, Cap(cap_cs));
  else if (truncate(getBase(cap_ct) + getOffset(cap_ct)) == negOne) s.wCR(cd, Cap(cap_cs));
  else if (getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else if (getSealed(cap_ct)) capTrap(s, CapExcSeal, ct);
  else if (!getPerms(cap_ct).permitSeal) capTrap(s, CapExcPermSeal, ct);
  else if (getOffset(cap_ct) >= getLength(cap_ct)) capTrap(s, CapExcLength, ct);
  else if (getBase(cap_ct) + getOffset(cap_ct) > zeroExtend(otypeMax)) capTrap(s, CapExcLength, ct); //XXX large ineq in spec
  else if (!canRepSealed(cap_cs, True)) capTrap(s, CapExcInexact, cs);
  else begin
    let new_cap = setSealed(cap_cs, True);
    new_cap = setType(new_cap, truncate(getBase(cap_ct) + getOffset(cap_ct)));
    s.wCR(cd, Cap(new_cap));
    //XXX logInst(s.pc, fmtInstXCHERI("ccseal", ct, cs, cd));
  end
endaction;

// Pointer-Arithmetic instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXCHERI_CToPtr(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) rd) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  let ct_reg = s.rCR(ct);
  let cap_ct = ct_reg.Cap;
  if (!isCap(ct_reg)) capTrap(s, CapExcTag, ct);
  else if (!isCap(cs_reg)) s.wGPR(rd, 0);
  else if (getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else begin
    s.wGPR(rd, truncate(getBase(cap_cs) + getOffset(cap_cs) - getBase(cap_ct)));
    //XXX logInst(s.pc, fmtInstXCHERI("ctoptr", ct, cs, cd));
  end
endaction;

function Action instrXCHERI_CFromPtr(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  let rt_val = s.rGPR(rt);
  RawCap n_cap = nullCap;
  if (rt_val == 0) s.wCR(cd, Data(pack(n_cap)));
  else if (!isCap(cs_reg)) capTrap(s, CapExcTag, cs);
  else if (getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else if (!canRepOffset(cap_cs, zeroExtend(rt_val))) begin
    s.wCR(cd, Data(pack(setOffset(n_cap, getBase(cap_cs) + zeroExtend(rt_val)))));
    //XXX logInst(s.pc, fmtInstXCHERI("cfromptr", ct, cs, cd));
  end else begin
    s.wCR(cd, Cap(setOffset(cap_cs, zeroExtend(rt_val))));
    //XXX logInst(s.pc, fmtInstXCHERI("cfromptr", ct, cs, cd));
  end
endaction;

function Action instrXCHERI_CMove(RVState s, Bit#(5) cs, Bit#(5) cd) = action
  s.wCR(cd, s.rCR(cs));
  //XXX logInst(s.pc, fmtInstXCHERI("cmove", ct, cs, cd));
endaction;

function Action instrXCHERI_CSpecialRW(RVState s, Bit#(5) idx, Bit#(5) cs, Bit#(5) cd) = action
  //TODO
  notImplemented("cspecialrw");
endaction;

// Control-Flow instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXCHERI_CJALR(RVState s, Bit#(5) cb, Bit#(5) cd) = action
  //TODO
  notImplemented("cjalr");
endaction;

function Action instrXCHERI_CCall(RVState s, Bit#(5) sel, Bit#(5) cb, Bit#(5) cs) = action
  //TODO
  notImplemented("ccall");
endaction;

// Assertion instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXCHERI_CCheckPerm(RVState s, Bit#(5) rt, Bit#(5) cs) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  let rt_val = s.rGPR(rt);
  Perms   rt_perms = unpack(truncate(rt_val));
  UPerms rt_uperms = rt_val[valueOf(TAdd#(SizeOf#(UPerms), SizeOf#(Perms)))-1:valueOf(SizeOf#(Perms))];
  Bit#(TSub#(XLEN, TAdd#(SizeOf#(UPerms), SizeOf#(Perms)))) rt_remain = truncateLSB(rt_val);
  if (!isCap(cs_reg)) capTrap(s, CapExcTag, cs);
  else if ((getPerms(cap_cs) & rt_perms) != rt_perms) capTrap(s, CapExcUser, cs);
  else if ((getUPerms(cap_cs) & rt_uperms) != rt_uperms) capTrap(s, CapExcUser, cs);
  else if (rt_remain != 0) capTrap(s, CapExcUser, cs);
  //XXX logInst(s.pc, fmtInstXCHERI("ccheckperm", ct, cs, cd));
endaction;

function Action instrXCHERI_CCheckType(RVState s, Bit#(5) ct, Bit#(5) cs) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  let ct_reg = s.rCR(ct);
  let cap_ct = ct_reg.Cap;
  if (!isCap(ct_reg)) capTrap(s, CapExcTag, ct);
  else if (!isCap(cs_reg)) capTrap(s, CapExcTag, cs);
  else if (!getSealed(cap_ct)) capTrap(s, CapExcSeal, ct);
  else if (!getSealed(cap_cs)) capTrap(s, CapExcSeal, cs);
  else if (getType(cap_ct) != getType(cap_cs)) capTrap(s, CapExcType, ct);
  //XXX logInst(s.pc, fmtInstXCHERI("cchecktype", ct, cs, cd));
endaction;

function Action instrXCHERI_CTestSubset(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) rd) = action
  let cs_reg = s.rCR(cs);
  let cap_cs = cs_reg.Cap;
  let ct_reg = s.rCR(ct);
  let cap_ct = ct_reg.Cap;
  let isSubset = True;
  if (isCap(cs_reg) != isCap(ct_reg)) isSubset = False;
  else if (getBase(cap_ct) < getBase(cap_cs)) isSubset = False;
  else if (getBase(cap_ct) + getLength(cap_ct) > getBase(cap_cs) + getLength(cap_cs)) isSubset = False;
  else if ((getPerms(cap_ct) & getPerms(cap_cs)) != getPerms(cap_ct)) isSubset = False;
  else if ((getUPerms(cap_ct) & getUPerms(cap_cs)) != getUPerms(cap_ct)) isSubset = False;
  s.wGPR(rd, zeroExtend(pack(isSubset)));
  //XXX logInst(s.pc, fmtInstXCHERI("ctestsubset", ct, cs, cd));
endaction;

// Fast Register-Clearing instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXCHERI_Clear(RVState s, Bit#(2) q, Bit#(3) m7_5, Bit#(5) m4_0) = action
  //TODO
  notImplemented("clear");
endaction;

function Action instrXCHERI_FPClear(RVState s, Bit#(2) q, Bit#(3) m7_5, Bit#(5) m4_0) = action
  //TODO
  notImplemented("fpclear");
endaction;

////////////////////////////////////////////////////////////////////////////////

module [ISADefModule] mkRV32XCHERI#(RVState s) ();

  // Capability inspection instructions
  defineInstEntry("cgetperm",   pat(n(7'h7f), n(5'h00), v, n(3'h0), v, n(7'h5b)), instrXCHERI_CGetPerm(s));
  defineInstEntry("cgettype",   pat(n(7'h7f), n(5'h01), v, n(3'h0), v, n(7'h5b)), instrXCHERI_CGetType(s));
  defineInstEntry("cgetbase",   pat(n(7'h7f), n(5'h02), v, n(3'h0), v, n(7'h5b)), instrXCHERI_CGetBase(s));
  defineInstEntry("cgetlen",    pat(n(7'h7f), n(5'h03), v, n(3'h0), v, n(7'h5b)), instrXCHERI_CGetLen(s));
  defineInstEntry("cgettag",    pat(n(7'h7f), n(5'h04), v, n(3'h0), v, n(7'h5b)), instrXCHERI_CGetTag(s));
  defineInstEntry("cgetsealed", pat(n(7'h7f), n(5'h05), v, n(3'h0), v, n(7'h5b)), instrXCHERI_CGetSealed(s));
  defineInstEntry("cgetoffset", pat(n(7'h7f), n(5'h06), v, n(3'h0), v, n(7'h5b)), instrXCHERI_CGetOffset(s));
  defineInstEntry("cgetaddr",   pat(n(7'h7f), n(5'h0f), v, n(3'h0), v, n(7'h5b)), instrXCHERI_CGetAddr(s));

  // Capability modification instructions
  defineInstEntry("cseal"     ,      pat(n(7'h0b), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CSeal(s));
  defineInstEntry("cunseal"   ,      pat(n(7'h0c), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CUnseal(s));
  defineInstEntry("candperm"  ,      pat(n(7'h0d), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CAndPerm(s));
  defineInstEntry("csetoffset",      pat(n(7'h0f), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CSetOffset(s));
  defineInstEntry("cincoffset",      pat(n(7'h11), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CIncOffset(s));
  defineInstEntry("cincoffsetimmediate", pat(v, v, n(3'h1), v, n(7'h5b)), instrXCHERI_CIncOffsetImmediate(s));
  defineInstEntry("csetbounds",      pat(n(7'h08), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CSetBounds(s));
  defineInstEntry("csetboundsexact", pat(n(7'h09), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CSetBounds(s));
  defineInstEntry("csetboundsimmediate", pat(v, v, n(3'h2), v, n(7'h5b)), instrXCHERI_CSetBoundsImmediate(s));
  defineInstEntry("ccleartag",       pat(n(7'h7f), n(5'h0b), v, n(3'h0), v, n(7'h5b)), instrXCHERI_CClearTag(s));
  defineInstEntry("cbuildcap",       pat(n(7'h1d), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CBuildCap(s));
  defineInstEntry("ccopytype",       pat(n(7'h1e), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CCopyType(s));
  defineInstEntry("ccseal",          pat(n(7'h1f), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CCSeal(s));

  // Pointer-Arithmetic instructions
  defineInstEntry("ctoptr",     pat(n(7'h12), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CToPtr(s));
  defineInstEntry("cfromptr",   pat(n(7'h13), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CFromPtr(s));
  defineInstEntry("cmove",      pat(n(7'h7f), n(5'h0a), v, n(3'h0), v, n(7'h5b)), instrXCHERI_CMove(s));
  defineInstEntry("cspecialrw", pat(n(7'h01), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CSpecialRW(s));

  // Control-Flow instructions
  defineInstEntry("cjalr", pat(n(7'h7f), n(5'h0c), v, n(3'h0), v, n(7'h5b)), instrXCHERI_CJALR(s));
  defineInstEntry("ccall", pat(n(7'h7e), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CCall(s)); // 1F selector for CReturn

  // Assertion instructions
  defineInstEntry("ccheckperm",  pat(n(7'h7f), n(5'h08), v, n(3'h0), v, n(7'h5b)), instrXCHERI_CCheckPerm(s));
  defineInstEntry("cchecktype",  pat(n(7'h7f), n(5'h09), v, n(3'h0), v, n(7'h5b)), instrXCHERI_CCheckType(s));
  defineInstEntry("ctestsubset", pat(n(7'h20), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_CTestSubset(s));

  // Fast Register-Clearing instructions
  defineInstEntry("clear",   pat(n(7'h7f), n(5'h0d), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_Clear(s));
  defineInstEntry("fpclear", pat(n(7'h7f), n(5'h10), v, v, n(3'h0), v, n(7'h5b)), instrXCHERI_FPClear(s));

endmodule
