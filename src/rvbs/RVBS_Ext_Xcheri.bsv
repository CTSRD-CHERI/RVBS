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

import             BID :: *;
import          BitPat :: *;
import          Recipe :: *;

import        CHERICap :: *;

import      RVBS_Types :: *;
import       RVBS_Trap :: *;
import RVBS_TraceInsts :: *;
import RVBS_TraceUtils :: *;
import  RVBS_MemAccess :: *;
import RVBS_Base_RV32I :: *;

function Action notImplemented(String str) = action
  $display(str + " is not currently implemented");
  $finish(0);
endaction;

// Capability inspection instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXcheri_CGetPerm(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  let cap = s.rCR(cb);
  s.wGPR(rd, zeroExtend(getPerms(cap)));
  logInst(s, fmtInstXcheriSrcDst("cgetperm", GPR(rd), CR(cb)));
endaction;

function Action instrXcheri_CGetType(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  let cap = s.rCR(cb);
  if (isSealedWithType(cap)) s.wGPR(rd, zeroExtend(getType(cap)));
  else s.wGPR(rd, signExtend(getType(cap)));
  logInst(s, fmtInstXcheriSrcDst("cgettype", GPR(rd), CR(cb)));
endaction;

function Action instrXcheri_CGetBase(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, truncate(getBase(s.rCR(cb))));
  logInst(s, fmtInstXcheriSrcDst("cgetbase", GPR(rd), CR(cb)));
endaction;

function Action instrXcheri_CGetLen(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  let len = getLength(s.rCR(cb));
  if (msb(len) == 1) s.wGPR(rd, ~0);
  else s.wGPR(rd, truncate(len));
  logInst(s, fmtInstXcheriSrcDst("cgetlen", GPR(rd), CR(cb)));
endaction;

function Action instrXcheri_CGetTag(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, zeroExtend(pack(isValidCap(s.rCR(cb)))));
  logInst(s, fmtInstXcheriSrcDst("cgettag", GPR(rd), CR(cb)));
endaction;

function Action instrXcheri_CGetFlags(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, zeroExtend(getFlags(s.rCR(cb))));
  logInst(s, fmtInstXcheriSrcDst("cgetflags", GPR(rd), CR(cb)));
endaction;

function Action instrXcheri_CGetSealed(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, zeroExtend(pack(isSealed(s.rCR(cb)))));
  logInst(s, fmtInstXcheriSrcDst("cgetsealed", GPR(rd), CR(cb)));
endaction;

function Action instrXcheri_CGetOffset(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, truncate(getOffset(s.rCR(cb))));
  logInst(s, fmtInstXcheriSrcDst("cgetoffset", GPR(rd), CR(cb)));
endaction;

function Action instrXcheri_CGetAddr(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, truncate(getAddr(s.rCR(cb))));
  logInst(s, fmtInstXcheriSrcDst("cgetaddr", GPR(rd), CR(cb)));
endaction;

// Capability modification instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXcheri_CSetFlags(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
  if (isValidCap(cap_cs) && isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else begin
    s.wCR(cd, setFlags(cap_cs, truncate(s.rGPR(rt))));
    logInst(s, fmtInstXcheri3op("csetflags", CR(cd), CR(cs), GPR(rt)));
  end
endaction;

function Action instrXcheri_CSeal(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) cd) = action
  let cap_ct = s.rCR(ct);
  let cap_cs = s.rCR(cs);
  let new_cap = setType(cap_cs, truncate(getBase(cap_ct) + getOffset(cap_ct)));
  if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (!isValidCap(cap_ct)) raiseCapException(s, CapExcTag, ct);
  else if (isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (isSealed(cap_ct)) raiseCapException(s, CapExcSeal, ct);
  else if (!getHardPerms(cap_ct).permitSeal) raiseCapException(s, CapExcPermSeal, ct);
  else if (getAddr(cap_ct) < getBase(cap_ct)) raiseCapException(s, CapExcLength, ct);
  else if ({0, getAddr(cap_ct)} >= getTop(cap_ct)) raiseCapException(s, CapExcLength, ct);
  else if (getAddr(cap_ct) > zeroExtend(otypeMax)) raiseCapException(s, CapExcLength, ct); //XXX large ineq in spec
  else if (!new_cap.exact) raiseCapException(s, CapExcInexact, cs);
  else begin
    s.wCR(cd, new_cap.value);
    logInst(s, fmtInstXcheri3op("cseal", CR(cd), CR(cs), CR(ct)));
  end
endaction;

function Action instrXcheri_CUnseal(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) cd) = action
  let cap_ct = s.rCR(ct);
  let cap_cs = s.rCR(cs);
  if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (!isValidCap(cap_ct)) raiseCapException(s, CapExcTag, ct);
  else if (!isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (isSealed(cap_ct)) raiseCapException(s, CapExcSeal, ct);
  else if (getAddr(cap_ct) != zeroExtend(getType(cap_cs))) raiseCapException(s, CapExcType, ct);
  else if (!getHardPerms(cap_ct).permitUnseal) raiseCapException(s, CapExcPermUnseal, ct);
  else if (getAddr(cap_ct) < getBase(cap_ct)) raiseCapException(s, CapExcLength, ct);
  else if ({0, getAddr(cap_ct)} >= getTop(cap_ct)) raiseCapException(s, CapExcLength, ct);
  else begin
    let new_cap  = setType(cap_cs, -1).value;
    HardPerms p  = getHardPerms(new_cap);
    p.global = p.global && getHardPerms(cap_ct).global;
    new_cap  = setHardPerms(new_cap, p);
    s.wCR(cd, new_cap);
    logInst(s, fmtInstXcheri3op("cunseal", CR(cd), CR(cs), CR(ct)));
  end
endaction;

function Action instrXcheri_CAndPerm(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
  if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else begin
    let rt_val = s.rGPR(rt);
    HardPerms   newHardPerms = unpack(pack(getHardPerms(cap_cs)) & rt_val[valueOf(SizeOf#(HardPerms))-1:0]);
    SoftPerms newSoftPerms = getSoftPerms(cap_cs) & rt_val[valueOf(TAdd#(SizeOf#(SoftPerms), SizeOf#(HardPerms)))-1:valueOf(SizeOf#(HardPerms))];
    let new_cap = setHardPerms(cap_cs, newHardPerms);
    new_cap = setSoftPerms(new_cap, newSoftPerms);
    s.wCR(cd, new_cap);
    logInst(s, fmtInstXcheri3op("candperms", CR(cd), CR(cs), GPR(rt)));
  end
endaction;

function Action instrXcheri_CSetOffset(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
  let rt_val = s.rGPR(rt);
  let new_cap = setOffset(cap_cs, zeroExtend(rt_val));
  if (isValidCap(cap_cs) && isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else begin
    if (!new_cap.exact) s.wCR(cd, nullWithAddr(truncate(getBase(cap_cs)) + rt_val));
    else s.wCR(cd, new_cap.value);
    logInst(s, fmtInstXcheri3op("csetoffset", CR(cd), CR(cs), GPR(rt)));
  end
endaction;

function Action instrXcheri_CIncOffset(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
  let rt_val = s.rGPR(rt);
  let new_cap = setOffset(cap_cs, getOffset(cap_cs) + zeroExtend(rt_val));
  if (isValidCap(cap_cs) && isSealed(cap_cs) /*XXX*/ && rt_val != 0 /*XXX real CMOVE inst?*/) raiseCapException(s, CapExcSeal, cs);
  else begin
    if (!new_cap.exact) begin
      CapType n_cap = nullCap;
      s.wCR(cd, nullWithAddr(truncate(getBase(cap_cs) + getOffset(cap_cs) + zeroExtend(rt_val))));
    end else s.wCR(cd, new_cap.value);
    logInst(s, fmtInstXcheri3op("cincoffset", CR(cd), CR(cs), GPR(rt)));
  end
endaction;

function Action instrXcheri_CIncOffsetImmediate(RVState s, Bit#(12) inc, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
  let new_cap = setOffset(cap_cs, getOffset(cap_cs) + signExtend(inc));
  if (isValidCap(cap_cs) && isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else begin
    if (!new_cap.exact) s.wCR(cd, nullWithAddr(truncate(getBase(cap_cs) + getOffset(cap_cs) + signExtend(inc))));
    else s.wCR(cd, new_cap.value);
    logInst(s, $format("cincoffsetimmediate"));
  end
endaction;

function Action instrXcheri_CSetBounds(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
  let rt_val = s.rGPR(rt);
  let addr = getAddr(cap_cs);
  if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (addr < getBase(cap_cs)) raiseCapException(s, CapExcLength, cs);
  else if ({0, addr} + zeroExtend(rt_val) > getTop(cap_cs)) raiseCapException(s, CapExcLength, cs);
  else begin
    s.wCR(cd, setBounds(cap_cs, rt_val).value);
    logInst(s, fmtInstXcheri3op("csetbounds", CR(cd), CR(cs), GPR(rt)));
  end
endaction;

function Action instrXcheri_CSetBoundsExact(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
  let rt_val = s.rGPR(rt);
  let addr = getAddr(cap_cs);
  let new_cap = setBounds(cap_cs, rt_val);
  if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (addr < getBase(cap_cs)) raiseCapException(s, CapExcLength, cs);
  else if ({0, addr} + zeroExtend(rt_val) > getTop(cap_cs)) raiseCapException(s, CapExcLength, cs);
  else if (!new_cap.exact) raiseCapException(s, CapExcInexact, cs);
  else begin
    s.wCR(cd, new_cap.value);
    logInst(s, fmtInstXcheri3op("csetboundsexact", CR(cd), CR(cs), GPR(rt)));
  end
endaction;

function Action instrXcheri_CSetBoundsImmediate(RVState s, Bit#(12) req_length, Bit#(5) cs, Bit#(5) cd) = action //XXX wrong way around in isa doc
  let cap_cs = s.rCR(cs);
  let addr = getAddr(cap_cs);
  if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (addr < getBase(cap_cs)) raiseCapException(s, CapExcLength, cs);
  else if ({0, addr} + zeroExtend(req_length) > getTop(cap_cs)) raiseCapException(s, CapExcLength, cs);
  else begin
    s.wCR(cd, setBounds(cap_cs, zeroExtend(req_length)).value);
    logInst(s, $format("csetboundimmediate"));
  end
endaction;

function Action instrXcheri_CClearTag(RVState s, Bit#(5) cs, Bit#(5) cd) = action
  s.wCR(cd, setValidCap(s.rCR(cs), False));
  logInst(s, fmtInstXcheriSrcDst("ccleartag", CR(cd), CR(cs)));
endaction;

function Action instrXcheri_CBuildCap(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = (cs == 0) ? s.ddc : s.rCR(cs);
  let cap_ct = s.rCR(ct);
  if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (getBase(cap_ct) < getBase(cap_cs)) raiseCapException(s, CapExcLength, cs);
  else if (getTop(cap_ct) > getTop(cap_cs)) raiseCapException(s, CapExcLength, cs);
  else if (getLength(cap_ct) < 0) raiseCapException(s, CapExcLength, ct);
  else if ((getHardPerms(cap_ct) & getHardPerms(cap_cs)) != getHardPerms(cap_ct)) raiseCapException(s, CapExcUser, cs);
  else if ((getSoftPerms(cap_ct) & getSoftPerms(cap_cs)) != getSoftPerms(cap_ct)) raiseCapException(s, CapExcUser, cs);
  else begin
    CapType new_cap = setOffset(cap_cs, getBase(cap_ct) - getBase(cap_cs)).value;
    new_cap = setBounds(new_cap, truncate(getLength(cap_ct))).value;
    new_cap = setHardPerms(new_cap, getHardPerms(cap_ct));
    new_cap = setSoftPerms(new_cap, getSoftPerms(cap_ct));
    new_cap = setOffset(new_cap, getOffset(cap_ct)).value;
    new_cap = setType(new_cap, -1).value;
    s.wCR(cd, new_cap);
    logInst(s, fmtInstXcheri3op("cbuildcap", CR(cd), CR(cs), CR(ct)));
  end
endaction;

function Action instrXcheri_CCopyType(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
  let cap_ct = s.rCR(ct);
  if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (!isSealedWithType(cap_ct)) s.wCR(cd, nullWithAddr(~0));
  else if (zeroExtend(getType(cap_ct)) < getBase(cap_cs)) raiseCapException(s, CapExcLength, cs);
  else if (zeroExtend(getType(cap_ct)) >= getTop(cap_cs)) raiseCapException(s, CapExcLength, cs);
  else begin
    s.wCR(cd, setOffset(cap_cs, zeroExtend(getType(cap_ct)) - getBase(cap_cs)).value);
    logInst(s, fmtInstXcheri3op("ccopytype", CR(cd), CR(cs), CR(ct)));
  end
endaction;

function Action instrXcheri_CCSeal(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
  let cap_ct = s.rCR(ct);
  let new_cap = setType(cap_cs, truncate(getBase(cap_ct) + getOffset(cap_ct)));
  Bit#(CC_ADDR) negOne = ~0;
  if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (!isValidCap(cap_ct)) s.wCR(cd, cap_cs);
  else if (truncate(getBase(cap_ct) + getOffset(cap_ct)) == negOne) s.wCR(cd, cap_cs);
  else if (isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (isSealed(cap_ct)) raiseCapException(s, CapExcSeal, ct);
  else if (!getHardPerms(cap_ct).permitSeal) raiseCapException(s, CapExcPermSeal, ct);
  else if (getAddr(cap_ct) < getBase(cap_ct)) raiseCapException(s, CapExcLength, ct);
  else if ({0, getAddr(cap_ct)} >= getTop(cap_ct)) raiseCapException(s, CapExcLength, ct);
  else if (getAddr(cap_ct) > zeroExtend(otypeMax)) raiseCapException(s, CapExcLength, ct); //XXX large ineq in spec
  else if (!new_cap.exact) raiseCapException(s, CapExcInexact, cs);
  else begin
    s.wCR(cd, new_cap.value);
    logInst(s, fmtInstXcheri3op("ccseal", CR(cd), CR(cs), CR(ct)));
  end
endaction;

// Pointer-Arithmetic instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXcheri_CToPtr(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) rd) = action
  let cap_cs = s.rCR(cs);
  let cap_ct = (ct == 0) ? s.ddc : s.rCR(ct);
  if (!isValidCap(cap_ct)) raiseCapException(s, CapExcTag, ct);
  else if (!isValidCap(cap_cs)) s.wGPR(rd, 0);
  else if (isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else begin
    s.wGPR(rd, truncate(getBase(cap_cs) + getOffset(cap_cs) - getBase(cap_ct)));
    logInst(s, fmtInstXcheri3op("ctoptr", GPR(rd), CR(cs), CR(ct)));
  end
endaction;

function Action instrXcheri_CFromPtr(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = (cs == 0) ? s.ddc : s.rCR(cs);
  let rt_val = s.rGPR(rt);
  let new_cap = setOffset(cap_cs, zeroExtend(rt_val));
  if (rt_val == 0) s.wCR(cd, nullCap);
  else if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else begin
    if (!new_cap.exact) s.wCR(cd, nullWithAddr(truncate(getBase(cap_cs)) + rt_val));
    else s.wCR(cd, new_cap.value);
    logInst(s, fmtInstXcheri3op("cfromptr", CR(cd), CR(cs), GPR(rt)));
  end
endaction;

function Action instrXcheri_CMove(RVState s, Bit#(5) cs, Bit#(5) cd) = action
  s.wCR(cd, s.rCR(cs));
  logInst(s, fmtInstXcheriSrcDst("cmove", CR(cd), CR(cs)));
endaction;

function Action instrXcheri_CSpecialRW(RVState s, Bit#(5) idx, Bit#(5) cs, Bit#(5) cd) = action
  case (s.getCSpecial(idx)) matches
    tagged Valid .cspecial: begin
      if (cd != 0) s.wCR(cd, cspecial);
      if (cs != 0) cspecial <= s.rCR(cs);
      logInst(s, $format("cspecialrw"));
    end
    default: raiseException(s, IllegalInst);
  endcase
endaction;

// Control-Flow instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXcheri_CJALR(RVState s, Bit#(5) cb, Bit#(5) cd) = action
  let cap_cb = s.rCR(cb);
  let tgt = getOffset(cap_cb);
  tgt[0] = 0;
  if (!isValidCap(cap_cb)) raiseCapException(s, CapExcTag, cb);
  else if (isSealedWithType(cap_cb)) raiseCapException(s, CapExcSeal, cb);
  else if (!getHardPerms(cap_cb).permitExecute) raiseCapException(s, CapExcPermExe, cb);
  else if (getAddr(cap_cb) < getBase(cap_cb)) raiseCapException(s, CapExcLength, cb);
  else if ({0, getAddr(cap_cb) + s.instByteSz} > getTop(cap_cb)) raiseCapException(s, CapExcLength, cb);
  else if (!isInstAligned(tgt)) raiseException(s, InstAddrAlign, tgt);
  else begin
    CapType link_cap = setOffset(s.pcc, s.pc + s.instByteSz).value;
    s.pc <= tgt;
    s.pcc <= cap_cb;
    s.wCR(cd, link_cap);
    logInst(s, fmtInstXcheriSrcDst("cjalr", CR(cd), CR(cb)));
    //TODO test this
  end
endaction;

function Action instrXcheri_CCall(RVState s, Bit#(5) cb, Bit#(5) cs, Bit#(5) sel) = action
  let cap_cs = s.rCR(cs);
  let cap_cb = s.rCR(cb);
  let cs_addr = getAddr(cap_cs);
  if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (!isValidCap(cap_cb)) raiseCapException(s, CapExcTag, cb);
  else if (!isSealedWithType(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (!isSealedWithType(cap_cb)) raiseCapException(s, CapExcSeal, cb);
  else if (getType(cap_cs) != getType(cap_cb)) raiseCapException(s, CapExcType, cs);
  else if (!getHardPerms(cap_cs).permitExecute) raiseCapException(s, CapExcPermExe, cs);
  else if (getHardPerms(cap_cb).permitExecute) raiseCapException(s, CapExcPermExe, cb);
  else if (cs_addr < getBase(cap_cs)) raiseCapException(s, CapExcLength, cs);
  else if ({0, cs_addr} >= getTop(cap_cs)) raiseCapException(s, CapExcLength, cs);
  else raiseCapException(s, CapExcCall, cs);
endaction;

// Assertion instructions
////////////////////////////////////////////////////////////////////////////////

// XXX DEPRECATED
/*
function Action instrXcheri_CCheckPerm(RVState s, Bit#(5) rt, Bit#(5) cs) = action
  let cap_cs = s.rCR(cs);
  let rt_val = s.rGPR(rt);
  HardPerms   rt_perms = unpack(truncate(rt_val));
  SoftPerms rt_uperms = rt_val[valueOf(TAdd#(SizeOf#(SoftPerms), SizeOf#(HardPerms)))-1:valueOf(SizeOf#(HardPerms))];
  Bit#(TSub#(XLEN, TAdd#(SizeOf#(SoftPerms), SizeOf#(HardPerms)))) rt_remain = truncateLSB(rt_val);
  if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if ((getHardPerms(cap_cs) & rt_perms) != rt_perms) raiseCapException(s, CapExcUser, cs);
  else if ((getSoftPerms(cap_cs) & rt_uperms) != rt_uperms) raiseCapException(s, CapExcUser, cs);
  else if (rt_remain != 0) raiseCapException(s, CapExcUser, cs);
endaction;
*/

// XXX DEPRECATED
/*
function Action instrXcheri_CCheckType(RVState s, Bit#(5) ct, Bit#(5) cs) = action
  let cap_cs = s.rCR(cs);
  let cap_ct = s.rCR(ct);
  if (!isValidCap(cap_ct)) raiseCapException(s, CapExcTag, ct);
  else if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (!isSealedWithType(cap_ct)) raiseCapException(s, CapExcSeal, ct);
  else if (!isSealedWithType(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (getType(cap_ct) != getType(cap_cs)) raiseCapException(s, CapExcType, ct);
endaction;
*/

function Action instrXcheri_CTestSubset(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) rd) = action
  let cap_cs = s.rCR(cs);
  let cap_ct = s.rCR(ct);
  let isSubset = True;
  if (isValidCap(cap_cs) != isValidCap(cap_ct)) isSubset = False;
  else if (getBase(cap_ct) < getBase(cap_cs)) isSubset = False;
  else if (getTop(cap_ct) > getTop(cap_cs)) isSubset = False;
  else if ((getHardPerms(cap_ct) & getHardPerms(cap_cs)) != getHardPerms(cap_ct)) isSubset = False;
  else if ((getSoftPerms(cap_ct) & getSoftPerms(cap_cs)) != getSoftPerms(cap_ct)) isSubset = False;
  s.wGPR(rd, zeroExtend(pack(isSubset)));
  logInst(s, fmtInstXcheri3op("ctestsubset", GPR(rd), CR(cs), CR(ct)));
endaction;

// Fast Register-Clearing instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXcheri_Clear(RVState s, Bit#(2) q, Bit#(3) m7_5, Bit#(5) m4_0) = action
  //TODO
  notImplemented("clear");
endaction;

function Action instrXcheri_FPClear(RVState s, Bit#(2) q, Bit#(3) m7_5, Bit#(5) m4_0) = action
  //TODO
  notImplemented("fpclear");
endaction;

// Memory-Access with Explicit Address Type Instructions
////////////////////////////////////////////////////////////////////////////////

function Recipe ddcCheriLoad(RVState s, LoadArgs args, Bit#(5) rs1, Bit#(5) rd_cd) =
  rIfElse(args.numBytes == 16,
          readCap(s, args, s.rGPR(rs1), 0, rd_cd),
          readData(s, args, s.rGPR(rs1), 0, rd_cd));

function Recipe ddcCheriStore(RVState s, StrArgs args, Bit#(5) rs2_cs, Bit#(5) rs1) =
  rIfElse(args.numBytes == 16,
          writeCap(s, args, s.rCR(rs2_cs), s.rGPR(rs1), 0),
          writeData(s, args, zeroExtend(s.rGPR(rs2_cs)), s.rGPR(rs1), 0));

function Recipe capCheriLoad(RVState s, LoadArgs args, Bit#(5) cb, Bit#(5) rd_cd) =
  rIfElse(args.numBytes == 16,
          capReadCap(s, args, cb, 0, rd_cd),
          capReadData(s, args, cb, 0, rd_cd));

function Recipe capCheriStore(RVState s, StrArgs args, Bit#(5) rs2_cs, Bit#(5) cb) =
  rIfElse(args.numBytes == 16,
          capWriteCap(s, args, s.rCR(rs2_cs), cb, 0),
          capWriteData(s, args, zeroExtend(s.rGPR(rs2_cs)), cb, 0));

// new capability load and store instructions
////////////////////////////////////////////////////////////////////////////////

function Recipe newCapLoad(RVState s, LoadArgs args, Bit#(12) imm, Bit#(5) rs1_cs1, Bit#(5) rd_cd) =
  rIfElse(inCapMode(s.pcc), // check for current capability mode
          capReadCap(s, args, rs1_cs1, signExtend(imm), rd_cd),
          readCap(s, args, s.rGPR(rs1_cs1), signExtend(imm), rd_cd));

function Recipe newCapStore(RVState s, StrArgs args, Bit#(7) imm11_5, Bit#(5) rs2_cs2, Bit#(5) rs1_cs1, Bit#(5) imm4_0);
  Bit#(XLEN) imm = {signExtend(imm11_5), imm4_0};
  return rIfElse(inCapMode(s.pcc), // check for current capability mode
                 capWriteCap(s, args, s.rCR(rs2_cs2), rs1_cs1, signExtend(imm)),
                 writeCap(s, args, s.rCR(rs2_cs2), s.rGPR(rs1_cs1), signExtend(imm)));
endfunction

// Override standard RISC-V memory accesses
////////////////////////////////////////////////////////////////////////////////

function Recipe overrideLoad(RVState s, LoadArgs args, Bit#(12) imm, Bit#(5) rs1_cs1, Bit#(5) rd_cd) =
  rIfElse(inCapMode(s.pcc), // check for current capability mode
          capReadData(s, args, rs1_cs1, signExtend(imm), rd_cd),
          readData(s, args, s.rGPR(rs1_cs1), signExtend(imm), rd_cd));

function Recipe overrideStore(RVState s, StrArgs args, Bit#(7) imm11_5, Bit#(5) rs2_cs2, Bit#(5) rs1_cs1, Bit#(5) imm4_0);
  Bit#(XLEN) imm = {signExtend(imm11_5), imm4_0};
  return rIfElse(inCapMode(s.pcc), // check for current capability mode
                 capWriteData(s, args, tpl_2(toMem(s.rCR(rs2_cs2))), rs1_cs1, signExtend(imm)),
                 writeData(s, args, zeroExtend(s.rGPR(rs2_cs2)), s.rGPR(rs1_cs1), signExtend(imm)));
endfunction

// Override standard RISC-V auipc -> auipcc in capmode
////////////////////////////////////////////////////////////////////////////////

function Recipe overrideAUIPC(RVState s, Bit#(20) imm, Bit#(5) rd_cd) =
  // check for current capability mode
  rIfElse(inCapMode(s.pcc),
    rAct(action
           let new_cap = setOffset(s.pcc, s.pc + signExtend({imm, 12'b0}));
           if (new_cap.exact) begin
             s.wCR(rd_cd, new_cap.value);
             logInst(s, $format("auipcc"));
           end else begin
             $display("ERROR on AUIPCC, unrepresentable PCC, should never happen");
             $finish(0);
           end
         endaction),
    rAct(instrAUIPC(s, imm, rd_cd)));

////////////////////////////////////////////////////////////////////////////////

module [ISADefModule] mkExt_Xcheri#(RVState s) ();

  // Capability inspection instructions
  defineInstEntry("cgetperm",   pat(n(7'h7f), n(5'h00), v, n(3'h0), v, n(7'h5b)), instrXcheri_CGetPerm(s));
  defineInstEntry("cgettype",   pat(n(7'h7f), n(5'h01), v, n(3'h0), v, n(7'h5b)), instrXcheri_CGetType(s));
  defineInstEntry("cgetbase",   pat(n(7'h7f), n(5'h02), v, n(3'h0), v, n(7'h5b)), instrXcheri_CGetBase(s));
  defineInstEntry("cgetlen",    pat(n(7'h7f), n(5'h03), v, n(3'h0), v, n(7'h5b)), instrXcheri_CGetLen(s));
  defineInstEntry("cgettag",    pat(n(7'h7f), n(5'h04), v, n(3'h0), v, n(7'h5b)), instrXcheri_CGetTag(s));
  defineInstEntry("cgetsealed", pat(n(7'h7f), n(5'h05), v, n(3'h0), v, n(7'h5b)), instrXcheri_CGetSealed(s));
  defineInstEntry("cgetoffset", pat(n(7'h7f), n(5'h06), v, n(3'h0), v, n(7'h5b)), instrXcheri_CGetOffset(s));
  defineInstEntry("cgetflags",  pat(n(7'h7f), n(5'h07), v, n(3'h0), v, n(7'h5b)), instrXcheri_CGetFlags(s));
  defineInstEntry("cgetaddr",   pat(n(7'h7f), n(5'h0f), v, n(3'h0), v, n(7'h5b)), instrXcheri_CGetAddr(s));

  // Capability modification instructions
  defineInstEntry("cseal"     ,      pat(n(7'h0b), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CSeal(s));
  defineInstEntry("cunseal"   ,      pat(n(7'h0c), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CUnseal(s));
  defineInstEntry("candperm"  ,      pat(n(7'h0d), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CAndPerm(s));
  defineInstEntry("csetflags",       pat(n(7'h0e), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CSetFlags(s));
  defineInstEntry("csetoffset",      pat(n(7'h0f), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CSetOffset(s));
  defineInstEntry("cincoffset",      pat(n(7'h11), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CIncOffset(s));
  defineInstEntry("cincoffsetimmediate", pat(v, v, n(3'h1), v, n(7'h5b)), instrXcheri_CIncOffsetImmediate(s));
  defineInstEntry("csetbounds",      pat(n(7'h08), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CSetBounds(s));
  defineInstEntry("csetboundsexact", pat(n(7'h09), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CSetBoundsExact(s));
  defineInstEntry("csetboundsimmediate", pat(v, v, n(3'h2), v, n(7'h5b)), instrXcheri_CSetBoundsImmediate(s));
  defineInstEntry("ccleartag",       pat(n(7'h7f), n(5'h0b), v, n(3'h0), v, n(7'h5b)), instrXcheri_CClearTag(s));
  defineInstEntry("cbuildcap",       pat(n(7'h1d), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CBuildCap(s));
  defineInstEntry("ccopytype",       pat(n(7'h1e), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CCopyType(s));
  defineInstEntry("ccseal",          pat(n(7'h1f), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CCSeal(s));

  // Pointer-Arithmetic instructions
  defineInstEntry("ctoptr",     pat(n(7'h12), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CToPtr(s));
  defineInstEntry("cfromptr",   pat(n(7'h13), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CFromPtr(s));
  defineInstEntry("cmove",      pat(n(7'h7f), n(5'h0a), v, n(3'h0), v, n(7'h5b)), instrXcheri_CMove(s));
  defineInstEntry("cspecialrw", pat(n(7'h01), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CSpecialRW(s));

  // Control-Flow instructions
  defineInstEntry("cjalr", pat(n(7'h7f), n(5'h0c), v, n(3'h0), v, n(7'h5b)), instrXcheri_CJALR(s));
  defineInstEntry("ccall", pat(n(7'h7e), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CCall(s)); // 1F selector for CReturn

  // Assertion instructions
  // XXX DEPRECATED defineInstEntry("ccheckperm",  pat(n(7'h7f), n(5'h08), v, n(3'h0), v, n(7'h5b)), instrXcheri_CCheckPerm(s));
  // XXX DEPRECATED defineInstEntry("cchecktype",  pat(n(7'h7f), n(5'h09), v, n(3'h0), v, n(7'h5b)), instrXcheri_CCheckType(s));
  defineInstEntry("ctestsubset", pat(n(7'h20), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CTestSubset(s));

  // Fast Register-Clearing instructions
  defineInstEntry("clear",   pat(n(7'h7f), n(5'h0d), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_Clear(s));
  defineInstEntry("fpclear", pat(n(7'h7f), n(5'h10), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_FPClear(s));

  // Memory-Stores with Explicit Address Type Instructions
  defineInstEntry("sbddc",  pat(n(7'h7c), v, v, n(3'h0), n(5'h00), n(7'h5b)), ddcCheriStore(s, StrArgs{name: "sbddc", numBytes: 1}));
  defineInstEntry("shddc",  pat(n(7'h7c), v, v, n(3'h0), n(5'h01), n(7'h5b)), ddcCheriStore(s, StrArgs{name: "shddc", numBytes: 2}));
  defineInstEntry("swddc",  pat(n(7'h7c), v, v, n(3'h0), n(5'h02), n(7'h5b)), ddcCheriStore(s, StrArgs{name: "swddc", numBytes: 4}));
  defineInstEntry("sdddc",  pat(n(7'h7c), v, v, n(3'h0), n(5'h03), n(7'h5b)), ddcCheriStore(s, StrArgs{name: "sdddc", numBytes: 8}));
  defineInstEntry("sqddc",  pat(n(7'h7c), v, v, n(3'h0), n(5'h04), n(7'h5b)), ddcCheriStore(s, StrArgs{name: "sqddc", numBytes: 16})); // also considers tag bit
  defineInstEntry("sbcap",  pat(n(7'h7c), v, v, n(3'h0), n(5'h08), n(7'h5b)), capCheriStore(s, StrArgs{name: "sbcap", numBytes: 1}));
  defineInstEntry("shcap",  pat(n(7'h7c), v, v, n(3'h0), n(5'h09), n(7'h5b)), capCheriStore(s, StrArgs{name: "shcap", numBytes: 2}));
  defineInstEntry("swcap",  pat(n(7'h7c), v, v, n(3'h0), n(5'h0a), n(7'h5b)), capCheriStore(s, StrArgs{name: "swcap", numBytes: 4}));
  defineInstEntry("sdcap",  pat(n(7'h7c), v, v, n(3'h0), n(5'h0b), n(7'h5b)), capCheriStore(s, StrArgs{name: "sdcap", numBytes: 8}));
  defineInstEntry("sqcap",  pat(n(7'h7c), v, v, n(3'h0), n(5'h0c), n(7'h5b)), capCheriStore(s, StrArgs{name: "sqcap", numBytes: 16})); // also considers tag bit
  // Memory-Loads with Explicit Address Type Instructions
  defineInstEntry("lbddc",  pat(n(7'h7d), n(5'h00), v, n(3'h0), v, n(7'h5b)), ddcCheriLoad(s, LoadArgs{name: "lbddc",  numBytes: 1,  sgnExt: True}));
  defineInstEntry("lhddc",  pat(n(7'h7d), n(5'h01), v, n(3'h0), v, n(7'h5b)), ddcCheriLoad(s, LoadArgs{name: "lhddc",  numBytes: 2,  sgnExt: True}));
  defineInstEntry("lwddc",  pat(n(7'h7d), n(5'h02), v, n(3'h0), v, n(7'h5b)), ddcCheriLoad(s, LoadArgs{name: "lwddc",  numBytes: 4,  sgnExt: True}));
  defineInstEntry("ldddc",  pat(n(7'h7d), n(5'h03), v, n(3'h0), v, n(7'h5b)), ddcCheriLoad(s, LoadArgs{name: "ldddc",  numBytes: 8,  sgnExt: True}));
  defineInstEntry("lbuddc", pat(n(7'h7d), n(5'h04), v, n(3'h0), v, n(7'h5b)), ddcCheriLoad(s, LoadArgs{name: "lbuddc", numBytes: 1,  sgnExt: False}));
  defineInstEntry("lhuddc", pat(n(7'h7d), n(5'h05), v, n(3'h0), v, n(7'h5b)), ddcCheriLoad(s, LoadArgs{name: "lhuddc", numBytes: 2,  sgnExt: False}));
  defineInstEntry("lwuddc", pat(n(7'h7d), n(5'h06), v, n(3'h0), v, n(7'h5b)), ddcCheriLoad(s, LoadArgs{name: "lwuddc", numBytes: 4,  sgnExt: False}));
  defineInstEntry("lduddc", pat(n(7'h7d), n(5'h07), v, n(3'h0), v, n(7'h5b)), ddcCheriLoad(s, LoadArgs{name: "lduddc", numBytes: 8,  sgnExt: False}));
  defineInstEntry("lbcap",  pat(n(7'h7d), n(5'h08), v, n(3'h0), v, n(7'h5b)), capCheriLoad(s, LoadArgs{name: "lbcap",  numBytes: 1,  sgnExt: True}));
  defineInstEntry("lhcap",  pat(n(7'h7d), n(5'h09), v, n(3'h0), v, n(7'h5b)), capCheriLoad(s, LoadArgs{name: "lhcap",  numBytes: 2,  sgnExt: True}));
  defineInstEntry("lwcap",  pat(n(7'h7d), n(5'h0a), v, n(3'h0), v, n(7'h5b)), capCheriLoad(s, LoadArgs{name: "lwcap",  numBytes: 4,  sgnExt: True}));
  defineInstEntry("ldcap",  pat(n(7'h7d), n(5'h0b), v, n(3'h0), v, n(7'h5b)), capCheriLoad(s, LoadArgs{name: "ldcap",  numBytes: 8,  sgnExt: True}));
  defineInstEntry("lbucap", pat(n(7'h7d), n(5'h0c), v, n(3'h0), v, n(7'h5b)), capCheriLoad(s, LoadArgs{name: "lbucap", numBytes: 1,  sgnExt: False}));
  defineInstEntry("lhucap", pat(n(7'h7d), n(5'h0d), v, n(3'h0), v, n(7'h5b)), capCheriLoad(s, LoadArgs{name: "lhucap", numBytes: 2,  sgnExt: False}));
  defineInstEntry("lwucap", pat(n(7'h7d), n(5'h0e), v, n(3'h0), v, n(7'h5b)), capCheriLoad(s, LoadArgs{name: "lwucap", numBytes: 4,  sgnExt: False}));
  defineInstEntry("lducap", pat(n(7'h7d), n(5'h0f), v, n(3'h0), v, n(7'h5b)), capCheriLoad(s, LoadArgs{name: "lducap", numBytes: 8,  sgnExt: False}));
  defineInstEntry("lqddc",  pat(n(7'h7d), n(5'h17), v, n(3'h0), v, n(7'h5b)), ddcCheriLoad(s, LoadArgs{name: "lqddc",  numBytes: 16, sgnExt: True})); // also considers tag bit
  defineInstEntry("lqcap",  pat(n(7'h7d), n(5'h1f), v, n(3'h0), v, n(7'h5b)), capCheriLoad(s, LoadArgs{name: "lqcap",  numBytes: 16, sgnExt: True})); // also considers tag bit
  // new capability load and store instruction
  defineInstEntry("loadCap",  pat(v, v, n(3'b010), v, n(7'b0001111)), newCapLoad(s, LoadArgs{name: "loadCap",  numBytes: 16, sgnExt: True}));
  defineInstEntry("storeCap", pat(v, v, v, n(3'b100), v, n(7'b0100011)), newCapStore(s, StrArgs{name: "storeCap", numBytes: 16}));
  // Overriding existing memory loads and stores
  defineInstEntry("lb",     pat(v, v, n(3'b000), v, n(7'b0000011)), overrideLoad(s, LoadArgs{name: "lb",  numBytes: 1, sgnExt: True}));
  defineInstEntry("lbu",    pat(v, v, n(3'b100), v, n(7'b0000011)), overrideLoad(s, LoadArgs{name: "lbu", numBytes: 1, sgnExt: False}));
  defineInstEntry("lh",     pat(v, v, n(3'b001), v, n(7'b0000011)), overrideLoad(s, LoadArgs{name: "lh",  numBytes: 2, sgnExt: True}));
  defineInstEntry("lhu",    pat(v, v, n(3'b101), v, n(7'b0000011)), overrideLoad(s, LoadArgs{name: "lhu", numBytes: 2, sgnExt: False}));
  defineInstEntry("lw",     pat(v, v, n(3'b010), v, n(7'b0000011)), overrideLoad(s, LoadArgs{name: "lw",  numBytes: 4, sgnExt: True}));
  defineInstEntry("sb",     pat(v, v, v, n(3'b000), v, n(7'b0100011)), overrideStore(s, StrArgs{name: "sb", numBytes: 1}));
  defineInstEntry("sh",     pat(v, v, v, n(3'b001), v, n(7'b0100011)), overrideStore(s, StrArgs{name: "sh", numBytes: 2}));
  defineInstEntry("sw",     pat(v, v, v, n(3'b010), v, n(7'b0100011)), overrideStore(s, StrArgs{name: "sw", numBytes: 4}));
  `ifdef XLEN64
  defineInstEntry("lwu",    pat(v, v, n(3'b110), v, n(7'b0000011)), overrideLoad(s, LoadArgs{name: "lwu", numBytes: 4, sgnExt: False}));
  defineInstEntry("ld",     pat(v, v, n(3'b011), v, n(7'b0000011)), overrideLoad(s, LoadArgs{name: "ld",  numBytes: 8, sgnExt: True}));
  defineInstEntry("sd",     pat(v, v, v, n(3'b011), v, n(7'b0100011)), overrideStore(s, StrArgs{name: "sd", numBytes: 8}));
  `endif
  // Overriding existing auipc instruction
  defineInstEntry("auipc",  pat(v, v, n(7'b0010111)), overrideAUIPC(s));

endmodule
