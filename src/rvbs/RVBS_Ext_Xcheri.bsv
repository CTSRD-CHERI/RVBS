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
import  RVBS_MemAccess :: *;

function Action notImplemented(String str) = action
  $display(str + " is not currently implemented");
  $finish(0);
endaction;

// Capability inspection instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXcheri_CGetPerm(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  let cap = s.rCR(cb);
  s.wGPR(rd, zeroExtend(getPerms(cap)));
  //XXX logInst(s.pc, fmtInstXcheri("cgetperm", rd, cb));
endaction;

function Action instrXcheri_CGetType(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  let cap = s.rCR(cb);
  if (isSealedWithType(cap)) s.wGPR(rd, zeroExtend(getType(cap)));
  else s.wGPR(rd, signExtend(getType(cap)));
  //XXX logInst(s.pc, fmtInstXcheri("cgettype", rd, cb));
endaction;

function Action instrXcheri_CGetBase(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, truncate(getBase(s.rCR(cb))));
  //XXX logInst(s.pc, fmtInstXcheri("cgetbase", rd, cb));
endaction;

function Action instrXcheri_CGetLen(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  let len = getLength(s.rCR(cb));
  if (msb(len) == 1) s.wGPR(rd, ~0);
  else s.wGPR(rd, truncate(len));
  //XXX logInst(s.pc, fmtInstXcheri("cgetlen", rd, cb));
endaction;

function Action instrXcheri_CGetTag(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, zeroExtend(pack(isValidCap(s.rCR(cb)))));
  //XXX logInst(s.pc, fmtInstXcheri("cgettag", rd, cb));
endaction;

function Action instrXcheri_CGetSealed(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, zeroExtend(pack(isSealed(s.rCR(cb)))));
  //XXX logInst(s.pc, fmtInstXcheri("cgetsealed", rd, cb));
endaction;

function Action instrXcheri_CGetOffset(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, truncate(getOffset(s.rCR(cb))));
  //XXX logInst(s.pc, fmtInstXcheri("cgetoffset", rd, cb));
endaction;

function Action instrXcheri_CGetAddr(RVState s, Bit#(5) cb, Bit#(5) rd) = action
  s.wGPR(rd, truncate(getAddr(s.rCR(cb))));
  //XXX logInst(s.pc, fmtInstXcheri("cgetaddr", rd, cb));
endaction;

// Capability modification instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXcheri_CSeal(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) cd) = action
  let cap_ct = s.rCR(ct);
  let cap_cs = s.rCR(cs);
  let new_cap = setType(cap_cs, truncate(getBase(cap_ct) + getOffset(cap_ct)));
  if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (!isValidCap(cap_ct)) raiseCapException(s, CapExcTag, ct);
  else if (isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (isSealed(cap_ct)) raiseCapException(s, CapExcSeal, ct);
  else if (!getHardPerms(cap_ct).permitSeal) raiseCapException(s, CapExcPermSeal, ct);
  else if ({0, getAddr(cap_ct)} >= getTop(cap_ct)) raiseCapException(s, CapExcLength, ct);
  else if (getAddr(cap_ct) > zeroExtend(otypeMax)) raiseCapException(s, CapExcLength, ct); //XXX large ineq in spec
  else if (!new_cap.exact) raiseCapException(s, CapExcInexact, cs);
  else begin
    s.wCR(cd, new_cap.value);
    //XXX logInst(s.pc, fmtInstXcheri("cseal", ct, cs, cd));
  end
endaction;

function Action instrXcheri_CUnseal(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) cd) = action
  let cap_ct = s.rCR(ct);
  let cap_cs = s.rCR(cs);
  if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (!isValidCap(cap_ct)) raiseCapException(s, CapExcTag, ct);
  else if (!isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (isSealed(cap_ct)) raiseCapException(s, CapExcSeal, ct);
  else if (getBase(cap_ct) + getOffset(cap_ct) != zeroExtend(getType(cap_cs))) raiseCapException(s, CapExcType, ct);
  else if (!getHardPerms(cap_ct).permitUnseal) raiseCapException(s, CapExcPermUnseal, ct);
  else if ({0, getAddr(cap_ct)} >= getTop(cap_ct)) raiseCapException(s, CapExcLength, ct);
  else begin
    let new_cap  = setType(cap_cs, -1).value;
    HardPerms p  = getHardPerms(new_cap);
    p.global = p.global && getHardPerms(cap_ct).global;
    new_cap  = setHardPerms(new_cap, p);
    s.wCR(cd, new_cap);
    //XXX logInst(s.pc, fmtInstXcheri("cunseal", ct, cs, cd));
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
    //XXX logInst(s.pc, fmtInstXcheri("candperms", ct, cs, cd));
  end
endaction;

function Action instrXcheri_CSetOffset(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
  let rt_val = s.rGPR(rt);
  let new_cap = setOffset(cap_cs, zeroExtend(rt_val));
  if (isValidCap(cap_cs) && isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (!new_cap.exact) begin
    s.wCR(cd, nullWithAddr(truncate(getBase(cap_cs)) + rt_val));
    //XXX logInst(s.pc, fmtInstXcheri("csetoffset", ct, cs, cd));
  end else begin
    s.wCR(cd, new_cap.value);
    //XXX logInst(s.pc, fmtInstXcheri("csetoffset", ct, cs, cd));
  end
endaction;

function Action instrXcheri_CIncOffset(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
  let rt_val = s.rGPR(rt);
  let new_cap = setOffset(cap_cs, getOffset(cap_cs) + zeroExtend(rt_val));
  if (isValidCap(cap_cs) && isSealed(cap_cs) /*XXX*/ && rt_val != 0 /*XXX real CMOVE inst?*/) raiseCapException(s, CapExcSeal, cs);
  else if (!new_cap.exact) begin
    CapType n_cap = nullCap;
    s.wCR(cd, nullWithAddr(truncate(getBase(cap_cs) + getOffset(cap_cs) + zeroExtend(rt_val))));
    //XXX logInst(s.pc, fmtInstXcheri("cincoffset", ct, cs, cd));
  end else begin
    s.wCR(cd, new_cap.value);
    //XXX logInst(s.pc, fmtInstXcheri("cincoffset", ct, cs, cd));
  end
endaction;

function Action instrXcheri_CIncOffsetImmediate(RVState s, Bit#(12) inc, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
  let new_cap = setOffset(cap_cs, getOffset(cap_cs) + signExtend(inc));
  if (isValidCap(cap_cs) && isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (!new_cap.exact) begin
    s.wCR(cd, nullWithAddr(truncate(getBase(cap_cs) + getOffset(cap_cs) + signExtend(inc))));
    //XXX logInst(s.pc, fmtInstXcheri("cincoffsetimmediate", ct, cs, cd));
  end else begin
    s.wCR(cd, new_cap.value);
    //XXX logInst(s.pc, fmtInstXcheri("cincoffsetimmediate", ct, cs, cd));
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
    //XXX logInst(s.pc, fmtInstXcheri("csetbounds", ct, cs, cd));
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
    //XXX logInst(s.pc, fmtInstXcheri("csetboundsexact", ct, cs, cd));
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
    //XXX logInst(s.pc, fmtInstXcheri("csetboundimmediate", ct, cs, cd));
  end
endaction;

function Action instrXcheri_CClearTag(RVState s, Bit#(5) cs, Bit#(5) cd) = action
  s.wCR(cd, setValidCap(s.rCR(cs), False));
  //XXX logInst(s.pc, fmtInstXcheri("ccleartag", ct, cs, cd));
endaction;

function Action instrXcheri_CBuildCap(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
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
    //XXX logInst(s.pc, fmtInstXcheri("cbuildcap", ct, cs, cd));
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
    //XXX logInst(s.pc, fmtInstXcheri("ccopytype", ct, cs, cd));
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
  else if ({0, getAddr(cap_ct)} >= getTop(cap_ct)) raiseCapException(s, CapExcLength, ct);
  else if (getAddr(cap_ct) > zeroExtend(otypeMax)) raiseCapException(s, CapExcLength, ct); //XXX large ineq in spec
  else if (!new_cap.exact) raiseCapException(s, CapExcInexact, cs);
  else begin
    s.wCR(cd, new_cap.value);
    //XXX logInst(s.pc, fmtInstXcheri("ccseal", ct, cs, cd));
  end
endaction;

// Pointer-Arithmetic instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXcheri_CToPtr(RVState s, Bit#(5) ct, Bit#(5) cs, Bit#(5) rd) = action
  let cap_cs = s.rCR(cs);
  let cap_ct = s.rCR(ct);
  if (!isValidCap(cap_ct)) raiseCapException(s, CapExcTag, ct);
  else if (!isValidCap(cap_cs)) s.wGPR(rd, 0);
  else if (isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else begin
    s.wGPR(rd, truncate(getBase(cap_cs) + getOffset(cap_cs) - getBase(cap_ct)));
    //XXX logInst(s.pc, fmtInstXcheri("ctoptr", ct, cs, cd));
  end
endaction;

function Action instrXcheri_CFromPtr(RVState s, Bit#(5) rt, Bit#(5) cs, Bit#(5) cd) = action
  let cap_cs = s.rCR(cs);
  let rt_val = s.rGPR(rt);
  let new_cap = setOffset(cap_cs, zeroExtend(rt_val));
  if (rt_val == 0) s.wCR(cd, nullCap);
  else if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (isSealed(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (!new_cap.exact) begin
    s.wCR(cd, nullWithAddr(truncate(getBase(cap_cs)) + rt_val));
    //XXX logInst(s.pc, fmtInstXcheri("cfromptr", ct, cs, cd));
  end else begin
    s.wCR(cd, new_cap.value);
    //XXX logInst(s.pc, fmtInstXcheri("cfromptr", ct, cs, cd));
  end
endaction;

function Action instrXcheri_CMove(RVState s, Bit#(5) cs, Bit#(5) cd) = action
  s.wCR(cd, s.rCR(cs));
  //XXX logInst(s.pc, fmtInstXcheri("cmove", ct, cs, cd));
endaction;

function Action instrXcheri_CSpecialRW(RVState s, Bit#(5) idx, Bit#(5) cs, Bit#(5) cd) = action
  case (s.getCSpecial(idx)) matches
    tagged Valid .cspecial: begin
      if (cd != 0) s.wCR(cd, cspecial);
      if (cs != 0) cspecial <= s.rCR(cs);
    end
    default: raiseException(s, IllegalInst);
  endcase
  //XXX logInst(s.pc, fmtInstXcheri("cspecialrw", idx, cs, cd));
endaction;

// Control-Flow instructions
////////////////////////////////////////////////////////////////////////////////

function Action instrXcheri_CJALR(RVState s, Bit#(5) cb, Bit#(5) cd) = action
  //TODO
  notImplemented("cjalr");
endaction;

function Action instrXcheri_CCall(RVState s, Bit#(5) cb, Bit#(5) cs, Bit#(5) sel) = action
  //TODO
  notImplemented("ccall");
endaction;

// Assertion instructions
////////////////////////////////////////////////////////////////////////////////

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
  //XXX logInst(s.pc, fmtInstXcheri("ccheckperm", ct, cs, cd));
endaction;

function Action instrXcheri_CCheckType(RVState s, Bit#(5) ct, Bit#(5) cs) = action
  let cap_cs = s.rCR(cs);
  let cap_ct = s.rCR(ct);
  if (!isValidCap(cap_ct)) raiseCapException(s, CapExcTag, ct);
  else if (!isValidCap(cap_cs)) raiseCapException(s, CapExcTag, cs);
  else if (!isSealedWithType(cap_ct)) raiseCapException(s, CapExcSeal, ct);
  else if (!isSealedWithType(cap_cs)) raiseCapException(s, CapExcSeal, cs);
  else if (getType(cap_ct) != getType(cap_cs)) raiseCapException(s, CapExcType, ct);
  //XXX logInst(s.pc, fmtInstXcheri("cchecktype", ct, cs, cd));
endaction;

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
  //XXX logInst(s.pc, fmtInstXcheri("ctestsubset", ct, cs, cd));
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

function Recipe ddcCheriLoad(RVState s, LoadArgs args, Bit#(5) rs1, Bit#(5) rd_cd);
  if (args.numBytes == 16) return readCap(s, args, s.rGPR(rs1), rd_cd);
  else return readData(s, args, s.rGPR(rs1), rd_cd);
endfunction

function Recipe ddcCheriStore(RVState s, StrArgs args, Bit#(5) rs2_cs, Bit#(5) rs1);
  if (args.numBytes == 16) return writeCap(s, args, s.rCR(rs2_cs), s.rGPR(rs1));
  else return writeData(s, args, zeroExtend(s.rGPR(rs2_cs)), s.rGPR(rs1));
endfunction

function Recipe capCheriLoad(RVState s, LoadArgs args, Bit#(5) cb, Bit#(5) rd_cd);
  if (args.numBytes == 16) return capReadCap(s, args, cb, rd_cd);
  else return capReadData(s, args, cb, rd_cd);
endfunction

function Recipe capCheriStore(RVState s, StrArgs args, Bit#(5) rs2_cs, Bit#(5) cb);
  if (args.numBytes == 16) return capWriteCap(s, args, s.rCR(rs2_cs), cb);
  else return capWriteData(s, args, zeroExtend(s.rGPR(rs2_cs)), cb);
endfunction

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
  defineInstEntry("cgetaddr",   pat(n(7'h7f), n(5'h0f), v, n(3'h0), v, n(7'h5b)), instrXcheri_CGetAddr(s));

  // Capability modification instructions
  defineInstEntry("cseal"     ,      pat(n(7'h0b), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CSeal(s));
  defineInstEntry("cunseal"   ,      pat(n(7'h0c), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CUnseal(s));
  defineInstEntry("candperm"  ,      pat(n(7'h0d), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CAndPerm(s));
  defineInstEntry("csetoffset",      pat(n(7'h0f), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CSetOffset(s));
  defineInstEntry("cincoffset",      pat(n(7'h11), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CIncOffset(s));
  defineInstEntry("cincoffsetimmediate", pat(v, v, n(3'h1), v, n(7'h5b)), instrXcheri_CIncOffsetImmediate(s));
  defineInstEntry("csetbounds",      pat(n(7'h08), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CSetBounds(s));
  defineInstEntry("csetboundsexact", pat(n(7'h09), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CSetBounds(s));
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
  defineInstEntry("ccheckperm",  pat(n(7'h7f), n(5'h08), v, n(3'h0), v, n(7'h5b)), instrXcheri_CCheckPerm(s));
  defineInstEntry("cchecktype",  pat(n(7'h7f), n(5'h09), v, n(3'h0), v, n(7'h5b)), instrXcheri_CCheckType(s));
  defineInstEntry("ctestsubset", pat(n(7'h20), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_CTestSubset(s));

  // Fast Register-Clearing instructions
  defineInstEntry("clear",   pat(n(7'h7f), n(5'h0d), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_Clear(s));
  defineInstEntry("fpclear", pat(n(7'h7f), n(5'h10), v, v, n(3'h0), v, n(7'h5b)), instrXcheri_FPClear(s));

  // Memory-Stores with Explicit Address Type Instructions
  defineInstEntry("sbddc",  pat(n(7'h7c), v, v, n(3'h0), n(5'h08), n(7'h00)), ddcCheriStore(s, StrArgs{name: "sbddc", numBytes: 1}));
  defineInstEntry("shddc",  pat(n(7'h7c), v, v, n(3'h0), n(5'h09), n(7'h01)), ddcCheriStore(s, StrArgs{name: "shddc", numBytes: 2}));
  defineInstEntry("swddc",  pat(n(7'h7c), v, v, n(3'h0), n(5'h0a), n(7'h02)), ddcCheriStore(s, StrArgs{name: "swddc", numBytes: 4}));
  defineInstEntry("sdddc",  pat(n(7'h7c), v, v, n(3'h0), n(5'h0b), n(7'h03)), ddcCheriStore(s, StrArgs{name: "sdddc", numBytes: 8}));
  defineInstEntry("sqddc",  pat(n(7'h7c), v, v, n(3'h0), n(5'h0c), n(7'h04)), ddcCheriStore(s, StrArgs{name: "sqddc", numBytes: 16})); // also considers tag bit
  defineInstEntry("sbcap",  pat(n(7'h7c), v, v, n(3'h0), n(5'h18), n(7'h08)), capCheriStore(s, StrArgs{name: "sbcap", numBytes: 1}));
  defineInstEntry("shcap",  pat(n(7'h7c), v, v, n(3'h0), n(5'h19), n(7'h09)), capCheriStore(s, StrArgs{name: "shcap", numBytes: 2}));
  defineInstEntry("swcap",  pat(n(7'h7c), v, v, n(3'h0), n(5'h1a), n(7'h0a)), capCheriStore(s, StrArgs{name: "swcap", numBytes: 4}));
  defineInstEntry("sdcap",  pat(n(7'h7c), v, v, n(3'h0), n(5'h1b), n(7'h0b)), capCheriStore(s, StrArgs{name: "sdcap", numBytes: 8}));
  defineInstEntry("sqcap",  pat(n(7'h7c), v, v, n(3'h0), n(5'h1c), n(7'h0c)), capCheriStore(s, StrArgs{name: "sqcap", numBytes: 16})); // also considers tag bit
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

endmodule
