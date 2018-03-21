// 2018, Alexandre Joannou, University of Cambridge

import Vector :: *;
import ConfigReg :: *;
import Recipe :: *;
import BID :: *;

import RV_BasicTypes :: *;
import RV_PMP :: *;
export RV_PMP :: *;
import RV_CSRs :: *;
export RV_CSRs :: *;
export RV_State :: *;

////////////////////////////////
// RISC-V architectural state //
////////////////////////////////////////////////////////////////////////////////

// state type
typedef struct {
  Reg#(PrivLvl) currentPrivLvl;
  PC#(VAddr) pc;
  Vector#(32,Reg#(Bit#(XLEN))) regFile;
  CSRs csrs;
  PMP pmp;
  RecipeFSM fetchInst;
  Mem#(PAddr, Bit#(InstSz)) imem;
  Mem#(PAddr, Bit#(XLEN)) dmem;
} RVState;

module [Module] mkState#(Mem2#(PAddr, Bit#(InstSz), Bit#(XLEN)) mem) (RVState);
  RVState s;
  //s.currentPrivLvl <- mkReg(M);
  s.currentPrivLvl <- mkConfigReg(M);
  s.pc <- mkPC(0);
  s.regFile <- mkRegFileZ;
  s.pmp <- mkPMP(2, s.currentPrivLvl); // PMP with two lookup interfaces
  s.csrs <- mkCSRs(s.pmp);
  s.imem = mem.p0;
  s.dmem = mem.p1;
  s.fetchInst <- compile(rPar(rBlock(
    action
      PMPReq req = PMPReq{addr: toPAddr(s.pc.next), numBytes: 4, reqType: READ};
      s.pmp.lookup[0].put(req);
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
    endaction, action
      PMPRsp rsp <- s.pmp.lookup[0].get();
      MemReq#(PAddr, Bit#(InstSz)) req = tagged ReadReq {addr: rsp.addr, numBytes: 4};
      s.imem.sendReq(req);
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(rsp)));
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
    endaction)));
  return s;
endmodule

// State instance
instance State#(RVState);

  function Fmt lightReport (RVState s) = fullReport(s);
  function Fmt fullReport (RVState s);
    Fmt str = $format("regfile\n");
    for (Integer i = 0; i < 6; i = i + 1) begin
      for (Integer j = 0; j < 5; j = j + 1) begin
        Bit#(5) ridx = fromInteger(i*5+j);
        str = str + $format(rName(ridx),": 0x%8x\t", s.regFile[ridx]);
      end
      str = str + $format("\n");
    end
    str = str + $format(rName(5'd30),": 0x%8x\t", s.regFile[30]);
    str = str + $format(rName(5'd31),": 0x%8x", s.regFile[31]);
    str = str + $format("\npc = 0x%8x", s.pc);
    str = str + $format(" - privilege mode = ", fshow(s.currentPrivLvl));
    return str;
  endfunction
  function Action reqNextInst(RVState s) = s.fetchInst.start();
  function ActionValue#(Bit#(MaxInstSz)) getNextInst(RVState s) = actionvalue
    let rsp <- s.imem.getRsp();
    return case (rsp) matches
      tagged ReadRsp .val: val;
      default: ?;
    endcase;
  endactionvalue;

endinstance
