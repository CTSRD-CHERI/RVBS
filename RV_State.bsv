// 2018, Alexandre Joannou, University of Cambridge

import Vector :: *;
import BID :: *;

import RV_BasicTypes :: *;
import RV_CSRs :: *;
export RV_CSRs :: *;
export RV_State :: *;

////////////////////////////////
// RISC-V architectural state //
////////////////////////////////////////////////////////////////////////////////

// state type
typedef struct {
  Reg#(PrivLvl) currentPrivLvl;
  Reg#(Bit#(XLEN)) pc;
  Vector#(32,Reg#(Bit#(XLEN))) regFile;
  CSRs csrs;
} RVArchState;

module [ArchStateDefModule#(XLEN)] mkArchState (RVArchState);
  RVArchState s;
  s.currentPrivLvl <- mkReg(M);
  s.pc <- mkPC;
  s.regFile <- mkRegFileZ;
  s.csrs <- mkCSRs;
  return s;
endmodule

// ArchState instance
instance ArchState#(RVArchState);

  function Fmt lightReport (RVArchState s) = fullReport(s);

  function Fmt fullReport (RVArchState s);
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

endinstance

/////////////////////
// RISC-V Memories //
////////////////////////////////////////////////////////////////////////////////

typedef FullMem#(Bit#(XLEN), Bit#(InstSz), Bit#(XLEN)) RVMem;
typedef Mem#(Bit#(XLEN), Bit#(XLEN)) RVDMem;
typedef Mem#(Bit#(XLEN), Bit#(InstSz)) RVIMem;
