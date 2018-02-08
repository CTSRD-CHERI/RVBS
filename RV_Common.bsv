// 2018, Alexandre Joannou, University of Cambridge

import BID :: *;
import Vector :: *;

import RV_BasicTypes :: *;
export RV_BasicTypes :: *;

import RV_CSRs :: *;
export RV_CSRs :: *;

export RV_Common :: *;

////////////////////////////////
// RISC-V architectural state //
////////////////////////////////////////////////////////////////////////////////

// state type
typedef struct {
  Reg#(Bit#(n)) pc;
  Vector#(32,Reg#(Bit#(n))) regFile;
  CSRs#(n) csrs;
} RVArchState#(numeric type n);

module [ArchStateDefModule#(XLEN)] mkArchState (RVArchState#(XLEN));
  RVArchState#(XLEN) s;
  s.pc <- mkPC;
  s.regFile <- mkRegFileZ;
  s.csrs <- mkCSRs;
  return s;
endmodule

// ArchState instance
instance ArchState#(RVArchState#(n));

  function Fmt lightReport (RVArchState#(n) s);
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
    return str;
  endfunction

  function Fmt fullReport (RVArchState#(n) s);
    return (
      $format("regFile %s \n", map(readReg,s.regFile)) +
      $format("pc = 0x%0x", s.pc)
    );
  endfunction

endinstance

///////////////////
// Logging utils //
////////////////////////////////////////////////////////////////////////////////

// pretty printing and logging utils
function Fmt gpRegName(Bit#(n) r) = $format("x%0d", r);
function Fmt abiRegName(Bit#(n) r) = case (r)
  0: $format("zero");
  1: $format("ra");
  2: $format("sp");
  3: $format("gp");
  4: $format("tp");
  5, 6, 7: $format("t%0d", r - 5);
  8: $format("s0/fp");
  9: $format("s1");
  10, 11, 12, 13, 14, 15, 16, 17: $format("a%0d", r - 10);
  18, 19, 20, 21, 22, 23, 24, 25, 26, 27: $format("s%0d", r - 16);
  28, 29, 30, 31: $format("t%0d", r - 25);
endcase;
function Fmt rName(Bit#(n) r) = abiRegName(r);
/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+

function a patIType (Bit#(3) funct3, Bit#(7) opcode) =
  pat(v, v, n(funct3), v, n(opcode))
// RHS arguments: (Bit#(12) imm11_0, Bit#(5) rs1, Bit#(5) rd)

  I-type - shifts by a constant

   31              25 24              20 19    15 14    12 11     7 6        0
  +------------------+------------------+--------+--------+--------+----------+
  |     imm[11:5]    |     imm[4:0]     |   rs1  | funct3 |   rd   |  opcode  |
  +------------------+------------------+--------+--------+--------+----------+

function a patITypeShamt (Bit#(7) imm_11_5, Bit#(3) funct3, Bit#(7) opcode) =
  pat(n(imm_11_5), v, v, n(funct3), v, n(opcode))
// RHS arguments: (Bit#(7) imm11_5, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd)
*/
function Action logInstI(String i, Bit#(5) rd, Bit#(5) rs1, Bit#(12) imm) =
  printTLogPlusArgs("itrace",
    $format(i,"\t", rName(rd), ", ", rName(rs1), ", 0x%0x", imm)
  );
/*
  R-type

   31                        25 24    20 19    15 14    12 11     7 6        0
  +----------------------------+--------+--------+--------+--------+----------+
  |           funct7           |   rs2  |   rs1  | funct3 |   rd   |  opcode  |
  +----------------------------+--------+--------+--------+--------+----------+

function BitPat#() patRType (Bit#(7) funct7, Bit#(3) funct3, Bit#(7) opcode) =
  return pat(n(funct7), v, v, n(funct3), v, n(opcode))
// RHS arguments: (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd)
*/
function Action logInstR(String i, Bit#(5) rd, Bit#(5) rs1, Bit#(5) rs2) =
  printTLogPlusArgs("itrace",
    $format(i,"\t", rName(rd), ", ", rName(rs1), ", ", rs2)
  );
/*
  U-type

   31                                                   12 11     7 6        0
  +-------------------------------------------------------+--------+----------+
  |                       imm[31:12]                      |   rd   |  opcode  |
  +-------------------------------------------------------+--------+----------+

function a patUType (Bit#(7) opcode) =
  pat(v, v, n(opcode))
// RHS arguments: (Bit#(20) imm31_12, Bit#(5) rd)
*/
function Action logInstU(String i, Bit#(5) rd, Bit#(20) imm) =
  printTLogPlusArgs("itrace",
    $format(i,"\t", rName(rd), ", 0x%0x", imm)
  );
/*
  J-type

     31    30              21    20   19                12 11     7 6        0
  +-------+------------------+-------+--------------------+--------+----------+
  |imm[20]|     imm[10:1]    |imm[11]|     imm[19:12]     |   rd   |  opcode  |
  +-------+------------------+-------+--------------------+--------+----------+

function a patJType (Bit#(7) opcode) =
  pat(v, v, v, v, v, n(opcode))
// RHS arguments: (Bit#(1) imm20, Bit#(10) imm10_1, Bit#(1) imm11, Bit#(8) imm19_12, Bit#(5) rd)
*/
function Action logInstJ(String i, Bit#(5) rd, Bit#(XLEN) imm) =
  printTLogPlusArgs("itrace",
    $format(i,"\t", rName(rd), ", 0x%0x", imm)
  );
/*
  B-type

     31    30      25 24    20 19    15 14    12 11       8    7    6        0
  +-------+----------+--------+--------+--------+----------+-------+----------+
  |imm[12]| imm[10:5]|   rs2  |   rs1  | funct3 | imm[4:1] |imm[11]|  opcode  |
  +-------+----------+--------+--------+--------+----------+-------+----------+

function a patBType (Bit#(3) funct3, Bit#(7) opcode) =
  pat(v, v, v, v, n(funct3), v, v, n(opcode))
// RHS arguments: (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11)
*/
function Action logInstB(String i, Bit#(5) rs1, Bit#(5) rs2, Bit#(XLEN) imm) =
  printTLogPlusArgs("itrace",
    $format(i,"\t", rName(rs1), ", ", rName(rs2), ", 0x%0x", imm)
  );
/*
  S-type

   31                        25 24    20 19    15 14    12 11     7 6        0
  +----------------------------+--------+--------+--------+--------+----------+
  |         imm[11:5]          |   rs2  |   rs1  | funct3 |imm[4:0]|  opcode  |
  +----------------------------+--------+--------+--------+--------+----------+

function a patSType (Bit#(3) funct3, Bit#(7) opcode) =
  pat(v, v, v, n(funct3), v, n(opcode))
// RHS arguments: (Bit#(7) imm11_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) imm4_0)
*/
function Action logInstS(String i, Bit#(5) rs1, Bit#(5) rs2, Bit#(XLEN) imm) =
  printTLogPlusArgs("itrace",
    $format(i,"\t", rName(rs1), ", ", rName(rs2), ", 0x%0x", imm)
  );

/////////////////////
// RISC-V Memories //
////////////////////////////////////////////////////////////////////////////////

typedef Mem#(Bit#(XLEN), Bit#(InstSz), Bit#(XLEN)) RVMem;
typedef DMem#(Bit#(XLEN), Bit#(XLEN)) RVDMem;
typedef IMem#(Bit#(XLEN), Bit#(InstSz)) RVIMem;
module initRVMem (RVMem);
  let imem <- mkSimpleIMem(4096, "test-prog.hex");
  let dmem <- mkSimpleDMem(4096);
  interface IMem inst = imem;
  interface DMem data = dmem;
endmodule
