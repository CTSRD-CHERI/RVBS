// 2018, Alexandre Joannou, University of Cambridge

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

/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+

function a patIType (Bit#(3) funct3, Bit#(7) opcode) =
  pat(v, v, n(funct3), v, n(opcode))
// RHS arguments: (Bit#(12) imm11_0, Bit#(5) rs1, Bit#(5) rd)
*/
/*
  I-type - shifts by a constant

   31              25 24              20 19    15 14    12 11     7 6        0
  +------------------+------------------+--------+--------+--------+----------+
  |     imm[11:5]    |     imm[4:0]     |   rs1  | funct3 |   rd   |  opcode  |
  +------------------+------------------+--------+--------+--------+----------+

function a patITypeShamt (Bit#(7) imm_11_5, Bit#(3) funct3, Bit#(7) opcode) =
  pat(n(imm_11_5), v, v, n(funct3), v, n(opcode))
// RHS arguments: (Bit#(7) imm11_5, Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd)
*/

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

import BID :: *;
import Vector :: *;

///////////////////////////////////
// Utility modules and functions //
////////////////////////////////////////////////////////////////////////////////

`ifdef XLEN_VALUE
typedef XLEN_VALUE XLEN;
`else
typedef 32 XLEN;
`endif
//TODO for SLL instruction, use something like this:
// typedef TSub#(TLog#(XLEN), 1) BitShAmnt;

////////////////////////////////
// RISC-V architectural state //
////////////////////////////////////////////////////////////////////////////////

typedef struct {
  Vector#(32,Reg#(Bit#(n))) regFile;
  Reg#(Bit#(n)) pc;
} RVArchState#(numeric type n);

instance ArchState#(RVArchState);

  module [ArchStateDefModule#(n)] initArchState (RVArchState#(n));
    RVArchState#(n) s;
    s.regFile <- mkRegFileZ;
    s.pc <- mkPC;
    return s;
  endmodule

  function Fmt lightReport (RVArchState#(n) s);
    Fmt str = $format("regfile\n");
    for (Integer i = 0; i < 6; i = i + 1) begin
      for (Integer j = 0; j < 5; j = j + 1) begin
        str = str + $format("\tx%0d: 0x%8x", (i*5)+j, s.regFile[(i*5)+j]);
      end
      str = str + $format("\n");
    end
    str = str + $format("\tx%0d: 0x%8x", 30, s.regFile[30]);
    str = str + $format("\tx%0d: 0x%8x", 31, s.regFile[31]);
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

//////////////////
// RISC-V World //
////////////////////////////////////////////////////////////////////////////////

typedef struct {
  Mem#(Bit#(XLEN), Bit#(XLEN)) mem;
} RVWorld;

instance World#(RVWorld);

  module initWorld (RVWorld);
    RVWorld w;
    w.mem <- mkSimpleMem(4096);
    return w;
  endmodule

endinstance

//////////////////////////////
// RISC-V common behaviours //
////////////////////////////////////////////////////////////////////////////////

function Action pcEpilogue(RVArchState#(XLEN) s, RVWorld w) =
  action
    printTLogPlusArgs("itrace", "--------------- epilogue --------------");
    Bit#(XLEN) tmpPC = s.pc + 4;
    s.pc <= tmpPC;
    printTLogPlusArgs("itrace", $format("s.pc <= 0x%0x", tmpPC));
  endaction;
