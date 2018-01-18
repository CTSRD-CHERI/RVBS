// 2018, Alexandre Joannou, University of Cambridge

import Vector :: *;
import BitPat :: *;
import BID :: *;

import RV_Common :: *;

module [Instr32DefModule] mkRV_I#(RVArchState s, World w) ();

////////////////////////////////////////
// Integer Computational Instructions //
////////////////////////////////////////////////////////////////////////////////

/////////////////////////////////////////////
// Integer Register-Immediate Instructions //
/////////////////////////////////////////////
/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
*/

  // funct3 = ADDI = 000
  // opcode = OP-IMM = 0010011
  // XXX pseudo-op: MV, NOP
  function Action instrADDI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] + signExtend(imm);
      $display("addi %0d, %0d, %0d", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b000), v, n(7'b0010011)), instrADDI);

  // funct3 = SLTI = 010
  // opcode = OP-IMM = 0010011
  function Action instrSLTI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= signedLT(s.regFile[rs1],signExtend(imm)) ? 1 : 0;
      $display("slti %0d, %0d, %0d", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b010), v, n(7'b0010011)), instrSLTI);

  // funct3 = SLTIU = 011
  // opcode = OP-IMM = 0010011
  // XXX pseudo-op: SEQZ
  function Action instrSLTIU (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= (s.regFile[rs1] < signExtend(imm)) ? 1 : 0;
      $display("sltiu %0d, %0d, %0d", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b011), v, n(7'b0010011)), instrSLTIU);

  // funct3 = ANDI = 111
  // opcode = OP-IMM = 0010011
  function Action instrANDI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] & signExtend(imm);
      $display("andi %0d, %0d, %0d", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b111), v, n(7'b0010011)), instrANDI);

  // funct3 = ORI = 110
  // opcode = OP-IMM = 0010011
  function Action instrORI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] | signExtend(imm);
      $display("ori %0d, %0d, %0d", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b110), v, n(7'b0010011)), instrORI);

  // funct3 = XORI = 100
  // opcode = OP-IMM = 0010011
  // XXX pseudo-op: NOT
  function Action instrXORI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] ^ signExtend(imm);
      $display("xori %0d, %0d, %0d", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b100), v, n(7'b0010011)), instrXORI);

/*
  I-type - shifts by a constant

   31              25 24              20 19    15 14    12 11     7 6        0
  +------------------+------------------+--------+--------+--------+----------+
  |     imm[11:5]    |     imm[4:0]     |   rs1  | funct3 |   rd   |  opcode  |
  +------------------+------------------+--------+--------+--------+----------+
*/

  // imm[11:5] = 0000000
  // funct3 = SLLI = 001
  // opcode = OP-IMM = 0010011
  function Action instrSLLI (Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] << imm4_0;
      $display("slli %0d, %0d, %0d", rd, rs1, imm4_0);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0010011)), instrSLLI);

  // imm[11:5] = 0000000
  // funct3 = SRLI = 101
  // opcode = OP-IMM = 0010011
  function Action instrSRLI (Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] >> imm4_0;
      $display("srli %0d, %0d, %0d", rd, rs1, imm4_0);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0010011)), instrSRLI);

  // imm[11:5] = 0100000
  // funct3 = SRAI = 101
  // opcode = OP-IMM = 0010011
  function Action instrSRAI (Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= arithRightShift(s.regFile[rs1], imm4_0);
      $display("srai %0d, %0d, %0d", rd, rs1, imm4_0);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0010011)), instrSRAI);

/*
  U-type

   31                                                   12 11     7 6        0
  +-------------------------------------------------------+--------+----------+
  |                       imm[31:12]                      |   rd   |  opcode  |
  +-------------------------------------------------------+--------+----------+
*/

  // opcode = LUI = 0110111
  function Action instrLUI (Bit#(20) imm, Bit#(5) rd) =
    action
      s.regFile[rd] <= {imm, 12'b0};
      $display("lui %0d, %0d", rd, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(7'b0110111)), instrLUI);

  // opcode = AUIPC = 0010111
  function Action instrAUIPC (Bit#(20) imm, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.pc + {imm, 12'b0};
      $display("auipc %0d, %0d", rd, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(7'b0010111)), instrAUIPC);

//////////////////////////////////////////
// Integer Register-Register Operations //
//////////////////////////////////////////
/*
  R-type

   31                        25 24    20 19    15 14    12 11     7 6        0
  +----------------------------+--------+--------+--------+--------+----------+
  |           funct7           |   rs2  |   rs1  | funct3 |   rd   |  opcode  |
  +----------------------------+--------+--------+--------+--------+----------+
*/

  // funct7 = 0000000
  // funct3 = ADD = 000
  // opcode = OP = 0110011
  function Action instrADD (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] + s.regFile[rs2];
      $display("add %0d, %0d, %0d", rd, rs1, rs2);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0110011)), instrADD);

  // funct7 = 0000000
  // funct3 = SLT = 010
  // opcode = OP = 0110011
  function Action instrSLT (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= (signedLT(s.regFile[rs1], s.regFile[rs2])) ? 1 : 0;
      $display("slt %0d, %0d, %0d", rd, rs1, rs2);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b010), v, n(7'b0110011)), instrSLT);

  // funct7 = 0000000
  // funct3 = SLTU = 011
  // opcode = OP = 0110011
  function Action instrSLTU (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= (s.regFile[rs1] < s.regFile[rs2]) ? 1 : 0;
      $display("sltu %0d, %0d, %0d", rd, rs1, rs2);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b011), v, n(7'b0110011)), instrSLTU);

  // funct7 = 0000000
  // funct3 = AND = 111
  // opcode = OP = 0110011
  function Action instrAND (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] & s.regFile[rs2];
      $display("and %0d, %0d, %0d", rd, rs1, rs2);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b111), v, n(7'b0110011)), instrAND);

  // funct7 = 0000000
  // funct3 = OR = 110
  // opcode = OP = 0110011
  function Action instrOR (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] | s.regFile[rs2];
      $display("or %0d, %0d, %0d", rd, rs1, rs2);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b110), v, n(7'b0110011)), instrOR);

  // funct7 = 0000000
  // funct3 = XOR = 100
  // opcode = OP = 0110011
  function Action instrXOR (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] ^ s.regFile[rs2];
      $display("xor %0d, %0d, %0d", rd, rs1, rs2);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b100), v, n(7'b0110011)), instrXOR);

  // funct7 = 0000000
  // funct3 = SLL = 001
  // opcode = OP = 0110011
  function Action instrSLL (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] << s.regFile[rs2][4:0]; // TODO need to be derived from XLEN
      $display("sll %0d, %0d, %0d", rd, rs1, rs2);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0110011)), instrSLL);

  // funct7 = 0000000
  // funct3 = SRL = 101
  // opcode = OP = 0110011
  function Action instrSRL (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] >> s.regFile[rs2][4:0]; // TODO need to be derived from XLEN
      $display("srl %0d, %0d, %0d", rd, rs1, rs2);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0110011)), instrSRL);

  // funct7 = 0100000
  // funct3 = SUB = 000
  // opcode = OP = 0110011
  function Action instrSUB (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] - s.regFile[rs2];
      $display("sub %0d, %0d, %0d", rd, rs1, rs2);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0100000), v, v, n(3'b000), v, n(7'b0110011)), instrSUB);

  // funct7 = 0100000
  // funct3 = SRA = 101
  // opcode = OP = 0110011
  function Action instrSRA (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= arithRightShift(s.regFile[rs1], s.regFile[rs2][4:0]); // TODO need to be derived from XLEN
      $display("sra %0d, %0d, %0d", rd, rs1, rs2);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0110011)), instrSRA);

///////////////////////////////////
// Control Transfer Instructions //
////////////////////////////////////////////////////////////////////////////////

/////////////////////////
// Unconditional Jumps //
/////////////////////////

/*
  J-type

     31    30              21    20   19                12 11     7 6        0
  +-------+------------------+-------+--------------------+--------+----------+
  |imm[20]|     imm[10:1]    |imm[11]|     imm[19:12]     |   rd   |  opcode  |
  +-------+------------------+-------+--------------------+--------+----------+
*/

  // opcode = JAL = 1101111
  function Action instrJAL(Bit#(1) imm20, Bit#(10) imm10_1, Bit#(1) imm11, Bit#(8) imm19_12, Bit#(5) rd) =
    action
      Bit#(XLEN) imm = {signExtend(imm20),imm19_12,imm11,imm10_1,1'b0};
      s.pc <= s.pc + imm;
      s.regFile[rd] <= s.pc + 4;
      $display("jal %0d, %0d", rd, imm);
    endaction;
  defineInstr(pat(v, v, v, v, v, n(7'b1101111)),instrJAL);

/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
*/

  // funct3 = 000
  // opcode = JALR = 1100111
  function Action instrJALR (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      Bit#(XLEN) newPC = s.regFile[rs1] + signExtend(imm);
      newPC[0] = 0;
      s.pc <= newPC;
      s.regFile[rd] <= s.pc + 4;
      $display("jalr %d, %d", rd, rs1, imm);
    endaction;
  defineInstr(pat(v, v, n(3'b000), v, n(7'b1100111)), instrJALR);

//////////////////////////
// Conditional Branches //
//////////////////////////

/*
  B-type

     31    30      25 24    20 19    15 14    12 11       8    7    6        0
  +-------+----------+--------+--------+--------+----------+-------+----------+
  |imm[12]| imm[10:5]|   rs2  |   rs1  | funct3 | imm[4:1] |imm[11]|  opcode  |
  +-------+----------+--------+--------+--------+----------+-------+----------+
*/

//TODO BEQ
//TODO BNE
//TODO BLT
//TODO BLTU
//TODO BGE
//TODO BGEU

/////////////////////////////////
// Load and Store Instructions //
////////////////////////////////////////////////////////////////////////////////

//TODO LOAD
//TODO STORE

//////////////////
// Memory Model //
////////////////////////////////////////////////////////////////////////////////

//TODO FENCE
//TODO FENCE.I

//////////////////////////////////////////////
// Control and Status Register Instructions //
////////////////////////////////////////////////////////////////////////////////

//TODO CSRRW
/*
TODO CSRRS
RDCYCLE[H]
RDTIME[H]
RDINSTRET[H]
*/
//TODO CSRRC
//TODO CSRRWI
//TODO CSRRSI
//TODO CSRRCI

//////////////////////////////////////
// Environment Call and Breakpoints //
////////////////////////////////////////////////////////////////////////////////

//TODO ECALL
//TODO EBREAK

endmodule
