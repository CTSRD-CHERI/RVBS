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
                    imm                     src              dest
*/

  // funct3 = ADDI = 000
  // opcode = OP-IMM = 0010011
  // XXX pseudo-op: MV, NOP
  function Action instrADDI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] + signExtend(imm);
      $display("addi %d, %d, %d", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b000), v, n(7'b0010011)), instrADDI);

  // funct3 = SLTI = 010
  // opcode = OP-IMM = 0010011
  function Action instrSLTI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= signedLT(s.regFile[rs1],signExtend(imm)) ? 1 : 0;
      $display("slti %d, %d, %d", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b010), v, n(7'b0010011)), instrSLTI);

  // funct3 = SLTIU = 011
  // opcode = OP-IMM = 0010011
  // XXX pseudo-op: SEQZ
  function Action instrSLTIU (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= (s.regFile[rs1] < signExtend(imm)) ? 1 : 0;
      $display("sltiu %d, %d, %d", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b011), v, n(7'b0010011)), instrSLTIU);

  // funct3 = ANDI = 111
  // opcode = OP-IMM = 0010011
  function Action instrANDI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] & signExtend(imm);
      $display("andi %d, %d, %d", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b111), v, n(7'b0010011)), instrANDI);

  // funct3 = ORI = 110
  // opcode = OP-IMM = 0010011
  function Action instrORI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] | signExtend(imm);
      $display("ori %d, %d, %d", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b110), v, n(7'b0010011)), instrORI);

  // funct3 = XORI = 100
  // opcode = OP-IMM = 0010011
  // XXX pseudo-op: NOT
  function Action instrXORI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] ^ signExtend(imm);
      $display("xori %d, %d, %d", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b100), v, n(7'b0010011)), instrXORI);

/*
  I-type - shifts by a constant

   31              25 24              20 19    15 14    12 11     7 6        0
  +------------------+------------------+--------+--------+--------+----------+
  |     imm[11:5]    |     imm[4:0]     |   rs1  | funct3 |   rd   |  opcode  |
  +------------------+------------------+--------+--------+--------+----------+
                             shamt          src              dest
*/

  // imm[11:5] = 0000000
  // funct3 = SLLI = 001
  // opcode = OP-IMM = 0010011
  function Action instrSLLI (Bit#(5) shamt, Bit#(5) src, Bit#(5) dest) =
    action
      $display("slli %d, %d, %d", dest, src, shamt);
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0010011)), instrSLLI);

  // imm[11:5] = 0000000
  // funct3 = SRLI = 101
  // opcode = OP-IMM = 0010011
  function Action instrSRLI (Bit#(5) shamt, Bit#(5) src, Bit#(5) dest) =
    action
      $display("srli %d, %d, %d", dest, src, shamt);
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0010011)), instrSRLI);

  // imm[11:5] = 0100000
  // funct3 = SRAI = 101
  // opcode = OP-IMM = 0010011
  function Action instrSRAI (Bit#(5) shamt, Bit#(5) src, Bit#(5) dest) =
    action
      $display("srai %d, %d, %d", dest, src, shamt);
    endaction;
  defineInstr(pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0010011)), instrSRAI);

/*
  U-type

   31                                                   12 11     7 6        0
  +-------------------------------------------------------+--------+----------+
  |                       imm[31:12]                      |   rd   |  opcode  |
  +-------------------------------------------------------+--------+----------+
                              imm                            dest
*/

  // opcode = LUI = 0110111
  function Action instrLUI (Bit#(20) imm, Bit#(5) dest) =
    action
      $display("lui %d, %d", dest, imm);
    endaction;
  defineInstr(pat(v, v, n(7'b0110111)), instrLUI);

  // opcode = AUIPC = 0010111
  function Action instrAUIPC (Bit#(20) imm, Bit#(5) dest) =
    action
      $display("auipc %d, %d", dest, imm);
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
                                  src2     src1              dest
*/

  // funct7 = 0000000
  // funct3 = ADD = 000
  // opcode = OP = 0110011
  function Action instrADD (Bit#(5) src2, Bit#(5) src1, Bit#(5) dest) =
    action
      $display("add %d, %d, %d", dest, src1, src2);
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0110011)), instrADD);

  // funct7 = 0000000
  // funct3 = SLT = 010
  // opcode = OP = 0110011
  function Action instrSLT (Bit#(5) src2, Bit#(5) src1, Bit#(5) dest) =
    action
      $display("slt %d, %d, %d", dest, src1, src2);
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b010), v, n(7'b0110011)), instrSLT);

  // funct7 = 0000000
  // funct3 = SLTU = 011
  // opcode = OP = 0110011
  function Action instrSLTU (Bit#(5) src2, Bit#(5) src1, Bit#(5) dest) =
    action
      $display("sltu %d, %d, %d", dest, src1, src2);
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b011), v, n(7'b0110011)), instrSLTU);

  // funct7 = 0000000
  // funct3 = AND = 111
  // opcode = OP = 0110011
  function Action instrAND (Bit#(5) src2, Bit#(5) src1, Bit#(5) dest) =
    action
      $display("and %d, %d, %d", dest, src1, src2);
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b111), v, n(7'b0110011)), instrAND);

  // funct7 = 0000000
  // funct3 = OR = 110
  // opcode = OP = 0110011
  function Action instrOR (Bit#(5) src2, Bit#(5) src1, Bit#(5) dest) =
    action
      $display("or %d, %d, %d", dest, src1, src2);
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b110), v, n(7'b0110011)), instrOR);

  // funct7 = 0000000
  // funct3 = XOR = 100
  // opcode = OP = 0110011
  function Action instrXOR (Bit#(5) src2, Bit#(5) src1, Bit#(5) dest) =
    action
      $display("xor %d, %d, %d", dest, src1, src2);
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b100), v, n(7'b0110011)), instrXOR);

  // funct7 = 0000000
  // funct3 = SLL = 001
  // opcode = OP = 0110011
  function Action instrSLL (Bit#(5) src2, Bit#(5) src1, Bit#(5) dest) =
    action
      $display("sll %d, %d, %d", dest, src1, src2);
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0110011)), instrSLL);

  // funct7 = 0000000
  // funct3 = SRL = 101
  // opcode = OP = 0110011
  function Action instrSRL (Bit#(5) src2, Bit#(5) src1, Bit#(5) dest) =
    action
      $display("srl %d, %d, %d", dest, src1, src2);
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0110011)), instrSRL);

  // funct7 = 0100000
  // funct3 = SUB = 000
  // opcode = OP = 0110011
  function Action instrSUB (Bit#(5) src2, Bit#(5) src1, Bit#(5) dest) =
    action
      $display("sub %d, %d, %d", dest, src1, src2);
    endaction;
  defineInstr(pat(n(7'b0100000), v, v, n(3'b000), v, n(7'b0110011)), instrSUB);

  // funct7 = 0100000
  // funct3 = SRA = 101
  // opcode = OP = 0110011
  function Action instrSRA (Bit#(5) src2, Bit#(5) src1, Bit#(5) dest) =
    action
      $display("sra %d, %d, %d", dest, src1, src2);
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
                                imm                          dest
*/

  // opcode = JAL = 1101111
  function Action instrJAL (Bit#(20) imm, Bit#(5) dest) =
    action
      $display("jal %d, %d", dest, imm);
    endaction;
  defineInstr(pat(v, v, n(7'b1101111)), instrJAL);

/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
                    imm                     src              dest
*/

  // funct3 = 000
  // opcode = JALR = 1100111
  function Action instrJALR (Bit#(12) imm, Bit#(5) src, Bit#(5) dest) =
    action
      $display("jalr %d, %d", dest, src, imm);
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
