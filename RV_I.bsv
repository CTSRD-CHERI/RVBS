// 2018, Alexandre Joannou, University of Cambridge

import Vector :: *;
import BitPat :: *;
import BID :: *;

import RV_Common :: *;

`ifdef XLEN32
module [Instr32DefModule] mkRV32I#(RVArchState s, RVDMem mem) ();

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
  function Action instrADDI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= s.regFile[rs1] + signExtend(imm);
    s.pc <= s.pc + 4;
    logInstI(s.pc, "addi", rd, rs1, imm);
  endaction;
  defineInstr("addi", pat(v, v, n(3'b000), v, n(7'b0010011)), instrADDI);

  // funct3 = SLTI = 010
  // opcode = OP-IMM = 0010011
  function Action instrSLTI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= signedLT(s.regFile[rs1],signExtend(imm)) ? 1 : 0;
    s.pc <= s.pc + 4;
    logInstI(s.pc, "slti", rd, rs1, imm);
  endaction;
  defineInstr("slti", pat(v, v, n(3'b010), v, n(7'b0010011)), instrSLTI);

  // funct3 = SLTIU = 011
  // opcode = OP-IMM = 0010011
  // XXX pseudo-op: SEQZ
  function Action instrSLTIU (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= (s.regFile[rs1] < signExtend(imm)) ? 1 : 0;
    s.pc <= s.pc + 4;
    logInstI(s.pc, "sltiu", rd, rs1, imm);
  endaction;
  defineInstr("sltiu", pat(v, v, n(3'b011), v, n(7'b0010011)), instrSLTIU);

  // funct3 = ANDI = 111
  // opcode = OP-IMM = 0010011
  function Action instrANDI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= s.regFile[rs1] & signExtend(imm);
    s.pc <= s.pc + 4;
    logInstI(s.pc, "andi", rd, rs1, imm);
  endaction;
  defineInstr("andi", pat(v, v, n(3'b111), v, n(7'b0010011)), instrANDI);

  // funct3 = ORI = 110
  // opcode = OP-IMM = 0010011
  function Action instrORI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= s.regFile[rs1] | signExtend(imm);
    s.pc <= s.pc + 4;
    logInstI(s.pc, "ori", rd, rs1, imm);
  endaction;
  defineInstr("ori", pat(v, v, n(3'b110), v, n(7'b0010011)), instrORI);

  // funct3 = XORI = 100
  // opcode = OP-IMM = 0010011
  // XXX pseudo-op: NOT
  function Action instrXORI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= s.regFile[rs1] ^ signExtend(imm);
    s.pc <= s.pc + 4;
    logInstI(s.pc, "xori", rd, rs1, imm);
  endaction;
  defineInstr("xori", pat(v, v, n(3'b100), v, n(7'b0010011)), instrXORI);

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
  function Action instrSLLI (Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= s.regFile[rs1] << imm4_0;
    s.pc <= s.pc + 4;
    logInstI(s.pc, "slli", rd, rs1, zeroExtend(imm4_0));
  endaction;
  defineInstr("slli", pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0010011)), instrSLLI);

  // imm[11:5] = 0000000
  // funct3 = SRLI = 101
  // opcode = OP-IMM = 0010011
  function Action instrSRLI (Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= s.regFile[rs1] >> imm4_0;
    s.pc <= s.pc + 4;
    logInstI(s.pc, "srli", rd, rs1, zeroExtend(imm4_0));
  endaction;
  defineInstr("srli", pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0010011)), instrSRLI);

  // imm[11:5] = 0100000
  // funct3 = SRAI = 101
  // opcode = OP-IMM = 0010011
  function Action instrSRAI (Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= arithRightShift(s.regFile[rs1], imm4_0);
    s.pc <= s.pc + 4;
    logInstI(s.pc, "srai", rd, rs1, zeroExtend(imm4_0));
  endaction;
  defineInstr("srai", pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0010011)), instrSRAI);

/*
  U-type

   31                                                   12 11     7 6        0
  +-------------------------------------------------------+--------+----------+
  |                       imm[31:12]                      |   rd   |  opcode  |
  +-------------------------------------------------------+--------+----------+
*/

  // opcode = LUI = 0110111
  function Action instrLUI (Bit#(20) imm, Bit#(5) rd) = action
    s.regFile[rd] <= signExtend({imm, 12'b0});
    s.pc <= s.pc + 4;
    logInstU(s.pc, "lui", rd, imm);
  endaction;
  defineInstr("lui", pat(v, v, n(7'b0110111)), instrLUI);

  // opcode = AUIPC = 0010111
  function Action instrAUIPC (Bit#(20) imm, Bit#(5) rd) = action
    s.regFile[rd] <= s.pc + signExtend({imm, 12'b0});
    s.pc <= s.pc + 4;
    logInstU(s.pc, "auipc", rd, imm);
  endaction;
  defineInstr("auipc", pat(v, v, n(7'b0010111)), instrAUIPC);

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
  function Action instrADD (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= s.regFile[rs1] + s.regFile[rs2];
    s.pc <= s.pc + 4;
    logInstR(s.pc, "add", rd, rs1, rs2);
  endaction;
  defineInstr("add", pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0110011)), instrADD);

  // funct7 = 0000000
  // funct3 = SLT = 010
  // opcode = OP = 0110011
  function Action instrSLT (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= (signedLT(s.regFile[rs1], s.regFile[rs2])) ? 1 : 0;
    s.pc <= s.pc + 4;
    logInstR(s.pc, "slt", rd, rs1, rs2);
  endaction;
  defineInstr("slt", pat(n(7'b0000000), v, v, n(3'b010), v, n(7'b0110011)), instrSLT);

  // funct7 = 0000000
  // funct3 = SLTU = 011
  // opcode = OP = 0110011
  function Action instrSLTU (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= (s.regFile[rs1] < s.regFile[rs2]) ? 1 : 0;
    s.pc <= s.pc + 4;
    logInstR(s.pc, "sltu", rd, rs1, rs2);
  endaction;
  defineInstr("sltu", pat(n(7'b0000000), v, v, n(3'b011), v, n(7'b0110011)), instrSLTU);

  // funct7 = 0000000
  // funct3 = AND = 111
  // opcode = OP = 0110011
  function Action instrAND (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= s.regFile[rs1] & s.regFile[rs2];
    s.pc <= s.pc + 4;
    logInstR(s.pc, "and", rd, rs1, rs2);
  endaction;
  defineInstr("and", pat(n(7'b0000000), v, v, n(3'b111), v, n(7'b0110011)), instrAND);

  // funct7 = 0000000
  // funct3 = OR = 110
  // opcode = OP = 0110011
  function Action instrOR (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= s.regFile[rs1] | s.regFile[rs2];
    s.pc <= s.pc + 4;
    logInstR(s.pc, "or", rd, rs1, rs2);
  endaction;
  defineInstr("or", pat(n(7'b0000000), v, v, n(3'b110), v, n(7'b0110011)), instrOR);

  // funct7 = 0000000
  // funct3 = XOR = 100
  // opcode = OP = 0110011
  function Action instrXOR (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= s.regFile[rs1] ^ s.regFile[rs2];
    s.pc <= s.pc + 4;
    logInstR(s.pc, "xor", rd, rs1, rs2);
  endaction;
  defineInstr("xor", pat(n(7'b0000000), v, v, n(3'b100), v, n(7'b0110011)), instrXOR);

  // funct7 = 0000000
  // funct3 = SLL = 001
  // opcode = OP = 0110011
  function Action instrSLL (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.regFile[rs2]);
    s.regFile[rd] <= s.regFile[rs1] << shiftAmnt;
    s.pc <= s.pc + 4;
    logInstR(s.pc, "sll", rd, rs1, rs2);
  endaction;
  defineInstr("sll", pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0110011)), instrSLL);

  // funct7 = 0000000
  // funct3 = SRL = 101
  // opcode = OP = 0110011
  function Action instrSRL (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.regFile[rs2]);
    s.regFile[rd] <= s.regFile[rs1] >> shiftAmnt;
    s.pc <= s.pc + 4;
    logInstR(s.pc, "srl", rd, rs1, rs2);
  endaction;
  defineInstr("srl", pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0110011)), instrSRL);

  // funct7 = 0100000
  // funct3 = SUB = 000
  // opcode = OP = 0110011
  function Action instrSUB (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= s.regFile[rs1] - s.regFile[rs2];
    s.pc <= s.pc + 4;
    logInstR(s.pc, "sub", rd, rs1, rs2);
  endaction;
  defineInstr("sub", pat(n(7'b0100000), v, v, n(3'b000), v, n(7'b0110011)), instrSUB);

  // funct7 = 0100000
  // funct3 = SRA = 101
  // opcode = OP = 0110011
  function Action instrSRA (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.regFile[rs2]);
    s.regFile[rd] <= arithRightShift(s.regFile[rs1], shiftAmnt);
    s.pc <= s.pc + 4;
    logInstR(s.pc, "sra", rd, rs1, rs2);
  endaction;
  defineInstr("sra", pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0110011)), instrSRA);

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
  function Action instrJAL(Bit#(1) imm20, Bit#(10) imm10_1, Bit#(1) imm11, Bit#(8) imm19_12, Bit#(5) rd) = action
    Bit#(XLEN) imm = {signExtend(imm20),imm19_12,imm11,imm10_1,1'b0};
    s.pc <= s.pc + imm;
    s.regFile[rd] <= s.pc + 4;
    logInstJ(s.pc, "jal", rd, imm);
  endaction;
  defineInstr("jal", pat(v, v, v, v, v, n(7'b1101111)),instrJAL);

/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
*/

  // funct3 = 000
  // opcode = JALR = 1100111
  function Action instrJALR (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
    Bit#(XLEN) newPC = s.regFile[rs1] + signExtend(imm);
    newPC[0] = 0;
    s.pc <= newPC;
    s.regFile[rd] <= s.pc + 4;
    logInstI(s.pc, "jalr", rd, rs1, imm);
  endaction;
  defineInstr("jalr", pat(v, v, n(3'b000), v, n(7'b1100111)), instrJALR);

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

  // Note from the RISC-V ISA document:
  // BGT, BGTU, BLE, and BLEU can be synthesized by reversing the operands
  // to BLT, BLTU, BGE, and BGEU, respectivelly.

  // funct3 = BEQ = 000
  // opcode = 1100011
  function Action instrBEQ (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
    Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
    if (s.regFile[rs1] == s.regFile[rs2]) s.pc <= s.pc + imm;
    else s.pc <= s.pc + 4;
    logInstB(s.pc, "beq", rs1, rs2, imm);
  endaction;
  defineInstr("beq", pat(v, v, v, v, n(3'b000), v, v, n(7'b1100011)), instrBEQ);

  // funct3 = BNE = 001
  // opcode = 1100011
  function Action instrBNE (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
    Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
    if (s.regFile[rs1] != s.regFile[rs2]) s.pc <= s.pc + imm;
    else s.pc <= s.pc + 4;
    logInstB(s.pc, "bne", rs1, rs2, imm);
  endaction;
  defineInstr("bne", pat(v, v, v, v, n(3'b001), v, v, n(7'b1100011)), instrBNE);

  // funct3 = BLT = 100
  // opcode = 1100011
  function Action instrBLT (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
    Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
    if (signedLT(s.regFile[rs1], s.regFile[rs2])) s.pc <= s.pc + imm;
    else s.pc <= s.pc + 4;
    logInstB(s.pc, "blt", rs1, rs2, imm);
  endaction;
  defineInstr("blt", pat(v, v, v, v, n(3'b100), v, v, n(7'b1100011)), instrBLT);

  // funct3 = BLTU = 110
  // opcode = 1100011
  function Action instrBLTU (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
    Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
    if (s.regFile[rs1] < s.regFile[rs2]) s.pc <= s.pc + imm;
    else s.pc <= s.pc + 4;
    logInstB(s.pc, "bltu", rs1, rs2, imm);
  endaction;
  defineInstr("bltu", pat(v, v, v, v, n(3'b110), v, v, n(7'b1100011)), instrBLTU);

  // funct3 = BGE = 101
  // opcode = 1100011
  function Action instrBGE (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
    Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
    if (signedGE(s.regFile[rs1], s.regFile[rs2])) s.pc <= s.pc + imm;
    else s.pc <= s.pc + 4;
    logInstB(s.pc, "bge", rs1, rs2, imm);
  endaction;
  defineInstr("bge", pat(v, v, v, v, n(3'b101), v, v, n(7'b1100011)), instrBGE);

  // funct3 = BGEU = 111
  // opcode = 1100011
  function Action instrBGEU (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) = action
    Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
    if (s.regFile[rs1] >= s.regFile[rs2]) s.pc <= s.pc + imm;
    else s.pc <= s.pc + 4;
    logInstB(s.pc, "bgeu", rs1, rs2, imm);
  endaction;
  defineInstr("bgeu", pat(v, v, v, v, n(3'b111), v, v, n(7'b1100011)), instrBGEU);

/////////////////////////////////
// Load and Store Instructions //
////////////////////////////////////////////////////////////////////////////////

/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
*/

  // funct3 = LB = 000
  // opcode = 0000011
  function List#(Action) instrLB(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = list(
    action
      Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
      mem.sendReq(tagged ReadReq {addr: addr, numBytes: 1});
      logInstI(s.pc, "lb(step1)", rd, rs1, imm);
    endaction,
    action
      let rsp <- mem.getRsp();
      case (rsp) matches
        tagged ReadRsp .r: begin
          Bit#(8) tmp = truncate(r);
          s.regFile[rd] <= signExtend(tmp);
        end
      endcase
      s.pc <= s.pc + 4;
      logInstI(s.pc, "lb(step2)", rd, rs1, imm);
    endaction
  );
  defineInstr("lb", pat(v, v, n(3'b000), v, n(7'b0000011)), instrLB);

  // funct3 = LBU = 100
  // opcode = 0000011
  function List#(Action) instrLBU(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = list(
    action
      Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
      mem.sendReq(tagged ReadReq {addr: addr, numBytes: 1});
      logInstI(s.pc, "lbu(step1)", rd, rs1, imm);
    endaction,
    action
      let rsp <- mem.getRsp();
      case (rsp) matches
        tagged ReadRsp .r: begin
          Bit#(8) tmp = truncate(r);
          s.regFile[rd] <= zeroExtend(tmp);
        end
      endcase
      s.pc <= s.pc + 4;
      logInstI(s.pc, "lbu(step2)", rd, rs1, imm);
    endaction
  );
  defineInstr("lbu", pat(v, v, n(3'b100), v, n(7'b0000011)), instrLBU);

  // funct3 = LH = 001
  // opcode = 0000011
  function List#(Action) instrLH(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = list(
    action
      Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
      mem.sendReq(tagged ReadReq {addr: addr, numBytes: 2});
      logInstI(s.pc, "lh(step1)", rd, rs1, imm);
    endaction,
    action
      let rsp <- mem.getRsp();
      case (rsp) matches
        tagged ReadRsp .r: begin
          Bit#(16) tmp = truncate(r);
          s.regFile[rd] <= signExtend(tmp);
        end
      endcase
      s.pc <= s.pc + 4;
      logInstI(s.pc, "lh(step2)", rd, rs1, imm);
    endaction
  );
  defineInstr("lh", pat(v, v, n(3'b001), v, n(7'b0000011)), instrLH);

  // funct3 = LHU = 101
  // opcode = 0000011
  function List#(Action) instrLHU(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = list(
    action
      Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
      mem.sendReq(tagged ReadReq {addr: addr, numBytes: 2});
      logInstI(s.pc, "lhu(step1)", rd, rs1, imm);
    endaction,
    action
      let rsp <- mem.getRsp();
      case (rsp) matches
        tagged ReadRsp .r: begin
          Bit#(18) tmp = truncate(r);
          s.regFile[rd] <= zeroExtend(tmp);
        end
      endcase
      s.pc <= s.pc + 4;
      logInstI(s.pc, "lhu(step2)", rd, rs1, imm);
    endaction
  );
  defineInstr("lhu", pat(v, v, n(3'b101), v, n(7'b0000011)), instrLHU);

  // funct3 = LW = 010
  // opcode = 0000011
  function List#(Action) instrLW(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = list(
    action
      Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
      mem.sendReq(tagged ReadReq {addr: addr, numBytes: 4});
      logInstI(s.pc, "lw(step1)", rd, rs1, imm);
    endaction,
    action
      let rsp <- mem.getRsp();
      case (rsp) matches
        tagged ReadRsp .r: begin
          Bit#(32) tmp = truncate(r);
          s.regFile[rd] <= signExtend(tmp);
        end
      endcase
      s.pc <= s.pc + 4;
      logInstI(s.pc, "lw(step2)", rd, rs1, imm);
    endaction
  );
  defineInstr("lw", pat(v, v, n(3'b010), v, n(7'b0000011)), instrLW);

/*
  S-type

   31                        25 24    20 19    15 14    12 11     7 6        0
  +----------------------------+--------+--------+--------+--------+----------+
  |         imm[11:5]          |   rs2  |   rs1  | funct3 |imm[4:0]|  opcode  |
  +----------------------------+--------+--------+--------+--------+----------+
*/

  // funct3 = SB = 000
  // opcode = 0100011
  function Action instrSB(Bit#(7) imm11_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) imm4_0) = action
    Bit#(XLEN) imm = {signExtend(imm11_5), imm4_0};
    Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
    mem.sendReq(tagged WriteReq {addr: addr, byteEnable: 'b1, data: s.regFile[rs2]});
    s.pc <= s.pc + 4;
    logInstS(s.pc, "sb", rs1, rs2, imm);
  endaction;
  defineInstr("sb", pat(v, v, v, n(3'b000), v, n(7'b0100011)), instrSB);

  // funct3 = SH = 001
  // opcode = 0100011
  function Action instrSH(Bit#(7) imm11_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) imm4_0) = action
    Bit#(XLEN) imm = {signExtend(imm11_5), imm4_0};
    Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
    mem.sendReq(tagged WriteReq {addr: addr, byteEnable: 'b11, data: s.regFile[rs2]});
    s.pc <= s.pc + 4;
    logInstS(s.pc, "sh", rs1, rs2, imm);
  endaction;
  defineInstr("sh", pat(v, v, v, n(3'b001), v, n(7'b0100011)), instrSH);

  // funct3 = SW = 010
  // opcode = 0100011
  function Action instrSW(Bit#(7) imm11_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) imm4_0) = action
    Bit#(XLEN) imm = {signExtend(imm11_5), imm4_0};
    Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
    mem.sendReq(tagged WriteReq {addr: addr, byteEnable: 'b1111, data: s.regFile[rs2]});
    s.pc <= s.pc + 4;
    logInstS(s.pc, "sw", rs1, rs2, imm);
  endaction;
  defineInstr("sw", pat(v, v, v, n(3'b010), v, n(7'b0100011)), instrSW);

//////////////////
// Memory Model //
////////////////////////////////////////////////////////////////////////////////


  // funct3 = FENCE = 000
  // opcode = 0001111
  function Action instrFENCE(Bit#(4) pred, Bit#(4) succ) = action
    //TODO
    s.pc <= s.pc + 4;
    printTLogPlusArgs("itrace", $format("pc: 0x%0x -- fence 0b%4b, 0b%4b", s.pc, pred, succ));
  endaction;
  defineInstr("fence", pat(n(4'b0000), v, v, n(5'b00000), n(3'b000), n(5'b00000), n(7'b0001111)), instrFENCE);

  // funct3 = FENCE.I = 001
  // opcode = 0001111
  function Action instrFENCE_I() = action
    //TODO
    s.pc <= s.pc + 4;
    printTLogPlusArgs("itrace", $format("pc: 0x%0x -- fence.i", s.pc));
  endaction;
  defineInstr("fence.i", pat(n(4'b0000), n(4'b0000), n(4'b0000), n(5'b00000), n(3'b001), n(5'b00000), n(7'b0001111)), instrFENCE_I);

//////////////////////////////////////////////
// Control and Status Register Instructions //
////////////////////////////////////////////////////////////////////////////////

/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
*/

  // funct3 = CSRRW = 001
  // opcode = 1110011
  // pseudo-op CSRW
  function Action instrCSRRW(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
    /* TODO
    `If rd = x0, then the instruction shall not read the CSR and shall not cause any of the side-effects that might occur on a CSR read.`
    Do the write side effect take place ?
    */
    let r = (rd == 0) ? rwCSRReqNoRead(imm, s.regFile[rs1]) : rwCSRReq(imm, s.regFile[rs1]);
    // XXX for some reason, bluespec doesn't like this way to write it:
    // s.regFile[rd] <- s.csrs.req(r);
    let val <- s.csrs.req(r);
    s.regFile[rd] <= val;
    s.pc <= s.pc + 4;
    logInstI(s.pc, "csrrw", rd, rs1, imm);
  endaction;
  defineInstr("csrrw", pat(v, v, n(3'b001), v, n(7'b1110011)), instrCSRRW);

  // funct3 = CSRRS = 010
  // opcode = 1110011
  // pseudo-op CSRR
  // XXX RDCYCLE[H], RDTIME[H], RDINSTRET[H]
  function Action instrCSRRS(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
    /* TODO
    `if rs1 = x0, then the instruction will not write to the CSR at all, and so shall not cause any of the side effects that might otherwise occur on a CSR write, such as raising illegal instruction exceptions on accesses to read-only CSR.`
    Do the read side effect take place ?
    */
    let r = (rs1 == 0) ? rsCSRReqNoWrite(imm, s.regFile[rs1]) : rsCSRReq(imm, s.regFile[rs1]);
    let val <- s.csrs.req(r);
    s.regFile[rd] <= val;
    s.pc <= s.pc + 4;
    logInstI(s.pc, "csrrs", rd, rs1, imm);
  endaction;
  defineInstr("csrrs", pat(v, v, n(3'b010), v, n(7'b1110011)), instrCSRRS);

  // funct3 = CSRRC = 011
  // opcode = 1110011
  function Action instrCSRRC(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
    /* TODO
    `if rs1 = x0, then the instruction will not write to the CSR at all, and so shall not cause any of the side effects that might otherwise occur on a CSR write, such as raising illegal instruction exceptions on accesses to read-only CSR.`
    Do the read side effect take place ?
    */
    let r = (rs1 == 0) ? rcCSRReqNoWrite(imm, s.regFile[rs1]) : rcCSRReq(imm, s.regFile[rs1]);
    let val <- s.csrs.req(r);
    s.regFile[rd] <= val;
    s.pc <= s.pc + 4;
    logInstI(s.pc, "csrrc", rd, rs1, imm);
  endaction;
  defineInstr("csrrc", pat(v, v, n(3'b011), v, n(7'b1110011)), instrCSRRC);

  // funct3 = CSRRWI = 101
  // opcode = 1110011
  function Action instrCSRRWI(Bit#(12) imm, Bit#(5) zimm, Bit#(5) rd) = action
    /* TODO
    `if rd = x0, then the instruction shall not read the CSR and shall not cause any of the side-effects that might occur on a CSR read.`
    Do the write side effect take place ?
    */
    let r = (rd == 0) ? rwCSRReqNoRead(imm, zeroExtend(zimm)) : rwCSRReq(imm, zeroExtend(zimm));
    let val <- s.csrs.req(r);
    s.regFile[rd] <= val;
    s.pc <= s.pc + 4;
    logInstI(s.pc, "csrrwi", rd, zimm, imm);
  endaction;
  defineInstr("csrrwi", pat(v, v, n(3'b101), v, n(7'b1110011)), instrCSRRWI);

  // funct3 = CSRRSI = 110
  // opcode = 1110011
  function Action instrCSRRSI(Bit#(12) imm, Bit#(5) zimm, Bit#(5) rd) = action
    /* TODO
    `if the the uimm[4:0] (zimm) field is zero, then these instructions will not write to the CSR, and shall not cause any of the side effects that might otherwise occur on a CSR write.`
    Do the read side effect take place ?
    */
    let r = (zimm == 0) ? rsCSRReqNoWrite(imm, zeroExtend(zimm)) : rsCSRReq(imm, zeroExtend(zimm));
    let val <- s.csrs.req(r);
    s.regFile[rd] <= val;
    s.pc <= s.pc + 4;
    logInstI(s.pc, "csrrsi", rd, zimm, imm);
  endaction;
  defineInstr("csrrsi", pat(v, v, n(3'b110), v, n(7'b1110011)), instrCSRRSI);

  // funct3 = CSRRCI = 111
  // opcode = 1110011
  function Action instrCSRRCI(Bit#(12) imm, Bit#(5) zimm, Bit#(5) rd) = action
    /* TODO
    `if the the uimm[4:0] (zimm) field is zero, then these instructions will not write to the CSR, and shall not cause any of the side effects that might otherwise occur on a CSR write.`
    Do the read side effect take place ?
    */
    let r = (zimm == 0) ? rcCSRReqNoWrite(imm, zeroExtend(zimm)) : rcCSRReq(imm, zeroExtend(zimm));
    let val <- s.csrs.req(r);
    s.regFile[rd] <= val;
    s.pc <= s.pc + 4;
    logInstI(s.pc, "csrrci", rd, zimm, imm);
  endaction;
  defineInstr("csrrci", pat(v, v, n(3'b111), v, n(7'b1110011)), instrCSRRCI);

//////////////////////////////////////
// Environment Call and Breakpoints //
////////////////////////////////////////////////////////////////////////////////

  // ECALL
  function Action instrECALL() = action
    //TODO
    MCause cause = case (s.currentPrivLvl)
      U: tagged Exception ECallFromU;
      S: tagged Exception ECallFromS;
      M: tagged Exception ECallFromM;
    endcase;
    trap(s, cause);
    printTLogPlusArgs("itrace", $format("pc: 0x%0x -- ecall", s.pc));
  endaction;
  defineInstr("ecall", pat(n(12'b000000000000), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrECALL);

  // EBREAK
  function Action instrEBREAK() = action
    //TODO
    printTLogPlusArgs("itrace", $format("pc: 0x%0x -- ebreak", s.pc));
  endaction;
  defineInstr("ebreak", pat(n(12'b000000000001), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrEBREAK);

/////////////////////////
// Unknown Instruction //
////////////////////////////////////////////////////////////////////////////////

  function Action unknownInst(Bit#(32) inst) = action
    // TODO
    s.pc <= s.pc + 4;
    printTLogPlusArgs("itrace", $format("pc: 0x%0x -- UNKNOWN INSTRUCTION 0x%0x", s.pc, inst));
  endaction;
  defineUnkInstr(unknownInst);

endmodule
`endif // XLEN32

////////////////////////////////////////////////////////////////////////////////

`ifdef XLEN64
module [Instr32DefModule] mkRV64I#(RVArchState s, RVDMem mem) ();

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

  // funct3 = ADDIW = 000
  // opcode = OP-IMM-32 = 0011011
  // XXX pseudo-op: SEXT.W
  function Action instrADDIW (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] + signExtend(imm));
    s.pc <= s.pc + 4;
    logInstI(s.pc, "addiw", rd, rs1, imm);
  endaction;
  defineInstr("addiw", pat(v, v, n(3'b000), v, n(7'b0011011)), instrADDIW);

/*
  I-type - shifts by a constant

   31              25 24              20 19    15 14    12 11     7 6        0
  +------------------+------------------+--------+--------+--------+----------+
  |     imm[11:5]    |     imm[4:0]     |   rs1  | funct3 |   rd   |  opcode  |
  +------------------+------------------+--------+--------+--------+----------+
*/

  // imm[11:6] = 000000
  // funct3 = SLLI = 001
  // opcode = OP-IMM = 0010011
  function Action instrSLLI (Bit#(6) imm5_0, Bit#(5) rs1, Bit#(5) rd) = action
    // TODO check MXL and imm[5] for exception in RV32I mode
    s.regFile[rd] <= s.regFile[rs1] << imm5_0;
    s.pc <= s.pc + 4;
    logInstI(s.pc, "slli", rd, rs1, zeroExtend(imm5_0));
  endaction;
  defineInstr("slli", pat(n(6'b000000), v, v, n(3'b001), v, n(7'b0010011)), instrSLLI);

  // imm[11:6] = 000000
  // funct3 = SRLI = 101
  // opcode = OP-IMM = 0010011
  function Action instrSRLI (Bit#(6) imm5_0, Bit#(5) rs1, Bit#(5) rd) = action
    // TODO check MXL and imm[5] for exception in RV32I mode
    s.regFile[rd] <= s.regFile[rs1] >> imm5_0;
    s.pc <= s.pc + 4;
    logInstI(s.pc, "srli", rd, rs1, zeroExtend(imm5_0));
  endaction;
  defineInstr("srli", pat(n(6'b000000), v, v, n(3'b101), v, n(7'b0010011)), instrSRLI);

  // imm[11:6] = 010000
  // funct3 = SRAI = 101
  // opcode = OP-IMM = 0010011
  function Action instrSRAI (Bit#(6) imm5_0, Bit#(5) rs1, Bit#(5) rd) = action
    // TODO check MXL and imm[5] for exception in RV32I mode
    s.regFile[rd] <= arithRightShift(s.regFile[rs1], imm5_0);
    s.pc <= s.pc + 4;
    logInstI(s.pc, "srai", rd, rs1, zeroExtend(imm5_0));
  endaction;
  defineInstr("srai", pat(n(6'b010000), v, v, n(3'b101), v, n(7'b0010011)), instrSRAI);

  // imm[11:5] = 0000000
  // funct3 = SLLIW = 001
  // opcode = OP-IMM-32 = 0011011
  function Action instrSLLIW (Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] << imm4_0);
    s.pc <= s.pc + 4;
    logInstI(s.pc, "slliw", rd, rs1, zeroExtend(imm4_0));
  endaction;
  defineInstr("slliw", pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0011011)), instrSLLIW);

  // imm[11:5] = 0000000
  // funct3 = SRLIW = 101
  // opcode = OP-IMM-32 = 0011011
  function Action instrSRLIW (Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] >> imm4_0);
    s.pc <= s.pc + 4;
    logInstI(s.pc, "srliw", rd, rs1, zeroExtend(imm4_0));
  endaction;
  defineInstr("srliw", pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0011011)), instrSRLIW);

  // imm[11:5] = 0100000
  // funct3 = SRAIW = 101
  // opcode = OP-IMM-32 = 0011011
  function Action instrSRAIW (Bit#(5) imm4_0, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= signExtend(arithRightShift(s.regFile[rs1][31:0], imm4_0));
    s.pc <= s.pc + 4;
    logInstI(s.pc, "sraiw", rd, rs1, zeroExtend(imm4_0));
  endaction;
  defineInstr("sraiw", pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0011011)), instrSRAIW);

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
  // funct3 = ADDW = 000
  // opcode = OP-32 = 0111011
  function Action instrADDW (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] + s.regFile[rs2][31:0]);
    s.pc <= s.pc + 4;
    logInstR(s.pc, "addw", rd, rs1, rs2);
  endaction;
  defineInstr("addw", pat(n(7'b0000000), v, v, n(3'b000), v, n(7'b0111011)), instrADDW);

  // funct7 = 0100000
  // funct3 = SUBW = 000
  // opcode = OP-32 = 0111011
  function Action instrSUBW (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] - s.regFile[rs2][31:0]);
    s.pc <= s.pc + 4;
    logInstR(s.pc, "subw", rd, rs1, rs2);
  endaction;
  defineInstr("subw", pat(n(7'b0100000), v, v, n(3'b000), v, n(7'b0111011)), instrSUBW);

  // funct7 = 0000000
  // funct3 = SLLW = 001
  // opcode = OP-32 = 0111011
  function Action instrSLLW (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    Bit#(5) shiftAmnt = truncate(s.regFile[rs2]);
    s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] << shiftAmnt);
    s.pc <= s.pc + 4;
    logInstR(s.pc, "sllw", rd, rs1, rs2);
  endaction;
  defineInstr("sllw", pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0111011)), instrSLLW);

  // funct7 = 0000000
  // funct3 = SRLW = 101
  // opcode = OP-32 = 0111011
  function Action instrSRLW (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    Bit#(5) shiftAmnt = truncate(s.regFile[rs2]);
    s.regFile[rd] <= signExtend(s.regFile[rs1][31:0] >> shiftAmnt);
    s.pc <= s.pc + 4;
    logInstR(s.pc, "srlw", rd, rs1, rs2);
  endaction;
  defineInstr("srlw", pat(n(7'b0000000), v, v, n(3'b101), v, n(7'b0111011)), instrSRLW);

  // funct7 = 0100000
  // funct3 = SRAW = 101
  // opcode = OP-32 = 0111011
  function Action instrSRAW (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) = action
    Bit#(5) shiftAmnt = truncate(s.regFile[rs2]);
    s.regFile[rd] <= signExtend(arithRightShift(s.regFile[rs1][31:0], shiftAmnt));
    s.pc <= s.pc + 4;
    logInstR(s.pc, "sraw", rd, rs1, rs2);
  endaction;
  defineInstr("sraw", pat(n(7'b0100000), v, v, n(3'b101), v, n(7'b0111011)), instrSRAW);

/////////////////////////////////
// Load and Store Instructions //
////////////////////////////////////////////////////////////////////////////////

/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |               imm[11:0]             |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
*/

  // funct3 = LWU = 110
  // opcode = 0000011
  function List#(Action) instrLWU(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = list(
    action
      Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
      mem.sendReq(tagged ReadReq {addr: addr, numBytes: 4});
      logInstI(s.pc, "lwu(step1)", rd, rs1, imm);
    endaction,
    action
      let rsp <- mem.getRsp();
      case (rsp) matches
        tagged ReadRsp .r: begin
          Bit#(32) tmp = truncate(r);
          s.regFile[rd] <= zeroExtend(tmp);
        end
      endcase
      s.pc <= s.pc + 4;
      logInstI(s.pc, "lwu(step2)", rd, rs1, imm);
    endaction
  );
  defineInstr("lwu", pat(v, v, n(3'b110), v, n(7'b0000011)), instrLWU);

  // funct3 = LD = 011
  // opcode = 0000011
  function List#(Action) instrLD(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) = list(
    action
      Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
      mem.sendReq(tagged ReadReq {addr: addr, numBytes: 8});
      logInstI(s.pc, "ld(step1)", rd, rs1, imm);
    endaction,
    action
      let rsp <- mem.getRsp();
      case (rsp) matches
        tagged ReadRsp .r: begin
          Bit#(64) tmp = truncate(r);
          s.regFile[rd] <= signExtend(tmp);
        end
      endcase
      s.pc <= s.pc + 4;
      logInstI(s.pc, "ld(step2)", rd, rs1, imm);
    endaction
  );
  defineInstr("ld", pat(v, v, n(3'b011), v, n(7'b0000011)), instrLD);

/*
  S-type

   31                        25 24    20 19    15 14    12 11     7 6        0
  +----------------------------+--------+--------+--------+--------+----------+
  |         imm[11:5]          |   rs2  |   rs1  | funct3 |imm[4:0]|  opcode  |
  +----------------------------+--------+--------+--------+--------+----------+
*/

  // funct3 = SD = 011
  // opcode = 0100011
  function Action instrSD(Bit#(7) imm11_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) imm4_0) = action
    Bit#(XLEN) imm = {signExtend(imm11_5), imm4_0};
    Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
    mem.sendReq(tagged WriteReq {addr: addr, byteEnable: 'b11111111, data: s.regFile[rs2]});
    s.pc <= s.pc + 4;
    logInstS(s.pc, "sd", rs1, rs2, imm);
  endaction;
  defineInstr("sd", pat(v, v, v, n(3'b011), v, n(7'b0100011)), instrSD);

endmodule
`endif // XLEN64
