// 2018, Alexandre Joannou, University of Cambridge

import Vector :: *;
import BitPat :: *;
import BID :: *;

import RV_Common :: *;

module [Instr32DefModule] mkRV_I#(RVArchState#(XLEN) s, RVWorld w) ();

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
      $display("addi %0d, %0d, 0x%0x", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b000), v, n(7'b0010011)), instrADDI);

  // funct3 = SLTI = 010
  // opcode = OP-IMM = 0010011
  function Action instrSLTI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= signedLT(s.regFile[rs1],signExtend(imm)) ? 1 : 0;
      $display("slti %0d, %0d, 0x%0x", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b010), v, n(7'b0010011)), instrSLTI);

  // funct3 = SLTIU = 011
  // opcode = OP-IMM = 0010011
  // XXX pseudo-op: SEQZ
  function Action instrSLTIU (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= (s.regFile[rs1] < signExtend(imm)) ? 1 : 0;
      $display("sltiu %0d, %0d, 0x%0x", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b011), v, n(7'b0010011)), instrSLTIU);

  // funct3 = ANDI = 111
  // opcode = OP-IMM = 0010011
  function Action instrANDI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] & signExtend(imm);
      $display("andi %0d, %0d, 0x%0x", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b111), v, n(7'b0010011)), instrANDI);

  // funct3 = ORI = 110
  // opcode = OP-IMM = 0010011
  function Action instrORI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] | signExtend(imm);
      $display("ori %0d, %0d, 0x%0x", rd, rs1, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(3'b110), v, n(7'b0010011)), instrORI);

  // funct3 = XORI = 100
  // opcode = OP-IMM = 0010011
  // XXX pseudo-op: NOT
  function Action instrXORI (Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.regFile[rs1] ^ signExtend(imm);
      $display("xori %0d, %0d, 0x%0x", rd, rs1, imm);
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
      $display("lui %0d, 0x%0x", rd, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, n(7'b0110111)), instrLUI);

  // opcode = AUIPC = 0010111
  function Action instrAUIPC (Bit#(20) imm, Bit#(5) rd) =
    action
      s.regFile[rd] <= s.pc + {imm, 12'b0};
      $display("auipc %0d, 0x%0x", rd, imm);
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
      Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.regFile[rs2]);
      s.regFile[rd] <= s.regFile[rs1] << shiftAmnt;
      $display("sll %0d, %0d, %0d", rd, rs1, rs2);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(n(7'b0000000), v, v, n(3'b001), v, n(7'b0110011)), instrSLL);

  // funct7 = 0000000
  // funct3 = SRL = 101
  // opcode = OP = 0110011
  function Action instrSRL (Bit#(5) rs2, Bit#(5) rs1, Bit#(5) rd) =
    action
      Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.regFile[rs2]);
      s.regFile[rd] <= s.regFile[rs1] >> shiftAmnt;
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
      Bit#(TLog#(XLEN)) shiftAmnt = truncate(s.regFile[rs2]);
      s.regFile[rd] <= arithRightShift(s.regFile[rs1], shiftAmnt);
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
      $display("jal %0d, 0x%0x", rd, imm);
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
      $display("jalr %0d, 0x%0x", rd, rs1, imm);
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

  // Note from the RISC-V ISA document:
  // BGT, BGTU, BLE, and BLEU can be synthesized by reversing the operands
  // to BLT, BLTU, BGE, and BGEU, respectivelly.

  // funct3 = BEQ = 000
  // opcode = 1100011
  function Action instrBEQ (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) =
    action
      Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
      if (s.regFile[rs1] == s.regFile[rs2])
        s.pc <= s.pc + imm;
      $display("beq %0d, %0d, 0x%0x", rs1, rs2, imm);
    endaction;
  defineInstr(pat(v, v, v, v, n(3'b000), v, v, n(7'b1100011)), instrBEQ);

  // funct3 = BNE = 001
  // opcode = 1100011
  function Action instrBNE (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) =
    action
      Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
      if (s.regFile[rs1] != s.regFile[rs2])
        s.pc <= s.pc + imm;
      $display("bne %0d, %0d, 0x%0x", rs1, rs2, imm);
    endaction;
  defineInstr(pat(v, v, v, v, n(3'b001), v, v, n(7'b1100011)), instrBNE);

  // funct3 = BLT = 100
  // opcode = 1100011
  function Action instrBLT (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) =
    action
      Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
      if (signedLT(s.regFile[rs1], s.regFile[rs2]))
        s.pc <= s.pc + imm;
      $display("blt %0d, %0d, 0x%0x", rs1, rs2, imm);
    endaction;
  defineInstr(pat(v, v, v, v, n(3'b100), v, v, n(7'b1100011)), instrBLT);

  // funct3 = BLTU = 110
  // opcode = 1100011
  function Action instrBLTU (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) =
    action
      Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
      if (s.regFile[rs1] < s.regFile[rs2])
        s.pc <= s.pc + imm;
      $display("bltu %0d, %0d, 0x%0x", rs1, rs2, imm);
    endaction;
  defineInstr(pat(v, v, v, v, n(3'b110), v, v, n(7'b1100011)), instrBLTU);

  // funct3 = BGE = 101
  // opcode = 1100011
  function Action instrBGE (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) =
    action
      Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
      if (signedGE(s.regFile[rs1], s.regFile[rs2]))
        s.pc <= s.pc + imm;
      $display("bge %0d, %0d, 0x%0x", rs1, rs2, imm);
    endaction;
  defineInstr(pat(v, v, v, v, n(3'b101), v, v, n(7'b1100011)), instrBGE);

  // funct3 = BGEU = 111
  // opcode = 1100011
  function Action instrBGEU (Bit#(1) imm12, Bit#(6) imm10_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(4) imm4_1, Bit#(1) imm11) =
    action
      Bit#(XLEN) imm = {signExtend(imm12),imm11,imm10_5,imm4_1,1'b0};
      if (s.regFile[rs1] >= s.regFile[rs2])
        s.pc <= s.pc + imm;
      $display("bgeu %0d, %0d, 0x%0x", rs1, rs2, imm);
    endaction;
  defineInstr(pat(v, v, v, v, n(3'b111), v, v, n(7'b1100011)), instrBGEU);

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
  function List#(Action) instrLB(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    list(
      action
        $display("lb %0d, %0d, 0x%0x - step 1", rd, rs1, imm);
        Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
        w.mem.sendReq(tagged ReadReq {addr: addr, numBytes: 1});
      endaction,
      action
        $display("lb %0d, %0d, 0x%0x - step 2", rd, rs1, imm);
        let rsp <- w.mem.getRsp();
        case (rsp) matches
          tagged ReadRsp .r: begin
            Bit#(8) tmp = truncate(r);
            s.regFile[rd] <= signExtend(tmp);
          end
        endcase
        s.pc <= s.pc + 4;
      endaction
    );
  defineInstr(pat(v, v, n(3'b000), v, n(7'b0000011)), instrLB);

  // funct3 = LBU = 100
  // opcode = 0000011
  function List#(Action) instrLBU(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    list(
      action
        $display("lbu %0d, %0d, 0x%0x - step 1", rd, rs1, imm);
        Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
        w.mem.sendReq(tagged ReadReq {addr: addr, numBytes: 1});
      endaction,
      action
        $display("lbu %0d, %0d, 0x%0x - step 2", rd, rs1, imm);
        let rsp <- w.mem.getRsp();
        case (rsp) matches
          tagged ReadRsp .r: begin
            Bit#(8) tmp = truncate(r);
            s.regFile[rd] <= zeroExtend(tmp);
          end
        endcase
        s.pc <= s.pc + 4;
      endaction
    );
  defineInstr(pat(v, v, n(3'b100), v, n(7'b0000011)), instrLBU);

  // funct3 = LH = 001
  // opcode = 0000011
  function List#(Action) instrLH(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    list(
      action
        $display("lh %0d, %0d, 0x%0x - step 1", rd, rs1, imm);
        Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
        w.mem.sendReq(tagged ReadReq {addr: addr, numBytes: 2});
      endaction,
      action
        $display("lh %0d, %0d, 0x%0x - step 2", rd, rs1, imm);
        let rsp <- w.mem.getRsp();
        case (rsp) matches
          tagged ReadRsp .r: begin
            Bit#(16) tmp = truncate(r);
            s.regFile[rd] <= signExtend(tmp);
          end
        endcase
        s.pc <= s.pc + 4;
      endaction
    );
  defineInstr(pat(v, v, n(3'b001), v, n(7'b0000011)), instrLH);

  // funct3 = LHU = 101
  // opcode = 0000011
  function List#(Action) instrLHU(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    list(
      action
        $display("lhu %0d, %0d, 0x%0x - step 1", rd, rs1, imm);
        Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
        w.mem.sendReq(tagged ReadReq {addr: addr, numBytes: 2});
      endaction,
      action
        $display("lhu %0d, %0d, 0x%0x - step 2", rd, rs1, imm);
        let rsp <- w.mem.getRsp();
        case (rsp) matches
          tagged ReadRsp .r: begin
            Bit#(18) tmp = truncate(r);
            s.regFile[rd] <= zeroExtend(tmp);
          end
        endcase
        s.pc <= s.pc + 4;
      endaction
    );
  defineInstr(pat(v, v, n(3'b101), v, n(7'b0000011)), instrLHU);

  // funct3 = LW = 010
  // opcode = 0000011
  function List#(Action) instrLW(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    list(
      action
        $display("lw %0d, %0d, 0x%0x - step 1", rd, rs1, imm);
        Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
        w.mem.sendReq(tagged ReadReq {addr: addr, numBytes: 4});
      endaction,
      action
        $display("lw %0d, %0d, 0x%0x - step 2", rd, rs1, imm);
        let rsp <- w.mem.getRsp();
        case (rsp) matches
          tagged ReadRsp .r: begin
            Bit#(32) tmp = truncate(r);
            s.regFile[rd] <= signExtend(tmp);
          end
        endcase
        s.pc <= s.pc + 4;
      endaction
    );
  defineInstr(pat(v, v, n(3'b010), v, n(7'b0000011)), instrLH);

/*
  S-type

   31                        25 24    20 19    15 14    12 11     7 6        0
  +----------------------------+--------+--------+--------+--------+----------+
  |         imm[11:5]          |   rs2  |   rs1  | funct3 |imm[4:0]|  opcode  |
  +----------------------------+--------+--------+--------+--------+----------+
*/

  // funct3 = SB = 000
  // opcode = 0100011
  function Action instrSB(Bit#(7) imm11_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) imm4_0) =
    action
      Bit#(XLEN) imm = {signExtend(imm11_5), imm4_0};
      Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
      w.mem.sendReq(tagged WriteReq {addr: addr, byteEnable: 'b1, data: s.regFile[rs2]});
      $display("sb %0d, %0d, 0x%0x", rs1, rs2, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, v, n(3'b000), v, n(7'b0100011)), instrSB);

  // funct3 = SH = 001
  // opcode = 0100011
  function Action instrSH(Bit#(7) imm11_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) imm4_0) =
    action
      Bit#(XLEN) imm = {signExtend(imm11_5), imm4_0};
      Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
      w.mem.sendReq(tagged WriteReq {addr: addr, byteEnable: 'b11, data: s.regFile[rs2]});
      $display("sh %0d, %0d, 0x%0x", rs1, rs2, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, v, n(3'b001), v, n(7'b0100011)), instrSH);

  // funct3 = SW = 010
  // opcode = 0100011
  function Action instrSW(Bit#(7) imm11_5, Bit#(5) rs2, Bit#(5) rs1, Bit#(5) imm4_0) =
    action
      Bit#(XLEN) imm = {signExtend(imm11_5), imm4_0};
      Bit#(XLEN) addr = s.regFile[rs1] + signExtend(imm);
      w.mem.sendReq(tagged WriteReq {addr: addr, byteEnable: 'b1111, data: s.regFile[rs2]});
      $display("sw %0d, %0d, 0x%0x", rs1, rs2, imm);
      s.pc <= s.pc + 4;
    endaction;
  defineInstr(pat(v, v, v, n(3'b010), v, n(7'b0100011)), instrSW);

//////////////////
// Memory Model //
////////////////////////////////////////////////////////////////////////////////


  // funct3 = FENCE = 000
  // opcode = 0001111
  function Action instrFENCE(Bit#(4) pred, Bit#(4) succ) =
    action
      //TODO
      $display("fence 0b%4b, 0b%4b", pred, succ);
    endaction;
  defineInstr(pat(n(4'b0000), v, v, n(5'b00000), n(3'b000), n(5'b00000), n(7'b0001111)), instrFENCE);

  // funct3 = FENCE.I = 001
  // opcode = 0001111
  function Action instrFENCE_I() =
    action
      //TODO
      $display("fence.i");
    endaction;
  defineInstr(pat(n(4'b0000), n(4'b0000), n(4'b0000), n(5'b00000), n(3'b001), n(5'b00000), n(7'b0001111)), instrFENCE_I);

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
  function Action instrCSRRW(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      //TODO
      $display("csrrw %0d, %0d, 0x%0x", rd, rs1, imm);
    endaction;
  defineInstr(pat(v, v, n(3'b001), v, n(7'b1110011)), instrCSRRW);

  // funct3 = CSRRS = 010
  // opcode = 1110011
  // pseudo-op CSRR
  // XXX RDCYCLE[H], RDTIME[H], RDINSTRET[H]
  function Action instrCSRRS(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      //TODO
      $display("csrrs %0d, %0d, 0x%0x", rd, rs1, imm);
    endaction;
  defineInstr(pat(v, v, n(3'b010), v, n(7'b1110011)), instrCSRRS);

  // funct3 = CSRRC = 011
  // opcode = 1110011
  function Action instrCSRRC(Bit#(12) imm, Bit#(5) rs1, Bit#(5) rd) =
    action
      //TODO
      $display("csrrc %0d, %0d, 0x%0x", rd, rs1, imm);
    endaction;
  defineInstr(pat(v, v, n(3'b011), v, n(7'b1110011)), instrCSRRC);

  // funct3 = CSRRWI = 101
  // opcode = 1110011
  function Action instrCSRRWI(Bit#(12) imm, Bit#(5) zimm, Bit#(5) rd) =
    action
      //TODO
      $display("csrrwi %0d, %0d, 0x%0x", rd, zimm, imm);
    endaction;
  defineInstr(pat(v, v, n(3'b101), v, n(7'b1110011)), instrCSRRWI);

  // funct3 = CSRRSI = 110
  // opcode = 1110011
  function Action instrCSRRSI(Bit#(12) imm, Bit#(5) zimm, Bit#(5) rd) =
    action
      //TODO
      $display("csrrsi %0d, %0d, 0x%0x", rd, zimm, imm);
    endaction;
  defineInstr(pat(v, v, n(3'b110), v, n(7'b1110011)), instrCSRRSI);

  // funct3 = CSRRCI = 111
  // opcode = 1110011
  function Action instrCSRRCI(Bit#(12) imm, Bit#(5) zimm, Bit#(5) rd) =
    action
      //TODO
      $display("csrrci %0d, %0d, 0x%0x", rd, zimm, imm);
    endaction;
  defineInstr(pat(v, v, n(3'b111), v, n(7'b1110011)), instrCSRRCI);

//////////////////////////////////////
// Environment Call and Breakpoints //
////////////////////////////////////////////////////////////////////////////////

  // ECALL
  function Action instrECALL() =
    action
      //TODO
      $display("ecall");
    endaction;
  defineInstr(pat(n(12'b000000000000), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrECALL);

  // EBREAK
  function Action instrEBREAK() =
    action
      //TODO
      $display("ebreak");
    endaction;
  defineInstr(pat(n(12'b000000000001), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrEBREAK);

endmodule
