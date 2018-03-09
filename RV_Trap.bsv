// 2018, Alexandre Joannou, University of Cambridge

import BitPat :: *;
import BID :: *;

import RV_BasicTypes :: *;
import RV_CSRTypes :: *;
import RV_State :: *;
import RV_Mem :: *;

////////////////
// Trap logic //
////////////////////////////////////////////////////////////////////////////////

function Action trap(RVArchState s, MCause cause) = action
  // TODO latch current priv in mstatus
  s.csrs.mcause <= cause;
  s.csrs.mepc <= unpack(s.pc);
  // prepare trap handler address
  Bit#(XLEN) tgt = {s.csrs.mtvec.base, 2'b00};
  case (s.csrs.mtvec.mode)
    Direct: s.pc <= tgt;
    Vectored: s.pc <= case (cause) matches
      tagged Interrupt .i: (tgt + zeroExtend({pack(i),2'b00}));
      default: tgt;
    endcase;
    default: begin
      printTLog($format("Unknown mtvec mode 0x%0x", pack(s.csrs.mtvec.mode)));
      $finish(1);
    end
  endcase
  s.currentPrivLvl <= M;
  printTLogPlusArgs("itrace", $format(">>> TRAP <<< -- mcause <= ", fshow(cause), ", mepc <= 0x%0x, pc <= 0x%0x", s.pc, s.csrs.mtvec));
endaction;

module [Instr32DefModule] mkRVTrap#(RVArchState s, RVDMem mem) ();
/*
  I-type

   31                                 20 19    15 14    12 11     7 6        0
  +-------------------------------------+--------+--------+--------+----------+
  |                funct12              |   rs1  | funct3 |   rd   |  opcode  |
  +-------------------------------------+--------+--------+--------+----------+
*/

  // funct12 = MRET = 001100000010
  // rs1 = 00000
  // funct3 = PRIV = 000
  // rd = 00000
  // opcode = SYSTEM = 1110011
  function Action instrMRET () = action
    // current privilege update
    s.currentPrivLvl <= s.csrs.mstatus.mpp;
    // pc update
    s.pc <= pack(s.csrs.mepc);
    // mstatus CSR manipulation
    let val = s.csrs.mstatus;
    val.mie = val.mpie;
    val.mpie = True;
    val.mpp = M; // change to U when user mode is supported
    s.csrs.mstatus <= val;
    // trace
    printTLogPlusArgs("itrace", $format("pc: 0x%0x -- mret", s.pc));
  endaction;
  defineInstr("mret", pat(n(12'b001100000010), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrMRET);

  // funct12 = WFI = 000100000101
  // rs1 = 00000
  // funct3 = PRIV = 000
  // rd = 00000
  // opcode = SYSTEM = 1110011
  function Action instrWFI () = action
    printTLogPlusArgs("itrace", $format("pc: 0x%0x -- wfi -- IMPLEMENTED AS NOP", s.pc));
  endaction;
  defineInstr("wfi", pat(n(12'b000100000101), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrWFI);

endmodule
