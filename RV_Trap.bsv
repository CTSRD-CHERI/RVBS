// 2018, Alexandre Joannou, University of Cambridge

import BitPat :: *;
import BID :: *;

import RV_BasicTypes :: *;
import RV_State :: *;

////////////////
// Trap logic //
////////////////////////////////////////////////////////////////////////////////

function Action trap(RVArchState s, MCause cause) = action
  // TODO latch current priv in mstatus
  s.csrs.mcause <= cause;
  s.csrs.mepc <= s.pc;
  s.pc <= s.csrs.mtvec;
  s.currentPrivLvl <= M;
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
    // TODO implement mstatus changes
    s.pc <= s.csrs.mepc;
    printTLogPlusArgs("itrace", $format("mret"));
  endaction;
  defineInstr("mret", pat(n(12'b001100000010), n(5'b00000), n(3'b000), n(5'b00000), n(7'b1110011)), instrMRET);

endmodule
