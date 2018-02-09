// 2018, Alexandre Joannou, University of Cambridge

import RV_BasicTypes :: *;
import RV_State :: *;

////////////////
// Trap logic //
////////////////////////////////////////////////////////////////////////////////

function Action trap(RVArchState s, MCause cause) = action
  // TODO latch current priv in mstatus
  s.csrs.mcause <= cause;
  s.currentPrivLvl <= M;
  s.pc <= s.csrs.mtvec;
endaction;
