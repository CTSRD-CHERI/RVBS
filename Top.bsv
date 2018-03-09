// 2018, Alexandre Joannou, University of Cambridge

import FIFO :: *;

import BID :: *;
import RV_Common :: *;
import RV_I :: *;

module top ();

  RVArchState s <- mkArchState;
  RVMem mem <- mkRVMem(16384, "test-prog.hex", s);

  // instanciating simulator
  `ifdef XLEN64
  mkISASim(mem, s, list(mkRVTrap, mkRV32I, mkRV64I));
  `else
  mkISASim(mem, s, list(mkRVTrap, mkRV32I));
  `endif

endmodule
