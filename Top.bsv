// 2018, Alexandre Joannou, University of Cambridge

import FIFO :: *;

import BID :: *;
import RV_Common :: *;
import RV_I :: *;
import RV_C :: *;

module top ();

  RVArchState s <- mkArchState;
  RVMem mem <- mkRVMem(16384, "test-prog.hex", s);

  // instanciating simulator
  `ifdef XLEN64
  mkISASim(mem, s, list(mkRVTrap, mkRV32I, mkRV32C, mkRV64I, mkRV64C));
  `else
  mkISASim(mem, s, list(mkRVTrap, mkRV32I, mkRV32C));
  `endif

endmodule
