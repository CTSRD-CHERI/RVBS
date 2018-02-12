// 2018, Alexandre Joannou, University of Cambridge

import FIFO :: *;

import BID :: *;
import RV_Common :: *;
import RV_I :: *;

module top ();

  //RVMem mem <- initRVMem;
  RVMem mem <- mkSharedMem(8192, "test-prog.hex");

  // instanciating simulator
  `ifdef XLEN64
  mkISASim(mem, mkArchState, list(mkRVTrap, mkRV32I, mkRV64I));
  `else
  mkISASim(mem, mkArchState, list(mkRVTrap, mkRV32I));
  `endif

endmodule
