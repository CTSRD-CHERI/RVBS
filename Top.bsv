// 2018, Alexandre Joannou, University of Cambridge

import FIFO :: *;

import BID :: *;
import RV_Common :: *;
import RV_I :: *;
import RV_C :: *;

module top ();

  Mem2#(PAddr, Bit#(InstSz), Bit#(XLEN)) mem <- mkSharedMem2(16384, "test-prog.hex");
  RVState s <- mkState(mem);

  // instanciating simulator
  `ifdef XLEN64
  mkISASim(s, list(mkRVTrap, mkRV32I, mkRV32C, mkRV64I, mkRV64C));
  `else
  mkISASim(s, list(mkRVTrap, mkRV32I, mkRV32C));
  `endif

endmodule
