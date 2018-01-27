// 2018, Alexandre Joannou, University of Cambridge

import FIFO :: *;

import BID :: *;
import RV_Common :: *;
import RV_I :: *;

module top ();

  RVWorld w <- initWorld;
  IMem#(Bit#(XLEN), Bit#(32)) instMem <- mkSimpleInstMem(4096, "test-prog.hex");

  // instanciating simulator
  mkISASim(instMem, w, initArchState, list(mkRV_I));

endmodule
