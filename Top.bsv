// 2018, Alexandre Joannou, University of Cambridge

import FIFO :: *;

import BID :: *;
import RV_Common :: *;
import RV_I :: *;

module top ();

  //RVMem mem <- initRVMem;
  RVMem mem <- mkSharedMem(8192, "test-prog.hex");

  // instanciating simulator
  mkISASim(mem, mkArchState, list(mkRV_I));

endmodule
