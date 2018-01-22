// 2018, Alexandre Joannou, University of Cambridge

import FIFO :: *;

import BID :: *;
import RV_Common :: *;
import RV_I :: *;

module top ();

  RVArchState s <- initArchState;
  RVWorld w <- initWorld;
  InstStream#(32) instStream <- mkInstStream("test-prog.hex");

  // instanciating simulator
  mkISASim(instStream, s, w, list(mkRV_I));

endmodule
