// 2018, Alexandre Joannou, University of Cambridge

import FIFO :: *;

import BID :: *;
import RV_Common :: *;
import RV_I :: *;

module top ();

  RVWorld w <- initWorld;
  InstStream#(32) instStream <- mkInstStream("test-prog.hex", 1024);

  // instanciating simulator
  mkISASim(instStream, w, initArchState, list(mkRV_I));

endmodule
