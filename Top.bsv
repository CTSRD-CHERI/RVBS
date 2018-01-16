// 2018, Alexandre Joannou, University of Cambridge

import BID :: *;
import RV_I :: *;

module top ();
  Bit#(32) instr = 32'b0000000_00100_10000_000_00001_0110011;

  mkISASim(instr, List::cons(mkRV_I, Nil));

endmodule
