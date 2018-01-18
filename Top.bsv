// 2018, Alexandre Joannou, University of Cambridge

import FIFO :: *;

import BID :: *;
import RV_Common :: *;
import RV_I :: *;

module top ();

  // local state
  Reg#(Bit#(2)) toggle <- mkReg(0);
  FIFO#(Bit#(32)) instq <- mkFIFO();

  // instanciating simulator
  RVArchState s <- initArchState;
  mkISASim(instq, s, list(mkRV_I));

  // rule to keep the simulator busy
  rule dummyFetch;
    if (toggle == 0) instq.enq(32'b0000000_00001_00010_000_00011_1101111); // JAL
    else if (toggle == 1) instq.enq(32'b0000000_00001_00010_000_00011_0110011); // ADD
    else if (toggle == 2) instq.enq(32'b0000000_00001_00010_100_00011_0010011); // XORI
    else if (toggle == 3) instq.enq(32'b0000000_00001_00010_000_00011_0010011); // ADDI
    toggle <= toggle + 1;
  endrule

endmodule
