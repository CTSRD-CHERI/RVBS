// 2018, Alexandre Joannou, University of Cambridge

import FIFO :: *;
import List :: *;

import BID :: *;
import RV_Common :: *;
import RV_I :: *;
`ifdef RVC
import RV_C :: *;
`endif

module top ();

  Mem2#(PAddr, Bit#(InstSz), Bit#(XLEN)) mem <- mkSharedMem2(16384, "test-prog.hex");
  RVState s <- mkState(mem);

  // instanciating simulator
  let modList = list(mkRVTrap, mkRV32I);
  `ifdef RVC
    modList = append(modList, list(mkRV32C));
  `endif
  `ifdef XLEN64
  modList = append(modList, list(mkRV64I));
    `ifdef RVC
      modList = append(modList, list(mkRV64C));
    `endif
  `endif
  mkISASim(s, modList);

endmodule
