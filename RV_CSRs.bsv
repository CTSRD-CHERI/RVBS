// 2018, Alexandre Joannou, University of Cambridge

import BID :: *;

interface CSRs#(numeric type n);
  method ActionValue#(Bit#(n)) rw (Bit#(12) idx, Bit#(n) val);
  method ActionValue#(Bit#(n)) rs (Bit#(12) idx, Bit#(n) val);
  method ActionValue#(Bit#(n)) rc (Bit#(12) idx, Bit#(n) val);
endinterface

module mkCSRs(CSRs#(n));
  method ActionValue#(Bit#(n)) rw (Bit#(12) idx, Bit#(n) val);
    //TODO
    printLog($format("CSR %0d, val 0x%0x", idx, val));
    printLog("CSRRW unimplemented");
    return 42;
  endmethod
  method ActionValue#(Bit#(n)) rs (Bit#(12) idx, Bit#(n) val);
    //TODO
    printLog($format("CSR %0d, val 0x%0x", idx, val));
    printLog("CSRRS unimplemented");
    return 42;
  endmethod
  method ActionValue#(Bit#(n)) rc (Bit#(12) idx, Bit#(n) val);
    //TODO
    printLog($format("CSR %0d, val 0x%0x", idx, val));
    printLog("CSRRC unimplemented");
    return 42;
  endmethod
endmodule
