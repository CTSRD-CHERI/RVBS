// 2018, Alexandre Joannou, University of Cambridge

import DefaultValue :: *;
import BID :: *;

///////////////////////////
// Interface to the CSRs //
////////////////////////////////////////////////////////////////////////////////

interface CSRs#(numeric type n);
  method ActionValue#(Bit#(n)) req (CSRReq#(n) r);
endinterface

typedef enum {RW, RS, RC} CSRReqType deriving (Eq, FShow);
typedef enum {ALL, NOREAD, NOWRITE} CSRReqEffects deriving (Eq, FShow);

typedef struct {
  Bit#(12) idx;
  Bit#(n) val;
  CSRReqType rType;
  CSRReqEffects rEffects;
} CSRReq#(numeric type n) deriving (FShow);

instance DefaultValue#(CSRReq#(n));
  function CSRReq#(n) defaultValue =
    CSRReq { idx: ?, val: ?, rType: RW, rEffects: ALL };
endinstance
function CSRReq#(n) rwCSRReq(Bit#(12) i, Bit#(n) v) =
  CSRReq { idx: i, val: v, rType: RW, rEffects: ALL };
function CSRReq#(n) rwCSRReqNoRead(Bit#(12) i, Bit#(n) v) =
  CSRReq { idx: i, val: v, rType: RW, rEffects: NOREAD };
function CSRReq#(n) rsCSRReq(Bit#(12) i, Bit#(n) v) =
  CSRReq { idx: i, val: v, rType: RS, rEffects: ALL };
function CSRReq#(n) rsCSRReqNoWrite(Bit#(12) i, Bit#(n) v) =
  CSRReq { idx: i, val: v, rType: RS, rEffects: NOWRITE };
function CSRReq#(n) rcCSRReq(Bit#(12) i, Bit#(n) v) =
  CSRReq { idx: i, val: v, rType: RC, rEffects: ALL };
function CSRReq#(n) rcCSRReqNoWrite(Bit#(12) i, Bit#(n) v) =
  CSRReq { idx: i, val: v, rType: RC, rEffects: NOWRITE };

//////////////////////////
// CSRs' implementation //
////////////////////////////////////////////////////////////////////////////////

function ActionValue#(Bit#(n)) readUpdateCSR(Reg#(Bit#(n)) csr, CSRReq#(n) r) = actionvalue
  if (r.rEffects != NOWRITE) begin
    let newval = ?;
    case (r.rType)
      RW: newval = r.val;
      RS: newval = csr | r.val;
      RC: newval = csr & ~r.val;
    endcase
    csr <= newval;
    printTLogPlusArgs("CSRs", $format("overwriting 0x%0x with 0x%0x", csr, newval));
  end
  return csr;
endactionvalue;

// machine level CSRs
module [ArchStateDefModule#(n)] mkMCSRs(CSRs#(n));
  // mscratch
  Bit#(12) idx_mscratch = 12'h340;
  Reg#(Bit#(n)) mscratch <- mkReg(0);

  // machine CSR requests
  method ActionValue#(Bit#(n)) req (CSRReq#(n) r);
    Bit#(n) ret = ?;
    case (r.idx)
      idx_mscratch: ret <- readUpdateCSR(mscratch,r);
      default: begin
        ret = ?;
        printLog($format("Machine CSR %0d unimplemented - ", r.idx, fshow(r)));
      end
    endcase
    return ret;
  endmethod

endmodule

// user level CSRs
module [ArchStateDefModule#(n)] mkUCSRs(CSRs#(n));

  //XXX hack for test success / Fail
  Bit#(12) idx_testsuccess = 12'hCC0;
  Bit#(12) idx_testfailure = 12'hCC1;

  // Read only cycle counter @ 0xC00 [and 0xC80]
  Bit#(12) idx_cycle = 12'hC00;
  Reg#(Bit#(64)) cycle <- mkReg(0);
  rule cycle_count;
    cycle <= cycle + 1;
  endrule
  // Read only time counter @ 0xC01 [and 0xC81]
  //TODO
  // Read only retired instruction counter @ 0xC02 [and 0xC82]
  Bit#(12) idx_instret = 12'hC02;
  Reg#(Bit#(64)) instret <- mkCommittedInstCnt;

  method ActionValue#(Bit#(n)) req (CSRReq#(n) r);
    Bit#(n) ret = ?;
    case (r.idx)
      idx_cycle: ret = cycle[valueOf(n)-1:0];
      idx_instret: ret = instret[valueOf(n)-1:0];
      // RV32I only
      //'hC80: ret = cycle[63:32];
      //XXX hack for test suite
      idx_testsuccess: begin // test success
        $display("TEST SUCCESS");
        $finish(0);
      end
      idx_testfailure: begin // test failure
        $display("TEST FAILURE");
        $finish(0);
      end
      default: begin
        ret = ?;
        printLog($format("User CSR %0d unimplemented - ", r.idx, fshow(r)));
      end
    endcase
    return ret;
  endmethod

endmodule

module [ArchStateDefModule#(n)] mkCSRs(CSRs#(n));

  CSRs#(n) uCSRs <- mkUCSRs;
  CSRs#(n) mCSRs <- mkMCSRs;

  method ActionValue#(Bit#(n)) req (CSRReq#(n) r);
    printTLogPlusArgs("CSRs", $format("received ", fshow(r)));
    Bit#(n) ret = ?;
    case (r.idx[9:8]) // lowest privilege level required for CSR access
      2'b00: ret <- uCSRs.req(r);
      2'b11: ret <- mCSRs.req(r);
      default: begin
        ret = ?;
        printLog($format("CSR %0d unimplemented - ", r.idx, fshow(r)));
      end
    endcase
    return ret;
  endmethod

endmodule
