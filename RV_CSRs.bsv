// 2018, Alexandre Joannou, University of Cambridge

import BID :: *;

import DefaultValue :: *;

///////////////////////////
// Interface to the CSRs //
////////////////////////////////////////////////////////////////////////////////

interface CSRs#(numeric type n);
  method ActionValue#(Bit#(n)) req (CSRReq#(n) r);
endinterface

typedef enum {RW, RS, RC} CSRReqType deriving (FShow);
typedef enum {ALL, NOREAD, NOWRITE} CSRReqEffects deriving (FShow);

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

module [ArchStateDefModule#(n)] mkCSRs(CSRs#(n));

  // Read only cycle counter @ 0xC00 [and 0xC80]
  Reg#(Bit#(64)) cycle <- mkReg(0);
  rule cycle_count;
    cycle <= cycle + 1;
  endrule
  // Read only time counter @ 0xC01 [and 0xC81]
  //TODO
  // Read only retired instruction counter @ 0xC02 [and 0xC82]
  Reg#(Bit#(64)) instret <- mkCommittedInstCnt;

  method ActionValue#(Bit#(n)) req (CSRReq#(n) r);
    Bit#(n) ret;
    case (r.idx)
      'hC00: ret = cycle[valueOf(n)-1:0];
      'hC02: ret = instret[valueOf(n)-1:0];
      // RV32I only
      //'hC80: ret = cycle[63:32];
      default: begin
        ret = ?;
        printLog($format("CSR %0d unimplemented - ", r.idx, fshow(r)));
      end
    endcase
    return ret;
  endmethod

endmodule
