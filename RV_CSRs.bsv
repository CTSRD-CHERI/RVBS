// 2018, Alexandre Joannou, University of Cambridge

import DefaultValue :: *;
import BID :: *;

import RV_BasicTypes :: *;
import RV_CSRTypes :: *;

///////////////////////////
// Interface to the CSRs //
////////////////////////////////////////////////////////////////////////////////

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

typedef struct {

  // machine information registers
  //////////////////////////////////////////////////////////////////////////////
  Reg#(MVendorID)  mvendorid;
  Reg#(Bit#(XLEN)) marchid;
  Reg#(Bit#(XLEN)) mimpid;
  Reg#(Bit#(XLEN)) mhartid;

  // machine trap setup registers
  //////////////////////////////////////////////////////////////////////////////
  Reg#(MStatus)    mstatus;
  Reg#(MISA)       misa;
  // medeleg
  // mideleg
  // mie
  Reg#(MTVec) mtvec;
  // mcounteren

  // machine trap handling
  //////////////////////////////////////////////////////////////////////////////
  Reg#(Bit#(XLEN)) mscratch;
  Reg#(MEPC) mepc;
  Reg#(MCause) mcause;
  // mtval
  // mip

  // machine protection and translation
  //////////////////////////////////////////////////////////////////////////////
  // pmpcfg0
  // pmpcfg1
  // pmpcfg2
  // pmpcfg3
  // pmpaddr0
  // pmpaddr1
  // ...
  // pmpaddr15

  // user trap setup registers
  //////////////////////////////////////////////////////////////////////////////
  // ustatus
  // uie
  // utvec

  // user trap handling
  //////////////////////////////////////////////////////////////////////////////
  // uscratch
  // uepc
  // ucause
  // utval
  // uip

  // user counters/timers
  //////////////////////////////////////////////////////////////////////////////
  Reg#(Bit#(64)) cycle;
  // time
  //Reg#(Bit#(64)) instret;
  // hpmcounter3
  // hpmcounter4
  // ...
  // hpmcounter31

  // CSR request
  //////////////////////////////////////////////////////////////////////////////
  function ActionValue#(Bit#(XLEN)) doReq (CSRReq#(XLEN) r) req;

} CSRs;

//////////////////////////
// CSRs' implementation //
////////////////////////////////////////////////////////////////////////////////

function ActionValue#(Bit#(n)) readUpdateCSR(Reg#(csr_t) csr, CSRReq#(n) r)
provisos(Bits#(csr_t, n), CSR#(csr_t)) = actionvalue
  if (r.rEffects != NOWRITE) begin
    Bit#(n) newval = ?;
    case (r.rType)
      RW: newval = r.val;
      RS: newval = pack(csr) | r.val;
      RC: newval = pack(csr) & ~r.val;
    endcase
    updateCSR(csr, unpack(newval));
    printTLogPlusArgs("CSRs", $format("overwriting CSR old value 0x%0x with new value 0x%0x", pack(csr), newval));
  end else
    printTLogPlusArgs("CSRs", $format("reading value 0x%0x from CSR", pack(csr)));
  return pack(csr);
endactionvalue;

module mkCSRs(CSRs);

  // instance of the CSRs struct
  CSRs csrs;

  // machine information registers
  //////////////////////////////////////////////////////////////////////////////
  csrs.mvendorid <- mkReg(defaultValue); // mvendorid 12'hF11
  csrs.marchid   <- mkReg(0); // marchid 12'hF12
  csrs.mimpid    <- mkReg(0); // mimpid 12'hF13
  csrs.mhartid   <- mkReg(0); // mhartid 12'hF14

  // machine trap setup registers
  //////////////////////////////////////////////////////////////////////////////
  csrs.mstatus <- mkReg(defaultValue); // mstatus 12'h300
  csrs.misa    <- mkReg(defaultValue); // misa 12'h301
  // medeleg 12'h302
  // mideleg 12'h303
  // mie 12'h304
  csrs.mtvec   <- mkReg(defaultValue); // mtvec 12'h305
  // mcounteren 12'h306

  // machine trap handling
  //////////////////////////////////////////////////////////////////////////////
  csrs.mscratch <- mkRegU; // mscratch 12'h340
  csrs.mepc <- mkReg(defaultValue); // mepc 12'h341
  csrs.mcause <- mkRegU; // mcause 12'h342
  // mtval 12'h343
  // mip 12'h344

  // machine protection and translation
  //////////////////////////////////////////////////////////////////////////////
  // pmpcfg0 12'h3A0
  // pmpcfg1 12'h3A1 (RV32 only)
  // pmpcfg2 12'h3A2
  // pmpcfg3 12'h3A3 (RV32 only)
  // pmpaddr0 12'h3B0
  // pmpaddr1 12'h3B1
  // ...
  // pmpaddr15 12'h3BF

  // machine counter / timers
  //////////////////////////////////////////////////////////////////////////////
  // mcycle 12'hB00
  // minsret 12'hB02
  // mhpmcounter3 12'hB03 (and 12'hB83 in RV32)
  // mhpmcounter4 12'hB04 (and 12'hB84 in RV32)
  // ...
  // mhpmcounter31 12'B1F (and 12'hB9F in RV32)

  // machine counter setup
  //////////////////////////////////////////////////////////////////////////////
  // mhpmevent3 12'h323
  // mhpmevent4 12'h324
  // ...
  // mhpmevent31 12'h33F

  // debug / trace registers (shared with debug mode)
  //////////////////////////////////////////////////////////////////////////////
  // tselect 12'h7A0
  // tdata1 12'h7A1
  // tdata2 12'h7A2
  // tdata3 12'h7A3

  // debug mode registers
  //////////////////////////////////////////////////////////////////////////////
  // dcsr 12'h7B0
  // dpc 12'h7B1
  // dscratch 12'h7B2

  // user trap setup registers
  //////////////////////////////////////////////////////////////////////////////
  // ustatus 12'h000
  // uie 12'h004
  // utvec 12'h005

  // user trap handling
  //////////////////////////////////////////////////////////////////////////////
  // uscratch 12'h040
  // uepc 12'h041
  // ucause 12'h042
  // utval 12'h043
  // uip 12'h044

  // user counters/timers
  //////////////////////////////////////////////////////////////////////////////
  csrs.cycle <- mkReg(0); // cycle 12'hC00 (and 12'hC80 in RV32)
  rule cycle_count;
    csrs.cycle <= csrs.cycle + 1;
  endrule
  // time 12'hC01 (and 12'hC81 in RV32)
  // csrs.instret <- mkCommittedInstCnt; // insret 12'hC02 (and 12'hC82 in RV32)
  // hpmcounter3 12'hC03 (and 12'hC83 in RV32)
  // hpmcounter4 12'hC04 (and 12'hC84 in RV32)
  // ...
  // hpmcounter31 12'hC1F (and 12'hC9F in RV32)

  // CSR requests
  function ActionValue#(Bit#(XLEN)) req (CSRReq#(XLEN) r) = actionvalue
    Bit#(XLEN) ret = ?;
    case (r.idx) // TODO sort out individual behaviours for each CSR
      12'h300: ret <- readUpdateCSR(csrs.mstatus,r);
      12'h301: ret <- readUpdateCSR(csrs.misa,r);
      12'h305: ret <- readUpdateCSR(csrs.mtvec,r);
      12'h340: ret <- readUpdateCSR(csrs.mscratch,r);
      12'h341: ret <- readUpdateCSR(csrs.mepc,r);
      12'h342: ret <- readUpdateCSR(csrs.mcause,r);
      12'h343: ret = 0; // mtval placeholder
      12'hF11: ret <- readUpdateCSR(csrs.mvendorid,r);
      12'hF12: ret <- readUpdateCSR(csrs.marchid,r);
      12'hF13: ret <- readUpdateCSR(csrs.mimpid,r);
      12'hF14: ret <- readUpdateCSR(csrs.mhartid,r);
      12'hC00: ret = csrs.cycle[valueOf(XLEN)-1:0];
      //12'hC02: ret = csrs.instret[valueOf(XLEN)-1:0];
      // RV32I only
      //'hC80: ret = cycle[63:32];
      //XXX hack for test suite
      12'hCC0: begin // test success
        $display("TEST SUCCESS");
        $finish(0);
      end
      12'hCC1: begin // test failure
        $display("TEST FAILURE");
        $finish(0);
      end
      default: begin
        ret = ?;
        printLog($format("CSR 0x%0x unimplemented - ", r.idx, fshow(r)));
      end
    endcase
    return ret;
  endactionvalue;
  csrs.req = req;

  return csrs;

endmodule
