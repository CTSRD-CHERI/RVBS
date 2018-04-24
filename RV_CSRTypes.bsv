// 2018, Alexandre Joannou, University of Cambridge

import DefaultValue :: *;

import RV_BasicTypes :: *;

// MISA
////////////////////////////////////////////////////////////////////////////////
`ifdef XLEN64
Bit#(2) nativeXLEN = 2'd2;
`else
Bit#(2) nativeXLEN = 2'd1;
`endif
typedef struct { Bit#(2) mxl; Bit#(TSub#(XLEN,28)) res; Bit#(26) extensions; }
  MISA deriving (Bits, FShow);
instance DefaultValue#(MISA);
  function MISA defaultValue() = MISA {
    mxl: nativeXLEN,
    res: ?,
    extensions: 26'b00000000000000000100000000
  };
endinstance
instance CSR#(MISA);
  function Action updateCSR(Reg#(MISA) csr, MISA val) = action
    let newval = val;
    if (newval.mxl != nativeXLEN) newval.mxl = nativeXLEN;// only support native XLEN
    csr <= newval;
  endaction;
endinstance

// MVendorID
////////////////////////////////////////////////////////////////////////////////
typedef struct { Bit#(TSub#(XLEN,7)) bank; Bit#(7) offset; }
  MVendorID deriving (Bits, FShow);
instance DefaultValue#(MVendorID);
  function MVendorID defaultValue() = MVendorID {bank: 0, offset: 0};
endinstance
instance CSR#(MVendorID);
  function Action updateCSR(Reg#(MVendorID) csr, MVendorID val) = action
    csr <= val;
  endaction;
endinstance

// MStatus
////////////////////////////////////////////////////////////////////////////////
function Bit#(2) xl_field(Integer xlen) = case (xlen)
  128: 2'b11; // 3
  64: 2'b10;  // 2
  32: 2'b01;  // 1
  default: 2'b00;
endcase;
typedef struct {
  Bool sd;
  `ifdef XLEN64 // MAX_XLEN > 32
  Bit#(TSub#(XLEN,37)) res4;
  Bit#(2) sxl;
  Bit#(2) uxl;
  Bit#(9) res3;
  `else // MAX_XLEN == 32
  Bit#(8) res3;
  `endif
  Bool tsr;
  Bool tw;
  Bool tvm;
  Bool mxr;
  Bool sum;
  Bool mprv;
  Bit#(2) xs;
  Bit#(2) fs;
  PrivLvl mpp;
  Bit#(2) res2;
  Bool spp;
  Bool mpie;
  Bool res1;
  Bool spie;
  Bool upie;
  Bool mie;
  Bool res0;
  Bool sie;
  Bool uie;
} MStatus deriving (Bits, FShow);
instance DefaultValue#(MStatus);
  function MStatus defaultValue() = MStatus {
    sd: False,
    `ifdef XLEN64 // MAX_XLEN > 32
    res4: ?, sxl: xl_field(valueOf(XLEN)), uxl: xl_field(valueOf(XLEN)), res3: ?,
    `else // MAX_XLEN == 32
    res3: ?,
    `endif
    tsr: False, tw: False, tvm: False, mxr: False, sum: False, mprv: False,
    xs: 0, fs: 0, mpp: M, res2: ?, spp: False,
    mpie: False, res1: ?, spie: False, upie: False,
    mie: False, res0: ?, sie: False, uie: False
  };
endinstance
instance CSR#(MStatus);
  function Action updateCSR(Reg#(MStatus) csr, MStatus val) = action
    let newval = val;
    if (newval.mpp != M ) newval.mpp = M;// only support Machine mode so far
    csr <= newval;
  endaction;
endinstance

// MEPC
////////////////////////////////////////////////////////////////////////////////
typedef struct {
  Bit#(XLEN) addr;
} MEPC deriving (Bits, FShow);
instance DefaultValue#(MEPC);
  function MEPC defaultValue() = MEPC{addr: {?,2'b00}}; // must not trigger unaligned inst fetch exception
endinstance
instance CSR#(MEPC);
  function Action updateCSR(Reg#(MEPC) csr, MEPC val) = action
    let newval = val;
    if (newval.addr[1:0] != 0) newval.addr[1:0] = 0; // must not trigger unaligned inst fetch exception
    csr <= newval;
  endaction;
endinstance

// MTVec
////////////////////////////////////////////////////////////////////////////////
typedef enum {Direct, Vectored, Res} MTVecMode deriving (Eq, FShow);
instance Bits#(MTVecMode, 2);
  function Bit#(2) pack (MTVecMode mode) = case (mode)
    Direct: 2'b00;
    Vectored: 2'b01;
    Res: 2'b11;
    default: 2'b11;
  endcase;
  function MTVecMode unpack (Bit#(2) mode) = case (mode)
    2'b00: Direct;
    2'b01: Vectored;
    default: Res;
  endcase;
endinstance
typedef struct { Bit#(TSub#(XLEN,2)) base;  MTVecMode mode; }
  MTVec deriving (Bits, FShow);
instance DefaultValue#(MTVec);
  function MTVec defaultValue() = MTVec {base: 0, mode: Direct};
endinstance
instance CSR#(MTVec);
  function Action updateCSR(Reg#(MTVec) csr, MTVec val) = action
    let newval = val;
    if (newval.mode != Direct || newval.mode != Vectored)
      newval.mode = csr.mode;
    csr <= newval;
  endaction;
endinstance

// MEDeleg
////////////////////////////////////////////////////////////////////////////////
typedef struct {Bit#(XLEN) val;} MEDeleg;
instance DefaultValue#(MEDeleg);
  function MEDeleg defaultValue() = MEDeleg {val: 0};
endinstance
instance CSR#(MEDeleg);
  function Action updateCSR(Reg#(MEDeleg) csr, MEDeleg val) = action
    let newval = val;
    csr <= newval;
  endaction;
endinstance

// MIDeleg
////////////////////////////////////////////////////////////////////////////////
typedef struct {Bit#(XLEN) val;} MIDeleg;
instance DefaultValue#(MIDeleg);
  function MIDeleg defaultValue() = MIDeleg {val: 0};
endinstance
instance CSR#(MIDeleg);
  function Action updateCSR(Reg#(MIDeleg) csr, MIDeleg val) = action
    let newval = val;
    csr <= newval;
  endaction;
endinstance

// MIP
////////////////////////////////////////////////////////////////////////////////
typedef struct {
  Bit#(TSub#(XLEN,12)) res3;
  Bool meip;
  Bool res2;
  Bool seip;
  Bool ueip;
  Bool mtip;
  Bool res1;
  Bool stip;
  Bool utip;
  Bool msip;
  Bool res0;
  Bool ssip;
  Bool usip;
} MIP deriving (Bits, FShow);
instance DefaultValue#(MIP); // XXX does spec actually specify reboot value ?
  function MIP defaultValue() = MIP {
    res3: 0,
    meip: False,
    res2: False,
    seip: False,
    ueip: False,
    mtip: False,
    res1: False,
    stip: False,
    utip: False,
    msip: False,
    res0: False,
    ssip: False,
    usip: False
  };
endinstance
instance CSR#(MIP);
  function Action updateCSR(Reg#(MIP) csr, MIP val) = action
    let newval = val;
    //TODO
    csr <= newval;
  endaction;
endinstance

// MIE
////////////////////////////////////////////////////////////////////////////////
typedef struct {
  Bit#(TSub#(XLEN,12)) res3;
  Bool meie;
  Bool res2;
  Bool seie;
  Bool ueie;
  Bool mtie;
  Bool res1;
  Bool stie;
  Bool utie;
  Bool msie;
  Bool res0;
  Bool ssie;
  Bool usie;
} MIE deriving (Bits, FShow);
instance DefaultValue#(MIE); // XXX does spec actually specify reboot value ?
  function MIE defaultValue() = MIE {
    res3: 0,
    meie: False,
    res2: False,
    seie: False,
    ueie: False,
    mtie: False,
    res1: False,
    stie: False,
    utie: False,
    msie: False,
    res0: False,
    ssie: False,
    usie: False
  };
endinstance
instance CSR#(MIE);
  function Action updateCSR(Reg#(MIE) csr, MIE val) = action
    let newval = val;
    //TODO
    csr <= newval;
  endaction;
endinstance
