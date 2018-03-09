// 2018, Alexandre Joannou, University of Cambridge

import BID :: *;
import Recipe :: *;

import RV_BasicTypes :: *;
import RV_State :: *;

///////////////////
// RISC-V Memory //
////////////////////////////////////////////////////////////////////////////////

typedef FullMem#(PAddr, Bit#(InstSz), Bit#(XLEN)) RVMem;
typedef Mem#(PAddr, Bit#(XLEN)) RVDMem;

module [Module] mkRVMem#(Integer size, String file, RVArchState s) (RVMem);

  Mem2#(PAddr, Bit#(InstSz), Bit#(XLEN)) mem <- mkSharedMem2(size, file);

  /*
  RecipeFSM fetchInst <- compile(rPar(rBlock(
    action
      PMPReq req = PMPReq{addr: toPAddr(s.pc.next), numBytes: 4, reqType: READ};
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
    endaction, action
      MemReq#(PAddr, Bit#(InstSz)) req = tagged ReadReq {addr: toPAddr(s.pc.next), numBytes: 4};
      mem.p0.sendReq(req);
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
    endaction)));
  */
  RecipeFSM fetchInst <- compile(rPar(rBlock(
    action
      PMPReq req = PMPReq{addr: toPAddr(s.pc.next), numBytes: 4, reqType: READ};
      s.pmp.lookup[0].put(req);
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
    endaction, action
      PMPRsp rsp <- s.pmp.lookup[0].get();
      MemReq#(PAddr, Bit#(InstSz)) req = tagged ReadReq {addr: rsp.addr, numBytes: 4};
      mem.p0.sendReq(req);
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(rsp)));
      printTLogPlusArgs("ifetch", $format("IFETCH ", fshow(req)));
    endaction)));

  interface IMem inst;
    method Action reqNext () = fetchInst.start();
    method ActionValue#(Bit#(InstSz)) get ();
      let rsp <- mem.p0.getRsp();
      return case (rsp) matches
        tagged ReadRsp .val: val;
        default: ?;
      endcase;
    endmethod
  endinterface
  interface data = mem.p1;

endmodule
