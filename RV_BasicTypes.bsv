// 2018, Alexandre Joannou, University of Cambridge

///////////////////////////////////
// Utility modules and functions //
////////////////////////////////////////////////////////////////////////////////

typedef 32 InstSz;

`ifdef XLEN64
typedef 64 XLEN;
`else
typedef 32 XLEN;
`endif
//TODO for SLL instruction, use something like this:
// typedef TSub#(TLog#(XLEN), 1) BitShAmnt;
