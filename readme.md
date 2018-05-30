# RVBS - RISC-V Bluespec SystemVerilog Specification

RVBS is an ISA level description of the RISC-V instruction set in [Bluespec SystemVerilog](http://wiki.bluespec.com/bluespec-systemverilog-and-compiler). It uses the [BID library](https://github.com/CTSRD-CHERI/BID) to describe the instructions, providing a readable, executable and synthesizeable specification, that could be used as a golden model.

RVBS currently supports:

- 32-bit and 64-bit RISC-V *I* base integer instructions
- RISC-V *C* compressed instructions
- *Machine mode*
- *PMP*

RVBS supports traps between privilege modes, but *Supervisor mode* is not fully implemented.

## Build options

In order to build a RVBS Bluesim simulator, you will need a valid installation of [Bluespec SystemVerilog](http://wiki.bluespec.com/bluespec-systemverilog-and-compiler) on your machine. RVBS relies on the following three Bluespec libraries:

- [Recipe](https://github.com/CTSRD-CHERI/Recipe)
- [BitPat](https://github.com/CTSRD-CHERI/BitPat)
- [BID](https://github.com/CTSRD-CHERI/BID)

[BID](https://github.com/CTSRD-CHERI/BID) is a submodule of the RVBS repo, and [Recipe](https://github.com/CTSRD-CHERI/Recipe) and [BitPat](https://github.com/CTSRD-CHERI/BitPat) are themselves subrepos in [BID](https://github.com/CTSRD-CHERI/BID). In order to checkout all of them, you need to run:
```sh
$ git submodule update --init --recursive
```

Once the libraries are available, you can build a simulator by typing `make`. You can specify a number of build options as environment variables:

- `MEM_SIZE` can be used to specify the size of the memory in bytes
- `MEM_IMG` can be used to specify the memory image used to initialize the memory
- `NO_LOGS` can be used to skip print statements (accelerates simulation)
- `PRINT_ABI_REG_NAME` can be used to use ABI names for registers instead of their index
- `XLEN` can be used to specify the XLEN to build with (only 32 and 64 are currently supported)
- `PMP` can be used to enable the Physical Memory Protection unit
- `RVC` can be used to enable the *C* compressed instructions extention

For example, to build a 64-bit simulator with support for compressed instructions and 32KB of memory, you can run:
```sh
$ make RVC=1 XLEN=64 MEM_SIZE=32768
```

TODO, not currently implemented:

- `USER_MODE` can be used to enable *User mode* support
- `SUPERVISOR_MODE` can be used to enable *Supervisor mode* support
- `RVN` can be used to enable the *N* *User mode* exceptions extention

## References
- [l3riscv](https://github.com/SRI-CSL/l3riscv)
- [RISC-V in Sail](https://github.com/rems-project/sail/tree/sail2/riscv)
- [Nikhil's initial RISC-V spec in Bluespec](https://github.com/rsnikhil/RISCV_ISA_Formal_Spec_in_BSV)
