#-
# Copyright (c) 2018 Alexandre Joannou
# All rights reserved.
#
# This software was developed by SRI International and the University of
# Cambridge Computer Laboratory (Department of Computer Science and
# Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
# DARPA SSITH research programme.
#
# @BERI_LICENSE_HEADER_START@
#
# Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
# license agreements.  See the NOTICE file distributed with this work for
# additional information regarding copyright ownership.  BERI licenses this
# file to you under the BERI Hardware-Software License, Version 1.0 (the
# "License"); you may not use this file except in compliance with the
# License.  You may obtain a copy of the License at:
#
#   http://www.beri-open-systems.org/legal/license-1-0.txt
#
# Unless required by applicable law or agreed to in writing, Work distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations under the License.
#
# @BERI_LICENSE_HEADER_END@
#

RVFIDIIDIR = BSV-RVFI-DII
BIDDIR = BID
RECIPEDIR = $(BIDDIR)/Recipe
BITPATDIR = $(BIDDIR)/BitPat
BLUESTUFFDIR = $(BIDDIR)/BlueStuff
AXIDIR = $(BLUESTUFFDIR)/AXI
BLUEBASICSDIR = $(BLUESTUFFDIR)/BlueBasics
BLUEUTILSDIR = $(BLUESTUFFDIR)/BlueUtils
SOCKETUTILSDIR = $(BLUESTUFFDIR)/SocketPacketUtils
RVBSSRCDIR = src
BSVPATH = +:$(RVBSSRCDIR):$(BIDDIR):$(RECIPEDIR):$(BITPATDIR):$(BLUESTUFFDIR):$(AXIDIR):$(BLUEBASICSDIR):$(BLUEUTILSDIR):$(RVFIDIIDIR)

RVBSSRCS = $(wildcard $(RVBSSRCDIR)/*.bsv)
RVBSSRCS += RVBS_Wrappers.bsv

BSCFLAGS = -p $(BSVPATH) -check-assert

ifdef MEM_SIZE
BSCFLAGS += -D MEM_SIZE=$(MEM_SIZE)
endif
ifdef MEM_IMG
BSCFLAGS += -D MEM_IMG="\"$(MEM_IMG)\""
endif
ifdef MEM_DELAY
BSCFLAGS += -D MEM_DELAY
endif

ifdef NO_LOGS
BSCFLAGS += -D NO_LOGS
endif
ifdef PRINT_ABI_REG_NAME
BSCFLAGS += -D PRINT_ABI_REG_NAME
endif

RVBSNAME := rvbs
BSCFLAGS += -D XLEN32
ifeq ($(XLEN),64)
BSCFLAGS += -D XLEN64
RVBSNAME := $(RVBSNAME)-rv64
else
RVBSNAME := $(RVBSNAME)-rv32
endif
RVBSNAME := $(RVBSNAME)i
ifeq ($(RVM),1)
BSCFLAGS += -D RVM
RVBSNAME := $(RVBSNAME)m
endif
ifeq ($(RVC),1)
BSCFLAGS += -D RVC
RVBSNAME := $(RVBSNAME)c
endif
ifeq ($(RVN),1)
BSCFLAGS += -D RVN
USER_MODE = 1
RVBSNAME := $(RVBSNAME)n
endif
ifeq ($(SUPERVISOR_MODE),1)
BSCFLAGS += -D SUPERVISOR_MODE
USER_MODE = 1
RVBSNAME := $(RVBSNAME)-s
endif
ifeq ($(USER_MODE),1)
BSCFLAGS += -D USER_MODE
RVBSNAME := $(RVBSNAME)u
endif
ifeq ($(PMP),1)
BSCFLAGS += -D PMP
RVBSNAME := $(RVBSNAME)-pmp
endif

# generated files directories
BUILDDIR = build
BDIR = $(BUILDDIR)/$(RVBSNAME)-bdir
SIMDIR = $(BUILDDIR)/$(RVBSNAME)-simdir

OUTPUTDIR = output
VDIR = $(OUTPUTDIR)/$(RVBSNAME)-vdir
INFODIR = $(OUTPUTDIR)/$(RVBSNAME)-info

BSCFLAGS += -show-schedule -sched-dot
#BSCFLAGS += -show-rule-rel \* \*
#BSCFLAGS += -steps-warn-interval n
BSCFLAGS += -steps-warn-interval 500000
BSCFLAGS += +RTS -K20M -RTS

BSC = bsc
#XXX Bluespec is not compatible with gcc > 4.9
#XXX This is actually problematic when using $test$plusargs/strings or something?
CC = gcc-4.8
CXX = g++-4.8

all: sim isa-test rvfi-dii verilog clint-verilog

sim: Top_sim.bsv $(RVBSSRCS)
	mkdir -p $(INFODIR)-sim $(BDIR)-sim $(SIMDIR)-sim $(OUTPUTDIR)
	$(BSC) $(BSCFLAGS) -bdir $(BDIR)-sim -simdir $(SIMDIR)-sim -info-dir $(INFODIR)-sim -sim -g mkRVBS_sim -u $<
	CC=$(CC) CXX=$(CXX) $(BSC) $(BSCFLAGS) -bdir $(BDIR)-sim -simdir $(SIMDIR)-sim -info-dir $(INFODIR)-sim -sim -o $(OUTPUTDIR)/$(RVBSNAME)-sim -e mkRVBS_sim $(BLUEUTILSDIR)/*.c $(SOCKETUTILSDIR)/*.c

isa-test: Top_isa_test.bsv $(RVBSSRCS)
	mkdir -p $(INFODIR)-isa-test $(BDIR)-isa-test $(SIMDIR)-isa-test $(OUTPUTDIR)
	$(BSC) $(BSCFLAGS) -bdir $(BDIR)-isa-test -simdir $(SIMDIR)-isa-test -info-dir $(INFODIR)-isa-test -sim -g mkRVBS_isa_test -u $<
	CC=$(CC) CXX=$(CXX) $(BSC) $(BSCFLAGS) -bdir $(BDIR)-isa-test -simdir $(SIMDIR)-isa-test -info-dir $(INFODIR)-isa-test -sim -o $(OUTPUTDIR)/$(RVBSNAME)-isa-test -e mkRVBS_isa_test $(BLUEUTILSDIR)/*.c $(SOCKETUTILSDIR)/*.c

rvfi-dii: Top_rvfi_dii.bsv $(RVBSSRCS)
	mkdir -p $(INFODIR)-rvfi-dii $(BDIR)-rvfi-dii $(SIMDIR)-rvfi-dii $(OUTPUTDIR)
	$(BSC) $(BSCFLAGS) -D RVFI_DII -bdir $(BDIR)-rvfi-dii -simdir $(SIMDIR)-rvfi-dii -info-dir $(INFODIR)-rvfi-dii -sim -g mkRVBS_rvfi_dii -u $<
	CC=$(CC) CXX=$(CXX) $(BSC) $(BSCFLAGS) -bdir $(BDIR)-rvfi-dii -simdir $(SIMDIR)-rvfi-dii -info-dir $(INFODIR)-rvfi-dii -sim -o $(OUTPUTDIR)/$(RVBSNAME)-rvfi-dii -e mkRVBS_rvfi_dii $(BLUEUTILSDIR)/*.c $(SOCKETUTILSDIR)/*.c

verilog: $(RVBSSRCS)
	mkdir -p $(INFODIR)-verilog $(BDIR)-verilog $(VDIR)-verilog
	$(BSC) $(BSCFLAGS) -bdir $(BDIR)-verilog -vdir $(VDIR)-verilog -info-dir $(INFODIR)-verilog -opt-undetermined-vals -unspecified-to X -D NO_LOGS -verilog -g mkRVBS_synth -u RVBS_Wrappers.bsv

.PHONY: clean clean-sim clean-isa-test clean-rvfi-dii clean-verilog
clean: clean-sim clean-isa-test clean-rvfi-dii clean-verilog
	rm -rf $(BUILDDIR)
clean-sim:
	rm -rf $(BDIR)-sim $(SIMDIR)-sim
clean-isa-test:
	rm -rf $(BDIR)-isa-test $(SIMDIR)-isa-test
clean-rvfi-dii:
	rm -rf $(BDIR)-rvfi-dii $(SIMDIR)-rvfi-dii
clean-verilog:
	rm -rf $(BDIR)-verilog

.PHONY: mrproper mrproper-sim mrproper-isa-test mrproper-rvfi-dii mrproper-verilog
mrproper: mrproper-sim mrproper-isa-test mrproper-rvfi-dii mrproper-verilog clean
	rm -rf $(OUTPUTDIR)
mrproper-sim: clean-sim
	#rm -rf $(INFODIR)-sim $(VDIR) $(OUTPUTDIR)-sim $(BUILDDIR)-sim
	rm -rf $(INFODIR)-sim
mrproper-isa-test: clean-isa-test
	rm -rf $(INFODIR)-isa-test
mrproper-rvfi-dii: clean-rvfi-dii
	rm -rf $(INFODIR)-rvfi-dii
mrproper-verilog: clean-verilog
	rm -rf $(INFODIR)-verilog $(VDIR)-verilog
