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
ifeq ($(RVFI_DII),1)
BSCFLAGS += -D RVFI_DII
RVBSNAME := $(RVBSNAME)-rvfi-dii
endif

# generated files directories
BUILDDIR = build
BDIR = $(BUILDDIR)/$(RVBSNAME)-bdir
SIMDIR = $(BUILDDIR)/$(RVBSNAME)-simdir

OUTPUTDIR = output
VDIR = $(OUTPUTDIR)/$(RVBSNAME)-vdir
INFODIR = $(OUTPUTDIR)/$(RVBSNAME)-info

BSCFLAGS += -bdir $(BDIR)
BSCFLAGS += -info-dir $(INFODIR)
BSCFLAGS += -show-schedule -sched-dot
#BSCFLAGS += -show-rule-rel \* \*
#BSCFLAGS += -steps-warn-interval n
BSCFLAGS += -steps-warn-interval 500000
BSCFLAGS += +RTS -K20M -RTS

BSC = bsc
# Bluespec is not compatible with gcc > 4.9
# This is actually problematic when using $test$plusargs
CC = gcc-4.8
CXX = g++-4.8

# Top level module
ifeq ($(RVFI_DII), 1)
SIMTOPFILE = TopRVBS.bsv
SIMTOPMOD = mkRVBS
else
SIMTOPFILE = TopSim.bsv
SIMTOPMOD = top
endif
VERILOGTOPFILE = TopRVBS.bsv
VERILOGTOPMOD = rvbs

.PHONY: sim verilog

all: sim
sim: $(SIMTOPMOD)

$(SIMTOPMOD): $(RVBSSRCDIR)/*.bsv
	mkdir -p $(INFODIR) $(BDIR) $(SIMDIR) $(OUTPUTDIR)
	$(BSC) $(BSCFLAGS) -simdir $(SIMDIR) -sim -g $(SIMTOPMOD) -u $(SIMTOPFILE)
	CC=$(CC) CXX=$(CXX) $(BSC) $(BSCFLAGS) -simdir $(SIMDIR) -sim -o $(OUTPUTDIR)/$(RVBSNAME) -e $(SIMTOPMOD) $(BLUEUTILSDIR)/*.c $(SOCKETUTILSDIR)/*.c

verilog: $(RVBSSRCDIR)/*.bsv
	mkdir -p $(INFODIR) $(BDIR) $(VDIR)
	$(BSC) $(BSCFLAGS) -vdir $(VDIR) -opt-undetermined-vals -unspecified-to X -D NO_LOGS -verilog -g $(VERILOGTOPMOD) -u $(VERILOGTOPFILE)

.PHONY: clean mrproper
clean:
	rm -rf $(BDIR) $(SIMDIR)
mrproper: clean
	rm -rf $(INFODIR) $(VDIR) $(OUTPUTDIR) $(BUILDDIR)
