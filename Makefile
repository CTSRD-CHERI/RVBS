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

BIDDIR = BID
RECIPEDIR = $(BIDDIR)/Recipe
BITPATDIR = $(BIDDIR)/BitPat
BSVPATH = +:$(BIDDIR):$(RECIPEDIR):$(BITPATDIR)
BSC = bsc
BSCFLAGS = -p $(BSVPATH) -check-assert
BSCFLAGS += -show-schedule -sched-dot
#BSCFLAGS += -show-rule-rel \* \*
#BSCFLAGS += +RTS -K18388608 -RTS
ifdef MEM_SIZE
BSCFLAGS += -D MEM_SIZE=$(MEM_SIZE)
endif
ifdef MEM_IMG
BSCFLAGS += -D MEM_IMG="\"$(MEM_IMG)\""
endif
ifdef NO_LOGS
BSCFLAGS += -D NO_LOGS
endif
ifdef PRINT_ABI_REG_NAME
BSCFLAGS += -D PRINT_ABI_REG_NAME
endif
BSCFLAGS += -D XLEN32
ifeq ($(XLEN),64)
BSCFLAGS += -D XLEN64
endif
ifeq ($(PMP),1)
BSCFLAGS += -D PMP
endif
ifeq ($(USER_MODE),1)
BSCFLAGS += -D USER_MODE
endif
ifeq ($(SUPERVISOR_MODE),1)
BSCFLAGS += -D SUPERVISOR_MODE
endif
ifeq ($(RVM),1)
BSCFLAGS += -D RVM
endif
ifeq ($(RVC),1)
BSCFLAGS += -D RVC
endif
ifeq ($(RVN),1)
BSCFLAGS += -D RVN
endif
# Bluespec is not compatible with gcc > 4.9
# This is actually problematic when using $test$plusargs
CC = gcc-4.8
CXX = g++-4.8

# Top level module
TOPFILE = Top.bsv
TOPMOD = rvbs

.PHONY: sim
sim: $(TOPMOD)

$(TOPMOD): *.bsv
	$(BSC) $(BSCFLAGS) -sim -g $(TOPMOD) -u $(TOPFILE)
	CC=$(CC) CXX=$(CXX) $(BSC) $(BSCFLAGS) -sim -o $(TOPMOD) -e $(TOPMOD) $(BIDDIR)/*.c

verilog: *.bsv
	$(BSC) $(BSCFLAGS) -D NO_LOGS -verilog -g $(TOPMOD) -u $(TOPFILE)

.PHONY: clean
clean:
	rm -f *.cxx *.o *.h *.ba *.bo *.so *.ipinfo *.v *.dot *.sched $(TOPMOD)
	$(MAKE) -C $(BIDDIR) clean
