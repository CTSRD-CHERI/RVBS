# BSV compiler flags
RECIPEDIR = /home/aj443/devstuff/Recipe
BITPATDIR = /home/aj443/devstuff/BitPat
BIDDIR = /home/aj443/devstuff/BID
BSVPATH = +:$(RECIPEDIR):$(BITPATDIR):$(BIDDIR)
BSC = bsc
BSCFLAGS = -p $(BSVPATH) -check-assert
BSCFLAGS += -show-schedule -sched-dot
#BSCFLAGS += -show-rule-rel \* \*
ifdef NO_LOGS
BSCFLAGS += -D NO_LOGS
endif
BSCFLAGS += -D XLEN32
ifeq ($(XLEN),64)
BSCFLAGS += -D XLEN64
endif
ifdef RVC
BSCFLAGS += -D RVC
endif
ifdef PMP
BSCFLAGS += -D PMP
endif
ifdef USER_MODE
BSCFLAGS += -D USER_MODE
MULTI_MODE=1
endif
ifdef SUPERVISOR_MODE
BSCFLAGS += -D SUPERVISOR_MODE
MULTI_MODE=1
endif
ifdef MULTI_MODE
BSCFLAGS += -D MULTI_MODE
endif
# Bluespec is not compatible with gcc > 4.9
# This is actually problematic when using $test$plusargs
CC = gcc-4.9
CXX = g++-4.9

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
