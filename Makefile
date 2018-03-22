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
# Bluespec is not compatible with gcc > 4.9
# This is actually problematic when using $test$plusargs
CC = gcc-4.9
CXX = g++-4.9

# Top level module
TOPFILE = Top.bsv
TOPMOD = top

.PHONY: sim
sim: $(TOPMOD)

$(TOPMOD): *.bsv
	$(BSC) $(BSCFLAGS) -sim -g $(TOPMOD) -u $(TOPFILE)
	CC=$(CC) CXX=$(CXX) $(BSC) $(BSCFLAGS) -sim -o $(TOPMOD) -e $(TOPMOD) $(BIDDIR)/*.c

.PHONY: clean
clean:
	rm -f *.cxx *.o *.h *.ba *.bo *.so *.ipinfo *.v *.dot *.sched $(TOPMOD)
	$(MAKE) -C $(BIDDIR) clean
