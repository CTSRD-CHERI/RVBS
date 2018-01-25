# BSV compiler flags
BITPATDIR = /home/aj443/devstuff/BitPat
BIDDIR = /home/aj443/devstuff/BID
BSVPATH = +:$(BITPATDIR):$(BIDDIR)
BSC = bsc
BSCFLAGS = -p $(BSVPATH) -check-assert
ifdef NO_LOGS
BSCFLAGS += -D NO_LOGS
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
	rm -f *.cxx *.o *.h *.ba *.bo *.so *.ipinfo *.v $(TOPMOD)
	$(MAKE) -C $(BIDDIR) clean
