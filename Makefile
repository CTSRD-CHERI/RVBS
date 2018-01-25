# BSV compiler flags
BITPATDIR=/home/aj443/devstuff/BitPat
BIDDIR=/home/aj443/devstuff/BID
BSVPATH=+:$(BITPATDIR):$(BIDDIR)
BSC = bsc
BSCFLAGS = -p $(BSVPATH) -check-assert

# Top level module
TOPFILE = Top.bsv
TOPMOD = top

.PHONY: sim
sim: $(TOPMOD)

$(TOPMOD): *.bsv
	$(BSC) $(BSCFLAGS) -sim -g $(TOPMOD) -u $(TOPFILE)
	$(BSC) $(BSCFLAGS) -sim -o $(TOPMOD) -e $(TOPMOD) $(BIDDIR)/*.c

.PHONY: clean
clean:
	rm -f *.cxx *.o *.h *.ba *.bo *.so *.ipinfo *.v $(TOPMOD)
	$(MAKE) -C $(BIDDIR) clean
