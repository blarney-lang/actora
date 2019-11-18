BLARNEY_ROOT = $(ACTORA_ROOT)/blarney
BLC = $(BLARNEY_ROOT)/Scripts/blc

.PHONEY: all
all: Config.h
	$(BLC) Main.hs

Config.h: Config.py
	./Config.py syn hw-cpp > Config.h

clean:
	rm -rf *.o *.hi Main *-Verilog Config.h
