BENCHMARKS = fib queens mss braun while adjoxo redblack shiftsub
CS = $(patsubst %, c-%, $(BENCHMARKS))
CRUNS = $(patsubst %, run-c-%, $(BENCHMARKS))
ACTS = $(patsubst %, %.act, $(BENCHMARKS))
ACTRUNS = $(patsubst %, run-act-%, $(BENCHMARKS))
TIME = time -f "\ttime: %U"
ELITE = $(ACTORA_ROOT)/compiler/elite
ACTEMU = $(ACTORA_ROOT)/emulator/actemu

.PHONY: all
all: $(CS) $(REDS)

.PHONY: run-c
run-c: $(CRUNS)

.PHONY: run-act
run-act: $(ACTRUNS)

$(ELITE):
	make -C $(ACTORA_ROOT)/compiler

$(REDEMU):
	make -C $(ACTORA_ROOT)/emulator

%.act: %.erl $(ELITE)
	$(ELITE) -b $< > $@

c-%: %.erl $(ELITE)
	$(ELITE) -c build-$@ $<
	make -C build-$@
	cp build-$@/main $@
	rm -rf build-$@

run-c-%: c-%
	@echo "$<:" && $(TIME) ./$< && echo

run-act-%: %.act $(ACTEMU)
	@echo "$<:" && $(ACTEMU) ./$< && echo

.PHONY: clean
clean:
	rm -f $(CS) $(REDS)
