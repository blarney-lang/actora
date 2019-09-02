BENCHMARKS = fib queens mss braun while adjoxo redblack
CS = $(patsubst %, c-%, $(BENCHMARKS))
CRUNS = $(patsubst %, run-c-%, $(BENCHMARKS))
REDS = $(patsubst %, %.red, $(BENCHMARKS))
REDRUNS = $(patsubst %, run-red-%, $(BENCHMARKS))
TIME = time -f "\ttime: %U"
ELITE = $(ELITE_ROOT)/compiler/elite
REDEMU = $(ELITE_ROOT)/emulator/redemu

.PHONY: all
all: $(CS) $(REDS)

.PHONY: run-c
run-c: $(CRUNS)

.PHONY: run-red
run-red: $(REDRUNS)

$(ELITE):
	make -C $(ELITE_ROOT)/compiler

$(REDEMU):
	make -C $(ELITE_ROOT)/emulator

%.red: %.erl $(ELITE)
	$(ELITE) -b $< > $@

c-%: %.erl $(ELITE)
	$(ELITE) -c build-$@ $<
	make -C build-$@
	cp build-$@/main $@
	rm -rf build-$@

run-c-%: c-%
	@echo "$<:" && $(TIME) ./$< && echo

run-red-%: %.red $(REDEMU)
	@echo "$<:" && $(REDEMU) ./$< && echo

.PHONY: clean
clean:
	rm -f $(CS) $(REDS)
