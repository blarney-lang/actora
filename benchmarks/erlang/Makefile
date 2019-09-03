ERC_OPTS = +native -W0

BENCHMARKS = fib queens mss braun while adjoxo redblack shiftsub
BEAMS = $(patsubst %, %.beam, $(BENCHMARKS))
RUNS = $(patsubst %, run-%, $(BENCHMARKS))

.PHONY: all
all: $(BEAMS)

.PHONY: run
run: $(RUNS)

%.beam: %.erl
	erlc $(ERC_OPTS) $<

run-%: %.beam
	@echo "$<:" && escript $< && echo

.PHONY: clean
clean:
	rm -f *.beam
