BENCHMARKS = fib queens mss braun while adjoxo
CS = $(patsubst %, c-%, $(BENCHMARKS))
CRUNS = $(patsubst %, run-c-%, $(BENCHMARKS))
TIME = time -f "\ttime: %U"
ELITE = $(ELITE_ROOT)/compiler/elite

.PHONY: all
all: $(CS)

.PHONY: run-c
run-c: $(CRUNS)

$(ELITE):
	make -C $(ELITE_ROOT)/compiler

c-%: %.erl $(ELITE)
	$(ELITE) -c build-$@ $<
	make -C build-$@
	cp build-$@/main $@
	rm -rf build-$@

run-c-%: c-%
	@echo "$<:" && $(TIME) ./$< && echo

.PHONY: clean
clean:
	rm -f $(CS)
