all:

.PHONY: clean
clean:
	make -C compiler clean
	make -C emulator clean
	make -C benchmarks clean
	make -C benchmarks/erlang clean
	make -C nios-ii/de5-net clean
	make -C nios-ii/de10-pro clean
	make -C rtl clean
	make -C emulator clean
