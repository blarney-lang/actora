all:

.PHONY: clean
clean:
	make -C compiler clean
	make -C nios-ii/de5-net clean
	make -C nios-ii/de5-net/software clean
