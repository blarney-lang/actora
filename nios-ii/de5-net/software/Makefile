# Binary utilities
CC      = nios2-elf-gcc
LD      = nios2-elf-ld
OBJCOPY = nios2-elf-objcopy

# Compiler and linker flags
CFLAGS  = -O2 -I. -static \
          -fvisibility=hidden -nostdlib -nostartfiles \
          -fsingle-precision-constant -fno-builtin-printf \
          -ffp-contract=off -fno-builtin

.PHONY: all
all: main.elf

baremetal.o: baremetal.c
	$(CC) $(CFLAGS) -Wall -c baremetal.c -o baremetal.o

main.o: main.c
	$(CC) $(CFLAGS) -Wall -c main.c -o main.o

entry.o: entry.S
	$(CC) $(CFLAGS) -Wall -c -o entry.o entry.S

main.elf: entry.o main.o baremetal.o link.ld
	$(LD) $(LDFLAGS) -T link.ld -o main.elf entry.o main.o baremetal.o

.PHONY: download-elf
download-elf: main.elf
	nios2-download -r -g main.elf

.PHONY: clean
clean:
	rm -f *.o *.elf
