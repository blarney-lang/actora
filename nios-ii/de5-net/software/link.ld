C_STACK_SIZE = 1024;
RET_STACK_SIZE = 1024;
STACK_SIZE = 4096;
HEAP_SIZE = 28000;

SECTIONS
{
  . = 0;
  _gp = 0;
  .bss    : { *.o(.bss*) }
  .rodata : { *.o(.rodata*) }
  .sdata  : { *.o(.sdata*) }
  .data   : { *.o(.data*) }
  __dataEnd = .;
  . += C_STACK_SIZE;
  __stackBase = .;
  . += 4;
  __e_retBase = .;
  . += RET_STACK_SIZE;
  __e_stackBase = .;
  . += STACK_SIZE;
  __e_heapBase = .;
  . += HEAP_SIZE;
  __e_heapEnd = .;
  ASSERT(. < 0x10000, "Data space exceeded")
  . = 0x10000;
  __textBase = 0x10000;
  .text   : { *.o(.text*) }
  ASSERT(. < 0x14000, "Instruction space exceeded")
}
