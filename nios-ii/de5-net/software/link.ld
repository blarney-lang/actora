SECTIONS
{
  __textBase = 0x0;
  .text   : { *.o(.text*) }
  ASSERT(. < 0x04000, "Instruction space exceeded")
  . = 0x100000;
  _gp = 0;
  .bss    : { *.o(.bss*) }
  .rodata : { *.o(.rodata*) }
  .sdata  : { *.o(.sdata*) }
  .data   : { *.o(.data*) }
  __dataEnd = .;
  __e_heapBase = .;
  . += HEAP_SIZE;
  __e_heapEnd = .;
  . += STACK_SIZE;
  __stackBase = . - 4;
  ASSERT(. < 1310720, "Data space exceeded")
}
