C_STACK_SIZE = 1024;
RET_STACK_SIZE = 1024;
STACK_SIZE = 4096;
HEAP_SIZE = 28000;

SECTIONS
{
  . = 0x10000;
  .text   : { *.o(.text*) }
  ASSERT(. < 0x14000, "Instruction space exceeded")
  . = 0x20000;
  _gp = 0x20000;
  .bss    : { *.o(.bss*) } = 0
  .rodata : { *.o(.rodata*) }
  .sdata  : { *.o(.sdata*) }
  .data   : { *.o(.data*) }
  . += C_STACK_SIZE;
  __stackBase = ALIGN(.);
  . += 4;
  __e_retBase = ALIGN(.);
  . += RET_STACK_SIZE;
  __e_stackBase = ALIGN(.);
  . += STACK_SIZE;
  __e_heapBase = ALIGN(.);
  . += HEAP_SIZE;
  __e_heapEnd = ALIGN(.);
  ASSERT(. < 0x30000, "Data space exceeded")
  . = 0x30000;
}
