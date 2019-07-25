_entry:

# 8 instructions before exception handler
nop
nop
nop
nop
nop
nop
nop
jmpi _exception_handler_end

_exception_handler:
jmpi _exception_handler

_exception_handler_end:

# Set stack pointer
movia sp, __stackBase
movia gp, _gp

# Jump to main
movia ra, _main_ret
jmpi main
_main_ret:

# Loop forever
jmpi .
