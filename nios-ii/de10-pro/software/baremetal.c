#include <stdarg.h>
#include <baremetal.h>

#define UART_DATA 0x10000000
#define UART_CTRL 0x10000004

int putchar(int ch)
{
  volatile unsigned* ctrl = (volatile unsigned*) UART_CTRL;
  volatile unsigned* data = (volatile unsigned*) UART_DATA;
  while ((*ctrl >> 16) == 0) {}
  *data = ch;
  return ch;
}

int puts(const char* s)
{
  int count = 0;
  while (*s) { putchar(*s); s++; count++; }
  return count;
}

int puthex(unsigned x)
{
  int count = 0;

  for (count = 0; count < 8; count++) {
    unsigned nibble = x >> 28;
    putchar(nibble > 9 ? ('a'-10)+nibble : '0'+nibble);
    x = x << 4;
  }

  return 8;
}

int printf(const char* fmt, ...)
{
  int count = 0;
  va_list args;

  va_start(args, fmt);

  while (*fmt) {
    if (*fmt == '%') {
      fmt++;
      if (*fmt == '\0') break;
      if (*fmt == 's') count += puts(va_arg(args, char*));
      if (*fmt == 'x') count += puthex(va_arg(args, unsigned));
    }
    else { putchar(*fmt); count++; }
    fmt++;
  }

  va_end(args);

  return count;
}

void getRegs(regs_t regs)
{ 
  register uint32_t r16 asm ("r16");
  register uint32_t r17 asm ("r17");
  register uint32_t r18 asm ("r18");
  register uint32_t r19 asm ("r19");
  register uint32_t r20 asm ("r20");
  register uint32_t r21 asm ("r21");
  register uint32_t r22 asm ("r22");
  regs[0] = r16;
  regs[1] = r17;
  regs[2] = r18;
  regs[3] = r19;
  regs[4] = r20;
  regs[5] = r21;
  regs[6] = r22;
} 
