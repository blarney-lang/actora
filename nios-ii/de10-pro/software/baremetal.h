#ifndef _BAREMETAL_H_
#define _BAREMETAL_H_

#include <stdint.h>

int putchar(int ch);
int puts(const char* s);
int puthex(unsigned x);
int printf(const char* fmt, ...);

#define NUM_REGS 7

typedef uint32_t regs_t[NUM_REGS];
void getRegs(regs_t regs);

#define PERF_BASE ((uint8_t*) 0x10000)

inline void perf_reset()
{
  __builtin_stwio(PERF_BASE, 1);
}

inline void perf_start(int n)
{
  __builtin_stwio(PERF_BASE + 4*(n*4+1), 0);
}

inline void perf_stop(int n)
{
  __builtin_stwio(PERF_BASE + 4*(n*4), 0);
}

inline uint32_t perf_get_hi(int n)
{
  uint32_t hi = __builtin_ldwio(PERF_BASE + 4*(n*4+1));
  return hi;
}

inline uint32_t perf_get_lo(int n)
{
  uint32_t lo = __builtin_ldwio(PERF_BASE + 4*(n*4));
  return lo;
}

#endif
