#ifndef _ELITE_H_
#define _ELITE_H_

#include <stdint.h>
#include <stdbool.h>

#define INLINE inline __attribute__((always_inline))

// Word: can be 16 or 32 or 64 bits
typedef uint32_t Unsigned;
typedef int32_t Signed;

// Tags
typedef uint8_t Tag;

#define PTR_APP   1
#define PTR_CONS  3
#define PTR_TUPLE 5
#define INT       2
#define ATOM      4
#define FUN       6

// Extract tag from word
INLINE uint32_t type(Tag t) { return t & 7; }
INLINE uint32_t ptrLen(Tag t) { return t >> 3; }
INLINE uint32_t funArity(Tag t) { return t >> 3; }
INLINE bool isPtr(Tag t) { return t & 1; }

// Construct tag
INLINE Tag makeTag(uint32_t t, uint32_t n)
  { return (n << 3) | t; }

#endif
