#ifndef __SKOOMA_COMMON_H__
#define __SKOOMA_COMMON_H__

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/* Leave defined for debug build */
#define DEBUG_TRACE_EXECUTION

typedef uint8_t Byte;
typedef uint32_t UInt;

/* GCC attribute definitions */
#define __UNUSED__ __attribute__((unused))
#define __FORCE_INLINE__ __attribute__((always_inline)) inline
#define LIKELY(cond) __glibc_likely(cond)
#define UNLIKELY(cond) __glibc_unlikely(cond)
#define UNREACHABLE() __builtin_unreachable()

#endif
