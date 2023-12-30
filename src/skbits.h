#ifndef SKBITS_H
#define SKBITS_H

#include "skooma.h"


// https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html#index-_005f_005fbuiltin_005fctz
#if __has_builtin(__builtin_ctz)
#define sk_ctz(un) __builtin_ctz(un)
#endif


#if defined(SK_PRECOMPUTED_GOTO) && defined(sk_ctz)
#define callbitmask(vm, isva, arity, argc, retcnt)                                       \
    cast_uint(                                                                           \
        0 | (!sk_ensurestack(vm, retcnt) * 1) | (((isva) * ((arity) > (argc))) * 2) |    \
        ((!(isva) * ((arity) != (argc))) * 4) | (((vm)->fc == VM_FRAMES_MAX) * 8) | 16)


// TODO: typebitmask
#endif


// check is 'x' power of 2
#define ispow2(x) (((x) & ((x)-1)) == 0)

// Return bit at 'bit' (0 or 1) from 'x'.
#define BIT_CHECK(x, bit) ((x >> ((bit)-1)) & ((size_t)1))

// Toggle 'bit' from 'x'
#define BIT_TOGGLE(x, bit, toggle) (x) ^= (-(toggle) ^ (x)) & ((size_t)1 << (bit - 1))

// Set 'bit' from 'x'
#define BIT_SET(x, bit) ((x) |= ((size_t)1 << ((bit)-1)))

// Clear 'bit' from 'x'
#define BIT_CLEAR(x, bit) ((x) &= ~((size_t)1 << ((bit)-1)))

// Generate uint with 'bits' all set to 1
#define MAXBITS(bits) (~((size_t)0) >> ((sizeof(size_t) * 8) - (size_t)(bits)))

// Wrapper around MAXBITS, uses 'bytes' instead
#define MAXBYTES(bytes) MAXBITS((bytes) * 8)

// Max unsigned 24-bit value
#define UINT24_MAX cast(UInt, 0xffffff)

// Max bytecode size
#define BYTECODE_MAX UINT24_MAX


#endif
