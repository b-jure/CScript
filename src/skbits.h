#ifndef SKBITS_H
#define SKBITS_H


#include "skooma.h"



#if defined(__GNUC__) && __has_builtin(__builtin_ctz)
#define sk_ctz(un) __builtin_ctz(un)
#elif defined(_MSC_VER) // https://stackoverflow.com/a/20468180
#include <intrin.h>
uint32_t __inline ctz(uint32_t value)
{
    DWORD trailing_zero = 0;
    if(_BitScanForward(&trailing_zero, value)) return trailing_zero;
    else return 32;
}
#define sk_ctz(un) ctz(un)
#endif



#if defined(sk_ctz)
#define callbitmask(vm, isva, arity, argc, retcnt)                                       \
    cast_uint(                                                                           \
        0 | (!sk_ensurestack(vm, retcnt) * 1) |                                          \
        ((((isva) * ((arity) > (argc))) | (!(isva) * ((arity) != (argc)))) * 2) |        \
        (((vm)->fc == VM_CALLSTACK_LIMIT) * 4) | 8)

#define val2tbmask(v)                                                                    \
    cast_uint(                                                                           \
        0 | (IS_NIL(v) * 1) | (IS_NUMBER(v) * 2) | (IS_STRING(v) * 4) |                  \
        (IS_BOOL(v) * 8) | (IS_CLASS(v) * 16) | (IS_INSTANCE(v) * 32) |                  \
        (IS_FUNCTION(v) * 64) | (IS_CLOSURE(v) * 128) | (IS_NATIVE(v) * 256) |           \
        (IS_BOUND_METHOD(v) * 512))

#define val2tbmask_1(v)                                                                  \
    cast_uint(                                                                           \
        0 | (IS_NIL(v) * 1) | (IS_NUMBER(v) * 2) | (IS_BOOL(v) * 4) | (IS_OBJ(v) * 8))
#endif



/* Is 'x' power of 2 (assuming 'x' is positive integer) */
#define ispow2(x) (((x) & ((x)-1)) == 0)


/* Set 'bit' in 'x' */
#define bset(x, bit) ((x) |= ((uint64_t)1 << ((bit)-1)))
/* Clear 'bit' in 'x' */
#define bclear(x, bit) ((x) &= ~((uint64_t)1 << ((bit)-1)))
/* Test 'bit' in 'x' */
#define btest(x, bit) ((x >> ((bit)-1)) & ((uint64_t)1))
/* Toggle 'bit' in 'x' */
#define btoggle(x, bit, t) (x) ^= (-(toggle) ^ (x)) & ((uint64_t)1 << (bit - 1))


#endif
