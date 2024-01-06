#include "common.h"
#include "hash.h"
#include "skmath.h"
#include "xxhash.h"

#define HASH_INF 314159

/* Hash double */
Hash dblhash(double dbl)
{
    if(sk_isinf(dbl) || sk_isnan(dbl))
        return (dbl > 0) ? cast_hash(HASH_INF) : cast_hash(-HASH_INF);
    union {
        double value;
        uint32_t ints[2];
    } bitcast;
    bitcast.value = (dbl) + 1.0;
    return bitcast.ints[0] + bitcast.ints[1];
}

/* Hash string (xxHash), strings get special hash. */
Hash stringhash(const char* str, size_t len, unsigned long seed)
{
    return XXH64(str, len, seed);
}

/* Hash pointer (for objects except strings) */
Hash ptrhash(const void* ptr)
{
    uintptr_t x = (uintptr_t)ptr;
    // https://github.com/python/cpython/blob/3375dfed400494ba5cc1b744d52f6fb8b7796059/Include/internal/pycore_pyhash.h#L10
    x = (x >> 4) | (x << (8 * sizeof(uintptr_t) - 4));
    return cast_hash(x);
}
