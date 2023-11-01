#include "common.h"
#include "hash.h"
#include "skmath.h"
#include "xxhash.h"

#define XXH64_SEED 31
#define HASH_INF   314159

Hash Hash_double(double dbl)
{
    if(is_infinity(dbl) || is_nan(dbl)) {
        return (dbl > 0) ? HASH_INF : -HASH_INF;
    }

    union {
        double   value;
        uint32_t ints[2];
    } bitcast;

    bitcast.value = (dbl) + 1.0;
    return bitcast.ints[0] + bitcast.ints[1];
}

Hash Hash_string(const char* str, size_t len)
{
    return XXH64(str, len, XXH64_SEED);
}
