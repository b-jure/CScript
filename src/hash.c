#include "common.h"
#include "hash.h"
#include "xxhash.h"

#define XXH64_SEED      31
#define HASH_INF        314159
#define SIZEOF_VOID_PTR sizeof(void*)

Hash Hash_ptr(const void* ptr)
{
    size_t y = (size_t)ptr;
    /* Pointer is likely aligned so the last 3-4 bits are 0,
     * rotate last 4 bits to improve hash. */
    y = (y >> 4) | (y << (8 * SIZEOF_VOID_PTR - 4));
    return (Hash)y;
}

Hash Hash_double(double dbl)
{
    if(is_infinity(dbl) || is_nan(dbl)) {
        return (dbl > 0) ? HASH_INF : -HASH_INF;
    }

    union BitCast {
        double   value;
        uint32_t ints[2];
    };

    union BitCast cast;
    cast.value = (dbl) + 1.0;
    return cast.ints[0] + cast.ints[1];
}

Hash Hash_string(const char* str, size_t len)
{
    return XXH64(str, len, XXH64_SEED);
}
