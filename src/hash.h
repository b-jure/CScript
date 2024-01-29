#ifndef SKOOMA_HASH_H
#define SKOOMA_HASH_H

#include "common.h"

typedef uint64_t Hash;

Hash dblhash(sk_number dbl);
Hash stringhash(const char* str, size_t len, unsigned long seed);
Hash ptrhash(const void* ptr);

#endif
