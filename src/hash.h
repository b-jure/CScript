#ifndef __SKOOMA_HASH_H__
#define __SKOOMA_HASH_H__

#include "common.h"

typedef size_t Hash;

Hash ptrhash(const void* ptr);
Hash dblhash(double dbl);
Hash stringhash(const char* str, size_t len);

#endif
