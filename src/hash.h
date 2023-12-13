#ifndef SKOOMA_HASH_H
#define SKOOMA_HASH_H

#include "common.h"

typedef size_t Hash;

Hash dblhash(double dbl);
Hash stringhash(const char* str, size_t len);

#endif
