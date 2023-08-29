#ifndef __SKOOMA_HASH_H__
#define __SKOOMA_HASH_H__

#include "common.h"

typedef size_t Hash;

Hash Hash_ptr(const void *ptr);
Hash Hash_double(double dbl);
Hash Hash_string(const char *str, size_t len);

#endif
