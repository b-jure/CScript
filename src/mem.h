#ifndef __SKOOMA_MEM_H___
#define __SKOOMA_MEM_H___

#include "common.h"

#include <memory.h>

void *reallocate(void *ptr, size_t oldCap, size_t newCap);

#endif
