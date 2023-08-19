#ifndef __SKOOMA_MEM_H___
#define __SKOOMA_MEM_H___

#include "common.h"

#include <memory.h>

/* Memory allocation/deallocation function (wrapper around realloc) */
void *reallocate(void *ptr, size_t oldCap, size_t newCap);

/* Wrapper around reallocate, allocates 'bytes' amount returning
 * the start of allocation back into 'ptr' */
#define MALLOC(ptr, bytes) reallocate((ptr), 0, bytes)

/* Wrapper around reallocate, frees the 'bytes' pointed by 'ptr' */
#define MFREE(ptr, bytes) reallocate((ptr), bytes, 0)

/* Extracts the byte at the 'offset' from 'x' */
#define BYTE(x, offset) (((x) >> (offset * 8)) & 0xff)

/* Extracts the first 3 bytes from 'ptr' into 32-bit unsigned integer */
#define GET_BYTES3(ptr)                                                        \
  (UInt)0 | ((UInt)(*(Byte *)(ptr + 2)) << 16) |                               \
      ((UInt)(*(Byte *)(ptr + 1)) << 8) | *(Byte *)ptr

/* Writes the first 3 bytes of 'x' to 'ptr' */
#define PUT_BYTES3(ptr, x)                                                     \
  *((Byte *)ptr) = BYTE(x, 0);                                                 \
  *((Byte *)ptr + 1) = BYTE(x, 1);                                             \
  *((Byte *)ptr + 2) = BYTE(x, 2);

#endif
