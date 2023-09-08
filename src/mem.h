#ifndef __SKOOMA_MEM_H___
#define __SKOOMA_MEM_H___

#include "common.h"

#include <memory.h>

/* Memory allocation/deallocation function (wrapper around realloc) */
void *reallocate(void *ptr, size_t oldCap, size_t newCap);
/* Copy 'len' bytes from 'src' returning null-terminated cstring */
char *strncopy(const char *src, UInt len);

/* Wrapper around reallocate that is equivalent to malloc, allocates 'bytes'
 * amount */
#define MALLOC(bytes) reallocate(NULL, 0, bytes)

/* Wrapper around reallocate, equivalent to realloc */
#define REALLOC(ptr, oldsize, newsize) reallocate(ptr, oldsize, newsize)

/* Wrapper around reallocate, allocates a new array */
#define ALLOC_ARRAY(type, size)                                                \
  (type *)reallocate(NULL, 0, sizeof(type) * (size))

/* Wrapper around reallocate, realloc's array at 'ptr' */
#define REALLOC_ARRAY(type, ptr, old_cap, new_cap)                             \
  (type *)reallocate((ptr), (old_cap) * sizeof(type), (new_cap) * sizeof(type))

/* Wrapper around reallocate, frees the 'bytes' pointed by 'ptr' */
#define MFREE(ptr, bytes) reallocate(ptr, bytes, 0)

/* Frees the 'list' using 'free_fn' */
#define MFREE_LIST(list, free_fn)                                              \
  do {                                                                         \
    Obj *head = list;                                                          \
    for (Obj *object = list; head != NULL; object = head->next) {              \
      head = object->next;                                                     \
      free_fn(object);                                                         \
    }                                                                          \
  } while (false)

/* Wrapper around reallocate, frees the array at 'ptr' of type 'type' */
#define MFREE_ARRAY(type, ptr, cap) reallocate(ptr, (cap) * sizeof(type), 0)

/* Returns the new array capacity */
#define GROW_ARRAY_CAPACITY(cap) ((cap) < 8 ? 8 : (cap)*2)

/* Grows the array at 'ptr' to 'new_cap' */
#define GROW_ARRAY(type, ptr, old_cap, new_cap)                                \
  (type *)reallocate(ptr, sizeof(type) * (old_cap), sizeof(type) * (new_cap))

/* Extracts the byte at the 'offset' from 'x' */
#define BYTE(x, offset) (((x) >> ((offset)*8)) & 0xff)

/* Extracts the first 3 bytes from 'ptr' into 32-bit unsigned integer */
#define GET_BYTES3(ptr)                                                        \
  (uint32_t)0 | ((uint32_t)(*(uint8_t *)((ptr) + 2)) << 16) |                  \
      ((uint32_t)(*(uint8_t *)((ptr) + 1)) << 8) | *(uint8_t *)(ptr)

/* Writes the first 3 bytes of 'x' to 'ptr' */
#define PUT_BYTES3(ptr, x)                                                     \
  *((uint8_t *)(ptr)) = BYTE((x), 0);                                          \
  *((uint8_t *)(ptr) + 1) = BYTE((x), 1);                                      \
  *((uint8_t *)(ptr) + 2) = BYTE((x), 2);

#endif
