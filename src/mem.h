#ifndef __SKOOMA_MEM_H___
#define __SKOOMA_MEM_H___

#include "common.h"
#include "compiler.h"
#include "vmachine.h"

#include <memory.h>

#define SK_ALLOCATOR // @MAYBE_REMOVE? Redeclaration guard
/* Memory allocation/deallocation function (wrapper around realloc) */
void *reallocate(void *ptr, size_t newCap);
void *c_reallocate(Compiler *c, void *ptr, size_t oldsize, size_t newsize);

/* Copy 'len' bytes from 'src' returning null-terminated cstring (VM) */
#define vm_strncopy(vm, src, len)                                              \
  ({                                                                           \
    char *str = VM_MALLOC(vm, len + 1);                                        \
    memcpy(str, src, len);                                                     \
    str[len] = '\0';                                                           \
    str;                                                                       \
  })

/* Copy 'len' bytes from 'src' returning null-terminated cstring (Compiler) */
#define c_strncopy(c, src, len)                                                \
  ({                                                                           \
    char *str = C_MALLOC(c, len + 1);                                          \
    memcpy(str, src, len);                                                     \
    str[len] = '\0';                                                           \
    str;                                                                       \
  })

/* Wrapper around reallocate that is equivalent to malloc, allocates 'bytes'
 * amount */
#define MALLOC(bytes) reallocate(NULL, bytes)
/* @MAYBE_REMOVE? Allocate 'bytes' managed by VM. */
#define VM_MALLOC(vm, bytes) vm_reallocate(vm, NULL, 0, bytes)
/* @MAYBE_REMOVE? Allocate 'bytes' managed by Compiler. */
#define C_MALLOC(c, bytes) c_reallocate(c, NULL, 0, bytes)

/* Wrapper around reallocate, equivalent to realloc */
#define REALLOC(ptr, newsize) reallocate(ptr, newsize)
/* @MAYBE_REMOVE? Reallocate allocation managed by VM. */
#define VM_REALLOC(vm, ptr, oldsize, newsize)                                  \
  vm_reallocate(vm, ptr, oldsize, newsize)
/* @MAYBE_REMOVE? Reallocate allocation managed by Compiler. */
#define C_REALLOC(c, ptr, oldsize, newsize)                                    \
  c_reallocate(c, ptr, oldsize, newsize)

/* Free allocation at 'ptr' */
#define FREE(ptr) reallocate(ptr, 0)

/* Free allocation and update gc */
#define VM_FREE(vm, ptr, oldsize) vm_reallocate(vm, ptr, oldsize, 0)

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
