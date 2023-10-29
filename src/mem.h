#ifndef __SKOOMA_MEM_H___
#define __SKOOMA_MEM_H___

#include "common.h"
#include "compiler.h"
#include "vmachine.h"

#include <memory.h>

void*  reallocate(void* ptr, size_t newCap);
void*  gc_reallocate(VM* vm, Compiler* C, void* ptr, size_t oldc, size_t newc);
size_t gc(VM* vm, Compiler* C);
void   mark_obj(VM* vm, Obj* obj);

extern double gc_grow_factor;

#define mark_value(vm, value)                                                            \
    do {                                                                                 \
        if(IS_OBJ(value)) {                                                              \
            mark_obj(vm, AS_OBJ(value));                                                 \
        }                                                                                \
    } while(false)

/* Wrapper around reallocate that is equivalent to malloc, allocates 'bytes'
 * amount */
#define MALLOC(bytes) reallocate(NULL, bytes)

/* GC tracked memory alloc. */
#define GC_MALLOC(vm, c, bytes) gc_reallocate(vm, c, NULL, 0, bytes)

/* Wrapper around reallocate, equivalent to realloc */
#define REALLOC(ptr, newsize) reallocate(ptr, newsize)

/* GC tracked memory realloc. */
#define GC_REALLOC(vm, c, ptr, oldsize, newsize)                                         \
    gc_reallocate(vm, c, ptr, oldsize, newsize)

/* Free allocation at 'ptr' */
#define FREE(ptr) reallocate(ptr, 0)

/* Free GC tracked allocation. */
#define GC_FREE(vm, c, ptr, oldsize) gc_reallocate(vm, c, ptr, oldsize, 0)

/* Extracts the byte at the 'offset' from 'x' */
#define BYTE(x, offset) (((x) >> ((offset) * 8)) & 0xff)

/* Extracts the first 3 bytes from 'ptr' into 32-bit unsigned integer */
#define GET_BYTES3(ptr)                                                                  \
    (uint32_t)0 | ((uint32_t)(*(uint8_t*)((ptr) + 2)) << 16) |                           \
        ((uint32_t)(*(uint8_t*)((ptr) + 1)) << 8) | *(uint8_t*)(ptr)

/* Writes the first 3 bytes of 'x' to 'ptr' */
#define PUT_BYTES3(ptr, x)                                                               \
    *((uint8_t*)(ptr))     = BYTE((x), 0);                                               \
    *((uint8_t*)(ptr) + 1) = BYTE((x), 1);                                               \
    *((uint8_t*)(ptr) + 2) = BYTE((x), 2);

#endif
