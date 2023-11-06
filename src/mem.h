#ifndef __SKOOMA_MEM_H___
#define __SKOOMA_MEM_H___

#include "common.h"
#include "compiler.h"
#include "vmachine.h"

#include <memory.h>

void*  reallocate(void* ptr, size_t newsize, void* userdata);
size_t gc(VM* vm);
void   mark_obj(VM* vm, Obj* obj);

#define CLEANUP(vm)                                                                        \
    do {                                                                                           \
        _cleanup_vm(vm);                                                                              \
        exit(EXIT_FAILURE);                                                                        \
    } while(false)

#define mark_value(vm, value)                                                                      \
    do {                                                                                           \
        if(IS_OBJ(value)) {                                                                        \
            mark_obj(vm, AS_OBJ(value));                                                           \
        }                                                                                          \
    } while(false)

/* Wrapper around reallocate that is equivalent to malloc, allocates 'bytes'
 * amount */
#define MALLOC(vm, bytes) (vm)->config.reallocate(NULL, bytes, (vm)->config.userdata)

/* GC tracked memory alloc. */
#define GC_MALLOC(vm, bytes) gc_reallocate(vm, NULL, 0, bytes)

/* Wrapper around reallocate, equivalent to realloc */
#define REALLOC(vm, ptr, newsize) (vm)->config.reallocate(ptr, newsize, (vm)->config.userdata)

/* Free allocation at 'ptr' */
#define FREE(vm, ptr) (vm)->config.reallocate(ptr, 0, (vm)->config.userdata)

/* Free GC tracked allocation. */
#define GC_FREE(vm, ptr, oldsize) gc_free(vm, ptr, oldsize, 0)

/* Extracts the byte at the 'offset' from 'x' */
#define BYTE(x, offset) (((x) >> ((offset) * 8)) & 0xff)

/* Extracts the first 3 bytes from 'ptr' into 32-bit unsigned integer */
#define GET_BYTES3(ptr)                                                                            \
    (uint32_t)0 | ((uint32_t)(*(uint8_t*)((ptr) + 2)) << 16) |                                     \
        ((uint32_t)(*(uint8_t*)((ptr) + 1)) << 8) | *(uint8_t*)(ptr)

/* Writes the first 3 bytes of 'x' to 'ptr' */
#define PUT_BYTES3(ptr, x)                                                                         \
    *((uint8_t*)(ptr))     = BYTE((x), 0);                                                         \
    *((uint8_t*)(ptr) + 1) = BYTE((x), 1);                                                         \
    *((uint8_t*)(ptr) + 2) = BYTE((x), 2);

#endif
