#ifndef __SKOOMA_MEM_H___
#define __SKOOMA_MEM_H___

#include "common.h"
#include "vmachine.h"

#include <memory.h>

void*  reallocate(void* ptr, size_t newsize, void* userdata);
size_t gc(VM* vm);
void   omark(VM* vm, O* obj);

#define CLEANUP(vm)                                                             \
    do {                                                                        \
        _cleanupvm(vm);                                                         \
        exit(EXIT_FAILURE);                                                     \
    } while(false)

#define vmark(vm, value)                                                        \
    do {                                                                        \
        if(IS_OBJ(value)) {                                                     \
            omark(vm, AS_OBJ(value));                                           \
        }                                                                       \
    } while(false)

/* Wrapper around reallocate that is equivalent to malloc, allocates 'bytes'
 * amount */
#define MALLOC(vm, bytes)                                                       \
    (vm)->config.reallocate(NULL, bytes, (vm)->config.userdata)

/* GC tracked memory alloc. */
#define GC_MALLOC(vm, bytes) gcrealloc(vm, NULL, 0, bytes)

/* Wrapper around reallocate, equivalent to realloc */
#define REALLOC(vm, ptr, newsize)                                               \
    (vm)->config.reallocate(ptr, newsize, (vm)->config.userdata)

/* Free allocation at 'ptr' */
#define FREE(vm, ptr) (vm)->config.reallocate(ptr, 0, (vm)->config.userdata)

/* Free GC tracked allocation. */
#define GC_FREE(vm, ptr, oldsize) gcfree(vm, ptr, oldsize, 0)

/* Extracts the byte at the 'offset' from 'x' */
#define BYTE(x, offset) (((x) >> ((offset) * 8)) & 0xff)

/* Cast pointer into byte pointer */
#define byteptr(ptr) ((uint8_t*)(ptr))
/* Cast value into 32-bit unsigned integer */
#define uintcast(val) ((uint32_t)(val))

/* Extracts the first 3 bytes from 'ptr' into 32-bit unsigned integer */
#define GET_BYTES3(ptr)                                                         \
    (uintcast(0) | ((uintcast(*(byteptr(ptr) + 2))) << 16) |                    \
     (uintcast(*(byteptr(ptr) + 1)) << 8) | uintcast(*byteptr(ptr)))

/* Writes the first 3 bytes of 'x' to 'ptr' */
#define PUT_BYTES3(ptr, x)                                                      \
    do {                                                                        \
        *(byteptr(ptr))     = BYTE(x, 0);                                       \
        *(byteptr(ptr) + 1) = BYTE(x, 1);                                       \
        *(byteptr(ptr) + 2) = BYTE(x, 2);                                       \
    } while(false)

#endif
