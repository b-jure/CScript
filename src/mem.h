/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Skooma.
 * Skooma is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Skooma is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Skooma.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#ifndef SKMEM_H
#define SKMEM_H

#include "common.h"
#include "vmachine.h"

#include <memory.h>


size_t gc(VM* vm);
void omark(VM* vm, O* obj);

#define vmark(vm, value)                                                                           \
    do {                                                                                           \
        if(IS_OBJ(value)) omark(vm, AS_OBJ(value));                                                \
    } while(false)

/* Wrapper around reallocate that is equivalent to malloc, allocates 'bytes'
 * amount */
#define MALLOC(vm, bytes) (vm)->hooks.reallocate(NULL, bytes, (vm)->hooks.userdata)

/* GC tracked memory alloc. */
#define GC_MALLOC(vm, bytes) gcrealloc(vm, NULL, 0, bytes)

/* Wrapper around reallocate, equivalent to realloc */
#define REALLOC(vm, ptr, newsize) (vm)->hooks.reallocate(ptr, newsize, (vm)->hooks.userdata)

/* Free allocation at 'ptr' */
#define FREE(vm, ptr) (vm)->hooks.reallocate(ptr, 0, (vm)->hooks.userdata)

/* Free GC tracked allocation. */
#define GC_FREE(vm, ptr, oldsize) gcfree(vm, ptr, oldsize, 0)

/* Extracts the byte at the 'offset' from 'x' */
#define BYTE(x, offset) (((x) >> ((offset) * 8)) & 0xff)

/* Cast pointer into byte pointer */
#define byteptr(ptr) ((uint8_t*)(ptr))
/* Cast value into 32-bit unsigned integer */
#define uintcast(val) ((uint32_t)(val))

/* Extracts the first 3 bytes from 'ptr' into 32-bit unsigned integer */
#define GET_BYTES3(ptr)                                                                            \
    (uintcast(0) | ((uintcast(*(byteptr(ptr) + 2))) << 16) |                                       \
     (uintcast(*(byteptr(ptr) + 1)) << 8) | uintcast(*byteptr(ptr)))

/* Writes the first 3 bytes of 'x' to 'ptr' */
#define PUT_BYTES3(ptr, x)                                                                         \
    do {                                                                                           \
        *(byteptr(ptr)) = BYTE(x, 0);                                                              \
        *(byteptr(ptr) + 1) = BYTE(x, 1);                                                          \
        *(byteptr(ptr) + 2) = BYTE(x, 2);                                                          \
    } while(false)

#endif
