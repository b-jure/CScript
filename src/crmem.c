/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include "crdebug.h"
#include "crhashtable.h"
#include "crmem.h"
#include "crobject.h"
#include "crparser.h"
#include "crconf.h"
#include "crvalue.h"
#include "crvm.h"

#include <stdio.h>
#include <stdlib.h>



/* can try to allocate second time */
#define cantryagain(vm)		(vminitialized(vm) && !(vm)->gc.stopem)



/* Auxiliary to 'cr_mm_realloc' and 'cr_malloc'. */
cr_sinline void *tryagain(VM *vm, void *ptr, size_t osize, size_t nsize)
{
	if (cantryagain(vm)) {
		cr_gc_full(vm);
		return cr_mm_rawrealloc(vm, ptr, nsize);
	}
	return NULL;
}


void *cr_mm_realloc(VM *vm, void *ptr, size_t osize, size_t nsize)
{
	void *memblock;

	cr_assert((osize == 0) == (ptr == NULL));
	memblock = cr_mm_rawrealloc(vm, ptr, nsize);
	if (cr_unlikely(!memblock && nsize != 0)) {
		memblock = tryagain(vm, ptr, osize, nsize);
		if (cr_unlikely(memblock == NULL))
			return NULL;
	}
	cr_assert((nsize == 0) == (memblock == NULL));
	vm->gc.allocated += nsize - osize;
	return memblock;
}


void *cr_mm_saferealloc(VM *vm, void *ptr, size_t osize, size_t nsize)
{
	void *memblock;

	memblock = cr_mm_realloc(vm, ptr, osize, nsize);
	if (cr_unlikely(memblock == NULL && nsize != 0))
		cr_assert(0 && "out of memory");
	return memblock;
}


void *cr_mm_malloc(VM *vm, size_t size)
{
	void *memblock;

	if (size == 0)
		return NULL;
	memblock = cr_mm_rawmalloc(vm, size);
	if (cr_unlikely(memblock == NULL)) {
		memblock = tryagain(vm, NULL, 0, size);
		if (cr_unlikely(memblock == NULL))
			cr_assert(0 && "out of memory");
	}
	vm->gc.allocated += size;
	return memblock;
}


void *cr_mm_growarr(VM *vm, void *ptr, int len, int *sizep,
			int elemsize, int extra, int limit, const char *what) 
{
	int size;

	size = *sizep;
	if (len + extra <= size)
		return ptr;
	size += extra;
	if (size >= limit / 2) {
		if (cr_unlikely(size >= limit))
			cr_dg_runerror(vm, "%s size limit", what);
		size = limit;
		cr_assert(size >= CRI_MINARRSIZE);
	} else {
		size *= 2;
		if (size < CRI_MINARRSIZE)
			size = CRI_MINARRSIZE;
	}
	ptr = cr_mm_saferealloc(vm, ptr, *sizep * elemsize, size * elemsize);
	*sizep = size;
	return ptr;
}


void cr_mm_free(VM *vm, void *ptr, size_t osize)
{
	cr_assert((osize == 0) == (ptr == NULL));
	cr_mm_rawfree(vm, ptr);
	vm->gc.allocated -= osize;
}
