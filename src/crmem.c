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
#define cantryagain(ts)		(tsinitialized(ts) && !(ts)->gc.stopem)



/* Auxiliary to 'cr_mm_realloc' and 'cr_malloc'. */
cr_sinline void *tryagain(TState *ts, void *ptr, size_t osize, size_t nsize)
{
	if (cantryagain(ts)) {
		cr_gc_full(ts);
		return cr_mm_rawrealloc(ts, ptr, nsize);
	}
	return NULL;
}


void *cr_mm_realloc(TState *ts, void *ptr, size_t osize, size_t nsize)
{
	void *memblock;

	cr_assert((osize == 0) == (ptr == NULL));
	memblock = cr_mm_rawrealloc(ts, ptr, nsize);
	if (cr_unlikely(!memblock && nsize != 0)) {
		memblock = tryagain(ts, ptr, osize, nsize);
		if (cr_unlikely(memblock == NULL))
			return NULL;
	}
	cr_assert((nsize == 0) == (memblock == NULL));
	ts->gc.allocated += nsize - osize;
	return memblock;
}


void *cr_mm_saferealloc(TState *ts, void *ptr, size_t osize, size_t nsize)
{
	void *memblock;

	memblock = cr_mm_realloc(ts, ptr, osize, nsize);
	if (cr_unlikely(memblock == NULL && nsize != 0))
		cr_assert(0 && "out of memory");
	return memblock;
}


void *cr_mm_malloc(TState *ts, size_t size)
{
	void *memblock;

	if (size == 0)
		return NULL;
	memblock = cr_mm_rawmalloc(ts, size);
	if (cr_unlikely(memblock == NULL)) {
		memblock = tryagain(ts, NULL, 0, size);
		if (cr_unlikely(memblock == NULL))
			cr_assert(0 && "out of memory");
	}
	ts->gc.allocated += size;
	return memblock;
}


void *cr_mm_growarr(TState *ts, void *ptr, int len, int *sizep,
			int elemsize, int extra, int limit, const char *what) 
{
	int size;

	size = *sizep;
	if (len + extra <= size)
		return ptr;
	size += extra;
	if (size >= limit / 2) {
		if (cr_unlikely(size >= limit))
			cr_dg_runerror(ts, "%s size limit", what);
		size = limit;
		cr_assert(size >= CRI_MINARRSIZE);
	} else {
		size *= 2;
		if (size < CRI_MINARRSIZE)
			size = CRI_MINARRSIZE;
	}
	ptr = cr_mm_saferealloc(ts, ptr, *sizep * elemsize, size * elemsize);
	*sizep = size;
	return ptr;
}


void cr_mm_free(TState *ts, void *ptr, size_t osize)
{
	cr_assert((osize == 0) == (ptr == NULL));
	cr_mm_rawfree(ts, ptr);
	ts->gc.allocated -= osize;
}
