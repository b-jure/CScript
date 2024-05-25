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

#include "skdebug.h"
#include "skerr.h"
#include "skhashtable.h"
#include "skmem.h"
#include "skobject.h"
#include "skparser.h"
#include "skconf.h"
#include "skvalue.h"
#include "skvm.h"

#include <stdio.h>
#include <stdlib.h>

/* Auxiliary to 'cr_realloc' and 'cr_malloc'. */
void *secondtry(VM *vm, void *ptr, cr_memsize osize, cr_memsize nsize)
{
	if (vminitialized(vm) && !vm->gc.stopped) {
		incgc(vm);
		return rawrealloc(vm, ptr, nsize);
	}
	return NULL;
}

void *cr_realloc(VM *vm, void *ptr, cr_memsize osize, cr_memsize nsize)
{
	void *memblock;

#ifdef DEBUG_STRESS_GC
	if (nsize > osize)
		incgc(vm);
#else
	if (!vm->gc.stopped && (vm->gc.nextgc <= vm->gc.allocated + (nsize - osize)))
		incgc(vm);
#endif
try_again:
	memblock = rawrealloc(vm, ptr, nsize);
	if (unlikely(!memblock && nsize != 0)) {
		memblock = secondtry(vm, ptr, osize, nsize);
		if (unlikely(memblock == NULL))
			return NULL;
	}
	cr_assert(vm, (nsize == 0) == (allocation == NULL), "invalid args or realloc invariant broken");
	vm->gc.allocated += nsize - osize;
	return memblock;
}

void *cr_saferealloc(VM *vm, void *ptr, cr_memsize osize, cr_memsize nsize)
{
	void *memblock;

	memblock = cr_realloc(vm, ptr, osize, nsize);
	if (unlikely(memblock == NULL && nsize != 0))
		memerror(vm);
	return memblock;
}

void *cr_malloc(VM *vm, cr_memsize size)
{
	void *memblock;

	if (size == 0)
		return NULL;
	memblock = rawmalloc(vm, size);
	if (unlikely(memblock == NULL)) {
		memblock = secondtry(vm, NULL, 0, size);
		if (unlikely(memblock == NULL))
			memerror(vm);
	}
	vm->gc.allocated += size;
	return memblock;
}

void cr_free(VM *vm, void *ptr, cr_memsize osize, cr_memsize nsize)
{
	cr_assert(vm, (osize == 0) == (ptr == NULL), "invalid args");
	rawfree(vm, ptr);
	vm->gc.allocated -= osize;
}
