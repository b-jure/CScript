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

#ifndef CRMEM_H
#define CRMEM_H

#include "crcommon.h"

#define rawmalloc(vm, s)     (vm)->hooks.reallocate(NULL, s, (vm)->hooks.userdata)
#define rawrealloc(vm, p, s) (vm)->hooks.reallocate(p, s, (vm)->hooks.userdata)
#define rawfree(vm, p)	     (vm)->hooks.reallocate(p, 0, (vm)->hooks.userdata)

void *cr_realloc(VM *vm, void *ptr, cr_umem osize, cr_umem nsize);

void *cr_malloc(VM *vm, cr_umem size);
void *cr_saferealloc(VM *vm, void *ptr, cr_umem osize, cr_umem nsize);
void *cr_free(VM *vm, void *ptr, cr_umem osize);


#endif
