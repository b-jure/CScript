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

#ifndef CRVEC_H
#define CRVEC_H


#include "crcommon.h"


/* usual 'Vec' fields */
#define VecFields(t)	t *ptr; cr_uint len; cr_uint cap; VM *vm

/* 'Vec' type constructor */
#define Vec(n, t)	typedef struct { VecFields(t); } n

/* helpers for defining 'Vec' functions */
#define VecFunction(name, namefn)		name##_##namefn(name *v)
#define VecFunctionArgs(name, namefn, ...)	name##_##namefn(name *v, __VA_ARGS__)

/* generic 'Vec' constructor */
#define VecGeneric(name, type) \
	Vec(name, type); \
	void VecFunctionArgs(name, init, VM *vm); \
	void VecFunctionArgs(name, init_cap, VM *vm, cr_uint cap); \
	void VecFunctionArgs(name, ensure, cr_uint len); \
	type *VecFunctionArgs(name, at, cr_uint i); \
	type *VecFunction(name, last); \
	type *VecFunction(name, first); \
	cr_uint VecFunctionArgs(name, push, type e); \
	type VecFunction(name, pop); \
	void VecFunctionArgs(name, insert, type e, cr_uint i); \
	type VecFunctionArgs(name, remove, cr_uint i); \
	void VecFunctionArgs(name, free, void (*ffree)(void *))



/* alloc/dealloc */
#define mallocvec(vm, v, nsize, t)	((v)->ptr = cast(t *, cr_malloc(vm, (nsize) * sizeof(t))))
#define reallocvec(vm, v, nsize, t)	((v)->ptr = cast(t *, cr_saferealloc(vm, v, (v)->cap * sizeof(t), (nsize) * sizeof(t))))
#define freevec(vm, v, t)		cr_free(vm, (v)->ptr, (v)->cap * sizeof(t))


/* initial 'Vec' size */
#define VEC_INIT_SIZE	cast(cr_uint, 8)

/* grow 'Vec' size */
#define VEC_GROW_SIZE(s)	((s) < VEC_INIT_SIZE ? VEC_INIT_SIZE : ((s) * 2))


/* commonly used 'Vec's */
VecGeneric(ubyteVec, cr_int);
VecGeneric(intVec, cr_int);
VecGeneric(uintVec, cr_int);


#endif
