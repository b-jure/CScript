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

#ifndef CRGC_H
#define CRGC_H

#include "crcommon.h"
#include "crobject.h"



/* GC state bits */
#define GCS_STOPPED 1

/* Configurable GC parameters. */
typedef struct {
	O *olist; /* GC list of allocated objects */
	O **sweeppos; /* current position of sweep in 'olist' */
	O **graystack; /* tricolor GC (stores marked objects) */
	cr_umem heapmin; /* minimum GC threshold */
	cr_umem nextgc; /* next byte threshold when GC triggers */
	cr_umem allocated; /* number of allocated bytes */
	double growfactor; /* GC grow factor */
	cr_ubyte stepsize; /* step size in bytes (log2) */
	cr_ubyte state; /* GC state */
} GC;



/* run full incremental gc (sweep all) */
size_t incfullgc(VM *vm);


#define vmark(vm, v)   \
	if (IS_OBJ(v)) \
		omark(vm, AS_OBJ(v));

void omark(VM *vm, O *obj);




#endif
