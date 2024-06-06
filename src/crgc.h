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



/* 'stopped' bits */
#define GCstopped	0
#define GCuserstopped	1


/* garbage collector is not 'stopped' */
#define gcrunning(gc)	((gc)->stopped == 0)


/* default GC parameters */
#define CRI_GCSTEPMUL		100	/* 'stepmul' */
#define CRI_GCSTEPSIZE		14	/* 'stepsize' */


/* Configurable GC parameters. */
typedef struct {
	GCObject *olist; /* GC list of allocated objects */
	GCObject **sweeppos; /* current position of sweep in 'olist' */
	GCObject **graystack; /* tricolor GC (stores marked objects) */
	cr_mem next; /* next byte threshold when GC triggers */
	cr_mem allocated; /* number of allocated bytes */
	cr_mem debt; /* memory unaccounted for by the collector */
	cr_ubyte stepmul; /* collector grow speed */
	cr_ubyte stepsize; /* step size in bytes (log2) */
	cr_ubyte stopem; /* stops emergency collection */
	cr_ubyte stopped; /* collector is stopped */
} GC;


size_t cr_gc_full(VM *vm);
size_t cf_gc_step(VM *vm);

void cr_gc_mark(VM *vm, Value *v);


#endif
