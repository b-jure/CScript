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

#include "crobject.h"


/* GCObject 'marked' bits */
#define BLACK		0 /* marks object black (temporary barrier) */
#define BARRIER		1 /* collector barrier (permanent) */


/* bit mask of mark bits */
#define MARKMASK	bit2mask(BLACK, BARRIER)


/* blackens object */
#define gcblack(o)	setbit(rawomark(o), BLACK)

/* sets gc barrier */
#define gcbarrier(o)	setbit(rawomark(o), BARRIER)

/* clears BLACK bit */
#define gcwhite(o)	clearbit(rawomark(o), BLACK)

/* clears BARRIER bit */
#define gcnobarrier(o)		clearbit(rawomark(o), BARRIER)



/* 'stopped' bits */
#define GCSTOPPED	0
#define GCUSRSTOPPED	1

/* garbage collector is not 'stopped' */
#define gcrunning(gc)	((gc)->stopped == 0)



/*
 * Performs a single step of collection when collector
 * dept becomes positive.
 */
#define gccheck(vm) \
	{ if ((vm)->gc.dept > 0) cr_gc_step(vm); }



/* default GC parameters */
#define CRI_GCSTEPMUL		100	/* 'stepmul' */
#define CRI_GCSTEPSIZE		14	/* 'stepsize' */



/* Configurable GC parameters. */
typedef struct {
	cr_mem sizegs; /* size of 'graystack' */
	cr_mem lengs; /* number of elements in 'graystack' */
	cr_mem next; /* next byte threshold when GC triggers */
	cr_mem allocated; /* number of allocated bytes */
	cr_mem debt; /* memory unaccounted for by the collector */
	GCObject *list; /* GC list of allocated objects */
	GCObject **sweeppos; /* current position of sweep in 'olist' */
	GCObject **graystack; /* tricolor GC (stores marked objects) */
	cr_ubyte stepmul; /* collector grow speed */
	cr_ubyte stepsize; /* step size in bytes (log2) */
	cr_ubyte stopem; /* stop emergency collection */
	cr_ubyte stopped; /* collector is stopped */
	cr_ubyte state; /* GC state bits */
} GC;


size_t cr_gc_full(VM *vm);
size_t cf_gc_step(VM *vm);

void cr_gc_mark(VM *vm, Value *v);


#endif
