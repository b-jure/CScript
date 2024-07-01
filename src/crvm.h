/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Cript.
 * Cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/


#ifndef CRVM_H
#define CRVM_H

#include "crgc.h"
#include "crstate.h"
#include "crhashtable.h"
#include "crvalue.h"


/* type for functions with error recovery */
typedef void (*ProtectedFn)(TState *ts, void *userdata);


/*
 * Extra stack space used mostly when calling vtable
 * methods. Useful in avoiding stack checks (branching).
 */
#define EXTRA_STACK	5


/* initial stack size */
#define STACKSIZE_INIT	(CR_MINSTACK*4)


/* stack size */
#define stacksize(ts)		cast_int((ts)->stackend.p - (ts)->stack.p)

/* current stack top offset */
#define topoffset(ts)		cast_int((ts)->stacktop.p - (ts)->stack.p)


/* save/restore stack position */
#define savestack(ts,ptr)	(cast(char*, (ptr)) - cast(char*, (ts)->stack.p))
#define restorestack(ts,o)	cast(SPtr, cast(char*, (ts)->stack.p) + (o))


/* grow stack if needed */
#define checkstack(ts,n) \
	if (cr_unlikely((ts)->stackend.p - (ts)->stacktop.p <= (n))) \
		cr_vm_growstack(ts, (n), 1);


/* get static string */
#define fstatic(ts,i)		(ts)->faststatic[(i)]


#define vminitialized(ts) (ttisnil(&(ts)->nil))


void cr_vm_inctop(TState *ts);
int cr_vm_growstack(TState *ts, int n, int raiseerr);
int cr_vm_reallocstack(TState *ts, int size, int raiseerr);
void cr_vm_ncall(TState *ts, SPtr callee, int nreturns);
cr_number cr_vm_modnum(TState *ts, cr_number x, cr_number y);
void cr_vm_incccalls(TState *ts);

void resetvm(TState *ts, int status);
void cr_vm_concat(TState *ts, int n);

void vminterpret(TState *ts, const char *source, const char *filename);
void vmrun(TState *ts);
void vmcall(TState *ts, SIndex *retstart, SIndex fn, int nreturns);
void vmpcall(TState *ts, ProtectedFn fn, void *userdata, ptrdiff_t oldtop);
cr_ubyte vmcompile(TState *ts, void *userdata, const char *name, int gscope);

void vmcloseupval(TState *ts, SIndex *last);

cr_ubyte vmbindmethod(TState *ts, OClass *oclass, SIndex name, SIndex receiver);

Value vmconcat(TState *ts, SIndex l, SIndex r);

cr_ubyte vmequal(TState *ts, SIndex l, SIndex r);
cr_ubyte vmeqraw(SIndex l, SIndex r);
cr_ubyte vmeq(TState *ts, SIndex l, SIndex r);
cr_ubyte vmne(TState *ts, SIndex l, SIndex r);
cr_ubyte vmlt(TState *ts, SIndex l, SIndex r);
cr_ubyte vmgt(TState *ts, SIndex l, SIndex r);
cr_ubyte vmle(TState *ts, SIndex l, SIndex r);
cr_ubyte vmge(TState *ts, SIndex l, SIndex r);

#endif
