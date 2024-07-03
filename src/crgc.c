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

#include "crgc.h"
#include "crobject.h"
#include "crstate.h"



void cr_gc_init(GC *gc)
{
	gc->sizegs = 0;
	gc->ngs = 0;
	gc->next = 0;
	gc->allocated = 0;
	gc->objects = NULL;
	gc->sweeppos = NULL;
	gc->graystack = NULL;
	gc->fixed = NULL;
	gc->stepmul = GCSTEPMUL;
	gc->stepsize = GCSTEPSIZE;
	gc->stopem = 0;
	gc->stopped = 0;
	gc->state = 0;
}


void cr_gc_setdebt(GState *gs, cr_mem debt)
{
	GC *gc;

	gc = &gs->gc;
}


/*
 * Marks object 'o'.
 * Some objects are directly marked as black, these objects
 * are objects that do not point to other objects.
 * Other objects are marked gray instead.
 * Some objects can call this function recursively.
 * However this recursion can occur at most for two levels.
 */
void markobject(cr_State *ts, GCObject *o)
{
	UValue *uv;

	switch(rawott(o)) {
	case CR_VSTRING:
		markblack(o);
		break;
	case CR_VUVALUE:
		uv = gco2uv(o);
		break;
	}
}


void cr_gc_barrier_(cr_State *ts, GCObject *r, GCObject *o)
{
	GState *gs;

	gs = GS(ts);
	if (invariantstate(gs->gc)) { /* invariant holds ? */
		cr_assert(isblack(r) && iswhite(o));
		markgray(o);
	} else { /* sweep phase */
		cr_assert(sweepstate(gs->gc));
	}
}


void omark(cr_State *ts, O *obj)
{
	if (obj == NULL || oismarked(obj))
		return;
	osetmark(obj, 1);
	if (otype(obj) == OBJ_STRING) {
		return;
	}
	GSARRAY_PUSH(ts, obj);
}


static cr_inline void marktable(cr_State *ts, HTable *table)
{
	for (uint32_t i = 0; i < table->size; i++) {
		Node *entry = &table->mem[i];
		if (!IS_EMPTY(entry->key)) {
			tsark(ts, entry->key);
			tsark(ts, entry->value);
		}
	}
}




// Generic function signature for Mark and Sweep functions
#define MS_FN(name) static cr_inline void name(cr_State *ts)

MS_FN(markglobals)
{
	for (uint32_t i = 0; i < ts->globids.cap; i++) {
		Node *entry = &ts->globids.entries[i];
		if (!IS_EMPTY(entry->key)) {
			// Mark identifier (ObjString)
			omark(ts, asobj(entry->key));
			// Mark value
			uint32_t idx = cast_uint(AS_NUMBER(entry->value));
			tsark(ts, ts->globvars.data[idx].value);
		}
	}
}

MS_FN(markstack)
{
	for (Value *local = ts->stack; local < ts->sp; local++)
		tsark(ts, *local);
}

MS_FN(markframes)
{
	for (int i = 0; i < ts->fc; i++)
		omark(ts, cast(GCObject *, ts->frames[i].closure));
}

MS_FN(markupvalues)
{
	for (OUpvalue *upval = ts->open_upvals; upval != NULL; upval = upval->next)
		omark(ts, cast(GCObject *, upval));
}

MS_FN(markstatics)
{
	for (uint32_t i = 0; i < SS_N; i++)
		omark(ts, cast(GCObject *, ts->faststatic[i]));
}

MS_FN(markinterned)
{
	for (uint32_t i = 0; i < ts->interned.len; i++)
		omark(ts, cast(GCObject *, ts->interned.data[i]));
}

MS_FN(markloaded)
{
	marktable(ts, &ts->loaded);
}

MS_FN(marktemp)
{
	for (uint32_t i = 0; i < ts->temp.len; i++)
		tsark(ts, ts->temp.data[i]);
}

MS_FN(markroots)
{
	markstack(ts); // cr_State stack
	markframes(ts); // cr_State call stack
	markupvalues(ts); // cr_State open upvalues (to be closed)
	markglobals(ts); // global values and identifiers
	markstatics(ts); // fast statics (interned strings)
	markinterned(ts); // normal statics (interned strings)
	markloaded(ts); // mark loaded script names table
}

MS_FN(rmweakrefs)
{
	for (uint32_t i = 0; i < ts->weakrefs.cap; i++) {
		Node *entry = &ts->weakrefs.entries[i];
		if (IS_OBJ(entry->key) && !oismarked(asobj(entry->key)))
			HTable_remove(ts, &ts->weakrefs, entry->key, 0);
	}
}

MS_FN(sweep)
{
	GCObject *previous = NULL;
	GCObject *current = ts->objects;
	while (current != NULL) {
		if (oismarked(current)) {
			osetmark(current, 0);
			previous = current;
			current = onext(current);
		} else {
			GCObject *unreached = current;
			current = onext(current);
			if (previous != NULL)
				osetnext(previous, current);
			else
				ts->objects = current;
			ofree(ts, unreached);
		}
	}
}




void mark_black(cr_State *ts, GCObject *obj)
{
#ifdef DEBUG_LOG_GC
	printf("%p blacken ", (void *)obj);
	vprint(OBJ_VAL(obj));
	printf("\n");
#endif
#ifdef CR_PRECOMPUTED_GOTO
#define OBJ_TABLE
#include "skjmptable.h"
#undef OBJ_TABLE
#else
#define DISPATCH(x) switch (x)
#define CASE(label) case label:
#define BREAK	    break
#endif
	ASSERT(oismarked(obj), "Object is not marked.");
	DISPATCH(otype(obj))
	{
		CASE(OBJ_UVAL)
		{
			tsark(ts, cast(OUpvalue *, obj)->closed);
			BREAK;
		}
		CASE(OBJ_FUNCTION)
		{
			Function *fn = cast(Function *, obj);
			omark(ts, cast(GCObject *, fn->p.name));
			omark(ts, cast(GCObject *, fn->p.source));
			for (uint32_t i = 0; i < fn->chunk.constants.len; i++)
				tsark(ts, fn->chunk.constants.data[i]);
			BREAK;
		}
		CASE(OBJ_CLOSURE)
		{
			CriptClosure *closure = (CriptClosure *)obj;
			omark(ts, (GCObject *)closure->fn);
			for (uint32_t i = 0; i < closure->fn->p.upvalc; i++)
				omark(ts, cast(GCObject *, closure->upvalue[i]));
			BREAK;
		}
		CASE(OBJ_CLASS)
		{
			OClass *oclass = cast(OClass *, obj);
			omark(ts, cast(GCObject *, oclass->name));
			marktable(ts, &oclass->methods);
			for (cr_ubyte i = 0; i < OM_CNT; i++)
				omark(ts, cast(GCObject *, oclass->omethods[i]));
			BREAK;
		}
		CASE(OBJ_INSTANCE)
		{
			Instance *instance = cast(Instance *, obj);
			omark(ts, cast(GCObject *, instance->oclass));
			marktable(ts, &instance->fields);
			BREAK;
		}
		CASE(OBJ_BOUND_METHOD)
		{
			InstanceMethod *bound_method = cast(InstanceMethod *, obj);
			omark(ts, cast(GCObject *, bound_method));
			tsark(ts, bound_method->receiver);
			omark(ts, cast(GCObject *, bound_method->method));
			BREAK;
		}
		CASE(OBJ_CFUNCTION)
		{
			CClosure *native = cast(CClosure *, obj);
			omark(ts, cast(GCObject *, native->p.name));
			for (uint32_t i = 0; i < native->p.upvalc; i++)
				tsark(ts, native->upvalue[i]);
			BREAK;
		}
		CASE(OBJ_STRING)
		cr_unreachable;
	}
}



cr_umem incgc(cr_State *ts)
{
#ifdef DEBUG_LOG_GC
	printf("--> GC start\n");
#endif
	cr_umem old_allocation = ts->gc.allocated;
	markroots(ts);
#ifdef CR_PRECOMPUTED_GOTO
	static const void *jmptable[] = { &&mark, &&skip };
	goto *jmptable[runtime];
mark:
	mark_function_roots(ts);
skip:
#else
	mark_function_roots(ts);
#endif
	while (ts->gslen > 0)
		mark_black(ts, GSARRAY_POP(ts));
	rmweakrefs(ts);
	sweep(ts);
	cr_umem nextgc = cast(cr_umem, cast(double, ts->gc.allocated) * ts->gc.growfactor);
	ts->gc.nextgc = MAX(nextgc, ts->gc.heapmin);
#ifdef DEBUG_LOG_GC
	printf("--> GC end\n");
	printf("    collected %lu bytes (from %lu to %lu) next collection at %lu\n", old_allocation - ts->gc.allocated,
	       old_allocation, ts->gc.allocated, (cr_umem)ts->gc.nextgc);
#endif
	return old_allocation - ts->gc.allocated;
}


