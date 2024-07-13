#include "crfunction.h"
#include "crgc.h"
#include "crstate.h"



Function *cr_function_newfunction(cr_State *ts)
{
	Function *fn;

	fn = cr_gc_new(ts, sizeof(Function), CR_VFUNCTION, Function);
	cr_mem_createvec(ts, &fn->constants, CRI_MAXCODE, "constants");
	cr_mem_createvec(ts, &fn->code, INT_MAX, "code");
	cr_mem_createvec(ts, &fn->lineinfo, INT_MAX, "lines");
	cr_mem_createvec(ts, &fn->lvars, CRI_MAXCODE, "local variables");
	return fn;
}


CrClosure *cr_function_newcrclosure(cr_State *ts, Function *fn, int nup)
{
	CrClosure *crcl;

	crcl = cr_gc_new(ts, sizeofcrcl(nup), CR_VCRCL, CrClosure);
	crcl->nupvalues = nup;
	crcl->fn = fn;
	memset(crcl->upvalue, 0, nup * sizeof(UpVal*));
	return crcl;
}


CClosure *cr_object_newcclosure(cr_State *ts, cr_cfunc fn, int nupvalues)
{
	CClosure *ccl;
	int i;

	ccl = cr_gc_new(ts, nupvalues * sizeof(TValue), CR_VCCL, CClosure);
	ccl->nupvalues = nupvalues;
	ccl->fn = fn;
	for (i = 0; i < nupvalues; i++)
		setnilval(&ccl->upvalue[i]);
	return ccl;
}


/*
 * Create a new upvalue and link it into 'openupval' list
 * right after 'prev'.
 */
static UpVal *newupval(cr_State *ts, SPtr val, UpVal **prev)
{
	GState *gs;
	UpVal *uv;
	UpVal *previous;

	uv = cr_gc_new(ts, sizeof(*uv), CR_VUVALUE, UpVal);
	previous = *prev;
	uv->v.location = s2v(val);
	uv->u.open.next = previous;
	if (previous) /* have previous upval ? */
		previous->u.open.prev = &uv->u.open.next;
	*prev = uv; /* adjust list head or 'previous.u.open.next' */
	cr_assert(prev == &ts->openupval);
	if (!isinthwouv(ts)) {
		ts->thwouv = gs->thwouv;
		gs->thwouv = ts;
	}
	return uv;
}


/*
 * Find and return already existing upvalue or create
 * and return a new one.
 */
UpVal *cr_function_findupval(cr_State *ts, SPtr sval)
{
	UpVal **pp;
	UpVal *upval;
	SPtr sp;

	cr_assert(isinthwouv(ts) || ts->openupval == NULL);
	pp = &ts->openupval;
	while ((upval = *pp) != NULL && (sp = upvaltostk(upval)) > sval) {
		cr_assert(!isdead(&GS(ts)->gc, upval));
		if (sp == sval)
			return upval;
		pp = &upval->u.open.next;
	}
	return newupval(ts, sval, pp);
}


/* unlinks upvalue from the list */
static void unlinkupval(UpVal *upval)
{
	*upval->u.open.prev = upval->u.open.next;
	if (upval->u.open.next)
		upval->u.open.next->u.open.prev = upval->u.open.prev;
}


/* free 'UpVal' */
void cr_function_freeupval(cr_State *ts, UpVal *upval)
{
	if (uvisopen(upval))
		unlinkupval(upval);
	cr_mem_free(ts, upval, sizeof(UpVal));
}


/* free 'Function' */
void cr_function_free(cr_State *ts, Function *fn)
{
	cr_mem_freevec(ts, &fn->fns);
	cr_mem_freevec(ts, &fn->constants);
	cr_mem_freevec(ts, &fn->code);
	cr_mem_freevec(ts, &fn->lineinfo);
	cr_mem_freevec(ts, &fn->lvars);
	cr_mem_freevec(ts, &fn->upvalues);
	cr_mem_free(ts, fn, sizeof(Function));
}
