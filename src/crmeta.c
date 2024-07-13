#include "crmeta.h"
#include "crstring.h"
#include "crdebug.h"
#include "crstate.h"
#include "crhashtable.h"
#include "crvalue.h"
#include "crgc.h"
#include "crvm.h"


void cr_meta_init(cr_State *ts)
{
	static const char *vtmnames[CR_NUMM] = {
		"__init__", "__tostring__", "__getidx__", "__setidx__",
		"__gc__", "__defer__", "__add__", "__sub__", "__mul__",
		"__div__", "__mod__", "__pow__", "__not__", "__umin__",
		"__bnot__", "__shl__", "__shr__", "__band__", "__bor__",
		"__xor__", "__eq__", "__lt__", "__le__",
	};
	int i;

	for (i = 0; i < CR_NUMM; i++) {
		GS(ts)->vtmnames[i] = cr_string_new(ts, vtmnames[i]);
		cr_gc_fix(ts, obj2gco(GS(ts)->vtmnames[i]));
	}
}


OClass *cr_meta_newclass(cr_State *ts, OString *id)
{
	OClass *cls;

	cls = cr_gc_new(ts, sizeof(OClass), CR_VCLASS, OClass);
	cls->methods = NULL;
	cls->name = id;
	memset(cls->vtable, 0, sizeof(cls->vtable) / sizeof(cls->vtable[0]));
	setsv2cls(ts, ts->stacktop.p++, cls);
	cls->methods = cr_htable_new(ts);
	ts->stacktop.p--;
	return cls;
}


Instance *cr_meta_newinstance(cr_State *ts, OClass *cls)
{
	Instance *ins;

	ins = cr_gc_new(ts, sizeof(Instance), CR_VINSTANCE, Instance);
	ins->oclass = cls;
	ins->fields = NULL;
	setsv2ins(ts, ts->stacktop.p++, ins);
	ins->fields = cr_htable_new(ts);
	ts->stacktop.p--;
	return ins;
}


InstanceMethod *cr_meta_newinstancemethod(cr_State *ts, Instance *receiver,
						CrClosure *method)
{
	InstanceMethod *im;

	im = cr_gc_new(ts, sizeof(InstanceMethod), CR_VMETHOD, InstanceMethod);
	im->receiver = receiver;
	im->method = obj2gco(method);
	return im;
}


UserData *cr_meta_newuserdata(cr_State *ts, size_t size, int nuv)
{
	UserData *ud;
	int i;

	ud = cr_gc_new(ts, sizeofud(nuv, size), CR_VUDATA, UserData);
	ud->nuv = nuv;
	ud->size = size;
	for (i = 0; i < CR_NUMM; i++)
		setnilval(&ud->vtable[i]);
	return ud;
}


/* get 'vtable' method */
const TValue *cr_meta_getvtable(cr_State *ts, const GCObject *o, int mt)
{
	cr_assert(CR_M_INIT <= mt && mt < CR_NUMM);
	switch (ott_(o)) {
		case CR_VCLASS: return &gco2cls(o)->vtable[mt];
		case CR_VUDATA: return &gco2ud(o)->vtable[mt];
		default: return NULL;
	}
}


/* perform the overloaded method call */
void cr_meta_callres(cr_State *ts, const TValue *selfarg, const TValue *fn,
		     const TValue *v1, const TValue *v2, SPtr res)
{
	/* assuming EXTRA_STACK */
	setsval(ts, ts->stacktop.p++, selfarg); /* self */
	setsval(ts, ts->stacktop.p++, fn);
	setsval(ts, ts->stacktop.p++, v1);
	setsval(ts, ts->stacktop.p++, v2);
	cr_vm_call(ts, ts->stacktop.p - 3, 1);
	setsval(ts, res, s2v(--ts->stacktop.p));
}


/* try to call overloaded binary op and store the result in 'res' */
static int callbinres(cr_State *ts, const TValue *v1, const TValue *v2, SPtr res, int mt)
{
	const TValue *method;
	const TValue *selfarg;

	selfarg = v1;
	method = cr_meta_vgetvtable(ts, v1, mt);
	if (!method) {
		selfarg = v2;
		method = cr_meta_vgetvtable(ts, v2, mt);
		if (!method) return 0;
	}
	cr_meta_callres(ts, selfarg, method, v1, v2, res);
	return 1;
}


/* try to call overloaded binary arithmetic method */
void cr_meta_arithm(cr_State *ts, const TValue *v1, const TValue *v2, SPtr res,
		    int mt)
{
	cr_assert(CR_MADD <= mt && mt <= CR_MBXOR);
	if (cr_unlikely(!callbinres(ts, v1, v2, res, mt))) {
		switch (mt) {
		case CR_MBNOT: case CR_MBSHL: case CR_MBSHR:
		case CR_MBAND: case CR_MBOR: case CR_MBXOR:
			cr_debug_operror(ts, v1, v2, "perform bitwise operation on");
			/* UNREACHED */
		default:
			cr_debug_operror(ts, v1, v2, "perform arithmetic operation on");
			/* UNREACHED */
		}
	}
}


/* try to call overloaded ordering vtable method */
int cr_meta_order(cr_State *ts, const TValue *v1, const TValue *v2, SPtr res,
		      int mt)
{
	cr_assert(CR_MEQ <= mt && mt < CR_NUMM);
	if (cr_likely(callbinres(ts, v1, v2, res, mt)))
		return ttisfalsey(s2v(ts->stacktop.p - 1));
	cr_debug_ordererror(ts, v1, v2);
	/* UNREACHED */
	return 0;
}


/* free 'OClass' */
void cr_meta_freeclass(cr_State *ts, OClass *cls)
{
	cr_htable_free(ts, cls->methods);
	cr_mem_free(ts, cls, sizeof(*cls));
}


/* free 'Instance' */
void cr_meta_freeinstance(cr_State *ts, Instance *ins)
{
	cr_htable_free(ts, ins->fields);
	cr_mem_free(ts, ins, sizeof(*ins));
}
