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
    static const char *vmtnames[CR_NUM_META] = {
        "__init__", "__tostring__", "__getidx__", "__setidx__",
        "__gc__", "__defer__", "__add__", "__sub__", "__mul__",
        "__div__", "__mod__", "__pow__", "__not__", "__umin__",
        "__bnot__", "__shl__", "__shr__", "__band__", "__bor__",
        "__xor__", "__eq__", "__lt__", "__le__",
    };
    for (int i = 0; i < CR_NUM_META; i++) {
        OString *s = cr_string_new(ts, vmtnames[i]);
        s->bits = bitmask(STRVMTBIT);
        s->extra = i;
        GS(ts)->vmtnames[i] = s;
        cr_gc_fix(ts, obj2gco(GS(ts)->vmtnames[i]));
    }
}


OClass *cr_meta_newclass(cr_State *ts, OString *id)
{
    OClass *cls = cr_gc_new(ts, sizeof(OClass), CR_VCLASS, OClass);
    cls->methods = NULL;
    cls->name = id;
    for (uint i = 0; i < VMTELEMS; i++)
        setnilval(&cls->vtable[i]);
    setsv2cls(ts, ts->stacktop.p++, cls);
    cls->methods = cr_htable_new(ts);
    ts->stacktop.p--;
    return cls;
}


Instance *cr_meta_newinstance(cr_State *ts, OClass *cls)
{
    Instance *ins = cr_gc_new(ts, sizeof(Instance), CR_VINSTANCE, Instance);
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
    InstanceMethod *im = cr_gc_new(ts, sizeof(InstanceMethod), CR_VMETHOD,
                                   InstanceMethod);
    im->receiver = receiver;
    im->method = obj2gco(method);
    return im;
}


UserData *cr_meta_newuserdata(cr_State *ts, size_t size, int nuv)
{
    UserData *ud = cr_gc_new(ts, sizeofud(nuv, size), CR_VUDATA, UserData);
    ud->nuv = nuv;
    ud->size = size;
    for (int i = 0; i < CR_NUM_META; i++)
        setnilval(&ud->vtable[i]);
    return ud;
}


/* get 'vtable' method */
const TValue *cr_meta_get(cr_State *ts, const GCObject *o, int mt)
{
    UNUSED(ts);
    cr_assert(CR_META_INIT <= mt && mt < CR_NUM_META);
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
static int callbinres(cr_State *ts, const TValue *v1, const TValue *v2,
                      SPtr res, int mt)
{
    const TValue *selfarg = v1;
    const TValue *method = cr_meta_vget(ts, v1, mt);
    if (!method) {
        selfarg = v2;
        method = cr_meta_vget(ts, v2, mt);
        if (!method) return 0;
    }
    cr_meta_callres(ts, selfarg, method, v1, v2, res);
    return 1;
}


/* try to call overloaded binary arithmetic method */
void cr_meta_arithm(cr_State *ts, const TValue *v1, const TValue *v2, SPtr res,
                    int mt)
{
    cr_assert(CR_META_ADD <= mt && mt <= CR_META_BXOR);
    if (cr_unlikely(!callbinres(ts, v1, v2, res, mt))) {
        switch (mt) {
        case CR_META_BNOT: case CR_META_BSHL: case CR_META_BSHR:
        case CR_META_BAND: case CR_META_BOR: case CR_META_BXOR:
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
    cr_assert(CR_META_EQ <= mt && mt < CR_NUM_META);
    if (cr_likely(callbinres(ts, v1, v2, res, mt)))
        return cr_isfalse(s2v(ts->stacktop.p - 1));
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
