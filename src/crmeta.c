#include "crmeta.h"
#include "crstring.h"
#include "crdebug.h"
#include "crstate.h"
#include "crhashtable.h"
#include "crobject.h"
#include "crgc.h"
#include "crvm.h"
#include "crmem.h"


void crMm_init(cr_State *ts) {
    static const char *vmtnames[CR_NUM_META] = {
        "__init__", "__tostring__", "__getidx__", "__setidx__",
        "__gc__", "__defer__", "__add__", "__sub__", "__mul__",
        "__div__", "__mod__", "__pow__", "__not__", "__umin__",
        "__bnot__", "__shl__", "__shr__", "__band__", "__bor__",
        "__xor__", "__eq__", "__lt__", "__le__",
    };
    for (int i = 0; i < CR_NUM_META; i++) {
        OString *s = crS_new(ts, vmtnames[i]);
        s->bits = bitmask(STRVMTBIT);
        s->extra = i;
        G_(ts)->vmtnames[i] = s;
        crG_fix(ts, obj2gco(G_(ts)->vmtnames[i]));
    }
}


OClass *crMm_newclass(cr_State *ts) {
    OClass *cls = crG_new(ts, sizeof(OClass), CR_VCLASS, OClass);
    cls->methods = NULL;
    cls->vmt = NULL;
    cls->methods = NULL;
    return cls;
}


Instance *crMm_newinstance(cr_State *ts, OClass *cls) {
    Instance *ins = crG_new(ts, sizeof(Instance), CR_VINSTANCE, Instance);
    ins->oclass = cls;
    ins->fields = NULL;
    setins2s(ts, ts->sp.p++, ins);
    ins->fields = crH_new(ts);
    ts->sp.p--;
    return ins;
}


InstanceMethod *crMm_newinstancemethod(cr_State *ts, Instance *receiver,
                                       CrClosure *method)
{
    InstanceMethod *im = crG_new(ts, sizeof(InstanceMethod), CR_VMETHOD,
                                   InstanceMethod);
    im->receiver = receiver;
    im->method = obj2gco(method);
    return im;
}


/* TODO: move to 'crapi' */
// static void parsevmt(cr_State *ts, TValue *actual, cr_VMT *vmt) {
//     for (int i = 0; i < CR_NUM_META; i++) {
//         switch (vmt->methods[i].mtt) {
//         case CR_METAT_CFUNCTION: {
//             // TODO
//         }
//         case CR_METAT_INDEX: {
//             // TODO
//         }
//         case CR_METAT_NONE: default:
//             setnilval(&actual[i]);
//             break;
//         }
//     }
// }


UserData *crMm_newuserdata(cr_State *ts, size_t size, int nuv) {
    UserData *ud = crG_new(ts, sizeofud(nuv, size), CR_VUDATA, UserData);
    ud->vmt = NULL;
    ud->nuv = nuv;
    ud->size = size;
    return ud;
}


/* get VMT method */
const TValue *crMm_get(cr_State *ts, const TValue *v, int mt) {
    UNUSED(ts);
    cr_assert(CR_META_INIT <= mt && mt < CR_NUM_META);
    switch (ttypetag(v)) {
    case CR_VCLASS: return &gco2cls(v)->vmt[mt];
    case CR_VUDATA: return &gco2ud(v)->vmt[mt];
    default: break; /* try basic type */
    } /* FALLTHRU */
    HTable *ht = G_(ts)->vmt[ttype(v)];
    return (ht ? crH_getp(ht, G_(ts)->vmtnames[mt]) : &G_(ts)->nil);
}


/* perform the overloaded method call */
void crMm_callres(cr_State *ts, const TValue *selfarg, const TValue *fn,
                  const TValue *v1, const TValue *v2, SPtr res)
{
    /* assuming EXTRA_STACK */
    // func = sp[0], self, fn, v1, v2, sp_here
    setobj2s(ts, ts->sp.p++, selfarg); /* self */
    setobj2s(ts, ts->sp.p++, fn);
    setobj2s(ts, ts->sp.p++, v1);
    setobj2s(ts, ts->sp.p++, v2);
    crV_call(ts, ts->sp.p - 3, 1);
    setobj2s(ts, res, s2v(--ts->sp.p));
}


/* try to call overloaded binary op and store the result in 'res' */
static int callbinres(cr_State *ts, const TValue *v1, const TValue *v2,
                      SPtr res, int mt)
{
    const TValue *selfarg = v1;
    const TValue *method = crMm_get(ts, v1, mt);
    if (ttisnil(method)) {
        selfarg = v2;
        method = crMm_get(ts, v2, mt);
        if (ttisnil(method))
            return 0;
    }
    crMm_callres(ts, selfarg, method, v1, v2, res);
    return 1;
}


/* try to call overloaded binary arithmetic method */
void crMm_arithm(cr_State *ts, const TValue *v1, const TValue *v2, SPtr res,
                 int mt)
{
    cr_assert(CR_META_ADD <= mt && mt <= CR_META_BXOR);
    if (cr_unlikely(!callbinres(ts, v1, v2, res, mt))) {
        switch (mt) {
        case CR_META_BNOT: case CR_META_BSHL: case CR_META_BSHR:
        case CR_META_BAND: case CR_META_BOR: case CR_META_BXOR:
            crD_operror(ts, v1, v2, "perform bitwise operation on");
            /* UNREACHED */
        default:
            crD_operror(ts, v1, v2, "perform arithmetic operation on");
            /* UNREACHED */
        }
    }
}


/* try to call overloaded ordering vtable method */
int crMm_order(cr_State *ts, const TValue *v1, const TValue *v2, SPtr res,
               int mt)
{
    cr_assert(CR_META_EQ <= mt && mt < CR_NUM_META);
    if (cr_likely(callbinres(ts, v1, v2, res, mt)))
        return cri_isfalse(s2v(ts->sp.p - 1));
    crD_ordererror(ts, v1, v2);
    /* UNREACHED */
    return 0;
}


void crMm_freeclass(cr_State *ts, OClass *cls) {
    if (cls->vmt)
        crM_free(ts, cls->vmt, SIZEVMT);
    crH_free(ts, cls->methods);
    crM_free(ts, cls, sizeof(*cls));
}


void crMm_freeinstance(cr_State *ts, Instance *ins) {
    crH_free(ts, ins->fields);
    crM_free(ts, ins, sizeof(*ins));
}


void crMm_freeuserdata(cr_State *ts, UserData *ud) {
    if (ud->vmt)
        crM_free(ts, ud->vmt, SIZEVMT);
    crM_free(ts, ud, sizeofud(ud->nuv, ud->size));
}
