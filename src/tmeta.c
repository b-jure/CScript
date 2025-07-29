/*
** tmeta.c
** Functions for metamethods and meta types
** See Copyright Notice in Tokudae.h
*/

#define tmeta_c
#define TOKU_CORE

#include "tokudaeprefix.h"

#include "tmeta.h"
#include "tlist.h"
#include "tlexer.h"
#include "tokudaeconf.h"
#include "tstring.h"
#include "tdebug.h"
#include "tstate.h"
#include "ttable.h"
#include "tobject.h"
#include "tgc.h"
#include "tvm.h"
#include "tmem.h"
#include "tprotected.h"


TOKUI_DEF const char *const tokuO_typenames[TOKUI_TOTALTYPES] = {
    "no value", "nil", "boolean", "number", "userdata", "light userdata",
    "string", "list", "table", "function", "bound method", "class",
    "instance", "thread",
    "upvalue", "proto" /* these last cases are used for tests only */
};


void tokuTM_init(toku_State *T) {
    const char *tmnames[TM_NUM] = { /* ORDER TM */
        "__getidx", "__setidx", "__gc", "__call", "__eq", "__name",
        "__init", "__add", "__sub", "__mul", "__div", "__idiv", "__mod",
        "__pow", "__shl", "__shr", "__band", "__bor", "__bxor", "__concat",
        "__unm", "__bnot", "__lt", "__le", "__close"
    };
    toku_assert(FIRST_TM + TM_NUM <= TOKU_MAXUBYTE);
    for (int i = 0; i < TM_NUM; i++) {
        OString *s = tokuS_new(T, tmnames[i]);
        s->extra = cast_ubyte(i + FIRST_TM);
        G(T)->tmnames[i] = s;
        tokuG_fix(T, obj2gco(G(T)->tmnames[i]));
    }
}


OClass *tokuTM_newclass(toku_State *T) {
    GCObject *o = tokuG_new(T, sizeof(OClass), TOKU_VCLASS);
    OClass *cls = gco2cls(o);
    cls->sclass = NULL;
    cls->metatable = NULL;
    cls->methods = NULL;
    return cls;
}


Instance *tokuTM_newinstance(toku_State *T, OClass *cls) {
    GCObject *o = tokuG_new(T, sizeof(Instance), TOKU_VINSTANCE);
    Instance *ins = gco2ins(o);
    ins->oclass = cls;
    ins->fields = NULL; /* to not confuse GC */
    setinsval2s(T, T->sp.p++, ins); /* anchor instance */
    ins->fields = tokuH_new(T);
    T->sp.p--; /* remove instance */
    return ins;
}


UserData *tokuTM_newuserdata(toku_State *T, size_t size, int nuv) {
    GCObject *o;
    UserData *ud;
    if (t_unlikely(size > TOKU_MAXSIZE - udmemoffset(nuv)))
        tokuM_toobig(T);
    o = tokuG_new(T, sizeofuserdata(nuv, size), TOKU_VUSERDATA);
    ud = gco2u(o);
    ud->metatable = NULL;
    ud->nuv = nuv;
    ud->size = size;
    for (int i = 0; i < nuv; i++)
        setnilval(&ud->uv[i].val);
    return ud;
}


IMethod *tokuTM_newinsmethod(toku_State *T, Instance *ins, const TValue *method) {
    GCObject *o = tokuG_new(T, sizeof(IMethod), TOKU_VIMETHOD);
    IMethod *im = gco2im(o);
    im->ins = ins;
    setobj(T, &im->method, method);
    return im;
}


int tokuTM_eqim(const IMethod *v1, const IMethod *v2) {
    return (v1 == v2) || /* same instance... */
        (v1->ins == v2->ins && /* ...or equal instances */
         tokuV_raweq(&v1->method, &v2->method)); /* ...and equal methods */
}


UMethod *tokuTM_newudmethod(toku_State *T, UserData *ud, const TValue *method) {
    GCObject *o = tokuG_new(T, sizeof(UMethod), TOKU_VUMETHOD);
    UMethod *um = gco2um(o);
    um->ud = ud;
    setobj(T, &um->method, method);
    return um;
}


int tokuTM_equm(const UMethod *v1, const UMethod *v2) {
    return (v1 == v2) || /* same instance... */
        (v1->ud == v2->ud && /* ...or equal userdata */
         tokuV_raweq(&v1->method, &v2->method)); /* ...and equal methods */
}


const TValue *tokuTM_objget(toku_State *T, const TValue *v, TM event) {
    Table *mt;
    toku_assert(0 <= event && event < TM_NUM);
    switch (ttypetag(v)) {
        case TOKU_VINSTANCE: mt = insval(v)->oclass->metatable; break;
        case TOKU_VUSERDATA: mt = udval(v)->metatable; break;
        default: mt = NULL; break;
    }
    return (mt ? tokuH_Hgetshortstr(mt, G(T)->tmnames[event]) : &G(T)->nil);
}


const TValue *tokuTM_get(Table *events, TM event, OString *ename) {
    const TValue *tm = tokuH_Hgetshortstr(events, ename);
    toku_assert(event <= TM_NUM);
    if (notm(tm)) { /* no tag method? */
        events->flags |= cast_byte(1u<<event); /* cache this fact */
        return NULL;
    } else
        return tm;
}


/*
** Return the name of the type of an object. For instances, classes and
** userdata with metatable, use their '__name', if it is a string value.
*/
const char *tokuTM_objtypename(toku_State *T, const TValue *o) {
    Table *t;
    if ((ttisinstance(o) && (t = insval(o)->oclass->metatable)) ||
        (ttisfulluserdata(o) && (t = udval(o)->metatable))) {
        const TValue *v = tokuTM_get(t, TM_NAME, tokuS_new(T, "__name"));
        if (ttisstring(v)) /* is '__name' a string? */
            return getstr(strval(v)); /* use it as type name */
    }
    return typename(ttype(o)); /* otherwise use standard type name */
}


/* call __setidx metamethod */
void tokuTM_callset(toku_State *T, const TValue *f, const TValue *o,
                                   const TValue *k, const TValue *v) {
    SPtr func = T->sp.p;
    setobj2s(T, func, f);
    setobj2s(T, func + 1, o);
    setobj2s(T, func + 2, k);
    setobj2s(T, func + 3, v);
    T->sp.p = func + 4;
    tokuV_call(T, func, 0);
}


/* call __getidx metamethod */
void tokuTM_callgetres(toku_State *T, const TValue *f, const TValue *o,
                                      const TValue *k, SPtr res) {
    ptrdiff_t result = savestack(T, res);
    SPtr func = T->sp.p;
    setobj2s(T, func, f);
    setobj2s(T, func + 1, o);
    setobj2s(T, func + 2, k);
    T->sp.p = func + 3;
    tokuV_call(T, func, 1);
    res = restorestack(T, result);
    setobjs2s(T, res, --T->sp.p);
}


/* call binary method and store the result in 'res' */
void tokuTM_callbinres(toku_State *T, const TValue *f, const TValue *o1,
                                      const TValue *o2, SPtr res) {
    ptrdiff_t result = savestack(T, res);
    SPtr func = T->sp.p;
    setobj2s(T, func, f);
    setobj2s(T, func + 1, o1);
    setobj2s(T, func + 2, o2);
    T->sp.p += 3; /* assuming EXTRA_STACK */
    tokuV_call(T, func, 1);
    res = restorestack(T, result);
    setobj2s(T, res, s2v(--T->sp.p));
}


static int callbinTM(toku_State *T, const TValue *v1, const TValue *v2,
                                                      SPtr res, TM event) {
    int t1 = ttypetag(v1);
    if (t1 != ttypetag(v2) ||
            (t1 == TOKU_VINSTANCE && insval(v1)->oclass != insval(v2)->oclass))
        return 0; /* different types or different classes */
    else {
        const TValue *fn = tokuTM_objget(T, v1, event);
        if (notm(fn)) { /* metamethod not found? */
            fn = tokuTM_objget(T, v2, event); /* try other instance */
            if (notm(fn))
                return 0; /* metamethod not found */
        }
        /* ok; found metamethod, now call it */
        tokuTM_callbinres(T, fn, v1, v2, res);
        return 1; /* ok */
    }
}


void tokuTM_trybin(toku_State *T, const TValue *v1, const TValue *v2,
                                  SPtr res, TM event) {
    if (t_unlikely(!callbinTM(T, v1, v2, res, event)))
        tokuD_binoperror(T, v1, v2, event);
}


void tokuTM_callunaryres(toku_State *T, const TValue *fn,
                                        const TValue *o, SPtr res) {
    ptrdiff_t result = savestack(T, res);
    SPtr func = T->sp.p;
    setobj2s(T, func, fn);
    setobj2s(T, func + 1, o);
    T->sp.p += 2; /* assuming EXTRA_STACK */
    tokuV_call(T, func, 1);
    res = restorestack(T, result);
    setobj2s(T, res, s2v(--T->sp.p));
}


static int callunMT(toku_State *T, const TValue *o, SPtr res, TM event) {
    const TValue *fn = tokuTM_objget(T, o, event);
    if (t_likely(!notm(fn))) {
        tokuTM_callunaryres(T, fn, o, res);
        return 1;
    }
    return 0;
}


void tokuTM_tryunary(toku_State *T, const TValue *o, SPtr res, TM event) {
    if (t_unlikely(!callunMT(T, o, res, event))) {
        TValue dummy;
        setival(&dummy, 0);
        tokuD_binoperror(T, o, &dummy, event);
    }
}


void tokuTM_tryconcat(toku_State *T) {
    SPtr p1 = T->sp.p - 2; /* first argument */
    if (t_unlikely(!callbinTM(T, s2v(p1), s2v(p1 + 1), p1, TM_CONCAT)))
        tokuD_concaterror(T, s2v(p1), s2v(p1 + 1));
}


/* call order method */
int tokuTM_order(toku_State *T, const TValue *v1, const TValue *v2, TM event) {
    if (t_likely(callbinTM(T, v1, v2, T->sp.p, event)))
        return !t_isfalse(s2v(T->sp.p));
    tokuD_ordererror(T, v1, v2);
    return 0; /* to avoid warnings */
}
