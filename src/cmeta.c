/*
** cmeta.c
** Functions for metamethods and meta types
** See Copyright Notice in CScript.h
*/

#define cmeta_c
#define CS_CORE

#include "cprefix.h"

#include "cmeta.h"
#include "clist.h"
#include "clexer.h"
#include "cscriptconf.h"
#include "cstring.h"
#include "cdebug.h"
#include "cstate.h"
#include "ctable.h"
#include "cobject.h"
#include "cgc.h"
#include "cvm.h"
#include "cmem.h"
#include "cprotected.h"


static const char udataname[] = "userdata";

CSI_DEF const char *const csO_typenames[CSI_TOTALTYPES] = {
    "no value", "nil", "boolean", "number", udataname, udataname, "string",
    "list", "table", "function", "class", "instance", "thread",
    "upvalue", "proto" /* these last cases are used for tests only */
};


void csMM_init(cs_State *C) {
    const char *mmnames[CS_MM_NUM] = { /* ORDER MM */
        "__getidx", "__setidx", "__gc", "__close", "__call", "__init",
        "__concat", "__add", "__sub", "__mul", "__div", "__idiv", "__mod",
        "__pow", "__shl", "__shr", "__band", "__bor", "__bxor", "__unm",
        "__bnot", "__eq", "__lt", "__le"
    };
    cs_assert(FIRSTMM + CS_MM_NUM <= MAXBYTE);
    for (int i = 0; i < CS_MM_NUM; i++) {
        OString *s = csS_new(C, mmnames[i]);
        s->extra = cast_byte(i + FIRSTMM);
        G(C)->mmnames[i] = s;
        csG_fix(C, obj2gco(G(C)->mmnames[i]));
    }
}


OClass *csMM_newclass(cs_State *C) {
    GCObject *o = csG_new(C, sizeof(OClass), CS_VCLASS);
    OClass *cls = gco2cls(o);
    cls->sclass = NULL;
    cls->metalist = NULL;
    cls->methods = NULL;
    return cls;
}


Instance *csMM_newinstance(cs_State *C, OClass *cls) {
    GCObject *o = csG_new(C, sizeof(Instance), CS_VINSTANCE);
    Instance *ins = gco2ins(o);
    ins->oclass = cls;
    ins->fields = NULL; /* to not confuse GC */
    setinsval2s(C, C->sp.p++, ins); /* anchor instance */
    ins->fields = csH_new(C);
    C->sp.p--; /* remove instance */
    return ins;
}


UserData *csMM_newuserdata(cs_State *C, size_t size, int nuv) {
    GCObject *o;
    UserData *ud;
    if (c_unlikely(size > MAXSIZE - udmemoffset(nuv)))
        csM_toobig(C);
    o = csG_new(C, sizeofuserdata(nuv, size), CS_VUSERDATA);
    ud = gco2u(o);
    ud->metalist = NULL;
    ud->methods = NULL;
    ud->nuv = nuv;
    ud->size = size;
    for (int i = 0; i < nuv; i++)
        setnilval(&ud->uv[i].val);
    return ud;
}


IMethod *csMM_newinsmethod(cs_State *C, Instance *ins, const TValue *method) {
    GCObject *o = csG_new(C, sizeof(IMethod), CS_VIMETHOD);
    IMethod *im = gco2im(o);
    im->ins = ins;
    setobj(C, &im->method, method);
    return im;
}


int csMM_eqimethod(const IMethod *v1, const IMethod *v2) {
    return (v1 == v2) || /* same instance... */
        (v1->ins == v2->ins && /* ...or equal instances */
         csV_raweq(&v1->method, &v2->method)); /* ...and equal methods */
}


UMethod *csMM_newudmethod(cs_State *C, UserData *ud, const TValue *method) {
    GCObject *o = csG_new(C, sizeof(UMethod), CS_VUMETHOD);
    UMethod *um = gco2um(o);
    um->ud = ud;
    setobj(C, &um->method, method);
    return um;
}


int csMM_equmethod(const UMethod *v1, const UMethod *v2) {
    return (v1 == v2) || /* same instance... */
        (v1->ud == v2->ud && /* ...or equal userdata */
         csV_raweq(&v1->method, &v2->method)); /* ...and equal methods */
}


/* get method 'mm' */
const TValue *csMM_get(cs_State *C, const TValue *v, int mm) {
    List *ml;
    cs_assert(0 <= mm && mm < CS_MM_NUM);
    switch (ttypetag(v)) {
        case CS_VINSTANCE: ml = insval(v)->oclass->metalist; break;
        case CS_VUSERDATA: ml = uval(v)->metalist; break;
        default: ml = NULL; break;
    }
    return (ml ? csA_getival(C, ml, mm) : &G(C)->nil);
}


/*
** Return the name of the type of an object. For tables and userdata
** with metatable, use their '__name' metafield, if present.
*/
const char *csMM_objtypename(cs_State *C, const TValue *o) {
    Table *t;
    if ((ttistable(o) && (t = tval(o))) ||
        (ttisinstance(o) && (t = insval(o)->fields))) {
        const TValue *name = csH_getshortstr(t, csS_new(C, "__name"));
        if (ttisstring(name)) /* is '__name' a string? */
            return getstr(strval(name)); /* use it as type name */
    }
    return typename(ttype(o)); /* else use standard type name */
}


/* call __setidx metamethod */
void csMM_callset(cs_State *C, const TValue *f, const TValue *o,
                               const TValue *k, const TValue *v) {
    SPtr func = C->sp.p;
    setobj2s(C, func, f);
    setobj2s(C, func + 1, o);
    setobj2s(C, func + 2, k);
    setobj2s(C, func + 3, v);
    C->sp.p = func + 4;
    csV_call(C, func, 0);
}


/* call __getidx metamethod */
void csMM_callgetres(cs_State *C, const TValue *f, const TValue *o,
                                  const TValue *k, SPtr res) {
    ptrdiff_t result = savestack(C, res);
    SPtr func = C->sp.p;
    setobj2s(C, func, f);
    setobj2s(C, func + 1, o);
    setobj2s(C, func + 2, k);
    C->sp.p = func + 3;
    csV_call(C, func, 1);
    res = restorestack(C, result);
    setobjs2s(C, res, --C->sp.p);
}


/* call binary method and store the result in 'res' */
void csMM_callbinres(cs_State *C, const TValue *f, const TValue *o1,
                                  const TValue *o2, SPtr res) {
    ptrdiff_t result = savestack(C, res);
    SPtr func = C->sp.p;
    setobj2s(C, func, f);
    setobj2s(C, func + 1, o1);
    setobj2s(C, func + 2, o2);
    C->sp.p += 3; /* assuming EXTRA_STACK */
    csV_call(C, func, 1);
    res = restorestack(C, result);
    setobj2s(C, res, s2v(--C->sp.p));
}


static int callbinMM(cs_State *C, const TValue *v1, const TValue *v2,
                                                    SPtr res, int mm) {
    int t1 = ttypetag(v1);
    if (t1 != ttypetag(v2) ||
            (t1 == CS_VINSTANCE && insval(v1)->oclass != insval(v2)->oclass)) {
        return 0; /* different types or different classes */
    } else {
        const TValue *fn = csMM_get(C, v1, mm);
        if (ttisnil(fn)) { /* metamethod not found? */
            fn = csMM_get(C, v2, mm); /* try other instance */
            if (ttisnil(fn)) return 0; /* no metamethod entry */
        }
        csMM_callbinres(C, fn, v1, v2, res);
        return 1; /* ok */
    }
}


void csMM_trybin(cs_State *C, const TValue *v1, const TValue *v2,
                              SPtr res, int texpect, int mm) {
    const int tarr[] = { texpect, CS_T_NONE };
    if (c_unlikely(!callbinMM(C, v1, v2, res, mm)))
        csD_binoperror(C, v1, v2, tarr, mm);
}


void csMM_callunaryres(cs_State *C, const TValue *fn,
                                    const TValue *o, SPtr res) {
    ptrdiff_t result = savestack(C, res);
    SPtr func = C->sp.p;
    setobj2s(C, func, fn);
    setobj2s(C, func + 1, o);
    C->sp.p += 2; /* assuming EXTRA_STACK */
    csV_call(C, func, 1);
    res = restorestack(C, result);
    setobj2s(C, res, s2v(--C->sp.p));
}


static int callunMM(cs_State *C, const TValue *o, SPtr res, int mt) {
    const TValue *fn = csMM_get(C, o, mt);
    if (c_likely(!ttisnil(fn))) {
        csMM_callunaryres(C, fn, o, res);
        return 1;
    }
    return 0;
}


void csMM_tryunary(cs_State *C, const TValue *o, SPtr res, int mm) {
    if (c_unlikely(!callunMM(C, o, res, mm)))
        csD_unoperror(C, o, mm);
}


void csMM_tryconcat(cs_State *C) {
    SPtr p1 = C->sp.p - 2; /* first argument */
    if (c_unlikely(!callbinMM(C, s2v(p1), s2v(p1 + 1), p1, CS_MM_CONCAT)))
        csD_concaterror(C, s2v(p1), s2v(p1 + 1));
}


/* call order method */
int csMM_order(cs_State *C, const TValue *v1, const TValue *v2, int mm) {
    cs_assert(CS_MM_EQ <= mm && mm <= CS_MM_NUM);
    if (c_likely(callbinMM(C, v1, v2, C->sp.p, mm)))
        return !c_isfalse(s2v(C->sp.p));
    csD_ordererror(C, v1, v2, mm);
    /* UNREACHED */
    return 0;
}


/*
** Same as 'csMM_order' except the second operand is an
** immediate value.
*/
int csMM_orderI(cs_State *C, const TValue *v1, int v2, int flip, int isflt,
                int mm) {
    const TValue *v2_;
    TValue aux;
    if (isflt) {
        setfval(&aux, cast_num(v2));
    } else
        setival(&aux, v2);
    v2_ = (flip ? v1 : &aux);
    return csMM_order(C, v1, v2_, mm);
}
