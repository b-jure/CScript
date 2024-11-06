/*
** cmeta.h
** Functions for metamethods and meta types
** See Copyright Notice in CScript.h
*/

#include "cmeta.h"
#include "cconf.h"
#include "cstring.h"
#include "cdebug.h"
#include "cstate.h"
#include "chashtable.h"
#include "cobject.h"
#include "cgc.h"
#include "cvm.h"
#include "cmem.h"



void crMM_init(cs_State *ts) {
    const char *mmnames[CS_NUM_MM] = {
        "__init", "__getidx", "__setidx", "__gc", "__close",
        "__add", "__sub", "__mul", "__div", "__mod", "__pow",
        "__not", "__bnot", "__shl", "__shr", "__band", "__bor",
        "__xor", "__eq", "__lt", "__le",
    };
    for (int i = 0; i < CS_NUM_MM; i++) {
        OString *s = crS_new(ts, mmnames[i]);
        s->bits = bitmask(STRVMTBIT);
        s->extra = i;
        G_(ts)->mmnames[i] = s;
        crG_fix(ts, obj2gco(G_(ts)->mmnames[i]));
    }
    /* create global 'HashTable' class */
    TValue key, val;
    OClass *cls = crMM_newclass(ts);
    cls->vmt = NULL;
    cls->methods = NULL;
    setstrval(ts, &key, crS_newlit(ts, CS_HASHTABLE));
    setclsval(ts, &val, cls);
    crH_set(ts, &GI(ts)->fields, &key, &val);
}


TValue *crMM_newvmt(cs_State *ts) {
    TValue *vmt = crM_malloc(ts, SIZEVMT);
    for (int i = 0; i < CS_NUM_MM; i++)
        setnilval(&vmt[i]);
    return vmt;
}


OClass *crMM_newclass(cs_State *ts) {
    OClass *cls = crG_new(ts, sizeof(OClass), CS_VCLASS, OClass);
    cls->methods = NULL;
    cls->vmt = NULL;
    cls->methods = NULL;
    return cls;
}


Instance *crMM_newinstance(cs_State *ts, OClass *cls) {
    Instance *ins = crG_new(ts, sizeof(Instance), CS_VINSTANCE, Instance);
    ins->oclass = cls;
    ins->fields.node = ins->fields.lastfree = NULL;
    ins->fields.size = 0;
    return ins;
}


IMethod *crMM_newinsmethod(cs_State *ts, Instance *ins, const TValue *method) {
    IMethod *im = crG_new(ts, sizeof(IMethod), CS_VMETHOD, IMethod);
    im->receiver = ins;
    setobj(ts, &im->method, method);
    return im;
}


UserData *crMM_newuserdata(cs_State *ts, size_t size, int nuv) {
    UserData *ud = crG_new(ts, sizeofud(nuv, size), CS_VUDATA, UserData);
    ud->vmt = NULL;
    ud->nuv = nuv;
    ud->size = size;
    return ud;
}


/* get method 'mm' */
const TValue *crMM_get(cs_State *ts, const TValue *v, cs_MM mm) {
    TValue *vmt;
    cs_assert(0 <= mm && mm < CS_NUM_MM);
    switch (ttypetag(v)) {
        case CS_VINSTANCE: {
            vmt = (gco2ins(v) != GI(ts) ? gco2ins(v)->oclass->vmt : NULL);
            break;
        }
        case CS_VUDATA: {
            vmt = gco2ud(v)->vmt;
            break;
        }
        default: {
            vmt = G_(ts)->vmt[ttype(v)];
            break;
        }
    }
    return (vmt ? &vmt[mm] : &G_(ts)->nil);
}


/* call hashtable method that doesn't return value/result */
void crMM_callhtm(cs_State *ts, const TValue *fn, const TValue *p1,
                  const TValue *p2, const TValue *p3) {
    SPtr func = ts->sp.p;
    setobj2s(ts, func, fn);
    setobj2s(ts, func + 1, p1);
    setobj2s(ts, func + 2, p2);
    setobj2s(ts, func + 3, p3);
    ts->sp.p = func + 4;
    crV_call(ts, func, 0);
}


/* call hashtable method that returns a value/result */
void crMM_callhtmres(cs_State *ts, const TValue *fn, const TValue *p1,
                     const TValue *p2, SPtr res) {
    ptrdiff_t result = savestack(ts, res);
    SPtr func = ts->sp.p;
    setobj2s(ts, func, fn);
    setobj2s(ts, func + 1, p1);
    setobj2s(ts, func + 2, p2);
    ts->sp.p = func + 3;
    crV_call(ts, func, 1);
    res = restorestack(ts, result);
    setobjs2s(ts, res, --ts->sp.p);
}


/* call binary method and store the result in 'res' */
void crMM_callbinres(cs_State *ts, const TValue *fn, const TValue *self,
                     const TValue *rhs, SPtr res) {
    /* assuming EXTRA_STACK */
    ptrdiff_t result = savestack(ts, res);
    SPtr func = ts->sp.p;
    setobj2s(ts, func, fn); /* push function */
    setobj2s(ts, func + 1, self); /* lhs arg */
    setobj2s(ts, func + 2, rhs); /* rhs arg */
    ts->sp.p += 3;
    crV_call(ts, func, 1);
    res = restorestack(ts, result);
    setobj2s(ts, res, s2v(--ts->sp.p));
}


static int callbinaux(cs_State *ts, const TValue *v1, const TValue *v2,
                      SPtr res, int mt) {
    const TValue *fn = crMM_get(ts, v1, mt);
    if (ttisnil(fn)) {
        fn = crMM_get(ts, v2, mt);
        if (ttisnil(fn)) return 0;
    }
    crMM_callbinres(ts, fn, v1, v2, res);
    return 1;
}


/* try to call binary arithmetic or bitwise method */
void crMM_trybin(cs_State *ts, const TValue *v1, const TValue *v2, SPtr res,
                 cs_MM mm) {
    if (cs_unlikely(ttypetag(v1) != ttypetag(v2) /* types don't match */
                || !callbinaux(ts, v1, v2, res, mm))) { /* or no method ? */
        switch (mm) {
        case CS_MM_BNOT: case CS_MM_BSHL: case CS_MM_BSHR:
        case CS_MM_BAND: case CS_MM_BOR: case CS_MM_BXOR:
            crD_bitwerror(ts, v1, v2);
            break; /* unreached */
        default:
            crD_aritherror(ts, v1, v2);
            break; /* unreached */
        }
    }
}


/* call unary method and store result in 'res' */
void crMM_callunaryres(cs_State *ts, const TValue *fn, const TValue *v,
                       SPtr res) {
    ptrdiff_t result = savestack(ts, res);
    SPtr func = ts->sp.p;
    setobj2s(ts, func, fn); /* push function */
    setobj2s(ts, func + 1, v); /* 'self' */
    res = restorestack(ts, result);
    setobj2s(ts, res, s2v(--ts->sp.p));
}


static int callunaryaux(cs_State *ts, const TValue *v, SPtr res, int mt) {
    const TValue *fn = crMM_get(ts, v, mt);
    if (!ttisnil(fn)) {
        crMM_callunaryres(ts, fn, v, res);
        return 1;
    }
    return 0;
}


/* try to call unary method */
void crMM_tryunary(cs_State *ts, const TValue *v, SPtr res, cs_MM mm) {
    if (cs_unlikely(!callunaryaux(ts, v, res, mm))) {
        switch (mm) {
        case CS_MM_BNOT: {
            crD_bitwerror(ts, v, v);
            break; /* UNREACHED */
        }
        case CS_MM_UNM: {
            crD_aritherror(ts, v, v);
            break; /* UNREACHED */
        }
        default: cs_unreachable(); break;
        }
    }
}


void crMM_tryconcat(cs_State *ts) {
    SPtr top = ts->sp.p;
    const TValue *self = s2v(top - 2);
    const TValue *rhs = s2v(top - 1);
    if (cs_unlikely(ttypetag(self) != ttypetag(rhs) || /* types not matching */
                !callbinaux(ts, self, rhs, top - 2, CS_MM_CONCAT))) {
        crD_concaterror(ts, self, rhs);
    }
}


/* call order method */
int crMM_order(cs_State *ts, const TValue *v1, const TValue *v2, cs_MM mm) {
    cs_assert(CS_MM_EQ <= mm && mm <= CS_NUM_MM);
    if (cs_likely(callbinaux(ts, v1, v2, ts->sp.p, mm)))
        return cri_isfalse(s2v(ts->sp.p));
    crD_ordererror(ts, v1, v2);
    /* UNREACHED */
    return 0;
}


/*
** Same as 'crMM_order' except the second operand is an
** immediate value.
*/
int crMM_orderI(cs_State *ts, const TValue *v1, int v2, int flip, int isflt,
                cs_MM mm) {
    const TValue *v2_;
    TValue aux;
    if (isflt) {
        setfval(&aux, cast_num(v2));
    } else
        setival(&aux, v2);
    v2_ = (flip ? v1 : &aux);
    return crMM_order(ts, v1, v2_, mm);
}


void crMM_freeclass(cs_State *ts, OClass *cls) {
    if (cls->vmt)
        crM_free(ts, cls->vmt, SIZEVMT);
    crH_free(ts, cls->methods);
    crM_free(ts, cls, sizeof(*cls));
}


void crMM_freeinstance(cs_State *ts, Instance *ins) {
    crM_freearray(ts, ins->fields.node, htsize(&ins->fields), Node);
    crM_free(ts, ins, sizeof(*ins));
}


void crMM_freeuserdata(cs_State *ts, UserData *ud) {
    if (ud->vmt)
        crM_free(ts, ud->vmt, SIZEVMT);
    crM_free(ts, ud, sizeofud(ud->nuv, ud->size));
}
