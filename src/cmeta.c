/*
** cmeta.h
** Functions for metamethods and meta types
** See Copyright Notice in CScript.h
*/


#define CS_CORE


#include "cmeta.h"
#include "csconf.h"
#include "cstring.h"
#include "cdebug.h"
#include "cstate.h"
#include "chashtable.h"
#include "cobject.h"
#include "cgc.h"
#include "cvm.h"
#include "cmem.h"



void csMM_init(cs_State *ts) {
    const char *mmnames[CS_NUM_MM] = {
        "__init", "__getidx", "__setidx", "__gc", "__close",
        "__add", "__sub", "__mul", "__div", "__mod", "__pow",
        "__not", "__bnot", "__shl", "__shr", "__band", "__bor",
        "__xor", "__eq", "__lt", "__le",
    };
    for (int i = 0; i < CS_NUM_MM; i++) {
        OString *s = csS_new(ts, mmnames[i]);
        s->bits = bitmask(STRVMTBIT);
        s->extra = i;
        G_(ts)->mmnames[i] = s;
        csG_fix(ts, obj2gco(G_(ts)->mmnames[i]));
    }
    /* create global 'HashTable' class */
    TValue key, val;
    OClass *cls = csMM_newclass(ts);
    cls->vmt = NULL;
    cls->methods = NULL;
    setstrval(ts, &key, csS_newlit(ts, CS_HASHTABLE));
    setclsval(ts, &val, cls);
    csH_set(ts, &GI(ts)->fields, &key, &val);
}


TValue *csMM_newvmt(cs_State *ts) {
    TValue *vmt = csM_malloc(ts, SIZEVMT);
    for (int i = 0; i < CS_NUM_MM; i++)
        setnilval(&vmt[i]);
    return vmt;
}


OClass *csMM_newclass(cs_State *ts) {
    OClass *cls = csG_new(ts, sizeof(OClass), CS_VCLASS, OClass);
    cls->methods = NULL;
    cls->vmt = NULL;
    cls->methods = NULL;
    return cls;
}


Instance *csMM_newinstance(cs_State *ts, OClass *cls) {
    Instance *ins = csG_new(ts, sizeof(Instance), CS_VINSTANCE, Instance);
    ins->oclass = cls;
    ins->fields.node = ins->fields.lastfree = NULL;
    ins->fields.size = 0;
    return ins;
}


IMethod *csMM_newinsmethod(cs_State *ts, Instance *ins, const TValue *method) {
    IMethod *im = csG_new(ts, sizeof(IMethod), CS_VMETHOD, IMethod);
    im->receiver = ins;
    setobj(ts, &im->method, method);
    return im;
}


UserData *csMM_newuserdata(cs_State *ts, size_t size, int nuv) {
    UserData *ud = csG_new(ts, sizeofud(nuv, size), CS_VUDATA, UserData);
    ud->vmt = NULL;
    ud->nuv = nuv;
    ud->size = size;
    return ud;
}


/* get method 'mm' */
const TValue *csMM_get(cs_State *ts, const TValue *v, cs_MM mm) {
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
void csMM_callhtm(cs_State *ts, const TValue *fn, const TValue *p1,
                  const TValue *p2, const TValue *p3) {
    SPtr func = ts->sp.p;
    setobj2s(ts, func, fn);
    setobj2s(ts, func + 1, p1);
    setobj2s(ts, func + 2, p2);
    setobj2s(ts, func + 3, p3);
    ts->sp.p = func + 4;
    csV_call(ts, func, 0);
}


/* call hashtable method that returns a value/result */
void csMM_callhtmres(cs_State *ts, const TValue *fn, const TValue *p1,
                     const TValue *p2, SPtr res) {
    ptrdiff_t result = savestack(ts, res);
    SPtr func = ts->sp.p;
    setobj2s(ts, func, fn);
    setobj2s(ts, func + 1, p1);
    setobj2s(ts, func + 2, p2);
    ts->sp.p = func + 3;
    csV_call(ts, func, 1);
    res = restorestack(ts, result);
    setobjs2s(ts, res, --ts->sp.p);
}


/* call binary method and store the result in 'res' */
void csMM_callbinres(cs_State *ts, const TValue *fn, const TValue *self,
                     const TValue *rhs, SPtr res) {
    /* assuming EXTRA_STACK */
    ptrdiff_t result = savestack(ts, res);
    SPtr func = ts->sp.p;
    setobj2s(ts, func, fn); /* push function */
    setobj2s(ts, func + 1, self); /* lhs arg */
    setobj2s(ts, func + 2, rhs); /* rhs arg */
    ts->sp.p += 3;
    csV_call(ts, func, 1);
    res = restorestack(ts, result);
    setobj2s(ts, res, s2v(--ts->sp.p));
}


static int callbinaux(cs_State *ts, const TValue *v1, const TValue *v2,
                      SPtr res, int mt) {
    const TValue *fn = csMM_get(ts, v1, mt);
    if (ttisnil(fn)) {
        fn = csMM_get(ts, v2, mt);
        if (ttisnil(fn)) return 0;
    }
    csMM_callbinres(ts, fn, v1, v2, res);
    return 1;
}


/* try to call binary arithmetic or bitwise method */
void csMM_trybin(cs_State *ts, const TValue *v1, const TValue *v2, SPtr res,
                 cs_MM mm) {
    if (c_unlikely(ttypetag(v1) != ttypetag(v2) /* types don't match */
                || !callbinaux(ts, v1, v2, res, mm))) { /* or no method ? */
        switch (mm) {
        case CS_MM_BNOT: case CS_MM_BSHL: case CS_MM_BSHR:
        case CS_MM_BAND: case CS_MM_BOR: case CS_MM_BXOR:
            csD_bitwerror(ts, v1, v2);
            break; /* unreached */
        default:
            csD_aritherror(ts, v1, v2);
            break; /* unreached */
        }
    }
}


/* call unary method and store result in 'res' */
void csMM_callunaryres(cs_State *ts, const TValue *fn, const TValue *v,
                       SPtr res) {
    ptrdiff_t result = savestack(ts, res);
    SPtr func = ts->sp.p;
    setobj2s(ts, func, fn); /* push function */
    setobj2s(ts, func + 1, v); /* 'self' */
    res = restorestack(ts, result);
    setobj2s(ts, res, s2v(--ts->sp.p));
}


static int callunaryaux(cs_State *ts, const TValue *v, SPtr res, int mt) {
    const TValue *fn = csMM_get(ts, v, mt);
    if (!ttisnil(fn)) {
        csMM_callunaryres(ts, fn, v, res);
        return 1;
    }
    return 0;
}


/* try to call unary method */
void csMM_tryunary(cs_State *ts, const TValue *v, SPtr res, cs_MM mm) {
    if (c_unlikely(!callunaryaux(ts, v, res, mm))) {
        switch (mm) {
        case CS_MM_BNOT: {
            csD_bitwerror(ts, v, v);
            break; /* UNREACHED */
        }
        case CS_MM_UNM: {
            csD_aritherror(ts, v, v);
            break; /* UNREACHED */
        }
        default: cs_unreachable(); break;
        }
    }
}


void csMM_tryconcat(cs_State *ts) {
    SPtr top = ts->sp.p;
    const TValue *self = s2v(top - 2);
    const TValue *rhs = s2v(top - 1);
    if (c_unlikely(ttypetag(self) != ttypetag(rhs) || /* types not matching */
                !callbinaux(ts, self, rhs, top - 2, CS_MM_CONCAT))) {
        csD_concaterror(ts, self, rhs);
    }
}


/* call order method */
int csMM_order(cs_State *ts, const TValue *v1, const TValue *v2, cs_MM mm) {
    cs_assert(CS_MM_EQ <= mm && mm <= CS_NUM_MM);
    if (c_likely(callbinaux(ts, v1, v2, ts->sp.p, mm)))
        return csi_isfalse(s2v(ts->sp.p));
    csD_ordererror(ts, v1, v2);
    /* UNREACHED */
    return 0;
}


/*
** Same as 'csMM_order' except the second operand is an
** immediate value.
*/
int csMM_orderI(cs_State *ts, const TValue *v1, int v2, int flip, int isflt,
                cs_MM mm) {
    const TValue *v2_;
    TValue aux;
    if (isflt) {
        setfval(&aux, cast_num(v2));
    } else
        setival(&aux, v2);
    v2_ = (flip ? v1 : &aux);
    return csMM_order(ts, v1, v2_, mm);
}


void csMM_freeclass(cs_State *ts, OClass *cls) {
    if (cls->vmt)
        csM_free(ts, cls->vmt, SIZEVMT);
    csH_free(ts, cls->methods);
    csM_free(ts, cls, sizeof(*cls));
}


void csMM_freeinstance(cs_State *ts, Instance *ins) {
    csM_freearray(ts, ins->fields.node, htsize(&ins->fields), Node);
    csM_free(ts, ins, sizeof(*ins));
}


void csMM_freeuserdata(cs_State *ts, UserData *ud) {
    if (ud->vmt)
        csM_free(ts, ud->vmt, SIZEVMT);
    csM_free(ts, ud, sizeofud(ud->nuv, ud->size));
}
