/*
** cmeta.h
** Functions for metamethods and meta types
** See Copyright Notice in cscript.h
*/

#include "cmeta.h"
#include "cstring.h"
#include "cdebug.h"
#include "cstate.h"
#include "chashtable.h"
#include "cobject.h"
#include "cgc.h"
#include "cvm.h"
#include "cmem.h"



void crMM_init(cr_State *ts) {
    const char *mmnames[CR_NUM_MM] = {
        "__init", "__getidx", "__setidx", "__gc", "__close",
        "__add", "__sub", "__mul", "__div", "__mod", "__pow",
        "__not", "__bnot", "__shl", "__shr", "__band", "__bor",
        "__xor", "__eq", "__lt", "__le",
    };
    for (int i = 0; i < CR_NUM_MM; i++) {
        OString *s = crS_new(ts, mmnames[i]);
        s->bits = bitmask(STRVMTBIT);
        s->extra = i;
        G_(ts)->mmnames[i] = s;
        crG_fix(ts, obj2gco(G_(ts)->mmnames[i]));
    }
}


TValue *crMM_newvmt(cr_State *ts) {
    TValue *vmt = crM_malloc(ts, SIZEVMT);
    for (int i = 0; i < CR_NUM_MM; i++)
        setnilval(&vmt[i]);
    return vmt;
}


OClass *crMM_newclass(cr_State *ts) {
    OClass *cls = crG_new(ts, sizeof(OClass), CR_VCLASS, OClass);
    cls->methods = NULL;
    cls->vmt = NULL;
    cls->methods = NULL;
    return cls;
}


Instance *crMM_newinstance(cr_State *ts, OClass *cls) {
    Instance *ins = crG_new(ts, sizeof(Instance), CR_VINSTANCE, Instance);
    ins->oclass = cls;
    ins->fields.node = ins->fields.lastfree = NULL;
    ins->fields.size = 0;
    return ins;
}


IMethod *crMM_newinsmethod(cr_State *ts, Instance *ins, const TValue *method) {
    IMethod *im = crG_new(ts, sizeof(IMethod), CR_VMETHOD, IMethod);
    im->receiver = ins;
    setobj(ts, &im->method, method);
    return im;
}


/* TODO: move to 'crapi' */
// static void parsevmt(cr_State *ts, TValue *actual, cr_VMT *vmt) {
//     for (int i = 0; i < CR_NUM_MM; i++) {
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


UserData *crMM_newuserdata(cr_State *ts, size_t size, int nuv) {
    UserData *ud = crG_new(ts, sizeofud(nuv, size), CR_VUDATA, UserData);
    ud->vmt = NULL;
    ud->nuv = nuv;
    ud->size = size;
    return ud;
}


/* get method 'mm' */
const TValue *crMM_get(cr_State *ts, const TValue *v, cr_MM mm) {
    TValue *vmt;
    cr_assert(0 <= mm && mm < CR_NUM_MM);
    UNUSED(ts);
    switch (ttypetag(v)) {
    case CR_VINSTANCE: vmt = gco2ins(v)->oclass->vmt; break;
    case CR_VUDATA: vmt = gco2ud(v)->vmt; break;
    default: vmt = G_(ts)->vmt[ttype(v)]; break;
    }
    return (vmt ? &vmt[mm] : &G_(ts)->nil);
}


/* call hashtable method that doesn't return value/result */
void crMM_callhtm(cr_State *ts, const TValue *fn, const TValue *p1,
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
void crMM_callhtmres(cr_State *ts, const TValue *fn, const TValue *p1,
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
void crMM_callbinres(cr_State *ts, const TValue *fn, const TValue *v1,
                     const TValue *v2, SPtr res) {
    /* assuming EXTRA_STACK */
    ptrdiff_t result = savestack(ts, res);
    SPtr func = ts->sp.p;
    setobj2s(ts, func, fn); /* push function */
    setobj2s(ts, func + 1, v1); /* lhs arg (self) */
    setobj2s(ts, func + 2, v2); /* rhs arg */
    ts->sp.p += 3;
    crV_call(ts, func, 1);
    res = restorestack(ts, result);
    setobj2s(ts, res, s2v(--ts->sp.p));
}


static int callbinaux(cr_State *ts, const TValue *v1, const TValue *v2,
                      SPtr res, int mt) {
    const TValue *fn = crMM_get(ts, v1, mt);
    if (ttisnil(fn)) {
        fn = crMM_get(ts, v2, mt);
        if (cr_unlikely(ttisnil(fn)))
            return 0;
    }
    crMM_callbinres(ts, fn, v1, v2, res);
    return 1;
}


/* try to call binary arithmetic or bitwise method */
void crMM_trybin(cr_State *ts, const TValue *v1, const TValue *v2, SPtr res,
                 cr_MM mm) {
    if (cr_unlikely(ttypetag(v1) != ttypetag(v2) /* types don't match */
                || !callbinaux(ts, v1, v2, res, mm))) { /* or no method ? */
        switch (mm) {
        case CR_MM_BNOT: case CR_MM_BSHL: case CR_MM_BSHR:
        case CR_MM_BAND: case CR_MM_BOR: case CR_MM_BXOR:
            crD_bitwerror(ts, v1, v2);
            /* UNREACHED */
        default:
            crD_aritherror(ts, v1, v2);
            /* UNREACHED */
        }
    }
}


/* call unary method and store result in 'res' */
void crMM_callunaryres(cr_State *ts, const TValue *fn, const TValue *v,
                       SPtr res) {
    ptrdiff_t result = savestack(ts, res);
    SPtr func = ts->sp.p;
    setobj2s(ts, func, fn); /* push function */
    setobj2s(ts, func + 1, v); /* 'self' */
    res = restorestack(ts, result);
    setobj2s(ts, res, s2v(--ts->sp.p));
}


static int callunaryaux(cr_State *ts, const TValue *v, SPtr res, int mt) {
    const TValue *fn = crMM_get(ts, v, mt);
    if (!ttisnil(fn)) {
        crMM_callunaryres(ts, fn, v, res);
        return 1;
    }
    return 0;
}


/* try to call unary method */
void crMM_tryunary(cr_State *ts, const TValue *v, SPtr res, cr_MM mm) {
    if (cr_unlikely(!callunaryaux(ts, v, res, mm))) {
        switch (mm) {
        case CR_MM_BNOT: {
            crD_bitwerror(ts, v, v);
            break; /* UNREACHED */
        }
        case CR_MM_UNM: {
            crD_aritherror(ts, v, v);
            break; /* UNREACHED */
        }
        default: cr_unreachable(); break;
        }
    }
}


/* call order method */
int crMM_order(cr_State *ts, const TValue *v1, const TValue *v2, cr_MM mm) {
    cr_assert(CR_MM_EQ <= mm && mm <= CR_NUM_MM);
    if (cr_likely(callbinaux(ts, v1, v2, ts->sp.p, mm)))
        return cri_isfalse(s2v(ts->sp.p));
    crD_ordererror(ts, v1, v2);
    /* UNREACHED */
    return 0;
}


/*
** Same as 'crMM_order' except the second operand is an
** immediate value.
*/
int crMM_orderI(cr_State *ts, const TValue *v1, int v2, int flip, int isflt,
                cr_MM mm) {
    const TValue *v2_;
    TValue aux;
    if (isflt) {
        setfval(&aux, cast_num(v2));
    } else
        setival(&aux, v2);
    v2_ = (flip ? v1 : &aux);
    return crMM_order(ts, v1, v2_, mm);
}


void crMM_freeclass(cr_State *ts, OClass *cls) {
    if (cls->vmt)
        crM_free(ts, cls->vmt, SIZEVMT);
    crH_free(ts, cls->methods);
    crM_free(ts, cls, sizeof(*cls));
}


void crMM_freeinstance(cr_State *ts, Instance *ins) {
    crM_freearray(ts, ins->fields.node, htsize(&ins->fields), Node);
    crM_free(ts, ins, sizeof(*ins));
}


void crMM_freeuserdata(cr_State *ts, UserData *ud) {
    if (ud->vmt)
        crM_free(ts, ud->vmt, SIZEVMT);
    crM_free(ts, ud, sizeofud(ud->nuv, ud->size));
}
