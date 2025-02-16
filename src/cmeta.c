/*
** cmeta.h
** Functions for metamethods and meta types
** See Copyright Notice in CScript.h
*/


#define CS_CORE


#include "cmeta.h"
#include "clexer.h"
#include "csconf.h"
#include "cstring.h"
#include "cdebug.h"
#include "cstate.h"
#include "ctable.h"
#include "cobject.h"
#include "cgc.h"
#include "cvm.h"
#include "cmem.h"


static const char udataname[] = "userdata";

CSI_DEF const char *const csO_typenames[CSI_TOTALTYPES] = {
    "no value", "nil", "boolean", "number", udataname, udataname, "string",
    "array", "table", "function", "class", "instance", "thread",
    "upvalue", "proto" /* these last cases are used for tests only */
};


void csMM_init(cs_State *C) {
    const char *mmnames[CS_MM_N] = { /* ORDER MM */
        "__getidx", "__setidx", "__gc", "__close", "__call", "__init",
        "__concat", "__add", "__sub", "__mul", "__div", "__mod", "__pow",
        "__shl", "__shr", "__band", "__bor", "__bxor", "__unm", "__bnot",
        "__eq", "__lt", "__le"
    };
    for (int i = 0; i < CS_MM_N; i++) {
        OString *s = csS_new(C, mmnames[i]);
        s->extra = i + NUM_KEYWORDS + 1;
        G(C)->mmnames[i] = s;
        csG_fix(C, obj2gco(G(C)->mmnames[i]));
    }
}


TValue *csMM_newvmt(cs_State *C) {
    TValue *vmt = csM_newarray(C, CS_MM_N, TValue);
    for (int i = 0; i < CS_MM_N; i++)
        setnilval(&vmt[i]);
    return vmt;
}


OClass *csMM_newclass(cs_State *C) {
    GCObject *o = csG_new(C, sizeof(OClass), CS_VCLASS);
    OClass *cls = gco2cls(o);
    cls->vmt = NULL;
    cls->methods = NULL;
    cls->gclist = NULL;
    return cls;
}


Instance *csMM_newinstance(cs_State *C, OClass *cls) {
    GCObject *o = csG_new(C, sizeof(Instance), CS_VINSTANCE);
    Instance *ins = gco2ins(o);
    ins->oclass = cls;
    ins->fields = NULL;
    setinsval2s(C, C->sp.p++, ins); /* anchor instance */
    ins->fields = csH_new(C);
    C->sp.p--; /* remove instance */
    return ins;
}


IMethod *csMM_newinsmethod(cs_State *C, Instance *ins, const TValue *method) {
    GCObject *o = csG_new(C, sizeof(IMethod), CS_VIMETHOD);
    IMethod *im = gco2im(o);
    im->ins = ins;
    setobj(C, &im->method, method);
    return im;
}


UserData *csMM_newuserdata(cs_State *C, size_t size, int nuv) {
    GCObject *o = csG_new(C, sizeofuserdata(nuv, size), CS_VUSERDATA);
    UserData *ud = gco2u(o);
    ud->vmt = NULL;
    ud->nuv = nuv;
    ud->size = size;
    return ud;
}


/* get method 'mm' */
const TValue *csMM_get(cs_State *C, const TValue *v, cs_MM mm) {
    TValue *vmt;
    cs_assert(0 <= mm && mm < CS_MM_N);
    switch (ttypetag(v)) {
        case CS_VINSTANCE: vmt = insval(v)->oclass->vmt; break;
        case CS_VUSERDATA: vmt = uval(v)->vmt; break;
        default: vmt = G(C)->vmt[ttype(v)]; break;
    }
    return (vmt ? &vmt[mm] : &G(C)->nil);
}


/* call __setidx fn */
void csMM_callset(cs_State *C, const TValue *fn, const TValue *p1,
                  const TValue *p2, const TValue *p3) {
    SPtr func = C->sp.p;
    setobj2s(C, func, fn);
    setobj2s(C, func + 1, p1);
    setobj2s(C, func + 2, p2);
    setobj2s(C, func + 3, p3);
    C->sp.p = func + 4;
    csV_call(C, func, 0);
}


/* call __getidx fn */
void csMM_callgetres(cs_State *C, const TValue *fn, const TValue *p1,
                     const TValue *p2, SPtr res) {
    ptrdiff_t result = savestack(C, res);
    SPtr func = C->sp.p;
    setobj2s(C, func, fn);
    setobj2s(C, func + 1, p1);
    setobj2s(C, func + 2, p2);
    C->sp.p = func + 3;
    csV_call(C, func, 1);
    res = restorestack(C, result);
    setobjs2s(C, res, --C->sp.p);
}


/* call binary method and store the result in 'res' */
void csMM_callbinres(cs_State *C, const TValue *fn, const TValue *self,
                     const TValue *rhs, SPtr res) {
    /* assuming EXTRA_STACK */
    ptrdiff_t result = savestack(C, res);
    SPtr func = C->sp.p;
    setobj2s(C, func, fn); /* push function */
    setobj2s(C, func + 1, self); /* lhs arg */
    setobj2s(C, func + 2, rhs); /* rhs arg */
    C->sp.p += 3;
    csV_call(C, func, 1);
    res = restorestack(C, result);
    setobj2s(C, res, s2v(--C->sp.p));
}


static int callbinaux(cs_State *C, const TValue *v1, const TValue *v2,
                      SPtr res, int mt) {
    const TValue *fn = csMM_get(C, v1, mt);
    if (ttisnil(fn)) {
        fn = csMM_get(C, v2, mt);
        if (ttisnil(fn)) return 0;
    }
    csMM_callbinres(C, fn, v1, v2, res);
    return 1;
}


/* try to call binary arithmetic or bitwise method */
void csMM_trybin(cs_State *C, const TValue *v1, const TValue *v2, SPtr res,
                 cs_MM mm) {
    if (c_unlikely(ttypetag(v1) != ttypetag(v2) /* types don't match */
                || !callbinaux(C, v1, v2, res, mm))) { /* or no method ? */
        switch (mm) {
        case CS_MM_BNOT: case CS_MM_BSHL: case CS_MM_BSHR:
        case CS_MM_BAND: case CS_MM_BOR: case CS_MM_BXOR:
            csD_bitwerror(C, v1, v2);
            break; /* unreached */
        default:
            csD_aritherror(C, v1, v2);
            break; /* unreached */
        }
    }
}


/* call unary method and store result in 'res' */
void csMM_callunaryres(cs_State *C, const TValue *fn, const TValue *v,
                       SPtr res) {
    ptrdiff_t result = savestack(C, res);
    SPtr func = C->sp.p;
    setobj2s(C, func, fn); /* push function */
    setobj2s(C, func + 1, v); /* 'self' */
    res = restorestack(C, result);
    setobj2s(C, res, s2v(--C->sp.p));
}


static int callunaryaux(cs_State *C, const TValue *v, SPtr res, int mt) {
    const TValue *fn = csMM_get(C, v, mt);
    if (!ttisnil(fn)) {
        csMM_callunaryres(C, fn, v, res);
        return 1;
    }
    return 0;
}


/* try to call unary method */
void csMM_tryunary(cs_State *C, const TValue *v, SPtr res, cs_MM mm) {
    if (c_unlikely(!callunaryaux(C, v, res, mm))) {
        switch (mm) {
            case CS_MM_BNOT: {
                csD_bitwerror(C, v, v);
                break; /* UNREACHED */
            }
            case CS_MM_UNM: {
                csD_aritherror(C, v, v);
                break; /* UNREACHED */
            }
            default: cs_assert(0); break;
        }
    }
}


void csMM_tryconcat(cs_State *C) {
    SPtr top = C->sp.p;
    const TValue *self = s2v(top - 2);
    const TValue *rhs = s2v(top - 1);
    if (c_unlikely(ttypetag(self) != ttypetag(rhs) || /* types not matching */
                !callbinaux(C, self, rhs, top - 2, CS_MM_CONCAT))) {
        csD_concaterror(C, self, rhs);
    }
}


/* call order method */
int csMM_order(cs_State *C, const TValue *v1, const TValue *v2, cs_MM mm) {
    cs_assert(CS_MM_EQ <= mm && mm <= CS_MM_N);
    if (c_likely(callbinaux(C, v1, v2, C->sp.p, mm)))
        return !c_isfalse(s2v(C->sp.p));
    csD_ordererror(C, v1, v2);
    /* UNREACHED */
    return 0;
}


/*
** Same as 'csMM_order' except the second operand is an
** immediate value.
*/
int csMM_orderI(cs_State *C, const TValue *v1, int v2, int flip, int isflt,
                cs_MM mm) {
    const TValue *v2_;
    TValue aux;
    if (isflt) {
        setfval(&aux, cast_num(v2));
    } else
        setival(&aux, v2);
    v2_ = (flip ? v1 : &aux);
    return csMM_order(C, v1, v2_, mm);
}
