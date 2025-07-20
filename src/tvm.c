/*
** tvm.c
** Tokudae Virtual Machine
** See Copyright Notice in tokudae.h
*/

#define tvm_c
#define TOKU_CORE

#include "tokudaeprefix.h"

#include <string.h>

#include "tapi.h"
#include "tlist.h"
#include "tokudaetonf.h"
#include "tfunction.h"
#include "tgc.h"
#include "ttable.h"
#include "tokudae.h"
#include "tokudaelimits.h"
#include "tobject.h"
#include "tdebug.h"
#include "tobject.h"
#include "tstate.h"
#include "tcode.h"
#include "tvm.h"
#include "tmeta.h"
#include "tstring.h"
#include "ttrace.h"
#include "tprotected.h"



/*
** By default, use jump table.
*/
#if !defined(TOKU_USE_JUMPTABLE)
#if defined(__GNUC__)
#define TOKU_USE_JUMPTABLE	1
#else
#define TOKU_USE_JUMPTABLE	0
#endif
#endif



/*
** 't_intfitsf' checks whether a given integer is in the range that
** can be converted to a float without rounding. Used in comparisons.
*/

/* number of bits in the mantissa of a float */
#define NBM		(t_floatatt(MANT_DIG))

/*
** Check whether some integers may not fit in a float, testing whether
** (maxinteger >> NBM) > 0. (That implies (1 << NBM) <= maxinteger.)
** (The shifts are done in parts, to avoid shifting by more than the size
** of an integer. In a worst case, NBM == 113 for long double and
** sizeof(long) == 32.)
*/
#if ((((TOKU_INTEGER_MAX >> (NBM / 4)) >> (NBM / 4)) >> (NBM / 4)) \
	>> (NBM - (3 * (NBM / 4))))  >  0

/* limit for integers that fit in a float */
#define MAXINTFITSF	((toku_Unsigned)1 << NBM)

/* check whether 'i' is in the interval [-MAXINTFITSF, MAXINTFITSF] */
#define t_intfitsf(i)	((MAXINTFITSF + t_castS2U(i)) <= (2 * MAXINTFITSF))

#else /* all integers fit in a float precisely */

#define t_intfitsf(i)	1

#endif


/* swap (const) TValue* */
#if !defined(t_swap)

#define t_swap(v1_,v2_) \
    { TValue *temp = (v1_); (v1_) = (v2_); (v2_) = temp; }

#define t_cswap(v1_,v2_) \
    { const TValue *temp = (v1_); (v1_) = (v2_); (v2_) = temp; }

#endif


static int booleans[2] = { TOKU_VFALSE, TOKU_VTRUE };


/*
** Allocate new CSript closure, push it on stack and
** initialize its upvalues.
*/
static void pushclosure(toku_State *T, Proto *p, UpVal **encup, SPtr base) {
    int nup = p->sizeupvals;
    UpValInfo *uv = p->upvals;
    CSClosure *cl = tokuF_newCSClosure(C, nup);
    cl->p = p;
    setclCSval2s(C, C->sp.p++, cl); /* anchor to stack */
    for (int i = 0; i < nup; i++) { /* fill its upvalues */
        if (uv[i].onstack) /* upvalue refers to local variable? */
            cl->upvals[i] = tokuF_findupval(C, base + uv[i].idx);
        else /* get upvalue from enclosing function */
            cl->upvals[i] = encup[uv[i].idx];
        tokuG_objbarrier(C, cl, cl->upvals[i]);
    }
}


/*
** Integer division; handles division by 0 and possible
** overflow if 'y' == '-1' and 'x' == TOKU_INTEGER_MIN.
*/
toku_Integer tokuV_divi(toku_State *T, toku_Integer x, toku_Integer y) {
    if (t_unlikely(t_castS2U(y) + 1 <= 1)) { /* 'y' == '0' or '-1' */
        if (y == 0)
            tokuD_runerror(C, "divide by zero");
        return intop(-, 0, x);
    } else {
        toku_Integer q = x / y; /* perform C division */
        if ((x ^ y) < 0 && x % y != 0) /* 'm/n' would be negative non-integer? */
            q -= 1; /* correct result for different rounding */
        return q;
    }
}


/*
** Integer modulus; handles modulo by 0 and overflow
** as explained in 'tokuV_divi()'.
*/
toku_Integer tokuV_modi(toku_State *T, toku_Integer x, toku_Integer y) {
    if (t_unlikely(t_castS2U(y) + 1 <= 1)) {
        if (y == 0)
            tokuD_runerror(C, "attempt to 'n%%0'");
        return 0;
    } else {
        toku_Integer r = x % y;
        if (r != 0 && (r ^ y) < 0) /* 'x/y' would be non-integer negative? */
            r += y; /* correct result for different rounding */
        return r;
    }
}


/* floating point modulus */
toku_Number tokuV_modf(toku_State *T, toku_Number x, toku_Number y) {
    toku_Number r;
    t_nummod(C, x, y, r);
    return r;
}


/*
** Perform binary arithmetic operations on objects, this function is free
** to call metamethods in cases where raw arithmetics are not possible.
*/
void tokuV_binarithm(toku_State *T, const TValue *v1,
                                const TValue *v2, SPtr res, int op) {
    if (!tokuO_arithmraw(C, v1, v2, s2v(res), op))
        tokuMM_trybin(C, v1, v2, res, (op - TOKU_OP_ADD) + TOKU_MT_ADD);
}


/*
** Perform unary arithmetic operations on objects, this function is free
** to call metamethods in cases where raw arithmetics are not possible.
*/
void tokuV_unarithm(toku_State *T, const TValue *v, SPtr res, int op) {
    TValue aux;
    setival(&aux, 0);
    if (!tokuO_arithmraw(C, v, &aux, s2v(C->sp.p - 1), op))
        tokuMM_tryunary(C, v, res, (op - TOKU_OP_UNM) + TOKU_MT_UNM);
}


t_sinline int intLEfloat(toku_Integer i, toku_Number f) {
    if (t_intfitsf(i))
        return t_numle(cast_num(i), f); /* compare them as floats */
    else {  /* i <= f <=> i <= floor(f) */
        toku_Integer fi;
        if (tokuO_n2i(f, &fi, N2IFLOOR)) /* fi = floor(f) */
            return i <= fi; /* compare them as integers */
        else /* 'f' is either greater or less than all integers */
            return f > 0; /* greater? */
    }
}


t_sinline int floatLEint(toku_Number f, toku_Integer i) {
    if (t_intfitsf(i))
        return t_numle(f, cast_num(i)); /* compare them as floats */
    else {  /* f <= i <=> ceil(f) <= i */
        toku_Integer fi;
        if (tokuO_n2i(f, &fi, N2ICEIL)) /* fi = ceil(f) */
            return fi <= i; /* compare them as integers */
        else /* 'f' is either greater or less than all integers */
            return f < 0; /* less? */
    }
}


/* less equal ordering on numbers */
t_sinline int LEnum(const TValue *v1, const TValue *v2) {
    toku_assert(ttisnum(v1) && ttisnum(v2));
    if (ttisint(v1)) {
        toku_Integer i1 = ival(v1);
        if (ttisint(v2))
            return i1 <= ival(v2);
        else
            return intLEfloat(i1, fval(v2));
    } else {
        toku_Number n1 = fval(v1);
        if (ttisint(v2))
            return floatLEint(n1, ival(v2));
        else
            return t_numle(n1, fval(v2));
    }
}


/* less equal ordering on non-number values */
t_sinline int LEother(toku_State *T, const TValue *v1, const TValue *v2) {
    if (ttisstring(v1) && ttisstring(v2))
        return (tokuS_cmp(strval(v1), strval(v2)) <= 0);
    else
        return tokuMM_order(C, v1, v2, TOKU_MT_LE);
}


/* 'less or equal' ordering '<=' */
int tokuV_orderle(toku_State *T, const TValue *v1, const TValue *v2) {
    if (ttisnum(v1) && ttisnum(v2))
        return LEnum(v1, v2);
    return LEother(C, v1, v2);
}


t_sinline int intLTfloat(toku_Integer i, toku_Number f) {
    if (t_intfitsf(i))
        return t_numlt(cast_num(i), f);
    else { /* i < f <=> i < ceil(f) */
        toku_Integer fi;
        if (tokuO_n2i(f, &fi, N2ICEIL)) /* fi = ceil(f) */
            return i < fi; /* compare them as integers */
        else /* 'f' is either greater or less than all integers */
            return f > 0; /* greater? */
    }
}


t_sinline int floatLTint(toku_Number f, toku_Integer i) {
    if (t_intfitsf(i))
        return t_numlt(f, cast_num(i)); /* compare them as floats */
    else { /* f < i <=> floor(f) < i */
        toku_Integer fi;
        if (tokuO_n2i(f, &fi, N2IFLOOR)) /* fi = floor(f) */
            return fi < i; /* compare them as integers */
        else /* 'f' is either greater or less than all integers */
            return f < 0; /* less? */
    }
}


/* 'less than' ordering '<' on number values */
t_sinline int LTnum(const TValue *v1, const TValue *v2) {
    toku_assert(ttisnum(v1) && ttisnum(v2));
    if (ttisint(v1)) {
        toku_Integer i1 = ival(v1);
        if (ttisint(v2))
            return i1 < ival(v2);
        else
            return intLTfloat(i1, fval(v2));
    } else {
        toku_Number n1 = fval(v1);
        if (ttisflt(v2))
            return t_numlt(n1, fval(v2));
        else
            return floatLTint(n1, ival(v2));
    }
}


/* 'less than' ordering '<' on non-number values */
t_sinline int LTother(toku_State *T, const TValue *v1, const TValue *v2) {
    if (ttisstring(v1) && ttisstring(v2))
        return tokuS_cmp(strval(v1), strval(v2)) < 0;
    else
        return tokuMM_order(C, v1, v2, TOKU_MT_LT);
}


/* 'less than' ordering '<' */
int tokuV_orderlt(toku_State *T, const TValue *v1, const TValue *v2) {
    if (ttisnum(v1) && ttisnum(v2))
        return LTnum(v1, v2);
    return LTother(C, v1, v2);
}


/* 
** Equality ordering '=='.
** In case 'C' is NULL perform raw equality (without invoking '__eq').
*/
int tokuV_ordereq(toku_State *T, const TValue *v1, const TValue *v2) {
    toku_Integer i1, i2;
    const TValue *fmm;
    int swap = 0;
    if (ttypetag(v1) != ttypetag(v2)) {
        if (ttype(v1) != ttype(v2) || ttype(v1) != TOKU_T_NUMBER)
            return 0;
        return (tokuO_tointeger(v1, &i1, N2IEQ) &&
                tokuO_tointeger(v2, &i2, N2IEQ) && i1 == i2);
    }
    switch (ttypetag(v1)) {
        case TOKU_VNIL: case TOKU_VFALSE: case TOKU_VTRUE: return 1;
        case TOKU_VNUMINT: return ival(v1) == ival(v2);
        case TOKU_VNUMFLT: return t_numeq(fval(v1), fval(v2));
        case TOKU_VLCF: return lcfval(v1) == lcfval(v2);
        case TOKU_VLIGHTUSERDATA: return pval(v1) == pval(v2);
        case TOKU_VSHRSTR: return eqshrstr(strval(v1), strval(v2));
        case TOKU_VLNGSTR: return tokuS_eqlngstr(strval(v1), strval(v2));
        case TOKU_VIMETHOD: return tokuMM_eqim(imval(v1), imval(v2));
        case TOKU_VUMETHOD: return tokuMM_equm(umval(v1), umval(v2));
        case TOKU_VUSERDATA: {
            if  (C == NULL || (ttisnil(fmm = tokuMM_get(C, v1, TOKU_MT_EQ)) &&
                    (swap = 1) && ttisnil(fmm = tokuMM_get(C, v2, TOKU_MT_EQ))))
                return udval(v1) == udval(v2);
            break;
        }
        case TOKU_VINSTANCE: {
            if (C == NULL || (insval(v1)->oclass != insval(v2)->oclass) ||
                    (ttisnil(fmm = tokuMM_get(C, v1, TOKU_MT_EQ)) &&
                    (swap = 1) && ttisnil(fmm = tokuMM_get(C, v2, TOKU_MT_EQ))))
                return insval(v1) == insval(v2);
            break;
        }
        default: return gcoval(v1) == gcoval(v2);
    }
    toku_assert(!ttisnil(fmm));
    if (swap) t_cswap(v1, v2);
    tokuMM_callbinres(C, fmm, v1, v2, C->sp.p);
    return !t_isfalse(s2v(C->sp.p));
}


/* generic set */
#define tokuV_setgen(C,o,k,v,f) \
    { const TValue *fmm = tokuMM_get(C, o, TOKU_MT_SETIDX); \
      if (ttisnil(fmm)) { f(C, o, k, v); } \
      else { tokuMM_callset(C, fmm, o, k, v); }}


void tokuV_rawsetstr(toku_State *T, const TValue *o, const TValue *k,
                                                 const TValue *v) {
    Table *t;
    switch (ttypetag(o)) {
        case TOKU_VLIST: {
            List *l = listval(o);
            tokuV_setlist(C, l, k, v, tokuA_setstr);
            break;
        }
        case TOKU_VTABLE: {
            t = tval(o);
            goto set_table;
        }
        case TOKU_VINSTANCE: {
            t = insval(o)->fields;
        set_table:
            tokuV_settable(C, t, strval(k), v, tokuH_setstr);
            break;
        }
        default: {
            tokuD_typeerror(C, o, "index");
            break; /* unreachable */
        }
    }
}


void tokuV_setstr(toku_State *T, const TValue *o, const TValue *k,
                                              const TValue *v) {
    tokuV_setgen(C, o, k, v, tokuV_rawsetstr);
}


void tokuV_rawsetint(toku_State *T, const TValue *o, const TValue *k,
                                                 const TValue *v) {
    Table *t;
    switch (ttypetag(o)) {
        case TOKU_VLIST: {
            List *l = listval(o);
            FatValue fv = { .v=k, .i=ival(k) };
            tokuV_setlist(C, l, &fv, v, tokuA_setint);
            break;
        }
        case TOKU_VTABLE: {
            t = tval(o);
            goto set_table;
        }
        case TOKU_VINSTANCE: {
            t = insval(o)->fields;
        set_table:
            tokuV_settable(C, t, ival(k), v, tokuH_setint);
            break;
        }
        default: {
            tokuD_typeerror(C, o, "index");
            break; /* unreachable */
        }
    }
}


void tokuV_setint(toku_State *T, const TValue *o, const TValue *k,
                                              const TValue *v) {
    tokuV_setgen(C, o, k, v, tokuV_rawsetint);
}


void tokuV_rawset(toku_State *T, const TValue *o, const TValue *k,
                                              const TValue *v) {
    Table *t;
    switch (ttypetag(o)) {
        case TOKU_VLIST: {
            List *l = listval(o);
            tokuV_setlist(C, l, k, v, tokuA_set);
            break;
        }
        case TOKU_VTABLE: {
            t = tval(o);
            goto set_table;
        }
        case TOKU_VINSTANCE: {
            t = insval(o)->fields;
        set_table:
            tokuV_settable(C, t, k, v, tokuH_set);
            break;
        }
        default: {
            tokuD_typeerror(C, o, "index");
            break; /* unreachable */
        }
    }
}


void tokuV_set(toku_State *T, const TValue *o, const TValue *k, const TValue *v) {
    tokuV_setgen(C, o, k, v, tokuV_rawset);
}


/* bind method to instance and set it at 'res' */
#define bindmethod(C,in,fn,res) \
        setimval2s(C, res, tokuMM_newinsmethod(C, in, fn))


/* generic get */
#define tokuV_getgen(C,o,k,res,f) \
    { const TValue *fmm = tokuMM_get(C, o, TOKU_MT_GETIDX); \
      if (ttisnil(fmm)) { f(C, o, k, res); } \
      else { tokuMM_callgetres(C, fmm, o, k, res); }}


t_sinline void finishTget(toku_State *T, const TValue *slot, SPtr res) {
    if (!ttisnil(slot)) {
        setobj2s(C, res, slot);
    } else
        setnilval(s2v(res));
}


t_sinline void trybindmethod(toku_State *T, const TValue *slot, Instance *in,
                                                              SPtr res) {
    if (!isempty(slot)) { /* have method? */
        setobj2s(C, res, slot);
        bindmethod(C, in, slot, res);
    } else
        setnilval(s2v(res));
}


void tokuV_rawgetstr(toku_State *T, const TValue *o, const TValue *k, SPtr res) {
    switch (ttypetag(o)) {
        case TOKU_VLIST: {
            tokuA_getstr(C, listval(o), k, s2v(res));
            break;
        }
        case TOKU_VTABLE: {
            finishTget(C, tokuH_getstr(tval(o), strval(k)), res);
            break;
        }
        case TOKU_VINSTANCE: {
            Instance *in = insval(o);
            const TValue *slot = tokuH_getstr(in->fields, strval(k));
            if (isempty(slot) && in->oclass->methods) {
                /* try methods table */
                slot = tokuH_getstr(in->oclass->methods, strval(k));
                trybindmethod(C, slot, in, res);
            } else
                setobj2s(C, res, slot);
            break;
        }
        default: {
            tokuD_typeerror(C, o, "index");
            break;
        }
    }
}


void tokuV_getstr(toku_State *T, const TValue *o, const TValue *k, SPtr res) {
    tokuV_getgen(C, o, k, res, tokuV_rawgetstr);
}


void tokuV_rawgetint(toku_State *T, const TValue *o, const TValue *k, SPtr res) {
    switch (ttypetag(o)) {
        case TOKU_VLIST: {
            FatValue fv = { .v=k, .i=ival(k) };
            tokuA_getint(C, listval(o), &fv, s2v(res));
            break;
        }
        case TOKU_VTABLE: {
            finishTget(C, tokuH_getint(tval(o), ival(k)), res);
            break;
        }
        case TOKU_VINSTANCE: {
            Instance *in = insval(o);
            const TValue *slot = tokuH_getint(in->fields, ival(k));
            if (isempty(slot) && in->oclass->methods) {
                /* try methods table */
                slot = tokuH_getint(in->oclass->methods, ival(k));
                trybindmethod(C, slot, in, res);
            } else
                setobj2s(C, res, slot);
            break;
        }
        default: {
            tokuD_typeerror(C, o, "index");
            break;
        }
    }
}


void tokuV_getint(toku_State *T, const TValue *o, const TValue *k, SPtr res) {
    tokuV_getgen(C, o, k, res, tokuV_rawgetint);
}


void tokuV_rawget(toku_State *T, const TValue *o, const TValue *k, SPtr res) {
    switch (ttypetag(o)) {
        case TOKU_VLIST: {
            tokuA_get(C, listval(o), k, s2v(res));
            break;
        }
        case TOKU_VTABLE: {
            finishTget(C, tokuH_get(tval(o), k), res);
            break;
        }
        case TOKU_VINSTANCE: {
            Instance *in = insval(o);
            const TValue *slot = tokuH_get(in->fields, k);
            if (isempty(slot) && in->oclass->methods) {
                /* try methods table */
                slot = tokuH_get(in->oclass->methods, k);
                trybindmethod(C, slot, in, res);
            } else
                setobj2s(C, res, slot);
            break;
        }
        default: {
            tokuD_typeerror(C, o, "index");
            break;
        }
    }
}


void tokuV_get(toku_State *T, const TValue *o, const TValue *k, SPtr res) {
    tokuV_getgen(C, o, k, res, tokuV_rawget);
}


#define getsuper(C,in,scl,k,res,fget) { \
    if (t_unlikely(scl == NULL)) \
        tokuD_runerror(C, "class instance has no superclass"); \
    if ((scl)->methods) { \
        const TValue *f = fget((scl)->methods, k); \
        if (!isempty(f)) { bindmethod(C, in, f, res); } \
      } else \
         setnilval(s2v(res)); }


/*
** Executes a return hook for Tokudae and C functions and sets/corrects
** 'oldpc'. (Note that this correction is needed by the line hook, so it
** is done even when return hooks are off.)
*/
static void rethook(toku_State *T, CallFrame *cf, int nres) {
    if (C->hookmask & TOKU_MASK_RET) { /* is return hook on? */
        SPtr firstres = C->sp.p - nres; /* index of first result */
        int delta = 0; /* correction for vararg functions */
        int ftransfer;
        if (isTokudae(cf)) {
            Proto *p = cf_func(cf)->p;
            if (p->isvararg)
                delta = cf->cs.nvarargs + p->arity + 1;
        }
        cf->func.p += delta; /* if vararg, back to virtual 'func' */
        ftransfer = cast(t_ushort, firstres - cf->func.p) - 1;
        toku_assert(ftransfer >= 0);
        tokuD_hook(C, TOKU_HOOK_RET, -1, ftransfer, nres); /* call it */
        cf->func.p -= delta;
    }
    if (isTokudae(cf = cf->prev))
        C->oldpc = relpc(cf->cs.pc, cf_func(cf)->p); /* set 'oldpc' */
}


/* properly move results and if needed close variables */
t_sinline void moveresults(toku_State *T, SPtr res, int nres, int wanted) {
    int i;
    SPtr firstresult;
    switch (wanted) {
        case TOKU_MULTRET: { /* all values needed */
            wanted = nres;
            break;
        }
        case 0: { /* no values needed */
            C->sp.p = res;
            return; /* done */
        }
        case 1: { /* one value needed */
            if (nres == 0)
                setnilval(s2v(res));
            else
                setobjs2s(C, res, C->sp.p - nres);
            C->sp.p = res + 1;
            return; /* done */
        }
        default: { /* two/more results and/or to-be-closed variables */
            if (hastocloseCfunc(wanted)) { /* to-be-closed variables? */
                res = tokuF_close(C, res, CLOSEKTOP); /* do the closing */
                if (C->hookmask) { /* if needed, call hook after '__close's */
                    ptrdiff_t savedres = savestack(C, res);
                    rethook(C, C->cf, nres);
                    res = restorestack(C, savedres); /* hook can move stack */
                }
                wanted = decodeNresults(wanted); /* decode nresults */
                if (wanted == TOKU_MULTRET)
                    wanted = nres; /* we want all results */
            }
            break;
        }
    }
    /* generic case (all values needed or 2 or more values needed) */
    firstresult = C->sp.p - nres;
    if (nres > wanted) /* have extra results? */
        nres = wanted; /* discard them */
    for (i = 0; i < nres; i++) /* move all the results */
        setobjs2s(C, res + i, firstresult + i);
    for (; i < wanted; i++)
        setnilval(s2v(res + i));
    C->sp.p = res + wanted;
}


#define next_cf(C)   ((C)->cf->next ? (C)->cf->next : tokuT_newcf(C))

t_sinline CallFrame *prepCallframe(toku_State *T, SPtr func, int nres,
                                   int mask, SPtr top) {
    CallFrame *cf = C->cf = next_cf(C);
    cf->func.p = func;
    cf->top.p = top;
    cf->nresults = nres;
    cf->status = mask;
    return cf;
}


/* move the results into correct place and return to caller */
t_sinline void poscall(toku_State *T, CallFrame *cf, int nres) {
    int wanted = cf->nresults;
    if (t_unlikely(C->hookmask && !hastocloseCfunc(wanted)))
        rethook(C, cf, nres);
    /* move results to proper place */
    moveresults(C, cf->func.p, nres, cf->nresults);
    /* function cannot be in any of these cases when returning */
    toku_assert(!(cf->status & (CFST_HOOKED | CFST_FIN)));
    C->cf = cf->prev; /* back to caller (after closing variables) */
}


t_sinline int precallC(toku_State *T, SPtr func, int nres, toku_CFunction f) {
    int n;
    CallFrame *cf;
    checkstackGCp(C, TOKU_MINSTACK, func); /* ensure minimum stack space */
    C->cf = cf = prepCallframe(C, func, nres, CFST_CCALL,
                                              C->sp.p + TOKU_MINSTACK);
    toku_assert(cf->top.p <= C->stackend.p);
    if (t_unlikely(C->hookmask & TOKU_MASK_CALL)) {
        int narg = cast_int(C->sp.p - func) - 1;
        tokuD_hook(C, TOKU_HOOK_CALL, -1, 0, narg);
    }
    toku_unlock(C);
    n = (*f)(C);
    toku_lock(C);
    api_checknelems(C, n);
    poscall(C, cf, n);
    return n;
}


/* 
** Shifts stack by one slot in direction of stack pointer,
** and inserts 'f' in place of 'func'.
** Warning: this function assumes there is enough space for 'f'.
*/
t_sinline void auxinsertf(toku_State *T, SPtr func, const TValue *f) {
    for (SPtr p = C->sp.p; p > func; p--)
        setobjs2s(C, p, p-1);
    C->sp.p++;
    setobj2s(C, func, f);
}


t_sinline SPtr trymetacall(toku_State *T, SPtr func) {
    const TValue *f;
    checkstackGCp(C, 1, func); /* space for func */
    f = tokuMM_get(C, s2v(func), TOKU_MT_CALL); /* (after previous GC) */
    if (t_unlikely(ttisnil(f))) /* missing __call? (after GC) */
        tokuD_callerror(C, s2v(func));
    auxinsertf(C, func, f);
    return func;
}


CallFrame *precall(toku_State *T, SPtr func, int nres) {
retry:
    switch (ttypetag(s2v(func))) {
        case TOKU_VCCL: { /* C closure */
            precallC(C, func, nres, clCval(s2v(func))->fn);
            return NULL; /* done */
        }
        case TOKU_VLCF: { /* light C function */
            precallC(C, func, nres, lcfval(s2v(func)));
            return NULL; /* done */
        }
        case TOKU_VCSCL: { /* Tokudae closure */
            CallFrame *cf;
            Proto *p = clCSval(s2v(func))->p;
            int nargs = (C->sp.p - func) - 1; /* number of args received */
            int nparams = p->arity; /* number of fixed parameters */
            int fsize = p->maxstack; /* frame size */
            checkstackGCp(C, fsize, func);
            C->cf = cf = prepCallframe(C, func, nres, 0, func + 1 + fsize);
            cf->cs.pc = cf->cs.pcret = p->code; /* set starting point */
            for (; nargs < nparams; nargs++)
                setnilval(s2v(C->sp.p++)); /* set missing as 'nil' */
            if (!p->isvararg) /* not a vararg function? */
                C->sp.p = func + 1 + nparams; /* might have extra args */
            toku_assert(cf->top.p <= C->stackend.p);
            return cf; /* new call frame */
        }
        case TOKU_VCLASS: { /* Class object */
            const TValue *fmm;
            Instance *ins = tokuMM_newinstance(C, classval(s2v(func)));
            tokuG_checkfin(C, obj2gco(ins), ins->oclass->metalist);
            setinsval2s(C, func, ins); /* replace class with its instance */
            fmm = tokuMM_get(C, s2v(func), TOKU_MT_INIT);
            if (!ttisnil(fmm)) { /* have __init ? */
                checkstackGCp(C, 1, func); /* space for fmm */
                fmm = tokuMM_get(C, s2v(func), TOKU_MT_INIT); /* (after GC) */
                if (t_likely(!ttisnil(fmm))) { /* have __init (after GC)? */
                    auxinsertf(C, func, fmm); /* insert it into stack... */
                    goto retry; /* ...and try calling it */
                } else goto noinit; /* no __init (after GC) */
            } else {
            noinit:
                C->sp.p -= (C->sp.p - func - 1); /* remove args */
                toku_assert(nres >= toku_MULRET); /* no close */
                moveresults(C, func, 1, nres);
                return NULL; /* done */
            }
        }
        case TOKU_VIMETHOD: { /* Instance method */
            IMethod *im = imval(s2v(func));
            checkstackGCp(C, 2, func); /* space for method and instance */
            auxinsertf(C, func, &im->method); /* insert method object... */
            setinsval2s(C, func + 1, im->ins); /* ...and ins. as first arg */
            goto retry;
        }
        case TOKU_VUMETHOD: { /* UserData method */
            UMethod *um = umval(s2v(func));
            checkstackGCp(C, 2, func); /* space for method and userdata */
            auxinsertf(C, func, &um->method); /* insert method object... */
            setudval2s(C, func + 1, um->ud); /* ...and udata as first arg */
            goto retry;
        }
        default: { /* try __call metamethod */
            func = trymetacall(C, func);
            goto retry;
        }
    }
}


t_sinline void ccall(toku_State *T, SPtr func, int nresults, t_uint32 inc) {
    CallFrame *cf;
    C->nCcalls += inc;
    if (t_unlikely(getCcalls(C) >= TOKUI_MAXCCALLS)) {
        checkstackp(C, 0, func); /* free any use of EXTRA_STACK */
        tokuT_checkCstack(C);
    }
    if ((cf = precall(C, func, nresults)) != NULL) { /* Tokudae function? */
        cf->status = CFST_FRESH; /* mark it as a "fresh" execute */
        tokuV_execute(C, cf); /* call it */
    }
    C->nCcalls -= inc;
}


/* external interface for 'ccall' */
void tokuV_call(toku_State *T, SPtr func, int nresults) {
    ccall(C, func, nresults, nyci);
}


#define isemptystr(v)   (ttisshrstring(v) && strval(v)->shrlen == 0)


static void copy2buff(SPtr top, int n, char *buff) {
    size_t done = 0;
    do {
        OString *s = strval(s2v(top - n));
        size_t len = getstrlen(s);
        memcpy(&buff[done], getstr(s), len * sizeof(char));
        done += len;
    } while (--n > 0);
}


void tokuV_concat(toku_State *T, int total) {
    if (total == 1)
        return; /* done */
    do {
        SPtr top = C->sp.p;
        int n = 2; /* number of elements (minimum 2) */
        if (!(ttisstring(s2v(top - 2)) && ttisstring(s2v(top - 1))))
            tokuMM_tryconcat(C);
        else if (isemptystr(s2v(top - 1))) /* second operand is empty string? */
            ; /* result already in the first operand */
        else if (isemptystr(s2v(top - 2))) { /* first operand is empty string? */
            setobjs2s(C, top - 2, top - 1); /* result is second operand */
        } else { /* at least 2 non-empty strings */
            size_t ltotal = getstrlen(strval(s2v(top - 1)));
            /* collect total length and number of strings */
            for (n = 1; n < total && ttisstring(s2v(top - n - 1)); n++) {
                size_t len = getstrlen(strval(s2v(top - n - 1)));
                if (t_unlikely(len >= TOKU_MAXSIZE - sizeof(OString) - ltotal)) {
                    C->sp.p = top - total; /* pop strings */
                    tokuD_runerror(C, "string length overflow");
                }
                ltotal += len;
            }
            OString *s;
            if (ltotal <= TOKUI_MAXSHORTLEN) { /* fits in a short string? */
                char buff[TOKUI_MAXSHORTLEN];
                copy2buff(top, n, buff);
                s = tokuS_newl(C, buff, ltotal);
            } else { /* otherwise long string */
                s = tokuS_newlngstrobj(C, ltotal);
                copy2buff(top, n, getstr(s));
            }
            setstrval2s(C, top - n, s);
        }
        total -= n - 1; /* got 'n' strings to create one new */
        C->sp.p -= n - 1; /* popped 'n' strings and pushed one */
    } while (total > 1);
}


t_sinline OClass *checksuper(toku_State *T, const TValue *scl) {
    if (t_unlikely(!ttisclass(scl)))
        tokuD_runerror(C, "inherit from %s value", typename(ttype(scl)));
    return classval(scl);
}


void tokuV_inherit(toku_State *T, OClass *cls, OClass *scl) {
    if (scl->methods) { /* superclass has methods? */
        if (!cls->methods) /* class needs a method table? */
            cls->methods = tokuH_new(C);
        tokuH_copykeys(C, cls->methods, scl->methods);
    }
    if (scl->metalist) { /* superclass has metalist entries? */
        if (!cls->metalist) /* class needs a metalist? */
            cls->metalist = tokuA_new(C);
        tokuA_ensure(C, cls->metalist, scl->metalist->len);
        for (int i = 0; i < scl->metalist->len; i++)
            setobj(C, &cls->metalist->arr[i], &scl->metalist->arr[i]);
    }
    cls->sclass = scl; /* set the superclass */
}


#define log2size1(b)     ((b > 0) ? (1<<((b)-1)) : 0)


t_sinline void pushclass(toku_State *T, int b) {
    OClass *cls = tokuMM_newclass(C);
    setclsval2s(C, C->sp.p++, cls);
    if (b & 0x80) { /* have metamethod entries? */
        toku_assert(cls->metalist == NULL);
        cls->metalist = tokuA_new(C);
        b &= 0x7F; /* remove flag */
    }
    if (b > 0) /* have methods? */
        cls->methods = tokuH_newsz(C, log2size1(b));
}


t_sinline void pushlist(toku_State *T, int b) {
    List *l = tokuA_new(C);
    setlistval2s(C, C->sp.p++, l);
    if (b > 0) /* list is not empty? */
        tokuA_ensure(C, l, log2size1(b));
}


t_sinline void pushtable(toku_State *T, int b) {
    Table *t = tokuH_new(C);
    settval2s(C, C->sp.p++, t);
    if (b > 0) /* table is not empty? */
        tokuH_resize(C, t, log2size1(b));
}


/* {======================================================================
** Macros for arithmetic/bitwise/comparison operations on numbers.
** ======================================================================= */

/* 'toku_Integer' arithmetic operations */
#define iadd(C,a,b)    (intop(+, a, b))
#define isub(C,a,b)    (intop(-, a, b))
#define imul(C,a,b)    (intop(*, a, b))

/* integer bitwise operations */
#define iband(a,b)      (intop(&, a, b))
#define ibor(a,b)       (intop(|, a, b))
#define ibxor(a,b)      (intop(^, a, b))

/* integer ordering operations */
#define ilt(a,b)        (a < b)
#define ile(a,b)        (a <= b)
#define igt(a,b)        (a > b)
#define ige(a,b)        (a >= b)


/* 
** Arithmetic operations
*/

#define op_arithKf_aux(C,v1,v2,fop) { \
    toku_Number n1, n2; \
    if (tonumber(v1, n1) && tonumber(v2, n2)) { \
        setfval(v1, fop(C, n1, n2)); \
    } else tokuD_aritherror(C, v1, v2); }


/* arithmetic operations with constant operand for floats */
#define op_arithKf(C,fop) { \
    TValue *v = peek(0); \
    TValue *lk; \
    savestate(C); \
    lk = K(fetch_l()); \
    op_arithKf_aux(C, v, lk, fop); }


/* arithmetic operations with number constant operand */
#define op_arithK(C,iop,fop) { \
    TValue *v = peek(0); \
    TValue *lk; \
    savestate(C); \
    lk = K(fetch_l()); toku_assert(ttisnum(lk)); \
    if (ttisint(v) && ttisint(lk)) { \
        toku_Integer i1 = ival(v); \
        toku_Integer i2 = ival(lk); \
        setival(v, iop(C, i1, i2)); \
    } else { \
        op_arithKf_aux(C, v, lk, fop); \
    }}


/* arithmetic operation error with immediate operand */
#define op_arithI_error(C,v,imm) \
    { TValue v2; setival(&v2, imm); tokuD_aritherror(C, v, &v2); }


/* arithmetic operations with immediate operand for floats */
#define op_arithIf(C,fop) { \
    TValue *v = peek(0); \
    int imm; \
    savestate(C); \
    imm = fetch_l(); \
    imm = IMML(imm); \
    toku_Number n; \
    if (tonumber(v, n)) { \
        toku_Number fimm = cast_num(imm); \
        setfval(v, fop(C, n, fimm)); \
    } else { \
        op_arithI_error(C, v, imm); \
    }}


/* arithmetic operations with immediate operand */
#define op_arithI(C,iop,fop) { \
    TValue *v = peek(0); \
    int imm; \
    savestate(C); \
    imm = fetch_l(); \
    imm = IMML(imm); \
    if (ttisint(v)) { \
        toku_Integer i = ival(v); \
        setival(v, iop(C, i, imm)); \
    } else if (ttisflt(v)) { \
        toku_Number n = fval(v); \
        toku_Number fimm = cast_num(imm); \
        setfval(v, fop(C, n, fimm)); \
    } else { \
        op_arithI_error(C, v, imm); \
    }}


#define op_arithf_aux(C,res,v1,v2,fop) { \
    toku_Number n1; toku_Number n2; \
    if (tonumber(v1, n1) && tonumber(v2, n2)) { \
        setfval(res, fop(C, n1, n2)); \
        sp--; /* v2 */ \
        pc += getopSize(OP_MBIN); \
    }/* else fall through to 'OP_MBIN' */}


/* arithmetic operations with stack operands for floats */
#define op_arithf(C,fop) { \
    TValue *res = peek(1); \
    TValue *v1 = res; \
    TValue *v2 = peek(0); \
    if (fetch_s()) t_swap(v1, v2); \
    op_arithf_aux(C, res, v1, v2, fop); }


/* arithmetic operations with stack operands */
#define op_arith(C,iop,fop) { \
    TValue *res = peek(1); \
    TValue *v1 = res; \
    TValue *v2 = peek(0); \
    if (fetch_s()) t_swap(v1, v2); \
    if (ttisint(v1) && ttisint(v2)) { \
        toku_Integer i1 = ival(v1); toku_Integer i2 = ival(v2); \
        setival(res, iop(C, i1, i2)); \
        sp--; /* v2 */ \
        pc += getopSize(OP_MBIN); \
    } else { \
        op_arithf_aux(C, res, v1, v2, fop); \
    }}



/*
** Bitwise operations
*/

/* bitwise operations with constant operand */
#define op_bitwiseK(C,op) { \
    TValue *v = peek(0); \
    TValue *lk; \
    toku_Integer i1, i2; \
    savestate(C); \
    lk = K(fetch_l()); \
    if (t_likely(tointeger(v, &i1) && tointeger(lk, &i2))) { \
        setival(v, op(i1, i2)); \
    } else tokuD_binoperror(C, v, lk, TOKU_MT_BAND); }


/* bitwise operations with immediate operand */
#define op_bitwiseI(C,op) { \
    TValue *v = peek(0); \
    int imm; \
    savestate(C); \
    imm = fetch_l(); \
    imm = IMML(imm); \
    toku_Integer i; \
    if (t_likely(tointeger(v, &i))) { \
        setival(v, op(i, imm)); \
    } else { \
        TValue vimm; setival(&vimm, imm); \
        tokuD_binoperror(C, v, &vimm, TOKU_MT_BAND); \
    }}


/* bitwise operations with stack operands */
#define op_bitwise(C,op) { \
    TValue *res = peek(1); \
    TValue *v1 = res; \
    TValue *v2 = peek(0); \
    toku_Integer i1; toku_Integer i2; \
    savestate(C); \
    if (fetch_s()) t_swap(v1, v2); \
    if (tointeger(v1, &i1) && tointeger(v2, &i2)) { \
        setival(res, op(i1, i2)); \
        sp--; /* v2 */ \
        pc += getopSize(OP_MBIN); \
    }/* fall through to OP_MBIN */}



/*
** Ordering operations
*/

/* set ordering result */
#define setorderres(v,cond_,eq_) \
    { toku_assert(0 <= (cond_) && (cond_) <= 1); \
      settt(v, booleans[(cond_) == (eq_)]); }


/* order operations with stack operands */
#define op_order(C,iop,fop,other) { \
    TValue *res = peek(1); \
    TValue *v1 = res; \
    TValue *v2 = peek(0); \
    int cond; \
    savestate(C); \
    if (fetch_s()) t_swap(v1, v2); \
    if (ttisint(v1) && ttisint(v2)) { \
        toku_Integer i1 = ival(v1); \
        toku_Integer i2 = ival(v2); \
        cond = iop(i1, i2); \
    } else if (ttisnum(v1) && ttisnum(v2)) { \
        cond = fop(v1, v2); \
    } else { \
        Protect(cond = other(C, v1, v2)); \
    } \
    setorderres(res, cond, 1); sp = --C->sp.p; }


/* order operation error with immediate operand */
#define op_orderI_error(C,v,imm) \
    { TValue v2; setival(&v2, imm); tokuD_ordererror(C, v, &v2); }


/* order operations with immediate operand */
#define op_orderI(C,iop,fop) { \
    int cond, imm; \
    savestate(C); \
    imm = fetch_l(); \
    imm = IMML(imm); \
    TValue *v = peek(0); \
    if (ttisint(v)) { \
        cond = iop(ival(v), imm); \
    } else if (ttisflt(v)) { \
        toku_Number n1 = fval(v); \
        toku_Number n2 = cast_num(imm); \
        cond = fop(n1, n2); \
    } else op_orderI_error(C, v, imm); \
    setorderres(v, cond, 1); }

/* }====================================================================== */


/* {======================================================================
** Interpreter loop
** ======================================================================= */

/* get reference to constant value from 'k' at index 'idx' */
#define K(idx)          (k + (idx))

/* get stack slot at index 'i_' */
#define STK(i_)         (base+(i_))

/* get stack slot of value at 'i_' slots from the value on top */
#define stkpeek(i_)     (sp-1-(i_))

/* idem but this gets the actual value */
#define peek(n)         s2v(stkpeek(n))


/* update global 'trap' */
#define updatetrap(cf)      (trap = cf->cs.trap)

/* update global 'base' */
#define updatebase(cf)      (base = (cf)->func.p + 1)

/*
** If 'trap' (maybe stack reallocation), then update global 'base'
** and global 'sp'.
*/
#define updatestack(cf) \
    { if (t_unlikely(trap)) { updatebase(cf); sp = C->sp.p; }}


/* store global 'pc' */
#define storepc(C)          (cf->cs.pc = pc)

/* store global 'sp' */
#define storesp(C)          (C->sp.p = sp)


#define savestate(C)        (storepc(C), storesp(C))


#if defined(TOKUI_TRACE_EXEC)
#include "ttrace.h"
#define tracepc(C,p)        (csTR_tracepc(C, sp, p, pc, 1))
#else
#define tracepc(C,p)        ((void)0)
#endif


/* protect code that can reallocate stack or change hooks */
#define Protect(exp)    ((exp), updatetrap(cf))


/* collector might of reallocated stack, so update global 'trap' */
#define checkGC(C)      tokuG_condGC(C, (void)0, updatetrap(cf))


/* fetch instruction */
#define fetch() { \
    if (t_unlikely(trap)) { /* stack reallocation or hooks? */ \
        ptrdiff_t sizestack = sp - base; \
        trap = tokuD_traceexec(C, pc, sizestack); /* handle hooks */ \
        updatebase(cf); /* correct stack */ \
        sp = base + sizestack; /* correct stack pointer */ \
    } \
    I = (tracepc(C, cl->p), *(pc++)); \
}

/* fetch short instruction argument */
#define fetch_s()       (*(pc++))

/* fetch long instruction argument */
#define fetch_l()       (pc += SIZE_ARG_L, get3bytes(pc - SIZE_ARG_L))


#define hookdelta()     (getopSize(*pc) + SIZE_INSTR)


/* In cases where jump table is not available or prefered. */
#define vm_dispatch(x)      switch(x)
#define vm_case(l)          case l:
#define vm_break            break


/*
** Do a conditional jump: skip next instruction if 'cond' is not what
** was expected (short arg), else do next instruction, which must be a jump.
*/
#define docondjump(pre) \
    { int cond = fetch_s(); TValue *v = peek(0); pre; \
      if ((!t_isfalse(v)) != cond) check_exp(getopSize(*pc) == 4, pc += 4); \
      vm_break; }


void tokuV_execute(toku_State *T, CallFrame *cf) {
    CSClosure *cl;              /* active Tokudae function (closure) */
    TValue *k;                  /* constants */
    SPtr base;                  /* frame stack base */
    SPtr sp;                    /* local stack pointer (for performance) */
    const Instruction *pc;      /* program counter */
    int trap;                   /* true if 'base' reallocated */
#if TOKU_USE_JUMPTABLE
#include "tjmptable.h"
#endif
startfunc:
    trap = C->hookmask;
    toku_assert(cf->cs.pcret == cf_func(cf)->p->code); /* must be at start */
returning: /* trap already set */
    cl = cf_func(cf);
    k = cl->p->k;
    sp = C->sp.p;
    pc = cf->cs.pcret;
    if (t_unlikely(trap)) /* hooks? */
        trap = tokuD_tracecall(C, hookdelta());
    base = cf->func.p + 1;
    /* main loop of interpreter */
    for (;;) {
        Instruction I; /* instruction being executed */
        fetch();
        toku_assert(base == cf->func.p + 1);
        toku_assert(base <= C->sp.p && C->sp.p <= C->stackend.p);
        vm_dispatch(I) {
            vm_case(OP_TRUE) {
                setbtval(s2v(sp));
                sp++;
                vm_break;
            }
            vm_case(OP_FALSE) {
                setbfval(s2v(sp));
                sp++;
                vm_break;
            }
            vm_case(OP_NIL) {
                int n = fetch_l();
                while (n--)
                    setnilval(s2v(sp++));
                vm_break;
            }
            vm_case(OP_SUPER) {
                OClass *scl = insval(peek(0))->oclass->sclass;
                toku_assert(scl != NULL);
                setclsval2s(C, sp - 1, scl);
                vm_break;
            }
            vm_case(OP_LOAD) {
                setobjs2s(C, sp, STK(fetch_l()));
                sp++;
                vm_break;
            }
            vm_case(OP_CONST) {
                setobj2s(C, sp, K(fetch_s()));
                sp++;
                vm_break;
            }
            vm_case(OP_CONSTL) {
                setobj2s(C, sp, K(fetch_l()));
                sp++;
                vm_break;
            }
            vm_case(OP_CONSTI) {
                int imm = fetch_s();
                setival(s2v(sp), IMM(imm));
                sp++;
                vm_break;
            }
            vm_case(OP_CONSTIL) {
                int imm = fetch_l();
                setival(s2v(sp), IMML(imm));
                sp++;
                vm_break;
            }
            vm_case(OP_CONSTF) {
                int imm = fetch_s();
                setfval(s2v(sp), cast_num(IMM(imm)));
                sp++;
                vm_break;
            }
            vm_case(OP_CONSTFL) {
                int imm = fetch_l();
                setfval(s2v(sp), cast_num(IMML(imm)));
                sp++;
                vm_break;
            }
            vm_case(OP_VARARGPREP) {
                savestate(C);
                /* 'tokuF_adjustvarargs' handles 'sp' */
                Protect(tokuF_adjustvarargs(C, fetch_l(), cf, &sp, cl->p));
                if (t_unlikely(trap)) {
                    storepc(C);
                    tokuD_hookcall(C, cf, hookdelta());
                    /* next opcode will be seen as a "new" line */
                    C->oldpc = getopSize(OP_VARARGPREP); 
                    sp = C->sp.p; /* to properly calculate stack size */
                }
                updatebase(cf); /* function has new base after adjustment */
                vm_break;
            }
            vm_case(OP_VARARG) {
                savestate(C);
                /* 'tokuF_getvarargs' handles 'sp' */
                Protect(tokuF_getvarargs(C, cf, &sp, fetch_l() - 1));
                updatebase(cf); /* make sure 'base' is up-to-date */
                vm_break;
            }
            vm_case(OP_CLOSURE) {
                savestate(C);
                pushclosure(C, cl->p->p[fetch_l()], cl->upvals, base);
                checkGC(C);
                sp++;
                vm_break;
            }
            vm_case(OP_NEWLIST) {
                savestate(C);
                pushlist(C, fetch_s());
                checkGC(C);
                sp++;
                vm_break;
            }
            vm_case(OP_NEWCLASS) {
                savestate(C);
                pushclass(C, fetch_s());
                checkGC(C);
                sp++;
                vm_break;
            }
            vm_case(OP_NEWTABLE) {
                savestate(C);
                pushtable(C, fetch_s());
                checkGC(C);
                sp++;
                vm_break;
            }
            vm_case(OP_METHOD) {
                Table *t = classval(peek(1))->methods;
                TValue *f = peek(0);
                TValue *key;
                savestate(C);
                key = K(fetch_l());
                toku_assert(t && ttisstring(key));
                tokuV_settable(C, t, strval(key), f, tokuH_setstr);
                sp--;
                vm_break;
            }
            vm_case(OP_SETMT) {
                TValue *o = peek(1); /* class */
                TValue *f = peek(0); /* func */
                List *ml = classval(o)->metalist;
                int mt; /* metalist index */
                savestate(C);
                toku_assert(ml != NULL);
                mt = fetch_s();
                toku_assert(cast_uint(mt) < toku_MT_NUM);
                tokuA_ensureindex(C, ml, mt);
                tokuA_fastset(C, ml, mt, f); /* set the entry */
                sp--;
                vm_break;
            }
            vm_case(OP_POP) {
                sp -= fetch_l();
                vm_break;
            }
            vm_case(OP_MBIN) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                savestate(C);
                /* operands are already swapped */
                Protect(tokuMM_trybin(C, v1, v2, sp-2, fetch_s()));
                sp--;
                vm_break;
            }
            vm_case(OP_ADDK) {
                op_arithK(C, iadd, t_numadd);
                vm_break;
            }
            vm_case(OP_SUBK) {
                op_arithK(C, isub, t_numsub);
                vm_break;
            }
            vm_case(OP_MULK) {
                op_arithK(C, imul, t_nummul);
                vm_break;
            }
            vm_case(OP_DIVK) {
                op_arithKf(C, t_numdiv);
                vm_break;
            }
            vm_case(OP_IDIVK) {
                op_arithK(C, tokuV_divi, t_numidiv);
                vm_break;
            }
            vm_case(OP_MODK) {
                op_arithK(C, tokuV_modi, tokuV_modf);
                vm_break;
            }
            vm_case(OP_POWK) {
                op_arithKf(C, t_numpow);
                vm_break;
            }
            vm_case(OP_BSHLK) {
                op_bitwiseK(C, tokuO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHRK) {
                op_bitwiseK(C, tokuO_shiftr);
                vm_break;
            }
            vm_case(OP_BANDK) {
                op_bitwiseK(C, iband);
                vm_break;
            }
            vm_case(OP_BORK) {
                op_bitwiseK(C, ibor);
                vm_break;
            }
            vm_case(OP_BXORK) {
                op_bitwiseK(C, ibxor);
                vm_break;
            }
            vm_case(OP_ADDI) {
                op_arithI(C, iadd, t_numadd);
                vm_break;
            }
            vm_case(OP_SUBI) {
                op_arithI(C, isub, t_numsub);
                vm_break;
            }
            vm_case(OP_MULI) {
                op_arithI(C, imul, t_nummul);
                vm_break;
            }
            vm_case(OP_DIVI) {
                op_arithIf(C, t_numdiv);
                vm_break;
            }
            vm_case(OP_IDIVI) {
                op_arithI(C, tokuV_divi, t_numidiv);
                vm_break;
            }
            vm_case(OP_MODI) {
                op_arithI(C, tokuV_modi, tokuV_modf);
                vm_break;
            }
            vm_case(OP_POWI) {
                op_arithIf(C, t_numpow);
                vm_break;
            }
            vm_case(OP_BSHLI) {
                op_bitwiseI(C, tokuO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHRI) {
                op_bitwiseI(C, tokuO_shiftr);
                vm_break;
            }
            vm_case(OP_BANDI) {
                op_bitwiseI(C, iband);
                vm_break;
            }
            vm_case(OP_BORI) {
                op_bitwiseI(C, ibor);
                vm_break;
            }
            vm_case(OP_BXORI) {
                op_bitwiseI(C, ibxor);
                vm_break;
            }
            vm_case(OP_ADD) {
                op_arith(C, iadd, t_numadd);
                vm_break;
            }
            vm_case(OP_SUB) {
                op_arith(C, isub, t_numsub);
                vm_break;
            }
            vm_case(OP_MUL) {
                op_arith(C, imul, t_nummul);
                vm_break;
            }
            vm_case(OP_DIV) {
                op_arithf(C, t_numdiv);
                vm_break;
            }
            vm_case(OP_IDIV) {
                savestate(C);
                op_arith(C, tokuV_divi, t_numidiv);
                vm_break;
            }
            vm_case(OP_MOD) {
                savestate(C);
                op_arith(C, tokuV_modi, tokuV_modf);
                vm_break;
            }
            vm_case(OP_POW) {
                op_arithf(C, t_numpow);
                vm_break;
            }
            vm_case(OP_BSHL) {
                op_bitwise(C, tokuO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHR) {
                op_bitwise(C, tokuO_shiftr);
                vm_break;
            }
            vm_case(OP_BAND) {
                op_bitwise(C, iband);
                vm_break;
            }
            vm_case(OP_BOR) {
                op_bitwise(C, ibor);
                vm_break;
            }
            vm_case(OP_BXOR) {
                op_bitwise(C, ibxor);
                vm_break;
            }
            /* } CONCAT_OP { */
            vm_case(OP_CONCAT) {
                int total;
                savestate(C);
                total = fetch_l();
                Protect(tokuV_concat(C, total));
                checkGC(C);
                sp -= total - 1;
                vm_break;
            }
            /* } ORDERING_OPS { */
            vm_case(OP_EQK) {
                TValue *v1 = peek(0);
                const TValue *vk = K(fetch_l());
                int eq = fetch_s();
                int cond = tokuV_raweq(v1, vk);
                setorderres(v1, cond, eq);
                vm_break;
            }
            vm_case(OP_EQI) {
                TValue *v1 = peek(0);
                int imm = fetch_l();
                int eq = fetch_s();
                int cond;
                if (ttisint(v1))
                    cond = (ival(v1) == IMML(imm));
                else if (ttisflt(v1))
                    cond = t_numeq(fval(v1), IMML(imm));
                else
                    cond = 0;
                setorderres(v1, cond, eq);
                vm_break;
            }
            vm_case(OP_LTI) {
                op_orderI(C, ilt, t_numlt);
                vm_break;
            }
            vm_case(OP_LEI) {
                op_orderI(C, ile, t_numle);
                vm_break;
            }
            vm_case(OP_GTI) {
                op_orderI(C, igt, t_numgt);
                vm_break;
            }
            vm_case(OP_GEI) {
                op_orderI(C, ige, t_numge);
                vm_break;
            }
            vm_case(OP_EQ) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                int condexp, cond;
                savestate(C);
                condexp = fetch_s();
                Protect(cond = tokuV_ordereq(C, v1, v2));
                setorderres(v1, cond, condexp);
                sp--;
                vm_break;
            }
            vm_case(OP_LT) {
                op_order(C, ilt, LTnum, LTother);
                vm_break;
            }
            vm_case(OP_LE) {
                op_order(C, ile, LEnum, LEother);
                vm_break;
            }
            vm_case(OP_EQPRESERVE) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                int cond;
                savestate(C);
                Protect(cond = tokuV_ordereq(C, v1, v2));
                setorderres(v2, cond, 1);
                vm_break;
            }
            vm_case(OP_NOT) {
                TValue *v = peek(0);
                if (t_isfalse(v))
                    setbtval(v);
                else
                    setbfval(v);
                vm_break;
            }
            vm_case(OP_UNM) {
                TValue *v = peek(0);
                if (ttisint(v)) {
                    toku_Integer ib = ival(v);
                    setival(v, intop(-, 0, ib));
                } else if (ttisflt(v)) {
                    toku_Number n = fval(v);
                    setfval(v, t_numunm(C, n));
                } else {
                    savestate(C);
                    Protect(tokuMM_tryunary(C, v, sp-1, TOKU_MT_UNM));
                }
                vm_break;
            }
            vm_case(OP_BNOT) {
                TValue *v = peek(0);
                if (ttisint(v)) {
                    toku_Integer i = ival(v);
                    setival(v, intop(^, ~t_castS2U(0), i));
                } else {
                    savestate(C);
                    Protect(tokuMM_tryunary(C, v, sp-1, TOKU_MT_BNOT));
                }
                vm_break;
            }
            vm_case(OP_JMP) {
                int offset = fetch_l();
                pc += offset;
                updatetrap(cf);
                vm_break;
            }
            vm_case(OP_JMPS) {
                int offset = fetch_l();
                pc -= offset;
                updatetrap(cf);
                vm_break;
            }
            vm_case(OP_TEST) {
                docondjump((void)0);
            }
            vm_case(OP_TESTPOP) {
                docondjump(sp--);
            }
            vm_case(OP_CALL) {
                CallFrame *newcf;
                SPtr func;
                int nres;
                savestate(C);
                func = STK(fetch_l());
                nres = fetch_l()-1;
                if ((newcf = precall(C, func, nres)) == NULL) /* C call? */
                    updatetrap(cf); /* done (C function already returned) */
                else { /* Tokudae call */
                    cf->cs.pcret = pc; /* after return, continue at 'pc' */
                    cf = newcf; /* run function in this same C frame */
                    goto startfunc;
                }
                /* recalculate 'sp' from maybe outdated (current) 'base' */
                sp = base + (cast_int(C->sp.p - cf->func.p) - 1);
                vm_break;
            }
            vm_case(OP_CLOSE) {
                savestate(C);
                Protect(tokuF_close(C, STK(fetch_l()), TOKU_STATUS_OK));
                vm_break;
            }
            vm_case(OP_TBC) {
                savestate(C);
                tokuF_newtbcvar(C, STK(fetch_l()));
                vm_break;
            }
            vm_case(OP_GETLOCAL) {
                setobjs2s(C, sp, STK(fetch_l()));
                sp++;
                vm_break;
            }
            vm_case(OP_SETLOCAL) {
                setobjs2s(C, STK(fetch_l()), sp - 1);
                sp--;
                vm_break;
            }
            vm_case(OP_GETUVAL) {
                setobj2s(C, sp, cl->upvals[fetch_l()]->v.p);
                sp++;
                vm_break;
            }
            vm_case(OP_SETUVAL) {
                UpVal *uv = cl->upvals[fetch_l()];
                setobj(C, uv->v.p, s2v(sp - 1));
                tokuG_barrier(C, uv, s2v(sp - 1));
                sp--;
                vm_break;
            }
            vm_case(OP_SETLIST) {
                List *l;
                SPtr sl;
                int last, n;
                savestate(C);
                sl = STK(fetch_l()); /* list stack slot */
                l = listval(s2v(sl)); /* 'sl' as list value */
                last = fetch_l(); /* num of elems. already in the list */
                n = fetch_s(); /* num of elements to store */
                if (n == 0)
                    n = (sp - sl) - 1; /* get up to the top */
                toku_assert(n >= 0);
                last += n - 1; /* make 'last' be the last index */
                tokuA_ensureindex(C, l, check_exp(last >= -1, last));
                for (; n > 0; n--) { /* set the values from the stack... */
                    /* ...into the list (in reverse order) */
                    TValue *v = s2v(sl + n);
                    tokuA_fastset(C, l, last, v); 
                    last--;
                }
                sp = sl + 1; /* pop off elements (if any) */
                vm_break;
            }
            vm_case(OP_SETPROPERTY) {
                TValue *v = peek(0);
                TValue *o;
                TValue *prop;
                savestate(C);
                o = peek(fetch_l());
                prop = K(fetch_l());
                toku_assert(ttisstring(prop));
                Protect(tokuV_setstr(C, o, prop, v));
                sp--;
                vm_break;
            }
            vm_case(OP_GETPROPERTY) {
                TValue *v = peek(0);
                TValue *prop;
                savestate(C);
                prop = K(fetch_l());
                toku_assert(ttisstring(prop));
                Protect(tokuV_getstr(C, v, prop, sp - 1));
                vm_break;
            }
            vm_case(OP_GETINDEX) {
                TValue *o = peek(1);
                TValue *key = peek(0);
                savestate(C);
                Protect(tokuV_get(C, o, key, sp - 2));
                sp--;
                vm_break;
            }
            vm_case(OP_SETINDEX) {
                TValue *v = peek(0);
                SPtr os;
                TValue *o;
                TValue *idx;
                savestate(C);
                os = stkpeek(fetch_l());
                o = s2v(os);
                idx = s2v(os+1);
                Protect(tokuV_set(C, o, idx, v));
                sp--;
                vm_break;
            }
            vm_case(OP_GETINDEXSTR) {
                TValue *v = peek(0);
                TValue *i;
                savestate(C);
                i = K(fetch_l());
                toku_assert(ttisstring(i));
                Protect(tokuV_getstr(C, v, i, sp - 1));
                vm_break;
            }
            vm_case(OP_SETINDEXSTR) {
                TValue *v = peek(0);
                TValue *o;
                TValue *idx;
                savestate(C);
                o = peek(fetch_l());
                idx = K(fetch_l());
                toku_assert(ttisstring(idx));
                Protect(tokuV_setstr(C, o, idx, v));
                sp--;
                vm_break;
            }
            vm_case(OP_GETINDEXINT) {
                TValue *v = peek(0);
                TValue i;
                int imm;
                savestate(C);
                imm = fetch_s();
                setival(&i, IMM(imm));
                Protect(tokuV_getint(C, v, &i, sp - 1));
                vm_break;
            }
            vm_case(OP_GETINDEXINTL) {
                TValue *v = peek(0);
                TValue i;
                int imm;
                savestate(C);
                imm = fetch_l();
                setival(&i, IMML(imm));
                Protect(tokuV_getint(C, v, &i, sp - 1));
                vm_break;
            }
            vm_case(OP_SETINDEXINT) {
                TValue *v = peek(0);
                TValue *o;
                toku_Integer imm;
                TValue index;
                savestate(C);
                o = peek(fetch_l());
                imm = fetch_s();
                setival(&index, IMM(imm));
                Protect(tokuV_setint(C, o, &index, v));
                sp--;
                vm_break;
            }
            vm_case(OP_SETINDEXINTL) {
                TValue *v = peek(0);
                TValue *o;
                toku_Integer imm;
                TValue index;
                savestate(C);
                o = peek(fetch_l());
                imm = fetch_l();
                setival(&index, IMML(imm));
                Protect(tokuV_setint(C, o, &index, v));
                sp--;
                vm_break;
            }
            vm_case(OP_GETSUP) {
                Instance *in = insval(peek(0));
                OClass *scl = in->oclass->sclass;
                TValue *sk;
                savestate(C);
                sk = K(fetch_l());
                getsuper(C, in, scl, strval(sk), sp - 1, tokuH_getstr);
                vm_break;
            }
            vm_case(OP_GETSUPIDX) {
                Instance *in = insval(peek(1));
                OClass *scl = in->oclass->sclass;
                TValue *idx = peek(0);
                savestate(C);
                getsuper(C, in, scl, idx, sp - 2, tokuH_get);
                sp--;
                vm_break;
            }
            vm_case(OP_GETSUPIDXSTR) {
                Instance *in = insval(peek(0));
                OClass *scl = in->oclass->sclass;
                TValue *sk;
                savestate(C);
                sk = K(fetch_l());
                getsuper(C, in, scl, strval(sk), sp - 1, tokuH_getstr);
                vm_break;
            }
            vm_case(OP_INHERIT) {
                TValue *o1 = peek(1); /* superclass */
                TValue *o2 = peek(0); /* class */
                OClass *cls = classval(o2);
                OClass *scl;
                savestate(C);
                toku_assert(!tokuV_raweq(o1, o2));
                scl = checksuper(C, o1);
                tokuV_inherit(C, cls, scl);
                checkGC(C);
                vm_break;
            }
            vm_case(OP_FORPREP) {
                int offset;
                savestate(C);
                /* create to-be-closed upvalue (if any) */
                tokuF_newtbcvar(C, STK(fetch_l()) + VAR_TBC);
                offset = fetch_l();
                pc += offset;
                /* go to the next instruction */
                I = (tracepc(C, cl->p), *(pc++));
                toku_assert(I == OP_FORCALL);
                goto l_forcall;
            }
            vm_case(OP_FORCALL) {
            l_forcall: {
                SPtr stk;
                savestate(C);
                stk = STK(fetch_l());
                /* 'stk' slot is iterator function, 'stk + 1' is the
                 * invariant state 'stk + 2' is the control variable, and
                 * 'stk + 3' is the to-be-closed variable. Call uses stack
                 * after these values (starting at 'stk + 4'). */
                /* push function, state and control variable */
                memcpy(stk+VAR_N, stk, VAR_TBC * sizeof(*stk));
                C->sp.p = stk + VAR_N + VAR_TBC; /* adjust stack pointer */
                Protect(tokuV_call(C, stk+VAR_N, fetch_l())); /* call iterator */
                updatestack(cf);
                sp = C->sp.p;
                /* go to the next instruction */
                I = (tracepc(C, cl->p), *(pc++));
                toku_assert(I == OP_FORLOOP);
                goto l_forloop;
            }}
            vm_case(OP_FORLOOP) {
            l_forloop: {
                SPtr stk = STK(fetch_l());
                int offset = fetch_l();
                int nvars = fetch_l();
                if (!ttisnil(s2v(stk + VAR_N))) { /* continue loop? */
                    /* save control variable */
                    setobjs2s(C, stk+VAR_CNTL, stk+VAR_N);
                    pc -= offset; /* jump back to loop body */
                } else /* otherwise leave the loop (fall through) */
                    sp -= nvars; /* remove leftover vars from previous call */
                vm_break;
            }}
            vm_case(OP_RET) {
                SPtr stk;
                int nres; /* number of results */
                savestate(C);
                stk = STK(fetch_l());
                nres = fetch_l() - 1;
                if (nres < 0) /* not fixed ? */
                    nres = sp - stk;
                if (fetch_s()) { /* have open upvalues? */
                    tokuF_close(C, base, CLOSEKTOP);
                    updatetrap(cf);
                    updatestack(cf);
                }
                if (cl->p->isvararg) /* vararg function? */
                    cf->func.p -= cf->cs.nvarargs + cl->p->arity + 1;
                C->sp.p = stk + nres; /* set stk ptr for 'poscall' */
                poscall(C, cf, nres);
                updatetrap(cf); /* 'poscall' can change hooks */
                /* return from Tokudae function */
                if (cf->status & CFST_FRESH) /* top-level function? */
                    return; /* end this frame */
                else {
                    cf = cf->prev; /* return to caller */
                    goto returning; /* continue running caller in this frame */
                }
            }
        }
    }
}

/* }====================================================================== */
