/*
** cvm.c
** CScript Virtual Machine
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include <string.h>

#include "capi.h"
#include "clist.h"
#include "csconf.h"
#include "cfunction.h"
#include "cgc.h"
#include "ctable.h"
#include "cscript.h"
#include "climits.h"
#include "cobject.h"
#include "cdebug.h"
#include "cobject.h"
#include "cstate.h"
#include "ccode.h"
#include "cvm.h"
#include "cmeta.h"
#include "cstring.h"
#include "ctrace.h"



/*
** By default, use jump table.
*/
#if !defined(CS_USE_JUMPTABLE)
#if defined(__GNUC__)
#define CS_USE_JUMPTABLE	1
#else
#define CS_USE_JUMPTABLE	0
#endif
#endif



/*
** By default, disable internal bytecode execution tracing.
*/
#if !defined(TRACE_EXEC)
#define TRACE_EXEC      1
#endif



/*
** 'c_intfitsf' checks whether a given integer is in the range that
** can be converted to a float without rounding. Used in comparisons.
*/

/* number of bits in the mantissa of a float */
#define NBM		(c_floatatt(MANT_DIG))

/*
** Check whether some integers may not fit in a float, testing whether
** (maxinteger >> NBM) > 0. (That implies (1 << NBM) <= maxinteger.)
** (The shifts are done in parts, to avoid shifting by more than the size
** of an integer. In a worst case, NBM == 113 for long double and
** sizeof(long) == 32.)
*/
#if ((((CS_INTEGER_MAX >> (NBM / 4)) >> (NBM / 4)) >> (NBM / 4)) \
	>> (NBM - (3 * (NBM / 4))))  >  0

/* limit for integers that fit in a float */
#define MAXINTFITSF	((cs_Unsigned)1 << NBM)

/* check whether 'i' is in the interval [-MAXINTFITSF, MAXINTFITSF] */
#define c_intfitsf(i)	((MAXINTFITSF + c_castS2U(i)) <= (2 * MAXINTFITSF))

#else /* all integers fit in a float precisely */

#define c_intfitsf(i)	1

#endif



static int booleans[2] = { CS_VFALSE, CS_VTRUE };


/*
** Allocate new CSript closure, push it on stack and
** initialize its upvalues.
*/
static void pushclosure(cs_State *C, Proto *p, UpVal **enc, SPtr base) {
    int nupvals = p->sizeupvals;
    CSClosure *cl = csF_newCSClosure(C, nupvals);
    cl->p = p;
    setclCSval2s(C, C->sp.p, cl); /* anchor to stack */
    C->sp.p += 1;
    for (int i = 0; i < nupvals; i++) {
        UpValInfo *uv = &p->upvals[i];
        if (uv->onstack)
            cl->upvals[i] = csF_findupval(C, base + uv->idx);
        else
            cl->upvals[i] = enc[uv->idx];
        csG_objbarrier(C, cl, cl->upvals[i]);
    }
}


/*
** Integer division; handles division by 0 and possible
** overflow if 'y' == '-1' and 'x' == CS_INTEGER_MIN.
*/
cs_Integer csV_divi(cs_State *C, cs_Integer x, cs_Integer y) {
    if (c_unlikely(c_castS2U(y) + 1 <= 1)) { /* 'y' == '0' or '-1' */
        if (y == 0)
            csD_runerror(C, "division by zero");
        return c_intop(-, 0, x);
    } else {
        cs_Integer q = x / y; /* perform C division */
        if ((x ^ y) < 0 && x % y != 0) /* 'm/n' would be negative non-integer? */
            q -= 1; /* correct result for different rounding */
        return q;
    }
}


/*
** Integer modulus; handles modulo by 0 and overflow
** as explained in 'csV_divi()'.
*/
cs_Integer csV_modi(cs_State *C, cs_Integer x, cs_Integer y) {
    if (c_unlikely(c_castS2U(y) + 1 <= 1)) {
        if (y == 0)
            csD_runerror(C, "attempt to x%%0");
        return 0;
    } else {
        cs_Integer r = x % y;
        if (r != 0 && (r ^ y) < 0) /* 'x/y' would be non-integer negative? */
            r += y; /* correct result for different rounding */
        return r;
    }
}


/* floating point modulus */
cs_Number csV_modf(cs_State *C, cs_Number x, cs_Number y) {
    cs_Number r;
    c_nummod(C, x, y, r);
    return r;
}


/*
** Perform binary arithmetic operations on objects, this function is free
** to call metamethods in cases where raw arithmetics are not possible.
*/
void csV_binarithm(cs_State *C, const TValue *v1, const TValue *v2, SPtr res,
                   int op) {
    if (!csO_arithmraw(C, v1, v2, s2v(res), op))
        csMM_trybin(C, v1, v2, res, (op - CS_OPADD) + CS_MM_ADD);
}


/*
** Perform unary arithmetic operations on objects, this function is free
** to call metamethods in cases where raw arithmetics are not possible.
*/
void csV_unarithm(cs_State *C, const TValue *v, int op) {
    TValue aux;
    setival(&aux, 0);
    if (!csO_arithmraw(C, v, &aux, s2v(C->sp.p - 1), op))
        csMM_tryunary(C, v, (op - CS_OPUNM) + CS_MM_UNM);
}


c_sinline int LEintfloat(cs_Integer i, cs_Number f) {
    if (c_intfitsf(i))
        return c_numle(cast_num(i), f); /* compare them as floats */
    else {  /* i <= f <=> i <= floor(f) */
        cs_Integer fi;
        if (csO_n2i(f, &fi, N2IFLOOR)) /* fi = floor(f) */
            return i <= fi; /* compare them as integers */
        else /* 'f' is either greater or less than all integers */
            return f > 0; /* greater? */
    }
}


c_sinline int LEfloatint(cs_Number f, cs_Integer i) {
    if (c_intfitsf(i))
        return c_numle(f, cast_num(i)); /* compare them as floats */
    else {  /* f <= i <=> ceil(f) <= i */
        cs_Integer fi;
        if (csO_n2i(f, &fi, N2ICEIL)) /* fi = ceil(f) */
            return fi <= i; /* compare them as integers */
        else /* 'f' is either greater or less than all integers */
            return f < 0; /* less? */
    }
}


/* less equal ordering on numbers */
c_sinline int LEnum(const TValue *v1, const TValue *v2) {
    cs_assert(ttisnum(v1) && ttisnum(v2));
    if (ttisint(v1)) {
        cs_Integer i1 = ival(v1);
        if (ttisint(v2))
            return i1 <= ival(v2);
        else
            return LEintfloat(i1, fval(v2));
    } else {
        cs_Number n1 = fval(v1);
        if (ttisint(v2))
            return LEfloatint(n1, ival(v2));
        else
            return c_numle(n1, fval(v2));
    }
}


/* less equal ordering on non-number values */
c_sinline int LEother(cs_State *C, const TValue *v1, const TValue *v2) {
    if (ttisstring(v1) && ttisstring(v2))
        return (csS_cmp(strval(v1), strval(v2)) <= 0);
    else
        return csMM_order(C, v1, v2, CS_MM_LE);
}


/* 'less or equal' ordering '<=' */
int csV_orderle(cs_State *C, const TValue *v1, const TValue *v2) {
    if (ttisnum(v1) && ttisnum(v2))
        return LEnum(v1, v2);
    return LEother(C, v1, v2);
}


c_sinline int LTintfloat(cs_Integer i, cs_Number f) {
    if (c_intfitsf(i))
        return c_numlt(cast_num(i), f);
    else { /* i < f <=> i < ceil(f) */
        cs_Integer fi;
        if (csO_n2i(f, &fi, N2ICEIL)) /* fi = ceil(f) */
            return i < fi; /* compare them as integers */
        else /* 'f' is either greater or less than all integers */
            return f > 0; /* greater? */
    }
}


c_sinline int LTfloatint(cs_Number f, cs_Integer i) {
    if (c_intfitsf(i))
        return c_numlt(f, cast_num(i)); /* compare them as floats */
    else { /* f < i <=> floor(f) < i */
        cs_Integer fi;
        if (csO_n2i(f, &fi, N2IFLOOR)) /* fi = floor(f) */
            return fi < i; /* compare them as integers */
        else /* 'f' is either greater or less than all integers */
            return f < 0; /* less? */
    }
}


/* 'less than' ordering '<' on number values */
c_sinline int LTnum(const TValue *v1, const TValue *v2) {
    cs_assert(ttisnum(v1) && ttisnum(v2));
    if (ttisint(v1)) {
        cs_Integer i1 = ival(v1);
        if (ttisint(v2))
            return i1 < ival(v2);
        else
            return LTintfloat(i1, fval(v2));
    } else {
        cs_Number n1 = fval(v1);
        if (ttisflt(v2))
            return c_numlt(n1, fval(v2));
        else
            return LTfloatint(n1, ival(v2));
    }
}


/* 'less than' ordering '<' on non-number values */
c_sinline int LTother(cs_State *C, const TValue *v1, const TValue *v2) {
    if (ttisstring(v1) && ttisstring(v2))
        return csS_cmp(strval(v1), strval(v2)) < 0;
    else
        return csMM_order(C, v1, v2, CS_MM_LT);
}


/* 'less than' ordering '<' */
int csV_orderlt(cs_State *C, const TValue *v1, const TValue *v2) {
    if (ttisnum(v1) && ttisnum(v2))
        return LTnum(v1, v2);
    return LTother(C, v1, v2);
}


/* 
** Equality ordering '=='.
** In case 'C' is NULL perform raw equality (without invoking '__eq').
*/
int csV_ordereq(cs_State *C, const TValue *v1, const TValue *v2) {
    cs_Integer i1, i2;
    const TValue *fmm;
    int swap = 0;
    if (ttypetag(v1) != ttypetag(v2)) {
        if (ttype(v1) != ttype(v2) || ttype(v1) != CS_TNUMBER)
            return 0;
        return (csO_tointeger(v1, &i1, N2IEXACT) &&
                csO_tointeger(v2, &i2, N2IEXACT) && i1 == i2);
    }
    switch (ttypetag(v1)) {
        case CS_VNIL: case CS_VFALSE: case CS_VTRUE: return 1;
        case CS_VNUMINT: return ival(v1) == ival(v2);
        case CS_VNUMFLT: return c_numeq(fval(v1), fval(v2));
        case CS_VLCF: return lcfval(v1) == lcfval(v2);
        case CS_VLIGHTUSERDATA: return pval(v1) == pval(v2);
        case CS_VSHRSTR: return eqshrstr(strval(v1), strval(v2));
        case CS_VLNGSTR: return csS_eqlngstr(strval(v1), strval(v2));
        case CS_VIMETHOD: return csMM_eqimethod(imval(v1), imval(v2));
        case CS_VUSERDATA: {
            if  (C == NULL || (ttisnil(fmm = csMM_get(C, v1, CS_MM_EQ)) &&
                    (swap = 1) && ttisnil(fmm = csMM_get(C, v2, CS_MM_EQ))))
                return uval(v1) == uval(v2);
            break;
        }
        case CS_VINSTANCE: {
            if (C == NULL || (insval(v1)->oclass != insval(v2)->oclass) ||
                    (ttisnil(fmm = csMM_get(C, v1, CS_MM_EQ)) &&
                    (swap = 1) && ttisnil(fmm = csMM_get(C, v2, CS_MM_EQ))))
                return insval(v1) == insval(v2);
            break;
        }
        default: return gcoval(v1) == gcoval(v2);
    }
    assert(!ttisnil(fmm));
    if (swap) { const TValue *temp = v1; v1 = v2; v2 = temp; }
    csMM_callbinres(C, fmm, v1, v2, C->sp.p);
    return !c_isfalse(s2v(C->sp.p));
}


static void setlistindex(cs_State *C, List *l, const TValue *index,
                          const TValue *val) {
    cs_Integer i;
    if (c_likely(tointeger(index, &i))) { /* index is integer? */
        if (c_likely(0 <= i)) { /* non-negative index? */
            if (c_unlikely(i > CS_MAXLISTINDEX)) /* 'index' too large? */
                csD_indexerror(C, i, "too large");
            csA_ensureindex(C, l, i); /* expand list */
            setobj(C, &l->b[i], val); /* set the value at index */
        } else /* negative index, error */
            csD_indexerror(C, i, "negative");
    } else
        csD_indextypeerror(C, index);
}


void csV_rawset(cs_State *C, const TValue *obj, const TValue *key,
                const TValue *val) {
    Table *t;
    switch (ttypetag(obj)) {
        case CS_VLIST: {
            List *l = listval(obj);
            setlistindex(C, l, key, val);
            csV_finishrawset(C, l, val);
            break;
        }
        case CS_VTABLE: {
            t = tval(obj);
            goto set_table;
        }
        case CS_VINSTANCE: {
            t = insval(obj)->fields;
        set_table:
            csV_settable(C, t, key, val);
            break;
        }
        default: {
            csD_typeerror(C, obj, "index");
            break; /* unreachable */
        }
    }
}


void csV_set(cs_State *C, const TValue *obj, const TValue *key,
             const TValue *val) {
    const TValue *fmm = csMM_get(C, obj, CS_MM_SETIDX);
    if (!ttisnil(fmm)) /* have metamethod? */
        csMM_callset(C, fmm, obj, key, val);
    else /* otherwise perform raw set */
        csV_rawset(C, obj, key, val);
}


static void listgetindex(cs_State *C, List *l, const TValue *index, SPtr res) {
    cs_Integer i;
    if (c_likely(tointeger(index, &i))) { /* index is integer? */
        if (c_likely(0 <= i)) { /* positive index? */
            if (i < l->n) { /* index in bounds? */
                setobj2s(C, res, &l->b[i]);
            } else /* index out of bounds */
                setnilval(s2v(res));
        } else /* negative index */
            csD_indexerror(C, i, "negative");
    } else /* invalid index */
        csD_indextypeerror(C, index);
}


/* bind method to instance and set it at 'res' */
#define bindmethod(C,in,fn,res) \
        setimval2s(C, res, csMM_newinsmethod(C, in, fn))


void csV_rawget(cs_State *C, const TValue *obj, const TValue *key, SPtr res) {
    switch (ttypetag(obj)) {
        case CS_VLIST: {
            listgetindex(C, listval(obj), key, res);
            break;
        }
        case CS_VTABLE: {
            const TValue *slot = csH_get(tval(obj), key);
            if (!ttisnil(slot)) {
                setobj2s(C, res, slot);
            } else
                setnilval(s2v(res));
            break;
        }
        case CS_VINSTANCE: {
            Instance *in = insval(obj);
            const TValue *slot = csH_get(in->fields, key);
            if (isempty(slot) && in->oclass->methods) {
                /* try methods table */
                slot = csH_get(in->oclass->methods, key);
                if (!isempty(slot)) { /* have method? */
                    setobj2s(C, res, slot);
                    bindmethod(C, in, slot, res);
                    break; /* done */
                } /* else fall through */
                setnilval(s2v(res));
            } else
                setobj2s(C, res, slot);
            break;
        }
        default: {
            csD_typeerror(C, obj, "index");
            break;
        }
    }
}


/*
** XXX
** In the next version of CScript, each instance and full userdata will
** contain additional state information that will tell us if the
** instance/userdata is in its own metamethod, and which one.
** This is usefull in order to avoid recursive calls to the same metamethod
** where this might be unwanted (e.g., __getidx, __setidx).
** In the mentioned example for __getidx/__setidx, user would then be able
** to index 'self' as usual without the need of 'raw(set)get' functions.
** So if the __getidx gets called and another function is called in the
** __getidx, then __getidx will be still active further up the stack,
** meaning the __getidx metamethod can't trigger for that instance/userdata
** until the initial __getidx returns.
*/
void csV_get(cs_State *C, const TValue *obj, const TValue *key, SPtr res) {
    const TValue *fmm = csMM_get(C, obj, CS_MM_GETIDX);
    if (ttisnil(fmm)) /* no metamethod? */
        csV_rawget(C, obj, key, res);
    else /* otherwise call metamethod */
        csMM_callgetres(C, fmm, obj, key, res);
}


#define getsuper(C,in,scl,k,res,fget) { \
    if (c_likely((scl)->methods)) { \
        const TValue *f = fget((scl)->methods, k); \
        if (!isempty(f)) { bindmethod(C, in, f, res); } \
      } else setnilval(s2v(res)); }


/*
** Call binary meta method, but before that perform a quick check
** and invoke error if types don't match, the values are instances
** that belong to different classes or v1 (self) doesn't have the
** metamethod.
*/
c_sinline void precallmbin(cs_State *C, const TValue *v1, const TValue *v2,
                            cs_MM op, SPtr res) {
    const TValue *func;
    const char *opname = getshrstr(G(C)->mmnames[op]) + 2; /* skip '__' */
    if (c_unlikely(ttypetag(v1) != ttypetag(v2)))
        csD_typeerrormeta(C, v1, v2, opname);
    if (c_unlikely(ttisinstance(v1) && insval(v1)->oclass != insval(v2)->oclass))
        csD_runerror(C, "tried to '%s' instances of different class", opname);
    func = csMM_get(C, v1, op);
    if (c_unlikely(ttisnil(func)))
        csD_typeerror(C, v1, opname);
    else
        csMM_callbinres(C, func, v1, v2, res);
}


/* properly move results and if needed close variables */
c_sinline void moveresults(cs_State *C, SPtr res, int nres, int wanted) {
    int i;
    SPtr firstresult;
    switch (wanted) {
        case CS_MULRET: {   /* all values needed */
            wanted = nres;
            break;
        }
        case 0: {           /* no values needed */
            C->sp.p = res;
            return; /* done */
        }
        case 1: {           /* one value needed */
            if (nres == 0)
                setnilval(s2v(res));
            else
                setobjs2s(C, res, C->sp.p - nres);
            C->sp.p = res + 1;
            return; /* done */
        }
        default: {
            if (hastocloseCfunc(wanted)) { /* tbc variables? */
                res = csF_close(C, res, CLOSEKTOP); /* do the closing */
                wanted = decodeNresults(wanted); /* decode nresults */
                if (wanted == CS_MULRET) /* all values needed? */
                    wanted = nres;
            }
            break;
        }
    }
    firstresult = C->sp.p - nres;
    if (nres > wanted) /* have extra results? */
        nres = wanted; /* discard them */
    for (i = 0; i < nres; i++) /* move all the results */
        setobjs2s(C, res + i, firstresult + i);
    for (; i < wanted; i++)
        setnilval(s2v(res + i));
    C->sp.p = res + wanted;
}


/* move the results into correct place and return to caller */
c_sinline void poscall(cs_State *C, CallFrame *cf, int nres) {
    moveresults(C, cf->func.p, nres, cf->nresults);
    C->cf = cf->prev; /* back to caller */
}


#define next_cf(C)   ((C)->cf->next ? (C)->cf->next : csT_newcf(C))

c_sinline CallFrame *initcallframe(cs_State *C, SPtr func, int nres,
                                    int mask, SPtr top) {
    CallFrame *cf = C->cf = next_cf(C);
    cf->func.p = func;
    cf->top.p = top;
    cf->nresults = nres;
    cf->status = mask;
    return cf;
}


c_sinline int precallC(cs_State *C, SPtr func, int nres, cs_CFunction f) {
    CallFrame *cf;
    int n; /* number of returns */
    checkstackGCp(C, CS_MINSTACK, func); /* ensure minimum stack space */
    C->cf = cf = initcallframe(C, func, nres, CFST_CCALL,
                                C->sp.p + CS_MINSTACK);
    cs_unlock(C);
    n = (*f)(C);
    cs_lock(C);
    api_checknelems(C, n);
    poscall(C, cf, n);
    return n;
}


/* 
** Adjust stack for metamethod or instance method call.
** 'func' is the instance or the object that has a metamethod
** and 'f' is the function.
** Stack is shifted so that 'f' is in place of 'func' and 'func' is its
** argument at 'func + 1'. This function assumes there is enough space
** on the stack for 'f'.
*/
c_sinline void auxinsertf(cs_State *C, SPtr func, const TValue *f) {
    for (SPtr p = C->sp.p; p > func; p--)
        setobjs2s(C, p, p-1);
    C->sp.p += 1;
    setobj2s(C, func, f);
}


/* try and call __call metamethod */
c_sinline SPtr trymetacall(cs_State *C, SPtr func) {
    const TValue *f;
    checkstackGCp(C, 1, func); /* space for func */
    f = csMM_get(C, s2v(func), CS_MM_CALL); /* (after GC) */
    if (c_unlikely(ttisnil(f))) /* missing __call? (after GC) */
        csD_callerror(C, s2v(func));
    auxinsertf(C, func, f);
    return func;
}


CallFrame *precall(cs_State *C, SPtr func, int nres) {
retry:
    switch (ttypetag(s2v(func))) {
        case CS_VCCL: { /* C closure */
            precallC(C, func, nres, clCval(s2v(func))->fn);
            return NULL; /* done */
        }
        case CS_VLCF: { /* light C function */
            precallC(C, func, nres, lcfval(s2v(func)));
            return NULL; /* done */
        }
        case CS_VCSCL: { /* CScript closure */
            CallFrame *cf;
            Proto *p = clCSval(s2v(func))->p;
            int nargs = (C->sp.p - func) - 1;
            int nparams = p->arity;
            int fsize = p->maxstack;
            checkstackGCp(C, fsize, func);
            C->cf = cf = initcallframe(C, func, nres, 0, func + fsize + 1);
            cf->pc = cf->realpc = p->code; /* set starting point */
            for (; nargs < nparams; nargs++) {
                setnilval(s2v(C->sp.p)); /* set missing args as 'nil' */
                C->sp.p += 1;
            }
            if (!p->isvararg)
                C->sp.p = func + nparams + 1; /* might have extra args */
            cs_assert(cf->top.p <= C->stackend.p);
            return cf; /* new call frame */
        }
        case CS_VCLASS: { /* Class object */
            const TValue *fmm;
            Instance *in = csMM_newinstance(C, classval(s2v(func)));
            csG_checkfin(C, obj2gco(in), in->oclass->metalist);
            setinsval2s(C, func, in); /* replace class with its instance */
            fmm = csMM_get(C, s2v(func), CS_MM_INIT);
            if (!ttisnil(fmm)) { /* have __init ? */
                checkstackGCp(C, 1, func); /* space for fmm */
                fmm = csMM_get(C, s2v(func), CS_MM_INIT); /* (after GC) */
                if (c_likely(!ttisnil(fmm))) { /* have __init (after GC)? */
                    auxinsertf(C, func, fmm); /* insert it into stack... */
                    goto retry; /* ...and try calling it */
                } else goto noinit; /* no __init (after GC) */
            } else {
            noinit:
                C->sp.p -= (C->sp.p - func - 1); /* remove args */
                return NULL; /* done */
            }
        }
        case CS_VIMETHOD: { /* Instance method */
            IMethod *im = imval(s2v(func));
            checkstackGCp(C, 2, func); /* space for method and instance */
            auxinsertf(C, func, &im->method); /* insert method... */
            setinsval2s(C, func + 1, im->ins); /* ...and ins. as first arg */
            goto retry;
        }
        default: { /* try metaamethod __call */
            func = trymetacall(C, func);
            goto retry;
        }
    }
}


c_sinline void ccall(cs_State *C, SPtr func, int nresults, c_uint32 inc) {
    CallFrame *cf;
    C->nCcalls += inc;
    if (c_unlikely(getCcalls(C) >= CSI_MAXCCALLS)) {
        checkstackp(C, 0, func);  /* free any use of EXTRA_STACK */
        csT_checkCstack(C);
    }
    if ((cf = precall(C, func, nresults)) != NULL) {  /* CScript function? */
        cf->status = CFST_FRESH;
        csV_execute(C, cf);
    }
    C->nCcalls -= inc;
}


/* external interface for 'ccall' */
void csV_call(cs_State *C, SPtr func, int nresults) {
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


void csV_concat(cs_State *C, int total) {
    if (total == 1)
        return; /* done */
    do {
        SPtr top = C->sp.p;
        int n = 2; /* number of elements (minimum 2) */
        if (!(ttisstring(s2v(top - 2)) && ttisstring(s2v(top - 1))))
            csMM_tryconcat(C);
        else if (isemptystr(s2v(top - 1))) /* second operand is empty string? */
            ; /* result already in the first operand */
        else if (isemptystr(s2v(top - 2))) { /* first operand is empty string? */
            setobjs2s(C, top - 2, top - 1); /* result is second operand */
        } else { /* at least 2 non-empty strings */
            size_t ltotal = getstrlen(strval(s2v(top - 1)));
            /* collect total length and number of strings */
            for (n = 1; n < total && ttisstring(s2v(top - n - 1)); n++) {
                size_t len = getstrlen(strval(s2v(top - n - 1)));
                if (c_unlikely(len >= MAXSIZE - sizeof(OString) - ltotal)) {
                    C->sp.p = top - total; /* pop strings */
                    csD_runerror(C, "string length overflow");
                }
                ltotal += len;
            }
            OString *s;
            if (ltotal <= CSI_MAXSHORTLEN) { /* fits in a short string? */
                char buff[CSI_MAXSHORTLEN];
                copy2buff(top, n, buff);
                s = csS_newl(C, buff, ltotal);
            } else { /* otherwise long string */
                s = csS_newlngstrobj(C, ltotal);
                copy2buff(top, n, getstr(s));
            }
            setstrval2s(C, top - n, s);
        }
        total -= n - 1; /* got 'n' strings to create one new */
        C->sp.p -= n - 1; /* popped 'n' strings and pushed one */
    } while (total > 1);
}


static OClass *checksuper(cs_State *C, const TValue *scl) {
    if (c_unlikely(!ttisclass(scl)))
        csD_runerror(C, "inherit from %s value", typename(ttype(scl)));
    return classval(scl);
}


void csV_inherit(cs_State *C, OClass *cls, OClass *scl) {
    if (scl->methods) { /* superclass has methods? */
        if (!cls->methods) /* class needs a method table? */
            cls->methods = csH_new(C);
        csH_copykeys(C, cls->methods, scl->methods);
    }
    cls->metalist = scl->metalist; /* set the metalist */
    cls->sclass = scl; /* set the superclass */
}


/* -----------------------------------------------------------------------
** Macros for arithmetic/bitwise/comparison operations on numbers.
**------------------------------------------------------------------------ */

/* 'cs_Integer' arithmetic operations */
#define iadd(C,a,b)    (c_intop(+, a, b))
#define isub(C,a,b)    (c_intop(-, a, b))
#define imul(C,a,b)    (c_intop(*, a, b))

/* integer bitwise operations */
#define iband(a,b)      (c_intop(&, a, b))
#define ibor(a,b)       (c_intop(|, a, b))
#define ibxor(a,b)      (c_intop(^, a, b))

/* integer ordering operations */
#define ilt(a,b)        (a < b)
#define ile(a,b)        (a <= b)
#define igt(a,b)        (a > b)
#define ige(a,b)        (a >= b)


/* 
** Arithmetic operations
*/

#define op_arithKf_aux(C,v1,v2,fop) { \
    cs_Number n1, n2; \
    if (tonumber(v1, n1) && tonumber(v2, n2)) { \
        setfval(v1, fop(C, n1, n2)); \
    } else csD_aritherror(C, v1, v2); }


/* arithmetic operations with constant operand for floats */
#define op_arithKf(C,fop) { \
    TValue *v = peek(0); \
    TValue *lk = K(fetchl()); \
    op_arithKf_aux(C, v, lk, fop); }


/* arithmetic operations with constant operand */
#define op_arithK(C,iop,fop) { \
    TValue *v = peek(0); \
    TValue *lk = K(fetchl()); cs_assert(ttisnum(lk)); \
    if (ttisint(v) && ttisint(lk)) { \
        cs_Integer i1 = ival(v); \
        cs_Integer i2 = ival(lk); \
        setival(v, iop(C, i1, i2)); \
    } else { \
        op_arithKf_aux(C, v, lk, fop); \
    }}


/* arithmetic operation error with immediate operand */
#define op_arithI_error(C,v,imm) \
    { TValue v2; setival(&v2, imm); csD_aritherror(C, v, &v2); }


/* arithmetic operations with immediate operand for floats */
#define op_arithIf(C,fop) { \
    TValue *v = peek(0); \
    int imm = fetchl(); \
    imm = IMML(imm); \
    cs_Number n; \
    if (tonumber(v, n)) { \
        cs_Number fimm = cast_num(imm); \
        setfval(v, fop(C, n, fimm)); \
    } else { \
        op_arithI_error(C, v, imm); \
    }}


/* arithmetic operations with immediate operand */
#define op_arithI(C,iop,fop) { \
    TValue *v = peek(0); \
    int imm = fetchl(); \
    imm = IMML(imm); \
    if (ttisint(v)) { \
        cs_Integer i = ival(v); \
        setival(v, iop(C, i, imm)); \
    } else if (ttisflt(v)) { \
        cs_Number n = fval(v); \
        cs_Number fimm = cast_num(imm); \
        setfval(v, fop(C, n, fimm)); \
    } else { \
        op_arithI_error(C, v, imm); \
    }}


#define op_arithf_aux(C,res,v1,v2,fop) { \
    cs_Number n1; cs_Number n2; \
    if (tonumber(v1, n1) && tonumber(v2, n2)) { \
        setfval(res, fop(C, n1, n2)); \
        SP(-1); /* v2 */ \
        pc += getOpSize(OP_MBIN); \
    }/* else fall through to 'OP_MBIN' */}


/* arithmetic operations with stack operands for floats */
#define op_arithf(C,fop) { \
    TValue *res = peek(1); \
    TValue *v1 = res; \
    TValue *v2 = peek(0); \
    if (fetchs()) { TValue *temp = v1; v1 = v2; v2 = temp; } \
    op_arithf_aux(C, res, v1, v2, fop); }


/* arithmetic operations with stack operands */
#define op_arith(C,iop,fop) { \
    TValue *res = peek(1); \
    TValue *v1 = res; \
    TValue *v2 = peek(0); \
    if (fetchs()) { TValue *temp = v1; v1 = v2; v2 = temp; } \
    if (ttisint(v1) && ttisint(v2)) { \
        cs_Integer i1 = ival(v1); cs_Integer i2 = ival(v2); \
        setival(res, iop(C, i1, i2)); \
        SP(-1); /* v2 */ \
        pc += getOpSize(OP_MBIN); \
    } else { \
        op_arithf_aux(C, res, v1, v2, fop); \
    }}



/*
** Bitwise operations
*/

/* bitwise operations with constant operand */
#define op_bitwiseK(C,op) { \
    TValue *v = peek(0); \
    TValue *lk = K(fetchl()); \
    cs_Integer i1, i2; \
    if (c_likely(tointeger(v, &i1) && tointeger(lk, &i2))) { \
        setival(v, op(i1, i2)); \
    } else { \
        if (ttisnum(v) && ttisnum(lk)) \
            csD_runerror(C, "number has no integer representation"); \
        else \
            csD_bitwerror(C, v, lk); \
    }}


/* bitwise operations with immediate operand */
#define op_bitwiseI(C,op) { \
    TValue *v = peek(0); \
    int imm = fetchl(); \
    imm = IMML(imm); \
    cs_Integer i; \
    if (c_likely(tointeger(v, &i))) { \
        setival(v, op(i, imm)); \
    } else { \
        if (ttisnum(v)) \
            csD_runerror(C, "number has no integer representation"); \
        else { \
            TValue vimm; setival(&vimm, imm); \
            csD_bitwerror(C, v, &vimm); \
        } \
    }}


/* bitwise operations with stack operands */
#define op_bitwise(C,op) { \
    TValue *res = peek(1); \
    TValue *v1 = res; \
    TValue *v2 = peek(0); \
    cs_Integer i1; cs_Integer i2; \
    if (fetchs()) { TValue *temp = v1; v1 = v2; v2 = temp; } \
    if (tointeger(v1, &i1) && tointeger(v2, &i2)) { \
        setival(res, op(i1, i2)); \
        SP(-1); /* v2 */ \
        pc += getOpSize(OP_MBIN); \
    } else if (c_unlikely(ttisnum(v1) && ttisnum(v2))) { \
        csD_runerror(C, "number has no integer representation"); \
    }/* else try 'OP_MBIN' */}



/*
** Ordering operations
*/

/* set ordering result */
#define setorderres(v,cond_,eq_) \
    { cs_assert(0 <= (cond_) && (cond_) <= 1); \
      settt(v, booleans[(cond_) == (eq_)]); }


/* order operations with stack operands */
#define op_order(C,iop,fop,other) { \
    TValue *res = peek(1); \
    TValue *v1 = res; \
    TValue *v2 = peek(0); \
    int cond; \
    if (fetchs()) { TValue *temp = v1; v1 = v2; v2 = temp; } \
    if (ttisint(v1) && ttisint(v2)) { \
        cs_Integer i1 = ival(v1); \
        cs_Integer i2 = ival(v2); \
        cond = iop(i1, i2); \
    } else if (ttisnum(v1) && ttisnum(v2)) { \
        cond = fop(v1, v2); \
    } else { \
        storepc(C); \
        cond = other(C, v1, v2); \
    } \
    SP(-1); /* v2 */ \
    setorderres(res, cond, 1); }


/* order operation error with immediate operand */
#define op_orderI_error(C,v,imm) \
    { TValue v2; setival(&v2, imm); csD_ordererror(C, v, &v2); }


/* order operations with immediate operand */
#define op_orderI(C,iop,fop) { \
    int cond; \
    int imm = fetchl(); \
    imm = IMML(imm); \
    TValue *v = peek(0); \
    if (ttisint(v)) { \
        cond = iop(ival(v), imm); \
    } else if (ttisflt(v)) { \
        cs_Number n1 = fval(v); \
        cs_Number n2 = cast_num(imm); \
        cond = fop(n1, n2); \
    } else op_orderI_error(C, v, imm); \
    setorderres(v, cond, 1); }


/* -----------------------------------------------------------------------
 * Interpreter loop
 * ----------------------------------------------------------------------- */

/*
** Correct global 'base' (function stack).
*/
#define updatebase(cf)      (base = (cf)->func.p + 1)

/* 
** Store global 'pc'.
*/
#define storepc(C)          (cf->pc = pc)

/*
** Store global 'pc' into 'realpc'.
*/
#define storerealpc(C)      (cf->realpc = pc)


#define correcttop(C, cf)   ((C)->sp.p = (cf)->top.p)


/*
** Protect code that can raise errors or overwrite stack values.
*/
#define ProtectTop(exp) \
    { ptrdiff_t oldtop = savestack(C, C->sp.p); correcttop(C, cf); \
        (exp); C->sp.p = restorestack(C, oldtop); }


/* correct global 'pc' before checking collector debt */
#define checkGC(C)     csG_condGC(C, (void)0, (void)0)


/*
** Fetch instruction.
*/
#if TRACE_EXEC
#include "ctrace.h"
#define fetch() \
        check_exp(SIZE_INSTR == sizeof(Instruction), \
                  (csTR_tracepc(C, cl->p, pc), *pc++))
#else
#define fetch()     check_exp(SIZE_INSTR == sizeof(Instruction), *pc++)
#endif

/*
** Fetch short instruction argument.
*/
#define fetchs()    check_exp(SIZE_ARG_S == SIZE_INSTR, *pc++)

/*
** Fetch long instruction argument.
*/
#define fetchl() \
        check_exp(SIZE_ARG_L == sizeof(Instruction[3]), \
                  (cast_void(pc+=SIZE_ARG_L), get3bytes(pc-SIZE_ARG_L)))


/* get constant at index 'idx' */
#define K(idx)          (k + (idx))


/* get sign value */
#define getsign()       (fetchs() - 1)


/* peek stack value */
#define peek(n)         s2v((C->sp.p-1)-n)


/* get stack slot (starting from 'base') */
#define STK(i_)         (base+(i_))

/* get stack slot (starting from top) */
#define SLOT(i_)        (C->sp.p-(i_)-1)

/* get top stack slot */
#define TOP()           SLOT(0)

/* mutate stack pointer by 'n' slots */
#define SP(n)           check_exp(C->sp.p+(n) >= base, C->sp.p += (n))


/* In cases where jump table is not available or prefered. */
#define vm_dispatch(x)      switch(x)
#define vm_case(l)          case l:
#define vm_break            break


void csV_execute(cs_State *C, CallFrame *cf) {
    register const Instruction *pc; /* program counter */
    register CSClosure *cl; /* closure being executed */
    register TValue *k; /* array of constants */
    register SPtr base; /* function base stack index */
#if CS_USE_JUMPTABLE
#include "cjmptable.h"
#endif
startfunc:
#if TRACE_EXEC
    #include <stdio.h>
    printf(">> Executing new closure...\n");
#endif
returning:
    cl = clCSval(s2v(cf->func.p));
    k = cl->p->k;
    pc = cf->realpc;
    base = cf->func.p + 1;
    for (;;) {
        vm_dispatch(fetch()) {
            vm_case(OP_TRUE) {
                setbtval(s2v(C->sp.p));
                SP(1);
                vm_break;
            }
            vm_case(OP_FALSE) {
                setbfval(s2v(C->sp.p));
                SP(1);
                vm_break;
            }
            vm_case(OP_NIL) {
                setnilval(s2v(C->sp.p));
                SP(1);
                vm_break;
            }
            vm_case(OP_SUPER) {
                OClass *scl = insval(peek(0))->oclass->sclass;
                cs_assert(scl != NULL);
                setclsval2s(C, C->sp.p - 1, scl);
                vm_break;
            }
            vm_case(OP_NILN) {
                int n = fetchl();
                SPtr p = C->sp.p;
                SP(n);
                while (n--) setnilval(s2v(p++));
                vm_break;
            }
            vm_case(OP_LOAD) {
                setobjs2s(C, C->sp.p, STK(fetchl()));
                SP(1);
                vm_break;
            }
            vm_case(OP_CONST) {
                TValue *sk = K(fetchs());
                setobj2s(C, C->sp.p, sk);
                SP(1);
                vm_break;
            }
            vm_case(OP_CONSTL) {
                TValue *lk = K(fetchl());
                setobj2s(C, C->sp.p, lk);
                SP(1);
                vm_break;
            }
            vm_case(OP_CONSTI) {
                int imm = fetchs();
                setival(s2v(C->sp.p), IMM(imm));
                SP(1);
                vm_break;
            }
            vm_case(OP_CONSTIL) {
                int imm = fetchl();
                setival(s2v(C->sp.p), IMML(imm));
                SP(1);
                vm_break;
            }
            vm_case(OP_CONSTF) {
                int imm = fetchs();
                setfval(s2v(C->sp.p), cast_num(IMM(imm)));
                SP(1);
                vm_break;
            }
            vm_case(OP_CONSTFL) {
                int imm = fetchl();
                setfval(s2v(C->sp.p), cast_num(IMML(imm)));
                SP(1);
                vm_break;
            }
            vm_case(OP_VARARGPREP) {
                int arity;
                storepc(C);
                arity = fetchl();
                csF_adjustvarargs(C, arity, cf, cl->p);
                updatebase(cf); /* update base (it changed) */
                vm_break;
            }
            vm_case(OP_VARARG) {
                int n; /* num of varargs wanted */
                storepc(C);
                n = fetchl() - 1;
                csF_getvarargs(C, cf, n);
                vm_break;
            }
            vm_case(OP_CLOSURE) {
                int findex;
                Proto *fn;
                storepc(C);
                findex = fetchl();
                fn = cl->p->p[findex];
                pushclosure(C, fn, cl->upvals, base);
                checkGC(C);
                vm_break;
            }
            vm_case(OP_NEWLIST) {
                List *l;
                int b;
                storepc(C);
                b = fetchs();
                if (b > 0) /* list has elements? */
                    b = 1 << (b - 1); /* size is 2^(b - 1) */
                SP(1);
                l = csA_new(C); /* memory allocation */
                setlistval2s(C, TOP(), l);
                if (b != 0) /* list is not empty? */
                    csA_ensure(C, l, b);
                checkGC(C);
                vm_break;
            }
            vm_case(OP_NEWCLASS) { 
                OClass *cls;
                int b;
                storepc(C);
                /* TODO: set 7th bit of 'b' if class has metamethods */
                b = fetchs();
                if (b > 0) /* class has methods? */
                    b = 1 << (b - 1); /* size of methods table is 2^(b - 1) */
                cls = csMM_newclass(C);
                setclsval2s(C, C->sp.p, cls); /* push on stack */
                SP(1);
                if (b > 0) /* have methods? */
                    cls->methods = csH_newsz(C, b);
                checkGC(C);
                vm_break;
            }
            vm_case(OP_NEWTABLE) {
                Table *t;
                int b;
                storepc(C);
                b = fetchs();
                if (b > 0) /* table has fields? */
                    b = 1 << (b - 1); /* size is 2^(b - 1) */
                SP(1);
                t = csH_new(C);
                settval2s(C, TOP(), t);
                if (b != 0) /* table is not empty? */
                    csH_resize(C, t, b); /* grow table to size 'b' */
                checkGC(C);
                vm_break;
            }
            vm_case(OP_METHOD) {
                Table *t = classval(peek(1))->methods;
                TValue *f = peek(0);
                TValue *key;
                storepc(C);
                key = K(fetchl());
                cs_assert(t && ttisstring(key));
                csV_settable(C, t, key, f);
                SP(-1); /* f */
                vm_break;
            }
            vm_case(OP_SETMM) {
                TValue *o = peek(1); /* class */
                TValue *f = peek(0); /* func */
                List *ml = classval(o)->metalist;
                cs_MM mm; /* metamethod tag */
                storepc(C);
                mm = fetchs();
                cs_assert(0 <= mm && mm < CS_MM_N);
                /* TODO: remove this check (see OP_NEWCLASS) */
                if (c_unlikely(!ml)) { /* no metalist? */
                    ml = csA_newmetalist(C);
                    classval(o)->metalist = ml;
                    checkGC(C);
                }
                cs_assert(ml && mm < ml->n);
                ml->b[mm] = *f; /* set the entry */
                csV_finishrawset(C, ml, f);
                SP(-1); /* f */
                vm_break;
            }
            vm_case(OP_POP) {
                SP(-1);
                vm_break;
            }
            vm_case(OP_POPN) {
                int n = fetchl();
                SP(-n);
                vm_break;
            }
            /* } BINARY_OPS { ARITHMETIC_OPS { */
            vm_case(OP_MBIN) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                cs_MM mm;
                storepc(C);
                mm = fetchs();
                if (mm & 0x80) { /* swap operands? */
                    TValue *temp = v1;
                    v1 = v2;
                    v2 = temp;
                }
                precallmbin(C, v1, v2, mm&0x7f, SLOT(1));
                SP(-1); /* v2 */
                vm_break;
            }
            vm_case(OP_ADDK) {
                storepc(C);
                op_arithK(C, iadd, c_numadd);
                vm_break;
            }
            vm_case(OP_SUBK) {
                storepc(C);
                op_arithK(C, isub, c_numsub);
                vm_break;
            }
            vm_case(OP_MULK) {
                storepc(C);
                op_arithK(C, imul, c_nummul);
                vm_break;
            }
            vm_case(OP_DIVK) {
                storepc(C);
                op_arithKf(C, c_numdiv);
                vm_break;
            }
            vm_case(OP_IDIVK) {
                storepc(C);
                op_arithK(C, csV_divi, c_numidiv);
                vm_break;
            }
            vm_case(OP_MODK) {
                storepc(C); /* in case of division by 0 */
                op_arithK(C, csV_modi, csV_modf);
                vm_break;
            }
            vm_case(OP_POWK) {
                storepc(C);
                op_arithKf(C, c_numpow);
                vm_break;
            }
            vm_case(OP_BSHLK) {
                storepc(C);
                op_bitwiseK(C, csO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHRK) {
                storepc(C);
                op_bitwiseK(C, csO_shiftr);
                vm_break;
            }
            vm_case(OP_BANDK) {
                storepc(C);
                op_bitwiseK(C, iband);
                vm_break;
            }
            vm_case(OP_BORK) {
                storepc(C);
                op_bitwiseK(C, ibor);
                vm_break;
            }
            vm_case(OP_BXORK) {
                storepc(C);
                op_bitwiseK(C, ibxor);
                vm_break;
            }
            vm_case(OP_ADDI) {
                storepc(C);
                op_arithI(C, iadd, c_numadd);
                vm_break;
            }
            vm_case(OP_SUBI) {
                storepc(C);
                op_arithI(C, isub, c_numsub);
                vm_break;
            }
            vm_case(OP_MULI) {
                storepc(C);
                op_arithI(C, imul, c_nummul);
                vm_break;
            }
            vm_case(OP_DIVI) {
                storepc(C);
                op_arithIf(C, c_numdiv);
                vm_break;
            }
            vm_case(OP_IDIVI) {
                storepc(C);
                op_arithI(C, csV_divi, c_numidiv);
                vm_break;
            }
            vm_case(OP_MODI) {
                storepc(C);
                op_arithI(C, csV_modi, csV_modf);
                vm_break;
            }
            vm_case(OP_POWI) {
                storepc(C);
                op_arithIf(C, c_numpow);
                vm_break;
            }
            vm_case(OP_BSHLI) {
                storepc(C);
                op_bitwiseI(C, csO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHRI) {
                storepc(C);
                op_bitwiseI(C, csO_shiftr);
                vm_break;
            }
            vm_case(OP_BANDI) {
                storepc(C);
                op_bitwiseI(C, iband);
                vm_break;
            }
            vm_case(OP_BORI) {
                storepc(C);
                op_bitwiseI(C, ibor);
                vm_break;
            }
            vm_case(OP_BXORI) {
                storepc(C);
                op_bitwiseI(C, ibxor);
                vm_break;
            }
            vm_case(OP_ADD) {
                storepc(C);
                op_arith(C, iadd, c_numadd);
                vm_break;
            }
            vm_case(OP_SUB) {
                storepc(C);
                op_arith(C, isub, c_numsub);
                vm_break;
            }
            vm_case(OP_MUL) {
                storepc(C);
                op_arith(C, imul, c_nummul);
                vm_break;
            }
            vm_case(OP_DIV) {
                storepc(C);
                op_arithf(C, c_numdiv);
                vm_break;
            }
            vm_case(OP_IDIV) {
                storepc(C);
                op_arith(C, csV_divi, c_numidiv);
                vm_break;
            }
            vm_case(OP_MOD) {
                storepc(C);
                op_arith(C, csV_modi, csV_modf);
                vm_break;
            }
            vm_case(OP_POW) {
                storepc(C);
                op_arithf(C, c_numpow);
                vm_break;
            }
            vm_case(OP_BSHL) {
                storepc(C);
                op_bitwise(C, csO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHR) {
                storepc(C);
                op_bitwise(C, csO_shiftr);
                vm_break;
            }
            vm_case(OP_BAND) {
                storepc(C);
                op_bitwise(C, iband);
                vm_break;
            }
            vm_case(OP_BOR) {
                storepc(C);
                op_bitwise(C, ibor);
                vm_break;
            }
            vm_case(OP_BXOR) {
                storepc(C);
                op_bitwise(C, ibxor);
                vm_break;
            }
            /* } CONCAT_OP { */
            vm_case(OP_CONCAT) {
                int n;
                storepc(C);
                n = fetchl();
                csV_concat(C, n); /* 'csV_concat handles 'sp' */
                checkGC(C);
                vm_break;
            }
            /* } ORDERING_OPS { */
            vm_case(OP_EQK) {
                TValue *v1 = peek(0);
                const TValue *vk = K(fetchl());
                int eq = fetchs();
                int cond = csV_raweq(v1, vk);
                setorderres(v1, cond, eq);
                vm_break;
            }
            vm_case(OP_EQI) {
                TValue *v1 = peek(0);
                int imm = fetchl();
                int eq = fetchs();
                int cond;
                if (ttisint(v1))
                    cond = (ival(v1) == IMML(imm));
                else if (ttisflt(v1))
                    cond = c_numeq(fval(v1), IMML(imm));
                else
                    cond = 0;
                setorderres(v1, cond, eq);
                vm_break;
            }
            vm_case(OP_LTI) {
                storepc(C);
                op_orderI(C, ilt, c_numlt);
                vm_break;
            }
            vm_case(OP_LEI) {
                storepc(C);
                op_orderI(C, ile, c_numle);
                vm_break;
            }
            vm_case(OP_GTI) {
                storepc(C);
                op_orderI(C, igt, c_numgt);
                vm_break;
            }
            vm_case(OP_GEI) {
                storepc(C);
                op_orderI(C, ige, c_numge);
                vm_break;
            }
            vm_case(OP_EQ) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                int condexp; /* iseq */
                int cond;
                storepc(C);
                condexp = fetchs();
                cond = csV_ordereq(C, v1, v2);
                setorderres(v1, cond, condexp);
                SP(-1); /* v2 */
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
                SPtr res = TOP();
                TValue *v1 = peek(1);
                TValue *v2 = s2v(res);
                int cond;
                storepc(C);
                cond = csV_ordereq(C, v1, v2);
                setorderres(v2, cond, 1);
                vm_break;
            }
            /* }} UNARY_OPS { */
            vm_case(OP_NOT) {
                TValue *v = peek(0);
                if (c_isfalse(v))
                    setbtval(v);
                else
                    setbfval(v);
                vm_break;
            }
            vm_case(OP_UNM) {
                TValue *v = peek(0);
                if (ttisint(v)) {
                    cs_Integer ib = ival(v);
                    setival(v, c_intop(-, 0, ib));
                } else if (ttisflt(v)) {
                    cs_Number n = fval(v);
                    setfval(v, c_numunm(C, n));
                } else {
                    storepc(C);
                    csMM_tryunary(C, v, CS_MM_UNM);
                }
                vm_break;
            }
            vm_case(OP_BNOT) {
                SPtr res = TOP();
                TValue *v = s2v(res);
                if (ttisint(v)) {
                    cs_Integer i = ival(v);
                    setival(v, c_intop(^, ~c_castS2U(0), i));
                } else {
                    storepc(C);
                    csMM_tryunary(C, v, CS_MM_BNOT);
                }
                vm_break;
            }
            /* } JMP_OPS { */
            vm_case(OP_JMP) {
                int off = fetchl();
                pc += off;
                vm_break;
            }
            vm_case(OP_JMPS) {
                int off = fetchl();
                pc -= off;
                vm_break;
            }
            vm_case(OP_BJMP) {
                int off = fetchl();
                int npop = fetchl();
                pc += off;
                SP(-npop);
                vm_break;
            } /* } TEST_OPS { */
            vm_case(OP_TEST) {
                TValue *v = peek(0);
                int off = fetchl();
                int cond = fetchs();
                if ((!c_isfalse(v)) == cond)
                    pc += off;
                vm_break;
            }
            vm_case(OP_TESTORPOP) {
                TValue *v = peek(0);
                int off = fetchl();
                int cond = fetchs();
                if ((!c_isfalse(v)) == cond)
                    pc += off;
                else
                    SP(-1); /* v */
                vm_break;
            }
            vm_case(OP_TESTPOP) {
                TValue *v = peek(0);
                int off = fetchl();
                int cond = fetchs();
                if ((!c_isfalse(v)) == cond)
                    pc += off;
                SP(-1); /* v */
                vm_break;
            } /* } */
            vm_case(OP_CALL) {
                CallFrame *newcf;
                SPtr func;
                int nres;
                storepc(C);
                func = STK(fetchl());
                nres = fetchl() - 1;
                storerealpc(C); /* will continue from 'realpc' */
                if ((newcf = precall(C, func, nres)) != NULL) {
                    cf = newcf;
                    goto startfunc;
                } /* else call is already done (not a CScript closure) */
                vm_break;
            }
            vm_case(OP_CLOSE) {
                SPtr level;
                storepc(C);
                level = STK(fetchl());
                ProtectTop(csF_close(C, level, CS_OK));
                vm_break;
            }
            vm_case(OP_TBC) {
                SPtr level;
                storepc(C);
                level = STK(fetchl());
                csF_newtbcvar(C, level);
                vm_break;
            }
            vm_case(OP_GETGLOBAL) {
                TValue *key = K(fetchl());
                Table *G = tval(getGtable(C));
                const TValue *val = csH_getstr(G, strval(key));
                if (!isempty(val)) {
                    setobj2s(C, C->sp.p, val);
                } else
                    setnilval(s2v(C->sp.p));
                SP(1);
                vm_break;
            }
            vm_case(OP_SETGLOBAL) {
                Table *G = tval(getGtable(C));
                TValue *v = peek(0);
                TValue *key;
                storepc(C);
                key = K(fetchl());
                cs_assert(ttisstring(key));
                csV_settable(C, G, key, v);
                SP(-1); /* v */
                vm_break;
            }
            vm_case(OP_GETLOCAL) {
                SPtr l = STK(fetchl());
                setobjs2s(C, C->sp.p, l);
                SP(1);
                vm_break;
            }
            vm_case(OP_SETLOCAL) {
                SPtr l = STK(fetchl());
                setobjs2s(C, l, C->sp.p - 1);
                SP(-1);
                vm_break;
            }
            vm_case(OP_GETUVAL) {
                UpVal *uv = cl->upvals[fetchl()];
                setobj2s(C, C->sp.p, uv->v.p);
                SP(1);
                vm_break;
            }
            vm_case(OP_SETUVAL) {
                UpVal *uv = cl->upvals[fetchl()];
                setobj(C, uv->v.p, s2v(C->sp.p - 1));
                csG_barrier(C, uv, s2v(C->sp.p - 1));
                SP(-1);
                vm_break;
            }
            vm_case(OP_SETLIST) {
                List *l;
                SPtr sl;
                uint last;
                int n;
                storepc(C);
                sl = STK(fetchl()); /* list stack slot */
                l = listval(s2v(sl)); /* 'sl' as list value */
                last = fetchl(); /* num of elems. already in the list */
                n = fetchs(); /* num of elements to store */
                if (n == 0)
                    n = (C->sp.p - sl) - 1; /* get up to the top */
                cs_assert(n > 0);
                last += n - 1;
                csA_ensureindex(C, l, last);
                for (; n > 0; n--) { /* set the values from the stack... */
                    /* ...into the list (in reverse order) */
                    TValue *v = s2v(sl + n);
                    setobj(C, &l->b[last--], v);
                    csV_finishrawset(C, l, v);
                }
                C->sp.p = sl + 1; /* pop off elements */
                vm_break;
            }
            vm_case(OP_SETPROPERTY) { /* NOTE: optimize? */
                TValue *v = peek(0);
                TValue *o;
                TValue *prop;
                storepc(C);
                o = peek(fetchl());
                prop = K(fetchl());
                cs_assert(ttisstring(prop));
                csV_set(C, o, prop, v);
                SP(-1); /* v */
                vm_break;
            }
            vm_case(OP_GETPROPERTY) { /* NOTE: optimize? */
                TValue *v = peek(0);
                TValue *prop;
                storepc(C);
                prop = K(fetchl());
                cs_assert(ttisstring(prop));
                csV_get(C, v, prop, TOP());
                vm_break;
            }
            vm_case(OP_GETINDEX) {
                TValue *o = peek(1);
                TValue *key = peek(0);
                storepc(C);
                csV_get(C, o, key, SLOT(1));
                SP(-1); /* v2 */
                vm_break;
            }
            vm_case(OP_SETINDEX) {
                TValue *v = peek(0);
                SPtr os;
                TValue *o;
                TValue *idx;
                storepc(C);
                os = SLOT(fetchl());
                o = s2v(os);
                idx = s2v(os + 1);
                csV_set(C, o, idx, v);
                SP(-1); /* v */
                vm_break;
            }
            vm_case(OP_GETINDEXSTR) {
                TValue *v = peek(0);
                TValue *i;
                storepc(C);
                i = K(fetchl());
                cs_assert(ttisstring(i));
                csV_get(C, v, i, TOP());
                vm_break;
            }
            vm_case(OP_SETINDEXSTR) {
                TValue *v = peek(0);
                TValue *o;
                TValue *idx;
                storepc(C);
                o = peek(fetchl());
                idx = K(fetchl());
                cs_assert(ttisstring(idx));
                csV_set(C, o, idx, v);
                SP(-1); /* v */
                vm_break;
            }
            vm_case(OP_GETINDEXINT) {
                TValue *v = peek(0);
                TValue i;
                int imm;
                storepc(C);
                imm = fetchs();
                setival(&i, IMM(imm));
                csV_get(C, v, &i, TOP());
                vm_break;
            }
            vm_case(OP_GETINDEXINTL) {
                TValue *v = peek(0);
                TValue i;
                int imm;
                storepc(C);
                imm = fetchl();
                setival(&i, IMML(imm));
                csV_get(C, v, &i, TOP());
                vm_break;
            }
            vm_case(OP_SETINDEXINT) {
                TValue *v = peek(0);
                TValue *o;
                cs_Integer imm;
                TValue index;
                storepc(C);
                o = peek(fetchl());
                imm = fetchs();
                setival(&index, IMM(imm));
                csV_set(C, o, &index, v);
                SP(-1); /* v */
                vm_break;
            }
            vm_case(OP_SETINDEXINTL) {
                TValue *v = peek(0);
                TValue *o;
                cs_Integer imm;
                TValue index;
                storepc(C);
                o = peek(fetchl());
                imm = fetchl();
                setival(&index, IMML(imm));
                csV_set(C, o, &index, v);
                SP(-1); /* v */
                vm_break;
            }
            vm_case(OP_GETSUP) {
                Instance *in = insval(peek(0));
                OClass *scl = in->oclass->sclass;
                TValue *sk;
                storepc(C);
                cs_assert(scl != NULL);
                sk = K(fetchl());
                getsuper(C, in, scl, strval(sk), SLOT(0), csH_getstr);
                vm_break;
            }
            vm_case(OP_GETSUPIDX) {
                Instance *in = insval(peek(1));
                OClass *scl = in->oclass->sclass;;
                TValue *idx = peek(0);
                storepc(C);
                cs_assert(scl != NULL);
                getsuper(C, in, scl, idx, SLOT(1), csH_get);
                SP(-1); /* idx */
                vm_break;
            }
            vm_case(OP_GETSUPIDXSTR) {
                Instance *in = insval(peek(0));
                OClass *scl = in->oclass->sclass;;
                TValue *sk;
                storepc(C);
                cs_assert(scl != NULL);
                sk = K(fetchl());
                getsuper(C, in, scl, strval(sk), SLOT(0), csH_getstr);
                vm_break;
            }
            vm_case(OP_INHERIT) {
                TValue *o1 = peek(1); /* superclass */
                TValue *o2 = peek(0); /* class */
                OClass *cls = classval(o2);
                OClass *scl;
                storepc(C);
                cs_assert(!csV_raweq(o1, o2));
                scl = checksuper(C, o1);
                csV_inherit(C, cls, scl);
                checkGC(C);
                vm_break;
            }
            vm_case(OP_FORPREP) {
                SPtr stk; /* slot of iterator function */
                int off; /* offset that skips loop body */
                storepc(C);
                stk = STK(fetchl());
                off = fetchl();
                /* create to-be-closed upvalue (if any) */
                csF_newtbcvar(C, stk+FORTBCVAR);
                pc += off;
                check_exp(*pc == OP_FORCALL, cast_void(fetch())); /* skip */
                goto l_forcall;
            }
            vm_case(OP_FORCALL) {
            l_forcall: {
                SPtr stk;
                int nres;
                storepc(C);
                stk = STK(fetchl());
                nres = fetchl();
                /* 'stk' slot is iterator function, 'stk + 1' is the
                 * invariant state 'stk + 2' is the control variable, and
                 * 'stk + 3' is the to-be-closed variable. Call uses stack
                 * after these values (starting at 'stk + 4'). */
                memcpy(stk+NSTATEVARS, stk, FORTBCVAR*sizeof(*stk));
                C->sp.p = stk+NSTATEVARS+FORTBCVAR; /* adjust stack pointer */
                csV_call(C, stk+NSTATEVARS, nres); /* call iter */
                updatebase(cf);
                check_exp(*pc == OP_FORLOOP, cast_void(fetch())); /* skip */
                goto l_forloop;
            }}
            vm_case(OP_FORLOOP) {
            l_forloop: {
                SPtr stk;
                int off;
                int nvars;
                stk = STK(fetchl());
                off = fetchl();
                nvars = fetchl();
                if (!ttisnil(s2v(stk + NSTATEVARS))) { /* result is not nil? */
                    /* save control variable */
                    setobjs2s(C, stk+FORCNTLVAR, stk+NSTATEVARS);
                    pc -= off; /* jump back to loop body */
                } else /* otherwise leave the loop (fall through) */
                    SP(-nvars); /* remove leftover vars from previous call */
                vm_break;
            }}
            vm_case(OP_RET) {
                SPtr stk;
                int nres; /* number of results */
                storepc(C);
                stk = STK(fetchl());
                nres = fetchl() - 1;
                if (nres < 0) /* not fixed ? */
                    nres = C->sp.p - stk;
                if (fetchs()) { /* have open upvalues? */
                    csF_close(C, base, CLOSEKTOP);
                    updatebase(cf);
                }
                if (cl->p->isvararg) /* vararg function ? */
                    cf->func.p -= cf->nvarargs + cl->p->arity + 1;
                C->sp.p = stk + nres;
                poscall(C, cf, nres);
                if (cf->status & CFST_FRESH) { /* top-level function? */
#if TRACE_EXEC
                    printf(">> Returning from main...\n");
#endif
                    return; /* end this frame */
                } else {
#if TRACE_EXEC
                    printf(">> Returning...\n");
#endif
                    cf = cf->prev; /* return to caller */
                    goto returning; /* continue running in this frame */
                }
            }
        }
    }
}
