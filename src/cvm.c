/*
** cvm.c
** CScript Virtual Machine
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include <string.h>

#include "capi.h"
#include "carray.h"
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
cs_Integer csV_div(cs_State *C, cs_Integer x, cs_Integer y) {
    if (c_unlikely(c_castS2U(y) + 1 <= 1)) { /* 'y' == '0' or '-1' */
        if (y == 0)
            csD_runerror(C, "division by 0");
        return c_intop(-, 0, x);
    }
    return (x / y); /* C division */
}


/*
** Integer modulus; handles modulo by 0 and overflow
** as explained in 'csV_div()'.
*/
cs_Integer csV_modint(cs_State *C, cs_Integer x, cs_Integer y) {
    cs_Integer r;
    if (c_unlikely(c_castS2U(y) + 1 <= 1)) {
        if (y == 0)
            csD_runerror(C, "attempt to x%%0");
        return 0;
    }
    c_nummod(C, x, y, r);
    return r;
}


/* floating point modulus */
cs_Number csV_modnum(cs_State *C, cs_Number x, cs_Number y) {
    cs_Number r;
    c_nummod(C, x, y, r);
    return r;
}


/*
** Perform binary arithmetic operations on objects, this function is free
** to call overloaded methods in cases where raw arithmetics are not possible.
*/
void csV_binarithm(cs_State *C, const TValue *v1, const TValue *v2, SPtr res,
                   int op) {
    if (!csO_arithmraw(C, v1, v2, s2v(res), op))
        csMM_trybin(C, v1, v2, res, (op - CS_OPADD) + CS_MM_ADD);
}


/*
** Perform unary arithmetic operations on objects, this function is free
** to call overloaded methods in cases where raw arithmetics are not possible.
*/
void csV_unarithm(cs_State *C, const TValue *v, SPtr res, int op) {
    TValue aux;
    setival(&aux, 0);
    if (!csO_arithmraw(C, v, &aux, s2v(res), op))
        csMM_tryunary(C, v, res, (op - CS_OPUNM) + CS_MM_UNM);
}


/*
** According to C99 6.3.1.8 page 45:
** "...if the corresponding real type of either operand is double, the other
** operand is converted, without change of type domain, to a type whose
** corresponding real type is double."
*/
c_sinline int intlenum(const TValue *v1, const TValue *v2) {
    return c_numle(cast_num(ival(v1)), fval(v2));
}


/* check 'intLEnum' */
c_sinline int numleint(const TValue *v1, const TValue *v2) {
    return c_numle(fval(v1), cast_num(ival(v2)));
}


/* less equal ordering on numbers */
c_sinline int numle(const TValue *v1, const TValue *v2) {
    cs_assert(ttisnum(v1) && ttisnum(v2));
    if (ttisint(v1)) {
        cs_Integer i1 = ival(v1);
        if (ttisint(v2)) return (i1 <= ival(v2));
        else return intlenum(v1, v2);
    } else {
        cs_Number n1 = fval(v1);
        if (ttisint(v2)) return numleint(v1, v2);
        else return c_numle(n1, fval(v2));
    }
}


/* less equal ordering on non-number values */
c_sinline int otherle(cs_State *C, const TValue *v1, const TValue *v2) {
    if (ttisstring(v1) && ttisstring(v2))
        return (csS_cmp(strval(v1), strval(v2)) <= 0);
    else
        return csMM_order(C, v1, v2, CS_MM_LE);
}


/* 'less or equal' ordering '<=' */
int csV_orderle(cs_State *C, const TValue *v1, const TValue *v2) {
    if (ttisnum(v1) && ttisnum(v2))
        return numle(v1, v2);
    return otherle(C, v1, v2);
}


/* check 'intLEnum' */
c_sinline int intltnum(const TValue *v1, const TValue *v2) {
    return c_numlt(cast_num(ival(v1)), fval(v2));
}


/* check 'intLEnum' */
c_sinline int numltint(const TValue *v1, const TValue *v2) {
    return c_numlt(fval(v1), cast_num(ival(v2)));
}


/* 'less than' ordering '<' on number values */
c_sinline int numlt(const TValue *v1, const TValue *v2) {
    cs_assert(ttisnum(v1) && ttisnum(v2));
    if (ttisint(v1)) {
        cs_Integer i1 = ival(v1);
        if (ttisint(v2)) return (i1 <= ival(v2));
        else return intltnum(v1, v2);
    } else {
        cs_Number n1 = fval(v1);
        if (ttisint(v2)) return numltint(v1, v2);
        else return c_numlt(n1, fval(v2));
    }
}


/* 'less than' ordering '<' on non-number values */
c_sinline int otherlt(cs_State *C, const TValue *v1, const TValue *v2) {
    if (ttisstring(v1) && ttisstring(v2))
        return csS_cmp(strval(v1), strval(v2)) < 0;
    else
        return csMM_order(C, v1, v2, CS_MM_LT);
}


/* 'less than' ordering '<' */
int csV_orderlt(cs_State *C, const TValue *v1, const TValue *v2) {
    if (ttisnum(v1) && ttisnum(v2))
        return numlt(v1, v2);
    return otherlt(C, v1, v2);
}


/* 
** Equality ordering '=='.
** In case 'C' is NULL perform raw equality (without invoking '__eq').
*/
int csV_ordereq(cs_State *C, const TValue *v1, const TValue *v2) {
    cs_Integer i1, i2;
    const TValue *fmm;
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
        case CS_VUSERDATA: {
            if (uval(v1) == uval(v2)) return 1;
            else if (C == NULL) return 0;
            fmm = csMM_get(C, v1, CS_MM_EQ);
            if (ttisnil(fmm))
                fmm = csMM_get(C, v2, CS_MM_EQ);
            break;
        }
        case CS_VINSTANCE: {
            if (insval(v1) == insval(v2)) return 1;
            else if (C == NULL) return 0;
            fmm = csMM_get(C, v1, CS_MM_EQ);
            if (ttisnil(fmm))
                fmm = csMM_get(C, v2, CS_MM_EQ);
            break;
        }
        default: return gcoval(v1) == gcoval(v2);
    }
    if (ttisnil(fmm)) {
        return 0;
    } else {
        csMM_callbinres(C, fmm, v1, v2, C->sp.p);
        return !c_isfalse(s2v(C->sp.p));
    }
}


static void setarrayindex(cs_State *C, Array *arr, const TValue *index,
                          const TValue *val) {
    cs_Integer i;
    if (c_likely(tointeger(index, &i))) { /* index is integer? */
        if (c_likely(0 <= i)) { /* non-negative index? */
            if (c_unlikely(i >= ARRAYLIMIT)) /* too large 'index'? */
                csD_indexerror(C, i, "too large");
            csA_ensure(C, arr, i); /* expand array */
            setobj(C, &arr->b[i], val); /* set the value at index */
            csG_barrierback(C, obj2gco(arr), val);
        } else /* negative index, error */
            csD_indexerror(C, i, "negative");
    } else {
        csD_indextypeerror(C, index);
    }
}


void csV_rawset(cs_State *C, const TValue *obj, const TValue *key,
                const TValue *val) {
    switch (ttypetag(obj)) {
        case CS_VARRAY: {
            setarrayindex(C, arrval(obj), key, val);
            break;
        }
        case CS_VTABLE: {
            csH_set(C, tval(obj), key, val);
            csG_barrierback(C, gcoval(obj), val);
            break;
        }
        case CS_VINSTANCE: {
            csH_set(C, insval(obj)->fields, key, val);
            csG_barrierback(C, gcoval(obj), val);
            break;
        }
        default: {
            csD_typeerror(C, obj, "index");
            break;
        }
    }
}


void csV_set(cs_State *C, const TValue *obj, const TValue *key,
             const TValue *val) {
    const TValue *fmm = csMM_get(C, obj, CS_MM_SETIDX);
    if (!ttisnil(fmm)) { /* have metamethod? */
        csMM_callset(C, fmm, obj, key, val);
        return; /* done */
    } else /* otherwise perform raw set */
        csV_rawset(C, obj, key, val);
}


static void arraygeti(cs_State *C, Array *arr, const TValue *index, SPtr res) {
    cs_Integer i;
    if (c_likely(tointeger(index, &i))) { /* index is integer? */
        if (0 <= i) { /* positive index? */
            if (i < arr->sz) { /* index in bounds? */
                setobj2s(C, res, &arr->b[i]);
            } else /* index out of bounds */
                setnilval(s2v(res));
        } else /* negative index */
            csD_indexerror(C, i, "negative");
    } else /* invalid index */
        csD_indextypeerror(C, index);
}


/* bind method to instance and set it at 'res' */
#define bindmethod(C,ins,fn,res) \
        setimval2s(C, res, csMM_newinsmethod(C, ins, fn))


void csV_rawget(cs_State *C, const TValue *obj, const TValue *key, SPtr res) {
    switch (ttypetag(obj)) {
        case CS_VARRAY: {
            arraygeti(C, arrval(obj), key, res);
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
            Instance *ins = insval(obj);
            const TValue *slot = csH_get(ins->fields, key);
            if (isempty(slot) && ins->oclass->methods) {
                /* try methods table */
                slot = csH_get(ins->oclass->methods, key);
                if (!isempty(slot)) { /* have method? */
                    setobj2s(C, res, slot);
                    bindmethod(C, ins, slot, res);
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


void csV_get(cs_State *C, const TValue *obj, const TValue *key, SPtr res) {
    const TValue *fmm = csMM_get(C, obj, CS_MM_GETIDX);
    if (!ttisnil(fmm)) { /* have metamethod ? */
        csMM_callgetres(C, fmm, obj, key, res);
    } else /* otherwise perform raw get */
        csV_rawget(C, obj, key, res);
}


#define checkmethods(cls,res) \
        (!(cls)->methods ? (setnilval(s2v(res)), 0) : 1)


#define getsuper(C,ins,cls,k,res,fget) \
    { if ((cls)->methods) { \
        const TValue *f = fget((cls)->methods, k); \
        if (!isempty(f)) { bindmethod(C, ins, f, res); }} \
        else setnilval(s2v(res)); }


/*
** Call binary meta method, but before that perform a quick check
** and invoke error if types don't match, the values are instances
** that belong to different classes or v1 (self) doesn't have the
** overloaded method.
*/
c_sinline void precallmbin(cs_State *C, const TValue *v1, const TValue *v2,
                            cs_MM op, SPtr res) {
    const TValue *func;
    const char *opname = getshrstr(G(C)->mmnames[op]);
    if (c_unlikely(ttypetag(v1) != ttypetag(v2)))
        csD_typeerrormeta(C, v1, v2, opname);
    if (c_unlikely(ttisinstance(v1) && insval(v1)->oclass != insval(v2)->oclass))
        csD_runerror(C, "tried to %s instances of different class", opname);
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
** Adjust stack for meta method call.
** func is the object that has metamethod and f is the metamethod.
** Stack is shifted so that the f is in place of func and func is its
** argument at func + 1. This function assumes there is enough space
** on the stack for f.
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
            cf->pc = p->code; /* set starting point */
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
            Instance *ins = csMM_newinstance(C, classval(s2v(func)));
            csG_checkfin(C, obj2gco(ins), ins->oclass->vmt);
            setinsval2s(C, func, ins); /* replace class with its instance */
            fmm = csMM_get(C, s2v(func), CS_MM_INIT);
            if (!ttisnil(fmm)) { /* have __init ? */
                checkstackGCp(C, 1, func); /* space for fmm */
                fmm = csMM_get(C, s2v(func), CS_MM_INIT); /* (after GC) */
                if (c_likely(!ttisnil(fmm))) { /* have __init (after GC)? */
                    auxinsertf(C, func, fmm); /* insert it into stack... */
                    cs_assert(ttisfunction(fmm));
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
            cs_assert(ttisfunction(&im->method));
            checkstackGCp(C, 1, func); /* space for method */
            auxinsertf(C, func, &im->method); /* insert method... */
            setinsval2s(C, func + 1, im->ins); /* ...and replace func */
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
    } else { \
        savepc(C); \
        csD_aritherror(C, v1, v2); \
    }}


/* arithmetic operations with constant operand for floats */
#define op_arithKf(C,fop) { \
    TValue *v = peek(0); \
    TValue *lk = K(fetchl()); \
    op_arithKf_aux(C, v, lk, fop); }


/* arithmetic operations with constant operand */
#define op_arithK(C,iop,fop) { \
    TValue *v = peek(0); \
    TValue *lk = K(fetchl()); \
    if (ttisint(v) && ttisint(lk)) { \
        cs_Integer i1 = ival(v); \
        cs_Integer i2 = ival(lk); \
        setival(v, iop(C, i1, i2)); \
    } else { \
        op_arithKf_aux(C, v, lk, fop); \
    }}


/* arithmetic operation error with immediate operand */
#define op_arithI_error(C,v,imm) \
    { TValue v2; setival(&v2, imm); \
      savepc(C); csD_aritherror(C, v, &v2); }


/* arithmetic operations with immediate operand for floats */
#define op_arithIf(C,fop) { \
    TValue *v = peek(0); \
    int imm = fetchl(); /* L */\
    imm *= getsign(); /* S */\
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
    int imm = fetchl(); /* L */\
    imm *= getsign(); /* S */\
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


#define op_arithf_aux(C,v1,v2,fop) { \
    cs_Number n1; cs_Number n2; \
    if (tonumber(v1, n1) && tonumber(v2, n2)) { \
        setfval(v1, fop(C, n1, n2)); \
        SP(-1); /* v2 */ \
        pc += getOpSize(OP_MBIN); \
    }/* else fall through to 'OP_MBIN' */}


/* arithmetic operations with stack operands for floats */
#define op_arithf(C,fop) { \
    TValue *v1 = peek(1); \
    TValue *v2 = peek(0); \
    op_arithf_aux(C, v1, v2, fop); }


/* arithmetic operations with stack operands */
#define op_arith(C,iop,fop) { \
    TValue *v1 = peek(1); \
    TValue *v2 = peek(0); \
    if (ttisint(v1) && ttisint(v2)) { \
        cs_Integer i1 = ival(v1); cs_Integer i2 = ival(v2); \
        setival(v1, iop(C, i1, i2)); \
        SP(-1); /* v2 */ \
        pc += getOpSize(OP_MBIN); \
    } else { \
        op_arithf_aux(C, v1, v2, fop); \
    }}



/*
** Bitwise operations
*/

/* bitwise operations with constant operand */
#define op_bitwiseK(C,op) { \
    TValue *v = peek(0); \
    TValue *lk = K(fetchl()); /* L */\
    cs_Integer i1; cs_Integer i2 = ival(lk);  \
    if (c_likely(tointeger(v, &i1))) { \
        setival(v, op(i1, i2)); \
    } else { \
        savepc(C); \
        csD_bitwerror(C, v, lk); \
    }}


/* bitwise operations with immediate operand */
#define op_bitwiseI(C,op) { \
    TValue *v = peek(0); \
    int imm = fetchl(); /* L */\
    imm *= getsign(); /* S */\
    cs_Integer i; \
    if (c_likely(tointeger(v, &i))) { \
        setival(v, op(i, imm)); \
    } else { \
        TValue vimm; setival(&vimm, imm); \
        savepc(C); \
        csD_bitwerror(C, v, &vimm); \
    }}


/* bitwise operations with stack operands */
#define op_bitwise(C,op) { \
    TValue *v1 = peek(1); \
    TValue *v2 = peek(0); \
    cs_Integer i1; cs_Integer i2; \
    if (tointeger(v1, &i1) && tointeger(v2, &i2)) { \
        setival(v1, op(i1, i2)); \
        SP(-1); /* v2 */ \
        pc += getOpSize(OP_MBIN); \
    }/* else try 'OP_MBIN' */}



/*
** Ordering operations
*/

/* set ordering result */
#define setorderres(v,cond,eq) \
    { cs_assert(0 <= cond && cond <= 1); settt(v, booleans[(cond) == (eq)]); }


/* order operations with stack operands */
#define op_order(C,iop,fop,other) { \
    TValue *v1 = peek(1); \
    TValue *v2 = peek(0); \
    int cond; \
    if (ttisint(v1) && ttisint(v2)) { \
        cs_Integer i1 = ival(v1); \
        cs_Integer i2 = ival(v2); \
        cond = iop(i1, i2); \
    } else if (ttisnum(v1) && ttisnum(v2)) { \
        cond = fop(v1, v2); \
    } else Protect(cond = other(C, v1, v2)); \
    SP(-1); /* v2 */ \
    setorderres(v1, cond, 1); }


/* order operations with immediate operand */
#define op_orderI(C,iop,fop) { \
    int imm = fetchl(); /* L */\
    imm *= getsign(); /* S */\
    int cond; \
    TValue *v = peek(0); \
    if (ttisint(v)) { \
        cond = iop(ival(v), imm); \
    } else if (ttisflt(v)) { \
        cs_Number n1 = fval(v); \
        cs_Number n2 = cast_num(imm); \
        cond = fop(n1, n2); \
    } else cond = 0; \
    setorderres(v, cond, 1); }


/* -----------------------------------------------------------------------
 * Interpreter loop
 * ----------------------------------------------------------------------- */

/*
** Correct global 'base' (function stack).
*/
#define updatebase(cf)      (base = (cf)->func.p + 1)

/* 
** Correct global 'pc'.
*/
#define savepc(C)          (cf->pc = pc)

/*
** Correct global 'pc' and set the stack pointer to point at the
** end of the call frame stack.
** Adjusting stack pointer is necessary when calling certain functions in
** order to avoid overwriting existing values on the current call frame
** stack (such as OP_CLOSE).
*/
#define savestate(C, cf)   (savepc(C), (C)->sp.p = (cf)->top.p)


/* 
** Protect code that can raise errors.
*/
#define Protect(exp)        (savepc(C), (exp))

/*
** Protect code that can raise errors or overwrite stack values.
*/
#define ProtectTop(exp) \
    { ptrdiff_t oldtop = savestack(C, C->sp.p); savestate(C, cf); \
        (exp); C->sp.p = restorestack(C, oldtop); }


/* correct global 'pc' before checking collector debt */
#define checkGC(C)     csG_condGC(C, savepc(C), (void)0)


#if TRACE_EXEC
#include "ctrace.h"
#define fetch()         (csTR_tracepc(C, cl->p, pc), *pc++)
#else
#define fetch()         (*pc++)
#endif

/* fetch short instruction argument */
#define fetchs()        (*pc++)

/* fetch long instruction argument */
#define fetchl()        (cast_void(pc += SIZEARGL), get3bytes(pc - SIZEARGL))


/* get constant at index 'idx' */
#define K(idx)          (k + (idx))


/* get sign value */
#define getsign()       (fetchs() - 1)


/* peek stack value */
#define peek(n)         s2v((C->sp.p-1)-n)


/* get stack slot (starting from 'base') */
#define STK(i)          (base+(i))

/* get stack slot (starting from top) */
#define SLOT(i)         (C->sp.p-(i)-1)

/* get top stack slot */
#define TOP()           SLOT(0)

/* increments/decrements stack pointer */
#define SP(n)           check_exp(C->sp.p + (n) >= base, C->sp.p += (n))


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
    pc = cf->pc;
    base = cf->func.p + 1;
    for (;;) {
        vm_dispatch(fetch()) {
            vm_case(OP_TRUE) {
                /* no args */
                setbtval(s2v(C->sp.p));
                SP(1);
                vm_break;
            }
            vm_case(OP_FALSE) {
                /* no args */
                setbfval(s2v(C->sp.p));
                SP(1);
                vm_break;
            }
            vm_case(OP_NIL) {
                /* no args */
                setnilval(s2v(C->sp.p));
                SP(1);
                vm_break;
            }
            vm_case(OP_NILN) {
                int n = fetchl();
                SPtr p = C->sp.p;
                SP(n);
                while (n--) setnilval(s2v(p++));
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
                int imm_i = fetchl();
                int sign = getsign();
                setival(s2v(C->sp.p), imm_i*sign);
                SP(1);
                vm_break;
            }
            vm_case(OP_CONSTF) {
                int imm_f = fetchl();
                int sign = getsign();
                setfval(s2v(C->sp.p), cast_num(imm_f*sign));
                SP(1);
                vm_break;
            }
            vm_case(OP_VARARGPREP) {
                int arity = fetchl();
                Protect(csF_adjustvarargs(C, arity, cf, cl->p));
                updatebase(cf); /* update base (it changed) */
                vm_break;
            }
            vm_case(OP_VARARG) {
                int n = fetchl() - 1; /* num of varargs wanted */
                Protect(csF_getvarargs(C, cf, n));
                vm_break;
            }
            vm_case(OP_CLOSURE) {
                int findex = fetchl();
                Proto *fn = cl->p->p[findex];
                Protect(pushclosure(C, fn, cl->upvals, base));
                checkGC(C);
                vm_break;
            }
            vm_case(OP_NEWARRAY) {
                int b = fetchs();
                Array *arr;
                if (b > 0) /* array has elements? */
                    b = 1 << (b - 1); /* size is 2^(b - 1) */
                SP(1);
                arr = csA_new(C); /* memory allocation */
                setarrval2s(C, TOP(), arr);
                if (b != 0) { /* array is not empty? */
                    csA_ensure(C, arr, b - 1); /* ensure arr[b-1] is valid */
                    csA_reset(arr); /* arr is empty (but can fit b-1 index) */
                }
                checkGC(C);
                vm_break;
            }
            vm_case(OP_NEWCLASS) {
                OClass *cls;
                int b = fetchs();
                if (b > 0) /* class has methods? */
                    b = 1 << (b - 1); /* size of methods table is 2^(b - 1) */
                savepc(C);
                cls = csMM_newclass(C);
                setclsval2s(C, C->sp.p, cls); /* push on stack */
                SP(1);
                if (b > 0)
                    cls->methods = csH_newsz(C, b);
                vm_break;
            }
            vm_case(OP_NEWTABLE) {
                int b = fetchs();
                Table *t;
                if (b > 0) /* table has fields? */
                    b = 1 << (b - 1); /* size is 2^(b - 1) */
                SP(1);
                savepc(C); /* allocations might fail */
                t = csH_new(C);
                settval2s(C, TOP(), t);
                if (b != 0) /* table is not empty? */
                    csH_resize(C, t, b); /* grow table to size 'b' */
                checkGC(C);
                vm_break;
            }
            vm_case(OP_METHOD) {
                TValue *cls = peek(1); /* class */
                TValue *f = peek(0); /* method */
                TValue *key = K(fetchl());
                cs_assert(ttisstring(key));
                cs_assert(classval(cls)->methods != NULL);
                Protect(csH_set(C, classval(cls)->methods, key, f));
                SP(-1); /* f */
                vm_break;
            }
            vm_case(OP_SETMM) {
                TValue *o = peek(1); /* class or userdata */
                TValue *f = peek(0); /* func */
                cs_MM mm = fetchs(); /* metamethod tag */
                TValue **vmt = &classval(o)->vmt; /* VMT */
                cs_assert(0 <= mm && mm < CS_MM_N);
                if (c_unlikely(!(*vmt))) { /* VMT is empty? */
                    savepc(C); /* allocation might fail */
                    *vmt = csMM_newvmt(C);
                }
                (*vmt)[mm] = *f; /* set the entry */
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
                int op = fetchs(); /* op */
                Protect(precallmbin(C, v1, v2, op, SLOT(1)));
                SP(-1); /* v2 */
                vm_break;
            }
            vm_case(OP_ADDK) {
                op_arithK(C, iadd, c_numadd);
                vm_break;
            }
            vm_case(OP_SUBK) {
                op_arithK(C, isub, c_numsub);
                vm_break;
            }
            vm_case(OP_MULK) {
                op_arithK(C, imul, c_nummul);
                vm_break;
            }
            vm_case(OP_DIVK) {
                op_arithKf(C, c_numdiv);
                vm_break;
            }
            vm_case(OP_MODK) {
                savepc(C); /* in case of division by 0 */
                op_arithK(C, csV_modint, csV_modnum);
                vm_break;
            }
            vm_case(OP_POWK) {
                op_arithKf(C, c_numpow);
                vm_break;
            }
            vm_case(OP_BSHLK) {
                op_bitwiseK(C, csO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHRK) {
                op_bitwiseK(C, csO_shiftr);
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
                op_arithI(C, iadd, c_numadd);
                vm_break;
            }
            vm_case(OP_SUBI) {
                op_arithI(C, isub, c_numsub);
                vm_break;
            }
            vm_case(OP_MULI) {
                op_arithI(C, imul, c_nummul);
                vm_break;
            }
            vm_case(OP_DIVI) {
                op_arithIf(C, c_numdiv);
                vm_break;
            }
            vm_case(OP_MODI) {
                savepc(C); /* in case of division by 0 */
                op_arithI(C, csV_modint, csV_modnum);
                vm_break;
            }
            vm_case(OP_POWI) {
                op_arithIf(C, c_numpow);
                vm_break;
            }
            vm_case(OP_BSHLI) {
                op_bitwiseI(C, csO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHRI) {
                op_bitwiseI(C, csO_shiftr);
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
                op_arith(C, iadd, c_numadd);
                vm_break;
            }
            vm_case(OP_SUB) {
                op_arith(C, isub, c_numsub);
                vm_break;
            }
            vm_case(OP_MUL) {
                op_arith(C, imul, c_nummul);
                vm_break;
            }
            vm_case(OP_DIV) {
                op_arithf(C, c_numdiv);
                vm_break;
            }
            vm_case(OP_MOD) {
                savepc(C); /* in case of division by 0 */
                op_arith(C, csV_modint, csV_modnum);
                vm_break;
            }
            vm_case(OP_POW) {
                op_arithf(C, c_numpow);
                vm_break;
            }
            vm_case(OP_BSHL) {
                op_bitwise(C, csO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHR) {
                op_bitwise(C, csO_shiftr);
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
                int n = fetchl();
                Protect(csV_concat(C, n)); /* 'csV_concat handles 'sp' */
                checkGC(C);
                vm_break;
            }
            /* } ORDERING_OPS { */
            vm_case(OP_EQK) {
                TValue *v1 = peek(0);
                const TValue *vK = K(fetchl());
                int cond = fetchs();
                setorderres(v1, csV_raweq(v1, vK), cond);
                vm_break;
            }
            vm_case(OP_EQI) {
                TValue *v1 = peek(0);
                int cond;
                int imm_i = fetchl();
                int sign = getsign();
                int condexp = fetchs();
                imm_i *= sign;
                if (ttisint(v1))
                    cond = (ival(v1) == imm_i);
                else if (ttisflt(v1))
                    cond = c_numeq(fval(v1), cast_num(imm_i));
                else
                    cond = 0;
                setorderres(v1, cond, condexp);
                vm_break;
            }
            vm_case(OP_LTI) {
                op_orderI(C, ilt, c_numlt);
                vm_break;
            }
            vm_case(OP_LEI) {
                op_orderI(C, ile, c_numle);
                vm_break;
            }
            vm_case(OP_GTI) {
                op_orderI(C, igt, c_numgt);
                vm_break;
            }
            vm_case(OP_GEI) {
                op_orderI(C, ige, c_numge);
                vm_break;
            }
            vm_case(OP_EQ) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                int condexp = fetchs(); /* iseq */
                int cond;
                Protect(cond = csV_ordereq(C, v1, v2));
                setorderres(v1, cond, condexp);
                SP(-1); /* v2 */
                vm_break;
            }
            vm_case(OP_LT) {
                op_order(C, ilt, numlt, otherlt);
                vm_break;
            }
            vm_case(OP_LE) {
                op_order(C, ile, numle, otherle);
                vm_break;
            }
            vm_case(OP_EQPRESERVE) {
                SPtr res = TOP();
                TValue *v1 = peek(1);
                TValue *v2 = s2v(res);
                int cond;
                Protect(cond = csV_ordereq(C, v1, v2));
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
                SPtr res = TOP();
                TValue *v = s2v(res);
                if (ttisint(v)) {
                    cs_Integer i = ival(v);
                    setival(v, c_intop(-, 0, i));
                } else if (ttisflt(v)) {
                    cs_Number n = fval(v);
                    setfval(v, c_numunm(C, n));
                } else
                    Protect(csMM_tryunary(C, v, res, CS_MM_UNM));
                vm_break;
            }
            vm_case(OP_BNOT) {
                SPtr res = TOP();
                TValue *v = s2v(res);
                if (ttisint(v)) {
                    cs_Integer i = ival(v);
                    setival(v, c_intop(^, ~c_castS2U(0), i));
                } else
                    Protect(csMM_tryunary(C, v, res, CS_MM_BNOT));
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
            vm_case(OP_TESTANDPOP) {
                TValue *v = peek(0);
                int off = fetchl();
                int cond = fetchs();
                if ((!c_isfalse(v)) == cond) {
                    pc += off;
                    SP(-1); /* v */
                }
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
                SPtr func = STK(fetchl());
                int nres = fetchl() - 1;
                savepc(C);
                if ((newcf = precall(C, func, nres)) != NULL) {
                    cf = newcf;
                    goto startfunc;
                } /* else call is already done (not a CScript closure) */
                vm_break;
            }
            vm_case(OP_CLOSE) {
                SPtr level = STK(fetchl());
                ProtectTop(csF_close(C, level, CS_OK));
                vm_break;
            }
            vm_case(OP_TBC) {
                SPtr level = STK(fetchl());
                Protect(csF_newtbcvar(C, level));
                vm_break;
            }
            vm_case(OP_GETGLOBAL) {
                TValue *key = K(fetchl());
                TValue *G = getGtable(C);
                const TValue *val = csH_getstr(tval(G), strval(key));
                if (!isempty(val)) {
                    setobj2s(C, C->sp.p, val);
                } else
                    setnilval(s2v(C->sp.p));
                SP(1);
                vm_break;
            }
            vm_case(OP_SETGLOBAL) {
                TValue *key = K(fetchl());
                TValue *G = getGtable(C);
                TValue *v = peek(0);
                cs_assert(ttisstring(key));
                csH_set(C, tval(G), key, v);
                SP(-1); /* v */
                vm_break;
            }
            vm_case(OP_GETLOCAL) {
                int i = fetchl();
                setobjs2s(C, C->sp.p, STK(i));
                SP(1);
                vm_break;
            }
            vm_case(OP_SETLOCAL) {
                int i = fetchl();
                setobjs2s(C, STK(i), SP(-1));
                vm_break;
            }
            vm_case(OP_GETUVAL) {
                int i = fetchl();
                cs_assert(cl->upvals != NULL);
                cs_assert(cl->nupvalues > i);
                setobj2s(C, C->sp.p, cl->upvals[i]->v.p);
                SP(1);
                vm_break;
            }
            vm_case(OP_SETUVAL) {
                int i = fetchl();
                cs_assert(cl->upvals != NULL);
                cs_assert(cl->nupvalues > i);
                setobj(C, cl->upvals[i]->v.p, s2v(SP(-1)));
                vm_break;
            }
            vm_case(OP_SETARRAY) {
                uint last = fetchl(); /* num of elems. already in the array */
                int n = fetchs(); /* num of elems. to store */
                SPtr sa = SLOT(n); /* array stack slot */
                Array *arr = arrval(s2v(sa));
                if (n == 0)
                    n = (C->sp.p - sa) - 1; /* get up to the top */
                cs_assert(n > 0);
                last += n - 1;
                csA_ensure(C, arr, last);
                for (; n > 0; n--) { /* set the values from the stack... */
                    /* ...into the array (in reverse order) */
                    TValue *v = s2v(sa + n);
                    setobj(C, &arr->b[last--], v);
                    csG_barrierback(C, obj2gco(arr), v);
                }
                C->sp.p = sa + 1; /* pop off elements */
                vm_break;
            }
            vm_case(OP_SETPROPERTY) { /* NOTE: optimize? */
                TValue *o = peek(fetchl());
                TValue *prop = K(fetchl());
                TValue *v = peek(0);
                cs_assert(ttisstring(prop));
                Protect(csV_set(C, o, prop, v));
                SP(-1); /* v */
                vm_break;
            }
            vm_case(OP_GETPROPERTY) { /* NOTE: optimize? */
                TValue *prop = K(fetchl());
                TValue *v = peek(0);
                cs_assert(ttisstring(prop));
                Protect(csV_get(C, v, prop, TOP()));
                vm_break;
            }
            vm_case(OP_GETINDEX) {
                TValue *o = peek(1);
                TValue *key = peek(0);
                Protect(csV_get(C, o, key, SLOT(1)));
                SP(-1); /* v2 */
                vm_break;
            }
            vm_case(OP_SETINDEX) {
                SPtr os = SLOT(fetchl());
                TValue *o = s2v(os);
                TValue *idx = s2v(os + 1);
                TValue *v = peek(0);
                Protect(csV_set(C, o, idx, v));
                SP(-1); /* v */
                vm_break;
            }
            vm_case(OP_GETINDEXSTR) { /* TODO: optimize */
                TValue *i = K(fetchl());
                TValue *v = peek(0);
                cs_assert(ttisstring(i));
                Protect(csV_get(C, v, i, TOP()));
                vm_break;
            }
            vm_case(OP_SETINDEXSTR) { /* TODO: optimize */
                TValue *o = peek(fetchl());
                TValue *idx = K(fetchl());
                TValue *v = peek(0);
                cs_assert(ttisstring(idx));
                Protect(csV_set(C, o, idx, v));
                SP(-1); /* v */
                vm_break;
            }
            vm_case(OP_GETINDEXINT) { /* TODO: optimize */
                TValue i;
                TValue *v = peek(0);
                setival(&i, fetchl());
                Protect(csV_get(C, v, &i, TOP()));
                vm_break;
            }
            vm_case(OP_SETINDEXINT) { /* TODO: optimize */
                TValue *o = peek(fetchl());
                TValue *v = peek(0);
                cs_Integer imm_i = fetchl();
                TValue index;
                setival(&index, imm_i);
                Protect(csV_set(C, o, &index, v));
                SP(-1); /* v */
                vm_break;
            }
            vm_case(OP_GETSUP) {
                TValue *ks = K(fetchl());
                Instance *ins = insval(peek(1));
                OClass *cls = classval(peek(0));
                savepc(C);
                getsuper(C, ins, cls, strval(ks), SLOT(1), csH_getstr);
                SP(-1); /* cls */
                vm_break;
            }
            vm_case(OP_GETSUPIDX) {
                Instance *ins = insval(peek(2));
                OClass *cls = classval(peek(1));
                TValue *idx = peek(0);
                savepc(C);
                getsuper(C, ins, cls, idx, SLOT(2), csH_get);
                SP(-2); /* cls, idx */
                vm_break;
            }
            vm_case(OP_GETSUPIDXSTR) {
                TValue *ks = K(fetchl());
                Instance *ins = insval(peek(1));
                OClass *cls = classval(peek(0));
                savepc(C);
                getsuper(C, ins, cls, strval(ks), SLOT(1), csH_getstr);
                SP(-1); /* cls */
                vm_break;
            }
            vm_case(OP_INHERIT) {
                TValue *o = peek(1);
                OClass *cls = classval(peek(0));
                OClass *sup;
                savepc(C);
                if (c_unlikely(!ttisclass(o)))
                    csD_runerror(C, "inherit from %s value", typename(ttype(o)));
                sup = classval(o);
                if (c_likely(sup->methods)) { /* superclass has methods? */
                    cls->methods = csH_new(C);
                    csH_copykeys(C, cls->methods, sup->methods);
                }
                if (sup->vmt) { /* superclass has metamethods? */
                    cs_assert(cls->vmt == NULL);
                    cls->vmt = csMM_newvmt(C);
                    for (int i = 0; i < CS_MM_N; i++)
                        setobj(C, &cls->vmt[i], &sup->vmt[i]);
                }
                vm_break;
            }
            vm_case(OP_FORPREP) {
                SPtr stk = STK(fetchl()); /* slot of iterator function */
                int off = fetchl(); /* offset that skips loop body */
                /* create to-be-closed upvalue (if any) */
                Protect(csF_newtbcvar(C, stk+FORTBCVAR));
                pc += off;
                check_exp(*pc == OP_FORCALL, cast_void(fetch())); /* skip */
                goto l_forcall;
            }
            vm_case(OP_FORCALL) {
            l_forcall: {
                SPtr stk = STK(fetchl());
                int nres = fetchl();
                /* 'stk' slot is iterator function, 'stk + 1' is the
                 * invariant state 'stk + 2' is the control variable, and
                 * 'stk + 3' is the to-be-closed variable. Call uses stack
                 * after these values (starting at 'stk + 4'). */
                memcpy(stk+NSTATEVARS, stk, FORTBCVAR*sizeof(*stk));
                C->sp.p = stk+NSTATEVARS+FORTBCVAR; /* adjust stack pointer */
                Protect(csV_call(C, stk+NSTATEVARS, nres)); /* call iter */
                updatebase(cf);
                check_exp(*pc == OP_FORLOOP, cast_void(fetch())); /* skip */
                goto l_forloop;
            }}
            vm_case(OP_FORLOOP) {
            l_forloop: {
                SPtr stk = STK(fetchl());
                int off = fetchl();
                int nvars = fetchl();
                if (!ttisnil(s2v(stk + NSTATEVARS))) { /* result is not nil? */
                    /* save control variable */
                    setobjs2s(C, stk+FORCNTLVAR, stk+NSTATEVARS);
                    pc -= off; /* jump back to loop body */
                } else /* otherwise leave the loop (fall through) */
                    SP(-nvars); /* remove leftover vars from previous call */
                vm_break;
            }}
            vm_case(OP_RET) {
                SPtr stk = STK(fetchl());
                int nres = fetchl() - 1; /* number of results */
                if (nres < 0) /* not fixed ? */
                    nres = C->sp.p - stk;
                savepc(C);
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
