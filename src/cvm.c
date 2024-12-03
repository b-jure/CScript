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
#include "chashtable.h"
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


/* by default, use jump table */
#if !defined(CS_USE_JUMPTABLE)
#if defined(__GNUC__)
#define CS_USE_JUMPTABLE	1
#else
#define CS_USE_JUMPTABLE	0
#endif
#endif


static int booleans[2] = { CS_VFALSE, CS_VTRUE };


/*
** Allocate new CSript closure, push it on stack and
** initialize its upvalues.
*/
static void pushclosure(cs_State *ts, Proto *p, UpVal **enc, SPtr base) {
    int nupvals = p->sizeupvals;
    CSClosure *cl = csF_newCSClosure(ts, nupvals);
    cl->p = p;
    setclCSval2s(ts, ts->sp.p++, cl); /* anchor to stack */
    for (int i = 0; i < nupvals; i++) {
        UpValInfo *uv = &p->upvals[i];
        if (uv->onstack)
            cl->upvals[i] = csF_findupval(ts, base + uv->idx);
        else
            cl->upvals[i] = enc[uv->idx];
        csG_objbarrier(ts, cl, cl->upvals[i]);
    }
}


/*
** Integer division; handles division by 0 and possible
** overflow if 'y' == '-1' and 'x' == CS_INTEGER_MIN.
*/
cs_Integer csV_div(cs_State *ts, cs_Integer x, cs_Integer y) {
    if (c_unlikely(csi_castS2U(y) + 1 <= 1)) { /* 'y' == '0' or '-1' */
        if (y == 0)
            csD_runerror(ts, "division by 0");
        return csi_intop(-, 0, x);
    }
    return (x / y);
}


/*
** Integer modulus; handles modulo by 0 and overflow
** as explained in 'csV_div()'.
*/
cs_Integer csV_modint(cs_State *ts, cs_Integer x, cs_Integer y) {
    cs_Integer r;
    if (c_unlikely(csi_castS2U(y) + 1 <= 1)) {
        if (y == 0)
            csD_runerror(ts, "attempt to x%%0");
        return 0;
    }
    csi_nummod(ts, x, y, r);
    return r;
}


/* floating point modulus */
cs_Number csV_modnum(cs_State *ts, cs_Number x, cs_Number y) {
    cs_Number r;
    csi_nummod(ts, x, y, r);
    return r;
}


/*
** Perform binary arithmetic operations on objects, this function is free
** to call overloaded methods in cases where raw arithmetics are not possible.
*/
void csV_binarithm(cs_State *ts, const TValue *v1, const TValue *v2, SPtr res,
                   int op) {
    if (!csO_arithmraw(ts, v1, v2, s2v(res), op))
        csMM_trybin(ts, v1, v2, res, (op - CS_OPADD) + CS_MM_ADD);
}


/*
** Perform unary arithmetic operations on objects, this function is free
** to call overloaded methods in cases where raw arithmetics are not possible.
*/
void csV_unarithm(cs_State *ts, const TValue *v, SPtr res, int op) {
    TValue aux;
    setival(&aux, 0);
    if (!csO_arithmraw(ts, v, &aux, s2v(res), op))
        csMM_tryunary(ts, v, res, (op - CS_OPUNM) + CS_MM_UNM);
}


/* set 'vmt' entry */
static void setmm(cs_State *ts, TValue **vmt, TValue *fn, int vmtt) {
    cs_assert(0 <= vmtt && vmtt < CS_MM_N);
    if (c_unlikely(!(*vmt))) /* empty 'vmt' */
        *vmt = csMM_newvmt(ts);
    (*vmt)[vmtt] = *fn; /* set the entry */
}


/*
** According to C99 6.3.1.8 page 45:
** "...if the corresponding real type of either operand is double, the other
** operand is converted, without change of type domain, to a type whose
** corresponding real type is double."
*/
cs_sinline int intlenum(cs_State *ts, const TValue *v1, const TValue *v2) {
    UNUSED(ts);
    return csi_numle(cast_num(ival(v1)), fval(v2));
}


/* check 'intLEnum' */
cs_sinline int numleint(cs_State *ts, const TValue *v1, const TValue *v2) {
    UNUSED(ts);
    return csi_numle(fval(v1), cast_num(ival(v2)));
}


/* less equal ordering on numbers */
cs_sinline int numle(cs_State *ts, const TValue *v1, const TValue *v2) {
    cs_assert(ttisnum(v1) && ttisnum(v2));
    if (ttisint(v1)) {
        cs_Integer i1 = ival(v1);
        if (ttisint(v2)) return (i1 <= ival(v2));
        else return intlenum(ts, v1, v2);
    } else {
        cs_Number n1 = fval(v1);
        if (ttisint(v2)) return numleint(ts, v1, v2);
        else return csi_numlt(n1, fval(v2));
    }
}


/* less equal ordering on non-number values */
cs_sinline int otherle(cs_State *ts, const TValue *v1, const TValue *v2) {
    if (ttisstring(v1) && ttisstring(v2))
        return (csS_cmp(strval(v1), strval(v2)) <= 0);
    else
        return csMM_order(ts, v1, v2, CS_MM_LE);
}


/* 'less or equal' ordering '<=' */
int csV_orderle(cs_State *ts, const TValue *v1, const TValue *v2) {
    if (ttisnum(v1) && ttisnum(v2))
        return numle(ts, v1, v2);
    return otherle(ts, v1, v2);
}


/* check 'intLEnum' */
cs_sinline int intltnum(const TValue *v1, const TValue *v2) {
    return csi_numlt(cast_num(ival(v1)), fval(v2));
}


/* check 'intLEnum' */
cs_sinline int numltint(const TValue *v1, const TValue *v2) {
    return csi_numlt(fval(v1), cast_num(ival(v2)));
}


/* 'less than' ordering '<' on number values */
cs_sinline int numlt(const TValue *v1, const TValue *v2) {
    cs_assert(ttisnum(v1) && ttisnum(v2));
    if (ttisint(v1)) {
        cs_Integer i1 = ival(v1);
        if (ttisint(v2)) return (i1 <= ival(v2));
        else return intltnum(v1, v2);
    } else {
        cs_Number n1 = fval(v1);
        if (ttisint(v2)) return numltint(v1, v2);
        else return csi_numlt(n1, fval(v2));
    }
}


/* 'less than' ordering '<' on non-number values */
cs_sinline int otherlt(cs_State *ts, const TValue *v1, const TValue *v2) {
    if (ttisstring(v1) && ttisstring(v2))
        return csS_cmp(strval(v1), strval(v2));
    else
        return csMM_order(ts, v1, v2, CS_MM_LT);
}


/* 'less than' ordering '<' */
int csV_orderlt(cs_State *ts, const TValue *v1, const TValue *v2) {
    if (ttisnum(v1) && ttisnum(v2))
        return numlt(v1, v2);
    return otherlt(ts, v1, v2);
}


/* 
** Equality ordering '=='.
** In case 'ts' is NULL perform raw equality (without invoking '__eq').
*/
int csV_ordereq(cs_State *ts, const TValue *v1, const TValue *v2) {
    cs_Integer i1, i2;
    const TValue *method;
    if (ttypetag(v1) != ttypetag(v2)) {
        if (ttype(v1) != ttype(v2) || ttype(v1) != CS_TNUMBER)
            return 0;
        return (csO_tointeger(v1, &i1, N2IEXACT) &&
                csO_tointeger(v2, &i2, N2IEXACT) && i1 == i2);
    }
    switch (ttypetag(v1)) {
        case CS_VNIL: case CS_VFALSE: case CS_VTRUE: return 1;
        case CS_VNUMINT: return (ival(v1) == ival(v2));
        case CS_VNUMFLT: return csi_numeq(fval(v1), fval(v2));
        case CS_VLIGHTUSERDATA: return (pval(v1) == pval(v2));
        case CS_VSHRSTR: return eqshrstr(strval(v1), strval(v2));
        case CS_VLNGSTR: return csS_eqlngstr(strval(v1), strval(v2));
        case CS_VUSERDATA: {
            if (uval(v1) == uval(v2)) return 1;
            else if (ts == NULL) return 0;
            method = csMM_get(ts, v1, CS_MM_EQ);
            if (ttisnil(method))
                method = csMM_get(ts, v2, CS_MM_EQ);
            break;
        }
        case CS_VINSTANCE: {
            if (insval(v1) == insval(v2)) return 1;
            else if (ts == NULL) return 0;
            method = csMM_get(ts, v1, CS_MM_EQ);
            if (ttisnil(method))
                method = csMM_get(ts, v2, CS_MM_EQ);
            break;
        }
        default: return (gcoval(v1) == gcoval(v2));
    }
    if (isabstkey(method))  {
        return 0;
    } else {
        csMM_callbinres(ts, method, v1, v2, ts->sp.p);
        return !c_isfalse(s2v(ts->sp.p));
    }
}


static void setarrayindex(cs_State *ts, Array *arr, const TValue *index,
                          const TValue *val) {
    cs_Integer i;
    if (c_likely(tointeger(index, &i))) { /* index is integer? */
        if (c_likely(0 <= i)) { /* non-negative index? */
            if (c_unlikely(i >= ARRAYLIMIT)) /* too large 'index'? */
                csD_indexerror(ts, i, "too large");
            csA_ensure(ts, arr, i); /* expand array */
            setobj(ts, &arr->b[i], val); /* set the value at index */
            csG_barrierback(ts, obj2gco(arr), val);
        } else { /* negative index, error */
            csD_indexerror(ts, i, "negative");
        }
    } else {
        csD_indextypeerror(ts, index);
    }
}


void csV_rawset(cs_State *ts, const TValue *obj, const TValue *key,
                const TValue *val) {
    switch (ttypetag(obj)) {
        case CS_VARRAY: {
            setarrayindex(ts, arrval(obj), key, val);
            break;
        }
        case CS_VHTABLE: {
            csH_set(ts, htval(obj), key, val);
            csG_barrierback(ts, gcoval(obj), val);
            break;
        }
        case CS_VINSTANCE: {
            csH_set(ts, insval(obj)->fields, key, val);
            csG_barrierback(ts, gcoval(obj), val);
            break;
        }
        default: {
            csD_typeerror(ts, obj, "index");
            break;
        }
    }
}


void csV_set(cs_State *ts, const TValue *obj, const TValue *key,
             const TValue *val) {
    const TValue *fmm = csMM_get(ts, obj, CS_MM_SETIDX);
    if (!ttisnil(fmm)) { /* have metamethod? */
        csMM_callset(ts, fmm, obj, key, val);
        return; /* done */
    } else /* otherwise perform raw set */
        csV_rawset(ts, obj, key, val);
}


static void arraygeti(cs_State *ts, Array *arr, const TValue *index, SPtr res) {
    cs_Integer i;
    if (c_likely(tointeger(index, &i))) { /* index is integer? */
        if (0 <= i) { /* positive index? */
            if (c_unlikely(ARRAYLIMIT <= i)) { /* too large index? */
                csD_indexerror(ts, i, "too large");
            } else if (i < arr->sz) { /* index in bounds? */
                setobj2s(ts, res, &arr->b[i]);
            } else /* index out of bounds */
                setnilval(s2v(res));
        } else { /* negative index */
            csD_indexerror(ts, i, "negative");
        }
    } else { /* invalid index */
        csD_indextypeerror(ts, index);
    }
}


/* bind method to instance and set it at 'res' */
#define bindmethod(ts,ins,fn,res) \
    setimval2s(ts, res, csMM_newinsmethod(ts, ins, fn))


void csV_rawget(cs_State *ts, const TValue *obj, const TValue *key, SPtr res) {
    switch (ttypetag(obj)) {
        case CS_VARRAY: {
            arraygeti(ts, arrval(obj), key, res);
            break;
        }
        case CS_VHTABLE: {
            const TValue *slot = csH_get(htval(obj), key);
            if (!ttisnil(slot)) {
                setobj2s(ts, res, slot);
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
                if (!isempty(slot)) {
                    setobj2s(ts, res, slot);
                    bindmethod(ts, ins, slot, res);
                    break; /* done */
                } /* else fall through */
                setnilval(s2v(res));
            } else
                setobj2s(ts, res, slot);
            break;
        }
        default: {
            csD_typeerror(ts, obj, "index");
            break;
        }
    }
}


void csV_get(cs_State *ts, const TValue *obj, const TValue *key, SPtr res) {
    const TValue *fmm = csMM_get(ts, obj, CS_MM_GETIDX);
    if (!ttisnil(fmm)) { /* have metamethod ? */
        csMM_callgetres(ts, fmm, obj, key, res);
    } else /* otherwise perform raw get */
        csV_rawget(ts, obj, key, res);
}


#define checkmethods(cls,res) \
        (!(cls)->methods ? (setnilval(s2v(res)), 0) : 1)


#define getsuper(ts,ins,cls,k,res,fget) \
    { if ((cls)->methods) { \
        const TValue *f = fget((cls)->methods, k); \
        if (!isabstkey(f)) bindmethod(ts, ins, f, res); \
      } else setnilval(s2v(res)); }


/*
** Call binary meta method, but before that perform a quick check
** and invoke error if types don't match, the values are instances
** that belong to different classes or v1 (self) doesn't have the
** overloaded method.
*/
cs_sinline void precallmbin(cs_State *ts, const TValue *v1, const TValue *v2,
                            cs_MM op, SPtr res) {
    const TValue *func;
    const char *opname = getshrstr(G_(ts)->mmnames[op]);
    if (c_unlikely(ttypetag(v1) != ttypetag(v2)))
        csD_typeerrormeta(ts, v1, v2, opname);
    if (c_unlikely(ttisinstance(v1) && insval(v1)->oclass != insval(v2)->oclass))
        csD_runerror(ts, "tried to %s instances of different class", opname);
    func = csMM_get(ts, v1, op);
    if (c_unlikely(ttisnil(func)))
        csD_typeerror(ts, v1, opname);
    else
        csMM_callbinres(ts, func, v1, v2, res);
}


/* properly move results and if needed close variables */
cs_sinline void moveresults(cs_State *ts, SPtr res, int nres, int wanted) {
    int i;
    SPtr firstresult;
    switch (wanted) {
        case 0: { /* no values needed */
            ts->sp.p = res;
            return;
        }
        case 1: { /* one value needed */
            if (nres == 0)
                setnilval(s2v(res));
            else
                setobjs2s(ts, res, ts->sp.p - nres);
            ts->sp.p = res + 1;
            return;
        }
        case CS_MULRET: { /* all values needed */
            wanted = nres;
            break;
        }
        default: {
            if (hastocloseCfunc(wanted)) { /* tbc variables? */
                res = csF_close(ts, res, CLOSEKTOP); /* do the closing */
                wanted = decodeNresults(wanted); /* decode nresults */
                if (wanted == CS_MULRET) /* all values needed? */
                    wanted = nres;
            }
            break;
        }
    }
    firstresult = ts->sp.p - nres;
    if (nres > wanted) /* have extra results? */
        nres = wanted; /* discard them */
    for (i = 0; i < nres; i++) /* move all the results */
        setobjs2s(ts, res + i, firstresult + i);
    for (; i < wanted; i++)
        setnilval(s2v(res + i));
    ts->sp.p = res + wanted;
}


/* move the results into correct place and return to caller */
cs_sinline void poscall(cs_State *ts, CallFrame *cf, int nres) {
    moveresults(ts, cf->func.p, nres, cf->nresults);
    ts->cf = cf->prev; /* back to caller */
}


#define next_cf(ts)   ((ts)->cf->next ? (ts)->cf->next : csT_newcf(ts))

cs_sinline CallFrame *prepcallframe(cs_State *ts, SPtr func, int nres,
                                    int mask, SPtr top) {
    CallFrame *cf = ts->cf = next_cf(ts);
    cf->func.p = func;
    cf->top.p = top;
    cf->nresults = nres;
    cf->status = mask;
    return cf;
}


cs_sinline int precallC(cs_State *ts, SPtr func, int nres, cs_CFunction f) {
    CallFrame *cf;
    int n; /* number of returns */
    checkstackGCp(ts, CS_MINSTACK, func);
    ts->cf = cf = prepcallframe(ts, func, nres, CFST_CCALL,
                                ts->sp.p + CS_MINSTACK);
    cs_unlock(ts);
    n = (*f)(ts);
    cs_lock(ts);
    api_checknelems(ts, n);
    poscall(ts, cf, n);
    return n;
}


/* 
** Adjust stack for meta method call.
** func is the object that has metamethod and f is the metamethod.
** Stack is shifted so that the f is in place of func and func is its
** argument at func + 1. This function assumes there is enough space
** on the stack for f.
*/
cs_sinline void auxinsertf(cs_State *ts, SPtr func, const TValue *f) {
    for (SPtr p = ts->sp.p; p > func; p--)
        setobjs2s(ts, p, p-1);
    ts->sp.p++;
    setobj2s(ts, func, f);
}


/* try and call __call metamethod */
cs_sinline SPtr trymetacall(cs_State *ts, SPtr func) {
    const TValue *f;
    checkstackGCp(ts, 1, func); /* space for func */
    f = csMM_get(ts, s2v(func), CS_MM_CALL); /* (after GC) */
    if (c_unlikely(ttisnil(f))) /* missing __call? (after GC) */
        csD_callerror(ts, s2v(func));
    auxinsertf(ts, func, f);
    return func;
}


CallFrame *precall(cs_State *ts, SPtr func, int nres) {
retry:
    switch (ttypetag(s2v(func))) {
        case CS_VCCL: { /* C closure */
            precallC(ts, func, CS_MULRET, clCval(s2v(func))->fn);
            return NULL; /* done */
        }
        case CS_VLCF: { /* light C function */
            precallC(ts, func, CS_MULRET, lcfval(s2v(func)));
            return NULL; /* done */
        }
        case CS_VCSCL: { /* CScript closure */
            CallFrame *cf;
            Proto *p = clCSval(s2v(func))->p;
            int nargs = (ts->sp.p - func) - 1;
            int fsize = p->maxstack;
            checkstackGCp(ts, fsize, func);
            ts->cf = cf = prepcallframe(ts, func, nres, 0, func + fsize + 1);
            for (; nargs < p->arity; nargs++)
                setnilval(s2v(ts->sp.p++)); /* set missing args as 'nil' */
            cs_assert(cf->top.p <= ts->stackend.p);
            return cf; /* new call frame */
        }
        case CS_VCLASS: { /* Class object */
            const TValue *fmm;
            Instance *ins = csMM_newinstance(ts, classval((s2v(func))));
            setinsval2s(ts, func, ins); /* replace class with its instance */
            fmm = csMM_get(ts, s2v(func), CS_MM_INIT);
            if (!ttisnil(fmm)) { /* have __init ? */
                checkstackGCp(ts, 1, func); /* space for fmm */
                fmm = csMM_get(ts, s2v(func), CS_MM_INIT); /* (after GC) */
                if (c_likely(!ttisnil(fmm))) { /* have __init (after GC)? */
                    auxinsertf(ts, func, fmm); /* insert it into stack... */
                    cs_assert(ttisfunction(fmm));
                    goto retry; /* ...and try calling it */
                } /* else fall through */
            } /* else fall through */
            return NULL; /* done */
        }
        case CS_VIMETHOD: { /* Instance method */
            IMethod *im = imval(s2v(func));
            cs_assert(ttisfunction(&im->method));
            checkstackGCp(ts, 1, func); /* space for method */
            auxinsertf(ts, func, &im->method); /* insert method... */
            setinsval2s(ts, func + 1, im->ins); /* ...and replace func */
            goto retry;
        }
        default: { /* check for __call */
            func = trymetacall(ts, func);
            goto retry;
        }
    }
}


cs_sinline void ccall(cs_State *ts, SPtr func, int nresults, cs_uint32 inc) {
    CallFrame *cf;
    ts->nCcalls += inc;
    if (c_unlikely(getCcalls(ts) >= CSI_MAXCCALLS)) {
        checkstackp(ts, 0, func);  /* free any use of EXTRA_STACK */
        csT_checkCstack(ts);
    }
    if ((cf = precall(ts, func, nresults)) != NULL) {  /* CScript function? */
        cf->status = CFST_FRESH;
        csV_execute(ts, cf);
    }
    ts->nCcalls -= inc;
}


/* external interface for 'ccall' */
void csV_call(cs_State *ts, SPtr func, int nresults) {
    ccall(ts, func, nresults, nyci);
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


void csV_concat(cs_State *ts, int total) {
    if (total == 1)
        return; /* done */
    do {
        SPtr top = ts->sp.p;
        int n = 2; /* number of elements (minimum 2) */
        if (!(ttisstring(s2v(top - 2)) && ttisstring(s2v(top - 1))))
            csMM_tryconcat(ts);
        else if (isemptystr(s2v(top - 1))) /* second operand is empty string? */
            ; /* result already in the first operand */
        else if (isemptystr(s2v(top - 2))) { /* first operand is empty string? */
            setobjs2s(ts, top - 2, top - 1); /* result is second operand */
        } else { /* at least 2 non-empty strings */
            size_t ltotal = getstrlen(strval(s2v(top - 1)));
            /* collect total length and number of strings */
            for (n = 1; n < total && ttisstring(s2v(top - n - 1)); n++) {
                size_t len = getstrlen(strval(s2v(top - n - 1)));
                if (c_unlikely(len >= MAXSIZE - sizeof(OString) - ltotal)) {
                    ts->sp.p = top - total; /* pop strings */
                    csD_runerror(ts, "string length overflow");
                }
                ltotal += len;
            }
            OString *s;
            if (ltotal <= CSI_MAXSHORTLEN) { /* fits in a short string? */
                char buff[CSI_MAXSHORTLEN];
                copy2buff(top, n, buff);
                s = csS_newl(ts, buff, ltotal);
            } else { /* otherwise long string */
                s = csS_newlngstrobj(ts, ltotal);
                copy2buff(top, n, getstr(s));
            }
            setstrval2s(ts, top - n, s);
        }
        total -= n - 1; /* got 'n' strings to create one new */
        ts->sp.p -= n - 1; /* popped 'n' strings and pushed one */
    } while (total > 1);
}


/* -----------------------------------------------------------------------
** Macros for arithmetic/bitwise/comparison operations on numbers.
**------------------------------------------------------------------------ */

/* 'cs_Integer' arithmetic operations */
#define iadd(ts,a,b)    (csi_intop(+, a, b))
#define isub(ts,a,b)    (csi_intop(-, a, b))
#define imul(ts,a,b)    (csi_intop(*, a, b))

/* integer bitwise operations */
#define iband(a,b)      (csi_intop(&, a, b))
#define ibor(a,b)       (csi_intop(|, a, b))
#define ibxor(a,b)      (csi_intop(^, a, b))

/* integer ordering operations */
#define ilt(a,b)        (a < b)
#define ile(a,b)        (a <= b)
#define igt(a,b)        (a > b)
#define ige(a,b)        (a >= b)


/* 
** Arithmetic operations
*/

#define op_arithKf_aux(ts,v1,v2,fop) { \
    cs_Number n1, n2; \
    if (tonumber(v1, &n1) && tonumber(v2, &n2)) { \
        setfval(v1, fop(ts, n1, n2)); \
    } else { \
        csD_aritherror(ts, v1, v2); \
    }}


/* arithmetic operations with constant operand for floats */
#define op_arithKf(ts,fop) { \
    TValue *v = peek(0); \
    TValue *lk = K(fetchl()); \
    op_arithKf_aux(ts, v, lk, fop); }


/* arithmetic operations with constant operand */
#define op_arithK(ts,iop,fop) { \
    TValue *v = peek(0); \
    TValue *lk = K(fetchl()); \
    if (ttisint(v) && ttisint(lk)) { \
        cs_Integer i1 = ival(v); \
        cs_Integer i2 = ival(lk); \
        setival(v, iop(ts, i1, i2)); \
    } else { \
        op_arithKf_aux(ts, v, lk, fop); \
    }}


/* arithmetic operation error with immediate operand */
#define op_arithI_error(ts,v,imm) \
    { TValue v2; setival(&v2, imm); csD_aritherror(ts, v, &v2); }


/* arithmetic operations with immediate operand for floats */
#define op_arithIf(ts,fop) { \
    TValue *v = peek(0); \
    int imm = fetchl(); /* L */\
    imm *= getsign(); /* S */\
    cs_Number n; \
    if (tonumber(v, &n)) { \
        cs_Number fimm = cast_num(imm); \
        setfval(v, fop(ts, n, fimm)); \
    } else { \
        op_arithI_error(ts, v, imm); \
    }}


/* arithmetic operations with immediate operand */
#define op_arithI(ts,iop,fop) { \
    TValue *v = peek(0); \
    int imm = fetchl(); /* L */\
    imm *= getsign(); /* S */\
    if (ttisint(v)) { \
        cs_Integer i = ival(v); \
        setival(v, iop(ts, i, imm)); \
    } else if (ttisflt(v)) { \
        cs_Number n = fval(v); \
        cs_Number fimm = cast_num(imm); \
        setfval(v, fop(ts, n, fimm)); \
    } else { \
        op_arithI_error(ts, v, imm); \
    }}


#define op_arithf_aux(ts,v1,v2,fop) { \
    cs_Number n1; cs_Number n2; \
    if (tonumber(v1, &n1) && tonumber(v2, &n2)) { \
        setfval(v1, fop(ts, n1, n2)); \
        pc += getOpSize(OP_MBIN); \
        pop(1); \
    }/* FALLTHRU to 'OP_MBIN' */}


/* arithmetic operations with stack operands for floats */
#define op_arithf(ts,fop) { \
    TValue *v1 = peek(1); \
    TValue *v2 = peek(0); \
    op_arithf_aux(ts, v1, v2, fop); }


/* arithmetic operations with stack operands */
#define op_arith(ts,iop,fop) { \
    TValue *v1 = peek(1); \
    TValue *v2 = peek(0); \
    if (ttisint(v1) && ttisint(v2)) { \
        cs_Integer i1 = ival(v1); cs_Integer i2 = ival(v2); \
        setival(v1, iop(ts, i1, i2)); \
        pc += getOpSize(OP_MBIN); \
        pop(1); \
    } else { \
        op_arithf_aux(ts, v1, v2, fop); \
    }}



/*
** Bitwise operations
*/

/* bitwise operations with constant operand */
#define op_bitwiseK(ts,op) { \
    TValue *v = peek(0); \
    TValue *lk = K(fetchl()); /* L */\
    cs_Integer i1; cs_Integer i2 = ival(lk);  \
    if (c_likely(tointeger(v, &i1))) { \
        setival(v, op(i1, i2)); \
    } else { \
        csD_bitwerror(ts, v, lk); \
    }}


/* bitwise operations with immediate operand */
#define op_bitwiseI(ts,op) { \
    TValue *v = peek(0); \
    int imm = fetchl(); /* L */\
    imm *= getsign(); /* S */\
    cs_Integer i; \
    if (c_likely(tointeger(v, &i))) { \
        setival(v, op(i, imm)); \
    } else { \
        TValue vimm; setival(&vimm, imm); \
        csD_bitwerror(ts, v, &vimm); \
    }}


/* bitwise operations with stack operands */
#define op_bitwise(ts,op) { \
    TValue *v1 = peek(1); \
    TValue *v2 = peek(0); \
    cs_Integer i1; cs_Integer i2; \
    if (tointeger(v1, &i1) && tointeger(v2, &i2)) { \
        setival(v1, op(i1, i2)); \
        pc += getOpSize(OP_MBIN); \
    }/* else try 'OP_MBIN' */}



/*
** Ordering operations
*/

/* set ordering result */
#define setorderres(v,cond,eq) \
    { cs_assert(0 <= cond && cond <= 1); settt(v, booleans[cond == eq]); }


/* order operations with stack operands */
#define op_order(ts,iop,fop,other) { \
    TValue *v1 = peek(1); \
    TValue *v2 = peek(0); \
    int iseq = fetchs(); /* S */\
    int cond; \
    if (ttisint(v1) && ttisint(v2)) { \
        cs_Integer i1 = ival(v1); \
        cs_Integer i2 = ival(v2); \
        cond = iop(i1, i2); \
    } else if (ttisnum(v1) && ttisnum(v2)) { \
        cond = fop(v1, v2); \
    } else { \
        Protect(cond = other(ts, v1, v2)); \
    } \
    setorderres(v1, cond, iseq); }


/* order operations with immediate operand */
#define op_orderI(ts,iop,fop) { \
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
    } else { \
        cond = 0; \
    } \
    setorderres(v, cond, 1); }


/* -----------------------------------------------------------------------
 * Interpreter loop
 * ----------------------------------------------------------------------- */

#define updatebase(cf)      (base = (cf)->func.p + 1)

/* 
** Correct global 'pc'.
*/
#define savepc(ts)      (cf->pc = pc)


/* protect code that can raise errors or reallocate the stack */
#define Protect(exp)            (savepc(ts), (exp))


/* correct global 'pc' before checking collector debt */
#define checkGC(ts)     csG_condGC(ts, savepc(ts), (void)0)


#if 1
#include "ctrace.h"
#define fetch()         (csTR_tracepc(ts, cl->p, pc), *pc++)
#else
#define fetch()         (*pc++)
#endif

/* fetch short instruction argument */
#define fetchs()        (*pc++)

/* fetch long instruction argument */
#define fetchl()        (cast_void(pc += SIZEARGL), get3bytes(pc - SIZEARGL))


/* get constant at index 'idx' */
#define K(idx)        (k + (idx))


/* get sign value */
#define getsign()       (fetchs() - 1)


/* peek stack (from (sp - 1) - n) */
#define peek(n)         s2v((ts->sp.p - 1) - n)

/* pop stack */
#define pop(n)          (ts->sp.p -= (n))


/* get stack slot (starting from 'base') */
#define STK(i)      (base+(i))

/* get stack slot (starting from top) */
#define SLOT(i)     (ts->sp.p-(i)-1)

/* get top stack slot */
#define TOPS()      SLOT(0)


/* In cases where jump table is not available or prefered. */
#define vm_dispatch(x)      switch(x)
#define vm_case(l)          case l:
#define vm_break            break


void csV_execute(cs_State *ts, CallFrame *cf) {
    register const Instruction *pc; /* program counter */
    register CSClosure *cl; /* closure being executed */
    register TValue *k; /* array of constants */
    register SPtr base; /* function base stack index */
#if CS_USE_JUMPTABLE
#include "cjmptable.h"
#endif
startfunc:
returning:
    cl = clCSval(s2v(cf->func.p));
    k = cl->p->k;
    pc = cl->p->code;
    base = cf->func.p + 1;
#if 1
        #include <stdio.h>
        printf(">> Tracing code execution <<\n");
#endif
    for (;;) {
        vm_dispatch(fetch()) {
            vm_case(OP_DUP) {
                /* no args */
                setobj2s(ts, ts->sp.p, s2v(ts->sp.p - 1));
                ts->sp.p++;
                vm_break;
            }
            vm_case(OP_TRUE) {
                /* no args */
                setbtval(s2v(ts->sp.p++));
                vm_break;
            }
            vm_case(OP_FALSE) {
                /* no args */
                setbfval(s2v(ts->sp.p++));
                vm_break;
            }
            vm_case(OP_NIL) {
                /* no args */
                setnilval(s2v(ts->sp.p++));
                vm_break;
            }
            vm_case(OP_NILN) {
                int n = fetchl();
                while (n--)
                    setnilval(s2v(ts->sp.p++));
                vm_break;
            } 
            vm_case(OP_CONST) {
                TValue *sk = K(fetchs());
                setobj2s(ts, ts->sp.p++, sk);
                vm_break;
            }
            vm_case(OP_CONSTL) {
                TValue *lk = K(fetchl());
                setobj2s(ts, ts->sp.p++, lk);
                vm_break;
            }
            vm_case(OP_CONSTI) {
                int imm_i = fetchl();
                int sign = getsign();
                setival(s2v(ts->sp.p++), imm_i*sign);
                vm_break;
            }
            vm_case(OP_CONSTF) {
                int imm_f = fetchl();
                int sign = getsign();
                setfval(s2v(ts->sp.p++), cast_num(imm_f*sign));
                vm_break;
            }
            vm_case(OP_VARARGPREP) {
                int arity = fetchl();
                Protect(csF_adjustvarargs(ts, arity, cf, cl->p));
                updatebase(cf); /* update base, it changed */
                vm_break;
            }
            vm_case(OP_VARARG) {
                int n = fetchl() - 1; /* num of varargs wanted */
                Protect(csF_getvarargs(ts, cf, n));
                vm_break;
            }
            vm_case(OP_CLOSURE) {
                int indexf = fetchl();
                Proto *fn = cl->p->p[indexf];
                Protect(pushclosure(ts, fn, cl->upvals, base));
                checkGC(ts);
                vm_break;
            }
            vm_case(OP_NEWARRAY) {
                int b = fetchs();
                Array *a;
                if (b > 0) /* array has elements? */
                    b = 1 << (b - 1); /* size is 2^(b - 1) */
                ts->sp.p++; /* inc. top */
                a = csA_new(ts); /* memory allocation */
                setarrval2s(ts, ts->sp.p - 1, a);
                if (b != 0) /* array is not empty? */
                    csA_ensure(ts, a, b - 1); /* ensure a[b-1] is valid */
                checkGC(ts);
                vm_break;
            }
            vm_case(OP_NEWCLASS) {
                OClass *cls;
                savepc(ts);
                cls = csMM_newclass(ts);
                setclsval2s(ts, ts->sp.p++, cls); /* push on stack */
                vm_break;
            }
            vm_case(OP_NEWTABLE) {
                int b = fetchs();
                HTable *t;
                if (b > 0) /* table has fields? */
                    b = 1 << (b - 1); /* size is 2^(b - 1) */
                ts->sp.p++; /* inc. top */
                t = csH_new(ts);
                sethtval2s(ts, ts->sp.p - 1, t);
                if (b != 0) /* table is not empty? */
                    csH_resize(ts, t, b); /* grow table to size 'b' */
                checkGC(ts);
                vm_break;
            }
            vm_case(OP_METHOD) {
                TValue *cls = peek(1);
                TValue *f = peek(0); /* method */
                TValue *key = K(fetchl());
                cs_assert(ttisstring(key));
                Protect(csH_set(ts, classval(f)->methods, key, cls));
                vm_break;
            }
            vm_case(OP_SETMM) {
                TValue *o = peek(1); /* class or userdata */
                TValue *f = peek(0); /* func */
                int mm = fetchs();
                Protect(setmm(ts, &classval(o)->vmt, f, mm));
                vm_break;
            }
            vm_case(OP_POP) {
                pop(1);
                vm_break;
            }
            vm_case(OP_POPN) {
                int n = fetchl();
                pop(n);
                vm_break;
            }
            /* } BINARY_OPS { ARITHMETIC_OPS { */
            vm_case(OP_MBIN) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                int op = fetchs(); /* op */
                Protect(precallmbin(ts, v1, v2, op, SLOT(1)));
                pop(1); /* v2 */
                vm_break;
            }
            vm_case(OP_ADDK) {
                op_arithK(ts, iadd, csi_numadd);
                vm_break;
            }
            vm_case(OP_SUBK) {
                op_arithK(ts, isub, csi_numsub);
                vm_break;
            }
            vm_case(OP_MULK) {
                op_arithK(ts, imul, csi_nummul);
                vm_break;
            }
            vm_case(OP_DIVK) {
                savepc(ts); /* in case of division by 0 */
                op_arithK(ts, csV_div, csi_numdiv);
                vm_break;
            }
            vm_case(OP_MODK) {
                savepc(ts); /* in case of division by 0 */
                op_arithK(ts, csV_modint, csV_modnum);
                vm_break;
            }
            vm_case(OP_POWK) {
                op_arithKf(ts, csi_numpow);
                vm_break;
            }
            vm_case(OP_BSHLK) {
                op_bitwiseK(ts, csO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHRK) {
                op_bitwiseK(ts, csO_shiftr);
                vm_break;
            }
            vm_case(OP_BANDK) {
                op_bitwiseK(ts, iband);
                vm_break;
            }
            vm_case(OP_BORK) {
                op_bitwiseK(ts, ibor);
                vm_break;
            }
            vm_case(OP_BXORK) {
                op_bitwiseK(ts, ibxor);
                vm_break;
            }
            vm_case(OP_ADDI) {
                op_arithI(ts, iadd, csi_numadd);
                vm_break;
            }
            vm_case(OP_SUBI) {
                op_arithI(ts, isub, csi_numsub);
                vm_break;
            }
            vm_case(OP_MULI) {
                op_arithI(ts, imul, csi_nummul);
                vm_break;
            }
            vm_case(OP_DIVI) {
                savepc(ts); /* in case of division by 0 */
                op_arithI(ts, csV_div, csi_numdiv);
                vm_break;
            }
            vm_case(OP_MODI) {
                savepc(ts); /* in case of division by 0 */
                op_arithI(ts, csV_modint, csV_modnum);
                vm_break;
            }
            vm_case(OP_POWI) {
                op_arithIf(ts, csi_numpow);
                vm_break;
            }
            vm_case(OP_BSHLI) {
                op_bitwiseI(ts, csO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHRI) {
                op_bitwiseI(ts, csO_shiftr);
                vm_break;
            }
            vm_case(OP_BANDI) {
                op_bitwiseI(ts, iband);
                vm_break;
            }
            vm_case(OP_BORI) {
                op_bitwiseI(ts, ibor);
                vm_break;
            }
            vm_case(OP_BXORI) {
                op_bitwiseI(ts, ibxor);
                vm_break;
            }
            vm_case(OP_ADD) {
                op_arith(ts, iadd, csi_numadd);
                vm_break;
            }
            vm_case(OP_SUB) {
                op_arith(ts, isub, csi_numsub);
                vm_break;
            }
            vm_case(OP_MUL) {
                op_arith(ts, imul, csi_nummul);
                vm_break;
            }
            vm_case(OP_DIV) {
                savepc(ts); /* in case of division by 0 */
                op_arith(ts, csV_div, csi_numdiv);
                vm_break;
            }
            vm_case(OP_MOD) {
                savepc(ts); /* in case of division by 0 */
                op_arith(ts, csV_modint, csV_modnum);
                vm_break;
            }
            vm_case(OP_POW) {
                op_arithf(ts, csV_modnum);
                vm_break;
            }
            vm_case(OP_BSHL) {
                op_bitwise(ts, csO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHR) {
                op_bitwise(ts, csO_shiftr);
                vm_break;
            }
            vm_case(OP_BAND) {
                op_bitwise(ts, iband);
                vm_break;
            }
            vm_case(OP_BOR) {
                op_bitwise(ts, ibor);
                vm_break;
            }
            vm_case(OP_BXOR) {
                op_bitwise(ts, ibxor);
                vm_break;
            }
            /* } CONCAT_OP { */
            vm_case(OP_CONCAT) { /* TODO: concat more than 2 at a time */
                Protect(csV_concat(ts, 2)); /* 'csV_concat handles 'sp' */
                checkGC(ts);
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
                    cond = csi_numeq(fval(v1), cast_num(imm_i));
                else
                    cond = 0;
                setorderres(v1, cond, condexp);
                vm_break;
            }
            vm_case(OP_LTI) {
                op_orderI(ts, ilt, csi_numlt);
                vm_break;
            }
            vm_case(OP_LEI) {
                op_orderI(ts, ile, csi_numle);
                vm_break;
            }
            vm_case(OP_GTI) {
                op_orderI(ts, igt, csi_numgt);
                vm_break;
            }
            vm_case(OP_GEI) {
                op_orderI(ts, ige, csi_numge);
                vm_break;
            }
            vm_case(OP_EQ) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                int condexp = fetchs(); /* iseq */
                int cond;
                Protect(cond = csV_ordereq(ts, v1, v2));
                setorderres(v1, cond, condexp);
                pop(1); /* v2 */
                vm_break;
            }
            vm_case(OP_LT) {
                op_order(ts, ilt, csi_numlt, otherlt);
                vm_break;
            }
            vm_case(OP_LE) {
                op_order(ts, ile, csi_numle, otherle);
                vm_break;
            }
            vm_case(OP_EQPRESERVE) {
                SPtr res = TOPS();
                TValue *v1 = peek(1);
                TValue *v2 = s2v(res);
                Protect(csV_ordereq(ts, v1, v2));
                setobj2s(ts, res, s2v(ts->sp.p));
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
                SPtr res = TOPS();
                TValue *v = s2v(res);
                if (ttisint(v)) {
                    cs_Integer i = ival(v);
                    setival(v, csi_intop(-, 0, i));
                } else if (ttisflt(v)) {
                    cs_Number n = fval(v);
                    setfval(v, csi_numunm(ts, n));
                } else
                    Protect(csMM_tryunary(ts, v, res, CS_MM_UNM));
                vm_break;
            }
            vm_case(OP_BNOT) {
                SPtr res = TOPS();
                TValue *v = s2v(res);
                if (ttisint(v)) {
                    cs_Integer i = ival(v);
                    setival(v, csi_intop(^, ~csi_castS2U(0), i));
                } else
                    Protect(csMM_tryunary(ts, v, res, CS_MM_BNOT));
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
            } /* } TEST_OPS { */
            vm_case(OP_TEST) {
                TValue *v = peek(0);
                int off = fetchl();
                int condexp = fetchs();
                int cond = !c_isfalse(v);
                if (cond == condexp)
                    pc += off;
                vm_break;
            }
            vm_case(OP_TESTORPOP) {
                TValue *v = peek(0);
                int off = fetchl();
                int condexp = fetchs();
                int cond = !c_isfalse(v);
                if (cond == condexp)
                    pc += off;
                else
                    pop(1); /* v */
                vm_break;
            }
            vm_case(OP_TESTANDPOP) {
                TValue *v = peek(0);
                int off = fetchl();
                int condexp = fetchs();
                int cond = !c_isfalse(v);
                if (cond == condexp) {
                    pc += off;
                    pop(1); /* v */
                }
                vm_break;
            }
            vm_case(OP_TESTPOP) {
                TValue *v = peek(0);
                int off = fetchl();
                int condexp = fetchs();
                int cond = !c_isfalse(v);
                if (cond == condexp)
                    pc += off;
                pop(1); /* v */
                vm_break;
            } /* } */
            vm_case(OP_CALL) {
                CallFrame *newcf;
                SPtr func = STK(fetchl());
                int nres = fetchl() - 1;
                savepc(ts);
                if ((newcf = precall(ts, func, nres)) != NULL) {
                    cf = newcf;
                    goto startfunc;
                } /* otherwise it was a C call or a class with no __init */
                vm_break;
            }
            vm_case(OP_CLOSE) {
                SPtr level = base + fetchl();
                Protect(csF_close(ts, level, CS_OK));
                vm_break;
            }
            vm_case(OP_TBC) {
                SPtr level = base + fetchl();
                Protect(csF_newtbcvar(ts, level));
                vm_break;
            }
            vm_case(OP_GETGLOBAL) {
                TValue *key = K(fetchl());
                TValue *G = getGtable(ts);
                const TValue *val = csH_getstr(htval(G), strval(key));
                if (!isempty(val)) {
                    setobj2s(ts, ts->sp.p++, val);
                } else
                    setnilval(s2v(ts->sp.p++));
                vm_break;
            }
            vm_case(OP_SETGLOBAL) {
                TValue *key = K(fetchl());
                TValue *G = getGtable(ts);
                TValue *v = peek(0);
                cs_assert(ttisstring(key));
                csH_set(ts, htval(G), key, v);
                pop(1); /* v */
                vm_break;
            }
            vm_case(OP_GETLOCAL) {
                int i = fetchl();
                setobjs2s(ts, ts->sp.p++, STK(i));
                vm_break;
            }
            vm_case(OP_SETLOCAL) {
                int i = fetchl();
                setobjs2s(ts, STK(i), pop(1));
                vm_break;
            }
            vm_case(OP_GETUVAL) {
                int i = fetchl();
                setobj2s(ts, ts->sp.p++, cl->upvals[i]->v.p);
                vm_break;
            }
            vm_case(OP_SETUVAL) {
                int i = fetchl();
                setobj(ts, cl->upvals[i]->v.p, s2v(pop(1)));
                vm_break;
            }
            vm_case(OP_SETARRAY) {
                uint last = fetchl(); /* num of elems. already in the array */
                int n = fetchs(); /* num of elems. to store */
                SPtr sa = SLOT(n); /* array stack slot */
                Array *a = arrval(s2v(sa));
                if (n == 0)
                    n = (ts->sp.p - sa) - 1; /* get up to the top */
                last += n;
                if (last > a->sz) /* need more space? */
                    csA_ensure(ts, a, last - 1); /* last-1 index must fit */
                for (; n > 0; n--) { /* set the values from stack... */
                    /* ...into the array in reverse order */
                    TValue *v = s2v(sa + n);
                    setobj(ts, &a->b[last - 1], v);
                    last--;
                    csG_barrierback(ts, obj2gco(a), v);
                }
                ts->sp.p = sa + 1; /* pop off elements */
                vm_break;
            }
            vm_case(OP_SETPROPERTY) { /* NOTE: optimize? */
                TValue *o = peek(fetchl());
                TValue *prop = K(fetchl());
                TValue *v = peek(0);
                cs_assert(ttisstring(prop));
                Protect(csV_set(ts, o, prop, v));
                pop(1); /* v */
                vm_break;
            }
            vm_case(OP_GETPROPERTY) { /* NOTE: optimize? */
                TValue *prop = K(fetchl());
                TValue *v = peek(0);
                cs_assert(ttisstring(prop));
                Protect(csV_get(ts, v, prop, TOPS()));
                vm_break;
            }
            vm_case(OP_GETINDEX) {
                TValue *o = peek(1);
                TValue *key = peek(0);
                Protect(csV_get(ts, o, key, SLOT(1)));
                pop(1); /* v2 */
                vm_break;
            }
            vm_case(OP_SETINDEX) {
                SPtr os = SLOT(fetchl());
                TValue *o = s2v(os);
                TValue *idx = s2v(os + 1);
                TValue *v = peek(0);
                Protect(csV_set(ts, o, idx, v));
                pop(1); /* v */
                vm_break;
            }
            vm_case(OP_GETINDEXSTR) { /* TODO: optimize */
                TValue *i = K(fetchl());
                TValue *v = peek(0);
                cs_assert(ttisstring(i));
                Protect(csV_get(ts, v, i, TOPS()));
                vm_break;
            }
            vm_case(OP_SETINDEXSTR) { /* TODO: optimize */
                TValue *o = peek(fetchl());
                TValue *idx = K(fetchl());
                TValue *v = peek(0);
                cs_assert(ttisstring(idx));
                Protect(csV_set(ts, o, idx, v));
                pop(1); /* v */
                vm_break;
            }
            vm_case(OP_GETINDEXINT) { /* TODO: optimize */
                TValue i;
                TValue *v = peek(0);
                setival(&i, fetchl());
                Protect(csV_get(ts, v, &i, TOPS()));
                vm_break;
            }
            vm_case(OP_SETINDEXINT) { /* TODO: optimize */
                TValue *o = peek(fetchl());
                TValue *v = peek(0);
                cs_Integer imm_i = fetchl();
                TValue index;
                setival(&index, imm_i);
                Protect(csV_set(ts, o, &index, v));
                pop(1); /* v */
                vm_break;
            }
            vm_case(OP_GETSUP) {
                TValue *key = K(fetchl());
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                savepc(ts);
                getsuper(ts, insval(v1), classval(v2), strval(key), SLOT(1),
                             csH_getstr);
                pop(1); /* v2 */
                vm_break;
            }
            vm_case(OP_GETSUPIDX) {
                TValue *v1 = peek(2);
                TValue *v2 = peek(1);
                TValue *i = peek(0);
                savepc(ts);
                getsuper(ts, insval(v1), classval(v2), i, SLOT(2), csH_get);
                pop(2); /* v2,v3 */
                vm_break;
            }
            vm_case(OP_GETSUPIDXSTR) {
                TValue *key = K(fetchl());
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                savepc(ts);
                getsuper(ts, insval(v1), classval(v2), strval(key), SLOT(1),
                             csH_getstr);
                pop(1); /* v2 */
                vm_break;
            }
            vm_case(OP_INHERIT) {
                TValue *o = peek(1);
                OClass *cls = classval(peek(0));
                OClass *src;
                savepc(ts);
                if (c_unlikely(!ttisclass(o)))
                    csD_runerror(ts, "inheriting a non-class value");
                src = classval(o);
                if (c_likely(src->methods)) { /* 'src' has methods ? */
                    cs_assert(cls->methods == NULL);
                    cls->methods = csH_new(ts);
                    csH_copykeys(ts, src->methods, cls->methods);
                }
                pop(2); /* remove class objects */
                vm_break;
            }
            vm_case(OP_FORPREP) {
                SPtr stk = STK(fetchl());
                int off = fetchl();
                Protect(csF_newtbcvar(ts, stk + FORTBCVAR));
                pc += off;
                cs_assert(*pc == OP_FORCALL);
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
                memcpy(stk+NSTATEVARS, stk, FORTBCVAR*sizeof(stk));
                ts->sp.p = stk + NSTATEVARS + FORTBCVAR;
                Protect(csV_call(ts, stk + NSTATEVARS, nres));
                updatebase(cf);
                cs_assert(*pc == OP_FORLOOP);
                goto l_forloop;
            }}
            vm_case(OP_FORLOOP) {
            l_forloop: {
                SPtr stk = STK(fetchl());
                int off = fetchl();
                if (!ttisnil(s2v(stk + NSTATEVARS))) { /* continue loop? */
                    /* save control variable */
                    setobjs2s(ts, stk + FORCNTLVAR, stk + NSTATEVARS);
                    pc -= off; /* jump back */
                }
                vm_break;
            }}
            vm_case(OP_RET) {
                SPtr stk = STK(fetchl());
                int nres = fetchl() - 1; /* number of results */
                if (nres < 0) /* not fixed ? */
                    nres = ts->sp.p - stk;
                savepc(ts);
                if (fetchs()) { /* have open upvalues? */
                    csF_close(ts, base, CLOSEKTOP);
                    updatebase(cf);
                }
                if (cl->p->isvararg) /* vararg function ? */
                    cf->func.p -= cf->nvarargs + cl->p->arity + 1;
                ts->sp.p = stk + nres;
                poscall(ts, cf, nres);
                if (cf->status & CFST_FRESH) { /* top-level function? */
                    return; /* end this frame */
                } else {
                    cf = cf->prev; /* return to caller */
                    goto returning; /* continue running in this frame */
                }
            }
        }
    }
}
