/*
** cvm.c
** CScript Virtual Machine
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include <string.h>
#include <stdio.h>

#include "capi.h"
#include "carray.h"
#include "csconf.h"
#include "cfunction.h"
#include "cgc.h"
#include "chashtable.h"
#include "cmem.h"
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


/*
** By default, use goto jump table in the interpreter loop
** if compiler supports precomputed goto.
*/
#if !defined(CS_USE_JUMPTABLE)
#if defined(__GNUC__)
#define CS_USE_JUMPTABLE	1
#else
#define CS_USE_JUMPTABLE	0
#endif
#endif


static int booleans[2] = { CS_VFALSE, CS_VTRUE };


/*
** Allocate new Cript closure, push it on stack and
** initialize its upvalues.
*/
static void pushclosure(cs_State *ts, Function *fn, UpVal **enc, SPtr base) {
    int nupvals = fn->sizeupvals;
    CrClosure *cl = csF_newCrClosure(ts, nupvals);
    cl->fn = fn;
    setcrcl2s(ts, ts->sp.p++, cl); /* anchor to stack */
    for (int i = 0; i < nupvals; i++) {
        UpValInfo *uv = &fn->upvals[i];
        if (uv->onstack)
            cl->upvals[i] = csF_findupval(ts, base + uv->idx);
        else
            cl->upvals[i] = enc[uv->idx];
        csG_objbarrier(ts, cl, cl->upvals[i]);
    }
}


/*
** Allocate new array, push it on stack and pop 'elems' values off the
** stack into the array.
*/
static void pusharray(cs_State *ts, int size, int elems) {
    Array *arr = csA_new(ts);
    if (size < 0) /* unbounded ? */
        arr->sz = elems; /* equal to number of elements */
    else /* fixed size */
        arr->sz = size;
    if (arr->sz > 0) {
        setarr2s(ts, ts->sp.p++, arr); /* anchor array */
        arr->b = csM_newarray(ts, arr->sz, TValue);
        ts->sp.p--; /* pop array */
        while (elems) { /* set all elements from stack into array */
            elems--;
            setobj(ts, &arr->b[elems], s2v(ts->sp.p - elems - 1));
        }
    }
    ts->sp.p -= elems - 1; /* remove all elems but one slot for array */
    setarr2s(ts, ts->sp.p - 1, arr); /* push array to stack */
}


/* allocate new class and push it on stack */
static void pushclass(cs_State *ts) {
    OClass *cls = csMM_newclass(ts);
    setcls2s(ts, ts->sp.p++, cls); /* anchor to stack */
}


/*
 * Integer division; handles division by 0 and possible
 * overflow if 'y' == '-1' and 'x' == CS_INTEGER_MIN.
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
 * Integer modulus; handles modulo by 0 and overflow
 * as explained in 'csV_div()'.
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
 * Perform binary arithmetic operations on objects, this function is free
 * to call overloaded methods in cases where raw arithmetics are not possible.
 */
void csV_binarithm(cs_State *ts, const TValue *v1, const TValue *v2, SPtr res,
                   int op) {
    if (!csO_arithmraw(ts, v1, v2, s2v(res), op))
        csMM_trybin(ts, v1, v2, res, (op - CS_OPADD) + CS_MM_ADD);
}


/*
 * Perform unary arithmetic operations on objects, this function is free
 * to call overloaded methods in cases where raw arithmetics are not possible.
 */
void csV_unarithm(cs_State *ts, const TValue *v, SPtr res, int op) {
    TValue aux;
    setival(&aux, 0);
    if (!csO_arithmraw(ts, v, &aux, s2v(res), op))
        csMM_tryunary(ts, v, res, (op - CS_OPUNM) + CS_MM_UNM);
}


/* set 'vmt' entry */
static void setmm(cs_State *ts, TValue **vmt, TValue *fn, int vmtt) {
    cs_assert(0 <= vmtt && vmtt < csNUM_MM);
    if (c_unlikely(!(*vmt))) /* empty 'vmt' */
        *vmt = csMM_newvmt(ts);
    (*vmt)[vmtt] = *fn; /* set the entry */
}


/*
 * According to C99 6.3.1.8 page 45:
 * "...if the corresponding real type of either operand is double, the other
 * operand is converted, without change of type domain, to a type whose
 * corresponding real type is double."
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
    if (ttisstr(v1) && ttisstr(v2))
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
    if (ttisstr(v1) && ttisstr(v2))
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
    case CS_VLUDATA: return (pval(v1) == pval(v2));
    case CS_VSTRING: return csS_eq(strval(v1), strval(v2));
    case CS_VUDATA: {
        if (udval(v1) == udval(v2)) return 1;
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
        return !csi_isfalse(s2v(ts->sp.p));
    }
}


/* 
** Check if global exists and return it, othwerwise invoke
** undefined global variable error.
*/
cs_sinline const TValue *checkglobal(cs_State *ts, TValue *key) {
    const TValue *out = csH_get(htval(&G_(ts)->ginstance), key);
    if (c_unlikely(isabstkey(out)))
        csD_globalerror(ts, "undefined", strval(key));
    return out;
}


cs_sinline void defineglobal(cs_State *ts, TValue *key, TValue *val) {
    cs_assert(ttisstr(key));
    const TValue *slot = csH_get(htval(&G_(ts)->ginstance), key);
    if (c_unlikely(!isabstkey(slot)))
        csD_runerror(ts, "global variable '%s' redefinition", cstrval(key));
    else
        csH_set(ts, htval(&G_(ts)->ginstance), key, val);
}


/* get global variable value */
cs_sinline void getglobal(cs_State *ts, TValue *key, TValue *out) {
    cs_assert(ttisstr(key));
    setobj(ts, out, checkglobal(ts, key));
}


/* set global variable value */
cs_sinline void setglobal(cs_State *ts, TValue *key, TValue *newval) {
    if (c_unlikely(isconst(checkglobal(ts, key))))
        csD_globalerror(ts, "read-only", strval(key));
    csH_set(ts, htval(&G_(ts)->ginstance), key, newval);
}


static void arrayseti(cs_State *ts, Array *arr, const TValue *index,
                      const TValue *val) {
    cs_Integer i;
    if (c_likely(tointeger(index, &i))) { /* index is integer? */
        if (c_likely(0 <= i)) { /* non-negative index */
            if (c_unlikely(i >= ARRAYLIMIT)) /* too large 'index'? */
                csD_indexerror(ts, i, "too large");
            csA_ensure(ts, arr, i); /* expand block */
            setobj(ts, &arr->b[i], val); /* set the value at index */
        } else { /* negative index (error) */
            csD_indexerror(ts, i, "negative");
        }
    } else {
        csD_indextypeerror(ts, index);
    }
}


void csV_set(cs_State *ts, const TValue *obj, const TValue *key,
             const TValue *val) {
    if (ttisarr(obj)) { /* array object? */
        arrayseti(ts, arrval(obj), key, val);
    } else {
        const TValue *fmm = csMM_get(ts, obj, CS_MM_SETIDX);
        if (!ttisnil(fmm)) /* have metamethod ? */
            csMM_callhtm(ts, fmm, obj, key, val);
        else if (ttisins(obj)) /* object is instance ? */
            csH_set(ts, &insval(obj)->fields, key, val);
        else /* error */
            csD_typeerror(ts, obj, "index");
    }
}


static void arraygeti(cs_State *ts, Array *arr, const TValue *index, SPtr res) {
    cs_Integer i;
    if (c_likely(tointeger(index, &i))) { /* index is integer? */
        if (i >= 0) { /* positive index? */
            if (c_unlikely(i >= ARRAYLIMIT)) { /* too large index? */
                csD_indexerror(ts, i, "too large");
            } else if (i < arr->sz) { /* index in array block? */
                setobj2s(ts, res, &arr->b[i]);
            } else /* out of bounds */
                setnilval(s2v(res));
        } else { /* negative index (error) */
            csD_indexerror(ts, i, "negative");
        }
    } else {
        csD_indextypeerror(ts, index);
    }
}


/* bind method to instance and set it at 'res' */
#define bindmethod(ts,ins,fn,res) \
    setim2s(ts, res, csMM_newinsmethod(ts, ins, v))


void csV_get(cs_State *ts, const TValue *obj, const TValue *key, SPtr res) {
    if (ttisarr(obj)) { /* array object? */
        arraygeti(ts, arrval(obj), key, res);
    } else {
        Instance *ins;
        const TValue *fmm, *v;
        fmm = csMM_get(ts, obj, CS_MM_GETIDX);
        if (!ttisnil(fmm)) { /* have metamethod ? */
            csMM_callhtmres(ts, fmm, obj, key, res);
        } else { /* otherwise perform raw access */
            if (c_unlikely(ttypetag(obj) != CS_VINSTANCE))
                csD_typeerror(ts, obj, "index");
            ins = insval(obj);
            v = csH_get(&ins->fields, key);
            if (!isabstkey(v)) { /* have field ? */
                setobj2s(ts, res, v);
                return;
            } else if (ins->oclass->methods) { /* have methods table? */
                v = csH_get(ins->oclass->methods, key);
                if (!isabstkey(v)) { /* have method ? */
                    /* bind it to instance and set 'res'  */
                    bindmethod(ts, ins, v, res)
                    return;
                } /* else fallthrough */
            }
            setnilval(s2v(res));
        }
    }
}


void csV_getsuper(cs_State *ts, Instance *ins, OClass *cls, const TValue *s,
                  SPtr res) {
    if (cls->methods) { /* superclass has methods ? */
        const TValue *v = csH_get(cls->methods, s);
        if (!isabstkey(v))
            bindmethod(ts, ins, v, res)
    } else { /* no methods; set nil */
        setnilval(s2v(res));
    }
}


/* 'dest' inherits methods from 'obj' (if any) */
cs_sinline void inherit(cs_State *ts, const TValue *obj, OClass *dest) {
    OClass *src;
    if (c_unlikely(!ttiscls(obj)))
        csD_runerror(ts, "inherit a non-class value");
    src = clsval(obj);
    if (c_likely(src->methods)) { /* 'src' has methods ? */
        cs_assert(dest->methods == NULL);
        dest->methods = csH_new(ts);
        csH_copykeys(ts, src->methods, dest->methods);
    }
}


/*
** Call binary meta method, but before that perform a quick check
** and invoke error if types don't match, the values are instances
** that belong to different classes or v1 (self) doesn't have the
** overloaded method.
*/
cs_sinline void precallmbin(cs_State *ts, const TValue *v1, const TValue *v2,
                            cs_MM op, SPtr res) {
    const TValue *func;
    const char *opname = getstrbytes(G_(ts)->mmnames[op]);
    if (c_unlikely(ttypetag(v1) != ttypetag(v2)))
        csD_typeerrormeta(ts, v1, v2, opname);
    if (c_unlikely(ttisins(v1) && insval(v1)->oclass != insval(v2)->oclass))
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


cs_sinline CallFrame *prepcallframe(cs_State *ts, SPtr func, int nret,
                                    int mask, SPtr top) {
    CallFrame *cf = (ts->cf->next ? ts->cf->next : csT_newcf(ts));
    cf->func.p = func;
    cf->top.p = top;
    cf->nresults = nret;
    cf->status = mask;
    return cf;
}


cs_sinline int precallC(cs_State *ts, SPtr func, int nres, cs_CFunction f) {
    CallFrame *cf;
    int n; /* number of returns */
    checkstackGCp(ts, CS_MINSTACK, func);
    ts->cf = cf = prepcallframe(ts, func, nres, CFST_CCALL, ts->sp.p+CS_MINSTACK);
    cs_unlock(ts);
    n = (*f)(ts);
    cs_lock(ts);
    api_checknelems(ts, n);
    poscall(ts, cf, n);
    return n;
}


cs_sinline SPtr adjustffunc(cs_State *ts, SPtr func, const TValue *f) {
    checkstackGCp(ts, 1, func); /* space for 'f' */
    for (SPtr p = func; p < ts->sp.p; p--)
        setobjs2s(ts, p, p-1);
    ts->sp.p++;
    setobj2s(ts, func, f);
    return func;
}


cs_sinline SPtr trymmcall(cs_State *ts, SPtr func) {
    const TValue *f;
    f = csMM_get(ts, s2v(func), CS_MM_CALL);
    if (c_unlikely(ttisnil(f)))
        csD_callerror(ts, s2v(func));
    return adjustffunc(ts, func, f);
}


CallFrame *precall(cs_State *ts, SPtr func, int nres) {
retry:
    switch (ttypetag(s2v(func))) {
        case CS_VCCL: { /* C closure */
            precallC(ts, func, CS_MULRET, cclval(s2v(func))->fn);
            return NULL;
        }
        case CS_VCFUNCTION: { /* light C function */
            precallC(ts, func, CS_MULRET, cfval(s2v(func)));
            return NULL;
        }
        case CS_VCLASS: { /* Class */
            const TValue *fmm;
            Instance *ins = csMM_newinstance(ts, clsval(s2v(func)));
            setins2s(ts, func, ins); /* replace class with its instance */
            fmm = csMM_get(ts, s2v(func), CS_MM_INIT);
            if (!ttisnil(fmm)) { /* have '__init' ? */
                func = adjustffunc(ts, func, fmm);
                cs_assert(ttiscl(s2v(func)) || ttiscfn(s2v(func)));
                goto retry; /* call '__init' */
            } else {
                return NULL; /* otherwise done */
            }
        }
        case CS_VCRCL: { /* Cript function (closure) */
            CallFrame *cf;
            Function *fn = crclval(s2v(func))->fn;
            int nargs = (ts->sp.p - func) - 1;
            int fsize = fn->maxstack;
            checkstackGCp(ts, fsize, func);
            ts->cf = cf = prepcallframe(ts, func, nres, 0, func+fsize+1);
            for (; nargs < fn->arity; nargs++)
                setnilval(s2v(ts->sp.p++));
            cs_assert(cf->top.p <= ts->stackend.p);
            return cf;
        }
        default: {
            func = trymmcall(ts, func);
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


#define isemptystr(v)   (cs_assert(ttisstr(v)), lenstr(v) == 0)

#define MAXSHRSTRLEN    95

static void copy2buff(SPtr top, int n, char *buff) {
    size_t done = 0;
    do {
        OString *s = strval(s2v(top - n));
        size_t len = getstrlen(s);
        memcpy(&buff[done], getstrbytes(s), len * sizeof(char));
        done += len;
    } while (--n > 0);
}


void csV_concat(cs_State *ts, int total) {
    if (total == 1)
        return; /* done */
    do {
        SPtr top = ts->sp.p;
        int n = 2; /* number of elements (minimum 2) */
        if (!(ttisstr(s2v(top - 2)) && ttisstr(s2v(top - 1))))
            csMM_tryconcat(ts);
        else if (isemptystr(s2v(top - 1))) /* second operand is empty string? */
            ; /* result already in the first operand */
        else if (isemptystr(s2v(top - 2))) { /* first operand is empty string? */
            setobjs2s(ts, top - 2, top - 1); /* result is second operand */
        } else { /* at least 2 non-empty strings */
            cs_assert(ttisstr(s2v(top - 2)) && ttisstr(s2v(top - 1)));
            size_t ltotal = lenstr(s2v(top - 1));
            /* collect total length and number of strings */
            for (n = 1; n < total && ttisstr(s2v(top - n - 1)); n++) {
                size_t len = lenstr(s2v(top - n - 1));
                if (c_unlikely(len >= SIZE_MAX - sizeof(OString) - ltotal)) {
                    ts->sp.p = top - total; /* pop strings */
                    csD_runerror(ts, "string length overflow");
                }
                ltotal += len;
            }
            OString *s;
            if (ltotal <= MAXSHRSTRLEN) {
                char buff[MAXSHRSTRLEN];
                copy2buff(top, n, buff);
                s = csS_newl(ts, buff, ltotal);
            } else {
                s = csS_newlobj(ts, ltotal);
                copy2buff(top, n, getstrbytes(s));
            }
            setstrval2s(ts, top - n, s);
        }
        total -= n - 1; /* got 'n' strings to create one new */
        ts->sp.p -= n - 1; /* popped 'n' strings and pushed one */
    } while (total > 1);
}


/* ------------------------------------------------------------------------
** Macros for arithmetic/bitwise/comparison instructions on integers.
**------------------------------------------------------------------------- */

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
    TValue *lk = getlK(); \
    op_arithKf_aux(ts, v, lk, fop); }


/* arithmetic operations with constant operand */
#define op_arithK(ts,iop,fop) { \
    TValue *v = peek(0); \
    TValue *lk = getlK(); \
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
    TValue *lk = getlK(); /* L */\
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
        protect(cond = other(ts, v1, v2)); \
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


/* ------------------------------------------------------------------------
 * Interpreter loop
 * ------------------------------------------------------------------------ */

/* update current function stack base */
#define updatebase(cf)      (base = (cf)->func.p + 1)

/* store current 'pc' */
#define storepc(ts)         (cf->pc = pc)

/* protect code that can raise errors or change the stack */
#define protect(e)          (storepc(ts), (e))

#if 0
/* fetch and trace the instruction */
#define fetch()         csTR_tracepc(cl->fn, pc)
#else
/* fetch instruction */
#define fetch()         (*pc++)
#endif

/* fetch short instruction parameter */
#define fetchs()        fetch()
/* fetch long instruction parameter */
#define fetchl()        (pc+=SIZEARGL, get3bytes(pc-SIZEARGL))

/* get constant */
#define getlK()         (k + fetchl())
#define getsK()         (k + fetchs())
/* get string constant */
#define getKStr()       (strval(getlK()))

/* get sign value */
#define getsign()       (fetchs() - 1)

/* peek stack (from (sp - 1) - n) */
#define peek(n)         s2v((ts->sp.p - 1) - n)
/* pop stack */
#define pop(n)          (ts->sp.p -= (n))

/* get stack slot (starting from 'base') */
#define STK(i)      (base+(i))
/* get stack slot (starting from top) */
#define SPTR(i)     (ts->sp.p-(i)-1)
/* get stack top - 1 */
#define TOPS()      SPTR(0)
/* get private variable */
#define PVAR(i)     (&cl->fn->private[i].val)



/* In case 'PRECOMPUTED_GOTO' not available. */
#define vm_dispatch(x)      switch(x)
#define vm_case(l)          case l:
#define vm_break            break


void csV_execute(cs_State *ts, CallFrame *cf) {
    register const Instruction *pc; /* program counter */
    register CrClosure *cl; /* closure being executed */
    register TValue *k; /* array of constants */
    register SPtr base; /* function base stack index */
#if CS_USE_JUMPTABLE
#include "cjmptable.h"
#endif
startfunc:
returning:
    cl = crclval(s2v(cf->func.p));
    k = cl->fn->k;
    pc = cl->fn->code;
    base = cf->func.p + 1;
    for (;;) {
        vm_dispatch(fetch()) {
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
                int L = fetchl();
                while (L--)
                    setnilval(s2v(ts->sp.p++));
                vm_break;
            } 
            vm_case(OP_CONST) {
                TValue *sk = getsK();
                setobj2s(ts, ts->sp.p++, sk);
                vm_break;
            }
            vm_case(OP_CONSTL) {
                TValue *lk = getlK();
                setobj2s(ts, ts->sp.p++, lk);
                vm_break;
            }
            vm_case(OP_CONSTI) {
                int L = fetchl(); /* int */
                int S = fetchs(); /* sign */
                setival(s2v(ts->sp.p++), L*S);
                vm_break;
            }
            vm_case(OP_CONSTF) {
                int L = fetchl(); /* flt */
                int S = getsign(); /* sign */
                setfval(s2v(ts->sp.p++), cast_num(L*S));
                vm_break;
            }
            vm_case(OP_VARARGPREP) {
                int L = fetchl();
                protect(csF_adjustvarargs(ts, L, cf, cl->fn));
                updatebase(cf); /* update base, it changed */
                vm_break;
            }
            vm_case(OP_VARARG) {
                int L = fetchl(); /* how many */
                protect(csF_getvarargs(ts, cf, L));
                vm_break;
            }
            vm_case(OP_CLOSURE) {
                int L = fetchl();
                Function *fn = cl->fn->funcs[L];
                protect(pushclosure(ts, fn, cl->upvals, base));
                vm_break;
            }
            vm_case(OP_ARRAY) {
                int size = fetchl() - 1;
                protect(pusharray(ts, size, 0));
                vm_break;
            }
            vm_case(OP_ARRAYELEMS) {
                int size = fetchl() - 1;
                int elems = ts->sp.p - STK(fetchl());
                cs_assert((0 <= size) == (elems <= size));
                protect(pusharray(ts, size, elems));
                vm_break;
            }
            vm_case(OP_CLASS) {
                protect(pushclass(ts));
                vm_break;
            }
            vm_case(OP_METHOD) {
                TValue *v1 = peek(1); /* class */
                TValue *v2 = peek(0); /* method */
                TValue *key = getlK();
                cs_assert(ttisstr(key));
                protect(csH_set(ts, clsval(v2)->methods, key, v1));
                vm_break;
            }
            vm_case(OP_SETMM) {
                TValue *v1 = peek(1); /* class or userdata */
                TValue *v2 = peek(0); /* func */
                int S = fetchs(); /* VMT index */
                protect(setmm(ts, &clsval(v1)->vmt, v2, S));
                vm_break;
            }
            vm_case(OP_POP) {
                pop(1);
                vm_break;
            }
            vm_case(OP_POPN) {
                int L = fetchl();
                pop(L);
                vm_break;
            }
            /* } BINARY_OPS { ARITHMETIC_OPS { */
            vm_case(OP_MBIN) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                int S = fetchs(); /* op */
                protect(precallmbin(ts, v1, v2, S, SPTR(1)));
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
                storepc(ts); /* in case of division by 0 */
                op_arithK(ts, csV_div, csi_numdiv);
                vm_break;
            }
            vm_case(OP_MODK) {
                storepc(ts); /* in case of division by 0 */
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
                storepc(ts); /* in case of division by 0 */
                op_arithI(ts, csV_div, csi_numdiv);
                vm_break;
            }
            vm_case(OP_MODI) {
                storepc(ts); /* in case of division by 0 */
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
                storepc(ts); /* in case of division by 0 */
                op_arith(ts, csV_div, csi_numdiv);
                vm_break;
            }
            vm_case(OP_MOD) {
                storepc(ts); /* in case of division by 0 */
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
                protect(csV_concat(ts, 2)); /* csV_concat handles stack ptr */
                csG_check(ts);
                vm_break;
            }
            /* } ORDERING_OPS { */
            vm_case(OP_EQK) {
                TValue *v1 = peek(0);
                const TValue *v2 = getlK(); /* L */
                int S = fetchs(); /* iseq */
                setorderres(v1, csV_raweq(v1, v2), S);
                vm_break;
            }
            vm_case(OP_EQI) {
                TValue *v1 = peek(0);
                int cond;
                int L = fetchl(); /* imm */
                int S1 = getsign(); /* sign */
                int S2 = fetchs(); /* iseq */
                L *= S1;
                if (ttisint(v1))
                    cond = (ival(v1) == L);
                else if (ttisflt(v1))
                    cond = csi_numeq(fval(v1), cast_num(L));
                else
                    cond = 0;
                setorderres(v1, cond, S2);
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
                int S = fetchs(); /* iseq */
                int cond;
                protect(cond = csV_ordereq(ts, v1, v2));
                setorderres(v1, cond, S);
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
                protect(csV_ordereq(ts, v1, v2));
                setobj2s(ts, res, s2v(ts->sp.p));
                vm_break;
            }
            /* }} UNARY_OPS { */
            vm_case(OP_NOT) {
                TValue *v = peek(0);
                if (csi_isfalse(v))
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
                } else {
                    protect(csMM_tryunary(ts, v, res, CS_MM_UNM));
                }
                vm_break;
            }
            vm_case(OP_BNOT) {
                SPtr res = TOPS();
                TValue *v = s2v(res);
                if (ttisint(v)) {
                    cs_Integer i = ival(v);
                    setival(v, csi_intop(^, ~csi_castS2U(0), i));
                } else {
                    protect(csMM_tryunary(ts, v, res, CS_MM_BNOT));
                }
                vm_break;
            }
            /* } JMP_OPS { */
            vm_case(OP_JMP) {
                int L = fetchl();
                pc += L;
                vm_break;
            }
            vm_case(OP_JMPS) {
                int L = fetchl();
                pc -= L;
                vm_break;
            }
            /* } TEST_OPS { */
            vm_case(OP_TEST) {
                TValue *v = peek(0);
                int L = fetchl(); /* offset */
                int S = fetchs(); /* cond */
                int cond = !csi_isfalse(v);
                if (cond == S)
                    pc += L;
                vm_break;
            }
            vm_case(OP_TESTORPOP) {
                TValue *v = peek(0);
                int L = fetchl(); /* offset */
                int S = fetchs(); /* cond */
                int cond = !csi_isfalse(v);
                if (cond == S)
                    pc += L;
                else
                    pop(1); /* v */
                vm_break;
            }
            vm_case(OP_TESTANDPOP) {
                TValue *v = peek(0);
                int L = fetchl(); /* offset */
                int S = fetchs(); /* cond */
                int cond = !csi_isfalse(v);
                if (cond == S) {
                    pc += L;
                    pop(1); /* v */
                }
                vm_break;
            }
            vm_case(OP_TESTPOP) {
                TValue *v = peek(0);
                int L = fetchl(); /* offset */
                int S = fetchs(); /* cond */
                int cond = !csi_isfalse(v);
                if (cond == S)
                    pc += L;
                pop(1); /* v */
                vm_break;
            }
            /* } */
            vm_case(OP_CALL) {
                CallFrame *newcf;
                SPtr func = STK(fetchl());
                int nresults = fetchl() - 1;
                storepc(ts);
                if ((newcf = precall(ts, func, nresults)) != NULL) {
                    cf = newcf;
                    goto startfunc;
                } /* otherwise it was a C call or a class with no '__init' */
                vm_break;
            }
            vm_case(OP_CLOSE) {
                SPtr level = base + fetchl();
                protect(csF_close(ts, level, CS_OK));
                vm_break;
            }
            vm_case(OP_TBC) {
                SPtr level = base + fetchl();
                protect(csF_newtbcvar(ts, level));
                vm_break;
            }
            vm_case(OP_GETLOCAL) {
                int L = fetchl();
                setobjs2s(ts, ts->sp.p++, STK(L));
                vm_break;
            }
            vm_case(OP_SETLOCAL) {
                int L = fetchl();
                setobjs2s(ts, STK(L), pop(1));
                vm_break;
            }
            vm_case(OP_GETPRIVATE) {
                int L = fetchl();
                setobj2s(ts, ts->sp.p++, PVAR(L));
                vm_break;
            }
            vm_case(OP_SETPRIVATE) {
                int L = fetchl();
                setobj(ts, PVAR(L), s2v(pop(1)));
                vm_break;
            }
            vm_case(OP_GETUVAL) {
                int L = fetchl();
                setobj2s(ts, ts->sp.p++, cl->upvals[L]->v.p);
                vm_break;
            }
            vm_case(OP_SETUVAL) {
                int L = fetchl();
                setobj(ts, cl->upvals[L]->v.p, s2v(pop(1)));
                vm_break;
            }
            vm_case(OP_DEFGLOBAL) {
                TValue* s = getlK();
                protect(defineglobal(ts, s, peek(0)));
                pop(1);
                vm_break;
            }
            vm_case(OP_GETGLOBAL) {
                TValue *s = getlK();
                protect(getglobal(ts, s, s2v(ts->sp.p)));
                ts->sp.p++;
                vm_break;
            }
            vm_case(OP_SETGLOBAL) {
                TValue *s = getlK();
                protect(setglobal(ts, s, peek(0)));
                pop(1);
                vm_break;
            }
            vm_case(OP_SETPROPERTY) { /* optimize ? */
                TValue *s = getlK();
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                cs_assert(ttisstr(s));
                protect(csV_set(ts, v1, s, v2));
                pop(2); /* v1,v2 */
                vm_break;
            }
            vm_case(OP_GETPROPERTY) { /* optimize ? */
                TValue *s = getlK();
                TValue *v = peek(0);
                cs_assert(ttisstr(s));
                protect(csV_get(ts, v, s, TOPS()));
                vm_break;
            }
            vm_case(OP_GETINDEX) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                protect(csV_get(ts, v1, v2, SPTR(1)));
                pop(1); /* v2 */
                vm_break;
            }
            vm_case(OP_SETINDEX) {
                TValue *v1 = peek(2);
                TValue *v2 = peek(1);
                TValue *v3 = peek(0);
                protect(csV_set(ts, v1, v2, v3));
                pop(3); /* v1,v2,v3 */
                vm_break;
            }
            vm_case(OP_GETINDEXSTR) { /* TODO: optimize */
                TValue *s = getlK();
                TValue *v = peek(0);
                cs_assert(ttisstr(s));
                protect(csV_get(ts, v, s, TOPS()));
                vm_break;
            }
            vm_case(OP_SETINDEXSTR) { /* TODO: optimize */
                TValue *s = getlK();
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                cs_assert(ttisstr(s));
                protect(csV_set(ts, v1, s, v2));
                vm_break;
            }
            vm_case(OP_GETINDEXINT) { /* TODO: optimize */
                TValue aux;
                TValue *v = peek(0);
                setival(&aux, fetchl());
                protect(csV_get(ts, v, &aux, TOPS()));
                vm_break;
            }
            vm_case(OP_SETINDEXINT) { /* TODO: optimize */
                TValue aux;
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                setival(&aux, fetchl());
                protect(csV_set(ts, v1, &aux, v2));
                vm_break;
            }
            vm_case(OP_GETSUP) {
                TValue *s = getlK();
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                cs_assert(ttisstr(s));
                protect(csV_getsuper(ts, insval(v1), clsval(v2), s, SPTR(1)));
                pop(1); /* v2 */
                vm_break;
            }
            vm_case(OP_GETSUPIDX) {
                TValue *v1 = peek(2);
                TValue *v2 = peek(1);
                TValue *v3 = peek(0);
                protect(csV_getsuper(ts, insval(v1), clsval(v2), v3, SPTR(2)));
                pop(2); /* v2,v3 */
                vm_break;
            }
            vm_case(OP_GETSUPIDXSTR) { /* TODO: optimize */
                TValue *s = getlK();
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                cs_assert(ttisstr(s));
                protect(csV_getsuper(ts, insval(v1), clsval(v2), s, SPTR(1)));
                pop(1); /* v2 */
                vm_break;
            }
            vm_case(OP_INHERIT) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                protect(inherit(ts, v1, clsval(v2)));
                pop(2); /* v1,v2 */
                vm_break;
            }
            vm_case(OP_FORPREP) {
                SPtr stk = STK(fetchl());
                int offset = fetchl();
                protect(csF_newtbcvar(ts, stk + FORTBCVAR));
                pc += offset;
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
                protect(csV_call(ts, stk + NSTATEVARS, nres));
                updatebase(cf);
                cs_assert(*pc == OP_FORLOOP);
                goto l_forloop;
            }}
            vm_case(OP_FORLOOP) {
            l_forloop: {
                SPtr stk = STK(fetchl());
                int offset = fetchl();
                if (!ttisnil(s2v(stk + NSTATEVARS))) { /* continue loop? */
                    /* save control variable */
                    setobjs2s(ts, stk + FORCNTLVAR, stk + NSTATEVARS);
                    pc -= offset; /* jump back */
                }
                vm_break;
            }}
            vm_case(OP_RET) {
                SPtr stk = STK(fetchl());
                int n = fetchl() - 1; /* number of results */
                if (n < 0) /* not fixed ? */
                    n = ts->sp.p - stk;
                storepc(ts);
                if (fetchs()) { /* have open upvalues? */
                    csF_close(ts, base, CLOSEKTOP);
                    updatebase(cf);
                }
                if (cl->fn->isvararg) /* vararg function ? */
                    cf->func.p -= cf->nvarargs + cl->fn->arity + 1;
                ts->sp.p = stk + n;
                poscall(ts, cf, n);
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
