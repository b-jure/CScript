/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "capi.h"
#include "cconf.h"
#include "cfunction.h"
#include "cgc.h"
#include "chashtable.h"
#include "cscript.h"
#include "climits.h"
#include "cmem.h"
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
#if !defined(CR_USE_JUMPTABLE)
#if defined(__GNUC__)
#define CR_USE_JUMPTABLE	1
#else
#define CR_USE_JUMPTABLE	0
#endif
#endif


static int booleans[2] = { CR_VFALSE, CR_VTRUE };


/*
** Allocate new Cript closure, push it on stack and
** initialize its upvalues.
*/
static void pushclosure(cr_State *ts, Function *fn, UpVal **enc, SPtr base) {
    int nupvals = fn->sizeupvals;
    CrClosure *cl = crF_newCrClosure(ts, nupvals);
    cl->fn = fn;
    setcrcl2s(ts, ts->sp.p++, cl); /* anchor to stack */
    for (int i = 0; i < nupvals; i++) {
        UpValInfo *uv = &fn->upvals[i];
        if (uv->onstack)
            cl->upvals[i] = crF_findupval(ts, base + uv->idx);
        else
            cl->upvals[i] = enc[uv->idx];
        crG_objbarrier(ts, cl, cl->upvals[i]);
    }
}


/* allocate new class and push it on stack */
static void pushclass(cr_State *ts) {
    OClass *cls = crMM_newclass(ts);
    setcls2s(ts, ts->sp.p++, cls); /* anchor to stack */
}


/*
 * Integer division; handles division by 0 and possible
 * overflow if 'y' == '-1' and 'x' == CR_INTEGER_MIN.
 */
cr_Integer crV_div(cr_State *ts, cr_Integer x, cr_Integer y) {
    if (cr_unlikely(cri_castS2U(y) + 1 <= 1)) { /* 'y' == '0' or '-1' */
        if (y == 0)
            crD_runerror(ts, "division by 0");
        return cri_intop(-, 0, x);
    }
    return (x / y);
}


/*
 * Integer modulus; handles modulo by 0 and overflow
 * as explained in 'crV_div()'.
 */
cr_Integer crV_modint(cr_State *ts, cr_Integer x, cr_Integer y) {
    cr_Integer r;
    if (cr_unlikely(cri_castS2U(y) + 1 <= 1)) {
        if (y == 0)
            crD_runerror(ts, "attempt to x%%0");
        return 0;
    }
    cri_nummod(ts, x, y, r);
    return r;
}


/* floating point modulus */
cr_Number crV_modnum(cr_State *ts, cr_Number x, cr_Number y) {
    cr_Number r;
    cri_nummod(ts, x, y, r);
    return r;
}


/*
 * Perform binary arithmetic operations on objects, this function is free
 * to call overloaded methods in cases where raw arithmetics are not possible.
 */
void crV_binarithm(cr_State *ts, const TValue *v1, const TValue *v2, SPtr res,
                   int op) {
    if (!crO_arithmraw(ts, v1, v2, s2v(res), op))
        crMM_trybin(ts, v1, v2, res, (op - CR_OPADD) + CR_MM_ADD);
}


/*
 * Perform unary arithmetic operations on objects, this function is free
 * to call overloaded methods in cases where raw arithmetics are not possible.
 */
void crV_unarithm(cr_State *ts, const TValue *v, SPtr res, int op) {
    TValue aux;
    setival(&aux, 0);
    if (!crO_arithmraw(ts, v, &aux, s2v(res), op))
        crMM_tryunary(ts, v, res, (op - CR_OPUNM) + CR_MM_UNM);
}


/* set 'vmt' entry */
static void setmm(cr_State *ts, TValue **vmt, TValue *fn, int vmtt) {
    cr_assert(0 <= vmtt && vmtt < CR_NUM_MM);
    if (cr_unlikely(!(*vmt))) /* empty 'vmt' */
        *vmt = crM_malloc(ts, SIZEVMT);
    (*vmt)[vmtt] = *fn; /* set the entry */
}


/*
 * According to C99 6.3.1.8 page 45:
 * "...if the corresponding real type of either operand is double, the other
 * operand is converted, without change of type domain, to a type whose
 * corresponding real type is double."
 */
cr_sinline int intlenum(cr_State *ts, const TValue *v1, const TValue *v2) {
    UNUSED(ts);
    return cri_numle(cast_num(ival(v1)), fval(v2));
}


/* check 'intLEnum' */
cr_sinline int numleint(cr_State *ts, const TValue *v1, const TValue *v2) {
    UNUSED(ts);
    return cri_numle(fval(v1), cast_num(ival(v2)));
}


/* less equal ordering on numbers */
cr_sinline int numle(cr_State *ts, const TValue *v1, const TValue *v2) {
    cr_assert(ttisnum(v1) && ttisnum(v2));
    if (ttisint(v1)) {
        cr_Integer i1 = ival(v1);
        if (ttisint(v2)) return (i1 <= ival(v2));
        else return intlenum(ts, v1, v2);
    } else {
        cr_Number n1 = fval(v1);
        if (ttisint(v2)) return numleint(ts, v1, v2);
        else return cri_numlt(n1, fval(v2));
    }
}


/* less equal ordering on non-number values */
cr_sinline int otherle(cr_State *ts, const TValue *v1, const TValue *v2) {
    if (ttisstr(v1) && ttisstr(v2))
        return (crS_cmp(strval(v1), strval(v2)) <= 0);
    else
        return crMM_order(ts, v1, v2, CR_MM_LE);
}


/* 'less or equal' ordering '<=' */
int crV_orderle(cr_State *ts, const TValue *v1, const TValue *v2) {
    if (ttisnum(v1) && ttisnum(v2))
        return numle(ts, v1, v2);
    return otherle(ts, v1, v2);
}


/* check 'intLEnum' */
cr_sinline int intltnum(const TValue *v1, const TValue *v2) {
    return cri_numlt(cast_num(ival(v1)), fval(v2));
}


/* check 'intLEnum' */
cr_sinline int numltint(const TValue *v1, const TValue *v2) {
    return cri_numlt(fval(v1), cast_num(ival(v2)));
}


/* 'less than' ordering '<' on number values */
cr_sinline int numlt(const TValue *v1, const TValue *v2) {
    cr_assert(ttisnum(v1) && ttisnum(v2));
    if (ttisint(v1)) {
        cr_Integer i1 = ival(v1);
        if (ttisint(v2)) return (i1 <= ival(v2));
        else return intltnum(v1, v2);
    } else {
        cr_Number n1 = fval(v1);
        if (ttisint(v2)) return numltint(v1, v2);
        else return cri_numlt(n1, fval(v2));
    }
}


/* 'less than' ordering '<' on non-number values */
cr_sinline int otherlt(cr_State *ts, const TValue *v1, const TValue *v2) {
    if (ttisstr(v1) && ttisstr(v2))
        return crS_cmp(strval(v1), strval(v2));
    else
        return crMM_order(ts, v1, v2, CR_MM_LT);
}


/* 'less than' ordering '<' */
int crV_orderlt(cr_State *ts, const TValue *v1, const TValue *v2) {
    if (ttisnum(v1) && ttisnum(v2))
        return numlt(v1, v2);
    return otherlt(ts, v1, v2);
}


/* 
** Equality ordering '=='.
** In case 'ts' is NULL perform raw equality (without invoking '__eq').
*/
int crV_ordereq(cr_State *ts, const TValue *v1, const TValue *v2) {
    cr_Integer i1, i2;
    const TValue *method;
    if (ttypetag(v1) != ttypetag(v2)) {
        if (ttype(v1) != ttype(v2) || ttype(v1) != CR_TNUMBER)
            return 0;
        return (crO_tointeger(v1, &i1, N2IEXACT) &&
                crO_tointeger(v2, &i2, N2IEXACT) && i1 == i2);
    }
    switch (ttypetag(v1)) {
    case CR_VNIL: case CR_VFALSE: case CR_VTRUE: return 1;
    case CR_VNUMINT: return (ival(v1) == ival(v2));
    case CR_VNUMFLT: return cri_numeq(fval(v1), fval(v2));
    case CR_VLUDATA: return (pval(v1) == pval(v2));
    case CR_VSTRING: return crS_eq(strval(v1), strval(v2));
    case CR_VUDATA: {
        if (udval(v1) == udval(v2)) return 1;
        else if (ts == NULL) return 0;
        method = crMM_get(ts, v1, CR_MM_EQ);
        if (ttisnil(method))
            method = crMM_get(ts, v2, CR_MM_EQ);
        break;
    }
    case CR_VINSTANCE: {
        if (insval(v1) == insval(v2)) return 1;
        else if (ts == NULL) return 0;
        method = crMM_get(ts, v1, CR_MM_EQ);
        if (ttisnil(method))
            method = crMM_get(ts, v2, CR_MM_EQ);
        break;
    }
    default: return (gcoval(v1) == gcoval(v2));
    }
    if (isabstkey(method))  {
        return 0;
    } else {
        crMM_callbinres(ts, method, v1, v2, ts->sp.p);
        return !cri_isfalse(s2v(ts->sp.p));
    }
}


/* 
** Check if global exists and return it, othwerwise invoke
** undefined global variable error.
*/
cr_sinline const TValue *checkglobal(cr_State *ts, TValue *key) {
    const TValue *out = crH_get(htval(&G_(ts)->globals), key);
    if (cr_unlikely(isabstkey(out)))
        crD_globalerror(ts, "undefined", strval(key));
    return out;
}


cr_sinline void defineglobal(cr_State *ts, TValue *key, TValue *val) {
    cr_assert(ttisstr(key));
    const TValue *slot = crH_get(htval(&G_(ts)->globals), key);
    if (cr_unlikely(!isabstkey(slot)))
        crD_runerror(ts, "global variable '%s' redefinition", cstrval(key));
    else
        crH_set(ts, htval(&G_(ts)->globals), key, val);
}


/* get global variable value */
cr_sinline void getglobal(cr_State *ts, TValue *key, TValue *out) {
    cr_assert(ttisstr(key));
    setobj(ts, out, checkglobal(ts, key));
}


/* set global variable value */
cr_sinline void setglobal(cr_State *ts, TValue *key, TValue *newval) {
    if (cr_unlikely(isconst(checkglobal(ts, key))))
        crD_globalerror(ts, "read-only", strval(key));
    crH_set(ts, htval(&G_(ts)->globals), key, newval);
}


void crV_setfield(cr_State *ts, TValue *obj, const TValue *key,
                  const TValue *val, cr_MM mm) {
    HTable *ht;
    const TValue *fmm;
    switch (ttypetag(obj)) {
        case CR_VINSTANCE: {
            ht = &insval(obj)->fields;
            break;
        }
        case CR_VUDATA: {
            ht = &insval(obj)->fields;
            break;
        }
        default: {
            fmm = crMM_get(ts, obj, mm);
            if (ttisnil(fmm))
                crD_typeerror(ts, obj, "index");
            goto callfmm;
        }
    }
    fmm = crMM_get(ts, obj, mm);
    if (!ttisnil(fmm)) {
        callfmm:
        crMM_callhtm(ts, fmm, obj, key, val);
    } else  {
        crH_set(ts, ht, key, val);
    }
}


/* bind method to instance and set it at 'res' */
#define bindmethod(ts,ins,fn,res) \
    setim2s(ts, res, crMM_newinsmethod(ts, ins, v))


void crV_getproperty(cr_State *ts, const TValue *obj, TValue *key, SPtr res,
                     cr_MM mm) {
    Instance *ins;
    HTable *fht, *mht;
    const TValue *fmm, *v;
    fmm = crMM_get(ts, obj, mm);
    if (!ttisnil(fmm)) { /* have metamethod ? */
        crMM_callhtmres(ts, fmm, obj, key, res);
        return;
    }
    switch (ttypetag(obj)) {
        case CR_VINSTANCE: {
            ins = insval(obj);
            fht = &ins->fields;
            mht = ins->oclass->methods;
        }
        case CR_VUDATA: {
            fht = &udval(obj)->fields;
            mht = NULL;
            break;
        }
        default: crD_typeerror(ts, obj, "index");
    }
    cr_assert(fht != NULL);
    v = crH_get(fht, key);
    if (!isabstkey(v)) { /* have field ? */
        setobj2s(ts, res, v);
        return;
    }
    if (mht) { /* have method table ? */
        cr_assert(ttisins(obj));
        v = crH_get(mht, key);
        if (!isabstkey(v)) { /* have method ? */
            /* bind it to instance and set 'res'  */
            bindmethod(ts, ins, v, res)
            return;
        }
    }
    setemptyval(s2v(res));
}


void crV_getsuper(cr_State *ts, Instance *ins, OClass *cls, const TValue *s,
                  SPtr res) {
    if (cls->methods) { /* superclass has methods ? */
        const TValue *v = crH_get(cls->methods, s);
        if (!isabstkey(v))
            bindmethod(ts, ins, v, res)
    } else { /* no methods; set nil */
        setnilval(s2v(res));
    }
}


/* 'dest' inherits methods from 'obj' (if any) */
cr_sinline void inherit(cr_State *ts, const TValue *obj, OClass *dest) {
    OClass *src;
    if (cr_unlikely(!ttiscls(obj)))
        crD_runerror(ts, "inherit a non-class value");
    src = clsval(obj);
    if (cr_likely(src->methods)) { /* 'src' has methods ? */
        cr_assert(dest->methods == NULL);
        dest->methods = crH_new(ts);
        crH_copykeys(ts, src->methods, dest->methods);
    }
}


/*
** Call binary meta method, but before that perform a quick check
** and invoke error if types don't match, the values are instances
** that belong to different classes or v1 (self) doesn't have the
** overloaded method.
*/
cr_sinline void precallmbin(cr_State *ts, const TValue *v1, const TValue *v2,
                            cr_MM op, SPtr res) {
    const TValue *func;
    const char *opname = getstrbytes(G_(ts)->mmnames[op]);
    if (cr_unlikely(ttypetag(v1) != ttypetag(v2)))
        crD_typeerrormeta(ts, v1, v2, opname);
    if (cr_unlikely(ttisins(v1) && insval(v1)->oclass != insval(v2)->oclass))
        crD_runerror(ts, "tried to %s instances of different class", opname);
    func = crMM_get(ts, v1, op);
    if (cr_unlikely(ttisnil(func)))
        crD_typeerror(ts, v1, opname);
    else
        crMM_callbinres(ts, func, v1, v2, res);
}


/* properly move results and if needed close variables */
cr_sinline void moveresults(cr_State *ts, SPtr res, int nres, int wanted) {
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
        case CR_MULRET: { /* all values needed */
            wanted = nres;
            break;
        }
        default: {
            if (hastocloseCfunc(wanted)) { /* tbc variables? */
                res = crF_close(ts, res, CLOSEKTOP); /* do the closing */
                wanted = decodeNresults(wanted); /* decode nresults */
                if (wanted == CR_MULRET) /* all values needed? */
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
cr_sinline void poscall(cr_State *ts, CallFrame *cf, int nres) {
    moveresults(ts, cf->func.p, nres, cf->nresults);
    ts->cf = cf->prev; /* back to caller */
}


cr_sinline CallFrame *prepcallframe(cr_State *ts, SPtr func, int nret,
                                    int mask, SPtr top) {
    CallFrame *cf = (ts->cf->next ? ts->cf->next : crT_newcf(ts));
    cf->func.p = func;
    cf->top.p = top;
    cf->nresults = nret;
    cf->status = mask;
    return cf;
}


cr_sinline int precallC(cr_State *ts, SPtr func, int nres, cr_CFunction f) {
    CallFrame *cf;
    int n; /* number of returns */
    checkstackGCp(ts, CR_MINSTACK, func);
    ts->cf = cf = prepcallframe(ts, func, nres, CFST_CCALL, ts->sp.p+CR_MINSTACK);
    cr_unlock(ts);
    n = (*f)(ts);
    cr_lock(ts);
    api_checknelems(ts, n);
    poscall(ts, cf, n);
    return n;
}


cr_sinline SPtr adjustffunc(cr_State *ts, SPtr func, const TValue *f) {
    checkstackGCp(ts, 1, func); /* space for 'f' */
    for (SPtr p = func; p < ts->sp.p; p--)
        setobjs2s(ts, p, p-1);
    ts->sp.p++;
    setobj2s(ts, func, f);
    return func;
}


cr_sinline SPtr trymmcall(cr_State *ts, SPtr func) {
    const TValue *f;
    f = crMM_get(ts, s2v(func), CR_MM_CALL);
    if (cr_unlikely(ttisnil(f)))
        crD_callerror(ts, s2v(func));
    return adjustffunc(ts, func, f);
}


CallFrame *precall(cr_State *ts, SPtr func, int nres) {
retry:
    switch (ttypetag(s2v(func))) {
        case CR_VCCL: { /* C closure */
            precallC(ts, func, CR_MULRET, cclval(s2v(func))->fn);
            return NULL;
        }
        case CR_VCFUNCTION: { /* light C function */
            precallC(ts, func, CR_MULRET, cfval(s2v(func)));
            return NULL;
        }
        case CR_VCLASS: { /* Class */
            const TValue *f;
            Instance *ins = crMM_newinstance(ts, clsval(s2v(func)));
            setins2s(ts, func, ins); /* replace class with its instance */
            f = crMM_get(ts, s2v(func), CR_MM_INIT);
            if (!ttisnil(f)) { /* have '__init' ? */
                func = adjustffunc(ts, func, f);
                cr_assert(ttiscl(s2v(func)) || ttiscfn(s2v(func)));
                goto retry; /* call '__init' */
            } else {
                return NULL; /* otherwise done */
            }
        }
        case CR_VCRCL: { /* Cript function (closure) */
            CallFrame *cf;
            Function *fn = crclval(s2v(func))->fn;
            int nargs = (ts->sp.p - func) - 1;
            int fsize = fn->maxstack;
            checkstackGCp(ts, fsize, func);
            ts->cf = cf = prepcallframe(ts, func, nres, 0, func+fsize+1);
            for (; nargs < fn->arity; nargs++)
                setnilval(s2v(ts->sp.p++));
            cr_assert(cf->top.p <= ts->stackend.p);
            return cf;
        }
        default: {
            func = trymmcall(ts, func);
            goto retry;
        }
    }
}


/* ------------------------------------------------------------------------
** Macros for arithmetic/bitwise/comparison instructions on integers.
**------------------------------------------------------------------------- */

/* 'cr_Integer' arithmetic operations */
#define iadd(ts,a,b)    (cri_intop(+, a, b))
#define isub(ts,a,b)    (cri_intop(-, a, b))
#define imul(ts,a,b)    (cri_intop(*, a, b))

/* integer bitwise operations */
#define iband(a,b)      (cri_intop(&, a, b))
#define ibor(a,b)       (cri_intop(|, a, b))
#define ibxor(a,b)      (cri_intop(^, a, b))

/* integer ordering operations */
#define ilt(a,b)        (a < b)
#define ile(a,b)        (a <= b)
#define igt(a,b)        (a > b)
#define ige(a,b)        (a >= b)


/* 
** Arithmetic operations
*/

#define op_arithKf_aux(ts,v1,v2,fop) { \
    cr_Number n1, n2; \
    if (tonumber(v1, &n1) && tonumber(v2, &n2)) { \
        setfval(v1, fop(ts, n1, n2)); \
    } else { \
        crD_aritherror(ts, v1, v2); \
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
        cr_Integer i1 = ival(v); \
        cr_Integer i2 = ival(lk); \
        setival(v, iop(ts, i1, i2)); \
    } else { \
        op_arithKf_aux(ts, v, lk, fop); \
    }}


/* arithmetic operation error with immediate operand */
#define op_arithI_error(ts,v,imm) \
    { TValue v2; setival(&v2, imm); crD_aritherror(ts, v, &v2); }


/* arithmetic operations with immediate operand for floats */
#define op_arithIf(ts,fop) { \
    TValue *v = peek(0); \
    int imm = fetchl(); /* L */\
    imm *= getsign(); /* S */\
    cr_Number n; \
    if (tonumber(v, &n)) { \
        cr_Number fimm = cast_num(imm); \
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
        cr_Integer i = ival(v); \
        setival(v, iop(ts, i, imm)); \
    } else if (ttisflt(v)) { \
        cr_Number n = fval(v); \
        cr_Number fimm = cast_num(imm); \
        setfval(v, fop(ts, n, fimm)); \
    } else { \
        op_arithI_error(ts, v, imm); \
    }}


#define op_arithf_aux(ts,v1,v2,fop) { \
    cr_Number n1; cr_Number n2; \
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
        cr_Integer i1 = ival(v1); cr_Integer i2 = ival(v2); \
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
    cr_Integer i1; cr_Integer i2 = ival(lk);  \
    if (cr_likely(tointeger(v, &i1))) { \
        setival(v, op(i1, i2)); \
    } else { \
        crD_bitwerror(ts, v, lk); \
    }}


/* bitwise operations with immediate operand */
#define op_bitwiseI(ts,op) { \
    TValue *v = peek(0); \
    int imm = fetchl(); /* L */\
    imm *= getsign(); /* S */\
    cr_Integer i; \
    if (cr_likely(tointeger(v, &i))) { \
        setival(v, op(i, imm)); \
    } else { \
        TValue vimm; setival(&vimm, imm); \
        crD_bitwerror(ts, v, &vimm); \
    }}


/* bitwise operations with stack operands */
#define op_bitwise(ts,op) { \
    TValue *v1 = peek(1); \
    TValue *v2 = peek(0); \
    cr_Integer i1; cr_Integer i2; \
    if (tointeger(v1, &i1) && tointeger(v2, &i2)) { \
        setival(v1, op(i1, i2)); \
        pc += getOpSize(OP_MBIN); \
    }/* else try 'OP_MBIN' */}



/*
** Ordering operations
*/

/* set ordering result */
#define setorderres(v,cond,eq) \
    { cr_assert(0 <= cond && cond <= 1); settt(v, booleans[cond == eq]); }


/* order operations with stack operands */
#define op_order(ts,iop,fop,other) { \
    TValue *v1 = peek(1); \
    TValue *v2 = peek(0); \
    int iseq = fetchs(); /* S */\
    int cond; \
    if (ttisint(v1) && ttisint(v2)) { \
        cr_Integer i1 = ival(v1); \
        cr_Integer i2 = ival(v2); \
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
        cr_Number n1 = fval(v); \
        cr_Number n2 = cast_num(imm); \
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

/* fetch instruction */
#define fetch()         (*pc++)
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


void crV_execute(cr_State *ts, CallFrame *cf) {
    register CrClosure *cl; /* closure being executed */
    register const Instruction *pc; /* program counter */
    register TValue *k; /* array of constants */
    register SPtr base; /* function base stack index */
#if CR_USE_JUMPTABLE
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
                protect(crF_adjustvarargs(ts, L, cf, cl->fn));
                updatebase(cf); /* update base, it changed */
                vm_break;
            }
            vm_case(OP_VARARG) {
                int L = fetchl(); /* how many */
                protect(crF_getvarargs(ts, cf, L));
                vm_break;
            }
            vm_case(OP_CLOSURE) {
                int L = fetchl();
                Function *fn = cl->fn->funcs[L];
                protect(pushclosure(ts, fn, cl->upvals, base));
                vm_break;
            }
            vm_case(OP_CLASS) {
                protect(pushclass);
                vm_break;
            }
            vm_case(OP_METHOD) {
                TValue *v1 = peek(1); /* class */
                TValue *v2 = peek(0); /* method */
                TValue *key = getlK();
                cr_assert(ttisstr(key));
                protect(crH_set(ts, clsval(v2)->methods, key, v1));
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
                op_arithK(ts, iadd, cri_numadd);
                vm_break;
            }
            vm_case(OP_SUBK) {
                op_arithK(ts, isub, cri_numsub);
                vm_break;
            }
            vm_case(OP_MULK) {
                op_arithK(ts, imul, cri_nummul);
                vm_break;
            }
            vm_case(OP_DIVK) {
                storepc(ts); /* in case of division by 0 */
                op_arithK(ts, crV_div, cri_numdiv);
                vm_break;
            }
            vm_case(OP_MODK) {
                storepc(ts); /* in case of division by 0 */
                op_arithK(ts, crV_modint, crV_modnum);
                vm_break;
            }
            vm_case(OP_POWK) {
                op_arithKf(ts, cri_numpow);
                vm_break;
            }
            vm_case(OP_BSHLK) {
                op_bitwiseK(ts, crO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHRK) {
                op_bitwiseK(ts, crO_shiftr);
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
                op_arithI(ts, iadd, cri_numadd);
                vm_break;
            }
            vm_case(OP_SUBI) {
                op_arithI(ts, isub, cri_numsub);
                vm_break;
            }
            vm_case(OP_MULI) {
                op_arithI(ts, imul, cri_nummul);
                vm_break;
            }
            vm_case(OP_DIVI) {
                storepc(ts); /* in case of division by 0 */
                op_arithI(ts, crV_div, cri_numdiv);
                vm_break;
            }
            vm_case(OP_MODI) {
                storepc(ts); /* in case of division by 0 */
                op_arithI(ts, crV_modint, crV_modnum);
                vm_break;
            }
            vm_case(OP_POWI) {
                op_arithIf(ts, cri_numpow);
                vm_break;
            }
            vm_case(OP_BSHLI) {
                op_bitwiseI(ts, crO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHRI) {
                op_bitwiseI(ts, crO_shiftr);
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
                op_arith(ts, iadd, cri_numadd);
                vm_break;
            }
            vm_case(OP_SUB) {
                op_arith(ts, isub, cri_numsub);
                vm_break;
            }
            vm_case(OP_MUL) {
                op_arith(ts, imul, cri_nummul);
                vm_break;
            }
            vm_case(OP_DIV) {
                storepc(ts); /* in case of division by 0 */
                op_arith(ts, crV_div, cri_numdiv);
                vm_break;
            }
            vm_case(OP_MOD) {
                storepc(ts); /* in case of division by 0 */
                op_arith(ts, crV_modint, crV_modnum);
                vm_break;
            }
            vm_case(OP_POW) {
                op_arithf(ts, crV_modnum);
                vm_break;
            }
            vm_case(OP_BSHL) {
                op_bitwise(ts, crO_shiftl);
                vm_break;
            }
            vm_case(OP_BSHR) {
                op_bitwise(ts, crO_shiftr);
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
            /* } RANGE_OPS { */
            vm_case(OP_RANGE) {
                /* TODO */
                cr_assert(0 && "instruction not implemented");
                vm_break;
            }
            /* } ORDERING_OPS { */
            vm_case(OP_EQK) {
                TValue *v1 = peek(0);
                const TValue *v2 = getlK(); /* L */
                int S = fetchs(); /* iseq */
                setorderres(v1, crV_raweq(v1, v2), S);
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
                    cond = cri_numeq(fval(v1), cast_num(L));
                else
                    cond = 0;
                setorderres(v1, cond, S2);
                vm_break;
            }
            vm_case(OP_LTI) {
                op_orderI(ts, ilt, cri_numlt);
                vm_break;
            }
            vm_case(OP_LEI) {
                op_orderI(ts, ile, cri_numle);
                vm_break;
            }
            vm_case(OP_GTI) {
                op_orderI(ts, igt, cri_numgt);
                vm_break;
            }
            vm_case(OP_GEI) {
                op_orderI(ts, ige, cri_numge);
                vm_break;
            }
            vm_case(OP_EQ) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                int S = fetchs(); /* iseq */
                int cond;
                protect(cond = crV_ordereq(ts, v1, v2));
                setorderres(v1, cond, S);
                pop(1); /* v2 */
                vm_break;
            }
            vm_case(OP_LT) {
                op_order(ts, ilt, cri_numlt, otherlt);
                vm_break;
            }
            vm_case(OP_LE) {
                op_order(ts, ile, cri_numle, otherle);
                vm_break;
            }
            vm_case(OP_EQPRESERVE) {
                SPtr res = TOPS();
                TValue *v1 = peek(1);
                TValue *v2 = s2v(res);
                protect(crV_ordereq(ts, v1, v2));
                setobj2s(ts, res, s2v(ts->sp.p));
                vm_break;
            }
            /* }} UNARY_OPS { */
            vm_case(OP_NOT) {
                TValue *v = peek(0);
                if (cri_isfalse(v))
                    setbtval(v);
                else
                    setbfval(v);
                vm_break;
            }
            vm_case(OP_UNM) {
                SPtr res = TOPS();
                TValue *v = s2v(res);
                if (ttisint(v)) {
                    cr_Integer i = ival(v);
                    setival(v, cri_intop(-, 0, i));
                } else if (ttisflt(v)) {
                    cr_Number n = fval(v);
                    setfval(v, cri_numunm(ts, n));
                } else {
                    protect(crMM_tryunary(ts, v, res, CR_MM_UNM));
                }
                vm_break;
            }
            vm_case(OP_BNOT) {
                SPtr res = TOPS();
                TValue *v = s2v(res);
                if (ttisint(v)) {
                    cr_Integer i = ival(v);
                    setival(v, cri_intop(^, ~cri_castS2U(0), i));
                } else {
                    protect(crMM_tryunary(ts, v, res, CR_MM_BNOT));
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
                if (!cri_isfalse(v) == S)
                    pc += L;
                vm_break;
            }
            vm_case(OP_TESTORPOP) {
                TValue *v = peek(0);
                int L = fetchl(); /* offset */
                int S = fetchs(); /* cond */
                if (!cri_isfalse(v) == S)
                    pc += L;
                else
                    pop(1); /* v */
                vm_break;
            }
            vm_case(OP_TESTANDPOP) {
                TValue *v = peek(0);
                int L = fetchl(); /* offset */
                int S = fetchs(); /* cond */
                if (!cri_isfalse(v) == S) {
                    pc += L;
                    pop(1); /* v */
                }
                vm_break;
            }
            vm_case(OP_TESTPOP) {
                TValue *v = peek(0);
                int L = fetchl(); /* offset */
                int S = fetchs(); /* cond */
                if (!cri_isfalse(v) == S)
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
                protect(crF_close(ts, level, CR_OK));
                vm_break;
            }
            vm_case(OP_TBC) {
                SPtr level = base + fetchl();
                protect(crF_newtbcvar(ts, level));
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
            vm_case(OP_SETPROPERTY) {
                TValue *s = getlK();
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                cr_assert(ttisstr(s));
                protect(crV_setfield(ts, v1, s, v2, CR_MM_SETFIELD));
                pop(2); /* v1,v2 */
                vm_break;
            }
            vm_case(OP_GETPROPERTY) {
                TValue *s = getlK();
                TValue *v = peek(0);
                cr_assert(ttisstr(s));
                protect(crV_getproperty(ts, v, s, TOPS(), CR_MM_GETFIELD));
                vm_break;
            }
            vm_case(OP_GETINDEX) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                protect(crV_getproperty(ts, v1, v2, SPTR(1), CR_MM_GETIDX));
                pop(1); /* v2 */
                vm_break;
            }
            vm_case(OP_SETINDEX) {
                TValue *v1 = peek(2);
                TValue *v2 = peek(1);
                TValue *v3 = peek(0);
                protect(crV_setfield(ts, v1, v2, v3, CR_MM_SETIDX));
                pop(3); /* v1,v2,v3 */
                vm_break;
            }
            vm_case(OP_GETINDEXSTR) { /* TODO: optimize */
                TValue *s = getlK();
                TValue *v = peek(0);
                cr_assert(ttisstr(s));
                protect(crV_getproperty(ts, v, s, TOPS(), CR_MM_GETIDX));
                vm_break;
            }
            vm_case(OP_SETINDEXSTR) { /* TODO: optimize */
                TValue *s = getlK();
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                cr_assert(ttisstr(s));
                protect(crV_setfield(ts, v1, s, v2, CR_MM_SETIDX));
                vm_break;
            }
            vm_case(OP_GETINDEXINT) { /* TODO: optimize */
                TValue aux;
                TValue *v = peek(0);
                setival(&aux, fetchl());
                protect(crV_getproperty(ts, v, &aux, TOPS(), CR_MM_GETIDX));
                vm_break;
            }
            vm_case(OP_SETINDEXINT) { /* TODO: optimize */
                TValue aux;
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                setival(&aux, fetchl());
                protect(crV_setfield(ts, v1, &aux, v2, CR_MM_SETIDX));
                vm_break;
            }
            vm_case(OP_GETSUP) {
                TValue *s = getlK();
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                cr_assert(ttisstr(s));
                protect(crV_getsuper(ts, insval(v1), clsval(v2), s, SPTR(1)));
                pop(1); /* v2 */
                vm_break;
            }
            vm_case(OP_GETSUPIDX) {
                TValue *v1 = peek(2);
                TValue *v2 = peek(1);
                TValue *v3 = peek(0);
                protect(crV_getsuper(ts, insval(v1), clsval(v2), v3, SPTR(2)));
                pop(2); /* v2,v3 */
                vm_break;
            }
            vm_case(OP_GETSUPIDXSTR) { /* TODO: optimize */
                TValue *s = getlK();
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                cr_assert(ttisstr(s));
                protect(crV_getsuper(ts, insval(v1), clsval(v2), s, SPTR(1)));
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
                protect(crF_newtbcvar(ts, stk + FORTBCVAR));
                pc += offset;
                cr_assert(*pc == OP_FORCALL);
                goto l_forcall;
            }
            vm_case(OP_FORCALL) {
            l_forcall: {
                SPtr nbase = STK(fetchl());
                /* 'nbase' slot is iterator function, 'nbase + 1' is the
                 * invariant state 'nbase + 2' is the control variable, and
                 * 'nbase + 3' is the to-be-closed variable. Call uses stack
                 * after these values (starting at 'nbase + 4'). */
                memcpy(nbase+NSTATEVARS, nbase, FORTBCVAR*sizeof(nbase));
                ts->sp.p = nbase + NSTATEVARS + FORTBCVAR;
                protect(crV_call(ts, nbase + NSTATEVARS, fetchl()));
                updatebase(cf);
                cr_assert(*pc == OP_FORLOOP);
                goto l_forloop;
            }}
            vm_case(OP_FORLOOP) {
            l_forloop: {
                SPtr nbase = STK(fetchl());
                int offset = fetchl();
                if (!ttisnil(s2v(nbase + NSTATEVARS))) { /* continue loop? */
                    /* save control variable */
                    setobjs2s(ts, nbase + FORCNTLVAR, nbase + NSTATEVARS);
                    pc -= offset; /* jump back */
                }
                vm_break;
            }}
            vm_case(OP_RET) {
                SPtr nbase = STK(fetchl());
                int n = fetchl() - 1; /* number of results */
                if (n < 0) /* not fixed ? */
                    n = ts->sp.p - nbase;
                storepc(ts);
                if (fetchs()) { /* have open upvalues? */
                    crF_close(ts, base, CLOSEKTOP);
                    updatebase(cf);
                }
                if (cl->fn->isvararg) /* vararg function ? */
                    cf->func.p -= cf->nvarargs + cl->fn->arity + 1;
                ts->sp.p = nbase + n;
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
