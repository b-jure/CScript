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

#include "crconf.h"
#include "crfunction.h"
#include "crhashtable.h"
#include "cript.h"
#include "crlimits.h"
#include "crmem.h"
#include "crobject.h"
#include "crobject.h"
#include "crstate.h"
#include "crcode.h"
#include "crvm.h"
#include "crmeta.h"

#include <stdio.h>
#include <stdlib.h>
#include <time.h>


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
    OClass *cls = crMm_newclass(ts);
    setcls2s(ts, ts->sp.p++, cls); /* anchor to stack */
}


/* set value or insert key/value pair into table 'ht' */
void crV_settable(cr_State *ts, HTable *ht, TValue *val, TValue *key) {

}


/* set 'vmt' entry */
static void setmm(cr_State *ts, TValue **vmt, TValue *fn, int vmtt) {
    cr_assert(0 <= vmtt && vmtt < CR_NUM_META);
    if (cr_unlikely(!(*vmt))) /* empty 'vmt' */
        *vmt = crM_malloc(ts, SIZEVMT); /* this can fail, 'protect' needed */
    (*vmt)[vmtt] = *fn; /* set the entry */
}



/* ------------------------------------------------------------------------
** Macros for arithmetic/bitwise/comparison opcodes on integers.
**------------------------------------------------------------------------- */


/* integer arithmetic operations */
#define iadd(ts,a,b)    (cri_intop(+, a, b))
#define isub(ts,a,b)    (cri_intop(-, a, b))
#define imul(ts,a,b)    (cri_intop(*, a, b))


/*
** Auxiliary to 'op_arith_aux'.
*/
#define op_arithf_aux(ts,v1,v2,fop,op,popn) { \
    cr_Number n1; cr_Number n2; \
    if (tonumber(v1, &n1) && tonumber(v2, &n2)) { \
        setfval(v1, fop(ts, n1, n2)); \
        pc += getOpSize(op); \
        pop(popn); \
    }}


/*
** Arithmetic operations over integers and floats.
** Result is placed into 'v1', 'popn' values are popped
** off the stack and 'op' is skipped.
*/
#define op_arith_aux(ts,v1,v2,iop,fop,op,popn) { \
    if (ttisint(v1) && ttisint(v2)) { \
        cr_Integer i1 = ival(v1); cr_Integer i2 = ival(v2); \
        setival(v1, iop(ts, i1, i2)); \
        pc += getOpSize(op); \
        pop(popn); \
    } else { \
        op_arithf_aux(ts, v1, v2, fop, op, popn); \
    }}


/*
** Arithmetic operations with K (constant) operand for floats.
*/
#define op_arithKf(ts,fop) { \
    TValue *v = peek(0); \
    TValue *lk = getlK(); ttisnum(lk); \
    op_arithf_aux(ts, v, lk, fop, OP_MBINK, 0); }


/*
** Arithmetic operations with K (constant) operand.
*/
#define op_arithK(ts,iop,fop) { \
    TValue *v = peek(0); \
    TValue *lk = getlK(); \
    op_arith_aux(ts, v, lk, iop, fop, OP_MBINK, 0); }


/*
** Arithmetic operations with I (immediate) operand for floats.
*/
#define op_arithIf(ts,fop) { \
    TValue *v = peek(0); \
    int L = fetchl(); \
    L *= getsign(); \
    cr_Number n; \
    if (tonumber(v, &n)) { \
        cr_Number imm = cast_num(L); \
        setfval(v, fop(ts, n, imm)); \
        pc += getOpSize(OP_MBINI); \
    }}


/* 
** Arithmetic operations with I (immediate) operand.
*/
#define op_arithI(ts,iop,fop) { \
    TValue *v = peek(0); \
    int imm = fetchl(); \
    imm *= getsign(); \
    if (ttisint(v)) { \
        cr_Integer i = ival(v); \
        setival(v, iop(ts, i, imm)); \
        pc += getOpSize(OP_MBINI); \
    } else if (ttisflt(v)) { \
        cr_Number n = fval(v); \
        cr_Number fimm = cast_num(imm); \
        setfval(v, fop(ts, n, fimm)); \
        pc += getOpSize(OP_MBINI); \
    }}


/*
** Arithmetic operations with stack operands for floats.
*/
#define op_arithf(ts,fop) { \
    TValue *v1 = peek(1); \
    TValue *v2 = peek(0); \
    op_arithf_aux(ts, v1, v2, fop, OP_MBIN, 1); }


/*
** Arithmetic operations with stack operands.
*/
#define op_arith(ts,iop,fop) { \
    TValue *v1 = peek(1); \
    TValue *v2 = peek(0); \
    op_arith_aux(ts, v1, v2, iop, fop, OP_MBIN, 1) }



/* integer bitwise operations */
#define iband(a,b)      (cri_intop(&, a, b))
#define ibor(a,b)       (cri_intop(|, a, b))
#define ibxor(a,b)      (cri_intop(^, a, b))


/*
** Bitwise operations with K (constant) operand.
*/
#define op_bitwiseK(ts,op) { \
    TValue *v = peek(0); \
    TValue *lk = getlK(); \
    cr_Integer i1; cr_Integer i2 = ival(lk);  \
    if (tointeger(v, &i1)) { \
        setival(v, op(i1, i2)); \
        pc += getOpSize(OP_MBINK); \
    }}


/*
** Bitwise operations with I (immediate) operand.
*/
#define op_bitwiseI(ts,op) { \
    TValue *v = peek(0); \
    int imm = fetchl(); \
    imm *= getsign(); \
    cr_Integer i; \
    if (tointeger(v, &i)) { \
        setival(v, op(i, imm)); \
        pc += getOpSize(OP_MBINI); \
    }}


/*
** Bitwise operations with stack operands.
*/
#define op_bitwise(ts,op) { \
    TValue *v1 = peek(1); \
    TValue *v2 = peek(0); \
    cr_Integer i1; cr_Integer i2; \
    if (tointeger(v1, &i1) && tointeger(v2, &i2)) { \
        setival(v1, op(i1, i2)); \
        pc += getOpSize(OP_MBIN); \
    }}



/* integer ordering operations */
#define ilt(a,b)        (a < b)
#define ile(a,b)        (a <= b)
#define igt(a,b)        (a > b)
#define ige(a,b)        (a >= b)


/* ------------------------------------------------------------------------
 * Interpreter loop
 * ------------------------------------------------------------------------ */

/* update current function stack base */
#define updatebase(cf)      (base = (cf)->callee.p + 1)

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

/* peek stack */
#define peek(n)         s2v(ts->sp.p - n)
/* pop stack */
#define pop(n)          (ts->sp.p -= (n))


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
#include "crjmptable.h"
#endif
    ret:
    cl = crclval(s2v(cf->callee.p));
    base = cf->callee.p + 1;
    pc = cl->fn->code;
    k = cl->fn->k;
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
            vm_case(OP_MBIN) {
                TValue *v1 = peek(1);
                TValue *v2 = peek(0);
                int S = fetchs(); /* op */
                // TODO
                vm_break;
            }
            vm_case(OP_MBINI) {
                TValue *v = peek(0);
                int L = fetchl();
                int S1 = fetchs(); /* sign */
                int S2 = fetchs(); /* flip */
                // TODO
                vm_break;
            }
            vm_case(OP_MBINK) {
                TValue *v = peek(0);
                TValue *lk = getlK();
                int S = fetchs(); /* flip */
                // TODO
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
                op_arithK(ts, crO_div, cri_numdiv);
                vm_break;
            }
            vm_case(OP_MODK) {
                storepc(ts); /* in case of division by 0 */
                op_arithK(ts, crO_modint, crO_modnum);
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
                op_arithI(ts, crO_div, cri_numdiv);
                vm_break;
            }
            vm_case(OP_MODI) {
                storepc(ts); /* in case of division by 0 */
                op_arithI(ts, crO_modint, crO_modnum);
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
                op_arith(ts, crO_div, cri_numdiv);
                vm_break;
            }
            vm_case(OP_MOD) {
                storepc(ts); /* in case of division by 0 */
                op_arith(ts, crO_modint, crO_modnum);
                vm_break;
            }
            vm_case(OP_POW) {
                op_arithf(ts, crO_modnum);
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
            vm_case(OP_RANGE) {
                // TODO
                vm_break;
            }
            vm_case(OP_EQK) {
                vm_break;
            }
            vm_case(OP_EQI) {
                vm_break;
            }
            vm_case(OP_LTI) {
                vm_break;
            }
            vm_case(OP_LEI) {
                vm_break;
            }
            vm_case(OP_GTI) {
                vm_break;
            }
            vm_case(OP_GEI) {
                vm_break;
            }
            vm_case(OP_EQ) {
                vm_break;
            }
            vm_case(OP_LT) {
                vm_break;
            }
            vm_case(OP_LE) {
                vm_break;
            }
            vm_case(OP_NOT) {
                vm_break;
            }
            vm_case(OP_UNM) {
                vm_break;
            }
            vm_case(OP_BNOT) {
                vm_break;
            }
            vm_case(OP_EQPRESERVE) {
                vm_break;
            }
            vm_case(OP_JMP) {
                vm_break;
            }
            vm_case(OP_JMPS) {
                vm_break;
            }
            vm_case(OP_TEST) {
                vm_break;
            }
            vm_case(OP_TESTORPOP) {
                vm_break;
            }
            vm_case(OP_TESTANDPOP) {
                vm_break;
            }
            vm_case(OP_TESTPOP) {
                vm_break;
            }
            vm_case(OP_CALL) {
                vm_break;
            }
            vm_case(OP_CLOSE) {
                vm_break;
            }
            vm_case(OP_TBC) {
                vm_break;
            }
            vm_case(OP_GETLOCAL) {
                vm_break;
            }
            vm_case(OP_SETLOCAL) {
                vm_break;
            }
            vm_case(OP_GETPRIVATE) {
                vm_break;
            }
            vm_case(OP_SETPRIVATE) {
                vm_break;
            }
            vm_case(OP_GETUVAL) {
                vm_break;
            }
            vm_case(OP_SETUVAL) {
                vm_break;
            }
            vm_case(OP_DEFGLOBAL) {
                vm_break;
            }
            vm_case(OP_GETGLOBAL) {
                vm_break;
            }
            vm_case(OP_SETGLOBAL) {
                vm_break;
            }
            vm_case(OP_SETPROPERTY) {
                vm_break;
            }
            vm_case(OP_GETPROPERTY) {
                vm_break;
            }
            vm_case(OP_GETINDEX) {
                vm_break;
            }
            vm_case(OP_SETINDEX) {
                vm_break;
            }
            vm_case(OP_GETINDEXSTR) {
                vm_break;
            }
            vm_case(OP_SETINDEXSTR) {
                vm_break;
            }
            vm_case(OP_GETINDEXINT) {
                vm_break;
            }
            vm_case(OP_SETINDEXINT) {
                vm_break;
            }
            vm_case(OP_GETSUP) {
                vm_break;
            }
            vm_case(OP_GETSUPIDX) {
                vm_break;
            }
            vm_case(OP_GETSUPIDXSTR) {
                vm_break;
            }
            vm_case(OP_INHERIT) {
                vm_break;
            }
            vm_case(OP_FORPREP) {
                vm_break;
            }
            vm_case(OP_FORCALL) {
                vm_break;
            }
            vm_case(OP_FORLOOP) {
                vm_break;
            }
            vm_case(OP_RET0) {
                vm_break;
            }
            vm_case(OP_RET1) {
                vm_break;
            }
            vm_case(OP_RET) {
                vm_break;
            }
        }
    }
}
