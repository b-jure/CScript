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
#include "cript.h"
#include "crlimits.h"
#include "crobject.h"
#include "crobject.h"
#include "crstate.h"
#include "crcode.h"
#include "crvm.h"

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


// /* Adjust return values after native call finishes. */
// static cr_inline void moveresults(cr_State *ts, Value *fn, int32_t got, int32_t expect)
// {
//     Value *retstart = ts->sp - got; // start of return values
//     if (expect == 0)
//         expect = got; // all results (MULRET)
//     if (got > expect)
//         got = expect; // remove extra results
//     memcpy(fn, retstart, got); // Safety: 'retstart' >= 'nativefn'
//     for (int32_t i = got; i < expect; i++) // replace missing values with nil
//         fn[i] = NIL_VAL;
//     ts->sp = fn + expect;
// }
// 
// 
// /* Call native function. */
// static cr_inline int32_t callnative(cr_State *ts, Value fn)
// {
//     cr_unlock(ts);
//     int32_t n = ascfn(fn)->fn(ts);
//     cr_lock(ts);
//     criptapi_checkelems(ts, n);
//     CallFrame *f = &last_frame(ts);
//     moveresults(ts, f->callee, n, f->retcnt);
//     ts->fc--; // pop frame
//     return n;
// }
// 
// 
// 
// /* Calls have lots of checks and there are two main reasons why.
//  * cript does not allow extra arguments if the function does not
//  * accept variable number of arguments and you are not allowed to
//  * provide less arguments than the function arity.
//  * In case 'callee' is a cript closure then just return the new 'CallFrame',
//  * otherwise run the C closure and return NULL. */
// static CallFrame *precall(cr_State *ts, Value callee, int32_t argc, int32_t retcnt)
// {
//     FnInfo *p = NULL;
//     CallFrame *frame = &ts->frames[ts->fc];
//     if (!iscfunction(callee)) {
//         CrClosure *closure = asclosure(callee);
//         Function *fn = closure->fn;
//         p = &fn->p;
//         frame->closure = closure;
//         frame->ip = fn->chunk.code.data;
//         frame->cfinfo = 0;
//         retcnt = (retcnt == CR_MULRET ? 0 : retcnt); // adjust return count
//     } else {
//         CClosure *native = ascfn(callee);
//         p = &native->p;
//         frame->closure = NULL;
//         frame->ip = NULL;
//         frame->cfinfo = CFI_CCALL;
//     }
// #if defined(callbitmask)
//     const static void *jmptable[] = {
//         &&l_stack_overflow,
//         &&l_invalid_argc,
//         &&l_callstack_overflow,
//         &&l_ok,
//     };
//     uint32_t bitmask = callbitmask(ts, p->isvararg, p->arity, argc, retcnt);
//     cr_ubyte idx = cr_ctz(bitmask);
//     goto *jmptable[idx];
// l_stack_overflow:
//     retovferror(ts, p->name->storage);
// l_invalid_argc:
//     arityerror(ts, p->arity, argc);
// l_callstack_overflow:
//     fcovferror(ts);
// #else
//     if (cr_unlikely(!cr_ensurestack(ts, retcnt))) {
//         retovferror(ts, p->name->storage);
//     } else if (cr_unlikely((p->isvararg && p->arity > argc) || (!p->isvararg && p->arity != argc))) {
//         arityerror(ts, p->arity, argc);
//     } else if (cr_unlikely(ts->fc == ts_CALLSTACK_LIMIT)) {
//         fcovferror(ts);
//     } else
//         goto ok;
// #endif
// l_ok:
//     frame->vacnt = argc - p->arity;
//     frame->retcnt = retcnt;
//     frame->callee = ts->sp - argc - 1;
//     ts->fc++;
//     if (frame->cfinfo & CFI_CCALL) {
//         callnative(ts, callee);
//         return NULL;
//     } else
//         return &last_frame(ts);
// }
// 
// 
// static CallFrame *precall(cr_State *ts, SPtr fn, int retcnt)
// {
// }
// 
// 
// /* Call '()' a value (closure, method, class). */
// cr_sinline void call(cr_State *ts, SPtr fn, int retcnt)
// {
//     int argc;
// 
//     argc = ts->sp.p - fn - 1;
//     if (cr_unlikely(!IS_OBJ(callee)))
//         callerror(ts, callee);
//     switch (OBJ_TYPE(callee)) {
//         case OBJ_BOUND_METHOD: {
//             InstanceMethod *bound = asboundmethod(callee);
//             ts->sp[-argc - 1] = bound->receiver; // class instance (self)
//             return precall(ts, OBJ_VAL(bound->method), argc, retcnt);
//         }
//         case OBJ_CLASS: {
//             OClass *oclass = asclass(callee);
//             Value instance = OBJ_VAL(Instance_new(ts, oclass));
//             if (!calloverload(ts, instance, OM_INIT)) { // not overloaded ?
//                 *stkpeek(argc) = instance; // 'self'
//                 int32_t arity = ominfo[OM_INIT].arity; // default arity
//                 if (cr_unlikely(argc != arity))
//                     arityerror(ts, arity, argc);
//             }
//             return NULL;
//         }
//         case OBJ_CLOSURE:
//         case OBJ_FUNCTION:
//         case OBJ_CFUNCTION:
//             return precall(ts, callee, argc, retcnt);
//         default:
//             cr_unreachable;
//     }
// }
// 
// 
// /* external interface for 'call'. */
// void crV_call(cr_State *ts, SPtr fn, int nreturns) {
//     call(ts, fn, nreturns);
// }
// 
// 
// /* Protected call with longjmp.
//  * Performs a protected call, calling the wrapper 'ProtectedFn' around
//  * a cript function or native C function.
//  * Returns status of the called function, this status is modified
//  * by function that errors and performs the long jump or it
//  * stays unchanged and the wrapper function just returns and
//  * execution continues. */
// cr_sinline int32_t protectedcall(cr_State *ts, ProtectedFn fn, void *userdata) {
// }
// 
// 
// /* Public interface to 'protectedcall'.
//  * In case of errors it performs a recovery by closing all
//  * open upvalues (values to be closed) and restoring the
//  * old stack pointer (oldtop). */
// int32_t crV_pcall(cr_State *ts, ProtectedFn fn, void *userdata, ptrdiff_t oldtop)
// {
//     int status;
//     SPtr oldsp;
// 
//     status = protectedcall(ts, fn, userdata);
//     if (cr_unlikely(status != CR_OK)) {
//         closeupval(ts, ts->sp);
//         oldsp = restorestack(ts, oldtop);
//         *oldsp = ts->sp[-1];
//         ts->sp = oldsp + 1;
//     }
//     return status;
// }
// 
// 
// /* Check if receiver and key are valid for indexing. */
// static cr_inline void checkindex(cr_State *ts, Value receiver, Value key)
// {
//     if (cr_unlikely(!isinstance(receiver)))
//         ipaerror(ts, receiver);
//     else if (cr_unlikely(IS_NIL(key)))
//         nilidxerror(ts);
// }
// 
// 
// /* Private to interpreter.
//  * Used when creating a cript closure. */
// static cr_inline OUpvalue *captureupval(cr_State *ts, Value *valp)
// {
//     OUpvalue **upvalpp = &ts->open_upvals;
//     while (*upvalpp != NULL && (*upvalpp)->location > valp)
//         upvalpp = &(*upvalpp)->next;
//     if (*upvalpp != NULL && (*upvalpp)->location == valp)
//         return *upvalpp;
//     OUpvalue *upvalp = OUpvalue_new(ts, valp);
//     upvalp->next = *upvalpp;
//     *upvalpp = upvalp;
//     return upvalp;
// }
// 
// /* Closes all of the captured variables moving
//  * them from the stack onto the heap (open_upvals array),
//  * making them reachable for gc. */
// void closeupval(cr_State *ts, Value *last)
// {
//     while (ts->openuvals != NULL && ts->openuvals->location >= last) {
//         OUpvalue *upvalp = ts->open_upvals;
//         upvalp->closed = *upvalp->location;
//         upvalp->location = &upvalp->closed;
//         ts->open_upvals = upvalp->next;
//     }
// }
// 
// /**
//  * Searches the entire table for the matching index in order to
//  * provide more descriptive runtime error.
//  **/
// CRString *globalname(cr_State *ts, uint32_t idx)
// {
//     for (uint32_t i = 0; i < ts->globids.cap; i++) {
//         Entry *entry = &ts->globids.entries[i];
//         if (!IS_EMPTY(entry->key) && AS_NUMBER(entry->value) == idx)
//             return (OString *)asobj(entry->key);
//     }
//     cr_unreachable;
// }



/* ------------------------------------------------------------------------
 * Interpreter loop
 * ------------------------------------------------------------------------ */

#define updatebase(cf)  (base = (cf)->func.p + 1)

/* save program counter */
#define savepc()        ((cf)->pc = pc)
/* save program counter and stack top */
#define savestate()     (savepc(), (ts)->stacktop.p = (cf)->stacktop.p)
/* protect code that can raise errors or change the stack */
#define protect(e)      (savestate(), (e))

/* fetch instruction */
#define fetch()         (pc+=SIZEINSTR, pc[-SIZEINSTR])
/* fetch short instruction parameter */
#define fetchs()        (pc+=SIZEARGS, pc[-SIZEARGS])
/* fetch long instruction parameter */
#define fetchl()        (pc+=SIZEARGL, get3bytes(&pc[-SIZEARGL]))

/* get constant */
#define getlK()         (k + fetchl())
#define getsK()         (k + fetchs())
/* get string constant */
#define getKStr()       (strval(getlK()))


/* In case 'PRECOMPUTED_GOTO' not available. */
#define vm_dispatch(x)      switch(x)
#define vm_case(l)          case l:
#define vm_break            break


void crV_execute(cr_State *ts, CallFrame *cf) {
    register CrClosure *cl; /* closure being executed */
    register SIndex base; /* function base stack index */
    register const Instruction *pc; /* program counter */
    register TValue *k; /* array of constants */
    register SPtr sp; /* stack pointer */
#if CR_USE_JUMPTABLE
#include "crjmptable.h"
#endif
    ret:
    cl = crclval(s2v(cf->callee.p));
    base.p = cf->callee.p + 1;
    pc = cl->fn->code;
    k = cl->fn->k;
    sp = ts->sp.p;
    for (;;) {
        vm_dispatch(fetch()) {
            vm_case(OP_TRUE) {
                setbtval(s2v(ts->sp.p++));
                vm_break;
            }
            vm_case(OP_FALSE) {
                setbfval(s2v(ts->sp.p++));
                vm_break;
            }
            vm_case(OP_NIL) {
                setnilval(s2v(ts->sp.p++));
                vm_break;
            }
            vm_case(OP_NILN) {
                int L = fetchl();
                while (L--)
                    setnilval(s2v(sp++));
                vm_break;
            } 
            vm_case(OP_CONST) {
                TValue *sk = getsK();
                setobj2s(ts, sp++, sk);
                vm_break;
            }
            vm_case(OP_CONSTL) {
                TValue *lk = getlK();
                setobj2s(ts, sp++, lk);
                vm_break;
            }
            vm_case(OP_CONSTI) {
                int L = fetchl(); /* int */
                int S = fetchs(); /* sign */
                setival(s2v(sp++), L*S);
                vm_break;
            }
            vm_case(OP_CONSTF) {
                int L = fetchl(); /* flt */
                int S = fetchs(); /* sign */
                setfval(s2v(sp++), cast_num(L*S));
                vm_break;
            }
            vm_case(OP_VARARGPREP) {
                vm_break;
            }
            vm_case(OP_VARARG) {
                vm_break;
            }
            vm_case(OP_CLOSURE) {
                vm_break;
            }
            vm_case(OP_CLASS) {
                vm_break;
            }
            vm_case(OP_METHOD) {
                vm_break;
            }
            vm_case(OP_SETMM) {
                vm_break;
            }
            vm_case(OP_POP) {
                vm_break;
            }
            vm_case(OP_POPN) {
                vm_break;
            }
            vm_case(OP_MBIN) {
                vm_break;
            }
            vm_case(OP_MBINI) {
                vm_break;
            }
            vm_case(OP_MBINK) {
                vm_break;
            }
            vm_case(OP_ADDK) {
                vm_break;
            }
            vm_case(OP_SUBK) {
                vm_break;
            }
            vm_case(OP_MULK) {
                vm_break;
            }
            vm_case(OP_DIVK) {
                vm_break;
            }
            vm_case(OP_MODK) {
                vm_break;
            }
            vm_case(OP_POWK) {
                vm_break;
            }
            vm_case(OP_BSHLK) {
                vm_break;
            }
            vm_case(OP_BSHRK) {
                vm_break;
            }
            vm_case(OP_BANDK) {
                vm_break;
            }
            vm_case(OP_BORK) {
                vm_break;
            }
            vm_case(OP_BXORK) {
                vm_break;
            }
            vm_case(OP_ADDI) {
                vm_break;
            }
            vm_case(OP_SUBI) {
                vm_break;
            }
            vm_case(OP_MULI) {
                vm_break;
            }
            vm_case(OP_DIVI) {
                vm_break;
            }
            vm_case(OP_MODI) {
                vm_break;
            }
            vm_case(OP_POWI) {
                vm_break;
            }
            vm_case(OP_BSHLI) {
                vm_break;
            }
            vm_case(OP_BSHRI) {
                vm_break;
            }
            vm_case(OP_BANDI) {
                vm_break;
            }
            vm_case(OP_BORI) {
                vm_break;
            }
            vm_case(OP_BXORI) {
                vm_break;
            }
            vm_case(OP_ADD) {
                vm_break;
            }
            vm_case(OP_SUB) {
                vm_break;
            }
            vm_case(OP_MUL) {
                vm_break;
            }
            vm_case(OP_DIV) {
                vm_break;
            }
            vm_case(OP_MOD) {
                vm_break;
            }
            vm_case(OP_POW) {
                vm_break;
            }
            vm_case(OP_BSHL) {
                vm_break;
            }
            vm_case(OP_BSHR) {
                vm_break;
            }
            vm_case(OP_BAND) {
                vm_break;
            }
            vm_case(OP_BOR) {
                vm_break;
            }
            vm_case(OP_BXOR) {
                vm_break;
            }
            vm_case(OP_RANGE) {
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
