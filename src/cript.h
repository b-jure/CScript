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
 *
 *
 *
 * Copyright (C) 1994-2023 Lua.org, PUC-Rio.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * ---------------------------------------------------------------------------------------------- */


#ifndef CR_H
#define CR_H

#include "crconf.h"

#include <stdarg.h>
#include <sys/types.h>


/* ================= Version info and Copyright ================= */
#define CR_VERSION_MAJOR   "1"
#define CR_VERSION_MINOR   "0"
#define CR_VERSION_RELEASE "0"

#define CR_VERSION_NUMBER      100
#define CR_VERSION_RELEASE_NUM (CR_VERSION_NUMBER * 100);

#define CR_VERSION   "cript " CR_VERSION_MAJOR "." CR_VERSION_MINOR
#define CR_RELEASE   CR_VERSION "." CR_VERSION_RELEASE
#define CR_COPYRIGHT CR_RELEASE " Copyright (C) 2023-2024 Jure Bagić"
#define CR_AUTHORS   "Jure Bagić"
/* -------------------------------------------------------------- */


/* ================ API assertions ================ */
/* You can define your own assert. */
#if !defined(cr_assert)
#define cr_assert(vm, cond, msg) ((void)0)
#endif
#if !defined(cr_checkapi)
#define cr_checkapi(vm, cond, msg) cr_assert(cond)
#endif

/* 
 * Locking mechanism (for thread safety).
 * By default both lock and unlock are nop.
 */
#if !defined(S_LOCK_USR)
#define cr_lock(vm)   ((void)0)
#define cr_unlock(vm) ((void)0)
#endif
/* ------------------------------------------------ */


/* =============== Integer types =============== */
/* 8 bit integers */
typedef uint8_t cr_ubyte;
typedef int8_t cr_sbyte;

/* 32 bit integers */
typedef int32_t cr_int;
typedef uint32_t cr_uint;

/* 64 bit integers */
typedef int64_t cr_lint;
typedef uint64_t cr_ulint;

/* signed and unsigned word size types */
typedef size_t cr_umem;
typedef ssize_t cr_mem;

/* hash size */
typedef cr_ulint cr_hash;

/* data pointers */
typedef uintptr_t cr_uintptr;
typedef intptr_t cr_intptr;

/* type for pointer arithmetics */
typedef ptrdiff_t cr_ptrdiff;

/* integer */
#define cr_integer cr_lint
/* unsigned integer */
#define cr_uinteger cr_ulint
/* --------------------------------------------- */


/* =============== Floating-point types =============== */
/* single/double floating point */
typedef double cr_double;
typedef float cr_float;

/* float number */
#define cr_floating cr_double
/* ---------------------------------------------------- */


/* =============== Other types =============== */
/* virtual machine (state) */
typedef struct VM VM;

/* debug information */
typedef struct cr_debuginfo cr_debuginfo;

/* light userdata */
#define cr_lud void *
/* ------------------------------------------- */


/* =============== Hooks =============== */
/* C function to be defined for usage in cript */
typedef cr_int (*cr_cfunc)(VM *vm);

/*
 * Panic handler.
 * As of version 1.0.0 this function should not return,
 * instead at some point it should call 'abort' or any
 * variant of 'exit'.
 */
typedef cr_int (*cr_panic)(VM *vm);

/* 
 * Memory allocator.
 * Note: If allocation fails or the allocator is used to 
 * free the memory then function should return NULL.
 */
typedef void *(*cr_alloc)(void *ptr, cr_umem newsize, void *userdata);

/* 
 * Reader.
 * 'cr_load()' uses this reader to compile cript scripts.
 * Each time 'cr_load()' tries to load another chunk of the
 * script, it calls this reader together with the 'userdata'.
 *
 * Expected behaviour:
 * - 'cr_reader' must return a pointer to the block of memory
 *   it read and set the 'szread' to the size of the block.
 * - The block must exist until the 'cr_reader' is called again.
 * - To signal the end of the chunk, 'cr_reader' must return NULL
 *   or set the 'szread' to 0.
 * - 'cr_reader' can return any block size greater than zero. 
 */
typedef const char *(*cr_reader)(VM *vm, void *userdata, cr_umem *szread);
/* ------------------------------------- */

/* Type tag for cript internal types. */
#define CR_TT_NONE (-1) // indicates absence of value

typedef enum {
	CR_TT_NIL = 0,
	CR_TT_NUMBER,
	CR_TT_STRING,
	CR_TT_BOOL,
	CR_TT_CLASS,
	CR_TT_INSTANCE,
	CR_TT_FUNCTION,
	CR_TT_N, // enum variant count, keep this last
} cr_tt;

/* --------------------------------------------------------------------- */


/* ========== 'VM' state manipulation ========== */
criptapi VM *cr_create(cr_alloc allocator, void *ud);
criptapi void cr_resetvm(VM *vm);
criptapi void cr_destroy(VM **vmp);

criptapi cr_panic cr_setpanic(VM *vm, cr_panic panicfn);

criptapi cr_umem cr_version(VM *vm);
/* --------------------------------------------------------------------- */


/* ============== Class method/field tags ============== */
/* overloadable method tag */
typedef enum {
	CR_OM_INIT = 0, /* __init__ */
	CR_OM_DISPLAY, /* __display__ */
	CR_OM_TOSTRING, /* __tostring__ */
	CR_OM_GETIDX, /* __getidx__ */
	CR_OM_SETIDX, /* __setidx__ */
	CR_OM_HASH, /* __hash__ */
	CR_OM_FREE, /* __free__ */
	CR_OM_ADD, /* __add__ */
	CR_OM_SUB, /* __sub__ */
	CR_OM_MUL, /* __mul__ */
	CR_OM_DIV, /* __div__ */
	CR_OM_MOD, /* __mod__ */
	CR_OM_POW, /* __pow__ */
	CR_OM_NOT, /* __not__ */
	CR_OM_UMIN, /* __umin__ */
	CR_OM_NE, /* __ne__ */
	CR_OM_EQ, /* __eq__ */
	CR_OM_LT, /* __lt__ */
	CR_OM_LE, /* __le__ */
	CR_OM_GT, /* __gt__ */
	CR_OM_GE, /* __ge__ */
	CR_OM_N, // enum variant count, keep this last
} cr_om;

#define omisunop(omtag) ((omtag) == OM_NOT | (omtag) == OM_UMIN)
/* --------------------------------------------------------------------- */


/* ========== ordering and arithmetic functions ========== */
/* ordering operations */
typedef enum {
	CR_ORD_EQ = 0, /* equal '==' */
	CR_ORD_NE, /* not equal '!=' */
	CR_ORD_LT, /* less '<' */
	CR_ORD_GT, /* greater '>' */
	CR_ORD_LE, /* less or equal '<=' */
	CR_ORD_GE, /* greater or equal '>=' */
	CR_ORD_N, // enum variant count, keep this last
} cr_ord;

criptapi cr_ubyte cr_compare(VM *vm, cr_int idx1, cr_int idx2, cr_ord op);
criptapi cr_ubyte cr_rawequal(VM *vm, cr_int idx1, cr_int idx2);

/* arithmetic operations */
typedef enum {
	CR_AR_ADD = 0, /* addition '+' */
	CR_AR_SUB, /* subtraction '-' */
	CR_AR_MUL, /* multiplication '*' */
	CR_AR_DIV, /* division '/' */
	CR_AR_MOD, /* mod '%' */
	CR_AR_POW, /* pow '2^n' */
	CR_AR_NOT, /* (unary) not '!' */
	CR_AR_UMIN, /* (unary) negation '-' */
	CR_AR_N, // enum variant count, keep this last
} cr_ar;

criptapi void cr_arith(VM *vm, cr_ar op);
/* --------------------------------------------------------------------- */


/* ========== push functions, C -> stack ========== */
criptapi void cr_pushnil(VM *vm);
criptapi void cr_pushinteger(VM *vm, cr_lint number);
criptapi void cr_pushfloat(VM *vm, cr_double number);
criptapi void cr_pushstring(VM *vm, const char *str, cr_umem len);
criptapi void cr_pushcstring(VM *vm, const char *str);
criptapi const char *cr_pushvfstring(VM *vm, const char *fmt, va_list argp);
criptapi const char *cr_pushfstring(VM *vm, const char *fmt, ...);
criptapi void cr_pushbool(VM *vm, cr_int boolean);
criptapi void cr_pushcclosure(VM *vm, const char *name, cr_cfunc fn, cr_uint args, cr_ubyte isvararg, cr_uint upvals);
criptapi void cr_push(VM *vm, cr_int idx);

typedef struct {
	const char *name;
	cr_cfunc fn; // C function
	cr_uint args; // argument count (arity)
	cr_ubyte isvararg; // is '...'
} cr_entry; // class method entry
criptapi void cr_pushclass(VM *vm, cr_entry entries[], cr_uint nup);
/* --------------------------------------------------------------------- */


/* ========== raw access ========== */
#define CR_RAWSET 0

criptapi cr_ubyte cr_rawindex(VM *vm, cr_int idx, cr_ubyte what);
/* --------------------------------------------------------------------- */


/* ========== get functions, cript -> stack ========== */
criptapi cr_ubyte cr_getglobal(VM *vm, const char *name);
criptapi cr_ubyte cr_getmethod(VM *vm, cr_int idx, const char *method);
criptapi cr_ubyte cr_getfield(VM *vm, cr_int idx, const char *field);
criptapi cr_ubyte cr_getindex(VM *vm, cr_int idx);
#define cr_rawgetindex(vm, idx) cr_rawindex(vm, idx, 1)
/* --------------------------------------------------------------------- */


/* ========== set functions stack -> cript =========== */
criptapi cr_ubyte cr_setglobal(VM *vm, const char *name, cr_int isfixed);
criptapi cr_ubyte cr_setfield(VM *vm, cr_int idx, const char *field);
criptapi cr_ubyte cr_setindex(VM *vm, cr_int idx);
#define cr_rawsetindex(vm, idx) cr_rawindex(vm, idx, CR_RAWSET)
/* --------------------------------------------------------------------- */


/* ========== access functions, stack -> C ========== */
criptapi cr_ubyte cr_isnil(const VM *vm, cr_int idx);
criptapi cr_ubyte cr_isnumber(const VM *vm, cr_int idx);
criptapi cr_ubyte cr_isstring(const VM *vm, cr_int idx);
criptapi cr_ubyte cr_isbool(const VM *vm, cr_int idx);
criptapi cr_ubyte cr_isclass(const VM *vm, cr_int idx);
criptapi cr_ubyte cr_isinstance(const VM *vm, cr_int idx);
criptapi cr_ubyte cr_isnative(const VM *vm, cr_int idx);
criptapi cr_ubyte cr_ismethod(const VM *vm, cr_int idx);
criptapi cr_ubyte cr_isclosure(const VM *vm, cr_int idx);
criptapi cr_tt cr_type(const VM *vm, cr_int idx);
criptapi const char *cr_typename(const VM *vm, cr_int idx);
criptapi const char *cr_tagname(const VM *vm, cr_tt type);

criptapi cr_panic cr_getpanic(VM *vm);
criptapi cr_alloc cr_getalloc(VM *vm, void **ud);
criptapi cr_ubyte cr_getbool(const VM *vm, cr_int idx, cr_ubyte *isbool);
criptapi cr_float cr_getnumber(const VM *vm, cr_int idx, cr_ubyte *isnum);
criptapi const char *cr_getstring(const VM *vm, cr_int idx);
criptapi cr_cfunc cr_getcfunction(const VM *vm, cr_int idx);
criptapi cr_umem cr_strlen(const VM *vm, cr_int idx);
/* --------------------------------------------------------------------- */


/* ========== stack manipulation functions ========== */
criptapi const char *cr_tostring(VM *vm, cr_int idx, cr_umem *len, cr_hash *hash);
criptapi void cr_settop(VM *vm, cr_int idx);
criptapi cr_uint cr_gettop(const VM *vm);
criptapi cr_uint cr_absidx(VM *vm, cr_int idx);
criptapi void cr_rotate(VM *vm, cr_int idx, cr_int n);
criptapi void cr_copy(VM *vm, cr_int src, cr_int dest);
criptapi cr_ubyte cr_checkstack(VM *vm, cr_int n);
/* --------------------------------------------------------------------- */


/* ========== error reporting ========== */
/* status codes */
typedef enum {
	CR_S_OK = 0,
	CR_S_EMEM, // memory allocation error
	CR_S_EARUN, // unary arithmetic operation error
	CR_S_EARBIN, // binary arithmetic operation error
	CR_S_EARG, // invalid argument
	CR_S_ECMP, // invalid comparison
	CR_S_ESOVERFLOW, // stack overflow
	CR_S_EFOVERFLOW, // CallFrame overflow
	CR_S_EARITY, // function argument count is less than its arity
	CR_S_EBINOP, // binary operator error
	CR_S_EUDPROPERTY, // undefined property
	CR_S_EPACCESS, // invalid property access
	CR_S_EINHERIT, // inheriting from non-class value
	CR_S_EFIXEDASSIGN, // assigning to fixed value
	CR_S_EUDGLOBAL, // undefined global variable
	CR_S_EGLOBALREDEF, // redefinition of global variable
	CR_S_EOMRET, // overload-able method invalid return type
	CR_S_ECALL, // tried calling non-callable value
	CR_S_ESTRFMT, // string format error
	CR_S_ECOMP, // compile error
	CR_S_ENILIDX, // indexing with 'nil' error
	CR_S_EFILE, // file related error
	CR_S_EBCLIMIT, // bytecode limit exceeded error
	CR_S_GSLIMIT, // gray stack limit exceeded error
	CR_S_N,
} cr_status;
criptapi cr_status cr_getstatus(VM *vm);
criptapi cr_int cr_error(VM *vm, cr_status errcode);
/* --------------------------------------------------------------------- */


/* ========== miscellaneous functions/macros ========== */
criptapi const char *cr_stringify(VM *vm, cr_int idx);
criptapi cr_ubyte cr_getupvalue(VM *vm, cr_int fidx, cr_int idx);
criptapi cr_int cr_setupvalue(VM *vm, cr_int fidx, cr_int idx);
criptapi const char *cr_concat(VM *vm);
criptapi cr_ubyte cr_nextproperty(VM *vm, cr_int idx, cr_ubyte what);

#define cr_nextfield(vm, idx)  cr_nextproperty(vm, idx, 0)
#define cr_nextmethod(vm, idx) cr_nextproperty(vm, idx, 1)
#define cr_register(vm, name, cfn, args, isvararg, upvals) \
	(cr_pushcclosure(vm, name, cfn, args, isvararg, upvals), cr_setglobal(vm, name, 0))
#define cr_pushcfunction(vm, name, cfn, args, isvararg) cr_pushcclosure(vm, name, cfn, args, isvararg, 0)
#define cr_pop(vm, n)					cr_settop(vm, -(n)-1)
#define cr_replace(vm, idx)				(cr_copy(vm, -1, idx), cr_pop(vm, 1))
#define cr_remove(vm, idx)				(cr_rotate(vm, idx, -1), cr_pop(vm, 1))
#define cr_insert(vm, idx)				cr_rotate(vm, idx, 1)
/* --------------------------------------------------------------------- */


/* ========== call/load ========== */
// returns all of the returned values from the function ('retcnt')
#define CR_MULRET (-1)

criptapi cr_status cr_pcall(VM *vm, cr_int argc, cr_int retcnt);
criptapi void cr_call(VM *vm, cr_int argc, cr_int retcnt);
criptapi cr_status cr_load(VM *vm, cr_reader reader, void *userdata, const char *source);
/* --------------------------------------------------------------------- */


/* ========== GC ========== */
/* GC options */
typedef enum {
	CR_GCO_STOP, // stop GC
	CR_GCO_RESTART, // restart GC (start if stopped)
	CR_GCO_COLLECT, // perform full GC cycle
	CR_GCO_STEP, // perform single gc step
	CR_GCO_COUNT, // get number of bytes allocated
	CR_GCO_ISRUNNING, // check whether GC is stopped
	CR_GCO_NEXTGC, // set bytes amount when the next GC will trigger
} cr_gco;

criptapi cr_umem cr_incgc(VM *vm, cr_gco option, ...);
/* --------------------------------------------------------------------- */


/* ============= debug API ============= */
criptapi cr_ubyte cr_getstack(VM *vm, cr_int level, cr_debuginfo *di);

/* bits for debug mask ('dbmask') */
typedef enum {
	CR_DW_FNGET = (1 << 0), // load the function on top of the stack (processed first)
	CR_DW_LINE = (1 << 1), // fill 'line'
	CR_DW_FNINFO = (1 << 2), // fill all function info in 'cr_debuginfo'
	CR_DW_FNSRC = (1 << 3), // fill function source information
	CR_DW_FNPUSH = (1 << 4), // push current function on the stack (processed last)
} cr_dw;
criptapi cr_ubyte cr_getinfo(VM *vm, cr_ubyte dbmask, cr_debuginfo *di);

/* Struct for interfacing with cript debug API, this gets passed
 * in every debug function, main one being 'cr_getinfo'.
 * These functions fill out 'cr_DebugInfo' with requested information
 * as stated in bit mask [@DebugWhat]. */
struct cr_debuginfo {
	const char *name; // function name (declaration name in cript script)
	const char *type; // function type ('cript', 'main' or 'C')
	const char *source; // function source
	cr_umem srclen; // length of 'source'
	cr_int line; // current line in cript script
	cr_uint nups; // number of function upvalues
	cr_uint nparams; // number of function parameters
	cr_ubyte isvararg; // is function vararg ('...')
	cr_int defline; // line number where the function definition starts
	cr_int deflastline; // line number where the function definition ends
	char shortsrc[CR_SRCID_MAX];
	/* private */
	union CallFrame *frame; // active function frame
};
/* --------------------------------------------------------------------- */

#endif
