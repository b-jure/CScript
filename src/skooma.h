/*
 ** Skooma FFI is mostly reimplementation of Lua FFI
 ** https://www.lua.org/source/5.4/lua.h.html
 */


#ifndef SKOOMA_H
#define SKOOMA_H

#include "skconf.h"

#include <stdarg.h>


/* ================= Version info and Copyright ================= */
#define SK_VERSION_MAJOR "1"
#define SK_VERSION_MINOR "0"
#define SK_VERSION_RELEASE "0"

#define SK_VERSION_NUMBER 100
#define SK_VERSION_RELEASE_NUM (SK_VERSION_NUMBER * 100);

#define SK_VERSION "Skooma " SK_VERSION_MAJOR "." SK_VERSION_MINOR
#define SK_RELEASE SK_VERSION "." SK_VERSION_RELEASE
#define SK_COPYRIGHT SK_RELEASE " Copyright (C) 2024 B. Jure"
#define SK_AUTHORS "B. Jure"
/* --------------------------------------------------------------------- */


/* ================ API assertions ================ */
/* User can define his own sk_assert if he requires
 * different/custom behaviour.
 * By default sk_assert is a nop. */
#if !defined(sk_assert)
#define sk_assert(vm, cond, msg) ((void)0)
#endif
#if !defined(sk_checkapi)
#define sk_checkapi(vm, cond, msg) sk_assert(cond)
#endif

/* Locking mechanism (by default nop).
 * By default skooma does not assume your VM is shared
 * by multiple threads, it is up to user to decide whether
 * to define his own sk_lock/sk_unlock. */
#if !defined(S_LOCK_USR)
#define sk_lock(vm) ((void)0)
#define sk_unlock(vm) ((void)0)
#endif
/* --------------------------------------------------------------------- */


/* =============== API integer typedefs =============== */
/* Skooma unsigned and signed byte */
typedef uint8_t sk_byte;
typedef int8_t sk_sbyte;
/* Skooma signed and unsigned integer (32 bit) */
typedef int32_t sk_int;
typedef uint32_t sk_uint;
/* Skooma signed and unsigned long integer (64 bit) */
typedef int64_t sk_lint;
typedef uint64_t sk_ulint;
/* Skooma (unsigned) size of objects in memory */
typedef size_t sk_memsize;
/* --------------------------------------------------------------------- */


/* ============== Skooma types ============== */
/* Skooma number */
typedef double sk_number;

/* Virtual Machine */
typedef struct VM VM;

/* Holds debug information */
typedef struct DebugInfo DebugInfo;

/* Native C function signature */
typedef sk_int (*CFunction)(VM* vm);

/* Panic handler (same signature as 'CFunction') */
typedef sk_int (*PanicFn)(VM* vm);

/* Memory allocator function signature. */
typedef void* (*AllocFn)(void* ptr, sk_memsize newsize, void* userdata);

/* Reader function signature.
 * @sk_load uses this reader to compile skooma scripts.
 * Each time @sk_load tries to load another chunk of the
 * script, it calls this reader together with the 'userdata'.
 *
 * Expected behaviour:
 * - 'ReadFn' must return a pointer to the block of memory
 *   it read and set the 'szread' to the size of the block.
 *
 * - The block must exist until the 'ReadFn' is called again.
 *
 * - To signal the end of the chunk, 'ReadFn' must return NULL
 *   or set the 'szread' to 0.
 *
 * - 'ReadFn' can return any block size greater than zero. */
typedef const char* (*ReadFn)(VM* vm, void* userdata, sk_memsize* szread);

#define TT_NONE (-1) // indicates absence of value
typedef enum {
    TT_NIL = 0,
    TT_NUMBER,
    TT_STRING,
    TT_BOOL,
    TT_CLASS,
    TT_INSTANCE,
    TT_FUNCTION,
    TT_CNT, // keep this last
} TypeTag;
/* --------------------------------------------------------------------- */


/* ========== 'VM' state manipulation ========== */
SK_API VM* sk_create(AllocFn allocator, void* ud);
SK_API void sk_resetvm(VM* vm);
SK_API void sk_destroy(VM** vmp);

/* Set VM hooks */
SK_API PanicFn sk_setpanic(VM* vm, PanicFn panicfn);
SK_API ReadFn sk_setreader(VM* vm, ReadFn readfn);
SK_API AllocFn sk_setalloc(VM* vm, AllocFn allocfn, void* ud);

SK_API sk_number sk_version(VM* vm);
/* --------------------------------------------------------------------- */


/* ============== Class method/field tags ============== */
typedef enum {
    OM_INIT = 0,
    OM_DISPLAY,
    OM_GETIDX,
    OM_SETIDX,
#if defined(SK_OVERLOAD_OPS)
    OM_ADD,
    OM_SUB,
    OM_MUL,
    OM_DIV,
    OM_MOD,
    OM_POW,
    OM_NOT,
    OM_UMIN,
    OM_NE,
    OM_EQ,
    OM_LT,
    OM_LE,
    OM_GT,
    OM_GE,
#endif
    OM_CNT, // keep this last
} OMTag; // tag for overload-able class methods

#if defined(SK_OVERLOAD_OPS)
#define omisunop(omtag) ((omtag) == OM_NOT | (omtag) == OM_UMIN)
#endif
/* --------------------------------------------------------------------- */


/* ========== Ordering and arithmetic functions ========== */
typedef enum {
    ORD_EQ = 0, // equal '=='
    ORD_NE, // not equal '!='
    ORD_LT, // less '<'
    ORD_GT, // greater '>'
    ORD_LE, // less or equal '<='
    ORD_GE, // greater or equal '>='
    ORD_CNT, // Ord count
} Ord; // ordering operations
SK_API sk_byte sk_compare(VM* vm, sk_int idx1, sk_int idx2, Ord op);
SK_API sk_byte sk_rawequal(VM* vm, sk_int idx1, sk_int idx2);

typedef enum {
    AR_ADD = 0, // addition '+'
    AR_SUB, // subtraction '-'
    AR_MUL, // multiplication '*'
    AR_DIV, // division '/'
    AR_MOD, // mod '%'
    AR_POW, // pow '2^n'
    AR_NOT, // (unary) not '!'
    AR_UMIN, // (unary) negation '-'
    AR_CNT, // Ar count
} Ar; // arithmetic operations
SK_API void sk_arith(VM* vm, Ar op);
/* --------------------------------------------------------------------- */


/* ========== push functions, C -> stack ========== */
SK_API void sk_pushnil(VM* vm);
SK_API void sk_pushnumber(VM* vm, sk_number number);
SK_API void sk_pushstring(VM* vm, const char* str, sk_memsize len);
SK_API void sk_pushcstring(VM* vm, const char* str);
SK_API const char* sk_pushvfstring(VM* vm, const char* fmt, va_list argp);
SK_API const char* sk_pushfstring(VM* vm, const char* fmt, ...);
SK_API void sk_pushbool(VM* vm, sk_int boolean);
SK_API void sk_pushcclosure(VM* vm, CFunction fn, sk_uint args, sk_byte isvararg, sk_uint upvals);
SK_API void sk_push(VM* vm, sk_int idx);
/* --------------------------------------------------------------------- */


/* ========== raw access ========== */
#define SK_RAWSET 0

SK_API sk_byte sk_rawindex(VM* vm, sk_int idx, sk_byte what);
/* --------------------------------------------------------------------- */


/* ========== get functions, skooma -> stack ========== */
SK_API sk_byte sk_getglobal(VM* vm, const char* name);
SK_API sk_byte sk_getmethod(VM* vm, sk_int idx, const char* method);
SK_API sk_byte sk_getfield(VM* vm, sk_int idx, const char* field);
SK_API sk_byte sk_getindex(VM* vm, sk_int idx);
#define sk_rawgetindex(vm, idx) sk_rawindex(vm, idx, SK_RAWGET)
/* --------------------------------------------------------------------- */


/* ========== set functions stack -> skooma =========== */
SK_API sk_byte sk_setglobal(VM* vm, const char* name, sk_int isfixed);
SK_API sk_byte sk_setfield(VM* vm, sk_int idx, const char* field);
SK_API sk_byte sk_setindex(VM* vm, sk_int idx);
#define sk_rawsetindex(vm, idx) sk_rawindex(vm, idx, SK_RAWSET)
/* --------------------------------------------------------------------- */


/* ========== access functions, stack -> C ========== */
SK_API sk_byte sk_isnil(const VM* vm, sk_int idx);
SK_API sk_byte sk_isnumber(const VM* vm, sk_int idx);
SK_API sk_byte sk_isstring(const VM* vm, sk_int idx);
SK_API sk_byte sk_isbool(const VM* vm, sk_int idx);
SK_API sk_byte sk_isclass(const VM* vm, sk_int idx);
SK_API sk_byte sk_isinstance(const VM* vm, sk_int idx);
SK_API sk_byte sk_isnative(const VM* vm, sk_int idx);
SK_API sk_byte sk_ismethod(const VM* vm, sk_int idx);
SK_API sk_byte sk_isclosure(const VM* vm, sk_int idx);
SK_API TypeTag sk_type(const VM* vm, sk_int idx);
SK_API const char* sk_typename(const VM* vm, sk_int idx);
SK_API const char* sk_tagname(const VM* vm, TypeTag type);

SK_API PanicFn sk_getpanic(VM* vm);
SK_API ReadFn sk_getreader(VM* vm);
SK_API AllocFn sk_getalloc(VM* vm, void** ud);
SK_API sk_byte sk_getbool(const VM* vm, sk_int idx, sk_byte* isbool);
SK_API sk_number sk_getnumber(const VM* vm, sk_int idx, sk_byte* isnum);
SK_API const char* sk_getstring(const VM* vm, sk_int idx);
SK_API CFunction sk_getcfunction(const VM* vm, sk_int idx);
SK_API sk_memsize sk_strlen(const VM* vm, sk_int idx);
/* --------------------------------------------------------------------- */


/* ========== stack manipulation functions ========== */
SK_API void sk_settop(VM* vm, sk_int idx);
SK_API sk_uint sk_gettop(const VM* vm);
SK_API sk_uint sk_absidx(VM* vm, sk_int idx);
SK_API void sk_rotate(VM* vm, sk_int idx, sk_int n);
SK_API void sk_copy(VM* vm, sk_int src, sk_int dest);
SK_API sk_int sk_ensurestack(VM* vm, sk_int n);
/* --------------------------------------------------------------------- */


/* ========== error reporting ========== */
typedef enum {
    S_OK = 0,
    S_EMEM, // memory allocation error
    S_EARUN, // unary arithmetic operation error
    S_EARBIN, // binary arithmetic operation error
    S_EARG, // invalid argument
    S_ECMP, // invalid comparison
    S_ESOVERFLOW, // stack overflow
    S_EFOVERFLOW, // CallFrame overflow
    S_EARITY, // function argument count is less than its arity
    S_EBINOP, // binary operator error
    S_EUDPROPERTY, // undefined property
    S_EPACCESS, // invalid property access
    S_EINHERIT, // inheriting from non-class value
    S_EFIXEDASSIGN, // assigning to fixed value
    S_EUDGLOBAL, // undefined global variable
    S_EGLOBALREDEF, // redefinition of global variable
    S_EDISPLAY, // display method returned invalid value
    S_ECALL, // tried calling non-callable value
    S_ESTRFMT, // string format error
    S_ECOMP, // compile error
    S_ENILIDX, // indexing with 'nil' error
    S_CNT,
} Status; // Runtime status codes
SK_API Status sk_getstatus(VM* vm);
SK_API sk_int sk_error(VM* vm, Status errcode);
/* --------------------------------------------------------------------- */


/* ========== miscellaneous functions/macros ========== */
SK_API const char* sk_stringify(VM* vm, sk_int idx);
SK_API sk_int sk_getupvalue(VM* vm, sk_int fidx, sk_int idx);
SK_API sk_int sk_setupvalue(VM* vm, sk_int fidx, sk_int idx);
SK_API const char* sk_concat(VM* vm);
SK_API sk_byte sk_nextproperty(VM* vm, sk_int idx, sk_byte what);

#define sk_nextfield(vm, idx) sk_nextproperty(vm, idx, 0)
#define sk_nextmethod(vm, idx) sk_nextproperty(vm, idx, 1)
#define sk_register(vm, name, cfn, args, isvararg, upvals)                                         \
    (sk_pushcclosure(vm, cfn, args, isvararg, upvals), sk_setglobal(vm, name, 0))
#define sk_pushcfunction(vm, cfn, args, isvararg) sk_pushcclosure(vm, cfn, args, isvararg, 0)
#define sk_pop(vm, n) sk_settop(vm, -(n)-1)
#define sk_replace(vm, idx) (sk_copy(vm, -1, idx), sk_pop(vm, 1))
#define sk_remove(vm, idx) (sk_rotate(vm, idx, -1), sk_pop(vm, 1))
#define sk_insert(vm, idx) sk_rotate(vm, idx, 1)
/* --------------------------------------------------------------------- */


/* ========== call/load/run functions ========== */
// returns all of the returned values from the function ('retcnt')
#define SK_MULRET (-1)

SK_API Status sk_pcall(VM* vm, sk_int argc, sk_int retcnt);
SK_API void sk_call(VM* vm, sk_int argc, sk_int retcnt);
SK_API Status sk_load(VM* vm, ReadFn reader, void* userdata, const char* source);
/* --------------------------------------------------------------------- */


/* ========== garbage collector ========== */

// @TODO: Implement 'GCO_STEP' option, this will invoke
//        garbage collection same as 'GCO_COLLECT', but
//        collection will continue only up to the provided
//        limit (in kilobytes).
//        For example 'sk_gc(vm, GCO_STEP, 5)' will sweep
//        5 kilobytes before the collection stops (or less in
//        case total collectable memory is less than 5 kilobytes).

typedef enum {
    GCO_STOP, // stop GC
    GCO_RESTART, // restart GC (start if stopped)
    GCO_COLLECT, // perform full GC cycle
    // @TODO: GCO_STEP,
    GCO_COUNT, // get number of bytes allocated
    GCO_ISRUNNING, // check whether GC is stopped
    GCO_NEXTGC, // set bytes amount when the next GC will trigger
} GCOpt; // Garbage collector options

SK_API sk_memsize sk_gc(VM* vm, GCOpt option, ...);
/* --------------------------------------------------------------------- */


/* ============= debug API ============= */
SK_API sk_byte sk_getstack(VM* vm, sk_int level, DebugInfo* di);
typedef enum {
    DW_FNGET = (1 << 0), // load the function on top of the stack (processed first)
    DW_LINE = (1 << 1), // fill 'line'
    DW_FNINFO = (1 << 2), // fill all function info in 'DebugInfo'
    DW_FNSRC = (1 << 3), // fill function source information
    DW_FNPUSH = (1 << 4), // push current function on the stack (processed last)
} DebugWhat; // bits for creating debug bitmask ('sk_getinfo')

SK_API sk_byte sk_getinfo(VM* vm, sk_byte dbmask, DebugInfo* di);

/* Forward declare the private type */
typedef struct CallFrame CallFrame;

/* Struct for interfacing with Skooma debug API, this gets passed
 * in every debug function, main one being 'sk_getinfo'.
 * These functions fill out 'DebugInfo' with requested information
 * as stated in bit mask [@DebugWhat]. */
struct DebugInfo {
    const char* name; // function name (declaration name in Skooma script)
    const char* type; // function type ('Skooma', 'main' or 'C')
    const char* source; // function source
    sk_memsize srclen; // length of 'source'
    sk_int line; // current line in Skooma script
    sk_uint nups; // number of function upvalues
    sk_uint nparams; // number of function parameters
    sk_byte isvararg; // is function vararg ('...')
    sk_int defline; // line number where the function definition starts
    sk_int deflastline; // line number where the function definition ends
    char shortsrc[SK_SRCID_MAX];
    /* private */
    CallFrame* frame; // active function frame
};
/* --------------------------------------------------------------------- */

#endif // SKOOMA_H
