/*
 ** Skooma FFI is mostly reimplementation of Lua FFI
 ** https://www.lua.org/source/5.4/lua.h.html
 */


#ifndef SKOOMA_H
#define SKOOMA_H

#include "skconf.h"


#define SK_VERSION_MAJOR   "1"
#define SK_VERSION_MINOR   "0"
#define SK_VERSION_RELEASE "0"

#define SK_VERSION_NUMBER      100
#define SK_VERSION_RELEASE_NUM (SK_VERSION_NUMBER * 100);

#define SK_VERSION   "Skooma " SK_VERSION_MAJOR "." SK_VERSION_MINOR
#define SK_RELEASE   SK_VERSION "." SK_VERSION_RELEASE
#define SK_COPYRIGHT SK_RELEASE " Copyright (C) 2024 B. Jure"
#define SK_AUTHORS   "B. Jure"



/*
 * User can define his own sk_assert if he requires
 * different/custom behaviour.
 * By default sk_assert is a nop.
 */
#if !defined(sk_assert)
#define sk_assert(vm, cond, msg) ((void)0)
#endif

#if !defined(sk_checkapi)
#define sk_checkapi(vm, cond, msg) sk_assert(cond)
#endif



/*
 * Locking mechanism (by default nop).
 * By default skooma does not assume your VM is shared
 * by multiple threads, it is up to user to decide whether
 * to define his own sk_lock/sk_unlock.
 */
#if !defined(S_LOCK_USR)
#define sk_lock(vm)   ((void)0)
#define sk_unlock(vm) ((void)0)
#endif




/*
 * ============== Skooma types ==============
 */


/* Skooma number */
typedef double sk_number;


/* Virtual Machine */
typedef struct VM VM;


/* Holds debug information */
typedef struct DebugInfo DebugInfo;


/* Native C function signature */
typedef int (*CFunction)(VM* vm);


/* Panic handler (same signature as 'CFunction') */
typedef int (*PanicFn)(VM* vm);


/* Memory allocator function signature. */
typedef void* (*AllocFn)(void* ptr, size_t newsize, void* userdata);


/* Reader function signature. */
typedef const char* (*ReadFn)(VM* vm, void* userdata, size_t* szread);



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

/* -------------------------------------------------*/ // Skooma types






/*
 * ============== Class method/field tags ==============
 */

/* Tag for overload-able class methods. */
typedef enum {
    OM_INIT = 0,
    OM_DISPLAY,
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
} OMTag;

/* Only useful if SK_OVERLOAD_OPS is defined. */
#define omisunop(omtag) ((omtag) == OM_NOT | (omtag) == OM_UMIN)

/* Tag for special class fields. */
typedef enum {
    SF_DEBUG = 0,
    SF_CNT, // keep this last
} SFTag;

/* -------------------------------------------------*/ // class method/field tag






/*
 * ========== API check ==========
 */

SK_API int sk_ensurestack(VM* vm, int n);

/* -------------------------------------------------*/ // API check







/*
 * ========== create/destroy VM ==========
 */

SK_API VM* sk_create(AllocFn allocator, void* ud);
SK_API void sk_resetvm(VM* vm);
SK_API void sk_destroy(VM** vmp);

/* -------------------------------------------------*/ // create/destroy







/*
 * ========== Ordering and arithmetic functions ==========
 */

typedef enum {
    ORD_EQ = 0, // equal '=='
    ORD_NE, // not equal '!='
    ORD_LT, // less '<'
    ORD_GT, // greater '>'
    ORD_LE, // less or equal '<='
    ORD_GE, // greater or equal '>='
    ORD_CNT, // Ord count
} Ord;

SK_API void sk_compare(VM* vm, int idx1, int idx2, Ord op);

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
} Ar;

#define arisbin(ar) ((ar) >= AR_ADD && (ar) <= AR_POW)
#define arisun(ar)  ((ar) >= AR_NOT && (ar) <= AR_UMIN)

SK_API void sk_arith(VM* vm, Ar op);

/* -------------------------------------------------*/ // Ordering/arith






/*
 * ========== push functions, skooma -> stack ==========
 */

SK_API void sk_pushnil(VM* vm);
SK_API void sk_pushnumber(VM* vm, sk_number number);
SK_API void sk_pushstring(VM* vm, const char* str, size_t len);
SK_API void sk_pushcstring(VM* vm, const char* str);
SK_API const char* sk_pushfstring(VM* vm, const char* fmt, ...);
SK_API void sk_pushbool(VM* vm, int boolean);
SK_API void sk_pushcfn(VM* vm, CFunction fn, int args, int isva, unsigned int upvals);
SK_API void sk_push(VM* vm, int idx);

/* -------------------------------------------------*/ // push functions




/*
 * ========== get functions, skooma -> stack ==========
 */

SK_API int8_t sk_getmethod(VM* vm, int idx, const char* method);
SK_API int sk_getglobal(VM* vm, const char* name);
SK_API TypeTag sk_getfield(VM* vm, int idx, const char* field);

/* -------------------------------------------------*/ // get functions





/*
 * ========== set functions ==========
 */

SK_API void sk_settop(VM* vm, int idx);
SK_API int sk_setglobal(VM* vm, const char* name, int isfixed);
SK_API int sk_setfield(VM* vm, int idx, const char* field);
SK_API PanicFn sk_setpanic(VM* vm, PanicFn panicfn);
SK_API ReadFn sk_setreader(VM* vm, ReadFn readfn);
SK_API AllocFn sk_setalloc(VM* vm, AllocFn allocfn, void* ud);

/* -------------------------------------------------*/ // set functions






/*
 * ========== get/access functions, stack -> C ==========
 */

SK_API int sk_isnil(const VM* vm, int idx);
SK_API int sk_isnumber(const VM* vm, int idx);
SK_API int sk_isstring(const VM* vm, int idx);
SK_API int sk_isbool(const VM* vm, int idx);
SK_API int sk_isclass(const VM* vm, int idx);
SK_API int sk_isinstance(const VM* vm, int idx);
SK_API int sk_isnative(const VM* vm, int idx);
SK_API int sk_ismethod(const VM* vm, int idx);
SK_API int sk_isclosure(const VM* vm, int idx);
SK_API int sk_type(const VM* vm, int idx);
SK_API const char* sk_typename(const VM* vm, int idx);
SK_API const char* sk_tagname(const VM* vm, TypeTag type);

SK_API PanicFn sk_getpanic(VM* vm);
SK_API ReadFn sk_getreader(VM* vm);
SK_API AllocFn sk_getalloc(VM* vm, void** ud);
SK_API int sk_getbool(const VM* vm, int idx, int* isbool);
SK_API sk_number sk_getnumber(const VM* vm, int idx, int* isnum);
SK_API const char* sk_getstring(const VM* vm, int idx);
SK_API CFunction sk_tocfunction(const VM* vm, int idx);

/* -------------------------------------------------*/ // get/access







/*
 * ========== stack manipulation functions ==========
 */

SK_API void sk_settop(VM* vm, int idx);
#define sk_pop(vm, n) sk_settop(vm, -(n)-1)
SK_API void sk_rotate(VM* vm, int idx, int n);
SK_API void sk_copy(VM* vm, int src, int dest);
/**
 * Pops off the stack top and copies over that
 * value into the stack slot at 'idx'.
 **/
#define sk_replace(vm, idx) (sk_copy(vm, -1, idx), sk_pop(vm, 1))
SK_API int sk_setfield(VM* vm, int idx, const char* field);
SK_API int sk_setglobal(VM* vm, const char* name, int isfixed);
/**
 * Removes the value on stack at 'idx' shifting
 * the stack once to the left and adjusting the
 * stack top by one to compensate the shift.
 **/
#define sk_remove(vm, idx) (sk_rotate(vm, idx, -1), sk_pop(vm, 1))
/**
 * Inserts the value on top of the stack at 'idx'
 * shifting the stack once to the right.
 **/
#define sk_insert(vm, idx) sk_rotate(vm, idx, 1)

/* -------------------------------------------------*/ // stack manipulation







/*
 * ========== error reporting ==========
 */

/* Runtime status codes */
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
} Status;

SK_API Status sk_getstatus(VM* vm);
SK_API int sk_error(VM* vm, Status errcode);

/* -------------------------------------------------*/ // error reporting






/*
 * ========== miscellaneous functions ==========
 */
SK_API int sk_version(VM* vm);
SK_API const char* sk_tostring(VM* vm, int idx);
SK_API int sk_getupvalue(VM* vm, int fidx, int idx);
SK_API int sk_setupvalue(VM* vm, int fidx, int idx);
SK_API size_t sk_strlen(const VM* vm, int idx);

/* -------------------------------------------------*/ // misc functions






/*
 * ========== call/load/run functions ==========
 */

/* option for multiple returns (retcnt) in sk_call and sk_pcall */
#define SK_MULRET (-1)

SK_API Status sk_pcall(VM* vm, int argc, int retcnt);
SK_API void sk_call(VM* vm, int argc, int retcnt);

SK_API Status sk_load(VM* vm, ReadFn reader, void* userdata, const char* dbgname);

/* -------------------------------------------------*/ // call/load/run






/*
 * ========== garbage collector ==========
 */

typedef enum {
    GCO_STOP, // stop GC
    GCO_RESTART, // restart GC (start if stopped)
    GCO_COLLECT, // perform full GC cycle
    GCO_COUNT, // get number of bytes allocated
    GCO_ISRUNNING, // check whether GC is stopped
    GCO_NEXTGC, // set bytes amount when the next GC will trigger
} GCOpt; // Garbage collector options

/* ---------------------------------------- */ // garbage collector








/*
 * ============= debug API =============
 * @https://www.lua.org/manual/5.4/manual.html#lua_Debug
 */

typedef enum {
    DW_FNGET = (1 << 0), // load the function on top of the stack (processed first)
    DW_LINE = (1 << 1), // fill 'line'
    DW_FNINFO = (1 << 2), // fill all function info in 'DebugInfo'
    DW_FNSRC = (1 << 3), // fill function source information
    DW_FNPUSH = (1 << 4), // push current function on the stack (processed last)
} DebugWhat; // bits for creating debug bitmask ('sk_getinfo')

SK_API uint8_t sk_getstack(VM* vm, int32_t level, DebugInfo* di);
SK_API uint8_t sk_getinfo(VM* vm, uint8_t dbmask, DebugInfo* di);

/* Forward declare the private type */
typedef struct CallFrame CallFrame;

struct DebugInfo {
    const char* name; // function name (declaration name in Skooma script)
    const char* type; // function type ('Skooma', 'main' or 'C')
    const char* source; // function source
    size_t srclen; // length of 'source'
    int32_t line; // current line in Skooma script
    uint32_t nups; // number of function upvalues
    uint32_t nparams; // number of function parameters
    uint8_t isvararg; // is function vararg ('...')
    int32_t defline; // line number where the function definition starts
    int32_t deflastline; // line number where the function definition ends
    char shortsrc[SK_SRCID_MAX];
    /* private */
    CallFrame* frame; // active function frame
};

/* ---------------------------------------- */ // debug API

#endif // SKOOMA_H
