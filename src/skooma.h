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
#define SK_COPYRIGHT SK_RELEASE " Copyright (C) 2023-2024 B. Jure"
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
 * Skooma uses only doubles for its number
 * representation there is no integer type,
 * this is to retain consistency between NaN
 * boxing and tagged union value representation.
 */
#define SK_NUMBER double
#define sk_number SK_NUMBER



/* Virtual Machine */
typedef struct VM VM;


/* Native C function signature */
typedef int (*CFunction)(VM* vm);


/* Memory allocator function signature. */
typedef void* (*AllocFn)(void* ptr, size_t newsize, void* userdata);



/*
 * ============== value types ==============
 */

#define TT_NONE (-1) // indicates absence of value
typedef enum {
    TT_NIL = 0,
    TT_NUMBER,
    TT_STRING,
    TT_BOOL,
    TT_CLASS,
    TT_INSTANCE,
    TT_FUNCTION,
    TT_CLOSURE,
    TT_NATIVE,
    TT_METHOD,
    TT_CNT, // keep this last
} TypeTag;

/* -------------------------------------------------*/



/*
 * ============== Class method/field tags ==============
 */

/* Tag for overload-able class methods. */
typedef enum {
    OM_INIT = 0,
    OM_DISPLAY,
#if defined(S_OVERLOAD_OPS)
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
    OM_AND,
    OM_OR,
#endif
    OM_CNT, // keep this last
} OMTag;

/* Only useful if S_OVERLOAD_OPS is defined. */
#define omisunop(omtag) ((omtag) == OM_NOT | (omtag) == OM_UMIN)

/* Tag for special class fields. */
typedef enum {
    SF_DEBUG = 0,
    SF_CNT, // keep this last
} SFTag;

/* -------------------------------------------------*/



/*
 * ========== API check ==========
 */

SK_API int sk_ensurestack(VM* vm, int n);

/* -------------------------------------------------*/



/*
 * ========== create/destroy VM ==========
 */

SK_API VM* sk_create(AllocFn allocator, void* ud);
SK_API void sk_destroy(VM** vmp);

/* -------------------------------------------------*/





/*
 * ========== Comparison and arithmetic functions ==========
 */

typedef enum {
    CMP_EQ = 0, // equal '=='
    CMP_LT, // less '<'
    CMP_GT, // greater '>'
    CMP_LE, // less or equal '<='
    CMP_GE, // greater or equal '>='
    CMP_CNT, // Cmp count
} Cmp;

SK_API int sk_compare(VM* vm, int idx1, int idx2, Cmp op);

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

/* -------------------------------------------------*/



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

/* -------------------------------------------------*/



/*
 * ========== get functions, skooma -> stack ==========
 */

SK_API void sk_getmethod(VM* vm, int idx, const char* method);
SK_API int sk_getglobal(VM* vm, const char* name);
SK_API TypeTag sk_getfield(VM* vm, int idx, const char* field);

/* -------------------------------------------------*/



/*
 * ========== set functions ==========
 */

SK_API void sk_settop(VM* vm, int idx);
SK_API int sk_setglobal(VM* vm, const char* name, int isfixed);
SK_API int sk_setfield(VM* vm, int idx, const char* field);
SK_API CFunction sk_setpanic(VM* vm, CFunction panicfn);
SK_API AllocFn sk_setalloc(VM* vm, AllocFn allocfn, void* ud);

/* -------------------------------------------------*/


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

SK_API int sk_getbool(const VM* vm, int idx, int* isbool);
SK_API sk_number sk_getnumber(const VM* vm, int idx, int* isnum);
SK_API const char* sk_getstring(const VM* vm, int idx);
SK_API CFunction sk_tocfunction(const VM* vm, int idx);

/* -------------------------------------------------*/



/*
 * ========== stack manipulation functions ==========
 */

SK_API void sk_settop(VM* vm, int idx);
#define sk_pop(vm, n) sk_settop(vm, -(n)-1)
SK_API int sk_setfield(VM* vm, int idx, const char* field);
SK_API int sk_setglobal(VM* vm, const char* name, int isfixed);
SK_API void sk_remove(VM* vm, int idx);
SK_API void sk_insert(VM* vm, int idx);
SK_API void sk_replace(VM* vm, int idx);
SK_API void sk_copy(VM* vm, int src, int dest);
#define sk_replace(vm, idx) (sk_copy(vm, -1, idx), sk_pop(vm, 1))

/* -------------------------------------------------*/



/*
 * ========== call functions ==========
 */

/* option for multiple returns (retcnt) in sk_call and sk_pcall */
#define SK_MULRET (-1)

typedef void (*ProtectedFn)(VM* vm, void* userdata);

SK_API int sk_pcall(VM* vm, int argc, int retcnt);
SK_API void sk_call(VM* vm, int argc, int retcnt);

/* -------------------------------------------------*/




/*
 * ========== miscellaneous functions ==========
 */

/* Runtime status codes */
typedef enum {
    S_OK = 0,
    S_EARUN, // unary arithmetic operation error
    S_EARBIN, // binary arithmetic operation error
    S_EARG, // invalid argument
    S_ECMP, // invalid comparison
    S_ESOVERFLOW, // stack overflow
    S_EFOVERFLOW, // CallFrame overflow
    S_EARGC, // argc does not match function arity
    S_EARGCMIN, // argc is smaller than arity
    S_EBINOP, // binary operator error
    S_EUDPROPERTY, // undefined property
    S_EPACCESS, // invalid property access
    S_EINHERIT, // inheriting from non-class value
    S_EFIXEDASSING, // assigning to fixed value
    S_EUDGLOBAL, // undefined global variable
    S_EGLOBALREDEF, // redefinition of global variable
    S_EDISPLAY, // display method returned invalid value
    S_ECALLVAL, // tried calling non-callable value
} Status;

SK_API int sk_version(VM* vm);
SK_API const char* sk_tostring(VM* vm, int idx);
SK_API int sk_getupvalue(VM* vm, int fidx, int idx);
SK_API int sk_setupvalue(VM* vm, int fidx, int idx);
SK_API size_t sk_strlen(const VM* vm, int idx);
SK_API int sk_error(VM* vm, Status errcode);

/* -------------------------------------------------*/





/*
 * ========== Static strings ==========
 */

/* Indices into static strings table */
typedef enum {
    /* Value types */
    SS_NIL = 0,
    SS_NUM,
    SS_STR,
    SS_BOOL,
    SS_CLASS,
    SS_INS,
    SS_FUNC,
    SS_CLS,
    SS_NAT,
    SS_UPVAL,
    SS_METHOD,
    /* Boolean strings */
    SS_TRUE,
    SS_FALSE,
    /* Class overload-able method names. */
    SS_INIT,
    SS_DISP,
#if defined(S_OVERLOAD_OPS)
    SS_ADD,
    SS_SUB,
    SS_MUL,
    SS_DIV,
    SS_MOD,
    SS_POW,
    SS_NOT,
    SS_UMIN,
    SS_NE,
    SS_EQ,
    SS_LT,
    SS_LE,
    SS_GT,
    SS_GE,
    SS_ANd,
    SS_OR,
#endif
    /* Class special field names. */
    SS_DBG,
    /* Other statics */
    SS_MANU,
    SS_AUTO,
    SS_ASSERT_MSG,
    SS_ERROR,
    SS_ASSERT,
    SS_SIZE,
} SSTag;

typedef struct {
    const char* name;
    const uint8_t len;
} InternedString;

#define sizeofstr(str) (sizeof(str) - 1)
static const InternedString static_str[] = {
  /* Value types */
    {"nil",               sizeofstr("nil")              },
    {"number",            sizeofstr("number")           },
    {"string",            sizeofstr("string")           },
    {"bool",              sizeofstr("bool")             },
    {"class",             sizeofstr("class")            },
    {"instance",          sizeofstr("instance")         },
    {"function",          sizeofstr("function")         },
    {"closure",           sizeofstr("closure")          },
    {"native",            sizeofstr("native")           },
    {"upvalue",           sizeofstr("upvalue")          },
    {"method",            sizeofstr("method")           },
 /* Boolean strings */
    {"true",              sizeofstr("true")             },
    {"false",             sizeofstr("false")            },
 /* Class overload-able method names. */
    {"__init__",          sizeofstr("__init__")         },
    {"__display__",       sizeofstr("__display__")      },
#if defined(S_OVERLOAD_OPS)  // operator overloading enabled?
  /* Overload-able operators */
    {"__add__",           sizeofstr("__add__")          },
    {"__sub__",           sizeofstr("__sub__")          },
    {"__mul__",           sizeofstr("__mul__")          },
    {"__div__",           sizeofstr("__div__")          },
    {"__mod__",           sizeofstr("__mod__")          },
    {"__pow__",           sizeofstr("__pow__")          },
    {"__not__",           sizeofstr("__not__")          },
    {"__umin__",          sizeofstr("__umin__")         },
    {"__ne__",            sizeofstr("__ne__")           },
    {"__eq__",            sizeofstr("__eq__")           },
    {"__lt__",            sizeofstr("__lt__")           },
    {"__le__",            sizeofstr("__le__")           },
    {"__gt__",            sizeofstr("__gt__")           },
    {"__ge__",            sizeofstr("__ge__")           },
    {"__and__",           sizeofstr("__and__")          },
    {"__or__",            sizeofstr("__or__")           },
#endif
  /* Class special field names. */
    {"__debug",           sizeofstr("__debug")          },
 /* Other statics */
    {"manual",            sizeofstr("manual")           },
    {"auto",              sizeofstr("auto")             },
    {"assertion failed.", sizeofstr("assertion failed.")},
    {"Error: ",           sizeofstr("Error: ")          },
    {"Assert: ",          sizeofstr("Assert: ")         },
};
#undef sizeofstr

/* -------------------------------------------------*/

#endif // SKOOMA_H
