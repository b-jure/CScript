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


/* TODO: Figure out do we need these after implementing corelib
typedef const char* (
    *ScriptRenameFn)(VM* vm, const char* importer_script, const char* name);


typedef struct ScriptLoadResult ScriptLoadResult;

typedef void (*ScriptLoadFinFn)(VM* vm, const char* name, ScriptLoadResult result);

struct ScriptLoadResult {
    const char* source;
    ScriptLoadFinFn finfn;
    void* userdata;
};

typedef ScriptLoadResult (*ScriptLoadFn)(VM* vm, const char* name);
*/


/* Return the version number */
SK_API sk_number sk_version(VM* vm);


/* Create new virtual machine. */
SK_API VM* sk_create(AllocFn allocator, void* ud);

/* Destroy/cleanup the virtual machine and null-out the provided pointer. */
SK_API void sk_destroy(VM** vmp);


/* Set panic handler and return old one */
SK_API CFunction sk_set_panic(VM* vm, CFunction panicfn);

/* Get panic handler */
SK_API CFunction sk_get_panic(VM* vm);

/* Set allocator function and return old one */
SK_API AllocFn sk_set_alloc(VM* vm, AllocFn allocfn, void* ud);

/* Get allocator function */
SK_API AllocFn sk_get_alloc(VM* vm, void** ud);




#define TT_NONE (-1) // indicates absence of value

/* Value Types */
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
    TT_UPVAL,
    TT_METHOD,
} TypeTag;

#define TT_CNT (TT_NATIVE + 1) // Types count


SK_API int sk_type(const VM* vm, int idx);
SK_API const char* sk_typename(const VM* vm, int idx);
SK_API const char* sk_tagname(const VM* vm, TypeTag tag);

SK_API int sk_isnil(const VM* vm, int idx);
SK_API int sk_isnumber(const VM* vm, int idx);
SK_API int sk_isstring(const VM* vm, int idx);
SK_API int sk_isbool(const VM* vm, int idx);
SK_API int sk_isclass(const VM* vm, int idx);
SK_API int sk_isinstance(const VM* vm, int idx);
SK_API int sk_isnative(const VM* vm, int idx);



/* Comparison operations */
typedef enum {
    CMP_EQ = 0,
    CMP_LT,
    CMP_GT,
    CMP_LEQ,
    CMP_GEQ,
} Cmp;

SK_API int sk_compare(VM* vm, int idx1, int idx2, Cmp op);





/*
 * PUSH from C -> STACK
 */


/* Tag for overloaded methods.
 * Each tag is index into overloaded methods array.
 * Each class has this array, use this enum to
 * interface with the 'sk_pushoverloaded' function
 * in order to soundly fetch overloaded methods if any. */
typedef enum {
    OM_INIT = 0,
    OM_DISPLAY,
} OMTag;

#define OM_CNT (OM_DISPLAY + 1)



/* Tag for special fields.
 * Each tag is index into special fields array.
 * Each class has this array, use this enum to
 * interface with the 'sk_pushspecialfield' function
 * in order to soundly fetch special fields if any. */
typedef enum {
    SF_DEBUG = 0,
} SFTag;

#define SF_CNT (SF_DEBUG + 1)



SK_API void sk_pushnil(VM* vm);
SK_API void sk_pushnumber(VM* vm, sk_number number);
SK_API void sk_pushstring(VM* vm, const char* str, size_t len);
SK_API const char* sk_pushfstring(VM* vm, const char* fmt, ...);
SK_API void sk_pushcstring(VM* vm, const char* str);
SK_API void sk_pushbool(VM* vm, int boolean);
SK_API void sk_pushmethod(VM* vm, int idx, const char* method);
SK_API TypeTag sk_pushfield(VM* vm, int idx, const char* field);
SK_API void sk_pushoverloaded(VM* vm, int idx, OMTag method);
SK_API int sk_pushglobal(VM* vm, const char* name);
SK_API void sk_push(VM* vm, int idx);



/* Check if C-stack has enough space */
SK_API int sk_ensurestack(VM* vm, int n);


/* option for multiple returns in sk_call and sk_pcall */
#define SK_MULRET (-1)


/* Check if function results would overflow the stack */
#define skapi_checkresults(vm, n, nr)                                                    \
    sk_checkapi(                                                                         \
        vm,                                                                              \
        (nr) == SK_MULRET || (((vm)->sp - (vm)->stack) + (n) + (nr)) < VM_STACK_MAX,     \
        "function results overflow the stack.")

/* Check if stack has enough elements */
#define skapi_checkelems(vm, n)                                                          \
    sk_checkapi(                                                                         \
        vm,                                                                              \
        ((vm)->sp - last_frame(vm).callee) > (n),                                        \
        "not enough elements in the stack.");

/* Check if the error code is valid (exists) */
#define skapi_checkerrcode(vm, errcode)                                                  \
    sk_checkapi(vm, (errcode) >= S_EARG && (errcode) <= S_EGLOBALREDEF, "invalid errcode")

/* Check if OMTag is valid */
#define skapi_checkomtag(vm, omtag)                                                      \
    sk_checkapi(vm, (omtag) >= 0 && (omtag) < OM_CNT, "invalid OMTag")

#define skapi_checksftag(vm, sftag)                                                      \
    sk_checkapi(vm, (sftag) >= 0 && (sftag) < SF_CNT, "invalid SFTag")




/* Convert value on the stack at 'idx' into string. */
SK_API const char* sk_tostring(VM* vm, int idx);



/*
 * GET from STACK -> C
 */

SK_API int sk_getbool(const VM* vm, int idx, int* isbool);
SK_API sk_number sk_getnumber(const VM* vm, int idx, int* isnum);
SK_API const char* sk_getstring(const VM* vm, int idx);
SK_API size_t sk_strlen(const VM* vm, int idx);
SK_API int sk_gettop(const VM* vm);




/*
 * STACK MANIPULATION
 */

SK_API void sk_settop(VM* vm, int idx);
#define sk_pop(vm, n) sk_settop(vm, -(n)-1)
SK_API int sk_setfield(VM* vm, int idx, const char* field);
SK_API void sk_remove(VM* vm, int idx);
SK_API void sk_insert(VM* vm, int idx);
SK_API void sk_replace(VM* vm, int idx);





/*
 * CALL
 */


/* Type of protected function */
typedef void (*ProtectedFn)(VM* vm, void* userdata);


/* Call return values, these indicate which kind
 * of function was called or if call error occurred. */
#define CALL_SKOOMAFN 0
#define CALL_NATIVEFN 1
#define CALL_CLASS    2


/* Call the value on the stack located on
 * the top right before arguments (argc).
 * Returns function type which was called or
 * if error occured as negative integer. */
SK_API void sk_call(VM* vm, int argc, int retcnt);





/*
 * ERROR
 */

/* Runtime status codes */
typedef enum {
    S_OK = 0,
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

/* Invoke runtime error */
SK_API int sk_error(VM* vm, Status errcode);






/*
 * INTERNED STRINGS
 */

#define sizeofstr(str) (sizeof(str) - 1)
/* Value types */
#define SS_NIL    0
#define SS_NUM    1
#define SS_STR    2
#define SS_BOOL   3
#define SS_CLASS  4
#define SS_INS    5
#define SS_FUNC   6
#define SS_CLS    7
#define SS_NAT    8
#define SS_UPVAL  9
#define SS_METHOD 10
/* Boolean strings */
#define SS_TRUE  11
#define SS_FALSE 12
/* Class overload-able method/field names */
#define SS_INIT 13
#define SS_DISP 14
#define SS_DBG  15
/* Native functions argument names */
#define SS_MANU       16
#define SS_AUTO       17
#define SS_ASSERT_MSG 18
#define SS_ERROR      19
#define SS_ASSERT     20
/* Size */
#define SS_SIZE (sizeof(static_str) / sizeof(static_str[0]))

typedef struct {
    const char* name;
    const uint8_t len;
} InternedString;

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
 /* Class special field names. */
    {"__debug",           sizeofstr("__debug")          },
 /* corelib statics */
    {"manual",            sizeofstr("manual")           },
    {"auto",              sizeofstr("auto")             },
    {"assertion failed.", sizeofstr("assertion failed.")},
    {"Error: ",           sizeofstr("Error: ")          },
    {"Assert: ",          sizeofstr("Assert: ")         },
};


#endif // SKOOMA_H
