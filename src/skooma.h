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


/* Native C function signature */
typedef int (*CFunction)(VM* vm);



/*
 * Runtime status codes
 */
#define SK_ROK      0 // no errors
#define SK_RERR     1 // generic runtime error
#define SK_RETYPE   2 // invalid type
#define SK_RESTKOVF 3 // stack overflow
#define SK_REARGC   4 // argc does not match arity
#define SK_REARGCVA 5 // argc is smaller than arity
#define SK_REFRAME  6 // CallFrame limit reached






/*
 * Value Types
 */
#define SK_TNONE     (-1)
#define SK_TNIL      0
#define SK_TNUMBER   1
#define SK_TSTRING   2
#define SK_TBOOL     3
#define SK_TCLASS    4
#define SK_TINSTANCE 5
#define SK_TFUNCTION 6
#define SK_TC        7 // Types count

SK_API int         sk_type(const VM* vm, int idx);
SK_API const char* sk_typename(const VM* vm, int idx);

SK_API int sk_isnil(const VM* vm, int idx);
SK_API int sk_isnumber(const VM* vm, int idx);
SK_API int sk_isstring(const VM* vm, int idx);
SK_API int sk_isbool(const VM* vm, int idx);
SK_API int sk_isclass(const VM* vm, int idx);
SK_API int sk_isinstance(const VM* vm, int idx);


/* Create new virtual machine. */
SK_API VM* sk_create(Config* cfg);

/* Destroy/cleanup the virtual machine. */
SK_API void sk_destroy(VM** vmp);




/*
 * PUSH from C -> STACK
 */

SK_API void sk_pushnil(VM* vm);
SK_API void sk_pushnumber(VM* vm, sk_number number);
SK_API void sk_pushstring(VM* vm, const char* str, size_t len);
SK_API void sk_pushcstring(VM* vm, const char* str);
SK_API void sk_pushbool(VM* vm, int boolean);
SK_API int  sk_pushmethod(VM* vm, int idx, const char* method);
SK_API int  sk_pushglobal(VM* vm, const char* name);
SK_API void sk_push(VM* vm, int idx);



/* Check if C-stack has enough space */
SK_API int sk_ensurestack(VM* vm, int n);

/* Check if stack has enough elements */
#define sk_checkelems(vm, n)                                                             \
    sk_checkapi(                                                                         \
        vm,                                                                              \
        ((vm)->sp - (vm)->frames[(vm)->fc - 1].callee) > (n),                            \
        "not enough elements in the stack.");





/*
 * GET from STACK -> C
 */

SK_API int         sk_getbool(const VM* vm, int idx, int* isbool);
SK_API sk_number   sk_getnumber(const VM* vm, int idx, int* isnum);
SK_API const char* sk_getstring(const VM* vm, int idx);
SK_API size_t      sk_rawlen(const VM* vm, int idx);
SK_API int         sk_gettop(const VM* vm);




/*
 * STACK MANIPULATION
 */

SK_API void sk_settop(VM* vm, int idx);
/* Pops n values off the stack */
#define sk_pop(vm, n) sk_settop(vm, -(n)-1)
SK_API void sk_remove(VM* vm, int idx);
SK_API void sk_insert(VM* vm, int idx);
SK_API void sk_replace(VM* vm, int idx);




/*
 * CALL
 */

/* Call return values, these indicate which kind
 * of function was called or if call error occurred. */
#define CALL_ERR      0
#define CALL_SKOOMAFN -1
#define CALL_NATIVEFN -2
#define CALL_CLASS    -3


/* Call the value on the stack located on
 * the top right before arguments (argc).
 * Returns function type which was called or
 * if error occured as negative integer. */
SK_API int sk_call(VM* vm, int argc, int retcnt);





/*
 * INTERNED STRINGS
 */

#define sizeofstr(str) (sizeof(str) - 1)
/* Value types */
#define SS_STR   0
#define SS_NUM   1
#define SS_INS   2
#define SS_CLASS 3
#define SS_BOOL  4
#define SS_NIL   5
#define SS_FUNC  6
/* Boolean strings */
#define SS_TRUE  7
#define SS_FALSE 8
/* Class overload-able method names */
#define SS_INIT 9
/* Native functions argument names */
#define SS_MANU       10
#define SS_AUTO       11
#define SS_ASSERT_MSG 12
#define SS_ERROR      13
#define SS_ASSERT     14
/* Size */
#define SS_SIZE (sizeof(static_str) / sizeof(static_str[0]))

typedef struct {
    const char*   name;
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
 /* Boolean strings */
    {"true",              sizeofstr("true")             },
    {"false",             sizeofstr("false")            },
 /* Class overload-able method names. */
    {"__init__",          sizeofstr("__init__")         },
 /* corelib statics */
    {"manual",            sizeofstr("manual")           },
    {"auto",              sizeofstr("auto")             },
    {"assertion failed.", sizeofstr("assertion failed.")},
    {"Error: ",           sizeofstr("Error: ")          },
    {"Assert: ",          sizeofstr("Assert: ")         },
};


#endif // SKOOMA_H
