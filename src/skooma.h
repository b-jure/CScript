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



#if !defined(sk_assert)
    #define sk_assert(vm, cond, msg) ((void)0)
#endif

#if !defined(sk_checkapi)
    #define sk_checkapi(vm, cond, msg) sk_assert(cond)
#endif

/*
 * Locking mechanism.
 * By default Skooma uses POSIX mutex from libc.
 * User can disable this in 'skconf.h', additionally
 * user can provide his own locking mechanism by disabling
 * default locking in 'skconf.h' and then defining his own sk_lock
 * and sk_unlock.
 */
#ifdef S_LOCK_DFLT
    #define sk_lock(vm)
    #define sk_unlock(vm)
#elif defined(S_LOCK_USR)
    #undef sk_lock(vm)
    #undef sk_unlock(vm)
#else
    #undef sk_lock(vm)
    #undef sk_unlock(vm)
#endif




/*
 * Skooma uses only doubles for its number
 * representation there is no integer type,
 * this is to retain consistency between NaN
 * boxing and tagged union value representation.
 */
#define SK_NUMBER double
#define SK_Number SK_NUMBER

/* C functions signature. */
typedef int (*CFunction)(VM* vm);




/* Create new virtual machine. */
SK_API VM* sk_create(Config* cfg);

/* Destroy/cleanup the virtual machine. */
SK_API void sk_destroy(VM** vmp);


/* API for pushing new Values on the C-stack */
SK_API void sk_pushnil(VM* vm);
SK_API void sk_pushnumber(VM* vm, double number);
SK_API void sk_pushstring(VM* vm, const char* str, size_t len);
SK_API void sk_pushcstring(VM* vm, const char* str);
SK_API void sk_pushbool(VM* vm, int boolean);


/* Check if C-stack has enough space */
SK_API int sk_ensurestack(VM* vm, int n);



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

/*
 * API for checking value types on the stack.
 * idx is the index into the C-stack, it can also
 * be a negative integer, this would then reference
 * the end of the stack going backwards.
 */
SK_API int sk_typeof(const VM* vm, int idx);
SK_API int sk_isnil(VM* vm, int idx);
SK_API int sk_isnumber(VM* vm, int idx);
SK_API int sk_isstring(VM* vm, int idx);
SK_API int sk_isbool(VM* vm, int idx);
SK_API int sk_isclass(VM* vm, int idx);
SK_API int sk_isinstance(VM* vm, int idx);



/*
 * API for getting the values from the C-stack.
 *
 * If the Value at 'idx' is not of the type you requested,
 * then the getter will return 0 or NULL.
 * If idx is negative then it refers to the
 * top of the stack going backwards (-1 == stack top).
 */
SK_API int         sk_getbool(const VM* vm, int idx, int* isbool);
SK_API double      sk_getnumber(const VM* vm, int idx, int* isnum);
SK_API const char* sk_getstring(const VM* vm, int idx);
SK_API size_t      sk_rawlen(const VM* vm, int idx);
SK_API int         sk_hasmethod(VM* vm, int idx, const char* method);




/*
 * Stack manipulation
 */

/* Return index of the last value on the stack
 * relative to the current function. */
SK_API int sk_gettop(const VM* vm);

/* Set the new stack top.
 * If the new stack top is higher than previous one,
 * then the nil's get pushed on stack.
 * If the stack top is lower than the previous top,
 * then the values get discarded. */
SK_API void sk_settop(VM* vm, int idx);

/* Pops n values off the stack */
#define sk_pop(vm, n) sk_settop(vm, -(n)-1)

/* Pushes the copy of the Value at idx on the stack. */
SK_API void sk_push(VM* vm, int idx);

/* Removes the Value at idx, shifting down other Values if any. */
SK_API void sk_remove(VM* vm, int idx);

/* Inserts the Value on top at the location of idx, shifting
 * other Values up to give space for the inserted value. */
SK_API void sk_insert(VM* vm, int idx);

/* Pops the Value on top and replaces the Value at idx with it. */
SK_API void sk_replace(VM* vm, int idx);





/*
 * Default interned strings.
 */

typedef struct {
    const char*   name;
    const uint8_t len;
} InternedString;

#define sizeofstr(str) (sizeof(str) - 1)
/* Class initializer */
#define SS_INIT 0
/* Value types */
#define SS_STR   1
#define SS_NUM   2
#define SS_INS   3
#define SS_CLASS 4
#define SS_BOOL  5
#define SS_NIL   6
#define SS_FUNC  7
/* Native functions argument names */
#define SS_MANU       8
#define SS_AUTO       9
#define SS_ASSERT_MSG 10
#define SS_ERROR      11
#define SS_ASSERT     12
/* Size */
#define SS_SIZE (sizeof(static_str) / sizeof(static_str[0]))

static const InternedString static_str[] = {
  /* Class initializer name. */
    {"__init__",          sizeofstr("__init__")         },
 /* (user) Value types */
    {"string",            sizeofstr("string")           },
    {"number",            sizeofstr("number")           },
    {"instance",          sizeofstr("instance")         },
    {"class",             sizeofstr("class")            },
    {"bool",              sizeofstr("bool")             },
    {"nil",               sizeofstr("nil")              },
    {"function",          sizeofstr("function")         },
 /* Native function statics */
    {"manual",            sizeofstr("manual")           },
    {"auto",              sizeofstr("auto")             },
    {"assertion failed.", sizeofstr("assertion failed.")},
    {"Error: ",           sizeofstr("Error: ")          },
    {"Assert: ",          sizeofstr("Assert: ")         },
};




/*
 * Casts
 */

#define cast_uint(e)   ((unsigned int)(e))
#define cast_int(e)    ((int)(e))
#define cast_intptr(e) ((intptr_t)(e))



#endif
