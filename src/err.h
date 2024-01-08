#ifndef SKOOMA_ERR_H
#define SKOOMA_ERR_H

#include "skooma.h"
#include "value.h"


/* Compile-time errors */
extern const char* comperrors[];

#define COMPILE_ERR(F, fmt, ...) error(F, fmt __VA_OPT__(, ) __VA_ARGS__)



/* Parse arglist */
#define ARGC_LIMIT_ERR(F, limit)                                                         \
    COMPILE_ERR(F, "Can't have more than %u arguments.", limit)
/* ------------- */

/* startscope() */
#define SCOPE_LIMIT_ERR(F, limit)                                                        \
    COMPILE_ERR(F, "Scope nesting limit reached (limit %u).", limit)
/* ------------- */

/* add_upval() */
#define UPVALUE_LIMIT_ERR(F, fnname, limit)                                              \
    COMPILE_ERR(                                                                         \
        F,                                                                               \
        "<fn %s>: closure variables (upvalues) limit reached (limit %d).",               \
        fnname,                                                                          \
        limit)
/* ------------- */

/* classdec() */
#define CLASS_INHERIT_ERR(F, cclass)                                                     \
    COMPILE_ERR(F, "class '%s' can't impl itself.", cclass);
/* ------------- */

/* _super() */
#define SUPER_ERR(F) COMPILE_ERR(F, "Can't use 'super' outside of a class.");
#define NO_SUPER_ERR(F)                                                                  \
    COMPILE_ERR(F, "Can't use 'super', class does not have a superclass.");
/* ------------- */

/* _self() */
#define SELF_ERR(F) COMPILE_ERR(F, "Can't use 'self' outside of a class.");
/* ------------- */

/* namelist() */
#define NAMELIST_LIMIT_ERR(F, limit)                                                     \
    COMPILE_ERR(F, "Too many names in namelist, limit [%d]", limit)
/* ------------- */

/* explist() */
#define EXPLIST_LIMIT_ERR(F, limit)                                                      \
    COMPILE_ERR(F, "Too many expressions in explist, expected at most %d.", limit)
/* ------------- */

/* exprstm() | foreachvars() */
#define VARLIST_LIMIT_ERR(F, limit)                                                      \
    COMPILE_ERR(F, "Too many variables in varlist, limit [%d].", limit)
/* ------------- */

/* suffixedexp() */
#define CALL_CONST_ERR(F) COMPILE_ERR(F, "Attempted to call a constant value.");
/* ------------- */

/* vararg() */
#define VARARG_ERR(F)                                                                    \
    COMPILE_ERR(                                                                         \
        F,                                                                               \
        "'...' can only be used inside functions that accept variable "                  \
        "number of arguments.")




/* ==================== runtime errors ====================== */

/* Lower-level error invocation, skips the part where
 * we check if the function is protected and instead
 * prints the stack trace and invokes panic handler. */
sk_noret printandpanic(VM* vm);

/* Generic runtime error */
sk_noret runerror(VM* vm, int8_t status);

/* Ordering error */
sk_noret ordererror(VM* vm, Value a, Value b);

/* Binary/Unary arithmetic operation error */
#define operror(vm, l, r, op)                                                            \
    (arisbin(op) ? binoperror(vm, l, r, cast(OMTag, op))                                 \
                 : unoperror(vm, l, cast(OMTag, op)))

/* Binary arithmetic operation error */
sk_noret binoperror(VM* vm, Value a, Value b, OMTag op);

/* Unary arithmetic operation error */
sk_noret unoperror(VM* vm, Value a, OMTag op);

/* Display method (__display__) error */
sk_noret disperror(VM* vm, Value result);

/* Object string format error */
sk_noret ofmterror(VM* vm, int8_t c, Value callee);

/* Stack overflow error */
sk_noret sovferror(VM* vm);

/* Undefined property error */
sk_noret udperror(VM* vm, Value property, OClass* oclass);

/* Return count stack overflow */
sk_noret retovferror(VM* vm, const char* fn);

/* Function invalid argument count error */
sk_noret arityerror(VM* vm, int32_t expected, int32_t got);

/* Call stack overflow (frame count) */
sk_noret fcovferror(VM* vm);

/* Called non-callable value */
sk_noret callerror(VM* vm, Value callee);

/* Invalid property access error */
sk_noret ipaerror(VM* vm, Value notinstance);

/* Global variable redefinition error */
sk_noret redefgerror(VM* vm, const char* gname);

/* Undefined global variable error */
sk_noret udgerror(VM* vm, const char* gname);

/* Assigning to variable defined as 'fixed' error */
sk_noret fixederror(VM* vm, const char* var);

/* 'nil' index error */
sk_noret nilidxerror(VM* vm);

/* Inheritance error */
sk_noret inheriterror(VM* vm, Value notclass);

/* ---------------------------------------------------------- */


#endif
