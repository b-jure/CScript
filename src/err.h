#ifndef SKOOMA_ERR_H
#define SKOOMA_ERR_H

#include "skooma.h"
#include "value.h"

/* FORMAT STRINGS */

#define FMT_VAR_FIXED_ERR(len, start)                                                    \
    "Can't assign to variable '%.*s' it is declared as 'fixed'.\n\n"                     \
    "Try removing 'fixed' from variable declaration.\n"                                  \
    "Example:\n\tvar a = \"mutable\"; // mutable variable\n\tfixed var b "               \
    "= "                                                                                 \
    "\"immutable\"; // immutable variable\n\tvar fixed c = \"also "                      \
    "immutable\"; // "                                                                   \
    "immutable variable",                                                                \
        len, start







/* Compile-time errors */


#define COMPILE_ERR(F, fmt, ...) error(F, fmt __VA_OPT__(, ) __VA_ARGS__)

/* make_constant() */
#define CONSTANT_LIMIT_ERR(F, fnstr, limit)                                              \
    COMPILE_ERR(                                                                         \
        F,                                                                               \
        "<fn %s>: Too many constants defined in a single chunk (limit "                  \
        "%u).",                                                                          \
        fnstr,                                                                           \
        limit)
/* ------------- */

/* codeset() */
#define LOCAL_FIXED_ERR(F, token)                                                        \
    COMPILE_ERR(F, FMT_VAR_FIXED_ERR((token).len, (token).start))
/* ------------- */

/* globalvar() */
#define GLOBALS_LIMIT_ERR(F, limit)                                                      \
    COMPILE_ERR(F, "Too many global values defined in script (limit %u).", limit)
/* ------------- */

/* ------------- */
#define GLOBAL_REDEFINITION_ERR(F, len, start)                                           \
    COMPILE_ERR(F, "Variable redefinition '%.*s'.", len, start)
/* ------------- */

/* local_new() */
#define LOCAL_LIMIT_ERR(F, limit)                                                        \
    COMPILE_ERR(F, "Too many local values defined in script (limit %u).", limit)
/* ------------- */

/* get_local() */
#define LOCAL_DEFINITION_ERR(F, len, start)                                              \
    COMPILE_ERR(F, "Can't read local variable %.*s in its own initializer.", len, start)
/* ------------- */

/* make_local() */
#define LOCAL_REDEFINITION_ERR(F, len, start)                                            \
    COMPILE_ERR(F, "Redefinition of local variable '%.*s'.", len, start)
/* ------------- */

/* codeloop() | patchjmp() */
#define JUMP_LIMIT_ERR(F, limit)                                                         \
    COMPILE_ERR(                                                                         \
        F,                                                                               \
        "Too much code to jump over. Bytecode indexing limit reached "                   \
        "[%u].",                                                                         \
        limit)
/* ------------- */

/* switchstm() */
#define SWITCH_DEFAULT_ERR(F) COMPILE_ERR(F, "Multiple 'default' labels.")
#define SWITCH_NOCASE_ERR(F)  COMPILE_ERR(F, "Can't have statements before first case.")
#define SWITCH_RBRACE_ERR(F)  COMPILE_ERR(F, "Expect '}' at the end of 'switch'.")
/* ------------- */

/* switchconstants() */
#define SWITCH_DUPLICATE_ERR(F, val)                                                     \
    COMPILE_ERR(F, "Already have case with constant '%s'.", val)
/* ------------- */

/* continuestm() */
#define CONTINUE_ERR(F) COMPILE_ERR(F, "'continue' statement not in loop statement.")
/* ------------- */

/* breakstm() */
#define BREAK_ERR(F) COMPILE_ERR(F, "'break' statement not in loop or switch statement.");
/* ------------- */

/* returnstm() */
#define RETURN_INIT_ERR(F, initstr)                                                      \
    COMPILE_ERR(F, "Can't return a value from '%s' method.", initstr)
/* ------------- */

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

/* Binary arithmetic operation error */
sk_noret binoperr(VM* vm, Value a, Value b, OMTag op);
/* Unary arithmetic operation error */
sk_noret unoperr(VM* vm, Value a, OMTag op);


#endif
