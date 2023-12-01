#ifndef __SKOOMA_ERR_H__
#define __SKOOMA_ERR_H__

/**
 * FORMAT STRINGS
 **/
#define FMT_VAR_FIXED_ERR(len, start)                                           \
    "Can't assign to variable '%.*s' it is declared as 'fixed'.\n\n"            \
    "Try removing 'fixed' from variable declaration.\n"                         \
    "Example:\n\tvar a = \"mutable\"; // mutable variable\n\tfixed var b = "    \
    "\"immutable\"; // immutable variable\n\tvar fixed c = \"also "             \
    "immutable\"; // "                                                          \
    "immutable variable",                                                       \
        len, start

#define FMT_VAR_UNDEFINED_ERR(name) "Undefined variable '%s'.", name







/**
 * Compile-time errors
 **/

/* Parse constant */
#define CONSTANT_LIMIT_ERR(F, fnstr, limit)                                     \
    error(                                                                      \
        F,                                                                      \
        "<fn %s>: Too many constants defined in a single chunk (limit %u).",    \
        fnstr,                                                                  \
        limit)
/* ------------- */

/* Parse variable */
#define VAR_UNDEFINED_ERR(F, varname) error(F, FMT_VAR_UNDEFINED_ERR(varname))
#define VAR_FIXED_ERR(F, len, start)  error(F, FMT_VAR_FIXED_ERR(len, start))
#define GLOBALS_LIMIT_ERR(F, limit)                                             \
    error(F, "Too many global values defined in script (limit %u).", limit)
#define GLOBAL_REDEFINITION_ERR(F, len, start)                                  \
    error(F, "Variable redefinition '%.*s'.", len, start)
#define LOCAL_LIMIT_ERR(F, limit)                                               \
    error(F, "Too many local values defined in script (limit %u).", limit)
#define LOCAL_DEFINITION_ERR(F, len, start)                                     \
    error(                                                                      \
        F,                                                                      \
        "Can't read local variable %.*s in its own initializer.",               \
        len,                                                                    \
        start)
#define LOCAL_REDEFINITION_ERR(F, len, start)                                   \
    error(F, "Redefinition of local variable '%.*s'.", len, start)
/* ------------- */

/* codeloop() */
#define JUMP_LIMIT_ERR(F, limit)                                                \
    error(                                                                      \
        F,                                                                      \
        "Too much code to jump over. Bytecode indexing limit reached [%u].",    \
        limit)
/* ------------- */

/* switchstm() */
#define SWITCH_DEFAULT_ERR(F) error(F, "Multiple 'default' labels.")
#define SWITCH_NOCASE_ERR(F)  error(F, "Can't have statements before first case.")
#define SWITCH_RBRACE_ERR(F)  error(F, "Expect '}' at the end of 'switch'.")
#define SWITCH_DUPLICATE_ERR(F, val)                                            \
    error(F, "Already have case with constant '%s'.", val)
/* ------------- */

/* continuestm() */
#define CONTINUE_ERR(F) error(F, "'continue' statement not in loop statement.")
/* ------------- */

/* breakstm() */
#define BREAK_ERR(F)                                                            \
    error(F, "'break' statement not in loop or switch statement.");
/* ------------- */

/* Parse 'return' */
#define RETURN_INIT_ERR(F, initstr)                                             \
    error(F, "Can't return a value from '%s' method.", initstr)
/* ------------- */

/* Parse arglist */
#define ARGC_LIMIT_ERR(F, limit)                                                \
    error(F, "Can't have more than %u arguments.", limit)
/* ------------- */

/* scope_start() */
#define SCOPE_LIMIT_ERR(F, limit)                                               \
    error(F, "Scope nesting limit reached (limit %u).", limit)
/* ------------- */

/* Parse closure upvalue */
#define UPVALUE_LIMIT_ERR(F, fnname, limit)                                     \
    error(                                                                      \
        F,                                                                      \
        "<fn %s>: closure variables (upvalues) limit reached (limit %u).",      \
        fnname,                                                                 \
        limit)
/* ------------- */

/* classdec() */
#define CLASS_INHERIT_ERR(F, cclass)                                            \
    error(F, "class '%s' can't impl itself.", cclass);
#define SUPER_ERR(F) error(F, "Can't use 'super' outside of a class.");
#define NO_SUPER_ERR(F)                                                         \
    error(F, "Can't use 'super', class does not have a superclass.");
#define SELF_ERR(F) error(F, "Can't use 'self' outside of a class.");
/* ------------- */

/* namelist() */
#define NAMELIST_LIMIT_ERR(F, limit)                                            \
    error(F, "Too many names in namelist, limit [%d]", limit)
/* ------------- */

/* explist() */
#define EXPLIST_LIMIT_ERR(F, limit)                                             \
    error(F, "Too many expressions in explist, expected at most %d.", limit)
/* ------------- */

/* exprstm() */
#define VARLIST_LIMIT_ERR(F, limit)                                             \
    error(F, "Too many variables in varlist, limit [%d].", limit)
/* ------------- */

/* suffixedexp() */
#define CALL_CONST_ERR(F) error(F, "Attempted to call a constant value.");
/* ------------- */








/**
 * NATIVE FUNCTIONS ERRORS
 **/

#define NATIVE_FN_ERR(fn, err, ...) ("<native-fn " #fn ">: " err)
#define ERR_NEW(vm, err)            ObjString_from(vm, err, sizeof(err) - 1)

/* native_clock */
#define CLOCK_ERR                                                               \
    NATIVE_FN_ERR(                                                              \
        clock,                                                                  \
        "Processor time is not available or its value cannot be represented.")
/* ------------- */

/* native_isfield */
#define ISFIELD_INSTANCE_ERR                                                    \
    NATIVE_FN_ERR(isfield, "Receiver is not a class instance.")
#define ISFIELD_FIELD_TYPE_ERR NATIVE_FN_ERR(isfield, "'field' is not a string.")
/* ------------- */

/* native_gcfactor */
#define GC_FACTOR_ARG_ERR                                                       \
    NATIVE_FN_ERR(                                                              \
        gcfactor,                                                               \
        "invalid argument, grow factor must be a number, either '0' or '>1'.")
/* ------------- */

#define INVALID_FIRST_ARG_TYPE(type)                                            \
    "invalid first argument type, argument must be #type."
#define INVALID_SECOND_ARG_TYPE(type)                                           \
    "invalid second argument type, argument must be #type."

/* native_gcmode */
#define GC_MODE_ARG_ERR NATIVE_FN_ERR(gcmode, INVALID_FIRST_ARG_TYPE(string))
#define GC_MODE_INVALID_MODE_ERR                                                \
    NATIVE_FN_ERR(                                                              \
        gcmode,                                                                 \
        "invalid mode string, mode can be either 'auto' or 'manual'.")
/* ------------- */

/* native_gcset */
#define GC_SET_ARG_ERR NATIVE_FN_ERR(gcset, INVALID_FIRST_ARG_TYPE(number))

#define GC_SET_NEGATIVE_LIMIT_ERR                                               \
    NATIVE_FN_ERR(gcset, "limit can't be negative, it must be positive or 0.")
/* ------------- */

/* native_assertf */
#define ASSERTF_SECOND_ARG_TYPE_ERR                                             \
    NATIVE_FN_ERR(assertf, INVALID_SECOND_ARG_TYPE(string))
/* ------------- */

/* native_error */
#define ERROR_FIRST_ARG_TYPE_ERR                                                \
    NATIVE_FN_ERR(error, INVALID_FIRST_ARG_TYPE(string))
/* ------------- */

/* native_strlen */
#define STRLEN_FIRST_ARG_TYPE_ERR                                               \
    NATIVE_FN_ERR(strlen, INVALID_FIRST_ARG_TYPE(string))
/* ------------- */

/* native_strlen */
#define STRPAT_FIRST_ARG_TYPE_ERR                                               \
    NATIVE_FN_ERR(strpat, INVALID_FIRST_ARG_TYPE(string))
#define STRPAT_SECOND_ARG_TYPE_ERR                                              \
    NATIVE_FN_ERR(strpat, INVALID_SECOND_ARG_TYPE(string))
/* ------------- */

/* native_strsub */
#define STRSUB_FIRST_ARG_TYPE_ERR                                               \
    NATIVE_FN_ERR(strsub, INVALID_FIRST_ARG_TYPE(string))
#define STRSUB_INDICES_TYPE_ERR                                                 \
    NATIVE_FN_ERR(strsub, "indices 'i' and 'j' must be numbers.")
/* ------------- */

/* native_loadscript */
#define LOADSCRIPT_ARG_TYPE_ERR                                                 \
    NATIVE_FN_ERR(loadscript, INVALID_FIRST_ARG_TYPE(string))
#define LOADSCRIPT_RESOLVE_ERR                                                  \
    NATIVE_FN_ERR(loadscript, "Couldn't resolve script.")
#define LOADSCRIPT_RECURSION_ERR                                                \
    NATIVE_FN_ERR(loadscript, "Can't recursively load the script.")
#define LOADSCRIPT_LOAD_ERR                                                     \
    NATIVE_FN_ERR(loadscript, "Couldn't load the script.")
#define LOADSCRIPT_COMPILE_ERR                                                  \
    NATIVE_FN_ERR(loadscript, "Errored while compiling the script.")
#define LOADSCRIPT_RUN_ERR                                                      \
    NATIVE_FN_ERR(loadscript, "Errored while running the script.")
/* ------------- */

/* native_strbyte */
#define STRBYTE_FIRST_ARG_TYPE_ERR                                              \
    NATIVE_FN_ERR(strbyte, INVALID_FIRST_ARG_TYPE(string))
#define STRBYTE_SECOND_ARG_TYPE_ERR                                             \
    NATIVE_FN_ERR(strbyte, INVALID_SECOND_ARG_TYPE(integer number))
/* ------------- */

#define INVALID_ARG(expected)                                                   \
    "Invalid argument provided, argument must be a #expected."

/* native_strbyte */
#define BYTE_ARG_ERR NATIVE_FN_ERR(byte, INVALID_ARG(string))
/* ------------- */

/* native_strlower */
#define STRLOWER_ARG_ERR NATIVE_FN_ERR(strlower, INVALID_ARG(string))
/* ------------- */

/* native_strupper */
#define STRUPPER_ARG_ERR NATIVE_FN_ERR(strupper, INVALID_ARG(string))
/* ------------- */

/* native_strrev */
#define STRREV_ARG_ERR NATIVE_FN_ERR(strrev, INVALID_ARG(string))
/* ------------- */

/* native_strrev */
#define STRCONCAT_FIRST_ARG_TYPE_ERR                                            \
    NATIVE_FN_ERR(strconcat, INVALID_FIRST_ARG_TYPE(string))
#define STRCONCAT_SECOND_ARG_TYPE_ERR                                           \
    NATIVE_FN_ERR(strconcat, INVALID_SECOND_ARG_TYPE(string))
/* ------------- */








/**
 * VM RUNTIME ERRORS
 **/
#define RUNTIME_ERR(vm, fmt, ...) runerror(vm, fmt __VA_OPT__(, ) __VA_ARGS__)

/* Operators */
#define VM_UNARY_NEGATION_ERR(vm, valstr)                                       \
    RUNTIME_ERR(                                                                \
        vm,                                                                     \
        "%s is not a number, operand must be a number for unary negation "      \
        "'-'.",                                                                 \
        valstr)
/*---------------------------------------------*/

/* Class errors */
#define VM_INSTANCE_ERR(vm, valstr)                                             \
    RUNTIME_ERR(                                                                \
        vm,                                                                     \
        "'%s' is not an <instance>, only class instances have properties.",     \
        valstr)
#define VM_INSTANCE_PROPERTY_ERR(vm, property_name, instance_name)              \
    RUNTIME_ERR(                                                                \
        vm,                                                                     \
        "Property '%s' is not defined for <instance '%s'>.",                    \
        property_name,                                                          \
        instance_name)
#define VM_INHERIT_ERR(vm, subclass, superclass)                                \
    RUNTIME_ERR(                                                                \
        vm,                                                                     \
        "Invalid \"class %s impl ...\", tried "                                 \
        "impl from '%s', can only impl classes.\n\n"                            \
        "Example:\n\tclass A {}\n\tclass B impl A {}\n",                        \
        subclass,                                                               \
        superclass);
#define VM_SUPER_CALL_ERR(vm, superclass, method_name)                          \
    RUNTIME_ERR(                                                                \
        vm,                                                                     \
        "Tried calling undefined method <fn '%s'> on superclass '%s'.",         \
        method_name,                                                            \
        superclass)
#define VM_EXPLICIT_INIT_CALL_ERR(vm)                                           \
    RUNTIME_ERR(                                                                \
        vm,                                                                     \
        "Initializer function can't be directly called.\n"                      \
        "It can only be implicitly invoked by calling the class.\n"             \
        "Try this:\n\tclass A {}\n\tvar instance = A();",                       \
        (vm)->statics[SS_INIT]->storage)
/*---------------------------------------------*/

/* Variables */
#define VM_GLOBAL_UNDEFINED_ERR(vm, name)                                       \
    RUNTIME_ERR(vm, FMT_VAR_UNDEFINED_ERR(name))
#define VM_VARIABLE_FIXED_ERR(vm, len, start)                                   \
    RUNTIME_ERR(vm, FMT_VAR_FIXED_ERR(len, start))
/*---------------------------------------------*/

/* Call frame */
#define VM_FRAME_LIMIT_ERR(vm, frames_max)                                      \
    RUNTIME_ERR(                                                                \
        vm,                                                                     \
        "VM call frame stack overflow, limit reached [%u].",                    \
        frames_max)
/*---------------------------------------------*/

/* @TODO: change this, after implementing array objects. */
#define VM_INDEX_RECEIVER_ERR(vm)                                               \
    RUNTIME_ERR(vm, "Reciever of index operator must be a class instance.")
#define VM_INDEX_NIL_ERR(vm)                                                    \
    RUNTIME_ERR(vm, "Indexing with 'nil' value is not allowed.");

/* Function errors */
#define VM_FN_ARGC_ERR(vm, arity, argc)                                         \
    RUNTIME_ERR(vm, "Expected %d argument/s, instead got %d.", arity, argc)
#define VM_FN_VA_ARGC_ERR(vm, arity, argc)                                      \
    RUNTIME_ERR(                                                                \
        vm,                                                                     \
        "Expected at least %d argument/s, instead got %d.",                     \
        arity,                                                                  \
        argc)
#define VM_NATIVE_ARGC_ERR(vm, fn, arity, argc)                                 \
    RUNTIME_ERR(                                                                \
        vm,                                                                     \
        "%s: Expected %d argument/s, instead got %d.",                          \
        fn,                                                                     \
        arity,                                                                  \
        argc)
#define VM_NATIVE_VA_ARGC_ERR(vm, fn, arity, argc)                              \
    RUNTIME_ERR(                                                                \
        vm,                                                                     \
        "%s: Expected at least %d argument/s, instead got %d.",                 \
        fn,                                                                     \
        arity,                                                                  \
        argc)
#define VM_NONCALLABLE_ERR(vm, valstr)                                          \
    RUNTIME_ERR(                                                                \
        vm,                                                                     \
        "Tried calling '%s' which is non-callable value, only functions and "   \
        "classes can be "                                                       \
        "called.",                                                              \
        valstr)
/*---------------------------------------------*/

#endif
