#ifndef __SKOOMA_ERR_H__
#define __SKOOMA_ERR_H__

/**
 * FORMAT STRINGS
 **/
#define FMT_VAR_FIXED_ERR(len, start)                                                              \
    "Can't assign to variable '%.*s' it is declared as 'fixed'.\n\n"                               \
    "Try removing 'fixed' from variable declaration.\n"                                            \
    "Example:\n\tvar a = \"mutable\"; // mutable variable\n\tfixed var b = "                       \
    "\"immutable\"; // immutable variable\n\tvar fixed c = \"also immutable\"; // "                \
    "immutable variable",                                                                          \
        len, start

#define FMT_VAR_UNDEFINED_ERR(name) "Undefined variable '%s'.", name




/**
 * COMPILER ERRORS
 **/

/* Compile constant */
#define COMPILER_CONSTANT_LIMIT_ERR(c, fnstr, limit)                                               \
    C_error(c, "<fn %s>: Too many constants defined in a single chunk (limit %u).", fnstr, limit)
/* ------------- */

/* Compile variable */
#define COMPILER_VAR_UNDEFINED_ERR(c, varname) C_error(c, FMT_VAR_UNDEFINED_ERR(varname))
#define COMPILER_VAR_FIXED_ERR(c, len, start)  C_error(c, FMT_VAR_FIXED_ERR(len, start))
#define COMPILER_GLOBALS_LIMIT_ERR(c, limit)                                                       \
    C_error(c, "Too many global values defined in script (limit %u).", limit)
#define COMPILER_GLOBAL_REDEFINITION_ERR(c, len, start)                                            \
    C_error(c, "Variable redefinition '%.*s'.", len, start)
#define COMPILER_LOCAL_LIMIT_ERR(c, limit)                                                         \
    C_error(c, "Too many local values defined in script (limit %u).", limit)
#define COMPILER_LOCAL_DEFINITION_ERR(c, len, start)                                               \
    C_error(c, "Can't read local variable %.*s in its own initializer.", len, start)
#define COMPILER_LOCAL_REDEFINITION_ERR(c, len, start)                                             \
    C_error(c, "Redefinition of local variable '%.*s'.", len, start)
/* ------------- */

/* Compile code jump */
#define COMPILER_JUMP_LIMIT_ERR(c, limit)                                                          \
    C_error(c, "Too much code to jump over. Bytecode indexing limit reached [%u].", limit)
/* ------------- */

/* Compile 'switch' statement */
#define COMPILER_SWITCH_DEFAULT_ERR(c) C_error(c, "Multiple 'default' labels in a single 'switch'.")
#define COMPILER_SWITCH_NOCASE_ERR(c)  C_error(c, "Can't have statements before first case.")
#define COMPILER_SWITCH_RBRACE_ERR(c)  C_error(c, "Expect '}' at the end of 'switch'.")
/* ------------- */

/* Compile 'continue' */
#define COMPILER_CONTINUE_ERR(c) C_error(c, "'continue' statement not in loop statement.")
/* ------------- */

/* Compile 'break' */
#define COMPILER_BREAK_ERR(c) C_error(c, "'break' statement not in loop or switch statement.");
/* ------------- */

/* Compile 'return' */
#define COMPILER_RETURN_SCRIPT_ERR(c) C_error(c, "Can't 'return' from top-level code.")
/* ------------- */

/* Compile arglist */
#define COMPILER_ARGC_LIMIT_ERR(c, limit) C_error(c, "Can't have more than %u arguments.", limit)
/* ------------- */

/* Compile expression */
#define COMPILER_EXPECT_EXPRESSION_ERR(c) C_error(c, "Expect expression.")
#define COMPILER_INVALID_ASSIGN_ERR(c)    C_error(c, "Invalid assignment target.")
/* ------------- */

/* Compile block */
#define COMPILER_SCOPE_LIMIT_ERR(c, limit)                                                         \
    C_error(c, "Scope nesting limit reached (limit %u).", limit)
/* ------------- */

/* Compile closure upvalue */
#define COMPILER_UPVALUE_LIMIT_ERR(c, fnname, limit)                                               \
    C_error(c, "<fn %s>: closure variables (upvalues) limit reached (limit %u).", fnname, limit)
/* ------------- */

/* Compile class */
#define COMPILER_CLASS_INHERIT_ERR(c, cclass) C_error(C(), "class '%s' can't impl itself.", cclass);
#define COMPILER_SUPER_ERR(c)                 C_error(c, "Can't use 'super' outside of a class.");
#define COMPILER_NO_SUPER_ERR(c)              C_error(c, "Can't use 'super', class does not have a superclass.");
#define COMPILER_SELF_ERR(c)                  C_error(c, "Can't use 'self' outside of a class.");
#define COMPILER_RETURN_INIT_ERR(c, initstr)                                                       \
    C_error(c, "Can't return a value from '%s' method.", initstr)
/* ------------- */




/**
 * NATIVE FUNCTIONS ERRORS
 **/

#define NATIVE_FN_ERR(fn, err, ...) ("<native-fn " #fn ">: " err)
#define ERR_NEW(vm, err)            ObjString_from(vm, err, sizeof(err) - 1)

/* native_clock */
#define CLOCK_ERR                                                                                  \
    NATIVE_FN_ERR(clock, "Processor time is not available or its value cannot be represented.")
/* ------------- */

/* native_isfield */
#define ISFIELD_INSTANCE_ERR   NATIVE_FN_ERR(isfield, "Receiver is not a class instance.")
#define ISFIELD_FIELD_TYPE_ERR NATIVE_FN_ERR(isfield, "'field' is not a string.")
/* ------------- */

/* native_gcfactor */
#define GC_FACTOR_ARG_ERR                                                                          \
    NATIVE_FN_ERR(gcfactor, "invalid argument, grow factor must be a number, either '0' or '>1'.")
/* ------------- */

#define INVALID_FIRST_ARG_TYPE(type)  "invalid first argument type, argument must be #type."
#define INVALID_SECOND_ARG_TYPE(type) "invalid second argument type, argument must be #type."

/* native_gcmode */
#define GC_MODE_ARG_ERR NATIVE_FN_ERR(gcmode, INVALID_FIRST_ARG_TYPE(string))
#define GC_MODE_INVALID_MODE_ERR                                                                   \
    NATIVE_FN_ERR(gcmode, "invalid mode string, mode can be either 'auto' or 'manual'.")
/* ------------- */

/* native_gcset */
#define GC_SET_ARG_ERR NATIVE_FN_ERR(gcset, INVALID_FIRST_ARG_TYPE(number))

#define GC_SET_NEGATIVE_LIMIT_ERR                                                                  \
    NATIVE_FN_ERR(gcset, "limit can't be negative, it must be positive or 0.")
/* ------------- */

/* native_assertf */
#define ASSERTF_SECOND_ARG_TYPE_ERR NATIVE_FN_ERR(assertf, INVALID_SECOND_ARG_TYPE(string))
/* ------------- */

/* native_error */
#define ERROR_FIRST_ARG_TYPE_ERR NATIVE_FN_ERR(error, INVALID_FIRST_ARG_TYPE(string))
/* ------------- */

/* native_strlen */
#define STRLEN_FIRST_ARG_TYPE_ERR NATIVE_FN_ERR(strlen, INVALID_FIRST_ARG_TYPE(string))
/* ------------- */

/* native_strlen */
#define STRPAT_FIRST_ARG_TYPE_ERR  NATIVE_FN_ERR(strpat, INVALID_FIRST_ARG_TYPE(string))
#define STRPAT_SECOND_ARG_TYPE_ERR NATIVE_FN_ERR(strpat, INVALID_SECOND_ARG_TYPE(string))
/* ------------- */

/* native_strsub */
#define STRSUB_FIRST_ARG_TYPE_ERR NATIVE_FN_ERR(strsub, INVALID_FIRST_ARG_TYPE(string))
#define STRSUB_INDICES_TYPE_ERR   NATIVE_FN_ERR(strsub, "indices 'i' and 'j' must be numbers.")
/* ------------- */

/* native_loadscript */
#define LOADSCRIPT_ARG_TYPE_ERR  NATIVE_FN_ERR(loadscript, INVALID_FIRST_ARG_TYPE(string))
#define LOADSCRIPT_RESOLVE_ERR   NATIVE_FN_ERR(loadscript, "Couldn't resolve script.")
#define LOADSCRIPT_RECURSION_ERR NATIVE_FN_ERR(loadscript, "Can't recursively load the script.")
#define LOADSCRIPT_LOAD_ERR      NATIVE_FN_ERR(loadscript, "Couldn't load the script.")
#define LOADSCRIPT_COMPILE_ERR   NATIVE_FN_ERR(loadscript, "Errored while compiling the script.")
#define LOADSCRIPT_RUN_ERR       NATIVE_FN_ERR(loadscript, "Errored while running the script.")
/* ------------- */

/* native_strbyte */
#define STRBYTE_FIRST_ARG_TYPE_ERR  NATIVE_FN_ERR(strbyte, INVALID_FIRST_ARG_TYPE(string))
#define STRBYTE_SECOND_ARG_TYPE_ERR NATIVE_FN_ERR(strbyte, INVALID_SECOND_ARG_TYPE(integer number))
/* ------------- */

#define INVALID_ARG(expected) "Invalid argument provided, argument must be a #expected."

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
#define STRCONCAT_FIRST_ARG_TYPE_ERR  NATIVE_FN_ERR(strconcat, INVALID_FIRST_ARG_TYPE(string))
#define STRCONCAT_SECOND_ARG_TYPE_ERR NATIVE_FN_ERR(strconcat, INVALID_SECOND_ARG_TYPE(string))
/* ------------- */




/**
 * VM RUNTIME ERRORS
 **/
#define RUNTIME_ERR(vm, fmt, ...) VM_error(vm, fmt __VA_OPT__(, ) __VA_ARGS__)

#define RUNTIME_UNARY_NEGATION_ERR(vm, valstr)                                                     \
    VM_error(vm, "%s is not a number, operand must be a number for unary negation '-'.", valstr)

#define RUNTIME_NONCALLABLE_ERR(vm, valstr)                                                        \
    VM_error(                                                                                      \
        vm,                                                                                        \
        "Tried calling '%s' which is non-callable value, only functions and classes can be "       \
        "called.",                                                                                 \
        valstr)

#define RUNTIME_ARGC_ERR(vm, fnstr, arity, argc)                                                   \
    VM_error(vm, "<fn %s>: expected %u arguments, but got %d instead.", fnstr, arity, argc)

#define RUNTIME_INSTANCE_ARGC_ERR(vm, classstr, methodstr, expect, argc)                           \
    VM_error(                                                                                      \
        vm,                                                                                        \
        "Method <fn %s> defined in class '%s' expects %d arguments but got %d.",                   \
        methodstr,                                                                                 \
        classstr,                                                                                  \
        expect,                                                                                    \
        argc)
#define RUNTIME_INSTANCE_INIT_ARGC_ERR(vm, classstr, argc)                                         \
    VM_error(                                                                                      \
        vm,                                                                                        \
        "Invoked initializer for class '%s' with %u argument/s but expected 0.",                   \
        classstr,                                                                                  \
        argc)
#define RUNTIME_INSTANCE_ERR(vm, valstr)                                                           \
    RUNTIME_ERR(vm, "'%s' is not an instance, only class instances have properties.", valstr)
#define RUNTIME_INSTANCE_PROPERTY_ERR(vm, property_name, instance_name)                            \
    RUNTIME_ERR(vm, "Property '%s' is not defined for '%s' instance.", property_name, instance_name)

#define RUNTIME_INHERIT_ERR(vm, subclass, superclass)                                              \
    VM_error(                                                                                      \
        vm,                                                                                        \
        "Invalid \"class %s impl ...\", tried "                                                    \
        "impl from '%s', can only impl classes.\n\n"                                               \
        "Example:\n\tclass A {}\n\tclass B impl A {}\n",                                           \
        subclass,                                                                                  \
        superclass);

#define RUNTIME_SUPER_CALL_ERR(vm, superclass, method_name)                                        \
    VM_error(                                                                                      \
        vm,                                                                                        \
        "Tried calling undefined method <fn '%s'> on superclass '%s'.",                            \
        method_name,                                                                               \
        superclass)

#define RUNTIME_EXPLICIT_INIT_CALL(vm)                                                             \
    VM_error(                                                                                      \
        vm,                                                                                        \
        "Initializer function '%s' can't be directly called.\n"                                    \
        "It can only be implicitly invoked by calling the class to instantiate it.\n"              \
        "Proper usage: \n\tclass A {}\n\tvar instance = A();",                                     \
        (vm)->statics[SS_INIT]->storage)

#define RUNTIME_GLOBAL_UNDEFINED_ERR(vm, name) RUNTIME_ERR(vm, FMT_VAR_UNDEFINED_ERR(name))

#define RUNTIME_VARIABLE_FIXED_ERR(vm, len, start) RUNTIME_ERR(vm, FMT_VAR_FIXED_ERR(len, start))

#define RUNTIME_INTERNAL_FRAME_LIMIT_ERR(vm, frames_max)                                           \
    VM_error(                                                                                      \
        vm,                                                                                        \
        "Internal error: VM call stack overflow, call frame limit reached [%u].",                  \
        frames_max)

#define RUNTIME_INTERNAL_SCRIPT_STACK_OVERFLOW_ERR(vm, tempmax)                                    \
    VM_error(                                                                                      \
        vm,                                                                                        \
        "Internal error: VM script stack overflow, compiling too many scripts, limit [%u].",       \
        tempmax)

#define RUNTIME_INDEX_RECEIVER_ERR(vm)                                                             \
    VM_error(vm, "Reciever of index operator must be a class instance.")

#define RUNTIME_INDEX_NIL_ERR(vm) VM_error(vm, "Indexing with 'nil' value is not allowed.");

#endif
