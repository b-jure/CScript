#ifndef __SKOOMA_ERR_H__
#define __SKOOMA_ERR_H__

/**
 * FORMAT STRINGS
 **/
#define FMT_VAR_FIXED_ERR(len, start)                                                    \
    "Can't assign to variable '%.*s' it is declared as 'fixed'.\n\n"                     \
    "Try removing 'fixed' from variable declaration.\n"                                  \
    "Example:\n\tvar a = \"mutable\"; // mutable variable\n\tfixed var b = "             \
    "\"immutable\"; // immutable variable\n\tvar fixed c = \"also immutable\"; // "      \
    "immutable variable",                                                                \
        len, start

#define FMT_VAR_UNDEFINED_ERR(name) "Undefined variable '%s'.", name




/**
 * COMPILER ERRORS
 **/
#define COMPILER_CONSTANT_LIMIT_ERR(c, fnstr, limit)                                     \
    C_error(                                                                             \
        c,                                                                               \
        "<fn %s>: Too many constants defined in a single chunk (limit %u).",             \
        fnstr,                                                                           \
        limit)

#define COMPILER_GLOBALS_LIMIT_ERR(c, limit)                                             \
    C_error(c, "Too many global values defined in script (limit %u).", limit)

#define COMPILER_GLOBAL_REDEFINITION_ERR(c, len, start)                                  \
    C_error(c, "Variable redefinition '%.*s'.", len, start)

#define COMPILER_VAR_UNDEFINED_ERR(c, varname) C_error(c, FMT_VAR_UNDEFINED_ERR(varname))
#define COMPILER_VAR_FIXED_ERR(c, len, start)  C_error(c, FMT_VAR_FIXED_ERR(len, start))

#define COMPILER_JUMP_LIMIT_ERR(c, limit)                                                \
    C_error(c, "Too much code to jump over. Bytecode indexing limit reached [%u].", limit)

#define COMPILER_SWITCH_DEFAULT_ERR(c)                                                   \
    C_error(c, "Multiple 'default' labels in a single 'switch'.")
#define COMPILER_SWITCH_NOCASE_ERR(c)                                                    \
    C_error(c, "Can't have statements before first case.")
#define COMPILER_SWITCH_RBRACE_ERR(c) C_error(c, "Expect '}' at the end of 'switch'.")

#define COMPILER_CONTINUE_ERR(c) C_error(c, "'continue' statement not in loop statement.")

#define COMPILER_BREAK_ERR(c)                                                            \
    C_error(c, "'break' statement not in loop or switch statement.");

#define COMPILER_RETURN_SCRIPT_ERR(c) C_error(c, "Can't 'return' from top-level code.")
#define COMPILER_RETURN_INIT_ERR(c, initstr)                                             \
    C_error(c, "Can't return a value from '%s' method.", initstr)

#define COMPILER_SELF_ERR(c) C_error(c, "Can't use 'self' outside of a class.");

#define COMPILER_LOCAL_LIMIT_ERR(c, limit)                                               \
    C_error(c, "Too many local values defined in script (limit %u).", limit)
#define COMPILER_LOCAL_DEFINITION_ERR(c, len, start)                                     \
    C_error(c, "Can't read local variable %.*s in its own initializer.", len, start)
#define COMPILER_LOCAL_REDEFINITION_ERR(c, len, start)                                   \
    C_error(c, "Redefinition of local variable '%.*s'.", len, start)

#define COMPILER_ARGC_LIMIT_ERR(c, limit)                                                \
    C_error(c, "Can't have more than %u arguments.", limit)

#define COMPILER_EXPECT_EXPRESSION_ERR(c) C_error(c, "Expect expression.")

#define COMPILER_INVALID_ASSIGN_ERR(c) C_error(c, "Invalid assignment target.")

#define COMPILER_SCOPE_LIMIT_ERR(c, limit)                                               \
    C_error(c, "Scope nesting limit reached (limit %u).", limit)

#define COMPILER_UPVALUE_LIMIT_ERR(c, fnname, limit)                                     \
    C_error(                                                                             \
        c,                                                                               \
        "<fn %s>: closure variables (upvalues) limit reached (limit %u).",               \
        fnname,                                                                          \
        limit)

#define COMPILER_CLASS_INHERIT_ERR(c, cclass)                                            \
    C_error(C(), "class '%s' can't impl itself.", cclass);




/**
 * NATIVE FUNCTIONS ERRORS
 **/
#define NATIVE_FN_ERR(fn, err) "<native-fn " fn ">: " err
#define ERR_NEW(vm, err)       ObjString_from(vm, NULL, err, sizeof(err) - 1)

#define CLOCK_ERR                                                                        \
    NATIVE_FN_ERR(                                                                       \
        "clock",                                                                         \
        "Processor time is not available or its value cannot be represented.")

#define INSTANCE_ERR(fn) NATIVE_FN_ERR(fn, "First parameter is not an instance.")
#define FIELD_ERR(fn)    NATIVE_FN_ERR(fn, "Second parameter is not a valid field name.")

#define ISFIELD_INSTANCE_ERR INSTANCE_ERR("isfield")
#define ISFIELD_FIELD_ERR    FIELD_ERR("isfield")

#define DELFIELD_INSTANCE_ERR INSTANCE_ERR("delfield")
#define DELFIELD_FIELD_ERR    FIELD_ERR("delfield")

#define SETFIELD_INSTANCE_ERR INSTANCE_ERR("setfield")
#define SETFIELD_FIELD_ERR    FIELD_ERR("setfield")




/**
 * VM RUNTIME ERRORS
 **/
#define RUNTIME_ERR(vm, fmt, ...) VM_error(vm, fmt __VA_OPT__(, ) __VA_ARGS__)

#define RUNTIME_UNARY_NEGATION_ERR(vm, valstr)                                           \
    VM_error(                                                                            \
        vm,                                                                              \
        "%s is not a number, operand must be a number for unary negation '-'.",          \
        valstr)

#define RUNTIME_NONCALLABLE_ERR(vm, valstr)                                              \
    VM_error(                                                                            \
        vm,                                                                              \
        "%s is non-callable value, only functions and classes can be called.",           \
        valstr)

#define RUNTIME_ARGC_ERR(vm, fnstr, arity, argc)                                         \
    VM_error(                                                                            \
        vm,                                                                              \
        "<fn %s>: expected %u arguments, but got %d instead.",                           \
        fnstr,                                                                           \
        arity,                                                                           \
        argc)

#define RUNTIME_INSTANCE_ARGC_ERR(vm, classstr, methodstr, argc)                         \
    VM_error(                                                                            \
        vm,                                                                              \
        "Method <fn %s> defined in class '%s' expects 0 arguments but got %d.",          \
        methodstr,                                                                       \
        classstr,                                                                        \
        argc)
#define RUNTIME_INSTANCE_INIT_ARGC_ERR(vm, classstr, argc)                               \
    VM_error(                                                                            \
        vm,                                                                              \
        "Invoked initializer for class '%s' with %u argument/s but expected 0.",         \
        classstr,                                                                        \
        argc)
#define RUNTIME_INSTANCE_ERR(vm, valstr)                                                 \
    RUNTIME_ERR(                                                                         \
        vm,                                                                              \
        "%s is not an instance, only class instances have properties.",                  \
        valstr)
#define RUNTIME_INSTANCE_PROPERTY_ERR(vm, instance_name, property_name)                  \
    RUNTIME_ERR(                                                                         \
        vm,                                                                              \
        "Property '%s' is not defined for '%s'.",                                        \
        property_name,                                                                   \
        instance_name)

#define RUNTIME_INHERIT_ERR(vm, subclass, superclass)                                    \
    VM_error(                                                                            \
        vm,                                                                              \
        "Invalid \"class %s impl ...\", tried "                                          \
        "impl from '%s', can only impl classes.\n\n"                                       \
        "Example:\n\tclass A {}\n\tclass B impl A {}\n",                                 \
        subclass,                                                                        \
        superclass);

#define RUNTIME_GLOBAL_UNDEFINED_ERR(vm, name)                                           \
    RUNTIME_ERR(vm, FMT_VAR_UNDEFINED_ERR(name))
#define RUNTIME_GLOBAL_FIXED_ERR(vm, len, start)                                         \
    RUNTIME_ERR(vm, FMT_VAR_FIXED_ERR(len, start))

#define RUNTIME_INTERNAL_FRAME_LIMIT_ERR(vm, frames_max)                                 \
    VM_error(                                                                            \
        vm,                                                                              \
        "Internal error: VM stack overflow, call frame limit reached [%u].",             \
        frames_max)

#define RUNTIME_INTERNAL_STACK_OVERFLOW_ERR(vm, stackmax)                                \
    VM_error(                                                                            \
        vm,                                                                              \
        "Internal error: VM stack overflow, stack size limit reached [%u].",             \
        stackmax)

#endif
