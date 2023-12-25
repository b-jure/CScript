#ifndef SKOOMA_ERR_H
#define SKOOMA_ERR_H


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








/* NATIVE FUNCTIONS ERRORS */


#define NATIVE_FN_ERR(fn, err, ...) ("<native-fn " #fn ">: " err)
#define ERR_NEW(vm, err)            OString_from(vm, err, sizeof(err) - 1)

/* native_clock() */
#define CLOCK_ERR                                                                        \
    NATIVE_FN_ERR(                                                                       \
        clock,                                                                           \
        "Processor time is not available or its value cannot be "                        \
        "represented.")
/* ------------- */

/* native_isfield() */
#define ISFIELD_INSTANCE_ERR   NATIVE_FN_ERR(isfield, "Receiver is not a class instance.")
#define ISFIELD_FIELD_TYPE_ERR NATIVE_FN_ERR(isfield, "'field' is not a string.")
/* ------------- */

/* native_gcfactor */
#define GC_FACTOR_ARG_ERR                                                                \
    NATIVE_FN_ERR(                                                                       \
        gcfactor,                                                                        \
        "invalid argument, grow factor must be a number, either '0' or "                 \
        "'>1'.")
/* ------------- */

/* Format strings */
#define INVALID_FIRST_ARG_TYPE(type)                                                     \
    "invalid first argument type, argument must be #type."
#define INVALID_SECOND_ARG_TYPE(type)                                                    \
    "invalid second argument type, argument must be #type."
/* ------------- */

/* native_gcmode() */
#define GC_MODE_ARG_ERR NATIVE_FN_ERR(gcmode, INVALID_FIRST_ARG_TYPE(string))
#define GC_MODE_INVALID_MODE_ERR                                                         \
    NATIVE_FN_ERR(gcmode, "invalid mode string, mode can be either 'auto' or 'manual'.")
/* ------------- */

/* native_gcset() */
#define GC_SET_ARG_ERR NATIVE_FN_ERR(gcset, INVALID_FIRST_ARG_TYPE(number))

#define GC_SET_NEGATIVE_LIMIT_ERR                                                        \
    NATIVE_FN_ERR(gcset, "limit can't be negative, it must be positive or 0.")
/* ------------- */

/* native_assertf() */
#define ASSERTF_SECOND_ARG_TYPE_ERR                                                      \
    NATIVE_FN_ERR(assertf, INVALID_SECOND_ARG_TYPE(string))
/* ------------- */

/* native_error() */
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
#define STRSUB_INDICES_TYPE_ERR                                                          \
    NATIVE_FN_ERR(strsub, "indices 'i' and 'j' must be numbers.")
/* ------------- */

/* native_loadscript() */
#define LOADSCRIPT_ARG_TYPE_ERR NATIVE_FN_ERR(loadscript, INVALID_FIRST_ARG_TYPE(string))
#define LOADSCRIPT_RESOLVE_ERR  NATIVE_FN_ERR(loadscript, "Couldn't resolve script.")
#define LOADSCRIPT_RECURSION_ERR                                                         \
    NATIVE_FN_ERR(loadscript, "Can't recursively load the script.")
#define LOADSCRIPT_LOAD_ERR NATIVE_FN_ERR(loadscript, "Couldn't load the script.")
#define LOADSCRIPT_COMPILE_ERR                                                           \
    NATIVE_FN_ERR(loadscript, "Errored while compiling the script.")
#define LOADSCRIPT_RUN_ERR NATIVE_FN_ERR(loadscript, "Errored while running the script.")
/* ------------- */

/* native_strbyte */
#define STRBYTE_FIRST_ARG_TYPE_ERR NATIVE_FN_ERR(strbyte, INVALID_FIRST_ARG_TYPE(string))
#define STRBYTE_SECOND_ARG_TYPE_ERR                                                      \
    NATIVE_FN_ERR(strbyte, INVALID_SECOND_ARG_TYPE(integer number))
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
#define STRCONCAT_FIRST_ARG_TYPE_ERR                                                     \
    NATIVE_FN_ERR(strconcat, INVALID_FIRST_ARG_TYPE(string))
#define STRCONCAT_SECOND_ARG_TYPE_ERR                                                    \
    NATIVE_FN_ERR(strconcat, INVALID_SECOND_ARG_TYPE(string))
/* ------------- */








/* VM RUNTIME ERRORS */


/* Create formatted object string */
#define OSTRINGF(vm, fmt, ...) OString_fmt(vm, fmt __VA_OPT__(, ) __VA_ARGS__)

/* OString_fmt_from() */
#define OSTRINGF_ERR(vm, c, fn)                                                          \
    OSTRINGF(vm, "invalid format specifier '%%%c' for '%s'", c, fn)
/*-----------------*/

/* OP_SUB | OP_MUL | OP_MOD | OP_POW | OP_DIV | OP_GREATER | OP_GREATER_EQUAL |
 * OP_LESS | OP_LESS_EQUAL */
#define BINARYOP_ERR(vm, op)                                                             \
    OSTRINGF(vm, "Operands must be numbers or overload the operator '#op'.")             \
/*-----------------*/

/* OP_NEG */
#define UNARYNEG_ERR(vm, valstr)                                                         \
    OSTRINGF(                                                                            \
        vm,                                                                              \
        "'%s' is not a number, operand must be a number or it must overload unary "      \
        "operator '-'.",                                                                 \
        valstr)
/*---------------------------------------------*/

/* invokeindex() | invoke() | OP_SET_PROPERTY | OP_GET_PROPERTY */
#define NOT_INSTANCE_ERR(vm, valstr)                                                     \
    OSTRINGF(                                                                            \
        vm,                                                                              \
        "'%s' is not an <instance>, only class instances have properties.",              \
        valstr)
/*-----------------*/

/* bindmethod() | invokefrom() | invokeindex() */
#define UNDEFINED_PROPERTY_ERR(vm, property, class)                                      \
    OSTRINGF(vm, "Property '%s' is not defined for <class '%s'>.", property, class)
/*-----------------*/

/* OP_INHERIT */
#define INHERIT_ERR(vm, subclass, superclass)                                            \
    OSTRINGF(                                                                            \
        vm,                                                                              \
        "Invalid impl: \"class %s impl ...\", tried "                                    \
        "inheriting from '%s', classes can only inherit from other classes.\n"           \
        "\tExample:\n\t\tclass A {}\n\t\tclass B impl A {}\n",                           \
        subclass,                                                                        \
        superclass)
/*-----------------*/

/* OP_GET_GLOBAL | OP_GET_GLOBALL | OP_SET_GLOBAL | OP_SET_GLOBALL */
#define UNDEFINED_GLOBAL_ERR(vm, name)                                                   \
    OSTRINGF(vm, "Undefined global variable '%s'.", name)
/*-----------------*/

/* OP_DEFINE_GLOBAL | OP_DEFINE_GLOBALL */
#define GLOBALVAR_REDEFINITION_ERR(vm, name)                                             \
    OSTRINGF(vm, "Redefinition of global variable '%s'.", name)
/*-----------------*/

/* OP_SET_UPVALUE */
#define VARIABLE_FIXED_ERR(vm, len, start) OSTRINGF(vm, FMT_VAR_FIXED_ERR(len, start))
/*-----------------*/

/* OP_INDEX | OP_SET_INDEX */
#define INDEX_RECEIVER_ERR(vm, receiver)                                                 \
    OSTRINGF(vm, "Can't index '%s', indexing can be used on instances.")
#define INVALID_INDEX_ERR(vm)                                                            \
    OSTRINGF(vm, "Instances can only be indexed with strings (literal or variable).");
/*-----------------*/

/* callv() | callnative() | callfn() */
#define FN_ARGC_ERR(vm, arity, argc)                                                     \
    OSTRINGF(vm, "Expected %d argument/s, instead got %d.", arity, argc)
#define FN_VA_ARGC_ERR(vm, arity, argc)                                                  \
    OSTRINGF(vm, "Expected at least %d argument/s, instead got %d.", arity, argc)
#define FRAME_LIMIT_ERR(vm, frames_max)                                                  \
    OSTRINGF(vm, "Call-frame stack overflow, limit reached [%u].", frames_max)
#define RETCNT_STACK_OVERFLOW(vm, native)                                                \
    OSTRINGF(                                                                            \
        vm,                                                                              \
        "Called function <native %s> return count would overflow the stack.",            \
        native)
/* -------------- */

/* callv() { OP_CALL } */
#define NONCALLABLE_ERR(vm, valstr)                                                      \
    OSTRINGF(                                                                            \
        vm,                                                                              \
        "Tried calling '%s' which is non-callable value, only functions "                \
        "and classes can be called.",                                                    \
        valstr)
/* -------------- */

/* OP_ADD */
#define ADD_OPERATOR_ERR(vm, a, b)                                                       \
    ({                                                                                   \
        OString* str1 = vtostr(vm, a);                                                   \
        push(vm, OBJ_VAL(str1));                                                         \
        OString* str2 = vtostr(vm, b);                                                   \
        push(vm, OBJ_VAL(str2));                                                         \
        OString* unesc2 = unescape(vm, str2);                                            \
        push(vm, OBJ_VAL(unesc2));                                                       \
        OString* unesc1 = unescape(vm, str1);                                            \
        push(vm, OBJ_VAL(unesc1));                                                       \
        OString* s = OSTRINGF(                                                           \
            vm,                                                                          \
            "Only two numbers can be added together or two strings "                     \
            "concatenated.\n"                                                            \
            "\tThis is invalid: \"%s\" + \"%s\"\n"                                       \
            "\tTry instead: \"%s%s%s\" + \"%s%s%s\"",                                    \
            unesc1->storage,                                                             \
            unesc2->storage,                                                             \
            IS_STRING(a) ? "" : "tostr(",                                                \
            unesc1->storage,                                                             \
            IS_STRING(a) ? "" : ")",                                                     \
            IS_STRING(b) ? "" : "tostr(",                                                \
            unesc2->storage,                                                             \
            IS_STRING(b) ? "" : ")");                                                    \
        popn(vm, 4);                                                                     \
        s;                                                                               \
    })

#endif
