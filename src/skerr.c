#include "common.h"
#include "debug.h"
#include "err.h"
#include "object.h"
#include "vmachine.h"

/* ================= runtime error format strings ================= */

/* Create formatted object string */
#define OSTRINGF(vm, fmt, ...) OString_fmt(vm, fmt __VA_OPT__(, ) __VA_ARGS__)


/* otryop() */
#define UNOP_ERR(vm, op, operand)                                                        \
    OSTRINGF(vm, "Attempt to perform unary %s on %s.", op, operand)
#define BINOP_ERR(vm, op, left, right)                                                   \
    OSTRINGF(                                                                            \
        vm,                                                                              \
        "Attempt to perform binary %s on %s (left) and %s (right).",                     \
        op,                                                                              \
        left,                                                                            \
        right)


/* OString_fmt_from() */
#define OSTRINGF_ERR(vm, c, fn)                                                          \
    OSTRINGF(vm, "invalid format specifier '%%%c' for '%s'", c, fn)


/* OP_SUB | OP_MUL | OP_MOD | OP_POW | OP_DIV | OP_GREATER | OP_GREATER_EQUAL |
 * OP_LESS | OP_LESS_EQUAL */
#define BINARYOP_ERR(vm, op)                                                             \
    OSTRINGF(vm, "Operands must be numbers or overload the operator '#op'.")


/* OP_NEG */
#define UNARYNEG_ERR(vm, valstr)                                                         \
    OSTRINGF(                                                                            \
        vm,                                                                              \
        "'%s' is not a number, operand must be a number or it must overload unary "      \
        "operator '-'.",                                                                 \
        valstr)


/* invokeindex() | invoke() | OP_SET_PROPERTY | OP_GET_PROPERTY */
#define NOT_INSTANCE_ERR(vm, valstr)                                                     \
    OSTRINGF(                                                                            \
        vm,                                                                              \
        "'%s' is not an <instance>, only class instances have properties.",              \
        valstr)


/* bindmethod() | invokefrom() | invokeindex() */
#define UNDEFINED_PROPERTY_ERR(vm, property, class)                                      \
    OSTRINGF(vm, "Property '%s' is not defined for <class '%s'>.", property, class)


/* bindomethod() */
#define NO_OVERLOAD_ERR(vm, olname, class)                                               \
    OSTRINGF(vm, "Method '%s' is not overloaded for <class '%s'>.", olname, class)


/* OP_INHERIT */
#define INHERIT_ERR(vm, subclass, superclass)                                            \
    OSTRINGF(                                                                            \
        vm,                                                                              \
        "Invalid impl: \"class %s impl ...\", tried "                                    \
        "inheriting from '%s', classes can only inherit from other classes.\n"           \
        "\tExample:\n\t\tclass A {}\n\t\tclass B impl A {}\n",                           \
        subclass,                                                                        \
        superclass)


/* OP_GET_GLOBAL | OP_GET_GLOBALL | OP_SET_GLOBAL | OP_SET_GLOBALL */
#define UNDEFINED_GLOBAL_ERR(vm, name)                                                   \
    OSTRINGF(vm, "Undefined global variable '%s'.", name)


/* OP_DEFINE_GLOBAL | OP_DEFINE_GLOBALL */
#define GLOBALVAR_REDEFINITION_ERR(vm, name)                                             \
    OSTRINGF(vm, "Redefinition of global variable '%s'.", name)


/* OP_SET_UPVALUE */
#define VARIABLE_FIXED_ERR(vm, len, start) OSTRINGF(vm, FMT_VAR_FIXED_ERR(len, start))


/* OP_INDEX | OP_SET_INDEX */
#define INDEX_RECEIVER_ERR(vm, receiver)                                                 \
    OSTRINGF(vm, "Can't index '%s', indexing can be used on instances.")
#define NIL_INDEX_ERR(vm) OSTRINGF(vm, "Instances can't be indexed with 'nil'.")


/* callv() | callnative() | callfn() */
#define FN_ARGC_ERR(vm, arity, argc)                                                     \
    OSTRINGF(vm, "Expected %d argument/s, instead got %d.", arity, argc)
#define FN_VA_ARGC_ERR(vm, arity, argc)                                                  \
    OSTRINGF(vm, "Expected at least %d argument/s, instead got %d.", arity, argc)
#define FRAME_LIMIT_ERR(vm, frames_max)                                                  \
    OSTRINGF(vm, "Call-frame stack overflow, limit reached [%u].", frames_max)
#define RETCNT_STACK_OVERFLOW(vm, native)                                                \
    OSTRINGF(vm, "Called function '%s' return count would overflow the stack.", native)


/* push() */
#define VM_STACK_OVERFLOW(vm, limit)                                                     \
    OSTRINGF(vm, "VM stack overflow, limit %ul.", cast(unsigned long, limit))


/* callv() { OP_CALL } */
#define NONCALLABLE_ERR(vm, valstr)                                                      \
    OSTRINGF(                                                                            \
        vm,                                                                              \
        "Tried calling '%s' which is non-callable value, only functions "                \
        "and classes can be called.",                                                    \
        valstr)


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


/* oprint() */
#define DISPLAY_INVALID_TYPE(vm, typename)                                               \
    OSTRINGF(vm, "Display method must return a string, instead got %s.", typename)




/* ==================== runtime errors ====================== */

sk_noret binoperr(VM* vm, Value a, Value b, OMTag op)
{
    push(vm, OBJ_VAL(vtostr(vm, a)));
    push(vm, OBJ_VAL(vtostr(vm, b)));
    const char* operation = vm->statics[op]->storage;
    const char* left = AS_CSTRING(*stackpeek(1));
    const char* right = AS_CSTRING(*stackpeek(0));
    push(vm, OBJ_VAL(BINOP_ERR(vm, operation, left, right)));
    runerror(vm, S_EARBIN);
}


sk_noret unoperr(VM* vm, Value a, OMTag op)
{
    push(vm, OBJ_VAL(vtostr(vm, a)));
    const char* operation = vm->statics[op]->storage;
    const char* operand = AS_CSTRING(*stackpeek(0));
    push(vm, OBJ_VAL(UNOP_ERR(vm, operation, operand)));
    runerror(vm, S_EARUN);
}


