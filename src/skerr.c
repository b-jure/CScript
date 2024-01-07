#include "common.h"
#include "debug.h"
#include "err.h"
#include "object.h"
#include "vmachine.h"

/* ================= runtime error format strings ================= */

/* Create formatted object string */
#define OSTRINGF(vm, fmt, ...) OString_fmt(vm, fmt __VA_OPT__(, ) __VA_ARGS__)



/* OString_fmt_from() */
#define OSTRINGF_ERR(vm, c, fn)                                                          \
    OSTRINGF(vm, "invalid format specifier '%%%c' for '%s'", c, fn)


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
#define FRAME_LIMIT_ERR(vm, frlimit)                                                     \
    OSTRINGF(vm, "Callstack overflow, limit overflown -> %u.", frlimit)
#define RETCNT_STACK_OVERFLOW(vm, fname)                                                 \
    OSTRINGF(vm, "Called function '%s' return count overflows the stack.", fname)


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





/* ==================== runtime errors ====================== */

/* Performs long jump if there is one otherwise prints
 * the runtime error and invokes either a panic handler or aborts.
 * Error message is on top of the stack, or whatever value
 * was passed to sk_error. */
sk_noret runerror(VM* vm, Int status)
{
    last_frame(vm).status = status;
    struct sk_longjmp* errjmp = vm->errjmp;
    if(errjmp) { // protected call?
        errjmp->status = status; // error status
        // sk_unlock is somewhere after the 'longjmp'
        longjmp(errjmp->buf, 1);
        unreachable;
    } // Otherwise print the error
    fputs("\nSkooma: [runtime error]\nSkooma: ", stderr);
    fprintf(stderr, "%s\n", vtostr(vm, *stackpeek(0))->storage);
    for(Int i = vm->fc - 1; i >= 0; i--) {
        CallFrame* frame = &vm->frames[i];
        if(frame->closure != NULL) { // Skooma function ?
            Chunk* chunk = &frame->closure->fn->chunk;
            UInt line = Chunk_getline(chunk, frame->ip - chunk->code.data - 1);
            Value _; // dummy
            bool loaded = false;
            if(HashTable_get(&vm->loaded, OBJ_VAL(FFN(frame)->name), &_)) {
                vm->script = OBJ_VAL(FFN(frame)->name);
                loaded = true;
            }
            fprintf(
                stderr,
                "        ['%s' on line %u] in ",
                AS_CSTRING(vm->script),
                line);
            if(loaded) fprintf(stderr, "script\n");
            else fprintf(stderr, "%s()\n", FFN(frame)->name->storage);
        } else { // this is a C function
            fprintf(
                stderr,
                "        in %s()\n",
                AS_NATIVE(*frame->callee)->name->storage);
        }
    }
    fflush(stderr);
    if(vm->config.panic) {
        sk_unlock(vm);
        vm->config.panic(vm);
        unreachable;
    } else {
        _cleanupvm(&vm);
        abort();
    }
}


sk_noret ordererror(VM* vm, Value a, Value b)
{
    const char* t1 = vm->statics[val2type(a)]->storage;
    const char* t2 = vm->statics[val2type(a)]->storage;
    if(strcmp(t1, t2) == 0) sk_pushfstring(vm, "Attempt to compare two %s values.", t1);
    else sk_pushfstring(vm, "Attempt to compare %s and %s.", t1, t2);
    runerror(vm, S_ECMP);
}


sk_noret binoperr(VM* vm, Value a, Value b, OMTag op)
{
    const char* fmt = "Attempt to perform binary %s on %s (left) and %s (right).";
    push(vm, OBJ_VAL(vtostr(vm, a)));
    push(vm, OBJ_VAL(vtostr(vm, b)));
    const char* operation = vm->statics[op + SS_OPADD]->storage;
    const char* left = AS_CSTRING(*stackpeek(1));
    const char* right = AS_CSTRING(*stackpeek(0));
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, operation, left, right)));
    runerror(vm, S_EARBIN);
}


sk_noret unoperr(VM* vm, Value a, OMTag op)
{
    const char* fmt = "Attempt to perform unary '%s' on %s.";
    push(vm, OBJ_VAL(vtostr(vm, a)));
    const char* operation = vm->statics[op + SS_OPADD]->storage;
    const char* operand = AS_CSTRING(*stackpeek(0));
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, operation, operand)));
    runerror(vm, S_EARUN);
}


sk_noret omdisplayerr(VM* vm, Value result)
{
    const char* fmt = "Display method must return a string, instead got %s.";
    push(vm, OBJ_VAL(vtostr(vm, result)));
    const char* resultstring = AS_CSTRING(*stackpeek(0));
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, resultstring)));
    runerror(vm, S_EDISPLAY);
}


sk_noret ostringfmterr(VM* vm, int c, Value callee)
{
    const char* fmt = "Invalid format specifier '%%%c' for '%s'";
    push(vm, OBJ_VAL(vtostr(vm, callee)));
    const char* fn = AS_CSTRING(*stackpeek(0));
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, c, fn)));
    runerror(vm, S_ESTRFMT);
}


sk_noret sovferror(VM* vm)
{
    const char* fmt = "Stack overflow, limit overflown -> %d.";
    vm->sp--; // make some space
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, VM_STACK_LIMIT)));
    runerror(vm, S_ESOVERFLOW);
}


sk_noret udproperror(VM* vm, Value property, OClass* oclass)
{
    const char* fmt = "Property '%s' is not defined for <class '%s'>.";
    const char* pname = AS_CSTRING(property);
    const char* classname = oclass->name->storage;
    push(vm, OBJ_VAL(OString_fmt(vm, pname, classname)));
    runerror(vm, S_EUDPROPERTY);
}


sk_noret retovferror(VM* vm, const char* fn)
{
    const char* fmt = "Called function '%s' return count overflows the stack.";
    if(vm->sp - vm->stack >= VM_STACK_LIMIT) vm->sp--;
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, fn)));
    runerror(vm, S_ESOVERFLOW);
}


sk_noret arityerror(VM* vm, int expected, int got)
{
    const char* fmt = "Expected %d arguments instead got %d.";
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, expected, got)));
    runerror(vm, S_EARITY);
}


sk_noret fcovferror(VM* vm)
{
    const char* fmt = "Callstack overflow, limit overflown -> %lu.";
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, VM_CALLSTACK_LIMIT)));
    runerror(vm, S_EFOVERFLOW);
}


sk_noret callerror(VM* vm, Value callee)
{
    const char* fmt = "Tried calling non-callable value %s.";
    push(vm, OBJ_VAL(vtostr(vm, callee)));
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, AS_CSTRING(*stackpeek(0)))));
    runerror(vm, S_ECALL);
}

/* --------------------------------------------------------- */ // runtime errors
