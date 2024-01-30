#include "err.h"
#include "object.h"
#include "value.h"
#include "vmachine.h"


/* ==================== runtime errors ====================== */


/* Performs long jump if there is one otherwise prints
 * the runtime error and invokes either a panic handler or aborts.
 * Error message is on top of the stack, or whatever value
 * was passed to sk_error. */
sk_noret runerror(VM* vm, int8_t status)
{
    vm->status = status;
    struct sk_longjmp* errjmp = vm->errjmp;
    if(errjmp) { // protected call?
        errjmp->status = status;
        longjmp(errjmp->buf, 1);
    } else if(vm->hooks.panic) { // panic handler ?
        sk_unlock(vm);
        vm->hooks.panic(vm);
    }
    abort(); // gg
}


sk_noret memerror(VM* vm)
{
    push(vm, OBJ_VAL(vm->memerror));
    runerror(vm, S_EMEM);
}


sk_noret ordererror(VM* vm, Value a, Value b)
{
    static const char* fmt1 = "Attempt to compare two %s values.";
    static const char* fmt2 = "Attempt to compare %s and %s.";
    const char* t1 = vm->faststatic[val2type(a)]->storage;
    const char* t2 = vm->faststatic[val2type(a)]->storage;
    if(strcmp(t1, t2) == 0) sk_pushfstring(vm, fmt1, t1);
    else sk_pushfstring(vm, fmt2, t1, t2);
    runerror(vm, S_ECMP);
}


sk_noret binoperror(VM* vm, Value a, Value b, sk_om op)
{
    static const char* fmt = "Attempt to perform binary %s on %s (left) and %s (right).";
    push(vm, OBJ_VAL(vtostr(vm, a)));
    push(vm, OBJ_VAL(vtostr(vm, b)));
    const char* operation = vm->faststatic[op + SS_OPADD]->storage;
    const char* left = AS_CSTRING(*stackpeek(1));
    const char* right = AS_CSTRING(*stackpeek(0));
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, operation, left, right)));
    runerror(vm, S_EARBIN);
}


sk_noret unoperror(VM* vm, Value a, sk_om op)
{
    static const char* fmt = "Attempt to perform unary '%s' on %s.";
    push(vm, OBJ_VAL(vtostr(vm, a)));
    const char* operation = vm->faststatic[op + SS_OPADD]->storage;
    const char* operand = AS_CSTRING(*stackpeek(0));
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, operation, operand)));
    runerror(vm, S_EARUN);
}


sk_noret disperror(VM* vm, Value result)
{
    static const char* fmt = "Display method must return a string, instead got %s.";
    push(vm, OBJ_VAL(vtostr(vm, result)));
    const char* resultstring = AS_CSTRING(*stackpeek(0));
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, resultstring)));
    runerror(vm, S_EDISPLAY);
}


sk_noret ofmterror(VM* vm, int8_t c, Value callee)
{
    static const char* fmt = "Invalid format specifier '%%%c' for '%s'";
    push(vm, OBJ_VAL(vtostr(vm, callee)));
    const char* fn = AS_CSTRING(*stackpeek(0));
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, c, fn)));
    runerror(vm, S_ESTRFMT);
}


sk_noret sovferror(VM* vm)
{
    static const char* fmt = "Stack overflow, limit overflown -> %d.";
    vm->sp--; // make some space
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, VM_STACK_LIMIT)));
    runerror(vm, S_ESOVERFLOW);
}


sk_noret udproperror(VM* vm, Value property, OClass* oclass)
{
    static const char* fmt = "Property '%s' is not defined for <class '%s'>.";
    const char* pname = AS_CSTRING(property);
    const char* classname = oclass->name->storage;
    push(vm, OBJ_VAL(OString_fmt(vm, pname, classname)));
    runerror(vm, S_EUDPROPERTY);
}


sk_noret retovferror(VM* vm, const char* fn)
{
    static const char* fmt = "Called function '%s' return count overflows the stack.";
    if(vm->sp - vm->stack >= VM_STACK_LIMIT) vm->sp--;
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, fn)));
    runerror(vm, S_ESOVERFLOW);
}


sk_noret arityerror(VM* vm, int expected, int got)
{
    static const char* fmt = "Expected %d arguments instead got %d.";
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, expected, got)));
    runerror(vm, S_EARITY);
}


sk_noret fcovferror(VM* vm)
{
    static const char* fmt = "Callstack overflow, limit overflown -> %lu.";
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, VM_CALLSTACK_LIMIT)));
    runerror(vm, S_EFOVERFLOW);
}


sk_noret callerror(VM* vm, Value callee)
{
    static const char* fmt = "Tried calling non-callable value %s.";
    push(vm, OBJ_VAL(vtostr(vm, callee)));
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, AS_CSTRING(*stackpeek(0)))));
    runerror(vm, S_ECALL);
}


sk_noret ipaerror(VM* vm, Value notinstance)
{
    static const char* fmt = "Invalid property access, tried accessing property on %s";
    push(vm, OBJ_VAL(vtostr(vm, notinstance)));
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, AS_CSTRING(*stackpeek(0)))));
    runerror(vm, S_EPACCESS);
}


sk_noret redefgerror(VM* vm, const char* gname)
{
    static const char* fmt = "Redefinition of global variable '%s'.";
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, gname)));
    runerror(vm, S_EGLOBALREDEF);
}


sk_noret udgerror(VM* vm, const char* gname)
{
    static const char* fmt = "Undefined global variable '%s'.";
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, gname)));
    runerror(vm, S_EUDGLOBAL);
}


sk_noret fixederror(VM* vm, const char* var)
{
    static const char* fmt = "Can't assign to variable '%s', it is declared as 'fixed'.";
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, var)));
    runerror(vm, S_EFIXEDASSIGN);
}


sk_noret nilidxerror(VM* vm)
{
    static const char* fmt = "Can't index with 'nil'.";
    push(vm, OBJ_VAL(OString_fmt(vm, fmt)));
    runerror(vm, S_ENILIDX);
}


sk_noret inheriterror(VM* vm, Value notclass)
{
    static const char* fmt = "Can't inherit from '%s', value must be class object.";
    push(vm, OBJ_VAL(vtostr(vm, notclass)));
    push(vm, OBJ_VAL(OString_fmt(vm, fmt, AS_CSTRING(*stackpeek(0)))));
    runerror(vm, S_EINHERIT);
}

/* --------------------------------------------------------- */ // runtime errors
