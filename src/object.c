#include "common.h"
#include "err.h"
#include "hashtable.h"
#include "mem.h"
#include "object.h"
#include "skconf.h"
#include "stdarg.h"
#include "value.h"

#include <stdio.h>
#include <stdlib.h>




#define ALLOC_OBJ(vm, object, type) ((object*)onew(vm, sizeof(object), type))

#define ALLOC_NATIVE(vm, upvals)                                                                   \
    ((ONative*)onew(vm, sizeof(ONative) + ((upvals) * sizeof(Value)), OBJ_NATIVE))

#define ALLOC_STRING(vm, len) ((OString*)onew(vm, sizeof(OString) + (len) + 1, OBJ_STRING))



/* Call info for overload-able methods */
const int overloadret[OM_DISPLAY + 1] = {
    0, // __init__
    1, // __display__
};



static force_inline O* onew(VM* vm, size_t size, OType type)
{
    O* object = GC_MALLOC(vm, size);
    object->header = (uint64_t)vm->objects | ((uint64_t)type << 56);
    osetmark(object, false);
    vm->objects = object; // Add object to the GC list
#ifdef DEBUG_LOG_GC
    printf("%p allocate %zu for ", (void*)object, size);
    otypeprint(type);
    printf("\n");
#endif
    return object;
}




static force_inline OString* OString_alloc(VM* vm, UInt len)
{
    OString* string = ALLOC_STRING(vm, len);
    string->len = len;
    return string;
}

OString* OString_new(VM* vm, const char* chars, size_t len)
{
    Hash hash = stringhash(chars, len, vm->seed);
    OString* interned = HashTable_get_intern(&vm->weakrefs, chars, len, hash);
    if(interned) return interned; // Return interned string
    OString* string = OString_alloc(vm, len);
    /**
     * According to C standard passing NULL pointer to memcpy
     * is considered undefined behaviour even if the number
     * of bytes to copy were 0.
     **/
    if(len == 0) *string->storage = '\0';
    else {
        memcpy(string->storage, chars, len);
        string->storage[len] = '\0';
    }
    string->hash = hash;
    push(vm, OBJ_VAL(string));
    HashTable_insert(vm, &vm->weakrefs, OBJ_VAL(string), NIL_VAL);
    pop(vm);
    return string;
}

OString* OString_fmt_from(VM* vm, const char* fmt, va_list argp)
{
    uint8_t c;
    Array_Byte buff;
    const char* fmttype = NULL;
    Array_Byte_init(&buff, vm);
    for(;;) {
        while((c = *fmt++) != '%')
            Array_Byte_push(&buff, c);
        c = *fmt++;
        switch(c) {
            case '%': {
                Array_Byte_push(&buff, '%');
                break;
            }
            case 'd': { /* int64_t */
                int64_t n = va_arg(argp, int64_t);
                Array_Byte_ensure(&buff, SK_NDIGITS);
                buff.len += snprintf((char*)&buff.data[buff.len], SK_NDIGITS, "%ld", n);
                break;
            }
            case 'n': { /* sk_number (as double) */
                fmttype = "%g";
                goto sknum;
            }
            case 'f': { /* sk_number (as float) */
                fmttype = "%f";
                goto sknum;
            }
            sknum: {
                sk_number n = va_arg(argp, sk_number);
                Array_Byte_ensure(&buff, SK_NDIGITS);
                char* s = cast_charp(&buff.data[buff.len]);
                buff.len += snprintf(s, SK_NDIGITS, fmttype, n);
                break;
            }
            case 's': { /* string */
                const char* str = va_arg(argp, char*);
                if(str == NULL) str = "(null)";
                size_t len = strlen(str);
                Array_Byte_ensure(&buff, len);
                memcpy(&buff.data[buff.len], str, len);
                buff.len += len;
                break;
            }
            case 'c': { /* char */
                int8_t c = va_arg(argp, int);
                Array_Byte_push(&buff, c);
                break;
            }
            case 'p': { /* pointer */
                Array_Byte_ensure(&buff, 11);
                void* ptr = va_arg(argp, void*);
                snprintf((char*)&buff.data[buff.len], 11, "%p", ptr);
                break;
            }
            default: { /* invalid format specifier */
                Array_Byte_free(&buff, NULL);
                ofmterror(vm, c, *last_frame(vm).callee);
            }
        }
    }
    OString* fstr = OString_new(vm, (char*)buff.data, buff.len);
    Array_Byte_free(&buff, NULL);
    return fstr;
}

OString* OString_fmt(VM* vm, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);
    OString* str = OString_fmt_from(vm, fmt, argp);
    va_end(argp);
    return str;
}

static force_inline void OString_free(VM* vm, OString* string)
{
    GC_FREE(vm, string, sizeof(OString) + string->len + 1);
}


OString* concatenate(VM* vm, Value a, Value b)
{
    OString* left = AS_STRING(a);
    OString* right = AS_STRING(b);
    size_t length = left->len + right->len;
    char buffer[length + 1];
    memcpy(buffer, left->storage, left->len);
    memcpy(buffer + left->len, right->storage, right->len);
    buffer[length] = '\0';
    OString* string = OString_new(vm, buffer, length);
    popn(vm, 2);
    return string;
}


OString* unescape(VM* vm, OString* string)
{
    Array_Byte new;
    Array_Byte_init(&new, vm);
    Array_Byte_init_cap(&new, string->len + 1);
    for(UInt i = 0; i < string->len; i++) {
        switch(string->storage[i]) {
            case '\n':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'n');
                break;
            case '\0':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, '0');
                break;
            case '\a':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'a');
                break;
            case '\b':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'b');
                break;
            case '\33':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'e');
                break;
            case '\f':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'f');
                break;
            case '\r':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'r');
                break;
            case '\t':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 't');
                break;
            case '\v':
                Array_Byte_push(&new, '\\');
                Array_Byte_push(&new, 'v');
                break;
            default:
                Array_Byte_push(&new, string->storage[i]);
        }
    }
    OString* unescaped = OString_new(vm, (void*)new.data, new.len);
    push(vm, OBJ_VAL(unescaped));
    Array_Byte_free(&new, NULL);
    pop(vm);
    return unescaped;
}




ONative* ONative_new(VM* vm, OString* name, CFunction fn, Int arity, bool isva, UInt upvals)
{
    ONative* native = ALLOC_NATIVE(vm, upvals);
    native->name = name;
    native->fn = fn;
    native->arity = arity;
    native->isva = isva;
    native->upvalc = upvals;
    for(Int i = 0; i < upvals; i++)
        native->upvalue[i] = NIL_VAL;
    return native;
}

static force_inline void ONative_free(VM* vm, ONative* native)
{
    GC_FREE(vm, native, sizeof(ONative) + (native->upvalc * sizeof(Value)));
}




OFunction* OFunction_new(VM* vm)
{
    OFunction* fn = ALLOC_OBJ(vm, OFunction, OBJ_FUNCTION);
    fn->name = NULL;
    fn->upvalc = 0;
    fn->arity = 0;
    fn->isva = 0;
    fn->isinit = 0;
    fn->gotret = 0;
    Chunk_init(&fn->chunk, vm);
    return fn;
}

static force_inline void ObjFunction_free(VM* vm, OFunction* fn)
{
    Chunk_free(&fn->chunk);
    GC_FREE(vm, fn, sizeof(OFunction));
}

static force_inline void fnprint(OFunction* fn, FILE* stream)
{
    if(unlikely(fn->name == NULL)) fprintf(stream, "<script>");
    else fprintf(stream, "<fn %s>: %p", fn->name->storage, fn);
}





OClosure* OClosure_new(VM* vm, OFunction* fn)
{
    OUpvalue** upvals = GC_MALLOC(vm, sizeof(OUpvalue*) * fn->upvalc);
    for(UInt i = 0; i < fn->upvalc; i++)
        upvals[i] = NULL;
    OClosure* closure = ALLOC_OBJ(vm, OClosure, OBJ_CLOSURE);
    closure->fn = fn;
    closure->upvalue = upvals;
    closure->upvalc = fn->upvalc;
    return closure;
}

static force_inline void OClosure_free(VM* vm, OClosure* closure)
{
    GC_FREE(vm, closure->upvalue, closure->upvalc * sizeof(OUpvalue*));
    GC_FREE(vm, closure, sizeof(OClosure));
}




OUpvalue* OUpvalue_new(VM* vm, Value* valp)
{
    OUpvalue* upval = ALLOC_OBJ(vm, OUpvalue, OBJ_UPVAL);
    upval->closed = EMPTY_VAL;
    upval->location = valp;
    upval->next = NULL;
    return upval;
}

static force_inline void OUpvalue_free(VM* vm, OUpvalue* upval)
{
    GC_FREE(vm, upval, sizeof(OUpvalue));
}




OClass* OClass_new(VM* vm, OString* name)
{
    OClass* oclass = ALLOC_OBJ(vm, OClass, OBJ_CLASS); // GC
    oclass->name = name;
    HashTable_init(&oclass->methods);
    memset(oclass->omethods, 0, sizeof(oclass->omethods) / sizeof(OClosure*));
    memset(oclass->sfields, 0, sizeof(oclass->sfields) / sizeof(Value));
    return oclass;
}

static force_inline void OClass_free(VM* vm, OClass* oclass)
{
    HashTable_free(vm, &oclass->methods);
    GC_FREE(vm, oclass, sizeof(OClass));
}




OInstance* OInstance_new(VM* vm, OClass* oclass)
{
    OInstance* instance = ALLOC_OBJ(vm, OInstance, OBJ_INSTANCE);
    instance->oclass = oclass;
    HashTable_init(&instance->fields);
    return instance;
}

/* Return overloaded method or NULL if value
 * is not an instance value or method is not overloaded. */
static force_inline OClosure* getomethod(VM* vm, Value val, OMTag om)
{
    if(IS_INSTANCE(val)) return AS_INSTANCE(val)->oclass->omethods[om];
    return NULL;
}


static force_inline void OInstance_free(VM* vm, OInstance* instance)
{
    HashTable_free(vm, &instance->fields);
    GC_FREE(vm, instance, sizeof(OInstance));
}




OBoundMethod* OBoundMethod_new(VM* vm, Value receiver, OClosure* method)
{
    OBoundMethod* bound_method = ALLOC_OBJ(vm, OBoundMethod, OBJ_BOUND_METHOD);
    bound_method->receiver = receiver;
    bound_method->method = method;
    return bound_method;
}

static force_inline void OBoundMethod_free(VM* vm, OBoundMethod* bound_method)
{
    GC_FREE(vm, bound_method, sizeof(OBoundMethod));
}





sdebug void otypeprint(OType type)
{
    switch(type) {
        case OBJ_STRING:
            printf("OBJ_STRING");
            break;
        case OBJ_FUNCTION:
            printf("OBJ_FUNCTION");
            break;
        case OBJ_CLOSURE:
            printf("OBJ_CLOSURE");
            break;
        case OBJ_NATIVE:
            printf("OBJ_NATIVE");
            break;
        case OBJ_UPVAL:
            printf("OBJ_UPVAL");
            break;
        case OBJ_CLASS:
            printf("OBJ_CLASS");
            break;
        case OBJ_INSTANCE:
            printf("OBJ_INSTANCE");
            break;
        case OBJ_BOUND_METHOD:
            printf("OBJ_BOUND_METHOD");
            break;
        default:
            unreachable;
    }
}

/* Hash object value */
Hash ohash(Value value)
{
    switch(OBJ_TYPE(value)) {
        case OBJ_STRING: // we already have the hash (xxHash64)
            return AS_STRING(value)->hash;
        default: // just hash the pointer
            return ptrhash(cast(const void*, AS_OBJ(value)));
    }
}


/* Free object memory */
void ofree(VM* vm, O* object)
{
#ifdef DEBUG_LOG_GC
    printf("%p free type ", (void*)object);
    otypeprint(otype(object));
    printf("\n");
#endif
#ifdef SK_PRECOMPUTED_GOTO
#define OBJ_TABLE
#include "jmptable.h"
#undef OBJ_TABLE
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#define BREAK       return
#endif
    DISPATCH(otype(object))
    {
        CASE(OBJ_STRING)
        {
            OString_free(vm, cast(OString*, object));
            BREAK;
        }
        CASE(OBJ_FUNCTION)
        {
            ObjFunction_free(vm, cast(OFunction*, object));
            BREAK;
        }
        CASE(OBJ_CLOSURE)
        {
            OClosure_free(vm, cast(OClosure*, object));
            BREAK;
        }
        CASE(OBJ_NATIVE)
        {
            ONative_free(vm, cast(ONative*, object));
            BREAK;
        }
        CASE(OBJ_UPVAL)
        {
            OUpvalue_free(vm, cast(OUpvalue*, object));
            BREAK;
        }
        CASE(OBJ_CLASS)
        {
            OClass_free(vm, cast(OClass*, object));
            BREAK;
        }
        CASE(OBJ_INSTANCE)
        {
            OInstance_free(vm, cast(OInstance*, object));
            BREAK;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            OBoundMethod_free(vm, cast(OBoundMethod*, object));
            BREAK;
        }
    }
    unreachable;
#ifdef SKJMPTABLE_H
#undef SKJMPTABLE_H
#endif
}







/* =============== call overload-able methods =============== */

static int callomdisplay(VM* vm, Value val)
{
    OClosure* omethod = getomethod(vm, val, OM_DISPLAY);
    if(omethod) {
        Value* ret = vm->sp;
        push(vm, val); // push instance 'self'
        push(vm, OBJ_VAL(omethod));
        ncall(vm, ret, OBJ_VAL(omethod), 1);
        Value top = *stackpeek(0);
        if(unlikely(!IS_STRING(top))) disperror(vm, top);
        return 1;
    }
    return 0;
}







/* =============== operator overloading =============== */

#if defined(SK_OVERLOAD_OPS)

/* Tries to call class overloaded unary operator method 'op'.
 * Returns 1 if it was called (class overloaded that method),
 * 0 otherwise. */
static force_inline int callunop(VM* vm, Value a, OMTag op, Value* res)
{
    OClosure* om = getomethod(vm, a, op);
    if(om == NULL) return 0;
    Value* fn = vm->sp;
    push(vm, OBJ_VAL(AS_INSTANCE(a)->oclass));
    push(vm, a);
    ncall(vm, fn, OBJ_VAL(om), 1);
    *res = pop(vm);
    return 1;
}


/* Tries to call class overloaded binary operator method 'op'.
 * Returns 1 if it was called (class overloaded that method),
 * 0 otherwise. */
static force_inline int callbinop(VM* vm, Value a, Value b, OMTag op, Value* res)
{
    Value receiver = a;
    OClosure* om = getomethod(vm, a, op);
    if(om == NULL) {
        receiver = b;
        om = getomethod(vm, b, op);
    }
    if(om == NULL) return 0;
    Value* fn = vm->sp;
    push(vm, OBJ_VAL(AS_INSTANCE(receiver)->oclass));
    push(vm, a);
    push(vm, b);
    ncall(vm, fn, OBJ_VAL(om), 1);
    *res = pop(vm);
    return 1;
}


/* Tries calling binary or unary overloaded operator method, errors on failure. */
void otryop(VM* vm, Value a, Value b, OMTag op, Value* res)
{
    if(!omisunop(op)) {
        if(unlikely(!callbinop(vm, a, b, op, res))) binoperror(vm, a, b, op - OM_ADD);
    } else if(unlikely(!callunop(vm, a, op, res))) unoperror(vm, a, op - OM_ADD);
}





/* =============== ordering =============== */

static force_inline int omcallorder(VM* vm, Value l, Value r, OMTag ordop)
{
    sk_assert(vm, ordop >= OM_NE && ordop <= OM_GE, "invalid OMTag for order");
    if(callbinop(vm, l, r, ordop, stackpeek(1))) { // try overload
        --vm->sp; // remove second operand
        return 1;
    }
    if(unlikely(ordop != OM_EQ || ordop != OM_NE)) ordererror(vm, l, r);
    return 0;
}

/* != */
void one(VM* vm, Value l, Value r)
{
    if(IS_STRING(l) && IS_STRING(r)) push(vm, BOOL_VAL(l != r));
    else if(!omcallorder(vm, l, r, OM_NE)) push(vm, BOOL_VAL(l != r));
}

/* == */
void oeq(VM* vm, Value l, Value r)
{
    if(IS_STRING(l) && IS_STRING(r)) push(vm, BOOL_VAL(l == r));
    else if(!omcallorder(vm, l, r, OM_EQ)) push(vm, BOOL_VAL(l == r));
}

/* < */
void olt(VM* vm, Value l, Value r)
{
    if(IS_STRING(l) && IS_STRING(r)) push(vm, BOOL_VAL(strcmp(AS_CSTRING(l), AS_CSTRING(r)) < 0));
    else omcallorder(vm, l, r, OM_LT);
}

/* > */
void ogt(VM* vm, Value l, Value r)
{
    if(IS_STRING(l) && IS_STRING(r)) push(vm, BOOL_VAL(strcmp(AS_CSTRING(l), AS_CSTRING(r)) > 0));
    else omcallorder(vm, l, r, OM_GT);
}

/* <= */
void ole(VM* vm, Value l, Value r)
{
    if(IS_STRING(l) && IS_STRING(r)) push(vm, BOOL_VAL(strcmp(AS_CSTRING(l), AS_CSTRING(r)) <= 0));
    else omcallorder(vm, l, r, OM_LE);
}

/* >= */
void oge(VM* vm, Value l, Value r)
{
    if(IS_STRING(l) && IS_STRING(r)) push(vm, BOOL_VAL(strcmp(AS_CSTRING(l), AS_CSTRING(r)) >= 0));
    else omcallorder(vm, l, r, OM_GE);
}

/* ---------------------------------------------------- */ // ordering

#endif // if defined(SK_OVERLOAD_OPS)

/* ---------------------------------------------------- */ // operator overloading






/* Create/Get object string from 'object' */
OString* otostr(VM* vm, O* object)
{
#ifdef SK_PRECOMPUTED_GOTO
#define OBJ_TABLE
#include "jmptable.h"
#undef OBJ_TABLE
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#endif
    DISPATCH(otype(object))
    {
        CASE(OBJ_STRING)
        {
            return cast(OString*, object);
        }
        CASE(OBJ_FUNCTION)
        {
            return cast(OFunction*, object)->name;
        }
        CASE(OBJ_CLOSURE)
        {
            return cast(OClosure*, object)->fn->name;
        }
        CASE(OBJ_NATIVE)
        {
            return cast(ONative*, object)->name;
        }
        CASE(OBJ_UPVAL)
        {
            return vtostr(vm, cast(OUpvalue*, object)->closed);
        }
        CASE(OBJ_CLASS)
        {
            return cast(OClass*, object)->name;
        }
        CASE(OBJ_INSTANCE)
        {
            OInstance* instance = cast(OInstance*, object);
            Value debug = getsfield(instance, SF_DEBUG);
            if(IS_STRING(debug)) return AS_STRING(debug);
            return instance->oclass->name;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            return cast(OBoundMethod*, object)->method->fn->name;
        }
    }
    unreachable;
#ifdef SKJMPTABLE_H
#undef SKJMPTABLE_H
#endif
}


/* Print the object value */
void oprint(VM* vm, Value value, FILE* stream)
{
#ifdef SK_PRECOMPUTED_GOTO
#define OBJ_TABLE
#include "jmptable.h"
#undef OBJ_TABLE
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#define BREAK       return
#endif
    DISPATCH(OBJ_TYPE(value))
    {
        CASE(OBJ_STRING)
        {
            fprintf(stream, "%s", AS_CSTRING(value));
            BREAK;
        }
        CASE(OBJ_FUNCTION)
        {
            fnprint(AS_FUNCTION(value), stream);
            BREAK;
        }
        CASE(OBJ_CLOSURE)
        {
            fnprint(AS_CLOSURE(value)->fn, stream);
            BREAK;
        }
        CASE(OBJ_NATIVE)
        {
            fprintf(stream, "<native-fn %s>", AS_NATIVE(value)->name->storage);
            BREAK;
        }
        CASE(OBJ_UPVAL)
        {
            OUpvalue* upval = AS_UPVAL(value);
            vprint(vm, *upval->location, stream);
            BREAK;
        }
        CASE(OBJ_CLASS)
        {
            fprintf(stream, "%p-%s", AS_OBJ(value), AS_CLASS(value)->name->storage);
            BREAK;
        }
        CASE(OBJ_INSTANCE)
        {
            if(callomdisplay(vm, value)) {
                fprintf(stream, "%s", AS_CSTRING(*stackpeek(0)));
            } else
                fprintf(
                    stream,
                    "%p-%s instance",
                    AS_OBJ(value),
                    AS_INSTANCE(value)->oclass->name->storage);
            BREAK;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            fnprint(AS_BOUND_METHOD(value)->method->fn, stream);
            BREAK;
        }
    }
    unreachable;
#ifdef SKJMPTABLE_H
#undef SKJMPTABLE_H
#endif
}
