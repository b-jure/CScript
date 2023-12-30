#include "array.h"
#include "common.h"
#include "debug.h"
#include "err.h"
#include "mem.h"
#include "object.h"
#include "skconf.h"
#include "stdarg.h"
#include "value.h"

#include <stdio.h>
#include <stdlib.h>




#define ALLOC_OBJ(vm, object, type) ((object*)onew(vm, sizeof(object), type))

#define ALLOC_STRING(vm, len)                                                            \
    ((OString*)onew(vm, sizeof(OString) + (len) + 1, OBJ_STRING))




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
    OString* interned = HashTable_get_intern(&vm->strings, chars, len, hash);
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
    HashTable_insert(vm, &vm->strings, OBJ_VAL(string), NIL_VAL);
    pop(vm);
    return string;
}

OString* OString_fmt_from(VM* vm, const char* fmt, va_list argp)
{
#define MAXDIGITS 45

    unsigned char c;
    Array_Byte buff;
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
            case 'd': { /* int */
                int n = va_arg(argp, int);
                Array_Byte_ensure(&buff, MAXDIGITS);
                buff.len += snprintf((char*)&buff.data[buff.len], MAXDIGITS, "%d", n);
                break;
            }
            case 'n': { /* SK_Number */
                sk_number n = va_arg(argp, sk_number);
                Array_Byte_ensure(&buff, MAXDIGITS);
                buff.len += dtos_generic(n, (char*)&buff.data[buff.len], MAXDIGITS);
                break;
            }
            case 'f': { /* double */
                double n = va_arg(argp, double);
                Array_Byte_ensure(&buff, MAXDIGITS);
                buff.len += snprintf((char*)&buff.data[buff.len], MAXDIGITS, "%g", n);
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
                char c = va_arg(argp, int);
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
                OString* fn = vtostr(vm, *vm->frames[vm->fc - 1].callee);
                vm->sp[-1] = OBJ_VAL(OSTRINGF_ERR(vm, c, fn->storage));
                return NULL;
            }
        }
    }
    OString* fstr = OString_new(vm, (char*)buff.data, buff.len);
    Array_Byte_free(&buff, NULL);
    return fstr;

#undef MAXDIGITS
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


ONative* ONative_new(VM* vm, OString* name, CFunction fn, Int arity, bool isva)
{
    ONative* native = ALLOC_OBJ(vm, ONative, OBJ_NATIVE);
    native->name = name;
    native->fn = fn;
    native->arity = arity;
    native->isva = isva;
    return native;
}

static force_inline void ONative_free(VM* vm, ONative* native)
{
    GC_FREE(vm, native, sizeof(ONative));
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

OClosure* OClosure_new(VM* vm, OFunction* fn)
{
    OUpvalue** upvals = GC_MALLOC(vm, sizeof(OUpvalue*) * fn->upvalc);
    for(UInt i = 0; i < fn->upvalc; i++)
        upvals[i] = NULL;
    OClosure* closure = ALLOC_OBJ(vm, OClosure, OBJ_CLOSURE);
    closure->fn = fn;
    closure->upvals = upvals;
    closure->upvalc = fn->upvalc;
    return closure;
}

static force_inline void OClosure_free(VM* vm, OClosure* closure)
{
    GC_FREE(vm, closure->upvals, closure->upvalc * sizeof(OUpvalue*));
    GC_FREE(vm, closure, sizeof(OClosure));
}

OUpvalue* OUpvalue_new(VM* vm, Value* valp)
{
    OUpvalue* upval = ALLOC_OBJ(vm, OUpvalue, OBJ_UPVAL);
    upval->closed = (Variable){EMPTY_VAL, 0};
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
    oclass->overloaded = NULL;
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

static force_inline void fnprint(OFunction* fn)
{
    if(unlikely(fn->name == NULL)) printf("<script>");
    else printf("<fn %s>: %p", fn->name->storage, fn);
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

void oprint(VM* vm, Value value)
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
            printf("%s", AS_CSTRING(value));
            BREAK;
        }
        CASE(OBJ_FUNCTION)
        {
            fnprint(AS_FUNCTION(value));
            BREAK;
        }
        CASE(OBJ_CLOSURE)
        {
            fnprint(AS_CLOSURE(value)->fn);
            BREAK;
        }
        CASE(OBJ_NATIVE)
        {
            printf("<native-fn %s>", AS_NATIVE(value)->name->storage);
            BREAK;
        }
        CASE(OBJ_UPVAL)
        {
            OUpvalue* upval = AS_UPVAL(value);
            vprint(*upval->location);
            BREAK;
        }
        CASE(OBJ_CLASS)
        {
            printf("%p-%s", AS_OBJ(value), AS_CLASS(value)->name->storage);
            BREAK;
        }
        CASE(OBJ_INSTANCE)
        {
            OInstance* instance = AS_INSTANCE(value);
            Value display;
            Value top;
            if(HashTable_get(
                   &instance->oclass->methods,
                   OBJ_VAL(vm->statics[SS_DISP]),
                   &display))
            {
                push(vm, display); // push display method
                sk_call(vm, 0, 1); // call it
                if(unlikely(!IS_STRING(top = *stackpeek(0)))) {
                    push(vm, OBJ_VAL(vtostr(vm, top)));
                    vm->sp[-2] =
                        OBJ_VAL(DISPLAY_INVALID_TYPE(vm, AS_CSTRING(*stackpeek(0))));
                    vm->sp--;
                    runerror(vm, S_EDISPLAY);
                }
                printf("%s", AS_CSTRING(top));
            } else {
                printf(
                    "%p-%s instance",
                    AS_OBJ(value),
                    AS_INSTANCE(value)->oclass->name->storage);
            }
            BREAK;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            fnprint(AS_BOUND_METHOD(value)->method->fn);
            BREAK;
        }
    }
    unreachable;
#ifdef SKJMPTABLE_H
#undef SKJMPTABLE_H
#endif
}

Hash ohash(Value value)
{
    switch(OBJ_TYPE(value)) {
        case OBJ_STRING:
            return AS_STRING(value)->hash;
        case OBJ_FUNCTION:
            return AS_FUNCTION(value)->name->hash;
        default:
            unreachable;
    }
}


static force_inline void OBoundMethod_free(VM* vm, OBoundMethod* bound_method)
{
    GC_FREE(vm, bound_method, sizeof(OBoundMethod));
}

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

OString* otostr(VM* vm, O* object)
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
            return vtostr(vm, cast(OUpvalue*, object)->closed.value);
        }
        CASE(OBJ_CLASS)
        {
            return cast(OClass*, object)->name;
        }
        CASE(OBJ_INSTANCE)
        {
            Value debug;
            OInstance* instance = cast(OInstance*, object);
            if(HashTable_get(&instance->fields, OBJ_VAL(vm->statics[SS_DBG]), &debug) &&
               IS_STRING(debug))
                return AS_STRING(debug);
            return instance->oclass->name;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            return cast(OBoundMethod*, object)->method->fn->name;
        }
    }
    unreachable;
#ifdef SKJMPTABLE_H
#undef SKJMPTLE_H
#endif
}
