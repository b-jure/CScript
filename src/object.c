#include "array.h"
#include "debug.h"
#include "mem.h"
#include "object.h"
#include "skconf.h"
#include "value.h"

#include <stdio.h>
#include <stdlib.h>




#define ALLOC_OBJ(vm, object, type) ((object*)onew(vm, sizeof(object), type))

#define ALLOC_STRING(vm, len)                                                            \
    ((OString*)onew(vm, sizeof(OString) + (len) + 1, OBJ_STRING))




sstatic force_inline O* onew(VM* vm, size_t size, OType type)
{
    O* object      = GC_MALLOC(vm, size);
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

sstatic force_inline OString* OString_alloc(VM* vm, UInt len)
{
    OString* string = ALLOC_STRING(vm, len);
    string->len     = len;
    return string;
}

OString* OString_from(VM* vm, const char* chars, size_t len)
{
    Hash     hash     = stringhash(chars, len, vm->seed);
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

sstatic force_inline void OString_free(VM* vm, OString* string)
{
    GC_FREE(vm, string, sizeof(OString) + string->len + 1);
}

ONative* ONative_new(VM* vm, OString* name, NativeFn fn, Int arity, bool isva)
{
    ONative* native = ALLOC_OBJ(vm, ONative, OBJ_NATIVE);
    native->name    = name;
    native->fn      = fn;
    native->arity   = arity;
    native->isva    = isva;
    return native;
}

sstatic force_inline void ONative_free(VM* vm, ONative* native)
{
    GC_FREE(vm, native, sizeof(ONative));
}

OFunction* OFunction_new(VM* vm)
{
    OFunction* fn = ALLOC_OBJ(vm, OFunction, OBJ_FUNCTION);
    fn->name      = NULL;
    fn->upvalc    = 0;
    fn->arity     = 0;
    fn->vacnt     = 0;
    fn->isva      = 0;
    fn->isinit    = 0;
    fn->gotret    = 0;
    Chunk_init(&fn->chunk, vm);
    return fn;
}

sstatic force_inline void ObjFunction_free(VM* vm, OFunction* fn)
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
    closure->fn       = fn;
    closure->upvals   = upvals;
    closure->upvalc   = fn->upvalc;
    return closure;
}

sstatic force_inline void OClosure_free(VM* vm, OClosure* closure)
{
    GC_FREE(vm, closure->upvals, closure->upvalc * sizeof(OUpvalue*));
    GC_FREE(vm, closure, sizeof(OClosure));
}

OUpvalue* OUpvalue_new(VM* vm, Value* valp)
{
    OUpvalue* upval = ALLOC_OBJ(vm, OUpvalue, OBJ_UPVAL);
    upval->closed   = (Variable){EMPTY_VAL, 0};
    upval->location = valp;
    upval->next     = NULL;
    return upval;
}

sstatic force_inline void OUpvalue_free(VM* vm, OUpvalue* upval)
{
    GC_FREE(vm, upval, sizeof(OUpvalue));
}

OClass* OClass_new(VM* vm, OString* name)
{
    OClass* oclass = ALLOC_OBJ(vm, OClass, OBJ_CLASS); // GC
    oclass->name   = name;
    HashTable_init(&oclass->methods);
    oclass->overloaded = NULL;
    return oclass;
}

sstatic force_inline void OClass_free(VM* vm, OClass* oclass)
{
    HashTable_free(vm, &oclass->methods);
    GC_FREE(vm, oclass, sizeof(OClass));
}


OInstance* OInstance_new(VM* vm, OClass* oclass)
{
    OInstance* instance = ALLOC_OBJ(vm, OInstance, OBJ_INSTANCE);
    instance->oclass    = oclass;
    HashTable_init(&instance->fields);
    return instance;
}

sstatic force_inline void OInstance_free(VM* vm, OInstance* instance)
{
    HashTable_free(vm, &instance->fields);
    GC_FREE(vm, instance, sizeof(OInstance));
}

OBoundMethod* OBoundMethod_new(VM* vm, Value receiver, OClosure* method)
{
    OBoundMethod* bound_method = ALLOC_OBJ(vm, OBoundMethod, OBJ_BOUND_METHOD);
    bound_method->receiver     = receiver;
    bound_method->method       = method;
    return bound_method;
}

sstatic force_inline void fnprint(OFunction* fn)
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

void oprint(Value value)
{
#ifdef S_PRECOMPUTED_GOTO
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
            printf(
                "%p-%s instance",
                AS_OBJ(value),
                AS_INSTANCE(value)->oclass->name->storage);
            BREAK;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            fnprint(AS_BOUND_METHOD(value)->method->fn);
            BREAK;
        }
    }
    unreachable;
#ifdef SKOOMA_JMPTABLE_H
    #undef SKOOMA_JMPTABLE_H
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


sstatic force_inline void OBoundMethod_free(VM* vm, OBoundMethod* bound_method)
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
#ifdef S_PRECOMPUTED_GOTO
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
            OString_free(vm, (OString*)object);
            BREAK;
        }
        CASE(OBJ_FUNCTION)
        {
            ObjFunction_free(vm, (OFunction*)object);
            BREAK;
        }
        CASE(OBJ_CLOSURE)
        {
            OClosure_free(vm, (OClosure*)object);
            BREAK;
        }
        CASE(OBJ_NATIVE)
        {
            ONative_free(vm, (ONative*)object);
            BREAK;
        }
        CASE(OBJ_UPVAL)
        {
            OUpvalue_free(vm, (OUpvalue*)object);
            BREAK;
        }
        CASE(OBJ_CLASS)
        {
            OClass_free(vm, (OClass*)object);
            BREAK;
        }
        CASE(OBJ_INSTANCE)
        {
            OInstance_free(vm, (OInstance*)object);
            BREAK;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            OBoundMethod_free(vm, (OBoundMethod*)object);
            BREAK;
        }
    }
    unreachable;
#ifdef SKOOMA_JMPTABLE_H
    #undef SKOOMA_JMPTABLE_H
#endif
}

OString* otostr(VM* vm, O* object)
{
#ifdef S_PRECOMPUTED_GOTO
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
            return (OString*)object;
        }
        CASE(OBJ_FUNCTION)
        {
            return ((OFunction*)object)->name;
        }
        CASE(OBJ_CLOSURE)
        {
            return ((OClosure*)object)->fn->name;
        }
        CASE(OBJ_NATIVE)
        {
            return ((ONative*)object)->name;
        }
        CASE(OBJ_UPVAL)
        {
            return vtostr(vm, ((OUpvalue*)object)->closed.value);
        }
        CASE(OBJ_CLASS)
        {
            return ((OClass*)object)->name;
        }
        CASE(OBJ_INSTANCE)
        {
            OString*    name        = ((OInstance*)object)->oclass->name;
            const char* class_name  = name->storage;
            UInt        class_len   = name->len;
            const char* literal     = " instance";
            UInt        literal_len = sizeof(" instance") - 1;
            UInt        arrlen      = literal_len + class_len + 1;
            char        buff[arrlen];
            memcpy(buff, class_name, class_len);
            memcpy(buff + class_len, literal, literal_len);
            buff[arrlen - 1] = '\0';
            return OString_from(vm, buff, arrlen - 1);
        }
        CASE(OBJ_BOUND_METHOD)
        {
            return ((OBoundMethod*)object)->method->fn->name;
        }
    }
    unreachable;
#ifdef SKOOMA_JMPTABLE_H
    #undef SKOOMA_JMPTLE_H
#endif
}
