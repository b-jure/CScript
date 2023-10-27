#include "array.h"
#include "mem.h"
#include "object.h"
#include "skconf.h"
#include "value.h"
#ifdef DEBUG
    #include "debug.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#define ALLOC_OBJ(vm, c, object, type) ((object*)Obj_new(vm, c, sizeof(object), type))

#define ALLOC_STRING(vm, c, len)                                                         \
    ((ObjString*)Obj_new(vm, c, sizeof(ObjString) + (len) + 1, OBJ_STRING))

SK_INTERNAL(force_inline Obj*) Obj_new(VM* vm, Compiler* C, size_t size, ObjType type)
{
    Obj* object = GC_MALLOC(vm, C, size);

    object->header = (uint64_t)vm->objects | ((uint64_t)type << 56);

    // malloc and C standard do not guarantee that upper 16 bits
    // will be initialized to 0, we initialized all the bits except the
    // mark bit, so make sure it is set to false!
    Obj_mark_set(object, false);

#ifdef DEBUG
    assert(Obj_type(object) == type);
    assert(Obj_next(object) == vm->objects);
    assert(Obj_marked(object) == false);
#endif

    // Add object to the GC list
    vm->objects = object;

#ifdef DEBUG_LOG_GC
    printf("%p allocate %zu for ", (void*)object, size);
    ObjType_print(type);
    printf("\n");
#endif

    return object;
}

SK_INTERNAL(force_inline ObjString*) ObjString_alloc(VM* vm, Compiler* C, UInt len)
{
    ObjString* string = ALLOC_STRING(vm, C, len);
    string->len       = len;
    return string;
}

ObjString* ObjString_from(VM* vm, Compiler* C, const char* chars, size_t len)
{
    uint64_t   hash     = Hash_string(chars, len);
    ObjString* interned = HashTable_get_intern(&vm->strings, chars, len, hash);

    if(interned) { // Return interned string
        return interned;
    }

    ObjString* string = ObjString_alloc(vm, C, len);
    memcpy(string->storage, chars, len);
    string->storage[len] = '\0';
    string->hash         = hash;

    VM_push(vm, OBJ_VAL(string)); // GC
    HashTable_insert(vm, C, &vm->strings, OBJ_VAL(string), NIL_VAL);
    VM_pop(vm); // GC
    return string;
}

SK_INTERNAL(force_inline void) ObjString_free(VM* vm, Compiler* C, ObjString* string)
{
    GC_FREE(vm, C, string, sizeof(ObjString) + string->len + 1);
}

ObjNative* ObjNative_new(VM* vm, Compiler* C, ObjString* name, NativeFn fn, Int arity)
{
    ObjNative* native = ALLOC_OBJ(vm, C, ObjNative, OBJ_NATIVE);
    //
    native->name  = name;
    native->fn    = fn;
    native->arity = arity;
    return native;
}

SK_INTERNAL(force_inline void) ObjNative_free(VM* vm, Compiler* C, ObjNative* native)
{
    GC_FREE(vm, C, native, sizeof(ObjNative));
}

ObjFunction* ObjFunction_new(VM* vm, Compiler* C)
{
    ObjFunction* fn = ALLOC_OBJ(vm, C, ObjFunction, OBJ_FUNCTION);
    fn->arity       = 0;
    fn->name        = NULL;
    fn->upvalc      = 0;
    Chunk_init(&fn->chunk);
    return fn;
}

SK_INTERNAL(force_inline void) ObjFunction_free(VM* vm, Compiler* C, ObjFunction* fn)
{
    Chunk_free(&fn->chunk, vm, C);
    GC_FREE(vm, C, fn, sizeof(ObjFunction));
}

ObjClosure* ObjClosure_new(VM* vm, Compiler* C, ObjFunction* fn)
{
    ObjUpvalue** upvals = GC_MALLOC(vm, C, sizeof(ObjUpvalue*) * fn->upvalc);
    for(UInt i = 0; i < fn->upvalc; i++) {
        upvals[i] = NULL;
    }

    ObjClosure* closure = ALLOC_OBJ(vm, C, ObjClosure, OBJ_CLOSURE);
    closure->fn         = fn;
    closure->upvals     = upvals;
    closure->upvalc     = fn->upvalc;

    return closure;
}

SK_INTERNAL(force_inline void) ObjClosure_free(VM* vm, Compiler* C, ObjClosure* closure)
{
    GC_FREE(vm, C, closure->upvals, closure->upvalc * sizeof(ObjUpvalue*));
    GC_FREE(vm, C, closure, sizeof(ObjClosure));
}

ObjUpvalue* ObjUpvalue_new(VM* vm, Compiler* C, Value* var_ref)
{
    ObjUpvalue* upval = ALLOC_OBJ(vm, C, ObjUpvalue, OBJ_UPVAL);
    upval->closed     = EMPTY_VAL;
    upval->location   = var_ref;
    upval->next       = NULL;
    return upval;
}

SK_INTERNAL(force_inline void) ObjUpvalue_free(VM* vm, Compiler* C, ObjUpvalue* upval)
{
    GC_FREE(vm, C, upval, sizeof(ObjUpvalue));
}

ObjClass* ObjClass_new(VM* vm, Compiler* C, ObjString* name)
{
    ObjClass* cclass = ALLOC_OBJ(vm, C, ObjClass, OBJ_CLASS); // GC
    cclass->name     = name;
    HashTable_init(&cclass->methods);
    for(UInt i = 0; i < OPSN; i++) {
        cclass->overloaded[i] = NULL;
    }
    return cclass;
}

SK_INTERNAL(force_inline void) ObjClass_free(VM* vm, Compiler* C, ObjClass* cclass)
{
    HashTable_free(vm, C, &cclass->methods);
    GC_FREE(vm, C, cclass, sizeof(ObjClass));
}


ObjInstance* ObjInstance_new(VM* vm, Compiler* C, ObjClass* cclass)
{
    ObjInstance* instance = ALLOC_OBJ(vm, C, ObjInstance, OBJ_INSTANCE);
    instance->cclass      = cclass;
    HashTable_init(&instance->fields);
    return instance;
}

SK_INTERNAL(force_inline void)
ObjInstance_free(VM* vm, Compiler* C, ObjInstance* instance)
{
    HashTable_free(vm, C, &instance->fields);
    GC_FREE(vm, C, instance, sizeof(ObjInstance));
}

ObjBoundMethod* ObjBoundMethod_new(VM* vm, Compiler* C, Value receiver, Obj* method)
{
    ObjBoundMethod* bound_method = ALLOC_OBJ(vm, C, ObjBoundMethod, OBJ_BOUND_METHOD);
    bound_method->receiver       = receiver;
    bound_method->method         = method;
    return bound_method;
}

SK_INTERNAL(force_inline void) print_fn(ObjFunction* fn)
{
    if(unlikely(fn->name == NULL)) {
        printf("<script>"); // Debug only
    } else {
        printf("<fn %s>", fn->name->storage);
    }
}

void ObjType_print(ObjType type) // Debug
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

void Obj_print(Value value)
{
#ifdef THREADED_CODE
    #define OBJ_TABLE
    #include "jmptable.h"
    #undef OBJ_TABLE
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
    #define BREAK       break
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
            print_fn(AS_FUNCTION(value));
            BREAK;
        }
        CASE(OBJ_CLOSURE)
        {
            print_fn(AS_CLOSURE(value)->fn);
            BREAK;
        }
        CASE(OBJ_NATIVE)
        {
            printf("<native fn %s>", AS_NATIVE(value)->name->storage);
            BREAK;
        }
        CASE(OBJ_UPVAL)
        {
            ObjUpvalue* upval = AS_UPVAL(value);
            Value_print(upval->closed);
            BREAK;
        }
        CASE(OBJ_CLASS)
        {
            printf("%s", AS_CLASS(value)->name->storage);
            BREAK;
        }
        CASE(OBJ_INSTANCE)
        {
            printf("%s instance", AS_INSTANCE(value)->cclass->name->storage);
            BREAK;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            ObjBoundMethod* bound_method = AS_BOUND_METHOD(value);
            if(Obj_type(bound_method->method) == OBJ_CLOSURE) {
                print_fn(((ObjClosure*)bound_method->method)->fn);
            } else {
                print_fn(((ObjFunction*)bound_method->method));
            }
            BREAK;
        }
    }

    unreachable;

#ifdef __SKOOMA_JMPTABLE_H__
    #undef __SKOOMA_JMPTABLE_H__
#endif
}

Hash Obj_hash(Value value)
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


SK_INTERNAL(force_inline void)
ObjBoundMethod_free(VM* vm, Compiler* C, ObjBoundMethod* bound_method)
{
    GC_FREE(vm, C, bound_method, sizeof(ObjBoundMethod));
}

void Obj_free(VM* vm, Compiler* C, Obj* object)
{
#ifdef DEBUG_LOG_GC
    printf("%p free type ", (void*)object);
    ObjType_print(Obj_type(object));
    printf("\n");
#endif

#ifdef THREADED_CODE
    #define OBJ_TABLE
    #include "jmptable.h"
    #undef OBJ_TABLE
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
    #define BREAK       break
#endif

    DISPATCH(Obj_type(object))
    {
        CASE(OBJ_STRING)
        {
            ObjString_free(vm, C, (ObjString*)object);
            BREAK;
        }
        CASE(OBJ_FUNCTION)
        {
            ObjFunction_free(vm, C, (ObjFunction*)object);
            BREAK;
        }
        CASE(OBJ_CLOSURE)
        {
            ObjClosure_free(vm, C, (ObjClosure*)object);
            BREAK;
        }
        CASE(OBJ_NATIVE)
        {
            ObjNative_free(vm, C, (ObjNative*)object);
            BREAK;
        }
        CASE(OBJ_UPVAL)
        {
            ObjUpvalue_free(vm, C, (ObjUpvalue*)object);
            BREAK;
        }
        CASE(OBJ_CLASS)
        {
            ObjClass_free(vm, C, (ObjClass*)object);
            BREAK;
        }
        CASE(OBJ_INSTANCE)
        {
            ObjInstance_free(vm, C, (ObjInstance*)object);
            BREAK;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            ObjBoundMethod_free(vm, C, (ObjBoundMethod*)object);
            BREAK;
        }
    }

    unreachable;

#ifdef __SKOOMA_JMPTABLE_H__
    #undef __SKOOMA_JMPTABLE_H__
#endif
}

ObjString* Obj_to_str(VM* vm, Compiler* C, Obj* object)
{
#ifdef THREADED_CODE
    #define OBJ_TABLE
    #include "jmptable.h"
    #undef OBJ_TABLE
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
    #define BREAK       break
#endif

    DISPATCH(Obj_type(object))
    {
        CASE(OBJ_STRING)
        {
            return (ObjString*)object;
        }
        CASE(OBJ_FUNCTION)
        {
            return ((ObjFunction*)object)->name;
        }
        CASE(OBJ_CLOSURE)
        {
            return ((ObjClosure*)object)->fn->name;
        }
        CASE(OBJ_NATIVE)
        {
            return ((ObjNative*)object)->name;
        }
        CASE(OBJ_UPVAL)
        {
            return Value_to_str(vm, C, ((ObjUpvalue*)object)->closed);
        }
        CASE(OBJ_CLASS)
        {
            return ((ObjClass*)object)->name;
        }
        CASE(OBJ_INSTANCE)
        {
            ObjString*  name        = ((ObjInstance*)object)->cclass->name;
            const char* class_name  = name->storage;
            UInt        class_len   = name->len;
            const char* literal     = " instance";
            UInt        literal_len = sizeof(" instance") - 1;
            UInt        arrlen      = literal_len + class_len + 1;
            char        buff[arrlen];

            memcpy(buff, class_name, class_len);
            memcpy(buff + class_len, literal, literal_len);
            buff[arrlen - 1] = '\0';

            return ObjString_from(vm, C, buff, arrlen - 1);
        }
        CASE(OBJ_BOUND_METHOD)
        {
            ObjBoundMethod* bound = (ObjBoundMethod*)object;
            if(Obj_type(bound->method) == OBJ_CLOSURE) {
                return ((ObjClosure*)bound->method)->fn->name;
            } else {
                return ((ObjFunction*)bound->method)->name;
            }
        }
    }

    unreachable;

#ifdef __SKOOMA_JMPTABLE_H__
    #undef __SKOOMA_JMPTABLE_H__
#endif
}
