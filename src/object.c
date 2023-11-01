#include "array.h"
#include "debug.h"
#include "mem.h"
#include "object.h"
#include "skconf.h"
#include "value.h"

#include <stdio.h>
#include <stdlib.h>

#define ALLOC_OBJ(vm, object, type) ((object*)Obj_new(vm, sizeof(object), type))

#define ALLOC_STRING(vm, len) ((ObjString*)Obj_new(vm, sizeof(ObjString) + (len) + 1, OBJ_STRING))

SK_INTERNAL(force_inline Obj*) Obj_new(VM* vm, size_t size, ObjType type)
{
    Obj* object = GC_MALLOC(vm, size);

    object->header = (uint64_t)vm->objects | ((uint64_t)type << 56);

    Obj_mark_set(object, false);

    // Add object to the GC list
    vm->objects = object;

#ifdef DEBUG_LOG_GC
    printf("%p allocate %zu for ", (void*)object, size);
    ObjType_print(type);
    printf("\n");
#endif

    return object;
}

SK_INTERNAL(force_inline ObjString*) ObjString_alloc(VM* vm, UInt len)
{
    ObjString* string = ALLOC_STRING(vm, len);
    string->len       = len;
    return string;
}

ObjString* ObjString_from(VM* vm, const char* chars, size_t len)
{
    uint64_t   hash     = Hash_string(chars, len);
    ObjString* interned = HashTable_get_intern(&vm->strings, chars, len, hash);

    if(interned) { // Return interned string
        return interned;
    }

    ObjString* string = ObjString_alloc(vm, len);
    memcpy(string->storage, chars, len);
    string->storage[len] = '\0';
    string->hash         = hash;

    VM_push(vm, OBJ_VAL(string)); // GC
    HashTable_insert(vm, &vm->strings, OBJ_VAL(string), NIL_VAL);
    VM_pop(vm); // GC
    return string;
}

SK_INTERNAL(force_inline void) ObjString_free(VM* vm, ObjString* string)
{
    GC_FREE(vm, string, sizeof(ObjString) + string->len + 1);
}

ObjNative* ObjNative_new(VM* vm, ObjString* name, NativeFn fn, Int arity)
{
    ObjNative* native = ALLOC_OBJ(vm, ObjNative, OBJ_NATIVE);
    //
    native->name  = name;
    native->fn    = fn;
    native->arity = arity;
    return native;
}

SK_INTERNAL(force_inline void) ObjNative_free(VM* vm, ObjNative* native)
{
    GC_FREE(vm, native, sizeof(ObjNative));
}

ObjFunction* ObjFunction_new(VM* vm)
{
    ObjFunction* fn = ALLOC_OBJ(vm, ObjFunction, OBJ_FUNCTION);
    fn->arity       = 0;
    fn->name        = NULL;
    fn->upvalc      = 0;
    Chunk_init(&fn->chunk);
    return fn;
}

SK_INTERNAL(force_inline void) ObjFunction_free(VM* vm, ObjFunction* fn)
{
    Chunk_free(&fn->chunk, vm);
    GC_FREE(vm, fn, sizeof(ObjFunction));
}

ObjClosure* ObjClosure_new(VM* vm, ObjFunction* fn)
{
    ObjUpvalue** upvals = GC_MALLOC(vm, sizeof(ObjUpvalue*) * fn->upvalc);
    for(UInt i = 0; i < fn->upvalc; i++) {
        upvals[i] = NULL;
    }

    ObjClosure* closure = ALLOC_OBJ(vm, ObjClosure, OBJ_CLOSURE);
    closure->fn         = fn;
    closure->upvals     = upvals;
    closure->upvalc     = fn->upvalc;

    return closure;
}

SK_INTERNAL(force_inline void) ObjClosure_free(VM* vm, ObjClosure* closure)
{
    GC_FREE(vm, closure->upvals, closure->upvalc * sizeof(ObjUpvalue*));
    GC_FREE(vm, closure, sizeof(ObjClosure));
}

ObjUpvalue* ObjUpvalue_new(VM* vm, Value* var_ref)
{
    ObjUpvalue* upval = ALLOC_OBJ(vm, ObjUpvalue, OBJ_UPVAL);
    upval->closed     = EMPTY_VAL;
    upval->location   = var_ref;
    upval->next       = NULL;
    return upval;
}

SK_INTERNAL(force_inline void) ObjUpvalue_free(VM* vm, ObjUpvalue* upval)
{
    GC_FREE(vm, upval, sizeof(ObjUpvalue));
}

ObjClass* ObjClass_new(VM* vm, ObjString* name)
{
    ObjClass* cclass = ALLOC_OBJ(vm, ObjClass, OBJ_CLASS); // GC
    cclass->name     = name;
    HashTable_init(&cclass->methods);
    cclass->overloaded = NULL;
    return cclass;
}

SK_INTERNAL(force_inline void) ObjClass_free(VM* vm, ObjClass* cclass)
{
    HashTable_free(vm, &cclass->methods);
    GC_FREE(vm, cclass, sizeof(ObjClass));
}


ObjInstance* ObjInstance_new(VM* vm, ObjClass* cclass)
{
    ObjInstance* instance = ALLOC_OBJ(vm, ObjInstance, OBJ_INSTANCE);
    instance->cclass      = cclass;
    HashTable_init(&instance->fields);
    return instance;
}

SK_INTERNAL(force_inline void)
ObjInstance_free(VM* vm, ObjInstance* instance)
{
    HashTable_free(vm, &instance->fields);
    GC_FREE(vm, instance, sizeof(ObjInstance));
}

ObjBoundMethod* ObjBoundMethod_new(VM* vm, Value receiver, Obj* method)
{
    ObjBoundMethod* bound_method = ALLOC_OBJ(vm, ObjBoundMethod, OBJ_BOUND_METHOD);
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
#ifdef SK_PRECOMPUTED_GOTO
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
ObjBoundMethod_free(VM* vm, ObjBoundMethod* bound_method)
{
    GC_FREE(vm, bound_method, sizeof(ObjBoundMethod));
}

void Obj_free(VM* vm, Obj* object)
{
#ifdef DEBUG_LOG_GC
    printf("%p free type ", (void*)object);
    ObjType_print(Obj_type(object));
    printf("\n");
#endif

#ifdef SK_PRECOMPUTED_GOTO
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
            ObjString_free(vm, (ObjString*)object);
            BREAK;
        }
        CASE(OBJ_FUNCTION)
        {
            ObjFunction_free(vm, (ObjFunction*)object);
            BREAK;
        }
        CASE(OBJ_CLOSURE)
        {
            ObjClosure_free(vm, (ObjClosure*)object);
            BREAK;
        }
        CASE(OBJ_NATIVE)
        {
            ObjNative_free(vm, (ObjNative*)object);
            BREAK;
        }
        CASE(OBJ_UPVAL)
        {
            ObjUpvalue_free(vm, (ObjUpvalue*)object);
            BREAK;
        }
        CASE(OBJ_CLASS)
        {
            ObjClass_free(vm, (ObjClass*)object);
            BREAK;
        }
        CASE(OBJ_INSTANCE)
        {
            ObjInstance_free(vm, (ObjInstance*)object);
            BREAK;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            ObjBoundMethod_free(vm, (ObjBoundMethod*)object);
            BREAK;
        }
    }

    unreachable;

#ifdef __SKOOMA_JMPTABLE_H__
    #undef __SKOOMA_JMPTABLE_H__
#endif
}

ObjString* Obj_to_str(VM* vm, Obj* object)
{
#ifdef SK_PRECOMPUTED_GOTO
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
            return Value_to_str(vm, ((ObjUpvalue*)object)->closed);
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

            return ObjString_from(vm, buff, arrlen - 1);
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
