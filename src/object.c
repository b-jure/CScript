#include "array.h"
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

SK_INTERNAL(force_inline void)
ObjClosure_free(VM* vm, Compiler* C, ObjClosure* objclosure);
SK_INTERNAL(force_inline void) ObjString_free(VM* vm, Compiler* C, ObjString* objstr);
SK_INTERNAL(force_inline void) ObjFunction_free(VM* vm, Compiler* C, ObjFunction* objfn);
SK_INTERNAL(force_inline void) ObjNative_free(VM* vm, Compiler* C, ObjNative* native);
SK_INTERNAL(force_inline void) ObjUpvalue_free(VM* vm, Compiler* C, ObjUpvalue* upval);
SK_INTERNAL(force_inline void) ObjClass_free(VM* vm, Compiler* C, ObjClass* cclass);
SK_INTERNAL(force_inline void) ObjInstance_free(VM* vm, Compiler* C, ObjInstance* object);

SK_INTERNAL(force_inline Obj*) Obj_new(VM* vm, Compiler* C, size_t size, ObjType type)
{
    Obj* object = GC_MALLOC(vm, C, size);

    object->header = (uint64_t)vm->objects | ((uint64_t)type << 56);

    // Malloc and C standard do not guarantee that upper 16 bits
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

SK_INTERNAL(force_inline void) print_fn(ObjFunction* fn)
{
    if(fn->name == NULL) {
        printf("<script>");
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
        default:
            unreachable;
    }
}

void Object_print(Value value)
{
    switch(OBJ_TYPE(value)) {
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
        case OBJ_FUNCTION:
            print_fn(AS_FUNCTION(value));
            break;
        case OBJ_CLOSURE:
            print_fn(AS_CLOSURE(value)->fn);
            break;
        case OBJ_NATIVE:
            printf("<native fn>");
            break;
        case OBJ_UPVAL:
            printf("upvalue");
            break;
        case OBJ_CLASS:
            printf("%s class", AS_CLASS(value)->name->storage);
            break;
        case OBJ_INSTANCE:
            printf("%s instance", AS_INSTANCE(value)->cclass->name->storage);
            break;
        default:
            unreachable;
    }
}

void Obj_free(VM* vm, Compiler* C, Obj* object)
{
#ifdef DEBUG_LOG_GC
    printf("%p free type ", (void*)object);
    ObjType_print(Obj_type(object));
    printf("\n");
#endif
    switch(Obj_type(object)) {
        case OBJ_STRING:
            ObjString_free(vm, C, (ObjString*)object);
            break;
        case OBJ_FUNCTION:
            ObjFunction_free(vm, C, (ObjFunction*)object);
            break;
        case OBJ_CLOSURE:
            ObjClosure_free(vm, C, (ObjClosure*)object);
            break;
        case OBJ_NATIVE:
            ObjNative_free(vm, C, (ObjNative*)object);
            break;
        case OBJ_UPVAL:
            ObjUpvalue_free(vm, C, (ObjUpvalue*)object);
            break;
        case OBJ_CLASS:
            ObjClass_free(vm, C, (ObjClass*)object);
            break;
        case OBJ_INSTANCE:
            ObjInstance_free(vm, C, (ObjInstance*)object);
            break;
        default:
            unreachable;
    }
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
    string->hash         = hash;
    string->storage[len] = '\0';

    VM_push(vm, OBJ_VAL(string)); // GC
    HashTable_insert(vm, C, &vm->strings, OBJ_VAL(string), NIL_VAL);
    VM_pop(vm); // GC
    return string;
}

SK_INTERNAL(force_inline void) ObjString_free(VM* vm, Compiler* C, ObjString* string)
{
    GC_FREE(vm, C, string, sizeof(ObjString) + string->len + 1);
}

ObjNative* ObjNative_new(VM* vm, Compiler* C, NativeFn fn, Int arity)
{
    ObjNative* native = ALLOC_OBJ(vm, C, ObjNative, OBJ_NATIVE);
    //
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
    ObjClass* cclass = ALLOC_OBJ(vm, C, ObjClass, OBJ_CLASS);
    cclass->name     = name;
    return cclass;
}

SK_INTERNAL(force_inline void) ObjClass_free(VM* vm, Compiler* C, ObjClass* cclass)
{
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
