#include "array.h"
#include "object.h"
#include "skconf.h"
#include "value.h"
#ifdef DEBUG
    #include "debug.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#define ALLOC_OBJ(roots, object, type) ((object*)Object_new(roots, sizeof(object), type))

#define ALLOC_STRING(roots, len)                                                         \
    ((ObjString*)Object_new(roots, sizeof(ObjString) + (len) + 1, OBJ_STRING))

SK_INTERNAL(force_inline void) ObjClosure_free(Roots* roots, ObjClosure* objclosure);
SK_INTERNAL(force_inline void) ObjString_free(Roots* roots, ObjString* objstr);
SK_INTERNAL(force_inline void) ObjFunction_free(Roots* roots, ObjFunction* objfn);
SK_INTERNAL(force_inline void) ObjNative_free(Roots* roots, ObjNative* native);
SK_INTERNAL(force_inline void) ObjUpvalue_free(Roots* roots, ObjUpvalue* upval);

SK_INTERNAL(force_inline Obj*) Object_new(Roots* roots, size_t size, ObjType type)
{
    Obj* object   = GC_MALLOC(roots, size);
    object->otype = type;

    /* Add the object to the GC list */
    object->next       = roots->vm->objects;
    roots->vm->objects = object;

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
    if(type & 1) { // If marked
        type ^= 1;
    }

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
        default:
            unreachable;
    }
}

void Obj_free(Roots* roots, Obj* object)
{
#ifdef DEBUG_LOG_GC
    printf("%p free type ", (void*)object);
    ObjType_print(object->otype);
    printf("\n");
#endif
    switch(object->otype & ~1) {
        case OBJ_STRING:
            ObjString_free(roots, (ObjString*)object);
            break;
        case OBJ_FUNCTION:
            ObjFunction_free(roots, (ObjFunction*)object);
            break;
        case OBJ_CLOSURE:
            ObjClosure_free(roots, (ObjClosure*)object);
            break;
        case OBJ_NATIVE:
            ObjNative_free(roots, (ObjNative*)object);
            break;
        case OBJ_UPVAL:
            ObjUpvalue_free(roots, (ObjUpvalue*)object);
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

SK_INTERNAL(force_inline ObjString*) ObjString_alloc(Roots* roots, UInt len)
{
    ObjString* string = ALLOC_STRING(roots, len);
    string->len       = len;
    return string;
}

ObjString* ObjString_from(Roots* roots, const char* chars, size_t len)
{
    uint64_t   hash     = Hash_string(chars, len);
    ObjString* interned = HashTable_get_intern(&roots->vm->strings, chars, len, hash);

    if(interned) { // Return interned string
        return interned;
    }

    ObjString* string = ObjString_alloc(roots, len);
    memcpy(string->storage, chars, len);
    string->hash         = hash;
    string->storage[len] = '\0';

    VM_push(roots->vm, OBJ_VAL(string)); // GC
    HashTable_insert(&roots->vm->strings, OBJ_VAL(string), NIL_VAL);
    VM_pop(roots->vm); // GC
    return string;
}

SK_INTERNAL(force_inline void) ObjString_free(Roots* roots, ObjString* string)
{
    GC_FREE(roots, string, sizeof(ObjString) + string->len + 1);
}

ObjNative* ObjNative_new(Roots* roots, NativeFn fn, UInt arity)
{
    ObjNative* native = ALLOC_OBJ(roots, ObjNative, OBJ_NATIVE);
    //
    native->fn    = fn;
    native->arity = arity;
    return native;
}

SK_INTERNAL(force_inline void) ObjNative_free(Roots* roots, ObjNative* native)
{
    GC_FREE(roots, native, sizeof(ObjNative));
}

ObjFunction* ObjFunction_new(Roots* roots)
{
    ObjFunction* fn = ALLOC_OBJ(roots, ObjFunction, OBJ_FUNCTION);
    fn->arity       = 0;
    fn->name        = NULL;
    fn->upvalc      = 0;
    Chunk_init(&fn->chunk, (void*)roots);
    return fn;
}

SK_INTERNAL(force_inline void) ObjFunction_free(Roots* roots, ObjFunction* fn)
{
    Chunk_free(&fn->chunk);
    GC_FREE(roots, fn, sizeof(ObjFunction));
}

ObjClosure* ObjClosure_new(Roots* roots, ObjFunction* fn)
{
    ObjUpvalue** upvals = GC_MALLOC(roots, sizeof(ObjUpvalue*) * fn->upvalc);
    for(UInt i = 0; i < fn->upvalc; i++) {
        upvals[i] = NULL;
    }

    ObjClosure* closure = ALLOC_OBJ(roots, ObjClosure, OBJ_CLOSURE);
    closure->fn         = fn;
    closure->upvals     = upvals;
    closure->upvalc     = fn->upvalc;

    return closure;
}

SK_INTERNAL(force_inline void) ObjClosure_free(Roots* roots, ObjClosure* closure)
{
    GC_FREE(roots, closure->upvals, closure->upvalc * sizeof(ObjUpvalue*));
    GC_FREE(roots, closure, sizeof(ObjClosure));
}

ObjUpvalue* ObjUpvalue_new(Roots* roots, Value* var_ref)
{
    ObjUpvalue* upval = ALLOC_OBJ(roots, ObjUpvalue, OBJ_UPVAL);
    upval->closed     = EMPTY_VAL;
    upval->location   = var_ref;
    upval->next       = NULL;
    return upval;
}

SK_INTERNAL(force_inline void) ObjUpvalue_free(Roots* roots, ObjUpvalue* upval)
{
    GC_FREE(roots, upval, sizeof(ObjUpvalue));
}
