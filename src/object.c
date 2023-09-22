#include "mem.h"
#include "object.h"
#include "skconf.h"

#include <stdio.h>
#include <stdlib.h>

#define ALLOC_OBJ(vm, object, type) ((object*)Object_new((vm), sizeof(object), type))

#define ALLOC_STRING(vm, len)                                                            \
    ((ObjString*)Object_new((vm), sizeof(ObjString) + (len) + 1, OBJ_STRING))

SK_INTERNAL(force_inline void) ObjString_free(ObjString* objstr);
SK_INTERNAL(force_inline void) ObjFunction_free(ObjFunction* objfn);
SK_INTERNAL(force_inline void) ObjNative_free(ObjNative* native);

SK_INTERNAL(force_inline Obj*) Object_new(VM* vm, size_t size, ObjType type)
{
    /* Allocate a new object */
    Obj* object  = MALLOC(size);
    object->type = type;
    object->next = NULL;

    /* Add the object to the GC list */
    object->next = vm->objects;
    vm->objects  = object;

    return object;
}

void Object_print(Value value)
{
    switch(OBJ_TYPE(value)) {
        case OBJ_STRING:
            printf("%s", AS_CSTRING(value));
            break;
        case OBJ_FUNCTION:
            if(AS_FUNCTION(value)->name == NULL) {
                printf("<script>");
            } else {
                printf("<fn %s>", AS_FUNCTION(value)->name->storage);
            }
            break;
        case OBJ_NATIVE:
            printf("<native fn>");
            break;
        default:
            unreachable;
    }
}

void Obj_free(Obj* object)
{
    switch(object->type) {
        case OBJ_STRING:
            ObjString_free((ObjString*)object);
            break;
        case OBJ_FUNCTION:
            ObjFunction_free((ObjFunction*)object);
            break;
        case OBJ_NATIVE:
            ObjNative_free((ObjNative*)object);
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

    if(interned) {
        /* Return interned string */
        return interned;
    }

    ObjString* string = ObjString_alloc(vm, len);
    memcpy(string->storage, chars, len);
    string->hash         = hash;
    string->storage[len] = '\0';

    HashTable_insert(&vm->strings, OBJ_VAL(string), NIL_VAL);
    return string;
}

SK_INTERNAL(force_inline void) ObjString_free(ObjString* string)
{
    MFREE(string, sizeof(ObjString) + string->len + 1);
}

ObjNative* ObjNative_new(VM* vm, NativeFn fn)
{
    ObjNative* native = ALLOC_OBJ(vm, ObjNative, OBJ_NATIVE);
    //
    native->fn = fn;
    return native;
}

SK_INTERNAL(force_inline void) ObjNative_free(ObjNative* native)
{
    MFREE(native, sizeof(ObjNative));
}

ObjFunction* ObjFunction_new(VM* vm)
{
    ObjFunction* fn = ALLOC_OBJ(vm, ObjFunction, OBJ_FUNCTION);
    fn->arity       = 0;
    fn->name        = NULL;
    Chunk_init(&fn->chunk);
    return fn;
}

SK_INTERNAL(force_inline void) ObjFunction_free(ObjFunction* fn)
{
    Chunk_free(&fn->chunk);
    MFREE(fn, sizeof(ObjFunction));
}
