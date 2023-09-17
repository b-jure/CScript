#include "mem.h"
#include "object.h"

#include <stdio.h>
#include <stdlib.h>

#define ALLOC_OBJ(vm, object, type) ((object*)Object_new((vm), sizeof(object), type))
#define ALLOC_STRING(vm, len)                                                            \
    ((ObjString*)Object_new((vm), sizeof(ObjString) + (len) + 1, OBJ_STRING))
#define ALLOC_FUNCTION(vm)                                                               \
    ((ObjFunction*)Object_new(vm, sizeof(ObjFunction), OBJ_FUNCTION))

static force_inline void ObjString_free(ObjString* objstr);
static force_inline void ObjFunction_free(ObjFunction* objfn);

static force_inline Obj* Object_new(VM* vm, size_t size, ObjType type)
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

static force_inline ObjString* ObjString_alloc(VM* vm, UInt len)
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

static force_inline void ObjString_free(ObjString* string)
{
    MFREE(string, sizeof(ObjString) + string->len + 1);
}

ObjFunction* ObjFunction_new(VM* vm)
{
    ObjFunction* fn = ALLOC_FUNCTION(vm);
    fn->arity       = 0;
    fn->name        = NULL;
    Chunk_init(&fn->chunk);
    return fn;
}

static force_inline void ObjFunction_free(ObjFunction* fn)
{
    Chunk_free(&fn->chunk);
    MFREE(fn, sizeof(ObjFunction));
}
