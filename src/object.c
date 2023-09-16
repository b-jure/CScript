#include "mem.h"
#include "object.h"

#include <stdio.h>
#include <stdlib.h>

#define ALLOC_OBJ(vm, object, type) ((object*)Object_new((vm), sizeof(object), type))
#define ALLOC_STRING(vm, len)                                                            \
    ((ObjString*)Object_new((vm), sizeof(ObjString) + (len) + 1, OBJ_STRING))

static _force_inline void ObjString_free(ObjString* objstr);

static _force_inline Obj* Object_new(VM* vm, size_t size, ObjType type)
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
            printf("%.*s", (int)AS_STRING(value)->len, AS_CSTRING(value));
            break;
        default:
            _unreachable;
    }
}

void Obj_free(Obj* object)
{
    switch(object->type) {
        case OBJ_STRING:
            ObjString_free((ObjString*)object);
            break;
        default:
            _unreachable;
    }
}

Hash Obj_hash(Value value)
{
    switch(OBJ_TYPE(value)) {
        case OBJ_STRING:
            return AS_STRING(value)->hash;
        default:
            _unreachable;
    }
}

static _force_inline ObjString* ObjString_alloc(VM* vm, UInt len)
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

static _force_inline void ObjString_free(ObjString* string)
{
    MFREE(string, sizeof(ObjString) + string->len + 1);
}
