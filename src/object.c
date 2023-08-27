#include "mem.h"
#include "object.h"
#include "xxhash.h"

#include <stdio.h>
#include <stdlib.h>

#define ALLOC_OBJ(vm, object, type) ((object*)Object_new((vm), sizeof(object), type))
#define HASH_STRING(string)         hashstr((string)->storage, (string)->len)
#define ALLOC_STRING(vm, len)                                                            \
    ((ObjString*)Object_new((vm), sizeof(ObjString) + (len) + 1, OBJ_STRING))

static _force_inline void ObjString_free(ObjString* objstr);
static _force_inline UInt hashstr(const char* str, size_t len);

static _force_inline Obj* Object_new(VM* vm, size_t size, ObjType type)
{
    /* Allocate a new object */
    Obj* object = MALLOC(size);
    object->type = type;

    /* Add the object to the intrusive list */
    if(vm->objects != NULL) {
        vm->objects->next = vm->objects;
        vm->objects = object;
    } else {
        vm->objects = object;
    }

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

bool Object_eq(Value a, Value b)
{
    switch(OBJ_TYPE(a)) {
        case OBJ_STRING: {
            ObjString* a_str = AS_STRING(a);
            ObjString* b_str = AS_STRING(b);
            return a_str->len == b_str->len &&
                   memcmp(a_str->storage, b_str->storage, a_str->len) == 0;
            break;
        }
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

static _force_inline UInt hashstr(const char* str, size_t len)
{
    return XXH64(str, len, 31);
}

static _force_inline ObjString* ObjString_alloc(VM* vm, UInt len)
{
    ObjString* string = ALLOC_STRING(vm, len);
    string->len = len;
    return string;
}

ObjString* ObjString_from(VM* vm, const char* chars, size_t len)
{
    ObjString* string = ObjString_alloc(vm, len);
    memcpy(string->storage, chars, len);
    string->storage[len] = '\0';
    string->hash = HASH_STRING(string);
    return string;
}

ObjString* ObjString_from_concat(
    VM*         vm,
    const char* left,
    size_t      llen,
    const char* right,
    size_t      rlen)
{
    ObjString* string = ObjString_alloc(vm, llen + rlen);
    memcpy(string->storage, left, llen);
    memcpy(string->storage + llen, right, rlen);
    string->storage[string->len] = '\0';
    string->hash = HASH_STRING(string);
    return string;
}

static _force_inline void ObjString_free(ObjString* string)
{
    MFREE(string, sizeof(ObjString) + string->len + 1);
}
