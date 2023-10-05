#include "array.h"
#include "mem.h"
#include "object.h"
#include "skconf.h"
#include "value.h"

#include <stdio.h>
#include <stdlib.h>

#define ALLOC_OBJ(vm, object, type) ((object*)Object_new((vm), sizeof(object), type))

#define ALLOC_STRING(vm, len)                                                            \
    ((ObjString*)Object_new((vm), sizeof(ObjString) + (len) + 1, OBJ_STRING))

SK_INTERNAL(force_inline void) ObjClosure_free(VM* vm, ObjClosure* objclosure);
SK_INTERNAL(force_inline void) ObjString_free(VM* vm, ObjString* objstr);
SK_INTERNAL(force_inline void) ObjFunction_free(VM* vm, ObjFunction* objfn);
SK_INTERNAL(force_inline void) ObjNative_free(VM* vm, ObjNative* native);
SK_INTERNAL(force_inline void) ObjUpvalue_free(VM* vm, ObjUpvalue* upval);

SK_INTERNAL(force_inline Obj*) Object_new(VM* vm, size_t size, ObjType type)
{
    /* Allocate a new object */
    Obj* object  = VM_MALLOC(vm, size);
    object->type = type;
    object->next = NULL;

    /* Add the object to the GC list */
    object->next = vm->objects;
    vm->objects  = object;

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

#define OBJ_IS_MARKED(obj) ((uint32_t)obj->type & (uint32_t)1)
#define OBJ_MARK(obj)      (obj->type |= (uint32_t)1)

void Obj_mark(Obj* obj)
{
    if(obj == NULL || OBJ_IS_MARKED(obj)) {
        return;
    }

#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void*)obj);
    Value_print(OBJ_VAL(obj));
    printf("\n");
#endif

    if(obj->type & (OBJ_STRING | OBJ_UPVAL)) {
        return;
    }

    OBJ_MARK(obj);
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

void Obj_free(VM* vm, Obj* object)
{
#ifdef DEBUG_LOG_GC
    printf("%p free type ", (void*)object);
    ObjType_print(object->type);
    printf("\n");
#endif
    switch(object->type) {
        case OBJ_STRING:
            ObjString_free(vm, (ObjString*)object);
            break;
        case OBJ_FUNCTION:
            ObjFunction_free(vm, (ObjFunction*)object);
            break;
        case OBJ_CLOSURE:
            ObjClosure_free(vm, (ObjClosure*)object);
            break;
        case OBJ_NATIVE:
            ObjNative_free(vm, (ObjNative*)object);
            break;
        case OBJ_UPVAL:
            ObjUpvalue_free(vm, (ObjUpvalue*)object);
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

SK_INTERNAL(force_inline void) ObjString_free(VM* vm, ObjString* string)
{
    VM_FREE(vm, string, sizeof(ObjString) + string->len + 1);
}

ObjNative* ObjNative_new(VM* vm, NativeFn fn, UInt arity)
{
    ObjNative* native = ALLOC_OBJ(vm, ObjNative, OBJ_NATIVE);
    //
    native->fn    = fn;
    native->arity = arity;
    return native;
}

SK_INTERNAL(force_inline void) ObjNative_free(VM* vm, ObjNative* native)
{
    VM_FREE(vm, native, sizeof(ObjNative));
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
    Chunk_free(&fn->chunk);
    VM_FREE(vm, fn, sizeof(ObjFunction));
}

ObjClosure* ObjClosure_new(VM* vm, ObjFunction* fn)
{
    ObjUpvalue** upvals = VM_MALLOC(vm, sizeof(ObjUpvalue*) * fn->upvalc);
    for(UInt i = 0; i < fn->upvalc; i++) {
        upvals[i] = NULL;
    }

    ObjClosure* closure = ALLOC_OBJ(vm, ObjClosure, OBJ_CLOSURE);
    closure->fn         = fn;
    closure->upvals     = upvals;

    return closure;
}

SK_INTERNAL(force_inline void) ObjClosure_free(VM* vm, ObjClosure* closure)
{
    VM_FREE(vm, closure->upvals, closure->fn->upvalc * sizeof(ObjUpvalue*));
    VM_FREE(vm, closure, sizeof(ObjClosure));
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
    VM_FREE(vm, upval, sizeof(ObjUpvalue));
}
