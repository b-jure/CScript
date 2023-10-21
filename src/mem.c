#include "compiler.h"
#include "hashtable.h"
#include "mem.h"
#include "object.h"
#include "skconf.h"
#include "value.h"
#include "vmachine.h"
#ifdef DEBUG
    #include "debug.h"
#endif
#ifdef DEBUG_LOG_GC
    #include "debug.h"
    #include <stdio.h>
#endif

#include <errno.h>
#include <stdlib.h>

#define GC_HEAP_GROW_FACTOR 2
double gc_grow_factor = 0; // @TODO: Use this in native GC interface

void mark_obj(VM* vm, Obj* obj)
{
    if(obj == NULL || Obj_marked(obj)) {
        return;
    }

    Obj_mark_set(obj, true);
    if(Obj_type(obj) == OBJ_STRING || Obj_type(obj) == OBJ_NATIVE) {
#ifdef DEBUG_LOG_GC
        printf("%p blacken ", (void*)obj);
        Value_print(OBJ_VAL(obj));
        printf("\n");
#endif
        return;
    }

#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void*)obj);
    Value_print(OBJ_VAL(obj));
    printf("\n");
#endif
    Array_ObjRef_push(&vm->gray_stack, obj);
}

SK_INTERNAL(force_inline void) mark_table(VM* vm, HashTable* table)
{
    for(UInt i = 0; i < table->cap; i++) {
        Entry* entry = &table->entries[i];
        if(entry->key.type != VAL_EMPTY) {
            mark_value(vm, entry->key);
            mark_value(vm, entry->value);
        }
    }
}

SK_INTERNAL(force_inline void) mark_globals(VM* vm)
{
    for(UInt i = 0; i < vm->globids.cap; i++) {
        Entry* entry = &vm->globids.entries[i];

        if(entry->key.type != VAL_EMPTY) {
            // Mark identifier (ObjString)
            mark_obj(vm, AS_OBJ(entry->key));
            // Mark value
            UInt idx = (UInt)AS_NUMBER(entry->value);
            mark_value(vm, vm->globvals[idx].value);
        }
    }
}

SK_INTERNAL(force_inline void) mark_stack(VM* vm)
{
    for(Value* local = vm->stack; local < vm->sp; local++) {
        mark_value(vm, *local);
    }
}

SK_INTERNAL(force_inline void) mark_frames(VM* vm)
{
    for(Int i = 0; i < vm->fc; i++) {
        if(vm->frames[i].closure == NULL) {
            mark_obj(vm, (Obj*)vm->frames[i].fn);
        } else {
            mark_obj(vm, (Obj*)vm->frames[i].closure);
        }
    }
}

SK_INTERNAL(force_inline void) mark_upvalues(VM* vm)
{
    for(ObjUpvalue* upval = vm->open_upvals; upval != NULL; upval = upval->next) {
        mark_obj(vm, (Obj*)upval);
    }
}

SK_INTERNAL(force_inline void) mark_black(VM* vm, Obj* obj)
{
#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void*)obj);
    Value_print(OBJ_VAL(obj));
    printf("\n");
#endif
    switch(Obj_type(obj)) {
        case OBJ_UPVAL:
            mark_value(vm, ((ObjUpvalue*)obj)->closed);
            break;
        case OBJ_FUNCTION: {
            ObjFunction* fn = (ObjFunction*)obj;
            mark_obj(vm, (Obj*)fn->name);
            for(UInt i = 0; i < fn->chunk.clen; i++) {
                mark_value(vm, fn->chunk.constants[i]);
            }
            break;
        }
        case OBJ_CLOSURE: {
            ObjClosure* closure = (ObjClosure*)obj;
            mark_obj(vm, (Obj*)closure->fn);
            for(UInt i = 0; i < closure->upvalc; i++) {
                mark_obj(vm, (Obj*)closure->upvals[i]);
            }
            break;
        }
        case OBJ_CLASS: {
            ObjClass* cclass = (ObjClass*)obj;
            mark_obj(vm, (Obj*)cclass->name);
            mark_table(vm, &cclass->methods);
            break;
        }
        case OBJ_INSTANCE: {
            ObjInstance* instance = (ObjInstance*)obj;
            mark_obj(vm, (Obj*)instance->cclass);
            mark_table(vm, &instance->fields);
            break;
        }
        case OBJ_BOUND_METHOD: {
            ObjBoundMethod* bound_method = (ObjBoundMethod*)obj;
            mark_obj(vm, (Obj*)bound_method);
            mark_value(vm, bound_method->receiver);
            mark_obj(vm, bound_method->method);
            break;
        }
        default:
            unreachable;
    }
}

SK_INTERNAL(force_inline void) mark_vm_roots(VM* vm)
{
    mark_stack(vm);
    mark_frames(vm);
    mark_upvalues(vm);
    mark_globals(vm);
}

SK_INTERNAL(force_inline void) remove_weak_refs(VM* vm)
{
    for(UInt i = 0; i < vm->strings.cap; i++) {
        Entry* entry = &vm->strings.entries[i];
        if(entry->key.type == VAL_OBJ && !Obj_marked(AS_OBJ(entry->key))) {
            HashTable_remove(&vm->strings, entry->key);
        }
    }
}

SK_INTERNAL(force_inline void) sweep(VM* vm, Compiler* C)
{
    Obj* previous = NULL;
    Obj* current  = vm->objects;

    while(current != NULL) {
        if(Obj_marked(current)) {
            Obj_mark_set(current, false);
#ifdef DEBUG
            assert(Obj_marked(current) == false);
#endif
            previous = current;
            current  = Obj_next(current);
        } else {
            Obj* unreached = current;
            current        = Obj_next(current);
            if(previous != NULL) {
                Obj_next_set(previous, current);
            } else {
                vm->objects = current;
            }

            Obj_free(vm, C, unreached);
        }
    }
}

void gc(VM* vm, Compiler* C)
{
#ifdef DEBUG_LOG_GC
    printf("--> GC start\n");
    size_t old_allocation = vm->gc_allocated;
#endif

    mark_vm_roots(vm);
#ifdef THREADED_CODE
    static const void* jmptable[] = {&&mark, &&skip};

    goto* jmptable[runtime];
mark:
    mark_c_roots(vm, C);
skip:
#else
    mark_c_roots(vm, C);
#endif

    while(vm->gray_stack.len > 0) {
        mark_black(vm, Array_ObjRef_pop(&vm->gray_stack));
    }

    remove_weak_refs(vm);
    sweep(vm, C);

    // @TODO: Make use of gc_grow_factor global when GC class gets implemented
    vm->gc_next = (double)vm->gc_allocated *
                  (double)((gc_grow_factor == 0) ? GC_HEAP_GROW_FACTOR : gc_grow_factor);

#ifdef DEBUG_LOG_GC
    printf("--> GC end\n");
    printf(
        "    collected %lu bytes (from %lu to %lu) next collection at %g\n",
        old_allocation - vm->gc_allocated,
        old_allocation,
        vm->gc_allocated,
        vm->gc_next);
#endif
}

/* Allocator that can trigger gc. */
void* gc_reallocate(VM* vm, Compiler* C, void* ptr, size_t oldc, size_t newc)
{
    vm->gc_allocated += newc - oldc;

#ifdef DEBUG_STRESS_GC
    if(newc > oldc) {
        gc(vm, C);
    }
#else
    if(!GC_CHECK(vm, GC_MANUAL_BIT) * vm->gc_next < vm->gc_allocated) {
        gc(vm, C);
    }
#endif

    return reallocate(ptr, newc);
}

/* Allocator that never triggers gc. */
void* reallocate(void* ptr, size_t newc)
{
    if(newc == 0) {
        free(ptr);
        return NULL;
    }

    void* alloc = realloc(ptr, newc);

    if(alloc == NULL) {
        fprintf(stderr, "Internal error, allocation failure!\n");
        exit(errno);
    }

    return alloc;
}
