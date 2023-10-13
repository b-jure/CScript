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
double gc_grow_factor = 0;

// Make extern for 'mark_c_roots' function in 'compiler.c'.
void mark_obj(VM* vm, Obj* obj)
{
    if(obj == NULL || Obj_marked(obj)) {
        return;
    }

    Obj_mark_set(obj, true);
    if(Obj_type(obj) & (OBJ_STRING | OBJ_NATIVE)) {
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

SK_INTERNAL(force_inline void) mark_globals(VM* vm)
{
    for(UInt i = 0; i < vm->global_ids.cap; i++) {
        Entry* entry = &vm->global_ids.entries[i];

        if(entry->key.type != VAL_EMPTY) {
            // Mark identifier
            mark_obj(vm, AS_OBJ(entry->key));
            // Mark value
            UInt    idx = (UInt)AS_NUMBER(entry->value);
            Global* val = Array_Global_index(&vm->global_vals, idx);
            mark_value(vm, val->value);
            // Mark global
            GLOB_SET(val, GLOB_MARKED_BIT);
        }
    }

    // Remove unmarked global values before their
    // identifiers get removed.
    for(UInt i = 0; i < vm->global_vals.len; i++) {
        Global* glob = Array_Global_index(&vm->global_vals, i);
        if(!GLOB_CHECK(glob, GLOB_MARKED_BIT)) {
            Array_Global_remove(&vm->global_vals, i);
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
            for(UInt i = 0; i < fn->chunk.constants.len; i++) {
                mark_value(vm, fn->chunk.constants.data[i]);
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

SK_INTERNAL(force_inline void) sweep(Roots* roots)
{
    Obj* previous = NULL;
    Obj* current  = roots->vm->objects;

    while(current != NULL) {
        if(Obj_marked(current)) {
            Obj_mark_set(current, false);
            previous = current;
            current  = Obj_next(current);
        } else {
            Obj* unreached = current;
            current        = Obj_next(current);
            if(previous != NULL) {
                Obj_next_set(previous, current);
            } else {
                roots->vm->objects = current;
            }

            Obj_free(roots, unreached);
        }
    }
}

void gc(Roots* roots)
{
#ifdef DEBUG_LOG_GC
    printf("--> GC start\n");
    size_t old_allocation = roots->vm->gc_allocated;
#endif

    Compiler* c  = roots->c;
    VM*       vm = roots->vm;

    mark_vm_roots(vm);
#ifdef THREADED_CODE
    static const void* jmptable[] = {&&mark, &&skip};

    goto* jmptable[runtime];
mark:
    mark_c_roots(vm, c);
skip:
#else
    mark_c_roots(vm, c);
#endif

    while(vm->gray_stack.len > 0) {
        mark_black(vm, Array_ObjRef_pop(&vm->gray_stack));
    }

    remove_weak_refs(vm);
    sweep(roots);

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
void* gc_reallocate(void* roots, void* ptr, size_t oldc, size_t newc)
{
    Roots* r             = roots;
    r->vm->gc_allocated += newc - oldc;

#ifdef DEBUG_STRESS_GC
    if(newc > oldc) {
        gc(r);
    }
#else
    if(!GC_CHECK(r->vm, GC_MANUAL_BIT) && r->vm->gc_next < r->vm->gc_allocated) {
        gc(r);
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
