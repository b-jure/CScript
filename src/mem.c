#include "compiler.h"
#include "debug.h"
#include "hashtable.h"
#include "mem.h"
#include "object.h"
#include "skconf.h"
#include "value.h"
#include "vmachine.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

// Generic function signature for Mark and Sweep functions
#define MANDS_FN(name) sstatic force_inline void name(VM* vm)

void mark_obj(VM* vm, Obj* obj)
{
    if(obj == NULL || Obj_marked(obj)) {
        return;
    }

    Obj_mark_set(obj, true);
    ASSERT(Obj_marked(obj), "Object not marked right after call to Obj_mark_set().");
    if(Obj_type(obj) == OBJ_STRING) {
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

    GSARRAY_PUSH(vm, obj);
}

// helper
sstatic force_inline void mark_table(VM* vm, HashTable* table)
{
    for(UInt i = 0; i < table->cap; i++) {
        Entry* entry = &table->entries[i];
        if(!IS_EMPTY(entry->key)) {
            mark_value(vm, entry->key);
            mark_value(vm, entry->value);
        }
    }
}

MANDS_FN(mark_globals)
{
    for(UInt i = 0; i < vm->globids.cap; i++) {
        Entry* entry = &vm->globids.entries[i];

        if(!IS_EMPTY(entry->key)) {
            // Mark identifier (ObjString)
            mark_obj(vm, AS_OBJ(entry->key));
            // Mark value
            UInt idx = (UInt)AS_NUMBER(entry->value);
            mark_value(vm, vm->globvals[idx].value);
        }
    }
}

MANDS_FN(mark_stack)
{
    for(Value* local = vm->stack; local < vm->sp; local++) {
        mark_value(vm, *local);
    }
}

MANDS_FN(mark_frames)
{
    for(Int i = 0; i < vm->fc; i++) {
        mark_obj(vm, vm->frames[i].fn);
    }
}

MANDS_FN(mark_upvalues)
{
    for(ObjUpvalue* upval = vm->open_upvals; upval != NULL; upval = upval->next) {
        mark_obj(vm, (Obj*)upval);
    }
}

MANDS_FN(mark_statics)
{
    for(UInt i = 0; i < SS_SIZE; i++) {
        mark_obj(vm, (Obj*)vm->statics[i]);
    }
}

MANDS_FN(mark_loaded)
{
    mark_table(vm, &vm->loaded);
}

MANDS_FN(mark_temp)
{
    for(UInt i = 0; i < vm->tempc; i++) {
        mark_value(vm, vm->temp[i]);
    }
}

MANDS_FN(mark_vm_roots)
{
    mark_stack(vm);
    mark_frames(vm);
    mark_upvalues(vm);
    mark_globals(vm);
    mark_statics(vm);
    mark_loaded(vm);
    mark_temp(vm);
    mark_value(vm, vm->script); // @? Remove
}

MANDS_FN(remove_weak_refs)
{
    for(UInt i = 0; i < vm->strings.cap; i++) {
        Entry* entry = &vm->strings.entries[i];
        if(IS_OBJ(entry->key) && !Obj_marked(AS_OBJ(entry->key))) {
            HashTable_remove(&vm->strings, entry->key);
        }
    }
}

MANDS_FN(sweep)
{
    Obj* previous = NULL;
    Obj* current  = vm->objects;

    while(current != NULL) {
        if(Obj_marked(current)) {
            Obj_mark_set(current, false);
            ASSERT(Obj_marked(current) == false, "Object remained marked after unmarking.");
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

            Obj_free(vm, unreached);
        }
    }
}

void mark_black(VM* vm, Obj* obj)
{
#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void*)obj);
    Value_print(OBJ_VAL(obj));
    printf(" header: 0x%08lx\n", obj->header);
#endif

#ifdef S_PRECOMPUTED_GOTO
    #define OBJ_TABLE
    #include "jmptable.h"
    #undef OBJ_TABLE
#else
    #define DISPATCH(x) switch(x)
    #define CASE(label) case label:
    #define BREAK       break
#endif

    ASSERT(Obj_marked(obj), "Object in mark_black() must be marked.");

    DISPATCH(Obj_type(obj))
    {
        CASE(OBJ_UPVAL)
        {
            mark_value(vm, ((ObjUpvalue*)obj)->closed);
            BREAK;
        }
        CASE(OBJ_FUNCTION)
        {
            ObjFunction* fn = (ObjFunction*)obj;
            mark_obj(vm, (Obj*)fn->name);
            for(UInt i = 0; i < fn->chunk.clen; i++) {
                mark_value(vm, fn->chunk.constants[i]);
            }
            BREAK;
        }
        CASE(OBJ_CLOSURE)
        {
            ObjClosure* closure = (ObjClosure*)obj;
            mark_obj(vm, (Obj*)closure->fn);
            for(UInt i = 0; i < closure->upvalc; i++) {
                mark_obj(vm, (Obj*)closure->upvals[i]);
            }
            BREAK;
        }
        CASE(OBJ_CLASS)
        {
            ObjClass* cclass = (ObjClass*)obj;
            mark_obj(vm, (Obj*)cclass->name);
            mark_table(vm, &cclass->methods);
            mark_obj(vm, (Obj*)cclass->overloaded);
            BREAK;
        }
        CASE(OBJ_INSTANCE)
        {
            ObjInstance* instance = (ObjInstance*)obj;
            mark_obj(vm, (Obj*)instance->cclass);
            mark_table(vm, &instance->fields);
            BREAK;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            ObjBoundMethod* bound_method = (ObjBoundMethod*)obj;
            mark_obj(vm, (Obj*)bound_method);
            mark_value(vm, bound_method->receiver);
            mark_obj(vm, bound_method->method);
            BREAK;
        }
        CASE(OBJ_NATIVE)
        {
            ObjNative* native = (ObjNative*)obj;
            mark_obj(vm, (Obj*)native->name);
            BREAK;
        }
        CASE(OBJ_STRING)
        unreachable;
    }
}

size_t gc(VM* vm)
{
#ifdef DEBUG_LOG_GC
    printf("--> GC start\n");
#endif

    size_t old_allocation = vm->gc_allocated;
    mark_vm_roots(vm);

#ifdef S_PRECOMPUTED_GOTO
    static const void* jmptable[] = {&&mark, &&skip};

    goto* jmptable[runtime];
mark:
    mark_c_roots(vm);
skip:
#else
    mark_c_roots(vm);
#endif

    while(vm->gslen > 0) {
        mark_black(vm, GSARRAY_POP(vm));
    }

    remove_weak_refs(vm);
    sweep(vm);

    vm->gc_next =
        MAX((double)vm->gc_allocated * vm->config.gc_grow_factor, vm->config.gc_min_heap_size);

#ifdef DEBUG_LOG_GC
    printf("--> GC end\n");
    printf(
        "    collected %lu bytes (from %lu to %lu) next collection at %lu\n",
        old_allocation - vm->gc_allocated,
        old_allocation,
        vm->gc_allocated,
        (size_t)vm->gc_next);
#endif

    return old_allocation - vm->gc_allocated;
}

/* Allocator that can trigger gc. */
void* gc_reallocate(VM* vm, void* ptr, ssize_t oldc, ssize_t newc)
{
    vm->gc_allocated += newc - oldc;
    ASSERT(newc > oldc, "Tried freeing memory with gc_reallocate() (or zero sized allocation).");

#ifdef DEBUG_STRESS_GC
    if(newc > oldc) {
        gc(vm);
    }
#else

    if(!GC_CHECK(vm, GC_MANUAL_BIT) && vm->gc_next < vm->gc_allocated) {
        gc(vm);
    }

#endif

    return REALLOC(vm, ptr, newc);
}

/* Freeing memory never triggers GC */
void* gc_free(VM* vm, void* ptr, ssize_t oldc, ssize_t newc)
{
    vm->gc_allocated += newc - oldc;
    ASSERT(newc <= oldc, "Tried allocating memory with gc_free().");
    return REALLOC(vm, ptr, newc);
}

/* Allocator that never triggers gc. */
void* reallocate(void* ptr, size_t newc, void* _)
{
    UNUSED(_);

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
