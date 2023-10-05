#include "compiler.h"
#include "hashtable.h"
#include "mem.h"
#include "object.h"
#include "skconf.h"
#include "value.h"

#ifdef DEBUG_LOG_GC
    #include "debug.h"
    #include <stdio.h>
#endif

#include <errno.h>
#include <stdlib.h>

SK_INTERNAL(force_inline void) mark_globals(HashTable* ids, Array_Global* vals)
{
    for(UInt i = 0; i < ids->cap; i++) {
        Entry* entry = &ids->entries[i];

        if(entry->key.type == VAL_OBJ) {
            Obj_mark(AS_OBJ(entry->key));
            Value_mark(vals->data[(UInt)AS_NUMBER(entry->value)].value);
        }
    }
}

SK_INTERNAL(force_inline void) mark_stack(Value* stack, Value* top)
{
    for(Value* local = stack; local < top; local++) {
        Value_mark(*local);
    }
}

SK_INTERNAL(force_inline void) mark_frames(CallFrame* frames, UInt fc)
{
    for(UInt i = 0; i < fc; i++) {
        Obj_mark((Obj*)frames[i].closure);
    }
}

SK_INTERNAL(force_inline void) mark_upvalues(ObjUpvalue* upvalues)
{
    for(ObjUpvalue* upval = upvalues; upval != NULL; upval = upval->next) {
        Obj_mark((Obj*)upval);
    }
}

SK_INTERNAL(force_inline void) gc_vm(VM* vm)
{
#ifdef DEBUG_LOG_GC
    printf("--> GC start\n");
#endif
    mark_stack(vm->stack, vm->sp);
    mark_frames(vm->frames, vm->fc);
    mark_upvalues(vm->open_upvals);
    mark_globals(&vm->global_ids, &vm->global_vals);
#ifdef DEBUG_LOG_GC
    printf("--> GC end\n");
#endif
}

SK_INTERNAL(force_inline void) gc_c(Compiler* C)
{
#ifdef DEBUG_LOG_GC
    printf("--> GC start\n");
#endif
    mark_c_roots(C);
#ifdef DEBUG_LOG_GC
    printf("--> GC end\n");
#endif
}

void* vm_reallocate(unused void* vm, void* ptr, size_t oldc, size_t newc)
{
    if(newc > oldc) {
#ifdef DEBUG_STRESS_GC
        gc_vm((VM*)vm);
#endif
    }
    return reallocate(ptr, newc);
}

void* c_reallocate(unused Compiler* C, void* ptr, size_t oldc, size_t newc)
{
    if(newc > oldc) {
#ifdef DEBUG_STRESS_GC
        gc_c(C);
#endif
    }
    return reallocate(ptr, newc);
}

void* reallocate(void* ptr, size_t newc)
{
    if(newc == 0) {
        free(ptr);
        return NULL;
    }

    void* alloc = realloc(ptr, newc);

    if(alloc == NULL) {
        exit(errno);
    }

    return alloc;
}
