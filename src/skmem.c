/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Skooma.
 * Skooma is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Skooma is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Skooma.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include "skdebug.h"
#include "skerr.h"
#include "skhashtable.h"
#include "skmem.h"
#include "skobject.h"
#include "skparser.h"
#include "skconf.h"
#include "skvalue.h"
#include "skvm.h"

#include <stdio.h>
#include <stdlib.h>




void omark(VM* vm, O* obj)
{
    if(obj == NULL || oismarked(obj)) return;
    osetmark(obj, true);
    if(otype(obj) == OBJ_STRING) {
#ifdef DEBUG_LOG_GC
        printf("%p blacken ", (void*)obj);
        vprint(OBJ_VAL(obj));
        printf("\n");
#endif
        return;
    }
#ifdef DEBUG_LOG_GC
    printf("%p mark ", (void*)obj);
    vprint(OBJ_VAL(obj));
    printf("\n");
#endif
    GSARRAY_PUSH(vm, obj);
}


static force_inline void marktable(VM* vm, HashTable* table)
{
    for(uint32_t i = 0; i < table->cap; i++) {
        Entry* entry = &table->entries[i];
        if(!IS_EMPTY(entry->key)) {
            vmark(vm, entry->key);
            vmark(vm, entry->value);
        }
    }
}




// Generic function signature for Mark and Sweep functions
#define MS_FN(name) static force_inline void name(VM* vm)

MS_FN(markglobals)
{
    for(uint32_t i = 0; i < vm->globids.cap; i++) {
        Entry* entry = &vm->globids.entries[i];
        if(!IS_EMPTY(entry->key)) {
            // Mark identifier (ObjString)
            omark(vm, AS_OBJ(entry->key));
            // Mark value
            uint32_t idx = cast_uint(AS_NUMBER(entry->value));
            vmark(vm, vm->globvars.data[idx].value);
        }
    }
}

MS_FN(markstack)
{
    for(Value* local = vm->stack; local < vm->sp; local++)
        vmark(vm, *local);
}

MS_FN(markframes)
{
    for(Int i = 0; i < vm->fc; i++)
        omark(vm, cast(O*, vm->frames[i].closure));
}

MS_FN(markupvalues)
{
    for(OUpvalue* upval = vm->open_upvals; upval != NULL; upval = upval->next)
        omark(vm, cast(O*, upval));
}

MS_FN(markstatics)
{
    for(uint32_t i = 0; i < SS_SIZE; i++)
        omark(vm, cast(O*, vm->faststatic[i]));
}

MS_FN(markinterned)
{
    for(uint32_t i = 0; i < vm->interned.len; i++)
        omark(vm, cast(O*, vm->interned.data[i]));
}

MS_FN(markloaded)
{
    marktable(vm, &vm->loaded);
}

MS_FN(marktemp)
{
    for(uint32_t i = 0; i < vm->temp.len; i++)
        vmark(vm, vm->temp.data[i]);
}

MS_FN(markroots)
{
    markstack(vm); // VM stack
    markframes(vm); // VM call stack
    markupvalues(vm); // VM open upvalues (to be closed)
    markglobals(vm); // global values and identifiers
    markstatics(vm); // fast statics (interned strings)
    markinterned(vm); // normal statics (interned strings)
    markloaded(vm); // mark loaded script names table
}

MS_FN(rmweakrefs)
{
    for(uint32_t i = 0; i < vm->weakrefs.cap; i++) {
        Entry* entry = &vm->weakrefs.entries[i];
        if(IS_OBJ(entry->key) && !oismarked(AS_OBJ(entry->key)))
            HashTable_remove(&vm->weakrefs, entry->key);
    }
}

MS_FN(sweep)
{
    O* previous = NULL;
    O* current = vm->objects;
    while(current != NULL) {
        if(oismarked(current)) {
            osetmark(current, false);
            previous = current;
            current = onext(current);
        } else {
            O* unreached = current;
            current = onext(current);
            if(previous != NULL) osetnext(previous, current);
            else vm->objects = current;
            ofree(vm, unreached);
        }
    }
}






void mark_black(VM* vm, O* obj)
{
#ifdef DEBUG_LOG_GC
    printf("%p blacken ", (void*)obj);
    vprint(OBJ_VAL(obj));
    printf("\n");
#endif
#ifdef SK_PRECOMPUTED_GOTO
#define OBJ_TABLE
#include "jmptable.h"
#undef OBJ_TABLE
#else
#define DISPATCH(x) switch(x)
#define CASE(label) case label:
#define BREAK break
#endif
    ASSERT(oismarked(obj), "Object is not marked.");
    DISPATCH(otype(obj))
    {
        CASE(OBJ_UPVAL)
        {
            vmark(vm, cast(OUpvalue*, obj)->closed);
            BREAK;
        }
        CASE(OBJ_FUNCTION)
        {
            OFunction* fn = cast(OFunction*, obj);
            omark(vm, cast(O*, fn->p.name));
            omark(vm, cast(O*, fn->p.source));
            for(uint32_t i = 0; i < fn->chunk.constants.len; i++)
                vmark(vm, fn->chunk.constants.data[i]);
            BREAK;
        }
        CASE(OBJ_CLOSURE)
        {
            OClosure* closure = (OClosure*)obj;
            omark(vm, (O*)closure->fn);
            for(uint32_t i = 0; i < closure->fn->p.upvalc; i++)
                omark(vm, cast(O*, closure->upvalue[i]));
            BREAK;
        }
        CASE(OBJ_CLASS)
        {
            OClass* oclass = cast(OClass*, obj);
            omark(vm, cast(O*, oclass->name));
            marktable(vm, &oclass->methods);
            for(uint8_t i = 0; i < OM_CNT; i++)
                omark(vm, cast(O*, oclass->omethods[i]));
            BREAK;
        }
        CASE(OBJ_INSTANCE)
        {
            OInstance* instance = cast(OInstance*, obj);
            omark(vm, cast(O*, instance->oclass));
            marktable(vm, &instance->fields);
            BREAK;
        }
        CASE(OBJ_BOUND_METHOD)
        {
            OBoundMethod* bound_method = cast(OBoundMethod*, obj);
            omark(vm, cast(O*, bound_method));
            vmark(vm, bound_method->receiver);
            omark(vm, cast(O*, bound_method->method));
            BREAK;
        }
        CASE(OBJ_NATIVE)
        {
            ONative* native = cast(ONative*, obj);
            omark(vm, cast(O*, native->p.name));
            for(uint32_t i = 0; i < native->p.upvalc; i++)
                vmark(vm, native->upvalue[i]);
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
    size_t old_allocation = vm->gc.gc_allocated;
    markroots(vm);
#ifdef SK_PRECOMPUTED_GOTO
    static const void* jmptable[] = {&&mark, &&skip};
    goto* jmptable[runtime];
mark:
    mark_function_roots(vm);
skip:
#else
    mark_function_roots(vm);
#endif
    while(vm->gslen > 0)
        mark_black(vm, GSARRAY_POP(vm));
    rmweakrefs(vm);
    sweep(vm);
    size_t nextgc = cast(size_t, cast(double, vm->gc.gc_allocated) * vm->gc.gc_growfactor);
    vm->gc.gc_nextgc = MAX(nextgc, vm->gc.gc_heapmin);
#ifdef DEBUG_LOG_GC
    printf("--> GC end\n");
    printf(
        "    collected %lu bytes (from %lu to %lu) next collection at %lu\n",
        old_allocation - vm->gc.gc_allocated,
        old_allocation,
        vm->gc.gc_allocated,
        (size_t)vm->gc.gc_nextgc);
#endif
    return old_allocation - vm->gc.gc_allocated;
}




/* Safe allocator that can trigger collection.
 * Upon failed allocation (allocator returned NULL),
 * full collection will be invoked (gc) and second try
 * in allocating will be attempted.
 * If second try fails memory error is invoked. */
void* gcrealloc(VM* vm, void* ptr, size_t oldc, size_t newc)
{
    uint8_t first_try = 1;
    sk_assert(vm, newc >= oldc, "Can't free memory with 'gcrealloc'");
#ifdef DEBUG_STRESS_GC
    if(newc > oldc) gc(vm);
#else
    if(!vm->gc.gc_stopped && (vm->gc.gc_nextgc <= vm->gc.gc_allocated + (newc - oldc))) gc(vm);
#endif
try_again:;
    void* allocation = REALLOC(vm, ptr, newc);
    if(unlikely(allocation == NULL)) {
        if(likely(first_try)) {
            gc(vm); // try free some memory
            first_try = 0;
            goto try_again; // copium
        } else memerror(vm); // we tried all we could :(
    }
    vm->gc.gc_allocated += newc - oldc;
    return allocation;
}



/* Free memory and update the allocated bytes count.
 * NOTE: Never triggers collection and can't fail. */
void gcfree(VM* vm, void* ptr, size_t oldc, size_t newc)
{
    vm->gc.gc_allocated += newc - oldc;
    sk_assert(vm, newc <= oldc, "Can't allocate with 'gcfree'");
    REALLOC(vm, ptr, newc);
}
