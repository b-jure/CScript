/*
** cmem.c
** Functions for memory management
** See Copyright Notice in cscript.h
*/

#define cmem_c
#define CS_CORE

#include "cprefix.h"

#include "cgc.h"
#include "cdebug.h"
#include "cmem.h"
#include "cstate.h"
#include "cprotected.h"


/* call allocator ('falloc') */
#define callfalloc(gs,b,os,ns)      (gs)->falloc(b, os, ns, (gs)->ud_alloc)


/* 
** When an allocation fails, it will try again after an emergency
** collection, except when it cannot run a collection.
** GC should not be called while the state is not fully built as
** some GC parameters might be uninitialized and if the 'gcstopem'
** is true, which means the interpreter is in the middle of a collection
** step.
*/
#define cantryagain(gs)     (statefullybuilt(gs) && !(gs)->gcstopem)


#if defined(EMERGENCYGCTESTS)

/*
** First allocation will fail except when freeing a block (frees never
** fail) and when it cannot try again; this fail will trigger 'tryagain'
** and a full GC cycle at every allocation.
*/
static void *firsttry(GState *gs, void *block, size_t os, size_t ns) {
    if (ns > 0 && cantryagain(gs))
        return NULL; /* fail */
    else
        return callfalloc(gs, block, os, ns);
}

#else

#define firsttry(gs,b,os,ns)    callfalloc(gs,b,os,ns)

#endif


c_sinline void *tryagain(cs_State *C, void *ptr, size_t osz, size_t nsz) {
    GState *gs = G(C);
    if (cantryagain(gs)) {
        csG_full(C, 1); /* try to reclaim some memory... */
        return callfalloc(gs, ptr, osz, nsz); /* ...and try again */
    }
    return NULL; /* cannot run an emergency collection */
}


void *csM_realloc_(cs_State *C, void *ptr, size_t osz, size_t nsz) {
    GState *gs = G(C);
    void *block;
    cs_assert((osz == 0) == (ptr == NULL));
    block = firsttry(gs, ptr, osz, nsz);
    if (c_unlikely(!block && nsz != 0)) {
        block = tryagain(C, ptr, osz, nsz);
        if (c_unlikely(block == NULL)) /* still no memory? */
            return NULL; /* do not update 'gcdebt' */
    }
    cs_assert((nsz == 0) == (block == NULL));
    gs->gcdebt = (gs->gcdebt + nsz) - osz;
    return block;
}


void *csM_saferealloc(cs_State *C, void *ptr, size_t osz, size_t nsz) {
    void *block = csM_realloc_(C, ptr, osz, nsz);
    if (c_unlikely(block == NULL && nsz != 0))
        csM_error(C);
    return block;
}


void *csM_malloc_(cs_State *C, size_t size, int tag) {
    if (size == 0) {
        return NULL;
    } else {
        GState *gs = G(C);
        void *block = firsttry(gs, NULL, tag, size);
        if (c_unlikely(block == NULL)) {
            block = tryagain(C, NULL, tag, size);
            if (c_unlikely(block == NULL))
                csM_error(C);
        }
        gs->gcdebt += size;
        return block;
    }
}


/* minimum size of array memory block */
#define MINSIZEARRAY    4

void *csM_growarr_(cs_State *C, void *block, int *sizep, int len,
                   int elemsize, int nelems, int limit, const char *what) {
    int size = *sizep;
    cs_assert(nelems > 0 && elemsize > 0 && what);
checkspace:
    if (c_likely(size - len >= nelems)) { /* have enough space for nelems? */
        cs_assert(size <= limit);
        return block; /* done; return the current block */
    } else { /* otherwise grow */
        if (c_unlikely(size >= limit / 2)) { /* cannot double it? */
            if (c_unlikely(size >= limit)) /* limit reached? */
                csD_runerror(C, "too many %s (limit is %d)", what, limit);
            size = limit;
        } else {
            size *= 2;
            if (size < MINSIZEARRAY)
                size = MINSIZEARRAY;
        }
        block = csM_saferealloc(C, block, cast_sizet(*sizep) * elemsize,
                                          cast_sizet(size) * elemsize);
        *sizep = size;
        goto checkspace;
    }
}


void *csM_shrinkarr_(cs_State *C, void *ptr, int *sizep, int nfinal,
                     int elemsize) {
    size_t osz = cast_sizet((*sizep) * elemsize);
    size_t nsz = cast_sizet(nfinal * elemsize);
    cs_assert(nsz <= osz);
    ptr = csM_saferealloc(C, ptr, osz, nsz);
    *sizep = nfinal;
    return ptr;
}


c_noret csM_toobig(cs_State *C) {
    csD_runerror(C, "memory allocation error: block too big");
}


void csM_free_(cs_State *C, void *ptr, size_t osz) {
    GState *gs = G(C);
    cs_assert((osz == 0) == (ptr == NULL));
    callfalloc(gs, ptr, osz, 0);
    gs->gcdebt -= osz;
}
