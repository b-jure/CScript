/*
** cmem.c
** Functions for memory management
** See Copyright Notice in cscript.h
*/


#define CS_CORE


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
#define cantryagain(gs)         (statefullybuilt(gs) && !(gs)->gcstopem)


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


c_sinline void *tryagain(cs_State *ts, void *ptr, size_t osz, size_t nsz) {
    GState *gs = G_(ts);
    if (cantryagain(gs)) {
        csG_full(ts, 1); /* try to reclaim some memory... */
        return callfalloc(gs, ptr, osz, nsz); /* ...and try again */
    }
    return NULL; /* cannot run an emergency collection */
}


void *csM_realloc_(cs_State *ts, void *ptr, size_t osz, size_t nsz) {
    GState *gs = G_(ts);
    void *block;
    cs_assert((osz == 0) == (ptr == NULL));
    block = firsttry(gs, ptr, osz, nsz);
    if (c_unlikely(!block && nsz != 0)) {
        block = tryagain(ts, ptr, osz, nsz);
        if (c_unlikely(block == NULL)) /* still no memory? */
            return NULL; /* do not update 'gcdebt' */
    }
    cs_assert((nsz == 0) == (block == NULL));
    gs->gcdebt = (gs->gcdebt + nsz) - osz;
    return block;
}


void *csM_saferealloc(cs_State *ts, void *ptr, size_t osz, size_t nsz) {
    void *block = csM_realloc_(ts, ptr, osz, nsz);
    if (c_unlikely(block == NULL && nsz != 0))
        csM_error(ts);
    return block;
}


void *csM_malloc_(cs_State *ts, size_t size, int tag) {
    if (size == 0) {
        return NULL;
    } else {
        GState *gs = G_(ts);
        void *block = firsttry(gs, NULL, tag, size);
        if (c_unlikely(block == NULL)) {
            block = tryagain(ts, NULL, tag, size);
            if (c_unlikely(block == NULL))
                csM_error(ts);
        }
        gs->gcdebt += size;
        return block;
    }
}


/* minimum size of array memory block */
#define MINSIZEARRAY    4

void *csM_growarr_(cs_State *ts, void *ptr, int *sizep, int len, int elemsize,
                   int space, int limit, const char *what) {
    int size = *sizep;
    cs_assert(space >= 1);
    if (size - len >= space) { /* have enough space? */
        return ptr; /* done */
    } else { /* otherwise expand */
        size *= 2; /* 2x size */
        if (c_unlikely(limit - space < len)) /* limit reached? */
            csD_runerror(ts, "too many %s (limit is %d)", what, limit);
        cs_assert(size <= limit);
        if (size < len + space)
            size = len + space;
        if (c_unlikely(size < MINSIZEARRAY))
            size = MINSIZEARRAY;
        ptr = csM_saferealloc(ts, ptr, (*sizep)*elemsize, size*elemsize);
        *sizep = size;
        return ptr;
    }
}


void *csM_shrinkarr_(cs_State *ts, void *ptr, int *sizep, int nfinal,
                     int elemsize) {
    size_t osz = cast_sizet((*sizep) * elemsize);
    size_t nsz = cast_sizet(nfinal * elemsize);
    cs_assert(nsz <= osz);
    ptr = csM_saferealloc(ts, ptr, osz, nsz);
    *sizep = nfinal;
    return ptr;
}


c_noret csM_toobig(cs_State *ts) {
    csD_runerror(ts, "memory allocation error: block too big");
}


void csM_free_(cs_State *ts, void *ptr, size_t osz) {
    GState *gs = G_(ts);
    cs_assert((osz == 0) == (ptr == NULL));
    callfalloc(gs, ptr, osz, 0);
    gs->gcdebt -= osz;
}
