/*
** tmem.c
** Functions for memory management
** See Copyright Notice in tokudae.h
*/

#define tmem_c
#define TOKU_CORE

#include "tokudaeprefix.h"

#include "tgc.h"
#include "tdebug.h"
#include "tmem.h"
#include "tstate.h"
#include "tprotected.h"


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


#if defined(TOKUI_EMERGENCYGCTESTS)

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


t_sinline void *tryagain(toku_State *T, void *ptr, size_t osz, size_t nsz) {
    GState *gs = G(T);
    if (cantryagain(gs)) {
        tokuG_fullinc(T, 1); /* try to reclaim some memory... */
        return callfalloc(gs, ptr, osz, nsz); /* ...and try again */
    }
    return NULL; /* cannot run an emergency collection */
}


void *tokuM_realloc_(toku_State *T, void *ptr, size_t osz, size_t nsz) {
    GState *gs = G(T);
    void *block;
    toku_assert((osz == 0) == (ptr == NULL));
    block = firsttry(gs, ptr, osz, nsz);
    if (t_unlikely(!block && nsz != 0)) {
        block = tryagain(T, ptr, osz, nsz);
        if (t_unlikely(block == NULL)) /* still no memory? */
            return NULL; /* do not update 'gcdebt' */
    }
    toku_assert((nsz == 0) == (block == NULL));
    gs->gcdebt = (gs->gcdebt + nsz) - osz;
    return block;
}


void *tokuM_saferealloc(toku_State *T, void *ptr, size_t osz, size_t nsz) {
    void *block = tokuM_realloc_(T, ptr, osz, nsz);
    if (t_unlikely(block == NULL && nsz != 0))
        tokuM_error(T);
    return block;
}


void *tokuM_malloc_(toku_State *T, size_t size, int tag) {
    if (size == 0) {
        return NULL;
    } else {
        GState *gs = G(T);
        void *block = firsttry(gs, NULL, tag, size);
        if (t_unlikely(block == NULL)) {
            block = tryagain(T, NULL, tag, size);
            if (t_unlikely(block == NULL))
                tokuM_error(T);
        }
        gs->gcdebt += size;
        return block;
    }
}


/* minimum size of array memory block */
#define MINASIZE    4

void *tokuM_growarr_(toku_State *T, void *block, int *sizep, int len,
                     int elemsize, int nelems, int limit, const char *what) {
    int size = *sizep;
    toku_assert(0 <= nelems && 0 < elemsize && what);
checkspace:
    if (t_likely(nelems <= size - len)) { /* have enough space for nelems? */
        toku_assert(size <= limit);
        return block; /* done; return the current block */
    } else { /* otherwise grow */
        if (t_unlikely(limit/2 <= size)) { /* cannot double it? */
            if (t_unlikely(limit <= size)) /* limit reached? */
                tokuD_runerror(T, "too many %s (limit is %d)", what, limit);
            size = limit;
        } else {
            size *= 2;
            size = (MINASIZE <= size) ? size : MINASIZE;
        }
        block = tokuM_saferealloc(T, block, cast_sizet(*sizep) * elemsize,
                                            cast_sizet(size) * elemsize);
        *sizep = size;
        goto checkspace;
    }
}


void *tokuM_shrinkarr_(toku_State *T, void *ptr, int *sizep, int nfinal,
                       int elemsize) {
    size_t osz = cast_sizet((*sizep) * elemsize);
    size_t nsz = cast_sizet(nfinal * elemsize);
    toku_assert(nsz <= osz);
    ptr = tokuM_saferealloc(T, ptr, osz, nsz);
    *sizep = nfinal;
    return ptr;
}


t_noret tokuM_toobig(toku_State *T) {
    tokuD_runerror(T, "memory allocation error: block too big");
}


void tokuM_free_(toku_State *T, void *ptr, size_t osz) {
    GState *gs = G(T);
    toku_assert((osz == 0) == (ptr == NULL));
    callfalloc(gs, ptr, osz, 0);
    gs->gcdebt -= osz;
}
