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


#define csM_rawmalloc(gs,s)         (gs)->falloc(NULL, 0, s, (gs)->ud_alloc)
#define csM_rawrealloc(gs,p,os,ns)  (gs)->falloc(p, os, ns, (gs)->ud_alloc)
#define csM_rawfree(gs,p,osz)       (gs)->falloc(p, osz, 0, (gs)->ud_alloc)


/* can try to allocate second time */
#define cantryagain(ts)     (gsinitialized(gs) && !(gs)->gcstopem)



/* Auxiliary to 'csM_realloc' and 'csmalloc'. */
cs_sinline void *tryagain(cs_State *ts, void *ptr, size_t osz, size_t nsz) {
    GState *gs = G_(ts);
    UNUSED(osz);
    if (cantryagain(gs)) {
        csG_full(ts, 1);
        return csM_rawrealloc(gs, ptr, osz, nsz);
    }
    return NULL;
}


void *csM_realloc_(cs_State *ts, void *ptr, size_t osz, size_t nsz) {
    GState *gs = G_(ts);
    void *block;
    cs_assert((osz == 0) == (ptr == NULL));
    block = csM_rawrealloc(gs, ptr, osz, nsz);
    if (c_unlikely(!block && nsz != 0)) {
        block = tryagain(ts, ptr, osz, nsz);
        if (c_unlikely(block == NULL))
            return NULL;
    }
    cs_assert((nsz == 0) == (block == NULL));
    gs->gcdebt = (gs->gcdebt + nsz) - osz;
    return block;
}


void *csM_saferealloc(cs_State *ts, void *ptr, size_t osz, size_t nsz) {
    void *block = csM_realloc_(ts, ptr, osz, nsz);
    if (c_unlikely(block == NULL && nsz != 0))
        cs_assert(0 && "out of memory");
    return block;
}


void *csM_malloc(cs_State *ts, size_t size) {
    GState *gs;
    void *block;
    if (size == 0)
        return NULL;
    gs = G_(ts);
    block = csM_rawmalloc(gs, size);
    if (c_unlikely(block == NULL)) {
        block = tryagain(ts, NULL, 0, size);
        if (c_unlikely(block == NULL))
            cs_assert(0 && "out of memory");
    }
    gs->gcdebt += size;
    return block;
}


void *csM_growarr(cs_State *ts, void *ptr, int *sizep, int len, int elemsize,
                  int extra, int limit, const char *what) {
    int size = *sizep;
    if (len + extra <= size)
        return ptr;
    size += extra;
    if (size >= limit / 2) {
        if (c_unlikely(size >= limit))
            csD_runerror(ts, "%s size limit", what);
        size = limit;
        cs_assert(size >= CSI_MINARRSIZE);
    } else {
        size *= 2;
        if (size < CSI_MINARRSIZE)
            size = CSI_MINARRSIZE;
    }
    ptr = csM_saferealloc(ts, ptr, *sizep * elemsize, size * elemsize);
    *sizep = size;
    return ptr;
}


void *csM_shrinkarr(cs_State *ts, void *ptr, int *sizep, int final,
                    int elemsize) {
    size_t oldsize = cast_sizet(*sizep * elemsize);
    size_t newsize = cast_sizet(final * elemsize);
    cs_assert(newsize <= oldsize);
    ptr = csM_saferealloc(ts, ptr, oldsize, newsize);
    *sizep = final;
    return ptr;
}


void csM_free_(cs_State *ts, void *ptr, size_t osz) {
    GState *gs = G_(ts);
    cs_assert((osz == 0) == (ptr == NULL));
    csM_rawfree(gs, ptr, osz);
    gs->gcdebt -= osz;
}
