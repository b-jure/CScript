/*
** cmem.c
** Functions for memory management
** See Copyright Notice in cscript.h
*/

#include "cdebug.h"
#include "cmem.h"
#include "cconf.h"
#include "cstate.h"


#define crM_rawmalloc(gs,s)         (gs)->falloc(NULL, 0, s, (gs)->udalloc)
#define crM_rawrealloc(gs,p,os,ns)  (gs)->falloc(p, os, ns, (gs)->udalloc)
#define crM_rawfree(gs,p,osz)       (gs)->falloc(p, osz, 0, (gs)->udalloc)


/* can try to allocate second time */
#define cantryagain(ts)     (gsinitialized(gs) && !(gs)->gc.stopem)



/* Auxiliary to 'crM_realloc' and 'cr_malloc'. */
cr_sinline void *tryagain(cr_State *ts, void *ptr, size_t osz, size_t nsz) {
    GState *gs = G_(ts);
    UNUSED(osz);
    if (cantryagain(gs)) {
        crG_full(ts, 1);
        return crM_rawrealloc(gs, ptr, osz, nsz);
    }
    return NULL;
}


void *crM_realloc(cr_State *ts, void *ptr, size_t osz, size_t nsz) {
    GState *gs = G_(ts);
    cr_assert((osz == 0) == (ptr == NULL));
    void *memblock = crM_rawrealloc(gs, ptr, osz, nsz);
    if (cr_unlikely(!memblock && nsz != 0)) {
        memblock = tryagain(ts, ptr, osz, nsz);
        if (cr_unlikely(memblock == NULL))
            return NULL;
    }
    cr_assert((nsz == 0) == (memblock == NULL));
    gs->gc.debt += nsz - osz;
    return memblock;
}


void *crM_saferealloc(cr_State *ts, void *ptr, size_t osz, size_t nsz) {
    void *memblock = crM_realloc(ts, ptr, osz, nsz);
    if (cr_unlikely(memblock == NULL && nsz != 0))
        cr_assert(0 && "out of memory");
    return memblock;
}


void *crM_malloc(cr_State *ts, size_t size) {
    if (size == 0)
        return NULL;
    GState *gs = G_(ts);
    void *memblock = crM_rawmalloc(gs, size);
    if (cr_unlikely(memblock == NULL)) {
        memblock = tryagain(ts, NULL, 0, size);
        if (cr_unlikely(memblock == NULL))
            cr_assert(0 && "out of memory");
    }
    gs->gc.debt += size;
    return memblock;
}


void *crM_growarr(cr_State *ts, void *ptr, int len, int *sizep,
                  int elemsize, int extra, int limit, const char *what) {
    int size = *sizep;
    if (len + extra <= size)
        return ptr;
    size += extra;
    if (size >= limit / 2) {
        if (cr_unlikely(size >= limit))
            crD_runerror(ts, "%s size limit", what);
        size = limit;
        cr_assert(size >= CRI_MINARRSIZE);
    } else {
        size *= 2;
        if (size < CRI_MINARRSIZE)
            size = CRI_MINARRSIZE;
    }
    ptr = crM_saferealloc(ts, ptr, *sizep * elemsize, size * elemsize);
    *sizep = size;
    return ptr;
}


void *crM_shrinkarr_(cr_State *ts, void *ptr, int *sizep, int final,
                     int elemsize) {
    size_t oldsize = cast_sizet(*sizep * elemsize);
    size_t newsize = cast_sizet(final * elemsize);
    cr_assert(newsize <= oldsize);
    ptr = crM_saferealloc(ts, ptr, oldsize, newsize);
    *sizep = final;
    return ptr;
}


void crM_free(cr_State *ts, void *ptr, size_t osz) {
    GState *gs = G_(ts);
    cr_assert((osz == 0) == (ptr == NULL));
    crM_rawfree(gs, ptr, osz);
    gs->gc.debt -= osz;
}
