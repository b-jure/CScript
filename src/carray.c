/*
** carray.h
** Array manipulation functions
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "cdebug.h"
#include "carray.h"
#include "cgc.h"
#include "climits.h"
#include "cmem.h"
#include "cobject.h"


Array *csA_new(cs_State *C) {
    GCObject *o = csG_new(C, sizeof(Array), CS_VARRAY);
    Array *arr = gco2arr(o);
    arr->sz = arr->n = 0;
    arr->b = NULL;
    return arr;
}


/* shrinks array size to the actual size being used */
void csA_shrink(cs_State *C, Array *arr) {
    if (arr->b && arr->sz > arr->n)
        csM_shrinkarray(C, arr->b, arr->sz, arr->n, TValue);
}


/* ensure that 'index' can fit into array memory block */
void csA_ensure(cs_State *C, Array *arr, int index) {
    cs_assert(index >= 0);
    if (cast_uint(index) < arr->n) { /* 'index' in bounds? */
        return; /* done */
    } else {
        csM_ensurearray(C, arr->b, arr->sz, arr->n, index + 1 - arr->n,
                        ARRAYLIMIT, "array elements", TValue);
        for (int i = arr->n; i <= index; i++) /* nil out values in-between */
            setnilval(&arr->b[i]);
        arr->n = index + 1; /* adjust new in-use length */
    }
}


void csA_free(cs_State *C, Array *arr) {
    csM_freearray(C, arr->b, arr->sz);
    csM_free(C, arr);
}
