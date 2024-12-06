/*
** carray.h
** Array manipulation functions
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "carray.h"
#include "cgc.h"
#include "climits.h"
#include "cmem.h"
#include "cobject.h"


Array *csA_new(cs_State *ts) {
    GCObject *o = csG_new(ts, sizeof(Array), CS_VARRAY);
    Array *arr = gco2arr(o);
    arr->sz = arr->n = 0;
    arr->b = NULL;
    return arr;
}


/* shrinks array size to the actual size being used */
void csA_shrink(cs_State *ts, Array *arr) {
    if (arr->b && arr->sz > arr->n)
        csM_shrinkarray(ts, arr->b, arr->sz, arr->n, TValue);
}


/* ensure that 'index' can fit into array memory block */
void csA_ensure(cs_State *ts, Array *arr, int index) {
    cs_assert(index >= 0);
    if (cast_uint(index) < arr->n) { /* 'index' in bounds? */
        return; /* done */
    } else { /* otherwise adjust new in-use length */
        cs_assert(arr->n <= cast_uint(index));
        csM_ensurearray(ts, arr->b, arr->sz, arr->n, index - arr->n + 1,
                        ARRAYLIMIT, "array elements", TValue);
        for (int i = arr->n; i <= index; i++)
            setnilval(&arr->b[i]);
        arr->n = index + 1;
    }
}


void csA_free(cs_State *ts, Array *arr) {
    csM_freearray(ts, arr->b, arr->sz);
    csM_free(ts, arr);
}
