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


Array *csA_new(cs_State *C) {
    GCObject *o = csG_new(C, sizeof(Array), CS_VARRAY);
    Array *arr = gco2arr(o);
    arr->sz = arr->n = 0;
    arr->b = NULL;
    return arr;
}


void csA_shrink(cs_State *C, Array *arr) {
    if (arr->b && arr->sz > arr->n)
        csM_shrinkarray(C, arr->b, arr->sz, arr->n, TValue);
}


int csA_ensure(cs_State *C, Array *arr, uint n) {
    if (n <= arr->n) /* in bound? */
        return 0; /* done */
    else {
        csM_ensurearray(C, arr->b, arr->sz, arr->n, n - arr->n,
                        ARRAYLIMIT, "array elements", TValue);
        for (uint i = arr->n; i < n; i++)
            setnilval(&arr->b[i]); /* clear new part */
        return 1;
    }
}


void csA_ensureindex(cs_State *C, Array *arr, uint index) {
    if (csA_ensure(C, arr, index + 1))
        arr->n = index + 1;
}


void csA_free(cs_State *C, Array *arr) {
    csM_freearray(C, arr->b, arr->sz);
    csM_free(C, arr);
}
