/*
** carray.h
** Array manipulation functions
** See Copyright Notice in cscript.h
*/

#include "carray.h"
#include "cgc.h"
#include "cmem.h"
#include "cobject.h"


Array *crA_new(cr_State *ts) {
    Array *arr = crG_new(ts, sizeof(Array), CR_VARRAY, Array);
    arr->sz = arr->n = 0;
    arr->b = NULL;
    return arr;
}


/* shrinks array size to the actual size being used */
void crA_shrink(cr_State *ts, Array *arr) {
    if (arr->b && arr->sz > arr->n)
        crM_shrinkvec(ts, arr->b, arr->sz, arr->n, TValue);
}


void crA_free(cr_State *ts, Array *arr) {
    crM_freearray(ts, arr->b, arr->sz, TValue);
    crM_free(ts, arr, sizeof(*arr));
}
