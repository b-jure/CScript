/*
** carray.h
** Array manipulation functions
** See Copyright Notice in cscript.h
*/

#include "carray.h"
#include "cgc.h"
#include "climits.h"
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


/* ensure that 'index' can fit into array memory block */
void crA_ensure(cr_State *ts, Array *arr, cr_Integer index) {
    if (cri_castS2U(index) >= arr->sz) {
        crM_ensurevec(ts, arr->b, arr->sz, arr->n, index - arr->n + 1, ARRAYLIMIT,
                      "array elements", TValue);
        for (uint i = arr->n; i < arr->sz; i++)
            setnilval(&arr->b[i]);
        arr->n = index + 1; /* adjust new length */
    }
}


void crA_free(cr_State *ts, Array *arr) {
    crM_freearray(ts, arr->b, arr->sz, TValue);
    crM_free(ts, arr, sizeof(*arr));
}
