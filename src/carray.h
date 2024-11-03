/*
** carray.h
** Array manipulation functions
** See Copyright Notice in cscript.h
*/

#ifndef CSTARRAY_H
#define CSTARRAY_H

#include "cobject.h"

/* array size limit */
#define ARRAYLIMIT      (INT_MAX - 1)

CRI_FUNC Array *crA_new(cr_State *ts);
CRI_FUNC void crA_shrink(cr_State *ts, Array *arr);
CRI_FUNC void crA_ensure(cr_State *ts, Array *arr, cr_Integer index);
CRI_FUNC void crA_free(cr_State *ts, Array *arr);

#endif
