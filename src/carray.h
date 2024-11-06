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

CSI_FUNC Array *crA_new(cs_State *ts);
CSI_FUNC void crA_shrink(cs_State *ts, Array *arr);
CSI_FUNC void crA_ensure(cs_State *ts, Array *arr, cs_Integer index);
CSI_FUNC void crA_free(cs_State *ts, Array *arr);

#endif
