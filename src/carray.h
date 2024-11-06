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

CSI_FUNC Array *csA_new(cs_State *ts);
CSI_FUNC void csA_shrink(cs_State *ts, Array *arr);
CSI_FUNC void csA_ensure(cs_State *ts, Array *arr, cs_Integer index);
CSI_FUNC void csA_free(cs_State *ts, Array *arr);

#endif
