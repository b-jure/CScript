/*
** carray.h
** Array manipulation functions
** See Copyright Notice in cscript.h
*/

#ifndef CARRAY_H
#define CARRAY_H

#include "cobject.h"


/* array size limit */
#define ARRAYLIMIT      INT_MAX


CSI_FUNC Array *csA_new(cs_State *ts);
CSI_FUNC void csA_shrink(cs_State *ts, Array *arr);
CSI_FUNC void csA_ensure(cs_State *ts, Array *arr, int index);
CSI_FUNC void csA_free(cs_State *ts, Array *arr);

#endif
