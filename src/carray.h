/*
** carray.h
** Array manipulation functions
** See Copyright Notice in cscript.h
*/

#ifndef CARRAY_H
#define CARRAY_H

#include "cobject.h"


/* array size limit */
#define ARRAYLIMIT      MAXINT


#define csA_reset(arr)      ((arr)->n = 0)


CSI_FUNC Array *csA_new(cs_State *C);
CSI_FUNC void csA_shrink(cs_State *C, Array *arr);
CSI_FUNC void csA_ensure(cs_State *C, Array *arr, int index);
CSI_FUNC void csA_free(cs_State *C, Array *arr);

#endif
