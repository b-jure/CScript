/*
** carray.h
** Array manipulation functions
** See Copyright Notice in cscript.h
*/

#ifndef CSTARRAY_H
#define CSTARRAY_H

#include "cobject.h"

CRI_FUNC Array *crA_new(cr_State *ts);
CRI_FUNC void crA_shrink(cr_State *ts, Array *arr);
CRI_FUNC void crA_free(cr_State *ts, Array *arr);

#endif
