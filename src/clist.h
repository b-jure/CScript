/*
** clist.h
** List manipulation functions
** See Copyright Notice in cscript.h
*/

#ifndef CLIST_H
#define CLIST_H

#include "cobject.h"


CSI_FUNC Array *csA_new(cs_State *C);
CSI_FUNC Array *csA_newl(cs_State *C, uint n);
CSI_FUNC void csA_shrink(cs_State *C, Array *arr);
CSI_FUNC int csA_ensure(cs_State *C, Array *arr, uint n);
CSI_FUNC void csA_ensureindex(cs_State *C, Array *arr, uint index);
CSI_FUNC void csA_free(cs_State *C, Array *arr);

#endif
