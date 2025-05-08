/*
** clist.h
** List manipulation functions
** See Copyright Notice in cscript.h
*/

#ifndef clist_h
#define clist_h

#include "cobject.h"


#define csA_newmetalist(C)      csA_newl(C, CS_MM_N)


CSI_FUNC List *csA_new(cs_State *C);
CSI_FUNC List *csA_newl(cs_State *C, int n);
CSI_FUNC void csA_shrink(cs_State *C, List *l);
CSI_FUNC int csA_ensure(cs_State *C, List *l, int n);
CSI_FUNC void csA_ensureindex(cs_State *C, List *l, int index);
CSI_FUNC void csA_free(cs_State *C, List *l);

#endif
