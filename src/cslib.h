/*
** cslib.h
** CScript standard libraries
** See Copyright Notice in cscript.h
*/

#ifndef CLIB_H
#define CLIB_H


#include "cscript.h"

CSMOD_API int csL_open_basic(cs_State *C);

CSLIB_API void csL_open_libs(cs_State *C);

#endif
