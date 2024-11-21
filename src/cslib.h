/*
** cslib.h
** CScript standard libraries
** See Copyright Notice in cscript.h
*/

#ifndef CSLIB_H
#define CSLIB_H


#include "cscript.h"

CSMOD_API int csL_open_core(cs_State *ts);

CSLIB_API void csL_open_libs(cs_State *ts);

#endif
