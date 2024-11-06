/*
** cslib.h
** CScript standard libraries
** See Copyright Notice in cscript.h
*/

#ifndef CSLIB_H
#define CSLIB_H


#include "cscript.h"


#define CS_CORELIBNAME      "core"
CRMOD_API int csL_open_core(cs_State *ts);


CRLIB_API void csL_open_libs(cs_State *ts);


#endif
