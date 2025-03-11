/*
** cslib.h
** CScript standard libraries
** See Copyright Notice in cscript.h
*/

#ifndef CLIB_H
#define CLIB_H


#include "cscript.h"


/* version suffix for environment variable names */
#define CS_VERSUFFIX          "_" CS_VERSION_MAJOR "_" CS_VERSION_MINOR


CSMOD_API int csopen_basic(cs_State *C);

#define CS_LOADLIBNAME      "package"
CSMOD_API int csopen_package(cs_State *C);

#define CS_STRLIBNAME       "string"
CSMOD_API int csopen_string(cs_State *C);

#define CS_MATHLIBNAME      "math"
CSMOD_API int csopen_math(cs_State *C);


/* open all previous libraries */
CSLIB_API void csL_openlibs(cs_State *C);

#endif
