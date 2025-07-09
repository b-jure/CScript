/*
** cscriptlib.h
** CScript standard libraries
** See Copyright Notice in cscript.h
*/

#ifndef cscriptlib_h
#define cscriptlib_h


#include "cscript.h"


/* version suffix for environment variable names */
#define CS_VERSUFFIX          "_" CS_VERSION_MAJOR "_" CS_VERSION_MINOR


// TODO: update docs
CSMOD_API int csopen_basic(cs_State *C);

#define CS_LOADLIBNAME  "package"
CSMOD_API int csopen_package(cs_State *C);

#define CS_STRLIBNAME   "string"
CSMOD_API int csopen_string(cs_State *C);

#define CS_MATHLIBNAME  "math"
CSMOD_API int csopen_math(cs_State *C);

#define CS_IOLIBNAME    "io"
CSMOD_API int csopen_io(cs_State *C);

#define CS_OSLIBNAME    "os"
CSMOD_API int csopen_os(cs_State *C);

// TODO: add docs
#define CS_REGLIBNAME   "reg"
CSMOD_API int csopen_reg(cs_State *C);

// TODO: add docs
#define CS_DBLIBNAME    "debug"
CSMOD_API int csopen_debug(cs_State *C);

// TODO: add docs
#define CS_LISTLIBNAME  "list"
CSMOD_API int csopen_list(cs_State *C);

// TODO: add docs
#define CS_UTF8NAME     "utf8"
CSMOD_API int csopen_utf8(cs_State *C);


/* open all previous libraries */
CSLIB_API void csL_openlibs(cs_State *C);


#endif
