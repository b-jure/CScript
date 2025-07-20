/*
** tokudaelib.h
** Tokudae standard libraries
** See Copyright Notice in tokudae.h
*/

#ifndef tokudaelib_h
#define tokudaelib_h


#include "tokudae.h"


/* version suffix for environment variable names */
#define TOKU_VERSUFFIX          "_" TOKU_VERSION_MAJOR "_" TOKU_VERSION_MINOR


// TODO: update docs
CSMOD_API int tokuopen_basic(toku_State *T);

#define TOKU_LOADLIBNAME  "package"
CSMOD_API int tokuopen_package(toku_State *T);

#define TOKU_STRLIBNAME   "string"
CSMOD_API int tokuopen_string(toku_State *T);

#define TOKU_MATHLIBNAME  "math"
CSMOD_API int tokuopen_math(toku_State *T);

#define TOKU_IOLIBNAME    "io"
CSMOD_API int tokuopen_io(toku_State *T);

#define TOKU_OSLIBNAME    "os"
CSMOD_API int tokuopen_os(toku_State *T);

// TODO: add docs
#define TOKU_REGLIBNAME   "reg"
CSMOD_API int tokuopen_reg(toku_State *T);

// TODO: add docs
#define TOKU_DBLIBNAME    "debug"
CSMOD_API int tokuopen_debug(toku_State *T);

// TODO: add docs
#define TOKU_LISTLIBNAME  "list"
CSMOD_API int tokuopen_list(toku_State *T);

// TODO: add docs
#define TOKU_UTF8NAME     "utf8"
CSMOD_API int tokuopen_utf8(toku_State *T);


/* open all previous libraries */
TOKULIB_API void tokuL_openlibs(toku_State *T);


#endif
