/*
** tokudaelib.h
** Tokudae standard libraries
** See Copyright Notice in tokudae.h
*/

#ifndef tokudaelib_h
#define tokudaelib_h


#include "tokudae.h"


/* version suffix for environment variable names */
#define TOKU_VERSUFFIX      "_" TOKU_VERSION_MAJOR "_" TOKU_VERSION_MINOR


// TODO: update docs
TOKUMOD_API int tokuopen_basic(toku_State *T);

#define TOKU_LOADLIBNAME  "package"
TOKUMOD_API int tokuopen_package(toku_State *T);

#define TOKU_STRLIBNAME   "string"
TOKUMOD_API int tokuopen_string(toku_State *T);

#define TOKU_MATHLIBNAME  "math"
TOKUMOD_API int tokuopen_math(toku_State *T);

#define TOKU_IOLIBNAME    "io"
TOKUMOD_API int tokuopen_io(toku_State *T);

#define TOKU_OSLIBNAME    "os"
TOKUMOD_API int tokuopen_os(toku_State *T);

// TODO: add docs
#define TOKU_REGLIBNAME   "reg"
TOKUMOD_API int tokuopen_reg(toku_State *T);

// TODO: add docs
#define TOKU_DBLIBNAME    "debug"
TOKUMOD_API int tokuopen_debug(toku_State *T);

// TODO: add docs
#define TOKU_LISTLIBNAME  "list"
TOKUMOD_API int tokuopen_list(toku_State *T);

// TODO: add docs
#define TOKU_UTF8NAME     "utf8"
TOKUMOD_API int tokuopen_utf8(toku_State *T);


/* open all previous libraries */
TOKULIB_API void tokuL_openlibs(toku_State *T);


#endif
