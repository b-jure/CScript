/*
** tokudaelib.c
** Tokudae standard libraries
** See Copyright Notice in tokudae.h
*/

#define tokudaelib_c
#define TOKU_LIB

#include "tokudaeprefix.h"

#include "tokudaelib.h"
#include "tokudaeaux.h"


static const tokuL_Entry loadedlibs[] = {
    {TOKU_GNAME, tokuopen_basic},
    {TOKU_LOADLIBNAME, tokuopen_package},
    {TOKU_STRLIBNAME, tokuopen_string},
    {TOKU_MATHLIBNAME, tokuopen_math},
    {TOKU_IOLIBNAME, tokuopen_io},
    {TOKU_OSLIBNAME, tokuopen_os},
    {TOKU_REGLIBNAME, tokuopen_reg},
    {TOKU_DBLIBNAME, tokuopen_debug},
    {TOKU_LISTLIBNAME, tokuopen_list},
    {TOKU_UTF8NAME, tokuopen_utf8},
    {NULL, NULL}
};


TOKULIB_API void tokuL_openlibs(toku_State *T) {
    for (const tokuL_Entry *lib = loadedlibs; lib->func != NULL; lib++) {
        tokuL_importf(T, lib->name, lib->func, 1);
        toku_pop(T, 1); /* remove module */
    }
}
