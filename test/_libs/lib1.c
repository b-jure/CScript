#include "tokudae.h"
#include "tokudaeaux.h"

static int id(toku_State *T) {
    return toku_getntop(T);
}


static const struct tokuL_Entry funcs[] = {
    {"id", id},
    {NULL, NULL}
};


/* function used by lib11.c */
TOKUMOD_API int lib1_export(toku_State *T) {
    toku_push_string(T, "exported");
    return 1;
}


TOKUMOD_API int onefunction(toku_State *T) {
    tokuL_check_version(T);
    toku_setntop(T, 2);
    toku_push(T, 0);
    return 2;
}


TOKUMOD_API int anotherfunc(toku_State *T) {
    tokuL_check_version(T);
    toku_push_fstring(T, "%d%%%d\n", (int)toku_to_integer(T, 0),
                                     (int)toku_to_integer(T, 1));
    return 1;
} 


TOKUMOD_API int tokuopen_lib1_sub(toku_State *T) {
    toku_set_global(T, "y"); /* 2nd arg: extra value (file name) */
    toku_set_global(T, "x"); /* 1st arg: module name */
    tokuL_push_lib(T, funcs);
    return 1;
}
