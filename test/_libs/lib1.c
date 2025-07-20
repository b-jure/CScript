#include "tokudae.h"
#include "tokudaeaux.h"

static int id(toku_State *T) {
    return toku_getntop(C);
}


static const struct tokuL_Entry funcs[] = {
    {"id", id},
    {NULL, NULL}
};


/* function used by lib11.c */
CSMOD_API int lib1_export(toku_State *T) {
    toku_push_string(C, "exported");
    return 1;
}


CSMOD_API int onefunction(toku_State *T) {
    tokuL_check_version(C);
    toku_setntop(C, 2);
    toku_push(C, 0);
    return 2;
}


CSMOD_API int anotherfunc(toku_State *T) {
    tokuL_check_version(C);
    toku_push_fstring(C, "%d%%%d\n", (int)toku_to_integer(C, 0),
                                   (int)toku_to_integer(C, 1));
    return 1;
} 


CSMOD_API int tokuopen_lib1_sub(toku_State *T) {
    toku_set_global(C, "y"); /* 2nd arg: extra value (file name) */
    toku_set_global(C, "x"); /* 1st arg: module name */
    tokuL_push_lib(C, funcs);
    return 1;
}
