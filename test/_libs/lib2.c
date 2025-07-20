#include "tokudae.h"
#include "tokudaeaux.h"


static int id(toku_State *T) {
    return toku_getntop(T);
}


static const struct tokuL_Entry funcs[] = {
    {"id", id},
    {NULL, NULL}
};


TOKUMOD_API int tokuopen_lib2(toku_State *T) {
    toku_setntop(T, 2);
    toku_set_global(T, "y"); /* y gets 2nd parameter */
    toku_set_global(T, "x"); /* x gets 1st parameter */
    tokuL_push_lib(T, funcs);
    return 1;
}
