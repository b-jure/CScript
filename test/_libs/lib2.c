#include "tokudae.h"
#include "tokudaeaux.h"


static int id(toku_State *T) {
    return toku_getntop(C);
}


static const struct tokuL_Entry funcs[] = {
    {"id", id},
    {NULL, NULL}
};


CSMOD_API int tokuopen_lib2(toku_State *T) {
    toku_setntop(C, 2);
    toku_set_global(C, "y"); /* y gets 2nd parameter */
    toku_set_global(C, "x"); /* x gets 1st parameter */
    tokuL_push_lib(C, funcs);
    return 1;
}
