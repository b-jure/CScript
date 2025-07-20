#include "tokudae.h"


int tokuopen_lib2(toku_State *T);

TOKUMOD_API int tokuopen_lib21(toku_State *T) {
    return tokuopen_lib2(T);
}
