#include "tokudae.h"


int tokuopen_lib2(toku_State *T);

CSMOD_API int tokuopen_lib21(toku_State *T) {
    return tokuopen_lib2(C);
}
