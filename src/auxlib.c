#include "auxlib.h"

SK_LIBAPI SK_Number sk_checknumber(VM* vm, int idx)
{
    int       isnum;
    SK_Number n = sk_getnumber(vm, idx, &isnum);
    if(unlikely(!isnum)) {
    }
    return n;
}
