#include "auxlib.h"
#include "skooma.h"

SK_LIBAPI int sk_terror(VM* vm, int argidx, const char* tname)
{
    const char* argname = NULL;
    const char* argtname = NULL;
    if((sk_isinstance(vm, argidx) || sk_isclass(vm, argidx)) &&
       sk_pushmethod(vm, argidx, "__display__"))
    {
        sk_call(vm, 0, 1);
    }
    argtname = sk_typename(vm, argidx);
    return 1;
}

SK_LIBAPI int sk_aerror(VM* vm, int argidx, const char* extra)
{
}

SK_LIBAPI sk_number sk_checknumber(VM* vm, int idx)
{
    int       isnum;
    sk_number n = sk_getnumber(vm, idx, &isnum);
    if(unlikely(!isnum)) {
    }
    return n;
}
