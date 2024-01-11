#include "auxlib.h"
#include "skooma.h"

#include <errno.h>
#include <string.h>


SK_LIBAPI int sk_argerror(VM* vm, int argidx, const char* extra)
{
    sk_pushfstring(vm, "Invalid argument '%d' %s", argidx, extra);
    return sk_error(vm, S_EARG);
}


SK_LIBAPI int sk_typeerror(VM* vm, int argidx, const char* tname)
{
    const char* argmsg = NULL;
    const char* argtype = NULL;
    if(sk_isinstance(vm, argidx) && sk_getmethod(vm, argidx, "__display__")) {
        sk_call(vm, 0, 1);
        argtype = sk_tostring(vm, -1); // leave on stack, who cares...
    } else argtype = sk_typename(vm, argidx);
    argmsg = sk_pushfstring(vm, "expected '%s', instead got '%s'", tname, argtype);
    return sk_argerror(vm, argidx, argmsg);
}


#define tagerror(vm, idx, type) sk_typeerror(vm, idx, sk_tagname(vm, idx))


SK_LIBAPI sk_number sk_checknumber(VM* vm, int idx)
{
    int isnum = 0;
    sk_number n = sk_getnumber(vm, idx, &isnum);
    if(unlikely(!isnum)) tagerror(vm, idx, TT_NUMBER);
    return n;
}


SK_LIBAPI const char* sk_checkstring(VM* vm, int idx)
{
    const char* str = sk_getstring(vm, idx);
    if(unlikely(str == NULL)) tagerror(vm, idx, TT_STRING);
    return str;
}


SK_LIBAPI int sk_checkbool(VM* vm, int idx)
{
    int isbool = 0;
    int b = sk_getbool(vm, idx, &isbool);
    if(unlikely(isbool == 0)) tagerror(vm, idx, TT_BOOL);
    return b;
}


SK_LIBAPI void sk_checktype(VM* vm, int idx, int type)
{
    if(unlikely(sk_type(vm, idx) != type)) tagerror(vm, idx, type);
}


/* File manipulation related error */
static ALE fileerror(VM* vm, const char* action, int32_t idx)
{
    static const char* fmt = "Cannot %s %s: %s.";
    const char* ferr = strerror(errno);
    const char* filename = sk_getstring(vm, idx);
    sk_pushfstring(vm, fmt, action, filename, ferr);
    sk_remove(vm, idx);
    return ALE_FILE;
}


