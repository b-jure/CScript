#ifndef SKAUXLIB_H
#define SKAUXLIB_H


#include "skooma.h"

SK_LIBAPI int sk_typeerror(VM* vm, int argidx, const char* tname);
SK_LIBAPI int sk_argerror(VM* vm, int argidx, const char* extra);

SK_LIBAPI sk_number sk_checknumber(VM* vm, int idx);
#define sk_checkinteger(vm, idx) cast(int64_t, sk_checknumber(vm, idx))
SK_LIBAPI const char* sk_checkstring(VM* vm, int idx);
SK_LIBAPI int sk_checkbool(VM* vm, int idx);
SK_LIBAPI void sk_checktype(VM* vm, int idx, int type);


#endif
