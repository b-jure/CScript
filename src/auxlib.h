#ifndef SKAUXLIB_H
#define SKAUXLIB_H

#include "skooma.h"

SK_LIBAPI SK_Number   sk_checknumber(VM* vm, int idx);
SK_LIBAPI const char* sk_checkstring(VM* vm, int idx);
SK_LIBAPI void        sk_checktype(VM* vm, int idx, int type);

#endif
