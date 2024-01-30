#ifndef SKAUXLIB_H
#define SKAUXLIB_H


#include "skooma.h"


/* ================= Create VM (auxlib allocator) ================= */
SK_LIBAPI VM* skaux_create(void);
/* ---------------------------------------------------------- */


/* ================= Auxiliary type checking ================= */
SK_LIBAPI sk_int skaux_typeerror(VM* vm, sk_int argidx, const char* tname);
SK_LIBAPI sk_int skaux_argerror(VM* vm, sk_int argidx, const char* extra);

SK_LIBAPI sk_number skaux_checknumber(VM* vm, sk_int idx);
SK_LIBAPI const char* skaux_checkstring(VM* vm, sk_int idx);
SK_LIBAPI sk_byte skaux_checkbool(VM* vm, sk_int idx);
SK_LIBAPI void skaux_checktype(VM* vm, sk_int idx, sk_int type);
/* ---------------------------------------------------------- */


/* ================= Load functions ================= */
SK_LIBAPI sk_status skaux_loadfile(VM* vm, const char* filename);
SK_LIBAPI sk_status skaux_loadstring(VM* vm, const char* string);
/* ---------------------------------------------------------- */


/* ================= Write to stderr ================= */
#if !defined(skaux_writetoerr)
#define skaux_writetoerr(msg, ...) (fprintf(stderr, msg), fflush(stderr))
#endif

#if !defined(skaux_writetoerrf)
#define skaux_writetoerrf(msg, ...) (fprintf(stderr, msg, __VA_ARGS__), fflush(stderr))
#endif
/* ---------------------------------------------------------- */


#endif
