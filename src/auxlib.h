#ifndef SKAUXLIB_H
#define SKAUXLIB_H


#include "skooma.h"




/*
 * ================= AUXLIB error codes =================
 */

typedef enum {
    ALE_FILE = S_CNT, // file related error
} ALE;

/* ---------------------------------------------------------- */ // error codes





/*
 * ================= Create VM (auxlib allocator) =================
 */

SK_LIBAPI VM* skaux_create(void);

/* ---------------------------------------------------------- */ // create VM





/*
 * ================= Auxiliary library errors =================
 */

SK_LIBAPI int skaux_typeerror(VM* vm, int argidx, const char* tname);
SK_LIBAPI int skaux_argerror(VM* vm, int argidx, const char* extra);

SK_LIBAPI sk_number skaux_checknumber(VM* vm, int idx);
#define skaux_checkinteger(vm, idx) cast(int64_t, skaux_checknumber(vm, idx))
SK_LIBAPI const char* skaux_checkstring(VM* vm, int idx);
SK_LIBAPI int skaux_checkbool(VM* vm, int idx);
SK_LIBAPI void skaux_checktype(VM* vm, int idx, int type);

/* ---------------------------------------------------------- */ // aux errors




/* Write to 'stderr' */
#if !defined(skaux_writetoerr)
#define skaux_writetoerr(msg, ...) (fprintf(stderr, msg), fflush(stderr))
#endif

/* Write to 'stderr' formatted */
#if !defined(skaux_writetoerrf)
#define skaux_writetoerrf(msg, ...) (fprintf(stderr, msg, __VA_ARGS__), fflush(stderr))
#endif



#endif
