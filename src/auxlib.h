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

SK_LIBAPI int sk_typeerror(VM* vm, int argidx, const char* tname);
SK_LIBAPI int sk_argerror(VM* vm, int argidx, const char* extra);

SK_LIBAPI sk_number sk_checknumber(VM* vm, int idx);
#define sk_checkinteger(vm, idx) cast(int64_t, sk_checknumber(vm, idx))
SK_LIBAPI const char* sk_checkstring(VM* vm, int idx);
SK_LIBAPI int sk_checkbool(VM* vm, int idx);
SK_LIBAPI void sk_checktype(VM* vm, int idx, int type);

/* ---------------------------------------------------------- */ // aux errors




/* Write to 'stderr' */
#if !defined(sk_writetoerr)
#define sk_writetoerr(msg, ...) (fprintf(stderr, msg), fflush(stderr))
#endif

/* Write to 'stderr' formatted */
#if !defined(sk_writetoerrf)
#define sk_writetoerrf(msg, ...) (fprintf(stderr, msg, __VA_ARGS__), fflush(stderr))
#endif



#endif
