/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Skooma.
 * Skooma is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Skooma is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Skooma.
 * If not, see <https://www.gnu.org/licenses/>.
 * ---------------------------------------------------------------------------------------------- */

#ifndef SKAUXLIB_H
#define SKAUXLIB_H


#include "skooma.h"


/* ================= create VM (auxlib allocator) ================= */
SK_LIBAPI VM* skaux_create(void);
/* ---------------------------------------------------------- */


/* ================= debug ================= */
SK_LIBAPI sk_int skaux_error(VM* vm, sk_status errcode, const char* fmt, ...);
SK_LIBAPI sk_int skaux_typeerror(VM* vm, sk_int argidx, const char* tname);
SK_LIBAPI sk_int skaux_argerror(VM* vm, sk_int argidx, const char* extra);

SK_LIBAPI sk_number skaux_checknumber(VM* vm, sk_int idx);
SK_LIBAPI const char* skaux_checkstring(VM* vm, sk_int idx);
SK_LIBAPI const char* skaux_optstring(VM* vm, sk_int idx);
SK_LIBAPI sk_byte skaux_checkbool(VM* vm, sk_int idx);
SK_LIBAPI void skaux_checktype(VM* vm, sk_int idx, sk_int type);
SK_LIBAPI void skaux_checkstack(VM* vm, sk_int space, const char* msg);
SK_LIBAPI void skaux_where(VM* vm, sk_uint level);
/* ---------------------------------------------------------- */


/* ================= load functions (protected) ================= */
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
