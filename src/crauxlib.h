/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ---------------------------------------------------------------------------------------------- */

#ifndef CRAUXLIB_H
#define CRSKAUXLIB_H


#include "cript.h"


/* ================= create VM (auxlib allocator) ================= */
CR_LIBAPI VM* skaux_create(void);
/* ---------------------------------------------------------- */


/* ================= debug ================= */
CR_LIBAPI int skaux_error(VM* vm, cr_status errcode, const char* fmt, ...);
CR_LIBAPI int skaux_typeerror(VM* vm, int argidx, const char* tname);
CR_LIBAPI int skaux_argerror(VM* vm, int argidx, const char* extra);

CR_LIBAPI cr_double skaux_checknumber(VM* vm, int idx);
CR_LIBAPI const char* skaux_checkstring(VM* vm, int idx);
CR_LIBAPI const char* skaux_optstring(VM* vm, int idx);
CR_LIBAPI cr_ubyte skaux_checkbool(VM* vm, int idx);
CR_LIBAPI void skaux_checktype(VM* vm, int idx, int type);
CR_LIBAPI void skaux_checkstack(VM* vm, int space, const char* msg);
CR_LIBAPI void skaux_where(VM* vm, int level);
/* ---------------------------------------------------------- */


/* ================= load functions (protected) ================= */
CR_LIBAPI cr_status skaux_loadfile(VM* vm, const char* filename);
CR_LIBAPI cr_status skaux_loadstring(VM* vm, const char* string);
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
