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
 * ----------------------------------------------------------------------------------------------*/

#ifndef CRHASH_H
#define CRHASH_H

#include "cript.h"


unsigned int hashnumber(cr_number n);
unsigned int hashstring(const char* str, size_t len, unsigned int seed);

#define hashinteger(i)		cast_uint(cr_castS2U((i)))
#define hashboolean(b)		cast_uint((b) != 0)
#define hashpointer(p)		pointer2uint((p))


#endif
