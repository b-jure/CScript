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


unsigned int cr_hash_number(cr_number n);
unsigned int cr_hash_string(const char* str, size_t len, unsigned int seed);

#define cr_hh_integer(i)	cast_uint(cri_castS2U((i)))
#define cr_hh_boolean(b)	cast_uint((b) != 0)
#define cr_hh_pointer(p)	pointer2uint((p))


#endif
