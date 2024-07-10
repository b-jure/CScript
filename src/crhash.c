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

#include "crhash.h"
#include "crlimits.h"


/* hash 'cr_number' */
unsigned int cr_hash_number(cr_number n)
{
	unsigned int ui;
	cr_integer ni;
	int exp;
	
	n = cr_mathop(frexp(n, &exp)) * -cast_num(INT_MIN);
	if (cr_likely(cr_number2integer(n,&ni))) {
		ui = cast_uint(exp) + cast_uint(ni);
		return (ui <= cast_uint(INT_MAX) ? ui : cast_int(~ui));
	}
	cr_assert(cr_numisnan(n) || cr_mathop(fabs)(n) == cast_num(HUGE_VAL));
	return 0;
}

/* 
 * Hash string.
 * One-byte-at-a-time hash based on Murmur's mix
 * Source: https://github.com/aappleby/smhasher/blob/master/src/Hashes.cpp 
 */
unsigned int cr_hash_string(const char *str, size_t len, unsigned int seed)
{
	const cr_ubyte *data = cast(const cr_ubyte *, str);
	unsigned int h = seed;
	int i;

	for (i = 0; i < len; i++) {
		h ^= data[i];
		h *= 0x5bd1e995;
		h ^= h >> 15;
	}
	return h;
}
