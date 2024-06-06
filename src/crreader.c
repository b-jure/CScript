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


#include "crreader.h"
#include "crlimits.h"



void cr_br_init(VM *vm, BuffReader *br, cr_reader reader, void *userdata)
{
	br->n = 0;
	br->buff = NULL;
	br->reader = reader;
	br->userdata = userdata;
	br->vm = vm;
}


/* 
 * Invoke 'cr_reader' returning the first character or CREOF (-1).
 * 'cr_reader' should set the 'size' to the amount of bytes
 * reader read and return the pointer to the start of that
 * buffer. 
 */
int cr_br_fill(BuffReader *br)
{
	VM *vm;
	size_t size;
	const char *buff;

	vm = br->vm;
	cr_unlock(vm);
	buff = br->reader(vm, br->userdata, &size);
	cr_lock(vm);
	if (buff == NULL || size == 0)
		return CREOF;
	br->buff = buff;
	br->n = size - 1;
	return *br->buff++;
}


/* 
 * Read 'n' bytes from 'BuffReader' returning
 * count of unread bytes or 0 if all bytes were read. 
 */
size_t cr_br_readn(BuffReader *br, size_t n)
{
	size_t min;

	while (n) {
		if (br->n == 0) {
			if (cr_br_fill(br) == CREOF)
				return n;
			br->n++; /* cr_br_fill decremented it */
			br->buff--; /* restore that character */
		}
		min = (br->n <= n ? br->n : n);
		br->n -= min;
		br->buff += min;
		n -= min;
	}
	return 0;
}
