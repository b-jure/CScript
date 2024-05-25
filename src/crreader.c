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

#include "skcommon.h"
#include "skreader.h"

void BuffReader_init(VM* vm, BuffReader* BR, cr_reader reader, void* userdata)
{
    BR->n = 0;
    BR->buff = NULL;
    BR->vm = vm;
    BR->reader = reader;
    BR->userdata = userdata;
}


/* Invoke 'cr_reader' returning the first character or SKEOF (-1).
 * 'cr_reader' should set the 'size' to the amount of bytes
 * reader read and return the pointer to the start of that
 * buffer. */
int32_t BuffReader_fill(BuffReader* BR)
{
    size_t size;
    VM* vm = BR->vm;
    cr_unlock(vm);
    const char* buff = BR->reader(vm, BR->userdata, &size);
    cr_lock(vm);
    if(buff == NULL || size == 0) return SKEOF;
    BR->buff = buff;
    BR->n = size - 1;
    return cast(cr_ubyte, *BR->buff++);
}


/* Read 'n' bytes from 'BuffReader' returning
 * count of unread bytes or 0 if all bytes were read. */
int8_t BuffReader_readn(BuffReader* BR, size_t n)
{
    while(n) {
        size_t min;
        if(BR->n == 0) {
            if(BuffReader_fill(BR) == SKEOF) return n;
            BR->n++; // BR_fill decremented it
            BR->buff--; // Restore that character
        }
        min = (BR->n <= n ? BR->n : n);
        BR->n -= min;
        BR->buff += min;
        n -= min;
    }
    return 0;
}
