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



void crR_init(cr_State *ts, BuffReader *br, cr_fReader freader, void *ud) {
    br->n = 0;
    br->buff = NULL;
    br->reader = freader;
    br->userdata = ud;
    br->ts = ts;
}


/* 
 * Invoke reader returning the first character or CREOF (-1).
 * 'crR' should set the 'size' to the amount of bytes
 * reader read and return the pointer to the start of that
 * buffer. 
 */
int crR_fill(BuffReader *br) {
    cr_State *ts = br->ts;
    size_t size;
    cr_unlock(ts);
    const char *buff = br->reader(ts, br->userdata, &size);
    cr_lock(ts);
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
size_t crR_readn(BuffReader *br, size_t n) {
    while (n) {
        if (br->n == 0) {
            if (crR_fill(br) == CREOF)
                return n;
            br->n++; /* 'crR_fill' decremented it */
            br->buff--; /* restore that character */
        }
        size_t min = (br->n <= n ? br->n : n);
        br->n -= min;
        br->buff += min;
        n -= min;
    }
    return 0;
}
