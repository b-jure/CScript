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

#ifndef CRREADER_H
#define CRREADER_H

#include "cript.h"


/* end of file */
#define CREOF	(-1)


/* Return next char and progress the buffer or try fill the buffer. */
#define brgetc(br) \
	((br)->n-- > 0 ? cast(cr_ubyte, *(br)->buff++) : cr_br_fill(br))

/* Go back one character (byte) */
#define brungetc(br)	((br)->n++, (br)->buff--)


typedef struct {
    size_t n; /* unread bytes */
    const char* buff; /* position in buffer */
    cr_reader reader; /* reader function */
    void* userdata; /* user data for 'cr_reader' */
    VM* vm; /* 'VM' for 'cr_reader' */
} BuffReader;


void cr_br_init(VM* vm, BuffReader* br, cr_reader reader, void* userdata);
int cr_br_fill(BuffReader* br);
size_t cr_br_readn(BuffReader* br, size_t n);

#endif
