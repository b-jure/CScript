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
 * ----------------------------------------------------------------------------------------------*/

#ifndef SKREADER_H
#define SKREADER_H

#include "skooma.h"

#define SKEOF -1

typedef struct {
    size_t n; /* unread bytes */
    const char* buff; /* position in buffer */
    sk_reader reader; /* reader function */
    void* userdata; /* user data for 'sk_reader' */
    VM* vm; /* 'VM' for 'sk_reader' */
} BuffReader;

/* Return next char and progress the buffer or try fill the buffer. */
#define brgetc(br) ((br)->n-- > 0 ? cast(uint8_t, *(br)->buff++) : BuffReader_fill(br))
/* Go back one character (byte) */
#define brungetc(br) ((br)->n++, (br)->buff--)

void BuffReader_init(VM* vm, BuffReader* br, sk_reader reader, void* userdata);

int32_t BuffReader_fill(BuffReader* br);

int8_t BuffReader_readn(BuffReader* br, size_t n);

#endif
