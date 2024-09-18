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
#include "crmem.h"


/* end of file */
#define CREOF	(-1)


/* Return next char and progress the buffer or try fill the buffer. */
#define brgetc(br) \
	((br)->n-- > 0 ? cast(cr_ubyte, *(br)->buff++) : crR_fill(br))

/* Go back one character (byte) */
#define brungetc(br)	((br)->n++, (br)->buff--)


typedef struct {
    size_t n; /* unread bytes */
    const char* buff; /* position in buffer */
    cr_fReader reader; /* reader function */
    void* userdata; /* user data for 'crR' */
    cr_State* ts; /* 'cr_State' for 'crR' */
} BuffReader;


CRI_FUNC void crR_init(cr_State* ts, BuffReader* br, cr_fReader freader,
                       void* userdata);
CRI_FUNC int crR_fill(BuffReader* br);
CRI_FUNC size_t crR_readn(BuffReader* br, size_t n);



#define crR_buffinit(b)     {(b)->str = NULL; (b)->len = (b)->size = 0;}

#define crR_buff(b)       ((b)->str)
#define crR_bufflen(b)    ((b)->len)
#define crR_buffsize(b)   ((b)->size)

#define crR_buffpop(b)        ((b)->len -= 1)
#define crR_buffreset(b)      ((b)->len = 0)

#define crR_buffresize(ts,b,s) \
    { (b)->str = crM_saferealloc(ts, (b)->str, (b)->size, s); \
      (b)->size = s; }


/* string buffer for lexer */
typedef struct Buffer {
  char *str;
  size_t len;
  size_t size;
} Buffer;

#endif
