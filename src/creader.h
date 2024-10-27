/*
** creader.h
** Buffered reader
** See Copyright Notice in cscript.h
*/

#ifndef CRREADER_H
#define CRREADER_H

#include "cscript.h"
#include "cmem.h"


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
    cr_Reader reader; /* reader function */
    void* userdata; /* user data for 'crR' */
    cr_State* ts; /* 'cr_State' for 'crR' */
} BuffReader;


CRI_FUNC void crR_init(cr_State* ts, BuffReader* br, cr_Reader freader,
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

#define crR_freebuffer(ts,b)    crR_buffresize(ts, b, 0)


/* string buffer for lexer */
typedef struct Buffer {
  char *str;
  size_t len;
  size_t size;
} Buffer;

#endif
