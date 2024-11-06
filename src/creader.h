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
	((br)->n-- > 0 ? cast(cs_ubyte, *(br)->buff++) : csR_fill(br))

/* Go back one character (byte) */
#define brungetc(br)	((br)->n++, (br)->buff--)


typedef struct {
    size_t n; /* unread bytes */
    const char* buff; /* position in buffer */
    cs_Reader reader; /* reader function */
    void* userdata; /* user data for 'crR' */
    cs_State* ts; /* 'cs_State' for 'crR' */
} BuffReader;


CSI_FUNC void csR_init(cs_State* ts, BuffReader* br, cs_Reader freader,
                       void* userdata);
CSI_FUNC int csR_fill(BuffReader* br);
CSI_FUNC size_t csR_readn(BuffReader* br, size_t n);



#define csR_buffinit(b)     {(b)->str = NULL; (b)->len = (b)->size = 0;}

#define csR_buff(b)       ((b)->str)
#define csR_bufflen(b)    ((b)->len)
#define csR_buffsize(b)   ((b)->size)

#define csR_buffpop(b)        ((b)->len -= 1)
#define csR_buffreset(b)      ((b)->len = 0)

#define csR_buffresize(ts,b,s) \
    { (b)->str = csM_saferealloc(ts, (b)->str, (b)->size, s); \
      (b)->size = s; }

#define csR_freebuffer(ts,b)    csR_buffresize(ts, b, 0)


/* string buffer for lexer */
typedef struct Buffer {
  char *str;
  size_t len;
  size_t size;
} Buffer;

#endif
