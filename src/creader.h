/*
** creader.h
** Buffered reader
** See Copyright Notice in cscript.h
*/

#ifndef CREADER_H
#define CREADER_H


#include "cscript.h"
#include "cmem.h"


/* end of file */
#define CSEOF	(-1)


/* Return next char and progress the buffer or try fill the buffer. */
#define brgetc(br) \
	((br)->n-- > 0 ? cast(c_byte, *(br)->buff++) : csR_fill(br))


typedef struct {
    size_t n; /* unread bytes */
    const char* buff; /* position in buffer */
    cs_Reader reader; /* reader function */
    void* userdata; /* user data for 'crR' */
    cs_State* C; /* 'cs_State' for 'crR' */
} BuffReader;


CSI_FUNC void csR_init(cs_State* C, BuffReader* br, cs_Reader freader,
                       void* userdata);
CSI_FUNC int csR_fill(BuffReader* br);
CSI_FUNC size_t csR_readn(BuffReader* br, size_t n);



#define csR_buffinit(b)     {(b)->str = NULL; (b)->len = (b)->size = 0;}

#define csR_buff(b)       ((b)->str)
#define csR_bufflen(b)    ((b)->len)
#define csR_buffsize(b)   ((b)->size)

#define csR_buffpop(b)          ((b)->len -= 1)
#define csR_buffreset(b)        ((b)->len = 0)
#define csR_buffpopn(b,n)       ((b)->len -= (n))

#define csR_buffresize(C,b,s) \
    { (b)->str = csM_saferealloc(C, (b)->str, (b)->size, s); \
      (b)->size = s; }

#define csR_freebuffer(C,b)     csR_buffresize(C, b, 0)


/* string buffer for lexer */
typedef struct Buffer {
  char *str;
  size_t len;
  size_t size;
} Buffer;

#endif
