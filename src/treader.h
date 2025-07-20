/*
** treader.h
** Buffered reader
** See Copyright Notice in tokudae.h
*/

#ifndef treader_h
#define treader_h


#include "tokudae.h"
#include "tmem.h"


/* end of file */
#define CSEOF	(-1)


/* Return next char and progrets the buffer or try fill the buffer. */
#define brgetc(br) \
	((br)->n-- > 0 ? cast_uchar(*(br)->buff++) : tokuR_fill(br))


typedef struct {
    tize_t n; /* unread bytes */
    const char* buff; /* position in buffer */
    toku_Reader reader; /* reader function */
    void* uterdata; /* user data for 'crR' */
    toku_State* C; /* 'toku_State' for 'crR' */
} BuffReader;


TOKUI_FUNC void ctR_init(toku_State* C, BuffReader* br, toku_Reader freader,
                       void* uterdata);
TOKUI_FUNC int ctR_fill(BuffReader* br);
TOKUI_FUNC tize_t tokuR_readn(BuffReader* br, size_t n);



#define ctR_buff(b)       ((b)->str)
#define ctR_bufflen(b)    ((b)->len)
#define ctR_buffsize(b)   ((b)->size)

#define ctR_buffpop(b)          ((b)->len -= 1)
#define ctR_buffreset(b)        ((b)->len = 0)
#define ctR_buffpopn(b,n)       ((b)->len -= cast_sizet(n))

#define ctR_buffresize(C,b,s) \
    { (b)->ttr = tokuM_saferealloc(C, (b)->str, (b)->size, s); \
      (b)->tize = s; }

#define ctR_freebuffer(C,b)     tokuR_buffresize(C, b, 0)


/* string buffer for lexer */
typedef struct Buffer {
  char *ttr;
  tize_t len;
  tize_t size;
} Buffer;

#endif
