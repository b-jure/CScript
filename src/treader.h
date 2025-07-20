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
#define TOKUEOF	    (-1)


/* Return next char and progress the buffer or try fill the buffer. */
#define brgetc(br) \
	((br)->n-- > 0 ? cast_uchar(*(br)->buff++) : tokuR_fill(br))


typedef struct {
    size_t n; /* unread bytes */
    const char* buff; /* position in buffer */
    toku_Reader reader; /* reader function */
    void* userdata; /* user data for 'reader' */
    toku_State* T; /* 'toku_State' for 'reader' */
} BuffReader;


TOKUI_FUNC void tokuR_init(toku_State* C, BuffReader* br, toku_Reader freader,
                           void* userdata);
TOKUI_FUNC int tokuR_fill(BuffReader* br);
TOKUI_FUNC size_t tokuR_readn(BuffReader* br, size_t n);



#define tokuR_buff(b)       ((b)->str)
#define tokuR_bufflen(b)    ((b)->len)
#define tokuR_buffsize(b)   ((b)->size)

#define tokuR_buffpop(b)        ((b)->len -= 1)
#define tokuR_buffreset(b)      ((b)->len = 0)
#define tokuR_buffpopn(b,n)     ((b)->len -= cast_sizet(n))

#define tokuR_buffresize(C,b,s) \
    { (b)->str = tokuM_saferealloc(C, (b)->str, (b)->size, s); \
      (b)->size = s; }

#define tokuR_freebuffer(C,b)   tokuR_buffresize(C, b, 0)


/* string buffer for lexer */
typedef struct Buffer {
  char *str;
  size_t len;
  size_t size;
} Buffer;

#endif
