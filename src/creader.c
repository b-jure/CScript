/*
** creader.c
** Buffered reader
** See Copyright Notice in cscript.h
*/

#define creader_c
#define CS_CORE

#include "cscriptprefix.h"

#include "creader.h"
#include "cscriptlimits.h"



void csR_init(cs_State *C, BuffReader *br, cs_Reader freader, void *ud) {
    br->n = 0;
    br->buff = NULL;
    br->reader = freader;
    br->userdata = ud;
    br->C = C;
}


/* 
** Invoke reader returning the first character or CSEOF (-1).
** 'reader' function should set the 'size' to the amount of bytes reader
** read and return the pointer to the start of that buffer. 
** In case there is no more data to be read, 'reader' should set 'size'
** to 0 or return NULL.
*/
int csR_fill(BuffReader *br) {
    size_t size;
    cs_State *C = br->C;
    const char *buff;
    cs_unlock(C);
    buff = br->reader(C, br->userdata, &size);
    cs_lock(C);
    if (buff == NULL || size == 0)
        return CSEOF;
    br->n = size - 1; /* discount char being returned */
    br->buff = buff;
    return cast_uchar(*(br->buff++));
}


/* 
** Read 'n' buffered bytes returning count of unread bytes or 0 if
** all bytes were read. 
*/
size_t csR_readn(BuffReader *br, size_t n) {
    while (n) {
        if (br->n == 0) {
            if (csR_fill(br) == CSEOF)
                return n;
            br->n++; /* 'csR_fill' decremented it */
            br->buff--; /* restore that character */
        }
        size_t min = (br->n <= n ? br->n : n);
        br->n -= min;
        br->buff += min;
        n -= min;
    }
    return 0;
}
