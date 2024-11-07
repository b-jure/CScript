/*
** creader.c
** Buffered reader
** See Copyright Notice in cscript.h
*/


#define CS_CORE


#include "creader.h"
#include "climits.h"



void csR_init(cs_State *ts, BuffReader *br, cs_Reader freader, void *ud) {
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
int csR_fill(BuffReader *br) {
    cs_State *ts = br->ts;
    size_t size;
    cs_unlock(ts);
    const char *buff = br->reader(ts, br->userdata, &size);
    cs_lock(ts);
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
size_t csR_readn(BuffReader *br, size_t n) {
    while (n) {
        if (br->n == 0) {
            if (csR_fill(br) == CREOF)
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
