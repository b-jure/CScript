/*
** treader.c
** Buffered reader
** See Copyright Notice in tokudae.h
*/

#define treader_c
#define TOKU_CORE

#include "tokudaeprefix.h"

#include "treader.h"
#include "tokudaelimits.h"



void tokuR_init(toku_State *T, BuffReader *br, toku_Reader freader, void *ud) {
    br->n = 0;
    br->buff = NULL;
    br->reader = freader;
    br->userdata = ud;
    br->T = T;
}


/* 
** Invoke reader returning the first character or TOKUEOF (-1).
** 'reader' function should set the 'size' to the amount of bytes reader
** read and return the pointer to the start of that buffer. 
** In case there is no more data to be read, 'reader' should set 'size'
** to 0 or return NULL.
*/
int tokuR_fill(BuffReader *br) {
    size_t size;
    toku_State *T = br->T;
    const char *buff;
    toku_unlock(T);
    buff = br->reader(T, br->userdata, &size);
    toku_lock(T);
    if (buff == NULL || size == 0)
        return TOKUEOF;
    br->n = size - 1; /* discount char being returned */
    br->buff = buff;
    return cast_uchar(*(br->buff++));
}


/* 
** Read 'n' buffered bytes returning count of unread bytes or 0 if
** all bytes were read. 
*/
size_t tokuR_readn(BuffReader *br, size_t n) {
    while (n) {
        if (br->n == 0) {
            if (tokuR_fill(br) == TOKUEOF)
                return n;
            br->n++; /* 'tokuR_fill' decremented it */
            br->buff--; /* restore that character */
        }
        size_t min = (br->n <= n ? br->n : n);
        br->n -= min;
        br->buff += min;
        n -= min;
    }
    return 0;
}
