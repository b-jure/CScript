#ifndef SKREADER_H
#define SKREADER_H

#include "skooma.h"

#define SKEOF -1

typedef struct {
    size_t n; /* unread bytes */
    const char* buff; /* position in buffer */
    ReadFn reader; /* reader function */
    void* userdata; /* user data for 'ReadFn' */
    VM* vm; /* 'VM' for 'ReadFn' */
} BuffReader;

/* Return next char and progress the buffer or try fill the buffer. */
#define brgetc(br) ((br)->n-- > 0 ? *(br)->buff++ : BR_fill(br))

/* Peek previous char. */
#define brprevc(br)                                                                      \
    (sk_assert((br)->vm, (br)->buff != NULL, "NULL buff"), *((br)->buff - 1))

/* Progress the buffer back by one char. */
#define brungetc(br)                                                                     \
    {                                                                                    \
        sk_assert((br)->vm, (br)->buff != NULL, "NULL buff");                            \
        (br)->buff--;                                                                    \
        (br)->n++;                                                                       \
    }


void BR_init(VM* vm, BuffReader* br, ReadFn reader, void* userdata);

int8_t BR_fill(BuffReader* br);

void BR_backone(BuffReader* br);

int8_t BR_readn(BuffReader* br, size_t n);

#endif
