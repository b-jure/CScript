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
#define brgetc(br) ((br)->n-- > 0 ? cast(uint8_t, *(br)->buff++) : BuffReader_fill(br))
/* Go back one character (byte) */
#define brungetc(br) ((br)->n++, (br)->buff--)

void BuffReader_init(VM* vm, BuffReader* br, ReadFn reader, void* userdata);

int32_t BuffReader_fill(BuffReader* br);

int8_t BuffReader_readn(BuffReader* br, size_t n);

#endif
