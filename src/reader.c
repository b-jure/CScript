#include "common.h"
#include "reader.h"

void BR_init(VM* vm, BuffReader* BR, ReadFn reader, void* userdata)
{
    BR->n = 0;
    BR->buff = NULL;
    BR->vm = vm;
    BR->reader = reader;
    BR->userdata = userdata;
}


/* Invoke 'ReadFn' returning the size read and the current
 * buffer position.
 * In case the buffer position is NULL and size is 0 return SKEOF (-1).
 * Otherwise progress the buffer and return the read character */
int8_t BR_fill(BuffReader* BR)
{
    size_t size;
    VM* vm = BR->vm;
    sk_unlock(vm);
    const char* buff = BR->reader(vm, BR->userdata, &size);
    sk_lock(vm);
    if(buff == NULL || size == 0) return SKEOF;
    BR->buff = buff;
    BR->n = size - 1;
    return cast_uchar(*BR->buff++);
}


/* Read 'n' bytes from 'BuffReader' returning
 * count of unread bytes or 0 if all bytes were read. */
int8_t BR_readn(BuffReader* BR, size_t n)
{
    while(n) {
        size_t min;
        if(BR->n == 0) {
            if(BR_fill(BR) == SKEOF) return n;
            BR->n++; // BR_fill decremented it
            BR->buff--; // Restore that character
        }
        min = (BR->n <= n ? BR->n : n);
        BR->n -= min;
        BR->buff += min;
        n -= min;
    }
    return 0;
}
