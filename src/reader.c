#include "common.h"
#include "reader.h"

void BuffReader_init(VM* vm, BuffReader* BR, sk_reader reader, void* userdata)
{
    BR->n = 0;
    BR->buff = NULL;
    BR->vm = vm;
    BR->reader = reader;
    BR->userdata = userdata;
}


/* Invoke 'sk_reader' returning the first character or SKEOF (-1).
 * 'sk_reader' should set the 'size' to the amount of bytes
 * reader read and return the pointer to the start of that
 * buffer. */
int32_t BuffReader_fill(BuffReader* BR)
{
    size_t size;
    VM* vm = BR->vm;
    sk_unlock(vm);
    const char* buff = BR->reader(vm, BR->userdata, &size);
    sk_lock(vm);
    if(buff == NULL || size == 0) return SKEOF;
    BR->buff = buff;
    BR->n = size - 1;
    return cast(uint8_t, *BR->buff++);
}


/* Read 'n' bytes from 'BuffReader' returning
 * count of unread bytes or 0 if all bytes were read. */
int8_t BuffReader_readn(BuffReader* BR, size_t n)
{
    while(n) {
        size_t min;
        if(BR->n == 0) {
            if(BuffReader_fill(BR) == SKEOF) return n;
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
