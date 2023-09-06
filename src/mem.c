#include "mem.h"

#include <errno.h>
#include <stdlib.h>

void* reallocate(void* ptr, unused size_t oldc, size_t newc)
{
    if(newc == 0) {
        free(ptr);
        return NULL;
    }

    void* alloc = realloc(ptr, newc);

    if(alloc == NULL) {
        exit(errno);
    }

    return alloc;
}

char* strncopy(const char* src, UInt len)
{
    char* str = MALLOC(len + 1);
    memcpy(str, src, len);
    str[len] = '\0';
    return str;
}
