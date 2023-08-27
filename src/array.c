#include "array.h"
#include "hashtable.h"
#include "mem.h"

#include <stdlib.h>

#define DEFINE_ARRAY(type)                                                               \
    uint32_t _ARRAY_METHOD(type, push, type value)                                       \
    {                                                                                    \
        if(self->cap <= self->len) {                                                     \
            size_t old_cap = self->cap;                                                  \
            self->cap      = GROW_ARRAY_CAPACITY(old_cap);                               \
                                                                                         \
            if(_unlikely(self->cap >= UINT32_MAX)) {                                     \
                exit(EXIT_FAILURE);                                                      \
            } else {                                                                     \
                self->data = GROW_ARRAY(type, self->data, old_cap, self->cap);           \
            }                                                                            \
        }                                                                                \
        self->data[self->len++] = value;                                                 \
        return self->len - 1;                                                            \
    }                                                                                    \
                                                                                         \
    void _ARRAY_METHOD(type, insert, size_t index, type value)                           \
    {                                                                                    \
        type* src  = self->data + index;                                                 \
        type* dest = src + 1;                                                            \
        memmove(dest, src, self->len - index);                                           \
        self->data[index] = value;                                                       \
    }                                                                                    \
                                                                                         \
    type _ARRAY_METHOD(type, remove, size_t index)                                       \
    {                                                                                    \
        type* src    = self->data + index;                                               \
        type* dest   = src - 1;                                                          \
        type  retval = self->data[index];                                                \
        memmove(dest, src, self->len - index);                                           \
        return retval;                                                                   \
    }

DEFINE_ARRAY(Byte);  /* Instructions array */
DEFINE_ARRAY(UInt);  /* Line array */
DEFINE_ARRAY(Value); /* Constants array */
