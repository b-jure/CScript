#include "array.h"
#include "mem.h"

#include <stdlib.h>

#define GROW_CAPACITY(cap) ((cap) < 8 ? 8 : (cap)*2)
#define GROW_ARRAY(type, ptr, old_cap, new_cap)                                          \
    (type*)reallocate(ptr, sizeof(type) * (old_cap), sizeof(type) * (new_cap))

#define DEFINE_ARRAY(type)                                                               \
    uint32_t _ARRAY_METHOD(type, push, type value)                                       \
    {                                                                                    \
        if(self->cap <= self->len) {                                                     \
            size_t old_cap = self->cap;                                                  \
            self->cap      = GROW_CAPACITY(old_cap);                                     \
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
    }                                                                                    \
                                                                                         \
/* Instructions array */
DEFINE_ARRAY(Byte)
/* Line array */
DEFINE_ARRAY(UInt)
/* Constants array */
DEFINE_ARRAY(Value)
