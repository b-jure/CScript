#ifndef __SKOOMA_ARRAY_H__
#define __SKOOMA_ARRAY_H__

#include "common.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

// Internal --------------------------------------------------
#define _CALL_ARRAY_METHOD(tname, name, ...)                                             \
    _ARRAY_METHOD_NAME(tname, name)(self __VA_OPT__(, ) __VA_ARGS__)
#define _ARRAY_METHOD_NAME(tname, name) tname##_##name
#define _ARRAY_METHOD(tname, name, ...)                                                  \
    _ARRAY_METHOD_NAME(tname, name)                                                      \
    (tname * self __VA_OPT__(, ) __VA_ARGS__)
// -----------------------------------------------------------

#ifndef SK_ALLOCATOR // @MAYBE_REMOVE?
void* reallocate(void* ptr, size_t bytes);
#endif

typedef void* (*AllocatorFn)(void* roots, void* ptr, size_t oldsize, size_t newsize);

static force_inline void*
arr_reallocate(unused void* _, void* ptr, unused size_t __, size_t newsize)
{
    return reallocate(ptr, newsize);
}

/* Returns the new array capacity */
#define GROW_ARRAY_CAPACITY(cap) ((cap) < 8 ? 8 : (cap)*2)

#define ARRAY_NEW(name, type)                                                            \
    typedef struct {                                                                     \
        void*       roots;                                                               \
        AllocatorFn allocator;                                                           \
        size_t      cap;                                                                 \
        size_t      len;                                                                 \
        type*       data;                                                                \
    } name;                                                                              \
                                                                                         \
    force_inline void _ARRAY_METHOD(name, init, void* roots, AllocatorFn allocfn)        \
    {                                                                                    \
        self->roots     = roots;                                                         \
        self->allocator = allocfn;                                                       \
        self->cap       = 0;                                                             \
        self->len       = 0;                                                             \
        self->data      = NULL;                                                          \
    }                                                                                    \
                                                                                         \
    force_inline void _ARRAY_METHOD(name, init_cap, uint32_t cap)                        \
    {                                                                                    \
        self->data = (type*)self->allocator(                                             \
            self->roots,                                                                 \
            self->data,                                                                  \
            self->cap * sizeof(type),                                                    \
            cap * sizeof(type));                                                         \
        self->cap = cap;                                                                 \
    }                                                                                    \
                                                                                         \
    force_inline UInt _ARRAY_METHOD(name, push, type value)                              \
    {                                                                                    \
        if(self->cap <= self->len) {                                                     \
            size_t old_cap = self->cap;                                                  \
            self->cap      = GROW_ARRAY_CAPACITY(old_cap);                               \
                                                                                         \
            if(unlikely(self->cap >= UINT32_MAX)) {                                      \
                fprintf(                                                                 \
                    stderr,                                                              \
                    "Internal error, " #name " capacity exceeded! [%lu]\n",              \
                    self->cap);                                                          \
                exit(ENOMEM);                                                            \
            } else {                                                                     \
                self->data = (type*)self->allocator(                                     \
                    self->roots,                                                         \
                    self->data,                                                          \
                    old_cap * sizeof(type),                                              \
                    self->cap * sizeof(type));                                           \
            }                                                                            \
        }                                                                                \
        self->data[self->len++] = value;                                                 \
        return self->len - 1;                                                            \
    }                                                                                    \
                                                                                         \
    force_inline type _ARRAY_METHOD(name, pop)                                           \
    {                                                                                    \
        return self->data[--self->len];                                                  \
    }                                                                                    \
                                                                                         \
    force_inline type* _ARRAY_METHOD(name, index, size_t index)                          \
    {                                                                                    \
        return &self->data[index];                                                       \
    }                                                                                    \
                                                                                         \
    force_inline type* _ARRAY_METHOD(name, last)                                         \
    {                                                                                    \
        return &self->data[self->len - 1];                                               \
    }                                                                                    \
                                                                                         \
    force_inline type* _ARRAY_METHOD(name, first)                                        \
    {                                                                                    \
        return &self->data[0];                                                           \
    }                                                                                    \
                                                                                         \
    force_inline void _ARRAY_METHOD(name, insert, size_t index, type value)              \
    {                                                                                    \
        type* src  = self->data + index;                                                 \
        type* dest = src + 1;                                                            \
        memmove(dest, src, self->len - index);                                           \
        self->len++;                                                                     \
        self->data[index] = value;                                                       \
    }                                                                                    \
                                                                                         \
    force_inline type _ARRAY_METHOD(name, remove, size_t index)                          \
    {                                                                                    \
        if(self->len == 1) {                                                             \
            return _CALL_ARRAY_METHOD(name, pop);                                        \
        }                                                                                \
        type* src    = self->data + index;                                               \
        type* dest   = src - 1;                                                          \
        type  retval = *src;                                                             \
        memmove(dest, src, self->len - index);                                           \
        self->len--;                                                                     \
        return retval;                                                                   \
    }                                                                                    \
                                                                                         \
    force_inline size_t _ARRAY_METHOD(name, len)                                         \
    {                                                                                    \
        return self->len;                                                                \
    }                                                                                    \
                                                                                         \
    force_inline void _ARRAY_METHOD(name, free)                                          \
    {                                                                                    \
        self->allocator(self->roots, self->data, self->cap * sizeof(type), 0);           \
        _CALL_ARRAY_METHOD(name, init, self->roots, self->allocator);                    \
    }

#endif
