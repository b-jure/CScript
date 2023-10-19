#ifndef __SKOOMA_ARRAY_H__
#define __SKOOMA_ARRAY_H__

#include "common.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#define GROW_ARRAY_CAPACITY(cap) ((cap) < 8 ? 8 : (cap) * 2)

/* CONSTANTS ARRAY (Chunk.h) */
#define CARRAY_INIT(chunk)                                                               \
    do {                                                                                 \
        (chunk)->constants = NULL;                                                       \
        (chunk)->clen      = 0;                                                          \
        (chunk)->ccap      = 0;                                                          \
    } while(false)

#define CARRAY_PUSH(chunk, constant, vm, compiler)                                       \
    ({                                                                                   \
        if((chunk)->ccap <= (chunk)->clen) {                                             \
            UInt oldcap        = (chunk)->ccap;                                          \
            (chunk)->ccap      = MIN(GROW_ARRAY_CAPACITY(oldcap), UINT24_MAX);           \
            (chunk)->constants = gc_reallocate(                                          \
                vm,                                                                      \
                compiler,                                                                \
                (chunk)->constants,                                                      \
                oldcap * sizeof(Value),                                                  \
                (chunk)->ccap * sizeof(Value));                                          \
        }                                                                                \
        (chunk)->constants[(chunk)->clen++] = constant;                                  \
        (chunk)->clen - 1;                                                               \
    })

#define CARRAY_FREE(chunk, vm, compiler)                                                 \
    do {                                                                                 \
        gc_reallocate(vm, compiler, (chunk)->constants, (chunk)->ccap, 0);               \
        CARRAY_INIT(chunk);                                                              \
    } while(false)



/* GLOBALS ARRAY (vm.h) */
#define GARRAY_INIT(vm)                                                                  \
    do {                                                                                 \
        (vm)->globvals = NULL;                                                           \
        (vm)->globlen  = 0;                                                              \
        (vm)->globcap  = 0;                                                              \
    } while(false)

#define GARRAY_PUSH(vm, compiler, global)                                                \
    ({                                                                                   \
        if((vm)->globcap <= (vm)->globlen) {                                             \
            UInt oldcap    = (vm)->globcap;                                              \
            (vm)->globcap  = MIN(GROW_ARRAY_CAPACITY(oldcap), UINT24_MAX);               \
            (vm)->globvals = gc_reallocate(                                              \
                vm,                                                                      \
                compiler,                                                                \
                (vm)->globvals,                                                          \
                oldcap * sizeof(Global),                                                 \
                (vm)->globcap * sizeof(Global));                                         \
        }                                                                                \
        (vm)->globvals[(vm)->globlen++] = global;                                        \
        (vm)->globlen - 1;                                                               \
    })

#define GARRAY_REMOVE(vm, index)                                                         \
    ({                                                                                   \
        Global  retval;                                                                  \
        Global* src = &vm->globvals[index];                                              \
        memcpy(&retval, src, sizeof(Global));                                            \
        memmove(src, src + 1, vm->globlen - index);                                      \
        vm->globlen--;                                                                   \
        retval;                                                                          \
    })

#define GARRAY_FREE(vm)                                                                  \
    do {                                                                                 \
        gc_reallocate(vm, NULL, (vm)->globvals, (vm)->globcap, 0);                       \
        GARRAY_INIT(vm);                                                                 \
    } while(false)




/* GENERIC ARRAY (does not trigger garbage collector)
 *
 * These are internal only macros. */
#define _CALL_ARRAY_METHOD(tname, name, ...)                                             \
    _ARRAY_METHOD_NAME(tname, name)(self __VA_OPT__(, ) __VA_ARGS__)
#define _ARRAY_METHOD_NAME(tname, name) tname##_##name
#define _ARRAY_METHOD(tname, name, ...)                                                  \
    _ARRAY_METHOD_NAME(tname, name)                                                      \
    (tname * self __VA_OPT__(, ) __VA_ARGS__)


typedef void (*FreeFn)(void* value);

/* Create new 'name' array with 'type' elements. */
#define ARRAY_NEW(name, type)                                                            \
    typedef struct {                                                                     \
        size_t cap;                                                                      \
        size_t len;                                                                      \
        type*  data;                                                                     \
    } name;                                                                              \
                                                                                         \
    force_inline void _ARRAY_METHOD(name, init)                                          \
    {                                                                                    \
        self->cap  = 0;                                                                  \
        self->len  = 0;                                                                  \
        self->data = NULL;                                                               \
    }                                                                                    \
                                                                                         \
    force_inline void _ARRAY_METHOD(name, init_cap, uint32_t cap)                        \
    {                                                                                    \
        self->data = (type*)realloc(self->data, cap * sizeof(type));                     \
        if(unlikely(self->data == NULL)) {                                               \
            fprintf(                                                                     \
                stderr,                                                                  \
                "Internal error, failed allocating memory: %d:%s.\n",                    \
                __LINE__,                                                                \
                __FILE__);                                                               \
            exit(ENOMEM);                                                                \
        }                                                                                \
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
                exit(EXIT_FAILURE);                                                      \
            } else {                                                                     \
                self->data = (type*)realloc(self->data, self->cap * sizeof(type));       \
                if(unlikely(self->data == NULL)) {                                       \
                    fprintf(                                                             \
                        stderr,                                                          \
                        "Internal error, failed allocating memory: %d:%s.\n",            \
                        __LINE__,                                                        \
                        __FILE__);                                                       \
                    exit(ENOMEM);                                                        \
                }                                                                        \
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
    force_inline void _ARRAY_METHOD(name, free, FreeFn fn)                               \
    {                                                                                    \
        if(fn != NULL) {                                                                 \
            for(UInt i = 0; i < self->len; i++) {                                        \
                fn((void*)&self->data[i]);                                               \
            }                                                                            \
        }                                                                                \
        free(self->data);                                                                \
    }

#endif
