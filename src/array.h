#ifndef __SKOOMA_ARRAY_H__
#define __SKOOMA_ARRAY_H__

#include "common.h"

#include <assert.h>
#include <errno.h>
#include <memory.h>
#include <stdio.h>
#include <stdlib.h>

#define ARRAY_INITIAL_SIZE 8

#define GROW_ARRAY_CAPACITY(cap, initial_size)                                  \
    ((cap) < (initial_size) ? (initial_size) : (cap) * 2)


/* GRAY STACK ARRAY (vm.h) */
#define GSARRAY_INIT(vm)                                                        \
    do {                                                                        \
        (vm)->gray_stack = NULL;                                                \
        (vm)->gslen      = 0;                                                   \
        (vm)->gscap      = 0;                                                   \
    } while(false)

#define GSARRAY_PUSH(vm, objref)                                                \
    ({                                                                          \
        if((vm)->gscap <= (vm)->gslen) {                                        \
            Int oldcap = (vm)->gscap;                                           \
            (vm)->gscap =                                                       \
                MIN(GROW_ARRAY_CAPACITY((Int)oldcap, ARRAY_INITIAL_SIZE),       \
                    UINT24_MAX);                                                \
            (vm)->gray_stack =                                                  \
                (O**)realloc((vm)->gray_stack, (vm)->gscap * sizeof(O*));       \
        }                                                                       \
        (vm)->gray_stack[(vm)->gslen++] = objref;                               \
        (vm)->gslen - 1;                                                        \
    })

#define GSARRAY_POP(vm)                                                         \
    ({                                                                          \
        O* obj = NULL;                                                          \
        if((vm)->gslen > 0) obj = (vm)->gray_stack[--(vm)->gslen];              \
        obj;                                                                    \
    })

#define GSARRAY_FREE(vm)                                                        \
    do {                                                                        \
        free((vm)->gray_stack);                                                 \
        GSARRAY_INIT(vm);                                                       \
    } while(false)



// @TODO: Make this generic
/* CONSTANTS ARRAY (chunk.h) */
#define CARRAY_INIT(chunk)                                                      \
    do {                                                                        \
        (chunk)->constants = NULL;                                              \
        (chunk)->clen      = 0;                                                 \
        (chunk)->ccap      = 0;                                                 \
    } while(false)

#define CARRAY_PUSH(chunk, constant, vm)                                        \
    ({                                                                          \
        if((chunk)->ccap <= (chunk)->clen) {                                    \
            Int oldcap = (chunk)->ccap;                                         \
            (chunk)->ccap =                                                     \
                MIN(GROW_ARRAY_CAPACITY(oldcap, ARRAY_INITIAL_SIZE),            \
                    UINT24_MAX);                                                \
            (chunk)->constants = gcrealloc(                                     \
                vm,                                                             \
                (chunk)->constants,                                             \
                oldcap * sizeof(Value),                                         \
                (chunk)->ccap * sizeof(Value));                                 \
        }                                                                       \
        (chunk)->constants[(chunk)->clen++] = constant;                         \
        (chunk)->clen - 1;                                                      \
    })

#define CARRAY_FREE(chunk, vm)                                                  \
    do {                                                                        \
        gcfree(vm, (chunk)->constants, (chunk)->ccap, 0);                       \
        CARRAY_INIT(chunk);                                                     \
    } while(false)



// @TODO: Make this generic
/* GLOBALS ARRAY (vmachine.h) */
#define GARRAY_INIT(vm)                                                         \
    do {                                                                        \
        (vm)->globvals = NULL;                                                  \
        (vm)->globlen  = 0;                                                     \
        (vm)->globcap  = 0;                                                     \
    } while(false)

#define GARRAY_PUSH(vm, global)                                                 \
    ({                                                                          \
        if((vm)->globcap <= (vm)->globlen) {                                    \
            Int oldcap = (vm)->globcap;                                         \
            (vm)->globcap =                                                     \
                MIN(GROW_ARRAY_CAPACITY(oldcap, ARRAY_INITIAL_SIZE),            \
                    UINT24_MAX);                                                \
            (vm)->globvals = gcrealloc(                                         \
                vm,                                                             \
                (vm)->globvals,                                                 \
                oldcap * sizeof(Variable),                                      \
                (vm)->globcap * sizeof(Variable));                              \
        }                                                                       \
        (vm)->globvals[(vm)->globlen++] = global;                               \
        (vm)->globlen - 1;                                                      \
    })

#define GARRAY_REMOVE(vm, index)                                                \
    ({                                                                          \
        Variable  retval;                                                       \
        Variable* src = &vm->globvals[index];                                   \
        retval        = *src;                                                   \
        memmove(src, src + 1, vm->globlen - index);                             \
        vm->globlen--;                                                          \
        retval;                                                                 \
    })

#define GARRAY_FREE(vm)                                                         \
    do {                                                                        \
        gcfree(vm, (vm)->globvals, (vm)->globcap, 0);                           \
        GARRAY_INIT(vm);                                                        \
    } while(false)




/* GENERIC ARRAY (does not trigger garbage collector)
 *
 * These are internal only macros. */
#define _ARRAY_METHOD_NAME(tname, name) tname##_##name
#define _CALL_ARRAY_METHOD(tname, name, ...)                                    \
    _ARRAY_METHOD_NAME(tname, name)(self __VA_OPT__(, ) __VA_ARGS__)
#define _ARRAY_METHOD(tname, name, ...)                                         \
    _ARRAY_METHOD_NAME(tname, name)                                             \
    (tname * self __VA_OPT__(, ) __VA_ARGS__)


typedef void (*FreeFn)(void* value);

/* Create new 'name' array with 'type' elements */
#define ARRAY_NEW(name, type)                                                   \
    typedef struct {                                                            \
        size_t cap;                                                             \
        size_t len;                                                             \
        type*  data;                                                            \
        VM*    vm;                                                              \
    } name;                                                                     \
                                                                                \
    sstatic force_inline void _ARRAY_METHOD(name, init, VM* vmachine)           \
    {                                                                           \
        self->cap  = 0;                                                         \
        self->len  = 0;                                                         \
        self->data = NULL;                                                      \
        self->vm   = vmachine;                                                  \
    }                                                                           \
                                                                                \
    sstatic force_inline void _ARRAY_METHOD(name, init_cap, uint32_t cap)       \
    {                                                                           \
        self->data =                                                            \
            (type*)gcrealloc(self->vm, self->data, 0, cap * sizeof(type));      \
        self->cap = cap;                                                        \
    }                                                                           \
                                                                                \
    sstatic force_inline void _ARRAY_METHOD(name, grow)                         \
    {                                                                           \
        size_t old_cap = self->cap;                                             \
        self->cap =                                                             \
            MIN(GROW_ARRAY_CAPACITY(old_cap, ARRAY_INITIAL_SIZE), UINT32_MAX);  \
        if(unlikely(self->cap >= UINT32_MAX)) {                                 \
            fprintf(                                                            \
                stderr,                                                         \
                "[%s:%d] Internal error, %s capacity exceeded! [capmax -> "     \
                "%d]\n",                                                        \
                __FILE__,                                                       \
                __LINE__,                                                       \
                #name,                                                          \
                UINT32_MAX);                                                    \
            _cleanupvm(self->vm);                                               \
        } else {                                                                \
            self->data = (type*)gcrealloc(                                      \
                self->vm,                                                       \
                self->data,                                                     \
                old_cap * sizeof(type),                                         \
                self->cap * sizeof(type));                                      \
        }                                                                       \
    }                                                                           \
                                                                                \
    sstatic force_inline UInt _ARRAY_METHOD(name, push, type value)             \
    {                                                                           \
        if(self->cap <= self->len) _CALL_ARRAY_METHOD(name, grow);              \
        self->data[self->len++] = value;                                        \
        return self->len - 1;                                                   \
    }                                                                           \
                                                                                \
    sstatic force_inline type _ARRAY_METHOD(name, pop)                          \
    {                                                                           \
        return self->data[--self->len];                                         \
    }                                                                           \
                                                                                \
    sstatic force_inline type* _ARRAY_METHOD(name, index, size_t index)         \
    {                                                                           \
        return &self->data[index];                                              \
    }                                                                           \
                                                                                \
    sstatic force_inline type* _ARRAY_METHOD(name, last)                        \
    {                                                                           \
        return &self->data[self->len - 1];                                      \
    }                                                                           \
                                                                                \
    sstatic force_inline type* _ARRAY_METHOD(name, first)                       \
    {                                                                           \
        return &self->data[0];                                                  \
    }                                                                           \
                                                                                \
    sstatic force_inline void _ARRAY_METHOD(                                    \
        name,                                                                   \
        insert,                                                                 \
        size_t index,                                                           \
        type   value)                                                           \
    {                                                                           \
        type* src  = self->data + index;                                        \
        type* dest = src + 1;                                                   \
        memmove(dest, src, self->len - index);                                  \
        self->len++;                                                            \
        self->data[index] = value;                                              \
    }                                                                           \
                                                                                \
    sstatic force_inline type _ARRAY_METHOD(name, remove, size_t index)         \
    {                                                                           \
        if(self->len == 1) return _CALL_ARRAY_METHOD(name, pop);                \
        type* src    = self->data + index;                                      \
        type* dest   = src - 1;                                                 \
        type  retval = *src;                                                    \
        memmove(dest, src, self->len - index);                                  \
        self->len--;                                                            \
        return retval;                                                          \
    }                                                                           \
                                                                                \
    sstatic force_inline void _ARRAY_METHOD(name, free, FreeFn fn)              \
    {                                                                           \
        if(fn != NULL)                                                          \
            for(UInt i = 0; i < self->len; i++)                                 \
                fn((void*)&self->data[i]);                                      \
        if(self->data != NULL) gcfree(self->vm, self->data, self->cap, 0);      \
    }

#endif
