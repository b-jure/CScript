#ifndef SKCOMMON_H
#define SKCOMMON_H

#include "skbits.h"
#include "skooma.h"

#include <stdbool.h>
#include <sys/types.h>

/* Forward declare */
typedef struct Function Function;

typedef uint8_t Byte;
typedef uint32_t UInt;
typedef int32_t Int;

#define UNUSED(x) (void)(x)

/* garbage collection flag (check mem.c -> gc())
 * 0 - compiling source code
 * 1 - VM is running */
extern volatile Int runtime; // in 'vmachine.c'


// Memory alloc/dealloc
void* gcrealloc(VM* vm, void* ptr, ssize_t oldc, ssize_t newc);
void* gcfree(VM* vm, void* ptr, ssize_t oldc, ssize_t newc);
void _cleanupvm(VM** vm); // cleanup function signature


/* Return MAX */
#if defined(__GNUC__) || defined(__clang__)
#define MAX(a, b)                                                                        \
    ({                                                                                   \
        __typeof__(a) _a = (a);                                                          \
        __typeof__(b) _b = (b);                                                          \
        _a > _b ? _a : _b;                                                               \
    })

/* Return MIN */
#define MIN(a, b)                                                                        \
    ({                                                                                   \
        __typeof__(a) _a = (a);                                                          \
        __typeof__(b) _b = (b);                                                          \
        _a > _b ? _b : _a;                                                               \
    })
#else
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define MIN(a, b) ((a) > (b) ? (b) : (a))
#endif


/* Casting */
#define cast_int(e)      ((int)(e))
#define cast_uint(e)     ((unsigned int)(e))
#define cast_intptr(e)   ((intptr_t)(e))
#define cast_double(e)   ((double)(e))
#define cast_char(e)     ((char)(e))
#define cast_uchar(e)    ((unsigned char)(e))
#define cast_charp(e)    ((char*)(e))
#define cast(type, expr) ((type)(expr))


#endif
