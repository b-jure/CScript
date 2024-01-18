#ifndef SKCOMMON_H
#define SKCOMMON_H

#include "skbits.h"
#include "sklimits.h"
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
extern volatile uint8_t runtime; // in 'vmachine.c'


// Memory alloc/dealloc
void* gcrealloc(VM* vm, void* ptr, size_t oldc, size_t newc);
void gcfree(VM* vm, void* ptr, size_t oldc, size_t newc);
void _cleanupvm(VM** vm); // cleanup function signature


/* Return MAX */
#if defined(__GNUC__)
#define MAX(a, b)                                                                                  \
    ({                                                                                             \
        __typeof__(a) _a = (a);                                                                    \
        __typeof__(b) _b = (b);                                                                    \
        _a > _b ? _a : _b;                                                                         \
    })

/* Return MIN */
#define MIN(a, b)                                                                                  \
    ({                                                                                             \
        __typeof__(a) _a = (a);                                                                    \
        __typeof__(b) _b = (b);                                                                    \
        _a > _b ? _b : _a;                                                                         \
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
#define cast_lint(e)     ((long int)(e))
#define cast_hash(e)     ((Hash)(e))
#define cast(type, expr) ((type)(expr))






/* ========== Static strings ========== */


/* Size of static string */
#define SSS(str) (sizeof(str) - 1)


/* Indices into static strings table (fast access) */
typedef enum {
    /* Value types */
    SS_NIL = 0,
    SS_NUM,
    SS_STR,
    SS_BOOL,
    SS_CLASS,
    SS_INS,
    SS_FUNC,
    SS_CLS,
    SS_NAT,
    SS_UPVAL,
    SS_METHOD,
    /* Boolean strings */
    SS_TRUE,
    SS_FALSE,
    /* Class overload-able method names. */
    SS_INIT,
    SS_DISP,
#if defined(SK_OVERLOAD_OPS)
    SS_ADD,
    SS_SUB,
    SS_MUL,
    SS_DIV,
    SS_MOD,
    SS_POW,
    SS_NOT,
    SS_UMIN,
    SS_NE,
    SS_EQ,
    SS_LT,
    SS_LE,
    SS_GT,
    SS_GE,
#endif
    /* Class special field names. */
    SS_DBG,
    /* Operator strings */
    SS_OPADD,
    SS_OPSUB,
    SS_OPMUL,
    SS_OPDIV,
    SS_OPMOD,
    SS_OPEXP,
    SS_OPNOT,
    SS_OPNEG,
    SS_OPNE,
    SS_OPEQ,
    SS_OPLT,
    SS_OPLE,
    SS_OPGT,
    SS_OPGE,
    SS_OPAND,
    SS_OPOR,
    /* Other statics */
    SS_UNKNOWN,
    SS_CSRC,
    SS_SIZE,
} SSTag;

typedef struct {
    const char* name;
    const uint8_t len;
} InternedString;

extern const InternedString static_strings[SS_SIZE];

#endif
