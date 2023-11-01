#ifndef SKOOMA_CORE_H
#define SKOOMA_CORE_H

#include "common.h"
#include "value.h"

typedef struct {
    const char*   name;
    const uint8_t len;
} InternedString;

#define sizeofstr(str) (sizeof(str) - 1)

/* Class initializer */
#define SS_INIT 0
/* Value types */
#define SS_STR  1
#define SS_NUM  2
#define SS_INS  3
#define SS_BOOL 4
#define SS_NIL  5
/* Native functions argument names */
#define SS_MANU       6
#define SS_AUTO       7
#define SS_ASSERT_MSG 8
#define SS_ERROR      9
#define SS_ASSERT     10
/* Size */
#define SS_SIZE (sizeof(static_str) / sizeof(static_str[0]))

static const InternedString static_str[] = {
  /* Class initializer name. */
    {"__init__",          sizeofstr("__init__")         },
 /* (user) Value types */
    {"string",            sizeofstr("string")           },
    {"number",            sizeofstr("number")           },
    {"instance",          sizeofstr("instance")         },
    {"bool",              sizeofstr("bool")             },
    {"nil",               sizeofstr("nil")              },
 /* Native function statics */
    {"manual",            sizeofstr("manual")           },
    {"auto",              sizeofstr("auto")             },
    {"assertion failed.", sizeofstr("assertion failed.")},
    {"Error: ",           sizeofstr("Error: ")          },
    {"Assert: ",          sizeofstr("Assert: ")         },
};


#define ISFALSEY(val) (IS_NIL(val) || (IS_BOOL(val) && !AS_BOOL(val)))

/* Native functions written in C. */
#define NATIVE(name) bool native_##name(VM* vm, Value* argv)

/* Time */
NATIVE(clock);

/* Class */
NATIVE(isfield);

/* Input/Output functions */
NATIVE(printl);
NATIVE(print);

/* String functions */
NATIVE(tostr);
NATIVE(isstr);
NATIVE(strlen);
NATIVE(strpat);
NATIVE(strsub);

/* Garbage collector API */
NATIVE(gcfactor);
NATIVE(gcmode);
NATIVE(gccollect);
NATIVE(gcleft);
NATIVE(gcusage);
NATIVE(gcnext);
NATIVE(gcset);
NATIVE(gcisauto);

NATIVE(assert);
NATIVE(assertf);
NATIVE(error);

// @IMPLEMENT
NATIVE(loadscript);

#endif
