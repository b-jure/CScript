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

Value             resolve_script(VM* vm, Value name);
const char*       load_script_default(VM* vm, const char* path);

#define ISFALSEY(val) (IS_NIL(val) || (IS_BOOL(val) && !AS_BOOL(val)))

/* Native functions written in C. */
#define snative(name) bool native_##name(VM* vm, Value* argv, Int argc)

/* Time */
snative(clock);

/* Class */
snative(isfield);

/* Input/Output functions */
snative(printl);
snative(print);

/* String functions */
snative(tostr);
snative(isstr);
snative(strlen);
snative(strpat);
snative(strsub);

/* Garbage collector API */
snative(gcfactor);
snative(gcmode);
snative(gccollect);
snative(gcleft);
snative(gcusage);
snative(gcnext);
snative(gcset);
snative(gcisauto);

snative(assert);
snative(assertf);
snative(error);

// @IMPLEMENT
snative(loadscript);

#endif
