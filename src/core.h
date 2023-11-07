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
#define SS_STR   1
#define SS_NUM   2
#define SS_INS   3
#define SS_CLASS 4
#define SS_BOOL  5
#define SS_NIL   6
#define SS_FUNC  7
/* Native functions argument names */
#define SS_MANU       8
#define SS_AUTO       9
#define SS_ASSERT_MSG 10
#define SS_ERROR      11
#define SS_ASSERT     12
/* Size */
#define SS_SIZE (sizeof(static_str) / sizeof(static_str[0]))

static const InternedString static_str[] = {
  /* Class initializer name. */
    {"__init__",          sizeofstr("__init__")         },
 /* (user) Value types */
    {"string",            sizeofstr("string")           },
    {"number",            sizeofstr("number")           },
    {"instance",          sizeofstr("instance")         },
    {"class",             sizeofstr("class")            },
    {"bool",              sizeofstr("bool")             },
    {"nil",               sizeofstr("nil")              },
    {"function",          sizeofstr("function")         },
 /* Native function statics */
    {"manual",            sizeofstr("manual")           },
    {"auto",              sizeofstr("auto")             },
    {"assertion failed.", sizeofstr("assertion failed.")},
    {"Error: ",           sizeofstr("Error: ")          },
    {"Assert: ",          sizeofstr("Assert: ")         },
};

Value       resolve_script(VM* vm, Value name);
const char* load_script_default(VM* vm, const char* path);

#define ISFALSEY(val) (IS_NIL(val) || (IS_BOOL(val) && !AS_BOOL(val)))

/* Native functions written in C. */
#define snative(name) bool native_##name(VM* vm, Value* argv, Int argc)

/* Time */
snative(clock);

snative(isfield);
snative(typeof);
snative(loadscript);

snative(printl);
snative(print);

snative(tostr);
snative(isstr);
snative(strlen);
snative(strpat);
snative(strsub);
snative(strupper);
snative(strlower);
snative(strbyte);
snative(strrev);
snative(strconcat);
snative(byte);

/* Garbage collector API */
snative(gcfactor);
snative(gcmode);
snative(gccollect);
snative(gcleft);
snative(gcusage);
snative(gcnext);
snative(gcset);
snative(gcisauto);

/* Debug */
snative(assert);
snative(assertf);
snative(error);


#endif
