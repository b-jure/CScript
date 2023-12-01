#ifndef SKOOMA_CORE_H
#define SKOOMA_CORE_H

#include "common.h"
#include "skmath.h"
#include "value.h"

typedef bool (*NativeFn)(VM* vm, Value* argv, Int argc, ...);

Value       resolve_script(VM* vm, Value name);
const char* load_script_default(VM* vm, const char* path);

#define ISFALSEY(val) (IS_NIL(val) || (IS_BOOL(val) && !AS_BOOL(val)))

/* Native functions written in C. */
#define snative(name) bool native_##name(VM* vm, Value* argv, Int argc, ...)

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
