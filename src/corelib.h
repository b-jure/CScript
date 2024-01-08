#ifndef SKOOMA_CORELIB_H
#define SKOOMA_CORELIB_H

#include "common.h"
#include "skmath.h"
#include "skooma.h"
#include "value.h"

Value       resolve_script(VM* vm, Value name);
const char* load_script_default(VM* vm, const char* path);

/* Native functions written in C. */
#define corelib(name) int skcore_##name(VM* vm)

/* Time */
corelib(clock);

corelib(isfield);
corelib(typeof);
corelib(loadscript);

corelib(printl);
corelib(print);

corelib(tostr);
corelib(isstr);
corelib(strlen);
corelib(strpat);
corelib(strsub);
corelib(strupper);
corelib(strlower);
corelib(strbyte);
corelib(strrev);
corelib(strconcat);
corelib(byte);

/* Garbage collector API */
corelib(gcfactor);
corelib(gcmode);
corelib(gccollect);
corelib(gcleft);
corelib(gcusage);
corelib(gcnext);
corelib(gcset);
corelib(gcisauto);

/* Debug */
corelib(assert);
corelib(assertf);
corelib(error);


#endif
