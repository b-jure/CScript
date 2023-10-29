#ifndef SKOOMA_CORE_H
#define SKOOMA_CORE_H

#include "common.h"
#include "object.h"
#include "value.h"

/* Native functions written in C. */
#define NATIVE(name) bool native_##name(VM* vm, Value* argv)

/* Time */
NATIVE(clock);

/* Class field functions */
NATIVE(isfield);
NATIVE(delfield);
NATIVE(setfield);

/* Input/Output functions */
NATIVE(printl);

/* String functions */
NATIVE(tostr);

/* Garbage collector API */
NATIVE(gcfactor);
NATIVE(gcmode);
NATIVE(gccollect);
NATIVE(gcleft);
NATIVE(gcusage);
NATIVE(gcnext);
NATIVE(gcset);

#endif
