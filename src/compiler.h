#ifndef __SKOOMA_COMPILER_H__
#define __SKOOMA_COMPILER_H__

#include "common.h"
#include "value.h"
#include "vmachine.h"

typedef struct Compiler Compiler;

ObjFunction *compile(VM *vm, const char *source);
void mark_c_roots(Compiler *c);

#endif
