#ifndef __SKOOMA_COMPILER_H__
#define __SKOOMA_COMPILER_H__

typedef struct Compiler Compiler;

#include "common.h"
#include "value.h"
#include "vmachine.h"

ObjFunction* compile(VM* vm, const char* source);
void         mark_c_roots(VM* vm, Compiler* c);

#endif
