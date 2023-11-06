#ifndef __SKOOMA_COMPILER_H__
#define __SKOOMA_COMPILER_H__

typedef struct Compiler Compiler;

#include "array.h"
#include "common.h"
#include "value.h"
#include "vmachine.h"

ObjFunction* compile(VM* vm, const char* source, Value name);
void         _cleanup_compiler(VM* vm, Compiler* C);
void         C_free(VM* vm, Compiler* C);
void         mark_c_roots(VM* vm);

#endif
