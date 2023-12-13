#ifndef SKOOMA_PARSER_H
#define SKOOMA_PARSER_H

#include "array.h"
#include "common.h"
#include "value.h"
#include "vmachine.h"

OClosure* compile(VM* vm, const char* source, Value name);
void      _cleanup_function(Function* F);
void      F_free(Function* F);
void      mark_function_roots(VM* vm);

#endif
