#ifndef __SKOOMA_PARSER_H__
#define __SKOOMA_PARSER_H__

typedef struct Function Function;

#include "array.h"
#include "common.h"
#include "value.h"
#include "vmachine.h"

OFunction* compile(VM* vm, const char* source, Value name);
void       _cleanup_function(Function* F);
void       F_free(Function* F);
void       mark_function_roots(VM* vm);

#endif
