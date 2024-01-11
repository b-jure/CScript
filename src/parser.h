#ifndef SKOOMA_PARSER_H
#define SKOOMA_PARSER_H

#include "array.h"
#include "common.h"
#include "reader.h"
#include "value.h"
#include "vmachine.h"

uint8_t protectedcompile(VM* vm, BuffReader* br, const char* name);
uint8_t compile(VM* vm, BuffReader* br, const char* name, bool globscope);
void _cleanup_function(Function* F);
void F_free(Function* F);
void mark_function_roots(VM* vm);

#endif
