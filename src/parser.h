#ifndef SKOOMA_PARSER_H
#define SKOOMA_PARSER_H

#include "array.h"
#include "common.h"
#include "reader.h"
#include "value.h"
#include "vmachine.h"

uint8_t pcompile(VM* vm, void* userdata, const char* name, uint8_t isingscope);
void _cleanup_function(Function* F);
void F_free(Function* F);
void mark_function_roots(VM* vm);

#endif
