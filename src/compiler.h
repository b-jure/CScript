#ifndef __SKOOMA_COMPILER_H__
#define __SKOOMA_COMPILER_H__

#include "common.h"
#include "value.h"
#include "vmachine.h"

ObjFunction *compile(VM *vm, const char *source);

#endif
