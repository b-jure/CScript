#ifndef CRVTABLE_H
#define CRVTABLE_H


#include "crvalue.h"


#define typename(t)	cr_vt_typenames[(t) + 1]


CRI_DEC(const char *const cr_vt_typenames[CR_TOTALTYPES];);


void cr_vt_init(TState *ts);


#endif
