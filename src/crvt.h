#ifndef CRVTABLE_H
#define CRVTABLE_H


#include "crvalue.h"



/* get type name */
#define typename(t)	cr_vt_typenames[(t) + 1]

CRI_DEC(const char *const cr_vt_typenames[CR_TOTALTYPES]);



typedef struct Tuple {
	int arity;
	int nreturns;
} Tuple;

/* get vtable method 'Tuple' */
#define vtmi(mt)	(&vtmethodinfo[(mt)])

CRI_DEC(const Tuple vtmethodinfo[CR_NUMM]);



/* virtual method table type */
typedef struct GCObject *VMT[CR_NUMM];



void cr_vt_init(cr_State *ts);

#endif
