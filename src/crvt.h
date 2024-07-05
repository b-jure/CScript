#ifndef CRVTABLE_H
#define CRVTABLE_H


#include "crvalue.h"



/* get type name */
#define typename(t)	cr_vt_typenames[(t) + 1]

CRI_DEC(const char *const cr_vt_typenames[CR_TOTALTYPES]);



/* get vtable method 'Tuple' */
#define vtmi(mt)	(&vtmethodinfo[(mt)])

typedef struct Tuple {
	int arity;
	int nreturns;
} Tuple;

CRI_DEC(const Tuple vtmethodinfo[CR_NUMM]);



/* virtual method table type */
typedef TValue VMT[CR_NUMM];


void cr_vmt_init(cr_State *ts);
const TValue *cr_vmt_get(cr_State *ts, const TValue *v, int mt);

#endif
