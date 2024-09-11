#ifndef CRPROTECTED_H
#define CRPROTECTED_H

#include "cript.h"


/* type for functions with error recovery */
typedef void (*ProtectedFn)(cr_State *ts, void *userdata);


CRI_FUNC void crPr_parse(cr_State *ts, cr_fReader freader, void *userdata,
                         const char *name); 
CRI_FUNC int crPr_call(cr_State *ts, ProtectedFn fn, void *ud, ptrdiff_t top);

#endif
