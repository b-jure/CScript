/*
** cprotected.h
** Functions for calling functions in protected mode
** See Copyright Notice in cscript.h
*/

#ifndef CRPROTECTED_H
#define CRPROTECTED_H

#include "creader.h"
#include "cscript.h"
#include "climits.h"


/* type for functions with error handler */
typedef void (*ProtectedFn)(cs_State *ts, void *userdata);


CSI_FUNC cs_noret csPRthrow(cs_State *ts, int code);
CSI_FUNC int csPRclose(cs_State *ts, ptrdiff_t level, int status);
CSI_FUNC int csPRrawcall(cs_State *ts, ProtectedFn fn, void *ud);
CSI_FUNC int csPRcall(cs_State *ts, ProtectedFn fn, void *ud, ptrdiff_t top,
                       ptrdiff_t errfunc);
CSI_FUNC int csPRparse(cs_State *ts, BuffReader *br, const char *name); 

#endif
