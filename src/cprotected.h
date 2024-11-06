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


CSI_FUNC cs_noret crPR_throw(cs_State *ts, int code);
CSI_FUNC int crPR_close(cs_State *ts, ptrdiff_t level, int status);
CSI_FUNC int crPR_rawcall(cs_State *ts, ProtectedFn fn, void *ud);
CSI_FUNC int crPR_call(cs_State *ts, ProtectedFn fn, void *ud, ptrdiff_t top,
                       ptrdiff_t errfunc);
CSI_FUNC int crPR_parse(cs_State *ts, BuffReader *br, const char *name); 

#endif
