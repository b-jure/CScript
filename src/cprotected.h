/*
** cprotected.h
** Functions for calling functions in protected mode
** See Copyright Notice in cscript.h
*/

#ifndef CPROTECTED_H
#define CPROTECTED_H


#include "creader.h"


/* type for functions with error handler */
typedef void (*ProtectedFn)(cs_State *C, void *userdata);


CSI_FUNC c_noret csPR_throw(cs_State *C, int code);
CSI_FUNC int csPR_close(cs_State *C, ptrdiff_t level, int status);
CSI_FUNC int csPR_rawcall(cs_State *C, ProtectedFn fn, void *ud);
CSI_FUNC int csPR_call(cs_State *C, ProtectedFn fn, void *ud, ptrdiff_t top,
                       ptrdiff_t errfunc);
CSI_FUNC int csPR_parse(cs_State *C, BuffReader *br, const char *name); 

#endif
