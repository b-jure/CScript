/*
** cstring.h
** Functions for CScript string objects
** See Copyright Notice in cscript.h
*/

#ifndef CRSTRING_H
#define CRSTRING_H


#include "cobject.h"


/* string contents for 'memerror' in 'GState' */
#define MEMERRMSG       "out of memory"


/* check if string has hash */
#define shashash(s)              testbit((s)->bits, STRHASHBIT)

/* check if string is reserved keyword */
#define siskeyword(s)            testbit((s)->bits, STRKWBIT)

/* check if string is vtable method */
#define sisvmtmethod(s)         testbit((s)->bits, STRVMTBIT)


/* size of 'OString' object */
#define sizeofstring(l) \
    (offsetof(OString, bytes) + (((l) + 1) * sizeof(char)))


/* create new string from literal 'lit' */
#define crS_newlit(ts, lit) \
    crS_newl(ts, "" lit, (sizeof(lit)/sizeof(char)) - 1)



CRI_FUNC void crS_init(cr_State *ts);
CRI_FUNC OString *crS_new(cr_State *ts, const char *str);
CRI_FUNC OString *crS_newl(cr_State *ts, const char *str, size_t len);
CRI_FUNC void crS_free(cr_State *ts, OString *s);
CRI_FUNC uint crS_hash(const char *str, size_t len, uint seed);
CRI_FUNC int crS_cmp(const OString *s1, const OString *s2);
CRI_FUNC int crS_eq(const OString *s1, const OString *s2);
CRI_FUNC const char *crS_pushvfstring(cr_State *ts, const char *fmt,
                                            va_list argp);
CRI_FUNC const char *crS_pushfstring(cr_State *ts, const char *fmt, ...);
CRI_FUNC size_t crS_tonum(const char *s, TValue *o, int *of);
CRI_FUNC int crS_tomt(cr_State *ts, OString *id);
CRI_FUNC void crS_numtostring(cr_State *ts, TValue *v);
CRI_FUNC int crS_hexvalue(int c);
CRI_FUNC const char *crS_tolowerall(const char *s);
CRI_FUNC void crS_sourceid(char *adest, const char *src, size_t len);

#endif
