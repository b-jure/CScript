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
#define csS_newlit(ts, lit) \
    csS_newl(ts, "" lit, (sizeof(lit)/sizeof(char)) - 1)



CSI_FUNC void csS_init(cs_State *ts);
CSI_FUNC OString *csS_new(cs_State *ts, const char *str);
CSI_FUNC OString *csS_newl(cs_State *ts, const char *str, size_t len);
CSI_FUNC OString *csS_newlobj(cs_State *ts, size_t len);
CSI_FUNC void csS_free(cs_State *ts, OString *s);
CSI_FUNC uint csS_hash(const char *str, size_t len, uint seed);
CSI_FUNC int csS_cmp(const OString *s1, const OString *s2);
CSI_FUNC int csS_eq(const OString *s1, const OString *s2);
CSI_FUNC const char *csS_pushvfstring(cs_State *ts, const char *fmt,
                                      va_list argp);
CSI_FUNC const char *csS_pushfstring(cs_State *ts, const char *fmt, ...);
CSI_FUNC size_t csS_tonum(const char *s, TValue *o, int *of);
CSI_FUNC int csS_tomt(cs_State *ts, OString *id);
CSI_FUNC const char *csS_numtostr(const TValue *o, size_t *plen);
CSI_FUNC int csS_hexvalue(int c);
CSI_FUNC const char *csS_tolowerall(const char *s);
CSI_FUNC void csS_strlimit(char *dest, const char *src, size_t len, size_t limit);
CSI_FUNC void csS_sourceid(char *dest, const char *src, size_t len);

#endif
