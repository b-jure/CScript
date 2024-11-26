/*
** cstring.h
** Functions for CScript string objects
** See Copyright Notice in cscript.h
*/

#ifndef CSTRING_H
#define CSTRING_H


#include "cobject.h"
#include "cstate.h"



/* memory allocation error message must be preallocated */
#define MEMERRMSG       "out of memory"



/* size of 'OString' object */
#define sizeofstring(l) \
        (offsetof(OString, bytes) + ((l) + 1) * sizeof(char))


/* create new string from literal 'lit' */
#define csS_newlit(ts, lit) \
        csS_newl(ts, "" lit, (sizeof(lit)/sizeof(char)) - 1)



/* test whether a string is a reserved word */
#define isreserved(s) \
        ((s)->tt_ == CS_VSHRSTR && (s)->extra > 0 && \
         (s)->extra <= NUM_KEYWORDS)


/* test wheter a string is a metamethod tag */
#define ismetatag(s)    ((s)->tt_ == CS_VSHRSTR && (s)->extra > NUM_KEYWORDS)


/* equality for short strings, which are always internalized */
#define eqshrstr(a,b)	check_exp((a)->tt_ == CS_VSHRSTR, (a) == (b))


/* size of buffer for 'csS_utf8esc' function */
#define UTF8BUFFSZ      8


CSI_FUNC int csS_eqlngstr(const OString *s1, const OString *s2);
CSI_FUNC void csS_clearcache(GState *gs);
CSI_FUNC uint csS_hash(const char *str, size_t len, uint seed);
CSI_FUNC uint csS_hashlngstr(OString *s);
CSI_FUNC void csS_resize(cs_State *ts, int nsz);
CSI_FUNC void csS_init(cs_State *ts);
CSI_FUNC OString *csS_newlngstrobj(cs_State *ts, size_t len);
CSI_FUNC void csS_remove(cs_State *ts, OString *s);
CSI_FUNC OString *csS_new(cs_State *ts, const char *str);
CSI_FUNC OString *csS_newl(cs_State *ts, const char *str, size_t len);
CSI_FUNC void csS_free(cs_State *ts, OString *s);
CSI_FUNC int csS_cmp(const OString *s1, const OString *s2);
CSI_FUNC const char *csS_pushvfstring(cs_State *ts, const char *fmt,
                                      va_list argp);
CSI_FUNC const char *csS_pushfstring(cs_State *ts, const char *fmt, ...);
CSI_FUNC size_t csS_tonum(const char *s, TValue *o, int *of);
CSI_FUNC const char *csS_numtostr(const TValue *o, size_t *plen);
CSI_FUNC int csS_utf8esc(char *buff, ulong n);
CSI_FUNC int csS_hexvalue(int c);
CSI_FUNC const char *csS_tolowerall(const char *s);
CSI_FUNC void csS_strlimit(char *dest, const char *src, size_t len, size_t limit);
CSI_FUNC void csS_sourceid(char *dest, const char *src, size_t len);

#endif
