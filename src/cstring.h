/*
** cstring.h
** Functions for CScript string objects
** See Copyright Notice in cscript.h
*/

#ifndef cstring_h
#define cstring_h


#include "cobject.h"
#include "cstate.h"
#include "clexer.h"



/* memory allocation error message must be preallocated */
#define MEMERRMSG       "out of memory"


/* size of 'OString' object */
#define sizeofstring(l) \
        (offsetof(OString, bytes) + ((l) + 1)*sizeof(char))


/* create new string from literal 'lit' */
#define csS_newlit(C, lit) \
        csS_newl(C, "" lit, (sizeof(lit)/sizeof(char)) - 1)


/* test whether a string is a reserved word */
#define isreserved(s) \
        ((s)->tt_ == CS_VSHRSTR && 0 < (s)->extra && \
         (s)->extra <= NUM_KEYWORDS)


/* value of 'extra' for first metamethod name */
#define FIRSTMM     (NUM_KEYWORDS + 1)


/* test wheter a string is a metamethod tag */
#define ismetatag(s) \
        ((s)->tt_ == CS_VSHRSTR && FIRSTMM <= (s)->extra && \
         (s)->extra < FIRSTMM + CS_MT_NUM)


/* equality for short strings, which are always internalized */
#define eqshrstr(a,b)	check_exp((a)->tt_ == CS_VSHRSTR, (a) == (b))



CSI_FUNC int csS_eqlngstr(const OString *s1, const OString *s2);
CSI_FUNC void csS_clearcache(GState *gs);
CSI_FUNC c_uint csS_hash(const char *str, size_t len, c_uint seed);
CSI_FUNC c_uint csS_hashlngstr(OString *s);
CSI_FUNC void csS_resize(cs_State *C, int nsz);
CSI_FUNC void csS_init(cs_State *C);
CSI_FUNC OString *csS_newlngstrobj(cs_State *C, size_t len);
CSI_FUNC void csS_remove(cs_State *C, OString *s);
CSI_FUNC OString *csS_new(cs_State *C, const char *str);
CSI_FUNC OString *csS_newl(cs_State *C, const char *str, size_t len);
CSI_FUNC void csS_free(cs_State *C, OString *s);
CSI_FUNC int csS_cmp(const OString *s1, const OString *s2);
CSI_FUNC const char *csS_pushvfstring(cs_State *C, const char *fmt,
                                      va_list argp);
CSI_FUNC const char *csS_pushfstring(cs_State *C, const char *fmt, ...);
CSI_FUNC size_t csS_tonum(const char *s, TValue *o, int *of);
CSI_FUNC unsigned csS_tostringbuff(const TValue *o, char *buff);
CSI_FUNC void csS_tostring(cs_State *C, TValue *obj);
CSI_FUNC int csS_hexvalue(int c);
CSI_FUNC void csS_trimstr(char *restrict out, size_t lout,
                                const char *s, size_t l);
CSI_FUNC void csS_chunkid(char *out, const char *source, size_t srclen);

#define UTF8BUFFSZ  8
CSI_FUNC int csS_utf8esc(char *buff, c_ulong n);

#endif
