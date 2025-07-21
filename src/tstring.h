/*
** tstring.h
** Functions for Tokudae string objects
** See Copyright Notice in tokudae.h
*/

#ifndef tstring_h
#define tstring_h


#include "tobject.h"
#include "tstate.h"
#include "tlexer.h"



/* memory allocation error message must be preallocated */
#define MEMERRMSG       "out of memory"


/* size of 'OString' object */
#define sizeofstring(l) \
        (offsetof(OString, bytes) + ((l) + 1)*sizeof(char))


/* create new string from literal 'lit' */
#define tokuS_newlit(T, lit) \
        tokuS_newl(T, "" lit, (sizeof(lit)/sizeof(char)) - 1)


/* test whether a string is a reserved word */
#define isreserved(s) \
        ((s)->tt_ == TOKU_VSHRSTR && 0 < (s)->extra && \
         (s)->extra <= NUM_KEYWORDS)


/* value of 'extra' for first metamethod name */
#define FIRSTMM     (NUM_KEYWORDS + 1)


/* test whether a string is a metamethod tag */
#define ismetatag(s) \
        ((s)->tt_ == TOKU_VSHRSTR && FIRSTMM <= (s)->extra && \
         (s)->extra < FIRSTMM + TOKU_MT_NUM)


/* equality for short strings, which are always internalized */
#define eqshrstr(a,b)	check_exp((a)->tt_ == TOKU_VSHRSTR, (a) == (b))



TOKUI_FUNC int tokuS_eqlngstr(const OString *s1, const OString *s2);
TOKUI_FUNC void tokuS_clearcache(GState *gs);
TOKUI_FUNC t_uint tokuS_hash(const char *str, size_t len, t_uint seed);
TOKUI_FUNC t_uint tokuS_hashlngstr(OString *s);
TOKUI_FUNC void tokuS_resize(toku_State *T, int nsz);
TOKUI_FUNC void tokuS_init(toku_State *T);
TOKUI_FUNC OString *tokuS_newlngstrobj(toku_State *T, size_t len);
TOKUI_FUNC void tokuS_remove(toku_State *T, OString *s);
TOKUI_FUNC OString *tokuS_new(toku_State *T, const char *str);
TOKUI_FUNC OString *tokuS_newl(toku_State *T, const char *str, size_t len);
TOKUI_FUNC void tokuS_free(toku_State *T, OString *s);
TOKUI_FUNC int tokuS_cmp(const OString *s1, const OString *s2);
TOKUI_FUNC const char *tokuS_pushvfstring(toku_State *T, const char *fmt,
                                          va_list ap);
TOKUI_FUNC const char *tokuS_pushfstring(toku_State *T, const char *fmt, ...);
TOKUI_FUNC size_t tokuS_tonum(const char *s, TValue *o, int *of);
TOKUI_FUNC unsigned tokuS_tostringbuff(const TValue *o, char *buff);
TOKUI_FUNC void tokuS_tostring(toku_State *T, TValue *obj);
TOKUI_FUNC int tokuS_hexvalue(int c);
TOKUI_FUNC void tokuS_trimstr(char *restrict out, size_t lout,
                              const char *s, size_t l);
TOKUI_FUNC void tokuS_chunkid(char *out, const char *source, size_t srclen);

#define UTF8BUFFSZ  8
TOKUI_FUNC int tokuS_utf8esc(char *buff, t_ulong n);

#endif
