#ifndef CRSTRING_H
#define CRSTRING_H


#include "crobject.h"


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
#define cr_string_newlit(ts, lit) \
    cr_string_newl(ts, "" lit, (sizeof(lit)/sizeof(char)) - 1)


CRI_FUNC OString *cr_string_new(cr_State *ts, const char *str);
CRI_FUNC OString *cr_string_newl(cr_State *ts, const char *str, size_t len);
CRI_FUNC void cr_string_free(cr_State *ts, OString *s);
CRI_FUNC uint cr_string_hash(const char *str, size_t len, uint seed);
CRI_FUNC int cr_string_cmp(const OString *s1, const OString *s2);
CRI_FUNC int cr_string_eq(const OString *s1, const OString *s2);
CRI_FUNC const char *cr_string_pushvfstring(cr_State *ts, const char *fmt,
                                            va_list argp);
CRI_FUNC const char *cr_string_pushfstring(cr_State *ts, const char *fmt, ...);
CRI_FUNC size_t cr_string_tonum(const char *s, TValue *o, int *of);
CRI_FUNC int cr_string_tomt(cr_State *ts, OString *id);
CRI_FUNC void cr_string_numtostring(cr_State *ts, TValue *v);
CRI_FUNC int cr_string_hexvalue(int c);
CRI_FUNC void cr_string_sourceid(char *adest, const char *src, size_t len);

#endif
