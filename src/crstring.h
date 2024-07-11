#ifndef CRSTRING_H
#define CRSTRING_H


#include "crobject.h"


/* string contents for 'memerror' in 'GState' */
#define MEMERRMSG	"out of memory"


/* check if string has hash */
#define hashash(s)		testbit((s)->bits, STRHASHASH)

/* check if string is reserved keyword */
#define iskeyword(s)		testbit((s)->bits, STRKEYWORD)

/* check if string is vtable method */
#define isvtabmethod(s)		testbit((s)->bits, STRVTABMETHOD)


/* size of 'OString' object */
#define sizeofstring(l)	\
	(offsetof(OString, bytes) + ((l)+1) * sizeof(char))


OString *cr_string_new(cr_State *ts, const char *str);
OString *cr_string_newl(cr_State *ts, const char *str, size_t len);
unsigned int cr_string_hash(const char *str, size_t len, unsigned int seed);
int cr_string_eq(OString *a, OString *b);
const char *cr_object_pushfstring(cr_State *ts, const char *fmt, ...);

#endif
