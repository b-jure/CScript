/*
** ttype.h
** 'ctype' functions for Tokudae
** See Copyright Notice in tokudae.h
*/


#ifndef ttype_h
#define ttype_h

#define	ttoascii(c)	((c) & 0x7F)
#define ttolower(c)     ((c) | 0x20)
#define ttoupper(c)     ((c) & 0x5F)

#define	tisascii(c)	(((c) & ~0x7F) == 0)
#define tisdigit(c)     ((unsigned)(c)-'0' < 10u)
#define tisalpha(c)     ((unsigned)ttolower(c)-'a' < 26u)
#define tisupper(c)     ((c)-'A' < 26)
#define tislower(c)     ((c)-'a' < 26)
#define tisalnum(c)     (tisalpha(c) || tisdigit(c))
#define tisxdigit(c)    (tisdigit(c) || ((unsigned)ttolower(c))-'a' < 6u)
#define tisblank(c)     ((c) == ' ' || c == '\t')
#define tiscntrl(c)     ((unsigned)(c) <= 0x20u || (c) == 0x7F)
#define tisgraph(c)     ((unsigned)(c)-0x21 < 0x5Eu)
#define tisprint(c)     ((unsigned)(c)-0x20 < 0x5Fu)
#define tispunct(c)     (tisgraph(c) && !tisalnum(c))
#define tisspace(c)     ((c) == ' ' || (unsigned)(c) - '\t' < 0x05u)

/* miscellaneous macros (not ISO C) */
#define ttodigit(c)     ((c) & 0x0F) /* c to digit (unchecked) */
#define tisodigit(c)    ((unsigned)(c)-'0' < 8u)
#define tisbdigit(c)    ((unsigned)(c)-'0' < 2u)

#endif
