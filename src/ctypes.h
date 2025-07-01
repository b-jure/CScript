/*
** ctypes.h
** 'ctype' functions for CScript
** See Copyright Notice in cscript.h
*/


#ifndef ctypes_h
#define ctypes_h

#define	ctoascii(c)	((c) & 0x7F)
#define ctolower(c)     ((c) | 0x20)
#define ctoupper(c)     ((c) & 0x5F)

#define	cisascii(c)	(((c) & ~0x7F) == 0)
#define cisdigit(c)     ((unsigned)(c)-'0' < 10u)
#define cisalpha(c)     ((unsigned)ctolower(c)-'a' < 26u)
#define cisupper(c)     ((c)-'A' < 26)
#define cislower(c)     ((c)-'a' < 26)
#define cisalnum(c)     (cisalpha(c) || cisdigit(c))
#define cisxdigit(c)    (cisdigit(c) || ((unsigned)ctolower(c))-'a' < 6u)
#define cisblank(c)     ((c) == ' ' || c == '\t')
#define ciscntrl(c)     ((unsigned)(c) <= 0x20u || (c) == 0x7F)
#define cisgraph(c)     ((unsigned)(c)-0x21 < 0x5Eu)
#define cisprint(c)     ((unsigned)(c)-0x20 < 0x5Fu)
#define cispunct(c)     (cisgraph(c) && !cisalnum(c))
#define cisspace(c)     ((c) == ' ' || (unsigned)(c) - '\t' < 0x05u)

/* miscellaneous macros (not ISO C) */
#define ctodigit(c)     ((c) & 0x0F) /* c to digit (unchecked) */
#define cisodigit(c)    ((unsigned)(c)-'0' < 8u)

#endif
