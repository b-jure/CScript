#ifndef CRBITS_H
#define CRBITS_H


#include "cript.h"


/* 
 * Max/Min value for integral type 't'. 
 * This assumes two-complement representation.
 */
#define bumax(t) ((t)(~(t)0))
#define bumin(t) ((t)0)
#define bsmax(t) (bumax(t)>>1)
#define bsmin(t) bumax(t)


/* check if 'x' is power of 2 */
#define ispow2(x) (((x)&((x)-1)) == 0)


/* 
 * round 'x' to the next highest power of 2 
 * (https://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2Float) 
 */
#define topow2(x) { (x)--; (x) |= (x) >> 1; \
	(x) |= (x) >> 2; (x) |= (x) >> 4; (x) \
	|= (x) >> 8; (x) |= (x) >> 16; (x)++; }


/* bit manipulation */
#define resetbits(x, m)		((x) &= ~(m))
#define setbits(x, m)		((x) |= (m))
#define testbits(x, m)		((x) & (m))
#define togglebits(x, m, t)	((x) ^ (((x) ^ -((t) != 0)) & (m)))
#define bitmask(b)		(1 << (b))
#define bit2mask(b1, b2)	(bitmask(b1) | bitmask(b2)) 
#define setbit(x, b)		setbits(x, bitmask(b))
#define clearbit(x, b)		resetbits(x, bitmask(b))
#define testbit(x, b)		testbits(x, bitmask(b))
#define togglebit(x, b, t)	togglebits(x, bitmask(b), t)


/* get byte at offset 'o' from 'x' */
#define getbyte(x, o) (((x) >> ((o) * 8)) & 0xff)


/* get first 3 bytes (LE byte order) from 'p' casted to 'int' */
#define get3bytes(p) \
	(cast(int, 0) | ((*cast(cr_ubyte *, p) + 2) << 16) | \
	 ((*cast(cr_ubyte *, p) + 1) << 8) | *cast(cr_ubyte *, p))


/* set first 3 bytes (LE byte order) from 'src' into 'dest' */
#define set3bytes(dest, src) { \
	*cast(cr_ubyte *, dest) = getbyte(src, 0); \
	*(cast(cr_ubyte *, dest) + 1) = getbyte(src, 1); \
	*(cast(cr_ubyte *, dest) + 2) = getbyte(src, 2); }


#endif
