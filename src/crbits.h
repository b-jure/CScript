#ifndef CRBITS_H
#define CRBITS_H


#include "cript.h"



/* raise 2 to the power of 'x' */
#define twoto(x)	(1<<(x))


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
#define topow2(x) { \
    (x)--; \
    (x) |= (x) >> 1; \
    (x) |= (x) >> 2; \
    (x) |= (x) >> 4; \
    (x) |= (x) >> 8; \
    (x) |= (x) >> 16; \
    (x)++; }


/* bit manipulation */
#define resetbits(x,m)		((x) &= ~(m))
#define setbits(x,m)		((x) |= (m))
#define testbits(x,m)		((x) & (m))
#define togglebits(x,m,t)	((x) ^ (((x) ^ -((t) != 0)) & (m)))
#define bitmask(b)		(1 << (b))
#define bit2mask(b1,b2)		(bitmask(b1) | bitmask(b2)) 
#define setbit(x,b)		setbits(x, bitmask(b))
#define clearbit(x,b)		resetbits(x, bitmask(b))
#define testbit(x,b)		testbits(x, bitmask(b))
#define togglebit(x,b,t)	togglebits(x, bitmask(b), t)


/* get byte at offset 'o' from 'v' */
#define getbyte(v,o)	(((v) >> ((o) * 8)) & 0xff)


/* 
 * Get first 3 bytes (LE byte order) from 'p' 
 * casted to 'int'
 */
#define get3bytes(p) \
    (cast_int(0) | \
    ((*(cast_ubytep(p) + 2)) << 16) | \
    ((*(cast_ubytep(p) + 1)) << 8) | \
    (*cast_ubytep(p)))


/* 
 * Set first 'n' (LE byte order) bytes from 'src'
 * (integer type) into 'dest' 
 */
#define setbytes(dest,src,n) \
    { for (cr_ubyte i_ = 0; i_ < (n); i_++) \
        *(cast_ubytep(dest) + i_) = getbyte(src, i_); }



/* check if 'c' is octal */
#define isodigit(c)	(isdigit(c) && (c) < '8')


#endif
