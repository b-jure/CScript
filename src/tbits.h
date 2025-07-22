/*
** tbits.h
** Bit manipulation functions
** See Copyright Notice in tokudae.h
*/

#ifndef tbits_h
#define tbits_h


/* raise 2 to the power of 'x' */
#define twoto(x)        (1u<<(x))


/* bit manipulation */
#define resetbits(x,m)	        ((x) &= ~(m))
#define setbits(x,m)		((x) |= (m))
#define testbits(x,m)		((x) & (m))
#define togglebits(x,m,t)	((x) ^ (((x) ^ -((t) != 0)) & (m)))
#define bitmask(b)		(1 << (b))
#define bit2mask(b1,b2)		(bitmask(b1) | bitmask(b2)) 
#define resetbit(x,b)		resetbits(x, bitmask(b))
#define setbit(x,b)		setbits(x, bitmask(b))
#define clearbit(x,b)		resetbits(x, bitmask(b))
#define testbit(x,b)		testbits(x, bitmask(b))
#define togglebit(x,b,t)	togglebits(x, bitmask(b), t)


/* get byte at offset 'o' from 'v' */
#define getbyte(v,o)	    (((v) >> ((o) * 8)) & 0xFF)


/* set 'src' byte at offset 'o' to 'v' */
#define setbyte(src,o,v)    (*(cast_ubytep(src) + (o)) = cast_ubyte(v))


/* 
** Get first 3 bytes (LE byte order) from 'p' casted to 't_uint'.
*/
#define get3bytes(p) \
        cast_int(cast_uint(0) | ((*(cast_ubytep(p) + 2)) << 16) | \
                 ((*(cast_ubytep(p) + 1)) << 8) | (*cast_ubytep(p)))


/* 
** Set first 3 (LE byte order) bytes from 'src'
** (integer type) into 'dest'.
*/
#define set3bytes(dest,src) \
    { t_ubyte *dest_=cast_ubytep(dest); int src_=cast_int(src); \
      setbyte(dest_, 0, getbyte(src_, 0)); \
      setbyte(dest_, 1, getbyte(src_, 1)); \
      setbyte(dest_, 2, getbyte(src_, 2)); }

#endif
