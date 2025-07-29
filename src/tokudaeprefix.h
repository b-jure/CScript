/*
** tokudaeprefix.h
** Definitions for Tokudae code that must come before any other header file
** See Copyright Notice in tokudae.h
*/

#ifndef tokudaeprefix_h
#define tokudaeprefix_h

/* Enable some POSIX functions and definitions */
#if !defined(_XOPEN_SOURCE)
#define _XOPEN_SOURCE       600
#define TOKU_POSIX_REV      2004
#elif _XOPEN_SOURCE == 0
#undef _XOPEN_SOURCE
#undef TOKU_POSIX_REV
#endif

/* Allows manipulation of large files in gcc and some other compilers */
#if !defined(_FILE_OFFSET_BITS)
#define _LARGEFILE_SOURCE       1
#define _FILE_OFFSET_BITS       64
#endif


/* Windows stuff */
#if defined(_WIN32)	/* { */

#if !defined(_CRT_SECURE_NO_WARNINGS)
#define _CRT_SECURE_NO_WARNINGS  /* avoid warnings about ISO C functions */
#endif

#endif			/* } */

/// TODO: remove
#include <stdio.h>
#include "ttrace.h"

#endif
