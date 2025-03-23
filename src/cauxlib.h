/*
** cauxlib.h
** Auxiliary library
** See Copyright Notice in cscript.h
*/

#ifndef CAUXLIB_H
#define CAUXLIB_H

#include <stdio.h>

#include "cscript.h"


/* global table */
#define CS_GNAME    "__G"


typedef struct csL_Buffer csL_Buffer;


/* extra error code for 'csL_loadfile' */
#define CS_ERRFILE      (CS_ERRERROR + 1)


/* key, in the registry table, for table of loaded modules */
#define CS_LOADED_TABLE     "__LOADED"


/* key, in the registry table, for table of preloaded loaders */
#define CS_PRELOAD_TABLE    "__PRELOAD"



/* {=======================================================================
** Errors
** ======================================================================== */

CSLIB_API int csL_error(cs_State *C, const char *fmt, ...);
CSLIB_API int csL_error_arg(cs_State *C, int index, const char *extra);
CSLIB_API int csL_error_type(cs_State *C, int index, const char *tname);

/* }======================================================================= */


/* {=======================================================================
** Required argument
** ======================================================================== */
CSLIB_API cs_Number     csL_check_number(cs_State *C, int index);
CSLIB_API cs_Integer    csL_check_integer(cs_State *C, int index);
CSLIB_API const char   *csL_check_lstring(cs_State *C, int index, size_t *l);
CSLIB_API void          csL_check_type(cs_State *C, int index, int t);
CSLIB_API void          csL_check_any(cs_State *C, int index);
CSLIB_API void          csL_check_stack(cs_State *C, int sz, const char *msg);
CSLIB_API void         *csL_check_userdata(cs_State *C, int index,
                                           const char *name);
CSLIB_API int           csL_check_option(cs_State *C, int index,
                                         const char *dfl,
                                         const char *const opts[]);
/* }======================================================================= */


/* {=======================================================================
** Optional argument
** ======================================================================== */
CSLIB_API cs_Number   csL_opt_number(cs_State *C, int index, cs_Number dfl);
CSLIB_API cs_Integer  csL_opt_integer(cs_State *C, int index, cs_Integer dfl);
CSLIB_API const char *csL_opt_lstring(cs_State *C, int index, const char *dfl,
                                      size_t *l);
/* }======================================================================= */


/* {=======================================================================
** Chunk loading
** ======================================================================== */
CSLIB_API int csL_loadfile(cs_State *C, const char *filename);
CSLIB_API int csL_loadstring(cs_State *C, const char *s);
CSLIB_API int csL_loadbuffer(cs_State *C, const char *buff, size_t sz,
                             const char *name);
/* }======================================================================= */



/* {=======================================================================
** Metalist functions
** ======================================================================== */
CSLIB_API int   csL_new_metalist(cs_State *C, const char *lname);
CSLIB_API int   csL_set_metalist(cs_State *C, const char *lname);
CSLIB_API void *csL_test_userdata(cs_State *C, int index, const char *lname);
/* }======================================================================= */


/* {=======================================================================
** Miscellaneous functions
** ======================================================================== */
CSLIB_API const char *csL_to_lstring(cs_State *C, int index, size_t *len);
CSLIB_API void       *csL_to_fulluserdata(cs_State *C, int index);
CSLIB_API void       *csL_to_lightuserdata(cs_State *C, int index);
CSLIB_API void        csL_where(cs_State *C, int level);
CSLIB_API int         csL_fileresult(cs_State *C, int ok, const char *fname);
CSLIB_API int         csL_get_property(cs_State *C, int index);
CSLIB_API void        csL_set_index(cs_State *C, int index, int i);
CSLIB_API cs_State   *csL_newstate(void);
CSLIB_API int         csL_get_subtable(cs_State *C, int index,
                                       const char *field);
CSLIB_API void        csL_importf(cs_State *C, const char *modname,
                                  cs_CFunction openf, int global);
CSLIB_API void        csL_traceback(cs_State *C, cs_State *C1, int level,
                                    const char *msg);
CSLIB_API void        csL_setfuncs(cs_State *C, const cs_Entry *l, int nup);
CSLIB_API void        csL_checkversion_(cs_State *C, cs_Number ver);
CSLIB_API const char *csL_gsub(cs_State *C, const char *s, const char *p,
                               const char *r);
CSLIB_API unsigned    csL_makeseed(cs_State *C);
/* }======================================================================= */


/* {=======================================================================
** Reference system
** ======================================================================== */
#define CS_NOREF        (-2)
#define CS_REFNIL       (-1)

CSLIB_API int   csL_ref(cs_State *C, int a);
CSLIB_API void  csL_unref(cs_State *C, int a, int ref);
/* }======================================================================= */


/* {=======================================================================
** Useful macros
** ======================================================================== */
#define csL_checkversion(C)     csL_checkversion_(C, CS_VERSION_NUMBER)

#define csL_typename(C,index)      cs_typename(C, cs_type(C, index))

#define csL_check_string(C,index)      csL_check_lstring(C, index, NULL)
#define csL_opt_string(C,index,dfl)    csL_opt_lstring(C, index, dfl, NULL)

#define csL_opt(C,fn,index,dfl) \
        (cs_is_noneornil(C, index) ? (dfl) : fn(C, index))

#define csL_check_arg(C,cond,index,extramsg) \
        ((void)(csi_likely(cond) || csL_error_arg(C, (index), (extramsg))))

#define csL_expect_arg(C,cond,index,tname) \
        ((void)(csi_likely(cond) || csL_error_type(C, (index), (tname))))

#define csL_push_fail(C)       cs_push_nil(C)

#define csL_newlibtable(C,l) \
        cs_push_table(C, sizeof(l)/sizeof((l)[0]) - 1)

#define csL_newlib(C,l) \
        (csL_checkversion(C), csL_newlibtable(C,l), csL_setfuncs(C,l,0))

#define csL_get_gsubtable(C, name) \
    { cs_push_globaltable(C); \
      csL_get_subtable(C, -1, name); \
      cs_remove(C, -2); }

#define csL_get_rsubtable(C, name) \
    { cs_push_registrytable(C); \
      csL_get_subtable(C, -1, name); \
      cs_remove(C, -2); }

#define csL_get_metalist(C, name)   csL_get_rtable(C, name)


/*
** Perform arithmetic operations on cs_Integer values with wrap-around
** semantics, as the CScript core does.
*/
#define csL_intop(op,x,y) \
	((cs_Integer)((cs_Unsigned)(x) op (cs_Unsigned)(y)))


/* internal assertions */
#if !defined(cs_assert)

#if defined CSI_ASSERT
#include <assert.h>
#define cs_assert(e)	    assert(e)
#else
#define cs_assert(e)	    ((void)0)
#endif

#endif
/* }======================================================================= */


/* {=======================================================================
** Buffer manipulation
** ======================================================================== */
struct csL_Buffer {
    char *b; /* buffer address */
    size_t n; /* buffer size */
    size_t sz; /* number of characters in buffer */
    cs_State *C;
    union {
        CSI_MAXALIGN; /* ensure maximum alignment for buffer */
        char b[CSL_BUFFERSIZE]; /* initial buffer */
    } init;
};

#define csL_buffptr(B)          ((B)->b)
#define csL_bufflen(B)          ((B)->n)

#define csL_buffadd(B, sz)      ((B)->n += (sz))
#define csL_buffsub(B, sz)      ((B)->n -= (sz))

#define csL_buff_push(B, c) \
        ((void)((B)->n < (B)->sz || csL_buff_ensure((B), 1)), \
        ((B)->b[(B)->n++] = (c)))

CSLIB_API void  csL_buff_init(cs_State *C, csL_Buffer *B);
CSLIB_API char *csL_buff_initsz(cs_State *C, csL_Buffer *B, size_t sz);
CSLIB_API char *csL_buff_ensure(csL_Buffer *B, size_t sz);
CSLIB_API void  csL_buff_push_lstring(csL_Buffer *B, const char *s, size_t l);
CSLIB_API void  csL_buff_push_string(csL_Buffer *B, const char *s);
CSLIB_API void  csL_buff_push_stack(csL_Buffer *B);
CSLIB_API void  csL_buff_push_gsub(csL_Buffer *B, const char *s,
                                   const char *p, const char *r);
CSLIB_API void  csL_buff_end(csL_Buffer *B);
CSLIB_API void  csL_buff_endsz(csL_Buffer *B, size_t sz);
/* }======================================================================= */


/* {=======================================================================
** File handles for IO library
** ======================================================================== */
/*
** A file handle is a userdata with 'CS_FILEHANDLE' metalist and
** initial structure 'csL_Stream' (it may contain other fields
** after that initial structure).
*/

#define CS_FILEHANDLE       "FILE*"

typedef struct csL_Stream {
  FILE *f; /* stream (NULL for incompletely created streams) */
  cs_CFunction closef; /* to close stream (NULL for closed streams) */
} csL_Stream;
/* }======================================================================= */


#endif
