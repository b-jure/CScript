/*
** tokudaeaux.h
** Auxiliary library
** See Copyright Notice in tokudae.h
*/

#ifndef tokudaeaux_h
#define tokudaeaux_h

#include <stddef.h>
#include <stdio.h>

#include "tokudaeconf.h"
#include "tokudae.h"


/* global table */
#define TOKU_GNAME    "__G"


// TODO: update docs
/* type for storing name:function pairs or placeholders */
typedef struct csL_Entry csL_Entry;

/* type for storing metaindex:function pairs */
typedef struct csL_MetaEntry csL_MetaEntry;


/* type for buffering system */
typedef struct csL_Buffer csL_Buffer;


/* new status code for file-related errors in 'csL_loadfile' */
#define TOKU_STATUS_EFILE     TOKU_STATUS_NUM


/*
** Meta tag for a new metalist metamethod that triggers in
** 'csL_to_lstring'
*/
#define TOKU_MT_TOSTRING      TOKU_MT_NUM


/* key, in the ctable, for table of loaded modules */
#define TOKU_LOADED_TABLE     "__LOADED"


/* key, in the ctable, for table of preloaded loaders */
#define TOKU_PRELOAD_TABLE    "__PRELOAD"


/* {=======================================================================
** Errors
** ======================================================================== */
CSLIB_API int csL_error(toku_State *T, const char *fmt, ...);
CSLIB_API int csL_error_arg(toku_State *T, int index, const char *extra);
CSLIB_API int csL_error_type(toku_State *T, int index, const char *tname);
/* }======================================================================= */

/* {=======================================================================
** Required argument/option and other checks
** ======================================================================== */
CSLIB_API toku_Number     csL_check_number(toku_State *T, int index);
CSLIB_API toku_Integer    csL_check_integer(toku_State *T, int index);
CSLIB_API const char   *csL_check_lstring(toku_State *T, int index, size_t *l);
CSLIB_API void          csL_check_type(toku_State *T, int index, int t);
CSLIB_API void          csL_check_any(toku_State *T, int index);
CSLIB_API void          csL_check_stack(toku_State *T, int sz, const char *msg);
CSLIB_API void          csL_check_version_(toku_State *T, toku_Number ver);
CSLIB_API void         *csL_check_userdata(toku_State *T, int index,
                                           const char *name);
CSLIB_API int           csL_check_option(toku_State *T, int index,
                                         const char *dfl,
                                         const char *const opts[]);
/* }======================================================================= */

/* {=======================================================================
** Optional argument
** ======================================================================== */
CSLIB_API toku_Number   csL_opt_number(toku_State *T, int index, toku_Number dfl);
CSLIB_API toku_Integer  csL_opt_integer(toku_State *T, int index, toku_Integer dfl);
CSLIB_API const char *csL_opt_lstring(toku_State *T, int index, const char *dfl,
                                      size_t *l);
/* }======================================================================= */

/* {=======================================================================
** Chunk loading
** ======================================================================== */
CSLIB_API int csL_loadfile(toku_State *T, const char *filename);
CSLIB_API int csL_loadstring(toku_State *T, const char *s);
CSLIB_API int csL_loadbuffer(toku_State *T, const char *buff, size_t sz,
                             const char *name);
/* }======================================================================= */

/* {=======================================================================
** Userdata and Metalist functions
** ======================================================================== */
CSLIB_API int   csL_new_metalist(toku_State *T, const char *lname);
CSLIB_API void  csL_set_metalist(toku_State *T, const char *lname);
CSLIB_API int   csL_get_metaindex(toku_State *T, int index, int mm);
CSLIB_API int   csL_callmeta(toku_State *T, int index, int mm);
CSLIB_API int   csL_new_usermethods(toku_State *T, const char *tname, int sz);
CSLIB_API void  csL_set_usermethods(toku_State *T, const char *tname);
CSLIB_API void *csL_test_userdata(toku_State *T, int index, const char *lname);

typedef struct csL_MetaEntry {
    int mm; /* metamethod index */
    toku_CFunction metaf; /* metamethod function */
} csL_MetaEntry;

CSLIB_API void csL_set_metafuncs(toku_State *T, const csL_MetaEntry *l, int nup);
/* }======================================================================= */

/* {=======================================================================
** File/Exec result process functions
** ======================================================================== */
CSLIB_API int csL_fileresult(toku_State *T, int ok, const char *fname);
CSLIB_API int csL_execresult(toku_State *T, int stat);
/* }======================================================================= */

/* {=======================================================================
** Miscellaneous functions
** ======================================================================== */
CSLIB_API const char *csL_to_lstring(toku_State *T, int index, size_t *len);
CSLIB_API void       *csL_to_fulluserdata(toku_State *T, int index);
CSLIB_API void       *csL_to_lightuserdata(toku_State *T, int index);
CSLIB_API void        csL_where(toku_State *T, int level);
CSLIB_API int         csL_get_fieldstr(toku_State *T, int index, const char *f);
CSLIB_API int         csL_get_property(toku_State *T, int index);
CSLIB_API toku_State   *csL_newstate(void);
CSLIB_API int         csL_get_subtable(toku_State *T, int index, const char *f);
CSLIB_API void        csL_importf(toku_State *T, const char *modname,
                                  toku_CFunction openf, int global);
CSLIB_API void        csL_traceback(toku_State *T, toku_State *T1, int level,
                                    const char *msg);
CSLIB_API const char *csL_gsub(toku_State *T, const char *s, const char *p,
                               const char *r);
CSLIB_API unsigned    csL_makeseed(toku_State *T);

struct csL_Entry {
    const char *name; /* NULL if sentinel entry */
    toku_CFunction func; /* NULL if placeholder or sentinel entry */
};

CSLIB_API void csL_set_funcs(toku_State *T, const csL_Entry *l, int nup);
/* }======================================================================= */

/* {=======================================================================
** Reference system
** ======================================================================== */
#define TOKU_NOREF        (-2)
#define TOKU_REFNIL       (-1)

CSLIB_API int   csL_ref(toku_State *T, int a);
CSLIB_API void  csL_unref(toku_State *T, int a, int ref);
/* }======================================================================= */

/* {=======================================================================
** Useful macros
** ======================================================================== */
#define csL_check_version(C)        csL_check_version_(C, TOKU_VERSION_NUM)
#define csL_check_string(C,index)   csL_check_lstring(C, index, NULL)

#define csL_check_arg(C,cond,index,extramsg) \
        ((void)(csi_likely(cond) || csL_error_arg(C, (index), (extramsg))))

#define csL_opt_string(C,index,dfl)    csL_opt_lstring(C, index, dfl, NULL)

#define csL_opt(C,fn,index,dfl) \
        (toku_is_noneornil(C, index) ? (dfl) : fn(C, index))

#define csL_expect_arg(C,cond,index,tname) \
        ((void)(csi_likely(cond) || csL_error_type(C, (index), (tname))))

#define csL_typename(C,index)   toku_typename(C, toku_type(C, index))

#define csL_push_fail(C)        toku_push_nil(C)
#define csL_push_libtable(C,l)  toku_push_table(C, sizeof(l)/sizeof((l)[0])-1)

#define csL_push_lib(C,l) \
        (csL_check_version(C), csL_push_libtable(C,l), csL_set_funcs(C,l,0))

#define csL_push_metalist(C,lname,l) \
        (csL_new_metalist(C,lname), csL_set_metafuncs(C,l,0))

#define csL_push_methods(C,tname,l) \
    { csL_new_usermethods(C, tname, sizeof(l)/sizeof(l[0]) - 1); \
      csL_set_funcs(C,l,0); }

#define csL_get_methods(C,tname)    toku_get_cfieldstr(C, tname)
#define csL_get_metalist(C,lname)   toku_get_cfieldstr(C, lname)

// TODO: add docs
#define csL_find_index(C,index,mask) \
        toku_find_index(C, index, mask, 0, toku_len(C, index))


/*
** Perform arithmetic operations on 'toku_Integer' values with wrap-around
** semantics, as the Tokudae core does.
*/
#define csL_intop(op,x,y) \
	((toku_Integer)((toku_Unsigned)(x) op (toku_Unsigned)(y)))


/* internal assertions */
#if !defined(toku_assert)

#if defined(TOKUI_ASSERT)
#include <assert.h>
#define toku_assert(e)	    assert(e)
#else
#define toku_assert(e)	    ((void)0)
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
    toku_State *T;
    union {
        TOKUI_MAXALIGN; /* ensure maximum alignment for buffer */
        char b[CSL_BUFFERSIZE]; /* initial buffer */
    } init;
};

#define csL_buffptr(B)      ((B)->b)
#define csL_bufflen(B)      ((B)->n)

#define csL_buffadd(B, sz)      ((B)->n += (sz))
#define csL_buffsub(B, sz)      ((B)->n -= (sz))

#define csL_buff_push(B, c) \
        ((void)((B)->n < (B)->sz || csL_buff_ensure((B), 1)), \
        ((B)->b[(B)->n++] = (c)))

CSLIB_API void  csL_buff_init(toku_State *T, csL_Buffer *B);
CSLIB_API char *csL_buff_initsz(toku_State *T, csL_Buffer *B, size_t sz);
CSLIB_API char *csL_buff_ensure(csL_Buffer *B, size_t sz);
CSLIB_API void  csL_buff_push_lstring(csL_Buffer *B, const char *s, size_t l);
CSLIB_API void  csL_buff_push_string(csL_Buffer *B, const char *s);
CSLIB_API void  csL_buff_push_stack(csL_Buffer *B);
CSLIB_API void  csL_buff_push_gsub(csL_Buffer *B, const char *s,
                                   const char *p, const char *r);
CSLIB_API void  csL_buff_end(csL_Buffer *B);
CSLIB_API void  csL_buff_endsz(csL_Buffer *B, size_t sz);

#define csL_buff_prep(B)    csL_buff_ensure(B, CSL_BUFFERSIZE)
/* }======================================================================= */

/* {=======================================================================
** File handles for IO library
** ======================================================================== */

/*
** A file handle is a userdata with 'TOKU_FILEHANDLE' metalist,
** 'TOKU_FILEHANDLE_METHODS' methods table and initial structure
** 'csL_Stream' (it may contain other fields after that initial
** structure).
*/

#define TOKU_FILEHANDLE   "FILE*"

#define TOKU_FILEHANDLE_METHODS   "FILEMTAB*"

typedef struct csL_Stream {
    FILE *f; /* stream (NULL for incompletely created streams) */
    toku_CFunction closef; /* to close stream (NULL for closed streams) */
} csL_Stream;

/* }======================================================================= */

#endif
