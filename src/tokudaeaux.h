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
#define TOKU_GNAME      "__G"


// TODO: update docs
/* type for storing name:function pairs or placeholders */
typedef struct tokuL_Entry tokuL_Entry;

/* type for storing metaindex:function pairs */
typedef struct tokuL_MetaEntry tokuL_MetaEntry;


/* type for buffering system */
typedef struct tokuL_Buffer tokuL_Buffer;


/* new status code for file-related errors in 'tokuL_loadfile' */
#define TOKU_STATUS_EFILE       TOKU_STATUS_NUM


/*
** Meta tag for a new metalist metamethod that triggers in
** 'tokuL_to_lstring'
*/
#define TOKU_MT_TOSTRING        TOKU_MT_NUM


/* key, in the ctable, for table of loaded modules */
#define TOKU_LOADED_TABLE       "__LOADED"


/* key, in the ctable, for table of preloaded loaders */
#define TOKU_PRELOAD_TABLE      "__PRELOAD"


/* {=======================================================================
** Errors
** ======================================================================== */
TOKULIB_API int tokuL_error(toku_State *T, const char *fmt, ...);
TOKULIB_API int tokuL_error_arg(toku_State *T, int idx, const char *extra);
TOKULIB_API int tokuL_error_type(toku_State *T, int idx, const char *tname);
/* }======================================================================= */

/* {=======================================================================
** Required argument/option and other checks
** ======================================================================== */
TOKULIB_API toku_Number  tokuL_check_number(toku_State *T, int idx);
TOKULIB_API toku_Integer tokuL_check_integer(toku_State *T, int idx);
TOKULIB_API const char *tokuL_check_lstring(toku_State *T, int idx, size_t *l);
TOKULIB_API void    tokuL_check_type(toku_State *T, int idx, int t);
TOKULIB_API void    tokuL_check_any(toku_State *T, int idx);
TOKULIB_API void    tokuL_check_stack(toku_State *T, int sz, const char *msg);
TOKULIB_API void    tokuL_check_version_(toku_State *T, toku_Number ver);
TOKULIB_API void   *tokuL_check_userdata(toku_State *T, int idx,
                                               const char *name);
TOKULIB_API int     tokuL_check_option(toku_State *T, int idx, const char *dfl,
                                       const char *const opts[]);
/* }======================================================================= */

/* {=======================================================================
** Optional argument
** ======================================================================== */
TOKULIB_API toku_Number   tokuL_opt_number(toku_State *T, int idx,
                                           toku_Number dfl);
TOKULIB_API toku_Integer  tokuL_opt_integer(toku_State *T, int idx,
                                            toku_Integer dfl);
TOKULIB_API const char *tokuL_opt_lstring(toku_State *T, int idx,
                                          const char *dfl, size_t *l);
/* }======================================================================= */

/* {=======================================================================
** Chunk loading
** ======================================================================== */
TOKULIB_API int tokuL_loadfile(toku_State *T, const char *filename);
TOKULIB_API int tokuL_loadstring(toku_State *T, const char *s);
TOKULIB_API int tokuL_loadbuffer(toku_State *T, const char *buff, size_t sz,
                                 const char *name);
/* }======================================================================= */

/* {=======================================================================
** Userdata and Metalist functions
** ======================================================================== */
TOKULIB_API int   tokuL_new_metalist(toku_State *T, const char *ln);
TOKULIB_API void  tokuL_set_metalist(toku_State *T, const char *ln);
TOKULIB_API int   tokuL_get_metaindex(toku_State *T, int idx, int mt);
TOKULIB_API int   tokuL_callmeta(toku_State *T, int idx, int mt);
TOKULIB_API int   tokuL_new_usermethods(toku_State *T, const char *tn, int sz);
TOKULIB_API void  tokuL_set_usermethods(toku_State *T, const char *tn);
TOKULIB_API void *tokuL_test_userdata(toku_State *T, int idx, const char *ln);

typedef struct tokuL_MetaEntry {
    int mt; /* metatag */
    toku_CFunction metaf; /* metamethod function */
} tokuL_MetaEntry;

TOKULIB_API void tokuL_set_metafuncs(toku_State *T, const tokuL_MetaEntry *l,
                                     int nup);
/* }======================================================================= */

/* {=======================================================================
** File/Exec result process functions
** ======================================================================== */
TOKULIB_API int tokuL_fileresult(toku_State *T, int ok, const char *fname);
TOKULIB_API int tokuL_execresult(toku_State *T, int stat);
/* }======================================================================= */

/* {=======================================================================
** Miscellaneous functions
** ======================================================================== */
TOKULIB_API const char *tokuL_to_lstring(toku_State *T, int idx, size_t *len);
TOKULIB_API void       *tokuL_to_fulluserdata(toku_State *T, int idx);
TOKULIB_API void       *tokuL_to_lightuserdata(toku_State *T, int idx);
TOKULIB_API void        tokuL_where(toku_State *T, int lvl);
TOKULIB_API int      tokuL_get_fieldstr(toku_State *T, int idx, const char *f);
TOKULIB_API int         tokuL_get_property(toku_State *T, int idx);
TOKULIB_API toku_State *tokuL_newstate(void);
TOKULIB_API int      tokuL_get_subtable(toku_State *T, int idx, const char *f);
TOKULIB_API void        tokuL_importf(toku_State *T, const char *modname,
                                      toku_CFunction fopen, int global);
TOKULIB_API void        tokuL_traceback(toku_State *T, toku_State *T1, int lvl,
                                        const char *msg);
TOKULIB_API const char *tokuL_gsub(toku_State *T, const char *s, const char *p,
                                   const char *r);
TOKULIB_API unsigned    tokuL_makeseed(toku_State *T);

struct tokuL_Entry {
    const char *name; /* NULL if sentinel entry */
    toku_CFunction func; /* NULL if placeholder or sentinel entry */
};

TOKULIB_API void tokuL_set_funcs(toku_State *T, const tokuL_Entry *l, int nup);
/* }======================================================================= */

/* {=======================================================================
** Reference system
** ======================================================================== */
#define TOKU_NOREF      (-2)
#define TOKU_REFNIL     (-1)

TOKULIB_API int  tokuL_ref(toku_State *T, int a);
TOKULIB_API void tokuL_unref(toku_State *T, int a, int ref);
/* }======================================================================= */

/* {=======================================================================
** Useful macros
** ======================================================================== */
#define tokuL_check_version(C)      tokuL_check_version_(C, TOKU_VERSION_NUM)
#define tokuL_check_string(C,idx)   tokuL_check_lstring(C, idx, NULL)

#define tokuL_check_arg(C,cond,idx,extramsg) \
        ((void)(csi_likely(cond) || tokuL_error_arg(C, (idx), (extramsg))))

#define tokuL_opt_string(C,idx,dfl)     tokuL_opt_lstring(C, idx, dfl, NULL)

#define tokuL_opt(C,fn,idx,dfl) \
        (toku_is_noneornil(C, idx) ? (dfl) : fn(C, idx))

#define tokuL_expect_arg(C,cond,idx,tname) \
        ((void)(csi_likely(cond) || tokuL_error_type(C, (idx), (tname))))

#define tokuL_typename(C,idx)   toku_typename(C, toku_type(C, idx))

#define tokuL_push_fail(C)       toku_push_nil(C)
#define tokuL_push_libtable(C,l) toku_push_table(C, sizeof(l)/sizeof((l)[0])-1)

#define tokuL_push_lib(C,l) \
    (tokuL_check_version(C), tokuL_push_libtable(C,l), tokuL_set_funcs(C,l,0))

#define tokuL_push_metalist(C,lname,l) \
        (tokuL_new_metalist(C,lname), tokuL_set_metafuncs(C,l,0))

#define tokuL_push_methods(C,tname,l) \
    { tokuL_new_usermethods(C, tname, sizeof(l)/sizeof(l[0]) - 1); \
      tokuL_set_funcs(C,l,0); }

#define tokuL_get_methods(C,tname)      toku_get_cfieldstr(C, tname)
#define tokuL_get_metalist(C,lname)     toku_get_cfieldstr(C, lname)

// TODO: add docs
#define tokuL_find_index(C,idx,mask) \
        toku_find_index(C, idx, mask, 0, toku_len(C, idx))


/*
** Perform arithmetic operations on 'toku_Integer' values with wrap-around
** semantics, as the Tokudae core does.
*/
#define tokuL_intop(op,x,y) \
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
struct tokuL_Buffer {
    char *b; /* buffer address */
    size_t n; /* buffer size */
    size_t sz; /* number of characters in buffer */
    toku_State *T;
    union {
        TOKUI_MAXALIGN; /* ensure maximum alignment for buffer */
        char b[CSL_BUFFERSIZE]; /* initial buffer */
    } init;
};

#define tokuL_buffptr(B)      ((B)->b)
#define tokuL_bufflen(B)      ((B)->n)

#define tokuL_buffadd(B, sz)      ((B)->n += (sz))
#define tokuL_buffsub(B, sz)      ((B)->n -= (sz))

#define tokuL_buff_push(B, c) \
        ((void)((B)->n < (B)->sz || tokuL_buff_ensure((B), 1)), \
        ((B)->b[(B)->n++] = (c)))

TOKULIB_API void  tokuL_buff_init(toku_State *T, tokuL_Buffer *B);
TOKULIB_API char *tokuL_buff_initsz(toku_State *T, tokuL_Buffer *B, size_t sz);
TOKULIB_API char *tokuL_buff_ensure(tokuL_Buffer *B, size_t sz);
TOKULIB_API void  tokuL_buff_push_lstring(tokuL_Buffer *B, const char *s,
                                          size_t l);
TOKULIB_API void  tokuL_buff_push_string(tokuL_Buffer *B, const char *s);
TOKULIB_API void  tokuL_buff_push_stack(tokuL_Buffer *B);
TOKULIB_API void  tokuL_buff_push_gsub(tokuL_Buffer *B, const char *s,
                                       const char *p, const char *r);
TOKULIB_API void  tokuL_buff_end(tokuL_Buffer *B);
TOKULIB_API void  tokuL_buff_endsz(tokuL_Buffer *B, size_t sz);

#define tokuL_buff_prep(B)    tokuL_buff_ensure(B, CSL_BUFFERSIZE)
/* }======================================================================= */

/* {=======================================================================
** File handles for IO library
** ======================================================================== */

/*
** A file handle is a userdata with 'TOKU_FILEHANDLE' metalist,
** 'TOKU_FILEHANDLE_METHODS' methods table and initial structure
** 'tokuL_Stream' (it may contain other fields after that initial
** structure).
*/

#define TOKU_FILEHANDLE   "FILE*"

#define TOKU_FILEHANDLE_METHODS   "FILEMTAB*"

typedef struct tokuL_Stream {
    FILE *f; /* stream (NULL for incompletely created streams) */
    toku_CFunction closef; /* to close stream (NULL for closed streams) */
} tokuL_Stream;

/* }======================================================================= */

#endif
