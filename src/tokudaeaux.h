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


/* type for buffering system */
typedef struct tokuL_Buffer tokuL_Buffer;


/* new status code for file-related errors in 'tokuL_loadfile' */
#define TOKU_STATUS_EFILE       TOKU_STATUS_NUM


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
** Userdata and metatable functions
** ======================================================================== */
TOKULIB_API int   tokuL_new_metatable(toku_State *T, const char *ln);
TOKULIB_API void  tokuL_set_metatable(toku_State *T, const char *ln);
TOKULIB_API int   tokuL_get_metafield(toku_State *T, int idx, const char *ev);
TOKULIB_API int   tokuL_callmeta(toku_State *T, int idx, const char *ev);
TOKULIB_API void *tokuL_test_userdata(toku_State *T, int idx, const char *ln);
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
TOKULIB_API toku_State *tokuL_newstate(void);
TOKULIB_API int      tokuL_get_subtable(toku_State *T, int idx, const char *k);
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
#define tokuL_check_version(T)      tokuL_check_version_(T, TOKU_VERSION_NUM)

#define tokuL_check_string(T,idx)   tokuL_check_lstring(T, idx, NULL)

#define tokuL_check_arg(T,cond,idx,extramsg) \
        ((void)(tokui_likely(cond) || tokuL_error_arg(T, (idx), (extramsg))))

#define tokuL_opt_string(T,idx,dfl)     tokuL_opt_lstring(T, idx, dfl, NULL)

#define tokuL_opt(T,fn,idx,dfl) \
        (toku_is_noneornil(T, idx) ? (dfl) : fn(T, idx))

#define tokuL_expect_arg(T,cond,idx,tname) \
        ((void)(tokui_likely(cond) || tokuL_error_type(T, (idx), (tname))))

#define tokuL_typename(T,idx)   toku_typename(T, toku_type(T, idx))

#define tokuL_push_fail(T)      toku_push_nil(T)

#define tokuL_push_libtable(T,l) \
        toku_push_table(T, sizeof(l)/sizeof((l)[0])-1)

#define tokuL_push_lib(T,l) \
    (tokuL_check_version(T), tokuL_push_libtable(T,l), tokuL_set_funcs(T,l,0))

#define tokuL_push_methods(T,tname,l) \
    { tokuL_new_usermethods(T, tname, sizeof(l)/sizeof(l[0]) - 1); \
      tokuL_set_funcs(T,l,0); }

#define tokuL_get_metatable(T,tname)    toku_get_cfield_str(T, (tname))

#define tokuL_find_index(T,idx,mask) \
        toku_find_index(T, idx, mask, 0, toku_len(T, idx))


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
        char b[TOKUL_BUFFERSIZE]; /* initial buffer */
    } init;
};

#define tokuL_buffptr(B)    ((B)->b)
#define tokuL_bufflen(B)    ((B)->n)

#define tokuL_buffadd(B, sz)    ((B)->n += (sz))
#define tokuL_buffsub(B, sz)    ((B)->n -= (sz))

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

#define tokuL_buff_prep(B)      tokuL_buff_ensure(B, TOKUL_BUFFERSIZE)
/* }======================================================================= */

/* {=======================================================================
** File handles for IO library
** ======================================================================== */

/*
** A file handle is a userdata with 'TOKU_FILEHANDLE' metatable,
** and initial structure 'tokuL_Stream' (it may contain other fields
** after that initial structure).
*/

#define TOKU_FILEHANDLE   "FILE*"

typedef struct tokuL_Stream {
    FILE *f; /* stream (NULL for incompletely created streams) */
    toku_CFunction closef; /* to close stream (NULL for closed streams) */
} tokuL_Stream;

/* }======================================================================= */

#endif
