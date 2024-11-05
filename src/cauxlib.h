/*
** cauxlib.h
** Auxiliary library
** See Copyright Notice in cscript.h
*/

#ifndef CRAUXLIB_H
#define CRAUXLIB_H


#include <stdio.h>

#include "cscript.h"


/* error code for file related errors */
#define CR_ERRFILE      (CR_ERRERROR + 1)


/*
** Name of the global hashtable class.
*/
#define CR_HASHTABLE        "Hashtable"


/* 
** Name of the field in global table where the 'lib' instance is located.
** All loaded libraries are set as fields of this instance (hashtable).
*/
#define CR_LOADED_LIBS      "lib"


/*
** This is maximum value of index when indexing the array.
*/
#define CR_ARRAYMAX     (INT_MAX - 1)


/* buffer */
typedef struct crL_Buffer crL_Buffer;


/* ------------------------------------------------------------------------ 
** error functions
** ------------------------------------------------------------------------ */
CRLIB_API int crL_error(cr_State *ts, const char *fmt, ...);
CRLIB_API int crL_arg_error(cr_State *ts, int argindex, const char *extra);
CRLIB_API int crL_type_error(cr_State *ts, int argindex, const char *tname);


/* ------------------------------------------------------------------------ 
** check functions
** ------------------------------------------------------------------------ */
CRLIB_API cr_Number     crL_check_number(cr_State *ts, int index);
CRLIB_API cr_Integer    crL_check_integer(cr_State *ts, int index);
CRLIB_API const char   *crL_check_lstring(cr_State *ts, int index, size_t *plen);
CRLIB_API void          crL_check_type(cr_State *ts, int index, int tt);
CRLIB_API void          crL_check_any(cr_State *ts, int index);
CRLIB_API void          crL_check_stack(cr_State *ts, int n, const char *msg);
CRLIB_API void         *crL_check_userdata(cr_State *ts, int index,
                                           const char *name);
CRLIB_API int           crL_check_option(cr_State *ts, int index,
                                         const char *dfl,
                                         const char *const opts[]);


/* ------------------------------------------------------------------------ 
** optional argument functions
** ------------------------------------------------------------------------ */
CRLIB_API cr_Number   crL_opt_number(cr_State *ts, int index, cr_Number dfl);
CRLIB_API cr_Integer  crL_opt_integer(cr_State *ts, int index, cr_Integer dfl);
CRLIB_API const char *crL_opt_lstring(cr_State *ts, int index, const char *dfl,
                                      size_t *plen);


/* ------------------------------------------------------------------------ 
** loading functions
** ------------------------------------------------------------------------ */
CRLIB_API int crL_loadfile(cr_State *ts, const char *filename);
CRLIB_API int crL_loadstring(cr_State *ts, const char *str);
CRLIB_API int crL_loadbuffer(cr_State *ts, const char *buff, size_t sz,
                             const char *name);


/* ------------------------------------------------------------------------ 
** miscellaneous functions
** ------------------------------------------------------------------------ */
CRLIB_API const char *crL_to_lstring(cr_State *ts, int index, size_t *plen);
CRLIB_API void        crL_where(cr_State *ts, int level);
CRLIB_API int         crL_fileresult(cr_State *ts, int ok, const char *fname);
CRLIB_API int         crL_get_property(cr_State *ts, int insobj);
CRLIB_API void        crL_set_cindex(cr_State *ts, int arrobj, cr_Integer i);
CRLIB_API cr_State   *crL_newstate(void);
CRLIB_API void        crL_push_hashtable(cr_State *ts);
CRLIB_API void        crL_include(cr_State *ts, const char *lib,
                                  cr_CFunction openf, int global);
CRLIB_API void       *crL_test_userdata(cr_State *ts, int index,
                                        const char *name);
CRLIB_API void        crL_traceback(cr_State *ts, cr_State *at, int level,
                                    const char *msg);


/* ------------------------------------------------------------------------ 
** useful macros
** ------------------------------------------------------------------------ */
#define crL_typename(ts, index)         cr_typename(ts, cr_type(ts, index))

#define crL_check_string(ts, index)     crL_check_lstring(ts, index, NULL)
#define crL_opt_string(ts, index, dfl)  crL_opt_lstring(ts, index, dfl, NULL)

#define crL_opt(ts, fn, index, dfl) \
    (cr_is_noneornil(ts, index) ? (dfl) : fn(ts, index))

#define crL_check_arg(ts, cond, arg, extramsg) \
    ((void)(cr_likely(cond) || crL_arg_error(ts, (arg), (extramsg))))

/* push value representing failure or error */
#define crL_push_fail(ts)       cr_push_nil(ts)


/* internal assertions */
#if !defined(cr_assert)
#if defined CRI_ASSERT
  #include <assert.h>
  #define cr_assert(e)	    assert(e)
#else
  #define cr_assert(e)	    ((void)0)
#endif
#endif


/* ------------------------------------------------------------------------ 
** buffer manipulation
** ------------------------------------------------------------------------ */

struct crL_Buffer {
    char *b;
    size_t n;
    size_t sz;
    cr_State *ts;
    union {
        CRI_MAXALIGN;
        char b[CRL_BUFFERSIZE];
    } init;
};

#define crL_bufflen(B)          ((B)->n)
#define crL_buffptr(B)          ((B)->b)

#define crL_buffadd(B, sz)      ((B)->n += (sz))
#define crL_buffsub(B, sz)      ((B)->n -= (sz))

#define crL_buff_push(B, c) \
    ((void)((B)->n < (B)->sz || crL_buff_ensure((B), 1)), \
     ((B)->b[(B)->n++] = (c)))

CRLIB_API void  crL_buff_init(cr_State *ts, crL_Buffer *B);
CRLIB_API char *crL_buff_initsz(cr_State *ts, crL_Buffer *B, size_t sz);
CRLIB_API char *crL_buff_ensure(crL_Buffer *B, size_t sz);
CRLIB_API void  crL_buff_push_lstring(crL_Buffer *B, const char *s, size_t l);
CRLIB_API void  crL_buff_push_string(crL_Buffer *B, const char *s);
CRLIB_API void  crL_buff_push_value(crL_Buffer *B);
CRLIB_API void crL_buff_end(crL_Buffer *B);


/* ------------------------------------------------------------------------ 
** basic message reporting
** ------------------------------------------------------------------------ */
/* write a message to 'fp' stream */
#if !defined(cst_writelen)
#define cst_writelen(fp,s,l)    fwrite((s), sizeof(char), (l), fp)
#endif

/* write a newline to 'fp' and flush it */
#if !defined(cst_writeline)
#define cst_writeline(fp)       (cst_writelen(fp, "\n", 1), fflush(fp))
#endif

/* write formatted message to 'fp' and flush it */
#if !defined(cst_writefmt)
#define cst_writefmt(fp, msg, ...)  (fprintf(fp, msg, __VA_ARGS__), fflush(fp))
#endif

/* write formatted message to 'fp' ('ap' is va_list) and flush it */
#if !defined(cst_writevfmt)
#define cst_writevfmt(fp,msg,ap)    (vfprintf(fp, msg, ap), fflush(fp))
#endif


#endif
