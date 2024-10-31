/*
** cauxlib.h
** Auxiliary library
** See Copyright Notice in cscript.h
*/

#include <stdio.h>

#include "cscript.h"


/* error code for file related errors */
#define CR_ERRFILE      (CR_ERRERROR + 1)


/* 
** Name of the field in global table where the library instance
** is located. All loaded libraries are set as fields of this
** instance.
*/
#define CR_LOADED_LIBS      "LOADED_LIBS"


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
CRLIB_API cr_State   *crL_newstate(void);

CRLIB_API void *crL_test_userdata(cr_State *ts, int index, const char *name);
CRLIB_API void  crL_traceback(cr_State *ts, cr_State *at, int level,
                              const char *msg);


/* ------------------------------------------------------------------------ 
** useful macros
** ------------------------------------------------------------------------ */
#define crL_typename(ts, index)         cr_typename(ts, cr_type(ts, index))

#define crL_check_string(ts, index)     crL_check_lstring(ts, index, NULL)
#define crL_opt_string(ts, index, dfl)  crL_opt_lstring(ts, index, dfl, NULL)

#define crL_opt(ts, fn, index, dfl) \
    (cr_is_noneornil(ts, index) ? (dfl) : fn(ts, index))

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

#define crL_buffpush(B, c) \
    ((void)((B)->n < (B)->sz || crL_buffensure((B), 1)), \
     ((B)->b[(B)->n++] = (c)))

CRLIB_API void  crL_buffinit(cr_State *ts, crL_Buffer *B);
CRLIB_API char *crL_buffinitsz(cr_State *ts, crL_Buffer *B, size_t sz);
CRLIB_API char *crL_buffensure(crL_Buffer *B, size_t sz);
CRLIB_API void  crL_buffpush_lstring(crL_Buffer *B, const char *s, size_t l);
CRLIB_API void  crL_buffpush_string(crL_Buffer *B, const char *s);
CRLIB_API void  crL_buffpush_value(crL_Buffer *B);
CRLIB_API void crL_buffend(crL_Buffer *B);


/* ------------------------------------------------------------------------ 
** basic report of messages and errors
** ------------------------------------------------------------------------ */
/* write a message (stdout) */
#if !defined(cst_writestring)
#define cst_writestring(s, l)   fwrite((s), sizeof(char), (l), stdout)
#endif

/* write a newline and flush the output (stdout) */
#if !defined(cst_writeline)
#define cst_writeline()         (cst_writestring("\n", 1), fflush(stdout))
#endif

/* write error message (stderr) */
#if !defined(cst_writeerror)
#define cst_writeerror(msg)     (fputs(msg, stderr), fflush(stderr))
#endif

/* write formatted error message (stderr) */
#if !defined(cst_writeferror)
#define cst_writeferror(msg, ...) \
    (fprintf(stderr, msg, __VA_ARGS__), fflush(stderr))
#endif
