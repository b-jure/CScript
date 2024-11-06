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
#define CS_ERRFILE      (CS_ERRERROR + 1)


/* 
** Name of the field in global table where the 'Lib' instance is located.
** All loaded libraries are set as fields of this instance.
*/
#define CS_LOADED_INSTANCE      "Lib"


/*
** This is maximum value of index when indexing the array.
*/
#define CS_ARRAYMAX     (INT_MAX - 1)


/* buffer */
typedef struct crL_Buffer crL_Buffer;


/* ------------------------------------------------------------------------ 
** error functions
** ------------------------------------------------------------------------ */
CRLIB_API int crL_error(cs_State *ts, const char *fmt, ...);
CRLIB_API int crL_arg_error(cs_State *ts, int argindex, const char *extra);
CRLIB_API int crL_type_error(cs_State *ts, int argindex, const char *tname);


/* ------------------------------------------------------------------------ 
** check functions
** ------------------------------------------------------------------------ */
CRLIB_API cs_Number     crL_check_number(cs_State *ts, int index);
CRLIB_API cs_Integer    crL_check_integer(cs_State *ts, int index);
CRLIB_API const char   *crL_check_lstring(cs_State *ts, int index, size_t *plen);
CRLIB_API void          crL_check_type(cs_State *ts, int index, int tt);
CRLIB_API void          crL_check_any(cs_State *ts, int index);
CRLIB_API void          crL_check_stack(cs_State *ts, int n, const char *msg);
CRLIB_API void         *crL_check_userdata(cs_State *ts, int index,
                                           const char *name);
CRLIB_API int           crL_check_option(cs_State *ts, int index,
                                         const char *dfl,
                                         const char *const opts[]);


/* ------------------------------------------------------------------------ 
** optional argument functions
** ------------------------------------------------------------------------ */
CRLIB_API cs_Number   crL_opt_number(cs_State *ts, int index, cs_Number dfl);
CRLIB_API cs_Integer  crL_opt_integer(cs_State *ts, int index, cs_Integer dfl);
CRLIB_API const char *crL_opt_lstring(cs_State *ts, int index, const char *dfl,
                                      size_t *plen);


/* ------------------------------------------------------------------------ 
** loading functions
** ------------------------------------------------------------------------ */
CRLIB_API int crL_loadfile(cs_State *ts, const char *filename);
CRLIB_API int crL_loadstring(cs_State *ts, const char *str);
CRLIB_API int crL_loadbuffer(cs_State *ts, const char *buff, size_t sz,
                             const char *name);


/* ------------------------------------------------------------------------ 
** miscellaneous functions
** ------------------------------------------------------------------------ */
CRLIB_API const char *crL_to_lstring(cs_State *ts, int index, size_t *plen);
CRLIB_API void        crL_where(cs_State *ts, int level);
CRLIB_API int         crL_fileresult(cs_State *ts, int ok, const char *fname);
CRLIB_API int         crL_get_property(cs_State *ts, int insobj);
CRLIB_API void        crL_set_cindex(cs_State *ts, int arrobj, cs_Integer i);
CRLIB_API cs_State   *crL_newstate(void);
CRLIB_API void        crL_push_hashtable(cs_State *ts);
CRLIB_API int         crL_get_subinstance(cs_State *ts, int idx,
                                          const char *field);
CRLIB_API void        crL_include(cs_State *ts, const char *lib,
                                  cs_CFunction openf, int global);
CRLIB_API void       *crL_test_userdata(cs_State *ts, int index,
                                        const char *name);
CRLIB_API void        crL_traceback(cs_State *ts, cs_State *at, int level,
                                    const char *msg);
CRLIB_API void        crL_set_funcs(cs_State *ts, const cs_Entry *l, int nup);


/* ------------------------------------------------------------------------ 
** useful macros
** ------------------------------------------------------------------------ */
#define crL_typename(ts, index)         cs_typename(ts, cs_type(ts, index))

#define crL_check_string(ts, index)     crL_check_lstring(ts, index, NULL)
#define crL_opt_string(ts, index, dfl)  crL_opt_lstring(ts, index, dfl, NULL)

#define crL_opt(ts, fn, index, dfl) \
    (cs_is_noneornil(ts, index) ? (dfl) : fn(ts, index))

#define crL_check_arg(ts, cond, arg, extramsg) \
    ((void)(cs_likely(cond) || crL_arg_error(ts, (arg), (extramsg))))

#define crL_push_fail(ts)               cs_push_nil(ts)

#define crL_push_hashtable(ts) \
    { cs_get_global(ts, CS_HASHTABLE); cs_push_instance(ts, -1); }


/* internal assertions */
#if !defined(cs_assert)
#if defined CSI_ASSERT
  #include <assert.h>
  #define cs_assert(e)	    assert(e)
#else
  #define cs_assert(e)	    ((void)0)
#endif
#endif


/* ------------------------------------------------------------------------ 
** buffer manipulation
** ------------------------------------------------------------------------ */

struct crL_Buffer {
    char *b;
    size_t n;
    size_t sz;
    cs_State *ts;
    union {
        CSI_MAXALIGN;
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

CRLIB_API void  crL_buff_init(cs_State *ts, crL_Buffer *B);
CRLIB_API char *crL_buff_initsz(cs_State *ts, crL_Buffer *B, size_t sz);
CRLIB_API char *crL_buff_ensure(crL_Buffer *B, size_t sz);
CRLIB_API void  crL_buff_push_lstring(crL_Buffer *B, const char *s, size_t l);
CRLIB_API void  crL_buff_push_string(crL_Buffer *B, const char *s);
CRLIB_API void  crL_buff_push_value(crL_Buffer *B);
CRLIB_API void crL_buff_end(crL_Buffer *B);


/* ------------------------------------------------------------------------ 
** basic message reporting
** ------------------------------------------------------------------------ */
/* write a message to 'fp' stream */
#if !defined(cs_writelen)
#define cs_writelen(fp,s,l)    fwrite((s), sizeof(char), (l), fp)
#endif

/* write a newline to 'fp' and flush it */
#if !defined(cs_writeline)
#define cs_writeline(fp)       (cs_writelen(fp, "\n", 1), fflush(fp))
#endif

/* write formatted message to 'fp' and flush it */
#if !defined(cs_writefmt)
#define cs_writefmt(fp, msg, ...)  (fprintf(fp, msg, __VA_ARGS__), fflush(fp))
#endif

/* write formatted message to 'fp' ('ap' is va_list) and flush it */
#if !defined(cs_writevfmt)
#define cs_writevfmt(fp,msg,ap)    (vfprintf(fp, msg, ap), fflush(fp))
#endif


#endif
