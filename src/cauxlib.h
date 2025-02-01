/*
** cauxlib.h
** Auxiliary library
** See Copyright Notice in cscript.h
*/

#ifndef CAUXLIB_H
#define CAUXLIB_H

#include <stdio.h>

#include "cscript.h"


/* global table name */
#define CS_GNAME    "__G"


/* error code for 'csL_loadfile' */
#define CS_ERRFILE      (CS_ERRERROR + 1)


/* key, in the global table, for hashtable of loaded modules */
#define CS_LOADED_TABLE     "__LOADED"


/* buffer */
typedef struct csL_Buffer csL_Buffer;


/* ------------------------------------------------------------------------ 
** Error functions
** ------------------------------------------------------------------------ */
CSLIB_API int csL_error(cs_State *C, const char *fmt, ...);
CSLIB_API int csL_arg_error(cs_State *C, int argindex, const char *extra);
CSLIB_API int csL_type_error(cs_State *C, int argindex, const char *tname);

/* ------------------------------------------------------------------------ 
** Check functions
** ------------------------------------------------------------------------ */
CSLIB_API cs_Number     csL_check_number(cs_State *C, int index);
CSLIB_API cs_Integer    csL_check_integer(cs_State *C, int index);
CSLIB_API const char   *csL_check_lstring(cs_State *C, int index, size_t *plen);
CSLIB_API void          csL_check_type(cs_State *C, int index, int tt);
CSLIB_API void          csL_check_any(cs_State *C, int index);
CSLIB_API void          csL_check_stack(cs_State *C, int n, const char *msg);
CSLIB_API void         *csL_check_userdata(cs_State *C, int index,
                                           const char *name);
CSLIB_API int           csL_check_option(cs_State *C, int index,
                                         const char *dfl,
                                         const char *const opts[]);

/* ------------------------------------------------------------------------ 
** Optional argument functions
** ------------------------------------------------------------------------ */
CSLIB_API cs_Number   csL_opt_number(cs_State *C, int index, cs_Number dfl);
CSLIB_API cs_Integer  csL_opt_integer(cs_State *C, int index, cs_Integer dfl);
CSLIB_API const char *csL_opt_lstring(cs_State *C, int index, const char *dfl,
                                      size_t *plen);

/* ------------------------------------------------------------------------ 
** Loading functions
** ------------------------------------------------------------------------ */
CSLIB_API int csL_loadfile(cs_State *C, const char *filename);
CSLIB_API int csL_loadstring(cs_State *C, const char *str);
CSLIB_API int csL_loadbuffer(cs_State *C, const char *buff, size_t sz,
                             const char *name);

/* ------------------------------------------------------------------------ 
** Miscellaneous functions
** ------------------------------------------------------------------------ */
CSLIB_API const char *csL_to_lstring(cs_State *C, int index, size_t *plen);
CSLIB_API void        csL_where(cs_State *C, int level);
CSLIB_API int         csL_fileresult(cs_State *C, int ok, const char *fname);
CSLIB_API int         csL_get_property(cs_State *C, int insobj);
CSLIB_API void        csL_set_cindex(cs_State *C, int arrobj, cs_Integer i);
CSLIB_API cs_State   *csL_newstate(void);
CSLIB_API int         csL_get_subtable(cs_State *C, int insobj,
                                       const char *field);
CSLIB_API void        csL_include(cs_State *C, const char *modname,
                                  cs_CFunction openf, int global);
CSLIB_API void       *csL_test_userdata(cs_State *C, int index,
                                        const char *name);
CSLIB_API void        csL_traceback(cs_State *C, cs_State *at, int level,
                                    const char *msg);
CSLIB_API void        csL_set_funcs(cs_State *C, const cs_Entry *l, int nup);

/* ------------------------------------------------------------------------ 
** Useful macros
** ------------------------------------------------------------------------ */
#define csL_typename(ts,index)          cs_typename(ts, cs_type(ts, index))

#define csL_check_string(ts,index)      csL_check_lstring(ts, index, NULL)
#define csL_opt_string(ts,index,dfl)    csL_opt_lstring(ts, index, dfl, NULL)

#define csL_opt(ts,fn,index,dfl) \
        (cs_is_noneornil(ts, index) ? (dfl) : fn(ts, index))

#define csL_check_arg(ts,cond,arg,extramsg) \
        ((void)(csi_likely(cond) || csL_arg_error(ts, (arg), (extramsg))))

#define csL_expect_arg(ts,cond,arg,tname) \
        ((void)(csi_likely(cond) || csL_type_error(ts, (arg), (tname))))

#define csL_push_fail(ts)               cs_push_nil(ts)

#define csL_get_gsubtable(ts, name) \
    { cs_push_globaltable(ts); \
      csL_get_subtable(ts, -1, name); \
      cs_remove(ts, -2); }


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

/* ------------------------------------------------------------------------ 
** Buffer manipulation
** ------------------------------------------------------------------------ */
struct csL_Buffer {
    char *b;
    size_t n;
    size_t sz;
    cs_State *C;
    union {
        CSI_MAXALIGN;
        char b[CSL_BUFFERSIZE];
    } init;
};

#define csL_bufflen(B)          ((B)->n)
#define csL_buffptr(B)          ((B)->b)

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
CSLIB_API void  csL_buff_end(csL_Buffer *B);

/* ------------------------------------------------------------------------ 
** Basic message reporting
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
