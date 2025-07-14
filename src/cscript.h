/*
** cscript.h
** CScript - A Scripting Language inspired by Lua and C
** See Copyright Notice at the end of this file
*/

#ifndef cscript_h
#define cscript_h

#include <stddef.h>
#include <stdarg.h>


#define CS_VERSION_MAJOR_N      1
#define CS_VERSION_MINOR_N      0
#define CS_VERSION_RELEASE_N    0

#define CS_VERSION_NUM  (CS_VERSION_MAJOR_N * 100 + CS_VERSION_MINOR_N)
#define CS_VERSION_RELEASE_NUM  (CS_VERSION_NUM * 100 + CS_VERSION_RELEASE_N)


#include "cscriptconf.h"


/* option for multiple returns in 'cs_pcall' and 'cs_call' */
#define CS_MULRET       (-1)


// TODO: update docs
/*
** Pseudo-indices
** (-CSI_MAXSTACK is the minimum valid index; we keep some free empty
** space after that to help overflow detection)
*/
#define CS_CLIST_INDEX              (-CSI_MAXSTACK - 1000)
#define CS_CTABLE_INDEX             (CS_CLIST_INDEX - 1)
#define cs_upvalueindex(i)          (CS_CTABLE_INDEX - 1 - (i))


// TODO: update docs
/* predefined values in the C list */
#define CS_CLIST_MAINTHREAD         0
#define CS_CLIST_GLOBALS            1
#define CS_CLIST_LAST               CS_CLIST_GLOBALS
    

// TODO: update docs
/* types of values */
#define CS_T_NONE               (-1)
#define CS_T_NIL                0   /* nil */
#define CS_T_BOOL               1   /* boolean */
#define CS_T_NUMBER             2   /* number */
#define CS_T_USERDATA           3   /* userdata */
#define CS_T_LIGHTUSERDATA      4   /* light userdata */
#define CS_T_STRING             5   /* string */
#define CS_T_LIST               6   /* list */
#define CS_T_TABLE              7   /* table */
#define CS_T_FUNCTION           8   /* function */
#define CS_T_CLASS              9   /* class */
#define CS_T_INSTANCE           10  /* instance */
#define CS_T_THREAD             11  /* thread */
// TODO: #define CS_T_BOUNDMETHOD        12  /* bounded (instance/userdata) method */
#define CS_T_NUM                12  /* total number of types */



/* minimum stack space available to a C function */
#define CS_MINSTACK         20



/* CScript thread state */
typedef struct cs_State cs_State;


/* type for integers */
typedef CS_INTEGER cs_Integer;

/* type for unsigned integers */
typedef CS_UNSIGNED cs_Unsigned;

/* type for floating point numbers */
typedef CS_NUMBER cs_Number;


/* type of C function registered with CScript */
typedef int (*cs_CFunction)(cs_State *C);

/* type of function that de/allocates memory */
typedef void *(*cs_Alloc)(void *ptr, size_t osz, size_t nsz, void *ud);

/* type of function that reads blocks when loading CScript chunks */
typedef const char *(*cs_Reader)(cs_State *C, void *data, size_t *szread);

/* type of warning function */
typedef void (*cs_WarnFunction)(void *ud, const char *msg, int tocont);


/* type for storing name:function pairs or placeholders */
typedef struct cs_Entry cs_Entry;

/* type for debug API */
typedef struct cs_Debug cs_Debug;


/* TODO: add docs */
/* type of function to be called by the debugger in specific events */
typedef void (*cs_Hook)(cs_State *C, cs_Debug *ar);


// TODO: update docs
/* meta tags for indexing the metalist (ORDER MT) */
#define CS_MT_GETIDX    0
#define CS_MT_SETIDX	1
#define CS_MT_GC	2
#define CS_MT_CLOSE	3
#define CS_MT_CALL	4
#define CS_MT_INIT	5
#define CS_MT_CONCAT	6
#define CS_MT_ADD	7
#define CS_MT_SUB	8
#define CS_MT_MUL	9
#define CS_MT_DIV	10
#define CS_MT_IDIV	11
#define CS_MT_MOD	12
#define CS_MT_POW	13
#define CS_MT_BSHL	14
#define CS_MT_BSHR	15
#define CS_MT_BAND	16
#define CS_MT_BOR	17
#define CS_MT_BXOR	18
#define CS_MT_UNM	19
#define CS_MT_BNOT	20
#define CS_MT_EQ	21
#define CS_MT_LT	22
#define CS_MT_LE	23
#define CS_MT_NAME	24
#define CS_MT_NUM	25


/* {======================================================================
** State manipulation
** ======================================================================= */
CS_API cs_State     *cs_newstate(cs_Alloc allocator, void *ud, unsigned seed); 
CS_API void          cs_close(cs_State *C);
CS_API cs_State     *cs_newthread(cs_State *C);
CS_API int           cs_resetthread(cs_State *C);
CS_API cs_CFunction  cs_atpanic(cs_State *C, cs_CFunction fn);
CS_API void          cs_setallocf(cs_State *C, cs_Alloc falloc, void *ud); 
CS_API cs_Alloc      cs_getallocf(cs_State *C, void **ud); 
/* }====================================================================== */

/* {======================================================================
** Stack manipulation
** ======================================================================= */
CS_API void  cs_setntop(cs_State *C, int n); 
CS_API int   cs_gettop(const cs_State *C); 
CS_API int   cs_absindex(cs_State *C, int index); 
CS_API void  cs_rotate(cs_State *C, int index, int n); 
CS_API void  cs_copy(cs_State *C, int src, int dest); 
CS_API int   cs_checkstack(cs_State *C, int n); 
CS_API void  cs_push(cs_State *C, int index); 
CS_API void  cs_xmove(cs_State *src, cs_State *dest, int n); 
/* }====================================================================== */

/* {======================================================================
** Access functions (Stack -> C)
** ======================================================================= */
CS_API int          cs_is_number(cs_State *C, int index); 
CS_API int          cs_is_integer(cs_State *C, int index); 
CS_API int          cs_is_string(cs_State *C, int index); 
CS_API int          cs_is_cfunction(cs_State *C, int index); 
CS_API int          cs_is_userdata(cs_State *C, int index); 
CS_API int          cs_type(cs_State *C, int index); 
CS_API const char  *cs_typename(cs_State *C, int type); 

CS_API cs_Number    cs_to_numberx(cs_State *C, int index, int *isnum); 
CS_API cs_Integer   cs_to_integerx(cs_State *C, int index, int *isnum); 
CS_API int          cs_to_boolx(cs_State *C, int index, int *isbool); 
CS_API const char  *cs_to_lstring(cs_State *C, int index, size_t *len); 
CS_API cs_CFunction cs_to_cfunction(cs_State *C, int index); 
CS_API void        *cs_to_userdata(cs_State *C, int index); 
CS_API const void  *cs_to_pointer(cs_State *C, int index); 
CS_API cs_State    *cs_to_thread(cs_State *C, int index); 
/* }====================================================================== */

/* {======================================================================
** Ordering & Arithmetic functions
** ======================================================================= */
// TODO: update docs
/* Arithmetic operations */
#define CS_OP_ADD       0
#define CS_OP_SUB       1
#define CS_OP_MUL       2
#define CS_OP_DIV       3
#define CS_OP_IDIV      4
#define CS_OP_MOD       5
#define CS_OP_POW       6
#define CS_OP_BSHL      7
#define CS_OP_BSHR      8
#define CS_OP_BAND      9
#define CS_OP_BOR       10
#define CS_OP_BXOR      11
#define CS_OP_UNM       12
#define CS_OP_BNOT      13
#define CS_OP_NUM       14

CS_API void cs_arith(cs_State *C, int op); 


/* TODO: update docs */
/* Ordering operations */
#define CS_ORD_EQ       0
#define CS_ORD_LT       1
#define CS_ORD_LE       2
#define CS_ORD_NUM      3

CS_API int cs_rawequal(cs_State *C, int idx1, int idx2); 
CS_API int cs_compare(cs_State *C, int idx1, int idx2, int op); 
/* }====================================================================== */

/* {======================================================================
** Push functions (C -> stack)
** ======================================================================= */
CS_API void        cs_push_nil(cs_State *C); 
CS_API void        cs_push_number(cs_State *C, cs_Number n); 
CS_API void        cs_push_integer(cs_State *C, cs_Integer n); 
CS_API const char *cs_push_lstring(cs_State *C, const char *str, size_t len); 
CS_API const char *cs_push_string(cs_State *C, const char *str); 
CS_API const char *cs_push_fstring(cs_State *C, const char *fmt, ...); 
CS_API const char *cs_push_vfstring(cs_State *C, const char *fmt, va_list argp); 
CS_API void        cs_push_cclosure(cs_State *C, cs_CFunction fn, int upvals); 
CS_API void        cs_push_bool(cs_State *C, int b); 
CS_API void        cs_push_lightuserdata(cs_State *C, void *p); 
CS_API void       *cs_push_userdata(cs_State *C, size_t sz, int nuv); 
CS_API void        cs_push_list(cs_State *C, int sz);
CS_API void        cs_push_table(cs_State *C, int sz);
CS_API int         cs_push_thread(cs_State *C); 
CS_API void        cs_push_instance(cs_State *C, int clsobj);

CS_API void cs_push_class(cs_State *C, int nup, const cs_Entry *l);
CS_API void cs_push_subclass(cs_State *C, int sc, int nup, const cs_Entry *l);
CS_API void cs_push_metaclass(cs_State *C, int ml, int nup, const cs_Entry *l);
CS_API void cs_push_metasubclass(cs_State *C, int sc, int ml, int nup,
                                 const cs_Entry *l);

struct cs_Entry {
    const char *name; /* NULL if sentinel entry */
    cs_CFunction func; /* NULL if placeholder or sentinel entry */
};
/* }====================================================================== */

/* {======================================================================
** Get functions (CScript -> stack)
** ======================================================================= */
CS_API int cs_get(cs_State *C, int index); 
CS_API int cs_get_raw(cs_State *C, int index); 
CS_API int cs_get_global(cs_State *C, const char *name); 
CS_API int cs_get_index(cs_State *C, int index, int i);
CS_API int cs_get_cindex(cs_State *C, int i); 
CS_API int cs_get_field(cs_State *C, int index); 
CS_API int cs_get_fieldstr(cs_State *C, int index, const char *field); 
CS_API int cs_get_fieldptr(cs_State *C, int index, const void *field); 
CS_API int cs_get_fieldint(cs_State *C, int index, cs_Integer field); 
CS_API int cs_get_fieldflt(cs_State *C, int index, cs_Number field); 
CS_API int cs_get_cfieldstr(cs_State *C, const char *field); 
CS_API int cs_get_class(cs_State *C, int index); 
CS_API int cs_get_superclass(cs_State *C, int index); 
CS_API int cs_get_method(cs_State *C, int index); 
CS_API int cs_get_supermethod(cs_State *C, int index);
CS_API int cs_get_metalist(cs_State *C, int index);
CS_API int cs_get_uservalue(cs_State *C, int index, unsigned short n); 
CS_API int cs_get_methods(cs_State *C, int index); 
/* }====================================================================== */

/* {======================================================================
** Set functions (stack -> CScript)
** ======================================================================= */
CS_API void  cs_set(cs_State *C, int index); 
CS_API void  cs_set_raw(cs_State *C, int index); 
CS_API void  cs_set_global(cs_State *C, const char *name); 
CS_API void  cs_set_index(cs_State *C, int index, int i);
CS_API void  cs_set_cindex(cs_State *C, int i);
CS_API void  cs_set_field(cs_State *C, int index); 
CS_API void  cs_set_fieldstr(cs_State *C, int index, const char *field); 
CS_API void  cs_set_fieldptr(cs_State *C, int index, const void *field); 
CS_API void  cs_set_fieldint(cs_State *C, int index, cs_Integer field); 
CS_API void  cs_set_fieldflt(cs_State *C, int index, cs_Number field); 
CS_API void  cs_set_cfieldstr(cs_State *C, const char *field);
CS_API void  cs_set_superclass(cs_State *C, int index); 
CS_API void  cs_set_metalist(cs_State *C, int index);
CS_API int   cs_set_uservalue(cs_State *C, int index, unsigned short n); 
CS_API void  cs_set_methods(cs_State *C, int index); 
CS_API void  cs_set_listlen(cs_State *C, int index, int len);
/* }====================================================================== */

/* {======================================================================
** Status and Error reporting
** ======================================================================= */
// TODO: update docs
/* thread status codes */
#define CS_STATUS_OK            0 /* ok */
#define CS_STATUS_ERUNTIME      1 /* runtime error */
#define CS_STATUS_ESYNTAX       2 /* syntax (compiler) error */
#define CS_STATUS_EMEM          3 /* memory related error */
#define CS_STATUS_EERROR        4 /* error while handling error */
#define CS_STATUS_NUM           5 /* total number of status codes */

CS_API int cs_status(cs_State *C); 
CS_API int cs_error(cs_State *C); 
/* }====================================================================== */

/* {======================================================================
** Call/Load CScript chunks
** ======================================================================= */
CS_API void cs_call(cs_State *C, int nargs, int nresults); 
CS_API int  cs_pcall(cs_State *C, int nargs, int nresults, int msgh); 
CS_API int  cs_load(cs_State *C, cs_Reader reader, void *userdata,
                    const char *chunkname); 
/* }====================================================================== */

/* {======================================================================
** Garbage collector API
** ======================================================================= */
/* TODO: udpate docs */
/* GC options (what) */
#define CS_GC_STOP              0 /* stop GC */
#define CS_GC_RESTART           1 /* restart GC (start if stopped) */
#define CS_GC_CHECK             2 /* check and clear GC collection flag */
#define CS_GC_COLLECT           3 /* perform full GC cycle */
#define CS_GC_COUNT             4 /* get number of bytes_allocated/1024 */
#define CS_GC_COUNTBYTES        5 /* get remainder of bytes_allocated/1024 */
#define CS_GC_STEP              6 /* performs incremental GC step */
#define CS_GC_PARAM             7 /* set or get GC parameter */
#define CS_GC_ISRUNNING         8 /* test whether GC is running */
#define CS_GC_INC               9 /* set GC in incremental mode */

/* TODO: update docs */
/* parameters for incremental mode */
#define CS_GCP_PAUSE            0 /* size of GC "pause" */
#define CS_GCP_STEPMUL          1 /* GC "speed" */
#define CS_GCP_STEPSIZE         2 /* GC "granularity" */
#define CS_GCP_NUM              3 /* number of parameters */

CS_API int cs_gc(cs_State *C, int what, ...); 
/* }====================================================================== */

/* {======================================================================
** Warning-related functions
** ======================================================================= */
CS_API void cs_setwarnf(cs_State *C, cs_WarnFunction fwarn, void *ud); 
CS_API void cs_warning(cs_State *C, const char *msg, int cont); 
/* }====================================================================== */

/* {======================================================================
** Miscellaneous functions and useful macros
** ======================================================================= */
#define CS_FI_NIL        (1 << 0) /* find first nil value */
#define CS_FI_REV        (1 << 1) /* reverse find */
#define CS_FI_MASK       (CS_FI_NIL | CS_FI_REV) /* mask of all options */
CS_API int cs_find_index(cs_State *C, int index, int fi, int s, int e);

#define CS_N2SBUFFSZ     64
CS_API unsigned cs_numbertocstring(cs_State *C, int index, char *buff); 

CS_API size_t      cs_stringtonumber(cs_State *C, const char *s, int *f); 
CS_API cs_Number   cs_version(cs_State *C);
CS_API cs_Integer  cs_len(cs_State *C, int index); 
CS_API size_t      cs_lenudata(cs_State *C, int index);
CS_API int         cs_nextfield(cs_State *C, int index); 
CS_API void        cs_concat(cs_State *C, int n); 
CS_API size_t      cs_numbertostring(cs_State *C, const char *s, int *f); 
CS_API void        cs_toclose(cs_State *C, int index); 
CS_API void        cs_closeslot(cs_State *C, int index); 
CS_API void        cs_shrinklist(cs_State *C, int index);

#define cs_is_function(C, n)        (cs_type(C, (n)) == CS_T_FUNCTION)
#define cs_is_list(C, n)            (cs_type(C, (n)) == CS_T_LIST)
#define cs_is_table(C, n)           (cs_type(C, (n)) == CS_T_TABLE)
#define cs_is_class(C, n)           (cs_type(C, (n)) == CS_T_CLASS)
#define cs_is_instance(C, n)        (cs_type(C, (n)) == CS_T_INSTANCE)
#define cs_is_lightuserdata(C, n)   (cs_type(C, (n)) == CS_T_LIGHTUSERDATA)
#define cs_is_fulluserdata(C, n)    (cs_type(C, (n)) == CS_T_USERDATA)
#define cs_is_nil(C, n)             (cs_type(C, (n)) == CS_T_NIL)
#define cs_is_bool(C, n)            (cs_type(C, (n)) == CS_T_BOOL)
#define cs_is_thread(C, n)          (cs_type(C, (n)) == CS_T_THREAD)
#define cs_is_none(C, n)            (cs_type(C, (n)) == CS_T_NONE)
#define cs_is_noneornil(C, n)       (cs_type(C, (n)) <= 0)

#define cs_to_string(C, i)      cs_to_lstring(C, i, NULL)
#define cs_to_number(C,i)       cs_to_numberx(C,(i),NULL)
#define cs_to_integer(C,i)      cs_to_integerx(C,(i),NULL)
// TODO: update docs
#define cs_to_bool(C,i)         cs_to_boolx(C,(i),NULL)

// TODO: add docs
#define cs_push_clist(C)        ((void)cs_push(C, CS_CLIST_INDEX))
#define cs_push_mainthread(C)   ((void)cs_get_cindex(C, CS_CLIST_MAINTHREAD))
#define cs_push_globaltable(C)  ((void)cs_get_cindex(C, CS_CLIST_GLOBALS))
#define cs_push_ctable(C)       ((void)cs_push(C, CS_CTABLE_INDEX))
#define cs_push_metalist(C)     cs_push_list(C, CS_MT_NUM)
#define cs_push_literal(C, s)   cs_push_string(C, "" s)
#define cs_push_cfunction(C,f)  cs_push_cclosure(C,f,0)

#define cs_register(C,n,f)  (cs_push_cfunction(C,(f)), cs_set_global(C,(n)))

#define cs_insert(C,index)      cs_rotate(C, (index), 1)
#define cs_pop(C,n)             cs_setntop(C, -(n)-1)
#define cs_remove(C,index)      (cs_rotate(C, (index), -1), cs_pop(C, 1))
#define cs_replace(C,index)     (cs_copy(C, -1, (index)), cs_pop(C, 1))

#define cs_getextraspace(C)     ((void *)((char *)(C) - CS_EXTRASPACE))

#define cs_getntop(C)           (cs_gettop(C) + 1)
/* }====================================================================== */

/* {======================================================================
** Debug API
** ======================================================================= */
// TODO: add docs
/* Event codes */
#define CS_HOOK_CALL        0
#define CS_HOOK_RET         1
#define CS_HOOK_LINE        2
#define CS_HOOK_COUNT       3

// TODO: add docs
/* Event masks */
#define CS_MASK_CALL        (1 << CS_HOOK_CALL)
#define CS_MASK_RET         (1 << CS_HOOK_RET)
#define CS_MASK_LINE        (1 << CS_HOOK_LINE)
#define CS_MASK_COUNT       (1 << CS_HOOK_COUNT)

// TODO: add docs
CS_API int         cs_getstack(cs_State *C, int level, cs_Debug *ar); 
CS_API int         cs_getinfo(cs_State *C, const char *what, cs_Debug *ar); 
CS_API int         cs_stackinuse(cs_State *C);
CS_API const char *cs_getlocal(cs_State *C, const cs_Debug *ar, int n); 
CS_API const char *cs_setlocal (cs_State *C, const cs_Debug *ar, int n); 
CS_API const char *cs_getupvalue(cs_State *C, int index, int n); 
CS_API const char *cs_setupvalue(cs_State *C, int index, int n); 
CS_API void       *cs_upvalueid(cs_State *C, int index, int n);
CS_API void        cs_upvaluejoin(cs_State *C, int index1, int n1,
                                               int index2, int n2);
CS_API void        cs_sethook(cs_State *C, cs_Hook func, int mask, int count);
CS_API cs_Hook     cs_gethook(cs_State *C);
CS_API int         cs_gethookmask(cs_State *C);
CS_API int         cs_gethookcount(cs_State *C);

// TODO: update docs
struct cs_Debug {
    int event;
    const char *name;           /* (n) */
    const char *namewhat;       /* (n) */
    const char *what;           /* (s) */
    const char *source;         /* (s) */
    size_t srclen;              /* (s) */
    int currline;               /* (l) */
    int defline;                /* (s) */
    int lastdefline;            /* (s) */
    int nupvals;                /* (u) */
    int nparams;                /* (u) */
    char isvararg;              /* (u) */
    int ftransfer;              /* (r) */
    int ntransfer;              /* (r) */
    char shortsrc[CS_IDSIZE];   /* (s) */
    /* private part */
    struct CallFrame *cf; /* active function */
};
/* }====================================================================== */


#define CSI_TOSTR_AUX(x)        #x
#define CSI_TOSTR(x)            CSI_TOSTR_AUX(x)

#define CS_VERSION_MAJOR        CSI_TOSTR(CS_VERSION_MAJOR_N)
#define CS_VERSION_MINOR        CSI_TOSTR(CS_VERSION_MINOR_N)
#define CS_VERSION_RELEASE      CSI_TOSTR(CS_VERSION_RELEASE_N)

#define CS_VERSION      "CScript " CS_VERSION_MAJOR "." CS_VERSION_MINOR
#define CS_RELEASE      CS_VERSION "." CS_VERSION_RELEASE

#define CS_COPYRIGHT "Copyright (C) 1994-2024 Lua.org, PUC-Rio\n" \
        CS_RELEASE " Copyright (C) 2024-2025 Jure Bagić"


/*----------------------------------,
 | Big Thank You to Lua Developers! |
 \________________________________*/
/* =======================================================================
** Copyright (C) 1994-2024 Lua.org, PUC-Rio.
** Copyright (C) 2024-2025 Jure Bagić
**
** Permission is hereby granted, free of charge, to any person obtaining
** a copy of this software and associated documentation files (the
** "Software"), to deal in the Software without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Software, and to
** permit persons to whom the Software is furnished to do so, subject to
** the following conditions:
**
** The above copyright notice and this permission notice shall be
** included in all copies or substantial portions of the Software.
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
** ======================================================================= */

#endif
