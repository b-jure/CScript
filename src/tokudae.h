/*
** tokudae.h
** Tokudae - A Scripting Language inspired by Lua and C
** See Copyright Notice at the end of this file
*/

#ifndef tokudae_h
#define tokudae_h

#include <stddef.h>
#include <stdarg.h>


#define TOKU_VERSION_MAJOR_N        1
#define TOKU_VERSION_MINOR_N        0
#define TOKU_VERSION_RELEASE_N      0

#define TOKU_VERSION_NUM  (TOKU_VERSION_MAJOR_N * 100 + TOKU_VERSION_MINOR_N)
#define TOKU_VERSION_RELEASE_NUM \
        (TOKU_VERSION_NUM * 100 + TOKU_VERSION_RELEASE_N)


#include "tokudaeconf.h"


/* option for multiple returns in 'toku_pcall' and 'toku_call' */
#define TOKU_MULTRET       (-1)


// TODO: update docs
/*
** Pseudo-indices
** (-TOKUI_MAXSTACK is the minimum valid index; we keep some free empty
** space after that to help overflow detection)
*/
#define TOKU_CLIST_INDEX        (-TOKUI_MAXSTACK - 1000)
#define TOKU_CTABLE_INDEX       (TOKU_CLIST_INDEX - 1)
#define toku_upvalueindex(i)    (TOKU_CTABLE_INDEX - 1 - (i))


// TODO: update docs
/* predefined values in the C list */
#define TOKU_CLIST_MAINTHREAD   0
#define TOKU_CLIST_GLOBALS      1
#define TOKU_CLIST_LAST         TOKU_CLIST_GLOBALS
    

// TODO: update docs
/* types of values */
#define TOKU_T_NONE             (-1)
#define TOKU_T_NIL              0   /* nil */
#define TOKU_T_BOOL             1   /* boolean */
#define TOKU_T_NUMBER           2   /* number */
#define TOKU_T_USERDATA         3   /* userdata */
#define TOKU_T_LIGHTUSERDATA    4   /* light userdata */
#define TOKU_T_STRING           5   /* string */
#define TOKU_T_LIST             6   /* list */
#define TOKU_T_TABLE            7   /* table */
#define TOKU_T_FUNCTION         8   /* function */
#define TOKU_T_BMETHOD          9   /* bound method */
#define TOKU_T_CLASS            10  /* class */
#define TOKU_T_INSTANCE         11  /* instance */
#define TOKU_T_THREAD           12  /* thread */
#define TOKU_T_NUM              13  /* total number of types */



/* minimum stack space available to a C function */
#define TOKU_MINSTACK       20



/* Tokudae thread state */
typedef struct toku_State toku_State;


/* type for integers */
typedef TOKU_INTEGER toku_Integer;

/* type for unsigned integers */
typedef TOKU_UNSIGNED toku_Unsigned;

/* type for floating point numbers */
typedef TOKU_NUMBER toku_Number;


/* type of C function registered with Tokudae */
typedef int (*toku_CFunction)(toku_State *T);

/* type of function that de/allocates memory */
typedef void *(*toku_Alloc)(void *ptr, size_t osz, size_t nsz, void *ud);

/* type of function that reads blocks when loading Tokudae chunks */
typedef const char *(*toku_Reader)(toku_State *T, void *data, size_t *szread);

/* type of warning function */
typedef void (*toku_WarnFunction)(void *ud, const char *msg, int tocont);


/* type for debug API */
typedef struct toku_Debug toku_Debug;


/* TODO: add docs */
/* type of function to be called by the debugger in specific events */
typedef void (*toku_Hook)(toku_State *T, toku_Debug *ar);


// TODO: update docs
/* meta tags for indexing the metalist (ORDER MT) */
#define TOKU_MT_GETIDX      0
#define TOKU_MT_SETIDX	    1
#define TOKU_MT_GC	    2
#define TOKU_MT_CLOSE	    3
#define TOKU_MT_CALL	    4
#define TOKU_MT_INIT	    5
#define TOKU_MT_CONCAT	    6
#define TOKU_MT_ADD	    7
#define TOKU_MT_SUB	    8
#define TOKU_MT_MUL	    9
#define TOKU_MT_DIV	    10
#define TOKU_MT_IDIV	    11
#define TOKU_MT_MOD	    12
#define TOKU_MT_POW	    13
#define TOKU_MT_BSHL	    14
#define TOKU_MT_BSHR	    15
#define TOKU_MT_BAND	    16
#define TOKU_MT_BOR	    17
#define TOKU_MT_BXOR	    18
#define TOKU_MT_UNM	    19
#define TOKU_MT_BNOT	    20
#define TOKU_MT_EQ	    21
#define TOKU_MT_LT	    22
#define TOKU_MT_LE	    23
#define TOKU_MT_NAME	    24
#define TOKU_MT_METALIST    25
#define TOKU_MT_NUM	    26


/* {======================================================================
** State manipulation
** ======================================================================= */
TOKU_API toku_State *toku_newstate(toku_Alloc falloc, void *ud, unsigned seed); 
TOKU_API void           toku_close(toku_State *T);
TOKU_API toku_State    *toku_newthread(toku_State *T);
TOKU_API int            toku_resetthread(toku_State *T);
TOKU_API toku_CFunction toku_atpanic(toku_State *T, toku_CFunction fn);
TOKU_API void       toku_setallocf(toku_State *T, toku_Alloc falloc, void *ud); 
TOKU_API toku_Alloc toku_getallocf(toku_State *T, void **ud); 
/* }====================================================================== */

/* {======================================================================
** Stack manipulation
** ======================================================================= */
TOKU_API void toku_setntop(toku_State *T, int n); 
TOKU_API int  toku_gettop(const toku_State *T); 
TOKU_API int  toku_absindex(toku_State *T, int idx); 
TOKU_API void toku_rotate(toku_State *T, int idx, int n); 
TOKU_API void toku_copy(toku_State *T, int src, int dest); 
TOKU_API int  toku_checkstack(toku_State *T, int n); 
TOKU_API void toku_push(toku_State *T, int idx); 
TOKU_API void toku_xmove(toku_State *src, toku_State *dest, int n); 
/* }====================================================================== */

/* {======================================================================
** Access functions (Stack -> C)
** ======================================================================= */
TOKU_API int          toku_is_number(toku_State *T, int idx); 
TOKU_API int          toku_is_integer(toku_State *T, int idx); 
TOKU_API int          toku_is_string(toku_State *T, int idx); 
TOKU_API int          toku_is_cfunction(toku_State *T, int idx); 
TOKU_API int          toku_is_udatamethod(toku_State *T, int idx); 
TOKU_API int          toku_is_userdata(toku_State *T, int idx); 
TOKU_API int          toku_type(toku_State *T, int idx); 
TOKU_API const char  *toku_typename(toku_State *T, int type); 

TOKU_API toku_Number    toku_to_numberx(toku_State *T, int idx, int *isnum); 
TOKU_API toku_Integer   toku_to_integerx(toku_State *T, int idx, int *isnum); 
TOKU_API int            toku_to_bool(toku_State *T, int idx); 
TOKU_API const char    *toku_to_lstring(toku_State *T, int idx, size_t *len); 
TOKU_API toku_CFunction toku_to_cfunction(toku_State *T, int idx); 
TOKU_API void          *toku_to_userdata(toku_State *T, int idx); 
TOKU_API const void    *toku_to_pointer(toku_State *T, int idx); 
TOKU_API toku_State    *toku_to_thread(toku_State *T, int idx); 
/* }====================================================================== */

/* {======================================================================
** Ordering & Arithmetic functions
** ======================================================================= */
// TODO: update docs
/* Arithmetic operations */
#define TOKU_OP_ADD         0
#define TOKU_OP_SUB         1
#define TOKU_OP_MUL         2
#define TOKU_OP_DIV         3
#define TOKU_OP_IDIV        4
#define TOKU_OP_MOD         5
#define TOKU_OP_POW         6
#define TOKU_OP_BSHL        7
#define TOKU_OP_BSHR        8
#define TOKU_OP_BAND        9
#define TOKU_OP_BOR         10
#define TOKU_OP_BXOR        11
#define TOKU_OP_UNM         12
#define TOKU_OP_BNOT        13
#define TOKU_OP_NUM         14

TOKU_API void toku_arith(toku_State *T, int op); 


/* TODO: update docs */
/* Ordering operations */
#define TOKU_ORD_EQ         0
#define TOKU_ORD_LT         1
#define TOKU_ORD_LE         2
#define TOKU_ORD_NUM        3

TOKU_API int toku_rawequal(toku_State *T, int idx1, int idx2); 
TOKU_API int toku_compare(toku_State *T, int idx1, int idx2, int op); 
/* }====================================================================== */

/* {======================================================================
** Push functions (C -> stack)
** ======================================================================= */
TOKU_API void        toku_push_nil(toku_State *T); 
TOKU_API void        toku_push_number(toku_State *T, toku_Number n); 
TOKU_API void        toku_push_integer(toku_State *T, toku_Integer n); 
TOKU_API const char *toku_push_lstring(toku_State *T, const char *s, size_t l); 
TOKU_API const char *toku_push_string(toku_State *T, const char *s); 
TOKU_API const char *toku_push_fstring(toku_State *T, const char *f, ...); 
TOKU_API const char *toku_push_vfstring(toku_State *T, const char *f, va_list ap); 
TOKU_API void    toku_push_cclosure(toku_State *T, toku_CFunction fn, int nup); 
TOKU_API void        toku_push_bool(toku_State *T, int b); 
TOKU_API void        toku_push_lightuserdata(toku_State *T, void *p); 
TOKU_API void       *toku_push_userdata(toku_State *T, size_t sz, int nuv); 
TOKU_API void        toku_push_list(toku_State *T, int sz);
TOKU_API void        toku_push_table(toku_State *T, int sz);
TOKU_API int         toku_push_thread(toku_State *T); 
TOKU_API void        toku_push_class(toku_State *T);
TOKU_API void        toku_push_instance(toku_State *T, int clsobj);
TOKU_API void        toku_push_method(toku_State *T, int idx);
/* }====================================================================== */

/* {======================================================================
** Get functions (Tokudae -> stack)
** ======================================================================= */
TOKU_API int  toku_get(toku_State *T, int idx); 
TOKU_API int  toku_get_raw(toku_State *T, int idx); 
TOKU_API int  toku_get_global(toku_State *T, const char *name); 
TOKU_API int  toku_get_index(toku_State *T, int idx, int i);
TOKU_API int  toku_get_cindex(toku_State *T, int i); 
TOKU_API int  toku_get_field(toku_State *T, int idx); 
TOKU_API int  toku_get_fieldstr(toku_State *T, int idx, const char *s); 
TOKU_API int  toku_get_fieldptr(toku_State *T, int idx, const void *p); 
TOKU_API int  toku_get_fieldint(toku_State *T, int idx, toku_Integer i); 
TOKU_API int  toku_get_fieldflt(toku_State *T, int idx, toku_Number f); 
TOKU_API int  toku_get_cfieldstr(toku_State *T, const char *s); 
TOKU_API int  toku_get_class(toku_State *T, int idx); 
TOKU_API int  toku_get_superclass(toku_State *T, int idx); 
TOKU_API int  toku_get_method(toku_State *T, int idx); 
TOKU_API int  toku_get_self(toku_State *T, int idx);
TOKU_API int  toku_get_supermethod(toku_State *T, int idx);
TOKU_API int  toku_get_metalist(toku_State *T, int idx);
TOKU_API int  toku_get_uservalue(toku_State *T, int idx, unsigned short n); 
TOKU_API int  toku_get_methods(toku_State *T, int idx); 
TOKU_API void toku_get_fields(toku_State *T, int idx); 
/* }====================================================================== */

/* {======================================================================
** Set functions (stack -> Tokudae)
** ======================================================================= */
TOKU_API void toku_set(toku_State *T, int idx); 
TOKU_API void toku_set_raw(toku_State *T, int idx); 
TOKU_API void toku_set_global(toku_State *T, const char *name); 
TOKU_API void toku_set_index(toku_State *T, int idx, int i);
TOKU_API void toku_set_cindex(toku_State *T, int i);
TOKU_API void toku_set_field(toku_State *T, int idx); 
TOKU_API void toku_set_fieldstr(toku_State *T, int idx, const char *s); 
TOKU_API void toku_set_fieldptr(toku_State *T, int idx, const void *p); 
TOKU_API void toku_set_fieldint(toku_State *T, int idx, toku_Integer i); 
TOKU_API void toku_set_fieldflt(toku_State *T, int idx, toku_Number f); 
TOKU_API void toku_set_cfieldstr(toku_State *T, const char *s);
TOKU_API void toku_set_superclass(toku_State *T, int idx); 
TOKU_API void toku_set_metalist(toku_State *T, int idx);
TOKU_API int  toku_set_uservalue(toku_State *T, int idx, unsigned short n); 
TOKU_API void toku_set_methods(toku_State *T, int idx); 
TOKU_API void toku_set_fields(toku_State *T, int idx);
TOKU_API void toku_sec_listlen(toku_State *T, int idx, int len);
/* }====================================================================== */

/* {======================================================================
** Status and Error reporting
** ======================================================================= */
// TODO: update docs
/* thread status codes */
#define TOKU_STATUS_OK          0 /* ok */
#define TOKU_STATUS_ERUNTIME    1 /* runtime error */
#define TOKU_STATUS_ESYNTAX     2 /* syntax (compiler) error */
#define TOKU_STATUS_EMEM        3 /* memory related error */
#define TOKU_STATUS_EERROR      4 /* error while handling error */
#define TOKU_STATUS_NUM         5 /* total number of status codes */

TOKU_API int toku_status(toku_State *T); 
TOKU_API int toku_error(toku_State *T); 
/* }====================================================================== */

/* {======================================================================
** Call/Load Tokudae chunks
** ======================================================================= */
TOKU_API void toku_call(toku_State *T, int nargs, int nresults); 
TOKU_API int  toku_pcall(toku_State *T, int nargs, int nresults, int msgh); 
TOKU_API int  toku_load(toku_State *T, toku_Reader freader, void *userdata,
                        const char *chunkname); 
/* }====================================================================== */

/* {======================================================================
** Garbage collector API
** ======================================================================= */
/* TODO: udpate docs */
/* GC options (what) */
#define TOKU_GC_STOP            0 /* stop GC */
#define TOKU_GC_RESTART         1 /* restart GC (start if stopped) */
#define TOKU_GC_CHECK           2 /* check and clear GC collection flag */
#define TOKU_GC_COLLECT         3 /* perform full GC cycle */
#define TOKU_GC_COUNT           4 /* get number of bytes_allocated/1024 */
#define TOKU_GC_COUNTBYTES      5 /* get remainder of bytes_allocated/1024 */
#define TOKU_GC_STEP            6 /* performs incremental GC step */
#define TOKU_GC_PARAM           7 /* set or get GC parameter */
#define TOKU_GC_ISRUNNING       8 /* test whether GC is running */
#define TOKU_GC_INC             9 /* set GC in incremental mode */

/* TODO: update docs */
/* parameters for incremental mode */
#define TOKU_GCP_PAUSE          0 /* size of GC "pause" */
#define TOKU_GCP_STEPMUL        1 /* GC "speed" */
#define TOKU_GCP_STEPSIZE       2 /* GC "granularity" */
#define TOKU_GCP_NUM            3 /* number of parameters */

TOKU_API int toku_gc(toku_State *T, int what, ...); 
/* }====================================================================== */

/* {======================================================================
** Warning-related functions
** ======================================================================= */
TOKU_API void toku_setwarnf(toku_State *T, toku_WarnFunction fwarn, void *ud); 
TOKU_API void toku_warning(toku_State *T, const char *msg, int cont); 
/* }====================================================================== */

/* {======================================================================
** Miscellaneous functions and useful macros
** ======================================================================= */
#define TOKU_FI_NIL         (1 << 0) /* find first nil value */
#define TOKU_FI_REV         (1 << 1) /* reverse find */
#define TOKU_FI_MASK        (TOKU_FI_NIL | TOKU_FI_REV)
TOKU_API int toku_find_index(toku_State *T, int idx, int fi, int s, int e);

#define TOKU_N2SBUFFSZ     64
TOKU_API unsigned toku_numbertocstring(toku_State *T, int idx, char *buff); 

TOKU_API size_t      toku_stringtonumber(toku_State *T, const char *s, int *f); 
TOKU_API toku_Number toku_version(toku_State *T);
TOKU_API toku_Integer toku_len(toku_State *T, int idx); 
TOKU_API size_t      toku_lenudata(toku_State *T, int idx);
TOKU_API int         toku_nextfield(toku_State *T, int idx); 
TOKU_API void        toku_concat(toku_State *T, int n); 
TOKU_API size_t      toku_numbertostring(toku_State *T, const char *s, int *f); 
TOKU_API void        toku_toclose(toku_State *T, int idx); 
TOKU_API void        toku_closeslot(toku_State *T, int idx); 
TOKU_API void        toku_shrinklist(toku_State *T, int idx);
TOKU_API unsigned    toku_numuservalues(toku_State *T, int idx);

#define toku_is_function(C, n)      (toku_type(C, (n)) == TOKU_T_FUNCTION)
#define toku_is_boundmethod(C, n)   (toku_type(C, (n)) == TOKU_T_BMETHOD)
#define toku_is_list(C, n)          (toku_type(C, (n)) == TOKU_T_LIST)
#define toku_is_table(C, n)         (toku_type(C, (n)) == TOKU_T_TABLE)
#define toku_is_class(C, n)         (toku_type(C, (n)) == TOKU_T_CLASS)
#define toku_is_instance(C, n)      (toku_type(C, (n)) == TOKU_T_INSTANCE)
#define toku_is_lightuserdata(C, n) (toku_type(C, (n)) == TOKU_T_LIGHTUSERDATA)
#define toku_is_fulluserdata(C, n)  (toku_type(C, (n)) == TOKU_T_USERDATA)
#define toku_is_nil(C, n)           (toku_type(C, (n)) == TOKU_T_NIL)
#define toku_is_bool(C, n)          (toku_type(C, (n)) == TOKU_T_BOOL)
#define toku_is_thread(C, n)        (toku_type(C, (n)) == TOKU_T_THREAD)
#define toku_is_none(C, n)          (toku_type(C, (n)) == TOKU_T_NONE)
#define toku_is_noneornil(C, n)     (toku_type(C, (n)) <= 0)

#define toku_to_string(C, i)        toku_to_lstring(C, i, NULL)
#define toku_to_number(C,i)         toku_to_numberx(C,(i),NULL)
#define toku_to_integer(C,i)        toku_to_integerx(C,(i),NULL)

#define toku_push_mainthread(C) \
        ((void)toku_get_cindex(C, TOKU_CLIST_MAINTHREAD))

#define toku_push_globaltable(C) \
        ((void)toku_get_cindex(C, TOKU_CLIST_GLOBALS))

#define toku_push_clist(C)        toku_push(C, TOKU_CLIST_INDEX)
#define toku_push_ctable(C)       toku_push(C, TOKU_CTABLE_INDEX)
#define toku_push_metalist(C)     toku_push_list(C, TOKU_MT_NUM)
#define toku_push_literal(C, s)   toku_push_string(C, "" s)
#define toku_push_cfunction(C,f)  toku_push_cclosure(C,f,0)

#define toku_register(C,n,f) \
        (toku_push_cfunction(C,(f)), toku_set_global(C,(n)))

#define toku_insert(C,idx)      toku_rotate(C, (idx), 1)
#define toku_pop(C,n)           toku_setntop(C, -(n)-1)
#define toku_remove(C,idx)      (toku_rotate(C, (idx), -1), toku_pop(C, 1))
#define toku_replace(C,idx)     (toku_copy(C, -1, (idx)), toku_pop(C, 1))

#define toku_getextraspace(C)   ((void *)((char *)(C) - TOKU_EXTRASPACE))

#define toku_getntop(C)         (toku_gettop(C) + 1)
/* }====================================================================== */

/* {======================================================================
** Debug API
** ======================================================================= */
// TODO: add docs
/* Event codes */
#define TOKU_HOOK_CALL      0
#define TOKU_HOOK_RET       1
#define TOKU_HOOK_LINE      2
#define TOKU_HOOK_COUNT     3

// TODO: add docs
/* Event masks */
#define TOKU_MASK_CALL      (1 << TOKU_HOOK_CALL)
#define TOKU_MASK_RET       (1 << TOKU_HOOK_RET)
#define TOKU_MASK_LINE      (1 << TOKU_HOOK_LINE)
#define TOKU_MASK_COUNT     (1 << TOKU_HOOK_COUNT)

// TODO: add docs
TOKU_API int    toku_getstack(toku_State *T, int level, toku_Debug *ar); 
TOKU_API int    toku_getinfo(toku_State *T, const char *what, toku_Debug *ar); 
TOKU_API int    toku_stackinuse(toku_State *T);
TOKU_API const char *toku_getlocal(toku_State *T, const toku_Debug *ar, int n); 
TOKU_API const char *toku_setlocal(toku_State *T, const toku_Debug *ar, int n); 
TOKU_API const char *toku_getupvalue(toku_State *T, int idx, int n); 
TOKU_API const char *toku_setupvalue(toku_State *T, int idx, int n); 
TOKU_API void  *toku_upvalueid(toku_State *T, int idx, int n);
TOKU_API void   toku_upvaluejoin(toku_State *T, int idx1, int n1,
                                                int idx2, int n2);
// TODO: add docs
TOKU_API void toku_sethook(toku_State *T, toku_Hook fh, int mask, int count);
TOKU_API toku_Hook toku_gethook(toku_State *T);
TOKU_API int  toku_gethookmask(toku_State *T);
TOKU_API int  toku_gethookcount(toku_State *T);

// TODO: update docs
struct toku_Debug {
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
    char shortsrc[TOKU_IDSIZE]; /* (s) */
    /* private part */
    struct CallFrame *cf; /* active function */
};
/* }====================================================================== */


#define TOKUI_TOSTR_AUX(x)      #x
#define TOKUI_TOSTR(x)          TOKUI_TOSTR_AUX(x)

#define TOKU_VERSION_MAJOR      TOKUI_TOSTR(TOKU_VERSION_MAJOR_N)
#define TOKU_VERSION_MINOR      TOKUI_TOSTR(TOKU_VERSION_MINOR_N)
#define TOKU_VERSION_RELEASE    TOKUI_TOSTR(TOKU_VERSION_RELEASE_N)

#define TOKU_VERSION    "Tokudae " TOKU_VERSION_MAJOR "." TOKU_VERSION_MINOR
#define TOKU_RELEASE    TOKU_VERSION "." TOKU_VERSION_RELEASE

#define TOKU_COPYRIGHT "Copyright (C) 1994-2024 Lua.org, PUC-Rio\n" \
        TOKU_RELEASE " Copyright (C) 2024-2025 Jure Bagić"


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
