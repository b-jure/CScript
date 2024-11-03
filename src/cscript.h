/*
** cscript.h
** CScript (inspired by Lua and C)
** See Copyright Notice at the end of this file
*/

#ifndef CSCRIPT_H
#define CSCRIPT_H

#include <stddef.h>
#include <stdarg.h>

#include "cconf.h"


#define CR_VERSION_MAJOR        "1"
#define CR_VERSION_MINOR        "0"
#define CR_VERSION_RELEASE      "0"

#define CR_VERSION_NUMBER               100
#define CR_VERSION_RELEASE_NUM          (CR_VERSION_NUMBER * 100);

#define CR_VERSION      "CScript " CR_VERSION_MAJOR "." CR_VERSION_MINOR
#define CR_RELEASE      CR_VERSION "." CR_VERSION_RELEASE
#define CR_COPYRIGHT    CR_RELEASE " Copyright (C) 2023-2024 Jure Bagić"



/* option for multiple returns in 'cr_pcall' and 'cr_call' */
#define CR_MULRET       (-1)


/*
** Pseudo-indices
** (-CRI_MAXSTACK is the minimum valid index; we keep some free empty
** space after that to help overflow detection)
*/
#define CR_REGISTRYINDEX            (-CRI_MAXSTACK - 1000)
#define cr_upvalueindex(i)          (CR_REGISTRYINDEX - (i))


/* minimum stack space available to a C function */
#define CR_MINSTACK     20


/* CScript thread state */
typedef struct cr_State cr_State;



/* types of values */
#define CR_TNONE        (-1)

#define CR_TNIL         0   /* nil */
#define CR_TBOOL        1   /* boolean */
#define CR_TNUMBER      2   /* number */
#define CR_TLUDATA      3   /* light userdata */
#define CR_TSTRING      4   /* string */
#define CR_TARRAY       5   /* string */
#define CR_TFUNCTION    6   /* function */
#define CR_TCLASS       7   /* class */
#define CR_TINSTANCE    8   /* instance */
#define CR_TUDATA       9   /* userdata */
#define CR_TTHREAD      10  /* thread */

#define CR_NUM_TYPES    11



/* type for integers */
typedef CR_INTEGER cr_Integer;

/* type for unsigned integers */
typedef CR_UNSIGNED cr_Unsigned;

/* type for floating point numbers */
typedef CR_NUMBER cr_Number;


/* C function registered with CScript */
typedef int (*cr_CFunction)(cr_State *ts);

/* function for memory de/allocation */
typedef void *(*cr_Alloc)(void *ptr, size_t osz, size_t nsz, void *ud);

/* function that reads blocks when loading CScript chunks */
typedef const char *(*cr_Reader)(cr_State *ts, void *data, size_t *szread);

/* type for warning functions */
typedef void (*cr_WarnFunction)(void *ud, const char *msg, int tocont);

/* Virtual Method Table (for metamethods) */
typedef struct cr_VMT cr_VMT;

/* Class method entry */
typedef struct cr_Entry cr_Entry;

/* type for debug API */
typedef struct cr_DebugInfo cr_DebugInfo;


/* meta methods ('mm') */
typedef enum cr_MM {
    CR_MM_INIT = 0,
    CR_MM_GETIDX,
    CR_MM_SETIDX,
    CR_MM_GC,
    CR_MM_CLOSE,
    CR_MM_CALL,
    CR_MM_CONCAT,
    CR_MM_ADD,
    CR_MM_SUB,
    CR_MM_MUL,
    CR_MM_DIV,
    CR_MM_MOD,
    CR_MM_POW,
    CR_MM_BSHL,
    CR_MM_BSHR,
    CR_MM_BAND,
    CR_MM_BOR,
    CR_MM_BXOR,
    CR_MM_UNM,
    CR_MM_BNOT,
    CR_MM_EQ,
    CR_MM_LT,
    CR_MM_LE,
} cr_MM;

#define CR_NUM_MM     (CR_MM_LE + 1)

struct cr_VMT {
    cr_CFunction func[CR_NUM_MM]; /* metamethods */
};

struct cr_Entry {
    const char *name;
    cr_CFunction func;
};


/* -------------------------------------------------------------------------
 * State manipulation
 * ------------------------------------------------------------------------- */
CR_API cr_State        *cr_newstate(cr_Alloc allocator, void *ud); 
CR_API void             cr_freestate(cr_State *ts);
CR_API cr_State        *cr_newthread(cr_State *ts);
CR_API int              cr_resetthread(cr_State *ts);
CR_API cr_CFunction     cr_atpanic(cr_State *ts, cr_CFunction fn);
CR_API cr_Number        cr_version(cr_State *ts);


/* -------------------------------------------------------------------------
 * Stack manipulation
 * ------------------------------------------------------------------------- */
CR_API void             cr_settop(cr_State *ts, int index); 
CR_API int              cr_gettop(const cr_State *ts); 
CR_API int              cr_absindex(cr_State *ts, int index); 
CR_API void             cr_rotate(cr_State *ts, int index, int n); 
CR_API void             cr_copy(cr_State *ts, int src, int dest); 
CR_API int              cr_checkstack(cr_State *ts, int n); 
CR_API void             cr_push(cr_State *ts, int index); 
CR_API void             cr_xmove(cr_State *src, cr_State *dest, int n); 


/* -------------------------------------------------------------------------
 * Access functions (Stack -> C)
 * ------------------------------------------------------------------------- */
CR_API int              cr_is_number(cr_State *ts, int index); 
CR_API int              cr_is_integer(cr_State *ts, int index); 
CR_API int              cr_is_string(cr_State *ts, int index); 
CR_API int              cr_is_cfunction(cr_State *ts, int index); 
CR_API int              cr_is_userdata(cr_State *ts, int index); 
CR_API int              cr_type(cr_State *ts, int index); 
CR_API const char      *cr_typename(cr_State *ts, int type); 

CR_API cr_Number        cr_to_numberx(cr_State *ts, int index, int *isnum); 
CR_API cr_Integer       cr_to_integerx(cr_State *ts, int index, int *isnum); 
CR_API int              cr_to_bool(cr_State *ts, int index); 
CR_API const char      *cr_to_lstring(cr_State *ts, int index, size_t *plen); 
CR_API cr_CFunction     cr_to_cfunction(cr_State *ts, int index); 
CR_API void            *cr_to_userdata(cr_State *ts, int index); 
CR_API const void      *cr_to_pointer(cr_State *ts, int index); 
CR_API cr_State        *cr_to_thread(cr_State *ts, int index); 


/* -------------------------------------------------------------------------
 * Ordering & Arithmetic functions
 * ------------------------------------------------------------------------- */
/* Arithmetic operations */
#define CR_OPADD        0
#define CR_OPSUB        1
#define CR_OPMUL        2
#define CR_OPDIV        3
#define CR_OPMOD        4
#define CR_OPPOW        5
#define CR_OPBSHL       6
#define CR_OPBSHR       7
#define CR_OPBAND       8
#define CR_OPBOR        9
#define CR_OPBXOR       10
#define CR_OPUNM        11
#define CR_OPBNOT       12

#define CR_NUM_ARITH    13

CR_API void     cr_arith(cr_State *ts, int op); 


/* Ordering operations */
#define CR_OPEQ         0
#define CR_OPLT         1
#define CR_OPLE         2

#define CR_NUM_CMP      3

CR_API int      cr_rawequal(cr_State *ts, int idx1, int idx2); 
CR_API int      cr_compare(cr_State *ts, int idx1, int idx2, int op); 


/* -------------------------------------------------------------------------
 * Push functions (C -> stack)
 * ------------------------------------------------------------------------- */
CR_API void        cr_push_nil(cr_State *ts); 
CR_API void        cr_push_number(cr_State *ts, cr_Number n); 
CR_API void        cr_push_integer(cr_State *ts, cr_Integer n); 
CR_API const char *cr_push_lstring(cr_State *ts, const char *str, size_t len); 
CR_API const char *cr_push_string(cr_State *ts, const char *str); 
CR_API const char *cr_push_vfstring(cr_State *ts, const char *fmt, va_list argp); 
CR_API const char *cr_push_fstring(cr_State *ts, const char *fmt, ...); 
CR_API void        cr_push_cclosure(cr_State *ts, cr_CFunction fn, int upvals); 
CR_API void        cr_push_bool(cr_State *ts, int b); 
CR_API void        cr_push_lightuserdata(cr_State *ts, void *p); 
CR_API void        cr_push_array(cr_State *ts);
CR_API int         cr_push_thread(cr_State *ts); 
CR_API void        cr_push_class(cr_State *ts, cr_VMT *vmt, int sindex,
                                 int nup, cr_Entry *list); 


/* -------------------------------------------------------------------------
 * Get functions (CScript -> stack)
 * ------------------------------------------------------------------------- */
CR_API int   cr_get_global(cr_State *ts, const char *name); 
CR_API int   cr_get(cr_State *ts, int obj); 
CR_API int   cr_get_index(cr_State *ts, int arrobj, cr_Integer index);
CR_API int   cr_get_field(cr_State *ts, int insobj); 
CR_API int   cr_get_class(cr_State *ts, int insobj); 
CR_API int   cr_get_method(cr_State *ts, int insobj, const char *name); 
CR_API int   cr_get_metamethod(cr_State *ts, int obj, cr_MM mm); 

CR_API void *cr_newuserdata(cr_State *ts, size_t sz, int nuv); 
CR_API int   cr_get_uservalue(cr_State *ts, int udobj, int n); 


/* -------------------------------------------------------------------------
 * Set functions (stack -> CScript)
 * ------------------------------------------------------------------------- */
CR_API void  cr_set_global(cr_State *ts, const char *name); 
CR_API void  cr_set(cr_State *ts, int obj); 
CR_API void  cr_set_index(cr_State *ts, int arrobj, cr_Integer index);
CR_API void  cr_set_field(cr_State *ts, int index, const char *field); 
CR_API void  cr_set_userdatavmt(cr_State *ts, int index, const cr_VMT *vmt); 
CR_API int   cr_set_uservalue(cr_State *ts, int index, int n); 
CR_API void  cr_set_userdatamm(cr_State *ts, int index, cr_MM mm); 


/* -------------------------------------------------------------------------
 * Error reporting
 * ------------------------------------------------------------------------- */
/* thread status codes */
#define CR_OK                   0  /* ok */
#define CR_ERRRUNTIME           1  /* runtime error */
#define CR_ERRSYNTAX            3  /* syntax error (compiler) */
#define CR_ERRMEM               4  /* memory related error (oom) */
#define CR_ERRERROR             5  /* error while handling error */

CR_API int cr_status(cr_State *ts); 
CR_API int cr_error(cr_State *ts); 


/* -------------------------------------------------------------------------
 * Call/Load CScript code
 * ------------------------------------------------------------------------- */
CR_API void cr_call(cr_State *ts, int nargs, int nresults); 
CR_API int  cr_pcall(cr_State *ts, int nargs, int nresults, int errfunc); 
CR_API int  cr_load(cr_State *ts, cr_Reader reader, void *userdata,
                    const char *source); 


/* -------------------------------------------------------------------------
 * Garbage collector
 * ------------------------------------------------------------------------- */

/* GC options */
#define CR_GCSTOP               0  /* stop GC */
#define CR_GCRESTART            1  /* restart GC (start if stopped) */
#define CR_GCCOLLECT            2  /* perform full GC cycle */
#define CR_GCCOUNT              3  /* get number of (bytes_allocated/1024) */
#define CR_GCCOUNTBYTES         4  /* get remainder of (bytes_allocated/1024) */
#define CR_GCSTEP               5  /* perform single GC step and or set debt */
#define CR_GCSETPAUSE           6  /* set GC pause (as percentage) */
#define CR_GCSETSTEPMUL         7  /* set GC step multiplier (as percentage) */
#define CR_GCISRUNNING          8  /* test whether GC is running */
#define CR_GCINC                10 /* set GC in incremental mode */

/* Limits for 'data' parameter for GC options. */
#define CR_MAXPAUSE         1023
#define CR_MAXSTEPMUL       1023

CR_API int cr_gc(cr_State *ts, int option, ...); 


/* -------------------------------------------------------------------------
 * Warning-related functions
 * ------------------------------------------------------------------------- */
CR_API void cr_setwarnf(cr_State *ts, cr_WarnFunction fwarn, void *ud); 
CR_API void cr_warning(cr_State *ts, const char *msg, int cont); 


/* -------------------------------------------------------------------------
 * Miscellaneous functions/macros
 * ------------------------------------------------------------------------- */
CR_API int              cr_hasvmt(cr_State *ts, int index); 
CR_API int              cr_hasmetamethod(cr_State *ts, int index, cr_MM mm); 
CR_API cr_Unsigned      cr_len(cr_State *ts, int index); 
CR_API int              cr_next(cr_State *ts, int index); 
CR_API void             cr_concat(cr_State *ts, int n); 
CR_API size_t           cr_stringtonumber(cr_State *ts, const char *s, int *povf); 
CR_API cr_Alloc         cr_getallocf(cr_State *ts, void **ud); 
CR_API void             cr_setallocf(cr_State *ts, cr_Alloc falloc, void *ud); 
CR_API void             cr_toclose(cr_State *ts, int index); 
CR_API void             cr_closeslot(cr_State *ts, int index); 


#define cr_getextraspace(ts)        ((void *)((char *)(ts) - CR_EXTRASPACE))

#define cr_to_number(ts,i)          cr_to_numberx(ts,(i),NULL)
#define cr_to_integer(ts,i)         cr_to_integerx(ts,(i),NULL)

#define cr_pop(ts,n)                cr_settop(ts, -(n)-1)

#define cr_push_cfunction(ts,f)     cr_push_cclosure(ts,f,0)

#define cr_register(ts,n,f)  (cr_push_cfunction(ts,(f)), cr_set_global(ts,(n)))

#define cr_is_function(ts, n)       (cr_type(ts, (n)) == CR_TFUNCTION)
#define cr_is_array(ts, n)          (cr_type(ts, (n)) == CR_TARRAY)
#define cr_is_class(ts, n)          (cr_type(ts, (n)) == CR_TCLASS)
#define cr_is_instance(ts, n)       (cr_type(ts, (n)) == CR_TINSTANCE)
#define cr_is_lightuserdata(ts, n)  (cr_type(ts, (n)) == CR_TLUDATA)
#define cr_is_nil(ts, n)            (cr_type(ts, (n)) == CR_TNIL)
#define cr_is_boolean(ts, n)        (cr_type(ts, (n)) == CR_TBOOL)
#define cr_is_thread(ts, n)         (cr_type(ts, (n)) == CR_TTHREAD)
#define cr_is_none(ts, n)           (cr_type(ts, (n)) == CR_TNONE)
#define cr_is_noneornil(ts, n)      (cr_type(ts, (n)) <= 0)

#define cr_push_literal(ts, s)      cr_push_string(ts, "" s)

#define cr_to_string(ts, i)         cr_to_lstring(ts, i, NULL)

#define cr_insert(ts,index)         cr_rotate(ts, (index), 1)

#define cr_remove(ts,index)         (cr_rotate(ts, (index), -1), cr_pop(ts, 1))

#define cr_replace(ts,index)        (cr_copy(ts, -1, (index)), cr_pop(ts, 1))


/* -------------------------------------------------------------------------
 * Debug API
 * ------------------------------------------------------------------------- */

CR_API int cr_getstack(cr_State *ts, int level, cr_DebugInfo *di); 
CR_API int cr_getinfo(cr_State *ts, const char *options, cr_DebugInfo *di); 

CR_API const char *cr_getlocal(cr_State *ts, const cr_DebugInfo *di, int n); 
CR_API const char *cr_setlocal (cr_State *ts, const cr_DebugInfo *ar, int n); 

CR_API const char *cr_getupvalue(cr_State *ts, int index, int n); 
CR_API const char *cr_setupvalue(cr_State *ts, int index, int n); 

struct cr_DebugInfo {
    /* (>) pop the function on top of the stack and load it into 'cf' */
    const char *name;       /* (n) */
    const char *namewhat;   /* (n) 'global', 'local', 'field', 'method' */
    const char *what;       /* (s) 'CScript', 'C', 'main' */
    const char *source;     /* (s) */
    size_t srclen;          /* (s) */
    int currline;           /* (l) */
    int defline;            /* (s) */
    int lastdefline;        /* (s) */
    int nupvals;            /* (u) */
    int nparams;            /* (u) */
    char isvararg;          /* (u) */
    char shortsrc[CRI_MAXSRC]; /* (s) */
    /* (f) pushes onto stack the function that is running at the given level */
    /* private */
    struct CallFrame *cf; /* active function */
};


/* 
** Big 'Thank You' to Lua Developers!
*/

/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 1994-2023 Lua.org, PUC-Rio.
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * ---------------------------------------------------------------------------------------------- */


#endif
