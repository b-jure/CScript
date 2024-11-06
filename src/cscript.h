/*
** cscript.h
** CScript - Scripting Language inspired by Lua and C
** See Copyright Notice at the end of this file
*/

#ifndef CSCRIPT_H
#define CSCRIPT_H

#include <stddef.h>
#include <stdarg.h>

#include "cconf.h"


#define CS_VERSION_MAJOR        "1"
#define CS_VERSION_MINOR        "0"
#define CS_VERSION_RELEASE      "0"

#define CS_VERSION_NUMBER               100
#define CS_VERSION_RELEASE_NUM          (CS_VERSION_NUMBER * 100);

#define CS_VERSION      "CScript " CS_VERSION_MAJOR "." CS_VERSION_MINOR
#define CS_RELEASE      CS_VERSION "." CS_VERSION_RELEASE
#define CS_COPYRIGHT    CS_RELEASE " Copyright (C) 2023-2024 Jure Bagić"



/* option for multiple returns in 'cs_pcall' and 'cs_call' */
#define CS_MULRET       (-1)


/*
** Pseudo-indices
** (-CSI_MAXSTACK is the minimum valid index; we keep some free empty
** space after that to help overflow detection)
*/
#define CS_GINSTANCEINDEX           (-CSI_MAXSTACK - 1000)
#define cs_upvalueindex(i)          (CS_GINSTANCEINDEX - (i))


/* name of the global hashtable class */
#define CS_HASHTABLE        "HashTable"


/* minimum stack space available to a C function */
#define CS_MINSTACK     20


/* CScript thread state */
typedef struct cs_State cs_State;



/* types of values */
#define CS_TNONE        (-1)

#define CS_TNIL         0   /* nil */
#define CS_TBOOL        1   /* boolean */
#define CS_TNUMBER      2   /* number */
#define CS_TLUDATA      3   /* light userdata */
#define CS_TSTRING      4   /* string */
#define CS_TARRAY       5   /* string */
#define CS_TFUNCTION    6   /* function */
#define CS_TCLASS       7   /* class */
#define CS_TINSTANCE    8   /* instance */
#define CS_TUDATA       9   /* userdata */
#define CS_TTHREAD      10  /* thread */

#define CS_NUM_TYPES    11



/* type for integers */
typedef CS_INTEGER cs_Integer;

/* type for unsigned integers */
typedef CS_UNSIGNED cs_Unsigned;

/* type for floating point numbers */
typedef CS_NUMBER cs_Number;


/* C function registered with CScript */
typedef int (*cs_CFunction)(cs_State *ts);

/* Function for memory de/allocation */
typedef void *(*cs_Alloc)(void *ptr, size_t osz, size_t nsz, void *ud);

/* Function that reads blocks when loading CScript chunks */
typedef const char *(*cs_Reader)(cs_State *ts, void *data, size_t *szread);

/* Type for warning functions */
typedef void (*cs_WarnFunction)(void *ud, const char *msg, int tocont);


/* Virtual Method Table (for metamethods) */
typedef struct cs_VMT cs_VMT;

/* Type for storing name:function pairs or placeholders */
typedef struct cs_Entry cs_Entry;

/* Type for debug API */
typedef struct cs_DebugInfo cs_DebugInfo;


/* metamethods */
typedef enum cs_MM {
    CS_MM_INIT = 0,
    CS_MM_GETIDX,
    CS_MM_SETIDX,
    CS_MM_GC,
    CS_MM_CLOSE,
    CS_MM_CALL,
    CS_MM_CONCAT,
    CS_MM_ADD,
    CS_MM_SUB,
    CS_MM_MUL,
    CS_MM_DIV,
    CS_MM_MOD,
    CS_MM_POW,
    CS_MM_BSHL,
    CS_MM_BSHR,
    CS_MM_BAND,
    CS_MM_BOR,
    CS_MM_BXOR,
    CS_MM_UNM,
    CS_MM_BNOT,
    CS_MM_EQ,
    CS_MM_LT,
    CS_MM_LE,
} cs_MM;

#define CS_NUM_MM     (CS_MM_LE + 1)

struct cs_VMT {
    cs_CFunction func[CS_NUM_MM]; /* metamethods */
};

struct cs_Entry {
    const char *name;
    cs_CFunction func;
};


/* -------------------------------------------------------------------------
 * State manipulation
 * ------------------------------------------------------------------------- */
CS_API cs_State        *cs_newstate(cs_Alloc allocator, void *ud); 
CS_API void             cs_freestate(cs_State *ts);
CS_API cs_State        *cs_newthread(cs_State *ts);
CS_API int              cs_resetthread(cs_State *ts);
CS_API cs_CFunction     cs_atpanic(cs_State *ts, cs_CFunction fn);
CS_API cs_Number        cs_version(cs_State *ts);


/* -------------------------------------------------------------------------
 * Stack manipulation
 * ------------------------------------------------------------------------- */
CS_API void             cs_setntop(cs_State *ts, int nvals); 
CS_API int              cs_gettop(const cs_State *ts); 
CS_API int              cs_absindex(cs_State *ts, int index); 
CS_API void             cs_rotate(cs_State *ts, int index, int n); 
CS_API void             cs_copy(cs_State *ts, int src, int dest); 
CS_API int              cs_checkstack(cs_State *ts, int n); 
CS_API void             cs_push(cs_State *ts, int index); 
CS_API void             cs_xmove(cs_State *src, cs_State *dest, int n); 


/* -------------------------------------------------------------------------
 * Access functions (Stack -> C)
 * ------------------------------------------------------------------------- */
CS_API int              cs_is_number(cs_State *ts, int index); 
CS_API int              cs_is_integer(cs_State *ts, int index); 
CS_API int              cs_is_string(cs_State *ts, int index); 
CS_API int              cs_is_cfunction(cs_State *ts, int index); 
CS_API int              cs_is_userdata(cs_State *ts, int index); 
CS_API int              cs_type(cs_State *ts, int index); 
CS_API const char      *cs_typename(cs_State *ts, int type); 

CS_API cs_Number        cs_to_numberx(cs_State *ts, int index, int *isnum); 
CS_API cs_Integer       cs_to_integerx(cs_State *ts, int index, int *isnum); 
CS_API int              cs_to_bool(cs_State *ts, int index); 
CS_API const char      *cs_to_lstring(cs_State *ts, int index, size_t *plen); 
CS_API cs_CFunction     cs_to_cfunction(cs_State *ts, int index); 
CS_API void            *cs_to_userdata(cs_State *ts, int index); 
CS_API const void      *cs_to_pointer(cs_State *ts, int index); 
CS_API cs_State        *cs_to_thread(cs_State *ts, int index); 


/* -------------------------------------------------------------------------
 * Ordering & Arithmetic functions
 * ------------------------------------------------------------------------- */
/* Arithmetic operations */
#define CS_OPADD        0
#define CS_OPSUB        1
#define CS_OPMUL        2
#define CS_OPDIV        3
#define CS_OPMOD        4
#define CS_OPPOW        5
#define CS_OPBSHL       6
#define CS_OPBSHR       7
#define CS_OPBAND       8
#define CS_OPBOR        9
#define CS_OPBXOR       10
#define CS_OPUNM        11
#define CS_OPBNOT       12

#define CS_NUM_ARITH    13

CS_API void     cs_arith(cs_State *ts, int op); 


/* Ordering operations */
#define CS_OPEQ         0
#define CS_OPLT         1
#define CS_OPLE         2

#define CS_NUM_CMP      3

CS_API int      cs_rawequal(cs_State *ts, int idx1, int idx2); 
CS_API int      cs_compare(cs_State *ts, int idx1, int idx2, int op); 


/* -------------------------------------------------------------------------
 * Push functions (C -> stack)
 * ------------------------------------------------------------------------- */
CS_API void        cs_push_nil(cs_State *ts); 
CS_API void        cs_push_number(cs_State *ts, cs_Number n); 
CS_API void        cs_push_integer(cs_State *ts, cs_Integer n); 
CS_API const char *cs_push_lstring(cs_State *ts, const char *str, size_t len); 
CS_API const char *cs_push_string(cs_State *ts, const char *str); 
CS_API const char *cs_push_vfstring(cs_State *ts, const char *fmt, va_list argp); 
CS_API const char *cs_push_fstring(cs_State *ts, const char *fmt, ...); 
CS_API void        cs_push_cclosure(cs_State *ts, cs_CFunction fn, int upvals); 
CS_API void        cs_push_bool(cs_State *ts, int b); 
CS_API void        cs_push_lightuserdata(cs_State *ts, void *p); 
CS_API void        cs_push_array(cs_State *ts);
CS_API int         cs_push_thread(cs_State *ts); 
CS_API void        cs_push_instance(cs_State *ts, int clsobj);
CS_API void        cs_push_class(cs_State *ts, cs_VMT *vmt, int clsobj,
                                 int nup, cs_Entry *list); 


/* -------------------------------------------------------------------------
 * Get functions (CScript -> stack)
 * ------------------------------------------------------------------------- */
CS_API int   cs_get_global(cs_State *ts, const char *name); 
CS_API int   cs_get(cs_State *ts, int obj); 
CS_API int   cs_get_index(cs_State *ts, int arrobj, cs_Integer index);
CS_API int   cs_get_field(cs_State *ts, int insobj); 
CS_API int   cs_get_fieldstr(cs_State *ts, int insobj, const char *field); 
CS_API int   cs_get_class(cs_State *ts, int insobj); 
CS_API int   cs_get_method(cs_State *ts, int insobj); 
CS_API int   cs_get_metamethod(cs_State *ts, int obj, cs_MM mm); 

CS_API void *cs_newuserdata(cs_State *ts, size_t sz, int nuv); 
CS_API int   cs_get_uservalue(cs_State *ts, int udobj, int n); 


/* -------------------------------------------------------------------------
 * Set functions (stack -> CScript)
 * ------------------------------------------------------------------------- */
CS_API void  cs_set_global(cs_State *ts, const char *name); 
CS_API void  cs_set(cs_State *ts, int obj); 
CS_API void  cs_set_index(cs_State *ts, int arrobj, cs_Integer index);
CS_API void  cs_set_field(cs_State *ts, int insobj); 
CS_API void  cs_set_fieldstr(cs_State *ts, int insobj, const char *field); 
CS_API void  cs_set_userdatavmt(cs_State *ts, int index, const cs_VMT *vmt); 
CS_API int   cs_set_uservalue(cs_State *ts, int index, int n); 
CS_API void  cs_set_userdatamm(cs_State *ts, int index, cs_MM mm); 


/* -------------------------------------------------------------------------
 * Error reporting
 * ------------------------------------------------------------------------- */
/* thread status codes */
#define CS_OK                   0  /* ok */
#define CS_ERRRUNTIME           1  /* runtime error */
#define CS_ERRSYNTAX            3  /* syntax error (compiler) */
#define CS_ERRMEM               4  /* memory related error (oom) */
#define CS_ERRERROR             5  /* error while handling error */

CS_API int cs_status(cs_State *ts); 
CS_API int cs_error(cs_State *ts); 


/* -------------------------------------------------------------------------
 * Call/Load CScript code
 * ------------------------------------------------------------------------- */
CS_API void cs_call(cs_State *ts, int nargs, int nresults); 
CS_API int  cs_pcall(cs_State *ts, int nargs, int nresults, int errfunc); 
CS_API int  cs_load(cs_State *ts, cs_Reader reader, void *userdata,
                    const char *source); 


/* -------------------------------------------------------------------------
 * Garbage collector
 * ------------------------------------------------------------------------- */

/* GC options */
#define CS_GCSTOP               0 /* stop GC */
#define CS_GCRESTART            1 /* restart GC (start if stopped) */
#define CS_GCCOLLECT            2 /* perform full GC cycle */
#define CS_GCCOUNT              3 /* get number of (bytes_allocated/1024) */
#define CS_GCCOUNTBYTES         4 /* get remainder of (bytes_allocated/1024) */
#define CS_GCSTEP               5 /* perform single GC step and or set debt */
#define CS_GCSETPAUSE           6 /* set GC pause (as percentage) */
#define CS_GCSETSTEPMUL         7 /* set GC step multiplier (as percentage) */
#define CS_GCISRUNNING          8 /* test whether GC is running */
#define CS_GCINC                9 /* set GC in incremental mode */

/* Limits for 'data' parameter for GC options. */
#define CS_MAXPAUSE         1023
#define CS_MAXSTEPMUL       1023

CS_API int cs_gc(cs_State *ts, int option, ...); 


/* -------------------------------------------------------------------------
 * Warning-related functions
 * ------------------------------------------------------------------------- */
CS_API void cs_setwarnf(cs_State *ts, cs_WarnFunction fwarn, void *ud); 
CS_API void cs_warning(cs_State *ts, const char *msg, int cont); 


/* -------------------------------------------------------------------------
 * Miscellaneous functions/macros
 * ------------------------------------------------------------------------- */
CS_API int              cs_hasvmt(cs_State *ts, int index); 
CS_API int              cs_hasmetamethod(cs_State *ts, int index, cs_MM mm); 
CS_API cs_Unsigned      cs_len(cs_State *ts, int index); 
CS_API int              cs_next(cs_State *ts, int insobj); 
CS_API void             cs_concat(cs_State *ts, int n); 
CS_API cs_Alloc         cs_getallocf(cs_State *ts, void **ud); 
CS_API void             cs_setallocf(cs_State *ts, cs_Alloc falloc, void *ud); 
CS_API void             cs_toclose(cs_State *ts, int index); 
CS_API void             cs_closeslot(cs_State *ts, int index); 
CS_API size_t           cs_stringtonumber(cs_State *ts, const char *s, int *of); 


#define cs_getextraspace(ts)        ((void *)((char *)(ts) - CS_EXTRASPACE))

#define cs_nvalues(ts)              (cs_gettop(ts) + 1)

#define cs_to_number(ts,i)          cs_to_numberx(ts,(i),NULL)
#define cs_to_integer(ts,i)         cs_to_integerx(ts,(i),NULL)

#define cs_pop(ts,n)                cs_setntop(ts, -(n)-1)

#define cs_push_cfunction(ts,f)     cs_push_cclosure(ts,f,0)

#define cs_register(ts,n,f)  (cs_push_cfunction(ts,(f)), cs_set_global(ts,(n)))

#define cs_is_function(ts, n)       (cs_type(ts, (n)) == CS_TFUNCTION)
#define cs_is_array(ts, n)          (cs_type(ts, (n)) == CS_TARRAY)
#define cs_is_class(ts, n)          (cs_type(ts, (n)) == CS_TCLASS)
#define cs_is_instance(ts, n)       (cs_type(ts, (n)) == CS_TINSTANCE)
#define cs_is_lightuserdata(ts, n)  (cs_type(ts, (n)) == CS_TLUDATA)
#define cs_is_nil(ts, n)            (cs_type(ts, (n)) == CS_TNIL)
#define cs_is_boolean(ts, n)        (cs_type(ts, (n)) == CS_TBOOL)
#define cs_is_thread(ts, n)         (cs_type(ts, (n)) == CS_TTHREAD)
#define cs_is_none(ts, n)           (cs_type(ts, (n)) == CS_TNONE)
#define cs_is_noneornil(ts, n)      (cs_type(ts, (n)) <= 0)

#define cs_push_literal(ts, s)      cs_push_string(ts, "" s)

#define cs_push_globalinstance(ts)  cs_push(ts, CS_GINSTANCEINDEX)

#define cs_to_string(ts, i)         cs_to_lstring(ts, i, NULL)

#define cs_insert(ts,index)         cs_rotate(ts, (index), 1)

#define cs_remove(ts,index)         (cs_rotate(ts, (index), -1), cs_pop(ts, 1))

#define cs_replace(ts,index)        (cs_copy(ts, -1, (index)), cs_pop(ts, 1))



/* -------------------------------------------------------------------------
 * Debug API
 * ------------------------------------------------------------------------- */

CS_API int cs_getstack(cs_State *ts, int level, cs_DebugInfo *di); 
CS_API int cs_getinfo(cs_State *ts, const char *options, cs_DebugInfo *di); 

CS_API const char *cs_getlocal(cs_State *ts, const cs_DebugInfo *di, int n); 
CS_API const char *cs_setlocal (cs_State *ts, const cs_DebugInfo *ar, int n); 

CS_API const char *cs_getupvalue(cs_State *ts, int index, int n); 
CS_API const char *cs_setupvalue(cs_State *ts, int index, int n); 

struct cs_DebugInfo {
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
    char shortsrc[CSI_MAXSRC]; /* (s) */
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
