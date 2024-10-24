#ifndef CSCRIPT_H
#define CSCRIPT_H

#include <stddef.h>
#include <stdarg.h>

#include "cconf.h"


/* version info and copyright */
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
#define CR_REGISTRYINDEX	    (-CRI_MAXSTACK - 1000)
#define cr_upvalueindex(i)	    (CR_REGISTRYINDEX - (i))


/* minimum stack space available to a C function */
#define CR_MINSTACK     20


/* CScript thread state */
typedef struct cr_State cr_State;



/* types of values */
#define CR_TNONE        (-1)

#define CR_TBOOL        0 /* boolean */
#define CR_TNUMBER      1 /* number */
#define CR_TLUDATA      2 /* light userdata */
#define CR_TSTRING      3 /* string */
#define CR_TFUNCTION    4 /* function */
#define CR_TCLASS       5 /* class */
#define CR_TINSTANCE    6 /* instance */
#define CR_TUDATA       7 /* userdata */
#define CR_TNIL         8 /* nil */
#define CR_TTHREAD      9 /* thread */

#define CR_NUM_TYPES    10



/* type for integers */
typedef CR_INTEGER cr_Integer;

/* type for unsigned integers */
typedef CR_UNSIGNED cr_Unsigned;

/* type for floating point numbers (CScript numbers) */
typedef CR_NUMBER cr_Number;


/* C function registered with CScript */
typedef int (*cr_CFunction)(cr_State *ts);

/* function for memory de/allocation */
typedef void *(*cr_fAlloc)(void *ptr, size_t osz, size_t nsz, void *userdata);

/* function that reads blocks when loading CScript chunks */
typedef const char *(*cr_fReader)(cr_State *ts, void *data, size_t *szread);

/* type for class API (Virtual Method Table) */
typedef struct cr_VMT cr_VMT;

/* type for debug API */
typedef struct cr_DebugInfo cr_DebugInfo;



/* -------------------------------------------------------------------------
 * State manipulation
 * ------------------------------------------------------------------------- */
CR_API cr_State        *cr_newstate(cr_fAlloc allocator, void *ud); /* DONE */
CR_API void             cr_freestate(cr_State *ts);/* DONE */
CR_API cr_State        *cr_newthread(cr_State *ts);/* DONE */
CR_API int              cr_resetthread(cr_State *ts);/* DONE */
CR_API cr_Number        cr_version(cr_State *ts);/* DONE */


/* -------------------------------------------------------------------------
 * Stack manipulation
 * ------------------------------------------------------------------------- */
CR_API void             cr_settop(cr_State *ts, int index); /* DONE */
CR_API int              cr_gettop(const cr_State *ts); /* DONE */
CR_API int              cr_absindex(cr_State *ts, int index); /* DONE */
CR_API void             cr_rotate(cr_State *ts, int index, int n); /* DONE */
CR_API void             cr_copy(cr_State *ts, int src, int dest); /* DONE */
CR_API int              cr_checkstack(cr_State *ts, int n); /* DONE */
CR_API void             cr_push(cr_State *ts, int index); /* DONE */
CR_API void             cr_xmove(cr_State *src, cr_State *dest, int n); /* DONE */


/* -------------------------------------------------------------------------
 * Access functions (Stack -> C)
 * ------------------------------------------------------------------------- */
CR_API int              cr_isnumber(cr_State *ts, int index); /* DONE */
CR_API int              cr_isinteger(cr_State *ts, int index); /* DONE */
CR_API int              cr_isstring(cr_State *ts, int index); /* DONE */
CR_API int              cr_iscfunction(cr_State *ts, int index); /* DONE */
CR_API int              cr_isuserdata(cr_State *ts, int index); /* DONE */
CR_API int              cr_type(cr_State *ts, int index); /* DONE */
CR_API const char      *cr_typename(cr_State *ts, int type); /* DONE */

CR_API cr_Number        cr_getnumber(cr_State *ts, int index, int *isnum); /* DONE */
CR_API cr_Integer       cr_getinteger(cr_State *ts, int index, int *isnum); /* DONE */
CR_API int              cr_getbool(cr_State *ts, int index); /* DONE */
CR_API const char      *cr_getstring(cr_State *ts, int index, size_t *plen); /* DONE */
CR_API cr_CFunction     cr_getcfunction(cr_State *ts, int index); /* DONE */
CR_API void            *cr_getuserdata(cr_State *ts, int index); /* DONE */
CR_API const void      *cr_getpointer(cr_State *ts, int index); /* DONE */
CR_API cr_State        *cr_getthread(cr_State *ts, int index); /* DONE */
CR_API cr_Unsigned      cr_len(cr_State *ts, int index); /* DONE */

/* TODO */
CR_API const char      *cr_tostring(cr_State *ts, int index, size_t *len);


/* -------------------------------------------------------------------------
 * Ordering & Arithmetic functions
 * ------------------------------------------------------------------------- */
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

CR_API void     cr_arith(cr_State *ts, int op); /* DONE */


#define CR_OPEQ         0
#define CR_OPLT         1
#define CR_OPLE         2

#define CR_NUM_CMP      3

CR_API int      cr_rawequal(cr_State *ts, int idx1, int idx2); /* DONE */
CR_API int      cr_compare(cr_State *ts, int idx1, int idx2, int op); /* DONE */


/* -------------------------------------------------------------------------
 * Push functions (C -> stack)
 * ------------------------------------------------------------------------- */
CR_API void             cr_pushnil(cr_State *ts); /* DONE */
CR_API void             cr_pushnumber(cr_State *ts, cr_Number n); /* DONE */
CR_API void             cr_pushinteger(cr_State *ts, cr_Integer n); /* DONE */
CR_API const char      *cr_pushstring(cr_State *ts, const char *str, size_t len); /* DONE */
CR_API const char      *cr_pushcstring(cr_State *ts, const char *str); /* DONE */
CR_API const char      *cr_pushvfstring(cr_State *ts, const char *fmt, va_list argp); /* DONE */
CR_API const char      *cr_pushfstring(cr_State *ts, const char *fmt, ...); /* DONE */
CR_API void             cr_pushcclosure(cr_State *ts, cr_CFunction fn, int upvals); /* DONE */
CR_API void             cr_pushbool(cr_State *ts, int b); /* DONE */
CR_API void             cr_pushlightuserdata(cr_State *ts, void *p); /* DONE */
CR_API int              cr_pushthread(cr_State *ts); /* DONE */


/* -------------------------------------------------------------------------
 * Get functions (CScript -> stack)
 * ------------------------------------------------------------------------- */
CR_API int cr_getglobal(cr_State *ts, const char *name);
CR_API int cr_getfield(cr_State *ts, int idx, const char *field);
CR_API int cr_getmethod(cr_State *ts, int idx, const char *method);
CR_API int cr_getindex(cr_State *ts, int idx);
CR_API int cr_rawget(cr_State *ts, int idx);
CR_API int cr_rawgeti(cr_State *ts, int idx, cr_Integer n);
CR_API int cr_rawgetp(cr_State *ts, int idx, const void *p);

CR_API int cr_createarray(cr_State *ts, int nelems); // TODO
CR_API int cr_createuserdata(cr_State *ts, size_t sz, int nuvalues); // TODO
CR_API int cr_getuservalue(cr_State *ts, int idx, int n);


/* -------------------------------------------------------------------------
 * Class interface
 * ------------------------------------------------------------------------- */
/* meta method type ('mmt') */
#define CR_MMT_NONE     (-1)
#define CR_MMT_CFN      0
#define CR_MMT_IDX      1

/* meta methods ('mm') */
typedef enum cr_MM {
    CR_MM_INIT = 0,
    CR_MM_GETIDX,
    CR_MM_SETIDX,
    CR_MM_GETFIELD,
    CR_MM_SETFIELD,
    CR_MM_GC,
    CR_MM_CLOSE,
    CR_MM_CALL,
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

/* Virtual Method Table */
struct cr_VMT {
    struct {
        union {
            cr_CFunction cfn; /* C function */
            int idx; /* value on stack */
        } m; /* method */
        int mmt; /* 'mmt' */
    } mm[CR_NUM_MM];
};

CR_API void cr_createclass(cr_State *ts, cr_VMT *vmt, int supidx);


/* -------------------------------------------------------------------------
 * Set functions (stack -> CScript)
 * ------------------------------------------------------------------------- */
CR_API int  cr_setglobal(cr_State *ts, const char *name, int isconst);
CR_API int  cr_setfield(cr_State *ts, int idx, const char *field);
CR_API int  cr_setindex(cr_State *ts, int idx);
CR_API int  cr_seti(cr_State *ts, int idx, cr_Integer n); // TODO
CR_API int  cr_rawset(cr_State *ts, int idx); // TODO
CR_API int  cr_rawseti(cr_State *ts, int idx, cr_Integer n); // TODO
CR_API int  cr_rawsetp(cr_State *ts, int idx, void *p); // TODO
CR_API int  cr_setuservalue(cr_State *ts, int idx, int n); // TODO


/* -------------------------------------------------------------------------
 * Error reporting
 * ------------------------------------------------------------------------- */

/* thread status codes */
#define CR_OK                   0  /* ok */
#define CR_ERRRUNTIME           1  /* runtime error */
#define CR_ERRSYNTAX            3  /* syntax error (compiler) */
#define CR_ERRMEM               4  /* memory related error (oom) */
#define CR_ERRERROR             5  /* error while handling error */

CR_API int  cr_getstatus(cr_State *ts);
CR_API int  cr_error(cr_State *ts);


/* -------------------------------------------------------------------------
 * Call/Load CScript code
 * ------------------------------------------------------------------------- */
CR_API int  cr_pcall(cr_State *ts, int argc, int retcnt);
CR_API void cr_call(cr_State *ts, int argc, int retcnt);
CR_API int  cr_load(cr_State *ts, cr_fReader reader, void *userdata, const char *source);


/* -------------------------------------------------------------------------
 * Garbage collector
 * ------------------------------------------------------------------------- */

/* GC options */
#define CR_GCSTOP               (1<<0) /* stop GC */
#define CR_GCRESTART            (1<<1) /* restart GC (start if stopped) */
#define CR_GCCOLLECT            (1<<2) /* perform full GC cycle */
#define CR_GCSTEP               (1<<3) /* perform single gc step */
#define CR_GCCOUNT              (1<<4) /* get number of bytes allocated */
#define CR_GCISRUNNING          (1<<5) /* check whether GC is stopped */
#define CR_GCNEXTGC             (1<<6) /* set bytes amount when the next GC 
                                          will trigger */

CR_API int cr_gc(cr_State *ts, int optmask, ...);


/* -------------------------------------------------------------------------
 * Miscellaneous functions/macros
 * ------------------------------------------------------------------------- */
CR_API const char      *cr_stringify(cr_State *ts, int idx); // TODO ?
CR_API int              cr_getupvalue(cr_State *ts, int fidx, int idx);
CR_API int              cr_setupvalue(cr_State *ts, int fidx, int idx);
CR_API const char      *cr_concat(cr_State *ts);
CR_API int              cr_nextproperty(cr_State *ts, int idx, int nextfield);
CR_API cr_CFunction     cr_setpanic(cr_State *ts, cr_CFunction panicfn);
CR_API cr_CFunction     cr_getpanic(cr_State *ts);
CR_API cr_CFunction     cr_setalloc(cr_State *ts, cr_fAlloc allocfn, void *ud);
CR_API cr_fAlloc        cr_getalloc(cr_State *ts, void **ud);

#define cr_getextraspace(ts)    ((void *)((char *)(ts) - CR_EXTRASPACE))

#define cr_nextfield(ts,idx)            cr_nextproperty((ts),(idx),0)
#define cr_nextmethod(ts,idx)           cr_nextproperty((ts),(idx),1)

#define cr_pushcfunction(ts,fn)         cr_pushcclosure((ts),(fn),0)

#define cr_registerfunction(ts,fn,name) \
    (cr_pushcfunction((ts),(fn)), cr_setglobal((ts),(name),1))

#define cr_pop(ts,n)            cr_settop((ts),-(n)-1)
#define cr_replace(ts,idx)      (cr_copy((ts),-1,(idx)), cr_pop((ts),1))
#define cr_remove(ts,idx)       (cr_rotate((ts),(idx),-1), cr_pop((ts),1))
#define cr_insert(ts,idx)       cr_rotate((ts),(idx),1)


/* -------------------------------------------------------------------------
 * Debug interface
 * ------------------------------------------------------------------------- */

/* bits for 'dbgmask' */
#define CR_DBG_FNGET    (1<<0) /* push func on stack (processed first) */
#define CR_DBG_LINE     (1<<1) /* fill 'line' */
#define CR_DBG_FNINFO   (1<<2) /* fill all function info in 'crD_info' */
#define CR_DBG_FNSRC    (1<<3) /* fill function source information */
#define CR_DBG_FNPUSH   (1<<4) /* push current func (processed last) */

CR_API int cr_getstack(cr_State *ts, int level, cr_DebugInfo *di);
CR_API int cr_getinfo(cr_State *ts, int dbgmask, cr_DebugInfo *di);

struct cr_DebugInfo {
    const char *type; /* function type ('cript', 'main' or 'C') */
    const char *source; /* function source */
    size_t srclen; /* length of 'source' */
    int line; /* current line in cript script */
    int nups; /* number of function upvalues */
    int nparams; /* number of function parameters */
    char isvararg; /* is function vararg ('...') */
    int defline; /* line number where the function definition starts */
    int deflastline; /* line number where the function definition ends */
    char shortsrc[CRI_MAXSRC];
    /* private */
    struct CallFrame *cf; /* active function frame */
};


#endif

/* Big Thank You to Lua Developers! */
/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 1994-2024 Lua.org, PUC-Rio.
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
