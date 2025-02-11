/*
** cscript.h
** CScript - Scripting Language inspired by Lua and C
** See Copyright Notice at the end of this file
*/

#ifndef CSCRIPT_H
#define CSCRIPT_H

#include <stddef.h>
#include <stdarg.h>

#include "csconf.h"


#define CS_VERSION_MAJOR        "1"
#define CS_VERSION_MINOR        "0"
#define CS_VERSION_RELEASE      "0"

#define CS_VERSION_NUMBER               100
#define CS_VERSION_RELEASE_NUM          (CS_VERSION_NUMBER * 100);

#define CS_VERSION      "CScript " CS_VERSION_MAJOR "." CS_VERSION_MINOR
#define CS_RELEASE      CS_VERSION "." CS_VERSION_RELEASE
#define CS_COPYRIGHT    CS_RELEASE " Copyright (C) 2024-2025 Jure Bagić"

/* For use in binary */
#define LUA_COPYRIGHT   "Lua 5.3.6  Copyright (C) 1994-2020 Lua.org, PUC-Rio"


/* option for multiple returns in 'cs_pcall' and 'cs_call' */
#define CS_MULRET       (-1)


/*
** Pseudo-indices
** (-CSI_MAXSTACK is the minimum valid index; we keep some free empty
** space after that to help overflow detection)
*/
#define CS_REGISTRYINDEX            (-CSI_MAXSTACK - 1000)
#define cs_upvalueindex(i)          (CS_REGISTRYINDEX - (i) - 1)


/* CScript thread state */
typedef struct cs_State cs_State;


/* types of values */
#define CS_TNONE        (-1)

#define CS_TNIL                 0   /* nil */
#define CS_TBOOL                1   /* boolean */
#define CS_TNUMBER              2   /* number */
#define CS_TUSERDATA            3   /* userdata */
#define CS_TLIGHTUSERDATA       4   /* light userdata */
#define CS_TSTRING              5   /* string */
#define CS_TARRAY               6   /* array */
#define CS_TTABLE               7   /* table */
#define CS_TFUNCTION            8   /* function */
#define CS_TCLASS               9   /* class */
#define CS_TINSTANCE            10  /* instance */
#define CS_TTHREAD              11  /* thread */

#define CS_NUM_TYPES            12



/* minimum stack space available to a C function */
#define CS_MINSTACK     20


/* predefined values in the registry */
#define CS_RINDEX_MAINTHREAD        0
#define CS_RINDEX_GLOBALS           1
#define CS_RINDEX_LAST              CS_RINDEX_GLOBALS



/* type for integers */
typedef CS_INTEGER cs_Integer;

/* type for unsigned integers */
typedef CS_UNSIGNED cs_Unsigned;

/* type for floating point numbers */
typedef CS_NUMBER cs_Number;


/* C function registered with CScript */
typedef int (*cs_CFunction)(cs_State *C);

/* Function for memory de/allocation */
typedef void *(*cs_Alloc)(void *ptr, size_t osz, size_t nsz, void *ud);

/* Function that reads blocks when loading CScript chunks */
typedef const char *(*cs_Reader)(cs_State *C, void *data, size_t *szread);

/* Type for warning functions */
typedef void (*cs_WarnFunction)(void *ud, const char *msg, int tocont);


/* Virtual Method Table */
typedef struct cs_VMT cs_VMT;

/* Type for storing name:function pairs or placeholders */
typedef struct cs_Entry cs_Entry;

/* Type for debug API */
typedef struct cs_Debug cs_Debug;


/* metamethods */
typedef enum cs_MM {    /* ORDER MM */
    CS_MM_GETIDX = 0,
    CS_MM_SETIDX,
    CS_MM_GC,
    CS_MM_CLOSE,
    CS_MM_CALL,
    CS_MM_CONCAT,
    CS_MM_ADD,          /* ORDER OP */
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
    CS_MM_N, /* number of mm */
} cs_MM;


struct cs_VMT {
    cs_CFunction func[CS_MM_N]; /* metamethods */
};


struct cs_Entry {
    const char *name;
    cs_CFunction func;
};


/* -------------------------------------------------------------------------
 * State manipulation
 * ------------------------------------------------------------------------- */
CS_API cs_State        *cs_newstate(cs_Alloc allocator, void *ud); 
CS_API void             cs_close(cs_State *C);
CS_API cs_State        *cs_newthread(cs_State *C);
CS_API int              cs_resetthread(cs_State *C);
CS_API cs_CFunction     cs_atpanic(cs_State *C, cs_CFunction fn);
CS_API cs_Number        cs_version(cs_State *C);

/* -----------------------------------------------------------------------
** Stack manipulation
** ----------------------------------------------------------------------- */
CS_API void             cs_setntop(cs_State *C, int n); 
CS_API int              cs_gettop(const cs_State *C); 
CS_API int              cs_absindex(cs_State *C, int index); 
CS_API void             cs_rotate(cs_State *C, int index, int n); 
CS_API void             cs_copy(cs_State *C, int src, int dest); 
CS_API int              cs_checkstack(cs_State *C, int n); 
CS_API void             cs_push(cs_State *C, int index); 
CS_API void             cs_xmove(cs_State *src, cs_State *dest, int n); 

/* -----------------------------------------------------------------------
** Access functions (Stack -> C)
** ----------------------------------------------------------------------- */
CS_API int              cs_is_number(cs_State *C, int index); 
CS_API int              cs_is_integer(cs_State *C, int index); 
CS_API int              cs_is_string(cs_State *C, int index); 
CS_API int              cs_is_cfunction(cs_State *C, int index); 
CS_API int              cs_is_userdata(cs_State *C, int index); 
CS_API int              cs_type(cs_State *C, int index); 
CS_API const char      *cs_typename(cs_State *C, int type); 

CS_API cs_Number        cs_to_numberx(cs_State *C, int index, int *isnum); 
CS_API cs_Integer       cs_to_integerx(cs_State *C, int index, int *isnum); 
CS_API int              cs_to_bool(cs_State *C, int index); 
CS_API const char      *cs_to_lstring(cs_State *C, int index, size_t *len); 
CS_API cs_CFunction     cs_to_cfunction(cs_State *C, int index); 
CS_API void            *cs_to_userdata(cs_State *C, int index); 
CS_API const void      *cs_to_pointer(cs_State *C, int index); 
CS_API cs_State        *cs_to_thread(cs_State *C, int index); 

/* -----------------------------------------------------------------------
** Ordering & Arithmetic functions
** ----------------------------------------------------------------------- */
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

CS_API void     cs_arith(cs_State *C, int op); 


/* Ordering operations */
#define CS_OPEQ         0
#define CS_OPLT         1
#define CS_OPLE         2

#define CS_NUM_CMP      3

CS_API int      cs_rawequal(cs_State *C, int idx1, int idx2); 
CS_API int      cs_compare(cs_State *C, int idx1, int idx2, int op); 

/* -----------------------------------------------------------------------
** Push functions (C -> stack)
** ----------------------------------------------------------------------- */
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
CS_API void        cs_push_array(cs_State *C, int sz);
CS_API void        cs_push_table(cs_State *C, int sz);
CS_API int         cs_push_thread(cs_State *C); 
CS_API void        cs_push_instance(cs_State *C, int clsobj);
CS_API void        cs_push_class(cs_State *C, const cs_VMT *vmt, int abscls,
                                 int nup, const cs_Entry *list); 

/* -----------------------------------------------------------------------
** Get functions (CScript -> stack)
** ----------------------------------------------------------------------- */
CS_API int   cs_get_global(cs_State *C, const char *name); 
CS_API int   cs_get(cs_State *C, int index); 
CS_API int   cs_get_raw(cs_State *C, int index); 
CS_API int   cs_get_index(cs_State *C, int index, cs_Integer i);
CS_API int   cs_get_nilindex(cs_State *C, int index, int begin, int end);
CS_API int   cs_get_field(cs_State *C, int index); 
CS_API int   cs_get_fieldstr(cs_State *C, int index, const char *field); 
CS_API int   cs_get_fieldptr(cs_State *C, int index, const void *field); 
CS_API int   cs_get_fieldint(cs_State *C, int index, cs_Integer field); 
CS_API int   cs_get_fieldflt(cs_State *C, int index, cs_Number field); 
CS_API int   cs_get_class(cs_State *C, int index); 
CS_API int   cs_get_method(cs_State *C, int index); 
CS_API int   cs_get_metamethod(cs_State *C, int index, cs_MM mm); 

CS_API void *cs_newuserdata(cs_State *C, size_t sz, int nuv); 
CS_API int   cs_get_uservalue(cs_State *C, int index, int n); 

/* -----------------------------------------------------------------------
** Set functions (stack -> CScript)
** ----------------------------------------------------------------------- */
CS_API void  cs_set_global(cs_State *C, const char *name); 
CS_API void  cs_set(cs_State *C, int index); 
CS_API void  cs_set_raw(cs_State *C, int index); 
CS_API void  cs_set_index(cs_State *C, int index, cs_Integer i);
CS_API void  cs_set_field(cs_State *C, int index); 
CS_API void  cs_set_fieldstr(cs_State *C, int index, const char *field); 
CS_API void  cs_set_fieldptr(cs_State *C, int index, const void *field); 
CS_API void  cs_set_fieldint(cs_State *C, int index, cs_Integer field); 
CS_API void  cs_set_fieldflt(cs_State *C, int index, cs_Number field); 
CS_API void  cs_set_uservmt(cs_State *C, int index, const cs_VMT *vmt); 
CS_API int   cs_set_uservalue(cs_State *C, int index, int n); 
CS_API void  cs_set_usermm(cs_State *C, int index, cs_MM mm); 

/* -----------------------------------------------------------------------
** Error reporting
** ----------------------------------------------------------------------- */
/* thread status codes */
#define CS_OK                   0  /* ok */
#define CS_ERRRUNTIME           1  /* runtime error */
#define CS_ERRSYNTAX            3  /* syntax error (compiler) */
#define CS_ERRMEM               4  /* memory related error (oom) */
#define CS_ERRERROR             5  /* error while handling error */

CS_API int cs_status(cs_State *C); 
CS_API int cs_error(cs_State *C); 

/* -----------------------------------------------------------------------
** Call/Load CScript code
** ----------------------------------------------------------------------- */
CS_API void cs_call(cs_State *C, int nargs, int nresults); 
CS_API int  cs_pcall(cs_State *C, int nargs, int nresults, int msgh); 
CS_API int  cs_load(cs_State *C, cs_Reader reader, void *userdata,
                    const char *chunkname); 

/* -----------------------------------------------------------------------
** Garbage collector
** ----------------------------------------------------------------------- */
/* GC options */
#define CS_GCSTOP               0 /* stop GC */
#define CS_GCRESTART            1 /* restart GC (start if stopped) */
#define CS_GCCOLLECT            2 /* perform full GC cycle */
#define CS_GCCOUNT              3 /* get number of (bytes_allocated/1024) */
#define CS_GCCOUNTBYTES         4 /* get remainder of (bytes_allocated/1024) */
#define CS_GCSTEP               5 /* perform single GC step and or set gcdebt */
#define CS_GCISRUNNING          6 /* test whether GC is running */
#define CS_GCINC                7 /* set GC in incremental mode */

CS_API int cs_gc(cs_State *C, int what, ...); 

/* -----------------------------------------------------------------------
** Warning-related functions
** ----------------------------------------------------------------------- */
CS_API void cs_setwarnf(cs_State *C, cs_WarnFunction fwarn, void *ud); 
CS_API void cs_warning(cs_State *C, const char *msg, int cont); 

/* -----------------------------------------------------------------------
** Miscellaneous functions/macros
** ----------------------------------------------------------------------- */
CS_API int              cs_hasvmt(cs_State *C, int index); 
CS_API int              cs_hasmetamethod(cs_State *C, int index, cs_MM mm); 
CS_API cs_Unsigned      cs_len(cs_State *C, int index); 
CS_API int              cs_next(cs_State *C, int index); 
CS_API void             cs_concat(cs_State *C, int n); 
CS_API size_t           cs_stringtonumber(cs_State *C, const char *s, int *f); 
CS_API cs_Alloc         cs_getallocf(cs_State *C, void **ud); 
CS_API void             cs_setallocf(cs_State *C, cs_Alloc falloc, void *ud); 
CS_API void             cs_toclose(cs_State *C, int index); 
CS_API void             cs_closeslot(cs_State *C, int index); 
CS_API int              cs_getfreereg(cs_State *C);

#define cs_getextraspace(C)         ((void *)((char *)(C) - CS_EXTRASPACE))

#define cs_nvalues(C)               (cs_gettop(C) + 1)

#define cs_to_number(C,i)           cs_to_numberx(C,(i),NULL)
#define cs_to_integer(C,i)          cs_to_integerx(C,(i),NULL)

#define cs_pop(C,n)                 cs_setntop(C, -(n)-1)

#define cs_push_cfunction(C,f)      cs_push_cclosure(C,f,0)

#define cs_register(C,n,f)  (cs_push_cfunction(C,(f)), cs_set_global(C,(n)))

#define cs_is_function(C, n)        (cs_type(C, (n)) == CS_TFUNCTION)
#define cs_is_array(C, n)           (cs_type(C, (n)) == CS_TARRAY)
#define cs_is_hashtable(C, n)       (cs_type(C, (n)) == CS_TTABLE)
#define cs_is_class(C, n)           (cs_type(C, (n)) == CS_TCLASS)
#define cs_is_instance(C, n)        (cs_type(C, (n)) == CS_TINSTANCE)
#define cs_is_lightuserdata(C, n)   (cs_type(C, (n)) == CS_TLUDATA)
#define cs_is_nil(C, n)             (cs_type(C, (n)) == CS_TNIL)
#define cs_is_bool(C, n)            (cs_type(C, (n)) == CS_TBOOL)
#define cs_is_thread(C, n)          (cs_type(C, (n)) == CS_TTHREAD)
#define cs_is_none(C, n)            (cs_type(C, (n)) == CS_TNONE)
#define cs_is_noneornil(C, n)       (cs_type(C, (n)) <= 0)

#define cs_push_literal(C, s)       cs_push_string(C, "" s)

#define cs_push_globaltable(C) \
        ((void)cs_get_index(C, CS_REGISTRYINDEX, CS_RINDEX_GLOBALS))

#define cs_to_string(C, i)          cs_to_lstring(C, i, NULL)

#define cs_insert(C,index)          cs_rotate(C, (index), 1)

#define cs_remove(C,index)          (cs_rotate(C, (index), -1), cs_pop(C, 1))

#define cs_replace(C,index)         (cs_copy(C, -1, (index)), cs_pop(C, 1))

/* -----------------------------------------------------------------------
** Debug API
** ----------------------------------------------------------------------- */
CS_API int cs_getstack(cs_State *C, int level, cs_Debug *ar); 
CS_API int cs_getinfo(cs_State *C, const char *what, cs_Debug *ar); 

CS_API const char *cs_getlocal(cs_State *C, const cs_Debug *ar, int n); 
CS_API const char *cs_setlocal (cs_State *C, const cs_Debug *ar, int n); 

CS_API const char *cs_getupvalue(cs_State *C, int index, int n); 
CS_API const char *cs_setupvalue(cs_State *C, int index, int n); 

struct cs_Debug {
    /* (>) pop the function on top of the stack and load it into 'cf' */
    const char *name;       /* (n) */
    const char *namewhat;   /* (n) 'upvalue', 'global', 'local', 'field', 'method' */
    const char *what;       /* (s) */
    const char *source;     /* (s) */
    size_t srclen;          /* (s) */
    int currline;           /* (l) */
    int defline;            /* (s) */
    int lastdefline;        /* (s) */
    int nupvals;            /* (u) */
    int nparams;            /* (u) */
    char isvararg;          /* (u) */
    char shortsrc[CS_MAXSRC]; /* (s) */
    /* (f) pushes onto stack the function that is running at the given level */
    /* private */
    struct CallFrame *cf;
};




/*----------------------------------,
 | Big Thank You to Lua Developers! |
 \________________________________*/
/* -----------------------------------------------------------------------
** Copyright (C) 1994-2020 Lua.org, PUC-Rio.
** Copyright (C) 2023-2024 Jure Bagić
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
** ----------------------------------------------------------------------- */

#endif
