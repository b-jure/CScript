/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#ifndef CROBJECT_H
#define CROBJECT_H


#include "crmem.h"
#include "cript.h"
#include "crvalue.h"



/* common header for objects */
#define ObjectHeader    struct GCObject* next; cr_ubyte tt_; cr_ubyte mark


/* size of 'ObjectHeader' fields */
#define OBJHEADERSIZE   (sizeof(GCObject*) + 2)


/* common type for collectable objects */
typedef struct GCObject {
    ObjectHeader;
} GCObject;


/* value type bit */
#define OBJECTTAG   (1 << 7)


#define rawtt_(o)   ((o)->tt_)
#define tt_(o)      novariant(rawtt_(o))
#define ott_(o)     withvariant(rawtt_(o))
#define omark_(o)   ((o)->mark)
#define onext_(o)   ((o)->next)

#define ott(v)      (ott_(oval(v)))
#define omark(v)    (omark_(oval(v)))
#define onext(v)    (onext_(oval(v)))

#define isott(v,t)  (ott(v) == (t))


/* set value to a generic object */
#define setv2gco(ts,v,o) \
    { TValue *v_=(v); GCObject *o_=o; \
      oval(v_) = obj2gco(o_); rawtt(v_) = OBJECTTAG; }


/* set value to GC object */
#define setv2o(ts,v,o,t) \
    { TValue *v_=(v); t *o_=o; oval(v_) = obj2gco(o_); \
      rawtt(v_) = OBJECTTAG; }


/* set stack value to GC object */
#define setsv2o(ts,sv,o,t)  setv2o(ts,s2v(sv),o,t)



/* -------------------------------------------------------------------------
 * Thread (cr_State)
 * ------------------------------------------------------------------------- */

#define CR_VTHREAD      makevariant(CR_TTHREAD, 0)

#define ttisthread(v)       isott(v, CR_VTHREAD)

#define threadvalue(v)      gco2th(oval(v))

/* set value to thread */
#define setv2th(ts,v,th)    setv2o(ts, v, th, cr_State)

/* set stack value to string */
#define setsv2th(ts,sv,th)  setv2s(ts, s2v(sv), th)



/* -------------------------------------------------------------------------
 * HTable (hashtable)
 * ------------------------------------------------------------------------- */

#define CR_VHTABLE  makevariant(CR_THTABLE, 1)

#define ttishtab(v) isott((v), CR_VHTABLE)
#define htabval(v)  gco2ht(oval(v))


/* set value to hashtable */
#define setv2ht(ts,v,ht)    setv2o(ts,v,ht,HTable)

/* set stack value to hashtable */
#define setsv2ht(ts,sv,ht)  setv2ht(ts,s2v(sv),ht)


#define keyval(n)   ((n)->s.keyval)
#define keybval(n)  rawbval(keyval(n))
#define keyival(n)  rawival(keyval(n))
#define keyfval(n)  rawfval(keyval(n))
#define keypval(n)  rawpval(keyval(n))
#define keycfval(n) rawcfval(keyval(n))
#define keyoval(n)  rawoval(keyval(n))
#define keystrval(n)    gco2str(rawoval(keyval(n)))

#define keytt(n)    ((n)->s.ttk)

#define keyisobj(n) (keytt(n) == OBJECTTAG)

#define keyobj(n)   rawoval(keyval(n))
#define keyobjN(n)  (keyisobj(n) ? keyobj(n) : NULL)


#define setemptykey(n)      (keytt(n) = CR_VEMPTY)


/* copy values from node 'n' key to 'v' */
#define setnodekey(ts,n,v) \
    { Node *n_ = (n); const TValue *v_ = (v); \
      keytt(n_) = rawtt(v_); keyval(n_) = vval(v_); }


/* copy values from node 'n' key to 'v' */
#define getnodekey(ts,v,n) \
    { TValue *v_ = (v); const Node *n_ = (n); \
      rawtt(v_) = keytt(n_); vmod(v_) = 0; \
      vval(v_) = keyval(n_); }


/*
 * Ordering of fields might seem weird but
 * this is to ensure optimal alignment.
 */
typedef union Node {
    struct {
        TValueFields; /* value fields */
        cr_ubyte ttk; /* type tag for key */
        Value keyval; /* key value */
    } s;
    TValue val;
} Node;



/* hash table */
typedef struct HTable {
    ObjectHeader; /* internal only object */
    cr_ubyte size; /* 2^size */
    cr_ubyte isweak; /* true if holds weak keys/values */
    int left; /* free slots before array needs to grow */
    int nnodes; /* number of nodes */
    Node *mem; /* memory block */
    GCObject *gclist;
} HTable;



/* -------------------------------------------------------------------------
 * OString
 * -------------------------------------------------------------------------- */

#define CR_VSTRING  makevariant(CR_TSTRING, 0)

#define ttisstr(v)  isott((v), CR_VSTRING)
#define strval(v)   gco2str(oval(v))
#define cstrval(v)  (strval(v)->bytes)


#define getstrbytes(s)   ((s)->bytes)


/* set value to string */
#define setv2s(ts,v,s)      setv2o(ts,v,s,OString)

/* set stack value to string */
#define setsv2s(ts,sv,s)    setv2s(ts,s2v(sv),s)


/* bits for string 'bits' :) */
#define STRHASHASH      (1<<0) /* string has hash */
#define STRKEYWORD      (1<<1) /* string is keyword */
#define STRVTABMETHOD       (1<<2) /* string is vtable method */


typedef struct OString {
    ObjectHeader;
    cr_ubyte extra; /* extra information */
    cr_ubyte bits; /* useful bits */
    int len; /* excluding null terminator */
    uint hash;
    char bytes[];
} OString;



/* -------------------------------------------------------------------------
 * UpVal
 * -------------------------------------------------------------------------- */

#define CR_VUVALUE  makevariant(CR_TUVALUE, 0)

#define ttisuval(o) isott((v), CR_VUVALUE)
#define uvval(v)    gco2uv(oval(v))


/* set value to upvalue */
#define setv2uv(ts,v,uv)    setv2o(ts,v,uv,UpVal)

/* set stack value to upvalue */
#define setsv2uv(ts,sv,uv)  setv2uv(ts,s2v(sv),uv)


/* check if upvalue is open */
#define uvisopen(uv)    ((uv)->v.location != &(uv)->u.value)


typedef struct UpVal {
    ObjectHeader;
    union {
        TValue *location; /* stack or 'closed' */
        ptrdiff_t offset; /* when reallocating stack */
    } v;
    union {
        struct {
            /* This is still a singly linked list, the role of '**prev'
             * is to easily update the previous 'UpVal' 'next' pointer
             * when unlinking the open upvalue; meaning that 'prev' points
             * not to the previous 'UpVal' but to the previous 'UpVal' 'next'
             * field. This avoids branching as much as possible */
            struct UpVal *next;
            struct UpVal **prev;
        } open;
        TValue value; /* value stored here when closed */
    } u;
} UpVal;



/* -------------------------------------------------------------------------
 * Function
 * -------------------------------------------------------------------------- */

#define CR_VFUNCTION    makevariant(CR_TFUNCTION, 0)

#define ttisfn(v)   isott((v), CR_VFUNCTION)
#define fnval(v)    gco2fn(oval(v))


/* set value to function */
#define setv2fn(ts,v,fn)    setv2o(ts,v,fn,Function)

/* set stack value to upvalue */
#define setsv2fn(ts,sv,fn)  setv2fn(ts,s2v(sv),fn)



/* upvalue variable debug information */
typedef struct UpValInfo {
    OString *name;
    int idx; /* index in stack or outer function local var list */
    cr_ubyte onstack; /* is it on stack */
    cr_ubyte mod; /* type of corresponding variable */
} UpValInfo;



/* Automatic (local) variable debug information */
typedef struct LVarInfo {
    OString *name;
    int alivepc; /* point where variable is in scope */
    int deadpc; /* point where variable is out of scope */
} LVarInfo;



/* line information and associated instruction */
typedef struct LineInfo {
    int pc;
    int line;
} LineInfo;



/* Cript function */
typedef struct Function {
    ObjectHeader;
    cr_ubyte isvararg;
    GCObject *gclist;
    OString *name; /* function name */
    OString *source; /* source name */
    struct Function **fn; /* functions defined inside of this function */
    TValue *constants; /* constant values */
    Instruction *code; /* bytecode */
    LineInfo *linfo; /* lines information for instructions */
    LVarInfo *locals; /* debug information for local variables */
    UpValInfo *upvals; /* debug information for upvalues */
    int nfn; /* number of elements in 'fn' */
    int nconst; /* number of elements in 'constants' */
    int ncode; /* number of elements in 'code' */
    int nlinfo; /* number of elements in 'linfo' */
    int nlocals; /* number of elements in 'locals' */
    int nupvals; /* number of elements in 'upvals' */
    int sizefn; /* size of 'fn' */
    int sizeconst; /* size of 'constants' */
    int sizecode; /* size of 'code' */
    int sizelinfo; /* size of 'linfo' */
    int sizelocals; /* size of 'locals' */
    int sizeupvals; /* size of 'upvals' */
    int maxstack; /* max stack size for this function */
    int arity;
    int defline; /* function definition line */
    int deflastline; /* function definition end line */
} Function;



/* -------------------------------------------------------------------------
 * Closures
 * -------------------------------------------------------------------------- */

#define CR_VCRCL    makevariant(CR_TFUNCTION, 1) /* 'CrClosure' */
#define CR_VCCL     makevariant(CR_TFUNCTION, 2) /* 'CClosure' */


#define ttiscrcl(v)     isott((v), CR_VCRCL)
#define crclval(v)      (clval(v)->crc)

/* set value to cript closure */
#define setv2crcl(ts,v,crcl)        setv2o(ts,v,crcl,CrClosure)

/* set stack value to cript closure */
#define setsv2crcl(ts,sv,crcl)      setv2crcl(ts,s2v(sv),crcl)


/* common closure header */
#define ClosureHeader   ObjectHeader; int nupvalues; GCObject *gclist;


typedef struct CrClosure {
    ClosureHeader;
    Function *fn;
    UpVal *upvalue[];
} CrClosure;


/* size of 'CrClosure' */
#define sizeofcrcl(nup) \
    (offsetof(CrClosure, upvalue) + ((nup) * sizeof(UpVal)))




#define ttisccl(v)      isott((v), CR_VCCL)
#define cclval(v)       (clval(v)->cc)

/* set value to C closure */
#define setv2ccl(ts,v,ccl)  setv2o(ts,v,ccl,CClosure)

/* set stack value to C closure */
#define setsv2ccl(ts,sv,ccl)    setv2ccl(ts,s2v(sv),ccl)

/* 'cl' is not a 'CrClosure' */
#define noCriptclosure(cl)  ((cl) == NULL || rawtt_(&(cl)->cc) != CR_VCRCL)


typedef struct {
    ClosureHeader;
    cr_cfunc fn;
    TValue upvalue[];
} CClosure;


/* size of 'CClosure' */
#define sizeofccl(nup) \
    (offsetof(CClosure, upvalue) + ((nup) * sizeof(TValue)))




/* set value to closure */
#define setv2cl(ts,v,cl)    setv2o(ts,v,cl,Closure)

/* set stack value to closure */
#define setsv2cl(ts,sv,cl)  setv2cl(ts,s2v(sv),cl)

#define ttiscl(v)   (ttisccl(v) || ttiscrcl(v))
#define clval(v)    gco2cl(oval(v))


typedef union Closure {
    CClosure cc;
    CrClosure crc;
} Closure;



/* --------------------------------------------------------------------------
 * OClass
 * -------------------------------------------------------------------------- */

/* virtual method table */
typedef TValue VMT[CR_NUM_META];


#define CR_VCLASS   makevariant(CR_TCLASS, 0)

#define ttiscls(v)  isott((v), CR_VCLASS)
#define clsval(v)   gco2cls(oval(v))


/* set value to class */
#define setv2cls(ts,v,cls)  setv2o(ts,v,cls,OClass)

/* set stack value to class */
#define setsv2cls(ts,sv,cls)    setv2cls(ts,s2v(sv),cls)


typedef struct OClass {
    ObjectHeader;
    OString *name;
    HTable *methods;
    VMT vtable;
    GCObject *gclist;
} OClass;



/* --------------------------------------------------------------------------
 *  Instance
 * -------------------------------------------------------------------------- */

#define CR_VINSTANCE    makevariant(CR_TINSTANCE, 0)

#define ttisins(v)  isott((v), CR_VINSTANCE)
#define insval(v)   gco2ins(oval(v))


/* set value to instance */
#define setv2ins(ts,v,ins)  setv2o(ts,v,ins,Instance)

/* set stack value to instance */
#define setsv2ins(ts,sv,ins)    setv2ins(ts,s2v(sv),ins)


/* 'OClass' instance */
typedef struct Instance {
    ObjectHeader;
    OClass *oclass; /* pointer to class */
    HTable *fields;
} Instance;



/* --------------------------------------------------------------------------
 *  InstanceMethod
 * --------------------------------------------------------------------------- */

#define CR_VMETHOD  makevariant(3, CR_TFUNCTION)

#define ttisim(v)   isott((v), CR_VMETHOD)
#define imval(v)    gco2im(oval(v))


/* set value to instance method */
#define setv2im(ts,v,im)        setv2o(ts,v,im,InstanceMethod)

/* set stack value to instance method */
#define setsv2im(ts,sv,im)      setv2im(ts,s2v(sv),im)



/* method bound to 'receiver' (Instance) */
typedef struct InstanceMethod {
    ObjectHeader;
    Instance *receiver;
    GCObject *method;
} InstanceMethod;



/* --------------------------------------------------------------------------
 *  UserData
 * -------------------------------------------------------------------------- */

#define CR_VUDATA   makevariant(CR_TUDATA, 0)

#define ttisud(v)   isott(v, CR_VUDATA)
#define udval(v)    gco2ud(oval(v))


/* set value to instance method */
#define setv2ud(ts,v,im)        setv2o(ts,v,im,InstanceMethod)

/* set stack value to instance method */
#define setsv2im(ts,sv,im)      setv2im(ts,s2v(sv),im)


/* userdata */
typedef struct UserData {
    ObjectHeader;
    cr_ubyte vmtempty; /* true if 'vtable' is empty */
    int nuv; /* number of 'uservalues' */
    size_t size; /* size of 'UserData' memory in bytes */
    VMT vtable;
    GCObject *gclist;
    TValue uv[]; /* user values */
    /* 'UserData' memory starts here */
} UserData;


/*
 * 'UserData' without user values, meaning 'uv' is empty
 * ('nuv' == 0). This is used when allocating 'UserData'
 * to properly calculate offset of user memory because
 * 'uv' is a flexible array member.
 * Internally Cript only uses 'UserData' to access
 * fields and it takes care to avoid using 'uv' and
 * 'gclist' fields when 'nuv' is 0.
 * Additionally GC marks 'UserData' black in case 'nuv'
 * is 0, to avoid using the 'gclist' field.
 */
typedef struct EmptyUserData {
    ObjectHeader;
    cr_ubyte vmtempty; /* true if 'vtable' is empty */
    int nuv; /* number of 'uservalues' */
    size_t size; /* size of 'UserData' memory in bytes */
    VMT vtable;
    union {CRI_MAXALIGN} usermem;
    /* 'UserData' memory starts here */
} EmptyUserData;


/* offset in 'UserData' where user memory begins */
#define udmemoffset(nuv) \
    ((nuv) == 0 \
        ? offsetof(EmptyUserData, usermem) \
        : offsetof(UserData, uv) + ((nuv) * sizeof(TValue)))


/* size of 'UserData' */
#define sizeofud(nuv, size) (udmemoffset(nuv) + (size))


#endif
