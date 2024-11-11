/*
** cobject.h
** Types definitions for CScript objects
** See Copyright Notice in cscript.h
*/

#ifndef CROBJECT_H
#define CROBJECT_H


#include "cscript.h"
#include "climits.h"


/*
 * Additional types that are used only internally
 * or as markers.
 */
#define CS_TUVALUE      CS_NUM_TYPES        /* upvalue */
#define CS_THTABLE      (CS_NUM_TYPES + 1)  /* hashtable */
#define CS_TDEADKEY     (CS_NUM_TYPES + 2)  /* mark for dead htable keys */


/* 
** Number of all types ('CS_T*') (including 'CS_TNONE' but excluding DEADKEY).
*/
#define CSI_TOTALTYPES      (CS_THTABLE + 2)


CSI_DEC(const char *const csO_typenames[CSI_TOTALTYPES]);

#define typename(t)     csO_typenames[(t) + 1]


/*
 * Tagged value types.
 * Bits 0-3 are for value types (CS_T*).
 * Bits 4-6 are for variant types (CS_V*).
 * Bit 7 for collectable object tag.
 */

/* set variant bytes for type 't' */
#define makevariant(t, v)       ((t) | ((v) << 4))


/* Cript values */
typedef union Value {
    struct GCObject *obj; /* collectable value */
    void *p; /* light userdata */
    int b; /* boolean */
    cs_Integer i; /* integer */
    cs_Number n; /* float */
    cs_CFunction cfn; /* C function */
} Value;


/* 'TValue' fields, defined for reuse and alignment purposes */
#define TValueFields    Value val; cs_ubyte tt; cs_ubyte mod


/*
 * 'Value' with type and modifiers.
 * 'mod' might be unused but this memory would
 * be padded by any sane compiler anyway.
 */
typedef struct TValue {
    TValueFields;
} TValue;



/* get raw union values */
#define rawbval(v)      ((v).b)
#define rawival(v)      ((v).i)
#define rawfval(v)      ((v).n)
#define rawpval(v)      ((v).p)
#define rawcfval(v)     ((v).cfn)
#define rawgcoval(v)    ((v).obj)


/* 'tt' */
#define rawtt(o)            ((o)->tt)


/* tag with no variant bits (0-3) */
#define novariant(t)        ((t) & 0x0F)


/* type tag of TValue; tag bits (0-3) + variant bits (4-6) */
#define withvariant(t)      ((t) & 0x3F)
#define ttypetag(o)         withvariant((o)->tt)


/* type of a TValue */
#define ttype(o)            novariant((o)->tt)


/* Macros to test type */
#define checktag(o,t)       (rawtt(o) == (t))
#define checktype(o,t)      (ttype(o) == (t))


/* 'mod' */
#define MODnone         0 /* no modifiers */
#define MODconst        1 /* value is 'const' */


/* Macros to test modifiers */
#define ismod(o,m)      testbit((o)->mod, (m))
#define isconst(o)      ismod(o, MODconst)



/* Macros for internal tests */


/* collectable object has the same tag as the original value */
#define righttt(obj)		(ttypetag(obj) == gcoval(obj)->tt_)


/*
** Any value being manipulated by the program either is non
** collectable, or the collectable object has the right tag
** and it is not dead. The option 'ts == NULL' allows other
** macros using this one to be used where ts is not available.
*/
#define checkliveness(ts,obj) \
    ((void)ts, cs_assert(!iscollectable(obj) || \
        (righttt(obj) && (ts == NULL || !isdead(G_(ts), gcoval(obj))))))



/* Macros to set values */

/* set object type tag */
#define settt(o,t)          (rawtt(o)=(t))

/* set object 'o1' to 'o2' */
#define setobj(ts,o1,o2) \
    { TValue *o1_=(o1); const TValue *o2_=(o2); \
      o1_->val = o2_->val; o1_->tt = o2_->tt; o1_->mod = o2_->mod; \
      checkliveness(ts,o1_); }

/* from stack to stack */
#define setobjs2s(ts,o1,o2)     setobj(ts,s2v(o1),s2v(o2))
/* set object to stack */
#define setobj2s(ts,o1,o2)      setobj(ts,s2v(o1),o2)


/*
** Entries in CScript stack.
** 'tbc' list contains 'delta' field which represents offset from the current
** stack value to the next value on the stack that needs to-be-closed.
** 'delta' being 0 indicates that the distance value doesn't fit in 'delta'
** and then it is assumed that the actual value is MAXDELTA. 
** This way we can represent larger distances without using larger data type.
** Note: On 8-byte alignment 'SValue' should be 16 
** bytes, while on 4-byte alignement 8 bytes.
*/
typedef union {
    TValue val_;
    struct {
        TValueFields;
        unsigned short delta;
    } tbc;
} SValue;


/* convert 'SValue' to a 'TValue' */
#define s2v(s)      (&(s)->val_)


/* pointer to the value on the stack */
typedef SValue *SPtr;


/*
 * Value that acts as index into the stack.
 * Before reallocation occurs 'offset' is filled
 * accordingly in case 'p' becomes invalid,
 * and then after reallocation 'p' is restored.
 */
typedef struct {
    SPtr p; /* pointer to the value on the stack */
    ptrdiff_t offset; /* used when stack is being reallocated */
} SIndex;



/* -------------------------------------------------------------------------
 * Boolean
 * ------------------------------------------------------------------------- */

#define CS_VFALSE       makevariant(CS_TBOOL, 0)
#define CS_VTRUE        makevariant(CS_TBOOL, 1)

#define bval(o)         rawbval((o)->val)

#define setbfval(o)     settt(o, CS_VFALSE)
#define setbtval(o)     settt(o, CS_VTRUE)

#define ttisbool(o)         checktype(o, CS_TBOOL)
#define ttistrue(o)         checktag(o, CS_VTRUE)
#define ttisfalse(o)        checktag(o, CS_VFALSE)

#define csi_isfalse(o)      (ttisfalse(o) || ttisnil(o))



/* -------------------------------------------------------------------------
 * Numbers
 * ------------------------------------------------------------------------- */

#define CS_VNUMINT      makevariant(CS_TNUMBER, 0)
#define CS_VNUMFLT      makevariant(CS_TNUMBER, 1)

#define ival(o)         rawival((o)->val)
#define fval(o)         rawfval((o)->val)
#define nval(o)         (checktag(o,CS_VNUMINT) ? cast_num(ival(o)) : fval(o))

#define setival(o,i) \
    { TValue *o_=(o); ival(o_)=(i); settt(o_, CS_VNUMINT); }

#define setfval(o,f) \
    { TValue *o_=(o); fval(o_)=(f); settt(o_, CS_VNUMFLT); }

#define ttisflt(o)      checktag(o, CS_VNUMFLT)
#define ttisint(o)      checktag(o, CS_VNUMINT)
#define ttisnum(o)      checktype(o, CS_TNUMBER)



/* -------------------------------------------------------------------------
 * Light userdata
 * ------------------------------------------------------------------------- */

#define CS_VLUDATA      makevariant(CS_TLUDATA, 0)

#define pval(o)         rawpval((o)->val)

#define setpval(o,p) \
    { TValue *o_=(o); pval(o_)=(p); settt(o_, CS_VLUDATA); }

#define ttislud(o)      checktag(o, CS_VLUDATA)



/* -------------------------------------------------------------------------
 * C function
 * ------------------------------------------------------------------------- */

#define CS_VCFUNCTION       makevariant(CS_TFUNCTION, 0)

#define cfval(o)        rawcfval((o)->val)

#define setcfval(o,cf) \
    { TValue *o_=(o); cfval(o_)=(cf); settt(o_, ctb(CS_VCFUNCTION)); }

#define ttiscfn(o)      checktag(o, CS_VCFUNCTION)


/* -------------------------------------------------------------------------
 * Nil
 * ------------------------------------------------------------------------- */

/* standard nil */
#define CS_VNIL         makevariant(CS_TNIL, 0)

/* empty slot */
#define CS_VEMPTY       makevariant(CS_TNIL, 1)

/* value returned for a key not found in a hashtable */
#define CS_VABSTKEY     makevariant(CS_TNIL, 2)

#define setnilval(o)    settt(o, CS_VNIL)
#define setemptyval(o)  settt(o, CS_VEMPTY)

#define ttisnil(o)      checktype((o), CS_TNIL)
#define isempty(o)      ttisnil(o)

#define isabstkey(v)    checktag((v), CS_VABSTKEY)

#define ABSTKEYCONSTANT     {NULL}, CS_VABSTKEY, 0



/* -------------------------------------------------------------------------
 * GCObject
 * ------------------------------------------------------------------------- */

/* common header for objects */
#define ObjectHeader    struct GCObject* next; cs_ubyte tt_; cs_ubyte mark


/* common type for collectable objects */
typedef struct GCObject {
    ObjectHeader;
} GCObject;


/* bit for collectable types */
#define BIT_COLLECTABLE      (1 << 7)

/* check if value is collectable */
#define iscollectable(o)    (rawtt(o) & BIT_COLLECTABLE)

/* mark type-tag as collectable */
#define ctb(tt)     ((tt) | BIT_COLLECTABLE)

/* get 'GCObject*' from 'TValue' */
#define gcoval(o)    rawgcoval((o)->val)


#define setgcoval(ts,o,x) \
    { TValue *o_=(o); GCObject *x_ = (x); \
      gcoval(o_)=(x_); settt(o_, ctb(x_->tt_)); }


#define gcomark_(o)   ((o)->mark)
#define gconext_(o)   ((o)->next)

#define gcomark(v)    (gcomark_(gcoval(v)))
#define gconext(v)    (gconext_(gcoval(v)))


/* set generic GC object value */
#define setgcotval(ts,obj,x,t) \
    { TValue *obj_=(obj); t *x_=(x); \
      gcoval(obj_) = obj2gco(x_); settt(obj_, ctb(x_->tt_)); \
      checkliveness(ts,obj_); }



/* -------------------------------------------------------------------------
 * Thread (cs_State)
 * ------------------------------------------------------------------------- */

#define CS_VTHREAD      makevariant(CS_TTHREAD, 0)

#define ttisthread(v)       checktag(v, ctb(CS_VTHREAD))

#define thval(v)      gco2th(gcoval(v))

#define setthval(ts,obj,th)     setgcotval(ts,obj,th,cs_State)

#define setsv2th(ts,sv,th)      setthval(ts,s2v(sv),th)



/* -------------------------------------------------------------------------
 * HTable (hashtable)
 * ------------------------------------------------------------------------- */

#define CS_VHTABLE      makevariant(CS_THTABLE, 0)

#define ttishtab(v)     checktag((v), ctb(CS_VHTABLE))

#define htval(v)        gco2ht(gcoval(v))

#define sethtval(ts,v,ht)       setgcotval(ts,v,ht,HTable)
#define setht2s(ts,sv,ht)       sethtval(ts,s2v(sv),ht)

/*
** Ordering of fields might seem weird but
** this is to ensure optimal alignment.
*/
typedef union Node {
    struct NodeKey {
        TValueFields; /* fields for value */
        cs_ubyte key_tt; /* key type tag */
        int next; /* offset for next node */
        Value key_val; /* key value */
    } s;
    TValue i_val; /* direct node value access as a proper 'TValue' */
} Node;


/* copy a value into a key */
#define setnodekey(ts,n,obj) \
    { Node *n_=(n); const TValue *obj_=(obj); \
      n_->s.key_val = obj_->val; n_->s.key_tt = obj_->tt; \
      checkliveness(ts,obj_); }


/* copy a value from a key */
#define getnodekey(ts,obj,n) \
    { TValue *obj_=(obj); const Node *n_=(n); \
      obj_->val = n_->s.key_val; obj_->tt = n_->s.key_tt; \
      checkliveness(ts,obj_); }



/* Hash Table */
typedef struct HTable {
    ObjectHeader; /* internal only object */
    cs_ubyte size; /* 2^size */
    Node *node; /* memory block */
    Node *lastfree; /* any free position is before this position */
    GCObject *gclist;
} HTable;


#define keytt(n)                ((n)->s.key_tt)
#define keyval(n)               ((n)->s.key_val)

#define keyival(n)              rawival(keyval(n))
#define keyfval(n)              rawfval(keyval(n))
#define keypval(n)              rawpval(keyval(n))
#define keycfval(n)             rawcfval(keyval(n))
#define keygcoval(n)            rawgcoval(keyval(n))
#define keystrval(n)            gco2str(rawgcoval(keyval(n)))

#define keyisobj(n)             (keytt(n) & BIT_COLLECTABLE)
#define keyisnil(n)	        (keytt(n) == CS_TNIL)

#define setnilkey(n)            (keytt(n) = CS_TNIL)

#define keyisinteger(n)         (keytt(n) == CS_VNUMINT)

#define keyiscollectable(n)     (keytt(n) & BIT_COLLECTABLE)

/* unused in 1.0.0 */
#define setdeadkey(node)    (keytt(node) = CS_TDEADKEY)
#define keyisdead(n)	    (keytt(n) == CS_TDEADKEY)



/* -------------------------------------------------------------------------
 * Array
 * -------------------------------------------------------------------------- */

#define CS_VARRAY       makevariant(CS_TARRAY, 0)

#define ttisarr(v)      checktag((v), ctb(CS_VARRAY))

#define arrval(v)       gco2arr(gcoval(v))

#define setarrval(ts,v,arr)     setgcotval(ts,v,arr,Array)
#define setarr2s(ts,sv,arr)     setarrval(ts,s2v(sv),arr)

typedef struct Array {
    ObjectHeader;
    GCObject *gclist;
    TValue *b; /* memory block */
    uint n; /* number of elements in 'b' */
    uint sz; /* size of 'b' */
} Array;



/* -------------------------------------------------------------------------
 * OString
 * -------------------------------------------------------------------------- */

#define CS_VSTRING      makevariant(CS_TSTRING, 0)

#define ttisstr(o)      checktag(o, ctb(CS_VSTRING))
#define strval(o)       gco2str(gcoval(o))

#define cstrval(o)      (strval(o)->bytes)
#define lenstr(o)       (strval(o)->len)

#define getstrbytes(s)      ((s)->bytes)
#define getstrlen(s)        ((s)->len)

#define setstrval(ts,obj,s)         setgcotval(ts,obj,s,OString)
#define setstrval2s(ts,sobj,s)      setstrval(ts,s2v(sobj),s)

/* string 'bits' */
#define STRHASHBIT      0 /* string has hash */
#define STRKWBIT        1 /* keyword string */
#define STRVMTBIT       2 /* string is a name of method in VMT */

typedef struct OString {
    ObjectHeader;
    cs_ubyte extra; /* extra information (for bits) */
    cs_ubyte bits; /* context bits */
    size_t len; /* excluding null terminator */
    uint hash;
    char bytes[];
} OString;



/* -------------------------------------------------------------------------
 * UpVal
 * -------------------------------------------------------------------------- */

#define CS_VUVALUE      makevariant(CS_TUVALUE, 0)

#define ttisuval(o)     checktag(o, ctb(CS_VUVALUE))
#define upval(o)        gco2uv(gcoval(o))

#define setv2uv(ts,obj,uv)      setgcotval(ts,obj,uv,UpVal)
#define setsv2uv(ts,sobj,uv)    setv2uv(ts,s2v(sobj),uv)

#define uvisopen(uv)    ((uv)->v.p != &(uv)->u.value)

typedef struct UpVal {
    ObjectHeader;
    union {
        TValue *p; /* stack or 'closed' */
        ptrdiff_t offset; /* when reallocating stack */
    } v;
    union {
        struct {
            /* This is still a singly linked list, the role of '**prev'
             * is to easily update the previous 'UpVal' 'next' pointer
             * when unlinking the open upvalue; meaning that 'prev' points
             * not to the previous 'UpVal' but to the previous 'UpVal' 'next'
             * field. This avoids branching as much as possible when
             * unlinking the upvalue. */
            struct UpVal *next;
            struct UpVal **prev;
        } open;
        TValue value; /* value stored here when closed */
    } u;
} UpVal;



/* -------------------------------------------------------------------------
 * Function
 * -------------------------------------------------------------------------- */

#define CS_VFUNCTION    makevariant(CS_TFUNCTION, 0)

#define ttisfunction(o)     checktype((o), CS_TFUNCTION)
#define ttisfn(o)           checktag((o), ctb(CS_VFUNCTION))

#define fnval(o)        gco2fn(gcoval(o))

#define setfnval(ts,obj,fn)         setgcotval(ts,obj,fn,Function)
#define setfnval2s(ts,sobj,fn)      setfnval(ts,s2v(sobj),fn)


/* private global variable */
typedef union PrivateVar {
    struct {
        TValueFields;
        OString *name;
    } s;
    TValue val;
} PrivateVar;



/* upvalue variable debug information */
typedef struct UpValInfo {
    OString *name;
    int idx; /* index in stack or outer function local var list */
    cs_ubyte onstack; /* is it on stack */
    cs_ubyte mod; /* type of corresponding variable */
} UpValInfo;



/* Automatic (local) variable debug information */
typedef struct LVarInfo {
    OString *name;
    int startpc; /* point where variable is in scope */
    int endpc; /* point where variable is out of scope */
} LVarInfo;



/* line information and associated instruction */
typedef struct LineInfo {
    int pc;
    int line;
} LineInfo;


typedef struct Function {
    ObjectHeader;
    cs_ubyte isvararg;
    GCObject *gclist;
    OString *source; /* source name */
    struct Function **funcs; /* functions defined inside of this function */
    TValue *k; /* constant values */
    PrivateVar *private; /* private variables (static globals) */
    Instruction *code; /* bytecode */
    LineInfo *linfo; /* lines information for instructions */
    LVarInfo *locals; /* debug information for local variables */
    UpValInfo *upvals; /* debug information for upvalues */
    int sizefn; /* size of 'fn' */
    int sizek; /* size of 'constants' */
    int sizeprivate; /* size of 'private' */
    int sizecode; /* size of 'code' */
    int sizelinfo; /* size of 'linfo' */
    int sizelocals; /* size of 'locals' */
    int sizeupvals; /* size of 'upvals' */
    int maxstack; /* max stack size for this function */
    int arity; /* function argument count */
    int defline; /* function definition line */
    int deflastline; /* function definition end line */
} Function;



/* -------------------------------------------------------------------------
 * Closures
 * -------------------------------------------------------------------------- */

#define CS_VCRCL    makevariant(CS_TFUNCTION, 1) /* 'CrClosure' */
#define CS_VCCL     makevariant(CS_TFUNCTION, 2) /* 'CClosure' */

#define ttiscrcl(o)     checktag((o), ctb(CS_VCRCL))

#define crclval(o)      gco2crcl(gcoval(o))

#define setcrclval(ts,obj,crcl)     setgcotval(ts,obj,crcl,CrClosure)
#define setcrcl2s(ts,sobj,crcl)     setcrclval(ts,s2v(sobj),crcl)


/* common closure header */
#define ClosureHeader   ObjectHeader; int nupvalues; GCObject *gclist


typedef struct CrClosure {
    ClosureHeader;
    Function *fn;
    UpVal *upvals[];
} CrClosure;


/* size of 'CrClosure' */
#define sizeofcrcl(nup) \
    (offsetof(CrClosure, upvals) + ((nup) * sizeof(UpVal)))



#define ttisccl(o)      checktag(o, ctb(CS_VCCL))

#define cclval(o)       gco2ccl(gcoval(o))

#define setcclval(ts,obj,ccl)   setgcotval(ts,obj,ccl,CClosure)
#define setccl2s(ts,sobj,ccl)   setcclval(ts,s2v(sobj),ccl)

#define CScriptclosure(cl)      ((cl) != NULL && (cl)->crc.tt_ == CS_VCRCL)


typedef struct {
    ClosureHeader;
    cs_CFunction fn;
    TValue upvals[];
} CClosure;


#define sizeofccl(nup) \
    (offsetof(CClosure, upvals) + ((nup) * sizeof(TValue)))



#define ttiscl(v)   (ttisccl(v) || ttiscrcl(v))

#define clval(v)    gco2cl(gcoval(v))

#define setclval(ts,obj,cl)     setgcotval(ts,obj,cl,Closure)
#define setcl2s(ts,sobj,cl)     setclval(ts,s2v(sobj),cl)

typedef union Closure {
    CClosure cc;
    CrClosure crc;
} Closure;



/* --------------------------------------------------------------------------
 * OClass
 * -------------------------------------------------------------------------- */

/* number of elements in VMT */
#define SIZEVMT     (sizeof(TValue)*CS_NUM_MM)


#define CS_VCLASS       makevariant(CS_TCLASS, 0)

#define ttiscls(o)      checktag(o, ctb(CS_VCLASS))

#define clsval(o)       (cs_assert(ttiscls(o)), gco2cls(gcoval(o)))

#define setclsval(ts,obj,cls)   setgcotval(ts,obj,cls,OClass)
#define setcls2s(ts,sv,cls)     setclsval(ts,s2v(sv),cls)

typedef struct OClass {
    ObjectHeader;
    TValue *vmt;
    HTable *methods;
    GCObject *gclist;
} OClass;



/* --------------------------------------------------------------------------
 *  Instance
 * -------------------------------------------------------------------------- */

#define CS_VINSTANCE    makevariant(CS_TINSTANCE, 0)

#define ttisins(o)      checktag(o, ctb(CS_VINSTANCE))

#define insval(o)       (cs_assert(ttisins(o)), gco2ins(gcoval(o)))

#define setinsval(ts,obj,ins)       setgcotval(ts,obj,ins,Instance)
#define setins2s(ts,sobj,ins)       setinsval(ts,s2v(sobj),ins)

typedef struct Instance {
    ObjectHeader;
    OClass *oclass; /* pointer to class */
    HTable *fields;
} Instance;



/* --------------------------------------------------------------------------
 *  IMethod (method binded to instance)
 * --------------------------------------------------------------------------- */

#define CS_VMETHOD      makevariant(3, CS_TFUNCTION)

#define ttisim(o)       checktag(o, ctb(CS_VMETHOD))

#define imval(o)        gco2im(gcoval(o))

#define setimval(ts,obj,im)     setgcotval(ts,obj,im,IMethod)
#define setim2s(ts,sobj,im)     setimval(ts,s2v(sobj),im)

typedef struct IMethod {
    ObjectHeader;
    Instance *receiver;
    TValue method;
} IMethod;



/* --------------------------------------------------------------------------
 *  UserData
 * -------------------------------------------------------------------------- */

#define CS_VUDATA       makevariant(CS_TUDATA, 0)

#define ttisud(o)       checktag(o, ctb(CS_VUDATA))

#define udval(o)        gco2ud(gcoval(o))

#define setudval(ts,obj,ud)     setgcotval(ts,obj,ud,UserData)
#define setud2s(ts,sobj,ud)     setudval(ts,s2v(sobj),ud)

typedef struct UserData {
    ObjectHeader;
    int nuv; /* number of 'uservalues' */
    TValue *vmt;
    size_t size; /* size of 'UserData' memory in bytes */
    GCObject *gclist;
    TValue uv[]; /* user values */
    /* 'UserData' memory starts here; after 'uv' elements */
} UserData;


/*
** 'UserData' without user values, meaning 'uv' is empty
** ('nuv' == 0). This is used when allocating 'UserData'
** to properly calculate offset of user memory because
** 'uv' is a flexible array member.
** Internally Cript only uses 'UserData' to access
** fields and it takes care to avoid using 'uv' and
** 'gclist' fields when 'nuv' is 0.
** Additionally GC marks 'UserData' black in case 'nuv'
** is 0, to avoid using the 'gclist' field.
*/
typedef struct EmptyUserData {
    ObjectHeader;
    int nuv; /* number of 'uservalues' */
    TValue *vmt;
    size_t size; /* size of 'UserData' memory in bytes */
    union {CSI_MAXALIGN;} usermem;
    /* 'UserData' memory starts here */
} EmptyUserData;


/* offset in 'UserData' where user memory begins */
#define udmemoffset(nuv) \
    ((nuv) == 0 \
        ? offsetof(EmptyUserData, usermem) \
        : offsetof(UserData, uv) + ((nuv) * sizeof(TValue)))

/* get the address of the memory block inside 'UserData' */
#define getudmem(u)	(cast_charp(u) + udmemoffset((u)->nuv))

/* size of 'UserData' */
#define sizeofud(nuv, size)     (udmemoffset(nuv) + (size))



/*
 * Conversion modes when converting 'cs_Integer'
 * into 'cs_Number'.
 */
typedef enum N2IMode {
    N2IFLOOR,
    N2ICEIL,
    N2IEXACT,
} N2IMode;


/* convert value to 'cs_Integer' */
#define tointeger(v,i) \
    (csi_likely(ttisint(v)) \
     ? (*(i) = ival(v), 1) \
     : csO_tointeger(v, i, N2IFLOOR))


/* convert value to 'cs_Number' */
#define tonumber(v,n) \
    (csi_likely(ttisflt(v)) \
     ? (*(n) = fval(v), 1) \
     : (ttisint(v)) ? (*(n) = ival(v), 1) : 0)


/* same as right shift but indicate left by making 'y' negative */
#define csO_shiftl(x,y)    csO_shiftr(x, -(y))


CSI_FUNC int csO_ceillog2(uint x);
CSI_FUNC int csO_n2i(cs_Number n, cs_Integer *i, N2IMode mode);
CSI_FUNC int csO_tointeger(const TValue *v, cs_Integer *i, int mode);
CSI_FUNC cs_Integer csO_shiftr(cs_Integer x, cs_Integer y);
CSI_FUNC int csO_arithmraw(cs_State *ts, const TValue *a, const TValue *b,
                           TValue *res, int op);

#endif
