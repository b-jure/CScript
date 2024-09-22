/*
** $Name: crobject.h
** $Description: Type definitions for Cript objects.
** $Copyright: Copyright Notice in cript.h.
*/

#ifndef CROBJECT_H
#define CROBJECT_H


#include "cript.h"
#include "crlimits.h"


/*
 * Additional types that are used only internally
 * or as markers.
 */
#define CR_TUVALUE      CR_NUM_TYPES            /* upvalue */
#define CR_THTABLE      (CR_NUM_TYPES + 1)      /* hashtable */


/* number of all types ('CR_T*') including 'CR_TNONE' */
#define CRI_TOTALTYPES      (CR_THTABLE + 2)


CRI_DEC(const char *const crO_typenames[CRI_TOTALTYPES]);

#define typename(t)     crO_typenames[(t) + 1]


/*
 * Tagged value types.
 * Bits 0-3 are for value types (CR_T*).
 * Bits 4-6 are for variant types (CR_V*).
 * Bit 7 for collectable object tag.
 */

/* set variant bytes for type 't' */
#define makevariant(t, v)       ((t) | ((v) << 4))


/* Cript values */
typedef union Value {
    int b; /* boolean */
    cr_Integer i; /* integer */
    cr_Number n; /* float */
    void *p; /* light userdata */
    cr_CFunction cfn; /* C function */
    struct GCObject *obj; /* collectable value */
} Value;


/* get raw union values */
#define rawbval(o)      ((o).b)
#define rawival(o)      ((o).i)
#define rawfval(o)      ((o).n)
#define rawpval(o)      ((o).p)
#define rawcfval(o)     ((o).cfn)
#define rawgcoval(o)    ((o).obj)


/* 'tt' */
#define rawtt(o)            ((o)->tt)


/* tag with no variant bits (0-3) */
#define novariant(t)        ((t) & 0x0F)


/* type tag of TValue; tag bits (0-3) + variant bits (4-6) */
#define withvariant(t)      ((t) & 0x3F)
#define ttypetag(o)         withvariant(o->tt)


/* type of a TValue */
#define ttype(o)        novariant(o->tt)


/* Macros to test type */
#define checktag(o,t)       (rawtt(o) == (t))
#define checktype(o,t)      (ttype(o) == (t))


/* 'mod' */
#define MODnone         0 /* no modifiers */
#define MODconst        1 /* value is 'const' */


/* Macros to test modifiers */
#define ismod(o,m)      testbit((o)->mod, (m))
#define isconst(o)      ismod(o, MODconst)



/* Set macros */

/* set object type tag */
#define settt(o,t)          (rawtt(o)=(t))

/* set object 'o1' to 'o2' */
#define setobj(ts,o1,o2) \
    { TValue *o1_ = (o1); const TValue *o2_ = (o2); \
      o1_->val = o2_->val; settt(o1_, rawtt(o2_)); o1_->mod = o2_->mod; }

/* from stack to stack */
#define setobjs2s(ts,o1,o2)     setobj(ts,s2v(o1),s2v(o2))
/* set object to stack */
#define setobj2s(ts,o1,o2)      setobj(ts,s2v(o1),o2)



/* 'TValue' fields, defined for reuse and alignment purposes */
#define TValueFields    Value val; cr_ubyte tt; cr_ubyte mod


/*
 * 'Value' with type and modifiers.
 * 'mod' might be unused but this memory would
 * be padded by any sane compiler anyway.
 */
typedef struct TValue {
    TValueFields;
} TValue;



/*
** Stack value.
** It contains 'delta' field which represents
** offset from the current stack value to the
** next value on the stack that needs to-be-closed.
** 'delta' being 0 indicates that the distance value
** doesn't fit in 'delta' and then it is ** assumed that
** the actual value is MAXDELTA. 
** This way we can represent larger distances without using
** larger data type.
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

#define CR_VFALSE       makevariant(CR_TBOOL, 0)
#define CR_VTRUE        makevariant(CR_TBOOL, 1)

#define bval(o)         rawbval((o)->val)

#define setbfval(o)     settt(o, CR_VFALSE)
#define setbtval(o)     settt(o, CR_VTRUE)

#define ttisbool(o)         checktype(o, CR_TBOOL)
#define ttistrue(o)         checktag(o, CR_VTRUE)
#define ttisfalse(o)        checktag(o, CR_VFALSE)

#define cri_isfalse(o)      (ttisfalse(o) || ttisnil(o))



/* -------------------------------------------------------------------------
 * Numbers
 * ------------------------------------------------------------------------- */

#define CR_VNUMINT      makevariant(CR_TNUMBER, 0)
#define CR_VNUMFLT      makevariant(CR_TNUMBER, 1)

#define ival(o)         rawival((o)->val)
#define fval(o)         rawfval((o)->val)
#define nval(o)         (checktag(o,CR_VNUMINT) ? cast_num(ival(o)) : fval(o))

#define setival(o,i) \
    { TValue *o_=(o); ival(o_)=(i); settt(o_, CR_VNUMINT); }

#define setfval(o,f) \
    { TValue *o_=(o); fval(o_)=(f); settt(o_, CR_VNUMFLT); }

#define ttisflt(o)      checktag(o, CR_VNUMFLT)
#define ttisint(o)      checktag(o, CR_VNUMINT)
#define ttisnum(o)      checktype(o, CR_TNUMBER)



/* -------------------------------------------------------------------------
 * Light userdata
 * ------------------------------------------------------------------------- */

#define CR_VLUDATA      makevariant(CR_TLUDATA, 0)

#define pval(o)         rawpval((o)->val)

#define setpval(o,p) \
    { TValue *o_=(o); pval(o_)=(p); settt(o_, CR_VLUDATA); }

#define ttislud(o)      checktag(o, CR_VLUDATA)



/* -------------------------------------------------------------------------
 * C function
 * ------------------------------------------------------------------------- */

#define CR_VCFUNCTION       makevariant(CR_TFUNCTION, 0)

#define cfval(o)        rawcfval((o)->val)

#define setcfval(o,cf) \
    { TValue *o_=(o); cfval(o_)=(cf); settt(o_, CR_VCFUNCTION); }

#define ttiscfn(o)      checktag(o, CR_VCFUNCTION)


/* -------------------------------------------------------------------------
 * Nil
 * ------------------------------------------------------------------------- */

#define CR_VNIL         makevariant(CR_TNIL, 0)
#define CR_VEMPTY       makevariant(CR_TNIL, 1)
#define CR_VTOMB        makevariant(CR_TNIL, 2)

#define setnilval(o)    settt(o, CR_VNIL)
#define setemptyval(o)  settt(o, CR_VEMPTY)

#define ttisnil(o)      checktype((o), CR_VNIL)
#define ttisempty(o)    checktag((o), CR_VEMPTY)



/* -------------------------------------------------------------------------
 * GCObject
 * ------------------------------------------------------------------------- */

/* common header for objects */
#define ObjectHeader    struct GCObject* next; cr_ubyte tt_; cr_ubyte mark


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
    { TValue *obj_=(obj); t *x_=x; \
      gcoval(obj_) = obj2gco(x_); settt(obj_, ctb(x_->tt_)); }


/* set GC object to stack */
#define setsv2gco(ts,sv,o,t)    setgcotval(ts,s2v(sv),o,t)



/* -------------------------------------------------------------------------
 * Thread (cr_State)
 * ------------------------------------------------------------------------- */

#define CR_VTHREAD      makevariant(CR_TTHREAD, 0)

#define ttisthread(v)       checktag(v, ctb(CR_VTHREAD))

#define thval(v)      gco2th(gcoval(v))

#define setthval(ts,obj,th)     setgcotval(ts,obj,th,cr_State)

#define setsv2th(ts,sv,th)      setobj2s(ts,s2v(sv),th)



/* -------------------------------------------------------------------------
 * HTable (hashtable)
 * ------------------------------------------------------------------------- */

#define CR_VHTABLE      makevariant(CR_THTABLE, 0)

#define ttishtab(v)     checktag((v), ctb(CR_VHTABLE))

#define htval(v)        gco2ht(gcoval(v))

#define setv2ht(ts,v,ht)        setgcotval(ts,v,ht,HTable)
#define setsv2ht(ts,sv,ht)      setv2ht(ts,s2v(sv),ht)

#define keyval(n)       ((n)->s.keyval)
#define keyival(n)      rawival(keyval(n))
#define keyfval(n)      rawfval(keyval(n))
#define keypval(n)      rawpval(keyval(n))
#define keycfval(n)     rawcfval(keyval(n))
#define keygcoval(n)    rawgcoval(keyval(n))
#define keystrval(n)    gco2str(rawgcoval(keyval(n)))

#define keytt(n)        ((n)->s.ttk)

#define keyisobj(n)     (keytt(n) & BIT_COLLECTABLE)

#define keyobj(n)       rawgcoval(keyval(n))
#define keyobjN(n)      (keyisobj(n) ? keyobj(n) : NULL)


#define setemptykey(n)      (keytt(n) = CR_VEMPTY)


/* copy a value into a key */
#define setnodekey(ts,n,obj) \
    { Node *n_ = (n); const TValue *obj_=(obj); \
      keyval(n_)=obj_->val; keytt(n_)=obj_->tt; }

/* copy a value from a key */
#define getnodekey(ts,obj,n) \
    { TValue *obj_ = (obj); const Node *n_=(n); \
      obj_->val=keyval(n_); obj_->tt=keytt(n_); obj_->mod=0; }


/*
** Ordering of fields might seem weird but
** this is to ensure optimal alignment.
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

#define CR_VSTRING      makevariant(CR_TSTRING, 0)

#define ttisstr(o)      checktag(o, ctb(CR_VSTRING))
#define strval(o)       gco2str(gcoval(o))
#define cstrval(o)      (strval(o)->bytes)

#define getstrbytes(s)      ((s)->bytes)

#define setstrval(ts,obj,s)     setgcotval(ts,obj,s,OString)
#define setstrval2s(ts,sobj,s)    setstrval(ts,s2v(sobj),s)

/* string 'bits' */
#define STRHASHBIT      0 /* string has hash */
#define STRKWBIT        1 /* keyword string */
#define STRVMTBIT       2 /* string is a name of method in VMT */

typedef struct OString {
    ObjectHeader;
    cr_ubyte extra; /* extra information (for bits) */
    cr_ubyte bits; /* context bits */
    size_t len; /* excluding null terminator */
    uint hash;
    char bytes[];
} OString;



/* -------------------------------------------------------------------------
 * UpVal
 * -------------------------------------------------------------------------- */

#define CR_VUVALUE      makevariant(CR_TUVALUE, 0)

#define ttisuval(o)     testtag(o, ctb(CR_VUVALUE))
#define upval(o)        gco2uv(gcoval(o))

#define setv2uv(ts,obj,uv)      setgcotval(ts,obj,uv,UpVal)
#define setsv2uv(ts,sobj,uv)    setv2uv(ts,s2v(sobj),uv)

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

#define CR_VFUNCTION    makevariant(CR_TFUNCTION, 0)

#define ttisfn(v)       testtag((v), ctb(CR_VFUNCTION))

#define fnval(v)        gco2fn(gcoval(v))

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
    cr_ubyte onstack; /* is it on stack */
    cr_ubyte mod; /* type of corresponding variable */
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
    cr_ubyte isvararg;
    GCObject *gclist;
    OString *source; /* source name */
    struct Function **funcs; /* functions defined inside of this function */
    TValue *k; /* constant values */
    PrivateVar *private;
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

#define CR_VCRCL    makevariant(CR_TFUNCTION, 1) /* 'CrClosure' */
#define CR_VCCL     makevariant(CR_TFUNCTION, 2) /* 'CClosure' */

#define ttiscrcl(o)     checktag((o), ctb(CR_VCRCL))

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



#define ttisccl(o)      checktag(o, ctb(CR_VCCL))

#define cclval(o)       gco2ccl(gcoval(o))

#define setcclval(ts,obj,ccl)   setgcotval(ts,obj,ccl,CClosure)
#define setccl2s(ts,sobj,ccl)   setcclval(ts,s2v(sobj),ccl)

#define noCriptclosure(cl) \
    ((cl) == NULL || (cl)->cc.tt_ != ctb(CR_VCRCL))


typedef struct {
    ClosureHeader;
    cr_CFunction fn;
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
#define SIZEVMT     (sizeof(TValue)*CR_NUM_MM)


#define CR_VCLASS       makevariant(CR_TCLASS, 0)

#define ttiscls(o)      checktag(o, ctb(CR_VCLASS))

#define clsval(o)       gco2cls(gcoval(o))

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

#define CR_VINSTANCE    makevariant(CR_TINSTANCE, 0)

#define ttisins(o)      checktag(o, ctb(CR_VINSTANCE))

#define insval(o)       gco2ins(gcoval(o))

#define setinsval(ts,obj,ins)       setgcotval(ts,obj,ins,Instance)
#define setins2s(ts,sobj,ins)       setinsval(ts,s2v(sobj),ins)

typedef struct Instance {
    ObjectHeader;
    OClass *oclass; /* pointer to class */
    HTable *fields;
} Instance;



/* --------------------------------------------------------------------------
 *  InstanceMethod
 * --------------------------------------------------------------------------- */

#define CR_VMETHOD      makevariant(3, CR_TFUNCTION)

#define ttisim(o)       testtag(o, ctb(CR_VMETHOD))

#define imval(o)        gco2im(gcoval(o))

#define setv2im(ts,obj,im)      setgcotval(ts,obj,im,InstanceMethod)
#define setsv2im(ts,sobj,im)    setv2im(ts,s2v(sobj),im)

typedef struct InstanceMethod {
    ObjectHeader;
    Instance *receiver;
    GCObject *method;
} InstanceMethod;



/* --------------------------------------------------------------------------
 *  UserData
 * -------------------------------------------------------------------------- */

#define CR_VUDATA       makevariant(CR_TUDATA, 0)

#define ttisud(o)       checktag(o, ctb(CR_VUDATA))

#define udval(o)        gco2ud(gcoval(o))

#define setudval(ts,obj,im)     setgcotval(ts,obj,im,InstanceMethod)
#define setud2s(ts,sobj,im)     setudval(ts,s2v(sobj),im)

typedef struct UserData {
    ObjectHeader;
    TValue *vmt;
    int nuv; /* number of 'uservalues' */
    size_t size; /* size of 'UserData' memory in bytes */
    GCObject *gclist;
    TValue uv[]; /* user values */
    /* 'UserData' memory starts here */
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
    cr_ubyte vmtempty; /* true if 'vtable' is empty */
    int nuv; /* number of 'uservalues' */
    size_t size; /* size of 'UserData' memory in bytes */
    TValue *vmt;
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




/*
 * Conversion modes when converting 'cr_Integer'
 * into 'cr_Number'.
 */
typedef enum N2IMode {
    CR_N2IFLOOR,
    CR_N2ICEIL,
    CR_N2IEXACT,
} N2IMode;


/* convert value to 'cr_Integer' */
#define tointeger(v,i) \
    (cr_likely(ttisint(v)) \
     ? (*(i) = ival(v), 1) \
     : crO_tointeger(v, i, CR_N2IFLOOR))


/* convert value to 'cr_Number' */
#define tonumber(v,n) \
    (cr_likely(ttisflt(v)) \
     ? (*(n) = fval(v), 1) \
     : (ttisint(v)) ? (*(n) = ival(v), 1) : 0)


/* same as right shift but indicate left by making 'y' negative */
#define crO_shiftl(x,y)    crO_shiftr(x, -(y))

/* hash primitives */
#define crO_hashint(i)     cast_uint(cri_castS2U(i))
#define crO_hashbool(b)    cast_uint((b) != 0)
#define crO_hashp(p)       pointer2uint(p)


CRI_FUNC uint crO_hashnum(cr_Number n);
CRI_FUNC int crO_ceillog2(uint x);
CRI_FUNC int crO_n2i(cr_Number n, cr_Integer *i, N2IMode mode);
CRI_FUNC int crO_tointeger(const TValue *v, cr_Integer *i, int mode);
CRI_FUNC cr_Integer crO_shiftr(cr_Integer x, cr_Integer y);
CRI_FUNC int crO_arithmraw(cr_State *ts, const TValue *a, const TValue *b,
                           TValue *res, int op);

#endif
