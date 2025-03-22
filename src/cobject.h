/*
** cobject.h
** Types definitions for CScript objects
** See Copyright Notice in cscript.h
*/

#ifndef COBJECT_H
#define COBJECT_H


#include "cscript.h"
#include "climits.h"


/*
 * Additional types that are used only internally
 * or as markers.
 */
#define CS_TUPVALUE     CS_NUM_TYPES        /* upvalue */
#define CS_TPROTO       (CS_NUM_TYPES + 1)  /* function prototype */
#define CS_TDEADKEY     (CS_NUM_TYPES + 2)  /* mark for dead table keys */


/* 
** Number of all types ('CS_T*') (including 'CS_TNONE' but excluding DEADKEY).
*/
#define CSI_TOTALTYPES      (CS_TPROTO + 2)


/*
** Tagged value types.
** Bits 0-3 are for value types (CS_T*).
** Bits 4-6 are for variant types (CS_V*).
** Bit 7 for collectable object tag.
*/

/* set variant bytes for type 't' */
#define makevariant(t, v)       ((t) | ((v) << 4))


/* CScript values */
typedef union Value {
    struct GCObject *gc; /* collectable value */
    void *p; /* light userdata */
    int b; /* boolean */
    cs_Integer i; /* integer */
    cs_Number n; /* float */
    cs_CFunction cfn; /* C function */
} Value;


/* 'TValue' fields, defined for reuse and alignment purposes */
#define TValueFields    Value val; c_byte tt


/* 'Value' with type */
typedef struct TValue {
    TValueFields;
} TValue;


#define val(o)      ((o)->val)


/* raw type tag of a TValue */
#define rawtt(o)            ((o)->tt)

/* tag with no variant (bits 0-3) */
#define novariant(t)        ((t) & 0x0F)

/* type tag of a TValue (bits 0-3 for tags + variant bits 4-5) */
#define withvariant(t)      ((t) & 0x3F)
#define ttypetag(o)         withvariant((o)->tt)

/* type of a TValue */
#define ttype(o)            novariant((o)->tt)


/* macros to test type */
#define checktag(o,t)       (rawtt(o) == (t))
#define checktype(o,t)      (ttype(o) == (t))


/* Macros for internal tests */

/* collectable object has the same tag as the original value */
#define righttt(obj)		(ttypetag(obj) == gcoval(obj)->tt_)

/*
** Any value being manipulated by the program either is non
** collectable, or the collectable object has the right tag
** and it is not dead. The option 'C == NULL' allows other
** macros using this one to be used where C is not available.
*/
#define checkliveness(C,obj) \
        ((void)C, cs_assert(!iscollectable(obj) || \
        (righttt(obj) && (C == NULL || !isdead(G(C), gcoval(obj))))))


/* Macros to set values */

/* set a value's tag */
#define settt(o,t)          (rawtt(o)=(t))

/* macro for copying values (from 'obj2' to 'obj1') */
#define setobj(C,obj1,obj2) \
    { TValue *o1_=(obj1); const TValue *o2_=(obj2); \
      o1_->val = o2_->val; settt(o1_, o2_->tt); \
      checkliveness(C,o1_); }

/* copy object from stack to stack */
#define setobjs2s(C,o1,o2)      setobj(C,s2v(o1),s2v(o2))
/* copy object to stack */
#define setobj2s(C,o1,o2)       setobj(C,s2v(o1),o2)


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
        ushort delta;
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



/* -----------------------------------------------------------------------
** Collectable Objects {
** ----------------------------------------------------------------------- */

/* common header for objects */
#define ObjectHeader    struct GCObject* next; c_byte tt_; c_byte mark


/* common type for collectable objects */
typedef struct GCObject {
    ObjectHeader;
} GCObject;


/* bit for collectable types */
#define BIT_COLLECTABLE     (1 << 6)

#define iscollectable(o)    (rawtt(o) & BIT_COLLECTABLE)

/* mark a tag as collectable */
#define ctb(tt)             ((tt) | BIT_COLLECTABLE)

#define gcoval(o)           check_exp(iscollectable(o), val(o).gc)

#define setgcoval(C,obj,x) \
    { TValue *o_=(obj); GCObject *x_=(x); \
      val(o_).gc = x_; settt(o_, ctb(x_->tt_)); }

/* } --------------------------------------------------------------------- */



/* ------------------------------------------------------------------------
** Boolean {
** ------------------------------------------------------------------------ */

#define CS_VFALSE           makevariant(CS_TBOOL, 0) /* false bool */
#define CS_VTRUE            makevariant(CS_TBOOL, 1) /* true bool */

#define ttisbool(o)         checktype(o, CS_TBOOL)
#define ttistrue(o)         checktag(o, CS_VTRUE)
#define ttisfalse(o)        checktag(o, CS_VFALSE)

#define c_isfalse(o)        (ttisfalse(o) || ttisnil(o))

#define setbfval(o)         settt(o, CS_VFALSE)
#define setbtval(o)         settt(o, CS_VTRUE)

/* } --------------------------------------------------------------------- */



/* -----------------------------------------------------------------------
** Numbers {
** ----------------------------------------------------------------------- */

#define CS_VNUMFLT      makevariant(CS_TNUMBER, 0) /* float numbers */
#define CS_VNUMINT      makevariant(CS_TNUMBER, 1) /* integer numbers */

#define ttisnum(o)      checktype(o, CS_TNUMBER)
#define ttisflt(o)      checktag(o, CS_VNUMFLT)
#define ttisint(o)      checktag(o, CS_VNUMINT)

#define nval(o)         check_exp(ttisnum(o), \
                        ttisint(o) ? cast_num(ival(o)) : fval(o))
#define ival(o)         check_exp(ttisint(o), val(o).i)
#define fval(o)         check_exp(ttisflt(o), val(o).n)

#define setival(obj,x) \
    { TValue *o_=(obj); val(o_).i = (x); settt(o_, CS_VNUMINT); }

#define changeival(obj,x) \
    { TValue *o_=(obj); cs_assert(ttisint(o_)); val_(o_).i = (x); }

#define setfval(obj,x) \
    { TValue *o_=(obj); val(o_).n = (x); settt(o_, CS_VNUMFLT); }

#define changefval(obj,x) \
    { TValue *o_=(obj); cs_assert(ttisflt(o_)); val_(o_).n = (x); }

/* } --------------------------------------------------------------------- */



/* -----------------------------------------------------------------------
** List {
** ------------------------------------------------------------------------ */

#define CS_VLIST        makevariant(CS_TLIST, 0)

#define ttislist(o)     checktag((o), ctb(CS_VLIST))

#define listval(o)      gco2list(val(o).gc)

#define setlistval(C,obj,x) \
    { TValue *o_=(obj); const List *x_=(x); \
      val(o_).gc = obj2gco(x_); settt(o_, ctb(CS_VLIST)); \
      checkliveness(C, o_); }

#define setlistval2s(C,o,l)     setlistval(C,s2v(o),l)

typedef struct List {
    ObjectHeader;
    GCObject *gclist;
    TValue *b; /* memory block */
    uint n; /* number of elements in use in 'b' */
    uint sz; /* size of 'b' */
} List;

/* } --------------------------------------------------------------------- */



/* -----------------------------------------------------------------------
** Nil {
** ----------------------------------------------------------------------- */

/* standard nil */
#define CS_VNIL         makevariant(CS_TNIL, 0)

/* empty hashtable slot */
#define CS_VEMPTY       makevariant(CS_TNIL, 1)

/* value returned for a key not found in a hashtable (absent key) */
#define CS_VABSTKEY     makevariant(CS_TNIL, 2)

#define setnilval(o)        settt(o, CS_VNIL)
#define setemptyval(o)      settt(o, CS_VEMPTY)

#define ttisnil(o)      checktype((o), CS_TNIL)
#define isempty(o)      ttisnil(o)

#define isabstkey(v)    checktag((v), CS_VABSTKEY)

#define ABSTKEYCONSTANT     {NULL}, CS_VABSTKEY

/* } --------------------------------------------------------------------- */



/* -------------------------------------------------------------------------
** Thread (cs_State) {
** ------------------------------------------------------------------------- */

#define CS_VTHREAD      makevariant(CS_TTHREAD, 0)

#define ttisthread(o)   checktag(o, ctb(CS_VTHREAD))

#define thval(o)        check_exp(ttisthread(o), gco2th(val(o).gc))

#define setthval(C,obj,x) \
    { TValue *o_=(obj); const cs_State *x_=(x); \
      val(o_).gc = obj2gco(x_); settt(o_, ctb(CS_VTHREAD)); \
      checkliveness(C, o_); }

#define setthval2s(C,o,th)      setthval(C,s2v(o),th)

/* } --------------------------------------------------------------------- */



/* -----------------------------------------------------------------------
** Hash Table {
** ----------------------------------------------------------------------- */

#define CS_VTABLE           makevariant(CS_TTABLE, 0)

#define ttishtab(o)         checktag((o), ctb(CS_VTABLE))

#define tval(o)            check_exp(ttishtab(o), gco2ht(val(o).gc))

#define settval(C,obj,x) \
    { TValue *o_=(obj); const Table *x_=(x); \
      val(o_).gc = obj2gco(x_); settt(o_, ctb(CS_VTABLE)); \
      checkliveness(C, o_); }

#define settval2s(C,o,ht)      settval(C,s2v(o),ht)


/*
** Nodes for hashtables; two TValue's for key-value pairs.
** 'next' field is to link the colliding entries.
** Ordering of fields might seem weird but this is to ensure optimal
** alignment in both 4-byte and 8-byte alignments.
*/
typedef union Node {
    struct NodeKey {
        TValueFields; /* fields for value */
        c_byte key_tt; /* key type tag */
        int next; /* offset for next node */
        Value key_val; /* key value */
    } s;
    TValue i_val; /* direct node value access as a proper 'TValue' */
} Node;


/* copy a value into a key */
#define setnodekey(C,n,obj) \
    { Node *n_=(n); const TValue *obj_=(obj); \
      n_->s.key_val = obj_->val; n_->s.key_tt = obj_->tt; \
      checkliveness(C,obj_); }


/* copy a value from a key */
#define getnodekey(C,obj,n) \
    { TValue *obj_=(obj); const Node *n_=(n); \
      obj_->val = n_->s.key_val; obj_->tt = n_->s.key_tt; \
      checkliveness(C,obj_); }


typedef struct Table {
    ObjectHeader; /* internal only object */
    c_byte size; /* 2^size */
    Node *node; /* memory block */
    Node *lastfree; /* any free position is before this position */
    GCObject *gclist;
} Table;


#define keytt(n)                ((n)->s.key_tt)
#define keyval(n)               ((n)->s.key_val)

#define keyival(n)              (keyval(n).i)
#define keyfval(n)              (keyval(n).n)
#define keypval(n)              (keyval(n).p)
#define keycfval(n)             (keyval(n).cfn)
#define keygcoval(n)            (keyval(n).gc)
#define keystrval(n)            (gco2str(keyval(n).gc))

#define keyiscollectable(n)     (keytt(n) & BIT_COLLECTABLE)
#define keyisnil(n)	        (keytt(n) == CS_TNIL)
#define keyisshrstr(n)          (keytt(n) == ctb(CS_VSHRSTR))
#define keyisint(n)             (keytt(n) == CS_VNUMINT)

#define setnilkey(n)            (keytt(n) = CS_TNIL)

#define setdeadkey(node)    (keytt(node) = CS_TDEADKEY)
#define keyisdead(n)	    (keytt(n) == CS_TDEADKEY)

/* } --------------------------------------------------------------------- */



/* -----------------------------------------------------------------------
** Strings {
** ----------------------------------------------------------------------- */

#define CS_VSHRSTR      makevariant(CS_TSTRING, 0) /* short string */
#define CS_VLNGSTR      makevariant(CS_TSTRING, 1) /* long string */

#define ttisstring(o)       checktype((o), CS_TSTRING)
#define ttisshrstring(o)    checktag((o), ctb(CS_VSHRSTR))
#define ttislngstring(o)    checktag((o), ctb(CS_VLNGSTR))

#define strval(o)       check_exp(ttisstring(o), gco2str(val(o).gc))

#define setstrval(C,obj,x) \
    { TValue *o_=(obj); const OString *x_=(x); \
      val(o_).gc = obj2gco(x_); settt(o_, ctb(x_->tt_)); \
      checkliveness((C), o_); }

#define setstrval2s(C,o,s)      setstrval(C,s2v(o),s)


typedef struct OString {
    ObjectHeader;
    /* reserved words or tag names index for short strings;
     * flag for long strings indicating that it has hash */
    c_byte extra;
    c_byte shrlen; /* length for short strings, 0xFF for longs strings */
    uint hash;
    union {
        size_t lnglen; /* length for long strings */
        struct OString *next; /* linked list for 'strtab' (hash table) */
    } u;
    char bytes[]; /* string contents */
} OString;


/*
** Get string bytes from 'OString'. (Both generic version and specialized
** versions for long and short strings.)
*/
#define getstr(os)       ((os)->bytes)
#define getlngstr(os)    check_exp((os)->shrlen == 0xFF, (os)->bytes)
#define getshrstr(os)    check_exp((os)->shrlen != 0xFF, (os)->bytes)

/* get string length from 'OString *s' */
#define getstrlen(s)    ((s)->shrlen != 0xFF ? (s)->shrlen : (s)->u.lnglen)

/* } --------------------------------------------------------------------- */



/* -----------------------------------------------------------------------
** Class {
** ----------------------------------------------------------------------- */

#define CS_VCLASS       makevariant(CS_TCLASS, 0)

#define ttisclass(o)    checktag(o, ctb(CS_VCLASS))

#define classval(o)     check_exp(ttisclass(o), gco2cls(val(o).gc))

#define setclsval(C,obj,x) \
    { TValue *o_=(obj); const OClass *x_=(x); \
      val(o_).gc = obj2gco(x_); settt(o_, ctb(CS_VCLASS)); \
      checkliveness(C, o_); }

#define setclsval2s(C,o,cls)        setclsval(C,s2v(o),cls)

typedef struct OClass {
    ObjectHeader;
    List *metalist;
    Table *methods;
    struct OClass *sclass;
} OClass;

/* } --------------------------------------------------------------------- */



/* ------------------------------------------------------------------------
** Function Prototypes {
** ------------------------------------------------------------------------ */

#define CS_VPROTO           makevariant(CS_TPROTO, 0)


/* 
** Information of the upvalues for function prototypes
*/
typedef struct UpValInfo {
    OString *name;  /* upvalue name */
    int idx;        /* index in stack or outer function local var list */
    c_byte onstack; /* is it on stack */
    c_byte kind;
} UpValInfo;


/* 
** Information of the local variable for function prototypes
** (used for debug information).
*/
typedef struct LVarInfo {
    OString *name;  /* local name */
    int startpc;    /* point where variable is in scope */
    int endpc;      /* point where variable is out of scope */
} LVarInfo;


/*
** Associates the absolute line source for a given instruction ('pc').
** The array 'lineinfo' gives, for each instruction, the difference in
** lines from the previous instruction. When that difference does not
** fit into a byte, CScript saves the absolute line for that instruction.
** (CScript also saves the absolute line periodically, to speed up the
** computation of a line number: we can use binary search in the
** absolute-line array, but we must traverse the 'lineinfo' array
** linearly to compute a line.)
*/
typedef struct AbsLineInfo {
    int pc;
    int line;
} AbsLineInfo;


/*
** Function Prototypes.
*/
typedef struct Proto {
    ObjectHeader;
    c_byte isvararg;        /* true if this function accepts extra params */
    int arity;              /* number of fixed (named) function parameters */
    int maxstack;           /* max stack size for this function */
    int sizep;              /* size of 'p' */
    int sizek;              /* size of 'k' */
    int sizecode;           /* size of 'code' */
    int sizeupvals;         /* size of 'upvals' */
    int sizelineinfo;       /* size of 'lineinfo' */
    int sizeabslineinfo;    /* size of 'abslineinfo' */
    int sizeinstpc;         /* size of 'instpc' */
    int sizelocals;         /* size of 'locals' */
    int defline;            /* function definition line (debug) */
    int deflastline;        /* function definition last line (debug) */
    struct Proto **p;       /* list of funcs defined inside of this function */
    TValue *k;              /* constant values */
    Instruction *code;      /* bytecode */
    UpValInfo *upvals;      /* debug information for upvalues */
    c_sbyte *lineinfo;      /* information about source lines (debug) */
    AbsLineInfo *abslineinfo; /* idem */
    int *instpc;            /* list of pc's for each instruction (debug) */
    LVarInfo *locals;       /* information about local variables (debug) */
    OString *source;        /* source name (debug information) */
    GCObject *gclist;
} Proto;

/* } --------------------------------------------------------------------- */



/* -----------------------------------------------------------------------
**  Instance {
** ----------------------------------------------------------------------- */

#define CS_VINSTANCE        makevariant(CS_TINSTANCE, 0)

#define ttisinstance(o)     checktag(o, ctb(CS_VINSTANCE))

#define insval(o)       check_exp(ttisinstance(o), gco2ins(val(o).gc))

#define setinsval(C,obj,x) \
    { TValue *o_=(obj); const Instance *x_=(x); \
      val(o_).gc = obj2gco(x_); settt(o_, ctb(CS_VINSTANCE)); \
      checkliveness(C, o_); }

#define setinsval2s(C,o,ins)        setinsval(C,s2v(o),ins)

typedef struct Instance {
    ObjectHeader;
    OClass *oclass;
    Table *fields;
} Instance;

/* } --------------------------------------------------------------------- */



/* -----------------------------------------------------------------------
** Functions {
** ----------------------------------------------------------------------- */

#define CS_VUPVALUE     makevariant(CS_TUPVALUE, 0)

#define CS_VCSCL        makevariant(CS_TFUNCTION, 0) /* CScript closure */
#define CS_VLCF         makevariant(CS_TFUNCTION, 1) /* light C function */
#define CS_VCCL         makevariant(CS_TFUNCTION, 2) /* C closure */
#define CS_VIMETHOD     makevariant(CS_TFUNCTION, 3) /* bounded method */

#define ttisfunction(o)         checktype(o, CS_TFUNCTION)
#define ttisCSclosure(o)        checktag(o, ctb(CS_VCSCL))
#define ttislcf(o)              checktag(o, CS_VLCF)
#define ttisCclosure(o)         checktag(o, ctb(CS_VCCL))
#define ttisinstancemethod(o)   checktag(o, ctb(CS_VIMETHOD))
#define ttisclosure(o)          (ttisCSclosure(o) || ttisCclosure(o))

#define clval(o)        check_exp(ttisclosure(o), gco2cl(val(o).gc))
#define clCSval(o)      check_exp(ttisCSclosure(o), gco2clcs(val(o).gc))
#define clCval(o)       check_exp(ttisCclosure(o), gco2clc(val(o).gc))
#define lcfval(o)       check_exp(ttislcf(o), val(o).cfn)
#define imval(o)        check_exp(ttisinstancemethod(o), gco2im(val(o).gc))

#define setclCSval(C,obj,x) \
    { TValue *o_=(obj); const CSClosure *x_=(x); \
      val(o_).gc = obj2gco(x_); settt(o_, ctb(CS_VCSCL)); \
      checkliveness(C, o_); }

#define setclCSval2s(C,o,cl)    setclCSval(C,s2v(o),cl)

#define setcfval(C,obj,x) \
    { TValue *o_ = (obj); val(o_).cfn=(x); settt(o_, CS_VLCF); }

#define setclCval(C,obj,x) \
    { TValue *o_=(obj); const CClosure *x_=(x); \
      val(o_).gc = obj2gco(x_); settt(o_, ctb(CS_VCCL)); \
      checkliveness(C, o_); }

#define setclCval2s(C,o,cl)     setclCval(C,s2v(o),cl)

#define setimval(C,obj,x) \
    { TValue *o_=(obj); const IMethod *x_=(x); \
      val(o_).gc = obj2gco(x_); settt(o_, ctb(CS_VIMETHOD)); \
      checkliveness(C, o_); }

#define setimval2s(C,o,im)      setimval(C,s2v(o),im)


/* method bounded to instance */
typedef struct IMethod {
    ObjectHeader;
    Instance *ins;
    TValue method;
} IMethod;


/* upvalues for CScript closures */
typedef struct UpVal {
    ObjectHeader;
    union {
        TValue *p; /* on stack or in 'u.value' */
        ptrdiff_t offset; /* when reallocating stack */
    } v;
    union {
        struct { /* valid when open */
            struct UpVal *next; /* linked list */
            struct UpVal **prev;
        } open;
        TValue value; /* value stored here when closed */
    } u;
} UpVal;



/* common closure header */
#define ClosureHeader   ObjectHeader; int nupvalues; GCObject *gclist


typedef struct CSClosure {
    ClosureHeader;
    Proto *p;
    UpVal *upvals[];
} CSClosure;


typedef struct CClosure {
    ClosureHeader;
    cs_CFunction fn;
    TValue upvals[];
} CClosure;


typedef union Closure {
    CClosure c;
    CSClosure cs;
} Closure;


#define getproto(o)	(clCSval(o)->p)

/* } --------------------------------------------------------------------- */



/* -----------------------------------------------------------------------
** Userdata {
** ----------------------------------------------------------------------- */

#define CS_VLIGHTUSERDATA           makevariant(CS_TUSERDATA, 0)
#define CS_VUSERDATA                makevariant(CS_TUSERDATA, 1)

#define ttislightuserdata(o)        checktag(o, CS_VLIGHTUSERDATA)
#define ttisfulluserdata(o)         checktag(o, ctb(CS_VUSERDATA))

#define pval(o)     check_exp(ttislightuserdata(o), val(o).p)
#define uval(o)     check_exp(ttisfulluserdata(o), gco2u(val(o).gc))

#define setpval(obj,x) \
    { TValue *o_=(obj); val(o_).p = (x); settt(o_, CS_VLIGHTUSERDATA); }

#define setuval(C,obj,x) \
    { TValue *o_=(obj); const UserData *x_=(x); \
      val(o_).gc = obj2gco(x_); settt(o_, ctb(CS_VUSERDATA)); \
      checkliveness(C,o_); }

#define setuval2s(C,o,uv)       setuval(C, s2v(o), uv)


/*
** 'TValue' that ensures that addresses after this type are
** always fully aligned.
*/
typedef union UValue {
    TValue val;
    CSI_MAXALIGN; /* ensures maximum alignment for udata bytes */
} UValue;


typedef struct UserData {
    ObjectHeader;
    ushort nuv; /* number of 'uservalues' */
    size_t size; /* size of 'UserData' memory in bytes */
    List *metalist;
    GCObject *gclist;
    UValue uv[]; /* user values */
    /* 'UserData' memory starts here */
} UserData;


/*
** 'UserData' without user values, meaning 'uv' is empty ('nuv' == 0).
** This is used when allocating 'UserData' to properly calculate offset
** of user memory because 'uv' is a flexible array member.
** Also this kind of userdata is never gray so it doesnt need 'gclist'.
** Internally CScript only uses 'UserData' to access fields and it takes
** care to avoid using 'uv' and 'gclist' fields when 'nuv' is 0.
*/
typedef struct EmptyUserData {
    ObjectHeader;
    ushort nuv; /* number of 'uservalues' */
    size_t size; /* size of 'usermem' in bytes */
    List *metalist;
    union {CSI_MAXALIGN;} usermem;
    /* 'UserData' memory starts here */
} EmptyUserData;


/* offset in 'UserData' where user memory begins */
#define udmemoffset(nuv) \
        ((nuv) == 0 ? offsetof(EmptyUserData, usermem) \
                    : offsetof(UserData, uv) + ((nuv)*sizeof(UValue)))

/* get the address of the memory block inside 'UserData' */
#define getuserdatamem(u)       (cast_charp(u) + udmemoffset((u)->nuv))

/* size of 'UserData' */
#define sizeofuserdata(nuv, size)   (udmemoffset(nuv) + (size))

/* } --------------------------------------------------------------------- */



/*
** Conversion modes when converting 'cs_Integer'
** into 'cs_Number'.
*/
typedef enum N2IMode {
    N2IFLOOR,
    N2ICEIL,
    N2IEXACT,
} N2IMode;


/* convert value to 'cs_Integer' */
#define tointeger(v,i) \
        (c_likely(ttisint(v)) ? (*(i) = ival(v), 1) \
                              : csO_tointeger(v, i, N2IEXACT))


/* convert value to 'cs_Number' */
#define tonumber(v,n) \
        (ttisflt(v) ? ((n) = fval(v), 1) : \
        (ttisint(v) ? ((n) = cast_num(ival(v)), 1) : 0))


/* same as left shift but indicate right by making 'y' negative */
#define csO_shiftr(x,y)     csO_shiftl(x, -(y))


/* fast 'module' operation for hashing (sz is always power of 2) */
#define hashmod(h,sz) \
        (check_exp(((sz&(sz-1))==0), (cast_int((h) & ((sz)-1)))))


CSI_FUNC int csO_ceillog2(uint x);
CSI_FUNC int csO_n2i(cs_Number n, cs_Integer *i, N2IMode mode);
CSI_FUNC int csO_tointeger(const TValue *v, cs_Integer *i, int mode);
CSI_FUNC cs_Integer csO_shiftl(cs_Integer x, cs_Integer y);
CSI_FUNC int csO_arithmraw(cs_State *C, const TValue *a, const TValue *b,
                           TValue *res, int op);

#endif
