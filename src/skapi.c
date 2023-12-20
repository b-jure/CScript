#include "object.h"
#include "skconf.h"
#include "skooma.h"
#include "value.h"
#include "vmachine.h"



/*
 * Create/Destroy the VM.
 */

force_inline VM* sk_create(Config* cfg)
{
    return VM_new(cfg);
}

force_inline void sk_destroy(VM** vmp)
{
    sk_lock(*vmp);
    VM_free(vmp);
}




#define stklast(vm) cast_intptr(vm->stack + VM_STACK_MAX - 1)

#define incsp(vm)                                                                        \
    do {                                                                                 \
        (vm)->sp++;                                                                      \
        sk_checkapi(vm, vm->sp - vm->stack <= VM_STACK_MAX, "stack overflow.");          \
    } while(0)

#define decsp(vm)                                                                        \
    do {                                                                                 \
        (vm)->sp--;                                                                      \
        sk_checkapi(vm, vm->stack <= (vm)->sp, "stack underflow.");                      \
    } while(0)

#define sk_apiensure(vm, n)                                                              \
    do {                                                                                 \
        (vm)->sp += n;                                                                   \
        sk_checkapi(vm, (vm)->stack <= (vm)->sp, "stack overflow.");                     \
    } while(0)


/*
 * Get stack value at 'idx'.
 */
static force_inline Value* idx2val(const VM* vm, int idx)
{
    Value* fn = vm->cinfo.fnloc;
    if(idx >= 0) {
        sk_checkapi(vm, idx < vm->sp - 1 - fn, "index too big.");
        return (fn + 1 + idx);
    } else { // idx is negative
        sk_checkapi(vm, -idx <= (vm->sp - fn), "Invalid index.");
        return (vm->sp + idx);
    }
}


/*
 * Push values on the stack.
 */
SK_API void sk_pushnil(VM* vm)
{
    sk_lock(vm);
    *vm->sp = NIL_VAL;
    incsp(vm);
    sk_unlock(vm);
}

SK_API void sk_pushnumber(VM* vm, double number)
{
    sk_lock(vm);
    *vm->sp = NUMBER_VAL(number);
    incsp(vm);
    sk_unlock(vm);
}

SK_API void sk_pushstring(VM* vm, const char* str, size_t len)
{
    sk_lock(vm);
    *vm->sp = OBJ_VAL(OString_from(vm, str, len));
    incsp(vm);
    sk_unlock(vm);
}

static force_inline size_t skstrlen(const char* str)
{
    size_t         len = 0;
    unsigned char* c   = (unsigned char*)str;
    while(*c++)
        len++;
    return len;
}

SK_API void sk_pushcstring(VM* vm, const char* str)
{
    sk_lock(vm);
    if(str == NULL) *vm->sp = NIL_VAL;
    else *vm->sp = OBJ_VAL(OString_from(vm, str, skstrlen(str)));
    incsp(vm);
    sk_unlock(vm);
}

SK_API void sk_pushbool(VM* vm, int boolean)
{
    sk_lock(vm);
    *vm->sp = BOOL_VAL(boolean);
    incsp(vm);
    sk_unlock(vm);
}

/*
 * Shifts values on stack either to the left or right (once).
 * 0 direction is a left shift, anything else is a right shift.
 */
static force_inline void stackshift(VM* vm, Value* val, int direction)
{
#define LSHFT 0

    uintptr_t shift = vm->sp - (val + 1);
    if(direction == LSHFT && shift > 0) memcpy(val, val + 1, shift);
    else if(shift > 0) memcpy(val + 1, val, shift);

#undef LSHFT
}


static force_inline void popatidx(VM* vm, int idx)
{
    Value* val = idx2val(vm, idx);
    stackshift(vm, val, 0);
    decsp(vm);
}

static force_inline void pusho(VM* vm, O* obj)
{
    *vm->sp = OBJ_VAL(obj);
    incsp(vm);
}

#define pushostring(vm, string)   pusho(vm, (O*)(string))
#define pushoclosure(vm, closure) pusho(vm, (O*)(closure))


static force_inline int getmethod(VM* vm, OClass* class, const char* method)
{
    Value    m;
    OString* name = OString_from(vm, method, skstrlen(method));
    pushostring(vm, name);
    if(HashTable_get(&class->methods, OBJ_VAL(name), &m)) {
        decsp(vm); // pop method string
        pushoclosure(vm, AS_CLOSURE(m));
        return 1;
    }
    return 0;
}

/*
 * Push class method of an instance at idx on top of the stack.
 * Return 1 if class instance has a method with 'name' otherwise 0.
 */
SK_API int sk_hasmethod(VM* vm, int idx, const char* method)
{
    int res;
    sk_lock(vm);
    Value val = *idx2val(vm, idx);
    sk_checkapi(vm, IS_INSTANCE(val), "expected instance.");
    res = getmethod(vm, AS_INSTANCE(val)->oclass, method);
    sk_unlock(vm);
    return res;
}






/*
 * Ensure the stack has enough space.
 */
SK_API int sk_ensurestack(VM* vm, int n)
{
    sk_checkapi(vm, n >= 0, "negative 'n'.");
    return (((vm->sp - vm->stack) + n) < VM_STACK_MAX);
}






/*
 * Check value type.
 */

/* Create type bitmask from the value.
 * First least significant set bit acts as a type tag.
 * bit 0 is set -> number
 * bit 1 is set -> string
 * bit 2 is set -> callable
 * bit 3 is set -> bool
 * bit 4 is set -> nil
 * bit 5 is set -> instance
 * bit 6 is set -> class */
#define val2tbmask(value)                                                                \
    cast_uint(                                                                           \
        0 | (IS_NUMBER(value) * 1) | (IS_STRING(value) * 2) |                            \
        ((IS_FUNCTION(value) | IS_BOUND_METHOD(value) | IS_CLOSURE(value) |              \
          IS_NATIVE(value)) *                                                            \
         4) |                                                                            \
        IS_BOOL(value) * 8 | IS_NIL(value) * 16 | IS_INSTANCE(value) * 32 |              \
        IS_CLASS(value) * 64)


static int sk_val2type(const VM* vm, Value* value)
{
#if defined(S_PRECOMPUTED_GOTO) && __has_builtin(__builtin_ctz)
    static const int typetable[] = {
        SK_TNUMBER,
        SK_TSTRING,
        SK_TFUNCTION,
        SK_TBOOL,
        SK_TNIL,
        SK_TINSTANCE,
        SK_TCLASS,
    };
    // https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html#index-_005f_005fbuiltin_005fctz
    unsigned char bitidx = __builtin_ctz(val2tbmask(*value));
    return typetable[bitidx];
#else
    if(IS_NUMBER(value)) return SK_TNUMBER;
    else if(IS_STRING(value)) return SK_TSTRING;
    else if(
        IS_FUNCTION(value) || IS_BOUND_METHOD(value) || IS_CLOSURE(value) ||
        IS_NATIVE(value))
        return SK_TFUNCTION;
    else if(IS_BOOL(value)) return SK_TBOOL;
    else if(IS_NIL(value)) return SK_TNIL;
    else if(IS_INSTANCE(value)) return SK_TINSTANCE;
    else if(IS_CLASS(value)) return SK_TCLASS;
#endif
    unreachable;
}

SK_API int sk_typeof(const VM* vm, int idx)
{
    Value* value = idx2val(vm, idx);
    return sk_val2type(vm, value);
}

SK_API int sk_isnil(VM* vm, int idx)
{
    return IS_NIL(*idx2val(vm, idx));
}

SK_API int sk_isnumber(VM* vm, int idx)
{
    return IS_NUMBER(*idx2val(vm, idx));
}

SK_API int sk_isstring(VM* vm, int idx)
{
    return IS_STRING(*idx2val(vm, idx));
}

SK_API int sk_isbool(VM* vm, int idx)
{
    return IS_BOOL(*idx2val(vm, idx));
}

SK_API int sk_isclass(VM* vm, int idx)
{
    return IS_CLASS(*idx2val(vm, idx));
}

SK_API int sk_isinstance(VM* vm, int idx)
{
    return IS_INSTANCE(*idx2val(vm, idx));
}




/*
 * Get values from stack.
 */
SK_API int sk_getbool(const VM* vm, int idx, int* isbool)
{
    int   bval;
    Value val = *idx2val(vm, idx);
    int   is  = tobool(val, &bval);
    if(isbool) *isbool = is;
    return bval;
}

SK_API double sk_getnumber(const VM* vm, int idx, int* isnum)
{
    double nval;
    Value  val = *idx2val(vm, idx);
    int    is  = tonumber(val, &nval);
    if(isnum) *isnum = is;
    return nval;
}

SK_API const char* sk_getstring(const VM* vm, int idx)
{
    Value val = *idx2val(vm, idx);
    return IS_STRING(val) ? AS_CSTRING(val) : 0;
}

SK_API size_t sk_rawlen(const VM* vm, int idx)
{
    size_t len;
    Value* val  = idx2val(vm, idx);
    int    type = sk_val2type(vm, val);
    switch(type) {
        case SK_TSTRING:
            len = AS_STRING(*val)->len;
            break;
        case SK_TCLASS:
            len = AS_CLASS(*val)->methods.len;
            break;
        default:
            len = 0;
            break;
    }
    return len;
}



SK_API int sk_gettop(const VM* vm)
{
    return cast_int(vm->sp - (vm->cinfo.fnloc + 1));
}


SK_API void sk_settop(VM* vm, int idx)
{
    Value *  fn, *val;
    Value*   newtop;
    intptr_t diff;
    sk_lock(vm);
    fn = vm->cinfo.fnloc;
    if(idx >= 0) {
        sk_checkapi(vm, idx < ((Value*)stklast(vm) - fn), "index too big.");
        diff = ((fn + 1) + idx) - vm->sp;
        for(; diff > 0; diff--)
            *vm->sp++ = NIL_VAL;
    } else { // index negative
        sk_checkapi(vm, -idx <= (vm->sp - fn), "invalid index.");
        diff = idx + 1;
    }
    newtop = vm->sp + diff;
    if(diff < 0) closeupval(vm, newtop);
    vm->sp = newtop;
    sk_unlock(vm);
}


SK_API void sk_push(VM* vm, int idx)
{
    sk_lock(vm);
    *vm->sp = *idx2val(vm, idx);
    incsp(vm);
    sk_unlock(vm);
}


SK_API void sk_remove(VM* vm, int idx)
{
    sk_lock(vm);
    Value* val = idx2val(vm, idx);
    stackshift(vm, val, 0);
    closeupval(vm, vm->sp - 1);
    decsp(vm);
    sk_unlock(vm);
}

SK_API void sk_insert(VM* vm, int idx)
{
    sk_lock(vm);
    Value* top = vm->sp - 1;
    Value* val = idx2val(vm, idx);
    stackshift(vm, val, 1);
    *val = *top;
    incsp(vm);
    sk_unlock(vm);
}

SK_API void sk_replace(VM* vm, int idx)
{
    sk_lock(vm);
    Value* top = vm->sp - 1;
    Value* val = idx2val(vm, idx);
    if(top != val) *val = *top;
    closeupval(vm, top);
    decsp(vm);
    sk_unlock(vm);
}
