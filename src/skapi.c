#include "object.h"
#include "skconf.h"
#include "skooma.h"
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
    sk_lock(*vmp)
}




/*
 * Push values on the stack.
 */

#define vmstklast(vm) (vm->stack + VM_STACK_MAX - 1)
#define incsp(vm)                                                                        \
    do {                                                                                 \
        (vm)->sp++;                                                                      \
        sk_checkapi(vm, vm->sp - vm->stack <= VM_STACK_MAX, "stack overflow.");          \
    } while(0)

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

static size_t sk_strlen(const char* str)
{
    size_t                  len = 0;
    register unsigned char* c   = (unsigned char*)str;
    while(*c++)
        len++;
    return len;
}

SK_API void sk_pushcstring(VM* vm, const char* str)
{
    sk_lock(vm);
    if(str == NULL) *vm->sp = NIL_VAL;
    else *vm->sp = OBJ_VAL(OString_from(vm, str, sk_strlen(str)));
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
 * Ensure the stack has enough space.
 */

SK_API int sk_ensurestack(VM* vm, int n)
{
    sk_lock(vm);
    sk_checkapi(vm, n >= 0, "negative 'n'.");
    int res = (((vm->sp - vm->stack) + n) < VM_STACK_MAX);
    sk_unlock(vm);
    return res;
}




static Value* sk_idx2val(const VM* vm, int idx)
{
    Value* val = NULL;
    if(idx < 0) {
        sk_checkapi(vm, idx > -(VM_STACK_MAX), "Invalid index.");
    } else {
    }
    return val;
}




/*
 * Check value type.
 */

/* Create type bitmask from the value.
 * First least significant set bit acts as a type tag.
 * bit 0 -> number
 * bit 1 -> string
 * bit 2 -> callable
 * bit 3 -> bool
 * bit 4 -> nil
 * bit 5 -> instance
 * bit 6 -> class */
#define val2tbmask(value)                                                                \
    cast_uint(                                                                           \
        (IS_NUMBER(value) * 1) | (IS_STRING(value) * 2) |                                \
        ((IS_FUNCTION(value) | IS_BOUND_METHOD(value) | IS_CLOSURE(value) |              \
          IS_NATIVE(value)) *                                                            \
         4) |                                                                            \
        IS_BOOL(value) * 8 | IS_NIL(value) * 16 | IS_INSTANCE(value) * 32 |              \
        IS_CLASS(value) * 64)


static int sk_val2type(const VM* vm, Value* value)
{
    int type = SK_TNONE;
#if defined(S_PRECOMPUTED_GOTO) && __has_builtin(__builtin_ctz)
    static const void* jmptable[] = {
        &&num,
        &&str,
        &&fn,
        &&bl,
        &&nil,
        &&ins,
        &&cls,
    };
    // https://gcc.gnu.org/onlinedocs/gcc/Other-Builtins.html#index-_005f_005fbuiltin_005fctz
    unsigned char bitidx = __builtin_ctz(val2tbmask(*value));
    goto*         jmptable[bitidx];
num:
    type = SK_TNUMBER;
str:
    type = SK_TSTRING;
fn:
    type = SK_TFUNCTION;
bl:
    type = SK_TBOOL;
nil:
    type = SK_TNIL;
ins:
    type = SK_TINSTANCE;
cls:
    type = SK_TCLASS;
#else
    if(IS_NUMBER(value)) return SK_TNUMBER;
    else if(IS_STRING(value)) type = SK_TSTRING;
    else if(
        IS_FUNCTION(value) || IS_BOUND_METHOD(value) || IS_CLOSURE(value) ||
        IS_NATIVE(value))
        type = SK_TFUNCTION;
    else if(IS_BOOL(value)) type = SK_TBOOL;
    else if(IS_NIL(value)) type = SK_TNIL;
    else if(IS_INSTANCE(value)) type = SK_TINSTANCE;
    else if(IS_CLASS(value)) type = SK_TCLASS;
#endif
    sk_checkapi(vm, type != SK_TNONE, "Invalid type.");
    return type;
}

SK_API int sk_typeof(const VM* vm, int idx)
{
    int    type;
    Value* value;
    sk_lock(vm);
    value = sk_idx2val(vm, idx);
    type  = sk_val2type(vm, value);
    sk_unlock(vm);
    return type;
}



/*
 * Get values from stack.
 */

SK_API int sk_getbool(const VM* vm, int idx)
{
    int    type = SK_TNONE;
    int    bval;
    Value* val;
    sk_lock(vm);
    val  = sk_idx2val(vm, idx);
    type = sk_val2type(vm, val);
    (type == SK_TBOOL) ? bval = AS_BOOL(*val) : 0;
    sk_unlock(vm);
    return bval;
}

SK_API double sk_getnumber(const VM* vm, int idx)
{
    int    type = SK_TNONE;
    double nval;
    Value* val;
    sk_lock(vm);
    val  = sk_idx2val(vm, idx);
    type = sk_val2type(vm, val);
    (type == SK_TNUMBER) ? nval = AS_NUMBER(*val) : 0.0;
    sk_unlock(vm);
    return nval;
}

SK_API const char* sk_getstring(const VM* vm, int idx)
{
    int         type = SK_TNONE;
    const char* sval = NULL;
    Value*      val;
    sk_lock(vm);
    val  = sk_idx2val(vm, idx);
    type = sk_val2type(vm, val);
    (type == SK_TSTRING) ? sval = AS_CSTRING(*val) : 0;
    sk_unlock(vm);
    return sval;
}


SK_API size_t sk_rawlen(const VM* vm, int idx)
{
#define stringlen(val) (AS_STRING(val)->len)
#define classlen(val)  (AS_CLASS(val)->methods.len)
    int    type = SK_TNONE;
    size_t len;
    Value* val;
    sk_lock(vm);
    val  = sk_idx2val(vm, idx);
    type = sk_val2type(vm, val);
    switch(type) {
        case SK_TSTRING:
            len = stringlen(*val);
            break;
        case SK_TCLASS:
            len = classlen(*val);
            break;
        default:
            len = 0;
            break;
    }
    sk_unlock(vm);
    return len;
#undef stringlen
#undef classlen
}



SK_API int sk_gettop(const VM* vm)
{
    return cast_int(vm->sp - (vm->cinfo.fnloc + 1));
}


SK_API void sk_settop(VM* vm, int idx)
{
    Value*   fn;
    intptr_t diff;
    sk_lock(vm);
    fn = vm->cinfo.fnloc;
    if(idx >= 0) { // index positive
        sk_checkapi(vm, idx < (vmstklast(vm) - fn), "index too big.");
        diff = ((fn + 1) + idx) - vm->sp;
        for(; diff > 0; diff--)
            *vm->sp++ = NIL_VAL;
    } else { // index negative
        sk_checkapi(vm, -idx <= (vm->sp - fn), "index too small.");
        diff = idx + 1;
    }
    vm->sp += diff;
    sk_unlock(vm);
}


SK_API void sk_push(VM* vm, int idx)
{
    Value* val;
    sk_lock(vm);
    val     = sk_idx2val(vm, idx);
    *vm->sp = *val;
    incsp(vm);
    sk_unlock(vm);
}


SK_API void sk_remove(VM* vm, int idx)
{
    // @TODO: Implement
    Value* val;
    sk_lock(vm);
    val = sk_idx2val(vm, idx);
    sk_unlock(vm);
}
