#include "debug.h"
#include "object.h"
#include "skapi.h"
#include "skconf.h"
#include "skooma.h"
#include "stdarg.h"
#include "value.h"
#include "vmachine.h"

#include <time.h>


/* Get stack value at 'idx'. */
static force_inline Value* idx2val(const VM* vm, int idx)
{
    Value* fn = vm->frames[vm->fc - 1].callee;
    if(idx >= 0) {
        sk_checkapi(vm, idx < vm->sp - 1 - fn, "index too big.");
        return (fn + 1 + idx);
    } else { // idx is negative
        sk_checkapi(vm, -idx <= (vm->sp - fn), "Invalid index.");
        return (vm->sp + idx);
    }
}



/* Shifts values on stack either to the left or right (once).
 * 0 direction is a left shift, anything else is a right shift. */
static force_inline void stackshift(VM* vm, Value* val, int direction)
{
    uintptr_t shift = vm->sp - (val + 1);
    if(direction == 0 && shift > 0) memcpy(val, val + 1, shift);
    else if(shift > 0) memcpy(val + 1, val, shift);
}



/* Ensure the stack has enough space. */
SK_API int sk_ensurestack(VM* vm, int n)
{
    sk_checkapi(vm, n >= 0, "negative 'n'.");
    return (((vm->sp - vm->stack) + n) < VM_STACK_MAX);
}



/* Create the VM, you can additionally provide your own allocator. */
SK_API VM* sk_create(AllocFn allocator, void* ud)
{
    AllocFn allocate = allocator ? allocator : reallocate;
    VM* vm = allocate(NULL, sizeof(VM), ud);
    memset(&vm->config, 0, sizeof(Config));
    vm->config.reallocate = allocate;
    vm->config.userdata = ud;
    srand(time(0));
    vm->seed = rand();
    vm->fc = 0;
    vm->objects = NULL;
    vm->F = NULL;
    vm->open_upvals = NULL;
    vm->script = NIL_VAL;
    vm->gc_allocated = 0;
    vm->gc_next = (1 << 20); // 1 MiB
    vm->gc_flags = 0;
    vm->sp = vm->stack;
    HashTable_init(&vm->loaded); // Loaded scripts and their functions
    HashTable_init(&vm->globids); // Global variable identifiers
    GSARRAY_INIT(vm); // Gray stack array (no GC)
    Array_Variable_init(&vm->globvars, vm);
    Array_Value_init(&vm->temp, vm); // Temp values storage (return values)
    Array_VRef_init(&vm->callstart, vm);
    Array_VRef_init(&vm->retstart, vm);
    HashTable_init(&vm->strings); // Interned strings table (Weak_refs)
    memset(vm->statics, 0, sizeof(vm->statics));
    for(UInt i = 0; i < SS_SIZE; i++)
        vm->statics[i] = OString_new(vm, static_str[i].name, static_str[i].len);
    // @REFACTOR?: Maybe make the native functions private and only
    //             callable inside class instances?
    //             Upside: less branching resulting in more straightforward code.
    //             Downside: slower function call (maybe not so bad because
    //             of removal of type checking inside the native functions, needs
    //             testing)
    //
    // @TODO?: Change NativeFn signature to accept variable amount of arguments.
    //         Upside: More expressive and flexible functions.
    //         Downside: va_list parsing resulting in slower function call
    //         processing
    // VM_define_native(vm, "clock", native_clock, 0, false); // GC
    // VM_define_native(vm, "isfield", native_isfield, 2, false); // GC
    // VM_define_native(vm, "printl", native_printl, 1, false); // GC
    // VM_define_native(vm, "print", native_print, 1, false); // GC
    // VM_define_native(vm, "tostr", native_tostr, 1, false); // GC
    // VM_define_native(vm, "isstr", native_isstr, 1, false); // GC
    // VM_define_native(vm, "strlen", native_strlen, 1, false); // GC
    // VM_define_native(vm, "strpat", native_strpat, 2, false); // GC
    // VM_define_native(vm, "strsub", native_strsub, 3, false); // GC
    // VM_define_native(vm, "strbyte", native_strbyte, 2, false); // GC
    // VM_define_native(vm, "strlower", native_strlower, 1, false); // GC
    // VM_define_native(vm, "strupper", native_strupper, 1, false); // GC
    // VM_define_native(vm, "strrev", native_strrev, 1, false); // GC
    // VM_define_native(vm, "strconcat", native_strconcat, 2, false); // GC
    // VM_define_native(vm, "byte", native_byte, 1, false); // GC
    // VM_define_native(vm, "gcfactor", native_gcfactor, 1, false); // GC
    // VM_define_native(vm, "gcmode", native_gcmode, 1, false); // GC
    // VM_define_native(vm, "gccollect", native_gccollect, 0, false); // GC
    // VM_define_native(vm, "gcleft", native_gcleft, 0, false); // GC
    // VM_define_native(vm, "gcusage", native_gcusage, 0, false); // GC
    // VM_define_native(vm, "gcnext", native_gcnext, 0, false); // GC
    // VM_define_native(vm, "gcset", native_gcset, 1, false); // GC
    // VM_define_native(vm, "gcisauto", native_gcisauto, 0, false); // GC
    // VM_define_native(vm, "assert", native_assert, 1, false); // GC
    // VM_define_native(vm, "assertf", native_assertf, 2, false); // GC
    // VM_define_native(vm, "error", native_error, 1, false); // GC
    // VM_define_native(vm, "typeof", native_typeof, 1, false); // GC
    // VM_define_native(vm, "loadscript", native_loadscript, 1, false); // GC
    return vm;
}



/* Free the VM allocation, the pointer to VM will be nulled out. */
SK_API void sk_destroy(VM** vmp)
{
    if(likely(vmp)) { // non-null pointer ?
        sk_lock(*vmp);
        if(*vmp == NULL) return;
        VM* vm = *vmp;
        HashTable_free(vm, &vm->loaded);
        HashTable_free(vm, &vm->globids);
        GSARRAY_FREE(vm);
        Array_Variable_free(&vm->globvars, NULL);
        Array_Value_free(&vm->temp, NULL);
        Array_VRef_free(&vm->callstart, NULL);
        Array_VRef_free(&vm->retstart, NULL);
        HashTable_free(vm, &vm->strings);
        O* next;
        for(O* head = vm->objects; head != NULL; head = next) {
            next = onext(head);
            ofree(vm, head);
        }
        FREE(vm, vm);
        *vmp = NULL;
    }
}



/* Convert 'acceptable' stack index into an absolute index.
 * For example: -1 would be index 4, if there are 5 values on the stack
 * after the callee of the call frame. */
SK_API int sk_absidx(VM* vm, int idx)
{
    return (idx >= 0) ? idx : cast_int(vm->sp - last_frame(vm).callee - 1) + idx;
}



/* Return type of the value on the stack at 'idx'. */
SK_API int sk_type(const VM* vm, int idx)
{
    Value* value = idx2val(vm, idx);
    return val2type(*value);
}



/* Return type name of the value on the stack at 'idx'.
 * This returned pointer is 'const' indicating the
 * memory it points to should not be modified. */
SK_API const char* sk_typename(const VM* vm, int idx)
{
    Value* value = idx2val(vm, idx);
    int type = val2type(*value);
    return vm->statics[type]->storage;
}



/* Convert type tag into name */
SK_API const char* sk_tagname(const VM* vm, TypeTag type)
{
    return vm->statics[type]->storage;
}



/* Check if the value on the stack at 'idx' is nil. */
SK_API int sk_isnil(const VM* vm, int idx)
{
    return IS_NIL(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is number. */
SK_API int sk_isnumber(const VM* vm, int idx)
{
    return IS_NUMBER(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is string. */
SK_API int sk_isstring(const VM* vm, int idx)
{
    return IS_STRING(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is bool. */
SK_API int sk_isbool(const VM* vm, int idx)
{
    return IS_BOOL(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is class. */
SK_API int sk_isclass(const VM* vm, int idx)
{
    return IS_CLASS(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is instance. */
SK_API int sk_isinstance(const VM* vm, int idx)
{
    return IS_INSTANCE(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is native C function. */
SK_API int sk_isnative(const VM* vm, int idx)
{
    return IS_NATIVE(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is bound method (instance method). */
SK_API int sk_ismethod(const VM* vm, int idx)
{
    return IS_BOUND_METHOD(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is skooma closure. */
SK_API int sk_isclosure(const VM* vm, int idx)
{
    return IS_CLOSURE(*idx2val(vm, idx));
}






/* Push nil on the stack */
SK_API void sk_pushnil(VM* vm)
{
    sk_lock(vm);
    skapi_pushnil(vm);
    sk_unlock(vm);
}



/* Push number on the stack */
SK_API void sk_pushnumber(VM* vm, sk_number number)
{
    sk_lock(vm);
    skapi_pushnum(vm, number);
    sk_unlock(vm);
}



/* Push string on the stack */
SK_API void sk_pushstring(VM* vm, const char* str, size_t len)
{
    sk_lock(vm);
    skapi_pushstr(vm, str, len);
    sk_unlock(vm);
}



/* Push cstring on the stack */
SK_API void sk_pushcstring(VM* vm, const char* str)
{
    sk_lock(vm);
    skapi_pushstr(vm, str, strlen(str));
    sk_unlock(vm);
}



/* Push formatted cstring on the stack */
SK_API const char* sk_pushfstring(VM* vm, const char* fmt, ...)
{
    const char* str;
    va_list argp;
    sk_lock(vm);
    va_start(argp, fmt);
    skapi_pushfstr(vm, fmt, argp);
    va_end(argp);
    sk_unlock(vm);
    return str;
}



/* Concatenate 2 strings on top of the stack, pop them off both
 * leaving the concatenated string on the top.
 * They are concatenated in the order they were pushed on the stack. */
SK_API const char* sk_concat(VM* vm)
{
    sk_lock(vm);
    skapi_checkelems(vm, 2);
    Value right = *idx2val(vm, -1);
    Value left = *idx2val(vm, -2);
    sk_checkapi(vm, IS_STRING(right) && IS_STRING(left), "expect strings");
    skapi_pushfstrva(vm, "%s%s", AS_CSTRING(left), AS_CSTRING(right));
    sk_unlock(vm);
}



/* Push boolean on the stack */
SK_API void sk_pushbool(VM* vm, int boolean)
{
    sk_lock(vm);
    sk_checkapi(vm, boolean == 0 || boolean == 1, "invalid boolean.");
    skapi_pushbool(vm, boolean);
    sk_unlock(vm);
}



/* Push C function (closure) on to the stack.
 * The 'args' is how many arguments this function expects (minimum),
 * 'isva' is a boolean value indicating if this function takes in
 * variable amount of arguments, 'upvals' is the number of
 * upvalues this C function (closure) has.
 * These upvalues are stored directly in this function and can
 * be accessed with the provided API in this source file.
 * This function will remove 'upvals' amount of values from the stack
 * and store them in this function. */
SK_API void sk_pushcfn(VM* vm, CFunction fn, int args, int isva, unsigned int upvals)
{
    sk_lock(vm);
    skapi_checkelems(vm, upvals);
    skapi_checkptr(vm, fn);
    ONative* native = ONative_new(vm, NULL, fn, args, isva, upvals);
    vm->sp -= upvals;
    while(upvals--)
        native->upvalue[upvals] = *(vm->sp + upvals);
    skapi_pushonative(vm, native);
    sk_unlock(vm);
}



/* Push value from the stack located at 'idx', on top of the stack */
SK_API void sk_push(VM* vm, int idx)
{
    sk_lock(vm);
    skapi_pushval(vm, *idx2val(vm, idx));
    sk_unlock(vm);
}



/* Push class method of an instance at idx on top of the stack.
 * If method doesn't exist this function invokes runtime error
 * with status 'S_EUDPROPERTY' (undefined property).
 * Class methods are all Skooma closures.
 * Once you push them on the stack they become Bound Methods
 * aka type of 'TT_METHOD'.
 * And this is because bound methods preserve the receiver (instance)
 * they belong to in order to access instance fields/methods. */
SK_API void sk_getmethod(VM* vm, int idx, const char* method)
{
    sk_lock(vm);
    skapi_checkptr(vm, method);
    Value val = *idx2val(vm, idx);
    sk_checkapi(vm, IS_INSTANCE(val), "expected class instance");
    skapi_pushstr(vm, method, strlen(method));
    bindmethod(vm, AS_INSTANCE(val)->oclass, *stackpeek(0), val);
    sk_unlock(vm);
}



/* Pushes the field value of the class instance at 'idx' on top
 * of the stack and returns the type of the value, or if the
 * field does not exist it returns 'TT_NONE' to indicate that. */
SK_API TypeTag sk_getfield(VM* vm, int idx, const char* field)
{
    int type;
    sk_lock(vm);
    skapi_checkptr(vm, field);
    Value insval = *idx2val(vm, idx);
    sk_checkapi(vm, IS_INSTANCE(insval), "expect class instance");
    OInstance* instance = AS_INSTANCE(insval);
    skapi_pushcstr(vm, field);
    Value fieldval;
    if(HashTable_get(&instance->fields, *stackpeek(0), &fieldval)) {
        skapi_pushval(vm, fieldval);
        type = val2type(fieldval);
    } else type = TT_NONE;
    sk_unlock(vm);
    return type;
}



/* Push global value on top of the stack.
 * In case global value was found, it will be on top of the stack,
 * and this function will return 1, otherwise nothing will be pushed
 * on the stack and the function will return 0. */
SK_API int sk_getglobal(VM* vm, const char* name)
{
    sk_lock(vm);
    skapi_checkptr(vm, name);
    int res = 0;
    Value gval;
    skapi_checkptr(vm, name);
    OString* str = OString_new(vm, name, strlen(name));
    if(HashTable_get(&vm->globids, OBJ_VAL(str), &gval)) {
        int idx = (int)AS_NUMBER(gval);
        skapi_pushval(vm, vm->globvars.data[idx].value);
        res = 1;
    }
    sk_unlock(vm);
    return res;
}



/* Get panic handler */
SK_API CFunction sk_getpanic(VM* vm)
{
    sk_lock(vm);
    CFunction panic_handler = vm->config.panic;
    sk_unlock(vm);
    return panic_handler;
}



/* Get allocator function */
SK_API AllocFn sk_getalloc(VM* vm, void** ud)
{
    sk_lock(vm);
    AllocFn alloc = vm->config.reallocate;
    if(ud) *ud = vm->config.userdata;
    sk_unlock(vm);
    return alloc;
}



/* Get boolean value (int 1/0) from the stack at 'idx'.
 * If the value at 'idx' is not a boolean, then the flag
 * if provided 'isbool' is set as 0, otherwise flag is set to 1. */
SK_API int sk_getbool(const VM* vm, int idx, int* isbool)
{
    int bval;
    Value val = *idx2val(vm, idx);
    int is = tobool(val, &bval);
    if(isbool) *isbool = is;
    return bval;
}



/* Get number value (sk_number) from the stack at 'idx'.
 * If the value at 'idx' is not a number, then the flag
 * if provided 'isnum' is set as 0, otherwise flag is set to 1. */
SK_API sk_number sk_getnumber(const VM* vm, int idx, int* isnum)
{
    sk_number nval = 0.0;
    Value val = *idx2val(vm, idx);
    int is = tonumber(val, &nval);
    if(isnum) *isnum = is;
    return nval;
}



/* Get string value from the stack at 'idx'.
 * Returns NULL (0) if the value is not a string.
 * Otherwise it returns pointer to the start of the string.
 * Returned pointer is 'const' indicating that user should not
 * modify the contents the pointer points to. */
SK_API const char* sk_getstring(const VM* vm, int idx)
{
    Value val = *idx2val(vm, idx);
    return IS_STRING(val) ? AS_CSTRING(val) : 0;
}



/* Return the number of values currently on the stack
 * relative to the current function */
SK_API int sk_gettop(const VM* vm)
{
    return cast_int(vm->sp - (last_frame(vm).callee + 1));
}



/* Copy value on the stack located at index 'src'
 * to value on the stack located at index 'dest'. */
SK_API void sk_copy(VM* vm, int src, int dest)
{
    sk_lock(vm);
    *idx2val(vm, dest) = *idx2val(vm, src);
    sk_unlock(vm);
}



/* Auxiliary to 'sk_rotate', reverses values from 'from' until 'to'. */
static force_inline void reverse(Value* from, Value* to)
{
    for(; from < to; from++, to--) {
        *from ^= *to;
        *to ^= *from;
        *from ^= *to;
    }
}



/**
 * This is basically a stack-array rotation between the
 * top of the stack and the index 'idx' for 'n' elements.
 * Negative '-n' indicates left-rotation, while positive
 * 'n' right-rotation.
 * The absolute value of 'n' must not be greater
 * than the array slice we are rotating.
 *
 * Example right-rotation:
 * - Before rotation:
 * [callee][0][1][2][3][4][sp]
 * - Do the rotation:
 * sk_rotate(vm, 2, 2);
 * - After right-rotation:
 * [callee][0][1][3][4][2][sp]
 *
 *
 * Example left-rotation:
 * - Before rotation:
 * [callee][0][1][2][3][4][sp]
 * - Do the rotation:
 * sk_rotate(vm, 2, -2);
 * -After left-rotation:
 * [callee][0][1][4][3][2][sp]
 **/
SK_API void sk_rotate(VM* vm, int idx, int n)
{
    sk_lock(vm);
    Value* end = stackpeek(0);
    Value* start = idx2val(vm, idx);
    sk_checkapi(vm, (n >= 0 ? n : -n) <= end - start + 1, "invalid 'n'");
    Value* pivot = (n >= 0 ? end - n : end - n - 1);
    reverse(pivot, start);
    reverse(pivot + 1, end);
    reverse(start, end);
    sk_unlock(vm);
}



/* Remove the value from the stack at 'idx' and shift the
 * stack to the left to fill the gap */
SK_API void sk_remove(VM* vm, int idx)
{
    sk_lock(vm);
    Value* val = idx2val(vm, idx);
    stackshift(vm, val, 0);
    closeupval(vm, vm->sp);
    skapi_decsp(vm);
    sk_unlock(vm);
}



/* Insert the value on top of the stack at the 'idx' and
 * shift the stack to the right to make space for the new value */
SK_API void sk_insert(VM* vm, int idx)
{
    sk_lock(vm);
    Value* top = vm->sp - 1;
    Value* val = idx2val(vm, idx);
    stackshift(vm, val, 1);
    *val = *top;
    skapi_incsp(vm);
    sk_unlock(vm);
}



/* Call the value on the stack (behind the arguments) */
SK_API void sk_call(VM* vm, int argc, int retcnt)
{
    sk_lock(vm);
    sk_checkapi(vm, retcnt >= SK_MULRET, "invalid return count");
    skapi_checkelems(vm, argc + 1);
    skapi_checkresults(vm, argc, retcnt);
    Value* fn = vm->sp - (argc + 1);
    ncall(vm, fn, *fn, retcnt);
    sk_unlock(vm);
}



/* Data used for 'fcall' */
struct CallStack {
    Value* callee;
    int retcnt;
};



/* Wrapper function */
static void fcall(VM* vm, void* userdata)
{
    struct CallStack* cs = cast(struct CallStack*, userdata);
    ncall(vm, cs->callee, *cs->callee, cs->retcnt);
}



/* Protected call.
 * Same as sk_call except this runs the function in protected
 * mode, meaning that in case the function errors it won't print
 * the error and abort or invoke panic handler.
 * Instead it restores the old call frame and returns the error
 * message and the status code in that order. */
SK_API int sk_pcall(VM* vm, int argc, int retcnt)
{
    sk_lock(vm);
    sk_checkapi(vm, retcnt >= SK_MULRET, "invalid return count");
    skapi_checkelems(vm, argc + 1);
    skapi_checkresults(vm, argc, retcnt);
    struct CallStack cs;
    cs.retcnt = retcnt;
    cs.callee = vm->sp - (argc + 1);
    int status = pcall(vm, fcall, &cs, save_stack(vm, cs.callee));
    sk_unlock(vm);
    return status;
}



SK_API void sk_dumpstack(VM* vm)
{
    // TODO: Implement (hint: dumpstack in debug.c)
    sk_lock(vm);
    sk_unlock(vm);
}



/* Sets the new stack top relative to the current function */
SK_API void sk_settop(VM* vm, int idx)
{
    sk_lock(vm);
    Value* newtop;
    Value* fn = vm->frames[vm->fc - 1].callee;
    ptrdiff_t diff;
    if(idx >= 0) {
        sk_checkapi(vm, idx < ((Value*)stklast(vm) - fn), "index too big.");
        diff = ((fn + 1) + idx) - vm->sp;
        for(; diff > 0; diff--)
            *vm->sp++ = NIL_VAL;
    } else { // index negative
        sk_checkapi(vm, -idx <= (vm->sp - fn), "invalid index.");
        diff = idx + 1;
    }
    vm->sp += diff; // set new top
    if(diff < 0) closeupval(vm, vm->sp);
    sk_unlock(vm);
}



/* Set global value 'name' to the value on top of the stack.
 * In case the global variable 'name' does not exist, then
 * the new one is declared and 'isfixed' modifier is considered
 * when creating it.
 * Otherwise 'isfixed' modifier is ignored and the global
 * variable is set to the new value UNLESS the variable was
 * previously defined, but this is up to the API user to ensure.
 * Meaning it won't get checked in the API call and might
 * cause bugs in user code if user doesn't respect this modifier. */
SK_API int sk_setglobal(VM* vm, const char* name, int isfixed)
{
    sk_lock(vm);
    skapi_checkelems(vm, 1);
    skapi_checkptr(vm, name);
    skapi_pushstr(vm, name, strlen(name));
    Value id = *stackpeek(0);
    Value newval = *stackpeek(1);
    int isnew = 0;
    Value gidx;
    if((isnew = !HashTable_get(&vm->globids, id, &gidx))) {
        Variable gvar = {newval, cast_uchar(0x00 & isfixed)};
        Value idx = NUMBER_VAL(cast(sk_number, Array_Variable_push(&vm->globvars, gvar)));
        HashTable_insert(vm, &vm->globids, id, idx);
        vm->sp -= 2;
    } else Array_Variable_index(&vm->globvars, AS_NUMBER(gidx))->value = newval;
    sk_unlock(vm);
    return isnew;
}



/* Set the field of the class instance to the value on top of the stack.
 * Class should be located at 'idx' and the name of the field to be set is 'field'.
 * This sets the field to that value and pops it off the top of the stack.
 * Returns true if the field didn't exist before and true if the value
 * of the field got overwritten. */
SK_API int sk_setfield(VM* vm, int idx, const char* field)
{
    sk_lock(vm);
    skapi_checkelems(vm, 1);
    skapi_checkptr(vm, field);
    Value insval = *idx2val(vm, idx);
    sk_checkapi(vm, IS_INSTANCE(insval), "expect class instance");
    OInstance* instance = AS_INSTANCE(insval);
    skapi_pushcstr(vm, field);
    int res = HashTable_insert(vm, &instance->fields, *stackpeek(0), *stackpeek(1));
    vm->sp -= 2;
    sk_unlock(vm);
    return res;
}



/* Set panic handler and return old one */
SK_API CFunction sk_setpanic(VM* vm, CFunction panicfn)
{
    sk_lock(vm);
    CFunction old_handler = vm->config.panic;
    vm->config.panic = panicfn;
    sk_unlock(vm);
    return old_handler;
}



/* Set allocator function and return old one */
SK_API AllocFn sk_setalloc(VM* vm, AllocFn allocfn, void* ud)
{
    sk_lock(vm);
    skapi_checkptr(vm, allocfn);
    AllocFn old_alloc = vm->config.reallocate;
    vm->config.reallocate = allocfn;
    vm->config.userdata = ud;
    sk_unlock(vm);
    return old_alloc;
}



/* Convert a value on the stack at 'idx' into string. */
SK_API const char* sk_tostring(VM* vm, int idx)
{
    sk_lock(vm);
    Value* slot = idx2val(vm, idx);
    Value value = *slot;
    if(!IS_STRING(value)) *slot = OBJ_VAL(vtostr(vm, value));
    sk_unlock(vm);
    return AS_CSTRING(*slot);
}



/* Auxiliary function for 'sk_getupval' and 'sk_setupval'.
 * Returns pointer to the upvalue. */
static force_inline Value* getupval(Value fn, int n)
{
    if(IS_CLOSURE(fn)) { // Skooma closure ?
        OClosure* closure = AS_CLOSURE(fn);
        if(cast_uint(n) > closure->upvalc - 1) return NULL;
        return closure->upvalue[n]->location;
    } else if(IS_NATIVE(fn)) { // native C function ?
        ONative* native = AS_NATIVE(fn);
        if(cast_uint(n) > native->upvalc - 1) return NULL;
        return &native->upvalue[n];
    } else return NULL;
}



/* Get upvalue belonging to the function at 'fidx'.
 * 'idx' is the index of the upvalue.
 * Function pushes the upvalue on top of the stack and returns 1.
 * If the upvalue does not exist and/or the function at 'fidx' is not
 * a closure or native C function then nothing will be pushed on
 * the stack and 0 is returned. */
SK_API int sk_getupvalue(VM* vm, int fidx, int idx)
{
    sk_lock(vm);
    int ret = 0;
    Value fn = *idx2val(vm, fidx);
    Value* upval = getupval(fn, idx);
    if(upval) {
        skapi_pushval(vm, *upval);
        ret = 1;
    }
    sk_unlock(vm);
    return ret;
}



/* Sets the upvalue belonging to the function at 'fidx'.
 * 'idx' is the index of the upvalue.
 * Function pops the top value on the stack and sets
 * it as the new value of the upvalue.
 * If the upvalue doesn't exist and/or 'fidx' is not a
 * closure or native C function, then the function returns 0
 * indicating the upvalue was not set, otherwise it returns 1. */
SK_API int sk_setupvalue(VM* vm, int fidx, int idx)
{
    sk_lock(vm);
    skapi_checkelems(vm, 1);
    int changed = 0;
    Value fn = *idx2val(vm, fidx);
    Value* upval = getupval(fn, idx);
    if(upval) {
        *upval = *stackpeek(0);
        vm->sp--;
        changed = 1;
    }
    sk_unlock(vm);
    return changed;
}



/* Return the length of the string at 'idx'. */
SK_API size_t sk_strlen(const VM* vm, int idx)
{
    Value val = *idx2val(vm, idx);
    sk_checkapi(vm, IS_STRING(val), "expect string");
    return AS_STRING(val)->len;
}



/* Invoke a runetime error with errcode */
SK_API int sk_error(VM* vm, Status errcode)
{
    sk_lock(vm);
    Value* errobj = stackpeek(0);
    skapi_checkelems(vm, 1);
    skapi_checkerrcode(vm, errcode);
    runerror(vm, errcode); // sk_unlock in here
    return 0; // to avoid compiler warnings
}



/* Compare 2 values on the stack */
SK_API int sk_compare(VM* vm, int idx1, int idx2, Cmp op)
{
    sk_lock(vm);
    Value a = *idx2val(vm, idx1);
    Value b = *idx2val(vm, idx2);
    int res;
    switch(op) {
        case CMP_EQ:
            res = veq(vm, a, b);
            break;
        case CMP_LT:
            res = vlt(vm, a, b);
            break;
        case CMP_GT:
            res = vgt(vm, a, b);
            break;
        case CMP_LE:
            res = vle(vm, a, b);
            break;
        case CMP_GE:
            res = vge(vm, a, b);
            break;
        default:
            sk_checkapi(vm, 0, "invalid comparison 'op'");
    }
    sk_unlock(vm);
    return res;
}



SK_API void sk_arith(VM* vm, Ar op)
{
    sk_lock(vm);
    skapi_checkarop(vm, op);
    int adjust = 0;
    if(arisbin(op)) {
        skapi_checkelems(vm, 2);
        adjust = 1;
    } else skapi_checkelems(vm, 1);
    tryvarithm(vm, *stackpeek(1), *stackpeek(0), op, stackpeek(0));
    vm->sp -= adjust; // result is where the first operand was
    sk_unlock(vm);
}



/* Return 'skooma' current version. */
SK_API int sk_version(VM* vm)
{
    UNUSED(vm);
    return SK_VERSION_NUMBER;
}
