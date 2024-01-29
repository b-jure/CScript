#include "err.h"
#include "object.h"
#include "parser.h"
#include "reader.h"
#include "skapi.h"
#include "skconf.h"
#include "skooma.h"
#include "stdarg.h"
#include "value.h"
#include "vmachine.h"

#include <time.h>


/* Get stack value at 'idx'. */
static force_inline Value* idx2val(const VM* vm, sk_int idx)
{
    Value* fn = last_frame(vm).callee;
    if(idx >= 0) {
        skapi_check(vm, idx < vm->sp - 1 - fn, "index too big.");
        return (fn + 1 + idx);
    } else { // idx is negative
        skapi_check(vm, -idx <= (vm->sp - fn), "Invalid index.");
        return (vm->sp + idx);
    }
}



/* Ensure the stack has enough space. */
SK_API sk_int sk_ensurestack(VM* vm, sk_int n)
{
    skapi_check(vm, n >= 0, "negative 'n'.");
    return (((vm->sp - vm->stack) + n) <= VM_STACK_LIMIT);
}



/* Create and allocate VM by providing your own 'allocator'.
 * In case the NULL pointer is provided as 'allocator' and/or
 * allocation fails NULL is returned. */
SK_API VM* sk_create(AllocFn allocator, void* ud)
{
    if(unlikely(allocator == NULL)) return NULL;
    VM* vm = allocator(NULL, sizeof(VM), ud);
    if(unlikely(vm == NULL)) return NULL;
    memset(&vm->hooks, 0, sizeof(Hooks));
    vm->hooks.reallocate = allocator;
    vm->hooks.userdata = ud;
    vm->gc.gc_stopped = 1; // wait until VM is initialized
    VM_init(vm);
    vm->gc.gc_stopped = 0;
    return vm;
}


/* Resets 'VM' clearing its call stack and closing all
 * to be closed variables. */
SK_API void sk_resetvm(VM* vm)
{
    resetvm(vm, vm->status);
}


/* Free the VM allocation, the pointer to VM will be nulled out. */
SK_API void sk_destroy(VM** vmp)
{
    if(likely(vmp != NULL)) { // non-null pointer ?
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
        Array_OSRef_free(&vm->interned, NULL);
        HashTable_free(vm, &vm->weakrefs);
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
 * For example: if there are 5 values on the stack after the
 * callee then -1 would be index 4. */
SK_API sk_uint sk_absidx(VM* vm, sk_int idx)
{
    return (idx >= 0) ? idx : cast_int(vm->sp - last_frame(vm).callee - 1) + idx;
}



/* Return type of the value on the stack at 'idx'. */
SK_API TypeTag sk_type(const VM* vm, sk_int idx)
{
    Value* value = idx2val(vm, idx);
    return val2type(*value);
}



/* Return type name of the value on the stack at 'idx'.
 * This returned pointer is 'const' indicating the
 * memory it points to should not be modified. */
SK_API const char* sk_typename(const VM* vm, sk_int idx)
{
    Value* value = idx2val(vm, idx);
    sk_int type = val2type(*value);
    return vm->faststatic[type]->storage;
}



/* Convert type tag into name */
SK_API const char* sk_tagname(const VM* vm, TypeTag type)
{
    return vm->faststatic[type]->storage;
}



/* Check if the value on the stack at 'idx' is nil. */
SK_API sk_byte sk_isnil(const VM* vm, sk_int idx)
{
    return IS_NIL(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is number. */
SK_API sk_byte sk_isnumber(const VM* vm, sk_int idx)
{
    return IS_NUMBER(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is string. */
SK_API sk_byte sk_isstring(const VM* vm, sk_int idx)
{
    return IS_STRING(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is bool. */
SK_API sk_byte sk_isbool(const VM* vm, sk_int idx)
{
    return IS_BOOL(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is class. */
SK_API sk_byte sk_isclass(const VM* vm, sk_int idx)
{
    return IS_CLASS(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is instance. */
SK_API sk_byte sk_isinstance(const VM* vm, sk_int idx)
{
    return IS_INSTANCE(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is native C function. */
SK_API sk_byte sk_isnative(const VM* vm, sk_int idx)
{
    return IS_NATIVE(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is bound method (instance method). */
SK_API sk_byte sk_ismethod(const VM* vm, sk_int idx)
{
    return IS_BOUND_METHOD(*idx2val(vm, idx));
}



/* Check if the value on the stack at 'idx' is skooma closure. */
SK_API sk_byte sk_isclosure(const VM* vm, sk_int idx)
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



/* Push formatted cstring on the stack, format arguments
 * start from 'argp'. */
SK_API const char* sk_pushvfstring(VM* vm, const char* fmt, va_list argp)
{
    const char* str = NULL;
    sk_lock(vm);
    skapi_pushfstr(vm, fmt, argp);
    str = AS_CSTRING(*stackpeek(0));
    sk_unlock(vm);
    return str;
}



/* Push formatted cstring on the stack */
SK_API const char* sk_pushfstring(VM* vm, const char* fmt, ...)
{
    const char* str = NULL;
    va_list argp;
    sk_lock(vm);
    va_start(argp, fmt);
    skapi_pushfstr(vm, fmt, argp);
    va_end(argp);
    str = AS_CSTRING(*stackpeek(0));
    sk_unlock(vm);
    return str;
}



/* Concatenate 2 strings on top of the stack.
 * Pops the string on top of the stack and replaces the first
 * one with the concatenated string.
 * They are concatenated in the order they were pushed on the stack. */
SK_API const char* sk_concat(VM* vm)
{
    sk_lock(vm);
    skapi_checkelems(vm, 2);
    Value right = *stackpeek(0);
    Value left = *stackpeek(1);
    skapi_check(vm, IS_STRING(right) && IS_STRING(left), "expect strings");
    concatonstack(vm);
    const char* concated = AS_CSTRING(*stackpeek(0));
    sk_unlock(vm);
    return concated;
}



/* Push boolean on the stack */
SK_API void sk_pushbool(VM* vm, sk_int boolean)
{
    sk_lock(vm);
    skapi_check(vm, boolean == 0 || boolean == 1, "invalid boolean.");
    skapi_pushbool(vm, boolean);
    sk_unlock(vm);
}



/* Push C closure on to the stack.
 * The 'args' is how many arguments this function expects (minimum),
 * 'isvararg' is a boolean value indicating if this function takes in
 * variable amount of arguments, 'upvals' is the number of
 * upvalues this C closure has.
 * These upvalues are stored directly in this function and can
 * be accessed with the provided API in this header file.
 * This function will remove 'upvals' amount of values from the stack
 * and store them in C closure. */
SK_API void sk_pushcclosure(VM* vm, CFunction fn, sk_uint args, sk_byte isvararg, sk_uint upvals)
{
    sk_lock(vm);
    skapi_checkelems(vm, upvals);
    skapi_checkptr(vm, fn);
    ONative* native = ONative_new(vm, NULL, fn, args, isvararg, upvals);
    vm->sp -= upvals;
    while(upvals--)
        native->upvalue[upvals] = *(vm->sp + upvals);
    skapi_pushonative(vm, native);
    sk_unlock(vm);
}



/* Push value from the stack located at 'idx', on top of the stack */
SK_API void sk_push(VM* vm, sk_int idx)
{
    sk_lock(vm);
    Value* val = idx2val(vm, idx);
    skapi_pushval(vm, *val);
    sk_unlock(vm);
}



/* Push class method of an instance at idx on top of the stack.
 * If method doesn't exist this function returns 0 otherwise 1.
 * Note: Class instance methods are all Skooma closures. */
SK_API uint8_t sk_getmethod(VM* vm, sk_int idx, const char* method)
{
    sk_lock(vm);
    skapi_checkptr(vm, method);
    Value val = *idx2val(vm, idx);
    if(!IS_INSTANCE(val)) return 0;
    skapi_pushstr(vm, method, strlen(method));
    uint8_t haveit = bindmethod(vm, AS_INSTANCE(val)->oclass, *stackpeek(0), val);
    sk_unlock(vm);
    return haveit;
}



/* Pushes the field value of the class instance at 'idx' on top
 * of the stack.
 * If field value was not found or the value at 'idx' is not
 * class instance return 0, otherwise 1. */
SK_API sk_byte sk_getfield(VM* vm, sk_int idx, const char* field)
{
    sk_byte res = 0;
    sk_lock(vm);
    skapi_checkptr(vm, field);
    Value insval = *idx2val(vm, idx);
    if(IS_INSTANCE(insval)) {
        OInstance* instance = AS_INSTANCE(insval);
        Value key = OBJ_VAL(OString_new(vm, field, strlen(field)));
        Value fieldval;
        if((res = rawget(&instance->fields, key, &fieldval))) skapi_pushval(vm, fieldval);
    }
    sk_unlock(vm);
    return res;
}



/* Get value on the stack at 'idx' and use index
 * operator '[]' (__getidx__) on it using the
 * value on top of the stack as index value.
 * If the value is not an instance or the
 * '__getidx__' is not overloaded, this function
 * returns 0, otherwise 1 and the value will
 * be on top of the stack. */
SK_API sk_byte sk_getindex(VM* vm, sk_int idx)
{
    sk_byte res = 0;
    sk_lock(vm);
    skapi_checkelems(vm, 1); // [index]
    Value* index = stackpeek(0);
    res = callomgetidx(vm, *idx2val(vm, idx));
    *index = *--vm->sp; // replace [index] with result
    sk_unlock(vm);
    return res;
}



/* Get value on the stack at 'idx' and use index
 * operator '[]' (__setidx__) on it, index value
 * is located one place before the top of the stack
 * while the value getting assigned is on top of the stack.
 * If the value is not an instance or '__setidx__' is not
 * overloaded, this function returns 0, otherwise 1. */
SK_API sk_byte sk_setindex(VM* vm, sk_int idx)
{
    sk_byte res = 0;
    sk_lock(vm);
    skapi_checkelems(vm, 2); // [index][expr]
    res = callomsetidx(vm, *idx2val(vm, idx));
    vm->sp -= 2; // pop [index] and [expr]
    sk_unlock(vm);
    return res;
}



/* Performs 'raw' index operation, meaning it doesn't invoke
 * overloaded methods when getting/setting the instance property.
 * 'what' parameter if it is a zero means we are setting and
 * non-zero 'what' means we are getting the value at that index.
 * In case we are setting the indexed value then the 'value' we are
 * assigning will be on top of the stack and the 'index' value right
 * below it; if we are getting the value then the 'key' will be on top
 * of the stack.
 * If the operation was successful then 1 is returned; otherwise 0.
 * @ERR: if value we are indexing with is 'nil'. */
SK_API sk_byte sk_rawindex(VM* vm, sk_int idx, uint8_t what)
{
    sk_byte res = 0;
    sk_lock(vm);
    skapi_checkelems(vm, what == SK_RAWSET ? 2 : 1);
    Value value = *idx2val(vm, idx);
    if(!IS_INSTANCE(value)) return res;
    res = rawindex(vm, value, what);
    sk_unlock(v);
    return res;
}



/* Push global value on top of the stack.
 * In case global value was found, it will be on top of the stack,
 * and this function will return 1, otherwise nothing will be pushed
 * on the stack and the function will return 0. */
SK_API sk_byte sk_getglobal(VM* vm, const char* name)
{
    sk_lock(vm);
    skapi_checkptr(vm, name);
    sk_int res = 0;
    Value gval;
    skapi_checkptr(vm, name);
    OString* str = OString_new(vm, name, strlen(name));
    if(rawget(&vm->globids, OBJ_VAL(str), &gval)) {
        sk_int idx = (sk_int)AS_NUMBER(gval);
        skapi_pushval(vm, vm->globvars.data[idx].value);
        res = 1;
    }
    sk_unlock(vm);
    return res;
}



/* Get panic handler */
SK_API PanicFn sk_getpanic(VM* vm)
{
    sk_lock(vm);
    PanicFn panic_handler = vm->hooks.panic;
    sk_unlock(vm);
    return panic_handler;
}



/* Get reader function */
SK_API ReadFn sk_getreader(VM* vm)
{
    sk_lock(vm);
    ReadFn reader = vm->hooks.reader;
    sk_unlock(vm);
    return reader;
}



/* Get allocator function */
SK_API AllocFn sk_getalloc(VM* vm, void** ud)
{
    sk_lock(vm);
    AllocFn alloc = vm->hooks.reallocate;
    if(ud) *ud = vm->hooks.userdata;
    sk_unlock(vm);
    return alloc;
}



/* Get boolean value (sk_int 1/0) from the stack at 'idx'.
 * If the value at 'idx' is not a boolean, then the flag
 * if provided 'isbool' is set as 0, otherwise flag is set to 1. */
SK_API sk_byte sk_getbool(const VM* vm, sk_int idx, sk_byte* isbool)
{
    sk_byte bval;
    Value val = *idx2val(vm, idx);
    sk_byte is = tobool(val, &bval);
    if(isbool) *isbool = is;
    return bval;
}



/* Get number value (sk_number) from the stack at 'idx'.
 * If the value at 'idx' is not a number, then the flag
 * if provided 'isnum' is set as 0, otherwise flag is set to 1. */
SK_API sk_number sk_getnumber(const VM* vm, sk_int idx, sk_byte* isnum)
{
    sk_number nval = 0.0;
    Value val = *idx2val(vm, idx);
    sk_byte is = tonumber(val, &nval);
    if(isnum) *isnum = is;
    return nval;
}



/* Get string value from the stack at 'idx'.
 * Returns NULL (0) if the value is not a string.
 * Otherwise it returns pointer to the start of the string.
 * Returned pointer is 'const' indicating that user should not
 * modify the contents the pointer points to. */
SK_API const char* sk_getstring(const VM* vm, sk_int idx)
{
    Value val = *idx2val(vm, idx);
    return IS_STRING(val) ? AS_CSTRING(val) : NULL;
}



/* Get native C function from the stack at 'idx'.
 * Return NULL if the value is not a 'CFunction'. */
SK_API CFunction sk_getcfunction(const VM* vm, sk_int idx)
{
    Value val = *idx2val(vm, idx);
    return IS_NATIVE(val) ? AS_NATIVE(val)->fn : NULL;
}



/* Return the number of values currently on the stack
 * relative to the current function */
SK_API sk_uint sk_gettop(const VM* vm)
{
    return cast_int(vm->sp - (last_frame(vm).callee + 1));
}



/* Copy value on the stack located at index 'src'
 * to value on the stack located at index 'dest'. */
SK_API void sk_copy(VM* vm, sk_int src, sk_int dest)
{
    sk_lock(vm);
    Value* from = idx2val(vm, src);
    Value* to = idx2val(vm, dest);
    *to = *from;
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



/*
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
 */
SK_API void sk_rotate(VM* vm, sk_int idx, sk_int n)
{
    sk_lock(vm);
    Value* end = stackpeek(0);
    Value* start = idx2val(vm, idx);
    skapi_check(vm, (n >= 0 ? n : -n) <= end - start + 1, "invalid 'n'");
    Value* pivot = (n >= 0 ? end - n : end - n - 1);
    reverse(pivot, start);
    reverse(pivot + 1, end);
    reverse(start, end);
    sk_unlock(vm);
}




/* Call the value on the stack with 'argc' arguments. */
SK_API void sk_call(VM* vm, sk_int argc, sk_int retcnt)
{
    sk_lock(vm);
    skapi_check(vm, retcnt >= SK_MULRET, "invalid return count");
    skapi_checkelems(vm, argc + 1);
    skapi_checkresults(vm, argc, retcnt);
    Value* fn = vm->sp - (argc + 1);
    ncall(vm, fn, *fn, retcnt);
    sk_unlock(vm);
}



/* Data used for 'fcall' */
struct CallData {
    Value* callee;
    sk_int retcnt;
};


/* Wrapper function */
static void fcall(VM* vm, void* userdata)
{
    struct CallData* cd = cast(struct CallData*, userdata);
    ncall(vm, cd->callee, *cd->callee, cd->retcnt);
}


/* Protected call.
 * Same as sk_call except this runs the function in protected
 * mode, meaning that in case the function errors it won't print
 * invoke panic handler.
 * Instead it restores the old call frame and pushes the error object
 * on top of the stack.
 * This function returns 'Status' [defined @skooma.h] code. */
SK_API Status sk_pcall(VM* vm, sk_int argc, sk_int retcnt)
{
    sk_lock(vm);
    skapi_check(vm, retcnt >= SK_MULRET, "invalid return count");
    skapi_checkelems(vm, argc + 1);
    skapi_checkresults(vm, argc, retcnt);
    struct CallData cd;
    cd.retcnt = retcnt;
    cd.callee = vm->sp - (argc + 1);
    sk_int status = pcall(vm, fcall, &cd, save_stack(vm, cd.callee));
    sk_unlock(vm);
    return status;
}



/*
 * Loads (compiles) skooma script using provided 'reader'.
 * Returns 'Status' [defined @skooma.h] code.
 * If the script compiled without any errors then the compiled
 * function (Skooma closure) gets pushed on top of the stack.
 * In case there were any compile errors, then the error object
 * gets pushed on top of the stack (error message).
 *
 * 'reader' - user provided 'ReadFn' responsible for reading
 *            the '.sk' source file.
 *            Refer to 'ReadFn' in [@skooma.h] for more
 *            information on how this reader should 'behave'.
 * 'userdata' - user provided data for 'reader'.
 * 'source' - name of the skooma script you are loading.
 */
SK_API Status sk_load(VM* vm, ReadFn reader, void* userdata, const char* source)
{
    BuffReader br;
    sk_lock(vm);
    BuffReader_init(vm, &br, reader, userdata);
    uint8_t status = pcompile(vm, &br, source, 0);
    sk_unlock(vm);
    return status;
}



SK_API size_t sk_gc(VM* vm, GCOpt option, ...)
{
    va_list argp;
    size_t res = 0;
    sk_lock(vm);
    va_start(argp, option);
    switch(option) {
        case GCO_STOP: vm->gc.gc_stopped = 1; break;
        case GCO_RESTART: vm->gc.gc_stopped = 0; break;
        case GCO_COLLECT: gc(vm); break;
        case GCO_COUNT: res = vm->gc.gc_allocated; break;
        case GCO_ISRUNNING: res = (vm->gc.gc_stopped == 0); break;
        case GCO_NEXTGC: res = vm->gc.gc_nextgc; break;
    }
    va_end(argp);
    sk_unlock(vm);
    return res;
}





SK_API void sk_dumpstack(VM* vm)
{
    // TODO: Implement (hint: dumpstack in debug.c)
    sk_lock(vm);
    sk_unlock(vm);
}



/* Sets the new stack top relative to the current function */
SK_API void sk_settop(VM* vm, sk_int idx)
{
    sk_lock(vm);
    Value* newtop;
    Value* fn = vm->frames[vm->fc - 1].callee;
    ptrdiff_t diff;
    if(idx >= 0) {
        skapi_check(vm, idx < ((Value*)stklast(vm) - fn), "index too big.");
        diff = ((fn + 1) + idx) - vm->sp;
        for(; diff > 0; diff--)
            *vm->sp++ = NIL_VAL;
    } else { // index negative
        skapi_check(vm, -idx <= (vm->sp - fn), "invalid index.");
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
SK_API sk_byte sk_setglobal(VM* vm, const char* name, sk_int isfixed)
{
    sk_lock(vm);
    skapi_checkelems(vm, 1); // value must be present
    skapi_checkptr(vm, name);
    Value newval = *stackpeek(0);
    Value key = OBJ_VAL(OString_new(vm, name, strlen(name)));
    sk_byte isnew = 0;
    Value gidx;
    if((isnew = !rawget(&vm->globids, key, &gidx))) {
        skapi_pushval(vm, key);
        Variable gvar = {newval, cast_uchar(0x01 & isfixed)};
        Value idx = NUMBER_VAL(Array_Variable_push(&vm->globvars, gvar));
        HashTable_insert(vm, &vm->globids, key, idx);
        vm->sp -= 2; // pop value and key
    } else {
        Array_Variable_index(&vm->globvars, AS_NUMBER(gidx))->value = newval;
        vm->sp--; // pop value
    }
    sk_unlock(vm);
    return isnew;
}



/* Set the field of the class instance to the value on top of the stack.
 * Class should be located at 'idx' and the name of the field to be set is 'field'.
 * This sets the field to that value and pops it off the top of the stack.
 * Returns 1 if the field didn't exist before or false if the value
 * of the field got overwritten. */
SK_API sk_byte sk_setfield(VM* vm, sk_int idx, const char* field)
{
    sk_lock(vm);
    skapi_checkelems(vm, 1);
    skapi_checkptr(vm, field);
    Value insval = *idx2val(vm, idx);
    skapi_check(vm, IS_INSTANCE(insval), "expect class instance");
    OInstance* instance = AS_INSTANCE(insval);
    skapi_pushcstr(vm, field);
    sk_byte res = HashTable_insert(vm, &instance->fields, *stackpeek(0), *stackpeek(1));
    vm->sp -= 2; // pop value and key
    sk_unlock(vm);
    return res;
}



/* Set panic handler and return old one */
SK_API PanicFn sk_setpanic(VM* vm, PanicFn panicfn)
{
    sk_lock(vm);
    PanicFn old_panic = vm->hooks.panic;
    vm->hooks.panic = panicfn;
    sk_unlock(vm);
    return old_panic;
}



/* Set read function and return the old one */
SK_API ReadFn sk_setreader(VM* vm, ReadFn readfn)
{
    sk_lock(vm);
    ReadFn old_reader = vm->hooks.reader;
    vm->hooks.reader = readfn;
    sk_unlock(vm);
    return old_reader;
}



/* Set allocator function and return old one */
SK_API AllocFn sk_setalloc(VM* vm, AllocFn allocfn, void* ud)
{
    sk_lock(vm);
    skapi_checkptr(vm, allocfn);
    AllocFn old_alloc = vm->hooks.reallocate;
    vm->hooks.reallocate = allocfn;
    vm->hooks.userdata = ud;
    sk_unlock(vm);
    return old_alloc;
}



/* Convert a value on the stack at 'idx' into string. */
SK_API const char* sk_stringify(VM* vm, sk_int idx)
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
static force_inline Value* getupval(Value fn, sk_int n)
{
    if(IS_CLOSURE(fn)) { // Skooma closure ?
        OClosure* closure = AS_CLOSURE(fn);
        if(cast_uint(n) > closure->fn->p.upvalc - 1) return NULL;
        return closure->upvalue[n]->location;
    } else if(IS_NATIVE(fn)) { // native C function ?
        ONative* native = AS_NATIVE(fn);
        if(cast_uint(n) > native->p.upvalc - 1) return NULL;
        return &native->upvalue[n];
    } else return NULL;
}



/* Get upvalue belonging to the function at 'fidx'.
 * 'idx' is the index of the upvalue.
 * Function pushes the upvalue on top of the stack and returns 1.
 * If the upvalue does not exist and/or the function at 'fidx' is not
 * a closure or native C function then nothing will be pushed on
 * the stack and 0 is returned. */
SK_API sk_int sk_getupvalue(VM* vm, sk_int fidx, sk_int idx)
{
    sk_lock(vm);
    sk_int ret = 0;
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
SK_API sk_int sk_setupvalue(VM* vm, sk_int fidx, sk_int idx)
{
    sk_lock(vm);
    skapi_checkelems(vm, 1);
    sk_int changed = 0;
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



/* Get the next property of the instance located at 'idx' on the stack.
 * The 'key' value (string) used for lookup is on top of the stack.
 * 'what' parameter determines which next property we need,
 * 0 means instance field otherwise instance method.
 * This function returns 1 if there is next property and
 * the value on top of the stack (key) is replaced with the
 * next key; additionally value associated with that key is
 * also pushed on top of the stack.
 * If there is no next property 0 is returned and stack
 * remains unchanged. */
SK_API sk_byte sk_nextproperty(VM* vm, sk_int idx, sk_byte what)
{
    sk_byte hasnext = 0;
    sk_lock(vm);
    skapi_checkelems(vm, 2); // key and instance
    skapi_checkstack(vm, 1); // for the value
    Value value = *idx2val(vm, idx);
    Value* key = stackpeek(0);
    skapi_check(vm, IS_INSTANCE(value), "Expect instance");
    OInstance* instance = AS_INSTANCE(value);
    HashTable* table = rawgettable(vm, instance, what);
    hasnext = HashTable_next(vm, table, key);
    if(hasnext) skapi_incsp(vm);
    else vm->sp--;
    sk_unlock(vm);
    return hasnext;
}



/* Return the length of the string at 'idx'.
 * If the value is not a string then return 0. */
SK_API size_t sk_strlen(const VM* vm, sk_int idx)
{
    Value val = *idx2val(vm, idx);
    return (IS_STRING(val) ? AS_STRING(val)->len : 0);
}


/* Return 'VM' 'Status' code. */
SK_API Status sk_getstatus(VM* vm)
{
    UNUSED(vm);
    return vm->status;
}


/* Invoke a runetime error with errcode */
SK_API sk_int sk_error(VM* vm, Status errcode)
{
    sk_lock(vm);
    Value* errobj = stackpeek(0);
    skapi_checkelems(vm, 1);
    skapi_checkerrcode(vm, errcode);
    runerror(vm, errcode); // sk_unlock in here
    return 0; // to avoid compiler warnings
}



/* Apply ordering on 2 values on the stack.
 * First both values are pushed on top of the stack (idx1 then idx2)
 * and ordering is applied.
 * This functions is free to call overloaded operator methods.
 * Result in placed in place of first operand and the second operand
 * is popped off.
 * Returned value of 1 means ordering applied is true, otherwise 0 is returned. */
SK_API sk_byte sk_compare(VM* vm, sk_int idx1, sk_int idx2, Ord ord)
{
    sk_lock(vm);
    skapi_checkordop(vm, ord);
    skapi_checkstack(vm, 2);
    Value l = *idx2val(vm, idx1);
    Value r = *idx2val(vm, idx2);
    *vm->sp++ = l; // push left operand
    *vm->sp++ = r; // push right operand
    switch(ord) {
        case ORD_EQ: veq(vm, l, r); break;
        case ORD_NE: vne(vm, l, r); break;
        case ORD_LT: vlt(vm, l, r); break;
        case ORD_GT: vgt(vm, l, r); break;
        case ORD_LE: vle(vm, l, r); break;
        case ORD_GE: vge(vm, l, r); break;
        default: unreachable;
    }
    sk_byte res = !ISFALSE(*stackpeek(0));
    sk_unlock(vm);
    return res;
}



/* Perform equality ordering on values at stack index 'idx1' and 'idx2'.
 * This function will not call overload-able operator methods (__eq__).
 * Result is returned directly without storing it on the stack.
 * Returned value of 1 means values are equal, otherwise 0 is returned. */
SK_API sk_byte sk_rawequal(VM* vm, sk_int idx1, sk_int idx2)
{
    sk_lock(vm);
    Value l = *idx2val(vm, idx1);
    Value r = *idx2val(vm, idx2);
    sk_byte res = raweq(l, r);
    sk_unlock(vm);
    return res;
}



/* Perform arithmetic 'op' on values on
 * top of the stack.
 * If 'op' is unary operation then the value on top
 * of the stack is considered as operand.
 * If 'op' is binary operation then the 2 values
 * on top of the stack are considered as operands.
 * This function is free to call overload-able operator methods.
 * Result is pushed on top of the stack in place of the
 * first operand and second operand is popped of. */
SK_API void sk_arith(VM* vm, Ar op)
{
    sk_lock(vm);
    skapi_checkarop(vm, op);
    sk_int adjust = 0;
    if(arisbin(op)) {
        skapi_checkelems(vm, 2);
        adjust = 1;
    } else skapi_checkelems(vm, 1);
    Value* res = stackpeek(1);
    arith(vm, *res, *stackpeek(0), op, res);
    vm->sp -= adjust; // result is where the first operand was
    sk_unlock(vm);
}



/* Return current version as 'sk_number'. */
SK_API sk_number sk_version(VM* vm)
{
    UNUSED(vm);
    return cast(sk_number, SK_VERSION_NUMBER);
}
