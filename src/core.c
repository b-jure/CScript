#include "core.h"
#include "debug.h"
#include "err.h"
#include "object.h"
#include "skconf.h"
#include "skmath.h"
#include "value.h"
#include "vmachine.h"

#include <ctype.h>
#include <fcntl.h>
#include <time.h>
#include <unistd.h>

sstatic force_inline ObjString*
ObjString_from_static_prefix(VM* vm, ObjString* string, const InternedString* staticstr)
{
    UInt len = string->len + staticstr->len;
    char buffer[len + 1];

    memcpy(buffer, staticstr->name, staticstr->len);
    memcpy(buffer + staticstr->len, string->storage, string->len);
    buffer[len] = '\0';

    return ObjString_from(vm, buffer, len);
}

//------------------------- snative -------------------------//

/**
 * Determines processor time.
 * @ret - approximation of processor time used by the program in seconds.
 * @err - if the processor time used is not available or its value cannot
 *        be represented.
 **/
snative(clock)
{
    UNUSED(argc);
    clock_t time = clock();

    if(likely(time != -1)) {
        argv[-1] = NUMBER_VAL((double)time / CLOCKS_PER_SEC);
        return true;
    }

    argv[-1] = OBJ_VAL(ERR_NEW(vm, CLOCK_ERR));
    return false;
}

/**
 * Checks if ObjInstance contains field.
 * @ret - bool, true if it contains, false otherwise
 * @err - if first argument is not ObjInstance
 *      - if second argument is not ObjString
 **/
snative(isfield)
{
    UNUSED(argc);
    Value      receiver = argv[0];
    Value      field    = argv[1];
    ObjString* err      = NULL;

    if(unlikely(!IS_INSTANCE(receiver))) {
        err = ERR_NEW(vm, ISFIELD_INSTANCE_ERR);
    } else if(unlikely(!IS_STRING(field))) {
        err = ERR_NEW(vm, ISFIELD_FIELD_TYPE_ERR);
    } else {
        goto fin;
    }
    argv[-1] = OBJ_VAL(err);
    return false;

fin:;
    Value _dummy;
    argv[-1] = BOOL_VAL(HashTable_get(&AS_INSTANCE(receiver)->fields, field, &_dummy));
    return true;
}

/**
 * Prints a value and a newline.
 * @ret - returns the 'value' printed
 **/
snative(printl)
{
    UNUSED(argc);
    UNUSED(vm);
    Value_print(argv[0]);
    printf("\n");
    argv[-1] = argv[0];
    return true;
}

/**
 * Prints a value.
 * @ret - returns the 'value' printed
 **/
snative(print)
{
    UNUSED(argc);
    UNUSED(vm);
    Value_print(argv[0]);
    argv[-1] = argv[0];
    return true;
}

/**
 * Converts the value into string.
 * @ret - returns 'string' of the value.
 **/
snative(tostr)
{
    UNUSED(argc);
    argv[-1] = OBJ_VAL(Value_to_str(vm, argv[0]));
    return true;
}

/**
 * Checks if the 'Value' is a string.
 * @ret - returns 'true' if the value is a string type,
 *        otherwise 'false'.
 **/
snative(isstr)
{
    UNUSED(argc);
    UNUSED(vm);
    argv[-1] = BOOL_VAL(IS_STRING(argv[0]));
    return true;
}

sstatic force_inline ObjString* typename(VM* vm, Value type)
{
    if(IS_NUMBER(type)) {
        return vm->statics[SS_NUM];
    } else if(IS_STRING(type)) {
        return vm->statics[SS_STR];
    } else if(IS_FUNCTION(type) || IS_BOUND_METHOD(type) || IS_CLOSURE(type) || IS_NATIVE(type)) {
        return vm->statics[SS_FUNC];
    } else if(IS_BOOL(type)) {
        return vm->statics[SS_BOOL];
    } else if(IS_NIL(type)) {
        return vm->statics[SS_NIL];
    } else if(IS_INSTANCE(type)) {
        return vm->statics[SS_INS];
    } else if(IS_CLASS(type)) {
        return vm->statics[SS_CLASS];
    }

    unreachable;
}

snative(typeof)
{
    UNUSED(argc);
    Value value = argv[0];

    if(IS_UPVAL(value)) {
        argv[-1] = OBJ_VAL(typename(vm, AS_UPVAL(value)->closed));
    } else {
        argv[-1] = OBJ_VAL(typename(vm, value));
    }

    return true;
}

/**
 * Returns the length of the string.
 * @err - if the value is not a string error is invoked,
 *        otherwise return string length (in bytes).
 **/
snative(strlen)
{
    UNUSED(argc);
    Value string = argv[0];
    if(unlikely(!IS_STRING(string))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, STRLEN_FIRST_ARG_TYPE_ERR));
        return false;
    }
    argv[-1] = NUMBER_VAL(AS_STRING(string)->len);
    return true;
}

/**
 * Looks for the first match of the 'pattern' in the string.
 * @ret - if it find a match it returns the index of where the
          pattern starts in the string (starting from 0), if
          pattern was not found it returns 'nil'.
          Invokes error if 'haystack' or a 'needle' (pattern) is not a string.
 **/
snative(strpat)
{
    UNUSED(argc);
    Value      string  = argv[0];
    Value      pattern = argv[1];
    ObjString* err     = NULL;
    if(unlikely(!IS_STRING(string))) {
        err = ERR_NEW(vm, STRPAT_FIRST_ARG_TYPE_ERR);
    } else if(unlikely(!IS_STRING(pattern))) {
        err = ERR_NEW(vm, STRPAT_SECOND_ARG_TYPE_ERR);
    } else {
        goto fin;
    }

    argv[-1] = OBJ_VAL(err);
    return false;

fin:;
    ObjString* haystack = AS_STRING(string);
    ObjString* needle   = AS_STRING(pattern);
    char*      start    = strstr(haystack->storage, needle->storage);
    argv[-1]            = start == NULL ? NIL_VAL : NUMBER_VAL(start - haystack->storage);
    return true;
}

/**
 * Returns a substring of 'string', starting from index 'i' ending
 * at index 'j'. Both 'i' and 'j' can be negative which means they
 * are indexing from the end of the string (in reverse).
 * If 'i' is less than 0, it is corrected to the reverse index.
 * If 'j' is higher than the 'string' length it is corrected to
 * the 'string' length ('string' length is actually - 1 because of index counting).
 * If after corrections 'i' is higher than 'j' empty string is returned.
 * @ret - substring of 'string' spanning from 'i' to 'j'.
 * @err - if first argument is not 'string' or both 'i' and 'j'
 *        are not numbers.
 **/
snative(strsub)
{
    UNUSED(argc);
    Value      string = argv[0];
    Value      i      = argv[1];
    Value      j      = argv[2];
    ObjString* err    = NULL;

    if(unlikely(!IS_STRING(string))) {
        err = ERR_NEW(vm, STRSUB_FIRST_ARG_TYPE_ERR);
    } else if(unlikely(
                  (!IS_NUMBER(i) || !IS_NUMBER(j)) &&
                  (sfloor(AS_NUMBER(i)) != AS_NUMBER(i) || sfloor(AS_NUMBER(j)) != AS_NUMBER(j))))
    {
        err = ERR_NEW(vm, STRSUB_INDICES_TYPE_ERR);
    } else {
        goto fin;
    }
    argv[-1] = OBJ_VAL(err);
    return false;

fin:;
    ObjString* substr = AS_STRING(string);
    int64_t    ii     = AS_NUMBER(i);
    int64_t    ij     = AS_NUMBER(j);
    int64_t    len    = substr->len + 1;

    // If negative, convert
    if(ii < 0) {
        ii = len + ii < 0 ? 0 : len + ii;
    }
    // If negative, convert
    if(ij < 0) {
        ij = len + ij < 0 ? 0 : len + ij;
    }

    // If 'j' bigger than len, truncate
    if(ij > len) {
        ij = len;
    }

    if(ii > ij) {
        argv[-1] = OBJ_VAL(ObjString_from(vm, "", 0));
    } else {
        argv[-1] = OBJ_VAL(ObjString_from(vm, substr->storage + ii, ij - ii));
    }
    return true;
}

snative(strbyte)
{
    UNUSED(argc);
    Value value = argv[0];
    Value index = argv[1];

    ObjString* err = NULL;

    if(unlikely(!IS_STRING(value))) {
        err = ERR_NEW(vm, STRBYTE_FIRST_ARG_TYPE_ERR);
    } else if(unlikely(!IS_NUMBER(index) || sfloor(AS_NUMBER(index)) != AS_NUMBER(index))) {
        err = ERR_NEW(vm, STRBYTE_SECOND_ARG_TYPE_ERR);
    } else {
        goto fin;
    }
    argv[-1] = OBJ_VAL(err);
    return false;

fin:;
    ObjString* string = AS_STRING(value);
    Int        slen   = string->len;
    Int        idx    = AS_NUMBER(index);

    if(idx < 0 || idx > slen - 1) { // Index out of range
        argv[-1] = NIL_VAL;
    } else {
        argv[-1] = NUMBER_VAL(string->storage[idx]);
    }
    return true;
}

sstatic force_inline ObjString* changecase(VM* vm, ObjString* string, int (*changecasefn)(int))
{
    UInt        slen = string->len;
    const char* str  = string->storage;
    char        buffer[slen];

    UInt i = 0;
    while(i < slen) {
        buffer[i++] = changecasefn(*str++);
    }
    buffer[i] = '\0';

    return ObjString_from(vm, buffer, slen);
}

snative(strlower)
{
    UNUSED(argc);

    Value value = argv[0];

    if(unlikely(!IS_STRING(value))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, STRLOWER_ARG_ERR));
        return false;
    }

    argv[-1] = OBJ_VAL(changecase(vm, AS_STRING(value), tolower));
    return true;
}

snative(strupper)
{
    UNUSED(argc);

    Value value = argv[0];

    if(unlikely(!IS_STRING(value))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, STRUPPER_ARG_ERR));
        return false;
    }

    argv[-1] = OBJ_VAL(changecase(vm, AS_STRING(value), toupper));
    return true;
}

sstatic force_inline ObjString* revstring(VM* vm, ObjString* string)
{
    char buffer[string->len];
    UInt slen = string->len - 1;
    UInt i    = 0;
    while(i <= slen) {
        buffer[i] = string->storage[slen - i];
        i++;
    }
    buffer[i] = '\0';
    return ObjString_from(vm, buffer, slen + 1);
}

snative(strrev)
{
    UNUSED(argc);

    Value value = argv[0];

    if(unlikely(!IS_STRING(value))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, STRREV_ARG_ERR));
        return false;
    }
    argv[-1] = OBJ_VAL(revstring(vm, AS_STRING(value)));
    return true;
}

sstatic force_inline ObjString* concatstring(VM* vm, ObjString* left, ObjString* right)
{
    size_t len = left->len + right->len;
    char   buffer[len + 1];
    memcpy(buffer, left->storage, left->len);
    memcpy(buffer + left->len, right->storage, right->len);
    buffer[len] = '\0';
    return ObjString_from(vm, buffer, len);
}

snative(strconcat)
{
    UNUSED(argc);

    Value left  = argv[0];
    Value right = argv[1];

    ObjString* err = NULL;

    if(unlikely(!IS_STRING(left))) {
        err = ERR_NEW(vm, STRCONCAT_FIRST_ARG_TYPE_ERR);
    } else if(unlikely(!IS_STRING(right))) {
        err = ERR_NEW(vm, STRCONCAT_SECOND_ARG_TYPE_ERR);
    } else {
        goto fin;
    }
    argv[-1] = OBJ_VAL(err);
    return false;

fin:;
    argv[-1] = OBJ_VAL(concatstring(vm, AS_STRING(left), AS_STRING(right)));
    return true;
}

snative(byte)
{
    UNUSED(argc);
    Value string = argv[0];
    if(unlikely(!IS_STRING(string))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, BYTE_ARG_ERR));
        return false;
    }
    argv[-1] = NUMBER_VAL(AS_STRING(string)->storage[0]);
    return true;
}

/**
 * Changes the garbage collector heap growth factor.
 * Smaller value means more frequent garbage collection.
 * If the value is '0' then the default gc growth factor will be used.
 * @err - if the value is neither '0' and bigger than '1',
 * @ret - returns 'true'.
 **/
snative(gcfactor)
{
    UNUSED(argc);
    Value factor = argv[0];
    if(unlikely(!IS_NUMBER(factor) || (AS_NUMBER(factor) <= 1 && AS_NUMBER(factor) != 0))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, GC_FACTOR_ARG_ERR));
        return false;
    }
    vm->config.gc_grow_factor = AS_NUMBER(factor);
    argv[-1]                  = TRUE_VAL;
    return true;
}


/**
 * Toggles garbage collection between 'auto' and 'manual'.
 * On 'manual' there will be no garbage collection and user
 * is responsible for invoking the collector using 'native_gccollect'.
 * On 'auto' garbage collection will proceed automatically as per
 * default.
 * @err - in case 'mode' argument is not a string or is a string
 *        but it is not 'auto' or 'manual' string.
 * @ret - returns 'true' if garbage collection is set as 'auto',
 *        otherwise 'false'.
 **/
snative(gcmode)
{
    UNUSED(argc);
    Value      mode = argv[0];
    ObjString* err  = NULL;

    if(unlikely(!IS_STRING(mode))) {
        err = ERR_NEW(vm, GC_MODE_ARG_ERR);
    } else if(unlikely(
                  AS_STRING(mode) != vm->statics[SS_MANU] &&
                  AS_STRING(mode) != vm->statics[SS_AUTO]))
    {
        err = ERR_NEW(vm, GC_MODE_INVALID_MODE_ERR);
    } else {
        goto fin;
    }

    argv[-1] = OBJ_VAL(err);
    return false;

fin:;
    bool manual = AS_STRING(mode) == vm->statics[SS_MANU];
    GC_TOGGLE(vm, GC_MANUAL_BIT, manual);
    argv[-1] = BOOL_VAL(!manual);
    return true;
}

/**
 * Invokes garbage collector and starts a collection.
 * It does mark and sweep collection.
 * @ret - returns number of bytes collected.
 **/
snative(gccollect)
{
    UNUSED(argc);
    argv[-1] = NUMBER_VAL((double)gc(vm));
    return true;
}

/**
 * Returns memory left before next collection.
 * @ret - memory in bytes before next gc.
 **/
snative(gcleft)
{
    UNUSED(argc);
    argv[-1] = NUMBER_VAL(((double)vm->gc_next - vm->gc_allocated));
    return true;
}

/**
 * Returns program current memory usage.
 * @ret - memory usage in bytes.
 **/
snative(gcusage)
{
    UNUSED(argc);
    argv[-1] = NUMBER_VAL((double)vm->gc_allocated);
    return true;
}

/**
 * Returns memory limit on which the next garbage
 * collection will trigger.
 * @ret - return amount of memory in bytes
 *        at which next collection triggers.
 **/
snative(gcnext)
{
    UNUSED(argc);
    argv[-1] = NUMBER_VAL((double)vm->gc_next);
    return true;
}

/**
 * Set the new collector limit in bytes, when the
 * memory limit is reached collection is triggered,
 * but only if the gcmode() is set to 'auto'.
 * @ret - returns the old limit in bytes
 * @err - if the limit is negative.
 **/
snative(gcset)
{
    UNUSED(argc);
    Value      bytes = argv[0];
    ObjString* err   = NULL;
    if(unlikely(!IS_NUMBER(bytes))) {
        err = ERR_NEW(vm, GC_SET_ARG_ERR);
    } else if(unlikely(AS_NUMBER(bytes) < 0)) {
        err = ERR_NEW(vm, GC_SET_NEGATIVE_LIMIT_ERR);
    } else {
        goto fin;
    }

    argv[-1] = OBJ_VAL(err);
    return false;

fin:;
    argv[-1]    = NUMBER_VAL(vm->gc_next);
    vm->gc_next = AS_NUMBER(bytes);
    return true;
}

/**
 * Checks if the garbage collector is in automatic mode (running).
 * @ret - 'true' if the gc is in 'auto' mode (running),
 *        'false' otherwise (if the mode is 'manual').
 **/
snative(gcisauto)
{
    UNUSED(argc);
    argv[-1] = !GC_CHECK(vm, GC_MANUAL_BIT);
    return true;
}

/**
 * Checks if the 'expr' is falsey ('false' or 'nil') and if it is
 * it invokes runtime error printing the 'Assertion failed.' message.
 * @ret - in case 'expr' is falsey it never returns, instead
 *        invokes runtime error.
 *        Otherwise it returns the 'expr' value (first argument).
 **/
snative(assert)
{
    UNUSED(argc);
    Value expr = argv[0];
    if(ISFALSEY(expr)) {
        argv[-1] = OBJ_VAL(
            ObjString_from_static_prefix(vm, vm->statics[SS_ASSERT_MSG], &static_str[SS_ASSERT]));
        return false;
    } else {
        argv[-1] = expr;
        return true;
    }
}

/**
 * Checks if the 'expr' is falsey ('false' or 'nil') and if
 * it is it invokes runtime error printing the passed in 'message'.
 * @ret - invokes runtime error if 'expr' is falsey or if
 *        the second argument 'message' is not string.
 *        Otherwise it returns the 'expr' value (first argument).
 **/
snative(assertf)
{
    UNUSED(argc);
    Value expr    = argv[0];
    Value message = argv[1];

    if(unlikely(!IS_STRING(message))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, ASSERTF_SECOND_ARG_TYPE_ERR));
    } else {
        if(ISFALSEY(expr)) {
            argv[-1] = OBJ_VAL(
                ObjString_from_static_prefix(vm, AS_STRING(message), &static_str[SS_ASSERT]));
        } else {
            argv[-1] = expr;
            return true;
        }
    }
    return false;
}

/**
 * Invokes runtime error printing the passed in 'message'.
 * @ret - errors in case the argument 'message' is not a string,
 *        otherwise it still only invokes runtime error with
 *        the passed in 'message' and never returns.
 **/
snative(error)
{
    UNUSED(argc);
    Value message = argv[0];
    if(unlikely(!IS_STRING(message))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, ERROR_FIRST_ARG_TYPE_ERR));
    } else {
        argv[-1] =
            OBJ_VAL(ObjString_from_static_prefix(vm, AS_STRING(message), &static_str[SS_ERROR]));
    }
    return false;
}

// @FIX: Make index operator work depending on the receiver,
//       if the receiver is the string type, than it can only be a
//       integer constant or integer variable.
//       If the type is class instance (ObjInstance) then the index
//       value can be integer constant, integer variable, boolean constant,
//       boolean variable, string constant or a string variable.


const char* load_script_default(VM* vm, const char* name)
{
    // Perform default script load
    int fd = open(name, O_RDONLY);

    if(unlikely(fd < 0)) {
        perror("ScriptLoader");
        return NULL;
    }

    ssize_t len = lseek(fd, 0, SEEK_END);

    if(unlikely(len < 0)) {
        perror("ScriptLoader");
        return NULL;
    }

    if(unlikely(lseek(fd, 0, SEEK_SET) < 0)) {
        perror("ScriptLoader");
        return NULL;
    }

    char*   source = MALLOC(vm, len + 1);
    ssize_t n      = read(fd, source, len);
    if(unlikely(n < len)) {
        if(n < 0) {
            perror("ScriptLoader");
        } else {
            fprintf(stderr, "Could not read script '%s'\n", name);
        }
        return NULL;
    }

    source[n] = '\0';
    return source;
}

ScriptLoadResult* load_script(VM* vm, ScriptLoadResult* result)
{
    if(vm->config.load_script != NULL) {
        *result = vm->config.load_script(vm, AS_CSTRING(vm->script));
    }

    if(result->source == NULL) {
        result->finfn  = NULL;
        result->source = load_script_default(vm, AS_CSTRING(vm->script));
    }

    if(result->source == NULL) {
        return NULL;
    }

    return result;
}

ObjFunction* compile_script(VM* vm, ScriptLoadResult* result)
{
    runtime         = 0;
    ObjFunction* fn = compile(vm, result->source, vm->script);
    runtime         = 1;

    if(result->finfn != NULL) {
        result->finfn(vm, AS_CSTRING(vm->script), *result);
    } else {
        FREE(vm, (char*)result->source);
    }

    return fn;
}

Value resolve_script(VM* vm, Value name)
{
    if(vm->config.rename_script == NULL) {
        return name;
    }

    const char* renamed = vm->config.rename_script(vm, AS_CSTRING(vm->script), AS_CSTRING(name));

    if(renamed == NULL) {
        return NIL_VAL;
    }

    if(renamed == AS_CSTRING(name)) {
        return name;
    }

    name = OBJ_VAL(ObjString_from(vm, renamed, strlen(renamed)));
    VM_push(vm, name);
    GC_FREE(vm, (char*)renamed, 0);
    VM_pop(vm);
    return name;
}

/**
 * Loads the 'sk' file, compiles and runs it.
 * @ret - returns 'true' if the file is successfully loaded,
 *        otherwise this function won't return, it will instead
 *        invoke error.
 **/
snative(loadscript)
{
    Value name = argv[0];
    if(unlikely(!IS_STRING(name))) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_ARG_TYPE_ERR));
        return false;
    }

    name = resolve_script(vm, name);
    if(name == NIL_VAL) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_RESOLVE_ERR));
        return false;
    }

    vm->script   = name;
    Value retval = EMPTY_VAL;
    if(HashTable_get(&vm->loaded, name, &retval)) {
        if(unlikely(retval == EMPTY_VAL)) {
            argv[-1] = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_RECURSION_ERR));
            return false;
        } else if(retval != NIL_VAL) {
            argv[-1] = retval;
            return true;
        } // else load the script again
    }

    ScriptLoadResult result = {0};
    if(load_script(vm, &result) == NULL) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_LOAD_ERR));
        return false;
    }

    ObjFunction* scriptfn = compile_script(vm, &result);
    if(scriptfn == NULL) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_COMPILE_ERR));
        return false;
    }

    Value fn = OBJ_VAL(scriptfn);
    VM_push_temp(vm, fn);
    HashTable_insert(vm, &vm->loaded, name, EMPTY_VAL); // Update loaded table
    vm->sp     -= argc;
    vm->script  = name;
    bool ok     = VM_call_fn(vm, AS_OBJ(fn), 0, false, NULL);
    vm->sp     += argc; // Little bit of cheating
    VM_pop_temp(vm);

    if(unlikely(!ok)) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_RUN_ERR));
        return false;
    }

    return true;
}
