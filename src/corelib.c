#include "corelib.h"
#include "debug.h"
#include "err.h"
#include "object.h"
#include "parser.h"
#include "skooma.h"

#include <fcntl.h>
#include <time.h>



/* Helper */
static force_inline OString*
OString_from_static_prefix(VM* vm, OString* string, const InternedString* staticstr)
{
    UInt len = string->len + staticstr->len;
    char buffer[len + 1];
    memcpy(buffer, staticstr->name, staticstr->len);
    memcpy(buffer + staticstr->len, string->storage, string->len);
    buffer[len] = '\0';
    return OString_from(vm, buffer, len);
}





/*
 * Determines processor time, returning approximation of processor time used by the
 * program in seconds.
 */
corelib(clock)
{
    clock_t time = clock();
    if(time == -1) {
    } else sk_pushnumber(vm, time);
    return 1;
}

/**
 * Checks if OInstance contains field.
 * @ret - bool, true if it contains, false otherwise
 * @err - if first argument is not OInstance
 *      - if second argument is not OString
 **/
corelib(isfield)
{
    return 0;
}

/**
 * Prints a value and a newline.
 * @ret - nil
 **/
corelib(printl)
{
    return 0;
}

/**
 * Prints a value.
 * @ret - nil
 **/
corelib(print)
{
    return 0;
}

/**
 * Returns the value type.
 * @ret - string
 **/
corelib(typeof)
{
    Value value  = argv[0];
    vm->sp      -= argc;
    if(IS_UPVAL(value)) value = AS_UPVAL(value)->closed.value;
    return true;
}


/**
 * Checks if the 'expr' is falsey ('false' or 'nil') and if it is
 * it invokes runtime error printing the default 'Assertion failed.' message.
 * @ret - true if assertion passed else it returns error message.
 **/
corelib(assert)
{
    Value expr  = argv[0];
    vm->sp     -= argc;
    if(ISFALSEY(expr)) {
        argv[-1] = OBJ_VAL(OString_from_static_prefix(
            vm,
            vm->statics[SS_ASSERT_MSG],
            &static_str[SS_ASSERT]));
        return false;
    } else {
        argv[-1] = TRUE_VAL;
        pushn(vm, retcnt - 1, NIL_VAL);
        return true;
    }
}

/**
 * Same as 'assert' except you can provide your own message.
 * @ret - true if assertion passed otherwise returns the provided error message.
 **/
corelib(assertf)
{
    Value expr     = argv[0];
    Value message  = argv[1];
    vm->sp        -= argc;
    if(unlikely(!IS_STRING(message)))
        argv[-1] = OBJ_VAL(ERR_NEW(vm, ASSERTF_SECOND_ARG_TYPE_ERR));
    else {
        if(ISFALSEY(expr))
            argv[-1] = OBJ_VAL(OString_from_static_prefix(
                vm,
                AS_STRING(message),
                &static_str[SS_ASSERT]));
        else {
            argv[-1] = expr;
            pushn(vm, retcnt - 1, NIL_VAL);
            return true;
        }
    }
    return false;
}

/**
 * Invokes runtime error printing the passed in 'message'.
 * @ret - string (error message)
 **/
corelib(error)
{
    UNUSED(retcnt);
    Value message  = argv[0];
    vm->sp        -= argc;
    if(unlikely(!IS_STRING(message)))
        argv[-1] = OBJ_VAL(ERR_NEW(vm, ERROR_FIRST_ARG_TYPE_ERR));
    else
        argv[-1] = OBJ_VAL(
            OString_from_static_prefix(vm, AS_STRING(message), &static_str[SS_ERROR]));
    return false;
}

/**
 * Load script (.sk) into memory (default).
 **/
const char* load_script_default(VM* vm, const char* name)
{
    int fd = open(name, O_RDONLY);
    if(unlikely(fd < 0)) {
        perror("ScriptLoader");
        return NULL;
    }
    ssize_t len = lseek(fd, 0, SEEK_END);
    if(unlikely(len < 0)) {
        perror("ScriptLoader");
        close(fd);
        return NULL;
    }
    if(unlikely(lseek(fd, 0, SEEK_SET) < 0)) {
        perror("ScriptLoader");
        close(fd);
        return NULL;
    }
    char*   source = MALLOC(vm, len + 1);
    ssize_t n      = read(fd, source, len);
    if(unlikely(n < len)) {
        if(n < 0) perror("ScriptLoader");
        else fprintf(stderr, "Could not read script '%s'\n", name);
        close(fd);
        return NULL;
    }
    source[n] = '\0';
    return source;
}

/**
 * Load script into memory depending on how 'Config' load function is set.
 **/
ScriptLoadResult* load_script(VM* vm, ScriptLoadResult* result)
{
    if(vm->config.load_script != NULL)
        *result = vm->config.load_script(vm, AS_CSTRING(vm->script));
    if(result->source == NULL) {
        result->finfn  = NULL;
        result->source = load_script_default(vm, AS_CSTRING(vm->script));
    }
    if(result->source == NULL) return NULL;
    return result;
}

/**
 * Compile the loaded script.
 **/
OClosure* compile_script(VM* vm, ScriptLoadResult* result)
{
    runtime           = 0;
    OClosure* closure = compile(vm, result->source, vm->script);
    runtime           = 1;
    if(result->finfn != NULL) result->finfn(vm, AS_CSTRING(vm->script), *result);
    else FREE(vm, (char*)result->source);
    return closure;
}

/**
 * Possibly canonicalize the name of the script.
 **/
Value resolve_script(VM* vm, Value name)
{
    if(vm->config.rename_script == NULL) return name;
    const char* renamed =
        vm->config.rename_script(vm, AS_CSTRING(vm->script), AS_CSTRING(name));
    if(renamed == NULL) return NIL_VAL;
    if(renamed == AS_CSTRING(name)) return name;
    name = OBJ_VAL(OString_from(vm, renamed, strlen(renamed)));
    push(vm, name);
    GC_FREE(vm, (char*)renamed, 0);
    pop(vm);
    return name;
}

/**
 * Loads, compiles and runs the script file (.sk).
 * @ret - script return values if the script has loaded,
 *        if the script was already loaded before, then it returns true.
 **/
corelib(loadscript)
{
    Value name = argv[0];
    if(unlikely(!IS_STRING(name))) {
        argv[-1]  = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_ARG_TYPE_ERR));
        vm->sp   -= argc;
        return false;
    }
    name = resolve_script(vm, name);
    if(name == NIL_VAL) {
        argv[-1]  = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_RESOLVE_ERR));
        vm->sp   -= argc;
        return false;
    }
    vm->script   = name;
    Value retval = EMPTY_VAL;
    if(HashTable_get(&vm->loaded, name, &retval)) { // is script already loaded ?
        if(unlikely(retval == EMPTY_VAL)) { // is this recursive load ?
            argv[-1]  = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_RECURSION_ERR));
            vm->sp   -= argc;
            return false;
        } else if(retval != NIL_VAL) { // do not reload the script ?
            argv[-1]  = TRUE_VAL;
            vm->sp   -= argc;
            pushn(vm, retcnt - 1, NIL_VAL);
            return true;
        } // else load the script again
    }
    ScriptLoadResult result = {0};
    if(load_script(vm, &result) == NULL) {
        argv[-1]  = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_LOAD_ERR));
        vm->sp   -= argc;
        return false;
    }
    OClosure* closure = compile_script(vm, &result);
    if(closure == NULL) {
        argv[-1]  = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_COMPILE_ERR));
        vm->sp   -= argc;
        return false;
    }
    Value scriptfn = OBJ_VAL(closure);
    push(vm, scriptfn);
    HashTable_insert(vm, &vm->loaded, name, TRUE_VAL); // Update loaded table
    pop(vm);
    vm->script  = name;
    vm->sp     -= argc;
    bool ok     = fncall(vm, AS_CLOSURE(scriptfn), 0, retcnt);
    if(unlikely(!ok)) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_RUN_ERR));
        ASSERT(vm->sp == &argv[0], "Stack pointer is set incorrectly.");
        return false;
    }
    ASSERT(vm->sp == &argv[-1] + retcnt, "Stack pointer is set incorrectly.");
    return true;
}
