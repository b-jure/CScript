#include "core.h"
#include "debug.h"
#include "err.h"
#include "object.h"
#include "parser.h"
#include "skconf.h"
#include "skmath.h"
#include "value.h"
#include "vmachine.h"

#include <ctype.h>
#include <fcntl.h>
#include <time.h>
#include <unistd.h>

sstatic force_inline OString* ObjString_from_static_prefix(
    VM*                   vm,
    OString*              string,
    const InternedString* staticstr)
{
    UInt len = string->len + staticstr->len;
    char buffer[len + 1];

    memcpy(buffer, staticstr->name, staticstr->len);
    memcpy(buffer + staticstr->len, string->storage, string->len);
    buffer[len] = '\0';

    return OString_from(vm, buffer, len);
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
 *      - if second argument is not OString
 **/
snative(isfield)
{
    UNUSED(argc);
    Value    receiver = argv[0];
    Value    field    = argv[1];
    OString* err      = NULL;

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
    argv[-1] =
        BOOL_VAL(HashTable_get(&AS_INSTANCE(receiver)->fields, field, &_dummy));
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
    vprint(argv[0]);
    putc('\n', stdout);
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
    vprint(argv[0]);
    argv[-1] = argv[0];
    return true;
}

sstatic force_inline OString* typename(VM* vm, Value type)
{
    if(IS_NUMBER(type)) {
        return vm->statics[SS_NUM];
    } else if(IS_STRING(type)) {
        return vm->statics[SS_STR];
    } else if(
        IS_FUNCTION(type) || IS_BOUND_METHOD(type) || IS_CLOSURE(type) ||
        IS_NATIVE(type))
    {
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
        argv[-1] = OBJ_VAL(typename(vm, AS_UPVAL(value)->closed.value));
    } else {
        argv[-1] = OBJ_VAL(typename(vm, value));
    }

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
        argv[-1] = OBJ_VAL(ObjString_from_static_prefix(
            vm,
            vm->statics[SS_ASSERT_MSG],
            &static_str[SS_ASSERT]));
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
            argv[-1] = OBJ_VAL(ObjString_from_static_prefix(
                vm,
                AS_STRING(message),
                &static_str[SS_ASSERT]));
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
        argv[-1] = OBJ_VAL(ObjString_from_static_prefix(
            vm,
            AS_STRING(message),
            &static_str[SS_ERROR]));
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
    if(vm->config.load_script != NULL)
        *result = vm->config.load_script(vm, AS_CSTRING(vm->script));
    if(result->source == NULL) {
        result->finfn  = NULL;
        result->source = load_script_default(vm, AS_CSTRING(vm->script));
    }
    if(result->source == NULL) return NULL;
    return result;
}

OClosure* compile_script(VM* vm, ScriptLoadResult* result)
{
    runtime           = 0;
    OClosure* closure = compile(vm, result->source, vm->script);
    runtime           = 1;
    if(result->finfn != NULL) result->finfn(vm, AS_CSTRING(vm->script), *result);
    else FREE(vm, (char*)result->source);
    return closure;
}

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
 * Loads the 'sk' file, compiles and runs it.
 * @ret - returns 'true' if the file is successfully loaded,
 *        otherwise this function won't return, it will instead
 *        invoke error.
 **/
snative(loadscript)
{
    TODO("Update 'loadscript' to work with new changes.");
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

    OClosure* closure = compile_script(vm, &result);
    if(closure == NULL) {
        argv[-1] = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_COMPILE_ERR));
        return false;
    }

    Value scriptfn = OBJ_VAL(closure);
    push(vm, scriptfn);
    HashTable_insert(vm, &vm->loaded, name, EMPTY_VAL); // Update loaded table
    pop(vm);
    vm->sp     -= argc;
    vm->script  = name;
    // bool ok     = VM_call_fn(vm, AS_OBJ(fn), 0, false, retcnt);
    vm->sp += argc; // Little bit of cheating

    // if(unlikely(!ok)) {
    //     argv[-1] = OBJ_VAL(ERR_NEW(vm, LOADSCRIPT_RUN_ERR));
    //     return false;
    // }

    return true;
}
