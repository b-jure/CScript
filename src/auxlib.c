#include "auxlib.h"
#include "skooma.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* Auxiliary to 'panic' handler, prints error message
 * located on top of the stack */
static force_inline void printerror(VM* vm)
{
    const char* errmsg = sk_getstring(vm, -1);
    if(errmsg == NULL) errmsg = "error object is not a string";
    skaux_writetoerrf(
        "Skooma [PANIC]: Errored in unprotected call to Skooma API (error: %s)\n",
        errmsg);
}

/* Auxiliary to 'panic', prints stack trace-back */
static void stacktraceback(VM* vm)
{
    DebugInfo di;
    sk_uint level = 0;
    if(!sk_getstack(vm, level, &di)) return;
    skaux_writetoerr("Stack traceback:\n");
    do {
        sk_getinfo(vm, DW_LINE | DW_FNINFO | DW_FNSRC, &di);
        if(*di.type == 'C') skaux_writetoerrf("\t'%s' in %s()\n", di.source, di.name);
        else {
            skaux_writetoerrf("\t'%s' on line '%u' in ", di.source, di.line);
            if(*di.type == 'm') skaux_writetoerr("main\n");
            else skaux_writetoerrf("%s()\n", di.name);
        }
    } while(sk_getstack(vm, level, &di));
}

/* Panic handler */
static sk_int panic(VM* vm)
{
    printerror(vm);
    stacktraceback(vm);
    return 0; // FALLTHRU into 'abort()'
}

/* Allocator */
static void* reallocate(void* ptr, sk_memsize newc, void* _)
{
    (void)(_); // unused
    if(newc == 0) {
        free(ptr);
        return NULL;
    }
    return realloc(ptr, newc);
}

/* Create and allocate VM using allocator and panic
 * handler from this library. */
SK_LIBAPI VM* skaux_create(void)
{
    VM* vm = sk_create(reallocate, NULL);
    if(likely(vm != NULL)) {
        sk_setpanic(vm, panic);
    }
    return vm;
}


/* Invokes runtime error 'invalid argument' at 'argidx'.
 * 'extra' is additional information user wants to display in the
 * error message. */
SK_LIBAPI sk_int skaux_argerror(VM* vm, sk_int argidx, const char* extra)
{
    sk_pushfstring(vm, "Invalid argument '%d' %s", argidx, extra);
    return sk_error(vm, S_EARG);
}


/* Invokes runtime error due to invalid type provided.
 * 'argidx' is the index of the invalid argument on the stack.
 * 'tname' is the cstring of the type that was expected instead. */
SK_LIBAPI sk_int skaux_typeerror(VM* vm, sk_int argidx, const char* tname)
{
    const char* argmsg = NULL;
    const char* argtype = NULL;
    if(sk_isinstance(vm, argidx) && (sk_getfield(vm, argidx, "__debug") == TT_STRING)) {
        argtype = sk_getstring(vm, -1); // leave on stack, who cares...
    } else argtype = sk_typename(vm, argidx);
    argmsg = sk_pushfstring(vm, "expected '%s', instead got '%s'", tname, argtype);
    return skaux_argerror(vm, argidx, argmsg);
}



/* Invokes generic 'skaux_typeerror' */
#define tagerror(vm, idx, type) skaux_typeerror(vm, idx, sk_tagname(vm, idx))



/* Checks if the value on the stack at 'idx' is number,
 * if not runtime error is invoked.
 * Otherwise the number value is returned. */
SK_LIBAPI sk_number skaux_checknumber(VM* vm, sk_int idx)
{
    sk_byte isnum = 0;
    sk_number n = sk_getnumber(vm, idx, &isnum);
    if(unlikely(!isnum)) tagerror(vm, idx, TT_NUMBER);
    return n;
}



/* Checks if the value on the stack at 'idx' is string,
 * if not runtime error is invoked.
 * Otherwise the string value is returned. */
SK_LIBAPI const char* skaux_checkstring(VM* vm, sk_int idx)
{
    const char* str = sk_getstring(vm, idx);
    if(unlikely(str == NULL)) tagerror(vm, idx, TT_STRING);
    return str;
}



/* Checks if the value on the stack at 'idx' is boolean,
 * if not runtime error is invoked.
 * Otherwise the boolean value is returned. */
SK_LIBAPI sk_byte skaux_checkbool(VM* vm, sk_int idx)
{
    sk_byte isbool = 0;
    sk_byte b = sk_getbool(vm, idx, &isbool);
    if(unlikely(isbool == 0)) tagerror(vm, idx, TT_BOOL);
    return b;
}



/* Checks if the value on the stack at 'idx' is 'type'.
 * If not then runtime error is invoked. */
SK_LIBAPI void skaux_checktype(VM* vm, sk_int idx, sk_int type)
{
    if(unlikely(sk_type(vm, idx) != type)) tagerror(vm, idx, type);
}



/* File manipulation related error */
static ALE fileerror(VM* vm, const char* action, sk_int idx)
{
    static const char* fmt = "Cannot %s %s: %s.";
    const char* ferr = strerror(errno);
    const char* filename = sk_getstring(vm, idx);
    sk_pushfstring(vm, fmt, action, filename, ferr);
    sk_remove(vm, idx);
    return ALE_FILE;
}
