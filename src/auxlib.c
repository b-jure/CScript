#include "auxlib.h"
#include "skooma.h"

#include <errno.h>
#include <stdlib.h>
#include <string.h>


// @TODO: Finish this, basic debugging is now implemented and I should
//        be able to implement stack traceback.

/*
static force_inline void printerror(VM* vm)
{
    Value errobj = *stackpeek(0);
    const char* errmsg = NULL;
    if(IS_STRING(errobj)) {
        errmsg = AS_CSTRING(errobj);
    } else errmsg = "error object is not a string";
    sk_writetoerrf(
        "Skooma [PANIC]: Errored in unprotected call to Skooma API (error: %s)\n",
        errmsg);
}

static void stacktraceback(VM* vm)
{
#define isloaded(vm, sname, closure) HashTable_get(&(vm)->loaded, sname, closure)

    static const char* fmt1 = "\t['%s' on line %u] in ";
    static const char* fmt2 = "\tin %s()\n";
    sk_writetoerr("Stack traceback:\n");
    for(int32_t i = vm->fc - 1; i >= 0; i--) {
        CallFrame* frame = &vm->frames[i];
        if(frame->closure) { // Skooma function ?
            Chunk* chunk = &frame->closure->fn->chunk;
            int32_t line = Chunk_getline(chunk, frame->ip - chunk->code.data - 1);
            Value _; // dummy
            Value sname = OBJ_VAL(FFN(frame)->name);
            bool loaded;
            if((loaded = isloaded(vm, sname, &_))) vm->script = sname;
            sk_writetoerrf(fmt1, AS_CSTRING(vm->script), line);
            if(loaded) sk_writetoerr("script\n");
            else sk_writetoerrf("%s()\n", FFN(frame)->name->storage);
        } else // this is a C function
            sk_writetoerrf(fmt2, AS_NATIVE(*frame->callee)->name->storage);
    }

#undef isloaded
}
*/


/* Panic handler */
int panic(VM* vm)
{
    // printerror(vm);
    // stacktraceback(vm);
    return 0; // FALLTHRU into 'abort()'
}


/* Allocator */
static void* reallocate(void* ptr, size_t newc, void* _)
{
    (void)(_);
    if(newc == 0) {
        free(ptr);
        return NULL;
    }
    return realloc(ptr, newc);
}


/* Create and allocate VM. */
SK_LIBAPI VM* skaux_create(void)
{
    VM* vm = sk_create(reallocate, NULL);
    if(likely(vm != NULL)) {
        sk_setalloc(vm, reallocate, NULL);
        sk_setpanic(vm, panic);
    }
    return vm;
}



SK_LIBAPI int sk_argerror(VM* vm, int argidx, const char* extra)
{
    sk_pushfstring(vm, "Invalid argument '%d' %s", argidx, extra);
    return sk_error(vm, S_EARG);
}


SK_LIBAPI int sk_typeerror(VM* vm, int argidx, const char* tname)
{
    const char* argmsg = NULL;
    const char* argtype = NULL;
    if(sk_isinstance(vm, argidx) && sk_getmethod(vm, argidx, "__display__")) {
        sk_call(vm, 0, 1);
        argtype = sk_tostring(vm, -1); // leave on stack, who cares...
    } else argtype = sk_typename(vm, argidx);
    argmsg = sk_pushfstring(vm, "expected '%s', instead got '%s'", tname, argtype);
    return sk_argerror(vm, argidx, argmsg);
}


#define tagerror(vm, idx, type) sk_typeerror(vm, idx, sk_tagname(vm, idx))


SK_LIBAPI sk_number sk_checknumber(VM* vm, int idx)
{
    int isnum = 0;
    sk_number n = sk_getnumber(vm, idx, &isnum);
    if(unlikely(!isnum)) tagerror(vm, idx, TT_NUMBER);
    return n;
}


SK_LIBAPI const char* sk_checkstring(VM* vm, int idx)
{
    const char* str = sk_getstring(vm, idx);
    if(unlikely(str == NULL)) tagerror(vm, idx, TT_STRING);
    return str;
}


SK_LIBAPI int sk_checkbool(VM* vm, int idx)
{
    int isbool = 0;
    int b = sk_getbool(vm, idx, &isbool);
    if(unlikely(isbool == 0)) tagerror(vm, idx, TT_BOOL);
    return b;
}


SK_LIBAPI void sk_checktype(VM* vm, int idx, int type)
{
    if(unlikely(sk_type(vm, idx) != type)) tagerror(vm, idx, type);
}


/* File manipulation related error */
static ALE fileerror(VM* vm, const char* action, int32_t idx)
{
    static const char* fmt = "Cannot %s %s: %s.";
    const char* ferr = strerror(errno);
    const char* filename = sk_getstring(vm, idx);
    sk_pushfstring(vm, fmt, action, filename, ferr);
    sk_remove(vm, idx);
    return ALE_FILE;
}
