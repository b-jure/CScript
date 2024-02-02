/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of Skooma.
 * Skooma is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * Skooma is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with Skooma.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include "skauxlib.h"
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
    sk_debuginfo di;
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
static void* allocator(void* ptr, sk_memsize size, void* _)
{
    (void)(_); // unused
    if(size == 0) {
        free(ptr);
        return NULL;
    }
    return realloc(ptr, size);
}

/* Create and allocate VM using 'alloc' from this library.
 * Additionally set 'panic' as the default panic handler. */
SK_LIBAPI VM* skaux_create(void)
{
    VM* vm = sk_create(allocator, NULL);
    if(likely(vm != NULL)) sk_setpanic(vm, panic);
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
#define tagerror(vm, idx, type) skaux_typeerror(vm, idx, sk_tagname(vm, type))



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


/* @TODO: add description */
SK_LIBAPI void skaux_where(VM* vm, sk_uint level)
{
    sk_debuginfo di;
    if(sk_getstack(vm, level, &di)) {
        sk_getinfo(vm, DW_LINE | DW_FNSRC, &di);
        if(di.line > 0) {
            sk_pushfstring(vm, "%s:%d ", di.shortsrc, di.line);
            return;
        }
    }
    sk_pushcstring(vm, "");
}


/* @TODO: add description */
SK_LIBAPI sk_int skaux_error(VM* vm, sk_status errcode, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);
    skaux_where(vm, 0); // current level
    sk_pushvfstring(vm, fmt, argp);
    va_end(argp);
    sk_concat(vm);
    return sk_error(vm, errcode);
}


/* @TODO: add description */
SK_LIBAPI void skaux_checkstack(VM* vm, sk_int space, const char* msg)
{
    if(unlikely(!sk_checkstack(vm, space))) {
        if(msg) skaux_error(vm, S_ESOVERFLOW, "stack overflow, %s", msg);
        else skaux_error(vm, S_ESOVERFLOW, "stack overflow");
    }
}





/* ================ Load chunk from file ================ */

typedef struct {
    FILE* fp;
    sk_memsize n;
    char buffer[BUFSIZ];
} FileReader;

/* File manipulation related error */
static sk_int fileerror(VM* vm, const char* action, sk_int idx)
{
    const char* ferr = strerror(errno);
    const char* filename = sk_getstring(vm, idx);
    sk_pushfstring(vm, "Cannot %s %s: %s.", action, filename, ferr);
    sk_remove(vm, idx);
    sk_error(vm, S_EFILE);
    return 0; // unreachable
}

static const char* filereader(VM* vm, void* userdata, sk_memsize* szread)
{
    (void)(vm); // unused
    FileReader* reader = (FileReader*)userdata;
    if(reader->n > 0) { // have unread chars ?
        *szread = reader->n;
        reader->n = 0;
    } else {
        if(feof(reader->fp)) return NULL; // last read has set 'EOF'
        *szread = fread(reader->buffer, sizeof(char), sizeof(reader->buffer), reader->fp);
    }
    return reader->buffer;
}

/* @TODO: add description */
SK_LIBAPI sk_status skaux_loadfile(VM* vm, const char* filename)
{
    FileReader reader = {0};
    sk_int fnameidx = sk_gettop(vm) + 1; // '+1' we will push it in case of errors
    if(filename == NULL) {
        sk_pushcstring(vm, "stdin");
        reader.fp = stdin;
    } else {
        sk_pushcstring(vm, filename);
        reader.fp = fopen(filename, "r");
        if(unlikely(reader.fp == NULL)) return fileerror(vm, "open", fnameidx);
    }
    sk_status status = sk_load(vm, filereader, &reader, sk_getstring(vm, -1));
    if(filename) fclose(reader.fp);
    if(ferror(reader.fp)) { // had read error?
        sk_settop(vm, fnameidx);
        return fileerror(vm, "read", fnameidx);
    }
    sk_remove(vm, fnameidx);
    return status;
}

/* --------------------------------------------------------- */






/* ================ Load chunk from string ================ */

typedef struct {
    const char* str;
    sk_memsize size;
} StringReader;


const char* stringreader(VM* vm, void* userdata, sk_memsize* szread)
{
    (void)(vm); // unused
    StringReader* reader = (StringReader*)userdata;
    if(reader->size == 0) return NULL;
    *szread = reader->size;
    reader->size = 0;
    return reader->str;
}

/* @TODO: add description */
SK_LIBAPI sk_status skaux_loadstring(VM* vm, const char* string)
{
    StringReader reader = {0};
    reader.str = string;
    reader.size = strlen(string);
    return sk_load(vm, stringreader, &reader, "string");
}

/* --------------------------------------------------------- */
