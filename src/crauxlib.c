/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#include "crauxlib.h"
#include "cript.h"

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* Auxiliary to 'panic' handler, prints error message
 * located on top of the stack */
static cr_inline void printerror(TState* ts)
{
    const char* errmsg = cr_getstring(ts, -1);
    if(errmsg == NULL) errmsg = "error object is not a string";
    skaux_writetoerrf(
        "cript [PANIC]: Errored in unprotected call to cript API (error: %s)\n",
        errmsg);
}

/* Auxiliary to 'panic', prints stack trace-back */
static void stacktraceback(TState* ts)
{
    cr_debuginfo di;
    int level = 0;
    if(!cr_getstack(ts, level, &di)) return;
    skaux_writetoerr("Stack traceback:\n");
    do {
        cr_getinfo(ts, DW_LINE | DW_FNINFO | DW_FNSRC, &di);
        if(*di.type == 'C') skaux_writetoerrf("\t'%s' in %s()\n", di.source, di.name);
        else {
            skaux_writetoerrf("\t'%s' on line '%u' in ", di.source, di.line);
            if(*di.type == 'm') skaux_writetoerr("main\n");
            else skaux_writetoerrf("%s()\n", di.name);
        }
    } while(cr_getstack(ts, level, &di));
}

/* Panic handler */
static int panic(TState* ts)
{
    printerror(ts);
    stacktraceback(ts);
    return 0; // FALLTHRU into 'abort()'
}

/* Allocator */
static void* allocator(void* ptr, cr_umem size, void* _)
{
    (void)(_); // unused
    if(size == 0) {
        free(ptr);
        return NULL;
    }
    return realloc(ptr, size);
}

/* Create and allocate TState using 'alloc' from this library.
 * Additionally set 'panic' as the default panic handler. */
CR_LIBAPI TState* skaux_create(void)
{
    TState* ts = cr_create(allocator, NULL);
    if(cr_likely(ts != NULL)) cr_setpanic(ts, panic);
    return ts;
}


/* Invokes runtime error 'invalid argument' at 'argidx'.
 * 'extra' is additional information user wants to display in the
 * error message. */
CR_LIBAPI int skaux_argerror(TState* ts, int argidx, const char* extra)
{
    cr_pushfstring(ts, "Invalid argument '%d' %s", argidx, extra);
    return cr_error(ts, S_EARG);
}


/* Invokes runtime error due to invalid type provided.
 * 'argidx' is the index of the invalid argument on the stack.
 * 'tname' is the cstring of the type that was expected instead. */
CR_LIBAPI int skaux_typeerror(TState* ts, int argidx, const char* tname)
{
    const char* argmsg = NULL;
    const char* argtype = NULL;
    if(cr_isinstance(ts, argidx) && (cr_getfield(ts, argidx, "__debug") == TT_STRING)) {
        argtype = cr_getstring(ts, -1); // leave on stack, who cares...
    } else argtype = cr_typename(ts, argidx);
    argmsg = cr_pushfstring(ts, "expected '%s', instead got '%s'", tname, argtype);
    return skaux_argerror(ts, argidx, argmsg);
}



/* Invokes generic 'skaux_typeerror' */
#define tagerror(ts, idx, type) skaux_typeerror(ts, idx, cr_tagname(ts, type))



/* Checks if the value on the stack at 'idx' is number,
 * if not runtime error is invoked.
 * Otherwise the number value is returned. */
CR_LIBAPI cr_double skaux_checknumber(TState* ts, int idx)
{
    cr_ubyte isnum = 0;
    cr_double n = cr_getnumber(ts, idx, &isnum);
    if(cr_unlikely(!isnum)) tagerror(ts, idx, TT_NUMBER);
    return n;
}



/* Checks if the value on the stack at 'idx' is string,
 * if not runtime error is invoked.
 * Otherwise the string value is returned. */
CR_LIBAPI const char* skaux_checkstring(TState* ts, int idx)
{
    const char* str = cr_getstring(ts, idx);
    if(cr_unlikely(str == NULL)) tagerror(ts, idx, TT_STRING);
    return str;
}



/* Checks if the value on the stack at 'idx' is boolean,
 * if not runtime error is invoked.
 * Otherwise the boolean value is returned. */
CR_LIBAPI cr_ubyte skaux_checkbool(TState* ts, int idx)
{
    cr_ubyte isbool = 0;
    cr_ubyte b = cr_getbool(ts, idx, &isbool);
    if(cr_unlikely(isbool == 0)) tagerror(ts, idx, TT_BOOL);
    return b;
}



/* Checks if the value on the stack at 'idx' is 'type'.
 * If not then runtime error is invoked. */
CR_LIBAPI void skaux_checktype(TState* ts, int idx, int type)
{
    if(cr_unlikely(cr_type(ts, idx) != type)) tagerror(ts, idx, type);
}


/* @TODO: add description */
CR_LIBAPI void skaux_where(TState* ts, int level)
{
    cr_debuginfo di;
    if(cr_getstack(ts, level, &di)) {
        cr_getinfo(ts, DW_LINE | DW_FNSRC, &di);
        if(di.line > 0) {
            cr_pushfstring(ts, "%s:%d ", di.shortsrc, di.line);
            return;
        }
    }
    cr_pushcstring(ts, "");
}


/* @TODO: add description */
CR_LIBAPI int skaux_error(TState* ts, cr_status errcode, const char* fmt, ...)
{
    va_list argp;
    va_start(argp, fmt);
    skaux_where(ts, 0); // current level
    cr_pushvfstring(ts, fmt, argp);
    va_end(argp);
    cr_concat(ts);
    return cr_error(ts, errcode);
}


/* @TODO: add description */
CR_LIBAPI void skaux_checkstack(TState* ts, int space, const char* msg)
{
    if(cr_unlikely(!cr_checkstack(ts, space))) {
        if(msg) skaux_error(ts, S_ESOVERFLOW, "stack overflow, %s", msg);
        else skaux_error(ts, S_ESOVERFLOW, "stack overflow");
    }
}





/* ================ Load chunk from file ================ */

typedef struct {
    FILE* fp;
    cr_umem n;
    char buffer[BUFSIZ];
} FileReader;

/* File manipulation related error */
static int fileerror(TState* ts, const char* action, int idx)
{
    const char* ferr = strerror(errno);
    const char* filename = cr_getstring(ts, idx);
    cr_pushfstring(ts, "Cannot %s %s: %s.", action, filename, ferr);
    cr_remove(ts, idx);
    cr_error(ts, S_EFILE);
    return 0; // cr_unreachable
}

static const char* filereader(TState* ts, void* userdata, cr_umem* szread)
{
    (void)(ts); // unused
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
CR_LIBAPI cr_status skaux_loadfile(TState* ts, const char* filename)
{
    FileReader reader = {0};
    int fnameidx = cr_gettop(ts) + 1; // '+1' we will push it in case of errors
    if(filename == NULL) {
        cr_pushcstring(ts, "stdin");
        reader.fp = stdin;
    } else {
        cr_pushcstring(ts, filename);
        reader.fp = fopen(filename, "r");
        if(cr_unlikely(reader.fp == NULL)) return fileerror(ts, "open", fnameidx);
    }
    cr_status status = cr_load(ts, filereader, &reader, cr_getstring(ts, -1));
    if(filename) fclose(reader.fp);
    if(ferror(reader.fp)) { // had read error?
        cr_settop(ts, fnameidx);
        return fileerror(ts, "read", fnameidx);
    }
    cr_remove(ts, fnameidx);
    return status;
}

/* --------------------------------------------------------- */






/* ================ Load chunk from string ================ */

typedef struct {
    const char* str;
    cr_umem size;
} StringReader;


const char* stringreader(TState* ts, void* userdata, cr_umem* szread)
{
    (void)(ts); // unused
    StringReader* reader = (StringReader*)userdata;
    if(reader->size == 0) return NULL;
    *szread = reader->size;
    reader->size = 0;
    return reader->str;
}

/* @TODO: add description */
CR_LIBAPI cr_status skaux_loadstring(TState* ts, const char* string)
{
    StringReader reader = {0};
    reader.str = string;
    reader.size = strlen(string);
    return cr_load(ts, stringreader, &reader, "string");
}

/* --------------------------------------------------------- */
