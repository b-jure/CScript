#include "cript.h"

#include <stdio.h>

int skcore_print(TState* ts)
{
    int args = cr_gettop(ts);
    while(args--) {

        fputc('\t', stdout);
    }
    return 0;
}

int skcore_println(TState* ts)
{
    return 0;
}

int skcore_tostring(TState* ts)
{
    return 1;
}

int skcore_tonumber(TState* ts)
{
    return 1;
}

int skcore_error(TState* ts)
{
    return 0;
}

int skcore_raweq(TState* ts)
{
    return 1;
}

int skcore_rawget(TState* ts)
{
    return 1;
}

int skcore_rawset(TState* ts)
{
    return 0;
}

int skcore_gcollect(TState* ts)
{
    return 1;
}

int skcore_typeof(TState* ts)
{
    return 1;
}

int skcore_next(TState* ts)
{
    return 1;
}

int skcore_loadfile(TState* ts)
{
    return 1;
}

int skcore_loadstring(TState* ts)
{
    return 1;
}


/* Generic reader */
static const char* genericreader(TState* ts, void* userdata, cr_umem* szread)
{
    return NULL;
}

int skcore_load(TState* ts)
{
    return 1;
}

int skcore_runfile(TState* ts)
{
    return cr_gettop(ts);
}

int skcore_assert(TState* ts)
{
    return cr_gettop(ts);
}

int skcore_take(TState* ts)
{
    return 1 /* change */;
}

int skcore_args(TState* ts)
{
    return 1;
}

int skcore_pcall(TState* ts)
{
    return 1 /* change */;
}

static const cr_entry corelib[] = {
    {"print",    skcore_print,    1, 1},
    {"println",  skcore_println,  0, 1},
    {"tostring", skcore_tostring, 1, 0},
    {"tonumber", skcore_tonumber, 1, 0},
    {"error",    skcore_error,    1, 1},
    {"raweq",    skcore_raweq,    1, 1},
    {"rawget",   skcore_rawget,   1, 1},
    {"rawset",   skcore_rawset,   2, 0},
    {"gcollect", skcore_gcollect, 1, 1},
    {"typeof",   skcore_typeof,   1, 1},
    {"next",     skcore_next,     1, 1},
    {"loadfile", skcore_loadfile, 0, 0},
    {"load",     skcore_load,     1, 1},
    {"runfile",  skcore_runfile,  0, 1},
    {"assert",   skcore_assert,   1, 1},
    {"vaselect", skcore_take,     2, 1},
    {"vacount",  skcore_args,     0, 1},
    {"pcall",    skcore_pcall,    1, 1},
    {NULL,       NULL,            0, 0}, // end
};

CR_LOADAPI int skload_corelib(TState* ts)
{
    return 1;
}
