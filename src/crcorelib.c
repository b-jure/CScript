#include "cript.h"

#include <stdio.h>

int skcore_print(VM* vm)
{
    int args = cr_gettop(vm);
    while(args--) {

        fputc('\t', stdout);
    }
    return 0;
}

int skcore_println(VM* vm)
{
    return 0;
}

int skcore_tostring(VM* vm)
{
    return 1;
}

int skcore_tonumber(VM* vm)
{
    return 1;
}

int skcore_error(VM* vm)
{
    return 0;
}

int skcore_raweq(VM* vm)
{
    return 1;
}

int skcore_rawget(VM* vm)
{
    return 1;
}

int skcore_rawset(VM* vm)
{
    return 0;
}

int skcore_gcollect(VM* vm)
{
    return 1;
}

int skcore_typeof(VM* vm)
{
    return 1;
}

int skcore_next(VM* vm)
{
    return 1;
}

int skcore_loadfile(VM* vm)
{
    return 1;
}

int skcore_loadstring(VM* vm)
{
    return 1;
}


/* Generic reader */
static const char* genericreader(VM* vm, void* userdata, cr_umem* szread)
{
    return NULL;
}

int skcore_load(VM* vm)
{
    return 1;
}

int skcore_runfile(VM* vm)
{
    return cr_gettop(vm);
}

int skcore_assert(VM* vm)
{
    return cr_gettop(vm);
}

int skcore_take(VM* vm)
{
    return 1 /* change */;
}

int skcore_args(VM* vm)
{
    return 1;
}

int skcore_pcall(VM* vm)
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

CR_LOADAPI int skload_corelib(VM* vm)
{
    return 1;
}
