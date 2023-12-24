#include "chunk.h"
#include "corelib.h"
#include "debug.h"
#include "mem.h"
#include "skooma.h"
#include "vmachine.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE BUFSIZ

static void File_run(VM* vm, const char* path)
{
    const char*     source = load_script_default(vm, path);
    InterpretResult result = interpret(vm, source, path);
    runtime                = 0;
    free((void*)source);
    if(result == INTERPRET_COMPILE_ERROR) {
        sk_destroy(&vm);
        exit(EXIT_FAILURE);
    }
    if(result == INTERPRET_RUNTIME_ERROR) {
        sk_destroy(&vm);
        exit(EXIT_FAILURE);
    }
}

int main(int argc, char* argv[])
{
    runtime = 0;
    VM* vm  = NULL;
    if(argc == 1) {
        fprintf(stderr, "REPL is not implemented, contributions are welcome :)!\n");
        return 1;
    } else if(argc == 2) {
        vm = sk_create(NULL);
        File_run(vm, argv[1]);
        sk_destroy(&vm);
    } else {
        fprintf(stderr, "Usage: skooma [path.sk]\n");
        exit(EXIT_FAILURE);
    }
    return 0;
}
