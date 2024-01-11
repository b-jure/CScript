#include "corelib.h"
#include "skooma.h"
#include "vmachine.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE BUFSIZ

static void File_run(VM* vm, const char* path)
{
    const char* source = load_script_default(vm, path);
    interpret(vm, source, path);
    runtime = 0;
    free((void*)source);
}

int main(int argc, char* argv[])
{
    runtime = 0;
    VM* vm = NULL;
    if(argc == 1) {
        fprintf(stderr, "REPL is not implemented, contributions are welcome :)!\n");
        return 1;
    } else if(argc == 2) {
        vm = sk_create(NULL, NULL);
        // TODO:  sk_runfile(vm, argv[1]);
        File_run(vm, argv[1]);
        sk_destroy(&vm);
    } else {
        fprintf(stderr, "Usage: skooma [path.sk]\n");
        exit(EXIT_FAILURE);
    }
    return 0;
}
