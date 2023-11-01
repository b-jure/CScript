#include "chunk.h"
#include "debug.h"
#include "mem.h"
#include "vmachine.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINE BUFSIZ

static void  repl(VM* vm);
static void  File_run(VM* vm, const char* path);
static char* File_read(const char* path);

// @FIX: fix repl
unused static void repl(VM* vm)
{
    char line[MAXLINE];

    while(true) {
        printf(">> ");

        if(!fgets(line, sizeof(line), stdin)) {
            printf("\n");
            break;
        }

        VM_interpret(vm, line);
        runtime = 0;
    }
}

static void File_run(VM* vm, const char* path)
{
    char*           source = File_read(path);
    InterpretResult result = VM_interpret(vm, source);
    runtime                = 0;
    free(source);

    if(result == INTERPRET_COMPILE_ERROR) {
        VM_free(vm);
        exit(65);
    }
    if(result == INTERPRET_RUNTIME_ERROR) {
        VM_free(vm);
        exit(70);
    }
}

static char* File_read(const char* path)
{
    FILE* fp = fopen(path, "rb");
    if(fp == NULL) {
        fprintf(stderr, "Could not open file \"%s\"\n", path);
        perror("Skooma");
        exit(74);
    }

    if(unlikely(fseek(fp, 0L, SEEK_END) < 0)) {
        fprintf(stderr, "Failed processing file \"%s\"\n", path);
        perror("Skooma");
        exit(74);
    }

    size_t len = ftell(fp);

    char* buffer = malloc(len + 1);
    if(unlikely(buffer == NULL)) {
        fprintf(stderr, "Could not allocate enough memory to read \"%s\"\n", path);
        perror("Skooma");
        exit(74);
    }

    rewind(fp);

    size_t n = fread(buffer, sizeof(char), len, fp);
    if(n < len) {
        fprintf(stderr, "Could not read file \"%s\"\n", path);
        free(buffer);
        exit(74);
    }

    if(unlikely(ferror(fp))) {
        fprintf(stderr, "Error ocurred while read file \"%s\"\n", path);
        perror("Skooma");
        free(buffer);
        exit(74);
    }

    buffer[n] = '\0';
    return buffer;
}

int main(int argc, char* argv[])
{
    runtime = 0;
    VM* vm  = VM_new(NULL);

    if(argc == 1) {
        fprintf(stderr, "REPL not functional.\n");
        return 1;
        // repl(vm);
    } else if(argc == 2) {
        File_run(vm, argv[1]);
    } else {
        fprintf(stderr, "Usage: skooma [path.sk]\n");
        VM_free(vm);
        exit(64);
    }

    VM_free(vm);
    return 0;
}
