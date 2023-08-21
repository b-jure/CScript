#include "compiler.h"
#include "scanner.h"

#include <stdio.h>

void compile(const char* source)
{
    Scanner_init(source);
    UInt line = -1;

    while (true) {
        Token token = Scanner_scan();
        if (token.line != line) {
            printf("%4d ", token.line);
            line = token.line;
        } else {
            printf("    | ");
        }
        printf("%2d '%.*s'\n", token.type, token.len, token.start);

        if (token.type == TOK_EOF) {
            break;
        }
    }
}
