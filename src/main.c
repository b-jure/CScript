#include "chunk.h"
#include "debug.h"
#include "vmachine.h"

int main()
{
    VM* vm = VM_new();

    Chunk chunk;
    Chunk_init(&chunk);
    Chunk_write_constant(&chunk, 8, 1);
    Chunk_write_constant(&chunk, 10, 1);
    Chunk_write(&chunk, OP_ADD, 1);
    Chunk_write_constant(&chunk, 3.5, 2);
    Chunk_write(&chunk, OP_DIV, 2);
    Chunk_write(&chunk, OP_NEG, 2);
    Chunk_write(&chunk, OP_RET, 2);

    VM_interpret(vm, &chunk);
    VM_free(vm);
}
