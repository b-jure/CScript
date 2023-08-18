#include "chunk.h"
#include "debug.h"
#include <assert.h>

int main()
{
    int i;
    Chunk chunk;
    Chunk_init(&chunk);
    for (i = 10; i < 267; i++) {
        Chunk_write_constant(&chunk, 5.5, i);
    }
    Chunk_write(&chunk, OP_RETURN, 266);
    Chunk_write_constant(&chunk, 15, 266);
    Chunk_debug(&chunk, "test_chunk");
    Chunk_free(&chunk);
}
