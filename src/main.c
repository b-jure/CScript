#include "chunk.h"
#include "debug.h"

int
main()
{
  Chunk chunk;
  Chunk_init(&chunk);
  size_t constant_index = Chunk_add_constant(&chunk, 5.5);
  Chunk_write(&chunk, OP_CONSTANT, 100);
  Chunk_write(&chunk, constant_index, 100);
  Chunk_write(&chunk, OP_RETURN, 100);
  Chunk_debug(&chunk, "test_chunk");
  Chunk_free(&chunk);
}
