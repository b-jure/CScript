#include "mem.h"

#include <stdlib.h>

void*
reallocate(void* ptr, _unused size_t oldc, size_t newc)
{
  if (newc == 0) {
    free(ptr);
    return NULL;
  }

  void* alloc = realloc(ptr, newc);

  if (alloc == NULL)
    exit(EXIT_FAILURE);
  return alloc;
}
