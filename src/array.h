#ifndef __SKOOMA_ARRAY_H__
#define __SKOOMA_ARRAY_H__

#include "common.h"
#include "mem.h"
#include "value.h"

#define _CALL_ARRAY_METHOD(type, name, ...)                                    \
  _ARRAY_METHOD_NAME(type, name)(self __VA_OPT__(, ) __VA_ARGS__)
#define _ARRAY_METHOD_NAME(type, b) type##Array_##b
#define _ARRAY_STRUCT(type) type##Array
#define _ARRAY_METHOD(type, name, ...)                                         \
  _ARRAY_METHOD_NAME(type, name)                                               \
  (type##Array * self __VA_OPT__(, ) __VA_ARGS__)

#define DECLARE_ARRAY(type)                                                    \
  typedef struct {                                                             \
    size_t cap;                                                                \
    size_t len;                                                                \
    type *data;                                                                \
  } _ARRAY_STRUCT(type);                                                       \
                                                                               \
  _force_inline void _ARRAY_METHOD(type, init) {                               \
    self->cap = 0;                                                             \
    self->len = 0;                                                             \
    self->data = NULL;                                                         \
  }                                                                            \
                                                                               \
  UInt _ARRAY_METHOD(type, push, type value);                                  \
                                                                               \
  _force_inline type _ARRAY_METHOD(type, pop) {                                \
    return self->data[--self->len];                                            \
  }                                                                            \
                                                                               \
  _force_inline type _ARRAY_METHOD(type, index, size_t index) {                \
    return self->data[index];                                                  \
  }                                                                            \
                                                                               \
  _force_inline type _ARRAY_METHOD(type, last) {                               \
    return self->data[self->len - 1];                                          \
  }                                                                            \
                                                                               \
  _force_inline type _ARRAY_METHOD(type, first) { return self->data[0]; }      \
                                                                               \
  void _ARRAY_METHOD(type, insert, size_t index, type value);                  \
  type _ARRAY_METHOD(type, remove, size_t index);                              \
                                                                               \
  _force_inline size_t _ARRAY_METHOD(type, len) { return self->len; }          \
                                                                               \
  _force_inline void _ARRAY_METHOD(type, free) {                               \
    MFREE_ARRAY(type, self->data, self->cap);                                  \
    _CALL_ARRAY_METHOD(type, init);                                            \
  }

DECLARE_ARRAY(Byte)  /* OpCode Array */
DECLARE_ARRAY(UInt)  /* Lines Array */
DECLARE_ARRAY(Value) /* Constants Array */

#endif
