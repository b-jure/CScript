#ifndef __SKOOMA_ARRAY_H__
#define __SKOOMA_ARRAY_H__

#include "common.h"
#include "value.h"

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
  void _ARRAY_METHOD(type, init);                                              \
  UInt _ARRAY_METHOD(type, push, type value);                                  \
  type _ARRAY_METHOD(type, pop);                                               \
  type _ARRAY_METHOD(type, index, size_t index);                               \
  type _ARRAY_METHOD(type, first);                                             \
  type _ARRAY_METHOD(type, last);                                              \
  void _ARRAY_METHOD(type, insert, size_t index, type value);                  \
  type _ARRAY_METHOD(type, remove, size_t index);                              \
  size_t _ARRAY_METHOD(type, len);                                             \
  void _ARRAY_METHOD(type, free);

DECLARE_ARRAY(Byte)
DECLARE_ARRAY(UInt)
DECLARE_ARRAY(Value)

#endif
