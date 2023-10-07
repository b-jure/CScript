#ifndef __SKOOMA_ERR_H__
#define __SKOOMA_ERR_H__

#define ERR_NEW(roots, err) ObjString_from(roots, err, sizeof(err) - 1)

#define CLOCK_ERR                                                              \
  "Processor time is not available or its value cannot be represented."

#endif
