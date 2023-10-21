#ifndef __SKOOMA_ERR_H__
#define __SKOOMA_ERR_H__

#define NATIVE_FN_ERR(fn, err) "<native-fn " fn ">: " err
#define ERR_NEW(vm, err)       ObjString_from(vm, NULL, err, sizeof(err) - 1)

#define CLOCK_ERR                                                                        \
    NATIVE_FN_ERR(                                                                       \
        "clock",                                                                         \
        "Processor time is not available or its value cannot be represented.")

#define INSTANCE_ERR(fn) NATIVE_FN_ERR(fn, "First parameter is not an instance.")
#define FIELD_ERR(fn)    NATIVE_FN_ERR(fn, "Second parameter is not a valid field name.")

#define ISFIELD_INSTANCE_ERR INSTANCE_ERR("isfield")
#define ISFIELD_FIELD_ERR    FIELD_ERR("isfield")

#define DELFIELD_INSTANCE_ERR INSTANCE_ERR("delfield")
#define DELFIELD_FIELD_ERR    FIELD_ERR("delfield")

#define SETFIELD_INSTANCE_ERR INSTANCE_ERR("setfield")
#define SETFIELD_FIELD_ERR FIELD_ERR("setfield")

#endif
