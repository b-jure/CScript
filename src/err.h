#ifndef __SKOOMA_ERR_H__
#define __SKOOMA_ERR_H__

#define NATIVE_FN_ERR(fn, err)     "<native-fn " fn ">: "
#define ERR_NEW(vm, compiler, err) ObjString_from(vm, compiler, err, sizeof(err) - 1)

#define CLOCK_ERR                                                                        \
    NATIVE_FN_ERR(                                                                       \
        "clock",                                                                         \
        "Processor time is not available or its value cannot be represented.")

#define COLLECT_OPT_TYPE_ERR                                                             \
    NATIVE_FN_ERR("gcollect", "Invalid option type, expected string.")
#define COLLECT_OPT_INVALID_ERR                                                          \
    NATIVE_FN_ERR(                                                                       \
        "gcollect",                                                                      \
        "Invalid option, valid options: \"collect\", \"stop\", \"restart\", \"bytes\".")
#define COLLECT_GFACT_ARGC_ERR                                                           \
    NATIVE_FN_ERR(                                                                       \
        "gcollect",                                                                      \
        "'gfact' option requires a single 'grow' argument of type number (grow "         \
        "factor).")
#define COLLECT_GFACT_GROW_TYPE_ERR                                                      \
    NATIVE_FN_ERR(                                                                       \
        "gcollect",                                                                      \
        "Invalid type for 'grow' parameter of 'gfact' option, expected number value.")
#define COLLECT_GFACT_GROW_INVALID_VALUE_ERR                                             \
    NATIVE_FN_ERR(                                                                       \
        "gcollect",                                                                      \
        "Invalid value for 'grow' parameter of 'gfact' option. Value must be non-zero "  \
        "positive number larger or equal to 1 ('grow' >= 1).")

#endif
