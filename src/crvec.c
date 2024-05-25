#include "crvec.h"


/* boilerplate code for generic 'Vec' implementation */
#define VecImpl(name, type) \
	void VecFunctionArgs(name, init, VM *vm) { \
		v->cap = 0; \
		v->len = 0; \
		v->ptr = NULL; \
	} \
	static void VecFunction(name, grow) { \
		cr_uint osize = v->cap; \
		if (unlikely(osize >= VECSIZE_MAX)) \
			
	} \
	void VecFunctionArgs(name, init_cap, VM *vm, cr_uint cap) { \
	} \
