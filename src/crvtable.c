#include "crvtable.h"
#include "crstring.h"
#include "crstate.h"


static const char udataname[] = "userdata";

CRI_DEF const char *const cr_vt_typenames[CR_TOTALTYPES] = {
	"no value", "boolean", "number", udataname, "string",
	"function", "class", "instance", udataname, "nil",
	"thread", "upvalue"
};


CRI_DEF const struct Tuple vtmethodinfo[CR_NUMM] = {
	{ 0, 1 }, /* __init__		{ args: self             - return: instance }  */
	{ 0, 1 }, /* __tostring__ 	{ args: self             - return: string   }  */
	{ 1, 1 }, /* __getidx__   	{ args: self, idx        - return: value    }  */
	{ 2, 0 }, /* __setidx__   	{ args: self, idx, value - return: none     }  */
	{ 1, 0 }, /* __gc__		{ args: self		 - return: none     }  */
	{ 0, 0 }, /* __free__     	{ args: self             - return: none     }  */
	{ 2, 1 }, /* __add__		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __sub__  		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __mul__  		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __div__  		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __mod__  		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __pow__  		{ args: self, lhs, rhs   - return: value    }  */
	{ 1, 1 }, /* __not__  		{ args: self, lhs        - return: value    }  */
	{ 1, 1 }, /* __umin__ 		{ args: self, lhs        - return: value    }  */
	{ 2, 1 }, /* __ne__   		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __eq__   		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __lt__   		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __le__   		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __gt__   		{ args: self, lhs, rhs   - return: value    }  */
	{ 2, 1 }, /* __ge__   		{ args: self, lhs, rhs   - return: value    }  */
};


void cr_vt_init(cr_State *ts)
{
	static const char *vtmnames[CR_NUMM] = {
		"__init__", "__display__", "__tostring__", "__getidx__",
		"__setidx__", "__gc__", "__defer__", "__add__", "__sub__",
		"__mul__", "__div__", "__mod__", "__pow__", "__not__",
		"__umin__", "__ne__", "__eq__", "__lt__", "__le__",
		"__gt__", "__ge__",
	};
	int i;

	for (i = 0; i < CR_NUMM; i++) {
		GS(ts)->vtmnames[i] = cr_string_new(ts, vtmnames[i]);
		cr_gc_fix(ts, obj2gco(GS(ts)->vtmnames[i]));
	}
}


const TValue *cr_vtable_get(cr_State *ts, const TValue *v, int mt)
{
	cr_assert(CR_M_INIT <= mt && mt < CR_NUMM);
	if (!ttiso(v)) return NULL;
	switch (ott(v)) {
		case CR_VCLASS: return &gco2cls(v)->vtable[mt];
		case CR_VUDATA: return &gco2ud(v)->vtable[mt];
		default: return NULL;
	}
}
