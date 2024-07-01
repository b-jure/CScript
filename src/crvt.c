#include "crvt.h"
#include "crobject.h"
#include "crstate.h"


static const char udataname[] = "userdata";

CRI_DEF const char *const cr_vt_typenames[CR_TOTALTYPES] = {
	"no value", "boolean", "number", udataname, "string",
	"functions", "class", "instance", udataname, "nil",
	"thread", "upvalue"
};


void cr_vt_init(TState *ts)
{
	static const char *vtmnames[CR_NUMM] = {
		"__init__", "__display__", "__tostring__", "__getidx__",
		"__setidx__", "__gc__", "__defer__", "__add__", "__sub__",
		"__mul__", "__div__", "__mod__", "__pow__", "__not__",
		"__umin__", "__ne__", "__eq__", "__lt__", "__le__",
		"__gt__", "__ge__",
	};
	int i;
	const char *str;

	for (i = 0; i < CR_NUMM; i++) {
		str = vtmnames[i];
		GS(ts)->vtmnames[i] = cr_ob_newstring(ts, str, strlen(str));
		cr_gc_fix(ts, objtogco(GS(ts)->vtmnames[i]));
	}
}
