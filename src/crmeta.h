#ifndef CRMETA_H
#define CRMETA_H


#include "crobject.h"


OClass *cr_meta_newclass(cr_State *ts, OString *id);
Instance *cr_meta_newinstance(cr_State *ts, OClass *cls);
UserData *cr_meta_newuserdata(cr_State *ts, size_t size, int nuv);
InstanceMethod *cr_meta_newinstancemethod(cr_State *ts, Instance *receiver,
						CrClosure *method);

#endif
