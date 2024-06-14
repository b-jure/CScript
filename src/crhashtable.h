/* ----------------------------------------------------------------------------------------------
 * Copyright (C) 2023-2024 Jure Bagić
 *
 * This file is part of cript.
 * cript is free software: you can redistribute it and/or modify it under the terms of the GNU
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * cript is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with cript.
 * If not, see <https://www.gnu.org/licenses/>.
 * ----------------------------------------------------------------------------------------------*/

#ifndef SKHASHTABLE_H
#define SKHASHTABLE_H


#include "crvalue.h"
#include "crbits.h"



#define keytt(n)	((n)->s.ttk)
#define keyisempty(n)	(keytt(n) == CR_VEMPTY)


#define keyval(n)	((n)->s.keyval)
#define keybvalue(n)	rawbvalue(keyval(n))
#define keyivalue(n)	rawivalue(keyval(n))
#define keyfvalue(n)	rawfvalue(keyval(n))
#define keypvalue(n)	rawpvalue(keyval(n))
#define keycfvalue(n)	rawcfvalue(keyval(n))
#define keyovalue(n)	rawovalue(keyval(n))
#define keystrvalue(n)	((OString*)rawovalue(keyval(n)))


/* copy values from node 'n' key to 'v' */
#define setnodekey(vm,n,v) \
	{ Node *n_ = (n); const TValue *v_ = (v); \
	  keytt(n_) = vtt(v_); keyval(n_) = vval(v_); }


/* copy values from node 'n' key to 'v' */
#define getnodekey(vm,v,n) \
	{ TValue *v_ = (v); const Node *n_ = (n); \
	  vtt(v_) = keytt(n_); vmod(v_) = 0; \
	  vval(v_) = keyval(n_); }



/* node val */
#define nval(n)		(&(n)->val)

/* get table slot */
#define tslot(t,i)	(&(t)->mem[(i)])

/*
 * Ordering of fields might seem weird but
 * this is to ensure proper alignment.
 */
typedef union Node {
	struct {
		TValueFields; /* value fields */
		cr_ubyte ttk;
		Value keyval;
	} s;
	TValue val;
} Node;




#define tsize(t)	(twoto((t)->size))

typedef struct {
	Node *mem; /* memory block */
	int left; /* free slots before array needs to grow */
	int nnodes; /* number of nodes */
	cr_ubyte size; /* 2^size */
} HTable;



CRI_FUNC void cr_ht_init(HTable *tab);
CRI_FUNC void cr_ht_newstab(VM *vm, HTable *tab);
CRI_FUNC int cr_ht_next(VM *vm, HTable *tab, SIndex *k);
CRI_FUNC void cr_ht_copykeys(VM *vm, HTable *stab, HTable *dtab);
CRI_FUNC int cr_ht_intern(VM *vm, const char *string);
CRI_FUNC int cr_ht_set(VM *vm, HTable *tab, const TValue *key, const TValue *val);
CRI_FUNC int cr_ht_remove(VM *vm, HTable *tab, const TValue *k);
CRI_FUNC struct OString *cr_ht_getinterned(HTable *tab, const char *str, 
					size_t len, unsigned int hash);
CRI_FUNC int cr_ht_get(HTable *tab, const TValue *key, TValue *o);
CRI_FUNC void cr_ht_free(VM *vm, HTable *tab);

#endif
