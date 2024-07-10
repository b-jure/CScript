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


#include "crobject.h"
#include "crvalue.h"
#include "crbits.h"


#define keyisempty(n)		(keytt(n) == CR_VEMPTY)

/* node value */
#define htnodevalue(n)		(&(n)->val)

/* get table slot */
#define htnode(ht,i)		(&(ht)->mem[(i)])

/* get table size */
#define htsize(ht)		(twoto((ht)->size))

#define htfirstnode(ht)		htnode(ht, 0)
#define htlastnode(ht)		htnode(ht, htsize(ht) - 1)



CRI_FUNC HTable *cr_htable_new(cr_State *ts);
CRI_FUNC void cr_htable_newstrtab(cr_State *ts, HTable *tab);
CRI_FUNC int cr_htable_next(cr_State *ts, HTable *tab, SIndex *k);
CRI_FUNC void cr_htable_copykeys(cr_State *ts, HTable *stab, HTable *dtab);
CRI_FUNC int cr_htable_intern(cr_State *ts, const char *string);
CRI_FUNC int cr_htable_set(cr_State *ts, HTable *tab, const TValue *key, const TValue *val);
CRI_FUNC int cr_htable_remove(HTable *tab, const TValue *k);
CRI_FUNC void cr_htable_removedirect(HTable *tab, Node *slot);
CRI_FUNC OString *cr_htable_getraw(HTable *tab, const char *str, size_t len, unsigned int hash);
CRI_FUNC int cr_htable_get(HTable *tab, const TValue *key, TValue *o);
CRI_FUNC void cr_htable_free(cr_State *ts, HTable *ht);

#endif
