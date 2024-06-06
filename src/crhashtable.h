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


#include "crcommon.h"
#include "crvalue.h"


typedef struct {
	Value key;
	Value value;
} Node;

typedef struct {
	int size;
	int len;
	int left;
	Node *mem;
} HashTable;


void cr_ht_init(HashTable *tab);
void cr_ht_free(VM *vm, HashTable *tab);

cr_ubyte cr_ht_insert(VM *vm, HashTable *tab, Value k, Value v);
cr_ubyte cr_ht_remove(VM *vm, HashTable *tab, Value k);

cr_ubyte cr_ht_get(VM *vm, HashTable *tab, Value k, Value *out);

Value cr_ht_getinterned(HashTable *tab, const char *str, size_t len, unsigned int hash);

cr_ubyte cr_ht_next(VM *vm, HashTable *tab, Value *k);

void cr_ht_into(VM *vm, HashTable *from, HashTable *to);

unsigned int cr_ht_resize(unsigned int wanted);

void cr_ht_interns(VM *vm, const char *string);
void cr_ht_internfmt(VM *vm, const char *fmt, ...);


#endif
