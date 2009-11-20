/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
 * 
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * %CopyrightEnd%
 *

 */
#include <stdlib.h>
#include "hash.h"

/* this function does a reverse lookup and returns the first key whose
 * value matches value. This operation may be lengthy! Also, there is
 * no guarantee that the *values* are unique in the hash table, so the
 * returned key might not be the one you expect. 
 */
const char *ei_hash_rlookup(ei_hash *tab, const void *value)
{
  ei_bucket *b;
  int i;

  for (i=0; i<tab->size; i++) {
    b=tab->tab[i];
    while (b) {
      if (b->value == value) return b->key;
      b = b->next;
    }
  }
  return NULL;
}

