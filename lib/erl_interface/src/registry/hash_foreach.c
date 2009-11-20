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
#include "hash.h"

/* perform f(key,value) on each key-value pair in the table.
 * hash_foreach() will traverse the table until the end is reached or
 * until f() returns a non-zero value, whichever comes first. The
 * return value from f() will be returned to the caller, or 0 if the
 * entire table was traversed.
 */
int ei_hash_foreach(ei_hash *tab, int (*f)(const char *key, const void *value))
{
  ei_bucket *b;
  int i;
  int r;

  for (i=0; i<tab->size; i++) {
    b=tab->tab[i];
    while (b) {
      if (f && (r=f(b->key,b->value))) return r;
      b = b->next;
    }
  }
  return 0;
}

