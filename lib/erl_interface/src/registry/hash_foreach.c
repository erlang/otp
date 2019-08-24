/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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

