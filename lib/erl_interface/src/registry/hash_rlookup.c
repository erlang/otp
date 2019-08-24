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

