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

/* remove all the key-values from a table. The 
 * values are removed with
 * the user-provided function f. 
 */
int ei_hash_freetab(ei_hash *tab, void (*f)(void *))
{
  ei_bucket *b, *next;
  int i;

  for (i=0; i<tab->size; i++) {
    b=tab->tab[i];
    while (b) {
      next = b->next;

      if (f) f((void *)b->value);

      /* no point in saving these buckets on freelist */
      free(b); 
      b = next;
    }
  }

  /* remove the freelist */
  b = tab->freelist;
  while (b) {
    next = b->next;
    free(b);
    b = next;
  }
  
  /* remove the table */
  free(tab);

  return 0;
}
