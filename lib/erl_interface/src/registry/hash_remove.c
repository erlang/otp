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
#include <string.h>
#include "hash.h"

/* free a hash bucket. If the bucket contained a long key (more that
 * EI_SMALLKEY) the bucket is thrown away (really freed). If the
 * bucket contained a short key, then it can be saved on the freelist
 * for later use. Buckets with short keys have (key == keybuf).
 */
void ei_hash_bfree(ei_hash *tab, ei_bucket *b)
{
  if (!b) return;

  /* we throw away buckets with long keys (i.e. non-standard buckets) */
  if (b->key != b->keybuf) {
    /* fprintf(stderr,"freeing bucket with long key (%s)\n",b->key); */
    free(b);
  }
    
  else {
    /* others we save on (tab-local) freelist */
    /* fprintf(stderr,"saving bucket with short key (%s)\n",b->key); */
    b->next = tab->freelist;
    tab->freelist = b;
  }

  return;
}

void *ei_hash_remove(ei_hash *tab, const char *key) 
{
  ei_bucket *b=NULL, *tmp=NULL;
  const void *oldval=NULL;
  int h, rh;

  rh = tab->hash(key);
  h =  rh % tab->size;

  /* is it in the first position? */
  if ((b=tab->tab[h])) {
    if ((rh == b->rawhash) && (!strcmp(key,b->key))) {
      tab->tab[h] = b->next;
      oldval = b->value;
      ei_hash_bfree(tab,b);

      tab->nelem--;
      if (!tab->tab[h]) tab->npos--;
    }
    else {
      /* is it later in the chain? */
      while (b->next) {
	if ((rh == b->next->rawhash) && (!strcmp(key,b->next->key))) {
	  tmp = b->next;
	  b->next = tmp->next;
	  oldval = tmp->value;
	  ei_hash_bfree(tab,tmp);

	  tab->nelem--;
	  break;
	}
	b=b->next;
      }
    }
  }
  return (void *)oldval;
}

