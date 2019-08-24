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

/* this function returns a bucket - from the freelist if one was found
 * there, or from malloc(). Only "small" buckets, i.e. those whose
 * keys are short enough to be stored in the bucket itself, are saved
 * on the freelist.
 */
static ei_bucket *ei_hash_bmalloc(ei_hash *tab)
{
  ei_bucket *new;
  
  if (tab->freelist) {
    new = tab->freelist;
    tab->freelist = new->next;
    /* fprintf(stderr,"getting bucket from freelist\n"); */
  }
  else {
    new = malloc(sizeof(*new));
    /* fprintf(stderr,"allocating new (small) bucket\n"); */
  }

  return new;
}

/* insert a new key-value pair. The old value (if any) is returned. If
 * the malloc fails the function returns NULL. This is potentially a
 * problem since the function returns the same thing when malloc fails
 * as when a item is inserted that did not previously exist in the
 * table. */
void *ei_hash_insert(ei_hash *tab, const char *key, const void *value) 
{
  const void *oldval=NULL;
  ei_bucket *b=NULL;
  int h, rh;

  rh = tab->hash(key);
  h =  rh % tab->size;

  b=tab->tab[h];
  while (b) {
    if ((rh == b->rawhash) && (!strcmp(key,b->key)))
      break;
    b=b->next;
  }

  if (b) {
    /* replace existing value, return old value */
    oldval = b->value;
    b->value = value;
  }
  else {
    int keylen = strlen(key);
    
    /* this element is new */
    if (keylen < EI_SMALLKEY) {
      /* short keys stored directly in bucket */
      /* try to get bucket from freelist */
      if ((b = ei_hash_bmalloc(tab)) == NULL) return NULL;
      b->key = b->keybuf;
    }
    else {
      /* for longer keys we allocate space */
      int keypos=sizeof(*b);

      ei_align(keypos);
      if ((b = malloc(keypos+keylen+1)) == NULL) return NULL;
      b->key = (char *)b + keypos;
      /* fprintf(stderr,"allocating new (large) bucket\n"); */
    }

    /* fill in the blanks */
    b->rawhash = rh;
    strcpy((char *)b->key,key);
    b->value = value;

    /* some statistiscs */
    if (!tab->tab[h]) tab->npos++;
    tab->nelem++;

    /* link in the new element */
    b->next = tab->tab[h];
    tab->tab[h] = b;
  }
  return (void *)oldval;
}

