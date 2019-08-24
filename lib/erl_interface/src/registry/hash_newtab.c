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

ei_hash *ei_hash_newtab(int tabsize)
{
  ei_hash *tab=NULL;
  int bucketpos=sizeof(*tab);
  
  /* make sure size is odd, then increase until prime */
  tabsize |= 0x1; 
  while (!ei_isprime(tabsize)) tabsize +=2;

  /* we will only do one malloc, so "sizeof(*tab)" 
   * must be adjusted to align tab->tab properly
   */
  ei_align(bucketpos);

  /* single malloc, then fill in all fields */
  if ((tab = malloc(bucketpos + (tabsize * (sizeof(*(tab->tab))))))) {
    tab->tab = (ei_bucket **)((char *)tab + bucketpos);
    memset(tab->tab,0,tabsize*sizeof(*(tab->tab)));
    tab->hash = ei_dohash;
    tab->size = tabsize;
    tab->npos = 0;
    tab->nelem = 0;
    tab->freelist = NULL;
  }

  return tab;
}

