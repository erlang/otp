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

/* move the elements from oldtab to a new table newsize. The old table
 * is freed and the caller should discard the pointer. On failure
 * (i.e. if malloc fails) return the old table and do nothing. 
*/
ei_hash *ei_hash_resize(ei_hash *oldtab, int newsize)
{
  ei_hash *newtab=NULL;
  ei_bucket *b, *next;
  int i,h;

  /* make sure size is odd, then increase until prime */
  newsize |= 0x1; 
  while (!ei_isprime(newsize)) newsize +=2;

  if (newsize == oldtab->size) return oldtab;
  
  /* make a new table */
  if (!(newtab = ei_hash_newtab(newsize))) return oldtab;
  newtab->hash = oldtab->hash;

  /* move the buckets, rehashing */
  /* note that this will reverse the order of any chains */
  for (i=0; i<oldtab->size; i++) {
    b=oldtab->tab[i];
    while (b) {
      next = b->next;
      h = b->rawhash % newtab->size;
      b->next=newtab->tab[h];
      if (!newtab->tab[h]) newtab->npos++;
      newtab->tab[h]=b;
      b = next;
    }
  }
  /* the new table has the same number of elements as the old one */
  newtab->nelem = oldtab->nelem;

  /* the new table takes over the freelist from the old one */
  newtab->freelist = oldtab->freelist;

  /* now it's safe to remove the old table */
  free(oldtab);

  return newtab;
}
