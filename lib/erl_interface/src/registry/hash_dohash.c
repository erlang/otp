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

/* This is hashpjw, from the dragon book */
/* Note that this function is only used as a default hash function.
 * All calls are made through the hash pointer in the tab structure.
 * The only place this function is explicitly referenced is in
 * hash_newtab(); Users can use hash_setfunc() to change the hash function.
 */
int ei_dohash(const char *key) 
{
  const char *s;
  unsigned h = 0;
  unsigned g;
  
  for (s=key; *s; s++) {
    h = (h << 4) + *s;
    if ((g = (h & 0xf0000000))) { /* assumes 32-bit int */
      h = h^(g >> 24);
      h = h^g;
    }
  }
  return h;
}


