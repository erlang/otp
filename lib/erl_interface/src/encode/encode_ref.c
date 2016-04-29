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
 */
#include <string.h>
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

int ei_encode_ref(char *buf, int *index, const erlang_ref *p)
{
  const char tag = (p->creation > 3) ? ERL_NEWER_REFERENCE_EXT : ERL_NEW_REFERENCE_EXT;
  char *s = buf + *index;
  int i;

  (*index) += 1 + 2; /* skip to node atom */
  if (ei_encode_atom_len_as(buf, index, p->node, strlen(p->node), ERLANG_UTF8,
			    ERLANG_LATIN1|ERLANG_UTF8) < 0) {
      return -1;
  }

  /* Always encode as an extended reference; all participating parties
     are now expected to be able to decode extended references. */
  if (buf) {
	  put8(s, tag);

	  /* first, number of integers */
	  put16be(s, p->len);

	  /* then the nodename */
	  s = buf + *index;

	  /* now the integers */
          if (tag == ERL_NEW_REFERENCE_EXT)
              put8(s,(p->creation & 0x03));
          else
              put32be(s, p->creation);
	  for (i = 0; i < p->len; i++)
	      put32be(s,p->n[i]);
  }
  
  *index += p->len*4 + (tag == ERL_NEW_REFERENCE_EXT ? 1 : 4);
  return 0;
}

