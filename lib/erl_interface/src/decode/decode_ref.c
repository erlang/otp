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
 */
#include <string.h>
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

int ei_decode_ref(const char *buf, int *index, erlang_ref *p)
{
  const char *s = buf + *index;
  const char *s0 = s;
  int count, len, i;
  
  switch (get8(s)) {
    case ERL_REFERENCE_EXT:

      /* first the nodename */
      if (get8(s) != ERL_ATOM_EXT) return -1;

      len = get16be(s);

      if (p) {
	  memmove(p->node, s, len);
	  p->node[len] = (char)0;
      }
      s += len;
  
      /* now the numbers: num (4), creation (1) */
      if (p) {
	  p->n[0] = get32be(s);
	  p->len = 1;
	  p->creation = get8(s) & 0x03;
      }
      else s += 5;
  
      *index += s-s0;
  
      return 0;
      break;
      
    case ERL_NEW_REFERENCE_EXT:
      /* first the integer count */
      count = get16be(s);
      if (p) p->len = count;

      /* then the nodename */
      if (get8(s) != ERL_ATOM_EXT) return -1;
      len = get16be(s);

      if (p) {
	  memmove(p->node, s, len);
	  p->node[len] = (char)0;
      }
      s += len;

      /* creation */
      if (p) {
	  p->creation = get8(s) & 0x03;
      }
      else s += 1;

      /* finally the id integers */
      if (p) {
	for (i = 0; (i<count) && (i<3); i++) {
	  p->n[i] = get32be(s);
	}
      }
      else s += 4 * count;
  
      *index += s-s0;
  
      return 0;
      break;
      
    default:
      return -1;
  }
}
