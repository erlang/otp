/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2011. All Rights Reserved.
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

static int utf8_to_latin1(char* dest, const char* source, unsigned len);

int ei_decode_atom(const char *buf, int *index, char *p)
{
  const char *s = buf + *index;
  const char *s0 = s;
  int len;

  switch (get8(s)) {
  case ERL_ATOM_EXT:
      len = get16be(s);
      if (len > MAXATOMLEN) return -1;
      if (p) {
	  memmove(p,s,len); 
	  p[len] = (char)0;
      }
      break;

  case ERL_SMALL_ATOM_EXT:
      len = get8(s);
      if (p) {
	  memmove(p,s,len); 
	  p[len] = (char)0;
      }
      break;

  case ERL_UNICODE_ATOM_EXT:
      len = get16be(s);

      if (len > 2*MAXATOMLEN) return -1;

      if (p && utf8_to_latin1(p, s, len) < 0) return -1;
      break;

  default:
      return -1;
  }

  s += len;
  *index += s-s0;
  return 0;
}

int ei_internal_get_atom(const char** bufp, char* p)
{
    int ix = 0;
    if (ei_decode_atom(*bufp, &ix, p) < 0) return -1;
    *bufp += ix;
    return 0;
}

static int utf8_to_latin1(char* dest, const char* source, unsigned slen)
{
    const char* dest_end = dest + MAXATOMLEN - 1;

    while (slen > 0 && dest < dest_end) {
	if ((source[0] & 0x80) == 0) {
	    *dest++ = *source++;
	    --slen;
	}
	else if (slen > 1 &&
		 (source[0] & 0xFE) == 0xC2 &&
		 (source[1] & 0xC0) == 0x80) {
	    *dest++ = (char) ((source[0] << 6) | (source[1] & 0x3F));
	    source += 2;
	    slen -= 2;
	}
	else return -1;
    }
    *dest = 0;
    return 0;
}

