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

int ei_decode_binary(const char *buf, int *index, void *p, long *lenp)
{
  const char *s = buf + *index;
  const char *s0 = s;
  long len;

  if (get8(s) != ERL_BINARY_EXT) return -1;

  len = get32be(s);
  if (p) memmove(p,s,len);
  s += len;

  if (lenp) *lenp = len;
  *index += s-s0; 

  return 0; 
}


