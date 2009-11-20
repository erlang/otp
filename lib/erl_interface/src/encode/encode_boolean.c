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

int ei_encode_boolean(char *buf, int *index, int p)
{
  char *s = buf + *index;
  char *s0 = s;
  char *val;
  int len;

  val = p ? "true" : "false";
  len = strlen(val);

  if (!buf) s += 3;
  else {
    put8(s,ERL_ATOM_EXT);
    put16be(s,len);

    memmove(s,val,len); /* unterminated string */
  }
  s += len;

  *index += s-s0; 

  return 0; 
}

