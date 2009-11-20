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

/* c non-zero -> erlang "true" atom, otherwise "false" */
int ei_decode_boolean(const char *buf, int *index, int *p)
{
  const char *s = buf + *index;
  const char *s0 = s;
  int len;
  int t;

  if (get8(s) != ERL_ATOM_EXT) return -1;

  len = get16be(s);

  switch (len) {
  case 4:
    /* typecast makes ansi happy */
    if (strncmp((char*)s,"true",4)) return -1;
    t = 1;
    break;

  case 5:
    if (strncmp((char*)s,"false",5)) return -1;
    t = 0;
    break;
    
  default:
    return -1;
  }
  
  s += len;
  if (p) *p = t;
  *index += s-s0;

  return 0; 
}
