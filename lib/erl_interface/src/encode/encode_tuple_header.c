/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2014. All Rights Reserved.
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
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

int ei_encode_tuple_header(char *buf, int *index, int arity)
{
  char *s = buf + *index;
  char *s0 = s;
  
  if (arity < 0) return -1;

  if (arity <= 0xff) {
    if (!buf) s += 2;
    else {
      put8(s,ERL_SMALL_TUPLE_EXT);
      put8(s,arity);
    }
  }
  else {
    if (!buf) s += 5;
    else {
      put8(s,ERL_LARGE_TUPLE_EXT);
      put32be(s,arity);
    }
  }

  *index += s-s0; 

  return 0;
}

int ei_encode_map_header(char *buf, int *index, int arity)
{
  char *s = buf + *index;
  char *s0 = s;

  if (arity < 0) return -1;

  if (!buf) s += 5;
  else {
      put8(s,ERL_MAP_EXT);
      put32be(s,arity);
  }

  *index += s-s0;

  return 0;
}
