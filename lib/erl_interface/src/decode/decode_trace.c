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
#include "eidef.h"
#include "putget.h"

int ei_decode_trace(const char *buf, int *index, erlang_trace *p)
{
  int arity = 0;
  int tindex = *index;
  
  /* use a temporary index if any function should fail */

  /* { Flags, Label, Serial, FromPid, Prev } */
  if (ei_decode_tuple_header(buf, &tindex, &arity)
      || (arity != 5)
      || ei_decode_long(buf, &tindex, &p->flags)
      || ei_decode_long(buf, &tindex, &p->label)
      || ei_decode_long(buf, &tindex, &p->serial)
      || ei_decode_pid( buf, &tindex, &p->from)
      || ei_decode_long(buf, &tindex, &p->prev)) return -1;

  /* index is updated by the functions we called */
  
  *index = tindex;

  return 0;
}
