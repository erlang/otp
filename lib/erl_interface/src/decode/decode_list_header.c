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
#include "eiext.h"
#include "putget.h"

int ei_decode_list_header(const char *buf, int *index, int *arity)
{
  const char *s = buf + *index;
  const char *s0 = s;

  switch (get8(s)) {
  case ERL_NIL_EXT:
    if (arity) *arity = 0;
    break;
    
  case ERL_LIST_EXT:
    if (arity) *arity = get32be(s);
    else s+= 4;
    break;

  default:
    return -1;
  }
  
  *index += s-s0; 

  return 0;
}
