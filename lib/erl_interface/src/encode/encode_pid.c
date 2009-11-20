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

int ei_encode_pid(char *buf, int *index, const erlang_pid *p)
{
  char *s = buf + *index;
  char *s0 = s;
  int len = strlen(p->node);
  
  if (!buf) s += 13 + len;
  else {
    put8(s,ERL_PID_EXT);

    /* first the nodename */
    put8(s,ERL_ATOM_EXT);

    put16be(s,len);
  
    memmove(s, p->node, len);
    s += len;

    /* now the integers */
    put32be(s,p->num & 0x7fff); /* 15 bits */
    put32be(s,p->serial & 0x1fff); /* 13 bits */
    put8(s,(p->creation & 0x03)); /* 2 bits */
  }
  
  *index += s-s0;
  
  return 0;
}

