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

int ei_encode_trace(char *buf, int *index, const erlang_trace *p)
{
  /* { Flags, Label, Serial, FromPid, Prev } */
  ei_encode_tuple_header(buf,index,5);
  ei_encode_long(buf,index,p->flags);
  ei_encode_long(buf,index,p->label);
  ei_encode_long(buf,index,p->serial);
  ei_encode_pid(buf,index,&p->from);
  ei_encode_long(buf,index,p->prev);

  /* index is updated by the functions we called */
  
  return 0;
}

