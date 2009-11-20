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
#include "ei_x_encode.h"
#include "erl_interface.h"
#include "erl_marshal.h"

/* FIXME: depends on old erl_interface */

int ei_x_encode_term(ei_x_buff* x, void* t)
{
  int i = x->index;
  ei_encode_term(NULL, &i, t);
  if (!x_fix_buff(x, i))
    return -1;
  return ei_encode_term(x->buff, &x->index, t);
}

int ei_encode_term(char *buf, int *index, void *t)
{
  char *s = buf + *index;
  char *s0 = s;

  if (!buf) s += erl_term_len(t) -1; /* -1 for version */
  else {
    /* this encodes all but the version at the start */
    /* and it will move s forward the right number of bytes */
    if (erl_encode_it(t,(unsigned char **)&s, 5)) return -1;
  }
  
  *index += s - s0;
  
  return 0;
}

