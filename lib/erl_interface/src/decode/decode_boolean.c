/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2013. All Rights Reserved.
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
  char tbuf[6];
  int t;

  if (ei_decode_atom_as(buf, index, tbuf, sizeof(tbuf), ERLANG_ASCII, NULL, NULL) < 0)
      return -1;

  if (memcmp(tbuf, "true", 5) == 0)
      t = 1;
  else if (memcmp(tbuf, "false", 6) == 0)
      t = 0;
  else
      return -1;
      
  if (p) *p = t;
  return 0; 
}

