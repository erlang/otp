/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2010. All Rights Reserved.
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
#include <stdio.h>
#include "eidef.h"
#include "eiext.h"
#include "putget.h"


int ei_decode_double(const char *buf, int *index, double *p)
{
  const char *s = buf + *index;
  const char *s0 = s;
  FloatExt f;

  switch (get8(s)) {
    case ERL_FLOAT_EXT:
      if (sscanf(s, "%lf", &f.d) != 1) return -1;
      s += 31;
      break;
    case NEW_FLOAT_EXT:
      /* IEEE 754 format */
      f.val = get64be(s);
      break;
    default:
      return -1;
  }

  if (p) *p = f.d;
  *index += s-s0; 
  return 0; 
}
