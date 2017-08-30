/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2016. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
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
