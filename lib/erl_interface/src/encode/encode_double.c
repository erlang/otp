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
#include <string.h>
#include "eidef.h"
#include "eiext.h"
#include "putget.h"
#if defined(HAVE_ISFINITE)
#include <math.h>
#endif

int ei_encode_double(char *buf, int *index, double p)
{
  char *s = buf + *index;
  char *s0 = s;

  /* Erlang does not handle Inf and NaN, so we return an error rather
   * than letting the Erlang VM complain about a bad external
   * term. */
#if defined(HAVE_ISFINITE)
  if(!isfinite(p)) {
      return -1;
  }
#endif

  if (!buf)
    s += 9;
  else {
    /* IEEE 754 format */
    put8(s, NEW_FLOAT_EXT);
    put64be(s, ((FloatExt*)&p)->val);
  }

  *index += s-s0; 

  return 0; 
}

