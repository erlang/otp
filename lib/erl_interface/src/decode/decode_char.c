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
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

int ei_decode_char(const char *buf, int *index, char *p)
{
  const char *s = buf + *index;
  const char *s0 = s;
  long n;
  int arity;

  switch (get8(s)) {
  case ERL_SMALL_INTEGER_EXT:
    n = get8(s);
    break;
    
  case ERL_INTEGER_EXT:
    n = get32be(s);
    if (n < 0 || n > 255)
      return -1;
    break;
    
  case ERL_SMALL_BIG_EXT:
    arity = get8(s);
    goto decode_big;
  case ERL_LARGE_BIG_EXT:
    arity = get32be(s);
  decode_big:
    {
      int sign = get8(s);
      int i;

      if (sign) return -1;	/* Char is always > 0 */

      n = get8(s);		/* First byte is our value */

      for (i = 1; i < arity; i++) {
	if (*(s++) != 0) return -1; /* All but first byte have to be 0 */
      }
    }
    break;
    
  default:
    return -1;
  }

  if (p) *p = n;
  *index += s-s0;
  
  return 0; 
}
