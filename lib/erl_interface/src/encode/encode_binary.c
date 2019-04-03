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
#include <string.h>
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

int ei_encode_binary(char *buf, int *index, const void *p, long len)
{
  char *s = buf + *index;
  char *s0 = s;

  if (!buf) s += 5;
  else {
    put8(s,ERL_BINARY_EXT);
    put32be(s,len);
    memmove(s,p,len);
  }
  s += len;
  
  *index += s-s0; 

  return 0; 
}

int ei_encode_bitstring(char *buf, int *index, const void *p, size_t bits)
{
  char *s = buf + *index;
  char *s0 = s;
  size_t bytes = (bits + 7) / 8;
  char last_bits = bits % 8;

  if (bytes == 0 || last_bits == 0)
      return ei_encode_binary(buf, index, p, bytes);

  if (!buf) s += 6;
  else {
      put8(s, ERL_BIT_BINARY_EXT);
      put32be(s, bytes);
      put8(s, last_bits);
      memcpy(s, p, bytes);
      s[bytes-1] &= (0xff << (8-last_bits));
  }
  s += bytes;

  *index += s-s0;

  return 0;
}
