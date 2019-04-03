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

int ei_decode_binary(const char *buf, int *index, void *p, long *lenp)
{
  const char *s = buf + *index;
  const char *s0 = s;
  long len;

  if (get8(s) != ERL_BINARY_EXT) return -1;

  len = get32be(s);
  if (p) memmove(p,s,len);
  s += len;

  if (lenp) *lenp = len;
  *index += s-s0; 

  return 0; 
}

int ei_decode_bitstring(const char *buf, int *index, void *p, size_t plen,
                        size_t *bitsp)
{
  const char *s = buf + *index;
  const char *s0 = s;
  unsigned long len;
  unsigned char last_bits;
  const unsigned char tag = get8(s);

  if (tag == ERL_BINARY_EXT) {
      long bytes;
      int ret = ei_decode_binary(buf, index, p, &bytes);
      if (bitsp)
          *bitsp = (size_t)bytes * 8;
      return ret;
  }

  if (tag != ERL_BIT_BINARY_EXT)
      return -1;

  len = get32be(s);
  last_bits = get8(s);

  if (len > plen || ((last_bits==0) != (len==0)) || last_bits > 8)
      return -1;

  if (p)
      memcpy(p, s, len);
  s += len;

  if (bitsp)
      *bitsp = (len == 0) ? 0 : ((len-1) * 8) + last_bits;

  *index += s-s0;
  return 0;
}

