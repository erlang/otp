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

static void copy_bits(const unsigned char* src, size_t soffs,
                      unsigned char* dst, size_t n);


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

int ei_encode_bitstring(char *buf, int *index,
                        const char *p,
                        size_t bitoffs,
                        size_t bits)
{
  char *s = buf + *index;
  char *s0 = s;
  size_t bytes = (bits + 7) / 8;
  char last_bits = bits % 8;

  if (!buf) s += last_bits ? 6 : 5;
  else {
      char* tagp = s++;
      put32be(s, bytes);
      if (last_bits) {
          *tagp = ERL_BIT_BINARY_EXT;
          put8(s, last_bits);
      }
      else
          *tagp = ERL_BINARY_EXT;

      copy_bits((const unsigned char*)p, bitoffs, (unsigned char*)s, bits);
  }
  s += bytes;

  *index += s-s0;

  return 0;
}


/*
 * MAKE_MASK(n) constructs a mask with n bits.
 * Example: MAKE_MASK(3) returns the binary number 00000111.
 */
#define MAKE_MASK(n) ((((unsigned) 1) << (n))-1)


static
void copy_bits(const unsigned char* src, /* Base pointer to source. */
	       size_t soffs,	         /* Bit offset for source relative to src. */
	       unsigned char* dst,	 /* Destination. */
	       size_t n)	         /* Number of bits to copy. */
{
    unsigned rmask;
    unsigned count;
    unsigned deoffs;
    unsigned bits;
    unsigned bits1;
    unsigned rshift;

    if (n == 0)
        return;

    deoffs = n & 7;
    rmask = deoffs ? (MAKE_MASK(deoffs) << (8-deoffs)) : 0;

    if (soffs == 0) {
        unsigned nbytes = (n + 7) / 8;
        memcpy(dst, src, nbytes);
        if (rmask)
            dst[nbytes-1] &= rmask;
        return;
    }

    src += soffs / 8;
    soffs &= 7;

    if (n < 8) {     /* Less than one byte */
        bits = (*src << soffs);
        if (soffs+n > 8) {
            src++;
            bits |= (*src >> (8 - soffs));
        }
        *dst = bits & rmask;
	return;
    }

    count = n >> 3;

    rshift = 8 - soffs;
    bits = *src;
    if (soffs + n > 8) {
        src++;
    }

    while (count--) {
        bits1 = bits << soffs;
        bits = *src;
        src++;
        *dst = bits1 | (bits >> rshift);
        dst++;
    }

    if (rmask) {
        bits1 = bits << soffs;
        if ((rmask << rshift) & 0xff) {
            bits = *src;
            bits1 |= (bits >> rshift);
        }
        *dst = bits1 & rmask;
    }
}
