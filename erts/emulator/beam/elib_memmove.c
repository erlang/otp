/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1997-2016. All Rights Reserved.
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

/* 
 * This memmove assumes that both src and dst are aligned on an address
 * divisible by 4 and that n is a multiple of four.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#ifndef HAVE_MEMMOVE

#define MEMCPY_LIMIT 12
typedef unsigned long  u_long;
typedef unsigned short u_short;

static void copy_high(dst, src, n)
char* dst; char* src; int n;
{
    dst += n;
    src += n;

    if (n >= MEMCPY_LIMIT) {
	while(((u_long) dst) & 3) {
	    *--dst = *--src;
	    n--;
	}
	if ((((u_long) src) & 3) == 0) {
	    while(n >= sizeof(u_long)) {
		src -= sizeof(u_long);
		dst -= sizeof(u_long);
		*((u_long*)dst) = *((u_long*)src);
		n -= sizeof(u_long);
	    }
	}
	else if ((((u_short) src) & 3) == 2) {
	    while(n >= sizeof(u_short)) {
		src -= sizeof(u_short);
		dst -= sizeof(u_short);
		*((u_short*)dst) = *((u_short*)src);
		n -= sizeof(u_short);
	    }
	}
    }
    while(n > 0) {
	*--dst = *--src;
	n--;
    }
}

static void copy_low(dst, src, n)
char* dst; char* src; int n;
{
    if (n >= MEMCPY_LIMIT) {
	while(((u_long) dst) & 3) {
	    *dst++ = *src++;
	    n--;
	}
	if ((((u_long) src) & 3) == 0) {
	    while(n >= sizeof(u_long)) {
		*((u_long*)dst) = *((u_long*)src);
		src += sizeof(u_long);
		dst += sizeof(u_long);
		n -= sizeof(u_long);
	    }
	}
	else if ((((u_long) src) & 3) == 2) {
	    while(n >= sizeof(u_short)) {
		*((u_short*)dst) = *((u_short*)src);
		src += sizeof(u_short);
		dst += sizeof(u_short);
		n -= sizeof(u_short);
	    }
	}
    }
    while(n > 0) {
	*dst++ = *src++;
	n--;
    }
}

/*
** Move memory (with overlap)
*/
void* memmove(dst, src, n)
char* dst; char* src; int n;
{
    if (dst < src)
	copy_low(dst, src, n);
    else if (dst > src)
	copy_high(dst, src, n);
    return dst;
}

#endif /* HAVE_MEMMOVE */
