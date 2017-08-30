/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2002-2016. All Rights Reserved.
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

#ifdef EI_64BIT
int ei_decode_long(const char *buf, int *index, long *p)
{
    return ei_decode_longlong(buf, index, (EI_LONGLONG *)p);
}
#endif

#ifdef _MSC_VER
#define MAX_TO_NEGATE 0x8000000000000000Ui64
#define MAX_TO_NOT_NEGATE 0x7FFFFFFFFFFFFFFFUi64
#else 
#define MAX_TO_NEGATE 0x8000000000000000ULL
#define MAX_TO_NOT_NEGATE 0x7FFFFFFFFFFFFFFFULL
#endif

int ei_decode_longlong(const char *buf, int *index, EI_LONGLONG *p)
{
    const char *s = buf + *index;
    const char *s0 = s;
    EI_LONGLONG n;
    int arity;

    switch (get8(s)) {
    case ERL_SMALL_INTEGER_EXT:
	n = get8(s);
	break;
    
    case ERL_INTEGER_EXT:
	n = get32be(s);
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
	    EI_ULONGLONG u = 0;

	    /* Little Endian, and n always positive, except for LONG_MIN */
	    for (i = 0; i < arity; i++) {
		if (i < 8) {
		    /* Use ULONGLONG not to get a negative integer if > 127 */
		    u |= ((EI_ULONGLONG)get8(s)) << (i * 8);
		} else if (get8(s) != 0) {
		    return -1; /* All but first byte have to be 0 */
		}
	    }

	    /* check for overflow */
	    if (sign) {
		if (u > MAX_TO_NEGATE) {
		    return -1;
		}
		n = -((EI_LONGLONG) u);
	    } else {
		if (u > MAX_TO_NOT_NEGATE) {
		    return -1;
		}
		n = (EI_LONGLONG) u;
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
