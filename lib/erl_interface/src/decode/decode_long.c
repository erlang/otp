/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1998-2009. All Rights Reserved.
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
#include "eidef.h"
#include "eiext.h"
#include "putget.h"

#ifndef EI_64BIT
int ei_decode_long(const char *buf, int *index, long *p)
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
	    unsigned long u = 0;

	    /* Little Endian, and n always positive, except for LONG_MIN */
	    for (i = 0; i < arity; i++) {
		if (i < 4) {
		    u |= get8(s) << (i * 8);
		} else if (get8(s) != 0) {
		    return -1; /* All but first byte have to be 0 */
		}
	    }

	    /* check for overflow */
	    if (sign) {
		if (u > 0x80000000UL) {
		    return -1;
		}
		n = -((long)u);
	    } else {
		if (u > 0x7FFFFFFF) {
		    return -1;
		}
		n = (long)u;
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
#endif /* !EI_64BIT */
