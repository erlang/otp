/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2016. All Rights Reserved.
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
 *

 */
#include <string.h>

#include "eidef.h"
#include "eiext.h"
#include "ei_decode_term.h"
#include "putget.h"

/* Returns 1 on successful encoding, -1 on error, and 0 if the term seems
   alright, but does not fit in the term structure. If it returns 1, the
   index will be incremented, and the term contains the decoded term. */

int ei_decode_ei_term(const char* buf, int* index, ei_term* term)
{
    const char* s = buf + *index, * s0 = s;
    int n, sign;
    char c;

    if (term == NULL) return -1;
    c = term->ei_type = get8(s);
    switch (c) {
    case ERL_SMALL_INTEGER_EXT:
	term->value.i_val = get8(s);
	break;
    case ERL_INTEGER_EXT:
	term->value.i_val = get32be(s);
	break;
    case ERL_FLOAT_EXT:
    case NEW_FLOAT_EXT:
        return (ei_decode_double(buf, index, &term->value.d_val) < 0
                ? -1 : 1);
    case ERL_ATOM_EXT:
    case ERL_ATOM_UTF8_EXT:
    case ERL_SMALL_ATOM_EXT:
    case ERL_SMALL_ATOM_UTF8_EXT:
	return (ei_decode_atom(buf, index, term->value.atom_name) < 0
                ? -1 : 1);
    case ERL_REFERENCE_EXT:
    case ERL_NEW_REFERENCE_EXT:
    case ERL_NEWER_REFERENCE_EXT:
        return (ei_decode_ref(buf, index, &term->value.ref) < 0
                ? -1 : 1);
    case ERL_PORT_EXT:
    case ERL_NEW_PORT_EXT:
        return (ei_decode_port(buf, index, &term->value.port) < 0
                ? -1 : 1);
    case ERL_PID_EXT:
    case ERL_NEW_PID_EXT:
        return (ei_decode_pid(buf, index, &term->value.pid) < 0
                ? -1 : 1);
    case ERL_SMALL_TUPLE_EXT:
	term->arity = get8(s);
	break;
    case ERL_LARGE_TUPLE_EXT:
	term->arity = get32be(s);
	break;
    case ERL_NIL_EXT:
	term->arity = 0;
	break;
    case ERL_STRING_EXT:
	term->size = get16be(s);
	return 0;
    case ERL_LIST_EXT:
    case ERL_MAP_EXT:
	term->arity = get32be(s);
	break;
    case ERL_BINARY_EXT:
	term->size = get32be(s);
	return 0;
    case ERL_BIT_BINARY_EXT: {
        int bytes = get32be(s);
        int last_bits = get8(s);
        if (((last_bits==0) != (bytes==0)) || last_bits > 8)
            return -1;
        term->size = bytes;
        return 0;
    }
    case ERL_SMALL_BIG_EXT:
	if ((term->arity = get8(s)) != 4) return -1;
	sign = get8(s);
	/* Little Endian, and n always positive, except for LONG_MIN */
	n = get32le(s);
	if (sign) {
	    /* check for overflow */
	    if ((n - 1) < 0) return -1;
	    n = -n;
	} else {
	    /* check for overflow */
	    if (n < 0) return -1;
	}
	break;
    case ERL_LARGE_BIG_EXT:
	return 0;
    case ERL_PASS_THROUGH:
	return 0;
    case ERL_NEW_CACHE:
	return -1;
    case ERL_CACHED_ATOM:
	return -1;
    default:
	return -1;
    }
    *index += s-s0;
    return 1;
}
