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

#if defined(HAVE_GMP_H) && defined(HAVE_LIBGMP)

#include <gmp.h>

#include "eidef.h"
#include "eiext.h"
#include "putget.h"


int ei_decode_bignum(const char *buf, int *index, mpz_t obj)
{
    const char *s = buf + *index;
    const char *s0 = s;
    int arity;
    int sign;
    unsigned long n;

    switch (get8(s)) {
    case ERL_SMALL_INTEGER_EXT:
	n = get8(s);
	mpz_set_ui(obj, n);
	break;
    
    case ERL_INTEGER_EXT:
	n = get32be(s);
	mpz_set_ui(obj, n);
	break;
    
    case ERL_SMALL_BIG_EXT:
	arity = get8(s);
	goto decode_bytes;

    case ERL_LARGE_BIG_EXT:
	arity = get32be(s);
    decode_bytes:
	sign = get8(s);
	mpz_import(obj, arity, -1, 1, 0, 0, s);
	s += arity;
	if (sign) {
	    mpz_neg(obj, obj);
	}
    
	break;
    
    default:
	return -1;
    }

    *index += s-s0;
  
    return 0; 
}

#endif /* HAVE_GMP_H && HAVE_LIBGMP */
