/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

#ifndef E_DIGEST_H__
#define E_DIGEST_H__ 1

#include "common.h"

struct digest_type_t {
    union {
	const char*  str;        /* before init, NULL for end-of-table */
	ERL_NIF_TERM atom;       /* after init, 'false' for end-of-table */
    }type;
    unsigned flags;
    union {
	const EVP_MD* (*funcp)(void);  /* before init, NULL if notsup */
	const EVP_MD* p;               /* after init, NULL if notsup */
    }md;
};

/* masks in the flags field if digest_type_t */
#define NO_FIPS_DIGEST 1

#ifdef FIPS_SUPPORT
/* May have FIPS support, must check dynamically if it is enabled */
# define DIGEST_FORBIDDEN_IN_FIPS(P) (((P)->flags & NO_FIPS_DIGEST) && FIPS_mode())
#else
/* No FIPS support since the symbol FIPS_SUPPORT is undefined */
# define DIGEST_FORBIDDEN_IN_FIPS(P) 0
#endif


void init_digest_types(ErlNifEnv* env);
struct digest_type_t* get_digest_type(ERL_NIF_TERM type);

#endif /* E_DIGEST_H__ */
