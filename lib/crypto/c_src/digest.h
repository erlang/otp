/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2021. All Rights Reserved.
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
    const char*  str;        /* before init, NULL for end-of-table */
    const char* str_v3;      /* the algorithm name as in OpenSSL 3.x */
    ERL_NIF_TERM atom;       /* after init, 'false' for end-of-table */
    unsigned flags;
    struct {
        const EVP_MD* (*funcp)(void);  /* before init, NULL if notsup */
        const EVP_MD* p;              /* after init, NULL if notsup */
    }md;
};

/* masks in the flags field if digest_type_t */
#define NO_FIPS_DIGEST 1
#define PBKDF2_ELIGIBLE_DIGEST 2

#ifdef FIPS_SUPPORT
# define DIGEST_FORBIDDEN_IN_FIPS(P) (((P)->flags & NO_FIPS_DIGEST) && FIPS_MODE())
#else
# define DIGEST_FORBIDDEN_IN_FIPS(P) 0
#endif


void init_digest_types(ErlNifEnv* env);
struct digest_type_t* get_digest_type(ERL_NIF_TERM type);

#ifdef HAS_3_0_API
ERL_NIF_TERM digest_types_as_list(ErlNifEnv* env);
#endif

#endif /* E_DIGEST_H__ */
