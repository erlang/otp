/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2010-2025. All Rights Reserved.
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

#ifndef E_PKEY_H__
#define E_PKEY_H__ 1

#include "common.h"

void prefetched_sign_algo_init(ErlNifEnv*);

enum pkey_format_t {
    PKEY_PUB  = 0,
    PKEY_PRIV = 1,
    PKEY_PRIV_SEED = 2
};

struct pkey_type_t {
    union {
        const char* atom_str;   // before init
        ERL_NIF_TERM atom;      // after init
    }name;
    const int evp_pkey_id;
    union {
        const char* alg_str;    // before init
#ifdef HAS_PREFETCH_SIGN_INIT
        EVP_SIGNATURE* alg;     // after init
#endif
    } sign;
    const bool allow_seed;
};

struct pkey_type_t* get_pkey_type(ERL_NIF_TERM alg_atom);
ERL_NIF_TERM build_pkey_type_list(ErlNifEnv* env, ERL_NIF_TERM tail, bool fips);

#ifdef HAS_3_0_API
int get_pkey_from_octet_string(ErlNifEnv*,
                               ERL_NIF_TERM alg_atom,
                               ERL_NIF_TERM key_bin,
                               enum pkey_format_t,
                               struct pkey_type_t *pkey_type,
                               EVP_PKEY **pkey_p,
                               ERL_NIF_TERM *ret_p);
#endif

ERL_NIF_TERM pkey_sign_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM pkey_sign_heavy_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM pkey_verify_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM pkey_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM privkey_to_pubkey_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif /* E_PKEY_H__ */
