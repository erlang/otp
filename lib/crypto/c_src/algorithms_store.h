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

#ifndef E_ALGORITHMS_STORE_H
#define E_ALGORITHMS_STORE_H 1

#include "common.h"

enum PKEY_AVAIL_FLAGS {
    FIPS_PKEY_NOT_AVAIL = 1,
    FIPS_FORBIDDEN_PKEY_KEYGEN = 2, /* not available by name */
    FIPS_FORBIDDEN_PKEY_SIGN = 4, /* not available for signing */
    FIPS_FORBIDDEN_PKEY_VERIFY = 8, /* not available for verification */
    FIPS_FORBIDDEN_PKEY_ENCRYPT = 16, /* not available for encryption */
    FIPS_FORBIDDEN_PKEY_DERIVE = 32, /* not available for key derivation */
    FIPS_FORBIDDEN_PKEY_ALL = FIPS_FORBIDDEN_PKEY_KEYGEN | FIPS_FORBIDDEN_PKEY_SIGN | FIPS_FORBIDDEN_PKEY_VERIFY |
                              FIPS_FORBIDDEN_PKEY_ENCRYPT | FIPS_FORBIDDEN_PKEY_DERIVE
};

/* C bool is int */
bool create_algorithm_mutexes(void);
void free_algorithm_mutexes(void);

typedef void (*init_algorithms_fn)(ErlNifEnv *env, bool fips_enabled);

void pubkey_algorithms_reset_cache(void);
size_t pubkey_algorithms_lazy_init(ErlNifEnv *env, bool fips_enabled, init_algorithms_fn init_algorithms);
ERL_NIF_TERM pubkey_algorithms_as_list(ErlNifEnv *env, bool fips_enabled);
void pubkey_add_algorithm(ErlNifEnv *env, const char *str_v3, unsigned unavailable, ERL_NIF_TERM atom);

#endif /* E_ALGORITHMS_STORE_H */
