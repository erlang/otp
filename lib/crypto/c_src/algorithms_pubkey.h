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

#pragma once
#include "algorithms_collection.h"

#ifdef __cplusplus
extern "C" {
#endif

#include "common.h"

//
// Pubkey Algorithms storage C API
//
size_t pubkey_algorithms_lazy_init(ErlNifEnv *env, bool fips_enabled);
ERL_NIF_TERM pubkey_algorithms_as_list(ErlNifEnv *env, bool fips_enabled);
void pubkey_add_algorithm(ErlNifEnv *env, const char *str_v3, unsigned unavailable, ERL_NIF_TERM atom);

#ifdef __cplusplus
}
#endif

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

#ifdef __cplusplus

struct pubkey_probe_t;

struct pubkey_availability_t {
    const pubkey_probe_t *init; // the pubkey_probe_t used to create this record
    size_t flags; // combination of PKEY_AVAIL_FLAGS

    bool is_forbidden_in_fips() const {
#ifdef FIPS_SUPPORT
        return (this->flags == FIPS_FORBIDDEN_PKEY_ALL || this->flags == FIPS_PKEY_NOT_AVAIL) && FIPS_MODE();
#else
        return false;
#endif
    }
    // Return the atom which goes to the Erlang caller
    ERL_NIF_TERM get_atom() const;
};

struct pubkey_probe_t {
    const char *str = nullptr;
    const char *str_v3 = nullptr; // keep this nullptr to avoid duplcaticatiuonon, .str will be used instead
    ERL_NIF_TERM atom = 0;

    // Perform probe on the algorithm. In case of success, fill the struct and push into the 'output'
    void probe(ErlNifEnv *env, bool fips_enabled, std::vector<pubkey_availability_t> &output);

private:
#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    void probe_algorithm_against_fips(size_t flags) const; // fips is supported AND enabled here
#endif // FIPS_SUPPORT && HAS_3_0_API
};

using pubkey_collection_t = algorithm_collection_t<pubkey_availability_t, pubkey_probe_t>;
extern pubkey_collection_t pubkey_collection;

#endif // __cplusplus
