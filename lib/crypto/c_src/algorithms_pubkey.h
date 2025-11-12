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
size_t pubkey_algorithms_lazy_init(ErlNifEnv* env, bool fips_enabled);
ERL_NIF_TERM pubkey_algorithms_as_list(ErlNifEnv* env, bool fips_enabled);
void pubkey_add_algorithm(ErlNifEnv* env, const char* str_v3, unsigned unavailable, ERL_NIF_TERM atom);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
struct pubkey_probe_t;

struct pubkey_availability_t {
    const pubkey_probe_t* init = nullptr; // the pubkey_probe_t used to create this record

    struct {
        bool not_available : 1; // algorithm init failed
        bool fips_forbidden_keygen : 1;
        bool fips_forbidden_sign : 1;
        bool fips_forbidden_verify : 1;
        bool fips_forbidden_encrypt : 1;
        bool fips_forbidden_derive : 1;
    } flags = {};

    bool is_forbidden_in_fips() const {
#ifdef FIPS_SUPPORT
        // Forbidden in FIPS if all operations are forbidden, or if algorithm is not available at all
        return (this->flags.not_available ||
                this->flags.fips_forbidden_keygen && this->flags.fips_forbidden_sign &&
                    this->flags.fips_forbidden_verify && this->flags.fips_forbidden_encrypt &&
                    this->flags.fips_forbidden_derive) &&
            FIPS_MODE();
#else
        return false;
#endif
    }
    // Return the atom which goes to the Erlang caller
    ERL_NIF_TERM get_atom() const;
#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    void check_against_fips(); // Result: flags set if FIPS is not supported
#endif // FIPS_SUPPORT && HAS_3_0_API
};

// A probe contains data required for creating the algorithm description structure and testing
// its availability. Each probe() call done by the algorithm_collection_t might or might not
// result in a new available algorithm creation.
struct pubkey_probe_t {
    const char* str = nullptr;
    const char* str_v3 = nullptr; // if this is nullptr, .str will be used instead
    ERL_NIF_TERM atom = 0;

    // Perform a probe on the algorithm. In case of success, fill the struct and push into the 'output'
    void probe(ErlNifEnv* env, bool fips_enabled, std::vector<pubkey_availability_t>& output);
};

using pubkey_collection_t = algorithm_collection_t<pubkey_availability_t, pubkey_probe_t>;
extern pubkey_collection_t pubkey_collection;

#endif // __cplusplus
