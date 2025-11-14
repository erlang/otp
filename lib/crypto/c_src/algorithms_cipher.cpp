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

#include "algorithms_cipher.h"

cipher_probe_t cipher_probes[] = {
        {} // stopper record
};

cipher_collection_t cipher_collection("crypto.cipher_collection", cipher_probes);

//
// Implementation of Known Cipher Algorithms storage API
//

// C API: Proxy the call to generic algorithm_collection_t
extern "C" size_t cipher_algorithms_lazy_init(ErlNifEnv *env, const bool fips_enabled) {
    return cipher_collection.lazy_init(env, fips_enabled);
}

// C API: Proxy the call to generic algorithm_collection_t
extern "C" ERL_NIF_TERM cipher_algorithms_as_list(ErlNifEnv *env, const bool fips_enabled) {
    return cipher_collection.to_list(env, fips_enabled);
}

ERL_NIF_TERM cipher_availability_t::get_atom() const { return this->init->atom; }

// for FIPS we will attempt to initialize the pubkey context to verify whether the
// algorithm is allowed, for non-FIPS keeping the old behavior - always allow the algorithm.
void cipher_probe_t::probe(ErlNifEnv *env, const bool fips_enabled, std::vector<cipher_availability_t> &output) {
    this->atom = create_or_existing_atom(env, this->str_v3, this->atom);
    const cipher_availability_t algo = {.init = this};
    // No extra checks, just convert name to atom and add
    return output.push_back(algo);
}
