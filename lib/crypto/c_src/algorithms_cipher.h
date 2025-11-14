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

#ifdef __cplusplus
extern "C" {
#endif

#include "common.h"

//
// Supported Cipher Algorithms storage C API
//
size_t cipher_algorithms_lazy_init(ErlNifEnv* env, bool fips_enabled);
ERL_NIF_TERM cipher_algorithms_as_list(ErlNifEnv* env, bool fips_enabled);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#include "algorithms_collection.h"

struct cipher_probe_t;

// Describes a cipher algorithm added by the collection's probe function, and checked for compatibility
// with FIPS if FIPS mode was on. If the FIPS mode changes this will be destroyed and
// created again.
struct cipher_availability_t {
    const cipher_probe_t* init = nullptr; // the cipher_probe_t used to create this record

    struct {
        bool fips_forbidden : 1;
    } flags = {};

    bool is_forbidden_in_fips() const {
#ifdef FIPS_SUPPORT
        return this->flags.fips_forbidden && FIPS_MODE();
#else
        return false;
#endif
    }
    bool is_available() const { return true; }
    // Return the atom which goes to the Erlang caller
    ERL_NIF_TERM get_atom() const;
};

// A probe contains data required for creating the algorithm description structure and testing
// its availability. Each probe() call done by the algorithm_collection_t might or might not
// result in a new available algorithm creation.
struct cipher_probe_t {
    const char* str_v3 = nullptr;
    ERL_NIF_TERM atom = 0;

    // Attempt to add a new known Cipher algorithm. In case of success, fill the struct and push into the 'output'
    void probe(ErlNifEnv* env, bool fips_enabled, std::vector<cipher_availability_t>& output);
    // Used as a stopper by the algorithm_collection_t
    bool is_last() const { return this->str_v3 == nullptr; }
};

using cipher_collection_t = algorithm_collection_t<cipher_availability_t, cipher_probe_t>;
extern cipher_collection_t cipher_collection;

#endif // __cplusplus
