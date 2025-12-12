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
extern "C"
{
#endif

#include "common.h"

//
// Supported RSA Options storage C API
//
ERL_NIF_TERM rsaopts_as_list(ErlNifEnv *env, bool fips_enabled);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#    include "algorithms_collection.h"
struct rsaopt_probe_t;

// Describes a RSA option added by the collection's probe function, and checked for compatibility
// with FIPS if FIPS mode was on. If the FIPS mode changes this will be destroyed and
// created again.
struct rsaopt_type_t {
    const rsaopt_probe_t *init = nullptr; // the rsaopt_probe_t used to create this record

    struct {
        bool fips_forbidden : 1;
    } flags = {};

    explicit rsaopt_type_t(const rsaopt_probe_t *init_): init(init_) {}
    bool is_forbidden_in_fips() const {
#    ifdef FIPS_SUPPORT
        return this->flags.fips_forbidden && FIPS_MODE();
#    else
        return false;
#    endif
    }
    static bool is_available() {
        return true;
    }
    // Return the atom which goes to the Erlang caller
    ERL_NIF_TERM get_atom() const;
};

// A probe contains data required for creating the algorithm description structure and testing
// its availability. Each probe() call done by the algorithm_collection_t might or might not
// result in a new available algorithm creation.
struct rsaopt_probe_t {
    const char *str_v3;
    ERL_NIF_TERM atom;

    explicit constexpr rsaopt_probe_t(const char *str_v3_): str_v3(str_v3_), atom(0) {}

    // Attempt to add a new known RSA option. In case of success, fill the struct and push into the 'output'
    void probe(ErlNifEnv *env, bool fips_enabled, std::vector<rsaopt_type_t> &output);
    static void post_lazy_init(std::vector<rsaopt_type_t> &) {}
};

using rsaopt_collection_t = algorithm_collection_t<rsaopt_type_t, rsaopt_probe_t>;
extern rsaopt_collection_t rsaopt_collection;

#endif // __cplusplus
