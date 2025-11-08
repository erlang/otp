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
// Curve Algorithms storage API
//
size_t curve_algorithms_lazy_init(ErlNifEnv *env, bool fips_enabled);
void curve_add_algorithm(ErlNifEnv *env, const char *str_v3, unsigned unavail_flags);
ERL_NIF_TERM curve_algorithms_as_list(ErlNifEnv *env, bool fips_enabled);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
struct curve_availability_t {
    const char *str_v3; // the algorithm name as in OpenSSL 3.x
    unsigned flags; // combination of CURVE_AVAIL_FLAGS
    ERL_NIF_TERM atom; // as returned to the library user on a query

    bool is_forbidden_in_fips() const {
#ifdef FIPS_SUPPORT
        return this->flags != 0 && FIPS_MODE();
#else
        return false;
#endif
    }
    // Return the atom which goes to the Erlang caller
    ERL_NIF_TERM get_atom() const { return this->atom; }
};

enum CURVE_AVAIL_FLAGS {
    FIPS_CURVE_INIT_FAILED = 1 // could not find by name or initialize
};

struct curve_probe_t {
    // Perform probe on the algorithm. In case of success, fill the struct and push into the 'output'
    void probe(ErlNifEnv *env, bool fips_mode, std::vector<curve_availability_t> &output);
};

// Forward declaration, find
using curve_collection_t = algorithm_collection_t<curve_availability_t, curve_probe_t>;
extern curve_collection_t curve_collection;

#endif // __cplusplus
