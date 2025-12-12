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
// Curve Algorithms storage API
//
ERL_NIF_TERM curve_algorithms_as_list(ErlNifEnv *env, bool fips_enabled);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#include "algorithms_collection.h"
struct curve_probe_t;

// Describes a curve algorithm added by the collection's probe function, and checked for compatibility
// with FIPS if FIPS mode was on. If the FIPS mode changes this will be destroyed and
// created again.
struct curve_type_t {
    const curve_probe_t *init = nullptr; // the probe which created this record, contains name, atom, etc.
    struct {
        bool fips_forbidden: 1;
        bool algorithm_init_failed: 1; // not possible to create with fips=yes
    } flags = {};

    explicit curve_type_t(const curve_probe_t *probe) : init(probe) {}

#ifdef FIPS_SUPPORT
    // Available if not forbidden with fips=yes, and if curve init did not fail
    bool is_forbidden_in_fips() const {
        return (this->flags.fips_forbidden || this->flags.algorithm_init_failed) && FIPS_MODE();
    }
#else
    static bool is_forbidden_in_fips() { return false; }
#endif

    bool is_available() const { return !this->flags.algorithm_init_failed; }
    // Return the atom which goes to the Erlang caller
    ERL_NIF_TERM get_atom() const;
    // Instantiate the algorithm (if FIPS is enabled) and set flags if not available
    void check_fips_availability(bool fips_mode);
};

// A probe contains data required for creating the algorithm description structure and testing
// its availability. Each probe() call done by the algorithm_collection_t might or might not
// result in a new available algorithm creation.
struct curve_probe_t {
    int nid; // NID_xxxx value (an OpenSSL macro)
    const char *sn; // serves as Erlang atom name, also equal to SN_xxxxx macro of OpenSSL
    ERL_NIF_TERM atom; // Atom for this->sn is cached here

    static constexpr curve_probe_t make(const int nid, const char *sn) { return {nid, sn, /* atom= */ 0}; }

    // Perform a probe on the algorithm. In case of success, fill the struct and push into the 'output'
    void probe(ErlNifEnv *env, bool fips_mode, std::vector<curve_type_t> &output);

    static void post_lazy_init(std::vector<curve_type_t> &) {}
#ifdef HAVE_EC
private:
    bool is_curve_valid_by_nid() const; // used by the probe() to check this->nid
#endif // HAVE_EC
};

// Forward declaration, find
using curve_collection_t = algorithm_collection_t<curve_type_t, curve_probe_t>;
extern curve_collection_t curve_collection;

#endif // __cplusplus
