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
// C Digest storage API
//
void digest_types_lazy_init(ErlNifEnv *env, bool fips_mode);
void digest_types_delayed_init(ErlNifEnv *env);
struct digest_availability_t *get_digest_type(ERL_NIF_TERM type);
ERL_NIF_TERM digest_types_as_list(ErlNifEnv *env, bool fips_forbidden);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

// Describes a digest method added by the init function, and checked for compatibility
// with FIPS if FIPS mode was on. If the FIPS mode changes this will be destroyed and
// created again.
struct digest_availability_t {
    // The definition used to create this record
    const struct digest_probe_t *init;
    // combination of DIGEST_TYPE_FLAGS from init + detected in runtime
    size_t flags = 0;
    // after init will contain the algorithm pointer, NULL if not supported. Frees automatically.
    const EVP_MD *md = nullptr;
    // 0 or default digest length for XOF digests
    size_t xof_default_length = 0;

    ~digest_availability_t();

    bool is_forbidden_in_fips() const {
#ifdef FIPS_SUPPORT
        return (this->flags & FIPS_FORBIDDEN_DIGEST) && FIPS_MODE();
#else
        return false;
#endif
    }
    // Return the atom which goes to the Erlang caller
    ERL_NIF_TERM get_atom() const;

    // Fetches the algorithm and sets the initial flags
    void create_md_resource(bool fips_mode);
    // Attempts to instantiate the algorithm (forbidden algorithms will fail)
#if defined(HAS_3_0_API)
    bool is_usable_algorithm(const EVP_MD *md) const;
#endif // HAS_3_0_API
};

// masks in the `flags` field of digest_type_t
enum DIGEST_TYPE_FLAGS {
    FIPS_FORBIDDEN_DIGEST = 1, /* no support in FIPS for digest */
    PBKDF2_ELIGIBLE_DIGEST = 2
};

// This runs for each algorithm at library start and every time FIPS mode changes.
// Describes data required by digest availability probing algorithm (separate branches for
// OpenSSL API < 3.0, 3.0, and for FIPS enabled/disabled).
struct digest_probe_t {
    // the algorithm name as in OpenSSL < 3, also atom used by Erlang API
    const char *str = nullptr;
    // the algorithm name as in OpenSSL 3.x
    const char *str_v3 = nullptr;
    // This will be updated to created atomfound exi
    ERL_NIF_TERM atom = 0;
    // initial DIGEST_TYPE_FLAGS value for digest_type_t::flags
    const size_t flags_hint = 0;
    // OpenSSL 1.0 API to create resource for this digest algorithm (not used in 3.0 API)
    const EVP_MD *(*v1_ctor)() = nullptr;
    size_t xof_default_length = 0;

    // Perform probe on the algorithm. In case of success, fill the struct and push into the 'output'
    void probe(ErlNifEnv *env, bool fips_mode, std::vector<digest_availability_t> &output);
};

using digest_collection_t = algorithm_collection_t<digest_availability_t, digest_probe_t>;
extern digest_collection_t digest_collection;

#endif // __cplusplus
