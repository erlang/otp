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

struct digest_availability_t;

#ifdef __cplusplus
extern "C" {
#endif

#include "common.h"

// Wraps a pointer to digest_availability_t which is a C++ struct with C++ features, for use in C API
struct digest_availability_Cptr {
    void* ptr;
};

//
// C Digest storage API
//
void digest_types_lazy_init(ErlNifEnv* env, bool fips_mode);
void digest_types_delayed_init(ErlNifEnv* env);
ERL_NIF_TERM digest_types_as_list(ErlNifEnv* env, bool fips_forbidden);

// Lookup and access fields
struct digest_availability_Cptr get_digest_type(ERL_NIF_TERM type); // linear lookup by atom
bool is_digest_forbidden_in_fips(struct digest_availability_Cptr p); // access C++ member from C
const EVP_MD* digest_availability_md(struct digest_availability_Cptr p); // access field
size_t digest_availability_xof_default_length(struct digest_availability_Cptr p); // access field

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus

// Describes a digest method added by the init function, and checked for compatibility
// with FIPS if FIPS mode was on. If the FIPS mode changes this will be destroyed and
// created again.
struct digest_availability_t {
    // The definition used to create this record
    const struct digest_probe_t* init = nullptr;
    struct {
        bool fips_forbidden : 1;
        bool pbkdf2_eligible : 1;
    } flags = {};
    // after init will contain the algorithm pointer, NULL if not supported. Frees automatically.
    const EVP_MD* md = nullptr;
    // 0 or default digest length for XOF digests
    size_t xof_default_length = 0;

    ~digest_availability_t();

    bool is_forbidden_in_fips() const {
#ifdef FIPS_SUPPORT
        return this->flags.fips_forbidden && FIPS_MODE();
#else
        return false;
#endif
    }
    // Return the atom which goes to the Erlang caller
    ERL_NIF_TERM get_atom() const;

    // Fetches the algorithm and sets the initial flags
    void create_md_resource(bool fips_mode);
#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    // Initialize an algorithm to check that all its dependencies are valid in FIPS
    static bool check_valid_in_fips(const EVP_MD* md);
#endif
};

// A probe contains data required for creating the algorithm description structure and testing
// its availability. Each probe() call done by the algorithm_collection_t might or might not
// result in a new available algorithm creation.
struct digest_probe_t {
    // the algorithm name as in OpenSSL < 3, also atom used by Erlang API
    const char* str = nullptr;
    // the algorithm name as in OpenSSL 3.x
    const char* str_v3 = nullptr;
    // This will be updated to created atomfound exi
    ERL_NIF_TERM atom = 0;
    // Hints that the algorithm is eligible for PBKDF2
    const bool pbkdf2 = false;
    // OpenSSL 1.0 API to create a resource for this digest algorithm (not used in 3.0 API)
    const EVP_MD* (*v1_ctor)() = nullptr;
    size_t xof_default_length = 0;

    // Perform probe on the algorithm. In case of success, fill the struct and push into the 'output'
    void probe(ErlNifEnv* env, bool fips_mode, std::vector<digest_availability_t>& output);
};

using digest_collection_t = algorithm_collection_t<digest_availability_t, digest_probe_t>;
extern digest_collection_t digest_collection;

#endif // __cplusplus
