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

struct digest_type_t;

#ifdef __cplusplus
extern "C"
{
#endif

#include "common.h"

    // Wraps a pointer to digest_availability_t which is a C++ struct with C++ features, for use in C API
    typedef struct digest_type_t digest_type_C;

    //
    // C Digest storage API
    //
    ERL_NIF_TERM digest_types_as_list(ErlNifEnv *env, bool fips_forbidden);

    // Lookup and access fields
    digest_type_C *get_digest_type(ErlNifEnv *env, ERL_NIF_TERM type); // linear lookup by atom
    bool is_digest_forbidden_in_fips(const digest_type_C *p);          // access C++ member from C
    const EVP_MD *get_digest_type_resource(const digest_type_C *p);    // access field
    size_t get_digest_type_xof_default_length(const digest_type_C *p); // access field
    const char *get_digest_type_str_v3(const digest_type_C *p);        // access str_v3 name (field of probe)
    bool is_digest_eligible_for_pbkdf2(const digest_type_C *p);        // check PBKDF2 availability bit

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#    include "algorithms_collection.h"
#    include "auto_openssl_resource.h"

struct digest_type_flags_t {
    bool fips_forbidden : 1;
    bool pbkdf2_eligible : 1;
};

// Describes a digest method added by the init function, and checked for compatibility
// with FIPS if FIPS mode was on. If the FIPS mode changes this will be destroyed and
// created again.
struct digest_type_t {
    // The definition used to create this record
    const struct digest_probe_t *init = nullptr;
    digest_type_flags_t flags = {};
    // after init will contain the algorithm pointer, NULL if not supported. Frees automatically.
    auto_md_t resource;
    // 0 or default digest length for XOF digests
    size_t xof_default_length = 0;

    explicit digest_type_t(const digest_probe_t *init_);

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

    // Fetches the algorithm and sets the initial flags
    void create_md_resource(bool fips_mode);
#    if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    // Initialize an algorithm to check that all its dependencies are valid in FIPS
    static bool check_valid_in_fips(const EVP_MD *md);
#    endif
};

using digest_construction_fn_t = const EVP_MD *(*)();

// A probe contains data required for creating the algorithm description structure and testing
// its availability. Each probe() call done by the algorithm_collection_t might or might not
// result in a new available algorithm creation.
struct digest_probe_t {
    // the algorithm name as in OpenSSL < 3, also atom used by Erlang API
    const char *str;
    // the algorithm name as in OpenSSL 3.x
    const char *str_v3;
    // This will be updated to created atomfound exi
    ERL_NIF_TERM atom = 0;
    digest_type_flags_t flags = {};
    // OpenSSL 1.0 API to create a resource for this digest algorithm (not used in 3.0 API)
    digest_construction_fn_t v1_ctor;
    size_t xof_default_length = 0;

    constexpr digest_probe_t(const char *str_, const char *str_v3_,
                            const digest_construction_fn_t ctor_): str(str_), str_v3(str_v3_), v1_ctor(ctor_) {
    }
    constexpr digest_probe_t set_pbkdf() {
        this->flags.pbkdf2_eligible = true;
        return *this;
    }
    constexpr digest_probe_t set_xof_default_length(const size_t xof_default_length_) {
        this->xof_default_length = xof_default_length_;
        return *this;
    }

    const char *get_v3_name() const {
        return this->str_v3 ? this->str_v3 : this->str;
    }
    // Perform probe on the algorithm. In case of success, fill the struct and push into the 'output'
    void probe(ErlNifEnv *env, bool fips_mode, std::vector<digest_type_t> &output);
    static void post_lazy_init(std::vector<digest_type_t> &) {}
};

using digest_collection_t = algorithm_collection_t<digest_type_t, digest_probe_t>;
extern digest_collection_t digest_collection;

#endif // __cplusplus
