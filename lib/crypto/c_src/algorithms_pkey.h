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
    // Pubkey Algorithms storage C API
    //
    typedef struct pkey_type_t pkey_type_C;

    ERL_NIF_TERM pkey_algorithms_as_list(ErlNifEnv *env, bool fips_enabled);

    pkey_type_C *get_pkey_type(ErlNifEnv *env, ERL_NIF_TERM atom);
    ERL_NIF_TERM get_pkey_type_atom(const pkey_type_C *p);
#ifdef HAS_PREFETCH_SIGN_INIT
    EVP_SIGNATURE *get_pkey_type_resource(const pkey_type_C *p);
#endif
    bool get_pkey_type_allow_seed(const pkey_type_C *p);
    int get_pkey_type_evp_pkey_id(const pkey_type_C *p);

#ifdef __cplusplus
}
#endif

#ifdef __cplusplus
#    include "algorithms_collection.h"
#ifdef HAS_PREFETCH_SIGN_INIT
#include "auto_openssl_resource.h" // only used in pkey_type_t under same ifdef
#endif
struct pkey_probe_t;

struct pubkey_type_flags_t {
    bool algorithm_init_failed : 1; // algorithm init failed
    bool fips_forbidden_keygen : 1;
    bool fips_forbidden_sign : 1;
    bool fips_forbidden_verify : 1;
    bool fips_forbidden_encrypt : 1;
    bool fips_forbidden_derive : 1;
};

// Describes a public key algorithm added by the collection's probe function, and checked for compatibility
// with FIPS if FIPS mode was on. If the FIPS mode changes this will be destroyed and
// created again.
struct pkey_type_t {
    const pkey_probe_t *init = nullptr; // the pubkey_probe_t used to create this record
    pubkey_type_flags_t flags = {};
#    ifdef HAS_PREFETCH_SIGN_INIT
    auto_signature_t alg; // after init
#    endif
    const int evp_pkey_id = 0;

    explicit pkey_type_t(const pkey_probe_t *probe) : init(probe) {
    }

    bool is_forbidden_in_fips() const {
#    ifdef FIPS_SUPPORT
        // Forbidden in FIPS if all operations are forbidden, or if algorithm is not available at all
        const auto all_ops_forbidden = this->flags.fips_forbidden_keygen && this->flags.fips_forbidden_sign &&
                                       this->flags.fips_forbidden_verify && this->flags.fips_forbidden_encrypt &&
                                       this->flags.fips_forbidden_derive;
        return (this->flags.algorithm_init_failed || all_ops_forbidden) && FIPS_MODE();
#    else
        return false;
#    endif
    }
    bool is_available() const {
        return !this->flags.algorithm_init_failed;
    }
    // Return the atom which goes to the Erlang caller
    ERL_NIF_TERM get_atom() const;
#    if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    void check_against_fips(); // Result: flags set if FIPS is not supported
#    endif                     // FIPS_SUPPORT && HAS_3_0_API
};

// A probe contains data required for creating the algorithm description structure and testing
// its availability. Each probe() call done by the algorithm_collection_t might or might not
// result in a new available algorithm creation.
struct pkey_probe_t {
    const char *str;
    const char *str_v3; // if this is nullptr, .str will be used instead
    ERL_NIF_TERM atom;

    const int evp_pkey_id; // Used by post-quantum algorithms probe
    const bool allow_seed; // Used by post-quantum algorithms probe

    static constexpr pkey_probe_t from_name(const char *str) {
        return {
            str, /* str_v3= */ nullptr, /* atom= */ CRYPTOENIF_BAD_ATOM_VALUE, /* evp_pkey_id= */ 0,
            /* allow_seed= */ false
        };
    }
#ifdef HAVE_ML_DSA
    static constexpr pkey_probe_t from_mldsa_params(const char *str, const int evp_pkey_id) {
        return {str, /* str_v3= */ nullptr, /* atom= */ CRYPTOENIF_BAD_ATOM_VALUE, evp_pkey_id, /* allow_seed= */ true};
    }
#endif
#ifdef HAVE_SLH_DSA
    static constexpr pkey_probe_t from_slhdsa_params(const char *str, const char *str_v3, const int evp_pkey_id) {
        return {str, str_v3, /* atom= */ CRYPTOENIF_BAD_ATOM_VALUE, evp_pkey_id, /* allow_seed= */ false};
    }
#endif

    const char *get_v3_name() const {
        return this->str_v3 ? this->str_v3 : this->str;
    }
    // Perform a probe on the algorithm. In case of success, fill the struct and push into the 'output'
    void probe(ErlNifEnv *env, bool fips_enabled, std::vector<pkey_type_t> &output);
    static void post_lazy_init(std::vector<pkey_type_t> &) {}
};

using pkey_collection_t = algorithm_collection_t<pkey_type_t, pkey_probe_t>;
extern pkey_collection_t pkey_collection;

#endif // __cplusplus
