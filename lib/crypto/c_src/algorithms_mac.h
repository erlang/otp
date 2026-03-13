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

    // Wraps a pointer to mac_availability_t which is a C++ struct with C++
    // features, for use in C API
    typedef struct mac_type_t mac_type_C;

    //
    // Supported MAC Options storage C API
    //
    ERL_NIF_TERM mac_algorithms_as_list(ErlNifEnv *env, bool fips_enabled);
    mac_type_C *find_mac_type_by_name_keylen(ErlNifEnv *env, ERL_NIF_TERM type, size_t key_len);
    mac_type_C *find_mac_type_by_name(ErlNifEnv *env, ERL_NIF_TERM type);
    bool is_mac_forbidden_in_fips(const mac_type_C *p);
    int get_mac_type_mactype(mac_type_C *p); // access field
#if defined(HAS_3_0_API)
    // access field evp_mac (OpenSSL resource)
    EVP_MAC *get_mac_type_resource(mac_type_C *p);
#endif

#ifdef __cplusplus
} // end extern "C"
#endif

enum MAC_TYPE {
    NO_mac,
    HMAC_mac,
    CMAC_mac,
    POLY1305_mac,
};

#ifdef __cplusplus

#    include "algorithms_collection.h"
#if defined(HAS_3_0_API)
#include "auto_openssl_resource.h" // Only used once in mac_type_t under same ifdef
#endif

struct mac_probe_t;
struct mac_type_flags_t {
    bool algorithm_init_failed : 1;
    bool fips_forbidden : 1;
};

// Describes a MAC algorithm added by the collection's probe function, and
// checked for compatibility with FIPS if FIPS mode was on. If the FIPS mode
// changes this will be destroyed and created again.
struct mac_type_t {
    const mac_probe_t *init = nullptr; // the mac_probe_t used to create this record
#    if defined(HAS_3_0_API)
    auto_mac_t evp_mac; // OpenSSL resource, frees automatically
#    endif

    mac_type_flags_t flags = {};

    explicit mac_type_t(const mac_probe_t *init_) : init(init_) {
    }

    bool is_forbidden_in_fips() const {
#    ifdef FIPS_SUPPORT
        return this->flags.fips_forbidden && FIPS_MODE();
#    else
        return false;
#    endif
    }
    bool is_available() const;
    // Return the atom which goes to the Erlang caller
    ERL_NIF_TERM get_atom() const;

    void update_flags(bool fips_enabled);
#if defined(HAS_3_0_API) && defined(FIPS_SUPPORT)
    void update_flags_check_fips_availability(bool fips_enabled);
#endif // defined(HAS_3_0_API) && defined(FIPS_SUPPORT)
};

// A probe contains data required for creating the algorithm description
// structure and testing its availability. Each probe() call done by the
// algorithm_collection_t might or might not result in a new available algorithm
// creation.
struct mac_probe_t {
    const char *str;
    const char *str_v3;
    ERL_NIF_TERM atom = CRYPTOENIF_BAD_ATOM_VALUE;
    // Suggests that the algorithm is not available in FIPS to skip the probe
    bool fips_forbidden_hint;
    int pkey_type = EVP_PKEY_NONE; // contains EVP_PKEY_* macro (a NID)
    MAC_TYPE type = NO_mac;
    size_t key_len = 0;

    constexpr mac_probe_t(const char *str_, const char *str_v3_, const bool fips_forbidden)
        : str(str_), str_v3(str_v3_), fips_forbidden_hint(fips_forbidden) {
    }

    // Attempt to add a new MAC algorithm. In case of success, fill the struct
    // and push into the 'output'
    void probe(ErlNifEnv *env, bool fips_enabled, std::vector<mac_type_t> &output);
    static void post_lazy_init(std::vector<mac_type_t> &) {}
};

using mac_collection_t = algorithm_collection_t<mac_type_t, mac_probe_t>;
extern mac_collection_t mac_collection;

#endif // __cplusplus
