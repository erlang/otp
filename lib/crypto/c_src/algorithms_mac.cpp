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

#include "algorithms_mac.h"

mac_probe_t mac_probes[] = {
        {
                .str = "poly1305",
                .str_v3 = "POLY1305",
                .fips_forbidden_hint = true,
#ifdef HAVE_POLY1305
                // If we have POLY then we have EVP_PKEY
                .pkey_type = EVP_PKEY_POLY1305,
                .type = POLY1305_mac,
                .key_len = 32,
#else
                .pkey_type = EVP_PKEY_NONE,
#endif
        },

        {.str = "hmac",
         .str_v3 = "HMAC",
#if defined(HAS_EVP_PKEY_CTX) && (!DISABLE_EVP_HMAC)
         .pkey_type = EVP_PKEY_HMAC,
#else
         // HMAC is always supported, but possibly with low-level routines
         .pkey_type = EVP_PKEY_NONE,
#endif
         .type = HMAC_mac},

        {
                .str = "cmac",
                .str_v3 = "CMAC",
#ifdef HAVE_CMAC
                // If we have CMAC then we have EVP_PKEY
                .pkey_type = EVP_PKEY_CMAC,
                .type = CMAC_mac,
#else
                .pkey_type = EVP_PKEY_NONE
#endif
        },
        {} // stopper record
};

mac_collection_t mac_collection("crypto.mac_collection", mac_probes);

//
// Implementation of Known MAC Algorithms storage API
//

// C API: Proxy the call to generic algorithm_collection_t
extern "C" size_t mac_algorithms_lazy_init(ErlNifEnv *env, const bool fips_enabled) {
    return mac_collection.lazy_init(env, fips_enabled);
}

// C API: Proxy the call to generic algorithm_collection_t
extern "C" ERL_NIF_TERM mac_algorithms_as_list(ErlNifEnv *env, const bool fips_enabled) {
    return mac_collection.to_list(env, fips_enabled);
}

ERL_NIF_TERM mac_type_t::get_atom() const { return this->init->atom; }

bool mac_type_t::is_available() const { return this->init->type != NO_mac; }

void mac_type_t::check_fips_availability(const bool fips_enabled) {
#ifdef HAS_3_0_API
#ifdef FIPS_SUPPORT
    // Initialize an algorithm to check that all its dependencies are valid in FIPS
    if (this->evp_mac) {
        auto_mac_ctx_t ctx(EVP_MAC_CTX_new(this->evp_mac.pointer));

        // Dummy key and parameters.
        constexpr unsigned char key[64] = {};
        OSSL_PARAM params[2];
        params[0] = OSSL_PARAM_construct_utf8_string("digest", const_cast<char *>("SHA256"), 0);
        params[1] = OSSL_PARAM_construct_end();

        // Try to initialize the digest algorithm for use, this will check the dependencies
        if (EVP_MAC_init(ctx.pointer, key, sizeof(key), params) == 1) {
            this->flags.fips_forbidden = true;
        }
    }
#endif /* FIPS_SUPPORT */
#endif /* HAS_3_0_API */
}

void mac_type_t::update_flags(const bool fips_enabled) {
#if defined(HAS_3_0_API)
    this->evp_mac.reset(EVP_MAC_fetch(nullptr, this->init->str_v3, nullptr));
    if (!this->evp_mac) {
        this->flags.algorithm_init_failed = true;
    } else {
        this->check_fips_availability(fips_enabled);
    }
    // const int unavail_flags = is_valid_in_fips(fetched_mac); // Also tests for NULL
    //     p->unavail_flags = 0; /* mark available */
    //     p->evp_mac = fetched_mac;
    // } else {
    //     p->unavail_flags |= unavail_flags;
    //     EVP_MAC_free(fetched_mac);
    // }
#endif
}

// for FIPS we will attempt to initialize the pubkey context to verify whether the
// algorithm is allowed, for non-FIPS keeping the old behavior - always allow the algorithm.
void mac_probe_t::probe(ErlNifEnv *env, const bool fips_enabled, std::vector<mac_type_t> &output) {
    this->atom = create_or_existing_atom(env, this->str_v3, this->atom);
    // No extra checks, just convert name to atom and add
    output.emplace_back(this, mac_type_flags_t {.fips_forbidden = this->fips_forbidden_hint});
    output.back().check_fips_availability(fips_enabled);
}

extern "C" mac_type_C *get_mac_type(ERL_NIF_TERM type, const size_t key_len) {
    for (auto &p: mac_collection) {
        if (type == p.get_atom() && key_len == p.init->key_len) {
            return &p;
        }
    }
    return nullptr;
}

extern "C" mac_type_C *get_mac_type_no_key(ERL_NIF_TERM type) {
    for (auto &p: mac_collection) {
        if (type == p.get_atom()) {
            return &p;
        }
    }
    return nullptr;
}

extern "C" bool is_mac_forbidden_in_fips(const mac_type_C *p) {
    return p ? p->is_forbidden_in_fips() : true; // forbidden if null
}

extern "C" int get_mac_type_mactype(mac_type_C *p) { return p ? p->init->type : NO_mac; }

extern "C" EVP_MAC *get_mac_type_resource(mac_type_C *p) { return p ? p->evp_mac.pointer : nullptr; }
