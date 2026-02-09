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

static constexpr mac_probe_t make_poly1305_probe() {
    mac_probe_t p("poly1305", "POLY1305", true);
#ifdef HAVE_POLY1305
    // If we have POLY then we have EVP_PKEY
    p.pkey_type = EVP_PKEY_POLY1305;
    p.type = POLY1305_mac;
    p.key_len = 32;
#endif
    return p;
}

static constexpr mac_probe_t make_hmac_probe() {
    mac_probe_t p("hmac", "HMAC", false);
#if defined(HAS_EVP_PKEY_CTX) && (!DISABLE_EVP_HMAC)
    p.pkey_type = EVP_PKEY_HMAC,
#endif
    // HMAC is always supported, but possibly with low-level routines
    p.type = HMAC_mac;
    return p;
}

static constexpr mac_probe_t make_cmac_probe() {
    mac_probe_t p("cmac", "CMAC", false);
#ifdef HAVE_CMAC
    // If we have CMAC then we have EVP_PKEY
    p.pkey_type = EVP_PKEY_CMAC;
    p.type = CMAC_mac;
#endif
    return p;
}

mac_probe_t mac_probes[] = { make_poly1305_probe(), make_hmac_probe(), make_cmac_probe() };

mac_collection_t mac_collection("crypto.mac_collection", mac_probes, sizeof(mac_probes) / sizeof(mac_probes[0]));

//
// Implementation of Known MAC Algorithms storage API
//

// C API: Proxy the call to generic algorithm_collection_t
extern "C" ERL_NIF_TERM mac_algorithms_as_list(ErlNifEnv *env, const bool fips_enabled) {
    return mac_collection.to_list(env, fips_enabled);
}

ERL_NIF_TERM mac_type_t::get_atom() const {
    return this->init->atom;
}

bool mac_type_t::is_available() const {
    return this->init->type != NO_mac;
}

#if defined(HAS_3_0_API) && defined(FIPS_SUPPORT)
void mac_type_t::update_flags_check_fips_availability(const bool fips_enabled) {
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
}
#endif // defined(HAS_3_0_API) && defined(FIPS_SUPPORT)

void mac_type_t::update_flags(const bool fips_enabled) {
#if defined(HAS_3_0_API) && defined(FIPS_SUPPORT)
    alg.update_flags_check_fips_availability(fips_enabled);
#endif // defined(HAS_3_0_API) && defined(FIPS_SUPPORT)

#if defined(HAS_3_0_API)
    this->evp_mac.reset(EVP_MAC_fetch(nullptr, this->init->str_v3, nullptr));
    if (!this->evp_mac) {
        this->flags.algorithm_init_failed = true;
    }
#endif
}

// for FIPS we will attempt to initialize the pubkey context to verify whether the
// algorithm is allowed, for non-FIPS keeping the old behavior - always allow the algorithm.
void mac_probe_t::probe(ErlNifEnv *env, const bool fips_enabled, std::vector<mac_type_t> &output) {
    this->atom = create_or_existing_atom(env, this->str, this->atom);

    output.emplace_back(this);
    auto &alg = output.back();

    alg.flags.fips_forbidden = this->fips_forbidden_hint;
    alg.update_flags(fips_enabled);
}

extern "C" mac_type_C *find_mac_type_by_name_keylen(ErlNifEnv *env, ERL_NIF_TERM type, const size_t key_len) {
    const bool fips_enabled = FIPS_MODE();
    for (auto p = mac_collection.begin(env, fips_enabled); p != mac_collection.end(fips_enabled); ++p) {
        if (p->get_atom() == type && (p->init->key_len == 0 || p->init->key_len == key_len)) {
            return &*p;
        }
    }
    return nullptr;
}

extern "C" mac_type_C *find_mac_type_by_name(ErlNifEnv *env, ERL_NIF_TERM type) {
    const bool fips_enabled = FIPS_MODE();
    for (auto p = mac_collection.begin(env, fips_enabled); p != mac_collection.end(fips_enabled); ++p) {
        ASSERT(p->init->atom != CRYPTOENIF_BAD_ATOM_VALUE);
        if (p->init->atom == type) {
            return &*p;
        }
    }
    return nullptr;
}

extern "C" bool is_mac_forbidden_in_fips(const mac_type_C *p) {
    return p ? p->is_forbidden_in_fips() : true; // forbidden if null
}

extern "C" int get_mac_type_mactype(mac_type_C *p) {
    return p ? p->init->type : NO_mac;
}

#if defined(HAS_3_0_API)
extern "C" EVP_MAC *get_mac_type_resource(mac_type_C *p) {
    return p ? p->evp_mac.pointer : nullptr;
}
#endif
