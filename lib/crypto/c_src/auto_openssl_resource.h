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
#include "common.h"
}

// A generic struct holding a pointer, constructable with a pointer or as null, and auto-destructable.
// The bool operator allows using the struct in if() conditions
// When inheriting: implement the destructor with the call to the corresponding OpenSSL free function
template <typename ResourceT, typename ImplementingT>
struct auto_openssl_resource_t {
    ResourceT pointer = nullptr;

    auto_openssl_resource_t() = default;
    explicit auto_openssl_resource_t(ResourceT p) : pointer(p) {}

    auto_openssl_resource_t(auto_openssl_resource_t const& other) = delete; // no copy
    auto_openssl_resource_t& operator=(auto_openssl_resource_t const&) = delete; // no copy assign

    auto_openssl_resource_t(auto_openssl_resource_t&& other) = default; // allow move
    auto_openssl_resource_t& operator=(auto_openssl_resource_t&&) = default; // allow move assign

    ~auto_openssl_resource_t() { ImplementingT::free_resource(this->pointer); }

    explicit operator bool() const { return this->pointer != nullptr; }

    void reset(ResourceT new_value) {
        ImplementingT::free_resource(this->pointer);
        this->pointer = new_value;
    }
};

#ifdef HAS_3_0_API
struct auto_evp_pkey_t : auto_openssl_resource_t<EVP_PKEY*, auto_evp_pkey_t> {
    auto_evp_pkey_t() = default;
    explicit auto_evp_pkey_t(EVP_PKEY* p) : auto_openssl_resource_t(p) {}
    static void free_resource(EVP_PKEY* p);
};

struct auto_evp_pkey_ctx_t : auto_openssl_resource_t<EVP_PKEY_CTX*, auto_evp_pkey_ctx_t> {
    auto_evp_pkey_ctx_t() = default;
    explicit auto_evp_pkey_ctx_t(EVP_PKEY_CTX* p) : auto_openssl_resource_t(p) {}
    static void free_resource(EVP_PKEY_CTX* p);
};
#else
struct auto_ec_key_t : auto_openssl_resource_t<EC_KEY*, auto_ec_key_t> {
    auto_ec_key_t() = default;
    explicit auto_ec_key_t(EC_KEY* p) : auto_openssl_resource_t(p) {}
    static void free_resource(EC_KEY* p);
};
#endif // HAS_3_0_API

#ifdef HAVE_ML_KEM
struct auto_evp_kem_t : auto_openssl_resource_t<EVP_KEM*, auto_evp_kem_t> {
    auto_evp_kem_t() = default;
    explicit auto_evp_kem_t(EVP_KEM* p) : auto_openssl_resource_t(p) {}
    static void free_resource(EVP_KEM* p);
};
#endif

struct auto_evp_mac_t : auto_openssl_resource_t<EVP_MAC*, auto_evp_mac_t> {
    auto_evp_mac_t() = default;
    explicit auto_evp_mac_t(EVP_MAC* p) : auto_openssl_resource_t(p) {}
    static void free_resource(EVP_MAC* p);
};

struct auto_evp_mac_ctx_t : auto_openssl_resource_t<EVP_MAC_CTX*, auto_evp_mac_ctx_t> {
    auto_evp_mac_ctx_t() = default;
    explicit auto_evp_mac_ctx_t(EVP_MAC_CTX* p) : auto_openssl_resource_t(p) {}
    static void free_resource(EVP_MAC_CTX* p);
};

struct auto_cipher_t : auto_openssl_resource_t<EVP_CIPHER*, auto_cipher_t> {
    auto_cipher_t() = default;
    explicit auto_cipher_t(EVP_CIPHER* p) : auto_openssl_resource_t(p) {}
    static void free_resource(EVP_CIPHER* p);
};

#endif // __cplusplus
