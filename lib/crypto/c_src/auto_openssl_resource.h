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
// When inheriting: implement the resource destructor as a static free_resource function with a call
// to a corresponding OpenSSL free function.
template<typename ResourceT, typename ImplementingT>
struct auto_openssl_resource_t {
    ResourceT pointer = nullptr;

    auto_openssl_resource_t() = default;
    explicit auto_openssl_resource_t(ResourceT p) : pointer(p) {}

    auto_openssl_resource_t(auto_openssl_resource_t const &other) = delete; // no copy
    auto_openssl_resource_t &operator=(auto_openssl_resource_t const &) = delete; // no copy assign

    // allow move and move assign
    auto_openssl_resource_t(auto_openssl_resource_t &&other) noexcept {
        this->pointer = other.pointer;
        other.pointer = nullptr;
    }
    auto_openssl_resource_t &operator=(auto_openssl_resource_t &&other) noexcept {
        this->pointer = other.pointer;
        other.pointer = nullptr;
        return *this;
    }

    ~auto_openssl_resource_t() { ImplementingT::free_resource(this->pointer); }

    explicit operator bool() const { return this->pointer != nullptr; }

    void reset(ResourceT new_value) {
        ImplementingT::free_resource(this->pointer);
        this->pointer = new_value;
    }
};

#ifdef HAVE_EC
#if defined(HAVE_DH) && defined(HAS_EVP_PKEY_CTX) && (!DISABLE_EVP_DH)
#define HAVE_AUTO_PKEY_T 1
struct auto_pkey_t : auto_openssl_resource_t<EVP_PKEY *, auto_pkey_t> {
    auto_pkey_t() = default;
    explicit auto_pkey_t(EVP_PKEY *p) : auto_openssl_resource_t(p) {}
    static void free_resource(EVP_PKEY *p);
};

struct auto_pkey_ctx_t : auto_openssl_resource_t<EVP_PKEY_CTX *, auto_pkey_ctx_t> {
    auto_pkey_ctx_t() = default;
    explicit auto_pkey_ctx_t(EVP_PKEY_CTX *p) : auto_openssl_resource_t(p) {}
    static void free_resource(EVP_PKEY_CTX *p);
};
#else
#define HAVE_AUTO_KEY_V1_T 1
// Pre-SSL 3.0 Key resource
struct auto_key_v1_t : auto_openssl_resource_t<EC_KEY *, auto_key_v1_t> {
    auto_key_v1_t() = default;
    explicit auto_key_v1_t(EC_KEY *p) : auto_openssl_resource_t(p) {}
    static void free_resource(EC_KEY *p);
};
#endif // HAS_3_0_API && !DISABLE_EVP_DH && HAVE_DH
#endif // HAVE_EC

#ifdef HAVE_ML_KEM
struct auto_kem_t : auto_openssl_resource_t<EVP_KEM *, auto_kem_t> {
    auto_kem_t() = default;
    explicit auto_kem_t(EVP_KEM *p) : auto_openssl_resource_t(p) {}
    static void free_resource(EVP_KEM *p);
};
#endif

#if defined(HAS_3_0_API)
struct auto_mac_t : auto_openssl_resource_t<EVP_MAC *, auto_mac_t> {
    auto_mac_t() = default;
    explicit auto_mac_t(EVP_MAC *p) : auto_openssl_resource_t(p) {}
    static void free_resource(EVP_MAC *p);
};
#endif

#if defined(HAS_3_0_API)
struct auto_mac_ctx_t : auto_openssl_resource_t<EVP_MAC_CTX *, auto_mac_ctx_t> {
    auto_mac_ctx_t() = default;
    explicit auto_mac_ctx_t(EVP_MAC_CTX *p) : auto_openssl_resource_t(p) {}
    static void free_resource(EVP_MAC_CTX *p);
};
#endif

struct auto_cipher_t : auto_openssl_resource_t<const EVP_CIPHER *, auto_cipher_t> {
    auto_cipher_t() = default;
    explicit auto_cipher_t(const EVP_CIPHER *p) : auto_openssl_resource_t(p) {}
    static void free_resource(const EVP_CIPHER *p);
};

struct auto_cipher_ctx_t : auto_openssl_resource_t<EVP_CIPHER_CTX *, auto_cipher_ctx_t> {
    auto_cipher_ctx_t() = default;
    explicit auto_cipher_ctx_t(EVP_CIPHER_CTX *p) : auto_openssl_resource_t(p) {}
    static void free_resource(EVP_CIPHER_CTX *p);
};

#if defined(HAS_3_0_API)
using evp_md_pointer_type_t = EVP_MD *;
using evp_md_ctx_pointer_type_t = EVP_MD_CTX *;
#else
// Same as auto_md_t but takes const EVP_MD* and does not free anything as its been loaned to us as const
using evp_md_pointer_type_t = const EVP_MD *;
using evp_md_ctx_pointer_type_t = const EVP_MD_CTX *;
#endif

struct auto_md_t : auto_openssl_resource_t<evp_md_pointer_type_t, auto_md_t> {
    auto_md_t() = default;
    explicit auto_md_t(const evp_md_pointer_type_t p) : auto_openssl_resource_t(p) {}
    static void free_resource(evp_md_pointer_type_t p);
};

struct auto_md_ctx_t : auto_openssl_resource_t<evp_md_ctx_pointer_type_t, auto_md_ctx_t> {
    auto_md_ctx_t() = default;
    explicit auto_md_ctx_t(const evp_md_ctx_pointer_type_t p) : auto_openssl_resource_t(p) {}
    static void free_resource(evp_md_ctx_pointer_type_t p);
};

#ifdef HAS_PREFETCH_SIGN_INIT
struct auto_signature_t : auto_openssl_resource_t<EVP_SIGNATURE *, auto_signature_t> {
    auto_signature_t() = default;
    explicit auto_signature_t(EVP_SIGNATURE *p) : auto_openssl_resource_t(p) {}
    static void free_resource(EVP_SIGNATURE *p);
};
#endif

#endif // __cplusplus
