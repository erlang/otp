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

#include "common.h"

#ifdef __cplusplus

// A generic struct holding a pointer, constructable with a pointer or as null, and auto-destructable.
// The bool operator allows using the struct in if() conditions
// When inheriting: implement the destructor with the call to the corresponding OpenSSL free function
template <typename T>
struct auto_openssl_resource_t {
    T* pointer = nullptr;
    explicit auto_openssl_resource_t(T* p) : pointer(p) {}
    explicit operator bool() const { return this->pointer != nullptr; }
};

struct auto_evp_pkey_t : auto_openssl_resource_t<EVP_PKEY> {
    explicit auto_evp_pkey_t(EVP_PKEY* p) : auto_openssl_resource_t(p) {}
    ~auto_evp_pkey_t() { reset(nullptr); }
    void reset(EVP_PKEY* new_value) {
        if (this->pointer) {
            EVP_PKEY_free(this->pointer);
        }
        this->pointer = new_value;
    }
};

struct auto_evp_pkey_ctx_t : auto_openssl_resource_t<EVP_PKEY_CTX> {
    explicit auto_evp_pkey_ctx_t(EVP_PKEY_CTX* c) : auto_openssl_resource_t(c) {}
    ~auto_evp_pkey_ctx_t() { reset(nullptr); }
    void reset(EVP_PKEY_CTX* new_value) {
        if (this->pointer) {
            EVP_PKEY_CTX_free(this->pointer);
        }
        this->pointer = new_value;
    }
};

struct auto_ec_key_t : auto_openssl_resource_t<EC_KEY> {
    explicit auto_ec_key_t(EC_KEY* c) : auto_openssl_resource_t(c) {}
    ~auto_ec_key_t() { reset(nullptr); }
    void reset(EC_KEY* new_value) {
        // TODO: EC_KEY_free is deprecated since OpenSSL 3.0
        if (this->pointer) {
            EC_KEY_free(this->pointer);
        }
        this->pointer = new_value;
    }
};

#ifdef HAVE_ML_KEM
struct auto_evp_kem_t : auto_openssl_resource_t<EVP_KEM> {
    explicit auto_evp_kem_t(EVP_KEM* kem) : auto_openssl_resource_t(kem) {}
    ~auto_evp_kem_t() { reset(nullptr); }
    void reset(EVP_KEM* new_value) {
        if (this->pointer) {
            EVP_KEM_free(this->pointer);
        }
        this->pointer = new_value;
    }
};
#endif

#endif // __cplusplus
