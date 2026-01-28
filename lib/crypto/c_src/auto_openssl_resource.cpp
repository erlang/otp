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
#include "auto_openssl_resource.h"

#ifdef HAVE_AUTO_PKEY_T
void auto_pkey_t::free_resource(EVP_PKEY *p) {
    if (p) {
        EVP_PKEY_free(p);
    }
}

void auto_pkey_ctx_t::free_resource(EVP_PKEY_CTX *p) {
    if (p) {
        EVP_PKEY_CTX_free(p);
    }
}
#endif // HAVE_AUTO_PKEY_T

#ifdef HAVE_AUTO_KEY_V1_T
void auto_key_v1_t::free_resource(EC_KEY *p) {
    if (p) {
        EC_KEY_free(p);
    }
}
#endif // HAVE_AUTO_KEY_V1_T

#ifdef HAVE_ML_KEM
void auto_kem_t::free_resource(EVP_KEM *p) {
    if (p) {
        EVP_KEM_free(p);
    }
}
#endif

#if defined(HAS_3_0_API)
void auto_mac_t::free_resource(EVP_MAC *p) {
    if (p) {
        EVP_MAC_free(p);
    }
}
#endif

#if defined(HAS_3_0_API)
void auto_mac_ctx_t::free_resource(EVP_MAC_CTX *p) {
    if (p) {
        EVP_MAC_CTX_free(p);
    }
}
#endif

void auto_cipher_t::free_resource(const EVP_CIPHER *p) {
#if defined(HAS_3_0_API)
    if (p) {
        EVP_CIPHER_free(const_cast<EVP_CIPHER*>(p));
    }
#endif
    // in pre-3.0 the CIPHER object is const and cannot be freed
}

void auto_cipher_ctx_t::free_resource(EVP_CIPHER_CTX *p) {
    if (p) {
        EVP_CIPHER_CTX_free(p);
    }
}

void auto_md_t::free_resource(evp_md_pointer_type_t p) {
#if defined(HAS_3_0_API)
    if (p) {
        EVP_MD_free(p);
    }
#endif
}

void auto_md_ctx_t::free_resource(evp_md_ctx_pointer_type_t p) {
#if defined(HAS_3_0_API)
    if (p) {
        EVP_MD_CTX_free(p);
    }
#endif
}

#ifdef HAS_PREFETCH_SIGN_INIT
void auto_signature_t::free_resource(EVP_SIGNATURE *p) {
    if (p) {
        EVP_SIGNATURE_free(p);
    }
}
#endif
