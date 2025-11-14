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

#ifdef HAS_3_0_API
void auto_evp_pkey_t::free_resource(EVP_PKEY *p) {
    if (p) {
        EVP_PKEY_free(p);
    }
}

void auto_evp_pkey_ctx_t::free_resource(EVP_PKEY_CTX *p) {
    if (p) {
        EVP_PKEY_CTX_free(p);
    }
}
#else
void auto_ec_key_t::free_resource(EC_KEY *p) {
    if (p) {
        EC_KEY_free(p);
    }
}
#endif // HAS_3_0_API

#ifdef HAVE_ML_KEM
void auto_evp_kem_t::free_resource(EVP_KEM *p) {
    if (p) {
        EVP_KEM_free(p);
    }
}
#endif

void auto_evp_mac_t::free_resource(EVP_MAC *p) {
    if (p) {
        EVP_MAC_free(p);
    }
}


void auto_evp_mac_ctx_t::free_resource(EVP_MAC_CTX *p) {
    if (p) {
        EVP_MAC_CTX_free(p);
    }
}
