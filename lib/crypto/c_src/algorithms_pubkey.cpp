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

#include "algorithms_pubkey.h"
#include "algorithms_collection.h"

pubkey_probe_t pubkey_probes[] = {
        {.str = "rsa"},
#ifdef HAVE_DSA
        {.str = "dss"},
#endif
#ifdef HAVE_DH
        {.str = "dh"},
#endif
#if defined(HAVE_EC)
#if !defined(OPENSSL_NO_EC2M)
        {.str = "ec_gf2m"},
#endif
        {.str = "ecdsa"},   {.str = "ecdh"},
#endif
// Non-validated algorithms follow
// Don't know if Edward curves are fips validated
#if defined(HAVE_EDDSA)
        {.str = "eddsa"},
#endif
#if defined(HAVE_EDDH)
        {.str = "eddh"},
#endif
        {.str = "srp"},
#ifdef HAVE_ML_DSA
        {.str = "mldsa44"}, {.str = "mldsa65"}, {.str = "mldsa87"},
#endif
};

pubkey_collection_t pubkey_collection("crypto.pkey_collection", pubkey_probes,
                                      sizeof(pubkey_probes) / sizeof(pubkey_probes[0]));

//
// Implementation of Pubkey Algorithm storage API
//

// C API: Proxy the call to generic algorithm_collection_t
extern "C" size_t pubkey_algorithms_lazy_init(ErlNifEnv *env, const bool fips_enabled) {
    return pubkey_collection.lazy_init(env, fips_enabled);
}

// C API: Proxy the call to generic algorithm_collection_t
extern "C" ERL_NIF_TERM pubkey_algorithms_as_list(ErlNifEnv *env, const bool fips_enabled) {
    return pubkey_collection.to_list(env, fips_enabled);
}

ERL_NIF_TERM pubkey_availability_t::get_atom() const { return this->init->atom; }

#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
// FIPS is supported AND enabled here
void pubkey_availability_t::probe_algorithm_against_fips() {
    EVP_PKEY_CTX *ctx = EVP_PKEY_CTX_new_from_name(NULL, str_v3, NULL);
    // failed: algorithm not available, do not add
    if (ctx) {
        if (EVP_PKEY_keygen_init(ctx) <= 0) { /* can't generate keys */
            flags |= FIPS_FORBIDDEN_PKEY_KEYGEN;
        }
        EVP_PKEY_CTX_free(ctx);

        ctx = EVP_PKEY_CTX_new_from_name(NULL, str_v3, NULL);
        if (EVP_PKEY_sign_init(ctx) <= 0) { /* can't sign */
            flags |= FIPS_FORBIDDEN_PKEY_SIGN;
        }
        EVP_PKEY_CTX_free(ctx);

        ctx = EVP_PKEY_CTX_new_from_name(NULL, str_v3, NULL);
        if (EVP_PKEY_verify_init(ctx) <= 0) { /* can't verify */
            flags |= FIPS_FORBIDDEN_PKEY_VERIFY;
        }
        EVP_PKEY_CTX_free(ctx);

        ctx = EVP_PKEY_CTX_new_from_name(NULL, str_v3, NULL);
        if (EVP_PKEY_encrypt_init(ctx) <= 0) { /* can't encrypt/decrypt */
            flags |= FIPS_FORBIDDEN_PKEY_ENCRYPT;
        }
        EVP_PKEY_CTX_free(ctx);

        ctx = EVP_PKEY_CTX_new_from_name(NULL, str_v3, NULL);
        if (EVP_PKEY_derive_init(ctx) <= 0) { /* can't derive */
            flags |= FIPS_FORBIDDEN_PKEY_DERIVE;
        }
        EVP_PKEY_CTX_free(ctx);
    } else {
        flags |= FIPS_PKEY_NOT_AVAIL;
    }
}
#endif // FIPS_SUPPORT && HAS_3_0_API

// for FIPS we will attempt to initialize the pubkey context to verify whether the
// algorithm is allowed, for non-FIPS keeping the old behavior - always allow the algorithm.
void pubkey_probe_t::probe(ErlNifEnv *env, bool fips_enabled, std::vector<pubkey_availability_t> &output) {
    this->atom = create_or_existing_atom(env, this->str, this->atom);
    pubkey_availability_t algo = {.init = this};
#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    if (fips_enabled) { // attempt to instantiate the algorithm and set availability flags
        algo->probe_algorithm_against_fips();
    }
#endif // FIPS_SUPPORT && HAS_3_0_API
    return output.push_back(algo);
}
