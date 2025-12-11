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

#include "algorithms_pkey.h"
#include "auto_openssl_resource.h"

static pkey_probe_t pkey_probes[] = {
        {.str = "rsa"},
#ifdef HAVE_DSA
        {.str = "dss"},
#endif
#ifdef HAVE_DH
        {.str = "dh"},
#endif
#if defined(HAVE_EC)
#    if !defined(OPENSSL_NO_EC2M)
        {.str = "ec_gf2m"},
#    endif
        {.str = "ecdsa"},
        {.str = "ecdh"},
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
        {.str = "mldsa44", .evp_pkey_id = EVP_PKEY_ML_DSA_44, .allow_seed = true},
        {.str = "mldsa65", .evp_pkey_id = EVP_PKEY_ML_DSA_65, .allow_seed = true},
        {.str = "mldsa87", .evp_pkey_id = EVP_PKEY_ML_DSA_87, .allow_seed = true},
#endif
#ifdef HAVE_SLH_DSA
        {.str = "slh_dsa_shake_128s", .str_v3 = "SLH-DSA-SHAKE-128s", .evp_pkey_id = EVP_PKEY_SLH_DSA_SHAKE_128S},
        {.str = "slh_dsa_shake_128f", .str_v3 = "SLH-DSA-SHAKE-128f", .evp_pkey_id = EVP_PKEY_SLH_DSA_SHAKE_128F},
        {.str = "slh_dsa_sha2_128s", .str_v3 = "SLH-DSA-SHA2-128s", .evp_pkey_id = EVP_PKEY_SLH_DSA_SHA2_128S},
        {.str = "slh_dsa_sha2_128f", .str_v3 = "SLH-DSA-SHA2-128f", .evp_pkey_id = EVP_PKEY_SLH_DSA_SHA2_128F},
        {.str = "slh_dsa_shake_192s", .str_v3 = "SLH-DSA-SHAKE-192s", .evp_pkey_id = EVP_PKEY_SLH_DSA_SHAKE_192S},
        {.str = "slh_dsa_shake_192f", .str_v3 = "SLH-DSA-SHAKE-192f", .evp_pkey_id = EVP_PKEY_SLH_DSA_SHAKE_192F},
        {.str = "slh_dsa_sha2_192s", .str_v3 = "SLH-DSA-SHA2-192s", .evp_pkey_id = EVP_PKEY_SLH_DSA_SHA2_192S},
        {.str = "slh_dsa_sha2_192f", .str_v3 = "SLH-DSA-SHA2-192f", .evp_pkey_id = EVP_PKEY_SLH_DSA_SHA2_192F},
        {.str = "slh_dsa_shake_256s", .str_v3 = "SLH-DSA-SHAKE-256s", .evp_pkey_id = EVP_PKEY_SLH_DSA_SHAKE_256S},
        {.str = "slh_dsa_shake_256f", .str_v3 = "SLH-DSA-SHAKE-256f", .evp_pkey_id = EVP_PKEY_SLH_DSA_SHAKE_256F},
        {.str = "slh_dsa_sha2_256s", .str_v3 = "SLH-DSA-SHA2-256s", .evp_pkey_id = EVP_PKEY_SLH_DSA_SHA2_256S},
        {.str = "slh_dsa_sha2_256f", .str_v3 = "SLH-DSA-SHA2-256f", .evp_pkey_id = EVP_PKEY_SLH_DSA_SHA2_256F},
#endif
};

pkey_collection_t pkey_collection("crypto.pkey_collection", pkey_probes, sizeof(pkey_probes) / sizeof(pkey_probes[0]));

//
// Implementation of Pubkey Algorithm storage API
//

// C API: Proxy the call to generic algorithm_collection_t
extern "C" ERL_NIF_TERM pkey_algorithms_as_list(ErlNifEnv *env, const bool fips_enabled) {
    return pkey_collection.to_list(env, fips_enabled);
}

ERL_NIF_TERM pkey_type_t::get_atom() const {
    return this->init->atom;
}

// Result: flags set if FIPS is not supported
#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
void pkey_type_t::check_against_fips() {
    auto_pkey_ctx_t ctx(EVP_PKEY_CTX_new_from_name(nullptr, this->init->get_v3_name(), nullptr));

    // failed: algorithm not available, do not add
    if (!ctx) {
        this->flags.algorithm_init_failed = true;
        return;
    }
    if (EVP_PKEY_keygen_init(ctx.pointer) <= 0) { // can't generate keys?
        this->flags.fips_forbidden_keygen = true;
    }

    // Drop previous pkey_ctx, create new
    ctx.reset(EVP_PKEY_CTX_new_from_name(nullptr, this->init->get_v3_name(), nullptr));
    if (EVP_PKEY_sign_init(ctx.pointer) <= 0) { // can't sign?
        this->flags.fips_forbidden_sign = true;
    }

    // Drop previous pkey_ctx, create new
    ctx.reset(EVP_PKEY_CTX_new_from_name(nullptr, this->init->get_v3_name(), nullptr));
    if (EVP_PKEY_verify_init(ctx.pointer) <= 0) { // can't verify?
        flags.fips_forbidden_verify = true;
    }

    ctx.reset(EVP_PKEY_CTX_new_from_name(nullptr, this->init->get_v3_name(), nullptr));
    if (EVP_PKEY_encrypt_init(ctx.pointer) <= 0) { // can't encrypt/decrypt?
        flags.fips_forbidden_encrypt = true;
    }

    ctx.reset(EVP_PKEY_CTX_new_from_name(nullptr, this->init->get_v3_name(), nullptr));
    if (EVP_PKEY_derive_init(ctx.pointer) <= 0) { // can't derive?
        flags.fips_forbidden_derive = true;
    }
}
#endif // FIPS_SUPPORT && HAS_3_0_API

// for FIPS we will attempt to initialize the pubkey context to verify whether the
// algorithm is allowed, for non-FIPS keeping the old behavior - always allow the algorithm.
void pkey_probe_t::probe(ErlNifEnv *env, const bool fips_enabled, std::vector<pkey_type_t> &output) {
    this->atom = create_or_existing_atom(env, this->str, this->atom);
    output.emplace_back(this);

#ifdef HAS_PREFETCH_SIGN_INIT
    // Post-Quantum algorithms have evp_pkey_id set
    if (this->evp_pkey_id) {
        output.back().alg.reset(EVP_SIGNATURE_fetch(nullptr, this->get_v3_name(), nullptr));
    }
#endif // HAS_PREFETCH_SIGN_INIT

#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    if (fips_enabled) { // attempt to instantiate the algorithm and set availability flags
        output.back().check_against_fips();
    }
#endif // FIPS_SUPPORT && HAS_3_0_API
}

extern "C" ERL_NIF_TERM get_pkey_type_atom(const pkey_type_C *p) {
    return p->init->atom;
}

extern "C" bool get_pkey_type_allow_seed(const pkey_type_C *p) {
    return p->init->allow_seed;
}

extern "C" int get_pkey_type_evp_pkey_id(const pkey_type_C *p) {
    return p->init->evp_pkey_id;
}

extern "C" pkey_type_C *get_pkey_type(ErlNifEnv *env, ERL_NIF_TERM atom) {
    const bool fips_enabled = FIPS_MODE();
    for (auto p = pkey_collection.begin(env, fips_enabled); p != pkey_collection.end(fips_enabled); ++p) {
        if (p->get_atom() == atom) {
            return &*p;
        }
    }
    return nullptr;
}

#ifdef HAS_PREFETCH_SIGN_INIT
extern "C" EVP_SIGNATURE *get_pkey_type_resource(const pkey_type_C *p) {
    return p->alg.pointer;
}
#endif
