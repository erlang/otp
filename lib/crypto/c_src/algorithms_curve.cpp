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

// extern "C" {
// #include <openssl/core_names.h>
// }

#include "algorithms_curve.h"

#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
#include <openssl/core_names.h>
#endif // FIPS && OPENSSL 3.0

#include "auto_openssl_resource.h"

curve_probe_t curve_probes[] = {
#if defined(HAVE_EC)
#ifdef NID_secp160k1
        curve_probe_t::make(NID_secp160k1, "secp160k1"),
#else
#endif
#ifdef NID_secp160r1
        curve_probe_t::make(NID_secp160r1, "secp160r1"),
#else
#endif
#ifdef NID_secp160r2
        curve_probe_t::make(NID_secp160r2, "secp160r2"),
#else
#endif
#ifdef NID_secp192k1
        curve_probe_t::make(NID_secp192k1, "secp192k1"),
#else
#endif
#ifdef NID_secp224k1
        curve_probe_t::make(NID_secp224k1, "secp224k1"),
#else
#endif
#ifdef NID_secp224r1
        curve_probe_t::make(NID_secp224r1, "secp224r1"),
#else
#endif
#ifdef NID_secp256k1
        curve_probe_t::make(NID_secp256k1, "secp256k1"),
#else
#endif
#ifdef NID_secp384r1
        curve_probe_t::make(NID_secp384r1, "secp384r1"),
#else
#endif
#ifdef NID_secp521r1
        curve_probe_t::make(NID_secp521r1, "secp521r1"),
#else
#endif
#ifdef NID_X9_62_prime192v1
        curve_probe_t::make(NID_X9_62_prime192v1, "secp192r1"),
        curve_probe_t::make(NID_X9_62_prime192v1, "prime192v1"),
#else
#endif
#ifdef NID_X9_62_prime192v2
        curve_probe_t::make(NID_X9_62_prime192v2, "prime192v2"),
#else
#endif
#ifdef NID_X9_62_prime192v3
        curve_probe_t::make(NID_X9_62_prime192v3, "prime192v3"),
#else
#endif
#ifdef NID_X9_62_prime239v1
        curve_probe_t::make(NID_X9_62_prime239v1, "prime239v1"),
#else
#endif
#ifdef NID_X9_62_prime239v2
        curve_probe_t::make(NID_X9_62_prime239v2, "prime239v2"),
#else
#endif
#ifdef NID_X9_62_prime239v3
        curve_probe_t::make(NID_X9_62_prime239v3, "prime239v3"),
#else
#endif
#ifdef NID_X9_62_prime256v1
        curve_probe_t::make(NID_X9_62_prime256v1, "secp256r1"),
        curve_probe_t::make(NID_X9_62_prime256v1, "prime256v1"),
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls7
        curve_probe_t::make(NID_wap_wsg_idm_ecid_wtls7, "wtls7"),
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls9
        curve_probe_t::make(NID_wap_wsg_idm_ecid_wtls9, "wtls9"),
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls12
        curve_probe_t::make(NID_wap_wsg_idm_ecid_wtls12, "wtls12"),
#else
#endif
#ifdef NID_brainpoolP160r1
        curve_probe_t::make(NID_brainpoolP160r1, "brainpoolP160r1"),
#else
#endif
#ifdef NID_brainpoolP160t1
        curve_probe_t::make(NID_brainpoolP160t1, "brainpoolP160t1"),
#else
#endif
#ifdef NID_brainpoolP192r1
        curve_probe_t::make(NID_brainpoolP192r1, "brainpoolP192r1"),
#else
#endif
#ifdef NID_brainpoolP192t1
        curve_probe_t::make(NID_brainpoolP192t1, "brainpoolP192t1"),
#else
#endif
#ifdef NID_brainpoolP224r1
        curve_probe_t::make(NID_brainpoolP224r1, "brainpoolP224r1"),
#else
#endif
#ifdef NID_brainpoolP224t1
        curve_probe_t::make(NID_brainpoolP224t1, "brainpoolP224t1"),
#else
#endif
#ifdef NID_brainpoolP256r1
        curve_probe_t::make(NID_brainpoolP256r1, "brainpoolP256r1"),
#else
#endif
#ifdef NID_brainpoolP256t1
        curve_probe_t::make(NID_brainpoolP256t1, "brainpoolP256t1"),
#else
#endif
#ifdef NID_brainpoolP320r1
        curve_probe_t::make(NID_brainpoolP320r1, "brainpoolP320r1"),
#else
#endif
#ifdef NID_brainpoolP320t1
        curve_probe_t::make(NID_brainpoolP320t1, "brainpoolP320t1"),
#else
#endif
#ifdef NID_brainpoolP384r1
        curve_probe_t::make(NID_brainpoolP384r1, "brainpoolP384r1"),
#else
#endif
#ifdef NID_brainpoolP384t1
        curve_probe_t::make(NID_brainpoolP384t1, "brainpoolP384t1"),
#else
#endif
#ifdef NID_brainpoolP512r1
        curve_probe_t::make(NID_brainpoolP512r1, "brainpoolP512r1"),
#else
#endif
#ifdef NID_brainpoolP512t1
        curve_probe_t::make(NID_brainpoolP512t1, "brainpoolP512t1"),
#else
#endif
// #if !defined(OPENSSL_NO_EC2M)
#ifdef NID_sect163k1
        curve_probe_t::make(NID_sect163k1, "sect163k1"),
#else
#endif
#ifdef NID_sect163r1
        curve_probe_t::make(NID_sect163r1, "sect163r1"),
#else
#endif
#ifdef NID_sect163r2
        curve_probe_t::make(NID_sect163r2, "sect163r2"),
#else
#endif
#ifdef NID_sect193r1
        curve_probe_t::make(NID_sect193r1, "sect193r1"),
#else
#endif
#ifdef NID_sect193r2
        curve_probe_t::make(NID_sect193r2, "sect193r2"),
#else
#endif
#ifdef NID_sect233k1
        curve_probe_t::make(NID_sect233k1, "sect233k1"),
#else
#endif
#ifdef NID_sect233r1
        curve_probe_t::make(NID_sect233r1, "sect233r1"),
#else
#endif
#ifdef NID_sect239k1
        curve_probe_t::make(NID_sect239k1, "sect239k1"),
#else
#endif
#ifdef NID_sect283k1
        curve_probe_t::make(NID_sect283k1, "sect283k1"),
#else
#endif
#ifdef NID_sect283r1
        curve_probe_t::make(NID_sect283r1, "sect283r1"),
#else
#endif
#ifdef NID_sect409k1
        curve_probe_t::make(NID_sect409k1, "sect409k1"),
#else
#endif
#ifdef NID_sect409r1
        curve_probe_t::make(NID_sect409r1, "sect409r1"),
#else
#endif
#ifdef NID_sect571k1
        curve_probe_t::make(NID_sect571k1, "sect571k1"),
#else
#endif
#ifdef NID_sect571r1
        curve_probe_t::make(NID_sect571r1, "sect571r1"),
#else
#endif
#ifdef NID_X9_62_c2pnb163v1
        curve_probe_t::make(NID_X9_62_c2pnb163v1, "c2pnb163v1"),
#else
#endif
#ifdef NID_X9_62_c2pnb163v2
        curve_probe_t::make(NID_X9_62_c2pnb163v2, "c2pnb163v2"),
#else
#endif
#ifdef NID_X9_62_c2pnb163v3
        curve_probe_t::make(NID_X9_62_c2pnb163v3, "c2pnb163v3"),
#else
#endif
#ifdef NID_X9_62_c2pnb176v1
        curve_probe_t::make(NID_X9_62_c2pnb176v1, "c2pnb176v1"),
#else
#endif
#ifdef NID_X9_62_c2tnb191v1
        curve_probe_t::make(NID_X9_62_c2tnb191v1, "c2tnb191v1"),
#else
#endif
#ifdef NID_X9_62_c2tnb191v2
        curve_probe_t::make(NID_X9_62_c2tnb191v2, "c2tnb191v2"),
#else
#endif
#ifdef NID_X9_62_c2tnb191v3
        curve_probe_t::make(NID_X9_62_c2tnb191v3, "c2tnb191v3"),
#else
#endif
#ifdef NID_X9_62_c2pnb208w1
        curve_probe_t::make(NID_X9_62_c2pnb208w1, "c2pnb208w1"),
#else
#endif
#ifdef NID_X9_62_c2tnb239v1
        curve_probe_t::make(NID_X9_62_c2tnb239v1, "c2tnb239v1"),
#else
#endif
#ifdef NID_X9_62_c2tnb239v2
        curve_probe_t::make(NID_X9_62_c2tnb239v2, "c2tnb239v2"),
#else
#endif
#ifdef NID_X9_62_c2tnb239v3
        curve_probe_t::make(NID_X9_62_c2tnb239v3, "c2tnb239v3"),
#else
#endif
#ifdef NID_X9_62_c2pnb272w1
        curve_probe_t::make(NID_X9_62_c2pnb272w1, "c2pnb272w1"),
#else
#endif
#ifdef NID_X9_62_c2pnb304w1
        curve_probe_t::make(NID_X9_62_c2pnb304w1, "c2pnb304w1"),
#else
#endif
#ifdef NID_X9_62_c2tnb359v1
        curve_probe_t::make(NID_X9_62_c2tnb359v1, "c2tnb359v1"),
#else
#endif
#ifdef NID_X9_62_c2pnb368w1
        curve_probe_t::make(NID_X9_62_c2pnb368w1, "c2pnb368w1"),
#else
#endif
#ifdef NID_X9_62_c2tnb431r1
        curve_probe_t::make(NID_X9_62_c2tnb431r1, "c2tnb431r1"),
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls3
        curve_probe_t::make(NID_wap_wsg_idm_ecid_wtls3, "wtls3"),
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls5
        curve_probe_t::make(NID_wap_wsg_idm_ecid_wtls5, "wtls5"),
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls10
        curve_probe_t::make(NID_wap_wsg_idm_ecid_wtls10, "wtls10"),
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls11
        curve_probe_t::make(NID_wap_wsg_idm_ecid_wtls11, "wtls11"),
#else
#endif
// Non-validated algorithms follow
#ifdef NID_secp112r1
        curve_probe_t::make(NID_secp112r1, "secp112r1"),
#else
#endif
#ifdef NID_secp112r2
        curve_probe_t::make(NID_secp112r2, "secp112r2"),
#else
#endif
#ifdef NID_secp128r1
        curve_probe_t::make(NID_secp128r1, "secp128r1"),
#else
#endif
#ifdef NID_secp128r2
        curve_probe_t::make(NID_secp128r2, "secp128r2"),
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls6
        curve_probe_t::make(NID_wap_wsg_idm_ecid_wtls6, "wtls6"),
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls8
        curve_probe_t::make(NID_wap_wsg_idm_ecid_wtls8, "wtls8"),
#else
#endif
// #if !defined(OPENSSL_NO_EC2M)
#ifdef NID_sect113r1
        curve_probe_t::make(NID_sect113r1, "sect113r1"),
#else
#endif
#ifdef NID_sect113r2
        curve_probe_t::make(NID_sect113r2, "sect113r2"),
#else
#endif
#ifdef NID_sect131r1
        curve_probe_t::make(NID_sect131r1, "sect131r1"),
#else
#endif
#ifdef NID_sect131r2
        curve_probe_t::make(NID_sect131r2, "sect131r2"),
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls1
        curve_probe_t::make(NID_wap_wsg_idm_ecid_wtls1, "wtls1"),
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls4
        curve_probe_t::make(NID_wap_wsg_idm_ecid_wtls4, "wtls4"),
#else
#endif
#ifdef NID_ipsec3
        curve_probe_t::make(NID_ipsec3, "ipsec3"),
#else
#endif
#ifdef NID_ipsec4
        curve_probe_t::make(NID_ipsec4, "ipsec4"),
#else
#endif

#if !defined(FIPS_SUPPORT)
#ifdef HAVE_ED25519
        curve_probe_t::make(0, "ed25519"),
#endif
#ifdef HAVE_ED448
        curve_probe_t::make(0, "ed448"),
#endif
#ifdef HAVE_X25519
        curve_probe_t::make(0, "x25519"),
#endif
#ifdef HAVE_X448
        curve_probe_t::make(0, "x448"),
#endif
#endif // !FIPS_SUPPORT
#endif // HAVE_EC
};

curve_collection_t curve_collection("crypto.curve_collection", curve_probes,
                                    sizeof(curve_probes) / sizeof(curve_probes[0]));

//
// Implementation of Curve Algorithm storage API
//

extern "C" ERL_NIF_TERM curve_algorithms_as_list(ErlNifEnv *env, const bool fips_enabled) {
    return curve_collection.to_list(env, fips_enabled);
}

#ifdef HAVE_EC
// Check if the curve in nid is supported by the current cryptolib and current FIPS state.
bool curve_probe_t::is_curve_valid_by_nid() const {
#ifdef HAVE_AUTO_PKEY_T
    auto_pkey_t pkey;
    auto_pkey_t params;

    const auto_pkey_ctx_t pctx(EVP_PKEY_CTX_new_id(EVP_PKEY_EC, nullptr));
    if (!pctx)
        return false;
    if (1 != EVP_PKEY_paramgen_init(pctx.pointer))
        return false;
    if (1 != EVP_PKEY_CTX_set_ec_paramgen_curve_nid(pctx.pointer, nid))
        return false;
    if (!EVP_PKEY_paramgen(pctx.pointer, &params.pointer))
        return false;

    const auto_pkey_ctx_t kctx(EVP_PKEY_CTX_new(params.pointer, nullptr));
    if (!kctx)
        return false;

    if (1 != EVP_PKEY_keygen_init(kctx.pointer))
        return false;
    if (1 != EVP_PKEY_keygen(kctx.pointer, &pkey.pointer))
        return false;

    return true;
#else
#ifdef HAVE_AUTO_KEY_V1_T
    auto_key_v1_t key(EC_KEY_new_by_curve_name(nid));

    if (!key)
        return false;
    if (1 != EC_KEY_generate_key(key.pointer))
        return false;
    return true;
#endif
#endif // HAVE_AUTO_PKEY_T
    return false;
}
#endif // HAVE_EC

ERL_NIF_TERM curve_type_t::get_atom() const { return this->init->atom; }

void curve_type_t::check_fips_availability(const bool fips_mode) {
#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    // This checking code only runs under FIPS and OpenSSL 3+, other cases algorithm is always added
    if (!fips_mode)
        return;

    OSSL_PARAM params[2];

    const auto_pkey_ctx_t pctx(EVP_PKEY_CTX_new_from_name(nullptr, "EC", "fips=yes"));
    if (!pctx) {
        this->flags.algorithm_init_failed = true;
        return; // EC keygen context not available
    }
    if (EVP_PKEY_keygen_init(pctx.pointer) <= 0) {
        this->flags.algorithm_init_failed = true;
        return;
    }
    params[0] = OSSL_PARAM_construct_utf8_string(OSSL_PKEY_PARAM_GROUP_NAME, const_cast<char *>(this->init->sn), 0);
    params[1] = OSSL_PARAM_construct_end();
    if (EVP_PKEY_CTX_set_params(pctx.pointer, params) <= 0) {
        this->flags.algorithm_init_failed = true;
        return;
    }
    auto_pkey_t pkey(nullptr);
    if (EVP_PKEY_generate(pctx.pointer, &pkey.pointer) <= 0) {
        this->flags.algorithm_init_failed = true;
    }
#endif
}

void curve_probe_t::probe(ErlNifEnv *env, const bool fips_mode, std::vector<curve_type_t> &output) {
    this->atom = create_or_existing_atom(env, this->sn, this->atom);

#ifdef HAVE_EC
    // Some curves can be pre-checked by their NID. Passing NID=0 will skip this check
    if (nid && !this->is_curve_valid_by_nid()) {
        return; // invalid/unsupported curves are skipped
    }
#endif // HAVE_EC

    // Construct in the container directly, passing 'this' to the ctor
    output.emplace_back(this);
    auto &algo = output.back();
    algo.check_fips_availability(fips_mode);
}
