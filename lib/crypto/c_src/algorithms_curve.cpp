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

extern "C" {
#include <openssl/core_names.h>
}

#include "algorithms_curve.h"
#include "auto_openssl_resource.h"

curve_probe_t curve_probes[] = {
#if defined(HAVE_EC)
#ifdef NID_secp160k1
        {.nid = NID_secp160k1, .sn = "secp160k1"},
#else
#endif
#ifdef NID_secp160r1
        {.nid = NID_secp160r1, .sn = "secp160r1"},
#else
#endif
#ifdef NID_secp160r2
        {.nid = NID_secp160r2, .sn = "secp160r2"},
#else
#endif
#ifdef NID_secp192k1
        {.nid = NID_secp192k1, .sn = "secp192k1"},
#else
#endif
#ifdef NID_secp224k1
        {.nid = NID_secp224k1, .sn = "secp224k1"},
#else
#endif
#ifdef NID_secp224r1
        {.nid = NID_secp224r1, .sn = "secp224r1"},
#else
#endif
#ifdef NID_secp256k1
        {.nid = NID_secp256k1, .sn = "secp256k1"},
#else
#endif
#ifdef NID_secp384r1
        {.nid = NID_secp384r1, .sn = "secp384r1"},
#else
#endif
#ifdef NID_secp521r1
        {.nid = NID_secp521r1, .sn = "secp521r1"},
#else
#endif
#ifdef NID_X9_62_prime192v1
        {.nid = NID_X9_62_prime192v1, .sn = "secp192r1"},
        {.nid = NID_X9_62_prime192v1, .sn = "prime192v1"},
#else
#endif
#ifdef NID_X9_62_prime192v2
        {.nid = NID_X9_62_prime192v2, .sn = "prime192v2"},
#else
#endif
#ifdef NID_X9_62_prime192v3
        {.nid = NID_X9_62_prime192v3, .sn = "prime192v3"},
#else
#endif
#ifdef NID_X9_62_prime239v1
        {.nid = NID_X9_62_prime239v1, .sn = "prime239v1"},
#else
#endif
#ifdef NID_X9_62_prime239v2
        {.nid = NID_X9_62_prime239v2, .sn = "prime239v2"},
#else
#endif
#ifdef NID_X9_62_prime239v3
        {.nid = NID_X9_62_prime239v3, .sn = "prime239v3"},
#else
#endif
#ifdef NID_X9_62_prime256v1
        {.nid = NID_X9_62_prime256v1, .sn = "secp256r1"},
        {.nid = NID_X9_62_prime256v1, .sn = "prime256v1"},
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls7
        {.nid = NID_wap_wsg_idm_ecid_wtls7, .sn = "wtls7"},
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls9
        {.nid = NID_wap_wsg_idm_ecid_wtls9, .sn = "wtls9"},
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls12
        {.nid = NID_wap_wsg_idm_ecid_wtls12, .sn = "wtls12"},
#else
#endif
#ifdef NID_brainpoolP160r1
        {.nid = NID_brainpoolP160r1, .sn = "brainpoolP160r1"},
#else
#endif
#ifdef NID_brainpoolP160t1
        {.nid = NID_brainpoolP160t1, .sn = "brainpoolP160t1"},
#else
#endif
#ifdef NID_brainpoolP192r1
        {.nid = NID_brainpoolP192r1, .sn = "brainpoolP192r1"},
#else
#endif
#ifdef NID_brainpoolP192t1
        {.nid = NID_brainpoolP192t1, .sn = "brainpoolP192t1"},
#else
#endif
#ifdef NID_brainpoolP224r1
        {.nid = NID_brainpoolP224r1, .sn = "brainpoolP224r1"},
#else
#endif
#ifdef NID_brainpoolP224t1
        {.nid = NID_brainpoolP224t1, .sn = "brainpoolP224t1"},
#else
#endif
#ifdef NID_brainpoolP256r1
        {.nid = NID_brainpoolP256r1, .sn = "brainpoolP256r1"},
#else
#endif
#ifdef NID_brainpoolP256t1
        {.nid = NID_brainpoolP256t1, .sn = "brainpoolP256t1"},
#else
#endif
#ifdef NID_brainpoolP320r1
        {.nid = NID_brainpoolP320r1, .sn = "brainpoolP320r1"},
#else
#endif
#ifdef NID_brainpoolP320t1
        {.nid = NID_brainpoolP320t1, .sn = "brainpoolP320t1"},
#else
#endif
#ifdef NID_brainpoolP384r1
        {.nid = NID_brainpoolP384r1, .sn = "brainpoolP384r1"},
#else
#endif
#ifdef NID_brainpoolP384t1
        {.nid = NID_brainpoolP384t1, .sn = "brainpoolP384t1"},
#else
#endif
#ifdef NID_brainpoolP512r1
        {.nid = NID_brainpoolP512r1, .sn = "brainpoolP512r1"},
#else
#endif
#ifdef NID_brainpoolP512t1
        {.nid = NID_brainpoolP512t1, .sn = "brainpoolP512t1"},
#else
#endif
// #if !defined(OPENSSL_NO_EC2M)
#ifdef NID_sect163k1
        {.nid = NID_sect163k1, .sn = "sect163k1"},
#else
#endif
#ifdef NID_sect163r1
        {.nid = NID_sect163r1, .sn = "sect163r1"},
#else
#endif
#ifdef NID_sect163r2
        {.nid = NID_sect163r2, .sn = "sect163r2"},
#else
#endif
#ifdef NID_sect193r1
        {.nid = NID_sect193r1, .sn = "sect193r1"},
#else
#endif
#ifdef NID_sect193r2
        {.nid = NID_sect193r2, .sn = "sect193r2"},
#else
#endif
#ifdef NID_sect233k1
        {.nid = NID_sect233k1, .sn = "sect233k1"},
#else
#endif
#ifdef NID_sect233r1
        {.nid = NID_sect233r1, .sn = "sect233r1"},
#else
#endif
#ifdef NID_sect239k1
        {.nid = NID_sect239k1, .sn = "sect239k1"},
#else
#endif
#ifdef NID_sect283k1
        {.nid = NID_sect283k1, .sn = "sect283k1"},
#else
#endif
#ifdef NID_sect283r1
        {.nid = NID_sect283r1, .sn = "sect283r1"},
#else
#endif
#ifdef NID_sect409k1
        {.nid = NID_sect409k1, .sn = "sect409k1"},
#else
#endif
#ifdef NID_sect409r1
        {.nid = NID_sect409r1, .sn = "sect409r1"},
#else
#endif
#ifdef NID_sect571k1
        {.nid = NID_sect571k1, .sn = "sect571k1"},
#else
#endif
#ifdef NID_sect571r1
        {.nid = NID_sect571r1, .sn = "sect571r1"},
#else
#endif
#ifdef NID_X9_62_c2pnb163v1
        {.nid = NID_X9_62_c2pnb163v1, .sn = "c2pnb163v1"},
#else
#endif
#ifdef NID_X9_62_c2pnb163v2
        {.nid = NID_X9_62_c2pnb163v2, .sn = "c2pnb163v2"},
#else
#endif
#ifdef NID_X9_62_c2pnb163v3
        {.nid = NID_X9_62_c2pnb163v3, .sn = "c2pnb163v3"},
#else
#endif
#ifdef NID_X9_62_c2pnb176v1
        {.nid = NID_X9_62_c2pnb176v1, .sn = "c2pnb176v1"},
#else
#endif
#ifdef NID_X9_62_c2tnb191v1
        {.nid = NID_X9_62_c2tnb191v1, .sn = "c2tnb191v1"},
#else
#endif
#ifdef NID_X9_62_c2tnb191v2
        {.nid = NID_X9_62_c2tnb191v2, .sn = "c2tnb191v2"},
#else
#endif
#ifdef NID_X9_62_c2tnb191v3
        {.nid = NID_X9_62_c2tnb191v3, .sn = "c2tnb191v3"},
#else
#endif
#ifdef NID_X9_62_c2pnb208w1
        {.nid = NID_X9_62_c2pnb208w1, .sn = "c2pnb208w1"},
#else
#endif
#ifdef NID_X9_62_c2tnb239v1
        {.nid = NID_X9_62_c2tnb239v1, .sn = "c2tnb239v1"},
#else
#endif
#ifdef NID_X9_62_c2tnb239v2
        {.nid = NID_X9_62_c2tnb239v2, .sn = "c2tnb239v2"},
#else
#endif
#ifdef NID_X9_62_c2tnb239v3
        {.nid = NID_X9_62_c2tnb239v3, .sn = "c2tnb239v3"},
#else
#endif
#ifdef NID_X9_62_c2pnb272w1
        {.nid = NID_X9_62_c2pnb272w1, .sn = "c2pnb272w1"},
#else
#endif
#ifdef NID_X9_62_c2pnb304w1
        {.nid = NID_X9_62_c2pnb304w1, .sn = "c2pnb304w1"},
#else
#endif
#ifdef NID_X9_62_c2tnb359v1
        {.nid = NID_X9_62_c2tnb359v1, .sn = "c2tnb359v1"},
#else
#endif
#ifdef NID_X9_62_c2pnb368w1
        {.nid = NID_X9_62_c2pnb368w1, .sn = "c2pnb368w1"},
#else
#endif
#ifdef NID_X9_62_c2tnb431r1
        {.nid = NID_X9_62_c2tnb431r1, .sn = "c2tnb431r1"},
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls3
        {.nid = NID_wap_wsg_idm_ecid_wtls3, .sn = "wtls3"},
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls5
        {.nid = NID_wap_wsg_idm_ecid_wtls5, .sn = "wtls5"},
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls10
        {.nid = NID_wap_wsg_idm_ecid_wtls10, .sn = "wtls10"},
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls11
        {.nid = NID_wap_wsg_idm_ecid_wtls11, .sn = "wtls11"},
#else
#endif
// Non-validated algorithms follow
#ifdef NID_secp112r1
        {.nid = NID_secp112r1, .sn = "secp112r1"},
#else
#endif
#ifdef NID_secp112r2
        {.nid = NID_secp112r2, .sn = "secp112r2"},
#else
#endif
#ifdef NID_secp128r1
        {.nid = NID_secp128r1, .sn = "secp128r1"},
#else
#endif
#ifdef NID_secp128r2
        {.nid = NID_secp128r2, .sn = "secp128r2"},
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls6
        {.nid = NID_wap_wsg_idm_ecid_wtls6, .sn = "wtls6"},
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls8
        {.nid = NID_wap_wsg_idm_ecid_wtls8, .sn = "wtls8"},
#else
#endif
// #if !defined(OPENSSL_NO_EC2M)
#ifdef NID_sect113r1
        {.nid = NID_sect113r1, .sn = "sect113r1"},
#else
#endif
#ifdef NID_sect113r2
        {.nid = NID_sect113r2, .sn = "sect113r2"},
#else
#endif
#ifdef NID_sect131r1
        {.nid = NID_sect131r1, .sn = "sect131r1"},
#else
#endif
#ifdef NID_sect131r2
        {.nid = NID_sect131r2, .sn = "sect131r2"},
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls1
        {.nid = NID_wap_wsg_idm_ecid_wtls1, .sn = "wtls1"},
#else
#endif
#ifdef NID_wap_wsg_idm_ecid_wtls4
        {.nid = NID_wap_wsg_idm_ecid_wtls4, .sn = "wtls4"},
#else
#endif
#ifdef NID_ipsec3
        {.nid = NID_ipsec3, .sn = "ipsec3"},
#else
#endif
#ifdef NID_ipsec4
        {.nid = NID_ipsec4, .sn = "ipsec4"},
#else
#endif

#if !defined(FIPS_SUPPORT)
#ifdef HAVE_ED25519
        {.nid = 0, .sn = "ed25519"},
#endif
#ifdef HAVE_ED448
        {.nid = 0, .sn = "ed448"},
#endif
#ifdef HAVE_X25519
        {.nid = 0, .sn = "x25519"},
#endif
#ifdef HAVE_X448
        {.nid = 0, .sn = "x448"},
#endif
#endif // FIPS_SUPPORT
#endif // HAVE_EC
        {} // stopper record
};

curve_collection_t curve_collection("crypto.curve_collection", curve_probes);

//
// Implementation of Curve Algorithm storage API
//

extern "C" size_t curve_algorithms_lazy_init(ErlNifEnv *env, const bool fips_enabled) {
    return curve_collection.lazy_init(env, fips_enabled);
}

extern "C" ERL_NIF_TERM curve_algorithms_as_list(ErlNifEnv *env, const bool fips_enabled) {
    return curve_collection.to_list(env, fips_enabled);
}

/*================================================================
  Curves
*/

/* Check if the curve in nid is supported by the
   current cryptolib and current FIPS state.
*/

bool curve_probe_t::is_curve_valid_by_nid() {
#ifdef HAVE_EC
#if defined(HAVE_DH)
#if defined(HAS_EVP_PKEY_CTX) && (!DISABLE_EVP_DH)
    auto_evp_pkey_t pkey;
    auto_evp_pkey_t params;

    const auto_evp_pkey_ctx_t pctx(EVP_PKEY_CTX_new_id(EVP_PKEY_EC, nullptr));
    if (!pctx)
        return false;
    if (1 != EVP_PKEY_paramgen_init(pctx.pointer))
        return false;
    if (1 != EVP_PKEY_CTX_set_ec_paramgen_curve_nid(pctx.pointer, nid))
        return false;
    if (!EVP_PKEY_paramgen(pctx.pointer, &params.pointer))
        return false;

    const auto_evp_pkey_ctx_t kctx(EVP_PKEY_CTX_new(params.pointer, nullptr));
    if (!kctx)
        return false;

    if (1 != EVP_PKEY_keygen_init(kctx.pointer))
        return false;
    if (1 != EVP_PKEY_keygen(kctx.pointer, &pkey.pointer))
        return false;

    return true;
#else
    auto_ec_key_t key(EC_KEY_new_by_curve_name(nid));

    if (!key)
        return false;
    if (1 != EC_KEY_generate_key(key.pointer))
        return false;
    return true;
#endif
#endif /* HAVE_DH etc */

    return false;
#else
    return false;
#endif // HAVE_EC
}

void curve_probe_t::probe(ErlNifEnv *env, const bool fips_mode, std::vector<curve_availability_t> &output) {
    this->atom = create_or_existing_atom(env, this->sn, this->atom);
    curve_availability_t algo = {.init = this};

    // Some curves can be pre-checked by their NID. Passing NID=0 will skip this check
    if (nid && !this->is_curve_valid_by_nid()) {
        return; // invalid/unsupported curves are skipped
    }

    algo.probe_under_fips(fips_mode);
    output.push_back(algo);
}

ERL_NIF_TERM curve_availability_t::get_atom() const { return this->init->atom; }

void curve_availability_t::probe_under_fips(bool fips_mode) {
#if defined(FIPS_SUPPORT) && defined(HAS_3_0_API)
    // This checking code only runs under FIPS and OpenSSL 3+, other cases algorithm is always added
    if (!fips_mode)
        return;

    OSSL_PARAM params[2];

    const auto_evp_pkey_ctx_t pctx(EVP_PKEY_CTX_new_from_name(nullptr, "EC", "fips=yes"));
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
    auto_evp_pkey_t pkey(nullptr);
    if (EVP_PKEY_generate(pctx.pointer, &pkey.pointer) <= 0) {
        this->flags.algorithm_init_failed = true;
    }
#endif
}
