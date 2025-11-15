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

#include "algorithms_cipher.h"

cipher_probe_t cipher_probes[] = {
#ifdef HAVE_RC2
#define CIPHER_RC2_CTOR &EVP_rc2_cbc
#else
#define CIPHER_RC2_CTOR nullptr
#endif
        {.str = "rc2_cbc", .v3 = "rc2-cbc", .ctor = CIPHER_RC2_CTOR, .flags = {.fips_forbidden = true}},
#undef CIPHER_RC2_CTOR
// --------------------------
#ifdef HAVE_RC4
#define CIPHER_RC4_CTOR &EVP_rc4
#else
#define CIPHER_RC4_CTOR nullptr
#endif
        {.str = "rc4", .v3 = "rc4", .ctor = CIPHER_RC4_CTOR, .flags = {.fips_forbidden = true}},
#undef CIPHER_RC4_CTOR
// --------------------------
#ifdef HAVE_DES
#define CIPHER_DES_CBC_CTOR &EVP_des_cbc
#define CIPHER_DES_CFB8_CTOR &EVP_des_cfb8
#define CIPHER_DES_ECB_CTOR &EVP_des_ecb
#else
#define CIPHER_DES_CBC_CTOR nullptr
#define CIPHER_DES_CFB8_CTOR nullptr
#define CIPHER_DES_ECB_CTOR nullptr
#endif
        {.str = "des_cbc", .v3 = "des-cbc", .ctor = CIPHER_DES_CBC_CTOR, .flags = {.fips_forbidden = true}},
        {.str = "des_cfb", .v3 = "des-cfb", .ctor = CIPHER_DES_CFB8_CTOR, .flags = {.fips_forbidden = true}},
        {.str = "des_ecb",
         .v3 = "des-ecb",
         .ctor = CIPHER_DES_ECB_CTOR,
         .flags = {.fips_forbidden = true, .ecb_bug_0_9_8l = true}},
#undef CIPHER_DES_CBC_CTOR
#undef CIPHER_DES_CFB8_CTOR
#undef CIPHER_DES_ECB_CTOR
// --------------------------
#ifdef HAVE_DES_ede3_cbc
#define CIPHER_DES_EDE3_CBC_CTOR &EVP_des_ede3_cbc
#else
#define CIPHER_DES_EDE3_CBC_CTOR nullptr
#endif
        {.str = "des_ede3_cbc", .v3 = "des-ede3-cbc", .ctor = CIPHER_DES_EDE3_CBC_CTOR},
#undef CIPHER_DES_EDE3_CBC_CTOR
// --------------------------
#ifdef HAVE_DES_ede3_cfb
#define CIPHER_DES_EDE3_CFB_CTOR &EVP_des_ede3_cfb8
#else
#define CIPHER_DES_EDE3_CFB_CTOR nullptr
#endif
        {.str = "des_ede3_cfb", .v3 = "des-ede3-cfb", .ctor = CIPHER_DES_EDE3_CFB_CTOR},
#undef CIPHER_DES_EDE3_CFB_CTOR
// --------------------------
#ifdef HAVE_BF
#define CIPHER_BF_CBC_CTOR &EVP_bf_cbc
#define CIPHER_BF_CFB64_CTOR &EVP_bf_cfb64
#define CIPHER_BF_OFB_CTOR &EVP_bf_ofb
#define CIPHER_BF_ECB_CTOR &EVP_bf_ecb
#else
#define CIPHER_BF_CBC_CTOR nullptr
#define CIPHER_BF_CFB64_CTOR nullptr
#define CIPHER_BF_OFB_CTOR nullptr
#define CIPHER_BF_ECB_CTOR nullptr
#endif
        {.str = "blowfish_cbc", .v3 = "BF-CBC", .ctor = CIPHER_BF_CBC_CTOR, .flags = {.fips_forbidden = true}},
        {.str = "blowfish_cfb64", .v3 = "BF-CFB", .ctor = CIPHER_BF_CFB64_CTOR, .flags = {.fips_forbidden = true}},
        {.str = "blowfish_ofb64", .v3 = "BF-OFB", .ctor = CIPHER_BF_OFB_CTOR, .flags = {.fips_forbidden = true}},
        {.str = "blowfish_ecb",
         .v3 = "BF-ECB",
         .ctor = CIPHER_BF_ECB_CTOR,
         .flags = {.fips_forbidden = true, .ecb_bug_0_9_8l = true}},
#undef CIPHER_BF_CBC_CTOR
#undef CIPHER_BF_CFB64_CTOR
#undef CIPHER_BF_OFB_CTOR
#undef CIPHER_BF_ECB_CTOR
// --------------------------
#ifdef HAVE_SM4
#define CIPHER_SM4_CBC_CTOR &EVP_sm4_cbc
#define CIPHER_SM4_ECB_CTOR &EVP_sm4_ecb
#define CIPHER_SM4_CFB_CTOR &EVP_sm4_cfb
#define CIPHER_SM4_OFB_CTOR &EVP_sm4_ofb
#define CIPHER_SM4_CTR_CTOR &EVP_sm4_ctr
#else
#define CIPHER_SM4_CBC_CTOR nullptr
#define CIPHER_SM4_ECB_CTOR nullptr
#define CIPHER_SM4_CFB_CTOR nullptr
#define CIPHER_SM4_OFB_CTOR nullptr
#define CIPHER_SM4_CTR_CTOR nullptr
#endif
        {.str = "sm4_cbc",
         .v3 = "sm4-cbc",
         .ctor = CIPHER_SM4_CBC_CTOR,
         .key_len = 16,
         .flags = {.fips_forbidden = true}},
        {.str = "sm4_ecb",
         .v3 = "sm4-ecb",
         .ctor = CIPHER_SM4_ECB_CTOR,
         .key_len = 16,
         .flags = {.fips_forbidden = true}},
        {.str = "sm4_cfb",
         .v3 = "sm4-cfb",
         .ctor = CIPHER_SM4_CFB_CTOR,
         .key_len = 16,
         .flags = {.fips_forbidden = true}},
        {.str = "sm4_ofb",
         .v3 = "sm4-ofb",
         .ctor = CIPHER_SM4_OFB_CTOR,
         .key_len = 16,
         .flags = {.fips_forbidden = true}},
        {.str = "sm4_ctr",
         .v3 = "sm4-ctr",
         .ctor = CIPHER_SM4_CTR_CTOR,
         .key_len = 16,
         .flags = {.fips_forbidden = true}},
#undef CIPHER_SM4_CBC_CTOR
#undef CIPHER_SM4_ECB_CTOR
#undef CIPHER_SM4_CFB_CTOR
#undef CIPHER_SM4_OFB_CTOR
#undef CIPHER_SM4_CTR_CTOR
        // --------------------------
        {.str = "aes_128_cbc", .v3 = "aes-128-cbc", .ctor = &EVP_aes_128_cbc, .key_len = 16},
        {.str = "aes_192_cbc", .v3 = "aes-192-cbc", .ctor = &EVP_aes_192_cbc, .key_len = 24},
        {.str = "aes_256_cbc", .v3 = "aes-256-cbc", .ctor = &EVP_aes_256_cbc, .key_len = 32},

        {.str = "aes_128_ofb", .v3 = "aes-128-ofb", .ctor = &EVP_aes_128_ofb, .key_len = 16},
        {.str = "aes_192_ofb", .v3 = "aes-192-ofb", .ctor = &EVP_aes_192_ofb, .key_len = 24},
        {.str = "aes_256_ofb", .v3 = "aes-256-ofb", .ctor = &EVP_aes_256_ofb, .key_len = 32},

        {.str = "aes_128_cfb8",
         .v3 = "aes-128-cfb8",
         .ctor = &EVP_aes_128_cfb8,
         .key_len = 16,
         .flags = {.aes_cfbx = true}},
        {.str = "aes_192_cfb8",
         .v3 = "aes-192-cfb8",
         .ctor = &EVP_aes_192_cfb8,
         .key_len = 24,
         .flags = {.aes_cfbx = true}},
        {.str = "aes_256_cfb8",
         .v3 = "aes-256-cfb8",
         .ctor = &EVP_aes_256_cfb8,
         .key_len = 32,
         .flags = {.aes_cfbx = true}},

        {.str = "aes_128_cfb128",
         .v3 = "aes-128-cfb",
         .ctor = &EVP_aes_128_cfb128,
         .key_len = 16,
         .flags = {.aes_cfbx = true}},
        {.str = "aes_192_cfb128",
         .v3 = "aes-192-cfb",
         .ctor = &EVP_aes_192_cfb128,
         .key_len = 24,
         .flags = {.aes_cfbx = true}},
        {.str = "aes_256_cfb128",
         .v3 = "aes-256-cfb",
         .ctor = &EVP_aes_256_cfb128,
         .key_len = 32,
         .flags = {.aes_cfbx = true}},

        {.str = "aes_128_ecb",
         .v3 = "aes-128-ecb",
         .ctor = &EVP_aes_128_ecb,
         .key_len = 16,
         .flags = {.ecb_bug_0_9_8l = true}},
        {.str = "aes_192_ecb",
         .v3 = "aes-192-ecb",
         .ctor = &EVP_aes_192_ecb,
         .key_len = 24,
         .flags = {.ecb_bug_0_9_8l = true}},
        {.str = "aes_256_ecb",
         .v3 = "aes-256-ecb",
         .ctor = &EVP_aes_256_ecb,
         .key_len = 32,
         .flags = {.ecb_bug_0_9_8l = true}},
// --------------------------
#if defined(HAVE_EVP_AES_CTR)
#define CIPHER_AES128_CTR_CTOR &EVP_aes_128_ctr
#define CIPHER_AES192_CTR_CTOR &EVP_aes_192_ctr
#define CIPHER_AES256_CTR_CTOR &EVP_aes_256_ctr
#else
#define CIPHER_AES128_CTR_CTOR nullptr
#define CIPHER_AES192_CTR_CTOR nullptr
#define CIPHER_AES256_CTR_CTOR nullptr
#endif
        {.str = "aes_128_ctr", .v3 = "aes-128-ctr", .ctor = CIPHER_AES128_CTR_CTOR, .key_len = 16},
        {.str = "aes_192_ctr", .v3 = "aes-192-ctr", .ctor = CIPHER_AES192_CTR_CTOR, .key_len = 24},
        {.str = "aes_256_ctr", .v3 = "aes-256-ctr", .ctor = CIPHER_AES256_CTR_CTOR, .key_len = 32},
#undef CIPHER_AES128_CTR_CTOR
#undef CIPHER_AES192_CTR_CTOR
#undef CIPHER_AES256_CTR_CTOR
// --------------------------
#if defined(HAVE_CHACHA20)
#define CIPHER_CHACHA20_CTOR &EVP_chacha20
#else
#define CIPHER_CHACHA20_CTOR nullptr
#endif
        {.str = "chacha20", .ctor = CIPHER_CHACHA20_CTOR, .key_len = 32, .flags = {.fips_forbidden = true}},
#undef CIPHER_CHACHA20_CTOR

// --------------------------
// AEAD ciphers
// --------------------------

#if defined(HAVE_CHACHA20_POLY1305)
#define CIPHER_CHACHA20_POLY1305_CTOR &EVP_chacha20_poly1305
#define CIPHER_CHACHA20_POLY1305_AEADCTRL AEAD_CTRL
#else
#define CIPHER_CHACHA20_POLY1305_CTOR nullptr
#define CIPHER_CHACHA20_POLY1305_AEADCTRL NOT_AEAD
#endif
        {.str = "chacha20_poly1305",
         .v3 = "chacha20-poly1305",
         .ctor = &EVP_chacha20_poly1305,
         .flags = {.fips_forbidden = true, .aead_cipher = true},
         .aead_ctrl_type = CIPHER_CHACHA20_POLY1305_AEADCTRL},
#undef CIPHER_CHACHA20_POLY1305_CTOR
#undef CIPHER_CHACHA20_POLY1305_AEADCTRL
// --------------------------
#if defined(HAVE_SM4_GCM)
        {.str = "sm4_gcm",
         .v3 = "sm4-gcm",
         .key_len = 16,
         .flags = {.fips_forbidden = true, .aead_cipher = true, .gcm_mode = true},
         .aead_ctrl_type = AEAD_CTRL},
#endif
// --------------------------
#if defined(HAVE_SM4_CCM)
        {.str = "sm4_ccm",
         .v3 = "sm4-ccm",
         .key_len = 16,
         .flags = {.fips_forbidden = true, .aead_cipher = true, .ccm_mode = true},
         .aead_ctrl_type = AEAD_CTRL},
#endif
// --------------------------
#if defined(HAVE_GCM)
#define CIPHER_AES128_GCM_CTOR &EVP_aes_128_gcm
#define CIPHER_AES192_GCM_CTOR &EVP_aes_192_gcm
#define CIPHER_AES256_GCM_CTOR &EVP_aes_256_gcm
#if defined(HAS_3_0_API)
#define CIPHER_AES_GCM_AEADCTRL AEAD_CTRL
#else
#define CIPHER_AES_GCM_AEADCTRL AEAD_CTRL_GCM
#endif
#else // not HAVE_GCM
#define CIPHER_AES128_GCM_CTOR nullptr
#define CIPHER_AES192_GCM_CTOR nullptr
#define CIPHER_AES256_GCM_CTOR nullptr
#define CIPHER_AES_GCM_AEADCTRL NOT_AEAD
#endif
        {.str = "aes_128_gcm",
         .v3 = "aes-128-gcm",
         .ctor = CIPHER_AES128_GCM_CTOR,
         .key_len = 16,
         .flags = {.aead_cipher = true, .gcm_mode = true},
         .aead_ctrl_type = CIPHER_AES_GCM_AEADCTRL},
        {.str = "aes_192_gcm",
         .v3 = "aes-192-gcm",
         .ctor = CIPHER_AES192_GCM_CTOR,
         .key_len = 24,
         .flags = {.aead_cipher = true, .gcm_mode = true},
         .aead_ctrl_type = CIPHER_AES_GCM_AEADCTRL},
        {.str = "aes_256_gcm",
         .v3 = "aes-256-gcm",
         .ctor = CIPHER_AES256_GCM_CTOR,
         .key_len = 32,
         .flags = {.aead_cipher = true, .gcm_mode = true},
         .aead_ctrl_type = CIPHER_AES_GCM_AEADCTRL},
#undef CIPHER_AES128_GCM_CTOR
#undef CIPHER_AES192_GCM_CTOR
#undef CIPHER_AES256_GCM_CTOR
#undef CIPHER_AES_GCM_AEADCTRL
// --------------------------
#if defined(HAVE_CCM)
#define CIPHER_AES128_CCM_CTOR &EVP_aes_128_ccm
#define CIPHER_AES192_CCM_CTOR &EVP_aes_192_ccm
#define CIPHER_AES256_CCM_CTOR &EVP_aes_256_ccm
#if defined(HAS_3_0_API)
#define CIPHER_AES_CCM_AEADCTRL AEAD_CTRL
#else
#define CIPHER_AES_CCM_AEADCTRL AEAD_CTRL_CCM
#endif
#else // not HAVE_GCM
#define CIPHER_AES128_CCM_CTOR nullptr
#define CIPHER_AES192_CCM_CTOR nullptr
#define CIPHER_AES256_CCM_CTOR nullptr
#define CIPHER_AES_CCM_AEADCTRL NOT_AEAD
#endif
        {.str = "aes_128_ccm",
         .v3 = "aes-128-ccm",
         .ctor = CIPHER_AES128_CCM_CTOR,
         .key_len = 16,
         .flags = {.aead_cipher = true, .ccm_mode = true},
         .aead_ctrl_type = CIPHER_AES_CCM_AEADCTRL},
        {.str = "aes_192_ccm",
         .v3 = "aes-192-ccm",
         .ctor = CIPHER_AES192_CCM_CTOR,
         .key_len = 24,
         .flags = {.aead_cipher = true, .ccm_mode = true},
         .aead_ctrl_type = CIPHER_AES_CCM_AEADCTRL},
        {.str = "aes_256_ccm",
         .v3 = "aes-256-ccm",
         .ctor = CIPHER_AES256_CCM_CTOR,
         .key_len = 32,
         .flags = {.aead_cipher = true, .ccm_mode = true},
         .aead_ctrl_type = CIPHER_AES_CCM_AEADCTRL},
#undef CIPHER_AES128_CCM_CTOR
#undef CIPHER_AES192_CCM_CTOR
#undef CIPHER_AES256_CCM_CTOR
#undef CIPHER_AES_CCM_AEADCTRL
        // --------------------------
        {} // stopper record
};

cipher_collection_t cipher_collection("crypto.cipher_collection", cipher_probes);

//
// Implementation of Known Cipher Algorithms storage API
//

// C API: Proxy the call to generic algorithm_collection_t
extern "C" size_t cipher_algorithms_lazy_init(ErlNifEnv *env, const bool fips_enabled) {
    return cipher_collection.lazy_init(env, fips_enabled);
}

// C API: Proxy the call to generic algorithm_collection_t
extern "C" ERL_NIF_TERM cipher_algorithms_as_list(ErlNifEnv *env, const bool fips_enabled) {
    return cipher_collection.to_list(env, fips_enabled);
}

ERL_NIF_TERM cipher_availability_t::get_atom() const { return this->init->atom; }

void cipher_availability_t::setup_cipher(bool fips_enabled) {
    if (this->init->ctor) {
    }
    if (this->flags.aead_cipher) {
        // NOT_AEAD = {{0, 0, 0}}
        // AEAD_CTRL = {{EVP_CTRL_AEAD_SET_IVLEN, EVP_CTRL_AEAD_GET_TAG, EVP_CTRL_AEAD_SET_TAG}}
        // AEAD+GCM {{EVP_CTRL_GCM_SET_IVLEN, EVP_CTRL_GCM_GET_TAG, EVP_CTRL_GCM_SET_TAG}}
        // AEAD+CM  {{EVP_CTRL_CCM_SET_IVLEN,EVP_CTRL_CCM_GET_TAG,EVP_CTRL_CCM_SET_TAG}}
        // todo:set AEAD_CTRL
    }
    // todo: if fips and not flags.fips_forbidden - test the cipher quick
}

// for FIPS we will attempt to initialize the pubkey context to verify whether the
// algorithm is allowed, for non-FIPS keeping the old behavior - always allow the algorithm.
void cipher_probe_t::probe(ErlNifEnv *env, const bool fips_enabled, std::vector<cipher_availability_t> &output) {
    this->atom = create_or_existing_atom(env, this->v3, this->atom);
    if (this->ctor != nullptr) {
        output.emplace_back(cipher_availability_t{.init = this, .flags = this->flags});
        output.back().setup_cipher(fips_enabled);
    }
}
