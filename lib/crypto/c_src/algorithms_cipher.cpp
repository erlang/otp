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

#include <algorithm>

cipher_probe_t cipher_probes[] = {
#ifdef HAVE_RC2
#    define CIPHER_RC2_CTOR &EVP_rc2_cbc
#else
#    define CIPHER_RC2_CTOR nullptr
#endif
        {.str = "rc2_cbc", .str_v3 = "rc2-cbc", .ctor_v1 = CIPHER_RC2_CTOR, .flags = {.fips_forbidden = true}},
#undef CIPHER_RC2_CTOR
// --------------------------
#ifdef HAVE_RC4
#    define CIPHER_RC4_CTOR &EVP_rc4
#else
#    define CIPHER_RC4_CTOR nullptr
#endif
        {.str = "rc4", .str_v3 = "rc4", .ctor_v1 = CIPHER_RC4_CTOR, .flags = {.fips_forbidden = true}},
#undef CIPHER_RC4_CTOR
// --------------------------
#ifdef HAVE_DES
#    define CIPHER_DES_CBC_CTOR &EVP_des_cbc
#    define CIPHER_DES_CFB8_CTOR &EVP_des_cfb8
#    define CIPHER_DES_ECB_CTOR &EVP_des_ecb
#else
#    define CIPHER_DES_CBC_CTOR nullptr
#    define CIPHER_DES_CFB8_CTOR nullptr
#    define CIPHER_DES_ECB_CTOR nullptr
#endif
        {.str = "des_cbc", .str_v3 = "des-cbc", .ctor_v1 = CIPHER_DES_CBC_CTOR, .flags = {.fips_forbidden = true}},
        {.str = "des_cfb", .str_v3 = "des-cfb", .ctor_v1 = CIPHER_DES_CFB8_CTOR, .flags = {.fips_forbidden = true}},
        {.str = "des_ecb",
         .str_v3 = "des-ecb",
         .ctor_v1 = CIPHER_DES_ECB_CTOR,
         .flags = {.fips_forbidden = true, .ecb_bug_0_9_8l = true}},
#undef CIPHER_DES_CBC_CTOR
#undef CIPHER_DES_CFB8_CTOR
#undef CIPHER_DES_ECB_CTOR
// --------------------------
#ifdef HAVE_DES_ede3_cbc
#    define CIPHER_DES_EDE3_CBC_CTOR &EVP_des_ede3_cbc
#else
#    define CIPHER_DES_EDE3_CBC_CTOR nullptr
#endif
        {.str = "des_ede3_cbc", .str_v3 = "des-ede3-cbc", .ctor_v1 = CIPHER_DES_EDE3_CBC_CTOR},
#undef CIPHER_DES_EDE3_CBC_CTOR
// --------------------------
#ifdef HAVE_DES_ede3_cfb
#    define CIPHER_DES_EDE3_CFB_CTOR &EVP_des_ede3_cfb8
#else
#    define CIPHER_DES_EDE3_CFB_CTOR nullptr
#endif
        {.str = "des_ede3_cfb", .str_v3 = "des-ede3-cfb", .ctor_v1 = CIPHER_DES_EDE3_CFB_CTOR},
#undef CIPHER_DES_EDE3_CFB_CTOR
// --------------------------
#ifdef HAVE_BF
#    define CIPHER_BF_CBC_CTOR &EVP_bf_cbc
#    define CIPHER_BF_CFB64_CTOR &EVP_bf_cfb64
#    define CIPHER_BF_OFB_CTOR &EVP_bf_ofb
#    define CIPHER_BF_ECB_CTOR &EVP_bf_ecb
#else
#    define CIPHER_BF_CBC_CTOR nullptr
#    define CIPHER_BF_CFB64_CTOR nullptr
#    define CIPHER_BF_OFB_CTOR nullptr
#    define CIPHER_BF_ECB_CTOR nullptr
#endif
        {.str = "blowfish_cbc", .str_v3 = "BF-CBC", .ctor_v1 = CIPHER_BF_CBC_CTOR, .flags = {.fips_forbidden = true}},
        {.str = "blowfish_cfb64",
         .str_v3 = "BF-CFB",
         .ctor_v1 = CIPHER_BF_CFB64_CTOR,
         .flags = {.fips_forbidden = true}},
        {.str = "blowfish_ofb64", .str_v3 = "BF-OFB", .ctor_v1 = CIPHER_BF_OFB_CTOR, .flags = {.fips_forbidden = true}},
        {.str = "blowfish_ecb",
         .str_v3 = "BF-ECB",
         .ctor_v1 = CIPHER_BF_ECB_CTOR,
         .flags = {.fips_forbidden = true, .ecb_bug_0_9_8l = true}},
#undef CIPHER_BF_CBC_CTOR
#undef CIPHER_BF_CFB64_CTOR
#undef CIPHER_BF_OFB_CTOR
#undef CIPHER_BF_ECB_CTOR
// --------------------------
#ifdef HAVE_SM4
#    define CIPHER_SM4_CBC_CTOR &EVP_sm4_cbc
#    define CIPHER_SM4_ECB_CTOR &EVP_sm4_ecb
#    define CIPHER_SM4_CFB_CTOR &EVP_sm4_cfb
#    define CIPHER_SM4_OFB_CTOR &EVP_sm4_ofb
#    define CIPHER_SM4_CTR_CTOR &EVP_sm4_ctr
#else
#    define CIPHER_SM4_CBC_CTOR nullptr
#    define CIPHER_SM4_ECB_CTOR nullptr
#    define CIPHER_SM4_CFB_CTOR nullptr
#    define CIPHER_SM4_OFB_CTOR nullptr
#    define CIPHER_SM4_CTR_CTOR nullptr
#endif
        {.str = "sm4_cbc",
         .str_v3 = "sm4-cbc",
         .ctor_v1 = CIPHER_SM4_CBC_CTOR,
         .key_len = 16,
         .flags = {.fips_forbidden = true}},
        {.str = "sm4_ecb",
         .str_v3 = "sm4-ecb",
         .ctor_v1 = CIPHER_SM4_ECB_CTOR,
         .key_len = 16,
         .flags = {.fips_forbidden = true}},
        {.str = "sm4_cfb",
         .str_v3 = "sm4-cfb",
         .ctor_v1 = CIPHER_SM4_CFB_CTOR,
         .key_len = 16,
         .flags = {.fips_forbidden = true}},
        {.str = "sm4_ofb",
         .str_v3 = "sm4-ofb",
         .ctor_v1 = CIPHER_SM4_OFB_CTOR,
         .key_len = 16,
         .flags = {.fips_forbidden = true}},
        {.str = "sm4_ctr",
         .str_v3 = "sm4-ctr",
         .ctor_v1 = CIPHER_SM4_CTR_CTOR,
         .key_len = 16,
         .flags = {.fips_forbidden = true}},
#undef CIPHER_SM4_CBC_CTOR
#undef CIPHER_SM4_ECB_CTOR
#undef CIPHER_SM4_CFB_CTOR
#undef CIPHER_SM4_OFB_CTOR
#undef CIPHER_SM4_CTR_CTOR
        // --------------------------
        {.str = "aes_128_cbc", .str_v3 = "aes-128-cbc", .ctor_v1 = &EVP_aes_128_cbc, .key_len = 16},
        {.str = "aes_192_cbc", .str_v3 = "aes-192-cbc", .ctor_v1 = &EVP_aes_192_cbc, .key_len = 24},
        {.str = "aes_256_cbc", .str_v3 = "aes-256-cbc", .ctor_v1 = &EVP_aes_256_cbc, .key_len = 32},

        {.str = "aes_128_ofb", .str_v3 = "aes-128-ofb", .ctor_v1 = &EVP_aes_128_ofb, .key_len = 16},
        {.str = "aes_192_ofb", .str_v3 = "aes-192-ofb", .ctor_v1 = &EVP_aes_192_ofb, .key_len = 24},
        {.str = "aes_256_ofb", .str_v3 = "aes-256-ofb", .ctor_v1 = &EVP_aes_256_ofb, .key_len = 32},

        {.str = "aes_128_cfb8",
         .str_v3 = "aes-128-cfb8",
         .ctor_v1 = &EVP_aes_128_cfb8,
         .key_len = 16,
         .flags = {.aes_cfbx = true}},
        {.str = "aes_192_cfb8",
         .str_v3 = "aes-192-cfb8",
         .ctor_v1 = &EVP_aes_192_cfb8,
         .key_len = 24,
         .flags = {.aes_cfbx = true}},
        {.str = "aes_256_cfb8",
         .str_v3 = "aes-256-cfb8",
         .ctor_v1 = &EVP_aes_256_cfb8,
         .key_len = 32,
         .flags = {.aes_cfbx = true}},

        {.str = "aes_128_cfb128",
         .str_v3 = "aes-128-cfb",
         .ctor_v1 = &EVP_aes_128_cfb128,
         .key_len = 16,
         .flags = {.aes_cfbx = true}},
        {.str = "aes_192_cfb128",
         .str_v3 = "aes-192-cfb",
         .ctor_v1 = &EVP_aes_192_cfb128,
         .key_len = 24,
         .flags = {.aes_cfbx = true}},
        {.str = "aes_256_cfb128",
         .str_v3 = "aes-256-cfb",
         .ctor_v1 = &EVP_aes_256_cfb128,
         .key_len = 32,
         .flags = {.aes_cfbx = true}},

        {.str = "aes_128_ecb",
         .str_v3 = "aes-128-ecb",
         .ctor_v1 = &EVP_aes_128_ecb,
         .key_len = 16,
         .flags = {.ecb_bug_0_9_8l = true}},
        {.str = "aes_192_ecb",
         .str_v3 = "aes-192-ecb",
         .ctor_v1 = &EVP_aes_192_ecb,
         .key_len = 24,
         .flags = {.ecb_bug_0_9_8l = true}},
        {.str = "aes_256_ecb",
         .str_v3 = "aes-256-ecb",
         .ctor_v1 = &EVP_aes_256_ecb,
         .key_len = 32,
         .flags = {.ecb_bug_0_9_8l = true}},
// --------------------------
#if defined(HAVE_EVP_AES_CTR)
#    define CIPHER_AES128_CTR_CTOR &EVP_aes_128_ctr
#    define CIPHER_AES192_CTR_CTOR &EVP_aes_192_ctr
#    define CIPHER_AES256_CTR_CTOR &EVP_aes_256_ctr
#else
#    define CIPHER_AES128_CTR_CTOR nullptr
#    define CIPHER_AES192_CTR_CTOR nullptr
#    define CIPHER_AES256_CTR_CTOR nullptr
#endif
        {.str = "aes_128_ctr", .str_v3 = "aes-128-ctr", .ctor_v1 = CIPHER_AES128_CTR_CTOR, .key_len = 16},
        {.str = "aes_192_ctr", .str_v3 = "aes-192-ctr", .ctor_v1 = CIPHER_AES192_CTR_CTOR, .key_len = 24},
        {.str = "aes_256_ctr", .str_v3 = "aes-256-ctr", .ctor_v1 = CIPHER_AES256_CTR_CTOR, .key_len = 32},
#undef CIPHER_AES128_CTR_CTOR
#undef CIPHER_AES192_CTR_CTOR
#undef CIPHER_AES256_CTR_CTOR
// --------------------------
#if defined(HAVE_CHACHA20)
#    define CIPHER_CHACHA20_CTOR &EVP_chacha20
#else
#    define CIPHER_CHACHA20_CTOR nullptr
#endif
        {.str = "chacha20", .ctor_v1 = CIPHER_CHACHA20_CTOR, .key_len = 32, .flags = {.fips_forbidden = true}},
#undef CIPHER_CHACHA20_CTOR

// --------------------------
// AEAD ciphers
// --------------------------

#if defined(HAVE_CHACHA20_POLY1305)
#    define CIPHER_CHACHA20_POLY1305_CTOR &EVP_chacha20_poly1305
#    define CIPHER_CHACHA20_POLY1305_AEADCTRL AEAD_CTRL
#else
#    define CIPHER_CHACHA20_POLY1305_CTOR nullptr
#    define CIPHER_CHACHA20_POLY1305_AEADCTRL NOT_AEAD
#endif
#ifdef HAVE_AEAD
        {.str = "chacha20_poly1305",
         .str_v3 = "chacha20-poly1305",
         .ctor_v1 = CIPHER_CHACHA20_POLY1305_CTOR,
         .flags = {.fips_forbidden = true, .aead_cipher = true},
         .aead_ctrl_type = CIPHER_CHACHA20_POLY1305_AEADCTRL},
#endif // HAVE_AEAD
#undef CIPHER_CHACHA20_POLY1305_CTOR
#undef CIPHER_CHACHA20_POLY1305_AEADCTRL
// --------------------------
#if defined(HAVE_SM4_GCM)
        {.str = "sm4_gcm",
         .str_v3 = "sm4-gcm",
         .key_len = 16,
         .flags = {.fips_forbidden = true, .aead_cipher = true, .gcm_mode = true},
         .aead_ctrl_type = AEAD_CTRL},
#endif
// --------------------------
#if defined(HAVE_SM4_CCM)
        {.str = "sm4_ccm",
         .str_v3 = "sm4-ccm",
         .key_len = 16,
         .flags = {.fips_forbidden = true, .aead_cipher = true, .ccm_mode = true},
         .aead_ctrl_type = AEAD_CTRL},
#endif
// --------------------------
#if defined(HAVE_GCM)
#    define CIPHER_AES128_GCM_CTOR &EVP_aes_128_gcm
#    define CIPHER_AES192_GCM_CTOR &EVP_aes_192_gcm
#    define CIPHER_AES256_GCM_CTOR &EVP_aes_256_gcm
#    if defined(HAS_3_0_API)
#        define CIPHER_AES_GCM_AEADCTRL AEAD_CTRL
#    else
#        define CIPHER_AES_GCM_AEADCTRL AEAD_CTRL_GCM
#    endif
#else // not HAVE_GCM
#    define CIPHER_AES128_GCM_CTOR nullptr
#    define CIPHER_AES192_GCM_CTOR nullptr
#    define CIPHER_AES256_GCM_CTOR nullptr
#    define CIPHER_AES_GCM_AEADCTRL NOT_AEAD
#endif
#ifdef HAVE_AEAD
        {.str = "aes_128_gcm",
         .str_v3 = "aes-128-gcm",
         .ctor_v1 = CIPHER_AES128_GCM_CTOR,
         .key_len = 16,
         .flags = {.aead_cipher = true, .gcm_mode = true},
         .aead_ctrl_type = CIPHER_AES_GCM_AEADCTRL},
        {.str = "aes_192_gcm",
         .str_v3 = "aes-192-gcm",
         .ctor_v1 = CIPHER_AES192_GCM_CTOR,
         .key_len = 24,
         .flags = {.aead_cipher = true, .gcm_mode = true},
         .aead_ctrl_type = CIPHER_AES_GCM_AEADCTRL},
        {.str = "aes_256_gcm",
         .str_v3 = "aes-256-gcm",
         .ctor_v1 = CIPHER_AES256_GCM_CTOR,
         .key_len = 32,
         .flags = {.aead_cipher = true, .gcm_mode = true},
         .aead_ctrl_type = CIPHER_AES_GCM_AEADCTRL},
#endif // HAVE_AEAD
#undef CIPHER_AES128_GCM_CTOR
#undef CIPHER_AES192_GCM_CTOR
#undef CIPHER_AES256_GCM_CTOR
#undef CIPHER_AES_GCM_AEADCTRL
// --------------------------
#if defined(HAVE_CCM)
#    define CIPHER_AES128_CCM_CTOR &EVP_aes_128_ccm
#    define CIPHER_AES192_CCM_CTOR &EVP_aes_192_ccm
#    define CIPHER_AES256_CCM_CTOR &EVP_aes_256_ccm
#    if defined(HAS_3_0_API)
#        define CIPHER_AES_CCM_AEADCTRL AEAD_CTRL
#    else
#        define CIPHER_AES_CCM_AEADCTRL AEAD_CTRL_CCM
#    endif
#else // not HAVE_GCM
#    define CIPHER_AES128_CCM_CTOR nullptr
#    define CIPHER_AES192_CCM_CTOR nullptr
#    define CIPHER_AES256_CCM_CTOR nullptr
#    define CIPHER_AES_CCM_AEADCTRL NOT_AEAD
#endif
#ifdef HAVE_AEAD
        {.str = "aes_128_ccm",
         .str_v3 = "aes-128-ccm",
         .ctor_v1 = CIPHER_AES128_CCM_CTOR,
         .key_len = 16,
         .flags = {.aead_cipher = true, .ccm_mode = true},
         .aead_ctrl_type = CIPHER_AES_CCM_AEADCTRL},
        {.str = "aes_192_ccm",
         .str_v3 = "aes-192-ccm",
         .ctor_v1 = CIPHER_AES192_CCM_CTOR,
         .key_len = 24,
         .flags = {.aead_cipher = true, .ccm_mode = true},
         .aead_ctrl_type = CIPHER_AES_CCM_AEADCTRL},
        {.str = "aes_256_ccm",
         .str_v3 = "aes-256-ccm",
         .ctor_v1 = CIPHER_AES256_CCM_CTOR,
         .key_len = 32,
         .flags = {.aead_cipher = true, .ccm_mode = true},
         .aead_ctrl_type = CIPHER_AES_CCM_AEADCTRL},
#endif // HAVE_AEAD
#undef CIPHER_AES128_CCM_CTOR
#undef CIPHER_AES192_CCM_CTOR
#undef CIPHER_AES256_CCM_CTOR
#undef CIPHER_AES_CCM_AEADCTRL
};

cipher_collection_t cipher_collection("crypto.cipher_collection",
                                      cipher_probes,
                                      sizeof(cipher_probes) / sizeof(cipher_probes[0]));

//
// Implementation of Known Cipher Algorithms storage API
//

void cipher_probe_t::post_lazy_init(std::vector<cipher_type_t> &algorithms) {
    std::sort(algorithms.begin(), algorithms.end(), cipher_type_t::compare_function);
}

// Partial order compare, returns a.atom < b.atom && a.key < b.key
bool cipher_type_t::compare_function(const cipher_type_t &a, const cipher_type_t &b) {
    return (a.atom < b.atom) || (a.atom == b.atom && a.key_len < b.key_len);
}

// Partial order compare, returns a.atom < b.atom
bool cipher_type_t::compare_function_no_key(const cipher_type_t &a, const cipher_type_t &b) {
    return a.atom < b.atom;
}

// C API: Proxy the call to generic algorithm_collection_t
extern "C" ERL_NIF_TERM cipher_algorithms_as_list(ErlNifEnv *env, const bool fips_enabled) {
    return cipher_collection.to_list(env, fips_enabled);
}

ERL_NIF_TERM cipher_type_t::get_atom() const {
    return this->init->atom;
}

#if defined(HAS_3_0_API)
// Initialize an algorithm to check that all its dependencies are valid
bool cipher_type_t::can_cipher_be_instantiated() const {
    const auto_cipher_ctx_t ctx(EVP_CIPHER_CTX_new());

    if (this->resource) {
        constexpr uint8_t key[64] = {};
        constexpr uint8_t iv[32] = {};
        // Try to initialize the cipher in encryption mode
        if (EVP_CipherInit_ex(ctx.pointer, this->resource.pointer, nullptr, key, iv, 1) == 1) {
            return true;
        }
    }
    return false;
}
#endif // HAS_3_0_API

void cipher_type_t::check_fips_availability(const bool fips_enabled) {
#ifdef HAS_3_0_API
    const auto name = this->init->get_v3_name();
    if (name) {
#    ifdef FIPS_SUPPORT
        if (fips_enabled) {
            if (this->flags.fips_forbidden) {
                // Shortcut when the fips_forbidden flag is already set from the probe data
                return;
            }
            this->resource.reset(EVP_CIPHER_fetch(nullptr, name, "fips=yes"));
            if (!this->can_cipher_be_instantiated()) {
                this->flags.fips_forbidden = true;
                this->resource.reset(nullptr); // free the resource
            }
        }
#    else
        this->resource.reset(EVP_CIPHER_fetch(nullptr, name, ""));
        if (!this->can_cipher_be_instantiated()) {
            this->flags.algorithm_init_failed = true;
            this->resource.reset(nullptr); // free the resource
        }
#    endif // FIPS_SUPPORT and >=3.0.0
    }
#else
    if (this->init->ctor_v1) {
        // Construct from the provided function
        this->resource.reset(this->init->ctor_v1());
    }
#endif
}

void cipher_type_t::check_availability(bool fips_enabled) {
    if (this->init->ctor_v1) {
        this->resource.reset(this->init->ctor_v1()); // take ownership on the cipher
    }
#ifdef HAVE_AEAD
    switch (this->init->aead_ctrl_type) {
    case NOT_AEAD:
        this->aead = {0, 0, 0};
        break;
    case AEAD_CTRL:
#if defined(HAVE_3_0_API)
        // This is defined in a much later version of OpenSSL like 3.x, AEAD was available from 1.0.1
        this->aead = {EVP_CTRL_AEAD_SET_IVLEN, EVP_CTRL_AEAD_GET_TAG, EVP_CTRL_AEAD_SET_TAG};
#endif
        break;
    case AEAD_CTRL_GCM:
        this->aead = {EVP_CTRL_GCM_SET_IVLEN, EVP_CTRL_GCM_GET_TAG, EVP_CTRL_GCM_SET_TAG};
        break;
    case AEAD_CTRL_CCM:
        this->aead = {EVP_CTRL_CCM_SET_IVLEN, EVP_CTRL_CCM_GET_TAG, EVP_CTRL_CCM_SET_TAG};
        break;
    }
#endif // HAVE_AEAD
    // We do not know for sure that the algorithm is unavailable (normal or FIPS)
    this->check_fips_availability(fips_enabled);
}

// for FIPS we will attempt to initialize the pubkey context to verify whether the
// algorithm is allowed, for non-FIPS keeping the old behavior - always allow the algorithm.
void cipher_probe_t::probe(ErlNifEnv *env, const bool fips_enabled, std::vector<cipher_type_t> &output) {
    this->atom = create_or_existing_atom(env, this->str, this->atom);
    output.emplace_back(this, this->atom, this->key_len, this->flags); // construct in place
    auto &algo = output.back();
    algo.check_availability(fips_enabled);
}

extern "C" const cipher_type_C *get_cipher_type(ErlNifEnv *env, ERL_NIF_TERM type, size_t key_len) {
    cipher_type_t sample(type, key_len);
    auto result = std::lower_bound(cipher_collection.cbegin(env, FIPS_MODE()),
                                   cipher_collection.cend(),
                                   sample,
                                   cipher_type_t::compare_function);
    if (result != cipher_collection.cend() && result->eq(sample)) {
        return &*result;
    }
    return nullptr;
}

extern "C" const cipher_type_C *get_cipher_type_no_key(ErlNifEnv *env, ERL_NIF_TERM type) {
    cipher_type_t sample(type);
    auto result = std::lower_bound(cipher_collection.cbegin(env, FIPS_MODE()),
                                   cipher_collection.cend(),
                                   sample,
                                   cipher_type_t::compare_function_no_key);
    if (result != cipher_collection.cend() && result->eq_no_key(sample)) {
        return &*result;
    }
    return nullptr;
}

extern "C" cipher_type_flags_t get_cipher_type_flags(const cipher_type_C *p) {
    return p->flags;
}
extern "C" cipher_type_aead_t get_cipher_type_aead(const cipher_type_C *p) {
    return p->aead;
}
extern "C" bool is_cipher_forbidden_in_fips(const cipher_type_C *p) {
    return p->is_forbidden_in_fips();
}
extern "C" const EVP_CIPHER *get_cipher_type_resource(const cipher_type_C *p) {
    return p->resource.pointer;
}
extern "C" const char *get_cipher_type_str_v3(const cipher_type_C *p) {
    return p->init->get_v3_name();
}
