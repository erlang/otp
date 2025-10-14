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
    cipher_probe_t("rc2_cbc", "rc2-cbc", CIPHER_RC2_CTOR).set_fips_forbidden(),
#undef CIPHER_RC2_CTOR
// --------------------------
#ifdef HAVE_RC4
#    define CIPHER_RC4_CTOR &EVP_rc4
#else
#    define CIPHER_RC4_CTOR nullptr
#endif
    cipher_probe_t("rc4", nullptr, CIPHER_RC4_CTOR).set_fips_forbidden(),
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
    cipher_probe_t("des_cbc", "des-cbc", CIPHER_DES_CBC_CTOR).set_fips_forbidden(),
    cipher_probe_t("des_cfb", "des-cfb", CIPHER_DES_CFB8_CTOR).set_fips_forbidden(),
    cipher_probe_t("des_ecb", "des-ecb", CIPHER_DES_ECB_CTOR).set_fips_forbidden(),
#undef CIPHER_DES_CBC_CTOR
#undef CIPHER_DES_CFB8_CTOR
#undef CIPHER_DES_ECB_CTOR
// --------------------------
#ifdef HAVE_DES_ede3_cbc
#    define CIPHER_DES_EDE3_CBC_CTOR &EVP_des_ede3_cbc
#else
#    define CIPHER_DES_EDE3_CBC_CTOR nullptr
#endif
    cipher_probe_t("des_ede3_cbc", "des-ede3-cbc", CIPHER_DES_EDE3_CBC_CTOR),
#undef CIPHER_DES_EDE3_CBC_CTOR
// --------------------------
#ifdef HAVE_DES_ede3_cfb
#    define CIPHER_DES_EDE3_CFB_CTOR &EVP_des_ede3_cfb8
#else
#    define CIPHER_DES_EDE3_CFB_CTOR nullptr
#endif
    cipher_probe_t("des_ede3_cfb", "des-ede3-cfb", CIPHER_DES_EDE3_CFB_CTOR),
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
    cipher_probe_t("blowfish_cbc", "BF-CBC", CIPHER_BF_CBC_CTOR).set_fips_forbidden(),
    cipher_probe_t("blowfish_cfb64", "BF-CFB", CIPHER_BF_CFB64_CTOR).set_fips_forbidden(),
    cipher_probe_t("blowfish_ofb64", "BF-OFB", CIPHER_BF_OFB_CTOR).set_fips_forbidden(),
    cipher_probe_t("blowfish_ecb", "BF-ECB", CIPHER_BF_ECB_CTOR).set_fips_forbidden(),
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
    cipher_probe_t("sm4_cbc", "sm4-cbc", CIPHER_SM4_CBC_CTOR).set_keylen(16).set_fips_forbidden(),
    cipher_probe_t("sm4_ecb", "sm4-ecb", CIPHER_SM4_ECB_CTOR).set_keylen(16).set_fips_forbidden(),
    cipher_probe_t("sm4_cfb", "sm4-cfb", CIPHER_SM4_CFB_CTOR).set_keylen(16).set_fips_forbidden(),
    cipher_probe_t("sm4_ofb", "sm4-ofb", CIPHER_SM4_OFB_CTOR).set_keylen(16).set_fips_forbidden(),
    cipher_probe_t("sm4_ctr", "sm4-ctr", CIPHER_SM4_CTR_CTOR).set_keylen(16).set_fips_forbidden(),
#undef CIPHER_SM4_CBC_CTOR
#undef CIPHER_SM4_ECB_CTOR
#undef CIPHER_SM4_CFB_CTOR
#undef CIPHER_SM4_OFB_CTOR
#undef CIPHER_SM4_CTR_CTOR
// --------------------------
    cipher_probe_t("aes_128_cbc", "aes-128-cbc", &EVP_aes_128_cbc).set_keylen(16),
    cipher_probe_t("aes_192_cbc", "aes-192-cbc", &EVP_aes_192_cbc).set_keylen(24),
    cipher_probe_t("aes_256_cbc", "aes-256-cbc", &EVP_aes_256_cbc).set_keylen(32),

    cipher_probe_t("aes_128_ofb", "aes-128-ofb", &EVP_aes_128_ofb).set_keylen(16),
    cipher_probe_t("aes_192_ofb", "aes-192-ofb", &EVP_aes_192_ofb).set_keylen(24),
    cipher_probe_t("aes_256_ofb", "aes-256-ofb", &EVP_aes_256_ofb).set_keylen(32),

    cipher_probe_t("aes_128_cfb8", "aes-128-cfb8", &EVP_aes_128_cfb8).set_keylen(16).set_aes_cfbx(),
    cipher_probe_t("aes_192_cfb8", "aes-192-cfb8", &EVP_aes_192_cfb8).set_keylen(24).set_aes_cfbx(),
    cipher_probe_t("aes_256_cfb8", "aes-256-cfb8", &EVP_aes_256_cfb8).set_keylen(32).set_aes_cfbx(),

    cipher_probe_t("aes_128_cfb128", "aes-128-cfb", &EVP_aes_128_cfb128).set_keylen(16).set_aes_cfbx(),
    cipher_probe_t("aes_192_cfb128", "aes-192-cfb", &EVP_aes_192_cfb128).set_keylen(24).set_aes_cfbx(),
    cipher_probe_t("aes_256_cfb128", "aes-256-cfb", &EVP_aes_256_cfb128).set_keylen(32).set_aes_cfbx(),

    cipher_probe_t("aes_128_ecb", "aes-128-ecb", &EVP_aes_128_ecb).set_keylen(16),
    cipher_probe_t("aes_192_ecb", "aes-192-ecb", &EVP_aes_192_ecb).set_keylen(24),
    cipher_probe_t("aes_256_ecb", "aes-256-ecb", &EVP_aes_256_ecb).set_keylen(32),
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
     cipher_probe_t("aes_128_ctr", "aes-128-ctr", CIPHER_AES128_CTR_CTOR).set_keylen(16),
     cipher_probe_t("aes_192_ctr", "aes-192-ctr", CIPHER_AES192_CTR_CTOR).set_keylen(24),
     cipher_probe_t("aes_256_ctr", "aes-256-ctr", CIPHER_AES256_CTR_CTOR).set_keylen(32),
#undef CIPHER_AES128_CTR_CTOR
#undef CIPHER_AES192_CTR_CTOR
#undef CIPHER_AES256_CTR_CTOR
// --------------------------
#if defined(HAVE_CHACHA20)
#    define CIPHER_CHACHA20_CTOR &EVP_chacha20
#else
#    define CIPHER_CHACHA20_CTOR nullptr
#endif
     cipher_probe_t("chacha20", nullptr, CIPHER_CHACHA20_CTOR).set_keylen(32).set_fips_forbidden(),
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
    cipher_probe_t(
        "chacha20_poly1305", "chacha20-poly1305", CIPHER_CHACHA20_POLY1305_CTOR
    ).set_aead(CIPHER_CHACHA20_POLY1305_AEADCTRL).set_fips_forbidden(),
#endif // HAVE_AEAD
#undef CIPHER_CHACHA20_POLY1305_CTOR
#undef CIPHER_CHACHA20_POLY1305_AEADCTRL
// --------------------------
#if defined(HAVE_SM4_GCM)
    cipher_probe_t("sm4_gcm", "sm4-gcm").set_keylen(16).set_fips_forbidden().set_aead(
        AEAD_CTRL, /* ccm_mode= */ false, /* gcm_mode= */ true),
#endif
// --------------------------
#if defined(HAVE_SM4_CCM)
    cipher_probe_t("sm4_ccm", "sm4-ccm").set_keylen(16).set_fips_forbidden().set_aead(AEAD_CTRL, /* ccm_mode= */ true),
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
    cipher_probe_t(
        "aes_128_gcm", "aes-128-gcm", CIPHER_AES128_GCM_CTOR
    ).set_keylen(16).set_aead(CIPHER_AES_GCM_AEADCTRL, /* ccm_mode */ false, /* gcm_mode */ true),
    cipher_probe_t(
        "aes_192_gcm", "aes-192-gcm", CIPHER_AES192_GCM_CTOR
    ).set_keylen(24).set_aead(CIPHER_AES_GCM_AEADCTRL, /* ccm_mode */ false, /* gcm_mode */ true),
    cipher_probe_t(
        "aes_256_gcm", "aes-256-gcm", CIPHER_AES256_GCM_CTOR
    ).set_keylen(32).set_aead(CIPHER_AES_GCM_AEADCTRL, /* ccm_mode */ false, /* gcm_mode */ true),
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
    cipher_probe_t(
        "aes_128_ccm", "aes-128-ccm", CIPHER_AES128_CCM_CTOR
    ).set_keylen(16).set_aead(CIPHER_AES_CCM_AEADCTRL, /* ccm_mode */ true),
    cipher_probe_t(
        "aes_192_ccm", "aes-192-ccm", CIPHER_AES192_CCM_CTOR
    ).set_keylen(24).set_aead(CIPHER_AES_CCM_AEADCTRL, /* ccm_mode */ true),
    cipher_probe_t(
        "aes_256_ccm", "aes-256-ccm", CIPHER_AES256_CCM_CTOR
    ).set_keylen(32).set_aead(CIPHER_AES_CCM_AEADCTRL, /* ccm_mode */ true),
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
    const bool fips_enabled = FIPS_MODE();
    auto result = std::lower_bound(cipher_collection.cbegin(env, fips_enabled),
                                   cipher_collection.cend(fips_enabled),
                                   sample,
                                   cipher_type_t::compare_function);
    if (result != cipher_collection.cend(fips_enabled) && result->eq(sample)) {
        return &*result;
    }
    return nullptr;
}

extern "C" const cipher_type_C *get_cipher_type_no_key(ErlNifEnv *env, ERL_NIF_TERM type) {
    cipher_type_t sample(type);
    const bool fips_enabled = FIPS_MODE();
    auto result = std::lower_bound(cipher_collection.cbegin(env, fips_enabled),
                                   cipher_collection.cend(fips_enabled),
                                   sample,
                                   cipher_type_t::compare_function_no_key);
    if (result != cipher_collection.cend(fips_enabled) && result->eq_no_key(sample)) {
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
