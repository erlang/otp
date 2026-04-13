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

using cipher_create_fn_t = const EVP_CIPHER *(*)();

/* masks in the flags field if cipher_type_t */
struct cipher_flags_t {
    bool no_fips : 1;
    bool aes_cfb : 1;
    // bool ecb_bug_0_9_8l : 1; // OpenSSL 0.x not supported anymore and not tested for
    bool aead : 1;
    bool non_evp : 1;
    bool aes_ctr_compat : 1;
    bool ccm_mode : 1;
    bool gcm_mode : 1;
};

#define ERL_CRYPTO_AEAD_ZEROES {0, 0, 0}
#define ERL_CRYPTO_AEAD_CTRL {EVP_CTRL_AEAD_SET_IVLEN, EVP_CTRL_AEAD_GET_TAG, EVP_CTRL_AEAD_SET_TAG}
#define ERL_CRYPTO_GCM_CTRL {EVP_CTRL_GCM_SET_IVLEN, EVP_CTRL_GCM_GET_TAG, EVP_CTRL_GCM_SET_TAG}
#define ERL_CRYPTO_CCM_CTRL {EVP_CTRL_CCM_SET_IVLEN, EVP_CTRL_CCM_GET_TAG, EVP_CTRL_CCM_SET_TAG}

struct cipher_type_t {
    const char *str;
    ERL_NIF_TERM atom = ERL_CRYPTO_BAD_ATOM_VALUE; /* updated after init */
    const char *str_v3; /* the algorithm name as in OpenSSL 3.x */
    const cipher_create_fn_t funcp; /* NULL if notsup */
    const EVP_CIPHER *p = nullptr; /* updated after init, NULL if notsup */
    size_t key_len = 0; /* != 0 to also match on key_len */
    cipher_flags_t flags = {};
    struct aead_ctrl_t {
        int ctx_ctrl_set_ivlen = 0;
        int ctx_ctrl_get_tag = 0;
        int ctx_ctrl_set_tag = 0;
    } aead;

    constexpr cipher_type_t(const char *str_v1, const char *str_v3, const cipher_create_fn_t create_fn) :
        str(str_v1), str_v3(str_v3), funcp(create_fn) {}

    constexpr cipher_type_t &no_fips() {
        flags.no_fips = true;
        return *this;
    }
    constexpr cipher_type_t &set_aes_cfb() {
        flags.aes_cfb = true;
        return *this;
    }
    constexpr cipher_type_t &set_aes_ctr_compat() {
        flags.aes_ctr_compat = true;
        return *this;
    }
    constexpr cipher_type_t &set_gcm_mode() {
        flags.gcm_mode = true;
        return *this;
    }
    constexpr cipher_type_t &set_ccm_mode() {
        flags.ccm_mode = true;
        return *this;
    }
    constexpr cipher_type_t &set_key_len(const size_t k) {
        key_len = k;
        return *this;
    }
    constexpr cipher_type_t &set_aead(const aead_ctrl_t new_value) {
        flags.aead = true;
        aead = new_value;
        return *this;
    }
    constexpr cipher_type_t &no_aead() {
        flags.aead = false;
        aead = ERL_CRYPTO_AEAD_ZEROES;
        return *this;
    }
};

static INLINE bool CIPHER_FORBIDDEN_IN_FIPS(const cipher_type_t *p) {
#ifdef FIPS_SUPPOR
    return p->flags.no_fips && FIPS_MODE();
#else
    return false;
#endif
}

extern ErlNifResourceType* evp_cipher_ctx_rtype;
struct evp_cipher_ctx {
    EVP_CIPHER_CTX* ctx;
    int iv_len;
    ERL_NIF_TERM padding; /* id of the padding to add by get_final_args() */
    ErlNifBinary key_bin;
    int padded_size;   /* Length of the padding that was added */
    int encflag; /* 1 if encrypting, 0 if decrypting */
    unsigned int size; /* The sum of all sizes of input texts to get_update_args() */
#if !defined(HAVE_EVP_AES_CTR)
    ErlNifEnv* env;
    ERL_NIF_TERM state; /* Is == atom_undefined if not handling an aes_ctr crypto */
#endif
};

ERL_NIF_TERM cipher_info_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

int init_cipher_ctx(ErlNifEnv *env, ErlNifBinary* rt_buf);

void init_cipher_types(ErlNifEnv* env);
const cipher_type_t * get_cipher_type_no_key(ERL_NIF_TERM type);
const cipher_type_t * get_cipher_type(ERL_NIF_TERM type, size_t key_len);

int cmp_cipher_types(const void *keyp, const void *elemp);
int cmp_cipher_types_no_key(const void *keyp, const void *elemp);

ERL_NIF_TERM cipher_types_as_list(ErlNifEnv* env);
