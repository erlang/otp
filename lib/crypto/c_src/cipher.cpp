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

#include "cipher.h"
#include "../../../erts/emulator/test/nif_SUITE_data/nif_api_2_4/erl_nif.h"
#include "evp.h"
#include "info.h"

static cipher_type_t cipher_types[] = {
#ifdef HAVE_RC2
        cipher_type_t("rc2_cbc", "rc2-cbc", &EVP_rc2_cbc).no_fips().no_aead(),
#else
        cipher_type_t("rc2_cbc", "rc2-cbc", nullptr).no_fips().no_aead(),
#endif

#ifdef HAVE_RC4
        cipher_type_t("rc4", "rc4", &EVP_rc4).no_fips().no_aead(),
#else
        cipher_type_t("rc4", "rc4", nullptr).no_fips().no_aead(),
#endif

#ifdef HAVE_DES
        cipher_type_t("des_cbc", "des-cbc", &EVP_des_cbc).no_fips(),
        cipher_type_t("des_cfb", "des-cfb", &EVP_des_cfb8).no_fips(),
        cipher_type_t("des_ecb", "des-ecb", &EVP_des_ecb).no_fips(),
#else
        cipher_type_t("des_cbc", "des-cbc", nullptr),
        cipher_type_t("des_cfb", "des-cfb", nullptr),
        cipher_type_t("des_ecb", "des-ecb", nullptr),
#endif

#ifdef HAVE_DES_ede3_cbc
        cipher_type_t("des_ede3_cbc", "des-ede3-cbc", &EVP_des_ede3_cbc ),
#else
        cipher_type_t("des_ede3_cbc", "des-ede3-cbc", nullptr),
#endif

#ifdef HAVE_DES_ede3_cfb
        cipher_type_t("des_ede3_cfb", "des-ede3-cfb", &EVP_des_ede3_cfb8),
#else
        cipher_type_t("des_ede3_cfb", "des-ede3-cfb", nullptr).no_aead(),
#endif

#ifdef HAVE_BF
        cipher_type_t("blowfish_cbc", "BF-CBC", &EVP_bf_cbc).no_fips().no_aead(),
        cipher_type_t("blowfish_cfb64", "BF-CFB", &EVP_bf_cfb64).no_fips().no_aead(),
        cipher_type_t("blowfish_ofb64", "BF-OFB", &EVP_bf_ofb).no_fips().no_aead(),
        cipher_type_t("blowfish_ecb", "BF-ECB", &EVP_bf_ecb).no_fips().no_aead(),
#else
        cipher_type_t("blowfish_cbc", "BF-CBC", nullptr).no_aead(),
        cipher_type_t("blowfish_cfb64", "BF-CFB", nullptr).no_aead(),
        cipher_type_t("blowfish_ofb64", "BF-OFB", nullptr).no_aead(),
        cipher_type_t("blowfish_ecb", "BF-ECB", nullptr).no_aead(),
#endif

#ifdef HAVE_SM4
        cipher_type_t("sm4_cbc", "sm4-cbc", &EVP_sm4_cbc).set_key_len(16).no_fips().no_aead(),
        cipher_type_t("sm4_ecb", "sm4-ecb", &EVP_sm4_ecb).set_key_len(16).no_fips().no_aead(),
        cipher_type_t("sm4_cfb", "sm4-cfb", &EVP_sm4_cfb).set_key_len(16).no_fips().no_aead(),
        cipher_type_t("sm4_ofb", "sm4-ofb", &EVP_sm4_ofb).set_key_len(16).no_fips().no_aead(),
        cipher_type_t("sm4_ctr", "sm4-ctr", &EVP_sm4_ctr).set_key_len(16).no_fips().no_aead(),
#else
        cipher_type_t("sm4_cbc", "sm4-cbc", nullptr).set_key_len(16).no_fips().no_aead(),
        cipher_type_t("sm4_ecb", "sm4-ecb", nullptr).set_key_len(16).no_fips().no_aead(),
        cipher_type_t("sm4_cfb", "sm4-cfb", nullptr).set_key_len(16).no_fips().no_aead(),
        cipher_type_t("sm4_ofb", "sm4-ofb", nullptr).set_key_len(16).no_fips().no_aead(),
        cipher_type_t("sm4_ctr", "sm4-ctr", nullptr).set_key_len(16).no_fips().no_aead(),
#endif

        cipher_type_t("aes_128_cbc", "aes-128-cbc", &EVP_aes_128_cbc).set_key_len(16).no_aead(),
        cipher_type_t("aes_192_cbc", "aes-192-cbc", &EVP_aes_192_cbc).set_key_len(24).no_aead(),
        cipher_type_t("aes_256_cbc", "aes-256-cbc", &EVP_aes_256_cbc).set_key_len(32).no_aead(),

        cipher_type_t("aes_128_ofb", "aes-128-ofb", &EVP_aes_128_ofb).set_key_len(16).no_aead(),
        cipher_type_t("aes_192_ofb", "aes-192-ofb", &EVP_aes_192_ofb).set_key_len(24).no_aead(),
        cipher_type_t("aes_256_ofb", "aes-256-ofb", &EVP_aes_256_ofb).set_key_len(32).no_aead(),

        cipher_type_t("aes_128_cfb8", "aes-128-cfb8", &EVP_aes_128_cfb8).set_key_len(16).set_aes_cfb().no_aead(),
        cipher_type_t("aes_192_cfb8", "aes-192-cfb8", &EVP_aes_192_cfb8).set_key_len(24).set_aes_cfb().no_aead(),
        cipher_type_t("aes_256_cfb8", "aes-256-cfb8", &EVP_aes_256_cfb8).set_key_len(32).set_aes_cfb().no_aead(),

        cipher_type_t("aes_128_cfb128", "aes-128-cfb", &EVP_aes_128_cfb128).set_key_len(16).set_aes_cfb().no_aead(),
        cipher_type_t("aes_192_cfb128", "aes-192-cfb", &EVP_aes_192_cfb128).set_key_len(24).set_aes_cfb().no_aead(),
        cipher_type_t("aes_256_cfb128", "aes-256-cfb", &EVP_aes_256_cfb128).set_key_len(32).set_aes_cfb().no_aead(),

        cipher_type_t("aes_128_ecb", "aes-128-ecb", &EVP_aes_128_ecb).set_key_len(16).no_aead(),
        cipher_type_t("aes_192_ecb", "aes-192-ecb", &EVP_aes_192_ecb).set_key_len(24).no_aead(),
        cipher_type_t("aes_256_ecb", "aes-256-ecb", &EVP_aes_256_ecb).set_key_len(32).no_aead(),

#if defined(HAVE_EVP_AES_CTR)
        cipher_type_t("aes_128_ctr", "aes-128-ctr", &EVP_aes_128_ctr).set_key_len(16).no_aead(),
        cipher_type_t("aes_192_ctr", "aes-192-ctr", &EVP_aes_192_ctr).set_key_len(24).no_aead(),
        cipher_type_t("aes_256_ctr", "aes-256-ctr", &EVP_aes_256_ctr).set_key_len(32).no_aead(),
#else
        cipher_type_t("aes_128_ctr", "aes-128-ctr", nullptr).set_key_len(16).set_aes_ctr_compat().no_aead(),
        cipher_type_t("aes_192_ctr", "aes-192-ctr", nullptr).set_key_len(24).set_aes_ctr_compat().no_aead(),
        cipher_type_t("aes_256_ctr", "aes-256-ctr", nullptr).set_key_len(32).set_aes_ctr_compat().no_aead(),
#endif

#if defined(HAVE_CHACHA20)
        cipher_type_t("chacha20", "chacha20", &EVP_chacha20).set_key_len(32).no_fips().no_aead(),
#else
        cipher_type_t("chacha20", "chacha20", nullptr).set_key_len(32).no_fips().no_aead(),
#endif

    /*==== AEAD ciphers ====*/
#if defined(HAVE_CHACHA20_POLY1305)
        cipher_type_t("chacha20_poly1305", "chacha20-poly1305", &EVP_chacha20_poly1305)
                .no_fips()
                .set_aead(ERL_CRYPTO_AEAD_CTRL),
#else
        cipher_type_t("chacha20_poly1305", "chacha20-poly1305", nullptr).no_fips().set_aead(ERL_CRYPTO_AEAD_ZEROES),
#endif

#if defined(HAVE_SM4_GCM)
        cipher_type_t("sm4_gcm", "sm4-gcm", nullptr)
                .set_key_len(16)
                .no_fips()
                .set_aead(ERL_CRYPTO_AEAD_CTRL)
                .set_gcm_mode(),
#endif

#if defined(HAVE_SM4_CCM)
        cipher_type_t("sm4_ccm", "sm4-ccm", nullptr)
                .set_key_len(16)
                .no_fips()
                .set_aead(ERL_CRYPTO_AEAD_CTRL)
                .set_ccm_mode(),
#endif

#if defined(HAVE_GCM) && defined(HAS_3_0_API)
        cipher_type_t("aes_128_gcm", "aes-128-gcm", &EVP_aes_128_gcm)
                .set_key_len(16)
                .set_aead(ERL_CRYPTO_AEAD_CTRL)
                .set_gcm_mode(),
        cipher_type_t("aes_192_gcm", "aes-192-gcm", &EVP_aes_192_gcm)
                .set_key_len(24)
                .set_aead(ERL_CRYPTO_AEAD_CTRL)
                .set_gcm_mode(),
        cipher_type_t("aes_256_gcm", "aes-256-gcm", &EVP_aes_256_gcm)
                .set_key_len(32)
                .set_aead(ERL_CRYPTO_AEAD_CTRL)
                .set_gcm_mode(),
#elif defined(HAVE_GCM)
        cipher_type_t("aes_128_gcm", "aes-128-gcm", &EVP_aes_128_gcm)
                .set_key_len(16)
                .set_aead(ERL_CRYPTO_GCM_CTRL)
                .set_gcm_mode(),
        cipher_type_t("aes_192_gcm", "aes-192-gcm", &EVP_aes_192_gcm)
                .set_key_len(24)
                .set_aead(ERL_CRYPTO_GCM_CTRL)
                .set_gcm_mode(),
        cipher_type_t("aes_256_gcm", "aes-256-gcm", &EVP_aes_256_gcm)
                .set_key_len(32)
                .set_aead(ERL_CRYPTO_GCM_CTRL)
                .set_gcm_mode(),
#else
        cipher_type_t("aes_128_gcm", "aes-128-gcm", nullptr)
                .set_key_len(16)
                .set_aead(ERL_CRYPTO_AEAD_ZEROES)
                .set_gcm_mode(),
        cipher_type_t("aes_192_gcm", "aes-192-gcm", nullptr)
                .set_key_len(24)
                .set_aead(ERL_CRYPTO_AEAD_ZEROES)
                .set_gcm_mode(),
        cipher_type_t("aes_256_gcm", "aes-256-gcm", nullptr)
                .set_key_len(32)
                .set_aead(ERL_CRYPTO_AEAD_ZEROES)
                .set_gcm_mode(),
#endif

#if defined(HAVE_CCM) && defined(HAS_3_0_API)
        cipher_type_t("aes_128_ccm", "aes-128-ccm", &EVP_aes_128_ccm)
                .set_key_len(16)
                .set_aead(ERL_CRYPTO_AEAD_CTRL)
                .set_ccm_mode(),
        cipher_type_t("aes_192_ccm", "aes-192-ccm", &EVP_aes_192_ccm)
                .set_key_len(24)
                .set_aead(ERL_CRYPTO_AEAD_CTRL)
                .set_ccm_mode(),
        cipher_type_t("aes_256_ccm", "aes-256-ccm", &EVP_aes_256_ccm)
                .set_key_len(32)
                .set_aead(ERL_CRYPTO_AEAD_CTRL)
                .set_ccm_mode(),
#elif defined(HAVE_CCM)
        cipher_type_t("aes_128_ccm", "aes-128-ccm", &EVP_aes_128_ccm)
                .set_key_len(16)
                .set_aead(ERL_CRYPTO_CCM_CTRL)
                .set_ccm_mode(),
        cipher_type_t("aes_192_ccm", "aes-192-ccm", &EVP_aes_192_ccm)
                .set_key_len(24)
                .set_aead(ERL_CRYPTO_CCM_CTRL)
                .set_ccm_mode(),
        cipher_type_t("aes_256_ccm", "aes-256-ccm", &EVP_aes_256_ccm)
                .set_key_len(32)
                .set_aead(ERL_CRYPTO_CCM_CTRL)
                .set_ccm_mode(),
#else
        cipher_type_t("aes_128_ccm", "aes-128-ccm", nullptr)
                .set_key_len(16)
                .set_aead(ERL_CRYPTO_AEAD_ZEROES)
                .set_ccm_mode(),
        cipher_type_t("aes_192_ccm", "aes-192-ccm", nullptr)
                .set_key_len(24)
                .set_aead(ERL_CRYPTO_AEAD_ZEROES)
                .set_ccm_mode(),
        cipher_type_t("aes_256_ccm", "aes-256-ccm", nullptr)
                .set_key_len(32)
                .set_aead(ERL_CRYPTO_AEAD_ZEROES)
                .set_ccm_mode(),
#endif

    /*==== End of list ==== */
};
static const size_t CIPHERS_ARRAY_SIZE = sizeof(cipher_types) / sizeof(cipher_types[0]);

ErlNifResourceType* evp_cipher_ctx_rtype;

static size_t num_cipher_types = 0;

static void evp_cipher_ctx_dtor(ErlNifEnv* env, evp_cipher_ctx * ctx) {
    if (ctx == nullptr)
        return;

    if (ctx->ctx)
        EVP_CIPHER_CTX_free(ctx->ctx);

#if !defined(HAVE_EVP_AES_CTR)
    if (ctx->env)
        enif_free_env(ctx->env);
#endif
}

int init_cipher_ctx(ErlNifEnv *env, ErlNifBinary* rt_buf) {
    evp_cipher_ctx_rtype = enif_open_resource_type(env, nullptr,
                                                   resource_name("EVP_CIPHER_CTX", rt_buf),
                                                   reinterpret_cast<ErlNifResourceDtor *>(evp_cipher_ctx_dtor),
                                                   static_cast<ErlNifResourceFlags>(ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER), nullptr);
    if (evp_cipher_ctx_rtype == nullptr)
        goto err;

    return 1;

 err:
    PRINTF_ERR0("CRYPTO: Could not open resource type 'EVP_CIPHER_CTX'");
    return 0;
}

void init_cipher_types(ErlNifEnv* env)
{
    for (auto i = 0; i < CIPHERS_ARRAY_SIZE; i++) {
        auto p = &cipher_types[i];
        p->atom = enif_make_atom(env, p->str);
#ifdef HAS_3_0_API
        if (cipher_types[i].str_v3) {
            p->p = EVP_CIPHER_fetch(nullptr, p->str_v3, "");
# ifdef FIPS_SUPPORT
            /* Try if valid in FIPS */
            {
                EVP_CIPHER *tmp = EVP_CIPHER_fetch(nullptr, p->str_v3, "fips=yes");

                if (tmp) {
                    EVP_CIPHER_free(tmp);
                    p->flags.no_fips = false;
                } else {
                    p->flags.no_fips = true;
                }
            }
# endif /* FIPS_SUPPORT and >=3.0.0 */
        }
#else
        if (p->cipher.funcp)
            p->cipher.p = p->cipher.funcp();
#endif
    }

    qsort(cipher_types, CIPHERS_ARRAY_SIZE, sizeof(cipher_type_t), cmp_cipher_types);
}

const cipher_type_t * get_cipher_type(ERL_NIF_TERM type, const size_t key_len)
{
    cipher_type_t key("", "", nullptr);
    key.atom = type;
    key.key_len = key_len;

    return static_cast<cipher_type_t *>(
            bsearch(&key, cipher_types, num_cipher_types, sizeof(cipher_types[0]), cmp_cipher_types));
}


int cmp_cipher_types(const void *keyp, const void *elemp) {
    const auto key  = static_cast<const cipher_type_t*>(keyp);
    const auto elem = static_cast<const cipher_type_t*>(elemp);

    if (key->atom < elem->atom) return -1;
    else if (key->atom > elem->atom) return 1;
    else /* key->atom == elem->atom */
        if (!elem->key_len || key->key_len == elem->key_len) return 0;
        else if (key->key_len < elem->key_len) return -1;
        else return 1;
}


ERL_NIF_TERM cipher_info_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type) */
    const cipher_type_t *cipherp;
    const EVP_CIPHER     *cipher;
    ERL_NIF_TERM         ret, ret_mode;
    ERL_NIF_TERM keys[6];
    ERL_NIF_TERM vals[6];

    if ((cipherp = get_cipher_type_no_key(argv[0])) == nullptr)
        return enif_make_badarg(env);

    if (CIPHER_FORBIDDEN_IN_FIPS(cipherp))
        return enif_raise_exception(env, atom_notsup);
    if ((cipher = cipherp->p) == nullptr)
        return enif_raise_exception(env, atom_notsup);

    const unsigned type = EVP_CIPHER_type(cipher);

    keys[0] = atom_type;
    vals[0] = type == NID_undef ? atom_undefined : enif_make_int(env, type);
    keys[1] = atom_key_length;
    vals[1] = enif_make_int(env, EVP_CIPHER_key_length(cipher));
    keys[2] = atom_iv_length;
    vals[2] = enif_make_int(env, EVP_CIPHER_iv_length(cipher));
    keys[3] = atom_block_size;
    vals[3] = enif_make_int(env, EVP_CIPHER_block_size(cipher));
    keys[4] = atom_prop_aead;
#if defined(HAVE_AEAD)
    vals[4] = (EVP_CIPHER_flags(cipher) & EVP_CIPH_FLAG_AEAD_CIPHER) != 0 ? atom_true : atom_false;
#else
    vals[4] = atom_false;
#endif

    const auto mode = EVP_CIPHER_mode(cipher);
    switch (mode) {
        case EVP_CIPH_ECB_MODE:
            ret_mode = atom_ecb_mode;
            break;

        case EVP_CIPH_CBC_MODE:
            ret_mode = atom_cbc_mode;
            break;

        case EVP_CIPH_CFB_MODE:
            ret_mode = atom_cfb_mode;
            break;

        case EVP_CIPH_OFB_MODE:
            ret_mode = atom_ofb_mode;
            break;

#ifdef EVP_CIPH_CTR_MODE
        case EVP_CIPH_CTR_MODE:
            ret_mode = atom_ctr_mode;
            break;
#endif

#ifdef EVP_CIPH_GCM_MODE
        case EVP_CIPH_GCM_MODE:
            ret_mode = atom_gcm_mode;
            break;
#endif

#ifdef EVP_CIPH_CCM_MODE
        case EVP_CIPH_CCM_MODE:
            ret_mode = atom_ccm_mode;
            break;
#endif

#ifdef EVP_CIPH_XTS_MODE
        case EVP_CIPH_XTS_MODE:
            ret_mode = atom_xts_mode;
            break;
#endif

#ifdef EVP_CIPH_WRAP_MODE
        case EVP_CIPH_WRAP_MODE:
            ret_mode = atom_wrap_mode;
            break;
#endif

#ifdef EVP_CIPH_OCB_MODE
        case EVP_CIPH_OCB_MODE:
            ret_mode = atom_ocb_mode;
            break;
#endif

        case EVP_CIPH_STREAM_CIPHER:
            ret_mode = atom_stream_cipher;
            break;

        default:
            ret_mode = atom_undefined;
            break;
    }
    keys[5] = atom_mode;
    vals[5] = ret_mode;

    auto ok = enif_make_map_from_arrays(env, keys, vals, 6, &ret);
    ASSERT(ok); (void)ok;

    return ret;
}

const cipher_type_t * get_cipher_type_no_key(ERL_NIF_TERM type)
{
    cipher_type_t key("", "", nullptr);
    key.atom = type;

    return static_cast<cipher_type_t *>(
            bsearch(&key, cipher_types, CIPHERS_ARRAY_SIZE, sizeof(cipher_type_t), cmp_cipher_types_no_key));
}

int cmp_cipher_types_no_key(const void *keyp, const void *elemp) {
    const auto key  = static_cast<const cipher_type_t *>(keyp);
    const auto elem = static_cast<const cipher_type_t *>(elemp);
    int ret;

    if (key->atom < elem->atom) ret = -1;
    else if (key->atom > elem->atom) ret = 1;
    else /* key->atom == elem->atom */ ret = 0;

    return ret;
}


ERL_NIF_TERM cipher_types_as_list(ErlNifEnv* env)
{
    ERL_NIF_TERM prev, hd;

    hd = enif_make_list(env, 0);
    prev = atom_undefined;

    for (auto i = 0; i < CIPHERS_ARRAY_SIZE; i++) {
        auto p = &cipher_types[i];
        if (prev == p->atom || CIPHER_FORBIDDEN_IN_FIPS(p) )
            continue;

        if (p->p != nullptr || p->flags.aes_ctr_compat) {
            hd = enif_make_list_cell(env, p->atom, hd);
        }
    }

    return hd;
}
