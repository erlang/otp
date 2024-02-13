/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2024. All Rights Reserved.
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
#include "info.h"

#define NOT_AEAD {{0,0,0}}
#define AEAD_CTRL {{EVP_CTRL_AEAD_SET_IVLEN,EVP_CTRL_AEAD_GET_TAG,EVP_CTRL_AEAD_SET_TAG}}

static struct cipher_type_t cipher_types[] =
{
#ifdef HAVE_RC2
    {{"rc2_cbc"}, "rc2-cbc", {&EVP_rc2_cbc}, 0, NO_FIPS_CIPHER, NOT_AEAD},
#else
    {{"rc2_cbc"}, "rc2-cbc", {NULL}, 0, NO_FIPS_CIPHER, NOT_AEAD},
#endif

#ifdef HAVE_RC4
    {{"rc4"}, "rc4", {&EVP_rc4}, 0, NO_FIPS_CIPHER, NOT_AEAD},
#else
    {{"rc4"}, "rc4", {NULL}, 0, NO_FIPS_CIPHER, NOT_AEAD},
#endif

#ifdef HAVE_DES
    {{"des_cbc"}, "des-cbc", {&EVP_des_cbc},  0, NO_FIPS_CIPHER},
    {{"des_cfb"}, "des-cfb", {&EVP_des_cfb8}, 0, NO_FIPS_CIPHER},
    {{"des_ecb"}, "des-ecb", {&EVP_des_ecb},  0, NO_FIPS_CIPHER | ECB_BUG_0_9_8L},
#else
    {{"des_cbc"}, "des-cbc", {NULL}, 0, 0},
    {{"des_cfb"}, "des-cfb", {NULL}, 0, 0},
    {{"des_ecb"}, "des-ecb", {NULL}, 0, 0},
#endif

#ifdef HAVE_DES_ede3_cbc
    {{"des_ede3_cbc"}, "des-ede3-cbc", {&EVP_des_ede3_cbc}, 0, 0},
#else
    {{"des_ede3_cbc"}, "des-ede3-cbc", {NULL}, 0, 0},
#endif

#ifdef HAVE_DES_ede3_cfb
    {{"des_ede3_cfb"}, "des-ede3-cfb", {&EVP_des_ede3_cfb8}, 0, 0},
#else
    {{"des_ede3_cfb"}, "des-ede3-cfb", {NULL}, 0, 0, NOT_AEAD},
#endif

#ifdef HAVE_BF
    {{"blowfish_cbc"},   "BF-CBC", {&EVP_bf_cbc},   0, NO_FIPS_CIPHER, NOT_AEAD},
    {{"blowfish_cfb64"}, "BF-CFB", {&EVP_bf_cfb64}, 0, NO_FIPS_CIPHER, NOT_AEAD},
    {{"blowfish_ofb64"}, "BF-OFB", {&EVP_bf_ofb},   0, NO_FIPS_CIPHER, NOT_AEAD},
    {{"blowfish_ecb"},   "BF-ECB", {&EVP_bf_ecb},   0, NO_FIPS_CIPHER | ECB_BUG_0_9_8L, NOT_AEAD},
#else
    {{"blowfish_cbc"},   "BF-CBC", {NULL}, 0, 0, NOT_AEAD},
    {{"blowfish_cfb64"}, "BF-CFB", {NULL}, 0, 0, NOT_AEAD},
    {{"blowfish_ofb64"}, "BF-OFB", {NULL}, 0, 0, NOT_AEAD},
    {{"blowfish_ecb"},   "BF-ECB", {NULL}, 0, 0, NOT_AEAD},
#endif

    {{"aes_128_cbc"}, "aes-128-cbc", {&EVP_aes_128_cbc}, 16, 0, NOT_AEAD},
    {{"aes_192_cbc"}, "aes-192-cbc", {&EVP_aes_192_cbc}, 24, 0, NOT_AEAD},
    {{"aes_256_cbc"}, "aes-256-cbc", {&EVP_aes_256_cbc}, 32, 0, NOT_AEAD},

    {{"aes_128_ofb"}, "aes-128-ofb", {&EVP_aes_128_ofb}, 16, 0, NOT_AEAD},
    {{"aes_192_ofb"}, "aes-192-ofb", {&EVP_aes_192_ofb}, 24, 0, NOT_AEAD},
    {{"aes_256_ofb"}, "aes-256-ofb", {&EVP_aes_256_ofb}, 32, 0, NOT_AEAD},

    {{"aes_128_cfb8"}, "aes-128-cfb8", {&EVP_aes_128_cfb8}, 16, AES_CFBx, NOT_AEAD},
    {{"aes_192_cfb8"}, "aes-192-cfb8", {&EVP_aes_192_cfb8}, 24, AES_CFBx, NOT_AEAD},
    {{"aes_256_cfb8"}, "aes-256-cfb8", {&EVP_aes_256_cfb8}, 32, AES_CFBx, NOT_AEAD},

    {{"aes_128_cfb128"}, "aes-128-cfb", {&EVP_aes_128_cfb128}, 16, AES_CFBx, NOT_AEAD},
    {{"aes_192_cfb128"}, "aes-192-cfb", {&EVP_aes_192_cfb128}, 24, AES_CFBx, NOT_AEAD},
    {{"aes_256_cfb128"}, "aes-256-cfb", {&EVP_aes_256_cfb128}, 32, AES_CFBx, NOT_AEAD},

    {{"aes_128_ecb"}, "aes-128-ecb", {&EVP_aes_128_ecb}, 16, ECB_BUG_0_9_8L, NOT_AEAD},
    {{"aes_192_ecb"}, "aes-192-ecb", {&EVP_aes_192_ecb}, 24, ECB_BUG_0_9_8L, NOT_AEAD},
    {{"aes_256_ecb"}, "aes-256-ecb", {&EVP_aes_256_ecb}, 32, ECB_BUG_0_9_8L, NOT_AEAD},

#if defined(HAVE_EVP_AES_CTR)
    {{"aes_128_ctr"}, "aes-128-ctr", {&EVP_aes_128_ctr}, 16, 0, NOT_AEAD},
    {{"aes_192_ctr"}, "aes-192-ctr", {&EVP_aes_192_ctr}, 24, 0, NOT_AEAD},
    {{"aes_256_ctr"}, "aes-256-ctr", {&EVP_aes_256_ctr}, 32, 0, NOT_AEAD},
#else
    {{"aes_128_ctr"}, "aes-128-ctr", {NULL}, 16, AES_CTR_COMPAT, NOT_AEAD},
    {{"aes_192_ctr"}, "aes-192-ctr", {NULL}, 24, AES_CTR_COMPAT, NOT_AEAD},
    {{"aes_256_ctr"}, "aes-256-ctr", {NULL}, 32, AES_CTR_COMPAT, NOT_AEAD},
#endif

#if defined(HAVE_CHACHA20)
    {{"chacha20"}, "chacha20", {&EVP_chacha20}, 32, NO_FIPS_CIPHER, NOT_AEAD},
#else
    {{"chacha20"}, "chacha20", {NULL}, 0, NO_FIPS_CIPHER, NOT_AEAD},
#endif

    /*==== AEAD ciphers ====*/
#if defined(HAVE_CHACHA20_POLY1305)
    {{"chacha20_poly1305"}, "chacha20-poly1305", {&EVP_chacha20_poly1305}, 0, NO_FIPS_CIPHER | AEAD_CIPHER, AEAD_CTRL},
#else
    {{"chacha20_poly1305"}, "chacha20-poly1305", {NULL}, 0, NO_FIPS_CIPHER | AEAD_CIPHER, {{0,0,0}}},
#endif

#if defined(HAVE_GCM) && defined(HAS_3_0_API)
    {{"aes_128_gcm"}, "aes-128-gcm", {&EVP_aes_128_gcm}, 16, AEAD_CIPHER|GCM_MODE, AEAD_CTRL},
    {{"aes_192_gcm"}, "aes-192-gcm", {&EVP_aes_192_gcm}, 24, AEAD_CIPHER|GCM_MODE, AEAD_CTRL},
    {{"aes_256_gcm"}, "aes-256-gcm", {&EVP_aes_256_gcm}, 32, AEAD_CIPHER|GCM_MODE, AEAD_CTRL},
#elif defined(HAVE_GCM)
    {{"aes_128_gcm"}, "aes-128-gcm", {&EVP_aes_128_gcm}, 16, AEAD_CIPHER|GCM_MODE, {{EVP_CTRL_GCM_SET_IVLEN,EVP_CTRL_GCM_GET_TAG,EVP_CTRL_GCM_SET_TAG}}},
    {{"aes_192_gcm"}, "aes-192-gcm", {&EVP_aes_192_gcm}, 24, AEAD_CIPHER|GCM_MODE, {{EVP_CTRL_GCM_SET_IVLEN,EVP_CTRL_GCM_GET_TAG,EVP_CTRL_GCM_SET_TAG}}},
    {{"aes_256_gcm"}, "aes-256-gcm", {&EVP_aes_256_gcm}, 32, AEAD_CIPHER|GCM_MODE, {{EVP_CTRL_GCM_SET_IVLEN,EVP_CTRL_GCM_GET_TAG,EVP_CTRL_GCM_SET_TAG}}},
#else
    {{"aes_128_gcm"}, "aes-128-gcm", {NULL}, 16, AEAD_CIPHER|GCM_MODE, {{0,0,0}}},
    {{"aes_192_gcm"}, "aes-192-gcm", {NULL}, 24, AEAD_CIPHER|GCM_MODE, {{0,0,0}}},
    {{"aes_256_gcm"}, "aes-256-gcm", {NULL}, 32, AEAD_CIPHER|GCM_MODE, {{0,0,0}}},
#endif

#if defined(HAVE_CCM) && defined(HAS_3_0_API)
    {{"aes_128_ccm"}, "aes-128-ccm", {&EVP_aes_128_ccm}, 16, AEAD_CIPHER|CCM_MODE, AEAD_CTRL},
    {{"aes_192_ccm"}, "aes-192-ccm", {&EVP_aes_192_ccm}, 24, AEAD_CIPHER|CCM_MODE, AEAD_CTRL},
    {{"aes_256_ccm"}, "aes-256-ccm", {&EVP_aes_256_ccm}, 32, AEAD_CIPHER|CCM_MODE, AEAD_CTRL},
#elif defined(HAVE_CCM)
    {{"aes_128_ccm"}, "aes-128-ccm", {&EVP_aes_128_ccm}, 16, AEAD_CIPHER|CCM_MODE, {{EVP_CTRL_CCM_SET_IVLEN,EVP_CTRL_CCM_GET_TAG,EVP_CTRL_CCM_SET_TAG}}},
    {{"aes_192_ccm"}, "aes-192-ccm", {&EVP_aes_192_ccm}, 24, AEAD_CIPHER|CCM_MODE, {{EVP_CTRL_CCM_SET_IVLEN,EVP_CTRL_CCM_GET_TAG,EVP_CTRL_CCM_SET_TAG}}},
    {{"aes_256_ccm"}, "aes-256-ccm", {&EVP_aes_256_ccm}, 32, AEAD_CIPHER|CCM_MODE, {{EVP_CTRL_CCM_SET_IVLEN,EVP_CTRL_CCM_GET_TAG,EVP_CTRL_CCM_SET_TAG}}},
#else
    {{"aes_128_ccm"}, "aes-128-ccm", {NULL}, 16, AEAD_CIPHER|CCM_MODE, {{0,0,0}}},
    {{"aes_192_ccm"}, "aes-192-ccm", {NULL}, 24, AEAD_CIPHER|CCM_MODE, {{0,0,0}}},
    {{"aes_256_ccm"}, "aes-256-ccm", {NULL}, 32, AEAD_CIPHER|CCM_MODE, {{0,0,0}}},
#endif

    /*==== End of list ==== */

    {{NULL},NULL,{NULL},0,0,NOT_AEAD}
};

ErlNifResourceType* evp_cipher_ctx_rtype;

static size_t num_cipher_types = 0;

static void evp_cipher_ctx_dtor(ErlNifEnv* env, struct evp_cipher_ctx* ctx) {
    if (ctx == NULL)
        return;

    if (ctx->ctx)
        EVP_CIPHER_CTX_free(ctx->ctx);

#if !defined(HAVE_EVP_AES_CTR)
    if (ctx->env)
        enif_free_env(ctx->env);
#endif
}

int init_cipher_ctx(ErlNifEnv *env, ErlNifBinary* rt_buf) {
    evp_cipher_ctx_rtype = enif_open_resource_type(env, NULL,
                                                   resource_name("EVP_CIPHER_CTX", rt_buf),
                                                   (ErlNifResourceDtor*) evp_cipher_ctx_dtor,
                                                   ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                                   NULL);
    if (evp_cipher_ctx_rtype == NULL)
        goto err;

    return 1;

 err:
    PRINTF_ERR0("CRYPTO: Could not open resource type 'EVP_CIPHER_CTX'");
    return 0;
}

void init_cipher_types(ErlNifEnv* env)
{
    struct cipher_type_t* p = cipher_types;

    num_cipher_types = 0;
    for (p = cipher_types; p->type.str; p++) {
        num_cipher_types++;
	p->type.atom = enif_make_atom(env, p->type.str);
#ifdef HAS_3_0_API
        if (p->str_v3) {
            p->cipher.p = EVP_CIPHER_fetch(NULL, p->str_v3, "");
# ifdef FIPS_SUPPORT
            /* Try if valid in FIPS */
            {
                EVP_CIPHER *tmp = EVP_CIPHER_fetch(NULL, p->str_v3, "fips=yes");

                if (tmp) {
                    EVP_CIPHER_free(tmp);
                    p->flags &= ~NO_FIPS_CIPHER;
                } else
                    p->flags |= NO_FIPS_CIPHER;
            }
# endif /* FIPS_SUPPORT and >=3.0.0 */
        }
#else
	if (p->cipher.funcp)
	    p->cipher.p = p->cipher.funcp();
#endif
    }
    p->type.atom = atom_false; /* end marker */

    qsort(cipher_types, num_cipher_types, sizeof(cipher_types[0]), cmp_cipher_types);
}

const struct cipher_type_t* get_cipher_type(ERL_NIF_TERM type, size_t key_len)
{
    struct cipher_type_t key;

    key.type.atom = type;
    key.key_len = key_len;

    return bsearch(&key, cipher_types, num_cipher_types, sizeof(cipher_types[0]), cmp_cipher_types);
}


int cmp_cipher_types(const void *keyp, const void *elemp) {
    const struct cipher_type_t *key  = keyp;
    const struct cipher_type_t *elem = elemp;

    if (key->type.atom < elem->type.atom) return -1;
    else if (key->type.atom > elem->type.atom) return 1;
    else /* key->type.atom == elem->type.atom */
        if (!elem->key_len || key->key_len == elem->key_len) return 0;
        else if (key->key_len < elem->key_len) return -1;
        else return 1;
}


ERL_NIF_TERM cipher_info_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type) */
    const struct cipher_type_t *cipherp;
    const EVP_CIPHER     *cipher;
    ERL_NIF_TERM         ret, ret_mode;
    unsigned             type;
    unsigned long        mode;
    ERL_NIF_TERM keys[6];
    ERL_NIF_TERM vals[6];
    int ok;

    if ((cipherp = get_cipher_type_no_key(argv[0])) == NULL)
        return enif_make_badarg(env);

    if (CIPHER_FORBIDDEN_IN_FIPS(cipherp))
        return enif_raise_exception(env, atom_notsup);
    if ((cipher = cipherp->cipher.p) == NULL)
        return enif_raise_exception(env, atom_notsup);

    type = EVP_CIPHER_type(cipher);

    keys[0] = atom_type;
    vals[0] = (type == NID_undef ? atom_undefined : enif_make_int(env, type));
    keys[1] = atom_key_length;
    vals[1] = enif_make_int(env, EVP_CIPHER_key_length(cipher));
    keys[2] = atom_iv_length;
    vals[2] = enif_make_int(env, EVP_CIPHER_iv_length(cipher));
    keys[3] = atom_block_size;
    vals[3] = enif_make_int(env, EVP_CIPHER_block_size(cipher));
    keys[4] = atom_prop_aead;
#if defined(HAVE_AEAD)
    vals[4] = (((EVP_CIPHER_flags(cipher) & EVP_CIPH_FLAG_AEAD_CIPHER) != 0) ? atom_true : atom_false);
#else
    vals[4] = atom_false;
#endif

    mode = EVP_CIPHER_mode(cipher);
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

    ok = enif_make_map_from_arrays(env, keys, vals, 6, &ret);
    ASSERT(ok); (void)ok;

    return ret;
}

const struct cipher_type_t* get_cipher_type_no_key(ERL_NIF_TERM type)
{
    struct cipher_type_t key;

    key.type.atom = type;

    return bsearch(&key, cipher_types, num_cipher_types, sizeof(cipher_types[0]), cmp_cipher_types_no_key);
}

int cmp_cipher_types_no_key(const void *keyp, const void *elemp) {
    const struct cipher_type_t *key  = keyp;
    const struct cipher_type_t *elem = elemp;
    int ret;

    if (key->type.atom < elem->type.atom) ret = -1;
    else if (key->type.atom > elem->type.atom) ret = 1;
    else /* key->type.atom == elem->type.atom */ ret = 0;

    return ret;
}


ERL_NIF_TERM cipher_types_as_list(ErlNifEnv* env)
{
    struct cipher_type_t* p;
    ERL_NIF_TERM prev, hd;

    hd = enif_make_list(env, 0);
    prev = atom_undefined;

    for (p = cipher_types; (p->type.atom & (p->type.atom != atom_false)); p++) {
        if ((prev == p->type.atom) ||
            CIPHER_FORBIDDEN_IN_FIPS(p) )
            continue;

        if ((p->cipher.p != NULL) ||
            (p->flags & AES_CTR_COMPAT))
            {
                hd = enif_make_list_cell(env, p->type.atom, hd);
            }
    }

    return hd;
}
