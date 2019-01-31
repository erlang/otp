/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2018. All Rights Reserved.
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

#ifdef OPENSSL_NO_DES
#define COND_NO_DES_PTR(Ptr) (NULL)
#else
#define COND_NO_DES_PTR(Ptr) (Ptr)
#endif

static struct cipher_type_t cipher_types[] =
{
#ifndef OPENSSL_NO_RC2
    {{"rc2_cbc"}, {&EVP_rc2_cbc}, 0, NO_FIPS_CIPHER},
#else
    {{"rc2_cbc"}, {NULL}, 0, NO_FIPS_CIPHER},
#endif
    {{"rc4"},     {&EVP_rc4}, 0, NO_FIPS_CIPHER},
    {{"des_cbc"}, {COND_NO_DES_PTR(&EVP_des_cbc)}, 0, NO_FIPS_CIPHER},
    {{"des_cfb"}, {COND_NO_DES_PTR(&EVP_des_cfb8)}, 0, NO_FIPS_CIPHER},
    {{"des_ecb"}, {COND_NO_DES_PTR(&EVP_des_ecb)}, 0, NO_FIPS_CIPHER | ECB},

    {{"des_ede3_cbc"}, {COND_NO_DES_PTR(&EVP_des_ede3_cbc)}, 0, 0},

#ifdef HAVE_DES_ede3_cfb_encrypt
    {{"des_ede3_cfb"}, {COND_NO_DES_PTR(&EVP_des_ede3_cfb8)}, 0, 0},
#else
    {{"des_ede3_cfb"}, {NULL}, 0, 0},
#endif

    {{"blowfish_cbc"}, {&EVP_bf_cbc}, 0, NO_FIPS_CIPHER},
    {{"blowfish_cfb64"}, {&EVP_bf_cfb64}, 0, NO_FIPS_CIPHER},
    {{"blowfish_ofb64"}, {&EVP_bf_ofb}, 0, NO_FIPS_CIPHER},
    {{"blowfish_ecb"}, {&EVP_bf_ecb}, 0, NO_FIPS_CIPHER | ECB},
    {{"aes_cbc"}, {&EVP_aes_128_cbc}, 16, 0},
    {{"aes_cbc"}, {&EVP_aes_192_cbc}, 24, 0},
    {{"aes_cbc"}, {&EVP_aes_256_cbc}, 32, 0},
    {{"aes_128_cbc"}, {&EVP_aes_128_cbc}, 16, 0},
    {{"aes_192_cbc"}, {&EVP_aes_192_cbc}, 24, 0},
    {{"aes_256_cbc"}, {&EVP_aes_256_cbc}, 32, 0},
    {{"aes_cfb8"}, {&EVP_aes_128_cfb8}, 16, NO_FIPS_CIPHER},
    {{"aes_cfb8"}, {&EVP_aes_192_cfb8}, 24, NO_FIPS_CIPHER},
    {{"aes_cfb8"}, {&EVP_aes_256_cfb8}, 32, NO_FIPS_CIPHER},
    {{"aes_cfb128"}, {&EVP_aes_128_cfb128}, 16, NO_FIPS_CIPHER},
    {{"aes_cfb128"}, {&EVP_aes_192_cfb128}, 24, NO_FIPS_CIPHER},
    {{"aes_cfb128"}, {&EVP_aes_256_cfb128}, 32, NO_FIPS_CIPHER},
    {{"aes_ecb"}, {&EVP_aes_128_ecb}, 16, ECB},
    {{"aes_ecb"}, {&EVP_aes_192_ecb}, 24, ECB},
    {{"aes_ecb"}, {&EVP_aes_256_ecb}, 32, ECB},
#if defined(HAVE_EVP_AES_CTR)
    {{"aes_128_ctr"}, {&EVP_aes_128_ctr}, 16, 0},
    {{"aes_192_ctr"}, {&EVP_aes_192_ctr}, 24, 0},
    {{"aes_256_ctr"}, {&EVP_aes_256_ctr}, 32, 0},
    {{"aes_ctr"},    {&EVP_aes_128_ctr}, 16, 0},
    {{"aes_ctr"},    {&EVP_aes_192_ctr}, 24, 0},
    {{"aes_ctr"},    {&EVP_aes_256_ctr}, 32, 0},
#else
    {{"aes_ctr"},    {NULL}, 0, 0},
#endif
#if defined(HAVE_CHACHA20)
    {{"chacha20"}, {&EVP_chacha20}, 32, NO_FIPS_CIPHER},
#else
    {{"chacha20"}, {NULL}, 0, NO_FIPS_CIPHER},
#endif

    /*==== AEAD ciphers ====*/
#if defined(HAVE_CHACHA20_POLY1305)
    {{"chacha20_poly1305"}, {&EVP_chacha20_poly1305}, 0, NO_FIPS_CIPHER | AEAD_CIPHER, {{EVP_CTRL_AEAD_SET_IVLEN,EVP_CTRL_AEAD_GET_TAG,EVP_CTRL_AEAD_SET_TAG}}},
#else
    {{"chacha20_poly1305"}, {NULL}, 0, NO_FIPS_CIPHER | AEAD_CIPHER, {{0,0,0}}},
#endif

#if defined(HAVE_GCM)
    {{"aes_gcm"}, {&EVP_aes_128_gcm}, 16, AEAD_CIPHER, {{EVP_CTRL_GCM_SET_IVLEN,EVP_CTRL_GCM_GET_TAG,EVP_CTRL_GCM_SET_TAG}}},
    {{"aes_gcm"}, {&EVP_aes_192_gcm}, 24, AEAD_CIPHER, {{EVP_CTRL_GCM_SET_IVLEN,EVP_CTRL_GCM_GET_TAG,EVP_CTRL_GCM_SET_TAG}}},
    {{"aes_gcm"}, {&EVP_aes_256_gcm}, 32, AEAD_CIPHER, {{EVP_CTRL_GCM_SET_IVLEN,EVP_CTRL_GCM_GET_TAG,EVP_CTRL_GCM_SET_TAG}}},
#else
    {{"aes_gcm"}, {NULL}, 0, AEAD_CIPHER, {{0,0,0}}},
#endif

#if defined(HAVE_CCM)
    {{"aes_ccm"}, {&EVP_aes_128_ccm}, 16, AEAD_CIPHER, {{EVP_CTRL_CCM_SET_IVLEN,EVP_CTRL_CCM_GET_TAG,EVP_CTRL_CCM_SET_TAG}}},
    {{"aes_ccm"}, {&EVP_aes_192_ccm}, 24, AEAD_CIPHER, {{EVP_CTRL_CCM_SET_IVLEN,EVP_CTRL_CCM_GET_TAG,EVP_CTRL_CCM_SET_TAG}}},
    {{"aes_ccm"}, {&EVP_aes_256_ccm}, 32, AEAD_CIPHER, {{EVP_CTRL_CCM_SET_IVLEN,EVP_CTRL_CCM_GET_TAG,EVP_CTRL_CCM_SET_TAG}}},
#else
    {{"aes_ccm"}, {NULL}, 0, AEAD_CIPHER, {{0,0,0}}},
#endif

    /*==== Specialy handled ciphers, only for inclusion in algorithm's list ====*/
#ifdef HAVE_AES_IGE
    {{"aes_ige256"}, {NULL}, 0, NO_FIPS_CIPHER | NON_EVP_CIPHER},
#endif

    /*==== End of list ==== */

    {{NULL},{NULL},0,0}
};

ErlNifResourceType* evp_cipher_ctx_rtype;

int num_cipher_types = 0;

static void evp_cipher_ctx_dtor(ErlNifEnv* env, struct evp_cipher_ctx* ctx) {
    if (ctx == NULL)
        return;

    if (ctx->ctx)
        EVP_CIPHER_CTX_free(ctx->ctx);
}

int init_cipher_ctx(ErlNifEnv *env) {
    evp_cipher_ctx_rtype = enif_open_resource_type(env, NULL, "EVP_CIPHER_CTX",
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

    for (p = cipher_types; p->type.str; p++) {
        num_cipher_types++;
	p->type.atom = enif_make_atom(env, p->type.str);
	if (p->cipher.funcp)
	    p->cipher.p = p->cipher.funcp();
    }
    p->type.atom = atom_false; /* end marker */

    qsort(cipher_types, num_cipher_types, sizeof(cipher_types[0]), cmp_cipher_types);
}

struct cipher_type_t* get_cipher_type(ERL_NIF_TERM type, size_t key_len)
{
    struct cipher_type_t key;

    key.type.atom = type;
    key.key_len = key_len;

    return bsearch(&key, cipher_types, num_cipher_types, sizeof(cipher_types[0]), cmp_cipher_types);
}


int cmp_cipher_types(const void *keyp, const void *elemp) {
    struct cipher_type_t *key  = (struct cipher_type_t *) keyp;
    struct cipher_type_t *elem = (struct cipher_type_t *) elemp;

    if (key->type.atom < elem->type.atom) return -1;
    else if (key->type.atom > elem->type.atom) return 1;
    else /* key->type.atom == elem->type.atom */
        if (!elem->key_len || key->key_len == elem->key_len) return 0;
        else if (key->key_len < elem->key_len) return -1;
        else return 1;
}


ERL_NIF_TERM cipher_types_as_list(ErlNifEnv* env)
{
    struct cipher_type_t* p;
    ERL_NIF_TERM prev, hd;

    hd = enif_make_list(env, 0);
    prev = atom_undefined;

    for (p = cipher_types; (p->type.atom & (p->type.atom != atom_false)); p++) {
        if ((prev != p->type.atom) &&
            ((p->cipher.p != NULL) || (p->flags & NON_EVP_CIPHER)) && /* Special handling. Bad indeed... */
            ! FORBIDDEN_IN_FIPS(p)
            )
        hd = enif_make_list_cell(env, p->type.atom, hd);
        prev = p->type.atom;
    }

    return hd;
}
