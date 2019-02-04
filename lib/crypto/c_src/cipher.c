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
    {{"rc2_cbc"},
#ifndef OPENSSL_NO_RC2
     {&EVP_rc2_cbc}
#else
     {NULL}
#endif
    ,0},
    {{"des_cbc"}, {COND_NO_DES_PTR(&EVP_des_cbc)}, 0},
    {{"des_cfb"}, {COND_NO_DES_PTR(&EVP_des_cfb8)}, 0},
    {{"des_ecb"}, {COND_NO_DES_PTR(&EVP_des_ecb)}, 0},
    {{"des_ede3_cbc"}, {COND_NO_DES_PTR(&EVP_des_ede3_cbc)}, 0},
    {{"des_ede3_cbf"}, /* Misspelled, retained */
#ifdef HAVE_DES_ede3_cfb_encrypt
     {COND_NO_DES_PTR(&EVP_des_ede3_cfb8)}
#else
     {NULL}
#endif
     ,0},
    {{"des_ede3_cfb"},
#ifdef HAVE_DES_ede3_cfb_encrypt
     {COND_NO_DES_PTR(&EVP_des_ede3_cfb8)}
#else
     {NULL}
#endif
     ,0},
    {{"blowfish_cbc"}, {&EVP_bf_cbc}, 0},
    {{"blowfish_cfb64"}, {&EVP_bf_cfb64}, 0},
    {{"blowfish_ofb64"}, {&EVP_bf_ofb}, 0},
    {{"blowfish_ecb"}, {&EVP_bf_ecb}, 0},
    {{"aes_cbc"}, {&EVP_aes_128_cbc}, 16},
    {{"aes_cbc"}, {&EVP_aes_192_cbc}, 24},
    {{"aes_cbc"}, {&EVP_aes_256_cbc}, 32},
    {{"aes_cbc128"}, {&EVP_aes_128_cbc}, 0},
    {{"aes_cbc256"}, {&EVP_aes_256_cbc}, 0},
    {{"aes_cfb8"}, {&EVP_aes_128_cfb8}, 0},
    {{"aes_cfb128"}, {&EVP_aes_128_cfb128}, 0},
    {{"aes_ecb"}, {&EVP_aes_128_ecb}, 16},
    {{"aes_ecb"}, {&EVP_aes_192_ecb}, 24},
    {{"aes_ecb"}, {&EVP_aes_256_ecb}, 32},
    {{NULL},{NULL},0}
};

#ifdef HAVE_EVP_AES_CTR
ErlNifResourceType* evp_cipher_ctx_rtype;

static void evp_cipher_ctx_dtor(ErlNifEnv* env, struct evp_cipher_ctx* ctx) {
    if (ctx == NULL)
        return;

    if (ctx->ctx)
        EVP_CIPHER_CTX_free(ctx->ctx);
}
#endif

int init_cipher_ctx(ErlNifEnv *env) {
#ifdef HAVE_EVP_AES_CTR
    evp_cipher_ctx_rtype = enif_open_resource_type(env, NULL, "EVP_CIPHER_CTX",
                                                   (ErlNifResourceDtor*) evp_cipher_ctx_dtor,
                                                   ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                                   NULL);
    if (evp_cipher_ctx_rtype == NULL)
        goto err;
#endif

    return 1;

#ifdef HAVE_EVP_AES_CTR
 err:
    PRINTF_ERR0("CRYPTO: Could not open resource type 'EVP_CIPHER_CTX'");
    return 0;
#endif
}

void init_cipher_types(ErlNifEnv* env)
{
    struct cipher_type_t* p = cipher_types;

    for (p = cipher_types; p->type.str; p++) {
	p->type.atom = enif_make_atom(env, p->type.str);
	if (p->cipher.funcp)
	    p->cipher.p = p->cipher.funcp();
    }
    p->type.atom = atom_false; /* end marker */
}

struct cipher_type_t* get_cipher_type(ERL_NIF_TERM type, size_t key_len)
{
    struct cipher_type_t* p = NULL;
    for (p = cipher_types; p->type.atom != atom_false; p++) {
	if (type == p->type.atom && (!p->key_len || key_len == p->key_len)) {
	    return p;
	}
    }
    return NULL;
}
