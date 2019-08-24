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

#ifndef E_CIPHER_H__
#define E_CIPHER_H__ 1

#include "common.h"

struct cipher_type_t {
    union {
	const char* str;    /* before init */
	ERL_NIF_TERM atom;  /* after init */
    }type;
    union {
	const EVP_CIPHER* (*funcp)(void); /* before init, NULL if notsup */
	const EVP_CIPHER* p;              /* after init, NULL if notsup */
    }cipher;
    size_t key_len;      /* != 0 to also match on key_len */
    unsigned flags;
    union {
        struct aead_ctrl {int ctx_ctrl_set_ivlen, ctx_ctrl_get_tag,  ctx_ctrl_set_tag;} aead;
    } extra;
};

/* masks in the flags field if cipher_type_t */
#define NO_FIPS_CIPHER 1
#define AES_CFBx 2
#define ECB_BUG_0_9_8L 4
#define AEAD_CIPHER 8
#define NON_EVP_CIPHER 16
#define AES_CTR_COMPAT 32
#define CCM_MODE 64
#define GCM_MODE 128


#ifdef FIPS_SUPPORT
/* May have FIPS support, must check dynamically if it is enabled */
# define CIPHER_FORBIDDEN_IN_FIPS(P) (((P)->flags & NO_FIPS_CIPHER) && FIPS_mode())
#else
/* No FIPS support since the symbol FIPS_SUPPORT is undefined */
# define CIPHER_FORBIDDEN_IN_FIPS(P) 0
#endif

extern ErlNifResourceType* evp_cipher_ctx_rtype;
struct evp_cipher_ctx {
    EVP_CIPHER_CTX* ctx;
    int iv_len;
#if !defined(HAVE_EVP_AES_CTR)
    ErlNifEnv* env;
    ERL_NIF_TERM state;
#endif
};

ERL_NIF_TERM cipher_info_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

int init_cipher_ctx(ErlNifEnv *env);

void init_cipher_types(ErlNifEnv* env);
const struct cipher_type_t* get_cipher_type_no_key(ERL_NIF_TERM type);
const struct cipher_type_t* get_cipher_type(ERL_NIF_TERM type, size_t key_len);

int cmp_cipher_types(const void *keyp, const void *elemp);
int cmp_cipher_types_no_key(const void *keyp, const void *elemp);

ERL_NIF_TERM cipher_types_as_list(ErlNifEnv* env);

#endif /* E_CIPHER_H__ */
