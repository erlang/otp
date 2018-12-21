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
    const size_t key_len;      /* != 0 to also match on key_len */
};

#ifdef HAVE_EVP_AES_CTR
extern ErlNifResourceType* evp_cipher_ctx_rtype;
struct evp_cipher_ctx {
    EVP_CIPHER_CTX* ctx;
};
#endif

int init_cipher_ctx(ErlNifEnv *env);

void init_cipher_types(ErlNifEnv* env);
struct cipher_type_t* get_cipher_type(ERL_NIF_TERM type, size_t key_len);

#endif /* E_CIPHER_H__ */
