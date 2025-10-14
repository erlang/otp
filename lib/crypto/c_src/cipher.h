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

#ifndef E_CIPHER_H__
#define E_CIPHER_H__ 1

#include "common.h"

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

#endif /* E_CIPHER_H__ */
