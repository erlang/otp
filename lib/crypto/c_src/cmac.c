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

#include "common.h"

/*****************************************************************
 *
 * This file has functions for compatibility with cryptolibs
 * lacking the EVP_Digest API.
 *
 * See mac.c for the implementation using the EVP interface.
 *
 ****************************************************************/

#if defined(HAVE_CMAC) && !defined(HAVE_EVP_PKEY_new_CMAC_key)

#include "cmac.h"

int cmac_low_level(ErlNifEnv* env,
                   ErlNifBinary key_bin, const EVP_CIPHER* cipher, ErlNifBinary text,
                   ErlNifBinary *ret_bin, int *ret_bin_alloc, ERL_NIF_TERM *return_term)
{
    CMAC_CTX *ctx = NULL;
    size_t size;

    if ((ctx = CMAC_CTX_new()) == NULL)
        goto local_err;

    if (!CMAC_Init(ctx, key_bin.data, key_bin.size, cipher, NULL))
        goto local_err;

    if (!CMAC_Update(ctx, text.data, text.size))
        goto local_err;

    if ((size = (size_t)EVP_CIPHER_block_size(cipher)) < 0)
        goto local_err;

    if (!enif_alloc_binary(size, ret_bin))
        goto local_err;
    *ret_bin_alloc = 1;
                
    if (!CMAC_Final(ctx, ret_bin->data, &ret_bin->size))
        goto local_err;

    CMAC_CTX_free(ctx);
    return 1;

 local_err:
    if (ctx)
        CMAC_CTX_free(ctx);

    *return_term = EXCP_ERROR(env,"Compat cmac");
    return 0;
}

#endif
