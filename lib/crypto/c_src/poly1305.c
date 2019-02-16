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

#include "poly1305.h"

/* For OpenSSL >= 1.1.1 the hmac_nif and cmac_nif could be integrated into poly1305 (with 'type' as parameter) */
ERL_NIF_TERM poly1305_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, Text) */
#ifdef HAVE_POLY1305
    ErlNifBinary key_bin, text, ret_bin;
    ERL_NIF_TERM ret;
    EVP_PKEY *key = NULL;
    EVP_MD_CTX *mctx = NULL;
    EVP_PKEY_CTX *pctx = NULL;
    const EVP_MD *md = NULL;
    size_t size;
    int ret_bin_alloc = 0;

    ASSERT(argc == 2);

    if (!enif_inspect_binary(env, argv[0], &key_bin))
        goto bad_arg;
    if (key_bin.size != 32)
        goto bad_arg;
    if (!enif_inspect_binary(env, argv[1], &text))
        goto bad_arg;

    if ((key = EVP_PKEY_new_raw_private_key(EVP_PKEY_POLY1305, /*engine*/ NULL, key_bin.data,  key_bin.size)) == NULL)
        goto err;

    if ((mctx = EVP_MD_CTX_new()) == NULL)
        goto err;
    if (EVP_DigestSignInit(mctx, &pctx, md, /*engine*/ NULL, key) != 1)
        goto err;
    if (EVP_DigestSignUpdate(mctx, text.data, text.size) != 1)
        goto err;

    if (EVP_DigestSignFinal(mctx, NULL, &size) != 1)
        goto err;
    if (!enif_alloc_binary(size, &ret_bin))
        goto err;
    ret_bin_alloc = 1;
    if (EVP_DigestSignFinal(mctx, ret_bin.data, &size) != 1)
        goto err;

    if (size != ret_bin.size) {
        if (!enif_realloc_binary(&ret_bin, size))
            goto err;
    }

    ret = enif_make_binary(env, &ret_bin);
    ret_bin_alloc = 0;
    goto done;

 bad_arg:
    return enif_make_badarg(env);

 err:
    if (ret_bin_alloc)
        enif_release_binary(&ret_bin);
    ret = atom_error;

 done:
    if (mctx)
        EVP_MD_CTX_free(mctx);
    if (key)
        EVP_PKEY_free(key);
    return ret;

#else
    return atom_notsup;
#endif
}
