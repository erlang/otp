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
    ERL_NIF_TERM ret = atom_error;
    EVP_PKEY *key = NULL;
    EVP_MD_CTX *mctx = NULL;
    EVP_PKEY_CTX *pctx = NULL;
    const EVP_MD *md = NULL;
    size_t size;
    int type;

    type = EVP_PKEY_POLY1305;

    if (!enif_inspect_binary(env, argv[0], &key_bin) ||
        !(key_bin.size == 32) ) {
        return enif_make_badarg(env);
    }

    if (!enif_inspect_binary(env, argv[1], &text) ) {
        return enif_make_badarg(env);
    }

    key = EVP_PKEY_new_raw_private_key(type, /*engine*/ NULL, key_bin.data,  key_bin.size);

    if (!key ||
        !(mctx = EVP_MD_CTX_new()) ||
        !EVP_DigestSignInit(mctx, &pctx, md, /*engine*/ NULL, key) ||
        !EVP_DigestSignUpdate(mctx, text.data, text.size)) {
        goto err;
    }

    if (!EVP_DigestSignFinal(mctx, NULL, &size) ||
        !enif_alloc_binary(size, &ret_bin) ||
        !EVP_DigestSignFinal(mctx, ret_bin.data, &size)) {
        goto err;
    }

    if ((size != ret_bin.size) &&
        !enif_realloc_binary(&ret_bin, size)) {
        goto err;
    }

    ret = enif_make_binary(env, &ret_bin);

 err:
    EVP_MD_CTX_free(mctx);
    EVP_PKEY_free(key);
    return ret;

#else
    return atom_notsup;
#endif
}

