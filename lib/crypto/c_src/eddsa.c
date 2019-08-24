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

#include "eddsa.h"

#ifdef HAVE_EDDSA
int get_eddsa_key(ErlNifEnv* env, int public, ERL_NIF_TERM key, EVP_PKEY **pkey)
{
    /* key=[K] */
    EVP_PKEY *result;
    ERL_NIF_TERM head, tail, tail2, algo;
    ErlNifBinary bin;
    int type;

    if (!enif_get_list_cell(env, key, &head, &tail))
        goto err;
    if (!enif_inspect_binary(env, head, &bin))
        goto err;
    if (!enif_get_list_cell(env, tail, &algo, &tail2))
        goto err;
    if (!enif_is_empty_list(env, tail2))
        goto err;

    if (algo == atom_ed25519) {
        type = EVP_PKEY_ED25519;
    } else if (algo == atom_ed448) {
        type = EVP_PKEY_ED448;
    } else {
        goto err;
    }

    if (public)
        result = EVP_PKEY_new_raw_public_key(type, NULL, bin.data, bin.size);
    else 
        result = EVP_PKEY_new_raw_private_key(type, NULL, bin.data, bin.size);

    if (result == NULL)
        goto err;

    *pkey = result;
    return 1;

 err:
    return 0;
}
#endif
