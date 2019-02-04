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

#include "evp.h"

ERL_NIF_TERM evp_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
    /*    (Curve, PeerBin, MyBin) */
{
#ifdef HAVE_ED_CURVE_DH
    ERL_NIF_TERM ret;
    int type;
    EVP_PKEY_CTX *ctx = NULL;
    ErlNifBinary peer_bin, my_bin, key_bin;
    EVP_PKEY *peer_key = NULL, *my_key = NULL;
    size_t max_size;
    int key_bin_alloc = 0;

    ASSERT(argc == 3);

    if (argv[0] == atom_x25519)
        type = EVP_PKEY_X25519;
    else if (argv[0] == atom_x448)
        type = EVP_PKEY_X448;
    else
        goto bad_arg;

    if (!enif_inspect_binary(env, argv[1], &peer_bin))
        goto bad_arg;
    if (!enif_inspect_binary(env, argv[2], &my_bin))
        goto bad_arg;

    if ((my_key = EVP_PKEY_new_raw_private_key(type, NULL, my_bin.data, my_bin.size)) == NULL)
        goto err;
    if ((ctx = EVP_PKEY_CTX_new(my_key, NULL)) == NULL)
        goto err;

    if (EVP_PKEY_derive_init(ctx) != 1)
        goto err;

    if ((peer_key = EVP_PKEY_new_raw_public_key(type, NULL, peer_bin.data, peer_bin.size)) == NULL)
        goto err;
    if (EVP_PKEY_derive_set_peer(ctx, peer_key) != 1)
        goto err;

    if (EVP_PKEY_derive(ctx, NULL, &max_size) != 1)
        goto err;

    if (!enif_alloc_binary(max_size, &key_bin))
        goto err;
    key_bin_alloc = 1;
    if (EVP_PKEY_derive(ctx, key_bin.data, &key_bin.size) != 1)
        goto err;

    if (key_bin.size < max_size) {
        if (!enif_realloc_binary(&key_bin, (size_t)key_bin.size))
            goto err;
    }

    ret = enif_make_binary(env, &key_bin);
    key_bin_alloc = 0;
    goto done;

 bad_arg:
 err:
    if (key_bin_alloc)
        enif_release_binary(&key_bin);
    ret = enif_make_badarg(env);

 done:
    if (my_key)
        EVP_PKEY_free(my_key);
    if (peer_key)
        EVP_PKEY_free(peer_key);
    if (ctx)
        EVP_PKEY_CTX_free(ctx);

    return ret;

#else
    return atom_notsup;
#endif
}

ERL_NIF_TERM evp_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
/* (Curve) */
{
#ifdef HAVE_ED_CURVE_DH
    int type;
    EVP_PKEY_CTX *ctx = NULL;
    EVP_PKEY *pkey = NULL;
    ERL_NIF_TERM ret_pub, ret_prv, ret;
    size_t key_len;
    unsigned char *out_pub = NULL, *out_priv = NULL;

    ASSERT(argc == 1);

    if (argv[0] == atom_x25519)
        type = EVP_PKEY_X25519;
    else if (argv[0] == atom_x448)
        type = EVP_PKEY_X448;
    else
        goto bad_arg;

    if ((ctx = EVP_PKEY_CTX_new_id(type, NULL)) == NULL)
        goto bad_arg;

    if (EVP_PKEY_keygen_init(ctx) != 1)
        goto err;
    if (EVP_PKEY_keygen(ctx, &pkey) != 1)
        goto err;

    if (EVP_PKEY_get_raw_public_key(pkey, NULL, &key_len) != 1)
        goto err;
    if ((out_pub = enif_make_new_binary(env, key_len, &ret_pub)) == NULL)
        goto err;
    if (EVP_PKEY_get_raw_public_key(pkey, out_pub, &key_len) != 1)
        goto err;

    if (EVP_PKEY_get_raw_private_key(pkey, NULL, &key_len) != 1)
        goto err;
    if ((out_priv = enif_make_new_binary(env, key_len, &ret_prv)) == NULL)
        goto err;
    if (EVP_PKEY_get_raw_private_key(pkey, out_priv, &key_len) != 1)
        goto err;

    ret = enif_make_tuple2(env, ret_pub, ret_prv);
    goto done;

 bad_arg:
    ret = enif_make_badarg(env);
    goto done;

 err:
    ret = atom_error;

 done:
    if (pkey)
        EVP_PKEY_free(pkey);
    if (ctx)
        EVP_PKEY_CTX_free(ctx);
    return ret;

#else
    return atom_notsup;
#endif
}

