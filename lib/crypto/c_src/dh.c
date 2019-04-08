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

#include "dh.h"
#include "bn.h"

ERL_NIF_TERM dh_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (PrivKey|undefined, DHParams=[P,G], Mpint, Len|0) */
#ifdef HAVE_DH
    DH *dh_params = NULL;
    unsigned int mpint; /* 0 or 4 */
    ERL_NIF_TERM head, tail;
    BIGNUM *dh_p = NULL;
    BIGNUM *dh_p_shared;
    BIGNUM *dh_g = NULL;
    BIGNUM *priv_key_in = NULL;
    unsigned long len = 0;
    unsigned char *pub_ptr, *prv_ptr;
    int pub_len, prv_len;
    ERL_NIF_TERM ret_pub, ret_prv, ret;
    const BIGNUM *pub_key_gen, *priv_key_gen;
#ifdef HAS_EVP_PKEY_CTX
    EVP_PKEY_CTX *ctx = NULL;
    EVP_PKEY *dhkey = NULL, *params = NULL;
#endif

    ASSERT(argc == 4);

    if (argv[0] != atom_undefined) {
        if (!get_bn_from_bin(env, argv[0], &priv_key_in))
            goto bad_arg;
    }
    if (!enif_get_list_cell(env, argv[1], &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &dh_p))
        goto bad_arg;

    if (!enif_get_list_cell(env, tail, &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &dh_g))
        goto bad_arg;

    if (!enif_is_empty_list(env, tail))
        goto bad_arg;

    if (!enif_get_uint(env, argv[2], &mpint))
        goto bad_arg;
    if (mpint != 0 && mpint != 4)
        goto bad_arg;

    if (!enif_get_ulong(env, argv[3], &len))
        goto bad_arg;
    if (len > LONG_MAX)
        goto bad_arg;

    /* Load dh_params with values to use by the generator.
       Mem mgmnt transfered from dh_p etc to dh_params */
    if ((dh_params = DH_new()) == NULL)
        goto bad_arg;
    if (priv_key_in) {
        if (!DH_set0_key(dh_params, NULL, priv_key_in))
            goto bad_arg;
        /* On success, dh_params owns priv_key_in */
        priv_key_in = NULL;
    }
    if (!DH_set0_pqg(dh_params, dh_p, NULL, dh_g))
        goto bad_arg;
    dh_p_shared = dh_p; /* Don't free this because dh_params owns it */
    /* On success, dh_params owns dh_p and dh_g */
    dh_p = NULL;
    dh_g = NULL;

    if (len) {
        int bn_len;

        if ((bn_len = BN_num_bits(dh_p_shared)) < 0)
            goto bad_arg;
        dh_p_shared = NULL;  /* dh_params owns the reference */
        if (len >= (size_t)bn_len)
            goto bad_arg;

        if (!DH_set_length(dh_params, (long)len))
            goto bad_arg;
    }

#ifdef HAS_EVP_PKEY_CTX
    if ((params = EVP_PKEY_new()) == NULL)
        goto err;

   /* set the key referenced by params to dh_params... */
    if (EVP_PKEY_set1_DH(params, dh_params) != 1)
        goto err;

    if ((ctx = EVP_PKEY_CTX_new(params, NULL)) == NULL)
        goto err;

    if (EVP_PKEY_keygen_init(ctx) != 1)
        goto err;

    if ((dhkey = EVP_PKEY_new()) == NULL)
        goto err;

    /* key gen op, key written to ppkey (=last arg) */
    if (EVP_PKEY_keygen(ctx, &dhkey) != 1)
        goto err;

    DH_free(dh_params);
    if ((dh_params = EVP_PKEY_get1_DH(dhkey)) == NULL)
        goto err;

#else
    if (!DH_generate_key(dh_params))
        goto err;
#endif

    DH_get0_key(dh_params, &pub_key_gen, &priv_key_gen);

    if ((pub_len = BN_num_bytes(pub_key_gen)) < 0)
        goto err;
    if ((prv_len = BN_num_bytes(priv_key_gen)) < 0)
        goto err;

    if ((pub_ptr = enif_make_new_binary(env, (size_t)pub_len+mpint, &ret_pub)) == NULL)
        goto err;
    if ((prv_ptr = enif_make_new_binary(env, (size_t)prv_len+mpint, &ret_prv)) == NULL)
        goto err;

    if (mpint) {
        put_uint32(pub_ptr, (unsigned int)pub_len);
        pub_ptr += 4;

        put_uint32(prv_ptr, (unsigned int)prv_len);
        prv_ptr += 4;
    }

    if (BN_bn2bin(pub_key_gen, pub_ptr) < 0)
        goto err;
    if (BN_bn2bin(priv_key_gen, prv_ptr) < 0)
        goto err;

    ERL_VALGRIND_MAKE_MEM_DEFINED(pub_ptr, pub_len);
    ERL_VALGRIND_MAKE_MEM_DEFINED(prv_ptr, prv_len);

    ret = enif_make_tuple2(env, ret_pub, ret_prv);
    goto done;

 bad_arg:
    ret = enif_make_badarg(env);
    goto done;

 err:
    ret = atom_error;

 done:
    if (priv_key_in)
        BN_free(priv_key_in);
    if (dh_p)
        BN_free(dh_p);
    if (dh_g)
        BN_free(dh_g);
    if (dh_params)
        DH_free(dh_params);

#ifdef HAS_EVP_PKEY_CTX
    if (ctx)
        EVP_PKEY_CTX_free(ctx);
    if (dhkey)
        EVP_PKEY_free(dhkey);
    if (params)
        EVP_PKEY_free(params);
#endif

    return ret;
#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

ERL_NIF_TERM dh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (OthersPublicKey, MyPrivateKey, DHParams=[P,G]) */
#ifdef HAVE_DH
    BIGNUM *other_pub_key = NULL;
    BIGNUM *dh_p = NULL;
    BIGNUM *dh_g = NULL;
    BIGNUM *dummy_pub_key = NULL;
    BIGNUM *priv_key = NULL;
    DH *dh_priv = NULL;
    ERL_NIF_TERM head, tail, ret;
    ErlNifBinary ret_bin;
    int size;
    int ret_bin_alloc = 0;
    int dh_size;

    /* Check the arguments and get
          my private key (dh_priv),
          the peer's public key (other_pub_key),
          the parameters p & q
    */
    ASSERT(argc == 3);

    if (!get_bn_from_bin(env, argv[0], &other_pub_key))
        goto bad_arg;
    if (!get_bn_from_bin(env, argv[1], &priv_key))
        goto bad_arg;

    if (!enif_get_list_cell(env, argv[2], &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &dh_p))
        goto bad_arg;

    if (!enif_get_list_cell(env, tail, &head, &tail))
        goto bad_arg;
    if (!get_bn_from_bin(env, head, &dh_g))
        goto bad_arg;

    if (!enif_is_empty_list(env, tail))
        goto bad_arg;

    /* Note: DH_set0_key() does not allow setting only the
     * private key, although DH_compute_key() does not use the
     * public key. Work around this limitation by setting
     * the public key to a copy of the private key.
     */
    if ((dummy_pub_key = BN_dup(priv_key)) == NULL)
        goto err;
    if ((dh_priv = DH_new()) == NULL)
        goto err;

    if (!DH_set0_key(dh_priv, dummy_pub_key, priv_key))
        goto err;
    /* dh_priv owns dummy_pub_key and priv_key now */
    dummy_pub_key = NULL;
    priv_key = NULL;

    if (!DH_set0_pqg(dh_priv, dh_p, NULL, dh_g))
        goto err;
    /* dh_priv owns dh_p and dh_g now */
    dh_p = NULL;
    dh_g = NULL;

    if ((dh_size = DH_size(dh_priv)) < 0)
        goto err;
    if (!enif_alloc_binary((size_t)dh_size, &ret_bin))
        goto err;
    ret_bin_alloc = 1;

    if ((size = DH_compute_key(ret_bin.data, other_pub_key, dh_priv)) < 0)
        goto err;
    if (size == 0)
        goto err;

    if ((size_t)size != ret_bin.size) {
        if (!enif_realloc_binary(&ret_bin, (size_t)size))
            goto err;
    }

    ret = enif_make_binary(env, &ret_bin);
    ret_bin_alloc = 0;
    goto done;

 bad_arg:
 err:
    if (ret_bin_alloc)
        enif_release_binary(&ret_bin);
    ret = enif_make_badarg(env);

 done:
    if (other_pub_key)
        BN_free(other_pub_key);
    if (priv_key)
        BN_free(priv_key);
    if (dh_p)
        BN_free(dh_p);
    if (dh_g)
        BN_free(dh_g);
    if (dummy_pub_key)
        BN_free(dummy_pub_key);
    if (dh_priv)
        DH_free(dh_priv);

    return ret;
#else
    return enif_raise_exception(env, atom_notsup);
#endif
}
