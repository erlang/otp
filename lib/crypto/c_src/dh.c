/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2023. All Rights Reserved.
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


#if !defined(HAVE_DH)
ERL_NIF_TERM dh_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return  enif_raise_exception(env, atom_notsup);
}

ERL_NIF_TERM dh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    return  enif_raise_exception(env, atom_notsup);
}

#else /* HAVE_DH */


# ifdef HAS_3_0_API

/* Has 3_0 */
ERL_NIF_TERM dh_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (PrivKey|undefined, DHParams=[P,G], 0, Len|0) */
    ErlNifUInt64 len = 0;
    int i = 0;
    OSSL_PARAM params[8];
    EVP_PKEY *pkey = NULL, *pkey_gen = NULL;
    EVP_PKEY_CTX *pctx = NULL, *pctx_gen = NULL;
    BIGNUM *pub_key_gen = NULL, *priv_key_gen = NULL;
    unsigned char *pub_ptr, *prv_ptr;
    int pub_len, prv_len;
    ERL_NIF_TERM ret_pub, ret_prv, ret = atom_undefined;

    /* Fetch parameters and assign them to params[] */
    if (argv[0] != atom_undefined)
        if (!get_ossl_BN_param_from_bin(env, "priv",  argv[0], &params[i++]))  {
            ret = EXCP_BADARG_N(env, 0, "PrivKeyIn");
            goto done;
        }

    { /*argv[1] - the lists [P,G] */
        ERL_NIF_TERM head, tail;

        head = argv[1];
        if (!get_ossl_param_from_bin_in_list(env, "p",  &head, &params[i++]) ) {
            ret = EXCP_BADARG_N(env, 1, "Bad value of 'p'");
            goto done;
        }

        if (!get_ossl_param_from_bin_in_list(env, "g",  &head, &params[i++]) ) {
            ret = EXCP_BADARG_N(env, 1, "Bad value of 'g'");
            goto done;
        }

        tail = head;
        if (!enif_is_empty_list(env,tail)) {
            ret = EXCP_BADARG_N(env, 1, "Not a two-element list");
            goto done;
        }
    }

    /* argv[2] is always hardcoded to 0 in crypto.erl. A left over from some older version?
       Let's skip that one
    */

    /* argv[3] is the length of the private key that is to be generated */
    if (!enif_get_uint64(env, argv[3], &len) ||
        (len > LONG_MAX) ) {
        ret = EXCP_BADARG_N(env, 3, "Bad value of length element");
        goto done;
    }
    else if (len)
        params[i++] = OSSL_PARAM_construct_uint64("priv_len", &len);

    /* End of parameter fetching */
    params[i++] = OSSL_PARAM_construct_end();

    pctx = EVP_PKEY_CTX_new_from_name(NULL, "DH", NULL);

    if (EVP_PKEY_fromdata_init(pctx) <= 0) {
        ret = EXCP_ERROR(env, "Can't init fromdata");
        goto done;
    }
    if (EVP_PKEY_fromdata(pctx, &pkey, EVP_PKEY_KEYPAIR, params) <= 0) {
        ret = EXCP_ERROR(env, "Can't do fromdata");
        goto done;
    }

    /* Generate a new pkey from the data in the old */
    
    pctx_gen = EVP_PKEY_CTX_new_from_pkey(NULL, pkey, NULL);
        
    if (!EVP_PKEY_keygen_init(pctx_gen)) {
        ret = EXCP_ERROR(env, "Can't init DH generation");
        goto done;
    }

    if (!EVP_PKEY_CTX_set_params(pctx_gen, params))  {
        ret = EXCP_ERROR(env, "Can't set params");
        goto done;
    }

    if (!EVP_PKEY_generate(pctx_gen, &pkey_gen)) {
        ret = EXCP_ERROR(env, "Can't generate DH key pair");
        goto done;
    }
    
    /* Finally return the OTP representation of the keys: */

    if (!EVP_PKEY_get_bn_param(pkey_gen, "pub",  &pub_key_gen)) {
        ret = EXCP_ERROR(env, "Can't get public key");
        goto done;
    }
    if ((pub_len = BN_num_bytes(pub_key_gen)) < 0 ||
        (pub_ptr = enif_make_new_binary(env, (size_t)pub_len, &ret_pub)) == NULL ||
        BN_bn2bin(pub_key_gen, pub_ptr) < 0)
        {
            ret = EXCP_ERROR(env, "Can't convert public key");
            goto done;
        }

    if (!EVP_PKEY_get_bn_param(pkey_gen, "priv",  &priv_key_gen)) {
        ret = EXCP_ERROR(env, "Can't get private key");
        goto done;
    }
    if ((prv_len = BN_num_bytes(priv_key_gen)) < 0 ||
        (prv_ptr = enif_make_new_binary(env, (size_t)prv_len, &ret_prv)) == NULL ||
        BN_bn2bin(priv_key_gen, prv_ptr) < 0)
        {
            ret = EXCP_ERROR(env, "Can't convert private key");
            goto done;
        }

    ERL_VALGRIND_MAKE_MEM_DEFINED(pub_ptr, pub_len);
    ERL_VALGRIND_MAKE_MEM_DEFINED(prv_ptr, prv_len);

    ret = enif_make_tuple2(env, ret_pub, ret_prv);

 done:
    if (pub_key_gen)  BN_free(pub_key_gen);
    if (priv_key_gen) BN_free(priv_key_gen);
    if (pkey) EVP_PKEY_free(pkey);
    if (pkey_gen) EVP_PKEY_free(pkey_gen);
    if (pctx) EVP_PKEY_CTX_free(pctx);
    if (pctx_gen) EVP_PKEY_CTX_free(pctx_gen);

    return ret;
}

/* Has 3_0 */
ERL_NIF_TERM dh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (OthersPublicKey, MyPrivateKey, DHParams=[P,G]) */
    ERL_NIF_TERM ret;
    ErlNifBinary ret_bin;
    size_t sz;
    int ret_bin_alloc = 0;
    int i = 0;
    OSSL_PARAM params[5];
    EVP_PKEY_CTX *own_pctx = NULL, *peer_pctx = NULL, *pctx_gen = NULL;
    EVP_PKEY *own_pkey = NULL, *peer_pkey = NULL;

    /* Fetch parameters */

    /* Build peer_pkey */
    
    if (!get_ossl_BN_param_from_bin(env, "pub",  argv[0], &params[i++]))
        assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Bad peer public key; binary expected"));

    { /*argv[2] - the lists [P,G] */
        ERL_NIF_TERM head, tail;

        head = argv[2];
        if (!get_ossl_param_from_bin_in_list(env, "p",  &head, &params[i++]))
            assign_goto(ret, err, EXCP_BADARG_N(env, 2, "Bad value of 'p'"));

        if (!get_ossl_param_from_bin_in_list(env, "g",  &head, &params[i++]))
            assign_goto(ret, err, EXCP_BADARG_N(env, 2, "Bad value of 'g'"));

        tail = head;
        if (!enif_is_empty_list(env,tail))
            assign_goto(ret, err, EXCP_BADARG_N(env, 2, "Not a two-element list"));
    }

    params[i++] = OSSL_PARAM_construct_end();

    peer_pctx = EVP_PKEY_CTX_new_from_name(NULL, "DH", NULL);

    if (EVP_PKEY_fromdata_init(peer_pctx) <= 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't init fromdata"));
    if (EVP_PKEY_fromdata(peer_pctx, &peer_pkey, EVP_PKEY_KEYPAIR, params) <= 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't do fromdata"));

    /* Build own_pkey. Just replace the pub key with the priv key in params */
    if (!get_ossl_BN_param_from_bin(env, "priv",  argv[1], &params[0]))
        assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Bad peer public key; binary expected"));

    own_pctx = EVP_PKEY_CTX_new_from_name(NULL, "DH", NULL);

    if (EVP_PKEY_fromdata_init(own_pctx) <= 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't init fromdata"));

    if (EVP_PKEY_fromdata(own_pctx, &own_pkey, EVP_PKEY_KEYPAIR, params) <= 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't do fromdata"));


    /* Derive the common secret */
    pctx_gen = EVP_PKEY_CTX_new(own_pkey, NULL);
    
    if (!EVP_PKEY_derive_init(pctx_gen))
        assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_derive_init"));

    if (!EVP_PKEY_derive_set_peer(pctx_gen, peer_pkey))
        assign_goto(ret, err, EXCP_ERROR(env, "Can't derive secret or set peer"));
    
    if (!EVP_PKEY_derive(pctx_gen, NULL, &sz))
        assign_goto(ret, err, EXCP_ERROR(env, "Can't get result size"));

    if (!enif_alloc_binary(sz, &ret_bin))
        assign_goto(ret, err, EXCP_ERROR(env, "Can't allcate binary"));
    ret_bin_alloc = 1;

    if (!EVP_PKEY_derive(pctx_gen, ret_bin.data, &ret_bin.size))
        assign_goto(ret, err, EXCP_ERROR(env, "Can't get result"));

    if (sz != ret_bin.size)
        if (!enif_realloc_binary(&ret_bin, ret_bin.size))
            assign_goto(ret, err, EXCP_ERROR(env, "Can't realloc binary"));
    
    ret = enif_make_binary(env, &ret_bin);
    ret_bin_alloc = 0;

 err:
    if (ret_bin_alloc)  enif_release_binary(&ret_bin);
    if (peer_pctx) EVP_PKEY_CTX_free(peer_pctx);
    if (peer_pkey) EVP_PKEY_free(peer_pkey);
    if (own_pctx) EVP_PKEY_CTX_free(own_pctx);
    if (own_pkey) EVP_PKEY_free(own_pkey);
    if (pctx_gen) EVP_PKEY_CTX_free(pctx_gen);
    return ret;
}

# else
/* Has not 3.0 */
ERL_NIF_TERM dh_generate_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (PrivKey|undefined, DHParams=[P,G], Mpint, Len|0) */
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
#  if defined(HAS_EVP_PKEY_CTX) && (! DISABLE_EVP_DH)
    EVP_PKEY_CTX *ctx = NULL;
    EVP_PKEY *dhkey = NULL, *params = NULL;
#  endif

    ASSERT(argc == 4);

    if (argv[0] != atom_undefined) {
        if (!get_bn_from_bin(env, argv[0], &priv_key_in))
            assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Can't get bignum from binary"));
    }
    if (!enif_get_list_cell(env, argv[1], &head, &tail))
        assign_goto(ret, err, EXCP_BADARG_N(env, 1, "List with exactly two elements expected"));
    if (!get_bn_from_bin(env, head, &dh_p))
        assign_goto(ret, err, EXCP_BADARG_N(env, 1, "Can't get bignum from binary"));

    if (!enif_get_list_cell(env, tail, &head, &tail))
        assign_goto(ret, err, EXCP_BADARG_N(env, 1, "List with exactly two elements expected"));
    if (!get_bn_from_bin(env, head, &dh_g))
        assign_goto(ret, err, EXCP_BADARG_N(env, 1, "Can't get bignum from binary"));

    if (!enif_is_empty_list(env, tail))
        assign_goto(ret, err, EXCP_BADARG_N(env, 1, "List with exactly two elements expected"));

    if (!enif_get_uint(env, argv[2], &mpint))
        assign_goto(ret, err, EXCP_BADARG_N(env, 2, "Integer expected"));
    if (mpint != 0 && mpint != 4)
        assign_goto(ret, err, EXCP_BADARG_N(env, 2, "Integer 0 or 4 expected"));

    if (!enif_get_ulong(env, argv[3], &len))
        assign_goto(ret, err, EXCP_BADARG_N(env, 3, "Integer expected"));
    if (len > LONG_MAX)
        assign_goto(ret, err, EXCP_BADARG_N(env, 3, "Too big integer"));

    /* Load dh_params with values to use by the generator.
       Mem mgmnt transferred from dh_p etc to dh_params */
    if ((dh_params = DH_new()) == NULL)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't do DH_new"));
    if (priv_key_in) {
        if (!DH_set0_key(dh_params, NULL, priv_key_in))
            assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Not accepted as private key"));
        /* On success, dh_params owns priv_key_in */
        priv_key_in = NULL;
    }
    if (!DH_set0_pqg(dh_params, dh_p, NULL, dh_g))
        assign_goto(ret, err, EXCP_BADARG_N(env, 1, "P and/or G not accepted"));
    dh_p_shared = dh_p; /* Don't free this because dh_params owns it */
    /* On success, dh_params owns dh_p and dh_g */
    dh_p = NULL;
    dh_g = NULL;

    if (len) {
        int bn_len;

        if ((bn_len = BN_num_bits(dh_p_shared)) < 0)
            assign_goto(ret, err, EXCP_ERROR(env, "BN_num_bits < 0"));
        dh_p_shared = NULL;  /* dh_params owns the reference */
        if (len >= (size_t)bn_len)
            assign_goto(ret, err, EXCP_ERROR_N(env, 3, "Too big length"));

        if (!DH_set_length(dh_params, (long)len))
            assign_goto(ret, err, EXCP_ERROR_N(env, 3, "The length is not accepted"));
    }

#  if defined(HAS_EVP_PKEY_CTX) && (! DISABLE_EVP_DH)
    if ((params = EVP_PKEY_new()) == NULL)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_new"));

   /* set the key referenced by params to dh_params... */
    if (EVP_PKEY_set1_DH(params, dh_params) != 1)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_set1_DH"));

    if ((ctx = EVP_PKEY_CTX_new(params, NULL)) == NULL)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_CTX_new"));

    if (EVP_PKEY_keygen_init(ctx) != 1)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_keygen_init"));

    if ((dhkey = EVP_PKEY_new()) == NULL)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_new"));

    /* key gen op, key written to ppkey (=last arg) */
    if (EVP_PKEY_keygen(ctx, &dhkey) != 1)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_keygen"));

    DH_free(dh_params);
    if ((dh_params = EVP_PKEY_get1_DH(dhkey)) == NULL)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_get1_DH"));

#  else
    if (!DH_generate_key(dh_params))
        assign_goto(ret, err, EXCP_ERROR(env, "Can't DH_generate_key"));
#  endif

    DH_get0_key(dh_params, &pub_key_gen, &priv_key_gen);

    if ((pub_len = BN_num_bytes(pub_key_gen)) < 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't get public key length"));
    if ((prv_len = BN_num_bytes(priv_key_gen)) < 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't get private key length"));

    if ((pub_ptr = enif_make_new_binary(env, (size_t)pub_len+mpint, &ret_pub)) == NULL)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate pub_ptr"));
    if ((prv_ptr = enif_make_new_binary(env, (size_t)prv_len+mpint, &ret_prv)) == NULL)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't allocate prv_ptr"));

    if (mpint) {
        put_uint32(pub_ptr, (unsigned int)pub_len);
        pub_ptr += 4;

        put_uint32(prv_ptr, (unsigned int)prv_len);
        prv_ptr += 4;
    }

    if (BN_bn2bin(pub_key_gen, pub_ptr) < 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't BN_bn2bin"));
    if (BN_bn2bin(priv_key_gen, prv_ptr) < 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't BN_bn2bin"));

    ERL_VALGRIND_MAKE_MEM_DEFINED(pub_ptr, pub_len);
    ERL_VALGRIND_MAKE_MEM_DEFINED(prv_ptr, prv_len);

    ret = enif_make_tuple2(env, ret_pub, ret_prv);
    goto done;

 err:
 done:
    if (priv_key_in)
        BN_free(priv_key_in);
    if (dh_p)
        BN_free(dh_p);
    if (dh_g)
        BN_free(dh_g);
    if (dh_params)
        DH_free(dh_params);

#  if defined(HAS_EVP_PKEY_CTX) && (! DISABLE_EVP_DH)
    if (ctx)
        EVP_PKEY_CTX_free(ctx);
    if (dhkey)
        EVP_PKEY_free(dhkey);
    if (params)
        EVP_PKEY_free(params);
#  endif

    return ret;
}


ERL_NIF_TERM dh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (OthersPublicKey, MyPrivateKey, DHParams=[P,G]) */
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
        assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Can't get bignum from binary"));
    if (!get_bn_from_bin(env, argv[1], &priv_key))
        assign_goto(ret, err, EXCP_BADARG_N(env, 1, "Can't get bignum from binary"));

    if (!enif_get_list_cell(env, argv[2], &head, &tail))
        assign_goto(ret, err, EXCP_BADARG_N(env, 2, "List with exactly two elements expected"));
    if (!get_bn_from_bin(env, head, &dh_p))
        assign_goto(ret, err, EXCP_BADARG_N(env, 2, "Can't get bignum from binary"));

    if (!enif_get_list_cell(env, tail, &head, &tail))
        assign_goto(ret, err, EXCP_BADARG_N(env, 2, "List with exactly two elements expected"));
    if (!get_bn_from_bin(env, head, &dh_g))
        assign_goto(ret, err, EXCP_BADARG_N(env, 2, "Can't get bignum from binary"));

    if (!enif_is_empty_list(env, tail))
        assign_goto(ret, err, EXCP_BADARG_N(env, 2, "List with exactly two elements expected"));

    /* Note: DH_set0_key() does not allow setting only the
     * private key, although DH_compute_key() does not use the
     * public key. Work around this limitation by setting
     * the public key to a copy of the private key.
     */
    if ((dummy_pub_key = BN_dup(priv_key)) == NULL)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't BN_dup"));
    if ((dh_priv = DH_new()) == NULL)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't DH_new"));

    if (!DH_set0_key(dh_priv, dummy_pub_key, priv_key))
        assign_goto(ret, err, EXCP_ERROR(env, "Can't DH_set0_key"));
    /* dh_priv owns dummy_pub_key and priv_key now */
    dummy_pub_key = NULL;
    priv_key = NULL;

    if (!DH_set0_pqg(dh_priv, dh_p, NULL, dh_g))
        assign_goto(ret, err, EXCP_BADARG_N(env, 2, "P and/or G not accepted"));
    /* dh_priv owns dh_p and dh_g now */
    dh_p = NULL;
    dh_g = NULL;

    if ((dh_size = DH_size(dh_priv)) < 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't DH_size"));
    if (!enif_alloc_binary((size_t)dh_size, &ret_bin))
        assign_goto(ret, err, EXCP_ERROR(env, "Can't allcate binary"));
    ret_bin_alloc = 1;

    if ((size = DH_compute_key(ret_bin.data, other_pub_key, dh_priv)) < 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't DH_compute_key"));
    if (size == 0)
        assign_goto(ret, err, EXCP_ERROR(env, "size == 0"));

    if ((size_t)size != ret_bin.size) {
        if (!enif_realloc_binary(&ret_bin, (size_t)size))
            assign_goto(ret, err, EXCP_ERROR(env, "Can't realloc binary"));
    }

    ret = enif_make_binary(env, &ret_bin);
    ret_bin_alloc = 0;
    goto done;

 err:
    if (ret_bin_alloc)
        enif_release_binary(&ret_bin);
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
}

# endif /* else part of HAS_3_0_API  */



#endif /* HAVE_DH */
