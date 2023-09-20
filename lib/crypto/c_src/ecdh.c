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

#include "ecdh.h"
#include "ec.h"

#if !defined(HAVE_EC)
ERL_NIF_TERM ecdh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
/* (OtherPublicKey, {CurveDef,CurveName}, My) */
{
    return EXCP_NOTSUP_N(env, 0, "EC not supported");
}

#else

# if defined(HAS_3_0_API)
#  include "bn.h"

ERL_NIF_TERM ecdh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
/* (OtherPublicKey, {CurveDef,CurveName}, My) */
{
    ERL_NIF_TERM ret = atom_undefined;
    ErlNifBinary ret_bin;
    size_t sz;
    int ret_bin_alloc = 0;
    int i = 0, i_key = 0;
    OSSL_PARAM params[15];
    struct get_curve_def_ctx gcd;
    EVP_PKEY_CTX *own_pctx = NULL, *peer_pctx = NULL, *pctx_gen = NULL;
    EVP_PKEY *own_pkey = NULL, *peer_pkey = NULL;
    int err;
    
    /**** Fetch parameters ****/

    /* Build peer_pkey */
    i_key = i;
    if (!get_ossl_octet_string_param_from_bin(env, "pub",  argv[0], &params[i++]))
        assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Bad peer public key; binary expected"));

    /* Build the remote public key in peer_pkey */
    peer_pctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", NULL);

    gcd.use_curve_name = 1;
retry_without_name:
    /* Curve definition/name */
    if (!get_curve_definition(env, &ret, argv[1], params, &i, NULL, &gcd))
        goto err;

    /* End of params */
    params[i++] = OSSL_PARAM_construct_end();

    if (EVP_PKEY_fromdata_init(peer_pctx) <= 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't init fromdata"));
    
    if (EVP_PKEY_fromdata(peer_pctx, &peer_pkey, EVP_PKEY_PUBLIC_KEY, params) <= 0) {
        if (gcd.use_curve_name) {
            gcd.use_curve_name = 0;
            i = 1;
            goto retry_without_name;
        }
        assign_goto(ret, err, EXCP_ERROR(env, "Can't do fromdata"));
    }

    if (!peer_pkey)
        assign_goto(ret, err, EXCP_ERROR(env, "No peer_pkey"));

    /* Build the local private (and public) key in own_pkey */

    /* Just replace the pub key with the priv key in params; the
       curve definition is of course the same
    */
    if (!get_ossl_BN_param_from_bin(env, "priv",  argv[2], &params[i_key]))
        assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Bad peer public key; binary expected"));

    own_pctx = EVP_PKEY_CTX_new_from_name(NULL, "EC", NULL);

    if (EVP_PKEY_fromdata_init(own_pctx) <= 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't init fromdata"));

    if (EVP_PKEY_fromdata(own_pctx, &own_pkey, EVP_PKEY_KEYPAIR, params) <= 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't do fromdata"));

    if (!own_pkey)
        assign_goto(ret, err, EXCP_ERROR(env, "No own_pkey"));

    /**** Derive the common secret from own_pkey and peer_pkey ****/

    if (!(pctx_gen = EVP_PKEY_CTX_new(own_pkey, NULL)))
        assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_CTX_init"));
        
    if (EVP_PKEY_derive_init(pctx_gen) <= 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't EVP_PKEY_derive_init"));

    if ((err = EVP_PKEY_derive_set_peer_ex(pctx_gen, peer_pkey, 0)) <= 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't derive secret or set peer"));
    
    if ((err = EVP_PKEY_derive(pctx_gen, NULL, &sz)) <= 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Can't get result size"));

    if (!enif_alloc_binary(sz, &ret_bin))
        assign_goto(ret, err, EXCP_ERROR(env, "Can't allcate binary"));
    ret_bin_alloc = 1;

    if ((err = EVP_PKEY_derive(pctx_gen, ret_bin.data, &ret_bin.size)) <=0)
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

# endif /* HAS_3_0_API */



# if ! defined(HAS_3_0_API)

ERL_NIF_TERM ecdh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
/* (OtherPublicKey, {CurveDef,_CurveName}, My) */
{
    ERL_NIF_TERM ret = atom_undefined;
    unsigned char *p;
    EC_KEY* key = NULL;
    int degree;
    size_t field_size;
    EC_GROUP *group = NULL;
    const BIGNUM *priv_key;
    EC_POINT *my_ecpoint = NULL;
    EC_KEY *other_ecdh = NULL;

    if (!get_ec_key_sz(env, argv[1], argv[2], atom_undefined, &key, NULL)) // my priv key
        assign_goto(ret, err, EXCP_BADARG_N(env, 2, "Couldn't get local key"));
    
    if ((group = EC_GROUP_dup(EC_KEY_get0_group(key))) == NULL)
         assign_goto(ret, err, EXCP_ERROR(env, "Couldn't duplicate EC key"));

    priv_key = EC_KEY_get0_private_key(key);

    if (!term2point(env, argv[0], group, &my_ecpoint))
        assign_goto(ret, err, EXCP_BADARG_N(env, 0, "Couldn't get ecpoint"));

    if ((other_ecdh = EC_KEY_new()) == NULL)
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't allocate EC_KEY"));
    
    if (!EC_KEY_set_group(other_ecdh, group))
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't set group"));

    if (!EC_KEY_set_private_key(other_ecdh, priv_key))
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't set private key"));

    if ((degree = EC_GROUP_get_degree(group)) <= 0)
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't get degree"));

    field_size = (size_t)degree;
    if ((p = enif_make_new_binary(env, (field_size+7)/8, &ret)) == NULL)
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't allocate binary"));

    if (ECDH_compute_key(p, (field_size+7)/8, my_ecpoint, other_ecdh, NULL) < 1)
        assign_goto(ret, err, EXCP_ERROR(env, "Couldn't compute key"));

 err:
    if (group)
        EC_GROUP_free(group);
    if (my_ecpoint)
        EC_POINT_free(my_ecpoint);
    if (other_ecdh)
        EC_KEY_free(other_ecdh);
    if (key)
        EC_KEY_free(key);

    return ret;
}
# endif /* ! HAS_3_0_API */
#endif /* HAVE_EC */
