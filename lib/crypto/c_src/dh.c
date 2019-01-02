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
    DH *dh_params = NULL;
    int mpint; /* 0 or 4 */

    {
        ERL_NIF_TERM head, tail;
        BIGNUM
            *dh_p = NULL,
            *dh_g = NULL,
            *priv_key_in = NULL;
        unsigned long
            len = 0;

        if (!(get_bn_from_bin(env, argv[0], &priv_key_in)
              || argv[0] == atom_undefined)
            || !enif_get_list_cell(env, argv[1], &head, &tail)
            || !get_bn_from_bin(env, head, &dh_p)
            || !enif_get_list_cell(env, tail, &head, &tail)
            || !get_bn_from_bin(env, head, &dh_g)
            || !enif_is_empty_list(env, tail)
            || !enif_get_int(env, argv[2], &mpint) || (mpint & ~4)
            || !enif_get_ulong(env, argv[3], &len)

            /* Load dh_params with values to use by the generator.
               Mem mgmnt transfered from dh_p etc to dh_params */
            || !(dh_params = DH_new())
            || (priv_key_in && !DH_set0_key(dh_params, NULL, priv_key_in))
            || !DH_set0_pqg(dh_params, dh_p, NULL, dh_g)
            ) {
            if (priv_key_in) BN_free(priv_key_in);
            if (dh_p) BN_free(dh_p);
            if (dh_g) BN_free(dh_g);
            if (dh_params) DH_free(dh_params);
            return enif_make_badarg(env);
        }

        if (len) {
            if (len < BN_num_bits(dh_p))
                DH_set_length(dh_params, len);
            else {
                if (priv_key_in) BN_free(priv_key_in);
                if (dh_p) BN_free(dh_p);
                if (dh_g) BN_free(dh_g);
                if (dh_params) DH_free(dh_params);
                return enif_make_badarg(env);
            }
        }
    }

#ifdef HAS_EVP_PKEY_CTX
    {
        EVP_PKEY_CTX *ctx;
        EVP_PKEY *dhkey, *params;
        int success;

        params = EVP_PKEY_new();
        success = EVP_PKEY_set1_DH(params, dh_params);   /* set the key referenced by params to dh_params... */
        DH_free(dh_params);                              /* ...dh_params (and params) must be freed */
        if (!success) return atom_error;

        ctx = EVP_PKEY_CTX_new(params, NULL);
        EVP_PKEY_free(params);
        if (!ctx) {
            return atom_error;
        }

        if (!EVP_PKEY_keygen_init(ctx)) {
            /* EVP_PKEY_CTX_free(ctx); */
            return atom_error;
        }

        dhkey = EVP_PKEY_new();
        if (!EVP_PKEY_keygen(ctx, &dhkey)) {         /* "performs a key generation operation, the ... */
                                                     /*... generated key is written to ppkey." (=last arg) */
             /* EVP_PKEY_CTX_free(ctx); */
             /* EVP_PKEY_free(dhkey); */
             return atom_error;
        }

        dh_params = EVP_PKEY_get1_DH(dhkey); /* return the referenced key. dh_params and dhkey must be freed */
        EVP_PKEY_free(dhkey);
        if (!dh_params) {
            /* EVP_PKEY_CTX_free(ctx); */
            return atom_error;
        }
        EVP_PKEY_CTX_free(ctx);
    }
#else
    if (!DH_generate_key(dh_params)) return atom_error;
#endif
    {
        unsigned char *pub_ptr, *prv_ptr;
        int pub_len, prv_len;
        ERL_NIF_TERM ret_pub, ret_prv;
        const BIGNUM *pub_key_gen, *priv_key_gen;

        DH_get0_key(dh_params,
                    &pub_key_gen, &priv_key_gen); /* Get pub_key_gen and priv_key_gen.
                                                     "The values point to the internal representation of
                                                     the public key and private key values. This memory
                                                     should not be freed directly." says man */
        pub_len = BN_num_bytes(pub_key_gen);
        prv_len = BN_num_bytes(priv_key_gen);
        pub_ptr = enif_make_new_binary(env, pub_len+mpint, &ret_pub);
        prv_ptr = enif_make_new_binary(env, prv_len+mpint, &ret_prv);
        if (mpint) {
            put_int32(pub_ptr, pub_len); pub_ptr += 4;
            put_int32(prv_ptr, prv_len); prv_ptr += 4;
        }
        BN_bn2bin(pub_key_gen, pub_ptr);
        BN_bn2bin(priv_key_gen, prv_ptr);
        ERL_VALGRIND_MAKE_MEM_DEFINED(pub_ptr, pub_len);
        ERL_VALGRIND_MAKE_MEM_DEFINED(prv_ptr, prv_len);

        DH_free(dh_params);

        return enif_make_tuple2(env, ret_pub, ret_prv);
    }
}

ERL_NIF_TERM dh_compute_key_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (OthersPublicKey, MyPrivateKey, DHParams=[P,G]) */
    BIGNUM *other_pub_key = NULL,
        *dh_p = NULL,
        *dh_g = NULL;
    DH *dh_priv = DH_new();

    /* Check the arguments and get
          my private key (dh_priv),
          the peer's public key (other_pub_key),
          the parameters p & q
    */

    {
        BIGNUM *dummy_pub_key = NULL,
               *priv_key = NULL;
        ERL_NIF_TERM head, tail;

        if (!get_bn_from_bin(env, argv[0], &other_pub_key)
            || !get_bn_from_bin(env, argv[1], &priv_key)
            || !enif_get_list_cell(env, argv[2], &head, &tail)
            || !get_bn_from_bin(env, head, &dh_p)
            || !enif_get_list_cell(env, tail, &head, &tail)
            || !get_bn_from_bin(env, head, &dh_g)
            || !enif_is_empty_list(env, tail)

            /* Note: DH_set0_key() does not allow setting only the
             * private key, although DH_compute_key() does not use the
             * public key. Work around this limitation by setting
             * the public key to a copy of the private key.
             */
            || !(dummy_pub_key = BN_dup(priv_key))
            || !DH_set0_key(dh_priv, dummy_pub_key, priv_key)
            || !DH_set0_pqg(dh_priv, dh_p, NULL, dh_g)
            ) {
            if (dh_p) BN_free(dh_p);
            if (dh_g) BN_free(dh_g);
            if (other_pub_key) BN_free(other_pub_key);
            if (dummy_pub_key) BN_free(dummy_pub_key);
            if (priv_key) BN_free(priv_key);
            return enif_make_badarg(env);
        }
    }
    {
        ErlNifBinary ret_bin;
        int size;

        enif_alloc_binary(DH_size(dh_priv), &ret_bin);
        size = DH_compute_key(ret_bin.data, other_pub_key, dh_priv);
        BN_free(other_pub_key);
        DH_free(dh_priv);
        if (size<=0) {
            enif_release_binary(&ret_bin);
            return atom_error;
        }

        if (size != ret_bin.size) enif_realloc_binary(&ret_bin, size);
        return enif_make_binary(env, &ret_bin);
    }
}
