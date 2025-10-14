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

#include "mac.h"
#include "cipher.h"
#include "cmac.h"
#include "common.h"
#include "hmac.h"
#include "info.h"

#include "algorithms_digest.h"
#include "algorithms_mac.h"
#include "algorithms_cipher.h"

/***************************
 Mandatory prototypes
***************************/

ERL_NIF_TERM mac_one_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

ERL_NIF_TERM mac_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

/*******************************************************************
 *
 * Mac nif
 *
 ******************************************************************/
ERL_NIF_TERM mac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (MacType, SubType, Key, Text) */
    ErlNifBinary  text;

    if (!enif_inspect_iolist_as_binary(env, argv[3], &text))
        return EXCP_BADARG_N(env, 3, "Bad text");

    if (text.size > INT_MAX)
        return EXCP_BADARG_N(env, 3, "Too long text");

    /* Run long jobs on a dirty scheduler to not block the current emulator thread */
    if (text.size > MAX_BYTES_TO_NIF) {
        return enif_schedule_nif(env, "mac_one_time",
                                 ERL_NIF_DIRTY_JOB_CPU_BOUND,
                                 mac_one_time, argc, argv);
    }

    return mac_one_time(env, argc, argv);
}



ERL_NIF_TERM mac_one_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (MacType, SubType, Key, Text) */

    mac_type_C* macp;
    ErlNifBinary key_bin, text;
    int ret_bin_alloc = 0;
    ERL_NIF_TERM return_term;
    ErlNifBinary ret_bin;
#if defined(HAS_3_0_API)
    const char *name;
    const char *subalg;
    unsigned char *out = NULL;
    size_t outlen;
#else
    /* Old style */
    const EVP_MD *md = NULL;
# ifdef HAS_EVP_PKEY_CTX
    size_t size;
    EVP_PKEY *pkey = NULL;
    EVP_MD_CTX *mctx = NULL;
# endif
#endif

    /*---------------------------------
      Get common indata and validate it
    */
    if (!enif_inspect_iolist_as_binary(env, argv[2], &key_bin)) {
        return_term = EXCP_BADARG_N(env, 2, "Bad key");
        goto err;
    }

    if (!enif_inspect_iolist_as_binary(env, argv[3], &text)) {
        return_term = EXCP_BADARG_N(env, 3, "Bad text");
        goto err;
    }

    macp = find_mac_type_by_name_keylen(env, argv[0], key_bin.size);
    if (!macp) {
        mac_type_C* macp2 = find_mac_type_by_name(env, argv[0]);
        if (!macp2)
            return_term = EXCP_BADARG_N(env, 0, "Unknown mac algorithm");
        else
            return_term = EXCP_BADARG_N(env, 2, "Bad key length");
        goto err;
    }

    if (is_mac_forbidden_in_fips(macp)) {
        return_term = EXCP_NOTSUP_N(env, 0, "MAC algorithm forbidden in FIPS");
        goto err;
    }

    /*--------------------------------------------------
      Algorithm dependent indata checking and computation.
      If EVP_PKEY is available, only set the pkey variable
      and do the computation after the switch statement.
      If not available, do the low-level calls in the 
      corresponding case part
    */
    switch (get_mac_type_mactype(macp)) {
        /********
         * HMAC *
         ********/
    case HMAC_mac:
        {
            digest_type_C* digp = get_digest_type(env, argv[1]);

            if (digp == NULL)
                {
                    return_term = EXCP_BADARG_N(env, 1, "Bad digest algorithm for HMAC");
                    goto err;
                }
            if (is_digest_forbidden_in_fips(digp))
                {
                    return_term = EXCP_NOTSUP_N(env, 1, "Digest algorithm for HMAC forbidden in FIPS");
                    goto err;
                }

#if defined(HAS_3_0_API)
            name = "HMAC";
            subalg = get_digest_type_str_v3(digp);
#else
            /* Old style */
            if (get_digest_type_resource(digp) == NULL)
                {
                    return_term = EXCP_NOTSUP_N(env, 1, "Unsupported digest algorithm");
                    goto err;
                }
            md = get_digest_type_resource(digp);
# if defined(HAS_EVP_PKEY_CTX) && (! DISABLE_EVP_HMAC)
#  ifdef HAVE_PKEY_new_raw_private_key
            /* Preferred for new applications according to EVP_PKEY_new_mac_key(3) */
            pkey = EVP_PKEY_new_raw_private_key(EVP_PKEY_HMAC, /*engine*/ NULL, key_bin.data,  key_bin.size);
#  else
            /* Available in older versions */
            pkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, /*engine*/ NULL, key_bin.data,  key_bin.size);
#  endif

# else
            if (!hmac_low_level(env, md, key_bin, text, &ret_bin, &ret_bin_alloc, &return_term))
                goto err;
            else
                goto success;
# endif
#endif
        }
        break;


        /********
         * CMAC *
         ********/
#ifdef HAVE_CMAC
    case CMAC_mac: {
            const cipher_type_C *cipherp = get_cipher_type(env, argv[1], key_bin.size);
            if (!cipherp)
                { /* Something went wrong. Find out what by retrying in another way. */
                    if (!get_cipher_type_no_key(env, argv[1]))
                        return_term = EXCP_BADARG_N(env, 1, "Unknown cipher");
                    else
                        /* Cipher exists, so it must be the key size that is wrong */
                        return_term = EXCP_BADARG_N(env, 2, "Bad key size");
                    goto err;
                }
            
            if (is_cipher_forbidden_in_fips(cipherp))
                {
                    return_term = EXCP_NOTSUP_N(env, 1, "Cipher algorithm not supported in FIPS");
                    goto err;
                }

            if (get_cipher_type_resource(cipherp) == NULL)
                {
                    return_term = EXCP_NOTSUP_N(env, 1, "Unsupported cipher algorithm");
                    goto err;
                }

# if defined(HAS_3_0_API)
            name = "CMAC";
            subalg = get_cipher_type_str_v3(cipherp);
# else
            /* Old style, pre 3.0 */
#  ifdef HAVE_EVP_PKEY_new_CMAC_key
            pkey = EVP_PKEY_new_CMAC_key(/*engine*/ NULL, key_bin.data,  key_bin.size, get_cipher_type_resource(cipherp));
#  else
            if (!cmac_low_level(env, key_bin, get_cipher_type_resource(cipherp), text, &ret_bin, &ret_bin_alloc, &return_term))
                goto err;
            else
                goto success;
#  endif
# endif
        }
        break;
#endif /* HAVE_CMAC */


        /************
         * POLY1305 *
         ************/
#ifdef HAVE_POLY1305
    case POLY1305_mac:
# if defined(HAS_3_0_API)
        name = "POLY1305";
        subalg = NULL;
# else
        /* Old style */
        /* poly1305 implies that EVP_PKEY_new_raw_private_key exists */
        pkey = EVP_PKEY_new_raw_private_key(EVP_PKEY_POLY1305, /*engine*/ NULL, key_bin.data,  key_bin.size);
# endif
        break;
#endif

        /***************
         * Unknown MAC *
         ***************/
    case NO_mac:
    default:
        /* We know that this mac is supported with some version(s) of cryptolib, but not here */
        return_term = EXCP_NOTSUP_N(env, 1, "Unsupported mac algorithm");
        goto err;
    }

#if defined(HAS_3_0_API)
    /*-----------------------------------------
      Common computations when we have 3.0 API
    */

    if (!(out = EVP_Q_mac(NULL, name, NULL,
                          subalg, NULL,
                          key_bin.data, key_bin.size,
                          text.data, text.size,
                          NULL, 0, &outlen)))
        assign_goto(return_term, err, EXCP_ERROR(env, "Couldn't get mac"));

    if (!enif_alloc_binary(outlen, &ret_bin))
        assign_goto(return_term, err, EXCP_ERROR(env, "Alloc binary"));
    ret_bin_alloc = 1;

    memcpy(ret_bin.data, out, outlen);
#else
    /*-----------------------------------------
      Common computations when we have EVP_PKEY
    */
# ifdef HAS_EVP_PKEY_CTX
    if (!pkey)
        {
            return_term = EXCP_ERROR(env, "EVP_PKEY_key creation");
            goto err;
        }
    
    if ((mctx = EVP_MD_CTX_new()) == NULL)
        {
            return_term = EXCP_ERROR(env, "EVP_MD_CTX_new");
            goto err;
        }

    if (EVP_DigestSignInit(mctx, /*&pctx*/ NULL, md, /*engine*/ NULL, pkey) != 1)
        {
            return_term = EXCP_ERROR(env, "EVP_DigestSign");
            goto err;
        }

#  ifdef HAVE_DigestSign_as_single_op
    if (EVP_DigestSign(mctx, NULL, &size, text.data, text.size) != 1)
        {
            return_term = EXCP_ERROR(env, "Can't get sign size");
            goto err;
        }
#  else
    if (EVP_DigestSignUpdate(mctx, text.data, text.size) != 1)
        {
            return_term = EXCP_ERROR(env, "EVP_DigestSignUpdate");
            goto err;
        }

    if (EVP_DigestSignFinal(mctx, NULL, &size) != 1)
        {
            return_term = EXCP_ERROR(env, "Can't get sign size");
            goto err;
        }
#  endif
   
    if (!enif_alloc_binary(size, &ret_bin))
        {
            return_term = EXCP_ERROR(env, "Alloc binary");
            goto err;
        }
    ret_bin_alloc = 1;

#  ifdef HAVE_DigestSign_as_single_op
    if (EVP_DigestSign(mctx, ret_bin.data, &size, text.data, text.size) != 1)
#  else    
    if (EVP_DigestSignFinal(mctx, ret_bin.data, &size) != 1)
#  endif
        {
            return_term = EXCP_ERROR(env, "Signing");
            goto err;
        }

    goto success; /* The label "success:" could be left without any "goto success"
                     in some combination of flags. This prevents a compiler warning
                  */
# endif /* ifdef HAS_EVP_PKEY_CTX */
 success:
#endif

    /****************************
     Exit when we got a signature
    *****************************/
    CONSUME_REDS(env, text);

    return_term = enif_make_binary(env, &ret_bin);
    ret_bin_alloc = 0;

 err:
#if defined(HAS_3_0_API)
    if (out)
        OPENSSL_free(out);
#else
            /* Old style */
# ifdef HAS_EVP_PKEY_CTX
    if (pkey)
        EVP_PKEY_free(pkey);
    if (mctx)
        EVP_MD_CTX_free(mctx);
# endif
#endif

    if (ret_bin_alloc)
        enif_release_binary(&ret_bin);

    return return_term;
}


/*******************************************************************
 *
 * Mac ctx
 *
 ******************************************************************/

struct mac_context
{
#if defined(HAS_3_0_API)
    EVP_MAC_CTX *ctx;
#else
    EVP_MD_CTX *ctx;
#endif
};

static ErlNifResourceType* mac_context_rtype;

static void mac_context_dtor(ErlNifEnv* env, struct mac_context*);

int init_mac_ctx(ErlNifEnv *env, ErlNifBinary* rt_buf) {
    mac_context_rtype = enif_open_resource_type(env, NULL,
                                                resource_name("mac_context", rt_buf),
                                                (ErlNifResourceDtor*) mac_context_dtor,
                                                ERL_NIF_RT_CREATE|ERL_NIF_RT_TAKEOVER,
                                                NULL);
    if (mac_context_rtype == NULL)
        goto err;

    return 1;

 err:
    PRINTF_ERR0("CRYPTO: Could not open resource type 'mac_context'");
    return 0;
}


static void mac_context_dtor(ErlNifEnv* env, struct mac_context *obj)
{
    if (obj == NULL)
        return;

    if (obj->ctx) {
#if defined(HAS_3_0_API)
        EVP_MAC_CTX_free(obj->ctx);
#else
        EVP_MD_CTX_free(obj->ctx);
#endif
    }
}

/*******************************************************************
 *
 * mac_init, mac_update, mac_final nifs
 *
 ******************************************************************/

ERL_NIF_TERM mac_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (MacType, SubType, Key) */

#if ! defined(HAS_EVP_PKEY_CTX) && ! defined(HAS_3_0_API)
    /* Only realy low-level available */
    if (argv[0] != atom_hmac)
        return EXCP_NOTSUP_N(env, 0, "Unsupported mac algorithm");

    return hmac_init_nif(env, argc, argv);

#else
    /* EVP_PKEY_CTX is available or even the 3.0 API */
    struct mac_context  *obj = NULL;
    mac_type_C* macp;
    ErlNifBinary key_bin;
    ERL_NIF_TERM return_term;
# if defined(HAS_3_0_API)
    const char *digest = NULL;
    const char *cipher = NULL;
    OSSL_PARAM params[3];
    size_t params_n = 0;
# else
    /* EVP_PKEY_CTX is available */
    const EVP_MD *md = NULL;
    EVP_PKEY *pkey = NULL;
# endif

    /*---------------------------------
      Get common indata and validate it
    */
    if (!enif_inspect_iolist_as_binary(env, argv[2], &key_bin)) {
        return_term = EXCP_BADARG_N(env, 2, "Bad key");
        goto err;
    }

    macp = find_mac_type_by_name_keylen(env, argv[0], key_bin.size);
    if (!macp) {
        if (find_mac_type_by_name(env, argv[0]) == NULL)
            return_term = EXCP_BADARG_N(env, 0, "Unknown mac algorithm");
        else
            return_term = EXCP_BADARG_N(env, 2, "Bad key length");
        goto err;
    }

    if (is_mac_forbidden_in_fips(macp)) {
        return_term = EXCP_NOTSUP_N(env, 0, "MAC algorithm forbidden in FIPS");
        goto err;
    }

    /*--------------------------------------------------
      Algorithm dependent indata checking and computation.
      If EVP_PKEY is available, only set the pkey variable
      and do the computation after the switch statement.
      If not available, do the low-level calls in the 
      corresponding case part
    */
    switch (get_mac_type_mactype(macp)) {
        /********
         * HMAC *
         ********/
    case HMAC_mac:
        {
            digest_type_C* digp = get_digest_type(env, argv[1]);
            if (digp == NULL)
                {
                    return_term = EXCP_BADARG_N(env, 1, "Bad digest algorithm for HMAC");
                    goto err;
                }
            if (is_digest_forbidden_in_fips(digp))
                {
                    return_term = EXCP_NOTSUP_N(env, 1, "Digest algorithm for HMAC forbidden in FIPS");
                    goto err;
                }
# if defined(HAS_3_0_API)
            digest = get_digest_type_str_v3(digp);
# else
            if (get_digest_type_resource(digp) == NULL)
                {
                    return_term = EXCP_NOTSUP_N(env, 1, "Unsupported digest algorithm");
                    goto err;
                }
            md = get_digest_type_resource(digp);

#  ifdef HAVE_PKEY_new_raw_private_key
            /* Preferred for new applications according to EVP_PKEY_new_mac_key(3) */
            pkey = EVP_PKEY_new_raw_private_key(EVP_PKEY_HMAC, /*engine*/ NULL, key_bin.data,  key_bin.size);
#  else
            /* Available in older versions */
            pkey = EVP_PKEY_new_mac_key(EVP_PKEY_HMAC, /*engine*/ NULL, key_bin.data,  key_bin.size);
#  endif
#endif
        }
        break;


        /********
         * CMAC *
         ********/
# if defined(HAVE_CMAC) && defined(HAVE_EVP_PKEY_new_CMAC_key)
    case CMAC_mac:
        {
            const struct cipher_type_t *cipherp = get_cipher_type(env, argv[1], key_bin.size);
            if (!cipherp)
                { /* Something went wrong. Find out what by retrying in another way. */
                    if (!get_cipher_type_no_key(env, argv[1]))
                        return_term = EXCP_BADARG_N(env, 1, "Unknown cipher");
                    else
                        /* Cipher exists, so it must be the key size that is wrong */
                        return_term = EXCP_BADARG_N(env, 2, "Bad key size");
                    goto err;
                }
            
            if (is_cipher_forbidden_in_fips(cipherp))
                {
                    return_term = EXCP_NOTSUP_N(env, 1, "Cipher algorithm not supported in FIPS");
                    goto err;
                }

            if (get_cipher_type_resource(cipherp) == NULL)
                {
                    return_term = EXCP_NOTSUP_N(env, 1, "Unsupported cipher algorithm");
                    goto err;
                }

#  if defined(HAS_3_0_API)
            cipher = get_cipher_type_str_v3(cipherp);
#  else
            /* Old style */
            pkey = EVP_PKEY_new_CMAC_key(/*engine*/ NULL, key_bin.data,  key_bin.size, get_cipher_type_resource(cipherp));
#  endif
        }
        break;
# endif /* HAVE_CMAC && HAVE_EVP_PKEY_new_CMAC_key */


        /************
         * POLY1305 *
         ************/
# ifdef HAVE_POLY1305
    case POLY1305_mac:
#  if !defined(HAS_3_0_API)
        /* Old style */
        /* poly1305 implies that EVP_PKEY_new_raw_private_key exists */
        pkey = EVP_PKEY_new_raw_private_key(EVP_PKEY_POLY1305, /*engine*/ NULL, key_bin.data,  key_bin.size);
#  endif
        break;
# endif


        /***************
         * Unknown MAC *
         ***************/
    case NO_mac:
    default:
        /* We know that this mac is supported with some version(s) of cryptolib */
        return_term = EXCP_NOTSUP_N(env, 0, "Unsupported mac algorithm");
        goto err;
    }

# if defined(HAS_3_0_API)
    /*-----------------------------------------
      Common computations when we have 3.0 API
    */
    if (!get_mac_type_resource(macp)) {
        assign_goto(return_term, err, EXCP_NOTSUP_N(env, 0, "Unsupported mac algorithm"));
    }

    if (cipher != NULL)
        params[params_n++] =
            OSSL_PARAM_construct_utf8_string("cipher", (char*)cipher, 0);
    if (digest != NULL)
        params[params_n++] =
            OSSL_PARAM_construct_utf8_string("digest", (char*)digest, 0);
    params[params_n] = OSSL_PARAM_construct_end();

    if ((obj = enif_alloc_resource(mac_context_rtype, sizeof(struct mac_context))) == NULL)
        assign_goto(return_term, err, EXCP_ERROR(env, "Can't allocate mac_context_rtype"));

    obj->ctx = EVP_MAC_CTX_new(get_mac_type_resource(macp));
    if (!obj->ctx)
        assign_goto(return_term, err, EXCP_ERROR(env, "Can't create EVP_MAC_CTX"));
    
    if (!EVP_MAC_init(obj->ctx, key_bin.data, key_bin.size, params))
        assign_goto(return_term, err, EXCP_ERROR(env, "Can't initialize EVP_MAC_CTX"));

    
# else
    /*-----------------------------------------
      Common computations when we have EVP_PKEY_CTX but not 3.0 API
    */
    if (!pkey)
        {
            return_term = EXCP_ERROR(env, "EVP_PKEY_key creation");
            goto err;
        }
    
    if ((obj = enif_alloc_resource(mac_context_rtype, sizeof(struct mac_context))) == NULL)
        {
            return_term = EXCP_ERROR(env, "Can't allocate mac_context_rtype");
            goto err;
        }

    if ((obj->ctx = EVP_MD_CTX_new()) == NULL)
        {
            return_term = EXCP_ERROR(env, "EVP_MD_CTX_new");
            goto err;
        }

    if (EVP_DigestSignInit(obj->ctx, /*&pctx*/ NULL, md, /*engine*/ NULL, pkey) != 1)
        {
            return_term = EXCP_ERROR(env, "EVP_DigestSign");
            goto err;
        }

# endif

    return_term = enif_make_resource(env, obj);

 err:

    if (obj)
        enif_release_resource(obj);

# if ! defined(HAS_3_0_API)
    if (pkey)
        EVP_PKEY_free(pkey);
# endif

    return return_term;

#endif
}



ERL_NIF_TERM mac_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Ref, Text) */
    ErlNifBinary  text;

    if (!enif_inspect_iolist_as_binary(env, argv[1], &text))
        return EXCP_BADARG_N(env, 1, "Bad text");

    if (text.size > INT_MAX)
        return EXCP_BADARG_N(env, 1, "Too long text");

    /* Run long jobs on a dirty scheduler to not block the current emulator thread */
    if (text.size > MAX_BYTES_TO_NIF) {
        return enif_schedule_nif(env, "mac_update",
                                 ERL_NIF_DIRTY_JOB_CPU_BOUND,
                                 mac_update, argc, argv);
    }

    return mac_update(env, argc, argv);
}


ERL_NIF_TERM mac_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Ref, Text) */
#if defined(HAS_EVP_PKEY_CTX) || defined(HAS_3_0_API)
    struct mac_context *obj = NULL;
    ErlNifBinary text;

    if (!enif_get_resource(env, argv[0], (ErlNifResourceType*)mac_context_rtype, (void**)&obj))
        return EXCP_BADARG_N(env, 0, "Bad ref");

    if (!enif_inspect_iolist_as_binary(env, argv[1], &text))
        return EXCP_BADARG_N(env, 1, "Bad text");

# if defined(HAS_3_0_API)
    if (!EVP_MAC_update(obj->ctx, text.data, text.size))
# else
    if (EVP_DigestSignUpdate(obj->ctx, text.data, text.size) != 1)
# endif
        return EXCP_ERROR(env, "mac update");

    CONSUME_REDS(env, text);
    return argv[0];

#else
    return hmac_update_nif(env, argc, argv);
#endif
}



ERL_NIF_TERM mac_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Ref) */
#if defined(HAS_EVP_PKEY_CTX) || defined(HAS_3_0_API)
    struct mac_context *obj;
    size_t size;
    ErlNifBinary ret_bin;
    
    if (!enif_get_resource(env, argv[0], (ErlNifResourceType*)mac_context_rtype, (void**)&obj))
        return EXCP_BADARG_N(env, 0, "Bad ref");

# if defined(HAS_3_0_API)
    if (!EVP_MAC_final(obj->ctx, NULL, &size, 0))
# else
    if (EVP_DigestSignFinal(obj->ctx, NULL, &size) != 1)
# endif
        return EXCP_ERROR(env, "Can't get sign size");
   
    if (!enif_alloc_binary(size, &ret_bin))
        return EXCP_ERROR(env, "Alloc binary");

# if defined(HAS_3_0_API)
    if (!EVP_MAC_final(obj->ctx, ret_bin.data, &size, size))
# else
    if (EVP_DigestSignFinal(obj->ctx, ret_bin.data, &size) != 1)
#endif
        {
            enif_release_binary(&ret_bin);
            return EXCP_ERROR(env, "Signing");
        }
 
    return enif_make_binary(env, &ret_bin);

#else
    return hmac_final_nif(env, argc, argv);
#endif
}

