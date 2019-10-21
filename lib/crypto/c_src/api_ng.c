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

#include "api_ng.h"
#include "aes.h"
#include "cipher.h"

/*
 * A unified set of functions for encryption/decryption.
 *
 */
ERL_NIF_TERM ng_crypto_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM ng_crypto_one_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#ifdef HAVE_ECB_IVEC_BUG
    /* <= 0.9.8l returns faulty ivec length */
# define GET_IV_LEN(Ciph) ((Ciph)->flags & ECB_BUG_0_9_8L) ? 0 : EVP_CIPHER_iv_length((Ciph)->cipher.p)
#else
# define GET_IV_LEN(Ciph) EVP_CIPHER_iv_length((Ciph)->cipher.p)
#endif

/*************************************************************************/
/* Get the arguments for the initialization of the EVP_CIPHER_CTX. Check */
/* them and initialize that context.                                     */
/*************************************************************************/
static int get_init_args(ErlNifEnv* env,
                         struct evp_cipher_ctx *ctx_res,
                         const ERL_NIF_TERM cipher_arg,
                         const ERL_NIF_TERM key_arg, 
                         const ERL_NIF_TERM ivec_arg,
                         const ERL_NIF_TERM encflg_arg,
                         const struct cipher_type_t **cipherp,
                         ERL_NIF_TERM *return_term)
{
    int ivec_len;
    ErlNifBinary key_bin;
    ErlNifBinary ivec_bin;
    int encflg;                    

    ctx_res->ctx = NULL; /* For testing if *ctx should be freed after errors */
#if !defined(HAVE_EVP_AES_CTR)
    ctx_res->env = NULL; /* For testing if *env should be freed after errors */
#endif

    /* Fetch the flag telling if we are going to encrypt (=true) or decrypt (=false) */
    if (encflg_arg == atom_true)
        encflg = 1;
    else if (encflg_arg == atom_false)
        encflg = 0;
    else if (encflg_arg == atom_undefined)
        /* For compat funcs in crypto.erl */
        encflg = -1;
    else
        {
            *return_term = EXCP_BADARG(env, "Bad enc flag");
            goto err;
        }

    /* Fetch the key */
    if (!enif_inspect_iolist_as_binary(env, key_arg, &key_bin))
        {
            *return_term = EXCP_BADARG(env, "Bad key");
            goto err;
        }

    /* Fetch cipher type */
    if (!enif_is_atom(env, cipher_arg))
        {
            *return_term = EXCP_BADARG(env, "Cipher id is not an atom");
            goto err;
        }

    if (!(*cipherp = get_cipher_type(cipher_arg, key_bin.size)))
        {
            if (!get_cipher_type_no_key(cipher_arg))
                *return_term = EXCP_BADARG(env, "Unknown cipher");
            else
                *return_term = EXCP_BADARG(env, "Bad key size");
            goto err;
        }

    if ((*cipherp)->flags &  AEAD_CIPHER)
        {
            *return_term = EXCP_BADARG(env, "Missing arguments for this cipher");
            goto err;
        }


    if (CIPHER_FORBIDDEN_IN_FIPS(*cipherp))
        {
            *return_term = EXCP_NOTSUP(env, "Forbidden in FIPS");
            goto err;
        }

    /* Get ivec_len for this cipher (if we found one) */
#if !defined(HAVE_EVP_AES_CTR)
    /* This code is for historic OpenSSL where EVP_aes_*_ctr is not defined.... */
    if ((*cipherp)->cipher.p) {
        /* Not aes_ctr compatibility code since EVP_*
           was defined and assigned to (*cipherp)->cipher.p */
        ivec_len = GET_IV_LEN(*cipherp);
    } else {
        /* No EVP_* was found */
        if ((*cipherp)->flags & AES_CTR_COMPAT)
            /* Use aes_ctr compatibility code later */
            ivec_len = 16;
        else {
            /* Unsupported crypto */
            *return_term = EXCP_NOTSUP(env, "Cipher not supported in this libcrypto version");
            goto err;
        }
    }
#else
    /* Normal code */
    if (!((*cipherp)->cipher.p)) {
        *return_term = EXCP_NOTSUP(env, "Cipher not supported in this libcrypto version");
        goto err;
    }
    ivec_len = GET_IV_LEN(*cipherp);
#endif
    
    /* (*cipherp)->cipher.p != NULL and ivec_len has a value */

    /* Fetch IV */
    if (ivec_len && (ivec_arg != atom_undefined)) {
        if (!enif_inspect_iolist_as_binary(env, ivec_arg, &ivec_bin))
            {
                *return_term = EXCP_BADARG(env, "Bad iv type");
                goto err;
            }

        if (ivec_len != ivec_bin.size)
            {
                *return_term = EXCP_BADARG(env, "Bad iv size");
                goto err;
            }
    }

    ctx_res->iv_len = ivec_len;
    
#if !defined(HAVE_EVP_AES_CTR)
    if (!((*cipherp)->cipher.p)
        && ((*cipherp)->flags & AES_CTR_COMPAT)
        ) {
        /* Must use aes_ctr compatibility code */
        ERL_NIF_TERM ecount_bin;
        unsigned char *outp;
        if ((outp = enif_make_new_binary(env, AES_BLOCK_SIZE, &ecount_bin)) == NULL) {
            *return_term = EXCP_ERROR(env, "Can't allocate ecount_bin");
            goto err;
        }
        memset(outp, 0, AES_BLOCK_SIZE);

        ctx_res->env = enif_alloc_env();
        if (!ctx_res->env) {
            *return_term = EXCP_ERROR(env, "Can't allocate env");
            goto err;
        }
        ctx_res->state =
            enif_make_copy(ctx_res->env,
                           enif_make_tuple4(env, key_arg, ivec_arg, ecount_bin, enif_make_int(env, 0)));
        goto success;
    } else {
        /* Flag for subsequent calls that no aes_ctr compatibility code should be called */
        ctx_res->state = atom_undefined;
        ctx_res->env = NULL;
    }
#endif

    /* Initialize the EVP_CIPHER_CTX */

    ctx_res->ctx = EVP_CIPHER_CTX_new();
    if (! ctx_res->ctx)
        {
            *return_term = EXCP_ERROR(env, "Can't allocate context");
            goto err;
        }

    if (!EVP_CipherInit_ex(ctx_res->ctx, (*cipherp)->cipher.p, NULL, NULL, NULL, encflg))
        {
            *return_term = EXCP_ERROR(env, "Can't initialize context, step 1");
            goto err;
        }

    if (!EVP_CIPHER_CTX_set_key_length(ctx_res->ctx, (int)key_bin.size))
        {
            *return_term = EXCP_ERROR(env, "Can't initialize context, key_length");
            goto err;
        }

#ifdef HAVE_RC2
    if (EVP_CIPHER_type((*cipherp)->cipher.p) == NID_rc2_cbc) {
        if (key_bin.size > INT_MAX / 8) {
            *return_term = EXCP_BADARG(env, "To large rc2_cbc key");
            goto err;
        }
        if (!EVP_CIPHER_CTX_ctrl(ctx_res->ctx, EVP_CTRL_SET_RC2_KEY_BITS, (int)key_bin.size * 8, NULL)) {
            *return_term = EXCP_ERROR(env, "ctrl rc2_cbc key");
            goto err;
        }
    }
#endif

    if (ivec_arg == atom_undefined || ivec_len == 0)
        {
            if (!EVP_CipherInit_ex(ctx_res->ctx, NULL, NULL, key_bin.data, NULL, -1)) {
                *return_term = EXCP_ERROR(env, "Can't initialize key");
                goto err;
            }
        }
    else
        if (!EVP_CipherInit_ex(ctx_res->ctx, NULL, NULL, key_bin.data, ivec_bin.data, -1))
            {
                *return_term = EXCP_ERROR(env, "Can't initialize key or iv");
                goto err;
            }

    EVP_CIPHER_CTX_set_padding(ctx_res->ctx, 0);

    *return_term = atom_ok;

#if !defined(HAVE_EVP_AES_CTR)
 success:
#endif
    return 1;

 err:
    if (ctx_res->ctx) EVP_CIPHER_CTX_free(ctx_res->ctx);
    return 0;
}

/*************************************************************************/
/* Get the arguments for the EVP_CipherUpdate function, and call it.     */
/*************************************************************************/

static int get_update_args(ErlNifEnv* env,
                           struct evp_cipher_ctx *ctx_res,
                           const ERL_NIF_TERM indata_arg,
                           ERL_NIF_TERM *return_term)
{
    ErlNifBinary in_data_bin, out_data_bin;
    int out_len, block_size;

    if (!enif_inspect_binary(env, indata_arg, &in_data_bin) )
        {
            *return_term = EXCP_BADARG(env, "Bad 2:nd arg");
            goto err;
        }

    ASSERT(in_data_bin.size <= INT_MAX);

#if !defined(HAVE_EVP_AES_CTR)
    if (ctx_res->state != atom_undefined) {
        ERL_NIF_TERM state0, newstate_and_outdata;
        const ERL_NIF_TERM *tuple_argv;
        int tuple_argc;
        
        state0 = enif_make_copy(env, ctx_res->state);
        
        if (enif_get_tuple(env, state0, &tuple_argc, &tuple_argv) && (tuple_argc == 4)) {
            /* A compatibility state term */
            /* encrypt and decrypt is performed by calling the same function */
            newstate_and_outdata = aes_ctr_stream_encrypt_compat(env, state0, indata_arg);

            if (enif_get_tuple(env, newstate_and_outdata, &tuple_argc, &tuple_argv) && (tuple_argc == 2)) {
                /* newstate_and_outdata = {NewState, OutData} */
                ctx_res->state = enif_make_copy(ctx_res->env, tuple_argv[0]);
                /* Return the OutData (from the newstate_and_outdata tuple) only: */
                *return_term = tuple_argv[1];
            }
        }
    } else
#endif
    {
        block_size = EVP_CIPHER_CTX_block_size(ctx_res->ctx);

        if (!enif_alloc_binary((size_t)in_data_bin.size+block_size, &out_data_bin))
            {
                *return_term = EXCP_ERROR(env, "Can't allocate outdata");
                goto err;
            }

        if (!EVP_CipherUpdate(ctx_res->ctx, out_data_bin.data, &out_len, in_data_bin.data, in_data_bin.size))
            {
                *return_term = EXCP_ERROR(env, "Can't update");
                goto err;
            }

        if (!enif_realloc_binary(&out_data_bin, (size_t)out_len))
            {
                *return_term = EXCP_ERROR(env, "Can't reallocate");
                goto err;
            }

        CONSUME_REDS(env, in_data_bin);
        /* return the result text as a binary: */
        *return_term = enif_make_binary(env, &out_data_bin);
    }

    /* success: */
    return 1;

 err:
    return 0;
}

/*************************************************************************/
/* Initialize the state for (de/en)cryption                              */
/*************************************************************************/

ERL_NIF_TERM ng_crypto_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Cipher, Key, IVec, Encrypt)  % if no IV for the Cipher, set IVec = <<>>
 */
    struct evp_cipher_ctx *ctx_res = NULL;
    const struct cipher_type_t *cipherp;
    ERL_NIF_TERM ret;
    int encflg;

    if (enif_is_atom(env, argv[0])) {
        if ((ctx_res = enif_alloc_resource(evp_cipher_ctx_rtype, sizeof(struct evp_cipher_ctx))) == NULL)
            return EXCP_ERROR(env, "Can't allocate resource");

        if (get_init_args(env, ctx_res, argv[0], argv[1], argv[2], argv[argc-1],
                           &cipherp, &ret))
            ret = enif_make_resource(env, ctx_res);
        /* else error msg in ret */

        if(ctx_res) enif_release_resource(ctx_res);

    } else if (enif_get_resource(env, argv[0], (ErlNifResourceType*)evp_cipher_ctx_rtype, (void**)&ctx_res)) {
        /* Fetch the flag telling if we are going to encrypt (=true) or decrypt (=false) */
        if (argv[3] == atom_true)
            encflg = 1;
        else if (argv[3] == atom_false)
            encflg = 0;
        else {
            ret = EXCP_BADARG(env, "Bad enc flag");
            goto ret;
        }
        if (ctx_res->ctx) {
            /* It is *not* a ctx_res for the compatibility handling of non-EVP aes_ctr */
            if (!EVP_CipherInit_ex(ctx_res->ctx, NULL, NULL, NULL, NULL, encflg)) {
                ret = EXCP_ERROR(env, "Can't initialize encflag");
                goto ret;
            }
        }
        ret = argv[0];
    } else {
        ret = EXCP_BADARG(env, "Bad 1:st arg");
        goto ret;
    }

 ret:
    return ret;
}


/*************************************************************************/
/* Encrypt/decrypt                                                       */
/*************************************************************************/

#if !defined(HAVE_EVP_CIPHER_CTX_COPY)
/*
  The EVP_CIPHER_CTX_copy is not available in older cryptolibs although
  the function is needed.
  Instead of implement it in-place, we have a copy here as a compatibility
  function
*/

int EVP_CIPHER_CTX_copy(EVP_CIPHER_CTX *out, const EVP_CIPHER_CTX *in);

int EVP_CIPHER_CTX_copy(EVP_CIPHER_CTX *out, const EVP_CIPHER_CTX *in)
{
    if ((in == NULL) || (in->cipher == NULL))
        {
            return 0;
        }
#ifdef HAS_ENGINE_SUPPORT
    /* Make sure it's safe to copy a cipher context using an ENGINE */
    if (in->engine && !ENGINE_init(in->engine))
        return 0;
#endif

    EVP_CIPHER_CTX_cleanup(out);
    memcpy(out,in,sizeof *out);

    if (in->cipher_data && in->cipher->ctx_size)
        {
            out->cipher_data=OPENSSL_malloc(in->cipher->ctx_size);
            if (!out->cipher_data)
                return 0;
            memcpy(out->cipher_data,in->cipher_data,in->cipher->ctx_size);
        }

#if defined(EVP_CIPH_CUSTOM_COPY) && defined(EVP_CTRL_COPY)
    if (in->cipher->flags & EVP_CIPH_CUSTOM_COPY)
        return in->cipher->ctrl((EVP_CIPHER_CTX *)in, EVP_CTRL_COPY, 0, out);
#endif
    return 1;
}
/****** End of compatibility function ******/
#endif


ERL_NIF_TERM ng_crypto_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data [, IV]) */
    struct evp_cipher_ctx *ctx_res;
    struct evp_cipher_ctx ctx_res_copy;
    ERL_NIF_TERM ret;

    ctx_res_copy.ctx = NULL;

    if (!enif_get_resource(env, argv[0], (ErlNifResourceType*)evp_cipher_ctx_rtype, (void**)&ctx_res))
        return EXCP_BADARG(env, "Bad 1:st arg");
    
    if (argc == 3) {
        ErlNifBinary ivec_bin;

        memcpy(&ctx_res_copy, ctx_res, sizeof ctx_res_copy);
#if !defined(HAVE_EVP_AES_CTR)
        if (ctx_res_copy.state == atom_undefined)
            /* Not going to use aes_ctr compat functions */
#endif
            {
                ctx_res_copy.ctx = EVP_CIPHER_CTX_new();

                if (!EVP_CIPHER_CTX_copy(ctx_res_copy.ctx, ctx_res->ctx)) {
                    ret = EXCP_ERROR(env, "Can't copy ctx_res");
                    goto err;
                }
            }

        ctx_res = &ctx_res_copy;

        if (!enif_inspect_iolist_as_binary(env, argv[2], &ivec_bin))
            {
                ret = EXCP_BADARG(env, "Bad iv type");
                goto err;
            }

        if (ctx_res_copy.iv_len != ivec_bin.size)
            {
                ret = EXCP_BADARG(env, "Bad iv size");
                goto err;
            }
        
#if !defined(HAVE_EVP_AES_CTR)
        if ((ctx_res_copy.state != atom_undefined) ) {
            /* replace the iv in state with argv[2] */
            ERL_NIF_TERM state0;
            const ERL_NIF_TERM *tuple_argv;
            int tuple_argc;
            state0 = enif_make_copy(env, ctx_res_copy.state);
            if (enif_get_tuple(env, state0, &tuple_argc, &tuple_argv) && (tuple_argc == 4)) {
                /* A compatibility state term */
                ctx_res_copy.state = enif_make_tuple4(env, tuple_argv[0], argv[2], tuple_argv[2], tuple_argv[3]);
            }
        } else
#endif
            if (!EVP_CipherInit_ex(ctx_res_copy.ctx, NULL, NULL, NULL, ivec_bin.data, -1))
                {
                    ret = EXCP_ERROR(env, "Can't set iv");
                    goto err;
                }
        
        get_update_args(env, &ctx_res_copy, argv[1], &ret);
    } else
        get_update_args(env, ctx_res, argv[1], &ret);

 err:
    if (ctx_res_copy.ctx)
        EVP_CIPHER_CTX_free(ctx_res_copy.ctx);

    return ret; /* Both success and error */
}


ERL_NIF_TERM ng_crypto_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data [, IV]) */
    ErlNifBinary   data_bin;

    ASSERT(argc <= 3);

    if (!enif_inspect_binary(env, argv[1], &data_bin))
        return EXCP_BADARG(env, "expected binary as data");

    if (data_bin.size > INT_MAX)
        return EXCP_BADARG(env, "to long data");

    /* Run long jobs on a dirty scheduler to not block the current emulator thread */
    if (data_bin.size > MAX_BYTES_TO_NIF) {
        return enif_schedule_nif(env, "ng_crypto_update",
                                 ERL_NIF_DIRTY_JOB_CPU_BOUND,
                                 ng_crypto_update, argc, argv);
    }

    return ng_crypto_update(env, argc, argv);
}

/*************************************************************************/
/* One shot                                                              */
/*************************************************************************/

ERL_NIF_TERM ng_crypto_one_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Cipher, Key, IVec, Data, Encrypt) */
    struct evp_cipher_ctx ctx_res;
    const struct cipher_type_t *cipherp;
    ERL_NIF_TERM ret;

    ctx_res.ctx = NULL;
#if !defined(HAVE_EVP_AES_CTR)
    ctx_res.env = NULL;
#endif

    if (!get_init_args(env, &ctx_res, argv[0], argv[1], argv[2], argv[4], &cipherp, &ret))
        goto ret;

    get_update_args(env, &ctx_res, argv[3], &ret);

 ret:
    if (ctx_res.ctx)
        EVP_CIPHER_CTX_free(ctx_res.ctx);

#if !defined(HAVE_EVP_AES_CTR)
    if (ctx_res.env)
         enif_free_env(ctx_res.env);
#endif

    return ret;
}


ERL_NIF_TERM ng_crypto_one_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Cipher, Key, IVec, Data, Encrypt)  % if no IV for the Cipher, set IVec = <<>>
  */
    ErlNifBinary   data_bin;

    ASSERT(argc == 5);

    if (!enif_inspect_binary(env, argv[3], &data_bin))
        return EXCP_BADARG(env, "expected binary as data");

    if (data_bin.size > INT_MAX)
        return EXCP_BADARG(env, "to long data");

    /* Run long jobs on a dirty scheduler to not block the current emulator thread */
    if (data_bin.size > MAX_BYTES_TO_NIF) {
        return enif_schedule_nif(env, "ng_crypto_one_time",
                                 ERL_NIF_DIRTY_JOB_CPU_BOUND,
                                 ng_crypto_one_time, argc, argv);
    }

    return ng_crypto_one_time(env, argc, argv);
}
