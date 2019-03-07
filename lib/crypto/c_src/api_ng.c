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
ERL_NIF_TERM ng_crypto_one_shot(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);



/* Try better error messages in new functions */
#define ERROR_Term(Env, ReasonTerm) enif_make_tuple2((Env), atom_error, (ReasonTerm))
#define ERROR_Str(Env, ReasonString) ERROR_Term((Env), enif_make_string((Env),(ReasonString),(ERL_NIF_LATIN1)))


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
            *return_term = ERROR_Str(env, "Bad enc flag");
            goto err;
        }

    /* Fetch the key */
    if (!enif_inspect_iolist_as_binary(env, key_arg, &key_bin))
        {
            *return_term = ERROR_Str(env, "Bad key");
            goto err;
        }

    /* Fetch cipher type */
    if (!enif_is_atom(env, cipher_arg))
        {
            *return_term = ERROR_Str(env, "Cipher id is not an atom");
            goto err;
        }

    if (!(*cipherp = get_cipher_type(cipher_arg, key_bin.size)))
        {
            *return_term = ERROR_Str(env, "Unknown cipher or bad key size for the cipher");
            goto err;
        }

    if (FORBIDDEN_IN_FIPS(*cipherp))
        {
            *return_term = enif_raise_exception(env, atom_notsup);
            goto err;
        }
    
    /* Fetch IV */
#if !defined(HAVE_EVP_AES_CTR)
    /* This is code for OpenSSL 0.9.8. Therefore we could accept some ineficient code */
    ctx_res->env = NULL;
    ctx_res->state = atom_undefined;

    if (!((*cipherp)->cipher.p) && (*cipherp)->flags & AES_CTR_COMPAT)
        ivec_len = 16;
    else
#endif
        ivec_len = GET_IV_LEN(*cipherp);

    if (ivec_len) {
        if (!enif_inspect_iolist_as_binary(env, ivec_arg, &ivec_bin))
            {
                *return_term = ERROR_Str(env, "Bad iv type");
                goto err;
            }

        if (ivec_len != ivec_bin.size)
            {
                *return_term = ERROR_Str(env, "Bad iv size");
                goto err;
            }
    }

    if (!((*cipherp)->cipher.p))
        {
#if !defined(HAVE_EVP_AES_CTR)
            if ((*cipherp)->flags & AES_CTR_COMPAT)
                {
                    ERL_NIF_TERM ecount_bin;
                    unsigned char *outp;
                    if ((outp = enif_make_new_binary(env, AES_BLOCK_SIZE, &ecount_bin)) == NULL) {
                        *return_term = ERROR_Str(env, "Can't allocate ecount_bin");
                        goto err;
                    }
                    memset(outp, 0, AES_BLOCK_SIZE);
                    
                    ctx_res->env = enif_alloc_env();
                    if (!ctx_res->env) {
                        *return_term = ERROR_Str(env, "Can't allocate env");
                        goto err;
                    }
                    ctx_res->state =
                        enif_make_copy(ctx_res->env,
                                       enif_make_tuple4(env, key_arg, ivec_arg, ecount_bin, enif_make_int(env, 0)));
                    goto success;
                } 
#endif
            *return_term = enif_raise_exception(env, atom_notsup);
            goto err;
        }

    /* Initialize the EVP_CIPHER_CTX */

    ctx_res->ctx = EVP_CIPHER_CTX_new();
    if (! ctx_res->ctx)
        {
            *return_term = ERROR_Str(env, "Can't allocate context");
            goto err;
        }

    if (!EVP_CipherInit_ex(ctx_res->ctx, (*cipherp)->cipher.p, NULL, NULL, NULL, encflg))
        {
            *return_term = ERROR_Str(env, "Can't initialize context, step 1");
            goto err;
        }

    if (!EVP_CIPHER_CTX_set_key_length(ctx_res->ctx, (int)key_bin.size))
        {
            *return_term = ERROR_Str(env, "Can't initialize context, key_length");
            goto err;
        }


    if (EVP_CIPHER_type((*cipherp)->cipher.p) == NID_rc2_cbc) {
        if (key_bin.size > INT_MAX / 8) {
            *return_term = ERROR_Str(env, "To large rc2_cbc key");
            goto err;
        }
        if (!EVP_CIPHER_CTX_ctrl(ctx_res->ctx, EVP_CTRL_SET_RC2_KEY_BITS, (int)key_bin.size * 8, NULL)) {
            *return_term = ERROR_Str(env, "ctrl rc2_cbc key");
            goto err;
        }
    }

    if (!EVP_CipherInit_ex(ctx_res->ctx, NULL, NULL, key_bin.data, ivec_bin.data, -1)) {
        *return_term = ERROR_Str(env, "Can't initialize key and/or iv");
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
            *return_term = ERROR_Str(env, "Bad 2:nd arg");
            goto err;
        }

    ASSERT(in_data_bin.size <= INT_MAX);

#if !defined(HAVE_EVP_AES_CTR)
    // enif_fprintf(stdout, "%s:%u state = %T\r\n", __FILE__, __LINE__, ctx_res->state);
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
                *return_term = ERROR_Str(env, "Can't allocate outdata");
                goto err;
            }

        if (!EVP_CipherUpdate(ctx_res->ctx, out_data_bin.data, &out_len, in_data_bin.data, in_data_bin.size))
            {
                *return_term = ERROR_Str(env, "Can't update");
                goto err;
            }

        if (!enif_realloc_binary(&out_data_bin, (size_t)out_len))
            {
                *return_term = ERROR_Str(env, "Can't reallocate");
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
            return ERROR_Str(env, "Can't allocate resource");

        if (!get_init_args(env, ctx_res, argv[0], argv[1], argv[2], argv[argc-1],
                           &cipherp, &ret))
            /* Error msg in &ret */
            goto ret;

        ret = enif_make_resource(env, ctx_res);
        if(ctx_res) enif_release_resource(ctx_res);

    } else if (enif_get_resource(env, argv[0], evp_cipher_ctx_rtype, (void**)&ctx_res)) {
        /* Fetch the flag telling if we are going to encrypt (=true) or decrypt (=false) */
        if (argv[3] == atom_true)
            encflg = 1;
        else if (argv[3] == atom_false)
            encflg = 0;
        else {
            ret = ERROR_Str(env, "Bad enc flag");
            goto ret;
        }
        if (ctx_res->ctx) {
            /* It is *not* a ctx_res for the compatibility handling of non-EVP aes_ctr */
            if (!EVP_CipherInit_ex(ctx_res->ctx, NULL, NULL, NULL, NULL, encflg)) {
                ret = ERROR_Str(env, "Can't initialize encflag");
                goto ret;
            }
        }
        ret = argv[0];
    } else {
        ret = ERROR_Str(env, "Bad 1:st arg");
        goto ret;
    }

 ret:
    return ret;
}


/*************************************************************************/
/* Encrypt/decrypt                                                       */
/*************************************************************************/


ERL_NIF_TERM ng_crypto_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    struct evp_cipher_ctx *ctx_res;
    ERL_NIF_TERM ret;

    if (!enif_get_resource(env, argv[0], evp_cipher_ctx_rtype, (void**)&ctx_res))
        return ERROR_Str(env, "Bad 1:st arg");
    
    get_update_args(env, ctx_res, argv[1], &ret);

    return ret; /* Both success and error */
}


ERL_NIF_TERM ng_crypto_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    ErlNifBinary   data_bin;

    ASSERT(argc <= 3);

    if (!enif_inspect_binary(env, argv[1], &data_bin))
        return ERROR_Str(env, "expected binary as data");

    if (data_bin.size > INT_MAX)
        return ERROR_Str(env, "to long data");

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

ERL_NIF_TERM ng_crypto_one_shot(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Cipher, Key, IVec, Data, Encrypt) */
    struct evp_cipher_ctx ctx_res;
    const struct cipher_type_t *cipherp;
    ERL_NIF_TERM ret;

    if (!get_init_args(env, &ctx_res, argv[0], argv[1], argv[2], argv[4], &cipherp, &ret))
        goto ret;

    get_update_args(env, &ctx_res, argv[3], &ret);

 ret:
    if (ctx_res.ctx)
        EVP_CIPHER_CTX_free(ctx_res.ctx);
    return ret;
}

ERL_NIF_TERM ng_crypto_one_shot_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Cipher, Key, IVec, Data, Encrypt)  % if no IV for the Cipher, set IVec = <<>>
  */
    ErlNifBinary   data_bin;

    ASSERT(argc == 5);

    if (!enif_inspect_binary(env, argv[3], &data_bin))
        return ERROR_Str(env, "expected binary as data");

    if (data_bin.size > INT_MAX)
        return ERROR_Str(env, "to long data");

    /* Run long jobs on a dirty scheduler to not block the current emulator thread */
    if (data_bin.size > MAX_BYTES_TO_NIF) {
        return enif_schedule_nif(env, "ng_crypto_one_shot",
                                 ERL_NIF_DIRTY_JOB_CPU_BOUND,
                                 ng_crypto_one_shot, argc, argv);
    }

    return ng_crypto_one_shot(env, argc, argv);
}
