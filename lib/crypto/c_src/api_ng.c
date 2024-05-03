/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2024. All Rights Reserved.
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

/*************************************************************************/
/* Compatibility functions.                                              */
/*************************************************************************/
#ifdef HAVE_ECB_IVEC_BUG
    /* <= 0.9.8l returns faulty ivec length */
# define GET_IV_LEN(Ciph) ((Ciph)->flags & ECB_BUG_0_9_8L) ? 0 : EVP_CIPHER_iv_length((Ciph)->cipher.p)
#else
# define GET_IV_LEN(Ciph) EVP_CIPHER_iv_length((Ciph)->cipher.p)
#endif

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
/****** End of !defined(HAVE_EVP_CIPHER_CTX_COPY) ******/
#endif

/*************************************************************************/
/* Get the arguments for the initialization of the EVP_CIPHER_CTX. Check */
/* them and initialize that context.                                     */
/*************************************************************************/

static ERL_NIF_TERM get_opts(ErlNifEnv* env, const ERL_NIF_TERM opts, int opts_arg_num, int *encflgp, ERL_NIF_TERM *padflgp)
{ /* boolean() | [{Tag,Val}]  Tag = encrypt | padding */
    unsigned list_len;
    ERL_NIF_TERM p, hd, tl;

    *padflgp = atom_false; /* Not valid as padding value */
    /* First check if the opts is an atom: */
    if (opts == atom_true)
        {
            *encflgp = 1;
            *padflgp = atom_undefined;
            return atom_ok;
        }

    if (opts == atom_false)
        {
            *encflgp = 0;
            *padflgp = atom_undefined;
            return atom_ok;
        }

    if (opts == atom_undefined)
        /* For compat funcs in crypto.erl. TODO: check and remove */
        {
            *encflgp = -1;
            *padflgp = atom_undefined;
            return atom_ok;
        }

    if (!enif_is_list(env, opts) || !enif_get_list_length(env, opts, &list_len))
        /* Not a boolean() and not a list, definitely an error */
        return EXCP_BADARG_N(env, opts_arg_num, "Options are not a boolean or a proper list");

    /* A list, might be a property list, as it should */
    *encflgp = -14; /* why not? */
    p = opts;
    while (enif_get_list_cell(env, p, &hd, &tl))
        /* Loop through the list [{Tag,Value},...]
              - check that
                 + the list is proper
                 + each list element is a 2-tuple
                 + the Tag is known
                 + the Value is of right type
              - assign Values of known Tags to their respective flags
        */
        {
            int arity;
            const ERL_NIF_TERM* elements;

            if (!(enif_get_tuple(env, hd, &arity, &elements) && (arity == 2)) )
                return EXCP_BADARG_N(env, opts_arg_num, "Options must be a property list!");

            if (elements[0] == atom_encrypt)
                {
                    if (*encflgp != -14)
                        return EXCP_BADARG_N(env, opts_arg_num, "'encrypt' option is present more than once!");
                    else if (elements[1] == atom_true)
                        *encflgp = 1;
                    else if (elements[1] == atom_false)
                        *encflgp = 0;
                    else if (elements[1] == atom_undefined)
                        *encflgp = -1; /* For compat funcs in crypto.erl. TODO: check and remove */
                    else
                        return EXCP_BADARG_N(env, opts_arg_num, "The 'encrypt' option must be a boolean!");
                }
            else if (elements[0] == atom_padding)
                {
                    if (*padflgp != atom_false)
                        return EXCP_BADARG_N(env, opts_arg_num, "The 'padding' option is present more than once!");

                    else if ((elements[1] == atom_undefined) ||
                             (elements[1] == atom_none) ||
                             (elements[1] == atom_zero) ||
                             (elements[1] == atom_random) ||
                             (elements[1] == atom_pkcs_padding)
                             )
                        *padflgp = elements[1];

                    else
                        return EXCP_BADARG_N(env, opts_arg_num, "Bad 'padding' option value");
                }
            else
                {
                    char msg[64];
                    if (enif_snprintf(msg, 64, "Bad tag in option: %T", elements[0]))
                        return EXCP_BADARG_N(env, opts_arg_num, msg);
                    else
                        return EXCP_BADARG_N(env, opts_arg_num, "Bad tag in option!");
                }

            p = tl; /* prepare for handling next list element or to exit the loop */
        }

    if (*encflgp == -14)
        *encflgp = 1; /* {encrypt,true} is the default */

    if (*padflgp == atom_false)
        *padflgp = atom_undefined; /* {padding,undefined} is the default */

    return atom_ok;
}


static int get_init_args(ErlNifEnv* env,
                         struct evp_cipher_ctx *ctx_res,
                         const ERL_NIF_TERM argv[],
                         int cipher_arg_num,
                         int key_arg_num,
                         int ivec_arg_num,
                         int opts_arg_num,
                         const struct cipher_type_t **cipherp,
                         ERL_NIF_TERM *return_term)
{
    int ivec_len;
    ErlNifBinary ivec_bin;

    ctx_res->ctx = NULL; /* For testing if *ctx should be freed after errors */
#if !defined(HAVE_EVP_AES_CTR)
    ctx_res->env = NULL; /* For testing if *env should be freed after errors */
#endif
    ctx_res->key_bin.data = NULL;
    ctx_res->padding = atom_undefined;
    ctx_res->padded_size = -1;
    ctx_res->size = 0;

    /* Two initializations to make CodeChecker happy: it gets a bit disoriented
       by the NIF Exception model */
    ctx_res->encflag = 0;
    ctx_res->padding = atom_error;

    /* Fetch the options */
    if ((*return_term =
         get_opts(env, argv[opts_arg_num], opts_arg_num, &(ctx_res->encflag), &(ctx_res->padding))
         ) != atom_ok)
        goto err;

    /* Fetch the key */
    if (!enif_inspect_iolist_as_binary(env, argv[key_arg_num], &ctx_res->key_bin))
        {
            *return_term = EXCP_BADARG_N(env, key_arg_num, "Bad key");
            goto err;
        }

    /* Fetch cipher type */
    if (!enif_is_atom(env, argv[cipher_arg_num]))
        {
            *return_term = EXCP_BADARG_N(env, cipher_arg_num, "Cipher id is not an atom");
            goto err;
        }

    if (!(*cipherp = get_cipher_type(argv[cipher_arg_num], ctx_res->key_bin.size)))
        {
            if (!get_cipher_type_no_key(argv[cipher_arg_num]))
                *return_term = EXCP_BADARG_N(env, cipher_arg_num, "Unknown cipher");
            else
                *return_term = EXCP_BADARG_N(env, key_arg_num, "Bad key size");
            goto err;
        }

    if ((*cipherp)->flags &  AEAD_CIPHER)
        {
            *return_term = EXCP_BADARG_N(env, cipher_arg_num, "Missing arguments for this cipher");
            goto err;
        }


    if (CIPHER_FORBIDDEN_IN_FIPS(*cipherp))
        {
            *return_term = EXCP_NOTSUP_N(env, cipher_arg_num, "Forbidden in FIPS");
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
            *return_term =
                EXCP_NOTSUP_N(env, cipher_arg_num, "Cipher not supported in this libcrypto version");
            goto err;
        }
    }
#else
    /* Normal code */
    if (!((*cipherp)->cipher.p)) {
        *return_term =
            EXCP_NOTSUP_N(env, cipher_arg_num, "Cipher not supported in this libcrypto version");
        goto err;
    }
    ivec_len = GET_IV_LEN(*cipherp);
#endif
    
    /* Here: (*cipherp)->cipher.p != NULL and ivec_len has a value */

    if (argv[ivec_arg_num] == atom_undefined) {
        *return_term = EXCP_NOTSUP(env, "Dynamic IV is not supported since OTP 27.0");
        goto err;
    }

    /* Fetch IV */
    if (ivec_len) {
        if (!enif_inspect_iolist_as_binary(env, argv[ivec_arg_num], &ivec_bin))
            {
                *return_term = EXCP_BADARG_N(env, ivec_arg_num, "Bad iv type");
                goto err;
            }

        if (ivec_len != ivec_bin.size)
            {
                *return_term = EXCP_BADARG_N(env, ivec_arg_num, "Bad iv size");
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
            *return_term = EXCP_ERROR(env, "Can't allocate output data binary");
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
                           enif_make_tuple4(env, argv[key_arg_num], argv[ivec_arg_num], ecount_bin, enif_make_int(env, 0)));
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

    if (!EVP_CipherInit_ex(ctx_res->ctx, (*cipherp)->cipher.p, NULL, NULL, NULL, ctx_res->encflag))
        {
            *return_term = EXCP_ERROR(env, "Can't initialize context, step 1");
            goto err;
        }

    if (!EVP_CIPHER_CTX_set_key_length(ctx_res->ctx, (int)ctx_res->key_bin.size))
        {
            *return_term = EXCP_ERROR_N(env, key_arg_num, "Can't initialize context, key_length");
            goto err;
        }

#ifdef HAVE_RC2
    if (EVP_CIPHER_type((*cipherp)->cipher.p) == NID_rc2_cbc) {
        if (ctx_res->key_bin.size > INT_MAX / 8) {
            *return_term = EXCP_BADARG_N(env, key_arg_num, "To large rc2_cbc key");
            goto err;
        }
        if (!EVP_CIPHER_CTX_ctrl(ctx_res->ctx, EVP_CTRL_SET_RC2_KEY_BITS, (int)ctx_res->key_bin.size * 8, NULL)) {
            *return_term = EXCP_BADARG_N(env, key_arg_num, "ctrl rc2_cbc key");
            goto err;
        }
    }
#endif

    if (ivec_len == 0)
        {
            if (!EVP_CipherInit_ex(ctx_res->ctx, NULL, NULL,ctx_res->key_bin.data, NULL, -1)) {
                *return_term = EXCP_BADARG_N(env, key_arg_num, "Can't initialize key");
                goto err;
            }
        }
    else
        if (!EVP_CipherInit_ex(ctx_res->ctx, NULL, NULL, ctx_res->key_bin.data, ivec_bin.data, -1))
            {
                *return_term = EXCP_ERROR(env, "Can't initialize key or iv");
                goto err;
            }

    /* Set padding */
    if (ctx_res->padding != atom_pkcs_padding) /* pkcs_padding is default */
        EVP_CIPHER_CTX_set_padding(ctx_res->ctx, 0);

    *return_term = atom_ok;

#if !defined(HAVE_EVP_AES_CTR)
 success:
#endif
    return 1;

 err:
    return 0;
}

/*************************************************************************/
/* Get the arguments for the EVP_CipherUpdate function, and call it.     */
/*************************************************************************/
static int get_update_args(ErlNifEnv* env,
                           struct evp_cipher_ctx *ctx_res,
                           const ERL_NIF_TERM argv[],
                           int indata_arg_num,
                           ERL_NIF_TERM *return_term)
{
    ErlNifBinary in_data_bin, out_data_bin;
    int out_len, block_size;

    if (!enif_inspect_iolist_as_binary(env, argv[indata_arg_num], &in_data_bin) )
        {
            *return_term = EXCP_BADARG_N(env, indata_arg_num, "Expected binary");
            goto err0;
        }

    ASSERT(in_data_bin.size <= INT_MAX);

    ctx_res->size += in_data_bin.size;

#if !defined(HAVE_EVP_AES_CTR)
    if (ctx_res->state != atom_undefined) {
        /* Use AES_CTR compatibility code */
        ERL_NIF_TERM state0, newstate_and_outdata;
        const ERL_NIF_TERM *tuple_argv;
        int tuple_argc;
        
        state0 = enif_make_copy(env, ctx_res->state);
        
        if (enif_get_tuple(env, state0, &tuple_argc, &tuple_argv) && (tuple_argc == 4)) {
            /* A compatibility state term */
            /* encrypt and decrypt is performed by calling the same function */
            newstate_and_outdata = aes_ctr_stream_encrypt_compat(env, state0, argv[indata_arg_num]);

            if (enif_get_tuple(env, newstate_and_outdata, &tuple_argc, &tuple_argv) && (tuple_argc == 2)) {
                /* newstate_and_outdata = {NewState, OutData} */
                ctx_res->state = enif_make_copy(ctx_res->env, tuple_argv[0]);
                /* Return the OutData (from the newstate_and_outdata tuple) only: */
                *return_term = tuple_argv[1];
            }
        }
    } else
#endif
#if defined(HAVE_UPDATE_EMPTY_DATA_BUG)
    if (in_data_bin.size == 0)
        {
            if (!enif_alloc_binary(0, &out_data_bin))
                {
                    *return_term = EXCP_ERROR(env, "Can't allocate outdata");
                    goto err;
                }
            *return_term = enif_make_binary(env, &out_data_bin);
        } else
#endif
    {
        block_size = EVP_CIPHER_CTX_block_size(ctx_res->ctx);

        if (!enif_alloc_binary((size_t)in_data_bin.size+block_size, &out_data_bin))
            {
                *return_term = EXCP_ERROR(env, "Can't allocate outdata");
                goto err0;
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
    enif_release_binary(&out_data_bin);
 err0:
    return 0;
}

/*************************************************************************/
/* Get the arguments for the EVP_CipherFinal function, and call it.      */
/*************************************************************************/

static int get_final_args(ErlNifEnv* env,
                          struct evp_cipher_ctx *ctx_res,
                          ERL_NIF_TERM *return_term
                          )
{
    ErlNifBinary out_data_bin;
    int block_size, pad_size;
    int out_len, pad_offset;

#if !defined(HAVE_EVP_AES_CTR)
    if (ctx_res->state != atom_undefined) {
        /* Use AES_CTR compatibility code */
        /* Padding size is always 0, because the block_size is 1 and therefore
           always filled
        */
        ctx_res->padded_size = 0;
        out_len = 0;

        if (!enif_alloc_binary(out_len, &out_data_bin))
            {
                *return_term = EXCP_ERROR(env, "Can't allocate empty outdata");
                goto err0;
            }
    } else
#endif
        {
            block_size = EVP_CIPHER_CTX_block_size(ctx_res->ctx);

            pad_size = ctx_res->size % block_size;
            if (pad_size)
                pad_size = block_size - pad_size;

            if (!enif_alloc_binary((size_t)block_size, &out_data_bin))
                {
                    *return_term = EXCP_ERROR(env, "Can't allocate final outdata");
                    goto err0;
                }

            if (ctx_res->encflag)
                {/* Maybe do padding */

                    /* First set lengths etc and do the otp_padding */
                    if (ctx_res->padding == atom_undefined)
                        {
                            ctx_res->padded_size = pad_size;
                            pad_offset = 0;
                        }

                    else if (ctx_res->padding == atom_none)
                        {
                            ctx_res->padded_size = pad_size;
                            pad_offset = 0;
                        }

                    else if (ctx_res->padding == atom_pkcs_padding)
                        {
                            ctx_res->padded_size = pad_size ? pad_size : block_size;
                            pad_offset = 0;
                        }

                    else if ((ctx_res->padding == atom_zero) ||
                             (ctx_res->padding == atom_random))
                        {
                            if (pad_size)
                                {
                                    unsigned char padding[EVP_MAX_BLOCK_LENGTH];
                                    int i;
                                    if (ctx_res->padding == atom_zero)
                                        for(i=0; i<pad_size; i++)  padding[i] = (unsigned char)0;
                                    else
                                        RAND_bytes(padding, pad_size);
                                    if (!EVP_CipherUpdate(ctx_res->ctx, out_data_bin.data, &out_len, padding, pad_size))
                                        {
                                            *return_term = EXCP_ERROR(env, "Can't pad");
                                            goto err;
                                        }
                                }
                            else
                                out_len = 0;

                            ctx_res->padded_size = pad_size;
                            pad_offset = out_len;
                        }

                    else
                        {
                            char msg[64];
                            if (enif_snprintf(msg, 64, "Bad padding flag: %T", ctx_res->padding))
                                *return_term = EXCP_ERROR(env, msg);
                            else
                                *return_term = EXCP_ERROR(env, "Bad padding flg");
                            goto err;
                        }
                    
                    /* Decide how many bytes that are to be returned and set out_len to that value */
                    if (ctx_res->padding == atom_undefined)
                        {
                            out_len = 0;
                        }

                    else
                        {
                            if (!EVP_CipherFinal_ex(ctx_res->ctx, out_data_bin.data+pad_offset, &out_len))
                                {
                                    if (ctx_res->padding == atom_none)
                                        *return_term = EXCP_ERROR(env, "Padding 'none' but unfilled last block");
                                    else if (ctx_res->padding == atom_pkcs_padding)
                                        *return_term = EXCP_ERROR(env, "Can't finalize");
                                    else
                                        *return_term = EXCP_ERROR(env, "Padding failed");
                                    goto err;
                                }
                            else
                                out_len += pad_offset;
                        }

                    /* (end of encryption part) */
                }
            else
                { /* decryption. */
                    /* Decide how many bytes that are to be returned and set out_len to that value */
                    if (ctx_res->padding == atom_undefined)
                        {
                            out_len = 0;
                        }

                    else if ((ctx_res->padding == atom_none) ||
                             (ctx_res->padding == atom_pkcs_padding) ||
                             (ctx_res->padding == atom_zero) ||
                             (ctx_res->padding == atom_random) )
                        {
                            if (!EVP_CipherFinal_ex(ctx_res->ctx, out_data_bin.data, &out_len))
                                {
                                    *return_term = EXCP_ERROR(env, "Can't finalize");
                                    goto err;
                                }
                        }
                    else
                        {
                            *return_term = EXCP_ERROR(env, "Bad padding flg");
                            goto err;
                        }
                    /* (end of decryption part) */
                }
        }

    /* success: */
    if (!enif_realloc_binary(&out_data_bin, (size_t)out_len))
        {
            *return_term = EXCP_ERROR(env, "Can't reallocate");
            goto err;
        }

    *return_term = enif_make_binary(env, &out_data_bin);

    return 1;

 err:
    enif_release_binary(&out_data_bin);
 err0:
    return 0;
}


/*************************************************************************/
/* Initialize the state for (de/en)cryption                              */
/*************************************************************************/

ERL_NIF_TERM ng_crypto_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Cipher, Key, IVec, OptionsMap)  % if no IV for the Cipher, set IVec = <<>>
 */
    struct evp_cipher_ctx *ctx_res = NULL;
    const struct cipher_type_t *cipherp;
    ERL_NIF_TERM ret;

    if (enif_is_atom(env, argv[0])) {
        if ((ctx_res = enif_alloc_resource(evp_cipher_ctx_rtype, sizeof(struct evp_cipher_ctx))) == NULL)
            return EXCP_ERROR(env, "Can't allocate resource");

        if (get_init_args(env, ctx_res, argv, 0, 1, 2, 3, &cipherp, &ret))
            ret = enif_make_resource(env, ctx_res);
        /* else error msg in ret */

        if(ctx_res) enif_release_resource(ctx_res);

    } else if (enif_get_resource(env, argv[0], (ErlNifResourceType*)evp_cipher_ctx_rtype, (void**)&ctx_res)) {
        /* Fetch the flag telling if we are going to encrypt (=true) or decrypt (=false) */
        if (argv[3] == atom_true)
            ctx_res->encflag = 1;
        else if (argv[3] == atom_false)
            ctx_res->encflag = 0;
        else {
            ret = EXCP_BADARG_N(env, 3, "Expected true or false");
            goto ret;
        }
        if (ctx_res->ctx) {
            /* It is *not* a ctx_res for the compatibility handling of non-EVP aes_ctr */
            if (!EVP_CipherInit_ex(ctx_res->ctx, NULL, NULL, NULL, NULL, ctx_res->encflag)) {
                ret = EXCP_ERROR(env, "Can't initialize encflag");
                goto ret;
            }
        }
        ret = argv[0];
    } else {
        ret = EXCP_BADARG_N(env, 0, "Expected cipher name atom");
        goto ret;
    }

 ret:
    return ret;
}


/*************************************************************************/
/* Encrypt/decrypt                                                       */
/*************************************************************************/

ERL_NIF_TERM ng_crypto_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data [, IV]) */
    struct evp_cipher_ctx *ctx_res;
    struct evp_cipher_ctx ctx_res_copy;
    ERL_NIF_TERM ret;

    ctx_res_copy.ctx = NULL;

    if (!enif_get_resource(env, argv[0], (ErlNifResourceType*)evp_cipher_ctx_rtype, (void**)&ctx_res))
        return EXCP_BADARG_N(env, 0, "Bad State");

    get_update_args(env, ctx_res, argv, 1, &ret);

    if (ctx_res_copy.ctx)
        EVP_CIPHER_CTX_free(ctx_res_copy.ctx);

    return ret;
}


ERL_NIF_TERM ng_crypto_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data [, IV]) */
    ErlNifBinary   data_bin;

    if (!enif_inspect_iolist_as_binary(env, argv[1], &data_bin))
        return EXCP_BADARG_N(env, 1, "expected binary");

    if (data_bin.size > INT_MAX)
        return EXCP_BADARG_N(env, 1, "too long data");

    /* Run long jobs on a dirty scheduler to not block the current emulator thread */
    if (data_bin.size > MAX_BYTES_TO_NIF) {
        return enif_schedule_nif(env, "ng_crypto_update",
                                 ERL_NIF_DIRTY_JOB_CPU_BOUND,
                                 ng_crypto_update, argc, argv);
    }

    return ng_crypto_update(env, argc, argv);
}

/*************************************************************************/
/* Final                                                                 */
/*************************************************************************/

ERL_NIF_TERM ng_crypto_final_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) */
    /* No need for enif_schedule_nif since maximum BlockSize-1 bytes are handled */
    struct evp_cipher_ctx *ctx_res;
    ERL_NIF_TERM ret;

    if (!enif_get_resource(env, argv[0], (ErlNifResourceType*)evp_cipher_ctx_rtype, (void**)&ctx_res))
        return EXCP_BADARG_N(env, 0, "Bad State");

    get_final_args(env, ctx_res, &ret);

    return ret;
}

/*************************************************************************/
/* One shot                                                              */
/*************************************************************************/

ERL_NIF_TERM ng_crypto_one_time(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Cipher, Key, IVec, Data, OptionsMap) */
    struct evp_cipher_ctx ctx_res;
    const struct cipher_type_t *cipherp;
    ERL_NIF_TERM ret;
    ErlNifBinary out_data_bin, final_data_bin;
    unsigned char *append_buf;
    
    ctx_res.ctx = NULL;
#if !defined(HAVE_EVP_AES_CTR)
    ctx_res.env = NULL;
#endif

    /* EVP_CipherInit */
    if (!get_init_args(env, &ctx_res, argv, 0, 1, 2, 4, &cipherp, &ret))
        goto out;

    /* out_data = EVP_CipherUpdate */
    if (!get_update_args(env, &ctx_res, argv, 3, &ret))
        /* Got an exception as result in &ret */
        goto out;

    if (!enif_inspect_binary(env, ret, &out_data_bin) )
        {
            ret = EXCP_ERROR(env, "Can't inspect first");
            goto out;
        }

    /* final_data = EVP_CipherFinal_ex */
    if (!get_final_args(env, &ctx_res, &ret))
        /* Got an exception as result in &ret */
        goto out;

    if (!enif_inspect_binary(env, ret, &final_data_bin) )
        {
            ret = EXCP_ERROR(env, "Can't inspect final");
            goto out;
        }

    /* Concatenate out_data and final_date into a new binary kept in the variable ret. */
    append_buf = enif_make_new_binary(env, out_data_bin.size + final_data_bin.size, &ret);
    if (!append_buf)
        {
            ret = EXCP_ERROR(env, "Can't append");
            goto out;
        }

    memcpy(append_buf,                   out_data_bin.data,   out_data_bin.size);
    memcpy(append_buf+out_data_bin.size, final_data_bin.data, final_data_bin.size);

 out:
    if (ctx_res.ctx)
        EVP_CIPHER_CTX_free(ctx_res.ctx);

#if !defined(HAVE_EVP_AES_CTR)
    if (ctx_res.env)
         enif_free_env(ctx_res.env);
#endif

    return ret;
}


ERL_NIF_TERM ng_crypto_one_time_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Cipher, Key, IVec, Data, Encrypt, Padding)  % if no IV for the Cipher, set IVec = <<>>
  */
    ErlNifBinary   data_bin;

    if (!enif_inspect_iolist_as_binary(env, argv[3], &data_bin))
        return EXCP_BADARG_N(env, 3, "expected binary as data");

    if (data_bin.size > INT_MAX)
        return EXCP_BADARG_N(env, 3, "too long data");

    /* Run long jobs on a dirty scheduler to not block the current emulator thread */
    if (data_bin.size > MAX_BYTES_TO_NIF) {
        return enif_schedule_nif(env, "ng_crypto_one_time",
                                 ERL_NIF_DIRTY_JOB_CPU_BOUND,
                                 ng_crypto_one_time, argc, argv);
    }

    return ng_crypto_one_time(env, argc, argv);
}


/*************************************************************************/
/* Get data from the cipher resource                                     */
/*************************************************************************/

ERL_NIF_TERM ng_crypto_get_data_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context) -> map */
    struct evp_cipher_ctx *ctx_res;
    ERL_NIF_TERM keys[4] = {
        atom_size, atom_padding_size, atom_padding_type, atom_encrypt
    };
    ERL_NIF_TERM values[4];
    ERL_NIF_TERM ret;
    int ok;

    if (!enif_get_resource(env, argv[0], (ErlNifResourceType*)evp_cipher_ctx_rtype, (void**)&ctx_res))
        return EXCP_BADARG_N(env, 0, "Bad State");

    values[0] = enif_make_int(env, ctx_res->size);
    values[1] = enif_make_int(env, ctx_res->padded_size);
    values[2] = ctx_res->padding;
    values[3] = (ctx_res->encflag) ? atom_true : atom_false;
    ok = enif_make_map_from_arrays(env, keys, values, 4, &ret);
    ASSERT(ok); (void)ok;

    return ret;
}
