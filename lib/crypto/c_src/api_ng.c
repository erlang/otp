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
 * EXPERIMENTAL!!
 *
 */
ERL_NIF_TERM ng_crypto_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);



/* Try better error messages in new functions */
#define ERROR_Term(Env, ReasonTerm) enif_make_tuple2((Env), atom_error, (ReasonTerm))
#define ERROR_Str(Env, ReasonString) ERROR_Term((Env), enif_make_string((Env),(ReasonString),(ERL_NIF_LATIN1)))

/* Initializes state for (de)encryption
 */
ERL_NIF_TERM ng_crypto_init_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Cipher, Key, IVec, Encrypt)  % if no IV for the Cipher, set IVec = <<>>
 */
    ErlNifBinary     key_bin, ivec_bin;
    unsigned char *iv = NULL;
    struct evp_cipher_ctx *ctx;
    const struct cipher_type_t *cipherp;
    const EVP_CIPHER *cipher;
    ERL_NIF_TERM enc_flg_arg, ret;
    int enc;
    unsigned iv_len;

    enc_flg_arg = argv[argc-1];
    if (enc_flg_arg == atom_true)
        enc = 1;
    else if (enc_flg_arg == atom_false)
        enc = 0;
    else if (enc_flg_arg == atom_undefined)
        /* For compat funcs in crypto.erl */
        enc = -1;
    else
        return ERROR_Str(env, "Bad enc flag");

    if (!enif_inspect_binary(env, argv[1], &key_bin))
        return ERROR_Str(env, "Bad key");

    if (!(cipherp = get_cipher_type(argv[0], key_bin.size)))
        return ERROR_Str(env, "Unknown cipher or bad key size");

    if (FORBIDDEN_IN_FIPS(cipherp))
        return enif_raise_exception(env, atom_notsup);

    if (enc == -1)
        return atom_undefined;

    if (!(cipher = cipherp->cipher.p)) {
#if !defined(HAVE_EVP_AES_CTR)
        if (cipherp->flags & AES_CTR_COMPAT)
            return aes_ctr_stream_init_compat(env, argv[1], argv[2]);
        else
#endif
            return enif_raise_exception(env, atom_notsup);
    }

#ifdef HAVE_ECB_IVEC_BUG
    if (cipherp->flags & ECB_BUG_0_9_8L)
        iv_len = 0; /* <= 0.9.8l returns faulty ivec length */
    else
#endif
        iv_len = EVP_CIPHER_iv_length(cipher);

    if (iv_len) {
        if (!enif_inspect_binary(env, argv[2], &ivec_bin))
            return ERROR_Str(env, "Bad iv type");

        if (iv_len != ivec_bin.size)
            return ERROR_Str(env, "Bad iv size");

        iv = ivec_bin.data;
    }

    if ((ctx = enif_alloc_resource(evp_cipher_ctx_rtype, sizeof(struct evp_cipher_ctx))) == NULL)
        return ERROR_Str(env, "Can't allocate resource");

    ctx->ctx = EVP_CIPHER_CTX_new();
    if (! ctx->ctx)
        return ERROR_Str(env, "Can't allocate context");

    if (!EVP_CipherInit_ex(ctx->ctx, cipher, NULL, NULL, NULL, enc)) {
        enif_release_resource(ctx);
        return ERROR_Str(env, "Can't initialize context, step 1");
    }

    if (!EVP_CIPHER_CTX_set_key_length(ctx->ctx, (int)key_bin.size)) {
        enif_release_resource(ctx);
        return ERROR_Str(env, "Can't initialize context, key_length");
    }

    if (EVP_CIPHER_type(cipher) == NID_rc2_cbc) {
        if (key_bin.size > INT_MAX / 8) {
            enif_release_resource(ctx);
            return ERROR_Str(env, "To large rc2_cbc key");
        }
        if (!EVP_CIPHER_CTX_ctrl(ctx->ctx, EVP_CTRL_SET_RC2_KEY_BITS, (int)key_bin.size * 8, NULL)) {
            enif_release_resource(ctx);
            return ERROR_Str(env, "ctrl rc2_cbc key");
        }
    }

    if (!EVP_CipherInit_ex(ctx->ctx, NULL, NULL, key_bin.data, iv, enc)) {
        enif_release_resource(ctx);
        return ERROR_Str(env, "Can't initialize key and/or iv");
    }

    EVP_CIPHER_CTX_set_padding(ctx->ctx, 0);

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return ret;
}

ERL_NIF_TERM ng_crypto_update(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data)
    (Context, Data, IV) */
    struct evp_cipher_ctx *ctx;
    ErlNifBinary   in_data_bin, ivec_bin, out_data_bin;
    int            out_len, block_size;

#if !defined(HAVE_EVP_AES_CTR)
    const ERL_NIF_TERM *state_term;
    int state_arity;

    if (enif_get_tuple(env, argv[0], &state_arity, &state_term) && (state_arity == 4)) {
        return aes_ctr_stream_encrypt_compat(env, argv[0], argv[1]);
    }
#endif

    if (!enif_get_resource(env, argv[0], evp_cipher_ctx_rtype, (void**)&ctx))
        return ERROR_Str(env, "Bad 1:st arg");
    
    if (!enif_inspect_binary(env, argv[1], &in_data_bin) )
        return ERROR_Str(env, "Bad 2:nd arg");

    /* arg[1] was checked by the caller */
    ASSERT(in_data_bin.size =< INT_MAX);

    block_size = EVP_CIPHER_CTX_block_size(ctx->ctx);

    if (argc==3) {
        if (!enif_inspect_iolist_as_binary(env, argv[2], &ivec_bin))
            return ERROR_Str(env, "Not binary IV");
       
        if (ivec_bin.size > INT_MAX)
            return ERROR_Str(env, "Too big IV");
       
        if (!EVP_CipherInit_ex(ctx->ctx, NULL, NULL, NULL, ivec_bin.data, -1))
            return ERROR_Str(env, "Can't set IV");
    }

    if (!enif_alloc_binary((size_t)in_data_bin.size+block_size, &out_data_bin))
        return ERROR_Str(env, "Can't allocate outdata");

    if (!EVP_CipherUpdate(ctx->ctx, out_data_bin.data, &out_len, in_data_bin.data, in_data_bin.size))
        return ERROR_Str(env, "Can't update");

    if (!enif_realloc_binary(&out_data_bin, (size_t)out_len))
        return ERROR_Str(env, "Can't reallocate");

    CONSUME_REDS(env, in_data_bin);
    return enif_make_binary(env, &out_data_bin);
}


ERL_NIF_TERM ng_crypto_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data)
    (Context, Data, IV) */
    int i;
    ErlNifBinary   data_bin;
    ERL_NIF_TERM new_argv[3];

    ASSERT(argc =< 3);

    if (!enif_inspect_iolist_as_binary(env, argv[1], &data_bin))
        return ERROR_Str(env, "iodata expected as data");

    if (data_bin.size > INT_MAX)
        return ERROR_Str(env, "to long data");

    for (i=0; i<argc; i++) new_argv[i] = argv[i];
    new_argv[1] = enif_make_binary(env, &data_bin);

    /* Run long jobs on a dirty scheduler to not block the current emulator thread */
    if (data_bin.size > MAX_BYTES_TO_NIF) {
        return enif_schedule_nif(env, "ng_crypto_update",
                                 ERL_NIF_DIRTY_JOB_CPU_BOUND,
                                 ng_crypto_update, argc, new_argv);
    }

    return ng_crypto_update(env, argc, new_argv);
}

