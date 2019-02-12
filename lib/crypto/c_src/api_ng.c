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
    ERL_NIF_TERM ret;
    int enc = (argv[argc-1] == atom_true)      ? 1  :
              (argv[argc-1] == atom_false)     ? 0  :
              (argv[argc-1] == atom_undefined) ? -1 : /* For compat funcs in crypto.erl */
             -100;
    unsigned iv_len;

    if (enc == -100)
        return ERROR_Str(env, "Bad enc flag");

    if (!enif_inspect_binary(env, argv[1], &key_bin))
        return ERROR_Str(env, "Bad key");

    if (!(cipherp = get_cipher_type(argv[0], key_bin.size)))
        return ERROR_Str(env, "Unknown cipher or bad key size");

    if (FORBIDDEN_IN_FIPS(cipherp))
        return enif_raise_exception(env, atom_notsup);

    if (!(cipher = cipherp->cipher.p)) {
#if !defined(HAVE_EVP_AES_CTR)
        if (cipherp->flags & AES_CTR_COMPAT)
            return aes_ctr_stream_init_compat(env, argv[1], argv[2]);
        else
#endif
            return enif_raise_exception(env, atom_notsup);
    }

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

    if (!EVP_CipherInit_ex(ctx->ctx, cipher, NULL, key_bin.data, iv, enc)) {
        enif_release_resource(ctx);
        return ERROR_Str(env, "Can't initialize key and/or iv");
    }

    EVP_CIPHER_CTX_set_padding(ctx->ctx, 0);

    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return ret;
}

ERL_NIF_TERM ng_crypto_flag_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, EncodeFlg) */
    struct evp_cipher_ctx *ctx;
    int ok = 1;

    if (!enif_get_resource(env, argv[0], evp_cipher_ctx_rtype, (void**)&ctx))
        return enif_make_badarg(env);

    if (argv[1] != atom_undefined) {
        /* Couldn't set the encrypt flag in the init call (= compatibility routines), so do
           it now
        */
        if (argv[1] == atom_true)
            ok = EVP_CipherInit_ex(ctx->ctx, EVP_CIPHER_CTX_cipher(ctx->ctx), NULL, NULL, NULL, 1);
        else if (argv[1] == atom_false)
            ok = EVP_CipherInit_ex(ctx->ctx, EVP_CIPHER_CTX_cipher(ctx->ctx), NULL, NULL, NULL, 0);
        else
            return ERROR_Term(env, enif_make_atom(env,"badarg"));
 }

 if (!ok)
     return ERROR_Str(env, "Couldn't set encrypt/decrypt flag");

 return atom_ok;
}


ERL_NIF_TERM ng_crypto_update_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
    struct evp_cipher_ctx *ctx;
    ErlNifBinary   data_bin;
    ERL_NIF_TERM   cipher_term;
    unsigned char  *out;
    int            outl = 0;

#if !defined(HAVE_EVP_AES_CTR)
    const ERL_NIF_TERM *state_term;
    int state_arity;

    if (enif_get_tuple(env, argv[0], &state_arity, &state_term) && (state_arity == 4)) {
        return aes_ctr_stream_encrypt_compat(env, argv[0], argv[1]);
    }
#endif

    if (!enif_get_resource(env, argv[0], evp_cipher_ctx_rtype, (void**)&ctx)
        || !enif_inspect_iolist_as_binary(env, argv[1], &data_bin)
        || (data_bin.size > INT_MAX) ) {
        return ERROR_Term(env, enif_make_atom(env,"badarg"));
    }

    out = enif_make_new_binary(env, data_bin.size, &cipher_term);
    if (!EVP_CipherUpdate(ctx->ctx, out, &outl, data_bin.data, data_bin.size))
        return ERROR_Term(env, enif_make_atom(env,"badarg"));

    ASSERT(outl == data_bin.size);
    CONSUME_REDS(env,data_bin);
    return cipher_term;
}

