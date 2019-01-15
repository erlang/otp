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

#include "chacha20.h"
#include "cipher.h"

ERL_NIF_TERM chacha20_stream_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IV) */
#if defined(HAVE_CHACHA20)
    ErlNifBinary     key_bin, ivec_bin;
    struct evp_cipher_ctx *ctx = NULL;
    const EVP_CIPHER *cipher;
    ERL_NIF_TERM     ret;

    ASSERT(argc == 2);

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin))
        goto bad_arg;
    if (key_bin.size != 32)
        goto bad_arg;
    if (!enif_inspect_binary(env, argv[1], &ivec_bin))
        goto bad_arg;
    if (ivec_bin.size != 16)
        goto bad_arg;

    cipher = EVP_chacha20();

    if ((ctx = enif_alloc_resource(evp_cipher_ctx_rtype, sizeof(struct evp_cipher_ctx))) == NULL)
        goto err;
    if ((ctx->ctx = EVP_CIPHER_CTX_new()) == NULL)
        goto err;

    if (EVP_CipherInit_ex(ctx->ctx, cipher, NULL,
                          key_bin.data, ivec_bin.data, 1) != 1)
        goto err;
    if (EVP_CIPHER_CTX_set_padding(ctx->ctx, 0) != 1)
        goto err;

    ret = enif_make_resource(env, ctx);
    goto done;

 bad_arg:
    return enif_make_badarg(env);

 err:
    ret = enif_make_badarg(env);

 done:
    if (ctx)
        enif_release_resource(ctx);
    return ret;

#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

ERL_NIF_TERM chacha20_stream_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (State, Data) */
#if defined(HAVE_CHACHA20)
    struct evp_cipher_ctx *ctx = NULL, *new_ctx = NULL;
    ErlNifBinary   data_bin;
    ERL_NIF_TERM   ret, cipher_term;
    unsigned char  *out;
    int            outl = 0;

    ASSERT(argc == 2);

    if (!enif_get_resource(env, argv[0], evp_cipher_ctx_rtype, (void**)&ctx))
        goto bad_arg;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &data_bin))
        goto bad_arg;
    if (data_bin.size > INT_MAX)
        goto bad_arg;

    if ((new_ctx = enif_alloc_resource(evp_cipher_ctx_rtype, sizeof(struct evp_cipher_ctx))) == NULL)
        goto err;
    if ((new_ctx->ctx = EVP_CIPHER_CTX_new()) == NULL)
        goto err;

    if (EVP_CIPHER_CTX_copy(new_ctx->ctx, ctx->ctx) != 1)
        goto err;
    if ((out = enif_make_new_binary(env, data_bin.size, &cipher_term)) == NULL)
        goto err;
    if (EVP_CipherUpdate(new_ctx->ctx, out, &outl, data_bin.data, (int)data_bin.size) != 1)
        goto err;
    ASSERT(outl >= 0 && (size_t)outl == data_bin.size);

    ret = enif_make_tuple2(env, enif_make_resource(env, new_ctx), cipher_term);
    CONSUME_REDS(env, data_bin);
    goto done;

 bad_arg:
    return enif_make_badarg(env);

 err:
    ret = enif_make_badarg(env);

 done:
    if (new_ctx)
        enif_release_resource(new_ctx);
    return ret;

#else
    return enif_raise_exception(env, atom_notsup);
#endif
}
