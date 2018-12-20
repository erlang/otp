#include "chacha20.h"
#include "cipher.h"

ERL_NIF_TERM chacha20_stream_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IV) */
#if defined(HAVE_CHACHA20)
    ErlNifBinary     key_bin, ivec_bin;
    struct evp_cipher_ctx *ctx;
    const EVP_CIPHER *cipher;
    ERL_NIF_TERM     ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin)
        || !enif_inspect_binary(env, argv[1], &ivec_bin)
        || key_bin.size != 32
        || ivec_bin.size != 16) {
        return enif_make_badarg(env);
    }

    cipher = EVP_chacha20();

    ctx = enif_alloc_resource(evp_cipher_ctx_rtype, sizeof(struct evp_cipher_ctx));
    ctx->ctx = EVP_CIPHER_CTX_new();


    EVP_CipherInit_ex(ctx->ctx, cipher, NULL,
                      key_bin.data, ivec_bin.data, 1);
    EVP_CIPHER_CTX_set_padding(ctx->ctx, 0);
    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return ret;
#else
    return enif_raise_exception(env, atom_notsup);
#endif
};

ERL_NIF_TERM chacha20_stream_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (State, Data) */
#if defined(HAVE_CHACHA20)
    struct evp_cipher_ctx *ctx, *new_ctx;
    ErlNifBinary   data_bin;
    ERL_NIF_TERM   ret, cipher_term;
    unsigned char  *out;
    int            outl = 0;

    if (!enif_get_resource(env, argv[0], evp_cipher_ctx_rtype, (void**)&ctx)
        || !enif_inspect_iolist_as_binary(env, argv[1], &data_bin)) {
        return enif_make_badarg(env);
    }
    new_ctx = enif_alloc_resource(evp_cipher_ctx_rtype, sizeof(struct evp_cipher_ctx));
    new_ctx->ctx = EVP_CIPHER_CTX_new();
    EVP_CIPHER_CTX_copy(new_ctx->ctx, ctx->ctx);
    out = enif_make_new_binary(env, data_bin.size, &cipher_term);
    EVP_CipherUpdate(new_ctx->ctx, out, &outl, data_bin.data, data_bin.size);
    ASSERT(outl == data_bin.size);

    ret = enif_make_tuple2(env, enif_make_resource(env, new_ctx), cipher_term);
    enif_release_resource(new_ctx);
    CONSUME_REDS(env,data_bin);
    return ret;
#else
    return enif_raise_exception(env, atom_notsup);
#endif
};
