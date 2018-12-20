#include "aes.h"
#include "cipher.h"

ERL_NIF_TERM aes_cfb_8_crypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec, Data, IsEncrypt) */
     ErlNifBinary key, ivec, text;
     AES_KEY aes_key;
     unsigned char ivec_clone[16]; /* writable copy */
     int new_ivlen = 0;
     ERL_NIF_TERM ret;

     CHECK_NO_FIPS_MODE();

     if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
         || !(key.size == 16 || key.size == 24 || key.size == 32)
         || !enif_inspect_binary(env, argv[1], &ivec) || ivec.size != 16
         || !enif_inspect_iolist_as_binary(env, argv[2], &text)) {
         return enif_make_badarg(env);
     }

     memcpy(ivec_clone, ivec.data, 16);
     AES_set_encrypt_key(key.data, key.size * 8, &aes_key);
     AES_cfb8_encrypt((unsigned char *) text.data,
                      enif_make_new_binary(env, text.size, &ret),
                      text.size, &aes_key, ivec_clone, &new_ivlen,
                      (argv[3] == atom_true));
     CONSUME_REDS(env,text);
     return ret;
}

ERL_NIF_TERM aes_cfb_128_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec, Data, IsEncrypt) */
    ErlNifBinary key, ivec, text;
    AES_KEY aes_key;
    unsigned char ivec_clone[16]; /* writable copy */
    int new_ivlen = 0;
    ERL_NIF_TERM ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key)
        || !(key.size == 16 || key.size == 24 || key.size == 32)
        || !enif_inspect_binary(env, argv[1], &ivec) || ivec.size != 16
        || !enif_inspect_iolist_as_binary(env, argv[2], &text)) {
        return enif_make_badarg(env);
    }

    memcpy(ivec_clone, ivec.data, 16);
    AES_set_encrypt_key(key.data, key.size * 8, &aes_key);
    AES_cfb128_encrypt((unsigned char *) text.data,
                       enif_make_new_binary(env, text.size, &ret),
                       text.size, &aes_key, ivec_clone, &new_ivlen,
                       (argv[3] == atom_true));
    CONSUME_REDS(env,text);
    return ret;
}

ERL_NIF_TERM aes_ige_crypt_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec, Data, IsEncrypt) */
#ifdef HAVE_AES_IGE
    ErlNifBinary key_bin, ivec_bin, data_bin;
    AES_KEY aes_key;
    unsigned char ivec[32];
    int i;
    unsigned char* ret_ptr;
    ERL_NIF_TERM ret;

    CHECK_NO_FIPS_MODE();

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin)
       || (key_bin.size != 16 && key_bin.size != 32)
       || !enif_inspect_binary(env, argv[1], &ivec_bin)
       || ivec_bin.size != 32
       || !enif_inspect_iolist_as_binary(env, argv[2], &data_bin)
       || data_bin.size % 16 != 0) {

       return enif_make_badarg(env);
    }

    if (argv[3] == atom_true) {
       i = AES_ENCRYPT;
       AES_set_encrypt_key(key_bin.data, key_bin.size*8, &aes_key);
    }
    else {
       i = AES_DECRYPT;
       AES_set_decrypt_key(key_bin.data, key_bin.size*8, &aes_key);
    }

    ret_ptr = enif_make_new_binary(env, data_bin.size, &ret);
    memcpy(ivec, ivec_bin.data, 32); /* writable copy */
    AES_ige_encrypt(data_bin.data, ret_ptr, data_bin.size, &aes_key, ivec, i);
    CONSUME_REDS(env,data_bin);
    return ret;
#else
    return atom_notsup;
#endif
}


/* Initializes state for ctr streaming (de)encryption
*/
#ifdef HAVE_EVP_AES_CTR
ERL_NIF_TERM aes_ctr_stream_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec) */
    ErlNifBinary     key_bin, ivec_bin;
    struct evp_cipher_ctx *ctx;
    const EVP_CIPHER *cipher;
    ERL_NIF_TERM     ret;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin)
        || !enif_inspect_binary(env, argv[1], &ivec_bin)
        || ivec_bin.size != 16) {
        return enif_make_badarg(env);
    }

    switch (key_bin.size)
    {
    case 16: cipher = EVP_aes_128_ctr(); break;
    case 24: cipher = EVP_aes_192_ctr(); break;
    case 32: cipher = EVP_aes_256_ctr(); break;
    default: return enif_make_badarg(env);
    }

    ctx = enif_alloc_resource(evp_cipher_ctx_rtype, sizeof(struct evp_cipher_ctx));
    ctx->ctx = EVP_CIPHER_CTX_new();
    EVP_CipherInit_ex(ctx->ctx, cipher, NULL,
                      key_bin.data, ivec_bin.data, 1);
    EVP_CIPHER_CTX_set_padding(ctx->ctx, 0);
    ret = enif_make_resource(env, ctx);
    enif_release_resource(ctx);
    return ret;
}

ERL_NIF_TERM aes_ctr_stream_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Context, Data) */
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
}

#else /* if not HAVE_EVP_AES_CTR */

ERL_NIF_TERM aes_ctr_stream_init(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Key, IVec) */
    ErlNifBinary key_bin, ivec_bin;
    ERL_NIF_TERM ecount_bin;

    if (!enif_inspect_iolist_as_binary(env, argv[0], &key_bin)
        || !enif_inspect_binary(env, argv[1], &ivec_bin)
        || !(key_bin.size == 16 || key_bin.size == 24 || key_bin.size ==32)
        || ivec_bin.size != 16) {
        return enif_make_badarg(env);
    }

    memset(enif_make_new_binary(env, AES_BLOCK_SIZE, &ecount_bin),
           0, AES_BLOCK_SIZE);
    return enif_make_tuple4(env, argv[0], argv[1], ecount_bin, enif_make_int(env, 0));
}

ERL_NIF_TERM aes_ctr_stream_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* ({Key, IVec, ECount, Num}, Data) */
    ErlNifBinary key_bin, ivec_bin, text_bin, ecount_bin;
    AES_KEY aes_key;
    unsigned int num;
    ERL_NIF_TERM ret, num2_term, cipher_term, ivec2_term, ecount2_term, new_state_term;
    int state_arity;
    const ERL_NIF_TERM *state_term;
    unsigned char * ivec2_buf;
    unsigned char * ecount2_buf;

    if (!enif_get_tuple(env, argv[0], &state_arity, &state_term)
        || state_arity != 4
        || !enif_inspect_iolist_as_binary(env, state_term[0], &key_bin)
        || AES_set_encrypt_key(key_bin.data, key_bin.size*8, &aes_key) != 0
        || !enif_inspect_binary(env, state_term[1], &ivec_bin) || ivec_bin.size != 16
        || !enif_inspect_binary(env, state_term[2], &ecount_bin) || ecount_bin.size != AES_BLOCK_SIZE
        || !enif_get_uint(env, state_term[3], &num)
        || !enif_inspect_iolist_as_binary(env, argv[1], &text_bin)) {
        return enif_make_badarg(env);
    }

    ivec2_buf = enif_make_new_binary(env, ivec_bin.size, &ivec2_term);
    ecount2_buf = enif_make_new_binary(env, ecount_bin.size, &ecount2_term);

    memcpy(ivec2_buf, ivec_bin.data, 16);
    memcpy(ecount2_buf, ecount_bin.data, ecount_bin.size);

    AES_ctr128_encrypt((unsigned char *) text_bin.data,
		       enif_make_new_binary(env, text_bin.size, &cipher_term),
		       text_bin.size, &aes_key, ivec2_buf, ecount2_buf, &num);

    num2_term = enif_make_uint(env, num);
    new_state_term = enif_make_tuple4(env, state_term[0], ivec2_term, ecount2_term, num2_term);
    ret = enif_make_tuple2(env, new_state_term, cipher_term);
    CONSUME_REDS(env,text_bin);
    return ret;
}
#endif /* !HAVE_EVP_AES_CTR */

#ifdef HAVE_GCM_EVP_DECRYPT_BUG
ERL_NIF_TERM aes_gcm_decrypt_NO_EVP(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type,Key,Iv,AAD,In,Tag) */
    GCM128_CONTEXT *ctx;
    ErlNifBinary key, iv, aad, in, tag;
    AES_KEY aes_key;
    unsigned char *outp;
    ERL_NIF_TERM out;

    if (!enif_inspect_iolist_as_binary(env, argv[1], &key)
        || AES_set_encrypt_key(key.data, key.size*8, &aes_key) != 0
        || !enif_inspect_binary(env, argv[2], &iv) || iv.size == 0
        || !enif_inspect_iolist_as_binary(env, argv[3], &aad)
        || !enif_inspect_iolist_as_binary(env, argv[4], &in)
        || !enif_inspect_iolist_as_binary(env, argv[5], &tag)) {
        return enif_make_badarg(env);
    }

    if (!(ctx = CRYPTO_gcm128_new(&aes_key, (block128_f)AES_encrypt)))
        return atom_error;

    CRYPTO_gcm128_setiv(ctx, iv.data, iv.size);

    if (CRYPTO_gcm128_aad(ctx, aad.data, aad.size))
        goto out_err;

    outp = enif_make_new_binary(env, in.size, &out);

    /* decrypt */
    if (CRYPTO_gcm128_decrypt(ctx, in.data, outp, in.size))
            goto out_err;

    /* calculate and check the tag */
    if (CRYPTO_gcm128_finish(ctx, tag.data, tag.size))
            goto out_err;

    CRYPTO_gcm128_release(ctx);
    CONSUME_REDS(env, in);

    return out;

out_err:
    CRYPTO_gcm128_release(ctx);
    return atom_error;
}
#endif /* HAVE_GCM_EVP_DECRYPT_BUG */

