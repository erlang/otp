#include "cmac.h"
#include "cipher.h"

ERL_NIF_TERM cmac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Key, Data) */
#if defined(HAVE_CMAC)
    struct cipher_type_t *cipherp = NULL;
    const EVP_CIPHER     *cipher;
    CMAC_CTX             *ctx;
    ErlNifBinary         key;
    ErlNifBinary         data;
    ERL_NIF_TERM         ret;
    size_t               ret_size;

    if (!enif_inspect_iolist_as_binary(env, argv[1], &key)
        || !(cipherp = get_cipher_type(argv[0], key.size))
        || !enif_inspect_iolist_as_binary(env, argv[2], &data)) {
        return enif_make_badarg(env);
    }
    cipher = cipherp->cipher.p;
    if (!cipher) {
        return enif_raise_exception(env, atom_notsup);
    }

    ctx = CMAC_CTX_new();
    if (!CMAC_Init(ctx, key.data, key.size, cipher, NULL)) {
        CMAC_CTX_free(ctx);
        return atom_notsup;
    }

    if (!CMAC_Update(ctx, data.data, data.size) ||
        !CMAC_Final(ctx,
                    enif_make_new_binary(env, EVP_CIPHER_block_size(cipher), &ret),
                    &ret_size)) {
        CMAC_CTX_free(ctx);
        return atom_notsup;
    }
    ASSERT(ret_size == (unsigned)EVP_CIPHER_block_size(cipher));

    CMAC_CTX_free(ctx);
    CONSUME_REDS(env, data);
    return ret;
#else
    /* The CMAC functionality was introduced in OpenSSL 1.0.1
     * Although OTP requires at least version 0.9.8, the versions 0.9.8 and 1.0.0 are
     * no longer maintained. */
    return atom_notsup;
#endif
}

