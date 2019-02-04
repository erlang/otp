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

#include "cmac.h"
#include "cipher.h"

ERL_NIF_TERM cmac_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type, Key, Data) */
#if defined(HAVE_CMAC)
    struct cipher_type_t *cipherp = NULL;
    const EVP_CIPHER     *cipher;
    CMAC_CTX             *ctx = NULL;
    ErlNifBinary         key;
    ErlNifBinary         data;
    ERL_NIF_TERM         ret;
    size_t               ret_size;
    unsigned char       *outp;
    int                  cipher_len;

    ASSERT(argc == 3);

    if (!enif_inspect_iolist_as_binary(env, argv[1], &key))
        goto bad_arg;
    if ((cipherp = get_cipher_type(argv[0], key.size)) == NULL)
        goto bad_arg;
    if (!enif_inspect_iolist_as_binary(env, argv[2], &data))
        goto bad_arg;

    if ((cipher = cipherp->cipher.p) == NULL)
        return enif_raise_exception(env, atom_notsup);

    if ((ctx = CMAC_CTX_new()) == NULL)
        goto err;
    if (!CMAC_Init(ctx, key.data, key.size, cipher, NULL))
        goto err;
    if (!CMAC_Update(ctx, data.data, data.size))
        goto err;
    if ((cipher_len = EVP_CIPHER_block_size(cipher)) < 0)
        goto err;
    if ((outp = enif_make_new_binary(env, (size_t)cipher_len, &ret)) == NULL)
        goto err;
    if (!CMAC_Final(ctx, outp, &ret_size))
        goto err;

    ASSERT(ret_size == (unsigned)EVP_CIPHER_block_size(cipher));
    CONSUME_REDS(env, data);
    goto done;

 bad_arg:
    return enif_make_badarg(env);

 err:
    ret = atom_notsup;

 done:
    if (ctx)
        CMAC_CTX_free(ctx);
    return ret;

#else
    /* The CMAC functionality was introduced in OpenSSL 1.0.1
     * Although OTP requires at least version 0.9.8, the versions 0.9.8 and 1.0.0 are
     * no longer maintained. */
    return atom_notsup;
#endif
}

