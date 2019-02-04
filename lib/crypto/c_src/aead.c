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

#include "aead.h"
#include "aes.h"

ERL_NIF_TERM aead_encrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type,Key,Iv,AAD,In) */
#if defined(HAVE_AEAD)
    EVP_CIPHER_CTX *ctx = NULL;
    const EVP_CIPHER *cipher = NULL;
    ErlNifBinary key, iv, aad, in;
    unsigned int tag_len;
    unsigned char *outp, *tagp;
    ERL_NIF_TERM type, out, out_tag, ret;
    int len, ctx_ctrl_set_ivlen, ctx_ctrl_get_tag;

    type = argv[0];

    ASSERT(argc == 6);

    if (!enif_is_atom(env, type))
        goto bad_arg;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &key))
        goto bad_arg;
    if (!enif_inspect_binary(env, argv[2], &iv))
        goto bad_arg;
    if (!enif_inspect_iolist_as_binary(env, argv[3], &aad))
        goto bad_arg;
    if (!enif_inspect_iolist_as_binary(env, argv[4], &in))
        goto bad_arg;
    if (!enif_get_uint(env, argv[5], &tag_len))
        goto bad_arg;

    if (tag_len > INT_MAX
        || iv.size > INT_MAX
        || in.size > INT_MAX
        || aad.size > INT_MAX)
        goto bad_arg;

    /* Use cipher_type some day.  Must check block_encrypt|decrypt first */
#if defined(HAVE_GCM)
    if (type == atom_aes_gcm) {
        if (iv.size == 0)
            goto bad_arg;
        if (tag_len < 1 || tag_len > 16)
            goto bad_arg;

        ctx_ctrl_set_ivlen = EVP_CTRL_GCM_SET_IVLEN;
        ctx_ctrl_get_tag = EVP_CTRL_GCM_GET_TAG;

        switch (key.size) {
        case 16:
            cipher = EVP_aes_128_gcm();
            break;
        case 24:
            cipher = EVP_aes_192_gcm();
            break;
        case 32:
            cipher = EVP_aes_256_gcm();
            break;
        default:
            goto bad_arg;
        }
    } else
#endif
#if defined(HAVE_CCM)
    if (type == atom_aes_ccm) {
        if (iv.size < 7 || iv.size > 13)
            goto bad_arg;
        if (tag_len < 4 || tag_len > 16)
            goto bad_arg;
        if ((tag_len & 1) != 0)
            goto bad_arg;

        ctx_ctrl_set_ivlen = EVP_CTRL_CCM_SET_IVLEN;
        ctx_ctrl_get_tag = EVP_CTRL_CCM_GET_TAG;

        switch (key.size) {
        case 16:
            cipher = EVP_aes_128_ccm();
            break;
        case 24:
            cipher = EVP_aes_192_ccm();
            break;
        case 32:
            cipher = EVP_aes_256_ccm();
            break;
        default:
            goto bad_arg;
        }
    } else
#endif
#if defined(HAVE_CHACHA20_POLY1305)
    if (type == atom_chacha20_poly1305) {
        if (key.size != 32)
            goto bad_arg;
        if (iv.size < 1 || iv.size > 16)
            goto bad_arg;
        if (tag_len != 16)
            goto bad_arg;

        ctx_ctrl_set_ivlen = EVP_CTRL_AEAD_SET_IVLEN;
        ctx_ctrl_get_tag = EVP_CTRL_AEAD_GET_TAG;

        cipher = EVP_chacha20_poly1305();

    } else
#endif
        return enif_raise_exception(env, atom_notsup);
 
    if ((ctx = EVP_CIPHER_CTX_new()) == NULL)
        goto err;

    if (EVP_EncryptInit_ex(ctx, cipher, NULL, NULL, NULL) != 1)
        goto err;
    if (EVP_CIPHER_CTX_ctrl(ctx, ctx_ctrl_set_ivlen, (int)iv.size, NULL) != 1)
        goto err;

#if defined(HAVE_CCM)
    if (type == atom_aes_ccm) {
        if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_CCM_SET_TAG, (int)tag_len, NULL) != 1)
            goto err;
        if (EVP_EncryptInit_ex(ctx, NULL, NULL, key.data, iv.data) != 1)
            goto err;
        if (EVP_EncryptUpdate(ctx, NULL, &len, NULL, (int)in.size) != 1)
            goto err;
    } else
#endif
    {
        if (EVP_EncryptInit_ex(ctx, NULL, NULL, key.data, iv.data) != 1)
            goto err;
    }

    if (EVP_EncryptUpdate(ctx, NULL, &len, aad.data, (int)aad.size) != 1)
        goto err;

    if ((outp = enif_make_new_binary(env, in.size, &out)) == NULL)
        goto err;

    if (EVP_EncryptUpdate(ctx, outp, &len, in.data, (int)in.size) != 1)
        goto err;
    if (EVP_EncryptFinal_ex(ctx, outp/*+len*/, &len) != 1)
        goto err;

    if ((tagp = enif_make_new_binary(env, tag_len, &out_tag)) == NULL)
        goto err;

    if (EVP_CIPHER_CTX_ctrl(ctx, ctx_ctrl_get_tag, (int)tag_len, tagp) != 1)
        goto err;

    CONSUME_REDS(env, in);
    ret = enif_make_tuple2(env, out, out_tag);
    goto done;

 bad_arg:
    ret = enif_make_badarg(env);
    goto done;

 err:
    ret = atom_error;

 done:
    if (ctx)
        EVP_CIPHER_CTX_free(ctx);
    return ret;

#else
    return enif_raise_exception(env, atom_notsup);
#endif
}

ERL_NIF_TERM aead_decrypt(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{/* (Type,Key,Iv,AAD,In,Tag) */
#if defined(HAVE_AEAD)
    EVP_CIPHER_CTX *ctx = NULL;
    const EVP_CIPHER *cipher = NULL;
    ErlNifBinary key, iv, aad, in, tag;
    unsigned char *outp;
    ERL_NIF_TERM type, out, ret;
    int len, ctx_ctrl_set_ivlen, ctx_ctrl_set_tag;

    ASSERT(argc == 6);

    type = argv[0];
#if defined(HAVE_GCM_EVP_DECRYPT_BUG)
    if (type == atom_aes_gcm)
        return aes_gcm_decrypt_NO_EVP(env, argc, argv);
#endif

    if (!enif_is_atom(env, type))
        goto bad_arg;
    if (!enif_inspect_iolist_as_binary(env, argv[1], &key))
        goto bad_arg;
    if (!enif_inspect_binary(env, argv[2], &iv))
        goto bad_arg;
    if (!enif_inspect_iolist_as_binary(env, argv[3], &aad))
        goto bad_arg;
    if (!enif_inspect_iolist_as_binary(env, argv[4], &in))
        goto bad_arg;
    if (!enif_inspect_iolist_as_binary(env, argv[5], &tag))
        goto bad_arg;

    if (tag.size > INT_MAX
        || key.size > INT_MAX
        || iv.size > INT_MAX
        || in.size > INT_MAX
        || aad.size > INT_MAX)
        goto bad_arg;

    /* Use cipher_type some day.  Must check block_encrypt|decrypt first */
#if defined(HAVE_GCM)
    if (type == atom_aes_gcm) {
        if (iv.size == 0)
            goto bad_arg;

        ctx_ctrl_set_ivlen = EVP_CTRL_GCM_SET_IVLEN;
        ctx_ctrl_set_tag = EVP_CTRL_GCM_SET_TAG;

        switch (key.size) {
        case 16:
            cipher = EVP_aes_128_gcm();
            break;
        case 24:
            cipher = EVP_aes_192_gcm();
            break;
        case 32:
            cipher = EVP_aes_256_gcm();
            break;
        default:
            goto bad_arg;
        }
    } else
#endif
#if defined(HAVE_CCM)
    if (type == atom_aes_ccm) {
        if (iv.size == 0)
            goto bad_arg;

        ctx_ctrl_set_ivlen = EVP_CTRL_CCM_SET_IVLEN;
        ctx_ctrl_set_tag = EVP_CTRL_CCM_SET_TAG;

        switch (key.size) {
        case 16:
            cipher = EVP_aes_128_ccm();
            break;
        case 24:
            cipher = EVP_aes_192_ccm();
            break;
        case 32:
            cipher = EVP_aes_256_ccm();
            break;
        default:
            goto bad_arg;
        }
    } else
#endif
#if defined(HAVE_CHACHA20_POLY1305)
    if (type == atom_chacha20_poly1305) {
        if (key.size != 32)
            goto bad_arg;
        if (iv.size < 1 || iv.size > 16)
            goto bad_arg;
        if (tag.size != 16)
            goto bad_arg;

        ctx_ctrl_set_ivlen = EVP_CTRL_AEAD_SET_IVLEN;
        ctx_ctrl_set_tag = EVP_CTRL_AEAD_SET_TAG;

        cipher = EVP_chacha20_poly1305();
    } else
#endif
        return enif_raise_exception(env, atom_notsup);

    if ((outp = enif_make_new_binary(env, in.size, &out)) == NULL)
        goto err;

    if ((ctx = EVP_CIPHER_CTX_new()) == NULL)
        goto err;
    if (EVP_DecryptInit_ex(ctx, cipher, NULL, NULL, NULL) != 1)
        goto err;
    if (EVP_CIPHER_CTX_ctrl(ctx,  ctx_ctrl_set_ivlen, (int)iv.size, NULL) != 1)
        goto err;

#if defined(HAVE_CCM)
    if (type == atom_aes_ccm) {
        if (EVP_CIPHER_CTX_ctrl(ctx, EVP_CTRL_CCM_SET_TAG, (int)tag.size, tag.data) != 1)
            goto err;
    }
#endif

    if (EVP_DecryptInit_ex(ctx, NULL, NULL, key.data, iv.data) != 1)
        goto err;

#if defined(HAVE_CCM)
    if (type == atom_aes_ccm) {
        if (EVP_DecryptUpdate(ctx, NULL, &len, NULL, (int)in.size) != 1)
            goto err;
    }
#endif

    if (EVP_DecryptUpdate(ctx, NULL, &len, aad.data, (int)aad.size) != 1)
        goto err;
    if (EVP_DecryptUpdate(ctx, outp, &len, in.data, (int)in.size) != 1)
        goto err;

#if defined(HAVE_GCM) || defined(HAVE_CHACHA20_POLY1305)
    if (type == atom_aes_gcm) {
        if (EVP_CIPHER_CTX_ctrl(ctx, ctx_ctrl_set_tag, (int)tag.size, tag.data) != 1)
             goto err;
        if (EVP_DecryptFinal_ex(ctx, outp+len, &len) != 1)
            goto err;
    }
#endif
    CONSUME_REDS(env, in);
    ret = out;
    goto done;

 bad_arg:
    ret = enif_make_badarg(env);
    goto done;

 err:
    ret = atom_error;

 done:
    if (ctx)
        EVP_CIPHER_CTX_free(ctx);
    return ret;

#else
    return enif_raise_exception(env, atom_notsup);
#endif
}
